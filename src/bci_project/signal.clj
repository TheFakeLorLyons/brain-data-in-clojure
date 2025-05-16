(ns bci-project.signal
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.core.reducers :as r]
            [tech.v3.tensor :as tensor]
            [tech.v3.tensor.dimensions :as tensor-dims])
  (:import [com.github.psambit9791.jdsp.filter Butterworth]))

;=================================================================================================
                                                         ;              Definitions              ;
;=================================================================================================

^:kindly/hide-code
(def SRATE 512)
^:kindly/hide-code
(def BAD-TRIAL-THRESHOLD 100)

^:kindly/hide-code
(def filtered-keys
  [:filtered_movement_left :filtered_movement_right
   :filtered_imagery_left :filtered_imagery_right])

^:kindly/hide-code
(def standard-keys
  [:movement_left :movement_right
   :imagery_left :imagery_right])

^:kindly/hide-code
(def avg-ref-keys
  [:avg_ref_mvmt_left :avg_ref_mvmt_right :avg_ref_img_left :avg_ref_img_right])

;=================================================================================================
                                                         ;        Common Average Reference       ;
;=================================================================================================

(defn calculate-common-average-reference
  "Calculate common average reference for all channels at each time point"
  [eeg-tensor dims]
  (let [[num-channels num-samples] dims]
    (mapv (fn [sample]
            (let [channel-values (mapv (fn [ch]
                                         (tensor/mget eeg-tensor ch sample))
                                       (range num-channels))]
              (/ (reduce + channel-values) num-channels)))
          (range num-samples))))

(defn apply-common-average-reference
  "Apply common average reference to EEG data"
  [eeg-tensor dims]
  (let [[num-channels num-samples] dims
        avg-per-sample               ; Precompute average per sample
        (mapv (fn [sample]
                (/ (reduce + (map (fn [ch]
                                    (tensor/mget eeg-tensor ch sample))
                                  (range num-channels)))
                   num-channels))
              (range num-samples))]
    (tensor/compute-tensor           ; Subtract average from each value
     dims
     (fn [ch sample]
       (- (tensor/mget eeg-tensor ch sample)
          (nth avg-per-sample sample)))
     :float64)))

(defn load-common-average-reference-data
  "Load common average reference data from saved files and update the eeg-data-atom."
  [eeg-data-atom]
  (let [eeg-data @eeg-data-atom
        missing-keys (filter #(not (contains? eeg-data %)) avg-ref-keys)]
    (when (seq missing-keys)
      (let [sample-number (:sample_number eeg-data)
            base-path (str "resources/average-referencing/s" sample-number)
            files (into {} (map (fn [key]
                                  [key (str base-path "/"
                                            (name key) "-car.edn")])
                                avg-ref-keys))]
        (doseq [[key file] files]
          (when (.exists (java.io.File. file))
            (println "Loading" (name key) "referenced data from" file)
            (let [referenced-data (read-string (slurp file))]
              (swap! eeg-data-atom assoc-in [(keyword (str (name key) "_car"))] referenced-data))))))))


(defn process-and-save-common-average-references
  "Process all trial types and save common average referenced data to the data atom"
  [data-atom]
  (println "Applying common average referencing to all trial types...")
  (let [data @data-atom
        trial-types standard-keys
        avg-ref-keys {:movement_left :avg_ref_mvmt_left
                      :movement_right :avg_ref_mvmt_right
                      :imagery_left :avg_ref_img_left
                      :imagery_right :avg_ref_img_right}]

    (doseq [trial-type trial-types]
      (let [eeg-data (get data trial-type)
            eeg-tensor (:values eeg-data)
            dims (:dims eeg-data)
            avg-ref-key (get avg-ref-keys trial-type)]

        (println "Processing" (name trial-type) "for common average reference")
        (let [car-tensor (apply-common-average-reference eeg-tensor dims)]

          (swap! data-atom assoc avg-ref-key
                 {:values car-tensor
                  :dims dims
                  :type (:type eeg-data)
                  :elements (:elements eeg-data)})

          (println "Completed common average referencing for" (name trial-type)))))

    (println "Common average referencing complete for all trial types.")))


;=================================================================================================
                                                         ;                  Filter               ;
;=================================================================================================

(defn frequency-range
  [band-name]
  (case band-name
    :delta     [0.5 4]
    :theta     [4 8]
    :alpha     [8 13]
    :bad-trial [8 30]
    :beta      [13 30]
    :gamma     [30 100]
    :all       [0.5 100]))

(defn butterworth-filter
  "Apply a Butterworth filter to a signal
   Parameters:
     - signal: Vector of signal values
     - cutoff-low: Low cutoff frequency (Hz)
     - cutoff-high: High cutoff frequency (Hz, use nil for lowpass)
     - srate: Sampling rate (Hz)
     - order: Filter order
     - filter-type: :low-pass, :high-pass, or :band-pass"
  [signal cutoff-low cutoff-high srate order filter-type]
  (let [signal-array (double-array signal)
        butterworth (Butterworth. srate)
        filtered-signal (case filter-type
                          :low-pass (.lowPassFilter butterworth signal-array order cutoff-low)
                          :high-pass (.highPassFilter butterworth signal-array order cutoff-low)
                          :band-pass (.bandPassFilter butterworth signal-array order cutoff-low cutoff-high))]
    (vec filtered-signal)))

(defn high-pass-butterworth-filter
  "Apply high-pass filter (>0.5 Hz) to remove drifts from EEG signal"
  [signal]
  (let [signal-array (cond
                       (tensor/tensor? signal) (double-array (tensor/->jvm signal))
                       (vector? signal) (double-array signal)
                       :else signal)]
    (let [filtered (butterworth-filter signal-array 0.5 nil SRATE 4 :high-pass)]
      (if (tensor/tensor? signal)
        (tensor/->tensor filtered)
        filtered))))

(defn band-pass-butterworth-filter
  [signal band-name]
  (let [[low-freq high-freq] (frequency-range band-name)
        signal-array (cond
                       (tensor/tensor? signal) (double-array (tensor/->jvm signal))
                       (vector? signal) (double-array signal)
                       :else signal)
        filtered (butterworth-filter signal-array low-freq high-freq SRATE 4 :band-pass)]
    (if (tensor/tensor? signal)
      (tensor/->tensor filtered)
      filtered)))

(defn apply-initial-filtering!
  "Apply initial Butterworth filtering to the EEG data"
  [data-atom frequency-band]
  (println "Applying initial Butterworth filtering...")
  (let [data @data-atom
        trial-types [:movement_left :movement_right :imagery_left :imagery_right]]
    (doseq [trial-type trial-types]
      (let [eeg-data (get data trial-type)
            eeg-tensor (:values eeg-data)
            dims (:dims eeg-data)
            num-channels (first dims)
            num-samples (second dims)]
        (println "Filtering channels for" (name trial-type))
        (let [filtered-tensor (tensor/new-tensor dims)]
          (doseq [ch (range num-channels)]
            (let [channel-data (mapv #(tensor/mget eeg-tensor ch %) (range num-samples))
                  high-pass-data (high-pass-butterworth-filter channel-data)
                  band-pass-data (band-pass-butterworth-filter high-pass-data frequency-band)]
              (doseq [i (range (count band-pass-data))]
                (tensor/mset! filtered-tensor ch i (nth band-pass-data i)))))
          (let [filtered-key (keyword (str "filtered_" (name trial-type)))]
            (swap! data-atom assoc filtered-key {:dims dims 
                                                 :values filtered-tensor}))
          (println "Completed filtering for" (name trial-type))))))
  (println "Initial filtering complete."))