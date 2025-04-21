(ns bci-project.signal
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.core.reducers :as r])
  (:import [com.github.psambit9791.jdsp.filter Butterworth]))

(defn average-vector
  "Calculate average vector across all channels."
  [channels]
  (let [n-channels (count channels)
        n-samples (count (first channels))
        avg-array (double-array n-samples 0.0)]
    (doseq [channel channels]
      (dotimes [j n-samples]
        (aset avg-array j (+ (aget avg-array j)
                             (/ (double (nth channel j)) n-channels)))))
    (vec avg-array)))

(defn common-average-reference
  "Apply common average reference to EEG data."
  [data]
  (if-let [values (:values data)]
    (let [eeg-channels (vec (take 64 values))
          non-eeg-channels (vec (drop 64 values))
          avg-reference (average-vector eeg-channels)
          referenced-eeg
          (into []
                (r/map
                 (fn [channel]
                   (into []
                         (r/map
                          (fn [i]
                            (- (nth channel i) (nth avg-reference i)))
                          (range (count channel)))))
                 eeg-channels))]
      (assoc data :values (vec (concat referenced-eeg non-eeg-channels))))
    data))

(defn process-stream-reducers
  "Process a single data stream using reducers and save it to a file"
  [eeg-data key base-path]
  (let [file-path (str base-path "/" (name key) ".edn")]
    (if (.exists (java.io.File. file-path))
      (do
        (println (str "Found existing " key " data, loading..."))
        (try
          (read-string (slurp file-path))
          (catch Exception e
            (println (str "Error loading " key " data: " (.getMessage e)))
            nil)))
      (do
        (println (str "Processing " key " data..."))
        (try
          (if-let [data (get eeg-data key)]
            (let [processed (common-average-reference data)]
              (println (str "Saving " key " data..."))
              (let [temp-file (str file-path ".tmp")]
                (with-open [w (clojure.java.io/writer temp-file)]
                  (binding [*out* w]

                    (print "{")
                    (doseq [[k v] (dissoc processed :values)]
                      (print (str ":" (name k) " "))
                      (pr v)
                      (print ", "))
                    (print ":values [")

                    (let [values (:values processed)]
                      (dotimes [i (count values)]
                        (when (> i 0) (print " "))
                        (pr (nth values i))))
                    (print "]}")))
                (.renameTo (java.io.File. temp-file) (java.io.File. file-path)))
              (System/gc)
              processed)
            (do
              (println (str "No data found for " key))
              nil))
          (catch Exception e
            (println (str "Error processing " key " data: " (.getMessage e)))
            (println (.printStackTrace e))
            nil))))))

(defn load-avg-ref-data!
  "Load the processed average reference data from saved files and update the eeg-data-atom."
  [eeg-data-atom]
  (let [sample-number (:sample_number @eeg-data-atom)
        base-path (str "resources/average-referencing/s" sample-number)
        files {:avg_ref_mvmt_left   (str base-path "/movement_left.edn")
               :avg_ref_mvmt_right  (str base-path "/movement_right.edn")
               :avg_ref_img_left    (str base-path "/imagery_left.edn")
               :avg_ref_img_right   (str base-path "/imagery_right.edn")}
        loaded-data (reduce-kv
                     (fn [acc key file-path]
                       (if (.exists (java.io.File. file-path))
                         (let [data (read-string (slurp file-path))]
                           (assoc acc key data))
                         acc))
                     {} files)]
    (swap! eeg-data-atom #(merge % loaded-data))))

(defn process-and-save-common-average-references
  [eeg-data-atom]
  (let [sample-number (:sample_number @eeg-data-atom)
        base-path (str "resources/average-referencing/s" sample-number)]
    (.mkdirs (java.io.File. (str base-path "/")))
    (let [eeg-data @eeg-data-atom
          data-keys [:movement_left :movement_right :imagery_left :imagery_right]
          result (reduce (fn [result key]
                           (if-let [processed (process-stream-reducers eeg-data key base-path)]
                             (assoc result key processed)
                             result))
                         {}
                         data-keys)]
      result)))

(defn apply-precomputed-car
  "Apply pre-computed common average reference to a specific channel
   Parameters:
     - channel-data: Vector of signal values for a single channel
     - car-values: Pre-computed CAR values (from 'load-common-average-reference')
     - start-idx: Start index in the original data
     - end-idx: End index in the original data"
  [channel-data car-values start-idx end-idx]
  (let [common-avg (:common-average car-values)
        section-avg (subvec common-avg start-idx end-idx)]

    (mapv - channel-data section-avg)))


(defn frequency-range
  [band-name]
  (case band-name
    :delta [0.5 4]
    :theta [4 8]
    :alpha [8 13]
    :beta [13 30]
    :gamma [30 100]
    :all [0.5 100]))

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
  [signal srate]
  (butterworth-filter signal 0.5 nil srate 4 :high-pass))

(defn band-pass-butterworth-filter
  [signal band-name srate]
  (let [[low-freq high-freq] (frequency-range band-name)]
    (butterworth-filter signal low-freq high-freq srate 4 :band-pass)))