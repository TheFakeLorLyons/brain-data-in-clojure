(ns bci-project.signal
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.core.reducers :as r])
  (:import [com.github.psambit9791.jdsp.filter Butterworth]))

(def movement-keys
  [:movement_left :movement_right :imagery_left :imagery_right])

(def avg-ref-keys
  [:avg_ref_mvmt_left :avg_ref_mvmt_right :avg_ref_img_left :avg_ref_img_right])

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

(defn load-avg-ref-data!
  "Load the processed average reference data from saved files and update the eeg-data-atom."
  [eeg-data-atom]
  (let [eeg-data @eeg-data-atom
        missing-keys (filter #(not (contains? eeg-data %)) avg-ref-keys)]

    (when (seq missing-keys)
      (let [sample-number (:sample_number eeg-data)
            base-path (str "resources/average-referencing/s" sample-number)
            files (into {} (map (fn [key]
                                  [key (str base-path "/"
                                            (name (case key
                                                    :avg_ref_mvmt_left "movement_left"
                                                    :avg_ref_mvmt_right "movement_right"
                                                    :avg_ref_img_left "imagery_left"
                                                    :avg_ref_img_right "imagery_right"
                                                    key)) ".edn")])
                                missing-keys))
            loaded-data (reduce-kv
                         (fn [acc key file-path]
                           (if (.exists (java.io.File. file-path))
                             (let [data (read-string (slurp file-path))]
                               (println "Loaded data for" key)
                               (assoc acc key data))
                             (do
                               (println "File not found:" file-path)
                               acc)))
                         {} files)]
        (println "Loaded data keys:" (keys loaded-data))
        (when (seq loaded-data)
          (swap! eeg-data-atom #(merge % loaded-data)))))))

(defn process-stream-reducers
  "Process a single data stream using reducers and save it to a file"
  [eeg-data-atom key]
  (let [eeg-data @eeg-data-atom
        sample-number (:sample_number eeg-data)
        base-path (str "resources/average-referencing/s" sample-number)
        file-path (str base-path "/" (name key) ".edn")]
    (if (.exists (java.io.File. file-path))
      (do
        (println (str "Found existing " key " data, loading..."))
        (load-avg-ref-data! eeg-data-atom)
        nil)
      (do
        (println (str "Processing " key " data..."))
        (try
          (.mkdirs (java.io.File. (str base-path "/")))
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
              (load-avg-ref-data! eeg-data-atom)
              processed)
            (do
              (println (str "No data found for " key))
              nil))
          (catch Exception e
            (println (str "Error processing " key " data: " (.getMessage e)))
            (.printStackTrace e)
            nil))))))

(defn process-and-save-common-average-references
  [eeg-data-atom]
    (let [result (reduce (fn [result key]
                           (if-let [processed (process-stream-reducers eeg-data-atom key)]
                             (assoc result key processed)
                             result))
                         {}
                         movement-keys)]
      result))

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