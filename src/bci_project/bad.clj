(ns bci-project.bad
  (:require [bci-project.bad :as bad]
            [bci-project.brain :as brain]
            [bci-project.signal :as sig]
            [bci-project.trial-analysis :as analysis] 
            [clojure.string :as str]
            [tech.v3.tensor :as tensor]
            [tech.v3.tensor.dimensions :as tensor-dims]
            [tech.v3.datatype.functional :as func]
            [tech.v3.datatype.argops :as argops]
            [tech.v3.datatype :as dt]))

(defn calculate-bad-channels
  "Calculate bad channels based on signal statistics.
   Returns a map with channel indices and their assessment."
  [data-atom frequency-band threshold]
  (let [data @data-atom
        eeg-values (get-in data [:movement_left :values])
        channel-count (first (tensor-dims/shape eeg-values))

        channel-analyses (for [ch-idx (range channel-count)]
                           (let [channel-data (analysis/extract-channel-data eeg-values ch-idx)

                                 rms (Math/sqrt (/ (reduce + (map #(* % %) channel-data))
                                                   (count channel-data)))
                                 std-dev (Math/sqrt (- (/ (reduce + (map #(* % %) channel-data))
                                                          (count channel-data))
                                                       (Math/pow (/ (reduce + channel-data)
                                                                    (count channel-data)) 2)))
                                 is-bad (or (> rms threshold)
                                            (< std-dev (/ threshold 10)))]
                             {:channel-index ch-idx
                              :rms rms
                              :std-dev std-dev
                              :is-bad is-bad}))]

    {:bad-channels-analysis channel-analyses}))
^:kindly/hide-code
(def bag-voltage-frame-segment
  [500 2500])

;(func/sum (:values (:movement_event @brain/eeg-data-atom)))
;(dt/shape (:values (:movement_event @brain/eeg-data-atom)))
;(tensor/->tensor (argops/argfilter #(= 1.0 %) (:values (:movement_event @brain/eeg-data-atom))))

^:kindly/hide-code
(defn create-trials
  "Create trials from EEG data using tensor operations"
  [data-atom trial-type]
  (let [data @data-atom
        tensor-data (get data trial-type)]
    (if (or (nil? tensor-data)
            (not (tensor/tensor? (:values tensor-data))))
      (do
        (println "No valid tensor data found for" (name trial-type))
        (tensor/->tensor []))
      (let [base-type (cond
                        (str/includes? (name trial-type) "movement") :movement
                        (str/includes? (name trial-type) "imagery") :imagery
                        :else :unknown)
            event-key (keyword (str (name base-type) "_event"))
            event-data (get data event-key)
            events (:values event-data)
            non-zero-indices (vec (argops/argfilter #(= 1.0 %) events))
            original-tensor (:values tensor-data)
            _ (println "Trials tensor:" original-tensor)
            [num-channels num-samples] (tensor-dims/shape (tensor/tensor->dimensions original-tensor))
            trial-length (* 4 sig/SRATE)
            num-trials (count non-zero-indices)
            trials-tensor (tensor/new-tensor [num-trials num-channels trial-length])]
        
        (doseq [[trial-idx event-idx] (map-indexed vector non-zero-indices)]
          (let [start-idx (max 0 (- event-idx (* 2 sig/SRATE)))
                end-idx (min num-samples (+ start-idx trial-length))]
            (when (and (< start-idx end-idx)
                       (<= end-idx num-samples))
              (doseq [chan-idx (range num-channels)]
                (doseq [i (range (min trial-length (- end-idx start-idx)))]
                  (let [src-idx (+ start-idx i)]
                    (when (< src-idx num-samples)
                      (tensor/mset! trials-tensor 
                                   trial-idx 
                                   chan-idx 
                                   i 
                                   (tensor/mget original-tensor chan-idx src-idx)))))))))
        (println "Created trials tensor shape:" (tensor-dims/shape (tensor/tensor->dimensions trials-tensor)))
        trials-tensor))))

^:kindly/hide-code
(defn calculate-badness-scores
  [trials]
  (let [time-window bag-voltage-frame-segment
        band :bad-trial
        start-sample (int (* (/ (first time-window) 1000) sig/SRATE))
        end-sample (int (* (/ (second time-window) 1000) sig/SRATE))]
    (println "Starting badness score calculation. Trial count:" (count trials))
    (println "Time window [ms]:" time-window)
    (println "Sample window [samples]:" start-sample "-" end-sample)
    (println "Bad trial threshold:" sig/BAD-TRIAL-THRESHOLD "μV")
    (doall
     (map-indexed
      (fn [trial-idx trial-data]
        (when (zero? (mod trial-idx 10))
          (println "Processing trial" trial-idx))
        (let [channels-analysis
              (map-indexed
               (fn [ch-idx channel]
                 (when (and (>= ch-idx 0) (< ch-idx 64))
                   (try
                     (let [channel-mean (/ (reduce + channel) (count channel))
                           channel-centered (map #(- % channel-mean) channel)
                           scaled (vec (map #(/ % 100.0) channel-centered))

                           band-pass-data (sig/band-pass-butterworth-filter scaled band)
                           window-data (subvec band-pass-data start-sample end-sample)
                           max-val (apply max window-data)
                           min-val (apply min window-data)
                           abs-max (max (Math/abs max-val) (Math/abs min-val))
                           exceeds-threshold? (or (> max-val sig/BAD-TRIAL-THRESHOLD)
                                                  (< min-val (- sig/BAD-TRIAL-THRESHOLD)))]
                       {:channel-index ch-idx
                        :is-bad exceeds-threshold?
                        :max-amplitude abs-max
                        :threshold-ratio (/ abs-max sig/BAD-TRIAL-THRESHOLD)
                        :max-value max-val
                        :min-value min-val})
                     (catch Exception e
                       (println "Error processing trial" trial-idx "channel" ch-idx ":" (.getMessage e))
                       {:channel-index ch-idx
                        :is-bad false
                        :error (.getMessage e)}))))
               trial-data)
              bad-channels (filter :is-bad channels-analysis)

              worst-channel (when (seq bad-channels)
                              (apply max-key :threshold-ratio bad-channels))

              overall-badness (if (seq bad-channels)
                                (:threshold-ratio worst-channel)
                                0.0)
              trial-result {:trial-index trial-idx
                            :is-bad (not (empty? bad-channels))
                            :badness-score (min 1.0 overall-badness)
                            :bad-channels (map :channel-index bad-channels)
                            :worst-channel-info worst-channel
                            :channels-analysis channels-analysis}]
          (when (and (< trial-idx 3) (or (:is-bad trial-result) (zero? (mod trial-idx 10))))
            (println "Trial" trial-idx "→" (select-keys trial-result [:is-bad :badness-score :bad-channels])))
          trial-result))
      trials))))

^:kindly/hide-code
(defn get-color-for-badness
  "Returns a color string based on badness score
   0.0 = Green (good)
   0.0-1.0 = Yellow to Red gradient (getting worse)
   1.0+ = Deep Red (very bad)"
  [badness-score]
  (cond
    (nil? badness-score) "rgba(128, 128, 128, 0.8)"
    (zero? badness-score) "rgba(0, 180, 0, 0.8)"
    (>= badness-score 1.0) "rgba(180, 0, 0, 0.9)"
    :else
    (let [red-component 180
          green-component (int (* 180 (- 1 badness-score)))]
      (format "rgba(%d, %d, 0, 0.8)" red-component green-component))))

^:kindly/hide-code
(defn detect-bad-trials
  "Detect bad trials using tensor operations"
  [data-atom trial-type]
  (println "Processing" (name trial-type) "trials...")
  (let [trials-tensor (create-trials data-atom trial-type)]

    (if (or (nil? trials-tensor)
            (zero? (first (tensor-dims/shape (tensor/tensor->dimensions trials-tensor)))))
      (do
        (println "No trials found or created for" (name trial-type))
        (swap! data-atom assoc-in [:bad-trials trial-type] #{})
        #{})

      (let [bad-trials-analysis (calculate-badness-scores trials-tensor)
            bad-trial-indices (set (map :trial-index (filter :is-bad bad-trials-analysis)))]

        (println "Found" (count bad-trial-indices) "bad trials out of" (count bad-trials-analysis))

        (swap! data-atom assoc-in [:bad-trials trial-type] bad-trial-indices)
        (swap! data-atom assoc-in [:bad-trials-analysis trial-type] bad-trials-analysis)

        (swap! data-atom update-in [:bad_trial_indices :bad_trial_idx_voltage]
               (fn [current]
                 (let [idx-by-type (case trial-type
                                     :filtered_movement_left 0
                                     :filtered_movement_right 1
                                     :filtered_imagery_left 0
                                     :filtered_imagery_right 1
                                     0)
                       row-idx (case trial-type
                                 (:filtered_movement_left :filtered_movement_right) 0
                                 (:filtered_imagery_left :filtered_imagery_right) 1
                                 0)

                       default [[{:dims [0 0] :elements 0 :values (tensor/->tensor [])}
                                 {:dims [0 0] :elements 0 :values (tensor/->tensor [])}]]

                       current-matrix (if (and (vector? current)
                                               (= 2 (count current))
                                               (every? vector? current))
                                        current
                                        default)

                       indices-tensor {:dims [(count bad-trial-indices) 1]
                                       :elements (count bad-trial-indices)
                                       :values (tensor/->tensor (vec bad-trial-indices))}]
                   (assoc-in current-matrix [row-idx idx-by-type] indices-tensor))))
        bad-trial-indices))))

^:kindly/hide-code
(defn obtain-all-bad-sample-data
  "Process all trial types for bad trial detection using tensor operations"
  [data-atom]
  (println "Processing all trial types for bad trial detection...")

  (let [trial-types sig/filtered-keys
        results (for [trial-type trial-types]
                  (let [bad-trials (detect-bad-trials data-atom trial-type)]
                    {:trial-type trial-type
                     :bad-trials bad-trials
                     :count (count bad-trials)}))]
    (println "\nBad trial detection summary:")
    (doseq [result results]
      (println (name (:trial-type result)) ":" (:count result) "bad trials"))
    {:results results
     :total-bad-trials (reduce + (map :count results))}))
