(ns bci-project.bad
  (:require [bci-project.bad :as bad]
            [bci-project.signal :as sig]
            [bci-project.trial-analysis :as analysis]))

^:kindly/hide-code
(def bag-voltage-frame-segment
  [500 2500])

^:kindly/hide-code
(defn create-trials
  "Create 7-second trials starting from each movement_event marker (no pre/post buffer)."
  [eeg-data-atom trial-type]
  (let [data @eeg-data-atom
        trial-key (case trial-type
                    :movement_left :movement_left
                    :movement_right :movement_right
                    :imagery_left :imagery_left
                    :imagery_right :imagery_right
                    :movement_left)
        trial-data (get-in data [trial-key :values])
        dims (get-in data [trial-key :dims])
        num-channels (first dims)
        samples-per-channel (second dims)

        trial-length-samples (* 7 sig/SRATE)
        event-key (case trial-type
                    (:movement_left :movement_right) :movement_event
                    (:imagery_left :imagery_right) :imagery_event
                    :movement_event)

        event-data (get data event-key)
        event-times (:non-zero-times (:analysis (analysis/extract-event-times event-data [0 141])))
        event-samples (mapv #(int (* % sig/SRATE)) event-times)

        trials
        (try
          (vec
           (for [event-sample event-samples
                 :when (<= (+ event-sample trial-length-samples) samples-per-channel)]
             (let [start event-sample
                   end (+ event-sample trial-length-samples)]
               (vec
                (for [ch (range num-channels)]
                  (let [channel-data (nth trial-data ch)]
                    (subvec channel-data start end)))))))
          (catch Exception e
            (println "Error creating trials:" (.getMessage e))
            (println "Stack trace:" (with-out-str (.printStackTrace e)))
            []))]

    (swap! eeg-data-atom assoc-in [:eeg-data trial-type :trials] trials)
    (println "Created" (count trials) (name trial-type) "trials.")
    (count trials)))

^:kindly/hide-code
(defn calculate-badness-scores
  [trials time-window]
  (let [band :bad-trial
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
(defn process-and-store-bad-trials
  "Process all trials and store bad trial information in the data atom
   Parameters:
     - data-atom: The atom containing the dataset
     - threshold: Amplitude threshold (μV), defaults to 100
     - time-window: [start-ms end-ms] window to analyze, defaults to [500 2500]"
  ([data-atom trial-type]
   (process-and-store-bad-trials data-atom trial-type bag-voltage-frame-segment))
  ([data-atom data-type time-window]
   (when (empty? (get-in @data-atom [:eeg-data data-type :trials]))
     (println "No trials found. Segmenting data into trials...")
     (let [trial-count (create-trials data-atom data-type)]
       (println "Created" trial-count data-type "trials")))
   (let [dir "resources/bad-trial-info"
         _   (.mkdirs (java.io.File. dir))
         filename (str dir "/bad-trials-" (:sample_number @data-atom) ".edn")
         file (java.io.File. filename)]
     (if (.exists file)
       (let [bad-trials-data (read-string (slurp filename))]
         (println "Loading existing bad trials file, found" (count (:bad-trials bad-trials-data)) "bad trials")
         (swap! data-atom assoc
                :bad-trials (:bad-trials bad-trials-data)
                :bad-trials-analysis (:bad-trials-analysis bad-trials-data))
         (:bad-trials bad-trials-data))

       (let [trials (get-in @data-atom [:eeg-data :trials])
             _ (println "Processing" (count trials) "trials for bad trial detection...")

             bad-trials-analysis (calculate-badness-scores trials time-window)
             _ (println "Completed badness analysis, found"
                        (count (filter :is-bad bad-trials-analysis))
                        "bad trials out of"
                        (count bad-trials-analysis))

             bad-trial-indices (set (map :trial-index (filter :is-bad bad-trials-analysis)))]

         (println "Bad trial indices:" (seq bad-trial-indices))

         (swap! data-atom assoc
                :bad-trials bad-trial-indices
                :bad-trials-analysis bad-trials-analysis)
         (try
           (spit filename (pr-str {:bad-trials bad-trial-indices
                                   :bad-trials-analysis bad-trials-analysis}))
           (println "Saved bad trials to" filename)
           (catch Exception e
             (println "Error saving bad trials to file:" (.getMessage e))))
         bad-trial-indices)))))

^:kindly/hide-code
(defn obtain-all-bad-sample-data
  "Process and store bad trials for all trial types (movement / imagery), and event types (left / right)
   in an individual sample."
  ([data-atom]
   (obtain-all-bad-sample-data data-atom [500 2500]))
  ([data-atom time-window]
   (println "Processing all trial types for bad trial detection...")
   (let [trial-types [:movement_left :movement_right :imagery_left :imagery_right]
         results (for [trial-type trial-types]
                   (do
                     (println "\nProcessing" (name trial-type) "trials...")
                     (let [bad-trials (process-and-store-bad-trials data-atom trial-type time-window)]
                       {:trial-type trial-type
                        :bad-trials bad-trials
                        :count (count bad-trials)})))]

     (println "\nBad trial detection summary:")
     (doseq [result results]
       (println (name (:trial-type result)) ":" (:count result) "bad trials -" (seq (:bad-trials result))))

     (swap! data-atom assoc :all-bad-trials
            (into {} (map (fn [r] [(:trial-type r) (:bad-trials r)]) results)))

     {:results results
      :total-bad-trials (reduce + (map :count results))})))