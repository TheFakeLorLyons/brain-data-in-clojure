(ns bci-project.trial-analysis
  (:require [bci-project.signal :as sig]
            [scicloj.kindly.v4.kind :as kind]))

;=================================================================================================
                                                         ;         Event Timing Extraction       ;
;=================================================================================================

^:kindly/hide-code
(defn extract-event-times
  "Event marker and frequency information."
  [event-data time-range]
  (let [event-values (if (map? event-data)
                       (:values event-data)
                       event-data)

        event-values-flat (if (and (sequential? event-values)
                                   (= 1 (count event-values))
                                   (sequential? (first event-values)))
                            (first event-values)
                            event-values)

        start-time (first time-range)
        end-time (second time-range)
        start-sample (int (* start-time sig/SRATE))
        end-sample (min (int (* end-time sig/SRATE))
                        (if (sequential? event-values-flat)
                          (count event-values-flat)
                          0))

        relevant-samples (when (sequential? event-values-flat)
                           (subvec (vec event-values-flat) start-sample end-sample))

        non-zero-indices (when (sequential? relevant-samples)
                           (keep-indexed
                            (fn [idx val] (when (pos? val) idx))
                            relevant-samples))

        non-zero-times (map #(/ (+ start-sample %) sig/SRATE) non-zero-indices)]

    {:structure {:type (type event-data)
                 :keys (when (map? event-data) (keys event-data))}
     :analysis {:total-samples (if (sequential? relevant-samples)
                                 (count relevant-samples)
                                 0)
                :non-zero-count (count non-zero-indices)
                :non-zero-times (vec non-zero-times)}}))

;=================================================================================================
                                                         ;  Accessing the EEG-Data from the atom ;
;=================================================================================================

^:kindly/hide-code
(defn get-channel-data
  [eeg-values ch-idx time-window]
  (when (and ch-idx (< ch-idx (count eeg-values)))
    (let [channel-data (nth eeg-values ch-idx nil)
          start-sample (int (* (first time-window) sig/SRATE))
          end-sample (int (* (second time-window) sig/SRATE))]
      (when channel-data
        (subvec channel-data
                (min start-sample (count channel-data))
                (min end-sample (count channel-data)))))))

^:kindly/hide-code
(defn prepare-eeg-dataset
  [eeg-values channel-indices start-sample end-sample]
  (into {}
        (for [ch-idx channel-indices
              :when (and ch-idx (< ch-idx (count eeg-values)))
              :let [channel-data (nth eeg-values ch-idx nil)]
              :when channel-data
              :let [safe-end (min end-sample (count channel-data))
                    safe-start (min start-sample (count channel-data))
                    data-segment (subvec channel-data safe-start safe-end)
                    mean-value (/ (reduce + data-segment) (count data-segment))]]
          [ch-idx
           (for [sample-idx (range safe-start safe-end)]
             {:time (/ sample-idx sig/SRATE)
              :amplitude (- (nth channel-data sample-idx) mean-value) ;Subtract mean to remove DC offset
              :raw-amplitude (nth channel-data sample-idx)
              :channel (str "Channel " ch-idx)
              :channel-index ch-idx})])))

^:kindly/hide-code
(defn prepare-events-dataset
  [event-values start-sample end-sample event-type]
  (keep-indexed
   (fn [idx val]
     (when (and (>= idx start-sample)
                (< idx end-sample)
                (pos? val))
       {:time (/ idx sig/SRATE)
        :event-type (name event-type)}))
   (cond
     (sequential? event-values) (if (sequential? (first event-values))
                                  (first event-values)
                                  event-values)
     :else [])))