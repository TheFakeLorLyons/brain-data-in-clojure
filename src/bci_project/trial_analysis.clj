(ns bci-project.trial-analysis
  (:require [bci-project.signal :as sig]
            [tech.v3.tensor :as tensor]
            [tech.v3.datatype :as dt]
            [scicloj.kindly.v4.kind :as kind]))

;=================================================================================================
                                                         ;         Event Timing Extraction       ;
;=================================================================================================

(defn create-event-marker
  "Creates a [channels x samples] binary tensor with 1s at event time indices."
  [event-data num-channels num-samples]
  (let [event-indices (extract-event-indices event-data)
        event-index-set (set event-indices)]
    (tensor/compute-tensor [num-channels num-samples]
                           (fn [_ j]
                             (if (event-index-set j) 1 0))
                           :int8)))

(defn add-event-markers!
  "Add event marker tensors into the data map."
  [eeg-data-atom]
  (let [data @eeg-data-atom
        all-event-types [:movement_event :imagery_event]
        updated-data
        (reduce (fn [acc event-type]
                  (let [event-data (get acc event-type)
                        dims (:dims (get acc :movement_left))
                        [channels samples] dims
                        marker-tensor (create-event-marker event-data channels samples)]
                    (assoc-in acc [:event-marker-tensors event-type] marker-tensor)))
                data
                all-event-types)]
    (reset! eeg-data-atom updated-data)))

;=================================================================================================
                                                         ;  Accessing the EEG-Data from the atom ;
;=================================================================================================

(defn get-channel-data
  "Extract a single channel's data from the EEG tensor
   Parameters:
     - eeg-tensor: The tensor containing EEG data
     - ch-idx: Channel index to extract
     - time-window: [start-time end-time] in seconds"
  [eeg-tensor ch-idx time-window]
  (let [start-sample (int (* (first time-window) sig/SRATE))
        end-sample (int (* (second time-window) sig/SRATE))
        samples-count (- end-sample start-sample)]
    (mapv (fn [i]
            (let [sample-idx (+ start-sample i)]
              (tensor/select eeg-tensor ch-idx sample-idx)))
          (range samples-count))))

(defn extract-channel-data
  "Extract a specific channel's data from a tensor."
  [tensor channel-idx]
  (let [channel-data (tensor/select tensor channel-idx :all)
        channel-vector (dt/->vector channel-data)]
    channel-vector))

(defn get-channel-data-tensor
  "Extract data for a specific channel from tensor with time window constraints.
   The tensor is expected to be in [channels samples] format."
  [tensor-values ch-idx time-window]
  (let [start-sample (int (* (first time-window) sig/SRATE))
        end-sample (min (int (* (second time-window) sig/SRATE))
                        (second (dt/shape tensor-values)))
        channel-data (tensor/select tensor-values ch-idx (range start-sample end-sample))
        channel-vector (dt/->vector channel-data)]
    channel-vector))

(defn prepare-eeg-dataset
  "Convert EEG tensor data for multiple channels into a format suitable for visualization,
   subtracting per-channel mean (DC offset removal)."
  [tensor-data channel-indices start-sample end-sample]
  (let [time-samples (- end-sample start-sample)]
    (reduce
     (fn [acc ch-idx]
       (let [channel-slice (tensor/select tensor-data ch-idx (range start-sample end-sample))
             channel-data (tensor/->jvm channel-slice)
             mean-value (/ (reduce + channel-data) (count channel-data))
             adjusted-data (map #(- % mean-value) channel-data) ;Subtract mean to remove DC offset
             time-points (map #(/ (+ % start-sample) sig/SRATE) (range time-samples))
             dataset (map (fn [time amp]
                            {:time time
                             :amplitude amp
                             :channel (str ch-idx)})
                          time-points
                          adjusted-data)]
         (assoc acc ch-idx dataset)))
     {}
     channel-indices)))

(defn extract-event-indices
  "Extract event indices from the event data, handling various possible formats.
   Returns a vector of indices where events occur."
  [event-data]
  (let [values (:values event-data)]
    (vec (keep-indexed (fn [idx val-group]
                         (when (some (fn [v] (and (number? v) (not (zero? v)))) val-group)
                           idx))
                       values))
    (sequential? values)))

(defn prepare-events-dataset
  "Convert events data to format suitable for visualization.
   Handle multiple possible data structures."
  [event-data start-sample end-sample event-type]
  (try
    (let [event-indices (if (map? event-data)
                          (extract-event-indices event-data)

                          (if (sequential? event-data)
                            (if (every? number? event-data)
                              (vec (filter (fn [idx] (and (>= idx start-sample) (< idx end-sample))) event-data))
                              (vec (keep-indexed (fn [idx val]
                                                   (when (and (number? val)
                                                              (not (zero? val))
                                                              (>= idx start-sample)
                                                              (< idx end-sample))
                                                     idx))
                                                 event-data)))
                            []))
          relevant-indices (filter #(and (>= % start-sample) (< % end-sample)) event-indices)
          event-times (mapv (fn [idx]
                              {:time (/ idx sig/SRATE)
                               :event-type (name event-type)
                               :value 1})
                            relevant-indices)]
      event-times)
    (catch Exception e
      (println "Error preparing events dataset:" (.getMessage e))
      (.printStackTrace e)
      [])))