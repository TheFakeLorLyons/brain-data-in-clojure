(ns bci-project.notebook
  (:require [bci-project.brain :as brain]
            [scicloj.kindly.v4.kind :as kind])
  (:import java.io.File
          javax.imageio.ImageIO))

(def dims-image
  (ImageIO/read (File. "resources/images/dims.png")))

(defn analyze-movement-events
  "Debug function to analyze movement event data"
  [data-atom time-range]
  (let [data @data-atom
        srate 512
        movement-events (:movement_event data)
        event-values (if (map? movement-events)
                       (:values movement-events)
                       movement-events)

        event-values-flat (if (and (sequential? event-values)
                                   (= 1 (count event-values))
                                   (sequential? (first event-values)))
                            (first event-values)
                            event-values)

        start-time (first time-range)
        end-time (second time-range)
        start-sample (int (* start-time srate))
        end-sample (min (int (* end-time srate))
                        (if (sequential? event-values-flat)
                          (count event-values-flat)
                          0))

        relevant-samples (when (sequential? event-values-flat)
                           (subvec (vec event-values-flat) start-sample end-sample))

        non-zero-indices (when (sequential? relevant-samples)
                           (keep-indexed
                            (fn [idx val] (when (pos? val) idx))
                            relevant-samples))

        non-zero-times (map #(/ (+ start-sample %) srate) non-zero-indices)]

    {:structure {:type (type movement-events)
                 :keys (when (map? movement-events) (keys movement-events))}
     :analysis {:total-samples (if (sequential? relevant-samples)
                                 (count relevant-samples)
                                 0)
                :non-zero-count (count non-zero-indices)
                :non-zero-times (vec non-zero-times)}}))

(defn eeg-visualization
  "A robust approach to visualize EEG with movement events"
  [data-atom channel-indices time-range event-type]
  (try
    (let [data @data-atom
          srate 512

          eeg-data-key (case event-type
                         :movement_event :movement_left
                         :imagery_event :imagery_left
                         event-type)

          eeg-data (eeg-data-key data)
          eeg-dims (:dims eeg-data)
          eeg-values (:values eeg-data)

          [num-channels num-samples] (:dims eeg-data)
          sorted-channel-indices (sort channel-indices)
          
          events (event-type data)
          event-values (:values events)

          start-time (first time-range)
          end-time (second time-range)
          start-sample (int (* start-time srate))
          end-sample (min (int (* end-time srate)) num-samples)

          eeg-dataset (flatten
                       (for [ch-idx sorted-channel-indices
                             :when (< ch-idx (first eeg-dims))
                             :let [channel-data (nth eeg-values ch-idx nil)]
                             :when channel-data
                             sample-idx (range start-sample end-sample)
                             :when (< sample-idx (count channel-data))]
                         {:time (/ sample-idx srate)
                          :amplitude (nth channel-data sample-idx)
                          :channel (str "Channel " ch-idx)
                          :channel-index ch-idx}))
          non-zero-events (keep-indexed
                           (fn [idx val]
                             (when (and (>= idx start-sample)
                                        (< idx end-sample)
                                        (pos? val))
                               {:time (/ idx srate)
                                :event-type (name event-type)}))
                           (if (sequential? event-values)
                             (if (sequential? (first event-values))
                               (first event-values)
                               event-values)
                             (range)))]
      (kind/vega-lite
       {:width 700
        :height 400
        :title (str "EEG Channel Data (" (count channel-indices) " channels)")
        :layer [{:data {:values non-zero-events}
                 :mark {:type "rule"
                        :color "red"
                        :strokeWidth 1.5
                        :opacity 0.8}
                 :encoding {:x {:field "time"
                                :type "quantitative"
                                :scale {:domain [start-time end-time]}}}}

                {:data {:values eeg-dataset}
                 :mark {:type "line"}
                 :encoding {:x {:field "time"
                                :type "quantitative"
                                :axis {:title "Time (s)"}
                                :scale {:domain [start-time end-time]}}
                            :y {:field "amplitude"
                                :type "quantitative"
                                :axis {:title "Amplitude (μV)"}}
                            :color {:field "channel"
                                    :type "nominal"
                                    :sort {:field "channel-index"
                                           :op "min"}}}}]}))
    (catch Exception e
      (println "Error in visualization:" (.getMessage e))
      (.printStackTrace e)
      {kind/hiccup
       [:div
        [:h2 "Error Visualizing EEG Data"]
        [:p "An error occurred: " (.getMessage e)]]})))

(defn comprehensive-eeg-analysis
  "Analyze EEG data with movement events and display complete results"
  [data-atom]
  (let [sample-number (:sample_number @data-atom)
        time-window [83 90]
        channel-indices [6 13 14 48 49 50 60 63]

        movement-analysis (analyze-movement-events data-atom time-window)

        movement-chart (eeg-visualization data-atom channel-indices time-window :movement_event)
        imagery-chart (eeg-visualization brain/eeg-data-atom channel-indices time-window :imagery_event)

        non-zero-times (get-in movement-analysis [:analysis :non-zero-times])
        non-zero-count (get-in movement-analysis [:analysis :non-zero-count])
        total-samples (get-in movement-analysis [:analysis :total-samples])

        formatted-times (map #(format "%.2fs" (double %)) non-zero-times)]
    (kind/hiccup
     [:div
      [:h1 "EEG Data Analysis"]
      [:h3 "Sample Number: " sample-number]
      [:p "This analysis is intended to recreate data from the following
           study: https://gigadb.org/dataset/view/id/100295/"]
      [:p "The data was taken via a Biosemi ActiveTwo [BCI] system."]
      [:p "The data was initially matlab data that I have converted
           to edn."]

      [:p "The positioning of the sensors: "]
      [:div
       (kind/image dims-image)]

      [:div.movement-summary
       [:h2 "Movement Event Analysis Summary"]
       [:ul
        [:li [:strong "Total movement events: "] non-zero-count]
        [:li [:strong "Total samples analyzed: "] total-samples]
        [:li [:strong "Event moments: "]
         [:div {:style "max-height: 100px; overflow-y: auto;"}
          (clojure.string/join ", " formatted-times)]]]]
      [:div
       [:h2 "EEG Channels"]
       [:div movement-chart]]

      [:div.imagery-summary
       [:h2 "Imagery Event Analysis Summary"]
       [:ul
        [:li [:strong "Total imagery events: "] non-zero-count]
        [:li [:strong "Total samples analyzed: "] total-samples]
        [:li [:strong "Event moments: "]
         [:div {:style "max-height: 100px; overflow-y: auto;"}
          (clojure.string/join ", " formatted-times)]]]
       [:div
        [:h2 "EEG Channels"]
        [:div imagery-chart]]]])))

(defn -main []
  (brain/load-eeg-data! "resources/data/s01.mat")
  (comprehensive-eeg-analysis brain/eeg-data-atom))