(ns bci-project.notebook
  (:require [bci-project.brain :as brain]
            [bci-project.components :as ui]
            [bci-project.regions :as regions]
            [bci-project.signal :as sig]
            [scicloj.kindly.v4.kind :as kind])
  (:import java.io.File
           javax.imageio.ImageIO))

(def SRATE 512)

(defn analyze-movement-events
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
        start-sample (int (* start-time SRATE))
        end-sample (min (int (* end-time SRATE))
                        (if (sequential? event-values-flat)
                          (count event-values-flat)
                          0))

        relevant-samples (when (sequential? event-values-flat)
                           (subvec (vec event-values-flat) start-sample end-sample))

        non-zero-indices (when (sequential? relevant-samples)
                           (keep-indexed
                            (fn [idx val] (when (pos? val) idx))
                            relevant-samples))

        non-zero-times (map #(/ (+ start-sample %) SRATE) non-zero-indices)]

    {:structure {:type (type event-data)
                 :keys (when (map? event-data) (keys event-data))}
     :analysis {:total-samples (if (sequential? relevant-samples)
                                 (count relevant-samples)
                                 0)
                :non-zero-count (count non-zero-indices)
                :non-zero-times (vec non-zero-times)}}))

(defn get-channel-data
  [eeg-values ch-idx time-window]
  (when (and ch-idx (< ch-idx (count eeg-values)))
    (let [channel-data (nth eeg-values ch-idx nil)
          start-sample (int (* (first time-window) SRATE))
          end-sample (int (* (second time-window) SRATE))]
      (when channel-data
        (subvec channel-data
                (min start-sample (count channel-data))
                (min end-sample (count channel-data)))))))

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
             {:time (/ sample-idx SRATE)
              :amplitude (- (nth channel-data sample-idx) mean-value) ;Subtract mean to remove DC offset
              :raw-amplitude (nth channel-data sample-idx)
              :channel (str "Channel " ch-idx)
              :channel-index ch-idx})])))

(defn prepare-events-dataset
  [event-values start-sample end-sample event-type]
  (keep-indexed
   (fn [idx val]
     (when (and (>= idx start-sample)
                (< idx end-sample)
                (pos? val))
       {:time (/ idx SRATE)
        :event-type (name event-type)}))
   (cond
     (sequential? event-values) (if (sequential? (first event-values))
                                  (first event-values)
                                  event-values)
     :else [])))

(defn individual-eeg-chart
  [channel-data events-dataset start-time end-time]
  (kind/vega-lite
   {:width 600
    :height 120
    :layer [{:data {:values events-dataset}
             :mark {:type "rule"
                    :color "red"
                    :strokeWidth 1.5
                    :opacity 0.8}
             :encoding {:x {:field "time"
                            :type "quantitative"
                            :scale {:domain [start-time end-time]}}}}
            {:data {:values channel-data}
             :mark {:type "line", :color "steelblue"}
             :encoding {:x {:field "time"
                            :type "quantitative"
                            :axis {:title "Time (s)"}
                            :scale {:domain [start-time end-time]}}
                        :y {:field "amplitude"
                            :type "quantitative"
                            :axis {:title "Amplitude (μV)"}
                            :scale {:zero false}}
                        :tooltip [{:field "channel", :type "nominal"}
                                  {:field "time", :type "quantitative", :format ".2f"}
                                  {:field "amplitude", :title "Amplitude", :type "quantitative"}]}}]
    :selection {:grid {:type "interval", :bind "scales"}}}))

(defn filter-single-channel
  [data ch-idx time-window frequency-band eeg-key]
  (let [eeg-data (eeg-key @data)
        eeg-values (:values eeg-data)

        filter-padding 1
        filter-time-window [(- (first time-window) filter-padding)
                            (+ (second time-window) filter-padding)]

        [low-freq high-freq] (sig/frequency-range frequency-band)

        channel-data (get-channel-data eeg-values ch-idx filter-time-window)
        filtered (sig/filter-by-frequency-band (vec channel-data) frequency-band SRATE)
        padding-samples (int (* filter-padding SRATE))
        display-filtered (subvec filtered
                                 padding-samples
                                 (- (count filtered) padding-samples))
        dataset (mapv (fn [i]
                        {:time (+ (first time-window) (/ i SRATE))
                         :amplitude (nth display-filtered i)
                         :channel-index ch-idx})
                      (range (count display-filtered)))]
    [:div.filtered-chart
     (kind/vega-lite
      {:width 600
       :height 120
       :mark {:type "line", :color "orange"}
       :data {:values dataset}
       :encoding {:x {:field "time"
                      :type "quantitative"
                      :axis {:title "Time (s)"}
                      :scale {:domain time-window}}
                  :y {:field "amplitude"
                      :type "quantitative"
                      :axis {:title "Amplitude (μV)"}
                      :scale {:zero false}}
                  :tooltip [{:field "channel-index", :title "Channel", :type "nominal"}
                            {:field "time", :type "quantitative", :format ".2f"}
                            {:field "amplitude", :type "quantitative"}]}
       :selection {:grid {:type "interval", :bind "scales"}}})]))

(defn eeg-visualization
  [data channel-indices time-window frequency-band event-type eeg-key]
  (try
    (let [eeg-data (eeg-key @data)
          eeg-values (:values eeg-data)

          events (event-type @data)
          event-values (:values events)

          start-time (first time-window)
          end-time (second time-window)
          start-sample (int (* start-time SRATE))
          end-sample (min (int (* end-time SRATE)) (second (:dims eeg-data)))

          channels-data (prepare-eeg-dataset eeg-values channel-indices start-sample end-sample)
          non-zero-events (prepare-events-dataset event-values start-sample end-sample event-type)

          sorted-channels (sort channel-indices)]
      (kind/hiccup
       [:div.eeg-visualization
        [:h3 (str "EEG Channel Data - " (name eeg-key))]
        [:div.charts-container {:style "display: flex; flex-wrap: wrap;"}
         [:div.charts-column {:style "flex: 3;"}
          (for [ch-idx sorted-channels
                :when (contains? channels-data ch-idx)
                :let [position-label (get-in regions/channel-brain-regions [ch-idx :position-name] "Unknown")
                      image-label (get-in regions/channel-brain-regions [ch-idx :path-endpoint] "Unknown")]]
            [:div.channel-chart {:style "margin-bottom: 10px;"}
             [:div.chart-with-label {:style "display: flex; align-items: center;"}
              [:div.channel-label {:style "width: 120px; text-align: right; padding-right: 15px; font-weight: bold; padding-bottom: 30px;"}
               (str "Channel: " (inc ch-idx))
               [:br]
               [:span {:style "font-size: 0.9em; align: top;"}
                position-label]]
              [:div.charts-with-picture {:style "flex-grow: 1;"}
               [:div.raw-and-filtered {:style "display: flex; flex-direction: column;"}
                (individual-eeg-chart (get channels-data ch-idx) non-zero-events start-time end-time)
                (filter-single-channel data ch-idx time-window frequency-band eeg-key)]]
              (when (or (= eeg-key :imagery_left) (= eeg-key :movement_left))
                [:div.head-model {:style {:display "flex" :width "175px" :height "175px"}}
                 (kind/image
                  (ImageIO/read (File. (str "resources/images/channel-labels" image-label ".png"))))])]])]]]))
    (catch Exception e
      (println "Error in visualization:" (.getMessage e))
      (.printStackTrace e)
      [:div
       [:h2 "Error Visualizing EEG Data"]
       [:p "An error occurred: " (.getMessage e)]])))

(defn generate-chart
  [data-atom channel-indices time-window event-type side frequency-band]
  (let [event-key (keyword (str (name event-type) "_event"))
        eeg-key (keyword (str (name event-type) "_" (name side)))]
      (eeg-visualization data-atom channel-indices time-window frequency-band event-key eeg-key)))

(defn build-category-section
  [data-atom channel-indices time-window event-type frequency-band]
  (let [event-key (keyword (str (name event-type) "_event"))
        analysis (analyze-movement-events (event-key @data-atom) time-window)
        events-count (get-in analysis [:analysis :non-zero-count])
        event-name (clojure.string/capitalize (name event-type))]
    [:div.event-section.viz-container
     [:h2 (str event-name " Events")]
     [:p "Total " (name event-type) " events in selected time window: " events-count]

     [:div {:style {:display "flex"}}
      [:div
       [:h3 "Left Side"]
       [:div {:style {:display "flex"}}
        [:div (generate-chart data-atom channel-indices time-window event-type :left frequency-band)]]]

      [:div [:h3 "Right Side"]
       [:div {:style {:display "flex"}}
        [:div (generate-chart data-atom channel-indices time-window event-type :right frequency-band)]]]]]))

(defn movement-and-imagery-visualized
  [data-atom channel-indices time-window frequency-band]
  [:div.raw-data-section.viz-container
   (build-category-section data-atom channel-indices time-window :movement frequency-band)
   (build-category-section data-atom channel-indices time-window :imagery frequency-band)])

(defn comprehensive-eeg-analysis
  ([data-atom]
   (comprehensive-eeg-analysis data-atom [0 10] [6 13 14 48 49 50 60 63] :alpha))
  ([data-atom time-window channel-indices]
   (comprehensive-eeg-analysis data-atom time-window channel-indices :alpha))
  ([data-atom time-window channel-indices frequency-band]
   (let [id (str "eeg-viz-" (System/currentTimeMillis))
         sample-number (:sample_number @data-atom)]
     (kind/hiccup
      [:div {:id id :style {:text-align "left"
                            :margin 0
                            :max-width "100vw"}}
       (ui/notebook-heading sample-number)
       [:h2 "Comprehensive EEG Data Analysis"]
       (movement-and-imagery-visualized data-atom channel-indices time-window frequency-band)]))))

(defn -main []
  (brain/load-eeg-data! "resources/data/s01.mat")
  #_(comprehensive-eeg-analysis brain/eeg-data-atom))

(comment
  (comprehensive-eeg-analysis brain/eeg-data-atom [99 107] [12 49]))