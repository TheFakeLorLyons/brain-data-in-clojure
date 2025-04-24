(ns bci-project.components
  (:require [bci-project.bad :as bad]
            [bci-project.trial-analysis :as analysis]
            [bci-project.regions :as regions]
            [bci-project.signal :as sig]
            [scicloj.kindly.v4.kind :as kind])
  (:import java.io.File
           javax.imageio.ImageIO))

(def dims-image
  (ImageIO/read (File. "resources/images/dims.png")))

(defn notebook-heading
  "Build the header section with metadata and information"
  [sample-number]
    [:div.notebook-heading {:style {:text-align "center"}}
     [:h1 "EEG Data Analysis"]
     [:h3 "Sample Number: " sample-number]
     [:p "This analysis is intended to recreate data from the following study: "
      [:a {:href "https://gigadb.org/dataset/view/id/100295/" :target "_blank"} "GigaDB Dataset"]]
     [:p "The data was taken via a Biosemi ActiveTwo [BCI] system."]
     [:p "The data was initially matlab data that I have converted to edn."]
     
     [:br]
     [:p {:style {:font-weight "bold" :padding-right "550px"}} "The positioning of the sensors: "]
     [:div
      (kind/image dims-image)]
     [:p "More information on sensor placement can be found in this article: "
      [:a {:href "https://www.acns.org/UserFiles/file/Guideline2-GuidelinesforStandardElectrodePositionNomenclature_v1.pdf" :target "_blank"}
       "10-10 system article."]]])

;=================================================================================================
                                                         ;                 Charts                ;
;=================================================================================================

^:kindly/hide-code
(defn raw-eeg-timeseries
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
            {:title {:text "Raw EEG Data"}
             :data {:values channel-data}
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

(defn avg-ref-eeg-timeseries
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
            {:title {:text "Common Average Reference"}
             :data {:values channel-data}
             :mark {:type "line", :color "purple"}
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

^:kindly/hide-code
(defn filter-single-channel
  [data ch-idx time-window frequency-band eeg-key]
  (let [eeg-data (eeg-key @data)
        eeg-values (:values eeg-data)

        filter-padding 1
        filter-time-window [(- (first time-window) filter-padding)
                            (+ (second time-window) filter-padding)]

        channel-data (analysis/get-channel-data eeg-values ch-idx filter-time-window)
        
        high-pass-data (sig/high-pass-butterworth-filter (vec channel-data))
        band-pass-data (sig/band-pass-butterworth-filter high-pass-data frequency-band)

        padding-samples (int (* filter-padding sig/SRATE))
        display-filtered (subvec band-pass-data
                                 padding-samples
                                 (- (count band-pass-data) padding-samples))
        dataset (mapv (fn [i]
                        {:time (+ (first time-window) (/ i sig/SRATE))
                         :amplitude (nth display-filtered i)
                         :channel-index ch-idx})
                      (range (count display-filtered)))]
    [:div.filtered-chart
     (kind/vega-lite
      {:title {:text "Filtered Raw Signal"}
       :width 600
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

^:kindly/hide-code
(defn filter-avg-ref-channel
  "The average common reference of an individual sensor, "
  [data ch-idx time-window frequency-band eeg-key]
  (let [avg-ref-key (case eeg-key
                      :movement_left :avg_ref_mvmt_left
                      :movement_right :avg_ref_mvmt_right
                      :imagery_left :avg_ref_img_left
                      :imagery_right :avg_ref_img_right
                      nil)

        avg-ref-data (avg-ref-key @data)

        avg-ref-values (:values avg-ref-data)
        filter-padding 1
        filter-time-window [(- (first time-window) filter-padding)
                            (+ (second time-window) filter-padding)]
        channel-data (analysis/get-channel-data avg-ref-values ch-idx filter-time-window)

        high-pass-data (sig/high-pass-butterworth-filter (vec channel-data))
        band-pass-data (sig/band-pass-butterworth-filter high-pass-data frequency-band)
        
        padding-samples (int (* filter-padding sig/SRATE))
        display-filtered (subvec band-pass-data
                                 padding-samples
                                 (- (count band-pass-data) padding-samples))
        dataset (mapv (fn [i]
                        {:time (+ (first time-window) (/ i sig/SRATE))
                         :amplitude (nth display-filtered i)
                         :channel-index ch-idx})
                      (range (count display-filtered)))]
    [:div.filtered-chart
     (kind/vega-lite
      {:title {:text "Filtered Average Reference"}
       :width 600
       :height 120
       :mark {:type "line", :color "green"}
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

(defn trial-badness-chart
  "Creates a chart showing the badness analysis for a single channel
   Parameters:
     - channel-data: Raw EEG data for this channel
     - channel-analysis: The analysis data for this channel
     - threshold: Amplitude threshold (μV)
     - time-window: [start-ms end-ms] window to analyze"
  [channel-data channel-analysis time-window]
  (let [band :bad-trial
        start-sample (int (* (/ (first time-window) 1000) sig/SRATE))
        end-sample (int (* (/ (second time-window) 1000) sig/SRATE))

        high-pass-data (sig/high-pass-butterworth-filter (vec channel-data))
        band-pass-data (sig/band-pass-butterworth-filter high-pass-data :bad-trial)
        
        window-data (subvec band-pass-data start-sample end-sample)
        time-points (map #(/ % sig/SRATE) (range start-sample end-sample))

        chart-data (map (fn [t v] {:time t :amplitude v}) time-points window-data)

        is-bad (:is-bad channel-analysis)
        threshold-ratio (:threshold-ratio channel-analysis)
        status-color (bad/get-color-for-badness threshold-ratio)
        status-text (if is-bad "BAD TRIAL" "GOOD TRIAL")
        status-text-color (if is-bad :red :green)]
    (kind/vega-lite
     {:width 600
      :height 120
      :title {:text (str "Bad Trial Detection (8-30 Hz) - " status-text)
              :color status-text-color}
      :data {:values chart-data}
      :layer [{:mark {:type "rule"
                      :color "black"
                      :strokeDash [5, 5]
                      :strokeWidth 1}
               :encoding {:y {:datum sig/BAD-TRIAL-THRESHOLD}}}
              {:mark {:type "rule"
                      :color "black"
                      :strokeDash [5, 5]
                      :strokeWidth 1}
               :encoding {:y {:datum (- sig/BAD-TRIAL-THRESHOLD)}}}
              {:mark {:type "line", :color status-color}
               :encoding {:x {:field "time"
                              :type "quantitative"
                              :axis {:title "Time (s)"}}
                          :y {:field "amplitude"
                              :type "quantitative"
                              :axis {:title "Amplitude (μV)"}
                              :scale {:domain [(* -1.5 sig/BAD-TRIAL-THRESHOLD) (* 1.5 sig/BAD-TRIAL-THRESHOLD)]}}
                          :tooltip [{:field "time", :type "quantitative", :format ".2f"}
                                    {:field "amplitude", :title "Amplitude", :type "quantitative"}]}}]
      :selection {:grid {:type "interval", :bind "scales"}}})))

;=================================================================================================
                                                         ;                Template               ;
;=================================================================================================

^:kindly/hide-code
(defn eeg-templates
  "Individual charts for each sensor in a particular time window of one trial."
  [data channel-indices time-window frequency-band event-type eeg-key & {:keys [trial-index] :or {trial-index 0}}]
  (try
    (let [eeg-data (eeg-key @data)
          eeg-values (:values eeg-data)

          avg-ref-key (case eeg-key
                        :movement_left :avg_ref_mvmt_left
                        :movement_right :avg_ref_mvmt_right
                        :imagery_left :avg_ref_img_left
                        :imagery_right :avg_ref_img_right
                        nil)

          avg-ref-data (when avg-ref-key (avg-ref-key @data))
          avg-ref-values (when avg-ref-data (:values avg-ref-data))

          events (event-type @data)
          event-values (:values events)

          start-time (first time-window)
          end-time (second time-window)
          start-sample (int (* start-time sig/SRATE))
          end-sample (min (int (* end-time sig/SRATE)) (second (:dims eeg-data)))

          channels-data (analysis/prepare-eeg-dataset eeg-values channel-indices start-sample end-sample)
          avg-ref-channels-data (when avg-ref-values
                                  (analysis/prepare-eeg-dataset avg-ref-values channel-indices start-sample end-sample))
          non-zero-events (analysis/prepare-events-dataset event-values start-sample end-sample event-type)
          
          bad-trials-analysis (:bad-trials-analysis @data)
          current-trial-analysis (when bad-trials-analysis
                                   (first (filter #(= (:trial-index %) trial-index) bad-trials-analysis)))
          sorted-channels (sort channel-indices)]
      (kind/hiccup
       [:div.eeg-visualization
        [:h3 (str "EEG Channel Data - " (name eeg-key))]
        [:div.charts-container {:style "display: flex; flex-wrap: wrap;"}
         [:div.charts-column {:style "flex: 3;"}
          (for [ch-idx sorted-channels
                :when (contains? channels-data ch-idx)
                :let [position-label (get-in regions/channel-brain-regions [ch-idx :position-name] "Unknown")
                      image-label (get-in regions/channel-brain-regions [ch-idx :path-endpoint] "Unknown")
                      channel-analysis (when current-trial-analysis
                                         (first (filter #(= (:channel-index %) ch-idx)
                                                        (:channels-analysis current-trial-analysis))))
                      channel-is-bad (when channel-analysis (:is-bad channel-analysis))]]
            [:div.channel-chart {:style "margin-bottom: 10px;"}
             [:div.chart-with-label {:style "display: flex; align-items: center;"}
              [:div.channel-label {:style "width: 120px; text-align: right; padding-right: 15px; font-weight: bold; padding-bottom: 30px;"}
               (str "Channel: " (inc ch-idx))
               [:br]
               [:span {:style "font-size: 0.9em; align: top;"}
                position-label]
               (if channel-is-bad
                 [:div {:style "color: red; font-weight: bold; margin-top: 5px;"}
                  "⚠️ BAD"]
                 [:div {:style "color: green; font-weight: bold; margin-top: 5px;"}
                  "✅ GOOD"])]
              [:div.charts-with-picture {:style "flex-grow: 1;"}
               [:div.raw-and-filtered {:style "display: flex; flex-direction: column;"}
                (raw-eeg-timeseries (get channels-data ch-idx) non-zero-events start-time end-time)
                (filter-single-channel data ch-idx time-window frequency-band eeg-key)
                (avg-ref-eeg-timeseries (get avg-ref-channels-data ch-idx) non-zero-events start-time end-time)
                (filter-avg-ref-channel data ch-idx time-window frequency-band eeg-key)
                #_(when channel-analysis
                  (trial-badness-chart (get channels-data ch-idx)
                                       channel-analysis
                                       sig/BAD-TRIAL-THRESHOLD
                                       time-window))
                (when (not (= ch-idx (last channel-indices)))
                  [:p {:style {:align-self "center"}} "----------------------------------------------"])]]
              (when (or (= eeg-key :imagery_left) (= eeg-key :movement_left))
                [:div.head-model {:style {:display "flex" :width "190px" :height "190px"}}
                 (kind/image
                  (ImageIO/read (File. (str "resources/images/channel-labels" image-label ".png"))))])]])]]]))
    (catch Exception e
      (println "Error in visualization:" (.getMessage e))
      (.printStackTrace e)
      [:div
       [:h2 "Error Visualizing EEG Data"]
       [:p "An error occurred: " (.getMessage e)]])))