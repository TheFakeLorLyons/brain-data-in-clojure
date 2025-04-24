^:kindly/hide-code
(ns bci-project.notebook
  (:require [bci-project.trial-analysis :as analysis]
            [bci-project.brain :as brain]
            [bci-project.bad :as bad]
            [bci-project.components :as ui]
            [scicloj.kindly.v4.kind :as kind])
  (:import java.io.File
           javax.imageio.ImageIO))

^:kindly/hide-code
(defn generate-chart
  "Creates the above EEG charts for four categors: left/right data, and movement/event trials 
   (e.g., movement right vs imagery left)"
  [data-atom channel-indices time-window event-type side frequency-band]
  (let [event-key (keyword (str (name event-type) "_event"))
        eeg-key (keyword (str (name event-type) "_" (name side)))]
    (ui/eeg-templates data-atom channel-indices time-window frequency-band event-key eeg-key)))

^:kindly/hide-code
(defn build-chart-category-section
  "Container for categorized charts, and contains the event information (how many events occurred in the time window)"
  [data-atom channel-indices time-window event-type frequency-band]
  (let [event-key (keyword (str (name event-type) "_event"))
        analysis (analysis/extract-event-times (event-key @data-atom) time-window)
        events-count (get-in analysis [:analysis :non-zero-count])
        event-name (clojure.string/capitalize (name event-type))]
    [:div.event-section.viz-container
     [:h2 {:style {:padding-left "300px"}}
      (str event-name " Events")]
     [:p {:style {:padding-left "450px"}}
      "Total " (name event-type) " events in selected time window: " events-count]

     [:div {:style {:display "flex"}}
      [:div
       [:h3 {:style {:font-weight "bold"}}
        "Left Side"]
       [:div {:style {:display "flex"}}
        [:div (generate-chart data-atom channel-indices time-window event-type :left frequency-band)]]]

      [:div
       [:h3 {:style {:font-weight "bold"}}
        "Right Side"]
       [:div {:style {:display "flex"}}
        [:div (generate-chart data-atom channel-indices time-window event-type :right frequency-band)]]]]]))

^:kindly/hide-code
(defn movement-and-imagery-visualized
  "There are two sections of the data: movement trials, and imagery trials. They are arranged on the page
   in that order, with the channels corresponding to the left side of the body displayed on the left hand
   side of the page, and the right on the righthand side."
  [data-atom channel-indices time-window frequency-band]
  [:div.raw-data-section.viz-container
   (build-chart-category-section data-atom channel-indices time-window :movement frequency-band)
   (build-chart-category-section data-atom channel-indices time-window :imagery frequency-band)])

^:kindly/hide-code
(defn comprehensive-eeg-analysis
  "Container for the entire analysis and heading. It is multi arity, allowing 2 alternative calling
   conventions
      Parameters:
     - data-atom:       An atom using brain/load-eeg-data! to fill itself with matlab data.
     - time-window:     A 2 element vector with a start and end time, 'defaults' to 0-10 seconds.
     - channel-indices: Zero based, and takes up to 68 channels, displaying the above information
                        for every channel specified. Defaults to [6 13 14 48 49 50 60 63]
     - frequency band:  Specify what frequencies you would like visualized (:alpha, :beta, :theta, :delta, :gamma)
                        Defaults to :alpha"
  ([data-atom]
   (comprehensive-eeg-analysis data-atom [0 10] [6 13 14 48 49 50 60 63] :alpha))
  ([data-atom time-window channel-indices]
   (comprehensive-eeg-analysis data-atom time-window channel-indices :alpha))
  ([data-atom time-window channel-indices frequency-band]
   (let [id (str "eeg-viz-" (System/currentTimeMillis))
         sample-number (:sample_number @data-atom)]
     (kind/hiccup
      [:div.column-screen {:id id
                           :style {:display "flex"
                                   :flex-direction "column"
                                   :max-width "100vw"}}
       (ui/notebook-heading sample-number)
       [:h2 {:style {:text-align "center"}}
        "Comprehensive EEG Data Analysis"]
       [:div.chart-container {:style {:display "flex"
                                      :flex-direction "column"
                                      :align-self "center"
                                      :justify-content "center"}}
        (movement-and-imagery-visualized data-atom channel-indices time-window frequency-band)]]))))

^:kindly/hide-code
(defn -main []
  (brain/initial-data-preparation "resources/data/s13.mat")
  (bad/obtain-all-bad-sample-data brain/eeg-data-atom)
  (comprehensive-eeg-analysis brain/eeg-data-atom))

^:kindly/hide-code
#_(comprehensive-eeg-analysis brain/eeg-data-atom [99 107] [12 33 49])

^:kindly/hide-code
(comment
  (comprehensive-eeg-analysis brain/eeg-data-atom [99 107] [12 33 49]))