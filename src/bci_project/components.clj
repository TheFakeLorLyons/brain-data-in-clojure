(ns bci-project.components
  (:require [scicloj.kindly.v4.kind :as kind])
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
      (kind/image dims-image)]])