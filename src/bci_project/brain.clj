(ns bci-project.brain
  (:require [bci-project.signal :as sig]
            [clojure.pprint :refer [pprint]]
            [tech.v3.tensor :as tensor])
  (:import [us.hebi.matlab.mat.format MatStruct Mat5 Mat5File]
           [us.hebi.matlab.mat.types Struct Array Matrix Cell]))

(set! *warn-on-reflection* true)

(declare struct->map cell->vec matrix->vec)

(defn complex
  [real imag]
  {:real real :imag imag})

(defn convert-element
  "MATLAB element to appropriate Clojure data structure"
  [element]
  (cond
    (instance? Struct element) (struct->map element)
    (instance? Cell element) (cell->vec element)
    (instance? Matrix element) (matrix->vec element)
    :else nil))

(defn cell->vec
  "MATLAB cell array to a Clojure vector"
  [^Cell cell]
  (when cell
    (let [dims (.getDimensions cell)
          rows (first dims)
          cols (second dims)]
      (vec (for [^int i (range rows)]
             (vec (for [^int j (range cols)]
                    (let [element (.get cell i j)]
                      (convert-element element)))))))))

(defn array->vec
  "MATLAB array to a Clojure vector"
  [^Array array]
  (when array
    (let [dims (.getDimensions array)
          rows (first dims)
          cols (second dims)]
      (if (and (= 1 rows) (= 1 cols))
        (cond
          (.isLogical array) (.getBoolean array 0)
          (.isComplex array) (complex (.getDouble array 0) (.getImaginaryDouble array 0))
          :else (.getDouble array 0))
        (vec (for [^int i (range rows)]
               (vec (for [^int j (range cols)]
                      (try
                        (if (.isComplex array)
                          
                          (complex (.getDouble array i j) (.getImaginaryDouble array i j))
                          (.getDouble array i j))
                        (catch Exception _ nil))))))))))

(defn matrix->vec
  "MATLAB matrix to a Clojure vector up to 2-dimensions."
  [^Matrix matrix]
  (when matrix
    (let [dims (.getDimensions matrix)
          total-elements (apply * dims)]
      (if (= 1 total-elements)
        (cond
          (.isLogical matrix) (.getBoolean matrix 0)
          (.isComplex matrix) (complex (.getDouble matrix 0) (.getImaginaryDouble matrix 0))
          :else (.getDouble matrix 0))
        (let [metadata {:dims (vec dims)
                        :type (str (.getType matrix))
                        :elements total-elements}
              values (case (count dims)
                       1
                       (vec (for [^int i (range (first dims))]
                              (.getDouble matrix i)))
                       ;; (tensor/compute-tensor dims
                       ;;                        (fn [^long i]
                       ;;                          (.getDouble matrix i))
                       ;;                        :float32)
                       2
                       (vec (for [^int i (range (first dims))]
                              (vec (for [^int j (range (second dims))]
                                     (.getDouble matrix i j)))))
                       ;; (tensor/compute-tensor dims
                       ;;                        (fn [^long i
                       ;;                             ^long j]
                       ;;                          (.getDouble matrix i j))
                       ;;                        :float32)
                       (throw (Exception. (str "Unsupported dimension count: " (count dims)))))]
          (assoc metadata :values values))))))

(set! *warn-on-reflection* true)
(defn struct->map
  "MATLAB struct to a Clojure map"
  [^Struct struct]
  (when struct
    (let [field-names (.getFieldNames struct)]; it takes effort for the JVM to know what to do
      (reduce (fn [acc field-name]            ; this is called reflection
                (let [value (.get struct field-name)]
                  (assoc acc (keyword field-name)
                         (convert-element value))))
              {}
              field-names))))

(defn read-eeg-data
  "Read EEG data from a MATLAB file and convert to a Clojure map"
  [file-path]
  (let [mat-file ^Mat5File (Mat5/readFromFile ^String file-path)
        eeg-struct ^MatStruct (.getStruct mat-file "eeg")]
    (struct->map eeg-struct)))

(defn mat-to-edn
  "Print the contents of the mat file to an edn log."
  []
  (let [eeg-data (read-eeg-data "resources/data/s01.mat")]
    (with-open [w (clojure.java.io/writer "brains!.edn")]
      (binding [*out* w]
        (clojure.pprint/pprint eeg-data)))
    (println "Data saved to brains!.edn")))

(defn load-eeg-data
  "Load EEG data from a MATLAB file with preprocessing"
  [file-path]
  (let [mat-file (time (Mat5/readFromFile (clojure.java.io/file file-path)))
        eeg-struct (.getStruct mat-file "eeg")
        sample-number (if-let [matches (re-find #"s(\d+)\.mat" file-path)]
                        (second matches)
                        "unknown")
        data (time (struct->map eeg-struct))
        processed-data {:sample_number  sample-number
                        :movement_left  (update (:movement_left  data) :values vec)
                        :movement_right (update (:movement_right data) :values vec)
                        :imagery_left   (update (:imagery_left   data) :values vec)
                        :imagery_right  (update (:imagery_right  data) :values vec)
                        :movement_event (update (:movement_event data) :values vec)
                        :imagery_event  (update (:imagery_event  data) :values vec)
                        :senloc         (update (:senloc         data) :values vec)
                        :psenloc        (update (:psenloc        data) :values vec)}]
    
    {:processed-data processed-data
     :data-keys (keys processed-data)
     :num-dimensions (time (into {} (map (fn [[k v]] [k (:dims v)])
                                         (select-keys processed-data [:movement_left  :movement_right
                                                                      :imagery_left   :imagery_right
                                                                      :movement_event :imagery_event
                                                                      :senloc         :psenloc]))))}))

(def memoized-eeg-data
  (memoize load-eeg-data))
