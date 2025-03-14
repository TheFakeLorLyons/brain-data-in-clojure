#!/usr/bin/env bb
(require '[babashka.process :refer [shell]]
         '[selmer.parser :refer [<<]]
         '[clojure.java.io :as io])

(def samples (range 1 53))

(def download-folder "/mnt/g/Downloads/brains/")

(.mkdirs (io/file download-folder))

(defn download-file [num]
  (let [file-num (format "%02d" num)
        url (<< "https://s3.ap-northeast-1.wasabisys.com/gigadb-datasets/live/pub/10.5524/100001_101000/100295/mat_data/s{{file-num}}.mat")
        output-path (str download-folder "s" file-num ".mat")]
    (if (.exists (io/file output-path))
      (println "File:" output-path "already exists, skipping.")
      (do
        (println "Downloading" url "to" output-path)
        (shell {:dir "."} (<< "wget -q --show-progress -O {{output-path}} {{url}}"))))))

(println " --Starting downloads--")

(doseq [num samples]
  (download-file num))

(println " --All downloads completed--")