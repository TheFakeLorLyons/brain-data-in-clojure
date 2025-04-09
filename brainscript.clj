#!/usr/bin/env bb
(require '[babashka.process :refer [shell]]
         '[selmer.parser :refer [<<]]
         '[clojure.java.io :as io]
         '[clojure.tools.cli :refer [parse-opts]])

(def download-folder "./resources/data/")
(.mkdirs (io/file download-folder))

(def cli-options
  [["-a" "--all" "Download all samples (1-52)"]
   ["-s" "--single NUMBER" "Download a single sample by number"
    :parse-fn #(Integer/parseInt %)]
   ["-r" "--range START END" "Download a range of samples (inclusive)"
    :parse-fn #(mapv (fn [x] (Integer/parseInt x)) (str/split % #"-"))
    :validate [#(= 2 (count %)) "Range must have start and end values"]]
   ["-h" "--help" "Display this help menu"]])

(defn helpful-message []
  (println "=========================================-- - °  -\n"
           "   _   _   _   _   _   _   _   _   _   _    °  °o\n"
           "  / \\ / \\ / \\ / \\ / \\ / \\ / \\ / \\ / \\   o°    |\n"
           "       s01 was already downloaded!            *O\n"
           "  ¡These files are big! - (approximately 2-300mb each),     \n"
           "   The following commands can download other files:     \n"
           "                                 ___          \\\n"
           "  Download additional datasets: /   \\        \\\n"
           "  -a for all datasets            |  |          \\\n"
           "  -s (number) for specific file  |  |            \\\n"
           "  -r (start-end) for a range     |  |            \\\n"
           "=============================================---- - --- -  -. ."))

(defn download-file [num]
  (let [file-num (format "%02d" num)
        url (<< "https://s3.ap-northeast-1.wasabisys.com/gigadb-datasets/live/pub/10.5524/100001_101000/100295/mat_data/s{{file-num}}.mat")
        output-path (str download-folder "s" file-num ".mat")]
    (if (.exists (io/file output-path))
      (helpful-message)
      (do
        (println "Downloading" url "to" output-path)
        (shell {:dir "."} (<< "wget -q --show-progress -O {{output-path}} {{url}}"))))))

(defn print-usage [options-summary]
  (println "\n Usage: script [options]")
  (println options-summary)
  (println "\n Examples:")
  (println "  ./brainscript.clj             # Downloads only the first sample (s01)")
  (println "  ./brainscript.clj -s 5        # Downloads sample s05")
  (println "  ./brainscript.clj -r 3-7      # Downloads samples s03 through s07")
  (println "  ./brainscript.clj -a          # Downloads all samples s01 through s52"))

(let [{:keys [options summary errors]} (parse-opts *command-line-args* cli-options)
      message-downloaded? (atom false)]

  (cond
    (:help options)
    (print-usage summary)

    errors
    (do
      (println "Errors:")
      (doseq [error errors]
        (println error))
      (println)
      (print-usage summary))

    (:all options)
    (do
      (println "--Starting download of ALL samples (1-52)--")
      (doseq [num (range 1 53)]
        (download-file num)))

    (:single options)
    (do
      (println (str "--Downloading sample s" (format "%02d" (:single options)) "--"))
      (download-file (:single options)))

    (:range options)
    (let [[start end] (:range options)]
      (println (str "--Downloading samples s" (format "%02d" start) " through s" (format "%02d" end) "--"))
      (doseq [num (range start (inc end))]
        (download-file num)))

    :else
    (do
      (println "\n --Downloading default sample (s01)--")
      (download-file 1)))

  (if @message-downloaded?
    (println "--Download operation completed-- \n")
    (println "\n    ~ \"Don't quote me on this\" ~ 
              -anon.\n")))