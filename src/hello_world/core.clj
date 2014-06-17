(ns hello-world.core
    (require [clojure.data.csv :as csv]
         [clojure.java.io :as io]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


(defn opencsv [filename]
    (with-open [in-file (io/reader filename)] 
        (doall
          (csv/read-csv in-file))))

(defn writecsv [filename]
    (with-open [out-file (io/writer filename)]
      (csv/write-csv out-file
                 [["abc" "def"]
                  ["ghi" "jkl"]])))

(def csvfile (opencsv "InOutData.csv"))

(defn openfile [filename]
    (with-open [rdr (io/reader filename)]
        (doseq [line (line-seq rdr)]
        (spit "test.txt" (drop-last (first line)) :append true))))




