(ns hello-world.core
    (require [clojure.data.csv :as csv]
         [clojure.java.io :as io]
         [clj-time.format :as f]))

(use 'clj-time.core)

(defn opencsv [filename]
    (with-open [in-file (io/reader filename)] 
        (doall
          (csv/read-csv in-file))))

(defn writecsv [filename]
    (with-open [out-file (io/writer filename)]
      (csv/write-csv out-file
                 [["abc" "def"]
                  ["ghi" "jkl"]])))

(defn strreplace
	"Quita comillas de las strings" 
	[str]
	(clojure.string/replace str #"\"" {"\"" ""}))

(defn openfile [filename]
    (with-open [rdr (io/reader filename)]
        (doseq [line (line-seq rdr)]
        (spit "test.csv" (str (strreplace line) "\n") :append true))))

(defn seq-contains?
  "Determine whether a sequence contains a given item"
  [sequence item]
  (if (empty? sequence)
    false
    (reduce #(or %1 %2) (map #(= %1 item) sequence))))

(def csvfile (opencsv "test.csv"))

(def fecha-csv (nth (nth csvfile 0) 2))

(def fecha-formatter (f/formatter "dd/MM/yyyy hh:mm:ss"))

(def fecha (f/parse fecha-formatter fecha-csv))

(def empleados (distinct (map first csvfile)))



