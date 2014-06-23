(ns hello-world.core
    (require [clojure.data.csv :as csv]
         [clojure.java.io :as io]
         [clj-time.format :as f]))

(use 'clj-time.core)
(require '[clj-time.core :as t])

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

(defn openfile [filename outfile]
    (with-open [rdr (io/reader filename)]
        (doseq [line (line-seq rdr)]
        (spit outfile (str (strreplace line) "\n") :append true))))

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

(t/in-hours (t/interval (t/date-time 1986) (t/date-time 1990)))


; lista de empleados
(def empleados (distinct (map first csvfile)))

(def diastrabajados (zipmap empleados (make-array Integer/TYPE (count empleados))))



