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

(def fecha-formatter (f/formatter "dd/MM/yyyy HH:mm:ss"))

(def fecha (f/parse fecha-formatter fecha-csv))


; lista de empleados
(def empleados (distinct (map first csvfile)))


; cuantos registros tiene el empleado emp en el archivo de entrada
(defn howmany [empleado csvfile]
    (count (filter (fn [a] (= (first a) empleado)) csvfile)))

; cada empleado con su numero de entradas en el archivo
(def cuantos
    (zipmap empleados (map (fn [n] (howmany n csvfile)) empleados)))

; dias trabajados por cada empleado
(def diastrabajados (zipmap empleados (make-array Integer/TYPE (count empleados))))

; porcion del csv pertenieciente al empleado "empleado"
(defn emp [empleado csvfile]
    (filter (fn [a] (= (first a) empleado)) csvfile))



; aplica proc a cada subseccion de empleados 
(defn res [proc csvfile emplados]
     (zipmap (map (fn [e] (count (distinct(proc (emp e csvfile)))) ) empleados) empleados))

(defn fdiastrabajados [fi]	
    (if (empty? fi) 
       nil
       (let [fecha (f/parse fecha-formatter (nth (first fi) 2)) ]
	(cons (str (t/day fecha) (t/month fecha) (t/year fecha)) (fdiastrabajados (rest fi))) ))
)

; dias trabajados
(def dt (res fdiastrabajados csvfile empleados)) 
