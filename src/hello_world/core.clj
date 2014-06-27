(ns hello-world.core
    (require [clojure.data.csv :as csv]
         [clojure.java.io :as io]
         ))

(use 'clj-time.core)
(require '[clj-time.core :as t])
(require '[clj-time.format :as f])

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


; porcion del csv pertenieciente al empleado "empleado"
(defn emp [empleado csvfile]
    (filter (fn [a] (= (first a) empleado)) csvfile))

; aplica proc a cada empleado 
(defn appe [proc csvfile empleados]
     (map (fn [e] (proc (emp e csvfile))) empleados))

; retorna la lista de dias trabajados distintos en fi
(defn fdiastrabajados [fi]	
    (if (empty? fi) 
       nil
       (let [fecha (f/parse fecha-formatter (nth (first fi) 2)) ]
	(cons (str (t/day fecha) (t/month fecha) (t/year fecha)) (fdiastrabajados (rest fi))) ))
)


; agrega un 0 si no lo tiene al principio
(defn st [s] (if (< s 10) (str "0" s) (str s)))

; total de horas,minutos y segundos de un intervalo
(defn fhours [inter]
    (let [hours (int (/ inter 3600))
         mins (int (/ (mod inter 3600) 60))
         secs (mod (mod inter 3600) 60)]
     (str (st hours) ":" (st mins) ":" (st secs)) ))

; calcula tiempo total trabajado sobre la tabla "tabla"
(defn intervalos [tabla]
    (if (< (count tabla) 2) 0
    (+ (t/in-seconds (t/interval 
        (f/parse fecha-formatter (nth (nth tabla 0) 2))
        (f/parse fecha-formatter (nth (nth tabla 1) 2)) ))
        (intervalos (drop 2 tabla))
    )
))

; calcula tiempo total trabajado sobre el dia "d"
(defn intervalosdia [dias]
    (if (< (count dia) 2) 0
    (+ (t/in-seconds (t/interval 
        (f/parse fecha-formatter (nth tabla 0))
        (f/parse fecha-formatter (nth tabla 1)) ))
        (intervalos (drop 2 tabla))
    )
))

----------------------------------------

; cantidad de dias trabajados por empleado
; ----------------------------------------
(def diastr 
    (zipmap 
        (appe (fn [x] (count (distinct (fdiastrabajados x)))) csvfile empleados)
        empleados))

; cantidad de horas,minutos,segundos trabajados por empleado
; ----------------------------------------
(def horastr 
    (zipmap 
        (appe (fn [x] (fhours (intervalos x))) csvfile empleados)
        empleados))

; horas trabajadas por dia
(def horasdia 
    (zipmap 
        (appe   (fn [x] 
                   (map (fn [d] (fhours 
                         (intervalos d))) (distinct (fdiastrabajados x))))
                csvfile empleados)
        empleados))


; horas trabajada por local

