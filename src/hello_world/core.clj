(ns hello-world.core
    (require [clojure.data.csv :as csv]
         [clojure.java.io :as io]
         ))

(use 'clj-time.core)
(require '[clj-time.core :as t])
(require '[clj-time.format :as f])
(use '[clojure.string :only (join)])

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
  [item sequence]
  (if (empty? sequence)
    false
    (reduce #(or %1 %2) (map #(= %1 item) sequence))))


(def csvfile (opencsv "test.csv"))

(def fecha-csv (nth (nth csvfile 0) 2))

(def fecha-formatter (f/formatter "dd/MM/yyyy HH:mm:ss"))

(def fecha (f/parse fecha-formatter fecha-csv))


; lista de empleados
(def empleados (distinct (map first csvfile)))


; retorna porcion del csv pertenieciente al empleado "empleado"
(defn emp [empleado csvfile]
    (filter (fn [a] (= (first a) empleado)) csvfile))

; funcion auxiliar
(defn legajo [empleado]
    (nth (first (filter (fn [a] (= (first a) empleado)) csvfile)) 1) )

; legajos de los empleados
(def legajos (map legajo empleados))

; funcion auxiliar
(defn sector [empleado]
    (nth (first (filter (fn [a] (= (first a) empleado)) csvfile)) 3) )

; legajos de los empleados
(def sectores (map sector empleados))

; agrega un 0 si no lo tiene al principio
(defn st [s] (if (< s 10) (str "0" s) (str s)))

; aplica proc a cada empleado 
(defn appe [proc csvfile empleados]
     (map (fn [e] (proc (emp e csvfile))) empleados))

; retorna la lista de dias trabajados distintos en fi
(defn fdiastrabajados [fi]	
    (if (empty? fi) 
       nil
       (let [fecha (f/parse fecha-formatter (nth (first fi) 2)) ]
	(cons (str (st (t/day fecha)) (st (t/month fecha)) (st (t/year fecha))) (fdiastrabajados (rest fi))) ))
)

; lista de los locales	
(def locales (sort (distinct (map last csvfile))))

(def dias (distinct (fdiastrabajados csvfile)))

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

; compara la fecha de la fila row con d
(defn sameday [row d]
	(let [fecha (f/parse fecha-formatter (nth row 2)) ]
	 (= (str (st (t/day fecha)) (st (t/month fecha)) (st (t/year fecha))) d))
)

; compara el local de la fila row con d
(defn samelocal [row l]
    (= (last row) l)
)

;----------------------------------------

; conectar con abm
(def feriados ["22022014" "05022014"])

; dias habiles trabajados por empleado ; los que NO son feriados
; ----------------------------------------
(def diashabiles (appe (fn [x] (count (filter (fn [y] (not (seq-contains? y feriados))) (distinct (fdiastrabajados x))))) csvfile empleados))

; dias totales trabajados por empleado
; ----------------------------------------
(def diastotal (appe (fn [x] (count (distinct (fdiastrabajados x)))) csvfile empleados))

; dias feriados trabajados por empleado
; ----------------------------------------
(def diasferiados (map - diastotal diashabiles))

; horas trabajada por local
; ----------------------------------------
(def horaslocal (appe   (fn [x] 
                   (map (fn [l] (fhours 
                         (intervalos (filter (fn [row] (samelocal row l)) x)))) locales))
                csvfile empleados))

; cantidad de horas,minutos,segundos trabajados por empleado
; ----------------------------------------
(def horastotal (appe (fn [x] (fhours (intervalos x))) csvfile empleados))

; horas trabajadas por dia
; ----------------------------------------
(def horasdia (appe   (fn [x] 
                   (map (fn [d] (fhours 
                         (intervalos (filter (fn [row] (sameday row d)) x)))) (distinct (fdiastrabajados csvfile))))
                csvfile empleados))

; meses
(def mesmap {"01" "Ene" "02" "Feb" "03" "Mar" "04" "Abr" "05" "May" "06" "Jun"
             "07" "Jul" "08" "Ago" "09" "Sep" "10" "Oct" "11" "Nov" "12" "Dic" })

; formatos de la columnas que contienen dias "02-Feb-2014"
(defn format-column-days [x]
   (str (subs x 0 2) "-" (get mesmap (subs x 2 4)) "-" (subs x 4 8) ))

; conectar con abm
(def cant-total-dias (apply max diastotal))
(def cant-habiles cant-total-dias)


;--------------------------------
; SALIDA

(def primerafilacsv 
  (flatten 
    ["NRO. LEGAJO" "EMPLEADO" "SECTOR" "D. HABILES" "D. FERIADOS" "D. NO TRABAJADOS" "D. TRABAJADOS" 
		(map (fn [s] (str "LOCAL " s)) locales) 
		 "TOTAL HORAS"
                (map format-column-days dias)]))

; datos particulares por empleados
(def data-empl (reverse (zipmap legajos (reverse (zipmap empleados (map (fn [s] (str "LOCAL " s)) sectores))))))

(def diasnotrabajados (map (fn[x] (- cant-total-dias x)) diastotal))

; informacion computada
(def datos
  (cons primerafilacsv (map flatten (reverse (zipmap data-empl (map (comp flatten vector) diashabiles diasferiados diasnotrabajados diastotal horaslocal horastotal horasdia))))))

; imprime csv
(defn salida [datos outfile]
    (map (fn [row] (spit outfile (str (join ", " row) "\n") :append true)) datos))
