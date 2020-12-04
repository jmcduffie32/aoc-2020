(ns aoc.3
  (:require [clojure.string :as str]))


(def map-data (-> "./3.dat"
                  slurp
                  (str/split-lines)
                  (->>
                   (mapv #(into [] (str/split % #""))))))

(def row-count (count map-data))
(def col-count (count (nth map-data 0)))

(def slopes [[1 1] [3 1] [5 1] [7 1] [1 2]])

(defn count-trees [[rise run]]
  (->>
   (for [n (range 0 row-count)
         :let [x (rem (* rise n) col-count)
               y (* n run)]
         :while (< y row-count)]
     (nth (nth map-data y) x))
   (filter #(= % "#"))
   count))

(defn solve []
  (count-trees [3 1]))

(defn solve-2 []
  (->> slopes
       (map count-trees)
       (apply *)))
