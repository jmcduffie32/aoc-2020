(ns aoc.1
  (:require [clojure.string :as str]))

(def expenses (-> "./1.dat"
                  slurp
                  (str/split-lines)
                  (->>
                   (map #(Integer/parseInt %)))))

(defn solve []
  (for [i expenses
        j expenses
        :when (and (not= i j) (= 2020 (+ i j)))]
    (* i j)))

(defn solve-2 []
  (for [i expenses
        j expenses
        k expenses
        :when (and (not= i j) (not= i k) (not= j k) (= 2020 (+ i j k)))]
    (* i j k)))
