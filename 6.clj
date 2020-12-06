(ns aoc.6
  (:require [clojure.string :as str]))

(defn count-yeses [data count-fn]
  (let [groups (str/split data #"\n\n")]
    (apply + (map
              (fn [group]
                (->> group
                     str/split-lines
                     (map #(into #{} %))
                     (apply count-fn)
                     (count)
                     ))
              groups))))

(println (str "Any yesses: " (count-yeses (slurp "./6.dat")
                                          clojure.set/union)))
(println (str "All yesses: " (count-yeses (slurp "./6.dat")
                                          clojure.set/intersection) "\n"))
