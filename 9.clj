(ns aoc.9
  (:require [clojure.string :as str]))

(def test-input
  "35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")

(def preamble-size 25)

(def numbers
  (->>
   "./9.dat"
   slurp
   str/split-lines
   (mapv #(BigInteger. %))))

(defn solve []
  (->> (for [numbers (partition (inc preamble-size) 1 numbers)
             :let [sum (last numbers)
                   values (into #{} (take preamble-size numbers))]]
         {:values (filter (comp not nil?)
                          (for [value values]
                            (and (not (= (/ sum 2) value))
                                 (values (- sum value)))))
          :sum sum})
       (filter (comp empty? :values))
       first))

(def target-val 41682220)
(defn solve-2 []
  (loop [start 0
         end 2]
    (let [num-range (subvec numbers start end)
          total (apply + num-range)]
      (cond
        (= target-val total) [(apply min num-range) (apply max num-range)]
        (> total target-val) (recur (inc start) (+ start 2))
        :else (recur start (inc end))))))
