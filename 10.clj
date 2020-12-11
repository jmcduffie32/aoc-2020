(ns aoc.10
  (:require [clojure.string :as str]))

(def test-input
  "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3")

(def numbers
  (->> "./10.dat"
       slurp
       str/split-lines
       (mapv #(BigInteger. %))))

(def differences
  (->> (concat [0] numbers [(+ 3 (apply max numbers))])
       sort
       (partition 2 1)
       (map (fn [[v1 v2]] (- v2 v1)))))

(def diff-1 (count (filter #(= 1 %) differences)))
(def diff-3 (count (filter #(= 3 %) differences)))
