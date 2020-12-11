(ns aoc.7
  (:require [clojure.string :as str]))

(def test-data
  "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
")

(def rule-re #"^(.*) bags contain (.*)$")

(defn parse-sub-bags [sub-bags]
  (if (= sub-bags "no other bags")
    []
    (reduce (fn [m [_ quantity bag]]
              (merge m {bag (Integer/parseInt quantity)}))
            {}
            (re-seq #"(\d+) (\w+ \w+)" sub-bags))))


(def rules
  (->> test-data
       str/split-lines
       (reduce (fn [rules line]
                 (let [[bag sub-bags] (drop 1 (re-matches rule-re line))]
                   (merge rules {bag (parse-sub-bags sub-bags)}))) {})))

(defn parse-sub-bags-2 [sub-bags]
  (if (= sub-bags "no other bags")
    []
    (mapcat (fn [[_ quantity bag]] (repeat (Integer/parseInt quantity) bag))
            (re-seq #"(\d+) (\w+ \w+)" sub-bags))))

(def rules-2
  (->> "./7.dat"
       slurp
       str/split-lines
       (reduce (fn [rules line]
                 (let [[bag sub-bags] (drop 1 (re-matches rule-re line))]
                   (merge rules {bag (parse-sub-bags-2 sub-bags)}))) {})))

(defn expand-bag [bag]
  (cond
    (empty? bag) nil
    (= "shiny gold" bag) "shiny gold"
    :else (map expand-bag (get rules-2 bag))))

(defn solve []
  (->> rules-2
       (filter (fn [[k v]]
                 (some #{"shiny gold"} v
                       ;; (flatten (expand-bag k))
                       )))
       ))
