(ns aoc.5
  (:require [clojure.string :as str]))

(def total-rows (range 128))
(def total-cols (range 8))

(defn find-row [[ch & rest] rows]
  (if ch
    (do
      (if (= ch \F)
        (recur rest (take (/ (count rows) 2) rows))
        (recur rest (drop (/ (count rows) 2) rows))))
    (first rows)))

(defn find-col [[ch & rest] cols]
  (if ch
    (do
      (if (= ch \L)
        (recur rest (take (/ (count cols) 2) cols))
        (recur rest (drop (/ (count cols) 2) cols))))
    (first cols)))

(defn solve []
  (->> "./5.dat"
       slurp
       (str/split-lines)
       (map (fn [ticket]
              (let [row (find-row (take 7 ticket) total-rows)
                    col (find-col (drop 7 ticket) total-cols)]
                {:ticket ticket
                 :id (+ col (* 8 row))
                 :row row
                 :col col})))
       (apply max-key :id)))

(def tickets
  (->> "./5.dat"
       slurp
       (str/split-lines)
       (map (fn [ticket]
              (let [row (find-row (take 7 ticket) total-rows)
                    col (find-col (drop 7 ticket) total-cols)]
                {:ticket ticket
                 :id (+ col (* 8 row))
                 :row row
                 :col col})))
       (sort-by :id)))

(def ids (map :id tickets))

(defn find-empty-seat []
  (->> (map vector (range (apply min ids) (apply max ids)) ids)
       (filter #(not= (first %) (second %)))
       first
       first))
