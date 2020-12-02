(ns aoc.2
  (:require [clojure.string :as str]))

(def pattern #"(\d+)-(\d+) ([a-z]): ([a-z]+)")

(def password-data (-> "./2.dat"
                       slurp
                       (str/split-lines)
                       (->>
                        (map (fn [val]
                               (let [matches (re-matches pattern val)]
                                 {:min (Integer/parseInt (nth matches 1))
                                  :max (Integer/parseInt (nth matches 2))
                                  :letter (nth matches 3)
                                  :password (nth matches 4)}))))))
(defn count-letters [word letter]
  (count (filter #(= (nth letter 0) %) word)))

(defn solve []
  (count (filter (fn [val]
                   (<= (:min val)
                      (count-letters (:password val) (:letter val))
                      (:max val)))
                 password-data)))

(defn solve-2 []
  (count (filter (fn [val]
                   (and
                    (or
                     (= (:letter val) (str (nth (:password val) (dec (:min val)))))
                     (= (:letter val) (str (nth (:password val) (dec (:max val)) ))))
                    (not= (nth  (:password val) (dec (:min val)))
                          (nth  (:password val) (dec (:max val))))))
                 password-data)))
