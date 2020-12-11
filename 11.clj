(ns aoc.11
  (:require [clojure.string :as str]))


(def test-data
  "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(def empty-seat? (comp not nil? #{"L" "."}))

(def layout
  (->> ;; test-data
   "./11.dat"
   slurp
   str/split-lines
   (map-indexed (fn [i line]
                  (->> (str/split line #"")
                       (map-indexed (fn [j sym]
                                      {:changed? false
                                       :i i
                                       :j j
                                       :empty? (empty-seat? sym)
                                       :floor? (= "." sym)}))
                       (into []))))
   (into [])))

(defn count-empty-adj-seats [layout [i j]]
  (->>
   (for [row (range (dec i) (+ 2 i))
         col (range (dec j) (+ 2 j))
         :when (or (not= row i) (not= col j))]
     (get-in layout [row col]))
   (filter #(or (nil? %) (:empty? %)))
   count))

(defn count-occupied-visible-seats [layout [i j]]
  (defn empty-seat? [seat]
    (cond
      (nil? seat) 0
      (:empty? seat) 0
      ((comp not :empty?) seat) 1))
  (->>
   [
    (loop [i (dec i)]
      (let [seat (get-in layout [i j])]
        (if (:floor? seat) (recur (dec i))
            (empty-seat? seat))))
    (loop [i (inc i)]
      (let [seat (get-in layout [i j])]
        (if (:floor? seat) (recur (inc i))
            (empty-seat? seat))))
    (loop [j (dec j)]
      (let [seat (get-in layout [i j])]
        (if (:floor? seat) (recur (dec j))
            (empty-seat? seat))))
    (loop [j (inc j)]
      (let [seat (get-in layout [i j])]
        (if (:floor? seat) (recur (inc j))
            (empty-seat? seat))))

    (loop [j (inc j)
           i (inc i)]
      (let [seat (get-in layout [i j])]
        (if (:floor? seat) (recur (inc j) (inc i))
            (empty-seat? seat))))
    (loop [j (dec j)
           i (dec i)]
      (let [seat (get-in layout [i j])]
        (if (:floor? seat) (recur (dec j) (dec i))
            (empty-seat? seat))))
    (loop [j (inc j)
           i (dec i)]
      (let [seat (get-in layout [i j])]
        (if (:floor? seat) (recur (inc j) (dec i))
            (empty-seat? seat))))
    (loop [j (dec j)
           i (inc i)]
      (let [seat (get-in layout [i j])]
        (if (:floor? seat) (recur (dec j) (inc i))
            (empty-seat? seat))))

    ]
   (apply +)))

(defn print-seat [seat]
  (cond
    (:floor? seat) "."
    (:empty? seat) "L"
    :else "#"))

(defn print-layout [layout]
  (doseq [row layout]
    (apply println (map print-seat row))))

(defn update-seat [layout {:keys [i j] :as seat}]
  (let [;; empty-seat-count (count-empty-adj-seats layout [i j])
        occupied-seat-count (count-occupied-visible-seats layout [i j])
        ]
    (cond
      (:floor? seat) (-> seat
                         (assoc :empty? true)
                         (assoc :changed? false))
      (:empty? seat) (if (= occupied-seat-count 0)
                       (-> seat
                           (assoc :empty? false)
                           (assoc :changed? true))
                       (assoc seat :changed? false))
      (not (:empty? seat)) (if (>= occupied-seat-count 5)
                             (-> seat
                                 (assoc :empty? true)
                                 (assoc :changed? true))
                             (assoc seat :changed? false))
      :else (assoc seat :changed? false))))

(defn update-seats [layout]
  (->> layout
       (mapv (partial mapv #(update-seat layout %)) )))


(defn solve []
  (loop [layout layout
         iter 0]
    (let [new-layout (update-seats layout)]
      (if (every? (comp not :changed?) (flatten new-layout))
        (->> new-layout
             flatten
             (filter (comp not :empty?))
             count)
        (recur new-layout (inc iter))))))
