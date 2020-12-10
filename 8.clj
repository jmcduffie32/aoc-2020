(ns aoc.8
  (:require [clojure.string :as str]))

(def test-src
  "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")


(defn parse-src-line [line]
  (let [[_ op arg] (re-matches #"(nop|acc|jmp) ([+-]\d+)" line)
        op (keyword op)
        arg (Integer/parseInt arg)]
    {:executed? false
     :op op
     :arg arg}))

(defn parse-src-str [src]
  (->> src
       str/split-lines
       (mapv parse-src-line)))

(defn run-src [src]
  (let [acc (atom 0)
        instructions (atom (parse-src-str src))
        i (atom 0)]
    (while (not (get-in @instructions [@i :executed?]))
      (swap! instructions assoc-in [@i :executed?] true)
      (case (get-in @instructions [@i :op])
        :nop (swap! i inc)
        :acc (do
               (swap! acc + (get-in @instructions [@i :arg]))
               (swap! i inc))
        :jmp (swap! i + (get-in @instructions [@i :arg]))))
    @acc))

(def possible-bugs
  (->> test-src
       parse-src-str
       (filter (fn [inst]
                 (or (= (:op inst) :nop)
                     (= (:op inst) :jmp))))))
