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


(defn parse-src-line [line-num line]
  (let [[_ op arg] (re-matches #"(nop|acc|jmp) ([+-]\d+)" line)
        op (keyword op)
        arg (Integer/parseInt arg)]
    {:executed? false
     :op op
     :arg arg
     :line-num line-num}))

(defn parse-src-str [src]
  (->> src
       str/split-lines
       (map-indexed parse-src-line)
       (into [])))

(def instructions
  (parse-src-str (slurp "./8.dat")))

(defn run-instructions [instructions]
  (let [acc (atom 0)
        instructions (atom instructions)
        i (atom 0)]
    (while (and (get-in @instructions [@i])
                (not (get-in @instructions [@i :executed?])))
      (swap! instructions assoc-in [@i :executed?] true)
      (case (get-in @instructions [@i :op])
        :nop (swap! i inc)
        :acc (do
               (swap! acc + (get-in @instructions [@i :arg]))
               (swap! i inc))
        :jmp (swap! i + (get-in @instructions [@i :arg]))
        (println (get-in @instructions [@i]))))
    (if (= (count @instructions) @i)
      {:result :completed :acc @acc}
      {:result :looped :acc @acc})))

(def possible-bugs
  (->> "./8.dat"
       slurp
       parse-src-str
       (filter (fn [inst]
                 (or (= (:op inst) :nop)
                     (= (:op inst) :jmp))))))

(filter #(= (:result %) :completed)
 (for [instruction possible-bugs
              :let [line-num (:line-num instruction)
                    op (:op instruction)]]
          (if (= op :nop)
            (run-instructions (assoc-in instructions [line-num :op] :jmp))
            (run-instructions (assoc-in instructions [line-num :op] :nop)))))
