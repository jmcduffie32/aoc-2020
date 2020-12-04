(ns aoc.4
  (:require [clojure.string :as str]))

(def test-data
"
ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
")


(defn parse-passports [data]
  (map str/trim-newline (str/split data #"\n\n")))

(defn parse-passport [passport-str]
  (-> passport-str
      (str/split #"\n| ")
      (->>
       (reduce (fn [passport val]
                 (let [[k v] (str/split val #":")]
                   (assoc passport (keyword k) v))) {}))))

(defn passport-valid? [passport]
  (and
   (:byr passport)
   (:iyr passport)
   (:eyr passport)
   (:hgt passport)
   (:hcl passport)
   (:ecl passport)
   (:pid passport)))

(defn hgt-valid? [val]
  (let [[_ digit unit] (re-matches #"(\d+)(cm|in)" val)]
    (cond
      (= unit "cm") (<= 150 (Integer/parseInt digit) 193)
      (= unit "in") (<= 59 (Integer/parseInt digit) 76))))

(defn passport-strict-valid? [passport]
  (when (passport-valid? passport)
    (and
     (<= 1920 (Integer/parseInt (:byr passport)) 2002)
     (<= 2010 (Integer/parseInt (:iyr passport)) 2020)
     (<= 2020 (Integer/parseInt (:eyr passport)) 2030)
     (hgt-valid? (:hgt passport))
     (re-find #"^#[0-9a-f]{6}$" (:hcl passport))
     (re-find #"^(amb|blu|brn|gry|grn|hzl|oth)$" (:ecl passport))
     (re-find #"^\d{9}$" (:pid passport)))))


(defn solve []
  (-> "./4.dat"
      slurp
      parse-passports
      (->>
       (map parse-passport)
       (filter passport-valid?)
       count
       )))

(defn solve-2 []
  (-> "./4.dat"
      slurp
      parse-passports
      (->>
       (map parse-passport)
       ;; (println)
       (filter passport-strict-valid?)
       count
       )))
