(ns aoc2020-4
  (:require [clojure.spec.alpha :as spec]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (io/resource "aoc2020_4.txt"))

(defn entry->fields
  "패스포트 입력을 각 필드의 kv로 변환합니다."
  [entry]
  (->> (str/split entry #" ")
       (map #(str/split % #":"))
       (map (fn [[left right]]
              [(keyword left) right]))))

(defn parse-input
  "입력을 패스포트 맵의 리스트로 변환합니다."
  [resource]
  (let [entries (-> resource
                    slurp
                    (str/replace #"\n" " ")
                    (str/split #"  "))]
    (->> entries
         (map entry->fields)
         (map #(into {} %)))))


;;; part1

(spec/def ::simple #(every? % #{:hcl :hgt :ecl :iyr :pid :byr :eyr}))

(defn part1 []
  (->> (parse-input input)
       (filter #(spec/valid? ::simple %))
       count))

;;; part2

(spec/def :passport/ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(spec/def :passport/pid #(re-find #"^\d{9}$" %))
(spec/def :passport/eyr #(spec/int-in-range? 2020 2031 (Integer. %)))
(spec/def :passport/hcl #(re-find #"^#[\w\d]{6}$" %))
(spec/def :passport/byr #(spec/int-in-range? 1920 2003 (Integer. %)))
(spec/def :passport/iyr #(spec/int-in-range? 2010 2021 (Integer. %)))
(spec/def :passport/hgt #(re-find #"^\d{3}cm$|^\d{2}in$" %))

(spec/def :passport/passport
  (spec/keys :req-un [:passport/ecl
                      :passport/pid
                      :passport/eyr
                      :passport/hcl
                      :passport/byr
                      :passport/iyr
                      :passport/hgt]))

(defn part2 []
  (->> (parse-input input)
       (filter #(spec/valid? :passport/passport %))
       count))

(comment
  (parse-input (input))

  (spec/explain :passport/passport {:ecl "gry" :pid 860033327 :eyr 2020 :hcl "#fffffd"
                                    :byr 1937 :iyr 2017 :cid 147 :hgt "183cm"}))
