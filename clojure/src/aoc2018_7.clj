(ns aoc2018-7
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def line-re #"Step (\w+) must be finished before step (\w+) can begin.")

(defn read-input [resource] (-> (io/resource resource)
                                slurp
                                str/split-lines))

(defn parse-input
  "입력에서 의존성에 따라 {A #{B C}}의 맵을 만듭니다."
  [lines]
  (->> lines
       (map #(re-matches line-re %))
       (map rest)
       (reduce (fn [acc [left right]]
                 (update
                  acc
                  left
                  #(if % (conj % right) #{right})))
               {})))

;; part 1
(defn find-entries
  [deps]
  (let [lefts (into (sorted-set) (keys deps))
        rights (apply clojure.set/union (vals deps))]
    (filter #(not (rights %)) lefts)))

(defn find-last-jobs
  "다른 작업들이 의존하지 않는 마지막 작업들을 찾습니다."
  [deps]
  (clojure.set/difference
   (apply clojure.set/union (vals deps))
   (into (sorted-set) (keys deps))))

(defn find-orders
  "{C #{F A}, A #{B D}, ...} -> [C A ...]"
  [deps]
  (loop [{:keys [remaining
                 done
                 entries]} {:remaining deps
                            :done []
                            :entries (find-entries deps)}]
    (prn remaining done entries)
    (if (empty? remaining)
      (apply conj done (find-last-jobs deps))
      (let [entry (first entries)
            rem (dissoc remaining entry)]
        (recur {:remaining rem
                :done (conj done entry)
                :entries (find-entries rem)})))))

(defn part1
  []
  (let [orders (-> (read-input "aoc2018_7.txt")
                   parse-input
                   find-orders)]
    (apply str orders)))

;; part 2


(comment

  (parse-input (read-input "aoc2018_7.txt")))
