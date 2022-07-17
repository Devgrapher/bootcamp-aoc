(ns aoc2020-8
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def input-sample (slurp (io/resource "aoc2020_8_1.txt")))
(def input (slurp (io/resource "aoc2020_8.txt")))

(defn parse-input [input]
  (->> input
       s/split-lines
       (map #(s/split % #"\s"))
       (map (fn [[op n]] {:op (keyword op)
                          :num (Integer. n)}))))

(defn execute [{:keys [instructions pc acc executions]}]
  (let [{:keys [op num]} (nth instructions pc)
        executions (conj executions pc)
        [acc pc] (case op
                   :acc [(+ acc num) (inc pc)]
                   :jmp [acc (+ pc num)]
                   :nop [acc (inc pc)])]
    {:instructions instructions
     :pc pc
     :acc acc
     :executions executions}))

(defn part1
  [input]
  (->> {:instructions (parse-input input)
        :pc           0
        :acc          0
        :executions #{}}
       (iterate execute)
       (drop-while #(not (contains? (:executions %) (:pc %))))
       first
       :acc))

(defn test-execution
  [instructions]
  (->> {:instructions instructions
        :pc           0
        :acc          0
        :executions #{}}
       (iterate execute)
       (drop-while #(and
                     (not (contains? (:executions %) (:pc %)))
                     (not= (:pc %) (count instructions)))) ;; 명령을 끝까지 실행하면 중단
       first))

(defn switch-op
  [m]
  (case (:op m)
    :jmp (assoc m :op :nop)
    :nop (assoc m :op :jmp)))


(defn gen-test-combinations
  "nop -> jmp or jmp -> nop 으로 변환한 모든 조합을 구한다.
   input: [{}{}...]
   output: ([{}{}...],[{}{}...] ...)"
  [instructions]
  (let [non-acc-indexes (->> instructions
                             (map-indexed vector)
                             (filter (fn [[_idx item]] (not= (:op item) :acc)))
                             (map first))]
    (map #(assoc instructions
                 %
                 (switch-op (nth instructions %)))
         non-acc-indexes)))

;; solution 2
;; 테스트셋을 만들때 index를 쓰는 대신 id를 담은 set으로 변환한다.
(defn gen-test-combinations2 [instructions]
  (let [instructions (->> (map-indexed vector instructions)
                          (map (fn [[idx e]] (assoc e :id idx)))
                          (apply sorted-set-by #(< (:id %1) (:id %2))))]
    (->> instructions
         (filter #(not= (:op %) :acc))
         (map #(-> instructions
                   (disj %)
                   (conj (switch-op %)))))))

(defn part2
  [input]
  (let [instructions (into [] (parse-input input))]
    (->> (gen-test-combinations instructions)
         (map test-execution)
         (filter (fn [{:keys [instructions pc]}] (= pc (count instructions))))
         first
         :acc)))

(comment
  (part1 input)
  (part2 input)

  (parse-input input-sample)
  (test-execution (parse-input input-sample))
  (gen-test-combinations (into [] (parse-input input)))

  (->> (into [] (parse-input input))
       (gen-test-combinations2)
       (map #(into [] %))
       (map test-execution)
       (filter (fn [{:keys [instructions pc]}] (= pc (count instructions))))
       first
       :acc))