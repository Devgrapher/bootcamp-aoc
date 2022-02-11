(ns aoc2018-5
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

#_(def input "dabAcCaCBAcCcaDA")
(def input (-> (io/resource "aoc2018_5.txt")
               slurp))

;; 파트 1
;; 입력: dabAcCaCBAcCcaDA

;; 같은 종류의 소문자와 대문자는 서로 ‘반응‘하여 사라짐. aABb -> ‘’
;; 사라진 자리는 진공이 되기 때문에 다른 문자들이 붙게 되고, 또 그 문자들끼리 반응할 수 있음.  abBA-> aA -> ‘’
;; 바로 옆에 붙어있어야만 서로 반응함. abAB -> abAB (반응 없음)
;; 대문자-대문자, 소문자-소문자는 서로 반응하지 않음. aabAAB-> aabAAB (반응 없음)
;; 예시 dabAcCaCBAcCcaDA => dabCBAcaDA

;; 주어진 input 에서 최종으로 남는 문자열을 리턴하시오.

(defn units-react?
  "두 유닛이 반응하는 조합인지 판단"
  [s1 s2]
  (and (not= s1 s2)
       (= (str/lower-case s1) (str/lower-case s2))))

(defn make-react
  "입력된 유닛을 모두 반응시켜서 남은 유닛만 반환합니다."
  [units]
  (->> (loop [acc [:none]
              [left right & remain] units]
         #_(println [acc left right remain])
         (if (empty? remain)
           (if (units-react? left right) acc (conj acc left right))
           (if (units-react? left right)
             (recur (pop acc) (cons (last acc) remain))
             (recur (conj acc left) (cons right remain)))))
       (remove #(= :none %))))

(defn part1 [] (-> input
                   make-react
                   count))

;; 파트 2
;; 주어진 문자열에서 한 유닛 (대문자와 소문자)을 전부 없앤 후 반응시켰을 때, 가장 짧은 문자열의 길이를 리턴하시오.
;; 예를 들어 dabAcCaCBAcCcaDA 에서 a/A를 없애고 모두 반응시키면 dbCBcD가 되고 길이는 6인데 비해,
;; 같은 문자열에서 c/C를 없애고 모두 반응시키면 daDA가 남고 길이가 4이므로 4가 가장 짧은 길이가 됨.

(defn char-range [start end]
  (map char (range (int start) (inc (int end)))))

(def filtering-units (map #(set [%1 %2]) (char-range \a \z) (char-range \A \Z)))

(defn part2 []
  (let [filtered-inputs (map #(remove % input) filtering-units)
        results (map make-react filtered-inputs)
        counts (map count results)]
    (->> (zipmap results counts)
         (sort-by val)
         first
         val)))

(comment
  ;; part2
  (char-range \a \z)
  (char-range \A \Z)

  filtering-units)