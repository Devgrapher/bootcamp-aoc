(ns aoc2018-2 (:require clojure.string
                        clojure.data
                        [clojure.math.combinatorics :as combinatorics]
                        [clojure.java.io :as io]))

(def input (io/resource "aoc2018_2.txt"))

(def ids (-> input
             slurp
             clojure.string/split-lines))
;; 파트 1
;; 주어진 각각의 문자열에서, 같은 문자가 두번 혹은 세번씩 나타난다면 각각을 한번씩 센다.
;; 두번 나타난 문자가 있는 문자열의 수 * 세번 나타난 문자가 있는 문자열의 수를 반환하시오.
;; abcdef 어떤 문자도 두번 혹은 세번 나타나지 않음 -> (두번 나오는 문자열 수: 0, 세번 나오는 문자열 수: 0)
;; bababc 2개의 a, 3개의 b -> (두번 나오는 문자열 수: 1, 세번 나오는 문자열 수: 1)
;; abbcde 2개의 b -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 1)
;; abcccd 3개의 c -> (두번 나오는 문자열 수: 2, 세번 나오는 문자열 수: 2)
;; aabcdd 2개의 a, 2개의 d 이지만, 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 3, 세번 나오는 문자열 수: 2)
;; abcdee 2개의 e -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 2)
;; ababab 3개의 a, 3개의 b 지만 한 문자열에서 같은 갯수는 한번만 카운트함 -> (두번 나오는 문자열 수: 4, 세번 나오는 문자열 수: 3)
;; 답 : 4 * 3 = 12

(defn frequency-numbers [coll]
  (-> coll
      frequencies
      vals
      distinct))

(defn frequencies-through [coll]
  (->> coll
       (map frequencies)
       (apply merge-with +)))

(def part1
  (let [{count-2 2 count-3 3} (frequencies-through (map frequency-numbers ids))]
    (* count-2 count-3)))

(comment
  (frequency-numbers "ymdrcyapvwfloiuktanxzjsieb"))

;; 파트 2
;; 여러개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오.
;; 예)
;; abcde
;; fghij
;; klmno
;; pqrst
;; fguij
;; axcye
;; wvxyz

;; 주어진 예시에서 fguij와 fghij는 같은 위치 (2번째 인덱스)에 정확히 한 문자 (u와 h)가 다름. 따라서 같은 부분인 fgij를 리턴하면 됨.

(defn single?
  [coll]
  (= (count (filter some? coll)) 1))

(defn common-chars [s1 s2]
  (let [[d1 _ common] (clojure.data/diff (seq s1) (seq s2))]
    (when (single? d1)
      (apply str common))))

(def part2
  (let [coll ids
        pairs (combinatorics/combinations coll 2)]
    (keep (fn [[first second]] (common-chars first second)) pairs)))

(comment
  ids
  (combinatorics/combinations ids 2))

;; #################################
;; ###        Refactoring        ###
;; #################################

;; frequencies 사용하기
;; PPAP (parse-process-aggregate-print) 원칙 따르기
;; declarative 한 함수 이름 사용하기