(ns aoc2018-1 (:require clojure.string [clojure.java.io :as io]))

(def input (clojure.string/split-lines (slurp (io/resource "aoc2018_1.txt"))))

;; 파트 1
;; 주어진 입력의 모든 숫자를 더하시오.
;; 예) +10 -2 -5 +1 이 입력일 경우 4를 출력
(def numbers (map #(Integer. %) input))

(def part1 (reduce + numbers))

;; 파트 2
;; 주어진 입력의 숫자를 더할 때 마다 나오는 숫자 중, 처음으로 두번 나오는 숫자를 리턴하시오.
;; 예) +3, +3, +4, -2, -4 는 10이 처음으로 두번 나오는 숫자임.
;; 0 -> 3 (+3) -> 6 (+3) -> 10(+4) -> 8(-2) -> 4(-4) -> 7(+3) -> 10(+3) -> ...

(def part2
  (->> (reductions + (cycle numbers))
       (reduce (fn [acc v] (if (contains? acc v)
                             (reduced v)
                             (conj acc v))) #{0})))

;; #################################
;; ###        Refactoring        ###
;; #################################

;; cycle 혹은 reductions 사용하기
;; loop-recur 시 let으로 바인딩하기