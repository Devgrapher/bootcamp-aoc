(ns aoc2018-3
  (:require clojure.set
            clojure.string
            [clojure.java.io :as io]))

(def input (-> (io/resource "aoc2018_3.txt")
               slurp
               clojure.string/split-lines))

(defn parse-claim [claim]
  (let [[_ & groups] (re-matches #"#(\d+) @ (\d+),(\d+): (\d+)x(\d+)" claim)
        [id x y w h] (map read-string groups)]
    {:id id :x x :y y :w w :h h}))

(def claims (map parse-claim input))

;; 파트 1
;; 다음과 같은 입력이 주어짐.

;; #1 @ 1,3: 4x4
;; #2 @ 3,1: 4x4
;; #3 @ 5,5: 2x2

;; # 뒤에 오는 숫자는 ID, @ 뒤에 오는 숫자 쌍 (a, b)는 시작 좌표, : 뒤에 오는 (c x d)는 격자를 나타냄.
;; 입력의 정보대로 격자 공간을 채우면 아래와 같이 됨.

;;      ........
;;      ...2222.
;;      ...2222.
;;      .11XX22.
;;      .11XX22.
;;      .111133.
;;      .111133.
;;      ........

;; 여기서 XX는 ID 1, 2, 3의 영역이 두번 이상 겹치는 지역.
;; 겹치는 지역의 갯수를 출력하시오. (위의 예시에서는 4)

(defn gen-rect
  ([{:keys [x y w h]}] (gen-rect [x y] [w h]))
  ([[x y] [w h]]
   (for [i (range w)
         j (range h)]
     [(+ x i) (+ y j)])))

(def claimed-rects (map gen-rect claims))

(def point-frequencies (frequencies (apply concat claimed-rects)))

(def part1
  (count
   (filter #(> (val %) 1) point-frequencies)))


(comment
  (parse-claim "#144 @ 208,134: 3x10")
  (gen-rect '(:x 208 :y 134 :w 3 :h 10)))

;; 파트 2
;; 입력대로 모든 격자를 채우고 나면, 정확히 한 ID에 해당하는 영역이 다른 어떤 영역과도 겹치지 않음
;; 위의 예시에서는 ID 3 이 ID 1, 2와 겹치지 않음. 3을 출력.
;; 겹치지 않는 영역을 가진 ID를 출력하시오. (문제에서 답이 하나만 나옴을 보장함)

(def rects-by-id
  (zipmap (map :id claims) (map gen-rect claims)))

(defn find-frequencies
  "rect의 각 좌표별 frequency들을 리스트로 반환합니다."
  [rect]
  (map point-frequencies rect))

(defn every-one?
  "`coll`의 모든 요소가 1인지 검사합니다."
  [coll]
  (every? #(= 1 %) coll))

(def part2
  (let [uniq-rects (remove #(let [rect (val %)
                                  freq (find-frequencies rect)]
                              ((complement every-one?) freq))
                           rects-by-id)]
    (-> uniq-rects first key)))

(comment
  (find-frequencies (gen-rect '(:x 208 :y 134 :w 3 :h 10)))
  (find-frequencies (gen-rect [1 3] [4 4])))