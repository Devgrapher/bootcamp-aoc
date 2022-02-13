(ns aoc2018-6
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(defn read-input [] (-> (io/resource "aoc2018_6.txt")
                        slurp
                        str/split-lines))

(defn apply-xs-and-ys
  "coords에서 x좌표들과 y좌표들에 fn을 적용한 결과를 pair로 리턴합니다."
  [fn coords]
  (let [[xs ys] (apply map list coords)]
    (list (apply fn xs) (apply fn ys))))

(def min-coord (partial apply-xs-and-ys min))
(def max-coord (partial apply-xs-and-ys max))

(defn parse-coords
  [input-lines]
  (let [coords (->> input-lines
                    (map #(str/split % #", "))
                    (map (fn [[x y]] (vector (Integer. x) (Integer. y)))))
        [min-x min-y] (min-coord coords)
        [max-x max-y] (max-coord coords)]
    {:min-x min-x
     :min-y min-y
     :max-x max-x
     :max-y max-y
     :coords coords}))

;; 파트 1
;; 입력 : 좌표의 쌍이 N개 주어짐

;; 1, 1
;; 1, 6
;; 8, 3
;; 3, 4
;; 5, 5
;; 8, 9

;; 각 점은 1 tick이 지날때 마다 상,하,좌,우로 증식함.


;;  ..........
;;  .A........
;;  ..........
;;  ........C.
;;  ...D......
;;  .....E....
;;  .B........
;;  ..........
;;  ..........
;;  ........F.


;;  aaaaa.cccc
;;  aAaaa.cccc
;;  aaaddecccc
;;  aadddeccCc
;;  ..dDdeeccc
;;  bb.deEeecc
;;  bBb.eeee..
;;  bbb.eeefff
;;  bbb.eeffff
;;  bbb.ffffFf


;; 여기서 . 으로 표기된 부분은 각 출발 지점으로부터 '같은 거리'에 있는 부분을 뜻함.
;; 맵 크기에는 제한이 없어 무한으로 뻗어나간다고 할 때, 가장 큰 유한한 면적의 크기를 반환 (part-1)

(defn abs [n] (max n (- n)))

(defn distance
  "맨해튼 거리를 계산합니다."
  [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn min-dist-butsame
  "좌표별 거리가 담긴 맵 리스트에서 가장 짧은 거리만 남깁니다."
  [coords-with-dist]
  (let [sorted (sort-by :dist coords-with-dist)]
    (when (not= (-> sorted first :dist) (-> sorted second :dist))
      (first sorted))))

(defn gen-distances-on-grid
  "그리드상 모든 좌표에서 주어진 좌표까지의 거리들을 계산합니다."
  [{:keys [min-x min-y max-x max-y coords] :as input}]
  (update input :coords
          (fn [_] (for [x (range min-x (inc max-x))
                        y (range min-y (inc max-y))
                        :let [distances (map #(hash-map :coord % :dist (distance [x y] %)) coords)]]
                    (list [x y] distances)))))

(defn find-perimeters
  "주어진 그리드내에서 경계에 위치한 좌표 set을 구합니다."
  [[left top] [right bottom]]
  (set (for [x (range left (inc right))
             y (range top (inc bottom))
             :when (or (= left x) (= right x) (= top y) (= bottom y))]
         [x y])))

(defn drop-coords-with-perimeters
  "그리드내 경계좌표를 포함하는 좌표는 무한대이므로 제외합니다."
  [{:keys [min-x min-y max-x max-y coords]}]
  (let [perimeters (find-perimeters [min-x min-y] [max-x max-y])]
    (->> coords
         (map (fn [[xy dists]] (list xy (min-dist-butsame dists))))
         (group-by #(-> % second :coord))
         (filter (fn [[_ xy_dists]] (not-any? #(perimeters (first %)) xy_dists))))))

(defn max-area
  "그리드 내에 가장 많은 영역을 가진 좌표를 구합니다."
  [coords_with_dists]
  (->> coords_with_dists
       (map (fn [[coord dists]] (list coord (count dists))))
       (apply max-key second)
       second))

(defn part1
  []
  (-> (read-input)
      parse-coords
      gen-distances-on-grid
      drop-coords-with-perimeters
      max-area))

;; 파트 2
;; 안전(safe) 한 지역은 근원지'들'로부터의 맨하탄거리(Manhattan distance, 격자를 상하좌우로만 움직일때의 최단 거리)의 '합'이 N 이하인 지역임.

;;  ..........
;;  .A........
;;  ..........
;;  ...###..C.
;;  ..#D###...
;;  ..###E#...
;;  .B.###....
;;  ..........
;;  ..........
;;  ........F.

;; Distance to coordinate A: abs(4-1) + abs(3-1) =  5
;; Distance to coordinate B: abs(4-1) + abs(3-6) =  6
;; Distance to coordinate C: abs(4-8) + abs(3-3) =  4
;; Distance to coordinate D: abs(4-3) + abs(3-4) =  2
;; Distance to coordinate E: abs(4-5) + abs(3-5) =  3
;; Distance to coordinate F: abs(4-8) + abs(3-9) = 10
;; Total distance: 5 + 6 + 4 + 2 + 3 + 10 = 30

;; N이 10000 미만인 안전한 지역의 사이즈를 구하시오.

(def max-safe-distance 10000)

(defn sum-distances
  [distances]
  (reduce #(+ (:dist %2) %1) 0 distances))

(defn part2
  []
  (let [{:keys [coords]} (-> (read-input)
                             parse-coords
                             gen-distances-on-grid)]
      (->> coords
           (map (fn [[_xy dists]] (sum-distances dists)))
           (filter #(< % max-safe-distance))
           count)))

(comment
  (distance [1 1] [3 3]))
