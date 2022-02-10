(ns aoc2018-4
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def input (-> (io/resource "aoc2018_4.txt")
               slurp
               str/split-lines
               sort))

(def line-re #"^\[\d+-(\d+)-(\d+) (\d+):(\d+)\] ([\w]+) ?\#?(\d+)?.+")

(defn to-int [s]
  (when (not (nil? s)) (Integer. s)))

(defn parse-events [lines]
  (map
   #(let [[_ & args] (re-matches line-re %)
          [m d hour min action guard] args]
      {:m (to-int m)
       :d (to-int d)
       :hour (to-int hour)
       :min (to-int min)
       :action (keyword (str/lower-case action))
       :guard (to-int guard)})
   lines))

(comment
  (re-matches line-re "[1518-05-26 23:59] Guard #71 begins shift")
  (re-matches line-re "[1518-10-24 00:52] falls asleep")
  (parse-events input))

;; 파트 1
;; 입력:

;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up
;; [1518-11-01 00:30] falls asleep
;; [1518-11-01 00:55] wakes up
;; [1518-11-01 23:58] Guard #99 begins shift
;; [1518-11-02 00:40] falls asleep
;; [1518-11-02 00:50] wakes up
;; [1518-11-03 00:05] Guard #10 begins shift
;; [1518-11-03 00:24] falls asleep
;; [1518-11-03 00:29] wakes up
;; [1518-11-04 00:02] Guard #99 begins shift
;; [1518-11-04 00:36] falls asleep
;; [1518-11-04 00:46] wakes up
;; [1518-11-05 00:03] Guard #99 begins shift
;; [1518-11-05 00:45] falls asleep
;; [1518-11-05 00:55] wakes up

;; 키워드: 가드(Guard) 번호, 자는 시간(falls asleep), 일어나는 시간(wakes up).
;; 각 가드들은 교대 근무를 시작하고 (begins shift) 졸았다가 일어났다를 반복함.
;; 위의 예시에서 10번 가드는 0시 5분에 잤다가 25분에 일어나고, 또 0시 30분에 잠들었다가 0시 55분에 깨어남.
;; 가드들에 대해서 자고 깨는 시간 정보들이 입력으로 주어짐.

;; 파트 1은 “주어진 입력에 대해서, 가장 오랜시간 잠들어있었던 가드의 ID와, 그 가드가 가장 빈번하게 잠들어 있었던 분(minute)의 곱을 구하라”
;; 만약 20번 가드가 0시 10분~36분, 2시 5분~11분, 3시 11분~13분 이렇게 잠들어 있었다면, “11분“이 가장 빈번하게 잠들어 있던 ‘분’. 그럼 답은 20 * 11 = 220.

(defn group-by-guard
  "이벤트들을 파싱해서 가드별 수면 시간들을 모읍니다."
  [events]
  (let [guard-sleeps (reduce (fn [{:keys [cur-guard] :as acc}
                                  {:keys [action guard min]}]
                               (case action
                                 :guard (assoc acc :cur-guard guard)
                                 :falls (assoc acc :falls min)
                                 :wakes (update acc cur-guard #(conj % [(:falls acc) min]))))
                             {}
                             events)]
    (dissoc guard-sleeps :cur-guard :falls)))

(defn sleepiest-minute
  "수면 시작 종료 쌍에서에 가장 빈번히 잠든 분(minute)과 그 시간의 쌍을 구합니다"
  [sleep-time-pairs]
  (let [minutes (map #(range (first %) (second %)) sleep-time-pairs)
        minute-frequencies (-> minutes
                               flatten
                               frequencies)]
    (->> minute-frequencies
         (sort-by val >)
         first)))

(defn sleep-sum
  "자고 일어난 시간 쌍의 목록에서 수면시간의 합을 구합니다."
  [pairs]
  (->> pairs
       (map (fn [[fall wake]] (- wake fall)))
       (apply +)))
  ;(reduce (fn [acc [fall wake]]
  ;          (+ acc (- wake fall))) 0 pairs))

(def part1
  (let [events (parse-events input)
        sleeps-by-guard (group-by-guard events)
        sleep-sums (for [[guard sleeps] sleeps-by-guard]
                     {:guard guard :sum (sleep-sum sleeps)})
        sleepiest-guard (-> (sort-by :sum > sleep-sums)
                            first
                            :guard)
        minute (first (sleepiest-minute (sleeps-by-guard sleepiest-guard)))]
    (* sleepiest-guard minute)))


(comment
  (sleepiest-minute '([24 29] [30 55] [5 25]))
  (sleep-sum '([24 29] [30 55] [5 25]))

  (for [[guard sleeps] (group-by-guard (parse-events input))]
    {:guard guard :sum (sleep-sum sleeps)}))

;; 파트 2
;; 주어진 분(minute)에 가장 많이 잠들어 있던 가드의 ID과 그 분(minute)을 곱한 값을 구하라.

(def part2
  (let [events (parse-events input)
        sleeps-by-guard (group-by-guard events)
        sleepiest-by-guard (for [[guard sleeps] sleeps-by-guard]
                             {:guard guard :sleepiest (sleepiest-minute sleeps)})
        {:keys [guard sleepiest]} (first (sort-by
                                          #(-> % :sleepiest second)
                                          >
                                          sleepiest-by-guard))
        minute (first sleepiest)]
    (* guard minute)))

(comment
  (for [[guard sleeps] (group-by-guard (parse-events input))]
    {:guard guard :sleepiest (sleepiest-minute sleeps)}))
