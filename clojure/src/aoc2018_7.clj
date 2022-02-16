(ns aoc2018-7
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def line-re #"Step (\w+) must be finished before step (\w+) can begin.")

(def input (-> (io/resource "aoc2018_7.txt")
               slurp
               str/split-lines))

(defn parse-input
  "입력에서 의존성에 따라 {A #{B C}}의 맵을 만듭니다."
  [lines]
  (let [pairs (map #(rest (re-matches line-re %)) lines)
        deps (->> pairs
                  (map (fn [[left right]] {right #{left}}))
                  (apply merge-with into))
        lefts (into #{} (map first pairs))
        rights (into #{} (map second pairs))
        entries (filter #(not (rights %)) lefts)]
    (apply conj deps (map (fn [en] {en #{}}) entries)))
  )

;; part 1

(defn find-entries
  "다른 작업에 의존성이 없는 시작 작업 목록을 반환합니다."
  [deps]
  (keys (filter #(empty? (val % )) deps)))

(defn drop-dep
  "의존성 목록에서 작업을 제거합니다."
  [deps job]
  (let [deps (->> deps
                  (map (fn [[left right]] {left (disj right job)}))
                  (into {}))]
    (dissoc deps job)))

(defn find-orders
  "{C #{F A}, A #{B D}, ...} -> [C A ...]"
  [deps]
  (loop [{:keys [deps
                 done]} {:deps deps
                         :done []}]
    (prn deps done)
    (if (empty? deps)
      done
      (let [entry (first (sort (find-entries deps)))
            rem (drop-dep deps entry)]
        (recur {:deps rem
                :done (conj done entry)})))))

(defn part1
  []
  (let [orders (-> input
                   parse-input
                   find-orders)]
    (apply str orders)))

;; part 2

(def base-time-taking 61)
(def workers 5)

(defn job-time
  "작업의 소요시간을 반환합니다."
  [job]
  (-> (int (first job))
      (as-> t (- t (int \A)))
      (as-> t (+ t base-time-taking))))

(defn alloc-jobs
  "의존성 맵에서 가용한 작업자수 만큼 꺼냅니다."
  [deps working]
  (let [avail-workers (- workers (count working))
        entries (into #{} (find-entries deps))
        avail-entries (->> working
                           (clojure.set/difference entries)
                           (take avail-workers))]
    (apply conj working avail-entries)))

(defn assign-time
  "작업 항목에 작업 소요시간을 쌍으로 반환합니다."
  [working]
  (->> working
       (map (fn [w] {w (job-time w)}))
       (into {})))

(defn elapse-work
  "진행중인 작업에 시간을 진척시키고 시간이 소진된 작업은 제거합니다."
  [work-with-time time-spending]
  (->> work-with-time
       (map (fn [[w t]] {w (- t time-spending)}))
       (filter #(> (val (first %)) 0))
       (into {})))

(defn do-work
  "의존성순서에 따라 작업을 진행하고 총 시간을 반환합니다."
  [deps]
  (loop [{:keys [deps
                 work-progressing
                 time-elaped]} {:deps deps
                                :work-progressing {} ; {job time-remaining}
                                :time-elaped 0}]
    (prn deps work-progressing)
    (let [time-dt (if (empty? work-progressing) 0 (apply min (map val work-progressing)))
          time-elaped (+ time-elaped time-dt)
          work-elapsed (elapse-work work-progressing time-dt)
          done (remove (set (keys work-elapsed)) (keys work-progressing))
          deps (reduce (fn [deps job] (drop-dep deps job))
                       deps
                       done)]
      (prn time-dt time-elaped work-elapsed work-elapsed)
      (if (and (empty? deps) (empty? work-elapsed))
        time-elaped
        (let [new-works (-> (alloc-jobs deps (keys work-elapsed))
                            assign-time)]
          (recur {:deps deps
                  :work-progressing (merge new-works work-elapsed)
                  :time-elaped time-elaped}))))))

(defn part2
  []
  (-> (parse-input input)
      (do-work)))

(comment
  (job-time "Z")
  (parse-input input))
