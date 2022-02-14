(ns aoc2018-7
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

(def line-re #"Step (\w+) must be finished before step (\w+) can begin.")

(defn read-input [] (-> (io/resource "aoc2018_7.txt")
                        slurp
                        str/split-lines))

(defn parse-input [lines]
  (->> lines
       (map #(re-matches line-re %))
       (map rest)
       #_(map (fn [[left right]] (list
                                (int (first (seq left)))
                                (int (first (seq right))))))))

;; part 1

(defn adder
  [acc nodes]
  (loop [acc acc
         nodes nodes
         len (count nodes)]
    (if (= (count acc) len)
      acc
      (recur (reduce (fn [acc [left right]]
                       (update acc left #(conj % right)))
                     acc
                     nodes)
             nodes
             len))))

(defn find-entries
  [deps]
  (let [lefts (into #{} (keys deps))
        rights (apply clojure.set/union (vals deps))]
    (->> lefts
         (filter #(not (rights %)))
         sort)))

(comment
  (let [input (parse-input (read-input))
        deps (reduce (fn [acc [left right]]
                       (update
                        acc
                        left
                        #(if % (conj % right) #{right})))
                     {} input)
        lasts (sort (clojure.set/difference
               (apply clojure.set/union (vals deps))
               (into #{} (keys deps))))]
    (loop [{:keys [remaining
                   done
                   entries] :as acc} {:remaining deps
                                      :done []
                                      :entries (find-entries deps)}]
      (println acc)
      (if (empty? remaining)
        (apply conj done lasts)
        (recur (reduce (fn [{:keys [remaining done]}
                            entry]
                         {:remaining (dissoc remaining entry)
                          :done (conj done entry)
                          :entries (find-entries (dissoc remaining entry))})
                 acc
                 entries)))))

  (parse-input (read-input))
  (zipmap (distinct (flatten (parse-input (read-input)))) (repeat nil))
  )

(comment
  )

