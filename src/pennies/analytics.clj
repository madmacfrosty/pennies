(ns pennies.analytics
  (:require
   [clj-time.core :as t]
   [clj-time.coerce :as c]))

(defn summary
  [statement]
  (let [{:keys [balance delta date]} (last statement)
        starting-balance (:balance (first statement))
        start-date (:date (first statement))
        duration (t/in-days (t/interval start-date date))]
    (hash-map
     :start-date start-date
     :starting-balance starting-balance
     :final-balance (+ balance delta)
     :end-date date
     :transactions (count statement)
     :days-spanned duration)))

(defn latest [& ts]
  (reduce #(if (t/after? %2 %1) %2 %1) (first ts) ts))

(defn earliest [& ts]
  (reduce #(if (t/before? %2 %1) %2 %1) (first ts) ts))


(defn balance-sheet
  [statements]
  (let [summaries (map summary statements)
        combined-start (apply latest (map :start-date summaries))
        combined-end (apply earliest (map :end-date summaries))
        combined-filter-fn (fn [statement] (take-while #((complement t/after?) (:date %) combined-end)
                                                      (drop-while #(t/before? (:date %) combined-start) statement))) 
        starting-balance (apply + (map (comp :balance first combined-filter-fn) statements))
        combined-transactions (sort-by (comp c/to-long :date) (mapcat combined-filter-fn (map (partial drop 1) statements)))
        transaction1 {:date combined-start :description "" :balance starting-balance :delta 0}]
    (cons transaction1 (second (reduce (fn [[balance txs] {:keys [date delta description]}]
                                         (let [new-balance (+ balance delta)]
                                           [new-balance (conj txs {:balance new-balance :date date :delta delta :description description})]))
                                       [starting-balance []] combined-transactions)))))

(defn histogram
  [transactions f bucket-size]
  (let [values (sort-by first (map (juxt f identity) transactions))
        lowest (ffirst values)
        bucket-index (fn [[k v]]
                       (let [bucket (quot (- k lowest) bucket-size)]
                         [bucket v]))
        buckets (group-by first (map bucket-index values))
        ks (sort (keys buckets))]
    (map #(map second (get buckets % [])) (range (first ks) (-> ks last inc)))))
