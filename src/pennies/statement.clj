(ns pennies.statement
  (:require [clj-time.core :as t]))

(defn- chain-summary
  [x]
  (when (seq x)
    (let [day1 (-> x first :date)
          dayn (-> x last :date)
          sameday? #(= %1 (:date %2))
          day1-tx? (partial sameday? day1)
          dayn-tx? (partial sameday? dayn)
          day1-n (count (take-while day1-tx? x))
          dayn-n (count (take-while dayn-tx? (reverse x)))]
      [day1 day1-n dayn dayn-n])))

(defn- earlier?
  "Checks whether the first chain starts earlier or same day with more transactions"
  [[xd1 xd1n _ _] [yd1 yd1n _ _]]
  (or (t/before? xd1 yd1)
      (and (t/equal? xd1 yd1)
           (> xd1n yd1n))))

(defn- later?
  "Checks whether the first chain ends later or same day with more transactions"
  [[_ _ xdn xdnn] [_ _ ydn ydnn]]
  (or (t/after? xdn ydn)
      (and (t/equal? xdn ydn)
           (> xdnn ydnn))))

(defn- subsume
  "Given x can subsume y, if y is a subsequence of x then returns x, otherwise nil"
  [x y]
  (let [begin (drop-while #(not= % (first y)) x)]
    (when (and (<= (count y) (count begin))
               (every? true? (map = y begin)))
      x)))

(defn- overlap
  "Given x begins before y, merge if there exists a common sequence stretching from the start or end of x or y"
  [x y]
  {:pre [(seq x)
         (seq y)
         (not (apply t/after? (map (comp :date first) [x y])))]}
  (let [[prefix begin] (split-with #(not= % (first y)) x)
        increasing #(if (apply <= (map count [%1 %2])) [%1 %2] [%2 %1])
        [shorter longer] (increasing begin y)
        [common suffix] (split-at (count shorter) longer)]
    (when (and (seq common)
               (every? true? (map = common shorter)))
      (concat prefix common suffix))))

(defn merge-chains
  "Merge two chains if they overlap - otherwise nil"
  [x y]
  (when (and (seq x) (seq y))
    (let [xs (chain-summary x)
          ys (chain-summary y)]
      (cond
       (and (earlier? xs ys) (later? ys xs)) (overlap x y)
       (earlier? xs ys) (subsume x y)
       (and (earlier? ys xs) (later? xs ys)) (overlap y x)
       (earlier? ys xs) (subsume y x)
       (later? xs ys) (subsume x y)
       (later? ys xs) (subsume y x)
       :else (subsume x y)))))


;; Take first element and try to merge
;; If success then repeat
;; If failure then add element to result

(defn preceding-transaction-matcher
  "Takes a transaction and returns a function taking another transaction
   Checks the second target is a valid predecessor and returns
   - Days between the transactions if valid
   - nil otherwise"
  [{:keys [delta balance date]}]
  (fn [transaction]
    (when (and (= (:balance transaction) (- balance delta))
             (not (t/after? (:date transaction) date)))
      (t/in-days (t/interval (:date transaction) date)))))

(defn adjoin
  [before after]
  (let [{ydelta :delta ybalance :balance} (first after)
        expected (- ybalance ydelta)
        target (:balance (last before))]
    (when (= target expected)
      (concat before after))))

(defn merge-adjoining-chains
  [x y]
  (when (and (seq x) (seq y))
    (let [xs (chain-summary x)
          ys (chain-summary y)]
      (cond (earlier? xs ys) (adjoin x y)
            (earlier? ys xs) (adjoin y x)
            :else nil))))

(defn merge-statements
  ([coll]
     (let [overlaps (merge-statements coll merge-chains)]
       (merge-statements overlaps merge-adjoining-chains)))
  ([coll f]
     (loop [result []
            current coll]
       (if (< (count current) 2)
         (concat result current)
         (let [[x & xs] current
               transformed (map (partial f x) xs)]
           (if (empty? (filter seq transformed))
             (recur (conj result x) xs)
             (recur result (map #(or %1 %2) transformed xs))))))))




