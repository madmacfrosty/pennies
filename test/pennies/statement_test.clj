(ns pennies.statement-test
  (:require [clj-time.core :as t]
            [clojure.test :refer :all]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.rose-tree :as rose]
            [clojure.test.check.properties :as prop]
            [pennies.statement :refer [merge-statements merge-chains preceding-transaction-matcher]]))

(defn create-transaction-chain
  "Takes a collections of tuples where:
   first is a day increment
   second is a cash adjustment in pence"
  ([cashflows] (create-transaction-chain (t/today-at 12 00) 0 cashflows))
  ([start-date start-balance cashflows]
     (let [adjust-balance #(-> %1 :balance (+ %2))
           inc-date #(t/plus (:date %1) (t/days %2))
           add-cashflows (fn [acc [day delta]]
                           (assoc acc
                             :balance (adjust-balance acc delta)
                             :delta delta
                             :date (inc-date acc day)))]
       (drop 1 (reductions add-cashflows {:balance start-balance :date start-date :delta 0} cashflows)))))

(defn create-statement
  [size]
  (let [cashflow-gen (gen/tuple (gen/choose 0 10) gen/int)]
    (create-transaction-chain (gen/sample cashflow-gen size))))

(defn statement-fragment-fgen
  ([statement min-fragment-size] (statement-fragment-fgen statement min-fragment-size (count statement)))
  ([statement min-fragment-size max-fragment-size]
     (gen/let [result-sz (gen/choose min-fragment-size max-fragment-size)
               discard-sz (gen/choose 0 (- (count statement)  result-sz))]
       (take result-sz (drop discard-sz statement)))))

(deftest merge-overlapping-chains-test
  (testing "Verify two statements with an overlap always merge back to one"
    (let [size 100
          statement (create-statement size)
          fragment-gen (statement-fragment-fgen statement (inc (/ size 2)))
          tests (partition 2 (gen/sample fragment-gen 1000))
          results (map (partial apply merge-chains) tests)]
      (is (every? seq results)))))

(deftest ^:integration merge-non-overlapping-chains-test
  (testing "As statement size increases, success rate merges towards frag-size * 2 / statement-size"
    (let [size 10000
          frag-size 100
          statement (create-statement size)
          fragment-gen (statement-fragment-fgen statement frag-size frag-size)
          tests (partition 2 (gen/sample fragment-gen 100000))
          results (map (partial apply merge-chains) tests)
          success-rate (/ (count (filter seq? results)) (count results))
          expected (/ (* 2 frag-size) size)
          tolerance (* 0.2 expected)
          _ (println (double success-rate))]
      (is (< (- expected tolerance) success-rate (+ expected tolerance)))))
  (testing "Empty statements"
    (let [size 100
          statement (create-statement size)]
      (is nil? (merge-chains statement []))
      (is nil? (merge-chains [] statement)))))

(deftest merge-statements-test
  (testing "Verify two statements with an overlap always merge back to one"
    (let [size 100
          statement (create-statement size)
          fragment-gen (statement-fragment-fgen statement (inc (/ size 2)))
          result (merge-statements (gen/sample fragment-gen 100))]
      (is (= 1 (count result))))))

(deftest adjoining-statement-test
  (testing ""
    (let [statement (create-statement 100)
          result (merge-statements (partition 10 statement))]
      (is (= 1 (count result)))
      (is (= statement (first result))))))


