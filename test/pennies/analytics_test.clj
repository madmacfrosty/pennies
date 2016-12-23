(ns pennies.analytics-test
  (:require
   [clojure.test :refer :all]
   [pennies.analytics :refer :all]
   [pennies.statement-test :refer [create-statement]]))

(deftest histogram-test
  (testing "histogram deltas"
    (let [statement (create-statement 100)
          histogram (histogram statement :delta 5)]
      (is (= 100 (apply + (map count histogram)))))))
