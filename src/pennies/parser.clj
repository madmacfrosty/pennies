(ns pennies.parser
  (:require
   [clj-time.format :as tf]
   [clojure.data.csv :as csv]
   [clojure.java.io :as io]
   [pennies.analytics :refer [summary balance-sheet]]
   [pennies.statement :refer [merge-statements]]
   [pennies.visualise :as visualise]))

(defn hsbc-statement
  [[date description amount balance]]
  (let [formatter (tf/formatter "dd/MM/YYYY")
        converter (comp #(Math/round %) (partial * 100) read-string)]
    (hash-map :date (tf/parse formatter date)
              :description description
              :delta (converter amount)
              :balance (converter balance))))

(def ^:private csv-parsers
  {["Date" "Description" "Amount" "Balance"] hsbc-statement})

(defn parse-statement
  [filename]
  (with-open [in (io/reader filename)]
    (try
      (let [rows (csv/read-csv in)
            header (first rows)
            parser (get csv-parsers header)]
        (when parser
          (mapv parser (reverse (drop 1 rows)))))
      (catch Throwable e
        (println filename)
        (println e)))))

(defn -main
  [& filenames]
  (let [statements (merge-statements (map parse-statement filenames))
        master (balance-sheet statements)]
    (doseq [statement statements]
      (println (summary statement)))
    (println (first master))
    (println (second master))
    (visualise/drawit master)))

