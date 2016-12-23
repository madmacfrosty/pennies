(ns pennies.visualise
  (:use [incanter core charts stats]))

(def joda->ms #(.getMillis %))
(def balance->pounds #(/ % 100.0))

(defn drawit
  [statement]
  (view (time-series-plot
         (map (comp joda->ms :date) statement)
         (map (comp balance->pounds :balance) statement)
         :title "Statement Balance"
         :x-label "Date"
         :y-label "£")
        :width 2500
        :height 1500))
