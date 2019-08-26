(ns foo.events
  (:require [clojure.string :as str]
            [foo.zoo :refer [zoo-fn]]))

(defn start-event
  [db [_ start-event-arg]]
  (assoc db :state state-event-arg))

(def max-value 10)

(def foo 1)
