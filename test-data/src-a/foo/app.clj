(ns foo.app
  (:require [re-frame.core :refer [reg-event-db reg-fx ref-event-fx reg-sub]]
            [schema.core :as s]
            [clj-time core coerce]
            [mix a [b :as b] [c :refer [c->d c->e]]]
            [foo.events :as events :refer [max-value]])
  ;; some comments
  (:require [cljs-time :as t])
  (:import [foo.bar Baz]))


:eof

:schema.core/keyword-with-full-ns

::s/schema-option

{:foo :bar
 {:x 1} {:outer-val-key {:k :val}}}

;; (do
;;   (foo bar baz))

(reg-event-db
 :connection-status
 (fn [db [_ status]]
   (assoc db :connection-status status)))

[]

(reg-event-db
 :defined-elsewhere
 events/start-event)

(reg-event-db
 :handler-with-schema
 (s/fn [db :- {s/Any s/Any} ev :- [s/Any]]
   db))

(reg-sub
 :current-status
 (fn [db _] (get-in db [:status])))

(reg-sub
 events/defined-key
 (fn [db _] :OK))

;;; fooo.bar OR [foo :a]
(defn- libspec?
  "Returns true if x is a libspec"
  [x]
  (or (symbol? x)
      (and (vector? x)
           (or
            (nil? (second x))
            (keyword? (second x))))))

(defn- prependss
  "Prepends a symbol or a seq to coll"
  [x coll]
  (if (symbol? x)
    (cons x coll)
    (concat x coll)))

(defn x-load-lib [prefix lib & options]
  (println "X-LOAD-LIB: " prefix lib (vec options)))

(defn x-load-libs [& args]
  (let [flags (filter keyword? args)
        opts (interleave flags (repeat true))
        args (filter (complement keyword?) args)]
    (println "X-LOAD-LIBS: " (vec args))
    (doseq [arg args]
      (if (libspec? arg)
        (apply x-load-lib nil (prependss arg opts))
        (let [[prefix & args] arg]
          (when (nil? prefix) (throw (ex-info "prefix can't be nil")))
          (doseq [arg args]
            (apply x-load-lib prefix (prependss arg opts))))))))

(defn x-require [& args]
  (apply x-load-libs :require args))

{
 ;; comment for :foo entry
 :foo 1
 :bar 2
 ;; comment at the end
 }

{
 :xyz 1
 :qux 2
 ;; comment at the end
 }

;;; comment at the very end
