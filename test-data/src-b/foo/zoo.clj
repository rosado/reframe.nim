(ns foo.zoo)

;;; this is imported in another ns, but before this file is read and
;;; added to the env, so the 'easy' symbol resolution will fail
(defn zoo-fn [] :ok)

