(ns godot-clojure.dev.testing
  "Utilities to aid testing."
  (:require
   [kaocha.repl :as k]))

(def p-get-proc-address nil)

(defn get-p-get-proc-address! []
  (when (nil? p-get-proc-address)
    (throw (Exception. "Cannot run tests because `p-get-proc-address` is missing!")))
  p-get-proc-address)

(defn run-godot-tests!
  "Run tests that rely on Godot's presence."
  [p-get-proc-address-local]
  ;; I think this function is called several times
  (alter-var-root #'p-get-proc-address #(or % p-get-proc-address-local))
  (k/run-all {:kaocha.filter/focus [:godot]}))
