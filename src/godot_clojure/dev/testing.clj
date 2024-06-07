(ns godot-clojure.dev.testing
  "Utilities to aid testing."
  (:require
   [godot-clojure.native-caller-test :as t]
   [kaocha.repl :as k]))

(defn run-godot-tests!
  "Run tests that rely on Godot's presence."
  [p-get-proc-address]
  ;; I think this function is called several times
  (alter-var-root #'t/p-get-proc-address #(or % p-get-proc-address))
  ;; A hack to turn t into fully qualified namespace
  (k/run (symbol (namespace ::t/foo))))
