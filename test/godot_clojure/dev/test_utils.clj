(ns godot-clojure.dev.test-utils
  (:require
   [godot-clojure.dev.gdextension-interface-interpreter :as gii]
   [malli.instrument :as mi]))

(defn make-ast-loader-fixture [v]
  (fn [f]
    (alter-var-root v (constantly (#'gii/read-gdextension-interface-ast!)))
    (f)))

(defn make-malli-instrumentation-fixture
  [ns]
  (fn [f]
    (mi/collect! {:ns ns})
    (mi/instrument!)
    (f)
    (mi/unstrument!)))
