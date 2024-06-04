(ns godot-clojure.dev.test-utils
  (:require
   [malli.instrument :as mi]))

(defn make-malli-instrumentation-fixture
  [ns]
  (fn [f]
    (mi/collect! {:ns ns})
    (mi/instrument!)
    (f)
    (mi/unstrument!)))
