(ns godot-clojure.dev.test-utils
  (:require
   [godot-clojure.dev.gdextension-interface-interpreter :as gii]))

(defn make-ast-loader-fixture [v]
  (fn [f]
    (alter-var-root v (constantly (#'gii/read-gdextension-interface-ast!)))
    (f)))
