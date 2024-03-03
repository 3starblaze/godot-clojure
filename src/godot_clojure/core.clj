(ns godot-clojure.core
  (:gen-class)
  (:require
   [godot-clojure.native-caller :as native-caller]
   [insn.core :as insn]))

;; Example struct to check struct handling
(def godot-version-struct-info
  (let [int-field (fn [field-name]
                    {:flags [:public]
                     :type :int
                     :name field-name})]
    {:name 'godot-clojure.core.gen.structs
     :super 'com.sun.jna.Structure
     :annotations {com.sun.jna.Structure$FieldOrder
                   ["major" "minor" "patch" "string"]}
     :fields [(int-field "major")
              (int-field "minor")
              (int-field "patch")
              {:flags [:public]
               :type String
               :name "string"}]}))

(def godot-version-structure (insn/define godot-version-struct-info))

(defn entry-point [p-get-proc-address]
  (native-caller/init! p-get-proc-address)
  (let [payload (.newInstance godot-version-structure)]
    (native-caller/call! "get_godot_version" payload)
    (println "Reporting from Clojure!")
    (println (format "v%d.%d.%d -- %s"
                     (.-major payload)
                     (.-minor payload)
                     (.-patch payload)
                     (.-string payload)))
    (println "Report from Clojure is over!")))
