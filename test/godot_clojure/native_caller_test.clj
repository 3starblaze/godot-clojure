(ns godot-clojure.native-caller-test
  (:require
   [clojure.test :refer [deftest is]]
   [godot-clojure.dev.testing :refer [get-p-get-proc-address!]]
   [godot-clojure.native-caller :as native-caller]
   [insn.core :as insn]))

(deftest ^:godot unsafe-get-godot-version-test []
  (let [int-field (fn [field-name]
                    {:flags [:public]
                     :type :int
                     :name field-name})
        insn-info {:name 'godot-clojure.core.gen.struct.GDExtensionInterfaceGetGodotVersion
                   :super 'com.sun.jna.Structure
                   :annotations {com.sun.jna.Structure$FieldOrder
                                 ["major" "minor" "patch" "string"]}
                   :fields [(int-field "major")
                            (int-field "minor")
                            (int-field "patch")
                            {:flags [:public]
                             :type String
                             :name "string"}]}
        struct-pointer (-> insn-info insn/define .newInstance)]
    (native-caller/init! (get-p-get-proc-address!))
    (native-caller/unsafe-call! "get_godot_version" Void struct-pointer)
    (is (= (.-major struct-pointer) 4))
    (is (= (.-minor struct-pointer) 2))
    (is (= (.-patch struct-pointer) 1))))
