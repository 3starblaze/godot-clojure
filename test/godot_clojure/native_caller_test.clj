(ns godot-clojure.native-caller-test
  (:require
   [clojure.test :refer [deftest is]]
   [godot-clojure.dev.testing :refer [get-p-get-proc-address!]]
   [godot-clojure.native-caller :as native-caller]
   [insn.core :as insn])
  (:import
   [com.sun.jna Function Memory Pointer]))

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

(deftest ^:godot utility-function-call-test []
  (native-caller/init! (get-p-get-proc-address!))
  (let [maxi-string (native-caller/java-string->string-name "maxi")
        maxi (native-caller/call! "variant_get_ptr_utility_function" maxi-string 3133453818)
        ;; HACK: Assuming Godot integer type (arg0, arg1, res)
        arg0 (com.sun.jna.ptr.LongByReference. 7)
        arg1 (com.sun.jna.ptr.LongByReference. 11)
        ;; HACK: I assumed that sizeof(void*) == 8, not sure how to get this info via JNA
        args (doto (Memory. 16)
               (.setLong 0 (-> arg0 .getPointer Pointer/nativeValue))
               (.setLong 8 (-> arg1 .getPointer Pointer/nativeValue)))
        res (com.sun.jna.ptr.LongByReference.)]
    (-> maxi
        Function/getFunction
        (.invoke Void (to-array [res args (int 2)])))
    (is (= (.getValue res) 11))))
