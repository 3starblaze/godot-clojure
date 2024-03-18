(ns godot-clojure.dev.gdextension-interface-interpreter-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [malli.core :as m]
   [godot-clojure.dev.gdextension-interface-interpreter :as gii]))

(def ast nil)

(defn ast-loader-fixture [f]
  (alter-var-root #'ast (constantly (#'gii/read-gdextension-interface-ast!)))
  (f))

(use-fixtures :once ast-loader-fixture)

(deftest ast->header-info-test []
  (let [info (#'gii/ast->header-info ast)]
    (testing "Header info matches the schema"
      (is (nil? (m/explain gii/header-info-schema info))))
    (testing "Test that no field is empty (sanity check)"
      (is (not (empty? (:functions info))))
      (is (not (empty? (:structs info))))
      (is (not (empty? (:enums info))))
      (is (not (empty? (:irreducible-type-mapping info)))))
    (testing "Check concrete examples"
      (is (= (->> info :functions (filter #(= (:name %) "variant_call")) first)
             {:name "variant_call",
              :since "4.1",
              :params
              (seq [{:name "p_self",
                     :type "GDExtensionVariantPtr",
                     :doc "A pointer to the Variant."}
                    {:name "p_method",
                     :type "GDExtensionConstStringNamePtr",
                     :doc "A pointer to a StringName identifying the method."}
                    {:name "p_args",
                     :type "const GDExtensionConstVariantPtr *",
                     :doc "A pointer to a C array of Variant."}
                    {:name "p_argument_count",
                     :type "GDExtensionInt",
                     :doc "The number of arguments."}
                    {:name "r_return",
                     :type "GDExtensionUninitializedVariantPtr",
                     :doc "A pointer a Variant which will be assigned the return value."}
                    {:name "r_error",
                     :type "GDExtensionCallError *",
                     :doc "A pointer the structure which will hold error information."}]),
              :return "void"}))
      (is (= (->> info :structs (filter #(= (:name %) "GDExtensionPropertyInfo")) first)
             {:name "GDExtensionPropertyInfo",
              :members
              (seq [{:name "type", :type "GDExtensionVariantType"}
                    {:name "name", :type "GDExtensionStringNamePtr"}
                    {:name "class_name", :type "GDExtensionStringNamePtr"}
                    {:name "hint", :type "uint32_t"}
                    {:name "hint_string", :type "GDExtensionStringPtr"}
                    {:name "usage", :type "uint32_t"}])}))
      (is (= (->> info :enums (filter #(= (:name %) "GDExtensionCallErrorType")) first)
             {:name "GDExtensionCallErrorType",
              :options
              (seq [{:name "GDEXTENSION_CALL_OK", :value 0}
                    {:name "GDEXTENSION_CALL_ERROR_INVALID_METHOD", :value 1}
                    {:name "GDEXTENSION_CALL_ERROR_INVALID_ARGUMENT", :value 2}
                    {:name "GDEXTENSION_CALL_ERROR_TOO_MANY_ARGUMENTS", :value 3}
                    {:name "GDEXTENSION_CALL_ERROR_TOO_FEW_ARGUMENTS", :value 4}
                    {:name "GDEXTENSION_CALL_ERROR_INSTANCE_IS_NULL", :value 5}
                    {:name "GDEXTENSION_CALL_ERROR_METHOD_NOT_CONST", :value 6}])})
          "Implicit enum values should be read correctly")
      (is (= (->> info :enums (filter #(= (:name %) "GDExtensionClassMethodFlags")) first)
             {:name "GDExtensionClassMethodFlags",
              :options
              (seq [{:name "GDEXTENSION_METHOD_FLAG_NORMAL", :value 1}
                    {:name "GDEXTENSION_METHOD_FLAG_EDITOR", :value 2}
                    {:name "GDEXTENSION_METHOD_FLAG_CONST", :value 4}
                    {:name "GDEXTENSION_METHOD_FLAG_VIRTUAL", :value 8}
                    {:name "GDEXTENSION_METHOD_FLAG_VARARG", :value 16}
                    {:name "GDEXTENSION_METHOD_FLAG_STATIC", :value 32}
                    {:name "GDEXTENSION_METHOD_FLAGS_DEFAULT", :value 1}])})
          "Explicit enum values should be read correctly")
      (is (= (get-in info [:irreducible-type-mapping "GDExtensionRefPtr"]) "void *"))
      (is (= (get-in info [:irreducible-type-mapping "GDExtensionConstObjectPtr"]) "const void *")
          "const modifier should not be dropped"))))
