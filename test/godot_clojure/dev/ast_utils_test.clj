(ns godot-clojure.dev.ast-utils-test
  (:require
   [clojure.test :refer [deftest is use-fixtures]]
   [godot-clojure.dev.ast-utils :as ast-utils]
   [godot-clojure.dev.test-utils :as u]))

(def registry @ast-utils/gd-extension-type-registry)

(use-fixtures :once
  (u/make-malli-instrumentation-fixture 'godot-clojure.dev.ast-utils))

(deftest registry-sanity-checks []
  (is (contains? (::ast-utils/lib-fn-registry registry) "get_godot_version")
      "`get_godot_version`` should exist in the registry")
  (is (=
       (->> (::ast-utils/type-registry registry)
            vals
            (map ::ast-utils/gd-extension-type-type)
            set)
       #{::ast-utils/struct
         ::ast-utils/fn
         ::ast-utils/lib-fn
         ::ast-utils/enum
         ;; Nameless pointer cannot be in registry. If a pointer is typedef'd, it's an atomic type.
         #_::ast-utils/pointer
         ::ast-utils/atomic-type})
      "Registry does not contain all types of gd-extension-type-type")
  (is (=
       (get-in registry [::ast-utils/lib-fn-registry "variant_call"])
       #::ast-utils {:name "GDExtensionInterfaceVariantCall"
                     :gd-extension-type-type ::ast-utils/lib-fn
                     :return-type #::ast-utils {:gd-extension-type-type ::ast-utils/atomic-type
                                                :name "void"}
                     :args (seq [#::ast-utils {:gd-extension-type-type ::ast-utils/atomic-type
                                               :name "GDExtensionVariantPtr"}
                                 #::ast-utils {:gd-extension-type-type ::ast-utils/atomic-type
                                               :name "GDExtensionConstStringNamePtr"}
                                 #::ast-utils {:gd-extension-type-type ::ast-utils/pointer
                                               :pointed-type-const? true
                                               :pointed-type #::ast-utils {:gd-extension-type-type ::ast-utils/atomic-type,
                                                                           :name "GDExtensionConstVariantPtr"}}
                                 #::ast-utils {:gd-extension-type-type ::ast-utils/atomic-type
                                               :name "GDExtensionInt"}
                                 #::ast-utils {:gd-extension-type-type ::ast-utils/atomic-type
                                               :name "GDExtensionUninitializedVariantPtr"}
                                 #::ast-utils {:gd-extension-type-type ::ast-utils/pointer
                                               :pointed-type-const? false
                                               :pointed-type #::ast-utils {:gd-extension-type-type ::ast-utils/atomic-type
                                                                           :name "GDExtensionCallError"}}])
                     :lib-name "variant_call"
                     :since "4.1"
                     :args-docs (seq
                                 [#::ast-utils {:name "p_self"
                                                :doc "A pointer to the Variant."}
                                  #::ast-utils {:name "p_method"
                                                :doc "A pointer to a StringName identifying the method."}
                                  #::ast-utils {:name "p_args"
                                                :doc "A pointer to a C array of Variant."}
                                  #::ast-utils {:name "p_argument_count"
                                                :doc "The number of arguments."}
                                  #::ast-utils {:name "r_return"
                                                :doc "A pointer a Variant which will be assigned the return value."}
                                  #::ast-utils {:name "r_error"
                                                :doc "A pointer the structure which will hold error information."}])})
      "Library function `variant_call` should match")
  (is (=
       (get-in registry [::ast-utils/type-registry "GDExtensionPropertyInfo"])
       #::ast-utils {:gd-extension-type-type ::ast-utils/struct
                     :name "GDExtensionPropertyInfo"
                     :members (seq
                               [#::ast-utils {:name "type"
                                              :type #::ast-utils {:gd-extension-type-type ::ast-utils/atomic-type
                                                                  :name "GDExtensionVariantType"}}
                                #::ast-utils {:name "name"
                                              :type #::ast-utils {:gd-extension-type-type ::ast-utils/atomic-type
                                                                  :name "GDExtensionStringNamePtr"}}
                                #::ast-utils {:name "class_name"
                                              :type #::ast-utils{:gd-extension-type-type ::ast-utils/atomic-type
                                                                 :name "GDExtensionStringNamePtr"}}
                                #::ast-utils {:name "hint"
                                              :type #::ast-utils{:gd-extension-type-type ::ast-utils/atomic-type
                                                                 :name "uint32_t"}}
                                #::ast-utils {:name "hint_string"
                                              :type #::ast-utils{:gd-extension-type-type ::ast-utils/atomic-type
                                                                 :name "GDExtensionStringPtr"}}
                                #::ast-utils {:name "usage"
                                              :type #::ast-utils{:gd-extension-type-type ::ast-utils/atomic-type
                                                                 :name "uint32_t"}}])})
      "Struct `GDExtensionPropertyInfo` should match")
  (is (=
       (get-in registry [::ast-utils/type-registry "GDExtensionCallErrorType"])
       #::ast-utils {:gd-extension-type-type
                     ::ast-utils/enum

                     :name
                     "GDExtensionCallErrorType",

                     :members
                     (seq [#::ast-utils {:name "GDEXTENSION_CALL_OK", :value 0}
                           #::ast-utils {:name "GDEXTENSION_CALL_ERROR_INVALID_METHOD", :value 1}
                           #::ast-utils {:name "GDEXTENSION_CALL_ERROR_INVALID_ARGUMENT", :value 2}
                           #::ast-utils {:name "GDEXTENSION_CALL_ERROR_TOO_MANY_ARGUMENTS", :value 3}
                           #::ast-utils {:name "GDEXTENSION_CALL_ERROR_TOO_FEW_ARGUMENTS", :value 4}
                           #::ast-utils {:name "GDEXTENSION_CALL_ERROR_INSTANCE_IS_NULL", :value 5}
                           #::ast-utils {:name "GDEXTENSION_CALL_ERROR_METHOD_NOT_CONST", :value 6}])})
      "Implicit enum should be interpreted correctly")
  (is (=
       (get-in registry [::ast-utils/type-registry "GDExtensionClassMethodFlags"])
       #::ast-utils {:gd-extension-type-type
                     ::ast-utils/enum

                     :name
                     "GDExtensionClassMethodFlags",

                     :members
                     (seq [#::ast-utils {:name "GDEXTENSION_METHOD_FLAG_NORMAL", :value 1}
                           #::ast-utils {:name "GDEXTENSION_METHOD_FLAG_EDITOR", :value 2}
                           #::ast-utils {:name "GDEXTENSION_METHOD_FLAG_CONST", :value 4}
                           #::ast-utils {:name "GDEXTENSION_METHOD_FLAG_VIRTUAL", :value 8}
                           #::ast-utils {:name "GDEXTENSION_METHOD_FLAG_VARARG", :value 16}
                           #::ast-utils {:name "GDEXTENSION_METHOD_FLAG_STATIC", :value 32}
                           #::ast-utils {:name "GDEXTENSION_METHOD_FLAGS_DEFAULT", :value 1}])})
      "Enum `GDExtensionClassMethodFlags` should match"))
