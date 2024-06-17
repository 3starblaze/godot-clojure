(ns godot-clojure.dev.type-utils-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [godot-clojure.dev.ast-utils :as ast-utils]
   [godot-clojure.dev.type-utils :as type-utils]))

(deftest gd-extension-type->java-type-test []
  (testing "Primitive types"
    (let [wrap-primitive (fn [s]
                           (type-utils/gd-extension-type->java-type
                            #::ast-utils{:gd-extension-type-type ::ast-utils/atomic-type
                                         :name s}))]
      (is (= (wrap-primitive "GDExtensionBool") Byte))
      (is (= (wrap-primitive "double") Double))))
  (testing "Other types"
    (is (= (type-utils/gd-extension-type->java-type
            #:godot-clojure.dev.ast-utils
             {:gd-extension-type-type ::ast-utils/atomic-type
              :name "GDExtensionVariantPtr"})
           com.sun.jna.Pointer)
        "Typedef'd pointer should be convertible")
    (is (= (type-utils/gd-extension-type->java-type
            #::ast-utils{:gd-extension-type-type ::ast-utils/pointer
                         :pointed-type-const? true
                         :pointed-type #::ast-utils
                                        {:gd-extension-type-type ::ast-utils/atomic-type
                                         :name "char"}})
           String)
        "char pointer should be convertible")))
