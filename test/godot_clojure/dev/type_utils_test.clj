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
      (is (= (wrap-primitive "GDExtensionBool") Boolean))
      (is (= (wrap-primitive "double") Double))
      (is (= (wrap-primitive "uint8_t *") com.sun.jna.ptr.ByteByReference)))))
