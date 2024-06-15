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
      (is (= (wrap-primitive "double") Double))
      (is (= (wrap-primitive "uint8_t *") com.sun.jna.ptr.ByteByReference))))
  ;; NOTE: In case the test fails, it shows all offending types that will make it easier to debug
  ;; the issue (which is why we check of emptiness)
  (is (empty? (->> (-> @ast-utils/gd-extension-type-registry ::ast-utils/type-registry vals)
                   (filter #(nil? (type-utils/gd-extension-type->java-type %)))))
      "all types should be convertible"))
