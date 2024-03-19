(ns godot-clojure.dev.type-util-test
  (:require
   [clojure.test :refer [are deftest is testing]]
   [godot-clojure.dev.type-util :as type-util]))

(def blank-header-info
  {:functions []
   :structs []
   :enums []
   :irreducible-type-mapping {}})

(deftest c->clojure-type-test []
  (testing "Check non-existent type"
    (is (nil? (type-util/c->clojure-type blank-header-info "IDontExist"))))
  (testing "Check struct type conversion"
    (let [info (assoc blank-header-info :structs
                      [{:name "GDExtensionCoolStruct" :members [{:name "name" :type "const char *"}]}
                       {:name "GDExtensionLameStruct" :members [{:name "id" :value "int"}]}])]
      ;; We don't care what the type is, just make sure there is conversion
      (is (type-util/c->clojure-type info "GDExtensionCoolStruct"))
      (is (type-util/c->clojure-type info "GDExtensionLameStruct"))))
  (testing "Primitive type conversion"
    (let [convert (fn [c-type] (type-util/c->clojure-type blank-header-info c-type))]
      (is (= (convert "void") Void))
      (is (= (convert "char *") String))
      (is (= (convert "void *") com.sun.jna.Pointer)))))
