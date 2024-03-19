(ns godot-clojure.dev.type-util-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [godot-clojure.dev.gdextension-interface-interpreter :as gii]
   [godot-clojure.dev.test-utils :as u]
   [godot-clojure.dev.type-util :as type-util]))

(def ast nil)

(use-fixtures :once (u/make-ast-loader-fixture #'ast))

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
      (is (= (convert "void *") com.sun.jna.Pointer))))
  (testing "Enum conversion"
    (let [info (assoc blank-header-info :enums
                      [{:name "GDExtensionCoolEnum"
                        :options [[:name "GD_EXTENSION_COOL_ZERO" :value 0]
                                  {:name "GD_EXTENSION_COOL_ONE" :value 1}]}
                       {:name "GDExtensionLameEnum"
                        :options [[:name "GD_EXTENSION_LAME_FLAG_ALPHA" :value 1]
                                  {:name "GD_EXTENSION_LAME_FLAG_BETA" :value 2}]}])
          convert (fn [c-type] (type-util/c->clojure-type info c-type))]
      (is (= (convert "GDExtensionCoolEnum") Integer))
      (is (= (convert "GDExtensionLameEnum") Integer))
      (is (nil? (convert "GDExtensionDontExistEnum")))))
  (testing "All types in functions are convertible"
    (let [info (#'gii/ast->header-info ast)
          types (->> (:functions info)
                     (map #(vector (map :type (:params %)) (:return %)))
                     flatten
                     set)]
      (is (empty? (filter #(nil? (type-util/c->clojure-type info %)) types))))))
