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
      (is (not (empty? (:irreducible-type-mapping info)))))))
