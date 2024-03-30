(ns godot-clojure.dev.ast-utils-test
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [godot-clojure.dev.ast-utils :as ast-utils]
   [godot-clojure.dev.test-utils :as u]))

(def ast nil)

(use-fixtures :once
  (u/make-ast-loader-fixture #'ast)
  (u/make-malli-instrumentation-fixture 'godot-clojure.dev.ast-utils))

(deftest ast->types-test []
  (testing "Types are not empty"
    (is (seq (ast-utils/ast->types ast)))))
