(ns godot-clojure.core
  (:use
   [clojure.java.shell :only [sh]])
  (:require
   [clojure.data.json :as json]
   [clojure.string :as str]))

(defn read-gdextension-interface-ast
  "Generate gd_extension.h AST and return it as JSON string."
  []
  (:out (sh "clang" "-Xclang" "-ast-dump=json" "godot-headers/gdextension_interface.h")))

(defn gather-function-typedefs
  "Given map representation of the AST, gather all the typedef'd functions."
  [m]
  (->> (get m "inner")
       (filter #(str/starts-with? (get % "name" "") "GDExtensionInterface"))
       ;; HACK First two results is not the stuff we need. The most reliable way
       ;; would be to check if a specific typedef has a docblock above it but for now
       ;; this is good enough.
       (drop 2)))

(defn function-typedef->signature
  "Given function-typedef m, return function signature map.

  The map does not contain argument names because the AST discarded that information."
  [m]
  {:pre [(= (get m "kind") "TypedefDecl")]}
  ;; All function signatures have "(*)" in the middle, so we can split the string there
  (let [[return-type argument-types]
        (str/split (get-in m ["type" "qualType"]) #"\(\*\)")

        ; argument-types always have surrounding parentheses
        strip-parens
        #(subs % 1 (dec (.length %)))]
    {:return (str/trim return-type)
     :args (map
            str/trim
            (-> argument-types
                strip-parens
                (str/split #",")))}))

(defn param-command-comment->info
  [m]
  {:pre [(= (get m "kind") "ParamCommandComment")]}
  {:name (-> m (get "param"))
   :doc (-> m (get "inner") first (get "inner") first (get "text") str/trim)})

(defn function-typedef->function-data
  [m]
  {:pre [(= (get m "kind") "TypedefDecl")]}
  (let [comment-nodes (-> (filter #(= (get % "kind") "FullComment") (get m "inner"))
                          first
                          (get "inner"))
        function-name (-> (filter #(= (get % "kind") "VerbatimLineComment") comment-nodes)
                          first
                          (get "text")
                          str/trim)
        since-info (-> (filter #(= (get % "name") "since") comment-nodes)
                       first
                       (get "inner")
                       first
                       (get "inner")
                       first
                       (get "text")
                       str/trim)
        param-info (->> comment-nodes
                        (filter #(= (get % "kind") "ParamCommandComment"))
                        (map param-command-comment->info))
        signature-info (function-typedef->signature m)]
    {:name function-name
     :since since-info
     :params (map (fn [named-info type]
                    {:name (:name named-info)
                     :type type
                     :doc (:doc named-info)})
                  param-info
                  (:args signature-info))
     :return (:return signature-info)}))

(defn get-header-info
  "Return a map that provides useful information about gdextension_interface.h"
  []
  {:functions
   (->> (read-gdextension-interface-ast)
        json/read-str
        gather-function-typedefs
        (map function-typedef->function-data))})
