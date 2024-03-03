(ns godot-clojure.dev.gdextension-interface-interpreter
  (:require
   [clojure.data.json :as json]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.string :as str]))

(defn- read-gdextension-interface-ast!
  "Generate gd_extension.h AST and return it as JSON string."
  []
  (let [res (shell/sh "clang" "-Xclang" "-ast-dump=json" "godot-headers/gdextension_interface.h")]
    (if (zero? (:exit res))
      (:out res)
      (throw (Throwable. (format "Unexpected error when getting AST: %s" (:err res)))))))

(defn- gather-function-typedefs
  "Given map representation of the AST, gather all the typedef'd functions."
  [m]
  (->> (get m "inner")
       (filter #(str/starts-with? (get % "name" "") "GDExtensionInterface"))
       ;; HACK First two results is not the stuff we need. The most reliable way
       ;; would be to check if a specific typedef has a docblock above it but for now
       ;; this is good enough.
       (drop 2)))

(defn- function-typedef->signature
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

(defn- param-command-comment->info
  [m]
  {:pre [(= (get m "kind") "ParamCommandComment")]}
  {:name (-> m (get "param"))
   :doc (-> m (get "inner") first (get "inner") first (get "text") str/trim)})

(defn- function-typedef->function-data
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
     :params (map (fn [named-info arg-type]
                    {:name (:name named-info)
                     :type arg-type
                     :doc (:doc named-info)})
                  param-info
                  (:args signature-info))
     :return (:return signature-info)}))

(defn- extract-irreducible-type-mapping
  "Extract mapping for types that should not be untypedef'd.

  For example `GDExtensionStringNamePtr` is actually a void * but we don't want to lose
  the type information by reducing this specific type into a void*."
  [ast]
  (let [typedef? #(= (get % "kind") "TypedefDecl")
        get-real-type #(get-in % ["type" "qualType"])
        fn-ptr? #(.contains (get-real-type %) "(*)")
        gd-type? #(str/starts-with? (get % "name") "GD")
        enum? #(str/starts-with? (get-real-type %) "enum ")
        struct? #(str/starts-with? (get-real-type %) "struct ")
        gen-mapping #(vector (get % "name") (get-in % ["type" "qualType"]))]
    (->> (get ast "inner")
         (filter typedef?)
         (filter gd-type?)
         (filter (complement fn-ptr?))
         (filter (complement enum?))
         (filter (complement struct?))
         (map gen-mapping)
         (into {}))))

(defn- ensure-dir!
  [dir-name]
  ;; TODO Handle situation when f exists but is not a dir
  ;; TODO Check result of mkdir
  (let [f (io/file dir-name)]
    (when (not (.exists f))
      (.mkdir f))))

;; TODO Expose a schema so that the information shape is more clear
;; TODO Don't hardcode build dir and export file name
(defn export-header-info!
  "Return a map that provides useful information about gdextension_interface.h."
  []
  (ensure-dir! "build")
  (spit
   "build/gdextension-interpretation.edn"
   {:functions
    (->> (read-gdextension-interface-ast!)
         json/read-str
         gather-function-typedefs
         (map function-typedef->function-data))}))
