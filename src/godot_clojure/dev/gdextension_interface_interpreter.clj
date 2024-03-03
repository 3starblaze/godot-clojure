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

(defn unit-typedef? [unit]
  (= (get unit "kind") "TypedefDecl"))

(defn unit-get-real-type [unit]
  (get-in unit ["type" "qualType"]))

(defn unit-struct? [unit]
  (str/starts-with? (unit-get-real-type unit) "struct "))

(defn unit-enum? [unit]
  (str/starts-with? (unit-get-real-type unit) "enum "))

(defn- unit-gd-type? [unit]
  (str/starts-with? (get unit "name") "GD"))

(defn- extract-irreducible-type-mapping
  "Extract mapping for types that should not be untypedef'd.

  For example `GDExtensionStringNamePtr` is actually a void * but we don't want to lose
  the type information by reducing this specific type into a void*."
  [ast]
  (let [unit-fn-ptr? #(.contains (unit-get-real-type %) "(*)")
        gen-mapping #(vector (get % "name") (get-in % ["type" "qualType"]))]
    (->> (get ast "inner")
         (filter unit-typedef?)
         (filter unit-gd-type?)
         (filter (complement unit-fn-ptr?))
         (filter (complement unit-enum?))
         (filter (complement unit-struct?))
         (map gen-mapping)
         (into {}))))

(defn- extract-structs
  [ast]
  (let [unit-struct-decl? #(= (get % "kind") "RecordDecl")
        ;; "ownedTagDecl" also has "kind" attribute which is probably always "RecordDecl"
        ;; in our case so we don't check it.
        decl-id->name (->> (get ast "inner")
                           (filter unit-typedef?)
                           (filter unit-gd-type?)
                           (filter unit-struct?)
                           (map #(vector (get-in % ["inner" 0 "ownedTagDecl" "id"])
                                         (get % "name")))
                           (into {}))
        collect-struct-members #(->> (get % "inner")
                                     (map (fn [field]
                                            {:name (get field "name")
                                             :type (get-in field ["type" "qualType"])})))]
    (->> (filter unit-struct-decl? (get ast "inner"))
         (map (fn [decl]
                (when-let [struct-name (get decl-id->name (get decl "id"))]
                  [struct-name decl])))
         (filter (complement nil?))
         (map (fn [[struct-name decl]]
                {:name struct-name
                 :members (collect-struct-members decl)})))))

(defn- extract-enums
  [ast]
  (let [decl-id->name (->> (get ast "inner")
                           (filter unit-typedef?)
                           (filter unit-gd-type?)
                           (filter unit-enum?)
                           (map #(vector (get-in % ["inner" 0 "ownedTagDecl" "id"])
                                         (get % "name")))
                           (into {}))
        unit-enum-decl? #(= (get % "kind") "EnumDecl")
        get-enum-value #(if-let [str-val (get-in % ["inner" 0 "value"])]
                          (Integer. str-val)
                          :auto)
        collect-enum-options #(->> (get % "inner")
                                   (map (fn [opt]
                                          {:name (get opt "name")
                                           :value (get-enum-value opt)})))
        auto-enum? (fn [enum] (every? #(= (:value %) :auto) (:options enum)))
        explicit-enum? (fn [enum] (every? #(not= (:value %) :auto) (:options enum)))
        res (->> (filter unit-enum-decl? (get ast "inner"))
                 (map (fn [decl]
                        (when-let [struct-name (get decl-id->name (get decl "id"))]
                          [struct-name decl])))
                 (filter (complement nil?))
                 (map (fn [[enum-name decl]]
                        {:name enum-name
                         :options (collect-enum-options decl)}))
                 (map #(if (auto-enum? %)
                         (update % :options (fn [old-options]
                                              (map-indexed
                                               (fn [i opt]
                                                 (assoc opt :value i))
                                               old-options)))
                         %)))]
    ;; Technically you can mix explicit and implicit values in an enum but I don't
    ;; have a need or desire to handle it.
    (if (not-every? #(or (auto-enum? %) (explicit-enum? %)) res)
      (throw (Throwable. "Mixed implict+explicit enum encountered!!"))
      res)))

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
