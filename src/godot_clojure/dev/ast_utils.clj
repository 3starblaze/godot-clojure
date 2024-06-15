(ns godot-clojure.dev.ast-utils
  (:require
   [clojure.data.json :as json]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.string :as str]
   [malli.core :as m]))

(def typedef-schema
  [:and
   [:map ["kind" [:= "TypedefDecl"]]]
   [:map-of :string :any]])

(def schema-registry
  {::gd-extension-type-type
   [:enum
    ::struct
    ::fn
    ::lib-fn
    ::enum
    ::pointer
    ::atomic-type]

   ::gd-extension-type
   [:multi {:dispatch ::gd-extension-type-type}
    [::struct [:ref ::struct]]
    [::fn [:ref ::fn]]
    [::lib-fn [:ref ::lib-fn]]
    [::enum [:ref ::enum]]
    [::pointer [:ref ::pointer]]
    [::atomic-type [:ref ::atomic-type]]]

   ::struct
   [:map
    [::name ::identifier]
    [::members [:sequential
                [:map
                 [::name ::identifier]
                 [::type [:ref ::gd-extension-type]]]]]]

   ::fn
   [:map
    [::return-type [:ref ::gd-extension-type]]
    [::args [:sequential [:ref ::gd-extension-type]]]]

   ::lib-fn
   [:map
    [::return-type [:ref ::gd-extension-type]]
    [::args [:sequential [:ref ::gd-extension-type]]]
    [::lib-name :string]
    [::since :string]
    [::args-docs [:sequential
                  [:map
                   [::name ::identifier]
                   [::doc :string]]]]]

   ::enum
   [:map
    [::name ::identifier]
    [::members [:sequential
                [:map
                 [::name ::identifier]
                 [::value :int]]]]]

   ::pointer
   [:map
    [::pointed-type-const? :boolean]
    [::pointed-type [:ref ::gd-extension-type]]]

   ::atomic-type
   [:map
    [::name ::identifier]]

   ::gd-extension-type-registry
   [:map
    [::type-registry [:map-of ::identifier ::gd-extension-type]]
    [::lib-fn-registry [:map-of ::identifier ::lib-fn]]]

   ::ast
   [:and
    [:map ["kind" [:= "TranslationUnitDecl"]]]
    [:map-of :string :any]]

   ::typedef
   typedef-schema

   ;; TODO: Make it more specific
   ::identifier
   :string})

(defn- my=>
  "Convenvience function for specifying function signatures."
  [& more]
  (into [:=> {:registry schema-registry}] more))

(defn- kind=
  {:malli/schema (my=> [:cat :string] [:=> [:cat ::typedef] :boolean])}
  [kind]
  (fn [typedef] (= (get typedef "kind") kind)))

(defn- fn-c-string? [s]
  {:malli/schema (my=> [:cat :string] :boolean)}
  (.contains s "(*)"))

(defn- lib-fn-typedef?
  {:malli/schema (my=> [:cat ::typedef] :boolean)}
  [typedef]
  (as-> typedef $
    (get $ "inner")
    (filter (kind= "FullComment") $)
    (first $)
    (get $ "inner")
    (filter (kind= "VerbatimLineComment") $)
    (first $)
    (get $ "text")
    (boolean $)))

(defn typedef-categorizer
  {:malli/schema (my=> [:cat ::typedef] ::gd-extension-type-type)}
  [typedef]
  (let [typename (get-in typedef ["type" "qualType"])]
    (cond
      (fn-c-string? typename) (if (lib-fn-typedef? typedef) ::lib-fn ::fn)
      (.contains typename "*") ::pointer
      (str/starts-with? typename "struct ") ::struct
      (str/starts-with? typename "enum ") ::enum
      :else ::atomic-type)))

(defn non-function-str-type->type-representation
  {:malli/schema (my=> [:cat :string] [:and ::gd-extension-type [:not ::fn]])}
  [s]
  ;; The regex splits by whitespace and makes sure that "*" is always a token of its own because
  ;; "void * foo", "void* foo" and "void*foo" are all valid and should return tokens "void" "*" "foo".
  ;; Sometimes this splitting algorithm returns empty strings, so we filter those out.
  (let [tokens (vec (filter (complement empty?) (str/split s #"\s+|(?<=\*)|(?=\*)")))
        ;; TODO We cannot handle multiple consts and multiple pointers
        ;; HACK We cannot match all sorts of weird types with multiple tokens that C allows so
        ;; right now we are just hoping that "long long" and "long double" are the only multi
        ;; token types that match. See https://en.cppreference.com/w/c/language/arithmetic_types
        tokens-schema [:catn
                       [::const [:? [:= "const"]]]
                       [::type [:alt
                                [:and :string [:not= "*"] [:not= "const"]]
                                [:cat [:= "long"] [:= "long"]]
                                [:cat [:= "long"] [:= "double"]]]]
                       [::pointer [:? [:= "*"]]]]
        parsed (m/parse tokens-schema tokens)]
    (if (not= parsed ::m/invalid)
      (let [typename (::type parsed)
            atomic {::gd-extension-type-type ::atomic-type
                    ::name (if (vector? typename)
                             (str/join " " typename)
                             typename)}]
        (if (::pointer parsed)
          {::gd-extension-type-type ::pointer
           ::pointed-type-const? (boolean (::const parsed))
           ::pointed-type atomic}
          atomic))
      (throw (java.lang.UnsupportedOperationException.
              (format "Could not understand type `%s`!" s))))))

(declare str-type->type-representation)

(defn function-str-type->type-representation
  {:malli/schema (my=> [:cat :string] ::fn)}
  [s]
  ;; We split only to 2 parts because functions can be nested thus having more than one "(*)".
  ;; We want to split only on first "(*)" because that is the outer separator.
  (let [[ret args] (str/split s #"\(\*\)" 2)
        ;; `or` is needed when s is trimmed to empty string e.g. "()"`
        trim-parens (fn [s] (or (->> (str/split s #"^\(|\)$") (filter (complement empty?)) first) ""))
        trimmed-args (trim-parens args)]
    {::gd-extension-type-type ::fn
     ::return-type (str-type->type-representation (trim-parens ret))
     ::args (if (empty? trimmed-args)
              []
              (map str-type->type-representation (str/split trimmed-args #"\s*,\s*")))}))

(defn str-type->type-representation
  {:malli/schema (my=> [:cat :string] ::gd-extension-type)}
  [s]
  (if (fn-c-string? s)
    (function-str-type->type-representation s)
    (non-function-str-type->type-representation s)))

(defn fn-typedef->fn
  {:malli/schema (my=> [:cat ::typedef] ::fn)}
  [typedef]
  (merge
   {::name (get typedef "name")}
   (function-str-type->type-representation (get-in typedef ["type" "qualType"]))))

(defn- param-command-comment->info
  [m]
  {::name (-> m (get "param"))
   ::doc (-> m (get "inner") first (get "inner") first (get "text") str/trim)})

(defn lib-fn-typedef->fn
  {:malli/schema (my=> [:cat ::typedef] ::lib-fn)}
  [typedef]
  ;; get since and arg docs
  (let [comment-nodes (get (first (filter (kind= "FullComment") (get typedef "inner"))) "inner")]
    (merge
     (fn-typedef->fn typedef)
     {::gd-extension-type-type ::lib-fn
      ::lib-name (-> (filter (kind= "VerbatimLineComment") comment-nodes)
                     first
                     (get "text")
                     str/trim)
      ::since (-> (filter #(= (get % "name") "since") comment-nodes)
                  first
                  (get "inner")
                  first
                  (get "inner")
                  first
                  (get "text")
                  str/trim)
      ::args-docs (->> comment-nodes
                       (filter #(= (get % "kind") "ParamCommandComment"))
                       (map param-command-comment->info))})))

(defn find-declaration
  {:malli/schema (my=> [:cat ::ast ::typedef] :any)} ; TODO Fix return type
  [ast typedef]
  (let [declaration-info (get-in typedef ["inner" 0 "ownedTagDecl"])]
    (->> (get ast "inner")
         (filter (kind= (get declaration-info "kind")))
         (filter #(= (get % "id") (get declaration-info "id")))
         first)))

(defn enum-typedef->enum
  {:malli/schema (my=> [:cat ::ast ::typedef] ::enum)}
  [ast typedef]
  (let [declaration (find-declaration ast typedef)
        members (map (fn [item]
                       [(get item "name")
                        (when-let [str-val (get-in item ["inner" 0 "value"])]
                          (Integer. str-val))])
                     (get declaration "inner"))
        auto-enum? (empty? (filter #(not (nil? (nth % 1))) members))
        all-explicit-enum? (empty? (filter #(nil? (nth % 1)) members))]
    (if (or auto-enum? all-explicit-enum?)
      {::gd-extension-type-type ::enum
       ::name (get typedef "name")
       ::members (map
                  (fn [[member-name value]]
                    {::name member-name
                     ::value value})
                  (if all-explicit-enum?
                    members
                    ;; If values are not explicitly defined, we fill them with 0, 1, 2...
                    ;; because that is the default C behavior.
                    (map (fn [i [member-name _]] [member-name i]) (range) members)))}
      (throw (java.lang.UnsupportedOperationException.
              "Mixing explicit and implicit enum values is not supported!")))))

(defn struct-typedef->struct
  {:malli/schema (my=> [:cat ::ast ::typedef] ::struct)}
  [ast typedef]
  {::gd-extension-type-type ::struct
   ::name (get typedef "name")
   ::members (map (fn [item]
                    {::name (get item "name")
                     ::type (str-type->type-representation
                             (get-in item ["type" "qualType"]))})
                  (get (find-declaration ast typedef) "inner"))})

(defn atomic-typedef->atomic-type
  {:malli/schema (my=> [:cat ::typedef] ::atomic-type)}
  [typedef]
  {::gd-extension-type-type ::atomic-type
   ::name (get typedef "name")})

(defn ast->types
  {:malli/schema (my=> [:cat ::ast] [:sequential ::gd-extension-type])}
  [ast]
  (let [typedefs (->> (get ast "inner")
                      (filter (m/validator typedef-schema))
                      (group-by typedef-categorizer))
        collector (fn [[->type-f k]]
                    (map ->type-f (k typedefs)))]
    (apply concat (map collector [[fn-typedef->fn ::fn]
                                  [(partial enum-typedef->enum ast) ::enum]
                                  [(partial struct-typedef->struct ast) ::struct]
                                  [atomic-typedef->atomic-type ::atomic-type]
                                  [lib-fn-typedef->fn ::lib-fn]]))))

(defn ast->gd-extension-type-registry
  {:malli/schema (my=> [:cat ::ast] [:map-of :string ::gd-extension-type-registry])}
  [ast]
  (let [typedefs (->> (get ast "inner")
                      ;; NOTE: It would be more robust to compare against "GDExtension" but there
                      ;; is "GDObjectInstanceID" which is the only item that doesn't start with
                      ;; "GDExtension".
                      (filter #(let [n (get % "name")]
                                 (and (string? n) (str/starts-with? n "GD"))))
                      (filter (m/validator typedef-schema))
                      (group-by typedef-categorizer))
        collector (fn [[->type-f k]]
                    (map ->type-f (k typedefs)))
        types (flatten
               (map collector [[fn-typedef->fn ::fn]
                               [(partial enum-typedef->enum ast) ::enum]
                               [(partial struct-typedef->struct ast) ::struct]
                               [atomic-typedef->atomic-type ::atomic-type]
                               [lib-fn-typedef->fn ::lib-fn]]))]
    {::type-registry (->> types
                          (filter #(not (nil? (::name %))))
                          (map #(vector (::name %) %))
                          (into {}))
     ::lib-fn-registry (->> types
                            (filter #(= (::gd-extension-type-type %) ::lib-fn))
                            (map #(vector (::lib-name %) %))
                            (into {}))}))


(def godot-clojure-dir ".godot-clojure")

(def gd-extension-type-registry-filename "gd-extension-type-registry.edn")

(defn read-gd-extension-interface-ast!
  "Try to retrieve gdextension_interface.h AST."
  []
  (let [res (shell/sh "clang" "-Xclang" "-ast-dump=json" "godot-headers/gdextension_interface.h")]
    (if (zero? (:exit res))
      (json/read-str (:out res))
      (throw (Throwable. (format "Unexpected error when getting AST: %s" (:err res)))))))

(defn export-gd-extension-type-registry!
  [registry]
  (let [dir (doto (io/file godot-clojure-dir) .mkdirs)]
    (spit (io/file dir gd-extension-type-registry-filename) registry)))

(defn import-gd-extension-type-registry!
  []
  (-> (io/file godot-clojure-dir gd-extension-type-registry-filename)
      io/reader
      java.io.PushbackReader.
      edn/read))
