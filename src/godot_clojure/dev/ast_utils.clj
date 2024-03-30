(ns godot-clojure.dev.ast-utils
  (:require
   [clojure.string :as str]
   [malli.core :as m]))

(def typedef-schema
  [:and
   [:map ["kind" [:= "TypedefDecl"]]]
   [:map-of :string :any]])

(def schema-registry
  (let [def-type-kv (fn [[typename & map-fields]]
                      [typename
                       (into [:map
                              [::class [:= typename]]
                              [::const :boolean]
                              [::name ::identifier]]
                             map-fields)])
        def-pairs (map def-type-kv
                       [[::struct
                         [::members [:sequential
                                     [:map
                                      [::name ::identifier]
                                      [::type [:ref ::type]]]]]]
                        [::fn ;; Functions are allowed to be anonymous (they are not always typedef'd)
                         [::return-type [:ref ::type]]
                         [::args [:sequential [:ref ::type]]]]
                        [::lib-fn
                         [::return-type [:ref ::type]]
                         [::args [:sequential [:ref ::type]]]
                         [::lib-name :string] ;; String
                         [::since :string]
                         [::param-docs [:sequential
                                        [:map
                                         [::name ::identifier]
                                         [::doc :string]]]]]
                        [::enum
                         [::members [:sequential
                                     [:map
                                      [::name ::identifier]
                                      [::value :int]]]]]
                        [::pointer
                         [::pointed-type [:ref ::type]]]
                        [::atomic-type]])
        types (map first def-pairs)]
    (into
     {::type (into [:or] types)
      ::typename (into [:enum] types)
      ::ast [:and
             [:map ["kind" [:= "TranslationUnitDecl"]]]
             [:map-of :string :any]]
      ::typedef typedef-schema
      ::identifier :string ; Should be a bit more concerete ig
      }
     def-pairs)))

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
  {:malli/schema (my=> [:cat ::typedef] ::typename)}
  [typedef]
  (let [typename (get-in typedef ["type" "qualType"])]
    (cond
      (fn-c-string? typename) (if (lib-fn-typedef? typedef) ::lib-fn ::fn)
      (.contains typename "*") ::pointer
      (str/starts-with? typename "struct ") ::struct
      (str/starts-with? typename "enum ") ::enum
      :else ::atomic-type)))

(defn non-function-str-type->type-representation
  {:malli/schema (my=> [:cat :string] [:and ::type [:not ::fn]])}
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
            atomic {::class ::atomic-type
                    ::const (boolean (::const parsed))
                    ::name (if (vector? typename)
                             (str/join " " typename)
                             typename)}]
        (if (::pointer parsed)
          {::class ::pointer
           ::name "TODO" ;; Pointers don't really have names
           ::const false
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
    {::class ::fn
     ::name "::TODO-REMOVE-ME"
     ::const false
     ::return-type (str-type->type-representation (trim-parens ret))
     ::args (if (empty? trimmed-args)
              []
              (map str-type->type-representation (str/split trimmed-args #"\s*,\s*")))}))

(defn str-type->type-representation
  {:malli/schema (my=> [:cat :string] ::type)}
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
     {::class ::lib-fn
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
      ::param-docs (->> comment-nodes
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
      {::class ::enum
       ::name (get typedef "name")
       ::const false
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
  {::class ::struct
   ::name (get typedef "name")
   ::const false
   ::members (map (fn [item]
                    {::name (get item "name")
                     ::type (str-type->type-representation
                             (get-in item ["type" "qualType"]))})
                  (get (find-declaration ast typedef) "inner"))})

(defn atomic-typedef->atomic-type
  {:malli/schema (my=> [:cat ::typedef] ::atomic-type)}
  [typedef]
  {::class ::atomic-type
   ::const false
   ::name (get typedef "name")})

(defn ast->types
  {:malli/schema (my=> [:cat ::ast] [:sequential ::type])}
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
