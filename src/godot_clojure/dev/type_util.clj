(ns godot-clojure.dev.type-util
  (:require
   [clojure.string :as s]))

(def gen-struct-ns 'godot-clojure.gen)

(defn struct-name->gen-class-name
  [struct-name]
  (symbol (str gen-struct-ns "." struct-name)))

(def c->java-base-mapping
  {"GDExtensionBool" Boolean
   "GDExtensionBool *" com.sun.jna.ptr.IntByReference ;; There is no boolean by reference type
   "char" Byte
   "char *" String
   "char16_t" Character
   "char16_t *" String
   "char32_t" Character
   "char32_t *" String
   "double" Double
   "double *" com.sun.jna.ptr.DoubleByReference
   "float" Float
   "float *" com.sun.jna.ptr.FloatByReference
   "int" Integer
   "int32_t" Integer
   "int32_t *" com.sun.jna.ptr.IntByReference
   "int64_t" Long
   "int64_t *" com.sun.jna.ptr.LongByReference
   "long" com.sun.jna.NativeLong
   "long long" Long
   "short" Short
   "size_t" Long ;; Sometimes it may not be a Long but this is probably good enough for now
   "uint64_t" Long
   "uint8_t *" com.sun.jna.ptr.ByteByReference
   "void" Void
   "void *" com.sun.jna.Pointer
   "wchar_t" Character
   "wchar_t *" String})

(defn c->clojure-type
  [header-info c-type]
  ;; TODO Keep info about const modifier
  (let [typename (if (s/starts-with? c-type "const ")
                   (subs c-type (count "const "))
                   c-type)]
    (or (get c->java-base-mapping typename)
      (when (first (filter #(= (:name %) typename) (:structs header-info)))
        (struct-name->gen-class-name typename))
      (when (seq (->> (:enums header-info) (filter #(= (:name %) typename))))
        (get c->java-base-mapping "int"))
      ;; TODO Respect iredducible types as their own types
      (when-let [new-typename (get (:irreducible-type-mapping header-info) typename)]
        (recur header-info new-typename)))))

(defn struct-info->class-map
  "Return a map that insn can use to make struct class."
  [struct-info]
  {:name (struct-name->gen-class-name (:name struct-info))
   :super 'com.sun.jna.Structure
   :annotations {com.sun.jna.Structure$FieldOrder (map :name (:members struct-info))}
   :fields (map (fn [member]
                  {:flags [:public]
                   :type (:type member) ;; TODO Handle type conversion, C->Java
                   :name (:name member)})
                (:members struct-info))})
