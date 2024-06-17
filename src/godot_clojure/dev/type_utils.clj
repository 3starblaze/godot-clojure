(ns godot-clojure.dev.type-utils
  (:require
   [godot-clojure.dev.ast-utils :as ast-utils])
  (:import
   [com.sun.jna Function Pointer]))

(def gen-struct-ns 'godot-clojure.gen.struct)

(defn struct-name->gen-class-name
  [struct-name]
  (symbol (str gen-struct-ns "." struct-name)))

;; HACK: In normal and pointer base-mappings we are assuming GD* types. This information should
;; be extracted from AST.
(def c->java-base-mapping
  {"GDObjectInstanceID" Long
   "GDExtensionBool" Byte
   "GDExtensionInt" Long
   "char" Byte
   "char16_t" Character
   "char32_t" Character
   "double" Double
   "float" Float
   "int" Integer
   "int32_t" Integer
   "int64_t" Long
   "long" com.sun.jna.NativeLong
   "long long" Long
   "short" Short
   "size_t" Long ;; HACK: Assumed that size_t is a long which might not always be the case
   "uint64_t" Long
   "void" Void
   "wchar_t" Character})

(def c-pointer->java-base-mapping
  "Like `c->java-base-mapping` but for pointers."
  {"GDObjectInstanceID" com.sun.jna.ptr.LongByReference
   "GDExtensionBool" com.sun.jna.ptr.ByteByReference
   "GDExtensionInt" com.sun.jna.ptr.LongByReference
   "char" String
   "char16_t" String
   "char32_t" String
   "double" com.sun.jna.ptr.DoubleByReference
   "float" com.sun.jna.ptr.FloatByReference
   "int32_t" com.sun.jna.ptr.IntByReference
   "int64_t" com.sun.jna.ptr.LongByReference
   "uint8_t" com.sun.jna.ptr.ByteByReference
   "void" com.sun.jna.Pointer
   "wchar_t" String})

(defn gd-extension-type->java-type
  "Match GDExtension type with a Java type."
  [gd-extension-type]
  (case (::ast-utils/gd-extension-type-type gd-extension-type)
    ::ast-utils/struct (struct-name->gen-class-name gd-extension-type)
    ::ast-utils/fn Function
    ::ast-utils/lib-fn Function
    ::ast-utils/enum Integer
    ::ast-utils/pointer (or (get c-pointer->java-base-mapping
                                 (-> gd-extension-type ::ast-utils/pointed-type ::ast-utils/name))
                            Pointer)
    ;; HACK: As far as I know, if a type is unknown, it is probably an opaque type that
    ;; eventually becomes a void pointer. This kind of information could probably be defined
    ;; in the registry but for now this will do.
    ::ast-utils/atomic-type (or (get c->java-base-mapping (::ast-utils/name gd-extension-type))
                                Pointer)
    (throw (ex-info "Could not match `gd-extension-type`"
                    {:gd-extension-type gd-extension-type}))))

(defn can-call-gd-extension-fn?
  [gd-extension-fn args]
  (every? identity
          (map instance?
               (map gd-extension-type->java-type (::ast-utils/args gd-extension-fn))
               args)))

(defn struct-info->class-map
  "Return a map that insn can use to make struct class."
  [struct-info]
  {:name (struct-name->gen-class-name (::ast-utils/name struct-info))
   :super 'com.sun.jna.Structure
   :annotations {com.sun.jna.Structure$FieldOrder
                 (map ::ast-utils/name (::ast-utils/members struct-info))}
   :fields (map (fn [member]
                  {:flags [:public]
                   :type (gd-extension-type->java-type (::ast-utils/type member))
                   :name (::ast-utils/name member)})
                (::ast-utils/members struct-info))})
