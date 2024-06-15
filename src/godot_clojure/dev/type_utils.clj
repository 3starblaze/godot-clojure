(ns godot-clojure.dev.type-utils
  (:require
   [godot-clojure.dev.ast-utils :as ast-utils])
  (:import
   [com.sun.jna Function Pointer]))

(def gen-struct-ns 'godot-clojure.gen.struct)

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

(defn gd-extension-type->java-type
  "Match GDExtension type with a Java type or return nil when it's not possible."
  [gd-extension-type]
  (case (::ast-utils/gd-extension-type-type gd-extension-type)
    ::ast-utils/struct (struct-name->gen-class-name gd-extension-type)
    ::ast-utils/fn Function
    ::ast-utils/lib-fn Function
    ::ast-utils/enum Integer
    ::ast-utils/pointer Pointer
    ::ast-utils/atomic-type (get c->java-base-mapping (::ast-utils/name gd-extension-type))))

(defn can-call-lib-fn?
  [lib-fn args]
  (= (map gd-extension-type->java-type (::ast-utils/args lib-fn))
     (map type args)))

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
