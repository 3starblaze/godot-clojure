(ns godot-clojure.dev.type-util)

(def gen-struct-ns 'godot-clojure.gen)

(defn struct-name->gen-class-name
  [struct-name]
  (symbol (str gen-struct-ns "." struct-name)))

(def c->java-base-mapping
  {"char" Byte
   "short" Short
   "wchar_t" Character
   "int" Integer
   "long" com.sun.jna.NativeLong
   "long long" Long
   "float" Float
   "double" Double
   "char *" String
   "void *" com.sun.jna.Pointer
   "void" Void})

(defn c->clojure-type
  [header-info c-type]
  (or (get c->java-base-mapping c-type)
      (when (first (filter #(= (:name %) c-type) (:structs header-info)))
        (struct-name->gen-class-name c-type))
      ;; TODO Enums
      ))

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
