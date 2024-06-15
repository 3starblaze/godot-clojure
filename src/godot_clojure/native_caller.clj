(ns godot-clojure.native-caller
  (:require
   [godot-clojure.dev.ast-utils :as ast-utils]
   [godot-clojure.dev.type-utils :as type-utils])
  (:import
   [com.sun.jna Function Pointer]))

(def init-state nil)

(defn init! [p-get-proc-address]
  ;; HACK: During testing I run `init!` several times and triggers this exception so I
  ;; temporarily commented it out until I figure out the API.
  #_(when (not (nil? init-state))
    (throw (Throwable. "Native caller reinitialization is not allowed!")))
  (alter-var-root
   #'init-state
   (constantly
    {:p-get-proc-address (Function/getFunction (Pointer. p-get-proc-address))
     :gd-extension-type-registry (ast-utils/import-gd-extension-type-registry!)})))

(defn unsafe-call!
  "Call `f-name` without any checks.

  Wrong parameters might cause segfault and unexpected behavior, be careful with this function."
  [f-name ret-type & args]
  (-> (.invokePointer (:p-get-proc-address init-state) (to-array [f-name]))
      Function/getFunction
      (.invoke ret-type (to-array args))))

(defn call! [f-name & args]
  (let [fn-info (get-in init-state [:gd-extension-type-registry
                                    ::ast-utils/lib-fn-registry
                                    f-name])]
    (when (nil? fn-info)
      (throw (Throwable. (format "Native function `%s` does not exist!" f-name))))
    (when (not (type-utils/can-call-lib-fn? fn-info args))
      (throw (Throwable. (format "Function arguments are mismatched!"))))
    (apply unsafe-call! f-name (type-utils/gd-extension-type->java-type (:return fn-info)) args)))
