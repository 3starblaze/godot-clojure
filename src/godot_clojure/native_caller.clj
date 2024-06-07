(ns godot-clojure.native-caller
  (:require
   [godot-clojure.dev.ast-utils :as ast-utils])
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

;; TODO Implement this properly
(defn eq-type? [_arg-info _arg]
  true)

(defn args-match? [fn-info given-args]
  (if (not= (count given-args) (count (:params fn-info)))
    false
    (->> (map vector (:params fn-info) given-args)
         (every? (fn [[arg-info arg]] (eq-type? arg-info arg))))))

;; TODO Finish this. Probably should combine with `eq-type?` since `eq-type?` will
;; probably need to convert the type anyways.
(defn c-type->java-type [c-type]
  (case c-type
    "void" Void
    (throw (Throwable. (format "Unexpected c-type \"%s\"" c-type)))))

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
    (when (not (args-match? fn-info args))
      (throw (Throwable. (format "Function arguments are mismatched!"))))
    (apply unsafe-call! f-name (c-type->java-type (:return fn-info)) args)))
