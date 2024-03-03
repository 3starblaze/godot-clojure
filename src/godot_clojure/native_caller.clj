(ns godot-clojure.native-caller
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io])
  (:import
   [com.sun.jna Function Pointer]))

(defonce proc-loader-f nil)
(defonce header-info nil)

(defn init! [p-get-proc-address]
  (when (or (not (nil? proc-loader-f))
            (not (nil? header-info)))
    (throw (Throwable. "Native caller reinitialization is not allowed!")))
  (let [build-path (System/getenv "BUILDPATH")
        ensure-trailing-slash (fn [s] (if (= (nth s (dec (count s))) \/)
                                        s
                                        (str s \/)))]
    (when (nil? build-path)
      (throw (Throwable. "Env var \"BUILDPATH\" is not set!")))
    (alter-var-root
     #'proc-loader-f
     (constantly (Function/getFunction (Pointer. p-get-proc-address))))
    (alter-var-root
     #'header-info
     ;; TODO Report when edn file is not found
     ;; TODO Don't hardode filename
     (constantly (-> (ensure-trailing-slash build-path)
                     (str "gdextension-interpretation.edn")
                     io/reader
                     java.io.PushbackReader.
                     edn/read)))))

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

(defn call! [f-name & args]
  (let [fn-info (->> (:functions header-info)
                     (filter #(= (:name %) f-name))
                     first)]
    (when (nil? fn-info)
      (throw (Throwable. (format "Native function `%s` does not exist!" name))))
    (when (not (args-match? fn-info args))
      (throw (Throwable. (format "Function arguments are mismatched!"))))
    (let [native-f (->> (to-array [f-name])
                        (.invokePointer proc-loader-f)
                        Function/getFunction)
          ret-type (c-type->java-type (:return fn-info))]
      (.invoke native-f ret-type (to-array args)))))
