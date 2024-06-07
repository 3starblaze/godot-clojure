#!/usr/bin/env bb

(ns bb-utils
  (:require
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.string :as str]))

(defn get-project-root! []
  ;; We assume this file is in "<repo>/bin" folder and we can find the repo by getting the parent
  ;; directory.
  (-> *file* io/as-file .getParentFile .getParent))

(defn get-classpath! []
  (->> (str/split
        (:out (shell/sh "clojure" "-Spath" "-A:dev" :dir (get-project-root!)))
        #":")
       ;; Classpath can include relative paths which need to be absolute because the godot project
       ;; is in another place and that probably breaks the classpaths. Making paths absolute does
       ;; the trick, so that's what we do.
       (map #(if (str/starts-with? % "/") % (str (get-project-root!) "/" %)))
       (str/join ":")))

(defn get-env! [ns f]
  (merge
   ;; `System/getenv` doesn't return a real map and we can't merge the result
   ;; so we use `into` to force it to be a map.
   (into {} (System/getenv))
   {"GODOT_CLOJURE_CLASSPATH" (get-classpath!)
    "GODOT_CLOJURE_ENTRY_NS" ns
    "GODOT_CLOJURE_ENTRY_FN" f
    "LD_LIBRARY_PATH" "/usr/lib/jvm/java-1.11.0-openjdk-amd64/lib/server"}))
