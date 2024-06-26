(ns godot-clojure.dev.nrepl
  "Namespace responsible for providing NREPL with access to Godot."
  (:require
   [cider.nrepl :refer [cider-nrepl-handler]]
   [nrepl.server :refer [start-server]]))

(def server (atom nil))

(def p-get-proc-address (atom nil))

(defn entry-fn [new-p-get-proc-address]
  (reset! p-get-proc-address new-p-get-proc-address)
  (reset! server (start-server :port 4200 :handler cider-nrepl-handler))
  (println "NREPL server has been started at localhost:4200"))
