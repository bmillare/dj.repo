(ns dj.web.server
  (:require [ring.adapter.jetty :as jetty]))

(defn ->dynamic-server
  "server that appends functionality while running

note, changing port after server is started will do nothing

Required keys:
:port
:handler

returns atom of opts and server
"
  [opts]
  (let [{:keys [port]} opts
        db (atom opts)
        server (jetty/run-jetty (fn [request]
                                  ((:handler @db) request))
                                {:port port
                                 :join? false})]
    (swap! db
           assoc
           :server
           server)
    db))

(defn stop [server]
  (.stop (:server @server)))
