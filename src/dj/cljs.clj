(ns dj.cljs
  (:refer-clojure :exclude [load-file])
  (:require [dj]
            [dj.io]
            [cljs.repl]
            [cljs.repl.browser]
            [cljs.repl.server]
            [cljs.analyzer :as ca]))

(defn ->cljs-browser-env
  "port: for repl/server
working-dir: path/file to generated js

Creates a browser connected evaluation environment object and returns
it. This object wraps a repl-env

To connect to server, run cljs in browser
 (clojure.browser.repl/connect \"http://localhost:<port>/repl\")
Make sure advanced optimizations is not activated. Simple works though.

Use load-file or load-namespace to do dynamic development"
  [opts]
  (let [{:keys [port working-dir]} opts]
    #_ (dj.io/rm (dj.io/file dj/system-root
                          "out"))
    #_ (dj.io/rm (dj.io/file dj/system-root
                          working-dir))
    (let [repl-env (doto (cljs.repl.browser/repl-env :port port
                                                     :working-dir working-dir)
                     cljs.repl/-setup)
          server (atom opts)]
      (swap! server
             assoc
             :repl-env
             repl-env)
      server)))

(defn stop [server]
  (cljs.repl/-tear-down (:repl-env @server)))

(defn cljs-eval
  "note this accepts the object returned from ->cljs-browser-env, not a repl-env"
  [server form]
  (dj.io/capture-out-err
   (cljs.repl/evaluate-form (:repl-env @server)
                            {:context :statement :locals {}
                             :ns (ca/get-namespace ca/*cljs-ns*)}
                            "<dj.cljs/cljs-eval>"
                            form)))

(defn load-file [server f]
  (cljs.repl/load-file (:repl-env @server) f))

(defn load-namespace [server n]
  (cljs.repl/load-namespace (:repl-env @server) n))
