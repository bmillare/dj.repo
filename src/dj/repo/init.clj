(ns dj.repo.init
  (:require [dj.dependencies]
            [dj.repo]))

(defn init-sys []
  {:dj/repositories (merge cemerick.pomegranate.aether/maven-central
                           {"clojars" "http://clojars.org/repo"})
   :dj/packages (dj.repo/default-packages}))

(def sys (init-sys))
