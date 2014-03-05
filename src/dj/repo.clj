(ns dj.repo
  (:require [dj]
            [dj.classloader :as dc]
            [dj.dependencies :as dd]
            [clojure.tools.namespace.file :as ctnf]
            [clojure.tools.namespace.parse :as ctnp]))

(defn default-packages
  "put in system state :dj/packages"
  []
  {:namespace-dependencies '{"dj.cljs" [#_ [org.clojure/clojurescript ""]]
                             "dj.web" []
                             "dj.web.server" [[ring/ring-jetty-adapter "1.2.1"]]
                             "dj.sgml" []
                             "dj.io.extended" [[org.apache.directory.studio/org.apache.commons.io "2.4"]]
                             "dj.git" [[org.eclipse.jgit/org.eclipse.jgit "2.0.0.201206130900-r"]]
                             "dj.git.dependencies" []
                             "dj.leiningen" [[leiningen-core "2.1.3"]]
                             "dj.install" []
                             "" []}})

(defn dependent-namespaces [repo-relative-path]
  (-> repo-relative-path
      dc/find-resource
      ctnf/read-file-ns-decl
      ctnp/deps-from-ns-decl))

(defn recursive-namespace-dependencies [namespace-names namespace-dependencies]
  (set (apply concat (for [n namespace-names
                           :let [dependencies (namespace-dependencies (name n))]
                           :when dependencies]
                       dependencies))))

(defn get-jar-dependencies* [repo-relative-path namespace-dependencies]
  (-> repo-relative-path
      dependent-namespaces
      (recursive-namespace-dependencies namespace-dependencies)))

(defn ns-name->repo-relative-path [n]
  (str (dj/replace-map n {"." "/"})
       ".clj"))

(defn get-jar-dependencies [ns:name sys]
  (let [namespace-dependencies (-> sys
                                   :dj/packages
                                   :namespace-dependencies)]
    (-> ns:name
        ns-name->repo-relative-path
        (get-jar-dependencies* namespace-dependencies)
        (into (namespace-dependencies (name ns:name))))))

(defn resolve-jar-dependencies [ns:name sys]
  (-> (get-jar-dependencies ns:name sys)
      (dd/add-dependencies sys)))

