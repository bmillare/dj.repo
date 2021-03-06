(ns dj.repo
  (:require [dj]
            [dj.classloader :as dc]
            [dj.dependencies :as dd]
            [clojure.tools.namespace.file :as ctnf]
            [clojure.tools.namespace.parse :as ctnp]))

(defn default-packages
  "put in system state :dj/packages"
  []
  ;; empty lists are for documentation purposes only
  ;; Non-existent entries and empty entries are treated the same
  {:namespace-dependencies '{"dj.color.convert" [[primitive-math "0.1.3"]]
                             "dj.cljs" [#_ [org.clojure/clojurescript ""]] ; deprecated, see scratch/gallook
                             "dj.cljs.repl" [] ; deprecated, see scratch/gallook
                             "dj.web" []
                             "dj.web.server" [[http-kit "2.1.18" :exclusions [org.clojure/clojure
                                                                              org.clojure/clojurescript]]]
                             "dj.io.extended" [[org.apache.directory.studio/org.apache.commons.io "2.4"]]
                             "dj.git" [[org.eclipse.jgit/org.eclipse.jgit "2.0.0.201206130900-r"]]
                             "dj.git.dependencies" []
                             "dj.leiningen" [[leiningen-core "2.1.3"]]
                             "dj.install" []
                             "dj.chm" [[org.bmillare/dj.peg "0.3.1"]]
                             "dj.search" []
                             "dj.security" []
                             "dj.jna" [[net.java.dev.jna/jna "4.1.0"]]
                             "" []}})

(defn dependent-namespaces
  "get namespace dependencies from ns form"
  [repo-relative-path]
  (-> repo-relative-path
      dc/find-resource
      ctnf/read-file-ns-decl
      ctnp/deps-from-ns-decl))

(defn ns-name->repo-relative-path [n]
  (str (dj/replace-map n {"." "/"})
       ".clj"))

(defn recursive-dependent-namespaces
  "like dependent-namespaces but gets all dependent namespaces recursively"
  [repo-relative-path]
  (loop [ret #{}
         new:ns:names (dependent-namespaces repo-relative-path)]
    (if (empty? new:ns:names)
      ret
      (let [new:ns:name (first new:ns:names)
            ns:dependents (try
                            (-> new:ns:name
                                ns-name->repo-relative-path
                                dependent-namespaces)
                            (catch Exception e
                              #{}))
            new:ns:dependents (clojure.set/difference ns:dependents
                                                      ret)]
        (if (empty? new:ns:dependents)
          (recur ret (disj new:ns:names new:ns:name))
          (recur (clojure.set/union ret new:ns:dependents)
                 (clojure.set/union new:ns:names new:ns:dependents)))))))

(defn recursive-namespace-dependencies
  "really this looks up and then returns the project dependencies of
  the namespace"
  [namespace-names namespace-dependencies]
  (set (apply concat (for [n namespace-names
                           :let [dependencies (namespace-dependencies (name n))]
                           :when dependencies]
                       dependencies))))

(defn get-jar-dependencies* [repo-relative-path namespace-dependencies]
  (-> repo-relative-path
      recursive-dependent-namespaces
      (recursive-namespace-dependencies namespace-dependencies)))

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

