(ns dj.install
  (:require [cemerick.pomegranate :as pom]
            [dj]
            [dj.io]))

;; TODO: add system to load 

;; [org.apache.directory.studio/org.apache.commons.io "2.4"] dj.io.extended
;; [org.eclipse.jgit/org.eclipse.jgit "2.0.0.201206130900-r"] dj.git and dj.leiningen
;; [com.datomic/datomic-free "0.9.4556"]
;; [leiningen-core "2.1.3"]

(defn install-datomic-into-local-maven-repository [datomic-zip-file tmp-dir]
  "
goto http://my.datomic.com/downloads/free and download the newest version of datomic

installs the downloaded version of datomic in local maven repository"
  (let [folder-name (dj/substring (dj.io/get-name datomic-zip-file)
                                  0
                                  -4)
        content-folder (dj.io/file tmp-dir folder-name)
        [_ name version] (re-matches #"(datomic-free)-(.+)"
                                     folder-name)
        pom-file (dj.io/file content-folder "pom.xml")]
    (dj.io/unzip datomic-zip-file
                 tmp-dir)
    (cemerick.pomegranate.aether/install :coordinates ['com.datomic/datomic-free version]
                                         :jar-file (dj.io/file content-folder (str folder-name ".jar"))
                                         :pom-file pom-file)))

