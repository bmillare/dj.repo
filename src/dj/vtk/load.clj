(ns dj.vtk.load
  (:require [dj.classloader]))

(defn install []
  "#for arch\nsudo pacman -Syu vtk")

(defn load []
  (let [;; ex: "/usr/lib64/vtk-5.10"
        native-library-directory-path (-> (into []
                                                (comp
                                                 (map dj.io/get-path)
                                                 (filter #(re-find #"vtk" %)))
                                                (dj.io/ls (dj.io/file "/usr/lib64")))
                                          first)
        native-library-names (into []
                                   (comp
                                    (filter #(re-find #"Java\.so$" (dj.io/get-path %)))
                                    (map dj.io/get-name)
                                    (map (fn [s]
                                           (second (re-matches #"lib(vtk.+?Java)\.(so|dll)" s)))))
                                   (dj.io/ls (dj.io/file native-library-directory-path)))]
    (dj.classloader/add-classpath "/usr/share/java/vtk/vtk.jar")
    (dj.classloader/append-native-path! [native-library-directory-path])
    
    (doseq [l native-library-names]
      (clojure.lang.RT/loadLibrary l))))









