(ns dj.vtk.load
  (:require [dj.classloader]))

(defn install []
  "#for arch\nsudo pacman -Syu vtk")

(defn load []
  (dj.classloader/add-classpath "/usr/share/java/vtk/vtk.jar")
  (dj.classloader/append-native-path! [(-> (into []
                                                 (comp
                                                  (map dj.io/get-path)
                                                  (filter #(re-find #"vtk" %)))
                                                 (dj.io/ls (dj.io/file "/usr/lib64")))
                                           first)]) ; ex: "/usr/lib64/vtk-5.10"
  (doseq [l ["vtkCommonJava"
             "vtkFilteringJava"
             "vtkIOJava"
             "vtkImagingJava"
             "vtkGraphicsJava"
             "vtkRenderingJava"]]
    (clojure.lang.RT/loadLibrary l)))

