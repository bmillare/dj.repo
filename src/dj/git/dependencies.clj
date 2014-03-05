(ns dj.git.dependencies
  (:require [dj.git]
            [dj.io]
            [dj.dependencies :as dd]))

(defmethod resolve-dj-dependency :git [entry-obj system]
  (let [f (resolve-path (:name entry-obj)
                        system)]
    (when-not (dj.io/exists? f)
      (dj.git/clone (:git-path entry-obj)
                    system))
    (resolve-project (dj.io/get-path f))))

(defn update-source-dependencies [path system]
  (->> (dd/project-source-dependencies path system)
       (reduce (fn [ret d]
                 (let [local-file (-> ((case (:dependency-type d)
                                         :source :relative-path
                                         :git :name) d)
                                      dj.git/proj)]
                   (if (dj.io/exists? (dj.io/file local-file ".git"))
                     (assoc ret
                       d
                       (dj.git/pull local-file))
                     (assoc ret
                       d
                       nil))))
               {})))
