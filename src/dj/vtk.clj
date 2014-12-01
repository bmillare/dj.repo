(ns dj.vtk
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn open-byu
  "
pathIn: filepath to byu file to read

returns ignore value, nvert, nface, nconn, V and F
converts F negative third argument to positive
"
  [pathIn] 
  (with-open [rdr (io/reader pathIn)]
    (let [
          [first-line second-line & rest-lines] (line-seq rdr)
          [a nvert nface nconn] (map #(Integer/parseInt %) (str/split first-line #" "))
          all-str-nums (partition 3 (mapcat #(str/split % #"\s+") rest-lines))
          V-lines (take nvert all-str-nums)
          V (mapv (fn [line]
                    (mapv #(Double/parseDouble %)
                          line)) V-lines)
          F-lines (take-last nface all-str-nums)
          F (mapv (fn [line]
                    (mapv #(Math/abs (Integer/parseInt %))
                          line)) F-lines)]
      [nvert nface nconn V F])))

(defn triangle-mesh-polydata
  "returns polydata with mesh data"
  [vertices faces]
  (let [vtk-points (vtk.vtkPoints.)
        vtk-face-cells (vtk.vtkCellArray.)]
    (.SetNumberOfPoints vtk-points (int (count vertices)))
    (doall (map-indexed (fn [i [x y z]]
                          (.SetPoint vtk-points
                                     (int i)
                                     (int x)
                                     (int y)
                                     (int z)))
                        vertices))
    (doall (map-indexed (fn [i [a b c]]
                          (doto vtk-face-cells
                            (.InsertNextCell (int 3))
                            (.InsertCellPoint (int (dec a)))
                            (.InsertCellPoint (int (dec b)))
                            (.InsertCellPoint (int (dec c)))))
                        faces))
    (doto (vtk.vtkPolyData.)
      (.SetPoints vtk-points)
      (.SetPolys vtk-face-cells))))

(defn byu-polydata [path]
  (let [[nvert nface nconn V F] (open-byu path)]
    (triangle-mesh-polydata V F)))
