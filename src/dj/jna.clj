(ns dj.jna
  (:import [com.sun.jna Native]))

(defn ffi-fn-obj [s]
  `(com.sun.jna.Function/getFunction ~(namespace s) ~(name s)))

(defmacro ffi-fn
  "return a function that wrabs a native library function call
(ffi-fn Integer c/printf)
"
  [return-type function-symbol]
  `(let [func# ~(ffi-fn-obj function-symbol)]
     (fn [& args#]
       (.invoke func# ~return-type (to-array args#)))))

(defn cbuffer
  "creates direct bytebuffer of given size with little-endian byte order"
  [size]
  (-> (java.nio.ByteBuffer/allocateDirect size)
      (.order java.nio.ByteOrder/LITTLE_ENDIAN)))

(defn pointer
  "pass in a bytebuffer and returns a JNA pointer that can be passed directly to native functions"
  [direct-buffer]
  (when direct-buffer
    (Native/getDirectBufferPointer direct-buffer)))
