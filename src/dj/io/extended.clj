(ns dj.io.extended)

(defn copy-url-to-file [url ^java.io.File f]
  (org.apache.commons.io.FileUtils/copyURLToFile (java.net.URL. url)
						 f))
