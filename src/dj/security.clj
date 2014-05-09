(ns dj.security)

(let [generator (java.security.SecureRandom.)]
  (defn random-string []
    (-> (BigInteger. 120 generator)
        (.toString 32))))
