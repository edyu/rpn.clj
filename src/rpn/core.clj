(ns rpn.core)

(defn runloop []
  (loop [line (read-line)]
    (if (or (nil? line) (= line "q"))
      (println "goodbye")
      (do
        (println line)
        (recur (read-line))))))

(defn -main [] (runloop))
