(ns rpn.core)

(defn str->num [str]
  (if (number? str)
    str
    (let [n (read-string str)]
      (if (number? n) n nil))))

(defn do-simple-math [stack]
  (try
    (apply (eval (symbol (first stack))) (map #(str->num %) (take 2 (rest stack))))
    (catch Exception e (println e))))

(defn calculate [stack]
  (let [op (first stack)]
    (cond
      (re-find #"\+|-|\*|/" op)
        (conj (drop 3 stack)
          (do-simple-math stack))
      :else
        stack)))

(defn runloop []
  (loop [line (read-line)
         stack ()]
    (if (or (nil? line) (= line "q"))
      (println "goodbye")
      (let [op line
            stack (calculate (conj stack op))]
        (do
          (println (first stack))
          (println (str "stack: " stack))
          (recur (read-line) stack))))))

(defn -main [] (runloop))
