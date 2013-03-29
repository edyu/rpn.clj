(ns rpn.core)
(use '[clojure.string :only (lower-case split)])

(defn str->num [str]
  (if (number? str)
    str
    (let [n (read-string str)]
      (if (number? n) n nil))))

(defn print-return [x]
  (prn x)
  x)

(defn do-simple-math [op stack]
  (try
    (conj (drop 2 stack)
          (print-return (apply (eval (symbol op)) (map #(str->num %) (take 2 stack)))))
    (catch Exception e
      (println e)
      stack)))

(defn sum [stack]
  (list (print-return (reduce + stack))))

(defn calculate [op stack]
  (cond
    (re-find #"\+|-|\*|/" op)
      (do-simple-math op stack)
    (= (lower-case op) "p")
      (print-return stack)
    (= (lower-case op) "sum")
      (sum stack)
    (str->num op)
      (conj stack (str->num op))
    :else
      stack))

(defn process-line [line stack]
  (letfn [(process [op stack line]
            (if (nil? op)
              stack
              (recur (first line) (calculate op stack) (rest line))))]
    (process (first line) stack (rest line))))

(defn read-prompt []
  (print "> ")
  (flush)
  (read-line))

(defn runloop []
  (loop [line (read-prompt)
         stack ()]
    (if (or (nil? line) (= line "q"))
      (println "goodbye")
      (let [ops (split line #"\s+")
            newstack (process-line ops stack)]
        (recur (read-prompt) newstack)))))

(defn -main [] (runloop))
