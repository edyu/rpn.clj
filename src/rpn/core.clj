(ns rpn.core
  (:use [clojure.string :only (blank? lower-case split)])
  (:import (java.lang Math)
           (java.lang.reflect Modifier)))

(defn check-stack [stack n]
  (>= (count stack) n))

(defn str->num [str]
  (if (number? str)
    str
    (let [n (read-string str)]
      (if (number? n) n nil))))

(defn println-err [& args]
  (binding [*out* *err*]
    (apply println args)
    (flush)))

(defn prn-return [x]
  (prn x)
  x)

(defn get-static-methods [jclass]
  (apply hash-map
         (flatten
           (for [m (filter #(. Modifier isStatic (.getModifiers %)) (.getMethods jclass))]
             (vector (.getName m) m)))))

(defn get-math-funcs []
  (get-static-methods Math))

(defn get-method-arity [jmethod]
  (count (.getParameterTypes jmethod)))

(defn do-math-func [func stack]
  (let [arity (get-method-arity func)]
    (if (check-stack stack arity)
      (try
        (conj (drop arity stack)
              (prn-return (. func invoke nil
                             (object-array (prn-return(reverse
                                                        (map #(double %)
                                                             (take arity stack))))))))
        (catch Exception e
          (println-err e)
          stack))
      (do (println-err "not enough operands on stack") stack))))

(defn do-simple-math [op stack]
  (try
    (let [op-map { "+" +, "-" -, "*" *, "/" / }
          arity 2]
      (conj (drop arity stack)
            (prn-return (apply (op-map op)
                               (map #(str->num %) (reverse (take arity stack)))))))
    (catch Exception e
      (println-err e)
      stack)))

(defn sum [stack]
  (list (prn-return (reduce + stack))))

(defn calculate [op stack]
  (cond
    (re-find #"\+|-|\*|/" op)
      (if (check-stack stack 2)
        (do-simple-math op stack)
        (do (println-err "not enough operands on stack") stack))
    (= (lower-case op) "p")
      (prn-return stack)
    (= (lower-case op) "m")
      (do (prn (keys (get-math-funcs))) stack)
    (= (lower-case op) "sum")
      (if (check-stack stack 1)
        (sum stack)
        (do (println-err "no operand on stack") stack))
    (= (lower-case op) "e")
      (conj stack (prn-return (. Math E)))
    (= (lower-case op) "pi")
      (conj stack (prn-return (. Math PI)))
    (str->num op)
      (conj stack (prn-return (str->num op)))
    (contains? (apply hash-set (keys (get-math-funcs))) (lower-case op))
      (do-math-func ((get-math-funcs) (lower-case op)) stack)
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
  (loop [stack ()]
    (let [line (read-prompt)]
      (if (or (nil? line) (= line "q"))
        (println "goodbye")
        (if (not (blank? line))
          (recur (process-line (split line #"\s+") stack))
          (recur stack))))))

(defn -main [] (runloop))
