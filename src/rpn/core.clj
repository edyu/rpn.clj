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
      (when (number? n) n))))

(defn prn-err [& args]
  (binding [*out* *err*]
    (apply prn args)
    (flush)))

(defn prn-exp [e]
  (prn-err (.getMessage e)))

(defn prn-ret [x]
  (prn x)
  x)

(defn get-static-methods [jclass]
  (into {}
        (for [m (remove #(= "float" (.getName (.getReturnType %)))
                        (filter #(. Modifier isStatic (.getModifiers %))
                                (.getMethods jclass)))]
          (vector (.getName m) m))))

(defn get-math-funcs []
  (get-static-methods Math))

(defn ratio->double [x]
  (if (ratio? x)
    (double x)
    x))

(defn abs [x]
  (if (< x 0)
    (- x)
    x))

(defn get-method-arity [jmethod]
  (count (.getParameterTypes jmethod)))

(defn do-math-func [func stack]
  (let [arity (get-method-arity func)]
    (if (check-stack stack arity)
      (let [args (object-array (reverse (map #(double %) (take arity stack))))]
        (conj (drop arity stack)
              (prn-ret (.invoke func nil args))))
      (do (prn-err "not enough operands on stack") stack))))

(defn do-simple-math [op stack]
  (let [op-map { "+" +', "-" -', "*" *', "/" / }
        arity 2]
    (conj (drop arity stack)
          (prn-ret (apply (op-map op)
                             (map #(str->num %) (reverse (take arity stack))))))))

(defn sum [stack]
  (list (prn-ret (reduce + stack))))

(defn fac [stack]
  (let [n (abs (bigint (first stack)))]
    (conj (drop 1 stack)
          (prn-ret (reduce * (range 2 (inc n)))))))

(defn fibo []
    (map first (iterate (fn [[a b]] [b (+ a b)]) [0N 1N])))

(defn fib [stack]
  (let [n (abs (int (first stack)))]
    (conj (drop 1 stack)
          (prn-ret (nth (fibo) n)))))

(defn calculate [op stack]
  (try
    (cond
      (re-matches #"^\+|-|\*|/$" op)
        (if (check-stack stack 2)
          (do-simple-math op stack)
          (do (prn-err "not enough operands on stack") stack))
      (= (lower-case op) "c")
        (prn-ret ())
      (= (lower-case op) "m")
        (do (prn (keys (get-math-funcs))) stack)
      (= (lower-case op) "p")
        (prn-ret stack)
      (= (lower-case op) "sum")
        (if (check-stack stack 1)
          (sum stack)
          (do (prn-err "no operand on stack") stack))
      (= op "!")
        (if (check-stack stack 1)
          (fac stack)
          (do (prn-err "no operand on stack") stack))
      (= (lower-case op) "fib")
        (if (check-stack stack 1)
          (fib stack)
          (do (prn-err "no operand on stack") stack))
      (= (lower-case op) "e")
        (conj stack (prn-ret (. Math E)))
      (= (lower-case op) "pi")
        (conj stack (prn-ret (. Math PI)))
      (str->num op)
        (conj stack (prn-ret (str->num op)))
      (contains? (apply hash-set (keys (get-math-funcs))) (lower-case op))
        (do-math-func ((get-math-funcs) (lower-case op)) stack)
      :else
        stack)
    (catch Exception e
      (prn-exp e)
      stack)))

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
