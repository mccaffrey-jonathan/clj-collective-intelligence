(ns utils.misc)

(defn ->?
  ([val]
   (->? val "->?"))
  ([val tag]
   (println tag " is " val)
   val))

(defn ->>?
  ([val]
   (->>? "->>?" val))
  ([tag val]
   (->? val tag)))

(defn ->?-fn
  [inner-fn tag & {:keys [args-only]
                   :or [args-only true]}]
  (fn [& args]
    (let [res (apply inner-fn args)]
      (if args-only
        (println "(" inner-fn args ") called")
        (println "(" inner-fn args ") is " res))
      res)))

(defn ->>?-fn
  [tag inner-fn & more]
  (apply ->?-fn inner-fn tag more))

(defmacro ?
  [val]
  `(let [x# ~val]
      (println '~val '~'is x#)
      x#))

(def sum
  (partial reduce + 0))

(defn square [x]
  (* x x))
