(ns utils.misc)

(defn ->?
  [val tag]
  (prn tag " is " val)
  val)

(defn ->>?
  [tag val]
  (->? val tag))

(defn ->?-fn
  [inner-fn tag & {:keys [args-only]
                   :or [args-only true]}]
  (fn [& args]
    (let [res (apply inner-fn args)]
      (if args-only
        (prn "(" inner-fn args ") called")
        (prn "(" inner-fn args ") is " res))
      res)))

(defn ->>?-fn
  [tag inner-fn & more]
  (apply ->?-fn inner-fn tag more))

(defmacro ?
  [val]
  `(let [x# ~val]
      (prn '~val '~'is x#)
      x#))

(def sum
  (partial reduce + 0))

(defn square [x]
  (* x x))
