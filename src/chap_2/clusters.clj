(ns chap_2.clusters
  (:use midje.sweet
        utils.misc)
  (:require
    [utils.blogs]))

(defn pearson-vector-distance
  "Pearson correlation for vectors, inverted to show a distance.
  0 for perfect matches, 1 for totally different."
  [v1 v2]
  (let [s1 (sum v1)
        s2 (sum v2)
        ss1 (sum (map square v1))
        ss2 (sum (map square v2))
        sp12 (sum (map * v1 v2))
        den (Math/sqrt
              (* (- ss1 (/ (square s1) (count v1)))
                 (- ss2 (/ (square s2) (count v2)))))
        numer (- sp12 (/ (* s1 s2) (count v1)))]
    ; TODO this isn't what the book uses,
    ; but that gave 2 (rather than 1) for the "orthogonal" case
    (if (= den 0) 0 (+ 0.5 (- (/ (/ numer den) 2))))))

(facts
  (fact "Identical vectors will have a 0 distance"
        (pearson-vector-distance [0 1 0 2]
                                 [0 1 0 2]) => 0.0)
  (fact "Opposite will have a 1 distance"
        (pearson-vector-distance [3 0 3 0]
                                 [0 3 0 3]) => 1.0)
  (fact "Very similar vectors will be low distance"
        (< (pearson-vector-distance [0 1 1 0]
                                 [0 1 1 0.1]) 0.5) => true))

; Leaf cluster:
; [{:vec ... :tag ...}]
; Internal:
; [{:vec ... :tag ...} child1 child2]

(defn hcluster
  [rows & {:keys [distance]
           :or {distance pearson-vector-distance}}]
  (let [memo-dist (memoize distance)]
    (loop [clusts (for [[k v] rows] [{:tag k :vec v}])]
      (if (= 1 (count clusts)) (first clusts)
        (let [[_ [[{vecc1 :vec} & _ :as c1]
                  [{vecc2 :vec} & _ :as c2]]]
              (first
                (sort-by first
                         (for [[{vecc1 :vec} & _ :as c1] clusts
                               [{vecc2 :vec} & _ :as c2] clusts
                               :when (not (= c1 c2))]
                           [(memo-dist vecc1 vecc2) [c1 c2]])))]
          (recur (conj
                   (remove #(or (= %1 c1) (= %1 c2)) clusts)
                   [{:vec (vec (map (fn [a b]
                                      (/ (+ a b) 2)) vecc1 vecc2))}
                    c1 c2])))))))

(defn strip-to-tags
  [[{:keys [tag]} & children]]
  (cond
    (and children tag) (apply vector
                              tag
                              (map strip-to-tags children))
    tag tag
    children (apply vector (map strip-to-tags children))))


(fact
  "Similar items are put in the same branches"
  (strip-to-tags (hcluster
                   [[:a [0 0 0 0 1.0]]
                    [:b [0 0 0 0 1.5]]
                    [:c [0 2.0 2.0 0 0]]
                    [:d [0 1.8 1.8 0 0]]])) =>
  ; Matching trees that don't care about order is hard...
  (just [(just [:b :a] :in-any-order)
         (just [:c :d] :in-any-order)] :in-any-order))

(def down-over \u2514)

(defn print-hcluster
  "TODO finish this function!"
  [{:keys [tag]} & children]
  (println "_" (if tag tag ""))
  (doseq [child children]
    (print-hcluster child)))
