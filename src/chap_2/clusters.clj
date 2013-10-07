(ns chap-2.clusters
  (:use midje.sweet
        utils.misc)
  (:require
    [clojure.pprint :as pp]
    [utils.blogs :as blogs]
    [net.cgrand.enlive-html :as html]))

(defn pearson-vector-distance
  "Pearson correlation for vectors, inverted to show a distance.
  0 for perfect matches, 1 for totally different."
  [v1 v2]
  (let [s1 (sum v1)
        s2 (sum v2)
        ss1 (sum (map square v1))
        ss2 (sum (map square v2))
        sp12 (sum (map * v1 v2))
        dens (* (- ss1 (/ (square s1)
                          (count v1)))
                 (- ss2 (/ (square s2)
                           (count v2))))
        numer (- sp12 (/ (* s1 s2)
                         (count v1)))]
    ; TODO this isn't what the book uses,
    ; but that gave 2 (rather than 1) for the "orthogonal" case
    (if (= 0 dens)
      0
      (+ 0.5 (- (/ (/ numer
                      (Math/sqrt dens)) 2))))))

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
; [{:vec :dist ... :tag ...} child1 child2]

(defn hcluster
  [rows & {:keys [distance]
           :or {distance pearson-vector-distance}}]
  (let [memo-dist (memoize distance)]
    (loop [clusts (for [[k v] rows] [{:tag k :vec v}])]
      (if (= 1 (count clusts)) (first clusts)
        (let [[calculated-dist [[{vecc1 :vec} & _ :as c1]
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
                                      (/ (+ a b) 2)) vecc1 vecc2))
                     :dist calculated-dist }
                    c1 c2])))))))

(defn strip-to-tags
  [[{:keys [tag]} & children]]
  (cond
    (and children tag) (apply vector
                              tag
                              (map strip-to-tags children))
    tag tag
    children (apply vector (map strip-to-tags children))))

(def simple-test-data
  [[:a [0 0   0   0 1.0]]
   [:b [0 0   0   0 1.5]]
   [:c [0 2.0 2.0 0 0  ]]
   [:d [0 1.8 1.8 0 0  ]]])

(fact
  "Similar items are put in the same branches"
  (strip-to-tags (hcluster simple-test-data)) =>
  ; Matching trees that don't care about order is hard...
  (just [(just [:b :a] :in-any-order)
         (just [:c :d] :in-any-order)] :in-any-order))

(defn print-hcluster
  [clust]
  (pp/pprint (strip-to-tags clust)))

;(print-hcluster (hcluster wcs))

; TODO funcs for dendogram drawing
(defn get-height
  [_ & children]
  (if children
    (sum (map get-height children))
    1))

(defn get-depth
  [{distance :dist} & children]
  (if children
    (+ (reduce #(Math/max %1 %2) (map get-depth children)) distance)
    0))

(defn row-mins-and-maxs
  "Calculate per-dimension mins and maxs for a table of vector data"
  [rows]
  (reduce 
    (fn [[mins maxs] [k v]]
         [(map #(Math/min %1 %2) mins v)
          (map #(Math/max %1 %2) maxs v)])
    [(repeat (count (nth (first rows) 1)) Integer/MAX_VALUE)
     (repeat (count (nth (first rows) 1)) Integer/MIN_VALUE)] rows))1

(defn random-vector-in-ranges
  "Produce a random point using the given generator 
  within the per-element minimum and maximum bounds supplied"
  [r mins maxs]
  (map (fn [minn maxx]
         (+ (* (.nextFloat r) (- maxx minn))
            minn))
       mins maxs))

(defn k-random-points-in-data
  "Pick k uniformly random points located within the bounds
  of the data.  Use an optional seed for deterministic results"
  [rows k & [seed]]
  (let [r (if seed
            (java.util.Random. seed)
            (java.util.Random.))
        [mins maxs] (row-mins-and-maxs rows)]
    (repeatedly k #(random-vector-in-ranges r mins maxs))))

(defn find-nearest-neighbor
  "Find the nearest neighbor for a vector from a set of neighbors using the
  passed-in distance metric"
  [neighbors distance v]
  (let [dist-pairs (for [neighbor neighbors]
                     [(distance neighbor v) neighbor])]
    (nth (first (sort-by first dist-pairs)) 1)))

(defn select-clusters
  "Print only the cluster keys to just show categories"
  [clusters]
  (for [[centroid rows] clusters] (keys rows)))

(defn kcluster
  "Cluster the rows using K Means, starting with initally random centroids"
  [rows & {:keys [distance k seed iter-limit]
           :or {distance pearson-vector-distance k 4 iter-limit 10 seed nil}}]
  (loop [starting-centroids (set (k-random-points-in-data rows k seed))
         iter 0]
    (let [grouped (group-by
                    (comp
                      (partial find-nearest-neighbor starting-centroids distance)
                      #(nth % 1))
                    rows)
          updated-centroids
          (set (for [grouping (map vals (vals grouped))
                     :let [cnt (count grouping)] ; don't recount this alot
                     :when (> cnt 0)] ; filter out abandoned centroids
              (vec (map (divide-by cnt)
                        (reduce pointwise-sum grouping)))))]
      (println "K-Means Iteration: " iter
               " Current cluster counts: " (map count (vals grouped)))
      (if (or (> iter iter-limit)
              (= starting-centroids updated-centroids))
        grouped
        (recur updated-centroids (inc iter))))))

(def simple-test-data2
  [[:a (concat [2 2] (repeat 8 0))]
   [:b (concat [3 3] (repeat 8 0))]
   [:c (concat [3 2] (repeat 8 0))]

   [:d (concat (repeat 7 1) [5 -5 5])]
   [:e (concat (repeat 7 1) [5 -6 7])]
   [:f (concat (repeat 7 1) [4 -7 6])]

   [:g (apply concat (repeat 5 [2 -2]))]
   [:h (apply concat (repeat 5 [2 -1.9]))]
   [:i (apply concat (repeat 5 [2.1 -2]))]

   [:k (concat (repeat 5 2) (repeat 5 -2))]
   [:l (concat (repeat 5 4) (repeat 5 -4))]
   [:m (concat (repeat 5 3) (repeat 5 -3))]])

(fact "K-Means can discover clusters of organized data points"
      (select-clusters (kcluster simple-test-data2 :seed 1)) =>
      (just [(just [:a :b :c] :in-any-order)
             (just [:d :e :f] :in-any-order)
             (just [:g :h :i] :in-any-order)
             (just [:k :l :m] :in-any-order)] :in-any-order))

; (def wcs (blogs/load-blog-data))
; 
; (pp/pprint 
;   (try
;     (select-clusters
;       (kcluster wcs :seed 1))
;     (catch Exception e
;       (.printStackTrace e))))

; TODO add zebo data
; Implement Multi-Dimensional Scaling
; Implement dendrogram randering
