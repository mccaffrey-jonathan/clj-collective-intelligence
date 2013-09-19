(ns chap-1.recommendations
  (:use clojure.test
        midje.sweet
        utils.misc)
  (:require [clojure.set]))

(def critics 
  "Critics is a map of names of movie critics to movies they have reviewed and
  the scores that they gave each of them.  It can be used to find similar
  critics, estimate how a critic will review a new movie, or pick movies a
  critic would want to see"
  { "Lisa Rose" {
      "Lady in the Water" 2.5,
      "Snakes on a Plane" 3.5,
      "Just My Luck" 3.0,
      "Superman Returns" 3.5,
      "You, Me and Dupree" 2.5,
      "The Night Listener" 3.0 },
  "Gene Seymour" {
      "Lady in the Water" 3.0, 
      "Snakes on a Plane" 3.5,
      "Just My Luck" 1.5,
      "Superman Returns" 5.0,
      "The Night Listener" 3.0,
      "You, Me and Dupree" 3.5 },
  "Michael Phillips" {
      "Lady in the Water" 2.5,
      "Snakes on a Plane" 3.0,
      "Superman Returns" 3.5,
      "The Night Listener" 4.0 },
  "Claudia Puig" {
      "Snakes on a Plane" 3.5,
      "Just My Luck" 3.0,
      "The Night Listener" 4.5,
      "Superman Returns" 4.0,
      "You, Me and Dupree" 2.5 },
  "Mick LaSalle" {
      "Lady in the Water" 3.0,
      "Snakes on a Plane" 4.0,
      "Just My Luck" 2.0,
      "Superman Returns" 3.0,
      "The Night Listener" 3.0,
      "You, Me and Dupree" 2.0 },
  "Jack Matthews" {
      "Lady in the Water" 3.0,
      "Snakes on a Plane" 4.0,
      "The Night Listener" 3.0,
      "Superman Returns" 5.0,
      "You, Me and Dupree" 3.5 },
  "Toby" {
      "Snakes on a Plane" 4.5,
      "You, Me and Dupree" 1.0,
      "Superman Returns" 4.0 }})

(def keys-set
  (comp set keys))

(def sum
  (partial reduce + 0))

(defn square [x]
  (* x x))

(defn mutually-rated
  "filter out items in both score maps"
  [scores1 scores2]
  (clojure.set/intersection
    (keys-set scores1)
    (keys-set scores2)))

(defn euclidean-distance
  "Return the euclidean distance 2 score maps, viewing each item
  potentially scored as a dimension"
  [scores1 scores2]
  (let [shared (mutually-rated scores1 scores2)]
    (if (empty? shared)
      0
      ; 1 / 1 + distance
      ; inversion is for
      ; making equal 1 and
      ; totally different 0
      (/ 1 (+ 1 (sum 
                  (map (fn [x]
                         (square (- (scores1 x) (scores2 x))))
                       shared)))))))

(defn transformed-euclidean-distance
  "Transform the euclidean distance so that 1 is identical and 0 is very
  different"
  [scores1 scores2]
  (/ 1 (inc (euclidean-distance scores1 scores2))))

(def vals-map
  #(map %1 (vals %2)))

(def ss-vals
  (comp sum (partial vals-map square)))

(def sum-vals
  (comp sum vals))

(def filter-keys
  #(reduce conj {} (filter (fn [[k v]] (%1 k)) %2)))

(defn shared-filter
  "Filter out map items that aren't in both maps"
  [m1 m2]
  (let [shared (mutually-rated m1 m2)]
    [(filter-keys shared m1) (filter-keys shared m2)]))

(defn pearson-correlation
  "Return the Pearson correlation of 2 scores maps.  The Pearson correlation is
  covariance of the 2 maps divided by the product of the std deviations"
  [scores1 scores2]
  (let [[shared1 shared2] (shared-filter scores1 scores2)]
    ; correlation is 0 if nothing shared
    (if (empty? shared1)
      0
      (let [n (count shared1)
            sum-pref1 (sum-vals shared1)
            sum-pref2 (sum-vals shared2)
            sum-prods (sum-vals (merge-with * shared1 shared2))
            den-helper #(- %1 (/ (square %2) (count shared1)))
            den (Math/sqrt (* (den-helper (ss-vals shared1) sum-pref1)
                              (den-helper (ss-vals shared2) sum-pref2)))]
        (if (= 0 den)
          0
          (/ (- sum-prods
                (/ (* sum-pref1 sum-pref2)
                   n))
             den))))))

(defn tanimoto-similarity
  "Returns the tanimoto similarity; the number of common items 
  divided by the total number of items between the 2 score maps. Useful
  for binary vectors like Users who follow Topics"
  [scores1 scores2]
  (let [ks1 (set (keys scores1))
        ks2 (set (keys score2))]
  (/ (count clojure.set/intersection ks1 ks2)
     (count clojure.set/union ks1 ks2))))

(defn top-matches
  "Take a map of score maps and a key and return the other keys with the most
  similar score maps.  Takes an option map defining how many top matches to
  return and which similarity metric to use."
  [prefs k & {:keys [n similarity]
              :or {n 5 similarity pearson-correlation}}]
  (take n (sort-by #(- (similarity (prefs k) (prefs %)))
                   (remove (partial = k) (keys prefs)))))

(fact "we can find similar critics using top-matches"
      (first (top-matches critics "Toby")) => "Lisa Rose")

(defn transform-prefs
  "Invert a 2-level preference map, so that a map of critics -> items -> scores
  becomes a map of items -> critics -> scores and vice verse"
  [prefs]
  (apply merge-with merge
         (for [[ok ov] prefs
               [ik iv] ov]
           {ik {ok iv}})))

(facts
  (fact "The inner and outer keys are revered"
        (= (get-in critics ["Lisa Rose" "Lady in the Water"])
           (get-in (transform-prefs critics) ["Lady in the Water" "Lisa Rose"]))
        => true)
  (fact "With transformed preferences, we can match items to similar items"
        (first (top-matches (transform-prefs critics) "Superman Returns"))
        => "You, Me and Dupree"))

(defn get-recommendations
  "Get recommendations for a person based on a similarity-weighted average of
  other person's recommendations"
  [prefs k & {:keys [similarity] :or {similarity pearson-correlation}}]
  (let [other-prefs (dissoc prefs k)
        ; short-hand for similarity to k
        k-sim (memoize (fn [k2]
                         (similarity (prefs k) (prefs k2))))
        ; Weight items reviewed by other ks by their similarity to k
        sim-weighted-other-prefs (into {}
                                       (for [[k2 scores] other-prefs
                                             :let [k2-sim (k-sim k2)]]
                                         [k2 (into {}
                                                   (for [[item score] scores]
                                                     [item (* score k2-sim)]))]))]
    (->> 
      sim-weighted-other-prefs
      ; invert map to items -> critics 
      transform-prefs
      ; transform each map of critics to a weighted review estimate
      (map 
        (fn [[item critics-scores]]
          [item (/ (sum-vals critics-scores)
                   (sum (map k-sim (keys critics-scores))))]))
      ; sort by review estimate
      (sort-by #(- (nth % 1)))
      ; drop already seen items
      (remove #((prefs k) (nth % 0))))))

(facts
  (fact "We can get item recommendations with get-recommendations"
        (first (first
                 (get-recommendations critics "Toby"))) => "The Night Listener")
  (fact "This isn't affected too much by similarity function choice"
        (first (first
                 (get-recommendations critics "Toby" :similarity transformed-euclidean-distance))) => "The Night Listener"))

; get-recommendatoins took me many tries to get correct ending in shame and
; confusion. Preserving mistakes for posterity.
; (def set-filter
;   (comp set filter))
; 
;  (letfn [(sum [xs]
;          (reduce + xs))
;          (totals [item]
;          (sum (vals prefs)
;
;          ]

;  (let [others (keys (dissoc prefs person))
;        unseens (clojure.set/minus
;                  (apply clojure.set/union (map set (keys (vals prefs))))
;                  (set (keys (prefs person))))
;        totals (apply merge-with + (vals prefs))
;        sims (memoize (partial similarity prefs person))]
;    (sort-by #(- (% 1))
;             (into {}
;               (for [unseen unseens]
;                 [unseen (/ )])
;               )
;             ))
                 
;  (let [item-pref-scores
;        (for [other (keys prefs) :when (not (= person))]
;          (let [sim (similarity prefs person other)
;                my-prefs (prefs person)]
;            (if (> sim 0)
;              (for [item-score (prefs other)
;                    :when (not (my-prefs (item-score 0)))]
;                {:item item :pref (item-score 0) :sim sim})
;              ) 
;            )
;          )
;        sim-sums 
;        ]
;
;    )
;  )

;  (let [items (set-filter #(not ((prefs person) %))
;                (reduce clojure.set/union 
;                        (map (comp set keys) (vals prefs))))]
;    (sort-by #(- (nth % 1)) 
;             (map
;               (fn [item]
;                 [item
;                  (/ 
;                    )])
;               items)
;             )
;    )
;
;  (map (partial * (similarity prefs person other))


