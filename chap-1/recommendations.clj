(require 'clojure.set) 
(def keys-set
  (comp set keys))

(def sum
  (partial reduce + 0))

(defn square [x]
  (* x x))

(defn mutually-rated
  [scores1 scores2]
  (clojure.set/intersection
    (keys-set scores1)
    (keys-set scores2)))

(defmacro ?
  [val]
  `(let [x# ~val]
      (prn '~val '~'is x#)
      x#))

(defn euclidean-distance
  [table key1 key2]
  (letfn [(square [x] (* x x))]
    (let [scores1 (table key1)
          scores2 (table key2)
          shared (mutually-rated scores1 scores2)]
      (if (empty? shared)
        0
        ; 1 / 1 + distance
        ; inversion is for
        ; making equal 1 and
        ; totally different 0
        (/ 1 (+ 1 (sum 
                    (map (fn [x]
                           (square (- (scores1 x) (scores2 x))))
                         shared)
                    )))
        )
      ))
  )

(def sum-vals
  (comp sum vals))

(def vals-map
  #(map %1 (vals %2)))

(def filter-keys
  #(reduce conj {} (filter (fn [[k v]] (%1 k)) %2)))

(defn pearson-correlation
  [table key1 key2]
  (let [scores1 (table key1)
        scores2 (table key2)
        shared (mutually-rated scores1 scores2)
        ; filters out non-shared items
        shared-filter (partial filter-keys shared)
        shared1 (shared-filter scores1)
        shared2 (shared-filter scores2)]
    ; correlation is 0 if nothing shared
    (if (empty? shared)
      0
      (let [n (count shared)
            ss-vals (comp sum (partial vals-map square))
            sum-pref1 (sum-vals shared1)
            sum-pref2 (sum-vals shared2)
            ss-pref1  (ss-vals shared1)
            ss-pref2  (ss-vals shared2)
            sum-prods (sum (map #(* (scores1 %1) (scores2 %1)) shared))
            den-helper #(- %1 (/ (square %2) n))
            den (Math/sqrt (* 
                             (den-helper ss-pref1 sum-pref1)
                             (den-helper ss-pref2 sum-pref2)))]
        (if (= 0 den)
          0
          (let [num (- sum-prods (/ (* sum-pref1 sum-pref2) n))]
            (/ num den)
            )
          )
        )
      )
    )
  )

(defn top-matches
  [prefs person & {:keys [n similarity]
                   :or {n 5 similarity pearson-correlation}}]
  (take n (sort-by #(- (similarity prefs person %)) (keys prefs))))

(def set-filter
  (comp set filter))

(defn transform-prefs
  [prefs]
  (reduce
    (fn 
      [inverted1 [reviewer item-map]]
      (reduce
        (fn
          [inverted2 [item score]]
          (assoc-in inverted2 [item reviewer] score)
          )
        inverted1
        item-map))
      {}
      prefs))

(defn get-recommendations
  [prefs person & {:keys [similarity]
                   :or {similarity pearson-correlation}}]
  (let [sum (partial reduce +)
        unseens (clojure.set/difference
                  (apply clojure.set/union (map (comp set keys) (vals prefs)))
                  (set (keys (prefs person))))
        others (keys (dissoc prefs person))
        sims (memoize (partial similarity prefs person))
        sim-others (filter #(> (sims %1) 0) others)
        totals (fn [item]
                 (sum (map
                        #(* (or (get-in prefs [%1 item]) 0)
                            (sims %1))
                        sim-others)))
        simSum (fn [item]
                 (sum (map
                        #(sims %1)
                        sim-others)))]
    (sort-by #(- (%1 0))
             (for [unseen unseens]
               [(/ (totals unseen)
                   (simSum unseen))
                unseen])
    )
  )
  )

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



(def critics {
  "Lisa Rose" {
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
