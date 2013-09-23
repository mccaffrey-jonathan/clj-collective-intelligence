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
    (println numer den)
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

; TODO use schema for cluster?
