(ns utils.delicious
  (:refer-clojure :exclude [resolve])
  (:use clojurewerkz.urly.core
        midje.sweet
        utils.misc)
  (:require [digest]
            [rate-gate.core]
            [clj-http.client]
            [clojure.set]
            [clojure.data.json :as json]))

; http://feeds.delicious.com/v2/{format}/recent
; Recent bookmarks by tag:
; http://feeds.delicious.com/v2/{format}/tag/{tag[+tag+...+tag]}
; {format} = replaced with either ârssâ or âjsonâ
; Delicious TOS

(def rate-get
  (->
    clj-http.client/get
    (->?-fn "rate-get url" :args-only true) ; print so we can see some progress
    (rate-gate.core/rate-limit 1 1000)))

(defn rate-get-json-body
  [url]
  (-> url
      rate-get
      :body
      json/read-str))

(defn get-popular
  [tag n]
  (rate-get-json-body
    (str "http://feeds.delicious.com/v2/json/tag/" tag "?count=" n)))

(defn get-popular-urls
  [tag n]
  (map (comp first vals)
       (clojure.set/project (get-popular tag n) ["u"])))

(facts :slow
  (fact "we can control the returned count"
        (count (get-popular "programming" 1)) => 1)
  (fact "we can project out fields from the array of maps"
        (count (get-popular-urls "programming" 1)) => 1))

;(rate-get
;  (str "https://api.delicious.com/v1/posts/get?url="
;       (encode-query "http://journeyintoearlychildhood.weebly.com/the-importance-of-block-play.html")))

(defn get-urlposts
  [url]
  (rate-get-json-body
    (str "http://feeds.delicious.com/v2/json/url/" (digest/md5 url))))

(defn get-popular-posts
  [tag]
  (mapcat get-urlposts (get-popular-urls tag 10)))

(defn get-popular-post-users
  [tag]
  (map (comp first vals)
       (clojure.set/project (get-popular-posts tag) ["a"])))

(fact :slow "we can look up many posts about a topic"
      (every? #(some #{"programming"} (% "t")) 
              (get-popular-posts "programming")) => true)

(defn get-recent-posts-for-user
  [username]
  (rate-get-json-body
    (str "http://feeds.delicious.com/v2/json/" username)))

; TODO less repetive, higher-order func way of doing this
(defn get-recent-post-urls-for-user
  [username]
  (map (comp first vals)
       (clojure.set/project
         (get-recent-posts-for-user username) ["u"])))

(defn recent-postings-map
  [users]
  (let [sparse (zipmap users 
                       (map (comp
                              (fn [urls]
                                (into {}
                                      (for [url urls]
                                        [url 1])))
                              get-recent-post-urls-for-user)
                            users))
        full-empty-url-map (zipmap
                             (apply clojure.set/union (map (comp set keys)
                                                           (vals sparse)))
                             (repeat 0))]
    (into {}
          (for [[ok ov] sparse]
            [ok (merge full-empty-url-map ov)]))))

(def recent-postings-map-for-popular-post-users
  (comp recent-postings-map get-popular-post-users))

