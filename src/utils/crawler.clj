(ns utils.crawler
  (:import
    [org.jsoup Jsoup])
  (:use midje.sweet
        utils.misc)
  (:require
    [clojurewerkz.urly.core :as urly]
    [clojure.pprint :as pp]
    [clj-http.client]
    [honeysql.core :as sql]
    [honeysql.helpers :refer :all]
    [rate-gate.core]))

; (def +db-path+  "...")
; (def +db-specs+ {:classname  "org.sqlite.JDBC",
;           	     :subprotocol   "sqlite",
;         	     :subname	    +db-path+})
; 
; (def +transactions-query+ "select * from my_table")
; 
; (with-connection +db-specs+
;   (with-query-results results [+transactions-query+]
;     ;; results is an array of column_name -> value maps
;     ))

(fact "urly behaves well enough "
      (urly/relative? "/search") => true
      (urly/relative? (urly/url-like "www.altavista.com")) => false
      (urly/resolve (.toString (urly/url-like "www.google.com")) "/search") =>
      "http://www.google.com/search")

(def +debug-prints+ true)

(defn clean-up-url
  "Clean up a url for crawling found within a page.
  Make it absolute and remove any fragment"
  [page href]
  (-> (urly/url-like
        (if (urly/relative? (urly/url-like href))
          (urly/resolve
            ; java URIs don't recognize raw www's as valid hosts
            (.toString (urly/url-like page))
            href)
          href)) 
      (.withoutFragment)
      (.toString)))

(fact "clean-up-url normalized 'anchors' found in text, removing fragments and
      making them absolute"
      (clean-up-url "www.google.com" "/search") =>
      "http://www.google.com/search"
      (clean-up-url "www.google.com" "www.altavista.com") =>
      "http://www.altavista.com/"
      (clean-up-url
        "http://en.wikipedia.org/wiki/Michel_Foucault"
        "http://en.wikipedia.org/wiki/Michel_Foucault#Growing_career") =>
      "http://en.wikipedia.org/wiki/Michel_Foucault")

(def rate-get
  (->
    clj-http.client/get
    (->?-fn "crawl url " :args-only true) ; print so we can see some progress
    (rate-gate.core/rate-limit 1 100)))

(def ignore-words
  #{"the" "of" "to" "a" "in" "is" "it"})

(def roots ["http://en.wikipedia.org/wiki/Perl"])

(defn clean-up-link
  "Resolve hrefs from links and extract link test"
  [page-url link]
  [(.ownText link)
   (clean-up-url page-url (.attr link "href"))])

(defn links-from-soup
  "Extract and clean up links from a soup"
  [page-url soup]
  (map
    (partial clean-up-link page-url)
    (.select soup "a[href]")))

(def test-div-with-links
  "<div>
  <a href=\"/search\">First</a>
  <div>Distraction!</div>
  <a href=\"http://en.wikipedia.org/wiki/Michel_Foucault#Growing_career\">Second</a>
  <a href=\"www.google.com\">Third</a>
  </div>")

(fact
  (links-from-soup
    "www.google.com"
    (Jsoup/parse
      test-div-with-links)) =>
  [["First" "http://www.google.com/search"]
   ["Second" "http://en.wikipedia.org/wiki/Michel_Foucault"]
   ["Third" "http://www.google.com/"]])

(defn incremental-crawl
  [[{:keys [visited-urls url-queue] :as state} _] url]
  (try 
    (let [body (-> url
                   rate-get
                   :body
                   Jsoup/parse)
          links (links-from-soup body)
          new-visited-urls (conj visited-urls url) ]
      (if +debug-prints+ (println "incremental-crawl is " url))
      [{:visited-urls new-visited-urls
        :url-queue (concat
                     url-queue
                     (set 
                       (for [link links
                             :when (not new-visited-urls link)]
                         [link 1])))}
       {:url map :links links :body body}])
    (catch Exception e [state nil])))

(defn crawl
  "Crawler that produces a lazy sequence of accessed pages"
  [urls & {:keys [max-depth visited-urls]
           :or {max-depth 2 visited-urls #{}}}]
  (if +debug-prints+ (println "crawl is " urls))
  (let [red (reductions incremental-crawl
                        [{:visited-urls visited-urls
                         :url-queue []} nil] urls)
        skinny (remove nil? (map #(nth % 1) red))]
    (if (<= max-depth 1)
      skinny
      (lazy-cat
        skinny
        (let [{:keys [new-visited-urls url-queue]} (first (last red))]
          (crawl url-queue
                 :visited-urls new-visited-urls
                 :max-depth (dec max-depth)))))))

(pp/pprint
  (map :url (crawl ["http://en.wikipedia.org/wiki/Perl"])))

;   (for 
;     [el (take 20 (-> page
;                      rate-get
;                      :body
;                      Jsoup/parse
;                      (.select "a[href]")))]
;     (clean-up-link page (.attr el "href"))))


