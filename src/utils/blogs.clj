(ns utils.blogs
  "Tools to parse a modest number of RSS and Atom feeds to build histograms of 
  words for use in clustering"
  (:use midje.sweet)
  (:import
    [java.util.regex Matcher]
    [org.jsoup Jsoup])
  (:require
    [clojure.java.io :as io]
    [clojure.string]
    [clojure.data.json :as json]
    [net.cgrand.enlive-html :as html]
    [feedparser-clj.core]))

(def small-feed-list
  ["http://blog.guykawasaki.com/index.rdf"
   "http://feeds.searchenginewatch.com/sewblog"
   "http://www.boingboing.net/index.rdf"
   "http://feeds.feedburner.com/37signals/beMH"
   "http://www.engadget.com/rss.xml"
   "http://feeds.dailykos.com/dailykos/index.xml"
   "http://thinkprogress.org/feed/"
   "http://www.lifehack.org/feed/"
   "http://www.456bereastreet.com/feed.xml"])

(def feed-list ; from http://kiwitobes.com/clusters/feedlist.txt
  ["http://feeds.feedburner.com/37signals/beMH"
   "http://feeds.feedburner.com/blogspot/bRuz"
   "http://battellemedia.com/index.xml"
   "http://blog.guykawasaki.com/index.rdf"
   "http://blog.outer-court.com/rss.xml"
   "http://feeds.searchenginewatch.com/sewblog"
   "http://blog.topix.net/index.rdf"
   "http://blogs.abcnews.com/theblotter/index.rdf"
   "http://feeds.feedburner.com/ConsumingExperienceFull"
   "http://flagrantdisregard.com/index.php/feed/"
   "http://featured.gigaom.com/feed/"
   "http://gizmodo.com/index.xml"
   "http://gofugyourself.typepad.com/go_fug_yourself/index.rdf"
   "http://googleblog.blogspot.com/rss.xml"
   "http://feeds.feedburner.com/GoogleOperatingSystem"
   "http://headrush.typepad.com/creating_passionate_users/index.rdf"
   "http://feeds.feedburner.com/hotair/main"
   "http://feeds.feedburner.com/instapundit/main"
   "http://jeremy.zawodny.com/blog/rss2.xml"
   "http://joi.ito.com/index.rdf"
   "http://journals.aol.com/thecoolerblog/AOLNewsCooler/rss.xml"
   "http://feeds.feedburner.com/Mashable"
   "http://michellemalkin.com/index.rdf"
   "http://moblogsmoproblems.blogspot.com/rss.xml"
   "http://newsbusters.org/node/feed"
   "http://beta.blogger.com/feeds/27154654/posts/full?alt=rss"
   "http://feeds.feedburner.com/paulstamatiou"
   "http://powerlineblog.com/index.rdf"
   "http://feeds.feedburner.com/Publishing20"
   "http://radar.oreilly.com/index.rdf"
   "http://scienceblogs.com/pharyngula/index.xml"
   "http://scobleizer.wordpress.com/feed/"
   "http://sethgodin.typepad.com/seths_blog/index.rdf"
   "http://rss.slashdot.org/Slashdot/slashdot"
   "http://thinkprogress.org/feed/"
   "http://feeds.feedburner.com/andrewsullivan/rApM"
   "http://wilwheaton.typepad.com/wwdnbackup/index.rdf"
   "http://www.43folders.com/feed/"
   "http://www.456bereastreet.com/feed.xml"
   "http://www.autoblog.com/rss.xml"
   "http://www.bloggersblog.com/rss.xml"
   "http://www.bloglines.com/rss/about/news"
   "http://www.blogmaverick.com/rss.xml"
   "http://www.boingboing.net/index.rdf"
   "http://www.buzzmachine.com/index.xml"
   "http://www.captainsquartersblog.com/mt/index.rdf"
   "http://www.coolhunting.com/index.rdf"
   "http://feeds.copyblogger.com/Copyblogger"
   "http://feeds.feedburner.com/crooksandliars/YaCP"
   "http://feeds.dailykos.com/dailykos/index.xml"
   "http://www.deadspin.com/index.xml"
   "http://www.downloadsquad.com/rss.xml"
   "http://www.engadget.com/rss.xml"
   "http://www.gapingvoid.com/index.rdf"
   "http://www.gawker.com/index.xml"
   "http://www.gothamist.com/index.rdf"
   "http://www.huffingtonpost.com/raw_feed_index.rdf"
   "http://www.hyperorg.com/blogger/index.rdf"
   "http://www.joelonsoftware.com/rss.xml"
   "http://www.joystiq.com/rss.xml"
   "http://www.kotaku.com/index.xml"
   "http://feeds.kottke.org/main"
   "http://www.lifehack.org/feed/"
   "http://www.lifehacker.com/index.xml"
   "http://littlegreenfootballs.com/weblog/lgf-rss.php"
   "http://www.makezine.com/blog/index.xml"
   "http://www.mattcutts.com/blog/feed/"
   "http://xml.metafilter.com/rss.xml"
   "http://www.mezzoblue.com/rss/index.xml"
   "http://www.micropersuasion.com/index.rdf"
   "http://www.neilgaiman.com/journal/feed/rss.xml"
   "http://www.oilman.ca/feed/"
   "http://www.perezhilton.com/index.xml"
   "http://www.plasticbag.org/index.rdf"
   "http://www.powazek.com/rss.xml"
   "http://www.problogger.net/feed/"
   "http://feeds.feedburner.com/QuickOnlineTips"
   "http://www.readwriteweb.com/rss.xml"
   "http://www.schneier.com/blog/index.rdf"
   "http://scienceblogs.com/sample/combined.xml"
   "http://www.seroundtable.com/index.rdf"
   "http://www.shoemoney.com/feed/"
   "http://www.sifry.com/alerts/index.rdf"
   "http://www.simplebits.com/xml/rss.xml"
   "http://feeds.feedburner.com/Spikedhumor"
   "http://www.stevepavlina.com/blog/feed"
   "http://www.talkingpointsmemo.com/index.xml"
   "http://www.tbray.org/ongoing/ongoing.rss"
   "http://feeds.feedburner.com/TechCrunch"
   "http://www.techdirt.com/techdirt_rss.xml"
   "http://www.techeblog.com/index.php/feed/"
   "http://www.thesuperficial.com/index.xml"
   "http://www.tmz.com/rss.xml"
   "http://www.treehugger.com/index.rdf"
   ; "http://www.tuaw.com/rss.xml"
   "http://www.valleywag.com/index.xml"
   "http://www.we-make-money-not-art.com/index.rdf"
   "http://www.wired.com/rss/index.xml"
   "http://www.wonkette.com/index.xml"])

(def short-feed-list
   ["http://www.boingboing.net/index.rdf"
   "http://www.buzzmachine.com/index.xml"
   "http://www.captainsquartersblog.com/mt/index.rdf"
   "http://www.coolhunting.com/index.rdf"
   "http://www.deadspin.com/index.xml"
    "http://www.gothamist.com/index.rdf"
     "http://www.gawker.com/index.xml"])

(defn html-text-to-words
  "Convert a chunk of html text into just words"
  [snippet]
  (->> snippet
       Jsoup/parse
       (.text)
       (.split #"[^a-z^A-Z]+")
       (remove empty?)))

(facts "html-text-to-words can clean up html blog entries for word-tracking"
       (fact "it splits words to tokens and drops symbols and whitespace"
             (first (html-text-to-words "Hello World")) => "Hello"
             (first (html-text-to-words "__Hello__World")) => "Hello")
       (fact "it removes html tags and captures text inside them"
             (html-text-to-words
               "That has <a href=\"lantern\"></a>such creatures<b> </b>in it") =>
             ["That" "has" "such" "creatures" "in" "it"]
             (html-text-to-words
               "<div>Silly man </div><div>its turtles <div>all the way down</div></div>") =>
             ["Silly" "man" "its" "turtles" "all" "the" "way" "down"]))

(defn histogram-seq
  [xs]
  (reduce (fn [acc nxt] (merge-with + {nxt 1} acc)) {} xs))

(fact "we can histogram any values"
      (histogram-seq [:a :a :b]) => {:a 2 :b 1}
      (histogram-seq [1 1 2 2 3 4]) => {1 2 2 2 3 1 4 1})

; TODO api to pass in body not url? Or test feed
(defn get-word-counts
  "Get a map of word counts in recent entries from a feed"
  [url]
  ; (println url)
  (try 
    (->> url
         feedparser-clj.core/parse-feed
         :entries
         (map 
           (comp
             html-text-to-words
             (fn [entry]
               ((some (fn [k] (entry k)) [:summary :description])
                :value))))
         (apply concat)
         (map clojure.string/lower-case)
         histogram-seq)
    (catch Exception e {})))

(fact :slow
      "get-word-counts maps words in a url to appearance count"
      (let [[k v] (first (get-word-counts "http://www.gothamist.com/index.rdf"))]
        (and (string? k) (integer? v))) => true)

(defn get-word-count-appearance-map-accum
  "get word counts and update a map of blogs that the URL appears in"
  [[wcs aps] url]
  (let [wc (get-word-counts url)]
    [(assoc wcs url wc)
     (merge-with concat aps (zipmap (keys wc) (repeat [url])))]))

(fact :slow
      "get-word-counts-appearance-map updates wcs and aps for the new url"
      (let [[wcs aps] (get-word-count-appearance-map-accum
                        [{} {}]
                        "http://www.gothamist.com/index.rdf")
            [k v] (first (first (vals wcs)))]
        (and (string? k) (integer? v))) => true)

(defn get-word-counts-appearance-map
  "Take a sequence of URLs and return a map of urls => words => counts and
  a map of words => blogs they appear in"
  [urls]
  (reduce get-word-count-appearance-map-accum [{} {}] urls))

 (defn remove-insignificant-words
   "Remove words that appear too frequently or infrequently to be meaningful"
   [aps urls]
   (for [[w ap] aps
         :when (let [frac (/ (count ap) (count urls))]
                 (and (< 0.1 frac) (> 0.5 frac)))]
     w))

(fact "we can remove very common (and rare )words with remove-frequent-and-infrequent-words"
      (set (remove-insignificant-words
             {"the" ["a.com" "b.com" "c.com" "d.com" "e.com"]
              "turtle" ["a.com" "b.com"]
              "starfish" ["c.com" "d.com"]}
             ["a.com" "b.com" "c.com" "d.com" "e.com"])) => #{"turtle" "starfish"})

(defn get-significant-word-vectors
  "Get word-counts for each feed URL, find the significant words, and
  transform each word-count map into a full-vector (to save on key-space)"
  [urls]
  (let [[wcs aps] (get-word-counts-appearance-map urls)
        words (set (remove-insignificant-words aps urls))]
    (into {}
      (for [[url wc] wcs]
        [url (vec (for [word words] (get-in wc [word] 0)))]))))

(defn generate-and-write-blog-data
  "Generate and write blog-associated data"
  [& {:keys [blog-data-path feeds]
      :or {blog-data-path "res/blogdata.json" feeds feed-list}}]
  (with-open [out-wr (io/writer blog-data-path)]
    (binding [*out* out-wr]
      (json/pprint (get-significant-word-vectors feeds)))))

(defn load-blog-data
  "Load previously stored blog data"
  [& {:keys [blog-data-path]
      :or {blog-data-path "res/blogdata.json"}}]
  (with-open [r (java.io.FileReader. blog-data-path)] 
      (json/read r)))

; (generate-and-write-blog-data )
; (generate-and-write-blog-data
;   :blog-data-path "res/small_blogdata.json"
;   :feeds small-feed-list)

