(ns clojure-lsp.snapshot
  (:require
   [clojure-lsp.shared :as shared]
   [clojure.edn :as edn]
   [clojure.java.io :as io])
  (:import
   (java.security MessageDigest)
   [java.util UUID]))

(defn string-to-uuid [s]
  (let [md5 (MessageDigest/getInstance "MD5")
        hash (.digest md5 (.getBytes s "UTF-8"))
        msb (->> (take 8 hash)
                 (map #(bit-and % 0xFF))
                 (reduce (fn [acc b] (bit-or (bit-shift-left acc 8) b)) 0))
        lsb (->> (drop 8 hash)
                 (map #(bit-and % 0xFF))
                 (reduce (fn [acc b] (bit-or (bit-shift-left acc 8) b)) 0))]
    (UUID. msb lsb)))

(defn ^:private string-to-md5
  [input-str]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes input-str "UTF-8"))]
    (format "%032x" (BigInteger. 1 raw))))

(defn ^:private string-to-hash
  [input-str]
  #_(string-to-uuid input-str)
  #_(keyword (string-to-md5 %))
  input-str)

(defn ^:private read-file
  ([] (read-file (io/file ".lsp" "snapshot.edn")))
  ([path]
   (shared/logging-time
     "[SNAPSHOT] Read took %s"
     (let [file (io/file path)]
       (if (.exists file)
         (let [result-hash (with-open [reader (io/reader file)]
                             (edn/read (java.io.PushbackReader. reader)))]
           #_(spit "snapshot-uuid.edn" (pr-str result-hash))
           result-hash)
         {})))))

(def read-file-memo (memoize read-file))

(defonce cache (atom {}))

(defn warm-cache!
  []
  (swap! cache (fn [_] (read-file))))

(defn discard
  [uri]
  (get @cache uri))
