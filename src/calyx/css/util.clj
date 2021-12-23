(ns calyx.css.util
  (:require
    [clojure.string :as str]
    [garden.selectors :as sel])
  (:import
    [java.security MessageDigest]
    [garden.selectors CSSSelector]))

(defn- encode
  [chars total b]
  (if (empty? b)
    ""
    (let [s          (StringBuilder.)
          zero-count (count (take-while zero? b))]
      ;; BigInteger's signum must be 1 so that b is processed unsigned
      (loop [i (BigInteger. 1 b)]
        (when-not (zero? i)
          (.append s (nth chars (mod i total)))
          (recur (quot i total))))
      (str (str/join (repeat zero-count "0")) (.reverse s)))))

(def base62 (partial encode "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" 62))
(def base52 (partial encode "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" 52))

(defn md5 [^String s]
  (let [^bytes data      (.getBytes s)
        ^MessageDigest h (doto (MessageDigest/getInstance "MD5")
                           (.update data))]
    (.digest h)))


(defn- is-selector? [v]
  (or (keyword? v)
      (string? v)
      (symbol? v)
      (instance? CSSSelector v)))

(defn- prepend-selector
  [scope selector]
  (if (str/starts-with? selector ".")
    (str scope selector)
    (str selector scope)))

(defn- prepend-scope [scope selector]
  (cond
    (keyword? selector) (keyword (prepend-selector scope (name selector)))
    (string? selector) (prepend-selector scope selector)
    (symbol? selector) (symbol (prepend-selector scope (name selector)))
    (instance? CSSSelector selector) (sel/selector (prepend-selector scope (sel/css-selector selector)))))

(defn scoped-rule
  [scope rule]
  (if (nil? scope)
    rule
    (let [[selectors other] (split-with is-selector? rule)]
      (if (seq selectors)
        (into (mapv #(prepend-scope scope %) selectors) other)
        (mapv
          #(scoped-rule scope %)
          other)))))
