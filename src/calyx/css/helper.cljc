(ns calyx.css.helper
  #?(:cljs
     (:require-macros [calyx.css.helper]))
  #?(:clj
     (:require
       [calyx.css.girouette :as gi]
       [calyx.css.util :as util]
       [clojure.string :as str]
       [garden.selectors :as s]
       [garden.stylesheet :as gss]
       [taoensso.encore :as encore])
     :cljs
     (:require
       [clojure.string :as str]))
  #?(:clj
     (:import [java.util Arrays])))


#?(:clj (def ^:dynamic *class-name->garden* gi/class-name->garden))

#?(:clj
   (defn tw->garden
     "Generate garden style map based on tailwind classnames.
     NOTE: Do not support tailwind variants, prefer garden selectors"
     [classes]
     (let [classes (if (string? classes)
                     (str/split classes #" ")
                     classes)]
       (->> classes
            (map *class-name->garden*)
            (reduce
              (fn [styles x]
                (if (vector? x)
                  (let [s (second x)]
                    (cond-> styles
                      (map? s) (merge s)))
                  styles))
              {})))))

#?(:clj
   (defn- tw*
     [& classes]
     (transduce (map tw->garden) merge classes)))

#?(:clj
   (defn tw
     [& classes]
     (if (= (count classes) 1)
       (tw->garden (first classes))
       (apply tw* classes)))

   :cljs
   (defn tw [& _]))

#?(:clj
   (defn dark
     [& rules]
     (apply gss/at-media {:prefers-color-scheme "dark"} rules))

   :cljs
   (defn dark [& _]))

#?(:clj
   (defn &>
     "Child combinator."
     ([a]
      (s/selector (str " > " (s/css-selector a))))
     ([a b]
      (s/selector (str (s/css-selector a) " > " (s/css-selector b))))
     ([a b & more]
      (->> (cons (&> a b) more)
           (clojure.core/map s/css-selector)
           (str/join " > ")
           (s/selector))))

   :cljs
   (defn &> [& _]))

(defn cx*
  [& classes]
  (letfn [(f [class]
            (cond
              (map? class) (->> class (filter (fn [[_ v]] v)) keys cx*)
              (sequential? class) (->> class (map cx*) (str/join " "))
              (keyword? class) (name class)
              (nil? class) ""
              :else class))]
    (str/join " " (map f classes))))

#?(:clj
   (defmacro cx
     [& classes]
     (let [raw       (filter string? classes)
           bool-sym? #(and (symbol? %) (str/ends-with? (name %) "?"))
           bools     (->> classes
                          (filter bool-sym?)
                          (map #(vector (str/replace (name %) #"\?+$", "") %))
                          (into {}))
           dyn       (->> classes
                          (filter #(and (not (string? %)) (not (bool-sym? %))))
                          (into [bools])
                          (filter #(or (symbol? %) (some? (seq %))))
                          (map (fn [x]
                                 (if (and (map? x) (= (count x) 1))
                                   (let [[c v] (first x)]
                                     `(when ~v ~c))
                                   x))))
           raw       (str/join " " raw)]
       `(cx* ~raw ~@dyn))))

#?(:clj
   (defn css-scope
     ([] (css-scope *ns*))
     ([s]
      (let [^bytes bytes (util/md5 (str s))]
        (str
          ;; make sure starts with letter
          (util/base52 (Arrays/copyOfRange bytes 0 1))
          (util/base62 (Arrays/copyOfRange bytes 1 6)))))))

#?(:clj
   (defmacro scx
     [& classes]
     (let [scope (css-scope *ns*)]
       `(cx ~scope ~@classes))))

#?(:clj
   (defmacro defstyle
     [sym & body]
     (let [sym (with-meta sym {:garden true})]
       `(encore/if-clj
          (def ~sym ~@body)
          (def ~sym nil)))))
