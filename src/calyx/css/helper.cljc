(ns calyx.css.helper
  #?(:cljs
     (:require-macros [calyx.css.helper]))
  (:require
    [clojure.string :as str]
    [garden.selectors :as s]
    [garden.stylesheet :as gss]
    [calyx.css.girouette :as gi]))


(def ^:dynamic *class-name->garden* gi/class-name->garden)

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
           {}))))

(defn- tw*
  [& classes]
  (transduce (map tw->garden) merge classes))

(defn tw
  [& classes]
  (if (= (count classes) 1)
    (tw->garden (first classes))
    (apply tw* classes)))

(defn dark
  [& rules]
  (apply gss/at-media {:prefers-color-scheme "dark"} rules))

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
     (let [[raw dyn] (split-with string? classes)
           raw (str/join " " raw)]
       `(cx* ~raw ~@dyn))))
