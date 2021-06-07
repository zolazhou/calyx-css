(ns calyx.css.core
  (:require
    [calyx.css.helper :refer [*class-name->garden*]]
    [clojure.java.io :as io]
    [clojure.stacktrace :as stacktrace]
    [clojure.string :as str]
    [clojure.tools.deps.alpha :as t]
    [clojure.tools.deps.alpha.util.dir :refer [*the-dir*]]
    [clojure.tools.reader :as reader]
    [clojure.walk :refer [postwalk]]
    [shadow.cljs.devtools.server.fs-watch :as fs])
  (:import
    [java.io File PushbackReader]
    [java.util.concurrent ArrayBlockingQueue BlockingQueue TimeUnit]
    [clojure.lang RT]))


(def ^:private data-readers
  (merge reader/default-data-readers
         {'js identity}))

(defn- get-cljs-part
  [conditional]
  (:cljs (reduce
           (fn [acc [k v]]
             (assoc acc k v))
           {}
           (partition-all 2 (:form conditional)))))

(defn- read-form
  [stream {:keys [read-cond?]}]
  (let [opts (cond-> {:eof :eof}
               read-cond? (assoc :read-cond :preserve))
        form (reader/read opts stream)]
    (if read-cond?
      (postwalk
        (fn [x]
          (if (reader-conditional? x)
            (get-cljs-part x)
            x))
        form)
      form)))

(defonce ^:private state (atom {}))

(defonce
  ^{:private true :tag BlockingQueue}
  queue (ArrayBlockingQueue. 512 false))

(defn- find-source-paths
  []
  (let [{:keys [root-edn user-edn project-edn]} (t/find-edn-maps)
        deps (t/merge-edns [root-edn user-edn project-edn])]
    (:paths deps)))

(defn- relative-path
  [^File file]
  (let [home-path (.toPath *the-dir*)]
    (.toString (.relativize home-path (.toPath (.getCanonicalFile file))))))

(defn- input-file?
  [extensions ^File file]
  (let [path (.getPath file)]
    (some #(str/ends-with? path (str "." %)) extensions)))

(defn- find-ns-file
  [ns-sym]
  (some-> ns-sym find-ns ns-interns vals first meta :file io/file relative-path))

(defn- find-ns-aliases
  [forms]
  (reduce
    (fn [aliases form]
      (if (= (first form) :require)
        (transduce
          (comp (filter (fn [[s x & _]] (and (symbol? s) (= x :as))))
                (map (fn [[s _ a & _]] [a (create-ns s)])))
          conj
          aliases
          (rest form))
        aliases))
    {}
    forms))

(defn- find-ns-refers
  [forms]
  (reduce
    (fn [refers form]
      (if (= (first form) :require)
        (transduce
          (comp (filter (fn [[ns & r]] (and (symbol? ns) (some (partial = :refer) r))))
                (map (fn [[ns & r]]
                       (mapv (fn [s] [s (create-ns ns)]) (last r)))))
          into
          refers
          (rest form))
        refers))
    {}
    forms))

(defn- find-ns-refer-clojure
  [forms]
  (reduce
    (fn [refers form]
      (if (= (first form) :refer-clojure)
        (conj refers form)
        refers))
    []
    forms))

(defn- read-ns
  [stream opts]
  (let [[x n & r] (read-form stream opts)]
    (when (= x 'ns)
      {:ns-sym           n
       :ns-aliases       (find-ns-aliases r)
       :ns-refers        (find-ns-refers r)
       :ns-refer-clojure (find-ns-refer-clojure r)})))

(def set-conj (fnil conj #{}))

(defn- deep-parse-deps
  [{:keys [ns-aliases ns-refers ns-vars] :as ns} forms deps vars]
  (postwalk
    (fn [x]
      (when (and (symbol? x) (not (contains? @vars x)))
        (if-let [n (some-> (namespace x) symbol)]
          (when-let [d (get ns-aliases n)]
            (swap! deps assoc-in [(ns-name d) :as] n))
          (do
            (when-let [d (get ns-refers x)]
              (swap! deps update-in [(ns-name d) :refer] set-conj x))
            (when-let [d (get ns-vars x)]
              (swap! vars conj x)
              (deep-parse-deps ns d deps vars))))))
    forms))

(defn- parse-deps
  [ns forms]
  (let [deps (atom {})
        vars (atom #{})]
    (deep-parse-deps ns forms deps vars)
    [@deps @vars]))

(defn- find-tw-classes
  [form]
  (let [classes (transient [])]
    (postwalk
      (fn [x]
        (cond
          (string? x)
          (doseq [n (->> (str/split x #" ")
                         (remove str/blank?))]
            (conj! classes n))
          (keyword? x)
          (doseq [n (->> (name x)
                         (re-seq #"\.[^\.#]+")
                         (map (fn [s] (subs s 1))))]
            (conj! classes n))))
      form)
    (persistent! classes)))

(defn- find-classes
  [forms]
  (let [classes (transient #{})]
    (postwalk
      (fn [x]
        (when (and (vector? x)
                   (= (count x) 2)
                   (#{:class :className} (first x)))
          (doseq [n (find-tw-classes (second x))]
            (conj! classes n)))
        x)
      forms)
    (persistent! classes)))

(defn- parse-component-css
  [[_defnc name params & body]]
  (let [body (if (string? params)
               (rest body)
               body)
        opts (first body)]
    (when (map? opts)
      (when-let [css (:css opts)]
        [name css]))))

(defn- parse-var
  [form]
  (when (and (list? form) (> (count form) 1)
             (not (#{'comment 'let} (second form))))
    [(second form)
     (find-classes form)]))

(defn- read-file
  [file]
  (let [stream    (PushbackReader. (io/reader (io/file file)))
        var-pos   (atom 0)
        tailwinds (transient #{})
        vars      (transient {})
        css       (transient {})
        read-opts {:read-cond? (str/ends-with? file ".cljc")}
        ns        (read-ns stream read-opts)]
    (binding [reader/*data-readers* data-readers
              reader/*alias-map*    (:ns-aliases ns)]
      (loop [form (read-form stream read-opts)]
        (when (list? form)
          (when-let [[v tw] (parse-var form)]
            (assoc! vars v (with-meta form {:position (swap! var-pos inc)}))
            (doseq [c tw] (conj! tailwinds c)))
          (condp = (first form)
            'def (let [s (second form)]
                   (when (true? (:garden (meta s)))
                     (assoc! css s (last form))))
            'defnc (when-let [[s c] (parse-component-css form)]
                     (assoc! css s c))
            'defsc (when-let [[s c] (parse-component-css form)]
                     (assoc! css s c))
            nil))
        (when (not= form :eof)
          (recur (read-form stream read-opts)))))
    (assoc ns :ns-vars (persistent! vars)
              :css (persistent! css)
              :tailwinds (persistent! tailwinds))))

(defn- find-css
  [file]
  (let [{:keys [css] :as ns} (read-file file)
        [deps locals] (parse-deps ns (vals css))]
    (assoc ns :locals locals :deps deps)))

(defn- ns-form
  [{:keys [ns-sym ns-refer-clojure ns-vars deps locals css]}]
  (when (seq css)
    (let [locals      (->> locals
                           (map ns-vars)
                           (filter some?)
                           (sort-by (fn [x] (:position (meta x)))))
          definitions (->> css
                           (map (fn [[k v]] (list 'def k v))))
          requires    (->> deps
                           (map (fn [[k {:keys [refer as]}]]
                                  (cond-> [k]
                                    (some? as) (conj :as as)
                                    (seq refer) (conj :refer (vec refer))))))
          ns-def      (cond-> ['ns ns-sym]
                        (seq ns-refer-clojure) (into ns-refer-clojure)
                        (seq requires) (conj (apply list :require requires)))]
      (list* (list* ns-def)
             (concat locals definitions)))))

(defn- depended-by*
  [tree ns deps]
  (reduce
    (fn [acc dep]
      (update-in acc [dep :depended-by] set-conj ns))
    tree
    deps))

(defn- depends-on*
  [tree ns deps]
  (-> tree
      (assoc-in [ns :depends-on] deps)
      (depended-by* ns deps)))

(defn- update-deps!
  [build-id ns]
  (let [name (ns-name ns)
        deps (->> (ns-aliases ns)
                  vals
                  (map ns-name)
                  (into #{}))]
    (swap! state update-in [build-id :deps-tree] #(depends-on* % name deps))))

(defn- generate-css
  [ns sym]
  (when-let [x (ns-resolve ns sym)]
    (let [css (requiring-resolve 'garden.core/css)]
      (str "/* generated from: " ns "/" (name sym) " */\n"
           (css (var-get x))))))

(defn- reload-deps
  [deps]
  ;; TODO skip already reloaded deps
  (let [deps (->> deps
                  (filter (fn [s]
                            (let [n (name s)]
                              (not (or (str/starts-with? n "clojure.")
                                       (str/starts-with? n "garden."))))))
                  vec)]
    (when (seq deps)
      (apply require (conj deps :reload)))))

(defn- reload-ns
  [{:keys [ns-sym deps] :as ns}]
  (when-let [forms (ns-form ns)]
    (locking RT/REQUIRE_LOCK
      (when (seq deps)
        (reload-deps (keys deps)))
      (binding [*ns* (create-ns ns-sym)]
        (doseq [form forms]
          (eval form))))))

(defn- build-css
  [build-id {:keys [ns-sym css] :as ns}]
  (when (seq css)
    (let [{:keys [garden-fn]} (get @state build-id)
          tw->garden (or (some-> garden-fn requiring-resolve)
                         *class-name->garden*)]
      (binding [*class-name->garden* tw->garden]
        (reload-ns ns)
        (let [ns (find-ns ns-sym)]
          (update-deps! build-id ns)
          (->> (keys css)
               (map #(generate-css ns %))
               (str/join "\n")))))))

(defn- gather-css!
  [build-id file]
  (try
    (let [{:keys [ns-sym tailwinds] :as ns} (find-css file)
          css (build-css build-id ns)]
      (when (or (some? css) (seq tailwinds))
        {:ns        ns-sym
         :file      file
         :order     (get (meta ns-sym) :garden-order 100)
         :tailwinds tailwinds
         :css       css}))
    (catch Exception e
      (println (str "[" build-id "]" file ": \uD83D\uDCA5 parse error!"))
      (stacktrace/print-cause-trace e))))

(defn- tw-classes->css
  [garden-fn classes]
  (let [rules (reduce
                (fn [acc class-name]
                  (if-let [rule (garden-fn class-name)]
                    (conj acc rule)
                    acc))
                []
                classes)]
    (when (seq rules)
      (let [css (requiring-resolve 'garden.core/css)]
        (str "/* generated tailwind classes */\n"
             (css rules))))))

(defn- spit-output
  [build-id]
  (let [{:keys [output-file file-data garden-fn verbose?]} (get @state build-id)]
    (let [file-parent (-> (io/file output-file)
                          (.getParent)
                          (io/file))]
      (when-not (.exists file-parent)
        (.mkdirs file-parent)))
    (let [ordered (->> (vals file-data)
                       (sort-by :order))
          tw-css  (when-let [garden-fn (some-> garden-fn requiring-resolve)]
                    (->> ordered
                         (mapcat :tailwinds)
                         (into #{})
                         (tw-classes->css garden-fn)))
          all-css (transduce (comp (map :css) (filter some?))
                             conj
                             ordered)]
      (spit output-file (str/join "\n\n" (conj all-css tw-css)))
      (when verbose?
        (println (str "[" build-id "] \uD83C\uDF89 \u001B[32;1m" output-file " generated! \u001B[0m"))))))

(defn- reload-file!
  [build-id path]
  (println (str "[" build-id "] \u001B[32;1mReloading " path "\u001B[0m"))
  (when-let [result (gather-css! build-id path)]
    (let [depended-by (get-in @state [build-id :deps-tree (:ns result) :depended-by])]
      (swap! state update-in [build-id :file-data] assoc path result)
      (when (seq depended-by)
        (doseq [dep depended-by]
          (when-let [file (find-ns-file dep)]
            (reload-file! build-id file)))))))

(defn- on-file-changed!
  [build-id file change-type]
  (let [path (relative-path file)]
    (if (#{:del} change-type)
      (swap! state update-in [build-id :file-data] dissoc path)
      (reload-file! build-id path))))

(defn- build
  [{:keys [build-id files change-type]}]
  (doseq [file files]
    (on-file-changed! build-id file change-type))
  (spit-output build-id))

(defonce ^:private worker-thread (atom nil))

(defn- start-worker-thread
  []
  (locking worker-thread
    (when (nil? @worker-thread)
      (let [f (fn []
                (when-let [task (.poll queue 1000 TimeUnit/MILLISECONDS)]
                  (time (build task)))
                (recur))
            t (Thread. ^Runnable f)]
        (doto t
          (.setName "css-builder")
          (.start))
        (reset! worker-thread t)))))

(defn process
  [{:keys [build-id source-paths file-extensions garden-fn output-file watch? verbose?]
    :or   {source-paths    (find-source-paths)
           file-extensions ["cljs" "cljc" "clj"]
           watch?          false
           verbose?        true}}]

  (assert (and (seq source-paths)
               (every? string? source-paths))
          "source-paths should be a sequence of strings")
  (assert (and (seq file-extensions)
               (every? string? file-extensions))
          "file-extensions should be a sequence of strings")

  (assert (or (nil? output-file)
              (string? output-file))
          "output-file should be a string")

  (assert (or (nil? garden-fn)
              (qualified-symbol? garden-fn))
          "garden-fn should be a qualified symbol")

  (assert (boolean? watch?)
          "watch? should be a boolean")
  (assert (boolean? verbose?)
          "verbose? should be a boolean")

  ;; stop previous watcher
  (when-let [watch (get-in @state [build-id :watch])]
    (fs/stop watch))

  ;; preload garden namespaces
  (locking RT/REQUIRE_LOCK
    (when-not (find-ns 'garden.core)
      (require
        '[garden.core]
        '[garden.color]
        '[garden.selectors]
        '[garden.units])))

  (let [output-file (or output-file "garden.css")
        input-file? (partial input-file? file-extensions)]

    (swap! state assoc build-id {:output-file output-file
                                 :garden-fn   garden-fn
                                 :verbose?    verbose?
                                 :deps-tree   {}
                                 :file-data   {}
                                 :watch       nil})

    (let [first-task {:build-id    build-id
                      :change-type :new
                      :files       (->> (map io/file source-paths)
                                        (mapcat file-seq)
                                        (filter input-file?)
                                        (into []))}]
      (if watch?
        (do
          (.offer queue first-task)
          (start-worker-thread)
          (when verbose?
            (println (str "[" build-id "] \n\uD83D\uDC40 Watching files in " (str/join ", " source-paths) " ...")))
          (swap! state assoc-in [build-id :watch]
                 (fs/start nil
                           (mapv io/file source-paths)
                           file-extensions
                           (fn [events]
                             (doseq [type [:mod :del :new]]
                               (when-let [updates (->> events
                                                       (filter #(= (:event %) type))
                                                       (seq))]
                                 (.offer queue {:build-id    build-id
                                                :change-type type
                                                :files       (mapv :file updates)})))))))
        ;; release build
        (time (build first-task))))))
