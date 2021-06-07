(ns calyx.css.shadow
  (:require
    [calyx.css.core :refer [process]]))


(defn hook
  {:shadow.build/stage :configure}
  [{:shadow.build/keys [build-id mode] :as build-state} & [options]]
  (process (cond-> options
             true (assoc :build-id build-id)
             (= mode :release) (assoc :watch? false)))
  build-state)
