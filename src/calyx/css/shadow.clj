(ns calyx.css.shadow
  (:require
    [calyx.css.core :refer [push-all process]]
    [calyx.css.server :as server :refer [-event-msg-handler]]))

(defonce started (atom false))
(defonce ^:private subscriptions (atom {}))

(defmethod -event-msg-handler :calyx.css/connect
  [{:keys [uid ?data]}]
  (let [{:keys [build-id]} ?data
        build-id (if (string? build-id)
                   (keyword build-id)
                   build-id)]
    (swap! subscriptions update build-id (fnil conj #{}) uid)
    (push-all build-id)))

(add-watch server/connected-uids :connected-uids
           (fn [_ _ old new]
             (when (not= old new)
               (let [alive? (partial contains? (:any new))]
                 (swap! subscriptions (fn [sub]
                                        (reduce
                                          (fn [acc [k v]]
                                            (assoc acc k (->> v
                                                              (filter alive?)
                                                              (into #{}))))
                                          {}
                                          sub)))))))

(defn hook
  {:shadow.build/stage :configure}
  [{:shadow.build/keys [build-id mode] :as build-state} & [options]]
  (let [pusher   (fn [msg]
                   (doseq [uid (get @subscriptions build-id)]
                     (server/chsk-send! uid [:calyx.css/build msg])))
        push?    (:push? options)
        release? (= mode :release)]
    (when (and push? (not release?))
      (locking started
        (when (and (not @started) push? (not (server/started?)))
          (reset! started true)
          (server/start! (get options :server-port)))))
    (process (cond-> options
               true (assoc :build-id build-id)
               push? (assoc :push-fn pusher)
               release? (assoc :watch? false :push-fn nil)))
    build-state))
