(ns calyx.css.preload
  (:require
    [calyx.css.core :as calyx]
    [goog.dom :as gdom]
    [shadow.cljs.devtools.client.env :as env]
    [taoensso.sente :as sente :refer (cb-success?)]))

(defn devtools-msg [msg & args]
  (when env/log
    (if (seq env/log-style)
      (js/console.log.apply js/console (into-array (into [(str "%ccalyx-css: " msg) env/log-style] args)))
      (js/console.log.apply js/console (into-array (into [(str "calyx-css: " msg)] args))))))

(defn- update-style
  [{:keys [ns order css]}]
  (devtools-msg "load CSS" ns)
  (if-let [^js node (js/document.getElementById ns)]
    (set! (.-innerHTML node) css)
    (let [node (gdom/createDom "style" #js {:id ns})]
      (set! (.-innerHTML node) css)
      (.appendChild js/document.head node))))

(defn- handle-changes
  [changes]
  (doseq [change changes]
    (update-style change)))

(let [{:keys [chsk ch-recv send-fn state]}
      (sente/make-channel-socket-client!
        "/chsk"                                             ; Must match server Ring routing URL
        nil
        {:protocol :http
         :host     "localhost"
         :port     calyx/port
         :type     :ws
         :packer   :edn})]

  (def chsk chsk)
  (def ch-chsk ch-recv)                                     ; ChannelSocket's receive channel
  (def chsk-send! send-fn)                                  ; ChannelSocket's send API fn
  (def chsk-state state)                                    ; Watchable, read-only atom
  )

;;;; Sente event handlers

(defmulti -event-msg-handler
  "Multimethod to handle Sente `event-msg`s"
  :id                                                       ; Dispatch on event-id
  )

(defn event-msg-handler
  "Wraps `-event-msg-handler` with logging, error catching, etc."
  [{:as ev-msg :keys [id ?data event]}]
  (-event-msg-handler ev-msg))

(defmethod -event-msg-handler
  :default                                                  ; Default/fallback case (no other matching handler)
  [{:as ev-msg :keys [event]}]
  (devtools-msg "Unhandled event: %s" event))

(defmethod -event-msg-handler :chsk/state
  [{:as ev-msg :keys [?data]}]
  (let [[old-state-map new-state-map] ?data]
    (if (:first-open? new-state-map)
      (devtools-msg "Channel socket successfully established!: %s" new-state-map)
      (devtools-msg "Channel socket state change: %s" new-state-map))))

(defmethod -event-msg-handler :chsk/recv
  [{:keys [?data]}]
  (let [[type data] ?data]
    (case type
      :calyx.css/build (handle-changes (:changed data))
      (devtools-msg "Push event from server: %s" ?data))))

(defmethod -event-msg-handler :chsk/handshake
  [{:as ev-msg :keys [?data]}]
  (let [[?uid] ?data]
    (devtools-msg "Handshake: %s" ?data)
    (chsk-send! [:calyx.css/connect {:build-id calyx/build-id}])))

;;;; Sente event router (our `event-msg-handler` loop)

(defonce router_ (atom nil))
(defn stop-router! [] (when-let [stop-f @router_] (stop-f)))
(defn start-router! []
  (stop-router!)
  (reset! router_
          (sente/start-client-chsk-router!
            ch-chsk event-msg-handler)))


(defmethod -event-msg-handler :calyx.css/build
  [{:as ev-msg :keys [?data]}]
  (devtools-msg "build: %s" ?data))

(defn init! []
  (start-router!))

(init!)
