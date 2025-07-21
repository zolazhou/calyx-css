(ns calyx.css.server
  (:require
   [org.httpkit.server :as http]
   [ring.middleware.defaults]
   [taoensso.sente :as sente]
   [taoensso.sente.server-adapters.http-kit :refer [get-sch-adapter]]))

(def ^:const default-server-port 5680)

(defonce server (atom nil))

(defn started? []
  (some? @server))

(let [{:keys [ch-recv send-fn connected-uids
              ajax-post-fn ajax-get-or-ws-handshake-fn]}
      (sente/make-channel-socket-server!
       (get-sch-adapter)
       {:csrf-token-fn nil
        :user-id-fn    (fn [ring-req] (get-in ring-req [:params :client-id]))})]
  (def ring-ajax-post ajax-post-fn)
  (def ring-ajax-get-or-ws-handshake ajax-get-or-ws-handshake-fn)
  (def ch-chsk ch-recv)                                     ; ChannelSocket's receive channel
  (def chsk-send! send-fn)                                  ; ChannelSocket's send API fn
  (def connected-uids connected-uids)                       ; Watchable, read-only atom
  )

(defmulti -event-msg-handler
  "Multimethod to handle Sente `event-msg`s"
  :id                                                       ; Dispatch on event-id
  )

(defmethod -event-msg-handler :chsk/ws-ping [_] nil)
(defmethod -event-msg-handler :chsk/ws-pong [_] nil)
(defmethod -event-msg-handler :chsk/uidport-open [_] nil)
(defmethod -event-msg-handler :chsk/uidport-close [_] nil)

(defmethod -event-msg-handler :default
  [{:keys [client-id id]}]
  (println "Unknown msg from client" client-id id))

(defn event-msg-handler
  "Wraps `-event-msg-handler` with logging, error catching, etc."
  [ev-msg]
  (-event-msg-handler ev-msg))

(defonce router_ (atom nil))
(defn stop-router! [] (when-let [stop-fn @router_] (stop-fn)))
(defn start-router! []
  (stop-router!)
  (reset! router_ (sente/start-server-chsk-router!
                   ch-chsk event-msg-handler)))

(comment
  @connected-uids)

(defn broadcast! [msg]
  (let [uids (:any @connected-uids)]
    (println "Broadcasting server>user: %s uids" (count uids))
    (doseq [uid uids]
      (chsk-send! uid msg))))

(defn- not-found
  []
  {:status  404
   :headers {"Content-Type" "text/html"}
   :body    "Not Found"})

(defn handler
  [{:keys [uri request-method] :as req}]
  (if (= uri "/chsk")
    (case request-method
      :get (ring-ajax-get-or-ws-handshake req)
      :post (ring-ajax-post req)
      (not-found))
    (not-found)))

(def app
  (ring.middleware.defaults/wrap-defaults
   handler
   (assoc ring.middleware.defaults/site-defaults
          :security nil)))

(defn stop-server! []
  (when-not (nil? @server)
    ;; graceful shutdown: wait 100ms for existing requests to be finished
    ;; :timeout is optional, when no timeout, stop immediately
    (@server :timeout 100)
    (reset! server nil)))

(defn start-server! [port]
  (stop-server!)
  (reset! server (http/run-server #'app {:port (or port default-server-port)})))

(defn start! [port]
  (start-router!)
  (start-server! port))

(defn stop! []
  (stop-router!)
  (stop-server!))

(comment

  (stop!)
  (start! nil)

  ;;(start-server)

  (defn log [x]
    (println x))

  (add-tap log)
  (remove-tap log))
