# calyx-css

Garden + JIT Tailwind (girouette) = 🎉

# Usage

```clojure
(ns calyx.ui.component
  (:require
    [calyx.css.helper :refer [tw cx]]
    [helix.core :refer [defnc]]
    [helix.dom :as d]))

;; vars with meta :garden = true will be compiled to css file
(def ^:garden css
  [:.dropbox (tw "relative" "mb-2" "w-7.5" "h-4.5")
   [:.popup (merge (tw "absolute" "top-0" "left-0")
                   {:height "100px"})]])

(defnc dropbox
  [{:keys [active?]}]
  ;; css within helix/fulcro component definition
  {:css [:.dropbox
         [:.input (tw ["flex" "items-center"])]]} 
  (d/div {:class "dropbox"}
    ;; class names matching tailwind class name rule will be included in compiled css file
    (d/div {:class "input outline-none p-4"})
    (d/div {:class (cx "popup" (when active? "active"))})))
```
