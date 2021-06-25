(ns ^{:garden-order 0 :garden-file "base.css"}
  calyx.css.test.base)


(def ^:garden reset
  [[:html {:-webkit-app-region :drag
           :min-height         "100%"}]])

(def ^:garden styles
  [[:body {:color "#444"
           :width "100%"}]
   [:.gap {:flex "1 1 0"}]
   [:.ghost.mask {:background-color "#fff"}]])
