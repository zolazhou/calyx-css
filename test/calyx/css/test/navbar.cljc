(ns calyx.css.test.navbar)


(def ^:garden navbar
  [[:.navbar {:position :relative}
    [:&:before {:content            :''
                :position           :absolute
                :-webkit-app-region :drag}]
    [:.gap {:-webkit-app-region :drag}]]])
