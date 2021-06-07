(ns calyx.css.girouette
  (:require
    [girouette.tw.accessibility :as accessibility]
    [girouette.tw.animation :as animation]
    [girouette.tw.background :as background]
    [girouette.tw.border :as border]
    [girouette.tw.box-alignment :as box-alignment]
    [girouette.tw.color :as color]
    [girouette.tw.common :as common]
    [girouette.tw.core :refer [make-api]]
    [girouette.tw.effect :as effect]
    [girouette.tw.flexbox :as flexbox]
    [girouette.tw.grid :as grid]
    [girouette.tw.interactivity :as interactivity]
    [girouette.tw.layout :as layout]
    [girouette.tw.sizing :as sizing]
    [girouette.tw.spacing :as spacing]
    [girouette.tw.svg :as svg]
    [girouette.tw.table :as table]
    [girouette.tw.transform :as transform]
    [girouette.tw.typography :as typography]))


(def all-components
  [common/components
   layout/components
   flexbox/components
   grid/components
   box-alignment/components
   spacing/components
   sizing/components
   typography/components
   background/components
   border/components
   effect/components
   table/components
   animation/components
   transform/components
   interactivity/components
   svg/components
   accessibility/components])


(def class-name->garden
  (let [f (:class-name->garden
            (make-api all-components
                      {:color-map       color/default-color-map
                       :font-family-map typography/default-font-family-map}))]
    (memoize f)))
