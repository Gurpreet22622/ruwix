(ns subs
  (:require [re-frame.core :as rf]))

(rf/reg-sub
 ::depth-level
 (fn [db _]
   (:depth-level db)))

(rf/reg-sub
 ::current-conf
 (fn [db _]
   (:current-conf db)))

(rf/reg-sub
 ::final-moves
 (fn [db _]
   (:final-moves db)))

(rf/reg-sub
 ::inp-moves
 (fn [db _]
   (:inp-moves db)))