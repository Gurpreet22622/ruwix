(ns events 
  (:require [re-frame.core :as rf]))


(rf/reg-event-db
 ::initialize
 (fn [_ _]
   {:depth-level 0
    :current-conf nil
    :final-moves []
    :inp-moves []}))

(rf/reg-event-db
 ::current-dpeth-level
 (fn [db [_ new-val]]
   (assoc db :depth-level new-val)))

(rf/reg-event-db
 ::current-conf-cng
 (fn [db [_ new-conf]]
   (assoc db :current-conf new-conf)))

(rf/reg-event-db
 ::final-moves-cng
 (fn [db [_ new-coll]]
   (assoc db :final-moves new-coll)))

(rf/reg-event-db
 ::inp-moves-cng
 (fn [db [_ new-coll]]
   (assoc db :inp-moves new-coll)))

(rf/reg-event-db
 ::inp-moves-append
 (fn [db [_ new-val]]
   (update db :inp-moves conj new-val)))