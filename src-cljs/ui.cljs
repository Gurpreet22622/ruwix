(ns ui
  (:require   [solve :as solve]
              [reagent.core :as r]
              [reagent.dom :as rdom]))





(defn generate-cube
  [{:keys [front back up down left right]}]
  (for [[face pos-x pos-y] [[left 170 0]
                            [front 170 160]
                            [right 170 320]
                            [back 170 480]
                            [up 10 160]
                            [down 330 160]]
        row                [0 1 2]
        col                [0 1 2]
        :let               [x (+ (* 50 row) (+ pos-x 10))
                            y (+ (* 50 col) (+ pos-y 10))
                            color (solve/face->color (get-in face [row col]))]]
    [:rect   {:x            y
              :y            x
              :width        50
              :height       50
              :fill         color
              :stroke       "black"
              :stroke-width 5
              :rx           15
              :ry           15}]))



(defn web-cube
  [current-cube]
  (into [:svg {:width 800 :height 600}]
        (generate-cube current-cube)))



(defn display-cube-moves
  [cube-moves]
  (let [*current (r/atom (dec (count cube-moves)))]
    (fn [cube-moves]
      (let [{:keys [cube moves]} (get cube-moves @*current)
            last? (= (inc @*current) (count cube-moves))
            next-moves (if last? []
                           (get-in cube-moves [(inc @*current) :moves]))]
        [:div
         [web-cube cube]
         [:pre (str "previous: " moves "    next: " next-moves)]
         [:button
          {:on-click (fn []
                       (swap! *current dec)
                       #_(js/console.log @*current))
           :disabled (zero? @*current)} "<<previous"]
         [:button
          {:on-click (fn []
                       (swap! *current inc)
                       #_(js/console.log @*current))
           :disabled last?} "next>>"]
         [:h2 "List of moves"]
         #_(js/console.log cube-moves)
         [:ul
          (doall (map-indexed (fn [idx {:keys [moves]}]
                                ^{:key idx} [:li [:input {:type "button"
                                                          :value (solve/vec-to-str moves)
                                                          :on-click #(reset! *current idx)}]])
                              cube-moves))]]))))




(def *depth-level (r/atom 0))
(def *current-conf (r/atom nil))
(defn counting-component
  [limit]
  [:div
   "The tree is at height "
   @*depth-level ". "
   [:input {:type "button"
            :value "Increase"
            :on-click (if (= @*depth-level limit)
                        #()
                        #(swap! *depth-level inc))}]
   [:input {:type "button"
            :value "Decrease"
            :on-click (if (= @*depth-level 0)
                        #()
                        #(swap! *depth-level dec))}]])


(defn max-depth
  [tree]
  (let [child-depth (if (:child-confs tree)
                      (map max-depth (:child-confs tree))
                      [-1])
        n-depth (inc (apply max child-depth))]
    n-depth))



(declare dfs-traversal-web')
(defn display-conf
  [conf depth show-children?]
  [:ul

   [:span {:on-click (fn []
                       (js/console.log "on-click: " (:end-cube conf))
                       (reset! *current-conf conf))
           :style {:color (if (= conf @*current-conf)
                            "blue"
                            "black")}}
    (str (get-in conf [:description])
         (when-let [moves (:moves conf)]
           moves))]
   (when show-children?
     (for [child (get-in conf [:child-confs])]
       ^{:key (get-in child [:description])}
       [:li


        [dfs-traversal-web' child (inc depth)]]))])


(defn dfs-traversal-web'
  ([tree]

   [dfs-traversal-web' tree 0])
  ([tree depth]
   [display-conf tree depth (not= @*depth-level depth)]))



(defn main
  []
  (let [final-tree (solve/l3-solver (solve/cube-solver-moves [:U :D :L :U' :R' :L]))]
    [:div
     [:p "CUBE SOLVER"]
     [counting-component (max-depth final-tree)]
     [dfs-traversal-web' final-tree]
     (when @*current-conf
       [web-cube (:end-cube @*current-conf)])]))

(defn start []
  (rdom/render [main] (js/document.getElementById "app")))
(start)