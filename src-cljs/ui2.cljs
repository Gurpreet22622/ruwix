(ns ui2
  (:require   [solve :as solve]
              [reagent.core :as r]
              [reagent.dom :as rdom]
              [cube :as cb]))





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
  (into [:svg
         {:viewBox "0 0 700 600"}]
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
         [:H2 "List of moves"]
         #_(js/console.log cube-moves)
         [:ul
          (doall (map-indexed (fn [idx {:keys [moves]}]
                                ^{:key idx} [:li [:button {:class "btn btn-outline-primary btn-lg me-md-4 col-1"
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
   [:button {:class "btn btn-outline-secondary btn-lg me-md-5 col-2"
            
            :on-click (if (= @*depth-level limit)
                        #()
                        #(swap! *depth-level inc))}
    "Increase"]
   [:button {:class "btn btn-outline-secondary btn-lg me-md-5 col-2"
            
            :on-click (if (= @*depth-level 0)
                        #()
                        #(swap! *depth-level dec))}
    "Decrease"]])


(defn max-depth
  [tree]
  (let [child-depth (if (seq (:child-confs tree))
                      (mapv max-depth (:child-confs tree))
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
     (for [[idx child] (map-indexed vector (get-in conf [:child-confs]))]
       ^{:key idx}
       [:li


        [dfs-traversal-web' child (inc depth)]]))])


(defn dfs-traversal-web'
  ([tree]

   [dfs-traversal-web' tree 0])
  ([tree depth]
   [display-conf tree depth (not= @*depth-level depth)]))


(def *final-moves (r/atom []))

(def *inp-mvs (r/atom []))

(defn move-box 
  []
  (fn []
    [:div
     [:p {:class "fs-4 fst-italic"} [:span {:style {:color "green"}} "Input box"]]

     [:button {
               :class "btn btn-outline-primary btn-lg me-md-4 col-1 "
               
               :on-click (fn []
                           (reset! *inp-mvs (vec (flatten (conj @*inp-mvs [:F])))))}
      "F"]
     [:button {:class "btn btn-outline-primary btn-lg me-md-4 col-1"

               :on-click (fn []
                           (reset! *inp-mvs (vec (flatten (conj @*inp-mvs [:R])))))}
      "R"]
     
     [:button {:class "btn btn-outline-primary btn-lg me-md-4 col-1"
               
               :on-click (fn []
                           (reset! *inp-mvs (vec (flatten (conj @*inp-mvs [:U])))))}
      "U"]
     
     [:button {:class "btn btn-outline-primary btn-lg me-md-4 col-1"

               :on-click (fn []
                           (reset! *inp-mvs (vec (flatten (conj @*inp-mvs [:B])))))}
      "B"]
     
     [:button {:class "btn btn-outline-primary btn-lg me-md-4 col-1"
               
               :on-click (fn []
                           (reset! *inp-mvs (vec (flatten (conj @*inp-mvs [:L])))))}
      "L"]
     [:button {:class "btn btn-outline-primary btn-lg me-md-4 col-1"

               :on-click (fn []
                           (reset! *inp-mvs (vec (flatten (conj @*inp-mvs [:D])))))}
      "D"]
     [:p " "]
     [:button {:class "btn btn-outline-primary btn-lg me-md-4 col-1"

               :on-click (fn []
                           (reset! *inp-mvs (vec (flatten (conj @*inp-mvs [:F'])))))}
      "F'"]
     
     [:button {:class "btn btn-outline-primary btn-lg me-md-4 col-1"
               
               :on-click (fn []
                           (reset! *inp-mvs (vec (flatten (conj @*inp-mvs [:R'])))))}
      "R'"]
     
     [:button {:class "btn btn-outline-primary btn-lg me-md-4 col-1"

               :on-click (fn []
                           (reset! *inp-mvs (vec (flatten (conj @*inp-mvs [:U'])))))}
      "U'"]
     
     
     [:button {:class "btn btn-outline-primary btn-lg me-md-4 col-1"
               
               :on-click (fn []
                           (reset! *inp-mvs (vec (flatten (conj @*inp-mvs [:B'])))))}
      "B'"]
     
     [:button {:class "btn btn-outline-primary btn-lg me-md-4 col-1"

               :on-click (fn []
                           (reset! *inp-mvs (vec (flatten (conj @*inp-mvs [:L'])))))}
      "L'"]
     
     [:button {:class "btn btn-outline-primary btn-lg me-md-4 col-1"
               
               :on-click (fn []
                           (reset! *inp-mvs (vec (flatten (conj @*inp-mvs [:D'])))))}
      "D'"]
     [:p " "]
     [:p " "]
     (if (empty? @*inp-mvs)
       [:p "Moves Buffer: " [:span {:style {:color "orange"}} "Empty"]]
       [:p "Moves Buffer: " [:span {:style {:color "orange"}} (solve/vec-to-str @*inp-mvs)] "."])
     (if (empty? @*final-moves)
       [:p "Moves Applied: " [:span {:style {:color "orange"}} "NIL"]]
       [:p "Moved Applied: " [:span {:style {:color "orange"}} (solve/vec-to-str @*final-moves) "."]])
     [:button {:class "btn btn-success btn-lg me-md-4"
               :on-click (fn []
                           (reset! *final-moves (vec (flatten (conj [] @*inp-mvs)))))}
      "Submit"]
     [:button {:class "btn btn-danger btn-lg me-md-4"
               :on-click (fn []
                           (reset! *final-moves [])
                           (reset! *inp-mvs []))}
      "Clear"]
     [:button {:class "btn btn-warning btn-lg"
               :on-click (fn []
                           (reset! *inp-mvs (vec (drop-last @*inp-mvs))))}
      "Clear recent"]
     [:p " "]
     [:p " "]
     #_(js/console.log @*inp-mvs)]))





(defn dropdown
  []
  [:div {:class "form-floating"}
   [:select {:class "form-select"
                          ;;:id "floatingSelect"
                          ;;:arial-label "Floating label select example" 
             :on-change (fn [e]
                          
                          (case (.-value (.-target e))
                            "Peak 1" (reset! *final-moves [:U :B :B :F :F :D' :R :L :U' :R :L :D :B' :F' :U' :R :L :B :F])
                            "Peak 2" (reset! *final-moves [:U' :L :L :D' :B :B :D :L :L :U' :L' :B' :D :U' :R' :B' :U :F :D :F :F :R' :U])
                            "Peak 3" (reset! *final-moves [:U' :R :R :D :B :B :R :R :D :R :R :U' :L' :F :F :L :F :R :B' :D' :L :R :R :U])
                            "Peak 4" (reset! *final-moves [:U :L :L :U :R :R :B :R' :B :B :L :L :F :D' :B' :L :U :U :L :B :R' :U :B' :U :U])
                            "Peak 5" (reset! *final-moves [:B :B :F :F :U :B :B :F :F :L :L :R :R :U' :L :L :R :F :L' :F' :R :B :R' :F :D :U'])
                            "Peak 6" (reset! *final-moves [:L :L :D' :F :D :F' :R :R :D' :U' :F' :L :R :B :D :D :L' :D' :L :U])
                            "Stripped 1" (reset! *final-moves [:D' :F :D' :L :B :D :D :F :F :U :R :B' :U :R :R :F :D' :R :F :U :U])
                            "Stripped 2" (reset! *final-moves [:L :U :B :B :L :L :D :D :B :U :B' :D' :R' :B' :F' :U :U :B :B :U :R])
                            "Two rings" (reset! *final-moves [:F :F :D' :R :R :D' :L' :U' :L' :R :B :D' :U :B :L :F :F :L :U :U])
                            "open this menu to select designed cubes" (reset! *final-moves [])))}
    [:option  "open this menu to select designed cubes"]
    [:option  "Peak 1"]
    [:option  "Peak 2"]
    [:option  "Peak 3"]
    [:option  "Peak 4"]
    [:option  "Peak 5"]
    [:option  "Peak 6"]
    [:option  "Stripped 1"]
    [:option  "Stripped 2"]
    [:option  "Two rings"]]
   [:label {:for "floatingSelect"}
    "variable cubes!"]])



(defn main
  []
  (let [final-tree (solve/l3-solver (solve/cube-solver-moves @*final-moves))
        depth (max-depth final-tree)]
    [:div
     [:p {:class "text-center fw-bold fs-1"} [:span {:style {:color "red"}} "Cube Solver"]]
     [:div {:class "row"}
      [:div {:class "col-sm-7"}

       [move-box]

       [counting-component depth]
       [dfs-traversal-web' final-tree]]
      [:div {:class "col-sm"}
       (if (and (empty? @*final-moves) (not-empty @*inp-mvs))
         [web-cube (cb/apply-moves cb/solved @*inp-mvs)]
         (if @*current-conf
           [web-cube (:end-cube @*current-conf)]
           [web-cube (get-in (second (get-in final-tree [:child-confs])) [:end-cube])]))

       [dropdown]]]
     [:div {:class "text-center p-4", :style {:background-color "rgba (0, 0, 0, 0.05)"}}
      [:hr]
      "© 2023 | Gurpreet Singh"
      [:p ""]
      [:p "v 1.0.0"]
      [:a {:href "https://github.com/Gurpreet22622/ruwix"
           :target "blank"} [:img {:src "./img/github-logo.png"
                                   :width "20"
                                   :height "20"}]]]]))


;; (defn start []
;;   (rdom/render [main] (js/document.getElementById "app")))
;; (start)