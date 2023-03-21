(ns solve
  (:require [cube :as cb]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            #_[nextjournal.clerk :as clerk]))

(def face->color
  {:a "green"
   :b "blue"
   :c "pink"
   :d "orange"
   :e "yellow"
   :f "white"})


(defn get-resultant-cubes
  [cb moves]
  (reduce (fn [cubes-moves move-to-apply]
            (let [{cube :cube} (last cubes-moves)]
              (conj cubes-moves {:cube (cb/apply-moves cube move-to-apply)
                                 :moves move-to-apply})))
          [{:cube cb
            :moves []}]
          moves))



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
                            color (face->color (get-in face [row col]))]]
    [:rect   {:x            y
              :y            x
              :width        50
              :height       50
              :fill         color
              :stroke       "black"
              :stroke-width 5
              :rx           15
              :ry           15}]))

(defonce *current-conf (atom {:cube-moves (get-resultant-cubes cb/solved [])
                              :current 0}))

(defn web-cube
  [current-cube]
  (into [:svg {:width 800 :height 600}]
        (generate-cube current-cube)))



;;white cross formation 
(def flip-upper-edge [:U' :R :B :R' :U :U])

;;white corner formation 
;;if the required corner is below the desired position then use this algo n times.
(def put-corner-above [:R' :D' :R :D])

;;adjusting second layer algo
;;if wrong orientation then perform the respective operation twice
(def adjust-left [:U' :L' :U :L :U :F :U' :F'])
(def adjust-right [:U :R :U' :R' :U' :F' :U :F])


;;upper cross formation
;;perform until cross is formed
(def upper-yellow-cross [:F :R :U :R' :U' :F'])

;;swap yellow corners to the left always
(def swap-yellow-edges [:R :U :R' :U :R :U :U :R' :U])

;Swap yellow corners and find if any corner is already at position
(def yellow-corners [:U :R :U' :L' :U :R' :U' :L])

;;orient yellow corners
(def last-move [:R' :D' :R :D])



;;make the alignment of cube as white in front and then rotate complete...
;; ...cube clockwise to set every one of four pieces.

(def ph-1-up [])
(def ph-1-left [:L' :U'])
(def ph-1-right [:R :U])
(def ph-1-down [:D :D :B :B :U :U])
(def ph-2-up-left [:U'])
(def ph-2-up-right [:U])
(def ph-2-down-left [:D' :B :B :D :U :U])
(def ph-2-down-right [:D :B' :B' :D' :U' :U'])
(def ph-3-up [:U :U])
(def ph-3-right [:B' :U :U])
(def ph-3-left [:B :U :U])
(def ph-3-down [:B :B :U :U])






;;set the white corners, required face should be in the upper side 
(def front-up-right-corner-moves
  {[:front 0 0] [:L :D :L']
   [:front 0 2] []
   [:front 2 0] [:D]
   [:front 2 2] []
   [:back 0 0] [:B' :D' :B]
   [:back 0 2] [:L' :D :D :L]
   [:back 2 0] [:D']
   [:back 2 2] [:D :D]})
;;apply this move until desired config. achieved
(def try-corner-move [:R' :D' :R :D])





(def front-up-edge-moves
  {[1 :up]         ph-1-up
   [1 :left]       ph-1-left
   [1 :right]      ph-1-right
   [1 :down]       ph-1-down
   [2 :up-left]    ph-2-up-left
   [2 :up-right]   ph-2-up-right
   [2 :down-left]  ph-2-down-left
   [2 :down-right] ph-2-down-right
   [3 :up]         ph-3-up
   [3 :left]       ph-3-left
   [3 :right]      ph-3-right
   [3 :down]       ph-3-down})



(defn get-piece
  [cube corner]
  (let [paths (case corner
                :FUR [[:front 0 2] [:up 2 2] [:right 0 0]]
                :FUL [[:front 0 0] [:up 2 0] [:left 0 2]]
                :FDR [[:front 2 2] [:down 0 2] [:right 2 0]]
                :FDL [[:front 2 0] [:down 0 0] [:left 2 2]]
                :BUL [[:back 0 0] [:up 0 2] [:right 0 2]]
                :BUR [[:back 0 2] [:up 0 0] [:left 0 0]]
                :BDL [[:back 2 0] [:down 2 2] [:right 2 2]]
                :BDR [[:back 2 2] [:down 2 0] [:left 2 0]])]
    (into #{} (map #(get-in cube %)) paths)))



(defn enumerate-edges
  [cb]
  (let [paths->loc {[1 :up]         [[:front 0 1] [:up 2 1]]
                    [1 :left]       [[:front 1 0] [:left 1 2]]
                    [1 :right]      [[:front 1 2] [:right 1 0]]
                    [1 :down]       [[:front 2 1] [:down 0 1]]
                    [2 :up-left]    [[:up 1 0] [:left 0 1]]
                    [2 :up-right]   [[:up 1 2] [:right 0 1]]
                    [2 :down-left]  [[:down 1 0] [:left 2 1]]
                    [2 :down-right] [[:down 1 2] [:right 2 1]]
                    [3 :up]         [[:up 0 1] [:back 0 1]]
                    [3 :left]       [[:right 1 2] [:back 1 0]]
                    [3 :right]      [[:left 1 0] [:back 1 2]]
                    [3 :down]       [[:down 2 1] [:back 2 1]]}]
    (update-vals paths->loc (fn [[p1 p2]]
                              #{(get-in cb p1) (get-in cb p2)}))))

(def test-cube (cb/apply-moves cb/solved [:R :L :D :L' :U' :L :D' :B :R :U' :U' :CU]))

(defn find-piece-edge-move
  [{:keys [front back right left up down] :as cb}]
  (let [f-col (get-in front [1 1])
        u-col (get-in up [1 1])]
    (get front-up-edge-moves (->> cb
                                  enumerate-edges
                                  keys
                                  (filter (comp #{#{f-col u-col}} (enumerate-edges cb)))
                                  first))))

(defn find-corner-edge-move
  [{:keys [front back right left up down] :as cb}]
  (let [req-piece #{(get-in front [1 1]) (get-in up [1 1]) (get-in right [1 1])}
        move-maps front-up-right-corner-moves]
    (cond
      (= req-piece (get-piece cb :FUL)) (get move-maps [:front 0 0])
      (= req-piece (get-piece cb :FUR)) (get move-maps [:front 0 2])
      (= req-piece (get-piece cb :FDL)) (get move-maps [:front 2 0])
      (= req-piece (get-piece cb :FDR)) (get move-maps [:front 2 2])
      (= req-piece (get-piece cb :BUL)) (get move-maps [:back 0 0])
      (= req-piece (get-piece cb :BUR)) (get move-maps [:back 0 2])
      (= req-piece (get-piece cb :BDL)) (get move-maps [:back 2 0])
      (= req-piece (get-piece cb :BDR)) (get move-maps [:back 2 2]))))


(defn set-corner-piece-moves
  [cube]
  (loop [cb cube
         cnt 0]
    (if (and (= (get-piece cb :FUR) #{(get-in cb [:front 1 1]) (get-in cb [:up 1 1]) (get-in cb [:right 1 1])})
             (= (get-in cb [:front 0 2]) (get-in cb [:front 1 1])))
      (into [] (apply concat (repeat cnt try-corner-move)))
      (recur (cb/apply-moves cb try-corner-move) (inc cnt)))))


(defonce current (r/atom 0))
(defn display-cube-moves
  [cube-moves]
  (let [current (r/atom (dec (count cube-moves)))]
    (fn [cube-moves]
      (let [{:keys [cube moves]} (get cube-moves @current)
            last? (= (inc @current) (count cube-moves))
            next-moves (if last? []
                           (get-in cube-moves [(inc @current) :moves]))]
        [:div
         [web-cube cube]
         [:pre (str "previous: " moves "    next: " next-moves)]
         [:button
          {:on-click (fn []
                       (swap! current dec)
                       (js/console.log @current))
           :disabled (zero? @current)} "<<previous"]
         [:button
          {:on-click (fn []
                       (swap! current inc)
                       (js/console.log @current))
           :disabled last?} "next>>"]]))))







(defn white-cross-moves
  [cb]
  (loop [ct 0
         cube cb
         moves []]
    (let [edge-moves   (find-piece-edge-move cube)
          inter-cube   (cb/apply-moves cube edge-moves)
          to-flip?     (not= (get-in inter-cube [:front 0 1]) (get-in inter-cube [:front 1 1]))
          extra-move   (conj (if to-flip? flip-upper-edge []) :CW)
          result-cube  (cb/apply-moves inter-cube extra-move)
          final-moves  (into edge-moves extra-move)]
      (if (< ct 4)
        (recur (inc ct) result-cube (conj moves final-moves))
        moves))))


(defn complete-white-corners
  [cb]
  (loop [ct 0
         cube cb
         moves []]
    (let [corner-moves (find-corner-edge-move cube)
          inter-cube (cb/apply-moves cube corner-moves)
          orient-moves (conj (set-corner-piece-moves inter-cube) :CL)
          result-cube (cb/apply-moves inter-cube orient-moves)
          final-moves (into corner-moves orient-moves)]
      (if (< ct 4)
        (recur (inc ct)
               result-cube
               (conj moves final-moves))
        moves))))

(defn layer-1
  [cube]
  (let [moves (conj (white-cross-moves cube) [:CU])
        cubes (get-resultant-cubes cube moves)
        inter-cube (:cube (last cubes))
        corner-moves (complete-white-corners inter-cube)]
    (concat moves corner-moves [[:CU :CU]])))


(defonce *debug-conf (r/atom []))

(defn debug-cube
  []
  [display-cube-moves @*debug-conf])



(defn in-position-yellow
  [cube]
  (loop [ct 0
         cb cube
         moves []]
    (let [yellow-piece (get-in cb [:up 1 1])
          left-yellow? (or (= (get-in cb [:front 1 0]) yellow-piece)
                           (= (get-in cb [:left 1 2]) yellow-piece))
          up-yellow? (or (= (get-in cb [:front 0 1]) yellow-piece)
                         (= (get-in cb [:up 2 1]) yellow-piece))
          n-moves (if left-yellow?
                    (conj moves :CL)
                    (if up-yellow?
                      (into moves (conj adjust-left :CL))
                      (conj moves :U)))]
      (if (< ct 4)
        (recur (if (= (last n-moves) :U)
                 ct
                 (inc ct))
               (cb/apply-moves cube n-moves)
               n-moves)
        moves))))


(def cb-test2 (reduce cb/apply-moves test-cube (layer-1 test-cube)))

(def cb-test3 (cb/apply-moves (reduce cb/apply-moves test-cube (layer-1 test-cube)) (in-position-yellow cb-test2)))




(defn layer-2
  [cube]
  (loop [ct 0
         cb cube
         moves []]
    (let [front-piece (get-in cb [:front 1 1])
          up-piece? (or (= (get-in cb [:front 0 1]) front-piece)
                        (= (get-in cb [:left 0 1]) front-piece)
                        (= (get-in cb [:right 0 1]) front-piece)
                        (= (get-in cb [:back 0 1]) front-piece))
          ready-to-move? (= (get-in cb [:front 0 1]) front-piece)
          in-left? (= (get-in cb [:up 2 1]) (get-in cb [:left 1 2]))
          in-conf? (and (= (get-in cb [:front 1 0]) (get-in cb [:front 1 1]) (get-in cb [:front 1 2]))
                        (= (get-in cb [:right 1 0]) (get-in cb [:right 1 1]) (get-in cb [:right 1 2]))
                        (= (get-in cb [:left 1 0]) (get-in cb [:left 1 1]) (get-in cb [:left 1 2]))
                        (= (get-in cb [:back 1 0]) (get-in cb [:back 1 1]) (get-in cb [:back 1 2])))
          new-moves (if up-piece?
                      (if ready-to-move?
                        (if in-left?
                          (into moves adjust-left)
                          (into moves adjust-right))
                        (conj moves :U))
                      (conj moves :CL))]
      (if (< ct 4)
        (recur (if (= (last new-moves) :CL)
                 (inc ct)
                 ct)
               (cb/apply-moves cube new-moves)
               new-moves)
        (if in-conf?
          new-moves
          (recur (inc ct)
                 (cb/apply-moves cube new-moves)
                 new-moves))))))



(def cb-test4 (get-in (last (let [moves (layer-2 cb-test3)
                                  cube-moves (get-resultant-cubes cb-test3 (mapv vector moves))]
                              (reset! *debug-conf cube-moves))) [:cube]))



(defn seq-yellow-cross
  [cube]
  (loop [cb cube
         moves []]
    (let [h-line? (= (get-in cb [:up 1 0]) (get-in cb [:up 1 1]) (get-in cb [:up 1 2]))
          v-line? (= (get-in cb [:up 0 1]) (get-in cb [:up 1 1]) (get-in cb [:up 2 1]))
          cross? (and v-line? h-line?)
          new-move (cond 
                     cross? moves
                     (and v-line? (not h-line?)) (into moves (into [:U] upper-yellow-cross))
                     :else (into (into moves upper-yellow-cross) [:U]))]
      (if cross?
        new-move
        (recur (cb/apply-moves cube new-move)
               new-move)))))

(comment 
  (reset! *debug-conf [])
  (seq-yellow-cross cb-test4)
  )

(comment
  (let [moves (layer-2 cb-test4)
        cube-moves (get-resultant-cubes cb-test4 (mapv vector moves))]
    (reset! *debug-conf cube-moves)))


(comment
  (let [moves (layer-2 cb-test3)
        cube-moves (get-resultant-cubes cb-test3 (mapv vector moves))]
    (reset! *debug-conf cube-moves)))


(comment
  (let [moves (in-position-yellow cb-test2)
        cube-moves (get-resultant-cubes cb-test2 (mapv vector moves))]
    (reset! *debug-conf cube-moves)))

(comment
  (white-cross-moves (cb/apply-moves cb/solved [:R :L :D :L' :U' :L :D' :B :R :U'])))




(comment
  (find-piece-edge-move (cb/apply-moves cb/solved [:F'])))


(comment
  (cb/apply-moves test-cube (flatten (layer-1 test-cube)))
  )

(defn main []
  [:<>
   (let [l1-moves (layer-1 test-cube)
         _ (js/console.log "l1-moves: "l1-moves)
         cube1 (cb/apply-moves test-cube (flatten l1-moves))
         _ (reset! *debug-conf [{:cube cube1 :moves []}])
         mid-l1-moves (in-position-yellow cube1)
         _ (js/console.log "mid l1 moves: "mid-l1-moves)
         cube2 (cb/apply-moves cube1 (flatten mid-l1-moves))
         _ (swap! *debug-conf conj {:cube cube2 :moves (flatten mid-l1-moves)})
         l2-moves (layer-2 cube2)
         _ (js/console.log "l2-moves: "l2-moves)
         l2-cube (cb/apply-moves cube2 l2-moves)
         l3-1-moves (seq-yellow-cross l2-cube)
         _ (js/console.log "l3-1-moves: " l3-1-moves)
         all-moves (conj (into [] l1-moves) mid-l1-moves l2-moves l3-1-moves)
         _ (js/console.log all-moves)
         ]
     [display-cube-moves (get-resultant-cubes test-cube all-moves)])
   [:h2 "debug-cube"]
   [debug-cube]])

(defn start []
  (rdom/render [main] (js/document.getElementById "app")))
(start)

