(ns solve
  (:require [cube :as cb]
            [nextjournal.clerk :as clerk]))

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
  (clerk/html (into [:svg {:width 800 :height 600}]
                    (generate-cube current-cube))))



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
                :FDR [[:front 2 2 ] [:down 0 2] [:right 2 0]]
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

(def test-cube (cb/apply-moves cb/solved [:R :L :D :L' :U' :L :D' :B :R :U' :U']))

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

(defn display-cube-moves
  [cube-moves]
  (map (comp web-cube :cube) cube-moves))







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
          final-moves (into corner-moves orient-moves )]
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



(display-cube-moves (get-resultant-cubes test-cube (layer-1 test-cube)))



(comment 
  (white-cross-moves (cb/apply-moves cb/solved [:R :L :D :L' :U' :L :D' :B :R :U']))
  )


  





(comment
  (find-piece-edge-move (cb/apply-moves cb/solved [:F'])))
nil

