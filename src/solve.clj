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


(defn web-cube
  [current-cube]
  (clerk/html (into [:svg {:width 800 :height 800}]
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
;; (defn get-location-edge 
;;   [{:keys [front back up down right left]} clr1 clr2]
;;   ())
(defn find-piece-edge-move
  [{:keys [front back right left up down] :as cb}]
  (let [f-col (get-in front [1 1])
        u-col (get-in up [1 1])]
    (get front-up-edge-moves (->> cb
                                  enumerate-edges
                                  keys
                                  (filter (comp #{#{f-col u-col}} (enumerate-edges cb)))
                                  first))))



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



(defn get-resultant-cubes
  [cb moves]
  (reduce (fn [cubes-moves move-to-apply]
            (let [[cube _] (last cubes-moves)] 
              (conj cubes-moves [(cb/apply-moves cube move-to-apply) move-to-apply]))) 
          [[cb []]]
          moves))





(white-cross-moves (cb/apply-moves cb/solved [:R :L :D :L' :U' :L :D' :B :R :U']))


(let [r-cube (cb/apply-moves cb/solved [:R :L :D :L' :U' :L :D' :B :R :U'])
      moves (white-cross-moves r-cube)
      cubes (get-resultant-cubes r-cube moves)]
  (map (comp web-cube first) cubes))



(comment
  (find-piece-edge-move (cb/apply-moves cb/solved [:F'])))
nil

