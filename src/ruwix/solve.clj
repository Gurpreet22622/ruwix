(ns ruwix.solve
  (:require [clojure.string :as str]
            [ruwix.cube :as cb]))

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

(comment 
  (get-resultant-cubes cb/solved [[:L :U'] [:U :L :D]]))



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
(def last-move [:R' :D' :R :D :R' :D' :R :D])



;;make the alignment of cube as white in front and then rotate complete...
;; ...cube clockwise to set every one of four pieces.







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
  {[1 :up]         []
   [1 :left]       [:L' :U']
   [1 :right]      [:R :U]
   [1 :down]       [:D :D :B :B :U :U]
   [2 :up-left]    [:U']
   [2 :up-right]   [:U]
   [2 :down-left]  [:D' :B :B :D :U :U]
   [2 :down-right] [:D :B' :B' :D' :U' :U']
   [3 :up]         [:U :U]
   [3 :left]       [:B :U :U]
   [3 :right]      [:B' :U :U]
   [3 :down]       [:B :B :U :U]})



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

#_(def test-cube (cb/apply-moves cb/solved [:L :D :L' :U' :B :R :U' :CU]))

(def layer2-testcube (cb/apply-moves cb/solved [:L :L' :U' :B :R :U' :CU]))
(def test-cube2 layer2-testcube)
(defn find-piece-edge-move2
  [{:keys [front back right left up down] :as cb}]
  (let [f-col (get-in front [1 1])
        u-col (get-in up [1 1])]
    (get front-up-edge-moves (->> cb
                                  enumerate-edges
                                  keys
                                  (filter (comp #{#{f-col u-col}} (enumerate-edges cb)))
                                  first))))
(defn find-piece-edge-move
  [{{:keys [front up] :as cb} :end-cube}]
  (let [f-col (get-in front [1 1])
        u-col (get-in up [1 1])
        front-up-edge-moves {[1 :up]         []
                             [1 :left]       [:L' :U']
                             [1 :right]      [:R :U]
                             [1 :down]       [:D :D :B :B :U :U]
                             [2 :up-left]    [:U']
                             [2 :up-right]   [:U]
                             [2 :down-left]  [:D' :B :B :D :U :U]
                             [2 :down-right] [:D :B' :B' :D' :U' :U']
                             [3 :up]         [:U :U]
                             [3 :left]       [:B :U :U]
                             [3 :right]      [:B' :U :U]
                             [3 :down]       [:B :B :U :U]}
        moves (get front-up-edge-moves (->> cb
                                            enumerate-edges
                                            keys
                                            (filter (comp #{#{f-col u-col}} (enumerate-edges cb)))
                                            first))]
    {:start-cube cb
     :moves moves
     :description "find move to correct front up edge peice"
     :end-cube (cb/apply-moves cb moves)}))


(defn find-corner-edge-move
  [{:keys [front up right] :as cb}]
  (let [req-piece #{(get-in front [1 1]) (get-in up [1 1]) (get-in right [1 1])}
        move-maps {[:front 0 0] [:L :D :L']
                   [:front 0 2] []
                   [:front 2 0] [:D]
                   [:front 2 2] []
                   [:back 0 0] [:B' :D' :B]
                   [:back 0 2] [:L' :D :D :L]
                   [:back 2 0] [:D']
                   [:back 2 2] [:D :D]}]
    (cond
      (= req-piece (get-piece cb :FUL)) (get move-maps [:front 0 0])
      (= req-piece (get-piece cb :FUR)) (get move-maps [:front 0 2])
      (= req-piece (get-piece cb :FDL)) (get move-maps [:front 2 0])
      (= req-piece (get-piece cb :FDR)) (get move-maps [:front 2 2])
      (= req-piece (get-piece cb :BUL)) (get move-maps [:back 0 0])
      (= req-piece (get-piece cb :BUR)) (get move-maps [:back 0 2])
      (= req-piece (get-piece cb :BDL)) (get move-maps [:back 2 0])
      (= req-piece (get-piece cb :BDR)) (get move-maps [:back 2 2]))))

(defn find-corner-edge-move2
  [{{:keys [front right up] :as cb} :end-cube}]
  (let [req-piece #{(get-in front [1 1]) (get-in up [1 1]) (get-in right [1 1])}
        move-maps {[:front 0 0] [:L :D :L']
                   [:front 0 2] []
                   [:front 2 0] [:D]
                   [:front 2 2] []
                   [:back 0 0] [:B' :D' :B]
                   [:back 0 2] [:L' :D :D :L]
                   [:back 2 0] [:D']
                   [:back 2 2] [:D :D]}
        moves (cond
                (= req-piece (get-piece cb :FUL)) (get move-maps [:front 0 0])
                (= req-piece (get-piece cb :FUR)) (get move-maps [:front 0 2])
                (= req-piece (get-piece cb :FDL)) (get move-maps [:front 2 0])
                (= req-piece (get-piece cb :FDR)) (get move-maps [:front 2 2])
                (= req-piece (get-piece cb :BUL)) (get move-maps [:back 0 0])
                (= req-piece (get-piece cb :BUR)) (get move-maps [:back 0 2])
                (= req-piece (get-piece cb :BDL)) (get move-maps [:back 2 0])
                (= req-piece (get-piece cb :BDR)) (get move-maps [:back 2 2]))]
    {:start-cube cb
     :end-cube (cb/apply-moves cb moves)
     :description "finding respective position of front up right corner and place it"
     :moves moves}))


(defn set-corner-piece-moves
  [cube]
  (loop [cb cube
         cnt 0]
    (if (and (= (get-piece cb :FUR) #{(get-in cb [:front 1 1]) (get-in cb [:up 1 1]) (get-in cb [:right 1 1])})
             (= (get-in cb [:front 0 2]) (get-in cb [:front 1 1])))
      (into [] (apply concat (repeat cnt try-corner-move)))
      (recur (cb/apply-moves cb try-corner-move) (inc cnt)))))

(defn set-corner-piece-moves2
  [{cube :end-cube}]
  (loop [cb cube
         cnt 0]
    (if (and (= (get-piece cb :FUR) #{(get-in cb [:front 1 1]) (get-in cb [:up 1 1]) (get-in cb [:right 1 1])})
             (= (get-in cb [:front 0 2]) (get-in cb [:front 1 1])))
      (let [moves (into [] (apply concat (repeat cnt try-corner-move)))]
        {:start-cube cube
         :end-cube cb
         :description (str "orient corner piece. flipping: " cnt)
         :moves moves})
      (recur (cb/apply-moves cb try-corner-move) (inc cnt)))))


(defn vec-to-str
  [list-moves]
  (let [k (for [key list-moves]
            (str (name key)))
        final-str (str/join " " (into [] k))]
    final-str))


(defn white-cross-moves'
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


(defn white-cross-moves
  [{cb :end-cube :as conf}]
  (loop [ct 0
         cube cb
         child-confs []]
    (if (< ct 4)
      (let [inter-conf   (find-piece-edge-move {:end-cube cube})
            inter-cube   (:end-cube inter-conf)
            to-flip?     (not= (get-in inter-cube [:front 0 1]) (get-in inter-cube [:front 1 1]))
            extra-move   (conj (if to-flip? flip-upper-edge []) :CW)
            result-cube  (cb/apply-moves inter-cube extra-move)
            final-conf   {:start-cube cube
                          :end-cube result-cube
                          :description (str "edge: " (inc ct))
                          :moves (into (:moves inter-conf) extra-move)}]
        (recur (inc ct)
               result-cube
               (conj child-confs final-conf)))
      {:start-cube (:start-cube conf)
       :end-cube (:end-cube (last child-confs))
       :description (str (:description conf) " + white cross moves")
       :child-confs [conf
                     {:start-cube cb
                      :end-cube (:end-cube (last child-confs))
                      :description "formation of white cross on front face"
                      :child-confs child-confs}]})))




(defn complete-white-corners
  [{cb :end-cube :as conf}]
  (loop [ct 0
         cube cb
         child-confs []]
    (if (< ct 4)
      (let [corner-conf (find-corner-edge-move2 {:end-cube cube})
            inter-cube (:end-cube corner-conf)
            orient-moves (conj (:moves (set-corner-piece-moves2 corner-conf)) :CL)
            result-cube (cb/apply-moves inter-cube orient-moves)
            final-conf {:start-cube cube
                        :end-cube result-cube
                        :description (str "count: " (inc ct))
                        :moves (into (:moves corner-conf) orient-moves)}]
        (recur (inc ct)
               result-cube
               (conj child-confs final-conf)))
      {:start-cube (:start-cube conf)
       :end-cube (:end-cube (last child-confs))
       :description (str (:description conf) " + complete white corners")
       :child-confs

       [conf
        {:start-cube cb
         :end-cube (:end-cube (last child-confs))
         :description "setting of all corner pieces on upper face"
         :child-confs child-confs}]})))



(defn complete-white-corners'
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


;;TODO: move to new data structure



(defn layer-1
  [conf]
  (let [init-conf (-> conf
                      white-cross-moves)
        conf-2 (-> init-conf
                   (cb/apply-moves-on-conf [:CU] "rotate cube front->up"))
        conf-3 (complete-white-corners conf-2)
        conf-4 (cb/apply-moves-on-conf conf-3 [:CU :CU] "rotate cube up->down")]
    #_(js/console.log moves)
    #_(js/console.log corner-moves)
    {:start-cube (get-in conf [:end-cube])
     :end-cube (get-in conf-4 [:end-cube])
     :description "Layer-1"
     :child-confs [(last (get-in init-conf [:child-confs]))
                   (last (get-in conf-2 [:child-confs]))
                   (last (get-in conf-3 [:child-confs]))
                   (last (get-in conf-4 [:child-confs]))]}))




(defn layer-1'
  [conf]
  (let [init-conf (-> conf
                      white-cross-moves
                      (cb/apply-moves-on-conf [:CU] "rotate cube front->up"))
        inter-conf (complete-white-corners init-conf)]
    #_(js/console.log moves)
    #_(js/console.log corner-moves)
    (cb/apply-moves-on-conf inter-conf [:CU :CU] "rotate cube up->down")))




(defn dfs-traversal
  ([tree]
   (println (get-in tree [:description]))
   (dfs-traversal tree 1))
  ([tree depth]

   (doseq [child (get-in tree [:child-confs])]

     (println (str (str/join (repeat depth "    ")) depth " : "
                   (get-in child [:description])))
     (dfs-traversal child (inc depth)))))





(defn cube-solver
  [moves]
  (let [variable-conf (cb/apply-moves-on-conf {:end-cube cb/solved
                                               :start-cube cb/solved
                                               :description "solved"}
                                              moves
                                              "variable cube")
        layer-1-conf (layer-1 variable-conf)]
    {:start-cube cb/solved
     :end-cube (get-in layer-1-conf [:end-cube])
     :description "ruwix cube solver"
     :child-confs [(first (get-in variable-conf [:child-confs]))
                   (last (get-in variable-conf [:child-confs]))
                   layer-1-conf]}))


(defn cube-solver'
  [cube]
  (let [variable-conf {:start-cube cb/solved
                       :end-cube cube
                       :description "variable cube"
                       :moves "unknown"}
        layer-1-conf (layer-1 variable-conf)]
    {:start-cube cb/solved
     :end-cube (get-in layer-1-conf [:end-cube])
     :description "ruwix cube solver"
     :child-confs [(first (get-in variable-conf [:child-confs]))
                   (last (get-in variable-conf [:child-confs]))
                   layer-1-conf]}))




(comment
  (let [req-tree (layer-1 (cb/apply-moves-on-conf {:end-cube cb/solved
                                                   :start-cube cb/solved
                                                   :description "solved"}
                                                  [:U :D :L :U' :R' :L]
                                                  "variable cube"))
        try-tree (white-cross-moves (cb/apply-moves-on-conf {:end-cube cb/solved
                                                             :start-cube cb/solved
                                                             :description "solved"}
                                                            [:U :D :L :U' :R' :L]
                                                            "variable cube"))
        try-tree-2 (-> (cb/apply-moves-on-conf {:end-cube cb/solved
                                                :start-cube cb/solved
                                                :description "solved"}
                                               [:U :D :L :U' :R' :L]
                                               "variable cube")
                       white-cross-moves
                       (cb/apply-moves-on-conf [:CU] "rotate cube front->up"))
        try-tree-3 (complete-white-corners try-tree-2)
        final-tree (cube-solver [:U :D :L :U' :R' :L])]
    (dfs-traversal final-tree)
    )
  )








(defn layer-1'
  [cube]
  (let [moves (conj (white-cross-moves' cube) [:CU])
        cubes (get-resultant-cubes cube moves)
        inter-cube (:cube (last cubes))
        corner-moves (complete-white-corners' inter-cube)]
    #_(js/console.log moves)
    #_(js/console.log corner-moves)
    (concat moves corner-moves [[:CU :CU]])))







;; (defn in-position-yellow
;;   [conf]
;;   (loop [ct 0
;;          cb (get-in conf [:end-cube])
;;          moves []]
;;     (let [yellow-piece (get-in cb [:up 1 1])
;;           left-yellow? (or (= (get-in cb [:front 1 0]) yellow-piece)
;;                            (= (get-in cb [:left 1 2]) yellow-piece))
;;           up-yellow? (or (= (get-in cb [:front 0 1]) yellow-piece)
;;                          (= (get-in cb [:up 2 1]) yellow-piece))
;;           n-moves (if left-yellow?
;;                     (conj moves :CL)
;;                     (if up-yellow?
;;                       (into moves (conj adjust-left :CL))
;;                       (conj moves :U)))]
;;       (if (< ct 4)
;;         (recur (if (= (last n-moves) :U)
;;                  ct
;;                  (inc ct))
;;                {:start-cube }
;;                n-moves)
;;         moves))))


(defn in-position-yellow'
  [conf]
  (loop [
         cb (:end-cube conf)
         child-conf []]
    (let [yellow-piece (get-in cb [:up 1 1])
          left-yellow? (or (= (get-in cb [:front 1 0]) yellow-piece)
                           (= (get-in cb [:left 1 2]) yellow-piece))
          up-yellow? (or (= (get-in cb [:front 0 1]) yellow-piece)
                         (= (get-in cb [:up 2 1]) yellow-piece))
          final? (and (or (= (get-in cb [:front 1 0]) yellow-piece)
                          (= (get-in cb [:left 1 2]) yellow-piece))
                      (or (= (get-in cb [:front 1 2]) yellow-piece)
                          (= (get-in cb [:right 1 0]) yellow-piece))
                      (or (= (get-in cb [:left 1 0]) yellow-piece)
                          (= (get-in cb [:back 1 2]) yellow-piece))
                      (or (= (get-in cb [:right 1 2]) yellow-piece)
                          (= (get-in cb [:back 1 0]) yellow-piece)))
          n-conf (if left-yellow?
                   {:start-cube cb
                    :end-cube (cb/apply-moves cb [:CL])
                    :description "piece in left position, cube front->left"
                    :moves [:CL]}
                   (if up-yellow?
                     {:start-cube cb
                      :end-cube (cb/apply-moves cb (into [] (conj adjust-left :CL)))
                      :description "piece is at upper position, adjusting it at left, cube front->left"
                      :moves (into [] (conj adjust-left :CL))}
                     {:start-cube cb
                      :end-cube (cb/apply-moves cb [:U])
                      :description "piece is not at upper position, adjusting it at top"
                      :moves [:U]}
                     ))]
      (if final?
        {:start-cube (:start-cube conf)
         :end-cube (if (empty? child-conf)
                     (:end-cube conf)
                     (:end-cube (last child-conf)))
         :description (:description conf)
         :child-confs (vec (flatten [(:child-confs conf)
                                     {:start-cube (:end-cube conf)
                                      :end-cube (if (empty? child-conf)
                                                  (:end-cube conf)
                                                  (:end-cube (last child-conf)))
                                      :description "Layer-2"
                                      :child-confs [{:start-cube (:end-cube conf)
                                                     :end-cube (if (empty? child-conf)
                                                                 (:end-cube conf)
                                                                 (:end-cube (last child-conf)))
                                                     :description "getting all yellow corner piece in position"
                                                     :child-confs child-conf}]}]))}
        (recur 
         (:end-cube n-conf)
         (conj child-conf n-conf))))))



(comment
  (let [req-tree (layer-1 (cb/apply-moves-on-conf {:end-cube cb/solved
                                                   :start-cube cb/solved
                                                   :description "solved"}
                                                  [:U :D :L :U' :R' :L]
                                                  "variable cube"))
        try-tree (white-cross-moves (cb/apply-moves-on-conf {:end-cube cb/solved
                                                             :start-cube cb/solved
                                                             :description "solved"}
                                                            [:U :D :L :U' :R' :L]
                                                            "variable cube"))
        try-tree-2 (-> (cb/apply-moves-on-conf {:end-cube cb/solved
                                                :start-cube cb/solved
                                                :description "solved"}
                                               [:U :D :L :U' :R' :L]
                                               "variable cube")
                       white-cross-moves
                       (cb/apply-moves-on-conf [:CU] "rotate cube front->up"))
        try-tree-3 (complete-white-corners try-tree-2)
        final-tree (cube-solver' {:front [[:e :e :b] [:a :e :a] [:c :c :e]],
                                  :back [[:d :d :f] [:b :f :c] [:b :a :a]],
                                  :right [[:f :f :a] [:c :c :f] [:c :f :e]],
                                  :left [[:b :c :d] [:e :d :d] [:d :e :a]],
                                  :up [[:c :b :f] [:b :b :d] [:b :b :d]],
                                  :down [[:f :f :a] [:d :a :a] [:e :e :c]]})]
    (dfs-traversal (in-position-yellow' final-tree)))
  )




(defn cube-solver-l2-I
  [conf]
  (let [layer-1-conf (layer-1 conf)]
    (in-position-yellow' layer-1-conf)))

(comment
  (dfs-traversal (cube-solver-l2-I [:U :D :L :U' :R' :L])))


(comment	 (def cb-test2 (reduce cb/apply-moves test-cube (layer-1' test-cube)))
          (def cb-test3 (cb/apply-moves (reduce cb/apply-moves test-cube (layer-1' test-cube)) (in-position-yellow cb-test2))))




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










(defn layer-2'
  [conf]
  (loop [ct 0
         cb (:end-cube conf)
         sub-confs []]

    (let [front-piece (get-in cb [:front 1 1])
          up-piece (get-in cb [:up 1 1])
          up-piece? (or (and (= (get-in cb [:front 0 1]) front-piece)
                             (not= (get-in cb [:up 2 1]) up-piece))
                        (and (= (get-in cb [:left 0 1]) front-piece)
                             (not= (get-in cb [:up 1 0]) up-piece))
                        (and (= (get-in cb [:right 0 1]) front-piece)
                             (not= (get-in cb [:up 1 2]) up-piece))
                        (and (= (get-in cb [:back 0 1]) front-piece)
                             (not= (get-in cb [:up 0 1]) up-piece)))
          up-flipped? (or (and (= (get-in cb [:front 0 1]) (get-in cb [:left 1 1]))
                               (= (get-in cb [:up 2 1]) (get-in cb [:front 1 1])))
                          (and (= (get-in cb [:right 0 1]) (get-in cb [:left 1 1]))
                               (= (get-in cb [:up 1 2]) (get-in cb [:front 1 1])))
                          (and (= (get-in cb [:left 0 1]) (get-in cb [:left 1 1]))
                               (= (get-in cb [:up 1 0]) (get-in cb [:front 1 1])))
                          (and (= (get-in cb [:back 0 1]) (get-in cb [:left 1 1]))
                               (= (get-in cb [:up 0 1]) (get-in cb [:front 1 1]))))
          ready-to-move? (= (get-in cb [:front 0 1]) front-piece)
          moving-piece-valid (and (not= (get-in cb [:front 0 1]) up-piece)
                                  (not= (get-in cb [:up 2 1]) up-piece))
          in-left? (= (get-in cb [:up 2 1]) (get-in cb [:left 1 1]))
          flipping-left? (and (= (get-in cb [:front 1 1]) (get-in cb [:up 2 1]))
                              (= (get-in cb [:left 1 1]) (get-in cb [:front 0 1])))
          flipping-right? (and (= (get-in cb [:front 1 1]) (get-in cb [:up 2 1]))
                               (= (get-in cb [:right 1 1]) (get-in cb [:front 0 1])))
          in-conf? (and (= (get-in cb [:front 1 0]) (get-in cb [:front 1 1]) (get-in cb [:front 1 2]))
                        (= (get-in cb [:right 1 0]) (get-in cb [:right 1 1]) (get-in cb [:right 1 2]))
                        (= (get-in cb [:left 1 0]) (get-in cb [:left 1 1]) (get-in cb [:left 1 2]))
                        (= (get-in cb [:back 1 0]) (get-in cb [:back 1 1]) (get-in cb [:back 1 2])))
          n-conf (if up-piece?
                   (if ready-to-move?
                     (if (and in-left? moving-piece-valid)
                       {:start-cube cb
                        :end-cube (cb/apply-moves cb adjust-left)
                        :description "putting upper edge -> left  edge"
                        :moves adjust-left}
                       (if moving-piece-valid
                         {:start-cube cb
                          :end-cube (cb/apply-moves cb adjust-right)
                          :description "putting upper edge -> right  edge"
                          :moves adjust-right}
                         {:start-cube cb
                          :end-cube (cb/apply-moves cb [:U])
                          :description "adjusting upper-most layer"
                          :moves [:U]}))
                     {:start-cube cb
                      :end-cube (cb/apply-moves cb [:U])
                      :description "adjusting upper-most layer"
                      :moves [:U]})
                   (if up-flipped?
                     (if (and flipping-left? moving-piece-valid)
                       {:start-cube cb
                        :end-cube (cb/apply-moves cb (vec (flatten (repeat 3 adjust-left))))
                        :description "putting upper egde -> left edge with flipping"
                        :moves (vec (flatten (repeat 3 adjust-left)))}
                       (if (and flipping-right? moving-piece-valid)
                         {:start-cube cb
                          :end-cube (cb/apply-moves cb (vec (flatten (repeat 3 adjust-right))))
                          :description "putting upper egde -> right edge with flipping"
                          :moves (vec (flatten (repeat 3 adjust-right)))}
                         {:start-cube cb
                          :end-cube (cb/apply-moves cb [:U])
                          :description "adjusting upper-most layer"
                          :moves [:U]}))
                     {:start-cube cb
                      :end-cube (cb/apply-moves cb [:CL])
                      :description "move cube front->left"
                      :moves [:CL]}))
          end-cube (if (empty? sub-confs)
                     (:end-cube conf)
                     (:end-cube (last sub-confs)))]
      (if in-conf?
        {:start-cube (:start-cube conf)
         :end-cube end-cube
         :description (:description  conf)
         :child-confs (vec (flatten [(vec (drop-last (:child-confs conf)))
                                     {:start-cube (:end-cube conf)
                                      :end-cube end-cube
                                      :description "Layer-2"
                                      :child-confs [(first (:child-confs (last (:child-confs conf))))
                                                    {:start-cube (get-in (last (:child-confs conf)) [:end-cube])
                                                     :end-cube end-cube
                                                     :description "adjusting layer 2 edges"
                                                     :child-confs sub-confs}]}]))}
        (recur
         (inc ct)
         (:end-cube n-conf)
         (conj sub-confs n-conf))))))





(comment
  
  (let [try-conf (layer-2' (in-position-yellow' (cube-solver' {:front [[:f :e :e] [:b :a :c] [:c :f :c]]
                                                               , :back [[:f :a :e] [:b :b :d] [:b :d :b]]
                                                               , :right [[:c :c :c] [:e :d :d] [:a :d :d]]
                                                               , :left [[:d :f :d] [:a :c :f] [:d :e :a]]
                                                               , :up [[:a :c :b] [:a :f :b] [:a :b :b]]
                                                               , :down [[:f :c :e] [:a :e :e] [:f :f :e]]})))]
    try-conf)
  )





(defonce p-conf (atom nil))

(defn cube-solver-l2
  [conf]
  (try
    (let [layer-1-conf (layer-1 conf)
             mid-l2 (in-position-yellow' layer-1-conf)]
         (layer-2' mid-l2))
    (catch Exception _e
           (do (reset! p-conf conf)
               (throw _e))
      )))

(comment
  @p-conf
  (cube-solver-l2 {:end-cube
                   {:front [[:a :c :e] [:f :e :e] [:e :e :b]],
                    :back [[:b :e :a] [:c :f :d] [:c :a :d]],
                    :right [[:a :d :d] [:c :b :b] [:f :b :b]],
                    :left [[:d :a :c] [:b :a :a] [:b :b :d]],
                    :up [[:f :d :f] [:d :d :f] [:f :f :c]],
                    :down [[:a :a :c] [:f :c :e] [:e :c :e]]},
                   :start-cube
                   {:front [[:a :c :e] [:f :e :e] [:e :e :b]],
                    :back [[:b :e :a] [:c :f :d] [:c :a :d]],
                    :right [[:a :d :d] [:c :b :b] [:f :b :b]],
                    :left [[:d :a :c] [:b :a :a] [:b :b :d]],
                    :up [[:f :d :f] [:d :d :f] [:f :f :c]],
                    :down [[:a :a :c] [:f :c :e] [:e :c :e]]}})
  )





(comment (def cb-test4 (get-in (last (let [moves (layer-2 cb-test3)
                                           cube-moves (get-resultant-cubes cb-test3 (mapv vector moves))]
                                       (reset! *debug-conf cube-moves))) [:cube])))



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






(defn seq-yellow-cross'
  [conf]
  (loop [cb (:end-cube conf)
         sub-confs []]
    (let [h-line? (= (get-in cb [:up 1 0]) (get-in cb [:up 1 1]) (get-in cb [:up 1 2]))
          v-line? (= (get-in cb [:up 0 1]) (get-in cb [:up 1 1]) (get-in cb [:up 2 1]))
          cross? (and v-line? h-line?)
          new-conf (cond
                     cross? sub-confs
                     (and v-line? (not h-line?))  {:start-cube cb
                                                   :end-cube (cb/apply-moves cb (into [:U] upper-yellow-cross))
                                                   :description "making upper cross"
                                                   :moves (into [:U] upper-yellow-cross)}
                     :else                        {:start-cube cb
                                                   :end-cube (cb/apply-moves cb (into upper-yellow-cross [:U]))
                                                   :description "making upper cross"
                                                   :moves (into upper-yellow-cross [:U])})
          end-cube (if (empty? sub-confs)
                     (:end-cube conf)
                     (:end-cube (last sub-confs)))]
      (if cross?
        {:start-cube (:start-cube conf)
         :end-cube end-cube
         :description (:description conf)
         :child-confs (vec (flatten [(:child-confs conf)
                                     {:start-cube (:end-cube conf)
                                      :end-cube end-cube
                                      :description "Layer-3"
                                      :child-confs [{:start-cube (:end-cube conf)
                                                     :end-cube end-cube
                                                     :description "sequencing upper cross"
                                                     :child-confs sub-confs}]}]))}
        (recur (:end-cube new-conf)
               (conj sub-confs new-conf))))))





(defn l3-solver-i
  [conf]
  (let [layer-1-conf (layer-1 conf)
        mid-l2 (in-position-yellow' layer-1-conf)
        l2 (layer-2' mid-l2)
        l3-i (seq-yellow-cross' l2)]
    l3-i))

(comment
  
  (let [try-conf (layer-2' (in-position-yellow' (cube-solver [:U :D :L :U' :R' :L])))
        l3-i-conf (seq-yellow-cross' try-conf)]
    ;;(first (:child-confs (last (:child-confs try-conf))))
    (:end-cube l3-i-conf))
  )







(comment (def cb-test5 (cb/apply-moves cb-test4 (seq-yellow-cross cb-test4))))


(defn adjust-yellow-cross
  [cube]
  (loop [ct 0
         cb cube
         moves []]
    (let [match? (= (get-in cb [:front 0 1]) (get-in cb [:front 1 1]))
          all-match? (and (= (get-in cb [:front 0 1]) (get-in cb [:front 1 1]))
                          (= (get-in cb [:left 0 1]) (get-in cb [:left 1 1]))
                          (= (get-in cb [:right 0 1]) (get-in cb [:right 1 1]))
                          (= (get-in cb [:back 0 1]) (get-in cb [:back 1 1])))
          new-move (cond
                     (and (= ct 0) (not match?)) (conj moves :U)
                     match? (conj moves :CR)
                     (not match?) (if (= (get-in cb [:front 1 1]) (get-in cb [:left 0 1]))
                                    (into moves swap-yellow-edges)
                                    (vec (flatten (conj moves [:CR] swap-yellow-edges [:CL] swap-yellow-edges)))))]
      (if all-match?
        moves
        (recur (if (= (last new-move) :CR)
                 (inc ct)
                 ct)
               (cb/apply-moves cube new-move)
               new-move)))))

(comment (def cb-test6 (cb/apply-moves cb-test5 (adjust-yellow-cross cb-test5))))



(defn move-yellow-corners
  [cube]
  (loop [ct 0
         cb cube
         moves []]
    (let [corner-set-right? (and (or (= (get-in cb [:up 1 1]) (get-in cb [:up 2 2]))
                                     (= (get-in cb [:up 1 1]) (get-in cb [:front 0 2]))
                                     (= (get-in cb [:up 1 1]) (get-in cb [:right 0 0])))
                                 (or (= (get-in cb [:front 1 1]) (get-in cb [:up 2 2]))
                                     (= (get-in cb [:front 1 1]) (get-in cb [:front 0 2]))
                                     (= (get-in cb [:front 1 1]) (get-in cb [:right 0 0])))
                                 (or (= (get-in cb [:right 1 1]) (get-in cb [:up 2 2]))
                                     (= (get-in cb [:right 1 1]) (get-in cb [:front 0 2]))
                                     (= (get-in cb [:right 1 1]) (get-in cb [:right 0 0]))))
          corner-set-left? (and (or (= (get-in cb [:up 1 1]) (get-in cb [:up 2 0]))
                                    (= (get-in cb [:up 1 1]) (get-in cb [:front 0 0]))
                                    (= (get-in cb [:up 1 1]) (get-in cb [:left 0 2])))
                                (or (= (get-in cb [:front 1 1]) (get-in cb [:up 2 0]))
                                    (= (get-in cb [:front 1 1]) (get-in cb [:front 0 0]))
                                    (= (get-in cb [:front 1 1]) (get-in cb [:left 0 2])))
                                (or (= (get-in cb [:left 1 1]) (get-in cb [:up 2 0]))
                                    (= (get-in cb [:left 1 1]) (get-in cb [:front 0 0]))
                                    (= (get-in cb [:left 1 1]) (get-in cb [:left 0 2]))))
          new-moves (cond
                      (and (< ct 4) (not corner-set-right?)) (conj moves :CR)
                      (and corner-set-left? corner-set-right?) moves
                      (and corner-set-right? (not corner-set-left?)) (into moves yellow-corners))]
      (if (and corner-set-left? corner-set-right?)
        new-moves
        (recur (inc ct)
               (cb/apply-moves cube new-moves)
               new-moves)))))

(comment (def cb-test7 (cb/apply-moves cb-test6 (move-yellow-corners cb-test6))))

(defn layer-3
  [cube]
  (loop [cb cube
         moves []]
    (let [in-position? (= (get-in cb [:up 1 1]) (get-in cb [:up 2 2]))
          f (get-in cb [:front 1 1])
          new-moves (cond
                      (not in-position?) (into moves last-move)
                      in-position? (conj moves :U'))]
      (if (and (= (get-in cb [:front]) [[f f f] [f f f] [f f f]])
               (= (get-in cb [:up]) (get-in cb/solved [:up])))
        moves
        (recur (cb/apply-moves cube new-moves)
               new-moves)))))


(comment
  (reset! *debug-conf [])
  (seq-yellow-cross cb-test4))

(comment
  (let [moves (layer-2 cb-test4)
        cube-moves (get-resultant-cubes cb-test4 (mapv vector moves))]
    (reset! *debug-conf cube-moves)))


(comment
  (let [moves (layer-2 cb-test3)
        cube-moves (get-resultant-cubes cb-test3 (mapv vector moves))]
    (reset! *debug-conf cube-moves)))


(comment
  (let [moves (in-position-yellow (reduce cb/apply-moves test-cube (layer-1' test-cube)))
        cube-moves (get-resultant-cubes (reduce cb/apply-moves test-cube (layer-1' test-cube)) (mapv vector moves))]
    cube-moves))

(comment
  (white-cross-moves' (cb/apply-moves cb/solved [:R :L :D :L' :U' :L :D' :B :R :U']))
  (let [req-conf (white-cross-moves {:end-cube (cb/apply-moves cb/solved [:R :L :D :L' :U' :L :D' :B :R :U'])})
        req-cube (:end-cube req-conf)]
    (complete-white-corners' (cb/apply-moves req-cube [:CU]))
    (complete-white-corners (cb/apply-moves-on-conf req-conf [:CU]))))




(comment
  (find-piece-edge-move {:end-cube (cb/apply-moves cb/solved [:F'])})
  (find-piece-edge-move2 (cb/apply-moves cb/solved [:F'])))


(comment
  (cb/apply-moves test-cube (flatten (layer-1' test-cube))))


