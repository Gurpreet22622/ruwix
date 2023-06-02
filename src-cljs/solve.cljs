(ns solve
  (:require [clojure.string :as str]
            [cube :as cb]
            ))

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
  (into [:svg {:width 800 :height 600}]
        (generate-cube current-cube)))



;;white cross formation 
(def flip-upper-edge [:U' :R :B :R' :U :U])


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



;;apply this move until desired config. achieved
(def try-corner-move [:R' :D' :R :D])






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
      (let [corner-conf (find-corner-edge-move {:end-cube cube})
            inter-cube (:end-cube corner-conf)
            orient-moves (conj (:moves (set-corner-piece-moves corner-conf)) :CL)
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





(defn dfs-traversal
  ([tree]
   (println (get-in tree [:description]))
   (dfs-traversal tree 1))
  ([tree depth]

   (doseq [child (get-in tree [:child-confs])]

     (println (str (str/join (repeat depth "    ")) depth " : "
                   (get-in child [:description])))
     (dfs-traversal child (inc depth)))))





(defn cube-solver-moves
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


(defn cube-solver-cube
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



(defn in-position-yellow
  [conf]
  (loop [cb (:end-cube conf)
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
                      :moves [:U]}))]
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





(defn cube-solver-l2-I
  [conf]
  (let [layer-1-conf (layer-1 conf)]
    (in-position-yellow layer-1-conf)))



(defn vec-to-str
  [list-moves]
  (let [k (for [key list-moves]
            (str (name key)))
        final-str (str/join " " (into [] k))]
    final-str))

(defn layer-2
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







(defonce p-conf (atom nil))

;; (defn cube-solver-l2
;;   [conf]
;;   (try
;;     (let [layer-1-conf (layer-1 conf)
;;           mid-l2 (in-position-yellow layer-1-conf)]
;;       (layer-2 mid-l2))
;;     (catch Exception _e
;;       (do (reset! p-conf conf)
;;           (throw _e)))))







(defn seq-yellow-cross
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
        mid-l2 (in-position-yellow layer-1-conf)
        l2 (layer-2 mid-l2)
        l3-i (seq-yellow-cross l2)]
    l3-i))










(defn adjust-yellow-cross
  [conf]
  (loop [ct 0
         cb (:end-cube conf)
         sub-confs []]
    (let [match? (= (get-in cb [:front 0 1]) (get-in cb [:front 1 1]))
          all-match? (and (= (get-in cb [:front 0 1]) (get-in cb [:front 1 1]))
                          (= (get-in cb [:left 0 1]) (get-in cb [:left 1 1]))
                          (= (get-in cb [:right 0 1]) (get-in cb [:right 1 1]))
                          (= (get-in cb [:back 0 1]) (get-in cb [:back 1 1])))
          search-adj? (= (get-in cb [:front 1 1]) (get-in cb [:left 0 1]))
          new-conf (cond
                     (and (= ct 0) (not match?)) {:start-cube cb
                                                  :end-cube (cb/apply-moves cb [:U])
                                                  :description "setting first edge"
                                                  :moves [:U]}
                     (and match? (= ct 1)) {:start-cube cb
                                            :end-cube (cb/apply-moves cb [:CR])
                                            :description "move cube front->right"
                                            :moves [:CR]}
                     (and match? (= ct 2)) {:start-cube cb
                                            :end-cube (cb/apply-moves cb [:CR])
                                            :description "move cube front->right"
                                            :moves [:CR]}
                     (and (not match?) (= ct 1)) (if search-adj?
                                                   {:start-cube cb
                                                    :end-cube (cb/apply-moves cb swap-yellow-edges)
                                                    :description "swapping the front and left upper edges"
                                                    :moves swap-yellow-edges}
                                                   {:start-cube cb
                                                    :end-cube (cb/apply-moves cb (vec (flatten (conj [:CR] swap-yellow-edges [:CL] swap-yellow-edges))))
                                                    :description "swapping the front and back upper edges"
                                                    :moves (vec (flatten (conj [:CR] swap-yellow-edges [:CL] swap-yellow-edges)))})
                     match? {:start-cube cb
                             :end-cube (cb/apply-moves cb [:CR])
                             :description "move cube front->right"
                             :moves [:CR]}
                     (not match?) {:start-cube cb
                                   :end-cube (cb/apply-moves cb swap-yellow-edges)
                                   :description "swapping the front and left upper edges"
                                   :moves swap-yellow-edges})
          end-cube (if (empty? sub-confs)
                     (:end-cube conf)
                     (:end-cube (last sub-confs)))]
      (if all-match?
        {:start-cube (:start-cube conf)
         :end-cube end-cube
         :description (:description conf)
         :child-confs (vec (flatten [(vec (drop-last (:child-confs conf)))
                                     {:start-cube (:end-cube conf)
                                      :end-cube end-cube
                                      :description "Layer-3"
                                      :child-confs [(first (:child-confs (last (:child-confs conf))))
                                                    {:start-cube (get-in (last (:child-confs conf)) [:end-cube])
                                                     :end-cube end-cube
                                                     :description "adjusting upper cross"
                                                     :child-confs sub-confs}]}]))}
        (recur (if match?
                 (inc ct)
                 ct)
               (:end-cube new-conf)
               (conj sub-confs new-conf))))))




(defn l3-solver-ii
  [conf]
  (let [layer-1-conf (layer-1 conf)
        mid-l2 (in-position-yellow layer-1-conf)
        l2 (layer-2 mid-l2)
        l3-i (seq-yellow-cross l2)
        l3-ii (adjust-yellow-cross l3-i)]
    l3-ii))




(defn move-yellow-corners
  [conf]
  (loop [cb (:end-cube conf)
         sub-confs []]
    (let [corner-set-right? (or (and (= (get-in cb [:up 1 1]) (get-in cb [:up 2 2]))
                                     (= (get-in cb [:front 1 1]) (get-in cb [:front 0 2]))
                                     (= (get-in cb [:right 1 1]) (get-in cb [:right 0 0])))
                                (and (= (get-in cb [:up 1 1]) (get-in cb [:front 0 2]))
                                     (= (get-in cb [:front 1 1]) (get-in cb [:right 0 0]))
                                     (= (get-in cb [:right 1 1]) (get-in cb [:up 2 2])))
                                (and (= (get-in cb [:up 1 1]) (get-in cb [:right 0 0]))
                                     (= (get-in cb [:front 1 1]) (get-in cb [:up 2 2]))
                                     (= (get-in cb [:right 1 1]) (get-in cb [:front 0 2]))))
          corner-set-left? (or (and (= (get-in cb [:up 1 1]) (get-in cb [:up 2 0]))
                                    (= (get-in cb [:front 1 1]) (get-in cb [:front 0 0]))
                                    (= (get-in cb [:left 1 1]) (get-in cb [:left 0 2])))
                               (and (= (get-in cb [:up 1 1]) (get-in cb [:front 0 0]))
                                    (= (get-in cb [:front 1 1]) (get-in cb [:left 0 2]))
                                    (= (get-in cb [:left 1 1]) (get-in cb [:up 2 0])))
                               (and (= (get-in cb [:up 1 1]) (get-in cb [:left 0 2]))
                                    (= (get-in cb [:front 1 1]) (get-in cb [:up 2 0]))
                                    (= (get-in cb [:left 1 1]) (get-in cb [:front 0 0]))))
          corner-set-back-left? (or (and (= (get-in cb [:back 1 1]) (get-in cb [:back 0 2]))
                                         (= (get-in cb [:up 1 1]) (get-in cb [:up 0 0]))
                                         (= (get-in cb [:left 1 1]) (get-in cb [:left 0 0])))
                                    (and (= (get-in cb [:back 1 1]) (get-in cb [:up 0 0]))
                                         (= (get-in cb [:up 1 1]) (get-in cb [:left 0 0]))
                                         (= (get-in cb [:left 1 1]) (get-in cb [:back 0 2])))
                                    (and (= (get-in cb [:back 1 1]) (get-in cb [:left 0 0]))
                                         (= (get-in cb [:up 1 1]) (get-in cb [:back 0 2]))
                                         (= (get-in cb [:left 1 1]) (get-in cb [:up 0 0]))))
          corner-set-back-right? (or (and (= (get-in cb [:back 1 1]) (get-in cb [:back 0 0]))
                                          (= (get-in cb [:up 1 1]) (get-in cb [:up 0 2]))
                                          (= (get-in cb [:right 1 1]) (get-in cb [:right 0 2])))
                                     (and (= (get-in cb [:back 1 1]) (get-in cb [:up 0 2]))
                                          (= (get-in cb [:up 1 1]) (get-in cb [:right 0 2]))
                                          (= (get-in cb [:right 1 1]) (get-in cb [:back 0 0])))
                                     (and (= (get-in cb [:back 1 1]) (get-in cb [:right 0 2]))
                                          (= (get-in cb [:up 1 1]) (get-in cb [:back 0 0]))
                                          (= (get-in cb [:right 1 1]) (get-in cb [:up 0 2]))))
          new-confs (cond
                      (and corner-set-right? (not corner-set-left?)) {:start-cube cb
                                                                      :end-cube (cb/apply-moves cb yellow-corners)
                                                                      :description "moving upper corners"
                                                                      :moves yellow-corners}
                      corner-set-left? {:start-cube cb
                                        :end-cube (cb/apply-moves cb [:CR])
                                        :description "adjusting req-piece to upper right corner"
                                        :moves [:CR]}
                      corner-set-back-right? {:start-cube cb
                                              :end-cube (cb/apply-moves cb [:CL])
                                              :description "adjusting req-piece to upper right corner"
                                              :moves [:CL]}
                      corner-set-back-left? {:start-cube cb
                                             :end-cube (cb/apply-moves cb [:CL :CL])
                                             :description "adjusting req-piece to upper right corner"
                                             :moves [:CL :CL]}
                      :else {:start-cube cb
                             :end-cube (cb/apply-moves cb yellow-corners)
                             :description "moving upper corners"
                             :moves yellow-corners})
          end-cube (if (empty? sub-confs)
                     (:end-cube conf)
                     (:end-cube (last sub-confs)))]
      (if (and corner-set-right?
               corner-set-left?)
        {:start-cube (:start-cube conf)
         :end-cube end-cube
         :description (:description conf)
         :child-confs (vec (flatten [(vec (drop-last (:child-confs conf)))
                                     {:start-cube (:end-cube conf)
                                      :end-cube end-cube
                                      :description "Layer-3"
                                      :child-confs [(first (:child-confs (last (:child-confs conf))))
                                                    (second (:child-confs (last (:child-confs conf))))
                                                    {:start-cube (get-in (last (:child-confs conf)) [:end-cube])
                                                     :end-cube end-cube
                                                     :description "move upper corners at right place"
                                                     :child-confs sub-confs}]}]))}
        (recur (:end-cube new-confs)
               (conj sub-confs new-confs))))))




(defn l3-solver-iii
  [conf]
  (let [layer-1-conf (layer-1 conf)
        mid-l2 (in-position-yellow layer-1-conf)
        l2 (layer-2 mid-l2)
        l3-i (seq-yellow-cross l2)
        l3-ii (adjust-yellow-cross l3-i)
        l3-iii (move-yellow-corners l3-ii)]
    l3-iii))





(defn layer-3
  [conf]
  (loop [cb (:end-cube conf)
         sub-confs []]
    (let [in-position? (= (get-in cb [:up 1 1]) (get-in cb [:up 2 2]))
          f (get-in cb [:front 1 1])
          u (get-in cb [:up 1 1])
          new-conf (cond
                     (not in-position?) {:start-cube cb
                                         :end-cube (cb/apply-moves cb last-move)
                                         :description "adjusting corners orientation"
                                         :moves last-move}
                     in-position? {:start-cube cb
                                   :end-cube (cb/apply-moves cb [:U'])
                                   :description "adjusting corners"
                                   :moves [:U']})
          end-cube (if (empty? sub-confs)
                     (:end-cube conf)
                     (:end-cube (last sub-confs)))]
      (if (and (= (get-in cb [:front]) [[f f f] [f f f] [f f f]])
               (= (get-in cb [:up]) [[u u u] [u u u] [u u u]]))
        {:start-cube (:start-cube conf)
         :end-cube end-cube
         :description (:description conf)
         :child-confs (vec (flatten [(vec (drop-last (:child-confs conf)))
                                     {:start-cube (:end-cube conf)
                                      :end-cube end-cube
                                      :description "Layer-3"
                                      :child-confs [(first (:child-confs (last (:child-confs conf))))
                                                    (second (:child-confs (last (:child-confs conf))))
                                                    (last (:child-confs (last (:child-confs conf))))
                                                    {:start-cube (get-in (last (:child-confs conf)) [:end-cube])
                                                     :end-cube end-cube
                                                     :description "orienting upper-corners"
                                                     :child-confs sub-confs}]}]))}
        (recur (:end-cube new-conf)
               (conj sub-confs new-conf))))))






(defn l3-solver
  [conf]
  (let [layer-1-conf (layer-1 conf)
        mid-l2 (in-position-yellow layer-1-conf)
        l2 (layer-2 mid-l2)
        l3-i (seq-yellow-cross l2)
        l3-ii (adjust-yellow-cross l3-i)
        l3-iii (move-yellow-corners l3-ii)
        l3-final (layer-3 l3-iii)]
    l3-final))



