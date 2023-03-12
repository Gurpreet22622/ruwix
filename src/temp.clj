(ns temp)

^{::clerk/viewer '(fn [current-conf]
                    (let [face->color
                          {:a "green"
                           :b "blue"
                           :c "pink"
                           :d "orange"
                           :e "yellow"
                           :f "white"}
                          current (:current current-conf)
                          cube-moves (:cube-moves current-conf) 
                          ;;{:keys [cube-moves current]} current-conf
                          generate-cube (fn
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
                          faces (generate-cube (get cube-moves current))
                          _ (js/console.log (type current-conf))]
                      [:div 
                             (into [:svg {:width 800 :height 600}]
                                   faces)
                             [:button.bg-blue-500.hover:bg-blue-700.text-white.font-bold.py-2.px-4.rounded
                              {:on-click (fn [e] (nextjournal.clerk.render/clerk-eval '(prn @*current-conf)))} "Roll 🎲!"]]))}