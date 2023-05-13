(ns ruwix.patterns 
  (:require [ruwix.cube :as cb]
            [ruwix.solve :refer [web-cube]]))




(def patterns [[:U :B :B :F :F :D' :R :L :U' :R :L :D :B' :F' :U' :R :L :B :F]
               [:U' :L :L :D' :B :B :D :L :L :U' :L' :B' :D :U' :R' :B' :U :F :D :F :F :R' :U]
               [:U' :R :R :D :B :B :R :R :D :R :R :U' :L' :F :F :L :F :R :B' :D' :L :R :R :U]
               [:U :L :L :U :R :R :B :R' :B :B :L :L :F :D' :B' :L :U :U :L :B :R' :U :B' :U :U]
               [:B :B :F :F :U :B :B :F :F :L :L :R :R :U' :L :L :R :F :L' :F' :R :B :R' :F :D :U']
               [:L :L :D' :F :D :F' :R :R :D' :U' :F' :L :R :B :D :D :L' :D' :L :U]])

(defn formation
  [sets]
  (map #(web-cube (cb/apply-moves cb/solved %)) sets))

(formation patterns)
nil