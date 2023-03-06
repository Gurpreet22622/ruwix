(ns solve
  ( :require [cube :as cb]))


;;white cross formation 
(def flip-upper-edge [:F :U' :R :U])

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


(cb/apply-moves cb/solved flip-upper-edge)