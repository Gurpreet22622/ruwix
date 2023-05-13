(ns test
  (:require [clojure.set :as set]
              [cube]
              [solve]))



(def all-moves
  [:F]
  #_[:F :F' :U :U' :L :L' :D :D' :R :R' :B :B' #_:CL #_:CR #_:CU #_:CW])


;; generates random numbers of length same as all-moves(unique numbers)
(defn unique-random-numbers
  []
  (let [n (count all-moves)
        a-set (set (take n (repeatedly #(rand-int n))))]
    (concat a-set (set/difference (set (take n (range)))
                                  a-set))))




;; generate random n moves from all-moves (not unique elements) 
(defn choose-random 
  [r]
  (let [v all-moves]
    (mapv v (repeatedly r #(rand-int (count v))))))



;; Generates random moves of length all-moves (unique elements)
(defn get-random-moves
  []
  (loop [nums (unique-random-numbers)
         result []]
    (let [first (first nums)
          value (get all-moves first)
          n-result (conj result value)]
      (if (not= (count nums) 1)
        (recur (rest nums)
               n-result)
        n-result))))


(defn generate-random-cube
  [order]
  (let [moves (choose-random order)]
    (cube/apply-moves cube/solved moves)))

(comment
  (generate-random-cube 20)
  )





(defn layer-1-complete?
  [cube]
  (let [d (get-in cube [:down 1 1])
        f (get-in cube [:front 1 1])
        r (get-in cube [:right 1 1])
        l (get-in cube [:left 1 1])
        b (get-in cube [:back 1 1])]
    (and (= (get cube :down) [[d d d] [d d d] [d d d]])
         (= (get cube :front) [(get-in cube [:front 0])
                               [(get-in cube [:front 1 0]) f (get-in cube [:front 1 2])]
                               [f f f]])
         (= (get cube :left) [(get-in cube [:left 0]) [(get-in cube [:left 1 0]) l (get-in cube [:left 1 2])] [l l l]])
         (= (get cube :right) [(get-in cube [:right 0]) [(get-in cube [:right 1 0]) r (get-in cube [:right 1 2])] [r r r]])
         (= (get cube :back) [(get-in cube [:back 0]) [(get-in cube [:back 1 0]) b (get-in cube [:back 1 2])] [b b b]]))))



(defn layer-1-test
  [order]
  (loop [random-cube solve/test-cube #_(generate-random-cube 20)
         ct 0
         success 0]
    (let [moves-to-solve (solve/layer-1 random-cube)
          solved-cube (cube/apply-moves random-cube moves-to-solve)
          test-result (layer-1-complete? solved-cube)]
      (js/console.log solved-cube)
      (if (= ct order)
        [success]
        (recur (generate-random-cube 20)
               (inc ct)
               (if test-result
                 (inc success)
                 success))))))


(comment
  (layer-1-test 1)
  )