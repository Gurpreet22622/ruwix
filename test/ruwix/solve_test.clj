(ns ruwix.solve-test
  (:require [ruwix.cube :as cb]
            [ruwix.solve :as solve]
            [clojure.test :as test :refer [deftest is testing]]))

(def all-moves
  [:F :F' :U :U' :L :L' :D :D' :R :R' :B :B' :CL :CR :CU :CW])

(defn choose-random
  [r]
  (let [v all-moves]
    (mapv v (repeatedly r #(rand-int (count v))))))


(defn generate-random-cube
  [order]
  (let [moves (choose-random order)]
    (cb/apply-moves cb/solved moves)))

(comment
  (generate-random-cube 20))

(deftest white-cross-moves-test []
  (let [input-confs (repeatedly 10 (fn []
                                      (let [rand-cube (generate-random-cube 20)]
                                        {:end-cube rand-cube
                                         :start-cube rand-cube})))
        end-confs (mapv solve/white-cross-moves input-confs)]
    (testing "cube generated at distinct"
      (is (not= (get-in end-confs [0 :start-cube]) 
                (get-in end-confs [1 :start-cube]))))
    (testing "all front edges are in place"
      (doseq
       [conf end-confs
        :let [cb (:end-cube conf)
              fc (get-in cb [:front 1 1])]]
        (is (= 2 (count (:child-confs conf))))
        (is (= (get-in cb [:front 0 1]) fc))
        (is (= (get-in cb [:front 1 0]) fc))
        (is (= (get-in cb [:front 1 2]) fc))
        (is (= (get-in cb [:front 2 1]) fc))
        (is (= (get-in cb [:left 1 1]) (get-in cb [:left 1 2])))
        (is (= (get-in cb [:right 1 1]) (get-in cb [:right 1 0])))
        (is (= (get-in cb [:up 1 1]) (get-in cb [:up 2 1])))
        (is (= (get-in cb [:down 1 1]) (get-in cb [:down 0 1])))))))


(deftest layer-1-test []
  (let [input-confs (repeatedly 1000 (fn []
                                     (let [rand-cube (generate-random-cube 20)]
                                       {:end-cube rand-cube
                                        :start-cube rand-cube})))
        end-conf (mapv solve/layer-1 input-confs)]
    (println (get-in end-conf [0 :start-cube]))
    (testing "layer 1 is completely solved"
      (doseq
       [conf end-conf
        :let [cb (:end-cube conf)
              fc (get-in cb [:front 1 1])
              dc (get-in cb [:down 1 1])
              bc (get-in cb [:back 1 1])
              lc (get-in cb [:left 1 1])
              rc (get-in cb [:right 1 1])]]
        (is (= (get-in cb [:front 2]) [fc fc fc]))
        (is (= (get-in cb [:down]) [[dc dc dc] [dc dc dc] [dc dc dc]]))
        (is (= (get-in cb [:back 2]) [bc bc bc]))
        (is (= (get-in cb [:left 2]) [lc lc lc]))
        (is (= (get-in cb [:right 2]) [rc rc rc]))))))



(deftest layer-2-al1-test []
  (let [input-confs (repeatedly 1 (fn []
                                    (let [rand-cube (generate-random-cube 20)]
                                      {:end-cube rand-cube
                                       :start-cube rand-cube})))
        end-conf (mapv solve/cube-solver-l2-I input-confs)] 
    (testing "layer 2 - I testing"
      (doseq [conf end-conf
              :let [cb (:end-cube conf)
                    req-piece (get-in cb [:up 1 1])]]
        (is (or (= (get-in cb [:front 1 0]) req-piece)
                (= (get-in cb [:left 1 2]) req-piece)))
        (is (or (= (get-in cb [:front 1 2]) req-piece)
                (= (get-in cb [:right 1 0]) req-piece)))
        (is (or (= (get-in cb [:left 1 0]) req-piece)
                (= (get-in cb [:back 1 2]) req-piece)))
        (is (or (= (get-in cb [:right 1 2]) req-piece)
                (= (get-in cb [:left 1 0]) req-piece)))
        (println cb)
        ))))


(deftest layer-2-test []
  (let [input-confs (repeatedly 10 (fn []
                                     (let [rand-cube (generate-random-cube 20)]
                                       {:end-cube rand-cube
                                        :start-cube rand-cube})))
        end-conf (mapv solve/cube-solver-l2 input-confs)]
    (testing "layer 2 testing"
      (doseq [conf end-conf
              :let [cb (:end-cube conf)
                    ]]
        (is (= (get-in cb [:front 1 0]) (get-in cb [:front 1 1]) (get-in cb [:front 1 2])))
        (is (= (get-in cb [:right 1 0]) (get-in cb [:right 1 1]) (get-in cb [:right 1 2])))
        (is (= (get-in cb [:left 1 0]) (get-in cb [:left 1 1]) (get-in cb [:left 1 2])))
        (is (= (get-in cb [:back 1 0]) (get-in cb [:back 1 1]) (get-in cb [:back 1 2])))))
    ))