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
  (let [input-confs (repeatedly 10000 (fn []
                                    (let [rand-cube (generate-random-cube 20)]
                                      {:end-cube rand-cube
                                       :start-cube rand-cube})))
        end-conf (mapv solve/cube-solver-l2-I input-confs)] 
    (testing "layer 2 - I testing"
      (doseq [conf end-conf
              :let [cb (:end-cube conf)
                    req-piece (get-in cb [:up 1 1])]]
        (when-not cb
          (println (:start-cube conf)))
        (is (some? cb))
        (is (or (= (get-in cb [:front 1 0]) req-piece)
                (= (get-in cb [:left 1 2]) req-piece)))
        (is (or (= (get-in cb [:front 1 2]) req-piece)
                (= (get-in cb [:right 1 0]) req-piece)))
        (is (or (= (get-in cb [:left 1 0]) req-piece)
                (= (get-in cb [:back 1 2]) req-piece)))
        (is (or (= (get-in cb [:right 1 2]) req-piece)
                (= (get-in cb [:back 1 0]) req-piece))) 
        ))))


(deftest layer-2-test []
  (let [input-confs (repeatedly 10000 (fn []
                                       (let [rand-cube (generate-random-cube 20)]
                                         {:end-cube rand-cube
                                          :start-cube cb/solved})))
        end-conf (mapv (fn [c]
                         
                         (solve/cube-solver-l2 c)) input-confs)]
    
    (testing "layer 2 testing"
      (doseq [conf end-conf
              :let [cb (:end-cube conf)
                    ]]
        (is (some? cb))
        (is (= (get-in cb [:front 1 0]) (get-in cb [:front 1 1]) (get-in cb [:front 1 2])))
        (is (= (get-in cb [:right 1 0]) (get-in cb [:right 1 1]) (get-in cb [:right 1 2])))
        (is (= (get-in cb [:left 1 0]) (get-in cb [:left 1 1]) (get-in cb [:left 1 2])))
        (is (= (get-in cb [:back 1 0]) (get-in cb [:back 1 1]) (get-in cb [:back 1 2])))))
    ))





(deftest layer-3-i-test []
  (let [input-confs (repeatedly 10000 (fn []
                                        (let [rand-cube (generate-random-cube 20)]
                                          {:end-cube rand-cube
                                           :start-cube cb/solved})))
        end-conf (mapv (fn [c]

                         (solve/l3-solver-i c)) input-confs)]

    (testing "layer 3 i testing"
      (doseq [conf end-conf
              :let [cb (:end-cube conf)
                    up (get-in cb [:up 1 1])]]
        (is (some? cb))
        (is (= (get-in cb [:up 0 1]) up))
        (is (= (get-in cb [:up 1 0]) up))
        (is (= (get-in cb [:up 1 2]) up))
        (is (= (get-in cb [:up 2 1]) up))
        ))))


(deftest layer-3-ii-test []
  (let [input-confs (repeatedly 100000 (fn []
                                         (let [rand-cube (generate-random-cube 20)]
                                           {:end-cube rand-cube
                                            :start-cube cb/solved})))
        end-conf (map (fn [c]

                        (solve/l3-solver-ii c)) input-confs)]

    (testing "layer 3 ii testing"
      (doseq [conf end-conf
              :let [cb (:end-cube conf)]]
        (is (some? cb))
        (is (= (get-in cb [:front 1 1]) (get-in cb [:front 0 1])))
        (is (= (get-in cb [:left 1 1]) (get-in cb [:left 0 1])))
        (is (= (get-in cb [:right 1 1]) (get-in cb [:right 0 1])))
        (is (= (get-in cb [:back 1 1]) (get-in cb [:back 0 1])))
        ))))





(deftest layer-3-iii-test []
  (let [input-confs (repeatedly 10000 (fn []
                                         (let [rand-cube (generate-random-cube 20)]
                                           {:end-cube rand-cube
                                            :start-cube cb/solved})))
        end-conf (map (fn [c]

                        (solve/l3-solver-iii c)) input-confs)]

    (testing "layer 3 iii testing"
      (doseq [conf end-conf
              :let [cb (:end-cube conf)
                    corner-set-right? (or (and (= (get-in cb [:up 1 1]) (get-in cb [:up 2 2]))
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
                                              (= (get-in cb [:left 1 1]) (get-in cb [:front 0 0]))))]]
        (is (some? cb))
        (is corner-set-left?)
        (is corner-set-right?)))))





(deftest layer-3-complete-test []
  (let [input-confs (repeatedly 100000 (fn []
                                         (let [rand-cube (generate-random-cube 20)]
                                           {:end-cube rand-cube
                                            :start-cube cb/solved})))
        end-conf (map-indexed (fn [i c]
                                (when (zero? (mod i 1000))
                                  (println "done: " i))
                                (solve/l3-solver c)) input-confs)]

    (testing "layer 3 complete testing"
      (doseq [conf end-conf
              :let [cb (:end-cube conf)
                    f (get-in cb [:front 1 1])
                    b (get-in cb [:back 1 1])
                    l (get-in cb [:left 1 1])
                    r (get-in cb [:right 1 1])
                    u (get-in cb [:up 1 1])
                    d (get-in cb [:down 1 1])]]
        (is (some? cb))
        (is (= (get-in cb [:front]) [[f f f] [f f f] [f f f]]))
        (is (= (get-in cb [:back]) [[b b b] [b b b] [b b b]]))
        (is (= (get-in cb [:left]) [[l l l] [l l l] [l l l]]))
        (is (= (get-in cb [:right]) [[r r r] [r r r] [r r r]]))
        (is (= (get-in cb [:up]) [[u u u] [u u u] [u u u]]))
        (is (= (get-in cb [:down]) [[d d d] [d d d] [d d d]]))))))