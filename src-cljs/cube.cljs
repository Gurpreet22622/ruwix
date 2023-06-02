(ns cube)
(defn- make-face
  [color]
  (into [] ( repeat 3 (into [] (repeat 3 color))) ))


(defn- init-cube
  []
  (let [face->color {:front :a
                     :back :b
                     :right :c
                     :left :d
                     :up :e
                     :down :f}]
    (update-vals face->color make-face)))


(def solved (init-cube))

(defn- rotate-clockwise
  [face]
  (let [fst (into [] (reverse (map first face)))
        snd (into [] (reverse (map second face)))
        lst (into [] (reverse (map last face)))]
    [fst snd lst]))



(defn apply-F 
  [{:keys [front right left up down] :as cb}]
  (let [[f s t]    (nth up 2)
        n-right    (-> right
                       (assoc-in [0 0] f)
                       (assoc-in [1 0] s)
                       (assoc-in [2 0] t))
        n-fst-down (into [] (reverse (map first right)))
        n-down     (assoc down 0 n-fst-down)
        [f s t]    (first down)
        n-left     (-> left
                       (assoc-in [0 2] f)
                       (assoc-in [1 2] s)
                       (assoc-in [2 2] t))
        n-lst-up   (into [] (reverse (map last left)))
        n-up       (assoc up 2 n-lst-up)]
    (-> cb
        #_(update :front rotate-clockwise)
        (assoc :front (rotate-clockwise front))
        (assoc :right n-right)
        (assoc :down n-down)
        (assoc :left n-left)
        (assoc :up n-up))))



(defn apply-U
  [{:keys [front right left up back] :as cb}]
  (let [n-right (assoc right 0 (nth back 0))
        n-left  (assoc left 0  (nth front 0))
        n-front (assoc front 0 (nth right 0))
        n-back  (assoc back 0  (nth left 0))]
    (-> cb
        (assoc :up (rotate-clockwise up))
        (assoc :front n-front)
        (assoc :left n-left)
        (assoc :right n-right)
        (assoc :back n-back))))


(defn apply-D
  [{:keys [front back left right down] :as cb}]
  (let [n-right (assoc right 2 (nth front 2))
        n-left  (assoc left 2  (nth back 2))
        n-front (assoc front 2 (nth left 2))
        n-back  (assoc back 2  (nth right 2))]
    (assoc cb  :down (rotate-clockwise down)
               :right n-right
               :left n-left
               :front n-front
               :back n-back)))


(defn apply-R
  [{:keys [front back right up down]
    :as   cb}]
  (let [move-last (fn [from to]
                    (let [[f s t] (map last from)]
                      (-> to
                          (assoc-in [0 2] f)
                          (assoc-in [1 2] s)
                          (assoc-in [2 2] t))))
        n-front   (move-last down front)
        n-up      (move-last front up)
        [f s t]   (map last up)
        n-back    (-> back
                      (assoc-in [0 0] t)
                      (assoc-in [1 0] s)
                      (assoc-in [2 0] f))
        [f s t]   (map first back)
        n-down    (-> down
                      (assoc-in [0 2] t)
                      (assoc-in [1 2] s)
                      (assoc-in [2 2] f))]
    (-> cb
        (assoc :right (rotate-clockwise right)
               :up n-up
               :front n-front
               :down n-down
               :back n-back))))



(defn apply-L
  [{:keys [left up front back down] :as cb}]
  (let [move-first (fn [from to]
                     (let [[f s t] (map first from)]
                       (-> to
                           (assoc-in [0 0] f)
                           (assoc-in [1 0] s)
                           (assoc-in [2 0] t))))
        n-front    (move-first up front)
        n-down     (move-first front down)
        [f s t]    (map first down)
        n-back     (-> back
                       (assoc-in [0 2] t)
                       (assoc-in [1 2] s)
                       (assoc-in [2 2] f))
        [f s t]    (map last back)
        n-up       (-> up
                       (assoc-in [0 0] t)
                       (assoc-in [1 0] s)
                       (assoc-in [2 0] f))]
    (-> cb
        (assoc :left (rotate-clockwise left)
               :up n-up
               :down n-down
               :back n-back
               :front n-front))))


(defn apply-B
  [{:keys [back up down left right] :as   cb}]
  (let [n-up    (assoc up 0 (mapv last right))
        [f s t] (first up)
        n-left  (-> left
                    (assoc-in [2 0] f)
                    (assoc-in [1 0] s)
                    (assoc-in [0 0] t))
        n-down  (assoc down 2 (mapv first left))
        [f s t] (last down)
        n-right (-> right
                    (assoc-in [2 2] f)
                    (assoc-in [1 2] s)
                    (assoc-in [0 2] t))]
    (-> cb
        (assoc :back (rotate-clockwise back)
               :up   n-up
               :left n-left
               :down n-down
               :right n-right))))


(defn- rot-cb-left
  [{:keys [front back left right up down] :as cb}]
   (-> cb
       (assoc :up (rotate-clockwise up)
              :down (nth (iterate rotate-clockwise down) 3)
              :front right
              :left front
              :back left 
              :right back)))


(defn- rot-cb-up
  [{:keys [front back left right up down] :as cb}]
   (-> cb
       (assoc :up front
              :front down
              :back (->> up
                         (map #(into [] (reverse %)))
                         reverse
                         (into []))
              :down (into [] (reverse (map #(into [] (reverse %)) back)))
              :right (rotate-clockwise right)
              :left (nth (iterate rotate-clockwise left) 3))))



;;rotate cube in clockwise direction
(defn rot-cb-clkwise
  [cb]
  (nth (iterate rot-cb-left (nth (iterate rot-cb-up (rot-cb-left cb)) 3)) 3))


(defn- apply-n
  [move-fn n]
  (fn [cb]
    (nth (iterate move-fn cb) n)))


(def apply-D' (apply-n apply-D 3))

(def apply-R' (apply-n apply-R 3))

(def apply-F' (apply-n apply-F 3))

(def apply-U' (apply-n apply-U 3))

(def apply-L' (apply-n apply-L 3))

(def apply-B' (apply-n apply-B 3))

(def rot-cb-right (apply-n rot-cb-left 3))

(def moves->movefn
  {:F  apply-F
   :F' apply-F'
   :U  apply-U
   :U' apply-U'
   :L  apply-L
   :L' apply-L'
   :D  apply-D
   :D' apply-D'
   :R  apply-R
   :R' apply-R'
   :B  apply-B
   :B' apply-B'
   :CL rot-cb-left
   :CR rot-cb-right
   :CU rot-cb-up
   :CW rot-cb-clkwise})



(defn apply-moves
  [cb moves]
  (reduce (fn [cube move]
            ((get moves->movefn move) cube)) 
          cb moves))

(defn apply-moves-on-conf
  [{:keys [end-cube description]
    :or {description "solved"}
    :as conf}
   moves
   desc]
  (let [final-cube (reduce (fn [cube move]
                             ((get moves->movefn move) cube))
                           end-cube moves)]
    {:start-cube (:start-cube conf)
     :end-cube final-cube
     :description (str description " + " desc)
     :child-confs [conf
                   {:start-cube end-cube
                    :end-cube final-cube
                    :description desc
                    :moves moves}]}))


