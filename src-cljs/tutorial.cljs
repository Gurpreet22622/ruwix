(ns tutorial
  (:require [clojure.string :as str]
            [reagent.core :as r]))



(def clk-count (r/atom -4))

(defn counting-button
  []
  [:div
   "The atom " [:code " click-count "] "is " @clk-count " ."
   [:input {:type "button"
            :value "click here!"
            :on-click #(swap! clk-count inc)}]])


(defn timer
  []
  (let [time-elapsed (r/atom 0)]
    (fn []
      (js/setTimeout #(swap! time-elapsed inc) 1000)
      [:p "time elapsed is " @time-elapsed " seconds."])))

(defn changing-state
  []
  (let [*box (r/atom "i am here!")]
    (fn []
      [:div
       [:p "The value changed is " @*box]
       [:p "change it here " [:input {:type "text"
                                      :value @*box
                                      :on-change (fn [evt]
                                                   #_(js/console.log evt)
                                                   #_(js/console.log (-> evt .-target .-value))
                                                   ;; get the text value from the react event 
                                                   (reset! *box (-> evt .-target .-value)))}]]])))


(defn lister
  [items]
  [:ul
   (for [item items]
     ^{:key item} [:li "item " item])])
(defn lister-user
  []
  [:div 
   [:p "Here is the list"]
   [lister (range 4)]])




(defn calc-bmi 
  [{:keys [height weight bmi] :as data}]
  (let [h (/ height 100)]
    (if (nil? bmi)
      (assoc data :bmi (/ weight (* h h)))
      (assoc data :weight (* bmi h h)))))


(def bmi-data (r/atom (calc-bmi {:height 184 :weight 80})))

(defn slider
  [param value min max invalidates]
  [:input {:type "range"
           :value value
           :min min
           :max max
           :style {:width "100%"}
           :on-change (fn [e]
                        (let [new-value (js/parseInt (.. e -target -value))]
                          (swap! bmi-data 
                                 (fn [data]
                                   (-> data
                                       (assoc param new-value)
                                       (dissoc invalidates)
                                       calc-bmi)))))}])


(defn bmi-component []
  (let [{:keys [weight height bmi]} @bmi-data
        [color condition] (cond
                          (< bmi 18.5) ["orange" "underweight"]
                          (< bmi 25) ["inherit" "normal"]
                          (< bmi 30) ["orange" "overweight"]
                          :else ["red" "obese"])]
    [:div
     [:h3 "BMI calculator"]
     [:div
      "Height: " (int height) "cm"
      [slider :height height 100 220 :bmi]]
     [:div
      "Weight: " (int weight) "kg"
      [slider :weight weight 30 150 :bmi]]
     [:div
      "BMI: " (int bmi) " "
      [slider :bmi bmi 10 50 :weight]
      [:span {:style {:color color}} condition]]]))


;; (defn render-simple []
;;   (rdom/render
;;    [simple-component]
;;    (.-body js/document)))









(defn clock
  [*time-color]
  (let [*current-time (r/atom (js/Date.))]
    (js/setInterval #(reset! *current-time (js/Date.)) 1000)
    [:div
     [:h1 [:span {:style {:color "green"}} "hello, The current time is"]]
     [:h1 [:span {:style {:color @*time-color}} (-> @*current-time .toTimeString (str/split " ") first)]]]))


(defn input-color
  []
  (let [*time-color (r/atom "red")]
    (fn []
      [:div
       [clock *time-color]
       [:p "Enter the color:  "
        [:input {:type "text"
                 :value @*time-color
                 :on-change (fn [evt]
                              (reset! *time-color (-> evt .-target .-value)))}]]])))




(defn simple-component
  [name]
  [:div
   [:p "I am a component!"]
   [:p "my name is : " [:span {:style {:color "green"}} name]]
   [:p.someclass
    "I have " [:strong "bold"]
    [:span {:style {:color "red"}} " and red "] "text."]
   [lister-user]
   [counting-button]
   [timer]
   [changing-state]
   [bmi-component]
   [input-color]
   [timer]])

(defn main
  []
  [:div 
   [:p "I include simple component"]
   [simple-component "gurpreet singh"]]
  )

(comment
  (main)
  )
