(ns solsort.minild70.minild70
  (:require-macros
   [cljs.core.async.macros :refer [go go-loop alt!]]
   [reagent.ratom :as ratom :refer  [reaction]])
  (:require
   [cljs.reader]
   [solsort.toolbox.appdb :refer [db db! db-async!]]
   [solsort.toolbox.ui :refer [input select]]
   [solsort.util
    :refer
    [<ajax <seq<! js-seq load-style! put!close!
     parse-json-or-nil log page-ready render dom->clj]]
   [reagent.core :as reagent :refer []]
   [clojure.string :as string :refer [replace split blank?]]
   [cljs.core.async :refer [>! <! chan put! take! timeout close! pipe]]
   ))

(load-style!
 {:body
  {:background :black}})
(db! [:path] [[3 9] [3 8] [3 7] [2 7] [2 8] [1 8] [1 7] [1 6] [2 6] [3 6] [4 6]])

(defn v- [[x0 y0] [x1 y1]] [(- x0 x1) (- y0 y1)])
(defn v+ [[x0 y0] [x1 y1]] [(+ x0 x1) (+ y0 y1)])
(defn vscale [k [x y]] [(* k x) (* k y)])
(defn img [name pos]
  [:img
   {:src (str "assets/" name ".png")
    :style
    {:position :absolute
    :left (* (first pos) 16)
    :top (* (second pos) 16)}}]
  )
(defn draw-arrow [pos type]
  (img (name type) pos))
(defn draw-route []
  (into [:div]
        (loop [acc []
               pos (first (db [:path]))
               next (rest (db [:path]))]
          (if (empty? next)
            acc
            (recur (conj acc (draw-arrow
                              (v+ pos (vscale 0.5 (v- (first next) pos)))
                              ({[0 1] :down
                                [0 -1] :up
                                [1 0] :right
                                [-1 0] :left} (v- (first next) pos))))
                   (first next)
                   (rest next)))))
  )
(defn enemies []
  (into [:div] (map #(img "enemy0" %)
        [[1 3] [2 6] [0 4] [2 4] [4 1] [6 2]]
        ))
  )

(defn move-towards [pos]
  (while (not= pos (last (db [:path])))
      (let [path (db [:path] [[3 7]])
         prev-pos (last path)
         [px py] prev-pos
         [dx dy] (v- pos prev-pos)
         new-pos (if (< (js/Math.abs dy) (js/Math.abs dx))
                   [(+ (js/Math.sign dx) px) py]
                   [px (+ (js/Math.sign dy) py)]
                   )
         dup-idx (.indexOf path new-pos)
         path (if (= -1 dup-idx) path
                  (subvec path 0 dup-idx))
         path (conj path new-pos)
         ]
     (db! [:path] path)
                                        ; (db-async! [:pos] new-pos)
     ))
  )
(defn add-route [pos]
  (move-towards pos)
 ; (db! [:pos] pos)
  )
(db! [:pos] [3 9])
(defn main []
  (let [scale-y (/ js/innerHeight 160)
        scale-x (min (* 1.2 scale-y) (/ js/innerWidth 112))
        handle-touch #(add-route
                                 [(bit-or 0 (/ (.-clientX %) scale-x 16))
                                  (bit-or 0 (/ (.-clientY %) scale-y 16))]
                                 )
        ]
   [:div
    {
     :on-touch-move #(handle-touch (aget (.-touches %) 0))
     :on-mouse-move handle-touch
     :style
     {:display :inline-block
      :margin 0
      :padding 0
      :height 159
      :width 111
      :overflow :hidden
      :transform
      (str "matrix("
                (apply str
                       (interpose
                  ","
                  [scale-x 0 0 scale-y ;0 0
                   (- (* 56 scale-x) 56)
                   (- (* 80 scale-y) 80)
                  ; (* 122 scale 0.25) (* 160 scale 0.25)
                   ]))
            ")")
      :background :gray
      }}
    [:div
     {
      :style {:display :inline-block
              :height 160
              :width 112
              :image-rendering "pixelated"
              ;:background-color :white
              :background-image "url(\"assets/background.png\")"}}
     [draw-route]
     (img "player" (db [:pos])) 
     (enemies)
     ]])
  )
(.indexOf ['foo 'bar 'baz] 'bar)
(render [:div [main]])
