(ns solsort.minild70.minild70
  (:require-macros
   [cljs.core.async.macros :refer [go go-loop alt!]]
   [reagent.ratom :as ratom :refer  [reaction]])
  (:require
   [cljs.reader]
   [solsort.toolbox.setup]
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

(def maps
  [["   x   "
    "  v   <"
    "       "
    "  bb   "
    "  b    "
    "bbb>   "
    "       "
    "  bbbb "
    "     b "
    "   o   "
    ]])
(defn load-map [n]
  (let [level (atom [])]
  (doall
   (for [y (range 10)
         x (range 7)
         symb (aget (nth (nth maps n) y) x)]
     (let [add #(swap! level conj (into % {:id [x y] :pos [x y]}))]
       (log 'here x y symb)
      (case symb
        "b" (add {:type :block})
        ">" (add {:type :enemy0 :direction :right})
        "<" (add {:type :enemy0 :direction :left})
        "^" (add {:type :enemy0 :direction :up})
        "v" (add {:type :enemy0 :direction :down})
        "o" (add {:type :player})
        "x" (add {:type :goal})
        nil
        ))))
  (db! [:level] @level)
  ))
(load-map 0)
(log (db [:level]))
(load-style!
 {:body
  {:background :black}})
(defn player-pos []
  (:pos (first (filter #(= :player (:type %)) (db [:level])))))
(db! [:path] [(player-pos)])

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

(defn move-towards [pos]
  (loop []
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
     (when
         (not (some #{new-pos}
                    (map
                     :pos
                     (filter #(= :block (:type %)) (db [:level])))))
       (db! [:path] path)
       (when (not= pos (last (db [:path])))
         (recur))))))

(defn main []
  (let [scale-y (/ js/innerHeight 160)
        scale-x (min (* 1.2 scale-y) (/ js/innerWidth 112))
        handle-touch #(move-towards
                       [(min 7 (bit-or 0 (/ (.-clientX %) scale-x 16)))
                                  (min 10 (bit-or 0 (/ (.-clientY %) scale-y 16)))])]
   [:div
    {:on-touch-move #(handle-touch (aget (.-touches %) 0))
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
     (into [:div]
           (for [obj (db [:level])]
             (img (name (:type obj)) (:pos obj))))]]))
(render [:div [main]])
