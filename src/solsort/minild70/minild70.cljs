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
   [cljs.core.async :refer [>! <! chan put! take! timeout close! pipe]]))

(def turn-time 100)
(db! [:current-level] 6)
(def maps
  [["       "
    "       "
    "     x "
    "       "
    "       "
    "       "
    "       "
    " o     "
    "       "
    "       "]
   ["   x   "
    "       "
    "      <"
    "      <"
    "      <"
    "      <"
    "      <"
    "      <"
    "       "
    "   o   "]
   ["   bbbx"
    " b b   "
    " b b b "
    " b   b "
    " bbbbbb"
    "       "
    "bbbbbb "
    "     b "
    " bbb b "
    "o  b   "]
   ["   x   "
    "      v"
    "      v"
    "       "
    "    <<<"
    "    bb "
    "    bb "
    "    b  "
    "    b  "
    "   ob  "]
   [" bbbbbb"
    "bb  xbb"
    "b     b"
    "b > < b"
    "b     b"
    "b > < b"
    "b bbb b"
    "b  o  b"
    "bb   bb"
    " bbbbb "]
    [" vv^b^x"
     "    b b"
     "    b  "
     "    b  "
     "       "
     "       "
     "     <<"
     ">>     "
     "       "
     "o      "]
   ["ov>   x"
    "    b b"
    " ^b   b"
    "      b"
    " vb b b"
    "     ^b"
    " vbbb  "
    "       "
    " vbbb  "
    "       "]
    ])
(defn goal-pos [] (:pos (first (filter #(= :goal (:type %)) (db [:level])))))
(defn player-pos [] (:pos (first (filter #(= :player (:type %)) (db [:level])))))
(defn load-map []
  (when (db [:win])
    (db! [:current-level] (inc (db [:current-level])))
    (db! [:path] nil)
    (db! [:win] false))
  (let [
        level
        (atom
         (into [] (concat
                    (for [x (range 7)] {:type :block :pos [x -1]})
                    (for [x (range 7)] {:type :block :pos [x 10]})
                    (for [y (range 10)] {:type :block :pos [-1 y]})
                    (for [y (range 10)] {:type :block :pos [7 y]})
                    )))]
    (doall
     (for [y (range 10)
           x (range 7)
           symb (aget (nth (nth maps (db [:current-level])) y) x)]
       (let [add #(swap! level conj (into % {:id [x y] :pos [x y]}))]
         (case symb
           "b" (add {:type :block})
           ">" (add {:type :glider-lr})
           "<" (add {:type :glider-lr :reverse true})
           "^" (add {:type :glider-ud})
           "v" (add {:type :glider-ud :reverse true})
           "o" (add {:type :player :player true})
           "x" (add {:type :goal})
           nil))))
    (db! [:level] @level)
    (db! [:path] [(player-pos)])))
(load-map)
(load-style!
 {:img
  {:transition-timing-function "linear"
   :transition (str "all " turn-time "ms")}
  :body
  {:background :black}})

(defn v- [[x0 y0] [x1 y1]] [(- x0 x1) (- y0 y1)])
(defn v+ [[x0 y0] [x1 y1]] [(+ x0 x1) (+ y0 y1)])
(defn vscale [k [x y]] [(* k x) (* k y)])
(defn img [name pos]
  [:img
   {:src (str "assets/" name ".png")
    :style
    {:position :absolute
     :left (* (first pos) 16)
     :top (* (second pos) 16)}}])
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
                   (rest next))))))

(declare move)
(defn move-towards [pos]
  (loop []
    (let [path (db [:path] [[3 7]])
          prev-pos (last path)
          [px py] prev-pos
          [dx dy] (v- pos prev-pos)
          new-pos (if (< (js/Math.abs dy) (js/Math.abs dx))
                    [(+ (js/Math.sign dx) px) py]
                    [px (+ (js/Math.sign dy) py)])
          dup-idx (.indexOf path new-pos)
          path (if (= -1 dup-idx) path
                   (subvec path 0 dup-idx))
          path (conj path new-pos)]
      (when
       (not (some #{new-pos}
                  (map
                   :pos
                   (filter #(= :block (:type %)) (db [:level])))))
        (db! [:path] path)
        (when (not= pos (last (db [:path])))
          (recur)))))
  (when (and (not (db [:moving]))
             (= (goal-pos) (last (db [:path]))))
    (db! [:moving] true)
    (move))
  )

(defn move-entity [o state]
  (let [collision (fn [p] (and (get state p)
                               (not= (:id (get state p)) (:id o))))]
   (case (:type o)
     :glider-ud 
     (let [new-pos
           (if (:reverse o)
             (v+ (:pos o) [0 -1])
             (v+ (:pos o) [0 1]))]
       (if (collision new-pos)
         (assoc o :reverse (not (:reverse o)))
         (assoc o :pos new-pos)
         )
       )
     :glider-lr 
     (let [new-pos
           (if (:reverse o)
             (v+ (:pos o) [-1 0])
             (v+ (:pos o) [1 0]))]
       (if (collision new-pos)
         (assoc o :reverse (not (:reverse o)))
         (assoc o :pos new-pos)
         )
       )
     :block o
     :goal o
     :player
     (let [time (get o :time 1)
           new-pos (nth (db [:path]) time (goal-pos))
           collision (get state new-pos)]
       (when collision
         (db! [:moving] false)
         (when (= :goal (:type collision))
           (db! [:win] true)))
       (when (< (count (db [:path]))
                (get o :time 1))
         (db! [:moving] false))
       (into o {:time (inc time) :pos new-pos})))))
(db! [:moving] false)
(defn move []
  (let [prev-state (db [:level])
        coords (into {} (for [o (remove :player prev-state)] [(:pos o) o]))
        next-state (for [o prev-state] (move-entity o coords))
        coords (into coords (for [o (remove :player next-state)] [(:pos o) o]))
        next-state (doall (for [o prev-state] (move-entity o coords)))]
    (when (db [:moving])
     (db! [:level] next-state))
    (if (db [:moving])
      (js/setTimeout move turn-time)
      (js/setTimeout #(load-map) (* 4 turn-time)))))

(defn main []
  (let [scale-y (/ js/innerHeight 160)
        scale-x (min (* 1.2 scale-y) (/ js/innerWidth 112))
        handle-touch #(when-not (db [:moving])(move-towards
                        [(min 7 (bit-or 0 (/ (.-clientX %) scale-x 16)))
                         (min 10 (bit-or 0 (/ (.-clientY %) scale-y 16)))]))]
    (if (<= (count maps) (db [:current-level]))
      [:div
       {:style
        {:display :inline-block
         :height js/window.innerHeight
         :width js/window.innerWidth
         :text-align "center"
         :position :absolute
         :background :white
                }}
       [:h1 {:style {:margin-top "20%"}} "You won!"]
       [:input {:type :submit
                :value "Play Again"
                :on-click
                #(do
                   (db! [:current-level] 0)
                   (load-map))}]
       ]

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
                     [scale-x 0 0 scale-y
                      (- (* 56 scale-x) 56)
                      (- (* 80 scale-y) 80)]))
             ")")
        :background :gray}}
      [:div
       {:style {:display :inline-block
                :height 160
                :width 112
                :image-rendering "pixelated"
                                        ;:background-color :white
                :background-image "url(\"assets/background.png\")"}}
       [draw-route]
       (into [:div]
             (for [obj (db [:level])]
               (img (name (:type obj)) (:pos obj))))]])))
(render [:div [main]])
