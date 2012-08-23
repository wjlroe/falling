(ns caves.game
  (:require [goog.dom :as dom]
            [goog.Timer :as timer]
            [goog.events :as events]
            [goog.events.EventType :as event-type]))

(def sq-width 30)
(def sq-height 30)

(defn grid-to-dimentions
  "Take grid coords, [0 0], [0 2] and make coords out of them for the squares"
  [[nx ny]]
  [(* sq-width nx)
   (* sq-height ny)])

(defn rotations-to-dimentions
  [rotations]
  (map grid-to-dimentions rotations))

(def shapes
  {:o
   (map rotations-to-dimentions
        [[[0 0] [1 1] [0 1] [1 0]]])
   :j
   (map rotations-to-dimentions
        [[[0 2] [1 0] [1 1] [1 2]]
         [[0 0] [0 1] [1 1] [2 1]]
         [[0 0] [0 1] [0 2] [1 0]]
         [[0 0] [1 0] [2 0] [2 1]]])
   :i
   (map rotations-to-dimentions
        [[[0 0] [0 1] [0 2] [0 3]]
         [[0 0] [1 0] [2 0] [3 0]]])
   :l
   (map rotations-to-dimentions
        [[[0 0] [0 1] [0 2] [1 2]]
         [[0 0] [1 0] [2 0] [0 1]]
         [[0 0] [1 0] [1 1] [1 2]]
         [[0 1] [1 1] [2 1] [2 0]]])
   :s
   (map rotations-to-dimentions
        [[[0 1] [1 0] [1 1] [2 0]]
         [[0 0] [0 1] [1 1] [1 2]]])
   :z
   (map rotations-to-dimentions
        [[[0 0] [1 0] [1 1] [2 1]]
         [[1 0] [1 1] [0 1] [0 2]]])
   :t
   (map rotations-to-dimentions
        [[[0 0] [1 0] [1 1] [2 0]]
         [[0 1] [1 0] [1 1] [1 2]]
         [[0 1] [1 0] [1 1] [2 1]]
         [[0 0] [0 1] [0 2] [1 1]]])})

(defn get-shape
  "Get the co-ordinates of a shape. Either returns the required shape or a random shape."
  ([]
     (get-shape (rand-nth (keys shapes))))
  ([shape-name]
     (get shapes shape-name)))

(defn make-shape
  "Make a shape. Either return the shape required or a random one."
  [origin & shape-name]
  {:origin origin
   :block-width sq-width
   :block-height sq-height
   :rotation 0
   :blocks
   (apply get-shape shape-name)})

(defn shift-down
  [shape]
  (assoc shape :origin (assoc (:origin shape) :y
                              (+ sq-height
                                 (:y (:origin shape))))))

(defn surface
  []
  (let [surface (dom/getElement "surface")]
    [(.getContext surface "2d")
     (. surface -width)
     (. surface -height)]))

(defn fill-rect [[surface] [x y width height] [r g b]]
  (set! (. surface -fillStyle) (str "rgb(" r "," g "," b ")"))
  (.fillRect surface x y width height))

(defn stroke-rect [[surface] [x y width height] line-width [r g b]]
  (set! (. surface -strokeStyle) (str "rgb(" r "," g "," b ")"))
  (set! (. surface -lineWidth) line-width)
  (.strokeRect surface x y width height))

(defn fill-circle [[surface] coords [r g b]]
  (let [[x y d] coords]
    (set! (. surface -fillStyle) (str "rgb(" r "," g "," b ")"))
    (. surface (beginPath))
    (.arc surface x y d 0 (* 2 Math/PI) true)
    (. surface (closePath))
    (. surface (fill))))

(defn draw-piece
  [surface piece]
  (let [{:keys [origin blocks rotation]} piece]
    (let [blocks (nth blocks rotation)]
      (.log js/console (format "rotation: %s blocks: %s" rotation blocks))
      (doseq [[incx incy] blocks]
        (let [x (+ incx (:x origin))
              y (+ incy (:y origin))]
          (fill-rect surface [x y sq-width sq-height] [60 80 160])
          (stroke-rect surface [x y sq-width sq-height] 2 [0 0 0]))))))

(defn update-canvas
  [world surface]
  (let [falling-shapes (:falling world)
        [_ width height] surface]
    (fill-rect surface [0 0 width height] [10 10 10])
    (stroke-rect surface [0 0 width height] 2 [0 0 0])
    (doseq [piece falling-shapes]
      (draw-piece surface piece))))

(defn rotate-piece
  [piece]
  (update-in piece [:rotation] #(mod (inc %) (count (:blocks piece)))))

(defn rotate-pieces
  [pieces]
  (map rotate-piece pieces))

(defn move-pieces-down
  [state [_ width height]]
  (let [{:keys [falling]} state]
    (assoc state :falling (map shift-down falling))))

(defn game
  [timer state surface]
  (let [[_ width height] surface]
    (swap! state (fn [curr]
                   (update-canvas curr surface)
                   (-> curr
                       (move-pieces-down surface)
                       ;; More shit
                       )))))

(defn click [timer state surface event]
  (if (not (.-enabled timer))
    (. timer (start))
    (. timer (stop))))

(defn keypress
  [state]
  (swap! state (fn [curr]
                 (update-in curr [:falling] rotate-pieces))))

(defn ^:export main
  []
  (let [surface (surface)
        timer (goog.Timer. 500)
        state (atom {:falling [(make-shape {:x 30 :y 0})]})]
    (update-canvas @state surface)
    (events/listen timer goog.Timer/TICK #(game timer state surface))
    (events/listen js/window event-type/KEYPRESS #(keypress state))
    (events/listen js/window event-type/CLICK #(click timer state surface %))))
