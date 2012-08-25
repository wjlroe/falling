(ns caves.game
  (:require [goog.dom :as dom]
            [goog.Timer :as timer]
            [goog.events :as events]
            [goog.events.EventType :as event-type]
            [goog.events.KeyCodes :as key-codes]))

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

(defn new-falling-shape
  [width]
  (make-shape {:x (int (/ width 2)) :y 0}))

(defn piece-bottom
  [shape]
  (let [{:keys [origin block-height rotation blocks]} shape
        blocks (nth blocks rotation)
        max-y (apply max (map second blocks))]
    (.log js/console "max-y:" max-y "y origin:" (:y origin))
    (+ (:y origin) max-y block-height)))

;;; Refactor the shit out of this...soon!!!!

(defn shape-lowest-points
  [shape]
  (let [{:keys [origin block-height rotation blocks]} shape
        blocks (nth blocks rotation)
        ;; partition into identical x-values
        columns (group-by #(+ (:x origin) (first %)) blocks)
        max-y-values (map (fn [x coords]
                            [x (apply max (map #(+ (:y origin)
                                                   block-height
                                                   (second %))
                                               coords))])
                          columns)]
    max-y-values))

(defn shape-highest-points
  [shape]
  (let [{:keys [origin block-height rotation blocks]} shape
        blocks (nth blocks rotation)
        columns (group-by #(+ (:x origin) (first %)) blocks)
        min-y-values (map (fn [x coords]
                            [x (apply min (map #(+ (:y origin)
                                                   (second %))
                                               coords))])
                          columns)]
    min-y-values))

(defn shapes-highest-points
  [shapes]
  (map shape-highest-points shapes))

(defn shapes-highest-points-by-x
  [shapes]
  (let [by-x (group-by first (shapes-highest-points shapes))
        min-y-points (map (fn [x coords]
                            [x (apply min (map second coords))])
                          by-x)]
    ;; Turn the [[0 1] [1 4]] array into {0 1 1 4} for easy lookup
    (into {} min-y-points)))

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

(defn fill-rect
  [[surface] [x y width height] [r g b]]
  (set! (. surface -fillStyle) (str "rgb(" r "," g "," b ")"))
  (.fillRect surface x y width height))

(defn stroke-rect
  [[surface] [x y width height] line-width [r g b]]
  (set! (. surface -strokeStyle) (str "rgb(" r "," g "," b ")"))
  (set! (. surface -lineWidth) line-width)
  (.strokeRect surface x y width height))

(defn fill-circle
  [[surface] coords [r g b]]
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
      (doseq [[incx incy] blocks]
        (let [x (+ incx (:x origin))
              y (+ incy (:y origin))]
          (fill-rect surface [x y sq-width sq-height] [60 80 160])
          (stroke-rect surface [x y sq-width sq-height] 2 [0 0 0]))))))

(defn update-canvas
  [world surface]
  (let [falling-shapes (:falling world)
        fallen-shapes (:fallen world)
        [_ width height] surface]
    (fill-rect surface [0 0 width height] [10 10 10])
    (stroke-rect surface [0 0 width height] 2 [0 0 0])
    (doseq [piece falling-shapes]
      (draw-piece surface piece))
    (doseq [piece fallen-shapes]
      (draw-piece surface piece))))

(defn rotate-piece
  [piece]
  (update-in piece [:rotation] #(mod (inc %) (count (:blocks piece)))))

(defn rotate-pieces
  [pieces]
  (map rotate-piece pieces))

(defn shift-piece
  [shift piece]
  (update-in piece [:origin :x] #(shift % sq-width)))

(defn shift-pieces
  [shift pieces]
  (map (partial shift-piece shift) pieces))

(defn fallen-piece?
  [total-height piece]
  (= total-height (piece-bottom piece)))

(defn collision?
  [fallen piece]
  (let [bottom-points (shape-lowest-points piece)
        fallen-top-points (shapes-highest-points-by-x fallen)
        collision (some true?
                        (for [[x y] bottom-points]
                          (= y (x fallen-top-points))))]
    (.log js/console "collision:" collision "bottom-points:" (pr-str bottom-points) "fallen-top-points:" (pr-str fallen-top-points))
    collision))

(defn hit-something?
  [total-height fallen piece]
  (if (or (fallen-piece? total-height piece)
          (collision? fallen piece))
    :fallen
    :falling))

(defn move-pieces-down
  [state [_ width height]]
  (let [{:keys [falling fallen]} state
        ;; Group by whether the piece has hit something or not:
        ;; {true, [piece, ..], false, [piece,..]}
        hit-or-not (group-by (partial hit-something? height fallen) falling)
        fallen (concat fallen
                       (:fallen hit-or-not))
        falling (:falling hit-or-not)
        ]
    (do
      (.log js/console "hit-or-not:" hit-or-not "fallen:" fallen "falling:" falling "height:" height)
      (assoc state
        :falling (if (not-empty falling)
                   (map shift-down falling)
                   [(new-falling-shape width)])
        :fallen (or fallen [])))))

(defn game
  [timer state surface]
  (let [[_ width height] surface]
    (swap! state (fn [curr]
                   (update-canvas curr surface)
                   (-> curr
                       (move-pieces-down surface)
                       ;; More shit
                       )))))

(defn click
  [timer state surface event]
  (if (not (.-enabled timer))
    (. timer (start))
    (. timer (stop))))

(defn rotate-falling-pieces
  [state]
  (swap! state (fn [curr]
                 (update-in curr [:falling] rotate-pieces))))

(defn shift-falling-left
  [state]
  (swap! state (fn [curr]
                 (update-in curr [:falling] (partial shift-pieces -)))))

(defn shift-falling-right
  [state]
  (swap! state (fn [curr]
                 (update-in curr [:falling] (partial shift-pieces +)))))

(defn keypress
  [state e]
  (let [browser-event (.getBrowserEvent e)]
   (do
     (.log js/console "event:" e)
     (.log js/console "br event:" browser-event)
     (cond (= (.-charCode browser-event) 97) ;; character 'a', lower case
           (shift-falling-left state)
           (= (.-charCode browser-event) 100) ;; character 'd', lower
           (shift-falling-right state)
           true
           (rotate-falling-pieces state)))))

(defn ^:export main
  []
  (let [surface (surface)
        [_ width _] surface
        timer (goog.Timer. 500)
        state (atom {:falling [(new-falling-shape width)]})]
    (update-canvas @state surface)
    (. timer (start))
    (events/listen timer goog.Timer/TICK #(game timer state surface))
    (events/listen js/window event-type/KEYPRESS #(keypress state %))
    (events/listen js/window event-type/TOUCHSTART #(keypress state %))
    (events/listen js/window event-type/CLICK #(click timer state surface %))))
