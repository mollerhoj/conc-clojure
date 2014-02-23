(in-ns 'worms) ;Create and enter the 'worms' namespace
(clojure.core/use 'clojure.core) ;Let clojure standard functionality be availible
;in this new namespace

; Helpers
(defn sleep [] (Thread/sleep 4000))

(.addShutdownHook (Runtime/getRuntime) (Thread. (fn [] (println "Thread died."))))

;World
(defstruct cell :worm)
(def width 4)
(def height 4)

(def running true)

(defn make_row
  [width]
  (apply vector (map (fn [_] (ref (struct cell 0))) (range width))))

(defn make_grid
  [width height]
  (apply vector (map (fn [_] (make_row width)) (range height))))

(def world (make_grid width height))

(defn place [[x y]]
  (-> world (nth x) (nth y)))

(defn place_meeting [loc] 
  (:worm @(place loc)))

(defn place_free [loc] 
  (= (place_meeting loc) 0))

;Worm

(defn random-move
  "random move. Only works for square fields"
  [loc mini maxi]
  (let [moves [[1 0] [-1 0] [0 1] [0 -1]]
        r (nth moves (rand-int 4))]
    (map-indexed (fn [i,x] (min maxi (max mini (+ x (nth r i))))) loc))
  )

(defn behave
  "worm behavior"
  [[id loc]]
  (dosync
    (let [newloc (random-move loc 0 (- width 1))]
    (. Thread (sleep 200))
    (when running
      (send-off *agent* #'behave)) ;see comments below
    (if (place_free newloc)
    (do
      (alter (place loc) assoc :worm 0)
      (alter (place newloc) assoc :worm id)
      [id newloc])
    [id loc])
    )))

;Transactions should not have side effects, because Clojure may retry a
;transaction an arbitrary number of times. However, sometimes you want a side
;effect when a transaction succeeds. Agents provide a solution. If you send an
;action to an agent from within a transaction, that action will be sent exactly
;once, if and only if the transaction succeeds. (Programming Clojure, p. 125) 

; #' is a reader macro that expands to (var foo). What you're doing here is not
; passing around unevaluated functions, you're passing around vars which
; contain functions. The reason this works the way it does is because vars are
; functions that look up their contained value and call it.
; You will almost never see the var form directly in Clojure code. Instead, you
; will see the equivalent reader macro #', which also returns the var for a
; symbol.

; *agent*: The agent currently running an action on this thread, else nil.

; Render

(defn draw_worms [x y]
  (if (place_free [x y])
    (pr '_)
    (print (char (+ 64 (place_meeting [x y]))))
  )
)

(defn draw_col [x y]
  (draw_worms x y)
  (when (> y 0)
    (recur x (- y 1))))

(defn draw_rows [x y]
  (draw_col x y)
  (prn)
  (when (> x 0)
    (recur (- x 1) y)))

(defn draw [x y]
  (draw_rows x y))

(defn clear-screen
  "Clear screen using ctrl chars"
  []
  (let [esc (char 27)]
    (print (str esc "[2J"))     ; ANSI: clear screen
    (print (str esc "[;H"))))   ; ANSI: move cursor to top left corner of screen


; Display
(defn display [x]
  "display simulation for x milisecs"
  (clear-screen)
  (draw (- width 1) (- height 1)) 
  (. Thread (sleep 100))
  (when (> x 1) (recur (- x 10))))

; Start
(defn send-worm
  "Adds a worm with specified id to the world"
  [id]
  (send-off (agent [id [0 0]]) behave))

(dorun (for [x (range 16)] (send-worm (+ x 1))))
(display 3000)
