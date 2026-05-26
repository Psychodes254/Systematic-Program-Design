;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; Space Invaders
;; ========================
(require 2htdp/universe)
(require 2htdp/image)

;; Constants:
;; ====================
(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))


;; Data Definitions:
;; ====================
(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))


(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))


(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit I1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit I1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit I1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; Handlers
;; ========================================================================
;; handle-key : Game KeyEvent -> Game
;; interp. handle keyboard input to control tank movement and fire missiles
(define (handle-key g ke)
  (cond
    [(key=? ke "left") (game-set-tank-dir g -1)]
    [(key=? ke "right") (game-set-tank-dir g 1)]
    [(key=? ke " ") (game-fire-missile g)]  ;spacebar to fire
    [else g]))

;; game-set-tank-dir : Game Integer -> Game
;; interp. set the tank direction to move left (-1) or right (1)
(define (game-set-tank-dir g dir)
  (make-game (game-invaders g)
             (game-missiles g)
             (make-tank (tank-x (game-tank g)) dir)))

;; game-fire-missile : Game -> Game
;; interp. create a new missile at the tank's current position
(define (game-fire-missile g)
  (let ((t (game-tank g)))
    (make-game (game-invaders g)
               (cons (make-missile (tank-x t) (- HEIGHT TANK-HEIGHT/2)) (game-missiles g))
               t)))

;; handle-key-release : Game KeyEvent -> Game
;; interp. stop tank movement when a key is released
(define (handle-key-release g ke)
  (cond
    [(key=? ke "left") (game-stop-if-dir g -1)]
    [(key=? ke "right") (game-stop-if-dir g 1)]
    [else g]))

;; game-stop-if-dir : Game Integer -> Game
;; interp. stop tank movement if it was moving in the given direction
(define (game-stop-if-dir g dir)
  (let ((t (game-tank g)))
    (if (= (tank-dir t) dir)
        (make-game (game-invaders g)
                   (game-missiles g)
                   (make-tank (tank-x t) 0))
        g)))


;; Collision detection
(define (missile-hits-invader? m inv)
  (< (sqrt (+ (sqr (- (missile-x m) (invader-x inv)))
              (sqr (- (missile-y m) (invader-y inv)))))
     HIT-RANGE))


(define (remove-hit-invaders invaders missiles)
  (filter (lambda (inv)
            (not (ormap (lambda (m) (missile-hits-invader? m inv)) missiles)))
          invaders))


(define (remove-hit-missiles missiles invaders)
  (filter (lambda (m)
            (not (ormap (lambda (inv) (missile-hits-invader? m inv)) invaders)))
          missiles))


;; Invader spawning — use random + INVADE-RATE
(define (maybe-spawn-invader invaders)
  (if (= (random INVADE-RATE) 0)
      (cons (make-invader (random WIDTH) 0 INVADER-X-SPEED) invaders)
      invaders))


;; update-game : Game -> Game
;; interp. update all game entities (tank, invaders, missiles) each clock tick
(define (update-game g)
  (let* ((invaders (game-invaders g))
         (missiles (game-missiles g))
         (live-invaders (remove-hit-invaders invaders missiles))
         (live-missiles (remove-hit-missiles missiles invaders)))
  (make-game (maybe-spawn-invader (update-invaders live-invaders))
             (update-missiles live-missiles)
             (update-tank (game-tank g)))))


;; update-tank : Tank -> Tank
;; interp. move tank in its current direction, keeping it within bounds
(define (update-tank t)
  (let ((new-x (+ (tank-x t) (* TANK-SPEED (tank-dir t)))))
    (make-tank (clamp new-x 0 WIDTH) (tank-dir t))))


;; clamp : Number Number Number -> Number
;; interp. constrain value to be between min and max
(define (clamp val min-val max-val)
  (cond
    [(< val min-val) min-val]
    [(> val max-val) max-val]
    [else val]))


;; update-invaders : (listof Invader) -> (listof Invader)
;; interp. move all invaders and update their positions
(define (update-invaders invaders)
  (map update-invader invaders))


;; update-invader : Invader -> Invader
;; interp. move invader one step
(define (update-invader inv)
  (let* ((new-x (+ (invader-x inv) (invader-dx inv)))
         (new-dx (if (or (> new-x WIDTH) (< new-x 0))
                     (- (invader-dx inv))
                     (invader-dx inv))))
  (make-invader (+ (invader-x inv) new-dx)
                (+ (invader-y inv) INVADER-Y-SPEED)
                new-dx)))


;; update-missiles : (listof Missile) -> (listof Missile)
;; interp. move all missiles and remove those that go off-screen
(define (update-missiles missiles)
  (filter-map (lambda (m)
                (let ((new-m (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED))))
                  (if (>= (missile-y new-m) 0) new-m #f)))
              missiles))


;; filter-map : (X -> (or Y #f)) (listof X) -> (listof Y)
;; interp. map a function that returns #f to filter, collecting non-#f results
(define (filter-map f lst)
  (cond
    [(empty? lst) empty]
    [else (let ((result (f (first lst))))
            (if (false? result)
                (filter-map f (rest lst))
                (cons result (filter-map f (rest lst)))))]))


;; render-game : Game -> Image
;; interp. render the current game state
(define (render-game g)
  (place-image TANK (tank-x (game-tank g)) (- HEIGHT TANK-HEIGHT/2)
    (foldl (lambda (inv scene)
             (place-image INVADER (invader-x inv) (invader-y inv) scene))
           (foldl (lambda (m scene)
                    (place-image MISSILE (missile-x m) (missile-y m) scene))
                  BACKGROUND
                  (game-missiles g))
           (game-invaders g))))


;; game-over? : Game -> Boolean
;; interp. check if any invader has reached the bottom
(define (game-over? g)
  (ormap (lambda (inv) (>= (invader-y inv) HEIGHT)) (game-invaders g)))


;; main : Game -> Game
;; interp. run the game with event handlers and rendering
(define (main g)
  (big-bang g
    (on-tick update-game)
    (on-key handle-key)
    (on-release handle-key-release)
    (to-draw render-game)
    (stop-when game-over?)))

;; Start the game
;; (main G0)