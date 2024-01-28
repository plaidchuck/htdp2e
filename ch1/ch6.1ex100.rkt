;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch6.1ex100) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define TANK
  (overlay/align "middle" "bottom" (rectangle 5 20 "solid" "grey")
                 (rectangle 15 10 "solid" "grey")))
(define UFO
  (overlay (circle 6 "solid" "green") (rectangle 30 5 "solid" "green")))
(define MISSILE
  (triangle 8 "solid" "red"))
(define MOON
  (circle 12 "solid" "white"))
(define BACKG
  (place-image MOON 150 (image-height MOON) (empty-scene 200 200 "black")))
(define UFO-WIDTH
  (image-width UFO))
(define UFO-HEIGHT
  (image-height UFO))
(define TANK-WIDTH
  (image-width TANK))
(define TANK-HEIGHT
  (image-height TANK))
(define HEIGHT
  (image-height BACKG))
(define WIDTH
  (image-width BACKG))
(define VEHICLE-SPEED
  1)
(define MISSILE-SPEED
  (* 3 VEHICLE-SPEED))
(define TANK-SPEED
  3)

; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a 
; space invader game
(define-struct aim [ufo tank])
(define-struct fired [ufo tank missile])

; A UFO is a Posn. 
; interpretation (make-posn x y) is the UFO's location 
; (using the top-down, left-to-right convention)
 
(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number). 
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT) and the tank's speed: dx pixels/tick 
 
; A Missile is a Posn. 
; interpretation (make-posn x y) is the missile's place

; A SIGS is one of: 
; – (make-aim UFO Tank)
; – (make-fired UFO Tank Missile)
; interpretation represents the complete state of a 
; space invader game

; SIGS -> Image
; adds TANK, UFO, and possibly MISSILE to 
; the BACKGROUND scene
; SIGS -> Image
; renders the given game state on top of BACKGROUND
(check-expect (si-render aim-s2)(tank-render (aim-tank aim-s2)
                                             (ufo-render (aim-ufo aim-s2) BACKG)))
(check-expect (si-render fired-s2)(tank-render (fired-tank fired-s2)
                                               (ufo-render (fired-ufo fired-s2)
                                               (missile-render (fired-missile fired-s2)
                                                              BACKG))))
(define (si-render s)
  (cond
    [(aim? s)
     (tank-render (aim-tank s)
                  (ufo-render (aim-ufo s) BACKG))]
    [(fired? s)
     (tank-render
       (fired-tank s)
       (ufo-render (fired-ufo s)
                   (missile-render (fired-missile s)
                                   BACKG)))]))

; SIGS -> SIGS Number
; Consumes SIGS and and produces SIGS and a random number derived from the
; UFO width which are then sent to the si-move-proper function.
(check-random (si-move aim-s2)(si-move-proper aim-s2 (ufo-random 5)))
(define (si-move s)
  (si-move-proper s (ufo-random 5)))

; SIGS Number -> SIGS
; Consumes SIGS and a number and produces for each tick a UFO, Tank, and Missle where the UFO and tank move at
; the vehicle speed but the UFO can move left or right by the randomly generated
; delta number and the missile moves at twice the vehicle speeds.
(define (si-move-proper s delta)
  (cond
    [(aim? s)
     (make-aim (make-posn (+ (posn-x (aim-ufo s)) delta)
                          (+ (posn-y (aim-ufo s)) VEHICLE-SPEED))
               (make-tank (+ (tank-loc (aim-tank s)) (tank-vel (aim-tank s)))
                          (tank-vel (aim-tank s))))]
    [(fired? s)
     (make-fired (make-posn (+ (posn-x (fired-ufo s)) delta)
                           (+ (posn-y (fired-ufo s)) VEHICLE-SPEED))
                 (make-tank (+ (tank-loc (fired-tank s)) (tank-vel (fired-tank s)))
                            (tank-vel (fired-tank s)))
                 (make-posn (posn-x (fired-missile s))
                            (+ (posn-y (fired-missile s)) (* MISSILE-SPEED -1))))]))

; SIGS KeyEvent -> SIGS
; Consumes one of three key events and produces a SIGS which has the following results:
; "left" changes tank direction to the left
; "right" changes tank direction to the right
; "space" fires a missile one time only
(define (si-control s ke)
  (cond
    [(and (key=? "left" ke)(aim? s))
     (make-aim (make-posn (posn-x (aim-ufo s))(posn-y (aim-ufo s)))
               (make-tank (tank-loc (aim-tank s))(* TANK-SPEED -1)))]
    [(and (key=? "right" ke) (aim? s))
     (make-aim (make-posn (posn-x (aim-ufo s))(posn-y (aim-ufo s)))
               (make-tank (tank-loc (aim-tank s)) TANK-SPEED))]
    [(and (key=? "left" ke)(fired? s))
     (make-fired (make-posn (posn-x (fired-ufo s)) (posn-y (fired-ufo s)))
               (make-tank (tank-loc (fired-tank s))(* TANK-SPEED -1))
               (make-posn (posn-x (fired-missile s))(posn-y (fired-missile s))))]
    [(and (key=? "right" ke)(fired? s))
     (make-fired (make-posn (posn-x (fired-ufo s))(posn-y (fired-ufo s)))
               (make-tank (tank-loc (fired-tank s)) TANK-SPEED)
               (make-posn (posn-x (fired-missile s))(posn-y (fired-missile s))))]
    [(and (key=? " " ke) (aim? s))
     (make-fired (make-posn (posn-x (aim-ufo s))(posn-y (aim-ufo s)))
                 (make-tank (tank-loc (aim-tank s))(tank-vel (aim-tank s)))
                 (make-posn (tank-loc (aim-tank s))(- HEIGHT (/ TANK-HEIGHT 2))))]))

; Number -> Number
; Consumes a number and produces a randomly generated number from
; 0 to the consumed number. Also randomly generats a 0 or a 1 and makes the produced
; number negative if the result is 0 or positive otherwise.
(check-random (ufo-random UFO-HEIGHT) (* (random UFO-HEIGHT) (if (= (random 2) 0) -1 1)))
(define (ufo-random u-width)
  (* (random u-width) (if (= (random 2) 0) -1 1)))

; Tank Image -> Image 
; adds t to the given image im
(check-expect (tank-render (make-tank 50 3) (place-image UFO 100 100 BACKG))
              (place-image TANK 50 (- HEIGHT (/ TANK-HEIGHT 2))
                           (place-image UFO 100 100 BACKG)))
(define (tank-render t im)
               (cond
                 [(<= (tank-loc t)(/ TANK-WIDTH 2))(place-image TANK
                  (/ TANK-WIDTH 2)(- HEIGHT (/ TANK-HEIGHT 2)) im)]
                 [(>= (tank-loc t)(- WIDTH (/ TANK-WIDTH 2)))
                  (place-image TANK (- WIDTH (/ TANK-WIDTH 2))
                               (- HEIGHT (/ TANK-HEIGHT 2)) im)]
                 [else (place-image TANK (tank-loc t)(- HEIGHT (/ TANK-HEIGHT 2)) im)]))
  
  
 
; UFO Image -> Image 
; adds u to the given image im
(check-expect (ufo-render (make-posn 100 100) BACKG)
              (place-image UFO 100 100 BACKG))
(check-expect (ufo-render (make-posn 0 100) BACKG)
              (place-image UFO (/ UFO-WIDTH 2) 100 BACKG))
(check-expect (ufo-render (make-posn 300 100) BACKG)
              (place-image UFO (- WIDTH (/ UFO-WIDTH 2)) 100 BACKG))
(define (ufo-render u im)
  (cond
    [(<= (posn-x u)(/ UFO-WIDTH 2)) (place-image UFO (/ UFO-WIDTH 2)(posn-y u) im)]
    [(>= (posn-x u)(- WIDTH (/ UFO-WIDTH 2))) (place-image UFO (- WIDTH (/ UFO-WIDTH 2)) (posn-y u) im)]
    [else (place-image UFO (posn-x u)(posn-y u) im)]))
 

; Missile Image -> Image 
; adds m to the given image im
(check-expect (missile-render (make-posn 32 (- HEIGHT TANK-HEIGHT)) BACKG)
              (place-image MISSILE 32 (- HEIGHT TANK-HEIGHT) BACKG))
(define (missile-render m im)
  (place-image MISSILE (posn-x m)(posn-y m) im))

; SIGS -> Boolean
; Given s, determines if Fired Missle (make-posn) is within half the width/height
; from the center of the current Fired-UFO (make-posn) horizontally and vertically
; from below and returns true or false.
; Given s determine if Aim UFO and Fired UFO (make-posn) is within half the height
; from its center vertically to the bottom of the scene (HEIGHT) and returns
; true or false.
 (check-expect (si-game-over? fired-s1) #false)
 (check-expect (si-game-over? fired-s2) #true)
 (check-expect (si-game-over? fired-s3) #true)
 (check-expect (si-game-over? aim-s1) #false)
 (check-expect (si-game-over? aim-s2) #false)
 (check-expect (si-game-over? aim-s3) #true)
(define (si-game-over? s)
  (cond
    [(fired? s)
    (if (or (and (>= (posn-x (fired-missile s)) (- (posn-x (fired-ufo s)) (/ UFO-WIDTH 2)))
         (<= (posn-x (fired-missile s)) (+ (posn-x (fired-ufo s)) (/ UFO-WIDTH 2)))
         (<= (posn-y (fired-missile s)) (+ (posn-y (fired-ufo s)) (/ UFO-HEIGHT 2)))
         (>= (posn-y (fired-missile s)) (- (posn-y (fired-ufo s)) (/ UFO-HEIGHT 2))))
            (>= (posn-y (fired-ufo s)) (- HEIGHT (/ UFO-HEIGHT 2))))
     #true #false)]
    [(aim? s)
     (if (>= (posn-y (aim-ufo s)) (- HEIGHT (/ UFO-HEIGHT 2))) #true #false)]))

; SIGS -> Image
; Consumes SIGS and produces an image of the final state of the game
; after it ends.
(define (si-render-final s)
  (cond
    [(aim? s) (si-render s)]
    [(fired? s) (si-render s)]))
    

; Application and tests
; Missle miss, UFO not near ground, game continues
(define u1 (make-posn 80 55))
(define t1 (make-tank 50 3))
(define m1 (make-posn 50 55))
(define fired-s1 (make-fired u1 t1 m1))
(define aim-s1 (make-aim u1 t1))

; Missle hit, UFO not near ground, game ends
(define u2 (make-posn 80 55))
(define t2 (make-tank 50 3))
(define m2 (make-posn 80 49))
(define fired-s2 (make-fired u2 t2 m2))
(define aim-s2 (make-aim u2 t2))

; Missle miss, UFO on ground, game ends
(define u3 (make-posn 80 (- HEIGHT (/ UFO-HEIGHT 2))))
(define t3 (make-tank 90 3))
(define m3 (make-posn 60 55))
(define fired-s3 (make-fired u3 t3 m3))
(define aim-s3 (make-aim u3 t3))

; Missle miss, UFO not near ground and on right left edge, tank is on far right edge, game continues
(define u4 (make-posn 400 55))
(define t4 (make-tank WIDTH 3))
(define m4 (make-posn 60 100))
(define fired-s4 (make-fired u4 t4 m4))
(define aim-s4 (make-aim u4 t4))

; Standard game start with tank and UFO in the middle of the screen, and UFO at top of screen
(define u5 (make-posn (/ WIDTH 2) (/ UFO-HEIGHT 2)))
(define t5 (make-tank (/ WIDTH 2) 3))
(define m5 (make-posn (/ WIDTH 2) (- HEIGHT (/ TANK-WIDTH 2))))
(define fired-s5 (make-fired u5 t5 m5))
(define aim-s5 (make-aim u5 t5))

(define (si-main s)
  (big-bang s
    [on-tick si-move]
    [on-key si-control]
    [stop-when si-game-over? si-render-final]
    [to-draw si-render]))