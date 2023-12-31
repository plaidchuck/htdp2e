;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch3.6ex44) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; total length the car will drive
(define WIDTH-OF-WORLD 200)

; Size of wheels and distance between them (5 pixels per pixels of radius)
(define WHEEL-RADIUS 5)
(define WHEEL-DIAMETER (* WHEEL-RADIUS 2))
(define WHEEL-DISTANCE (* WHEEL-RADIUS 3))

(define WHEEL
  (circle WHEEL-RADIUS "solid" "black"))

; Dimensions of space between wheels
(define SPACE
  (rectangle WHEEL-DISTANCE WHEEL-RADIUS "solid" "white"))
; Image constant of two wheels and space between
(define BOTH-WHEELS
  (beside WHEEL SPACE WHEEL))

; Body of car above wheel space and wheels
(define FRAME
  (rectangle (+ (image-width BOTH-WHEELS) WHEEL-DIAMETER)
             WHEEL-DIAMETER "solid" "red"))
             
; Cab of car
(define CAB
  (rectangle (+ WHEEL-DIAMETER WHEEL-DISTANCE)
             WHEEL-RADIUS "solid" "red"))

; Car is an image formed by the frame, cab and wheels
(define CAR
   (underlay/offset (above CAB FRAME)
                   0 WHEEL-DIAMETER BOTH-WHEELS))

; Tree image to be added to background scene
(define TREE
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))

; Background scene defined by constant and
; height car plus one wheel radius size
(define BACKGROUND
  (place-image TREE (/ WIDTH-OF-WORLD 2)
               (- (image-height CAR) WHEEL-RADIUS)
  (empty-scene WIDTH-OF-WORLD
               (+ (image-height CAR) WHEEL-RADIUS))))


; Y Position of car in background scene
(define Y-CAR
  (- (image-height CAR) WHEEL-RADIUS))

; Relates clock-ticks to position of car in world
(define VELOCITY
  WHEEL-RADIUS )

; Number -> Number
; Converts ticks to the cars x position based
; on the Velocity constant and half the width of
; the car image
(define (car-position ticks)
  ticks)

; Number -> Number
; Converts ticks to the cars y position based on
; the sine wave formula y = Asin(2pi / x)
(define (car-y-position ticks)
  (- (image-height CAR) WHEEL-RADIUS
  (* 2 (sin (* (/ ticks 6) pi)))))

; WorldState Number Number String -> WorldState
; places the car at x-mouse
; if the given me is "button-down"
(check-expect (hyper 21 10 20 "enter") 21)
(check-expect (hyper 42 10 20 "button-down") 42)
(check-expect (hyper 42 10 20 "move") 10)
(define (hyper as x-mouse y-mouse me)
  (cond
    [(string=? "move" me) x-mouse]
    [else (car-position as)]))
  

; An AnimationState is a Number.
; interpretation: the number of clock ticks
; since the animation started

; AnimationState -> AnimationState
; launches the program from some initial state 
(define (main as)
  (big-bang as
    [on-tick tock]
    [on-mouse hyper]
    [to-draw render]
    [stop-when end?]))


; AnimationState -> Image
; places the image of the car x pixels from 
; the left margin of the BACKGROUND image
(define (render as)
  (place-image CAR (car-position as)
                      (car-y-position as) BACKGROUND))
 
; AnimationState -> AnimationState
; moves the car 1 pixel for each clock tick
; (check-expect (tock 20) 21)
; (check-expect (tock 78) 79)
(define (tock as)
  (+ as 3))

; WorldState -> Boolean
; Ends Animation when the car has left the
; BACKGROUND image
(define (end? as)
  (> (car-position as)
     (+ WIDTH-OF-WORLD (image-width CAR))))