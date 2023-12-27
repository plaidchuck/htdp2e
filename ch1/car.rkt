;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname car) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define WIDTH-OF-WORLD 200) ; total distance car can travel

(define WHEEL-RADIUS 5)
(define WHEEL-DISTANCE (* WHEEL-RADIUS 2))
(define WHEEL
  (circle WHEEL-RADIUS "solid" "black"))

(define SPACE
  (rectangle WHEEL-DISTANCE (/ WHEEL-RADIUS 2) "solid" "white"))
(define BOTH-WHEELS
  (beside WHEEL SPACE WHEEL))


(define BODY 
  (rectangle (+ WHEEL-DISTANCE (* WHEEL-RADIUS 6))
             (* WHEEL-RADIUS 2) "solid" "red"))

(define CAB
  (rectangle (+ WHEEL-DISTANCE
                (* WHEEL-RADIUS 2)) WHEEL-RADIUS "solid" "red"))


(define FRAME 
  (underlay/align/offset "middle" "top" BODY 0 WHEEL-RADIUS BOTH-WHEELS))

(define CAR (above CAB FRAME))

(define TREE
  (underlay/xy (circle 10 "solid" "green")
               9 15
               (rectangle 2 20 "solid" "brown")))

(define Y-CAR 15)

(define BACKGROUND
  (place-image TREE 100 Y-CAR
  (empty-scene WIDTH-OF-WORLD (* WHEEL-RADIUS 5))))

(define (car-pos x)
  (+ x (/ (image-width CAR) 2)))



; WorldState -> WorldState 
; moves the car by 3 pixels for every clock tick
; examples: 
(define (tock cw)
  (+ cw 3))

; WorldState -> Image
; places the car into the BACKGROUND scene,
; according to the given world state 
 (define (render cw)
   (place-image CAR (car-pos cw) Y-CAR BACKGROUND))

 
 (define (end cw)
   (> cw (+ WIDTH-OF-WORLD (image-width CAR))))


 ; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
   (big-bang ws
     [on-tick tock]
     [to-draw render]
     [stop-when end]))