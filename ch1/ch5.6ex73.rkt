;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch5.6ex73) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define MTS (empty-scene 100 100))
(define DOT (circle 3 "solid" "red"))
 
; A Posn represents the state of the world.
 
; Posn -> Posn 
(define (main p0)
  (big-bang p0
    [on-tick y+ .3]
    [on-mouse reset-dot]
    [to-draw scene+dot]))

; Posn -> Image
; adds a red spot to MTS at p
;(check-expect (scene+dot (make-posn 10 20))
;              (place-image DOT 10 20 MTS))
;(check-expect (scene+dot (make-posn 88 73))
;              (place-image DOT 88 73 MTS))

(define (scene+dot p)
  (place-image DOT (posn-x p) (posn-y p) MTS))

; Posn -> Posn
; Increases y by 3 with each clock tick
(check-expect (y+ (make-posn 10 0))
              (make-posn 10 3))
(check-expect (y+ (make-posn 10 20))
              (make-posn 10 23))

(define (y+ p)
   (posn-up-y p (+ (posn-y p) 3)))

; Helper function for x+ to take the posn and increment the y field by 3
;(check-expect (posn-up-y (make-posn 0 0) 5)
;              (make-posn 0 5))
(define (posn-up-y p n)
  (make-posn (posn-x p) n))

; Posn Number Number MouseEvebt -> Posn
; for mouse clicks, (make-posn x y), otherwise p
;(check-expect
;  (reset-dot (make-posn 10 20) 29 31 "button-down")
;  (make-posn 29 31))
;(check-expect
;  (reset-dot (make-posn 10 20) 29 31 "button-up")
;  (make-posn 10 20))

(define (reset-dot p x y me)
  (cond
    [(mouse=? me "button-down") (make-posn x y)]
    [else p]))