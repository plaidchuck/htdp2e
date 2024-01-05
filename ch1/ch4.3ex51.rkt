;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch4.3ex51) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; A TrafficLight is one of the following Strings:
; – "red"
; – "green"
; – "yellow"
; interpretation the three strings represent the three 
; possible states that a traffic light may assume

(define RADIUS 10)

; TrafficLight -> TrafficLight
; yields the next state given current state s
; (check-expect (traffic-light-next "red") "green")
; (check-expect (traffic-light-next "yellow") "red")
; (check-expect (traffic-light-next "green") "yellow")
(define (traffic-light-next s)
  (cond
    [(string=? "red" s) "green"]
    [(string=? "green" s) "yellow"]
    [(string=? "yellow" s) "red"]))

; Number -> TrafficLight
; yields a traffic light string from a tick in the main program
; (check-expect (traffic-light-tick "green") "yellow")
(define (traffic-light-tick tick)
  (traffic-light-next tick))

; TrafficLight -> Image
; renders traffic light as a colored circle based on the
; string of the TrafficLight
; (check-expect (traffic-light-on "red") (circle RADIUS "solid" "red"))
(define (traffic-light-on s)
  (circle RADIUS "solid" s))

; Takes TrafficLight String as initial state
(define (main ts)
  (big-bang ts
    [to-draw traffic-light-on]
    [on-tick traffic-light-tick 1]))