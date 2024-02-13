;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch6.2ex109) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Expect is one of:
; – AA First expected state, represented by a yellow rectangle
; – BB Second expect state, also reprsented by a yellow rectangle
; – DD  Third expected state, represented by a green rectangle
; – ER Unexpected state if any incorrect key is entered during the state pattern,
; represented by a red rectangle

; Representations of each state and keypress
; – AA expect 'a'
; – BB expect 'b', 'c', 'd'
; – DD  finished
; – ER illegal key
; All other keypress is "incorrect" and considered an illegal key


; Data definitions using strings represented each states color
(define AA "white")
(define BB "yellow")
(define DD "green")
(define ER "red")

(define INIT AA)

(define WIDTH 100)
(define HEIGHT 100)

; String -> Image
; Consumes a String based on the state definitions and produces an image
; of the appropriately colored rectangle
(define (render expect)
  (rectangle WIDTH HEIGHT "solid" expect))

; Keypress String -> String
; Consumes a key press and produces a string based on what key is pressed
(check-expect (keypress BB "b") BB)
(check-expect (keypress BB "c") BB)
(check-expect (keypress BB "a") ER)
(check-expect (keypress BB "d") DD)
(check-expect (keypress DD "d") DD)
(define (keypress expect ke)
  (cond
  [(and (key=? "a" ke)(and (not (equal? ER expect)) (not (equal? BB expect))(not (equal? DD expect)))) BB]
  [(and (or (key=? "b" ke)(key=? "c" ke))(equal? BB expect)) BB]
  [(and (key=? "d" ke)(or (equal? BB expect) (equal? DD expect))) DD]
  [(string=? DD expect) DD]
  [(string=? ER expect) ER]
  [else ER]))

(define (fsm expect)
  (big-bang expect
    [on-key keypress]
    [to-draw render]))