;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch5.8ex81-82) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; A Time is a structure:
; (make-time Number Number Number)
; (make-time h m s) represents
; a point in time since midnight
; at h hours,
; m minutes,
; s seconds.
(define-struct time [hour minute second])
(define ex1 (make-time 12 30 2))
(define ex2 (make-time 0 12 2))
(define ex3 (make-time 0 0 30))

; Time (Structure) -> Seconds (Number)
; Produces number of seconds that have past since midnight from Time (t)
;(check-expect (time->seconds ex1) 45002)
;(check-expect (time->seconds ex2) 722)
;(check-expect (time->seconds ex3) 30)

(define (time->seconds t)
  (+ (* 3600 (time-hour t)) (* 60 (time-minute t)) (time-second t)))

; A word is a structure:
; (make-word 1String 1String 1String)
; (make-word letter1 letter2 letter3)
; Letters are one of:
; 1String or
; #false boolean

(define-struct word [letter1 letter2 letter3])
(define ex4 (make-word "h" "o" "w"))
(define ex5 (make-word "w" "o" "w"))

; Word Word (structure) -> Word (structure)
; Consumes two words and produces a word that indicates where the given
; ones agree and disagree. Retains the letters that agree, otherwise it places
; #false in the field of the resulting word
 (check-expect (compare-word ex4 ex5) (make-word #false "o" "w"))
(define (compare-word w1 w2)
  (make-word [compare-letter (word-letter1 w1)(word-letter2 w2)]
             [compare-letter (word-letter2 w1)(word-letter2 w2)]
             [compare-letter (word-letter3 w1)(word-letter3 w2)]))

; 1String 1String -> 1String or Boolean
; Helper method to compare two letters, returns letter if they are equal
; #false if otherwise
(check-expect (compare-letter "w" "w") "w")
(check-expect (compare-letter "w" "h") #false)
(define (compare-letter l1 l2)
  (if (equal? l1 l2) l1 #false))