;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch4.5ex57) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; An LRCD (for launching rocket countdown) is one of:
; – "resting"
; – a Number between -3 and -1
; – a NonnegativeNumber 
; interpretation: a grounded rocket, in countdown mode,
; a number denotes the number of pixels between the
; grounded rocket and the rockets center

(define HEIGHT 300) ; distances in pixels 
(define WIDTH  100)
(define YDELTA 3)
 
(define BACKG  (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))
 
(define CENTER (/ (image-height ROCKET) 2))
(define ROCKET-GROUND (- HEIGHT CENTER))

; LRCD -> Image
; Helper method to draw rocket state with constant x and scene variables
;(check-expect (draw-rocket 50)
;              (place-image ROCKET 10 50 BACKG))
(define (draw-rocket x)
  (place-image ROCKET 10 x BACKG))


; LRCD -> Image
; renders the state as a resting or flying rocket 
(define (show x)
  (cond
    [(string? x)
     (draw-rocket ROCKET-GROUND)]
    [(<= -3 x -1)
     (place-image (text (number->string x) 20 "red")
                  10 (* 3/4 WIDTH)
                  (draw-rocket ROCKET-GROUND))]
    [(<= x (+ HEIGHT CENTER))
     (draw-rocket (- ROCKET-GROUND x))]))


 
; LRCD KeyEvent -> LRCD
; starts the countdown when space bar is pressed, 
; if the rocket is still resting 
(define (launch x ke)
  (cond
    [(string? x) (if (string=? " " ke) -3 x)]
    [(<= -3 x -1) x]
    [(>= x 0) x]))
 
; LRCD -> LRCD
; raises the rocket by YDELTA if it is moving already 
(define (fly x)
  (cond
    [(string? x) x]
    [(<= -3 x -1) (if (= x -1) 0 (+ x 1))]
    [(>= x 0) (+ x YDELTA)]))

; LRCD -> LRCD
; Stops animation when rocket has left canvas
(define (off-screen x)
  (if (number? x)(>= x (+ ROCKET-GROUND CENTER)) #false))

; LRCD -> LRCD
(define (main1 s)
  (big-bang s
    [to-draw show]
    [on-tick fly 0.3]
    [stop-when off-screen]
    [on-key launch]))
