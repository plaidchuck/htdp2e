;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch4.5ex56) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; An LRCD (for launching rocket countdown) is one of:
; – "resting"
; – a Number between -3 and -1
; – a NonnegativeNumber 
; interpretation a grounded rocket, in countdown mode,
; a number denotes the number of pixels between the
; top of the canvas and the rocket (its height)

(define HEIGHT 300) ; distances in pixels 
(define WIDTH  100)
(define YDELTA 3)
 
(define BACKG  (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))
 
(define CENTER (/ (image-height ROCKET) 2))

; LRCD -> Image
; renders the state as a resting or flying rocket 
;(check-expect
; (show "resting")
; (place-rocket (- HEIGHT CENTER)))
 
;(check-expect
; (show -2)
; (place-image (text (number->string -2) 20 "red")
;                  10 (* 3/4 WIDTH)
;                  (place-rocket (- HEIGHT CENTER))))
 
;(check-expect
; (show 53)
; (place-rocket (- 53 CENTER)))

(define (show x)
  (cond
    [(string? x)(place-rocket (- HEIGHT CENTER))]
    [(<= -3 x -1)
     (place-image (text (number->string x) 20 "red")
                  10 (* 3/4 WIDTH)
                  (place-rocket (- HEIGHT CENTER)))]
    [(>= x 0)
     (place-rocket(- x CENTER))]
     [else (place-rocket (- HEIGHT CENTER))]))


; LRCD -> Image
; Helper function to render image of rocket as only y position
; of rocket changes which is received from show function
(define (place-rocket x)
  (place-image ROCKET 10 x BACKG))

; LRCD KeyEvent -> LRCD
; starts the countdown when space bar is pressed, 
; if the rocket is still resting 
(check-expect (launch "resting" " ") -3)
(check-expect (launch "resting" "a") "resting")
(check-expect (launch -3 " ") -3)
(check-expect (launch -1 " ") -1)
(check-expect (launch 33 " ") 33)
(check-expect (launch 33 "a") 33)

(define (launch x ke)
  (cond
    [(string? x)(if (string=? ke " ")-3 x)]
    [(<= -3 x -1) x]
    [(>= x 0) x]))

 
; LRCD -> LRCD
; raises the rocket by YDELTA,
;  if it is moving already 
(define (fly x)
  x)

; LRCD -> LRCD
(define (main1 s)
  (big-bang s
    [to-draw show]
    [on-key launch]))

