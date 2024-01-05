;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch4.4UFO) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; A WorldState falls into one of three intervals: 
; – between 0 and CLOSE
; – between CLOSE and HEIGHT
; – greater than HEIGHT
 
(define WIDTH 300) ; distances in terms of pixels 
(define HEIGHT 100)
(define CLOSE (/ HEIGHT 3))
(define MTSCN (empty-scene WIDTH HEIGHT)) ; short for empty scene 
(define UFO (overlay (circle 10 "solid" "green")
                     (rectangle 40 5 "solid" "green" )))
 
; WorldState -> WorldState
(define (main y0)
  (big-bang y0
     [on-tick nxt]
     [to-draw render/status]
     [stop-when landed]))
 
; WorldState -> WorldState
; computes next location of UFO 
; (check-expect (nxt 11) 14)
(define (nxt y)
  (+ y 3))
 
; WorldState -> Image
; places UFO at given height into the center of MTSCN
; (check-expect (render 11) (place-image UFO (/ WIDTH 2) 11 MTSCN))
(define (render y)
  (place-image UFO (/ WIDTH 2) y MTSCN))

; WorldState -> WorldState
; Ends program when UFO worldstate is greater than HEIGHT
;(check-expect (landed 101) #true)
;(check-expect (landed 50) #false)
(define (landed y)
  (> y HEIGHT))

; WorldState -> Image
; adds a status line to the scene created by render  
 
;(check-expect (render/status 42)
;             (place-image (text "closing in" 11 "orange")
;                         30 20
;                        (render 42)))
 
(define (render/status y)
  (place-image
    (cond
      [(<= 0 y CLOSE)
       (text "descending" 11 "green")]
      [(and (< CLOSE y) (<= y HEIGHT))
       (text "closing in" 11 "orange")]
      [(> y HEIGHT)
       (text "landed" 11 "red")])
    30 20
    (render y)))