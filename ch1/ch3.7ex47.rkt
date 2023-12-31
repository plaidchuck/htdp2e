;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch3.7ex47) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; Happiness Level is a number.
; Design a world program that maintains and displays
; a “happiness gauge.” Let’s call it gauge-prog,
; and let’s agree that happiness is a number
; between 0 and 100 (inclusive)

; Background of gauge to show contrast of
; happiness level lowering
(define BACKGROUND
  (empty-scene 100 25))

; Happiness gauge
(define GAUGE
  (rectangle 100 (image-height BACKGROUND)
             "solid" "red"))


; Happiness Level -> Happiness Level
; launches the program from intitial
; Happiness Level(hl) 
(define (gauge-prog hl)
  (big-bang hl
    [on-tick tock]
    [on-key keypress]
    [to-draw render]))

; Happiness Level -> Happiness Level
; With each clock tick, the happiness level decreases
; by 0.1 but does not go below 0
(define (tock hl)
  (cond
    [(> hl 100) 100]
    [(> hl 0)(- hl 0.1)]
    [else 0]))

; Happiness Level Key Press -> Happiness Level
; if Up Arrow key is pressed, happiness level increases
; by 1/3 of gauge length
; if Down Arrow key is pressed, happiness level
; decreases by 1/5 of gauge length
(define (keypress hl key)
  (cond
    [(key=? key "up") (round (+ hl (/ (image-width BACKGROUND) 3)))]
    [(key=? key "down") (round (- hl (/ (image-width BACKGROUND) 5)))]
    [else hl]))

  

; Happiness Level -> Image
; Represents the Happiness Level by the Happiness
; guage
(define (render hl)
  (place-image GAUGE
            (- hl (/ (image-width GAUGE) 2))
             (/ (image-height BACKGROUND) 2)
             BACKGROUND))
  
  
  
  