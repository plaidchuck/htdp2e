; A VCat is a structure.
; (make-VCat Number Number Boolean)
; (make-VCat x hl dir) describes a VCat whose horizontal position
; is the x Number that ranges between the origin and the
; width of the backgroudn constant image and whose happiness level is the hl number which
; is in an interval [0, 100] and the direction the VCat is moving
; on the canvas/background which is defined by a boolean that is true if moving
; right and false if moving left
(define-struct VCat [xpos happy moving-right?])
(define ex1 (make-VCat 0 101 #true))
(define ex2 (make-VCat 0 100 #true))
(define ex3 (make-VCat 100 0 #true))
(define ex4 (make-VCat 100 100 #false))
(define ex5 (make-VCat 400 100 #false))

(define cat1.)

(define cat2 .)

(define GAUGE-BACKGROUND
  (empty-scene 50 25))


; Background scene to fit moving VCat and happiness level gauge
(define BACKGROUND
  (empty-scene (* (image-width cat1) 4)
               (+ (image-height cat1) (image-height GAUGE-BACKGROUND))))




; VCat [xpos] [happy] ->  VCat [xpos] [happy]
; Consumes a VCat and with each clock tick, produces a VCat that increases xpos by 1
; and decrease happiness level by 1, moving the cat accross the screen to the right
; and lowering the happiness gauge

(define (tock cat)
  (cond
    [(> (VCat-happy cat) 100) (make-VCat
                (if (VCat-moving-right? cat) (+ (VCat-xpos cat) 3) (- (VCat-xpos cat) 3))
                                    100 (VCat-moving-right? cat))]
    [(>= (VCat-xpos cat)(- (image-width BACKGROUND)(image-width cat1 )))
                 (make-VCat (- (VCat-xpos cat) 3)(- (VCat-happy cat) 0.5) #false)]
    [(<= (VCat-xpos cat) 1)(make-VCat (+ (VCat-xpos cat ) 3) (- (VCat-happy cat) 0.5) #true)]
    [else (make-VCat
                 (if (VCat-moving-right? cat) (+ (VCat-xpos cat) 3) (- (VCat-xpos cat) 3))                
               (- (VCat-happy cat) 0.5) (VCat-moving-right? cat))]))

; Cat Keyevent -> Cat
; Consumes a cat and a keypress (either "up" or "f") and produces a cat
; where if up is pressed the happiness increases by 0.5 and if f is pressed
; the happiness increases by 25.
(check-expect (keypress ex3 "up")(make-VCat 100 5 #true))
(check-expect (keypress ex3 "f")(make-VCat 100 25 #true))
(define (keypress cat ke)
  (cond
    [(key=? ke "up") (make-VCat(VCat-xpos cat) (+ (VCat-happy cat) 5) (VCat-moving-right? cat))]
    [(key=? ke "f") (make-VCat (VCat-xpos cat) (+ (VCat-happy cat) 25)(VCat-moving-right? cat))]
    [else (make-VCat (VCat-xpos cat)(VCat-happy cat)(VCat-moving-right? cat))]))

; Cat -> Boolean
; Consumes a cat and stops the program if the happiness level
; is 0 which produces a true boolean
;(check-expect (end ex3) #true)
(define (end cat)
  (= (VCat-happy cat) 0))


; VCat [xpos] [happy] -> Image
; A function that consumes a VCat structure's fields and produces an image
; of the current x position of the cat and its current happiness level
(define (render cat)
  (overlay/align "middle" "bottom"
                 (place-image (rectangle (VCat-happy cat) (image-height GAUGE-BACKGROUND)
             "solid" "red")
            0 (/ (image-height GAUGE-BACKGROUND) 2) GAUGE-BACKGROUND)
         (place-image (if (odd? (VCat-xpos cat)) cat1 cat2)
                      (+ (VCat-xpos cat) (/ (image-width cat1) 2))(/ (image-height cat1) 2)
                      BACKGROUND)))


(define (happy-cat cat)
  (big-bang cat
    [on-tick tock]
    [on-key keypress]
    [stop-when end]
    [to-draw render]))
