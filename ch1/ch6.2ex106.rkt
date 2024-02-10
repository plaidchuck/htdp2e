
; A VCat is a structure.
; (make-VCat Number Number)
; (make-VCat x hl) describes a VCat whose horizontal position
; is the x Number and whose happiness level is the hl number which
; is in an interval [0, 100].
(define-struct VCat [xpos happy])
(define excat1 (make-VCat 20 20))
(define excat2 (make-VCat 20 100))
(define excat3 (make-VCat 0 0))
(define excat4 (make-VCat 0 102))

; A VCham is a structure:
; (make-VCam Number Number String)
; (make-VCam x hl c)
; Where x is the x horizontal position of the VCham on the main backgroud canvas,
; hl is the happiness level of the VCham, that has a value of [0, 100]
; and color is its current color which is either red, green, or blue.
(define-struct VCham [xpos happy color])
(define excham1 (make-VCham 20 20 "red"))
(define excham2 (make-VCham 100 0 "blue"))
(define excham3 (make-VCham 0 101 "green"))

; A VAnimal is either
; – a VCat
; – a VCham


; A horizontal position is a number [0, Main Background width]
; Happiness is a Positive number [0, 100].
; A color is a String, represented by primitive Color words "red", "blue" "green"

; Insert cat image #1(define cat1.)

; Insert cat image #2(define cat2 .)


; Insert transparent chameleon image here (define cham.)
(define cham1 (make-VCham 0 0 "green"))

(define CAT-WIDTH (image-width cat1))
(define CHAM-WIDTH (image-width cham))
(define CAT-HEIGHT (image-height cat1))
(define CHAM-HEIGHT (image-height cham))

; How fast VAnimal moves across screen
(define VA-SPEED 3)
; How fast the happiness level decreases for the VAnimal
(define VA-HAPPY 0.5)

(define PET 2)
(define FEED 10)

; Size of happiness gauge
(define GAUGE-BACKGROUND
  (empty-scene 50 25))


; Background scene to fit moving VCat/VCham and happiness/hunger level gauge
(define MAIN-BACKGROUND
  (empty-scene (* CAT-WIDTH 4)
               (+ CHAM-HEIGHT (image-height GAUGE-BACKGROUND))))

; VAnimal -> Image
; Renders a VAnimal onto the background with a happiness gauge displayed on the
; bottom. The VAnimals xposition and happiness gauge are for the location and current
; status of the gauge, respectively.
(check-expect (render excat1)(overlay/align "middle" "bottom" (draw-gauge excat1)
                                            (draw-animal excat1) MAIN-BACKGROUND))
(define (render va)
  (overlay/align "middle" "bottom"
                 (draw-gauge va)
         (draw-animal va)
                      MAIN-BACKGROUND))

; VAnimal -> Vanimal
; Consumes a clock tick and alters the state of the VAnimal by moving it
; to the right across the canvas while also lowering the happiness level
; by a certain amount
(check-expect (tick excat4)(make-VCat 3 100))
(check-expect (tick excat2)(make-VCat 23 99.5))
(check-expect (tick (make-VCat (image-width MAIN-BACKGROUND) 100))
              (make-VCat 3 99.5))
(check-expect (tick excham1)(make-VCham 23 19.5 "red"))
(check-expect (tick excham3)(make-VCham 3 100 "green"))
(check-expect (tick excat3)(make-VCat 3 0 ))
(check-expect (tick excham2)(make-VCham 103 0 "blue"))
(define (tick va)
  (cond
    [(VCat? va)
     (cond
       [(> (VCat-happy va) 100)
        (make-VCat (move-animal (+ (VCat-xpos va) VA-SPEED)) 100)]
       [(<= (VCat-happy va) 0)
        (make-VCat (move-animal (+ (VCat-xpos va) VA-SPEED)) 0 )]
       [else (make-VCat (move-animal (+ (VCat-xpos va) VA-SPEED)) (- (VCat-happy va) VA-HAPPY))])]
    [(VCham? va)
     (cond
       [(> (VCham-happy va) 100)
           (make-VCham (move-animal (+ (VCham-xpos va) VA-SPEED)) 100 (VCham-color va))]
       [(<= (VCham-happy va) 0)
        (make-VCham (move-animal (+ (VCham-xpos va) VA-SPEED)) 0  (VCham-color va))]
        [else (make-VCham (move-animal (+ (VCham-xpos va) VA-SPEED)) (- (VCham-happy va) VA-HAPPY)
                                       (VCham-color va))])]))

; VAnimal KeyEvent -> VAnimal
; Consumes a VAnimal and one of three keystrokes: "up", "down", "r", "g", "b"
; Where up is a pet that increases happiness only for the cat, down feeds both
; the VCat and VCham, and r,g,b, changes the colors respectively only for the
; VCham
(check-expect (keypress excat3 "up")(make-VCat 0 2))
(check-expect (keypress excat3 "down")(make-VCat 0 10))
(check-expect (keypress excat3 "r")(make-VCat 0 0))
(check-expect (keypress excham1 "down")(make-VCham 20 30 "red"))
(check-expect (keypress excham1 "g")(make-VCham 20 20 "green"))
(check-expect (keypress excham1 "up")(make-VCham 20 20 "red"))
(define (keypress va ke)
  (cond
    [(VCat? va)
     (cond
       [(key=? "up" ke)(make-VCat (VCat-xpos va)(+ (VCat-happy va) PET))]
       [(key=? "down" ke)(make-VCat (VCat-xpos va)(+ (VCat-happy va) FEED))]
       [else (make-VCat (VCat-xpos va)(VCat-happy va))])]
    [(VCham? va)
     (cond
       [(key=? "down" ke)(make-VCham (VCham-xpos va)(+ (VCham-happy va) FEED)(VCham-color va))]
       [(or (key=? "r" ke)(key=? "g" ke)(key=? "b" ke))(make-VCham (VCham-xpos va)
                                                             (VCham-happy va)
                                                             (change-color ke))]
     [else (make-VCham (VCham-xpos va)(VCham-happy va)(VCham-color va))])]))
      
; 1String -> String
; Consumes a single character r, g, or b, and returns the respective color
; string representation
(check-expect (change-color "r") "red")
(define (change-color ke)
  (cond
    [(equal? "r" ke) "red"]
    ((equal? "g" ke) "green")
    [(equal? "b" ke) "blue"]))

; Number -> Number
; Consumes a number which is the xposition of a VAnimal after moving a tick
; and returns the modulo of that number divided by the length of the main background
; to make the VAnimal reappear on the left side of the screen when he exits via the right side.
(check-expect (move-animal 23) 23)
(check-expect (move-animal (image-width MAIN-BACKGROUND)) 0)
(define (move-animal xpos)
  (modulo xpos (image-width MAIN-BACKGROUND)))


; VAnimal -> Image
; Consumes VAnimal and draws VAnimal based on type and its xposition
; as well as its color if it is a VCham
(check-expect (draw-animal excham1)(place-image (draw-cham excham1)
                       (+ (VCham-xpos excham1)(/ (image-width cham) 2))(/ (image-height cham) 2)
                          MAIN-BACKGROUND))
(check-expect (draw-animal excat1)(place-image cat2
                       (+ (VCat-xpos excat1)(/ (image-width cat1) 2))(/ (image-height cat1) 2)
                          MAIN-BACKGROUND))
(define (draw-animal va)
  (cond
    [(VCat? va)(place-image (if (odd? (VCat-xpos va)) cat1 cat2)
                      (+ (VCat-xpos va) (/ (image-width cat1) 2))(/ (image-height cat1) 2)
                      MAIN-BACKGROUND)]
    [(VCham? va)(place-image (draw-cham va) (+ (VCham-xpos va)(/ (image-width cham) 2))(/ (image-height cham) 2)
                          MAIN-BACKGROUND)]))

;VCham -> Image
; Consumes a VCham and produes an image of the cham overlaid on
; a rectangle that has a transparent portion to allow the cham to change colors.
(check-expect (draw-cham cham1) (overlay cham (rectangle (image-width cham) (image-height cham)
                                               "solid" "green")))
(define (draw-cham vcham) (overlay cham (rectangle (image-width cham) (image-height cham)
                                               "solid" (VCham-color vcham))))

; VAnimal -> Image
; Draws happiness gauge and its level based on the VAnimal type
(check-expect (draw-gauge excat1)(place-image (rectangle 20
                                              (image-height GAUGE-BACKGROUND) "solid" "red")
                                              0 (/ (image-height GAUGE-BACKGROUND) 2)
                                              GAUGE-BACKGROUND))
(check-expect (draw-gauge excham1)(place-image (rectangle 20
                                               (image-height GAUGE-BACKGROUND) "solid" "red")
                                              0 (/ (image-height GAUGE-BACKGROUND) 2)
                                              GAUGE-BACKGROUND))
(check-expect (draw-gauge excat4)(place-image (rectangle 100 (image-height GAUGE-BACKGROUND) "solid" "red")
                                0 (/ (image-height GAUGE-BACKGROUND) 2) GAUGE-BACKGROUND))
(define (draw-gauge va)
  (cond
    [(VCat? va)
     (cond
       [(>= (VCat-happy va) 100)
        (place-image (rectangle 100 (image-height GAUGE-BACKGROUND) "solid" "red")
                                0 (/ (image-height GAUGE-BACKGROUND) 2) GAUGE-BACKGROUND)]
       [(<= (VCat-happy va) 0)
        (place-image (rectangle 0 (image-height GAUGE-BACKGROUND) "solid" "red")
                                0 (/ (image-height GAUGE-BACKGROUND) 2) GAUGE-BACKGROUND)]
       [else (place-image (rectangle (VCat-happy va)(image-height GAUGE-BACKGROUND) "solid" "red")
                  0 (/ (image-height GAUGE-BACKGROUND) 2) GAUGE-BACKGROUND)])]
    [(VCham? va)
     (cond
        [(>= (VCham-happy va) 100)
        (place-image (rectangle 100 (image-height GAUGE-BACKGROUND) "solid" "red")
                                0 (/ (image-height GAUGE-BACKGROUND) 2) GAUGE-BACKGROUND)]
       [(<= (VCham-happy va) 0)
        (place-image (rectangle 0 (image-height GAUGE-BACKGROUND) "solid" "red")
                                0 (/ (image-height GAUGE-BACKGROUND) 2) GAUGE-BACKGROUND)]
     [else (place-image (rectangle (VCham-happy va)(image-height GAUGE-BACKGROUND) "solid" "red")
                   0 (/ (image-height GAUGE-BACKGROUND) 2) GAUGE-BACKGROUND)])]))

; Application
(define (cat-cham va)
  (big-bang va
    [on-tick tick]
    [on-key keypress]
    [to-draw render]))
