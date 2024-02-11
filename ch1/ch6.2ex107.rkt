;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch6.2ex107) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define KITTY "k")
(define LIZARD "l")
;; A Focus is one of:
;; - KITTY
;; - LIZARD


; A VCat is a structure.
; (make-VCat Number Number)
; (make-VCat x hl) describes a VCat whose horizontal position
; is the x Number and whose happiness level is the hl number which
; is in an interval [0, 100].
(define-struct VCat [xpos happy])
(define excat1 (make-VCat 0 20))
(define excat2 (make-VCat 100 100))
(define excat3 (make-VCat 0 0))
(define excat4 (make-VCat 0 102))

; A VCham is a structure:
; (make-VCam Number Number String)
; (make-VCam x hl c)
; Where x is the x horizontal position of the VCham on the main backgroud canvas,
; hl is the happiness level of the VCham, that has a value of [0, 100]
; and color is its current color which is either red, green, or blue.
(define-struct VCham [xpos happy color])
(define excham1 (make-VCham 0 20 "red"))
(define excham2 (make-VCham 100 100 "blue"))
(define excham3 (make-VCham 0 101 "green"))

; A VAnimal is either
; – a VCat
; – a VCham


; A horizontal position is a number [0, Main Background width]
; Happiness is a Positive number [0, 100].
; A color is a String, represented by primitive Color words "red", "blue" "green"

;; A KeyEvent is one of:
;; - "up"
;; - "down"
;; - "r"
;; - "b"
;; - "g"
;; - "k"
;; - "l"
;; Represents a pressed key that triggers
;; a change of the world state.

; A Zoo is a structure
; (make-Zoo struct struct String)
; (make-Zoo VCat VCham f) describes a zoo containing the respective
; animal structures and the string which denotes which VAnimal in the
; zoo has the current focus
(define-struct zoo [cat cham focus])
(define TEST-ZOO-KITTY (make-zoo excat1 excham1 KITTY))
(define TEST-ZOO-LIZARD (make-zoo excat2 excham2 LIZARD))



(define cat1)

(define cat2)


(define cham)
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
  (empty-scene (* CAT-WIDTH 8)
               (+ (* CHAM-HEIGHT 1.3) (image-height GAUGE-BACKGROUND))))

; Zoo -> Image
; Renders a VAnimal onto the background with a happiness gauge displayed on the
; bottom. The VAnimals xposition and happiness gauge are for the location and current
; status of the gauge, respectively.
(define (render zoo)
  (overlay/align "middle" "bottom"
                 (draw-gauge (zoo-focus zoo)
                             (zoo-cat zoo)
                             (zoo-cham zoo))
         (draw-animals (zoo-cat zoo)
                       (zoo-cham zoo))
                      MAIN-BACKGROUND))

; Zoo -> Zoo
; Consumes a clock tick and alters the state of the Zoo by moving 
; incrementing the xposition of each VAnimal and lowering the happiness
; level by set amounts. Produces a zoo which includes these changes
; to the VAnimals state.
(define (tick zoo)
  (make-zoo (make-VCat (+ (move-animal (VCat-xpos (zoo-cat zoo))) VA-SPEED)
                       (cond
                         [(equal? (happiness-exceed-min (VCat-happy (zoo-cat zoo))) #true) 0]
                         [(equal? (happiness-exceed-max (VCat-happy (zoo-cat zoo))) #true) 100]
                         [else (- (VCat-happy (zoo-cat zoo)) VA-HAPPY)]))
            (make-VCham (+ (move-animal (VCham-xpos (zoo-cham zoo))) VA-SPEED)
                        (cond
                          [(equal? (happiness-exceed-min (VCham-happy (zoo-cham zoo))) #true) 0]
                          [(equal? (happiness-exceed-max (VCham-happy (zoo-cham zoo))) #true) 100]
                          [else (- (VCham-happy (zoo-cham zoo)) VA-HAPPY)])
                        (VCham-color (zoo-cham zoo)))
            (zoo-focus zoo)))


; Zoo KeyEvent -> Zoo
; Consumes a Zoo and one of five keystrokes: "up", "down", "r", "g", "b", "l", and "k"
; Where up is a pet that increases happiness only for the cat, down feeds both
; the VCat and VCham, and r,g,b, changes the colors respectively only for the
; VCham. l and k change the focus so the above listed keystrokes only work when
; the respective VAnimal is the focus.
(define (key-handler zoo ke)
  (cond
    [(key=? "k" ke)(make-zoo (zoo-cat zoo)(zoo-cham zoo) KITTY)]
    [(key=? "l" ke)(make-zoo (zoo-cat zoo)(zoo-cham zoo) LIZARD)]
    [(and (equal? (zoo-focus zoo) LIZARD)(or (key=? "r" ke)(key=? "g" ke)(key=? "b" ke)))
     (cond
       [(key=? ke "r")(make-zoo (zoo-cat zoo)(make-VCham
                                              (VCham-xpos (zoo-cham zoo))
                                              (VCham-happy (zoo-cham zoo))
                                              (change-color ke))
                                              (zoo-focus zoo))]
       [(key=? ke "g")(make-zoo (zoo-cat zoo)(make-VCham
                                              (VCham-xpos (zoo-cham zoo))
                                              (VCham-happy (zoo-cham zoo))
                                              (change-color ke))
                                              (zoo-focus zoo))]
       [(key=? ke "b")(make-zoo (zoo-cat zoo)(make-VCham
                                              (VCham-xpos (zoo-cham zoo))
                                              (VCham-happy (zoo-cham zoo))
                                              (change-color ke))
                                              (zoo-focus zoo))])]
    [(and (key=? "up" ke)(equal? (zoo-focus zoo) KITTY))
     (make-zoo (make-VCat (VCat-xpos (zoo-cat zoo)) (+ (VCat-happy (zoo-cat zoo)) PET))
               (zoo-cham zoo) (zoo-focus zoo))]
    [(key=? "down" ke)
     (cond
       [(string=? (zoo-focus zoo) KITTY)(make-zoo (make-VCat (VCat-xpos (zoo-cat zoo))
                                                            (+ (VCat-happy (zoo-cat zoo)) FEED))
                                                 (zoo-cham zoo)(zoo-focus zoo))]
       [(string=? (zoo-focus zoo) LIZARD)(make-zoo (zoo-cat zoo)
                                                   (make-VCham (VCham-xpos (zoo-cham zoo))
                                                               (+ (VCham-happy (zoo-cham zoo)) FEED)
                                                               (VCham-color (zoo-cham zoo)))
                                                   (zoo-focus zoo))])]
    [else (make-zoo (zoo-cat zoo)(zoo-cham zoo)(zoo-focus zoo))]))

      
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





;VCham -> Image
; Consumes a VCham and produes an image of the cham overlaid on
; a rectangle that has a transparent portion to allow the cham to change colors.
(check-expect (draw-cham cham1) (overlay cham (rectangle (image-width cham) (image-height cham)
                                               "solid" "green")))
(define (draw-cham vcham) (overlay cham (rectangle (image-width cham) (image-height cham)
                                               "solid" (VCham-color vcham))))

; Number Number -> Image
; Consumes happiness level and VAnimal focus and produces a happiness gauge
; that has the label of which animal is the focus as well as the current
; happiness level of the animal
(check-expect (draw-gauge LIZARD excat1 excham1)
              (beside (text "Lizard" 15 "red") (place-image 
                   (rectangle 20 (image-height GAUGE-BACKGROUND) "solid" "red")
                  0 (/ (image-height GAUGE-BACKGROUND) 2) GAUGE-BACKGROUND)))
(check-expect (draw-gauge KITTY excat1 excham1)
              (beside (text "Kitty" 15 "blue") (place-image 
                   (rectangle 20 (image-height GAUGE-BACKGROUND) "solid" "red")
                  0 (/ (image-height GAUGE-BACKGROUND) 2) GAUGE-BACKGROUND)))
(define (draw-gauge focus vcat vcham)
(if (string=? focus KITTY)
     (beside (text "Kitty" 15 "blue") (place-image 
                   (rectangle (VCat-happy vcat) (image-height GAUGE-BACKGROUND) "solid" "red")
                  0 (/ (image-height GAUGE-BACKGROUND) 2) GAUGE-BACKGROUND))
    (beside (text "Lizard" 15 "red") (place-image 
                   (rectangle (VCham-happy vcham) (image-height GAUGE-BACKGROUND) "solid" "red")
                  0 (/ (image-height GAUGE-BACKGROUND) 2) GAUGE-BACKGROUND))))

; Number -> Boolean
; Consumes a happiness level number and returns true if whether
; the happiness level has exceeded 100.
(define (happiness-exceed-max hl)
  (if (> hl 100) #true #false))

; Number -> Boolean
; Consumes a happiness level number and returns true if whether
; the happiness level has exceeded 0.
(define (happiness-exceed-min hl)
  (if (<= hl 0) #true #false))

; VAnimal VAnimal -> Image
; Consumes the two VAnimals (VCat and VCham) and displays them side by side at their
; defined xpositions, which for this program will always be the same.
(check-expect (draw-animals excat1 excham1)
              (place-image (draw-cham excham1) (+ (* CAT-WIDTH 1.4) 0) (/ CHAM-HEIGHT 2)
             (place-image cat2 (+ (/ CAT-WIDTH 2) 0) (/ CAT-HEIGHT 2) MAIN-BACKGROUND)))
              
(define (draw-animals vcat vcham)
(place-image (draw-cham vcham) (+ (* CAT-WIDTH 1.4) (VCham-xpos vcham)) (/ CHAM-HEIGHT 2)
             (place-image (if (odd? (VCat-xpos vcat)) cat1 cat2)
                           (+ (/ CAT-WIDTH 2) (VCat-xpos vcat)) (/ CAT-HEIGHT 2) MAIN-BACKGROUND)))

;; Application
(define (cat-and-cham zoo)
  (big-bang zoo
  [on-tick tick]
  [on-key key-handler]  
  [to-draw render]))