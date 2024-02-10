(define WIDTH  100)
(define HEIGHT  60)
(define MTSCN  (empty-scene WIDTH HEIGHT)) ; short for empty scene 
(define ROCKET .)
(define ROCKET-CENTER-TO-TOP
  (- HEIGHT (/ (image-height ROCKET) 2)))
 
(define (create-rocket-scene.v5 h)
    (place-image ROCKET 50
                 (cond [(<= h ROCKET-CENTER-TO-TOP) h]
                       [else ROCKET-CENTER-TO-TOP])
                       MTSCN))
