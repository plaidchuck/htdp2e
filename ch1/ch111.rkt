;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch111) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define (distance x y)
  (sqrt (+ (* x x)(* y y))))

(define (csurface length)
  (* length length))

(define (cvolume length)
  (* length (csurface length)))

(define (string-first string)
  (if (string=? string "") "Empty string" (string-ith string 0)))

(define (string-last string)
  (if (string=? string "") "Empty string"
      (string-ith string (- (string-length string) 1))))

(define (===> sunny friday)
  (or (not sunny) friday))

(define (image-area image)
  (* (image-width image)(image-height image)))

(define (image-classify image)
  (if (> (image-width image)(image-height image))
         "wide" (if (> (image-height image)(image-width image))
         "tall" "square")))

(define (string-join string1 string2)
  (string-append string1 "_" string2))

(define (string-insert string i)
  (if (string=? string "") "_"
   (string-append (substring string 0 i) "_" (substring string i))))

(define (string-delete string i)
  (if (string=? string "") "No character to delete"
  (string-append (substring string 0 i)(substring string (+ i 1)))))