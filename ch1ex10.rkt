;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch1ex10) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define in 42)
(cond [(string? in) (string-length in)]
      [(image? in)(* (image-height in)(image-width in))]
      [(number? in)(if (<= in 0) in (- in 1))]
      [(boolean? in)(if #true 10 20)]
      [else "Not an expected input"])