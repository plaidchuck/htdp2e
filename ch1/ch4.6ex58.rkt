;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch4.6ex58) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; A Price falls into one of three intervals with the appropriate tax: 
; — 0 through 999  0%
; — 1000 through 9999 5%
; — 10000 and above. 8%
; interpretation the price of an item

(define LOW-PRICE-TAX
  0.05)

(define LUXURY-PRICE-TAX
  0.08)

(define LOW-PRICE-BOUNDS
  1000)

(define LUXURY-PRICE-BOUNDS
  10000)

; Price -> Number
; computes the amount of tax charged for p
(check-expect (sales-tax 0) 0)
(check-expect (sales-tax 537) 0)
(check-expect (sales-tax 999) 0)
(check-expect (sales-tax 1000) (* LOW-PRICE-TAX 1000))
(check-expect (sales-tax 1282) (* LOW-PRICE-TAX 1282))
(check-expect (sales-tax 10000) (* LUXURY-PRICE-TAX 10000))
(check-expect (sales-tax 12017) (* LUXURY-PRICE-TAX 12017))


(define (sales-tax p)
  (cond
    [(and (<= 0 p) (< p LOW-PRICE-BOUNDS)) 0]
    [(and (<= LOW-PRICE-BOUNDS p)(< p LUXURY-PRICE-BOUNDS))(* LOW-PRICE-TAX p)]
    [(>= p LUXURY-PRICE-BOUNDS)(* LUXURY-PRICE-TAX p)]))