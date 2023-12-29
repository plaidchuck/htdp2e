;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch3.2) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; A string is one or more 1Strings.
; String -> 1String
; Extracts the first character (1String) from a non-empty string
; given "Test" expect "T"
; given: "hellO" expect "h"
(define (string-first string)
  (string-ith string 0))

; A string is one or more 1Strings
; String -> 1String
; Extracts the last character from a non-empty string
; given "Test" expect "t"
; given "hellO" expect "O"
(define (string-last string)
  (string-ith string (- (string-length string) 1)))

; A pixel count is a number. An image is any external file or
; primitive construction composed of pixels.
; Image -> Number
; Counts the number of pixels in an image by determining its area (l * w).
; given 200x200 image expect: 40,000
; given 225x225 image expect: 50,625
(define (image-area image)
  (* (image-width image)(image-height image)))

; A string is composed of one or more 1Strings.
; String -> String
; Consumes a String and returns the String with the first 1String removed.
; given "Test" expect: "est"
; given "hello" expect: "ello"
(define (string-rest string)
  (substring string 1))

; A string is composed of one or more 1Strings.
; String -> String
; Consumes a string and returns the string with the last 1String removed.
; given "Test" expect: "Tes"
; given "hello" expect: "hell"
(define (string-remove-last string)
  (substring string 0 (- (string-length string) 1)))