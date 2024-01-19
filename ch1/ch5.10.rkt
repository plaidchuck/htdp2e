;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch5.10) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define BACKG (empty-scene 200 20))
(define CURSOR (rectangle 1 20 "solid" "red"))
(define BACKG-WIDTH (image-width BACKG))

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with 
; the cursor displayed between s and t
(define ex1
  (make-editor "hello" "world"))
(define ex2
  (make-editor "walla" "walla"))
(define ex3
  (make-editor "" ""))

; Editor -> Image
; Consumes and editor structure and produces image of both strings from
; the editor structure
;(check-expect (render ex2)
;              (overlay/align "left" "center"
;              (beside (text "walla" 16 "black") CURSOR
;                      (text "walla" 16 "black")) BACKG))
(define (render ed)
(overlay/align "left" "center" (beside (draw-text (editor-pre ed)) CURSOR
        (draw-text (editor-post ed))) BACKG))

; String -> Image
; Helper function to consume String from editor field
; and produce an image of the text
;(check-expect (draw-text "TEST")(text "TEST" 16 "black"))
(define (draw-text str)
  (text str 16 "black"))

; Editor Keyevent -> Editor
; Adds a single character to the end of pre field of editor, unless
; the keyevent is backspace which it will instead delete one character
; left of the cursor, or the keyevent is a left or right arrow
; which will move the cursor. Ignores all other key events.
;(check-expect (edit ex3 "a")(make-editor "a" ""))
;(check-expect (edit ex3 "\b")(make-editor "" ""))
;(check-expect (edit ex1 "\b")(make-editor "hell" "world"))
;(check-expect (edit (make-editor "h" "igh") "\b")(make-editor "" "igh"))
;(check-expect (edit (make-editor "test" "") "\b") (make-editor "tes" ""))
;(check-expect (edit (make-editor "" "world") "\b") (make-editor "" "world"))
;(check-expect (edit ex3 "\t")(make-editor "" ""))
;(check-expect (edit ex3 "\r") (make-editor "" ""))
;(check-expect (edit ex3 "up") (make-editor "" ""))
;(check-expect (edit ex2 "left")(make-editor "wall" "awalla"))
;(check-expect (edit ex2 "right")(make-editor "wallaw" "alla"))
;(check-expect (edit ex1 "right")(make-editor "hellow" "orld"))
;(check-expect (edit ex3 "right")(make-editor "" ""))
;(check-expect (edit (make-editor "hello" "") "right") (make-editor "hello" ""))
;(check-expect (edit (make-editor "hif" "i") "right") (make-editor "hifi" ""))
;(check-expect (edit (make-editor "hifi" "") "right") (make-editor "hifi" ""))
;(check-expect (edit (make-editor "h" "ifi") "left") (make-editor "" "hifi"))
;(check-expect (edit (make-editor "" "hifi") "left") (make-editor "" "hifi"))
;(check-expect (edit
;               (make-editor "abcdefghijklmnopqrstuvwxyz" "") "1" )
;              (make-editor "abcdefghijklmnopqrstuvwxyz" ""))

(define (edit ed ke)
  (cond
   [(string=? ke "\b")(make-editor (string-remove-last (editor-pre ed))
                                (editor-post ed))]
   [(string=? ke "left")(make-editor (string-remove-last (editor-pre ed))
                                     (string-append
                                      (string-last (editor-pre ed))
                                      (editor-post ed)))]
   [(string=? ke "right")
         (make-editor (string-append
                                       (editor-pre ed)
                                       (string-first (editor-post ed)))
                                      (string-rest (editor-post ed)))]
   [(and (= (string-length ke) 1)
         (not (string=? ke "\t"))
         (not (string=? ke "\r"))
         (not (string=? ke "\u007F")))
    (make-editor (string-append (editor-pre ed) ke) (editor-post ed))]
   
   [else ed]))

; String -> String
; Consumes a string and returns a string with the last 1String of the
; original String removed
;(check-expect (string-remove-last "test") "tes")
(define (string-remove-last str)
  (if
   (> (string-length str) 0) (substring str 0 (- (string-length str) 1)) str)) 

;String -> String
; Consumes a string and returns a string with the first character removed
;(check-expect (string-rest "hello") "ello")
(define (string-rest str)
  (if (> (string-length str) 0)
  (substring str 1) str))

; String -> 1String 
; Consumes a string and produces the last character of the string
;(check-expect (string-last "test") "t")
(define (string-last str)
  (if
   (> (string-length str) 0) (string-ith str (- (string-length str) 1)) str))

; String -> 1String
; Consumes a String and produces the first character from the string
;(check-expect (string-first "hey") "h")
(define (string-first str)
  (if (> (string-length str) 0) (string-ith str 0) str))


(define (run ed)
  (big-bang ed
    [on-key edit]
    [to-draw render]))
