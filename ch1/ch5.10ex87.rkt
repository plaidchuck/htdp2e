;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ch5.10ex87) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(define BACKG (empty-scene 200 20))
(define CURSOR (rectangle 1 20 "solid" "red"))
(define BACKG-WIDTH (image-width BACKG))

(define-struct editor [text count])
; An Editor is a structure:
;   (make-editor String Number)
; interpretation (make-editor s n) describes an editor where s is a String
; of the entire entered text and n is the number(count) of 1Strings to the
; left of the cursor.
(define ex1 (make-editor "hello world" 11))
(define ex2 (make-editor "" 0))
(define ex3 (make-editor "abcdeghjiklfkfkfkfkfkkfkfkfkfkkdkd" 34))

; Editor -> Image
; Consumes and editor structure and produces image of both strings from
; the editor structure
(check-expect (render ex1)
              (overlay/align "left" "center"
              (beside (draw-text (substring (editor-text ex1)
                                            0 (editor-count ex1)))
                      CURSOR
                      (draw-text (substring (editor-text ex1)
                                            (editor-count ex1))))
              BACKG))
(check-expect (render ex2)
              (overlay/align "left" "center"
              (beside (draw-text (substring (editor-text ex2)
                                            0 (editor-count ex2)))
                      CURSOR
                      (draw-text (substring (editor-text ex2)
                                            (editor-count ex2))))
              BACKG))

(define (render ed)
(overlay/align "left" "center" (beside (draw-text (substring (editor-text ed)
                                                   0 (editor-count ed)))
                                                             CURSOR
                                       (draw-text (substring (editor-text ed)
                                                   (editor-count ed) )))
               BACKG))

; String -> Image
; Helper function to consume String from editor field
; and produce an image of the text
;(check-expect (draw-text "TEST")(text "TEST" 16 "black"))
(define (draw-text str)
  (text str 16 "black"))

; Editor Keyevent -> Editor
; Consumes an editor structure and keyevent and produces
; another editor structure to be rendered
(check-expect (edit ex2 "a")(make-editor "a" 1))
(check-expect (edit (make-editor "hello" 2) "a")(make-editor "heallo" 3))
(check-expect (edit ex2 "\t")(make-editor "" 0))
(check-expect (edit ex2 "\r")(make-editor "" 0))
(check-expect (edit ex1 "\b")(make-editor "hello worl" 10))
(check-expect (edit ex2 "\b")(make-editor "" 0))
(check-expect (edit (make-editor "hello" 1) "\b")(make-editor "ello" 0))
(check-expect (edit ex1 "left")(make-editor "hello world" 10))
(check-expect (edit ex2 "left")(make-editor "" 0))
(check-expect (edit (make-editor "hello" 5) "left") (make-editor "hello" 4))
(check-expect (edit ex2 "right")(make-editor "" 0))
(check-expect (edit (make-editor "hello" 1) "right")(make-editor "hello" 2))
(check-expect (edit ex3 "a")(make-editor (editor-text ex3)(editor-count ex3)))
(check-expect (edit ex3 "right")(make-editor (editor-text ex3)(editor-count ex3)))
(define (edit ed ke)
  (cond
    [(and (check-length ed ke)
         (not (string=? ke "\b"))
         (not (string=? ke "left"))
         (not (string=? ke "right")))
         ed]
    [(string=? ke "\b")(make-editor
                        (string-append (string-remove-last (substring
                                                         (editor-text ed) 0
                                                         (editor-count ed)))
                                       (substring (editor-text ed)
                                                  (editor-count ed)))
                        (if (> (editor-count ed) 0) (- (editor-count ed) 1)
                            (editor-count ed)))]
    [(string=? ke "left")(make-editor
                          (editor-text ed)
                          (if (> (editor-count ed) 0) (- (editor-count ed) 1) 0))]
    [(string=? ke "right")(make-editor
                           (editor-text ed)
                           (if (< (editor-count ed) (string-length (editor-text ed)))
                               (+ (editor-count ed) 1) (editor-count ed)))]
    [(and (= (string-length ke) 1)
         (not (string=? ke "\t"))
         (not (string=? ke "\r"))
         (not (string=? ke "\u007F")))
    (make-editor (string-append
               (string-append (substring (editor-text ed) 0 (editor-count ed)) ke)
               (substring (editor-text ed)(editor-count ed)))
              (+ (editor-count ed) 1))]
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

; Image -> Boolean
; Consumes an image of the text rendering from the editor structure
; and keyevent and returns a boolean determining whether the rendered
; text output is longer than the background canvas
;(check-expect (check-length ex3 "t") #true)
;(check-expect (check-length ex2 "a") #false)
(define (check-length ed ke)
  (if (> (image-width (draw-text
                       (string-append (editor-text ed) ke)))
                      BACKG-WIDTH) #true #false))

(define (run ed)
  (big-bang ed
    [on-key edit]
    [to-draw render]))