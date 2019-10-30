#lang racket
(require 2htdp/universe 2htdp/image)


;; CONSTANTS

(define TEXT-SIZE 14)
(define WIDTH 375)
(define HEIGHT 250)

; Text X location and Y location of two instances of help text
(define TEXT-X 0)
(define TEXT-UPPER-Y 200)
(define TEXT-LOWER-Y 50)


; Strucrture with initialization numbers for game
(struct interval (small big))

; First instance of help-text
(define HELP-TEXT
  (text "arrow up for larger numbers, for smaller ones arrow down"
        TEXT-SIZE
        "blue"))

; Second instance of help-text
(define HELP-TEXT2
  (text "Press \"=\" when your number is guessed; q to quit."
        TEXT-SIZE
        "blue"))

; Gaming text color
(define COLOR "red")

; Scene
(define MT-SC
  (place-image/align
   HELP-TEXT TEXT-X TEXT-UPPER-Y "left" "top"
   (place-image/align
    HELP-TEXT2 TEXT-X TEXT-LOWER-Y "left" "bottom"
    (empty-scene WIDTH HEIGHT))))


;; FUNCTIONS

; To start game enter (start <lower> <upper>)
(define (start lower upper)
  (big-bang (interval lower upper)
    (on-key deal-with-guess)
    (to-draw render)
    (stop-when single? render-last-scene)))

; Key handler
(define (deal-with-guess w key)
  (cond [(key=? key "up") (bigger w)]
        [(key=? key "down") (smaller w)]
        [(key=? key "q") (stop-with w)]
        [(key=? key "=") (stop-with w)]
        [else w]))

; Decrease guess
(define (smaller w)
  (interval (interval-small w)
            (max (interval-small w) (sub1 (guess w)))))

; Increase guess
(define (bigger w)
  (interval (min (interval-big w) (add1 (guess w)))
            (interval-big w)))

; Current guess
(define (guess w)
  (quotient (+ (interval-small w) (interval-big w)) 2))

; Render scene
(define (render w)
  (overlay (text (number->string (guess w)) TEXT-SIZE COLOR) MT-SC))

(define (render-last-scene w)
  (overlay (text "End" TEXT-SIZE COLOR) MT-SC))

(define (single? w)
  (= (interval-small w) (interval-big w)))