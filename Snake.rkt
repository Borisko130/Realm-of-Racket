#lang racket
(require 2htdp/universe 2htdp/image)

;; CONSTANTS

(define WIDTH 300)
(define HEIGHT 300)

; Game field main objects: snake, goos
(struct pit (snake goos)#:transparent)

; Snake: head direction, segments
(struct snake (dir segs) #:transparent)

; Position of objects on the field
(struct posn (x y)#:transparent)

; Goo: location on the field, time until expiration
(struct goo (loc expire)#:transparent)


;; FUNCTIONS

(define (start-snake)
  (big-bang (pit (snake "right" (list (posn 1 1)))
                 (list (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)
                       (fresh-goo)))
    (on-tick next-pit TICK-RATE)
    (on-key direct-snake)
    (to-draw render-pit)
    (stop-when dead? render-end)))


; Checks gamestate, if snake can eat goo it is eaten, otherwise goo ages and snake moves
(define (next-pit w)
  (define snake (pit-snake w))
  (define goos (pit-goos w))
  (define goo-to-eat (can-eat snake goos))
  (if goo-to-eat
      (pit (grow snake) (age-goo (eat goos goo-to-eat)))
      (pit (slither snake) (age-goo goos))))

; Function that checks if there is nearby goo that snake can eat
(define (can-eat snake goos)
  (cond [(empty? goos) #f]
        [else (if (close? (snake-head snake) (first goos))
                  (first goos)
                  (can-eat snake (rest goos)))]))

; Checks if the goo close to the snake
(define (close? s g)
  (posn=? s (goo-loc g)))

; Eats goo
(define (eat goos goo-to-eat)
  (cons (fresh-goo) (remove goo-to-eat goos)))

; Grow snake
(define (grow sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (snake-segs sn))))

; Snake movement
(define (slither sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (all-but-last (snake-segs sn)))))

; Draws head in new place and removes tail from old place
(define (all-but-last segs)
  (cond [(empty? (rest segs)) empty]
        [else (cons (first segs) (all-but-last (rest segs)))]))

;(define snake-example
;  (snake "up" (list (posn 1 1) (posn 1 2) (posn 1 3))))
;
;(define goo-example
;  (list (goo (posn 1 0) 3) (goo (posn 5 8) 15)))
;
;(define pit-example
;  (pit snake-example goo-example))