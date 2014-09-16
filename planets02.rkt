#lang racket
;; Daniel Baron
;;  
;; A multi-threaded planet animation
;;
;; I have modified my planets01 file to use an explicit 
;; thread for each planet's position loop, and one more thread for
;; the animation loop.
;;
;; As in planets01, the checkbox callback invokes "animate". 
;; Now, "animate" sends "suspend-threads" or "resume-threads" to
;; "planet-container", in addition to suspending or resuming
;; "animation-thread". The animation thread now
;; simply refreshes the canvas every "animation-interval" seconds.
;; 
;; The planet positions are updated by the individual planet threads:
;; "planet-container" maintains a list of these, spawns a new one
;; for each new planet created, and offers public methods to suspend
;; or resume them en masse.
;;
;; Each planet thread invokes its planet's "loop" method, which
;; updates the planet's position every "planet-interval" seconds.
;;
;; Provided:
;;    thread animation-thread
;;    procedure animate
;;    procedure callback for run-checkbox
;;
;;    constant planet-interval
;;    constant animation-interval
;;    methods suspend-threads, resume-threads for planet-container%
;;    field planet-threads for planet-container%
;;    method loop for planet%
;;    
;; Modified:   
;;    procedure animate
;;    thread animation-thread
;;    method add-planet for planet-container%
;;    

(require racket/gui)

;; Refresh intervals:
(define planet-interval 0.03)
(define animation-interval 0.05)


;; Small 2d vector library for the Newtonian physics
(define (x v) (vector-ref v 0))
(define (y v) (vector-ref v 1))
(define (x! v value) (vector-set! v 0 value))
(define (y! v value) (vector-set! v 1 value))
(define (v* v value) (vector-map (lambda (x) (* x value)) v))
(define (v+ v w) (vector-map + v w))
(define (v- v w) (vector-map - v w))
(define (v-zero! v) (vector-map! (lambda (x) 0) v))
(define (v-dot v w) (let ((vw (vector-map * v w))) (+ (x vw) (y vw))))
(define (v-mag v) (sqrt (v-dot v v)))

;; Planet object
(define planet%
  (class object%
    (public m p v draw loop)
    (init-field (mass 1)
                (position (vector 0 0 ))
                (velocity (vector 0 0 ))
                (force (vector 0 0 )))
    (define (m) mass)
    (define (p) position)
    (define (v) velocity)
    ;; Use Newton's law of gravitation.
    ;; I assume the gravitational constant is one
    (define (calculate-force pl)
      (v-zero! force)
      (for-each (lambda (other-planet)
                  (when (not (equal? this other-planet))
                    (let* ((direction (v- (send other-planet p) position))
                           (dist (v-mag direction))
                           (other-mass (send other-planet m))
                           (new-force (v* direction (/ (* mass other-mass) (* dist dist))))
                           )
                      (vector-map! + force new-force))))
                pl)
      )
    ;; Simple Euler integration of acceleration and velocity
    (define (move) 
      (let ((acc (v* force (/ 1.0 mass))))
        (vector-map! + velocity acc)
        (vector-map! + position velocity)))
    ;; Draw a circle 
    (define (draw dc) 
      (send dc set-brush brush)
      (send dc set-pen pen)
      (send dc draw-ellipse (x position) (y position) radius radius ))
    ;; Initialize to random velocity, mass, and color
    (x! velocity (* 2 (random)))
    (y! velocity (* 2 (random)))
    (set! mass (+ 1 (* 10 (random))))
    (define radius (* 5 (sqrt mass)))
    (define color 
      (let* ((r (random))
             (b (real->floating-point-bytes r 4)))
        (make-object color% (bytes-ref b 0) (bytes-ref b 1) (bytes-ref b 2) )))
    (define brush (make-object brush% color))
    (define pen (make-object pen% color))
    
    ;; The position loop
    (define (loop)
      (sleep planet-interval)
      (calculate-force (send planet-container get-planets))
      (move)
      (loop))
    
    (super-new) ))


;; Abstract the list-handling for a list of planets
(define planet-container%
  (class object%
    (public add-planet draw get-planets 
            suspend-threads resume-threads)
    (init-field (planets '()))
    (init-field (planet-threads '())); list of threads
    (define (get-planets) planets)
    (define (add-planet planet)
      (set! planets (cons planet planets))
      
      ; Invoke the new planet's "loop" method in a new thread,
      ; push the new thread onto the list:
      (set! planet-threads (cons 
                            (thread (lambda () (send planet loop)))
                            planet-threads))
      ; If animation is currently off, we suspend the new thread:
      (when (not (thread-running? animation-thread))
        (thread-suspend (car planet-threads))))
    
    ;Suspend all the planet threads:
    (define (suspend-threads)
      (for-each thread-suspend planet-threads))
    ;Resume all the planet threads:
    (define (resume-threads)
      (for-each thread-resume planet-threads))
    
    (define (draw dc)
      (for-each (lambda (planet)
                  (send planet draw dc))
                planets))
    (super-new)
    )
  )
(define planet-container (new planet-container%))

;; The GUI
(define frame (new frame% 
                   (label "Planets")
                   (min-width 120)
                   (min-height 80)
                   ))
(send frame create-status-line)
(send frame show #t)

(define h-panel
  (new horizontal-panel%
       (parent frame)
       (stretchable-height #f)
       (style '(border))
       (border 2)))

(define run-checkbox
  (new check-box%
       (parent h-panel)
       (label "Run animation")
       
       ;; The callback procedure invokes animate
       (callback (lambda (self event)
                   (animate)))))

(define my-canvas%
  (class canvas%
    (override on-paint on-event)
    
    (define (on-paint)
      (let ((dc (send this get-dc))
            (w (send this get-width))
            (h (send this get-height)))
        (send dc clear)
        (send planet-container draw dc)
        ))
    (define (on-event event)
      (when (send event button-down?)
        (let ((x (send event get-x))
              (y (send event get-y)))
          (send frame set-status-text (format "Mouse at ~a ~a" x y))
          (send planet-container add-planet (new planet% (position (vector x y))))
          (send this refresh)))
      )
    (super-new)
    (send (send this get-dc) set-background (make-object color% 8 8 64))
    ))

(define canvas
  (new my-canvas%
       (parent frame)
       (style '(border))
       (min-width 640)
       (min-height 480)))


;;Planet Animator loop thread
(define animation-thread
  (thread (lambda ()
            (let loop ()
              (sleep animation-interval)
              (send canvas refresh)
              (loop) ))))
;; Animation is initially suspended:
(thread-suspend animation-thread)

;;Animation switch, invoked by the checkbox.
(define (animate)
  (cond ((thread-running? animation-thread)
         (send planet-container suspend-threads)
         (thread-suspend animation-thread))
        (else
         (thread-resume animation-thread)
         (send planet-container resume-threads))))


