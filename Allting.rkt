;; Othello av Johan Brandhorst och Robin Carlsson

#lang racket/gui

;; Egen kod för grafisk spelplan
;; Den enda skillnaden är andra färger och fler knappar samt att spelplanen är större.
;; my-othello-ui.scm


;; Created:         021112
;; Last modified::  2004-11-15,  10:53:11
;; Created by:      Tord Svensson (torsv@ida.liu.se)

(require (lib "class.ss" "mzlib")
         (lib "list.ss" "mzlib")
         (lib "mred.ss" "mred"))

;;  set-title!: string -> void
;;  Sets the title on the frame to new-title.
(define (set-title! new-title)
  (send win set-label new-title))

;; set-highlights! (list (cons integer integer)) -> void
;; Takes a list of cons pairs containing positions on the board
;; and draws them in a slightly lighter color. This can be used
;; to show the possible moves to the user. If it is called with
;; the empty list no positions are highlighted.
(define (set-highlights! x)
  (set! highlights x)
  (refresh))

;; show-board: void -> void
;; Showes the frame containing the board.
(define (show-board)
  (send win show #t))

;; hide-board: void -> void
;; Hides the frame containing the board and exits the program.
(define (hide-board)
  (send win show #f)
  ;(exit)
  )

;; clear-board!: void -> void
;; Removes all the pieces from the board.
(define (clear-board!)
  (vector-fill! board 'NONE)
  ;(set! highlights '())
  (refresh))

;; set-piece-at!: integer x integer x symbol -> void
;; Sets the piece at the position x y to piece.
;; Valid symbols for piece are: BLACK, WHITE or NONE.
(define (set-piece-at! x y piece)
  (vector-set! board (+ (* y 8) x) piece)
  (refresh))

;; Hemmagjorda knappar

;; Ångra-knapp
(define (set-undo-fn! fn)
  (set! undo-fn fn))

;; Sätt svårighetsgrad till svår
(define (set-switch-grid-fn! fn)
  (set! switch-grid-fn fn))

;; Sätt svårighetsgrad till lätt
(define (set-switch-difficulty-fn! fn)
  (set! switch-difficulty-fn fn))

;; AI/manuell-switch
(define (set-switch-player-fn! fn)
  (set! switch-player-fn fn))

;; Ställning
(define (set-score-fn! fn)
  (set! score-fn fn))

;; Hjälp
(define (set-help-fn! fn)
  (set! help-fn fn))

;; set-quit-fn!: (void -> void) -> void
;; Sets the callback function to be called when the
;; quit button is pressed.
(define (set-quit-fn! fn)
  (set! quit-fn fn))

;; set-restart-fn!: (void -> void) -> void
;; Sets the callback function to be called when the
;; restart button is pressed.
(define (set-restart-fn! fn)
  (set! restart-fn fn))

;; get-next-move: void -> (cons integer integer)
;; Returns the next move of the user (i.e. the square
;; that the user clicked on. The returned pair contains
;; the x coordinate in the car position and the y coordinate
;; in the cdr position.
;; Note, this functions blocks until the user has performed
;; a move.
(define (get-next-move)
  (add-listener! move-callback)
  (set! is-getting-next-move? #t)
  (semaphore-wait synch)
  (set! is-getting-next-move? #f)
  (del-listener! move-callback)
  last-move)

;; abort-get-next-move: void -> void
;; Aborts the call to get-next-move so that
;; it returns immediately with the symbol
;; aborted.
(define (abort-get-next-move)
  ;; we only abort if get-next-move has
  ;; been called.
  (when is-getting-next-move?
    (set! last-move 'aborted)
    (semaphore-post synch)))



;; add-listener!: (integer x integer -> void) -> void
;; Takes a callback function that will be called when
;; the user clicks on a board-position and adds it to
;; the list of callbacks.
(define (add-listener! listener)
  (set! listeners (cons listener listeners)))

;; del-listener!: (integer x integer -> void) -> void
;; Takes a callback function and removes if from the list
;; of callbacks.
(define (del-listener! listener)
  (set! listeners (remove listener listeners)))


;; General utilities -------------------------------------------------

(define (repeat n f)
  (when (not (= n 0))
    (f n)
    (repeat (- n 1) f)))

(define (do-vector v f)
  (define len (vector-length v))
  (let loop ((n 0))
    (when (< n len)
      (f n (vector-ref v n))
      (loop (+ n 1)))))

;; Graphical utilities -------------------------------------------------


(define glue%
  (class panel%
    (super-instantiate ()
      (stretchable-width #t)
      (stretchable-height #t))))

(define (get-pen color width style)
  (send the-pen-list find-or-create-pen color width style))

(define (get-brush color style)
  (send the-brush-list find-or-create-brush color style))

(define (set-color! dc brush pen)
  (send dc set-brush brush)
  (send dc set-pen pen))


;; Board management ----------------------------------------------------

(define board (make-vector 64 'NONE))

(define (get-piece x y)
  (vector-ref board (+ (* y 8) x)))


;; Callback management --------------------------------------------------

(define listeners '())
(define undo-fn (lambda () #f))
(define switch-grid-fn (lambda () #f))
(define switch-difficulty-fn (lambda () #f))
(define switch-player-fn (lambda () #f))
(define score-fn (lambda () #f))
(define help-fn (lambda () #f))
(define quit-fn (lambda () #f))
(define restart-fn (lambda () #f))

(define (propagate x y)
  (set! x (inexact->exact (floor (/ x tile-size))))
  (set! y (inexact->exact (floor (/ y tile-size))))
  (when (and (>= x 0) (>= y 0)
             (< x 8) (< y 8))
    (for-each (lambda (f)
                (f x y)) listeners)))


;; Moves ----------------------------------------------------------------

(define synch (make-semaphore))
(define is-getting-next-move? #f)
(define last-move #f)
(define (move-callback x y)
  (set! last-move (cons x y))
  (semaphore-post synch))


;; Colors, pens and brushes --------------------------------------------

(define highlight-color  (instantiate color% (70 150 0)))
(define board-color      (instantiate color% (70 130 40)))
(define background-color (instantiate color% (20 70 30)))
(define black-piece-pen   (get-pen   "BLACK"               1 'solid))
(define black-piece-brush (get-brush "BLACK"                 'solid))
(define white-piece-pen   (get-pen   "WHITE"               1 'solid))
(define white-piece-brush (get-brush "WHITE"                 'solid))
(define highlight-pen     (get-pen   highlight-color       1 'solid))
(define highlight-brush   (get-brush highlight-color         'solid))
(define grid-pen          (get-pen   "BLACK"               1 'solid))
(define grid-brush        (get-brush "BLACK"                 'solid))
(define background-pen    (get-pen   background-color      1 'solid))
(define background-brush  (get-brush background-color        'solid))
(define board-pen         (get-pen   board-color           1 'solid))
(define board-brush       (get-brush board-color             'solid))

;; Graphics -------------------------------------------------------------

(define get-x car)
(define get-y cdr)
(define preferred-tile-size 60)
(define tile-size preferred-tile-size)
(define scale-factor 0.8)
(define offset 0)
(define piece-size (* scale-factor preferred-tile-size))
(define highlights '())

;; A double-buffered board-view.
(define board-view%
  (class canvas%
    (override on-size
              on-paint
              on-event
              on-superwindow-show
              on-focus)

    (init-field parent)
    (init-field (paint-callback #f))
    (init-field (invalidated-buffer #f))
    (init-field (lock (make-semaphore 1)))

    (define gbuffer #f)
    (define gbuffer-dc #f)

    (define (make-new-bitmap w h bitmap)
      (let ((res
             (if (and bitmap (send bitmap ok?))
                 bitmap
                 (make-new-bitmap w h (make-object bitmap% w h #f)))))
        bitmap))

    (define (on-paint)
      (define dc (send this get-dc))
      (semaphore-wait lock)
      (when (not gbuffer)
        (call-with-values
         (lambda () (send dc get-size))
         (lambda (w h)
           (set! gbuffer
                 (make-new-bitmap (inexact->exact w)
                                  (inexact->exact h)
                                  (make-object bitmap%
                                    (inexact->exact w)
                                    (inexact->exact h)
                                    #f)))
           (set! gbuffer-dc (instantiate bitmap-dc% (gbuffer)))
           (when invalidated-buffer
             (invalidated-buffer)))))

      (send gbuffer-dc set-origin 0 0)
      (draw-board gbuffer-dc)

      (send dc draw-bitmap gbuffer 0 0)
      (semaphore-post lock))

    (define (on-superwindow-show shown?)
      (when shown?
        (semaphore-wait lock)
        (set! gbuffer #f)
        (semaphore-post lock)))

    (define (on-size w h)
      (semaphore-wait lock)
      (set! gbuffer #f)
      (semaphore-post lock))

    (define (on-event event)
      (when (send event button-up?)
        (propagate (send event get-x) (send event get-y))))

    (define (on-focus x)
      (on-paint))
    (super-instantiate (parent))))


(define (draw-highlights dc)
  (set-color! dc highlight-brush highlight-pen)
  (for-each (lambda (coord)
              (send dc draw-rectangle
                    (* tile-size (get-x coord))
                    (* tile-size (get-y coord))
                    tile-size
                    tile-size))
            highlights))

(define (draw-grid dc)
  (set-color! dc grid-brush grid-pen)
  (repeat 8 (lambda (n)
              (send dc draw-line (* n tile-size) 0
                    (* n tile-size) (* tile-size 8))
              (send dc draw-line 0 (* n tile-size) (* tile-size 8)
                    (* n tile-size)))))

(define (draw-pieces dc)
  (do-vector board
             (lambda (index element)
               (when (not (eq? element 'NONE))
                 (if (eq? element 'BLACK)
                     (set-color! dc
                                 black-piece-brush
                                 black-piece-pen)
                     (set-color! dc
                                 white-piece-brush
                                 white-piece-pen))
                 (send dc draw-ellipse
                       (+ offset (* tile-size
                                    (remainder index 8)))
                       (+ offset (* tile-size
                                    (floor (/ index 8))))
                       piece-size piece-size)))))

(define (draw-board-background dc)
  (set-color! dc board-brush board-pen)
  (send dc draw-rectangle 0 0 (* tile-size 8) (* tile-size 8)))

(define (draw-board dc)
  ;; recalculate sizes and clear the buffer
  (call-with-values
   (lambda () (send dc get-size))
   (lambda (width height)
     (set! tile-size (/ (- (min width height) 7) 8))
     (set! piece-size (* tile-size scale-factor))
     (set! offset (+ 1 (inexact->exact
                        (floor (/ (- tile-size piece-size) 2)))))
     (set-color! dc background-brush background-pen)
     (send dc draw-rectangle 0 0 width height)))

  (draw-board-background dc)
  (draw-highlights dc)
  (draw-grid dc)
  (draw-pieces dc))


;; Build the user interface -------------------------------------------
(define win (let ((new-es (make-eventspace)))
              (parameterize ((current-eventspace new-es))
                (instantiate frame% ("Othello")))))

(define main-panel (instantiate horizontal-panel% (win)
                     (alignment '(center top))))

(define control-panel (instantiate vertical-panel% (main-panel)
                        (alignment '(center top))
                        (stretchable-width #f)
                        (stretchable-height #t)))

(define canvas (instantiate board-view% (main-panel)))

(define (refresh)
  (send canvas on-paint))

(send canvas min-height (+ 8 (* preferred-tile-size 8)))
(send canvas min-width (+ 8 (* preferred-tile-size 8)))
(instantiate glue% (control-panel))

(instantiate button%
  ("Ångra" control-panel (lambda (e b) (undo-fn)))
  (horiz-margin 2)
  (vert-margin 2)
  (stretchable-width #t))

(instantiate button%
  ("Assist På/Av" control-panel (lambda (e b) (switch-grid-fn)))
  (horiz-margin 2)
  (vert-margin 2)
  (stretchable-width #t))

(instantiate button%
  ("Lätt/Svår" control-panel (lambda (e b) (switch-difficulty-fn)))
  (horiz-margin 2)
  (vert-margin 2)
  (stretchable-width #t))

(instantiate button%
  ("Ställning" control-panel (lambda (e b) (score-fn)))
  (horiz-margin 2)
  (vert-margin 2)
  (stretchable-width #t))

(instantiate button%
  ("Spelare/AI" control-panel (lambda (e b) (switch-player-fn)))
  (horiz-margin 2)
  (vert-margin 2)
  (stretchable-width #t))

(instantiate button%
  ("Starta om" control-panel (lambda (e b) (restart-fn)))
  (horiz-margin 2)
  (vert-margin 2)
  (stretchable-width #t))

(instantiate button%
  ("Hjälp" control-panel (lambda (e b) (help-fn)))
  (horiz-margin 2)
  (vert-margin 2)
  (stretchable-width #t))

(instantiate button%
  ("Avsluta" control-panel (lambda (e b) (quit-fn)))
  (horiz-margin 2)
  (vert-margin 2)
  (stretchable-width #t))






;; ADT.scm






;; Skapa rad och kolumn ------------------------------------------------------------------

; Skapa rad

(define (skapa-tom-rad n)
  (if (= n 1)
      (mcons tom '())
      (mcons tom (skapa-tom-rad (- n 1)))))

; Skapa plan

(define (skapa-spelplan rad kolumn)
  (if (= rad 1)
      (mcons (skapa-tom-rad kolumn) '())
      (mcons (skapa-tom-rad kolumn) (skapa-spelplan (- rad 1) kolumn))))

;; Globala Variabler ------------------------------------------------------------------

; Spelplanens storlek
(define storlek 8)

; Istället för svart och vit
(define svart 'X)
(define vit 'O)
(define tom '-)

; Vems tur det är, svart börjar
(define spelare svart)

; Spelplanen
(define othello (skapa-spelplan storlek storlek))

; Skapar spelplan för Ångra-drag-funktionen
(define tidigare-spelplan (skapa-spelplan storlek storlek))

; Svårighetsgrad, lätt eller svår

(define svårighetsgrad 'svår)

; AI på eller av
(define AI? #t)

; Hjälp-grid av eller på
(define grid? #f)


;; Selektorer ------------------------------------------------------------------

; Välj rad
(define (välj-rad n spelplan)
  (if (= n 1)
      (mcar spelplan)
      (välj-rad (- n 1) (mcdr spelplan))))

; Välj kolumn
(define (välj-kolumn n rad)
  (if (= n 1)
      rad
      (välj-kolumn (- n 1) (mcdr rad))))

; Hämtar rutan + resten av raden, kan muteras med set-mcar!
(define (hämta-ruta radnr kolnr spelplan)
  (välj-kolumn kolnr (välj-rad radnr spelplan)))

; Hämtar värdet i rutan (svart, vit, tom)
(define (hämta-värde radnr kolnr spelplan)
  (mcar (välj-kolumn kolnr (välj-rad radnr spelplan))))

; Tar fram koordinater för vektorbaserad spelplan
(define kolumn car)
(define rad cdr)

; Visar spelplanen i fina rader
(define (visa-spelplan spelplan)
  (if (null? (mcdr spelplan))
      (display (mcar spelplan))
      (begin (display (mcar spelplan))
             (newline)
             (visa-spelplan (mcdr spelplan)))))

; Visar färg på svenska
(define (visa-färg färg)
  (cond ((eq? färg vit) 'Vit)
        ((eq? färg svart) 'Svart)
        (else 'tom)))

; Visar motsatta färgen på svenska
(define (visa-färg-omvänd färg)
  (cond ((eq? färg svart) 'Vit)
        ((eq? färg vit) 'Svart)
        (else 'tom)))

; Visar färg på engelska
(define (visa-färg-eng färg)
  (cond ((eq? färg vit) 'WHITE)
        ((eq? färg svart) 'BLACK)
        (else 'NONE)))

; Hämtar omvända färgen
(define (hämta-omvänd-färg färg)
  (if (eq? färg vit)
      svart
      vit))


;; Mutatorer ------------------------------------------------------------------

; Placerar nya pjäser på tomma rutor
(define (placera-pjäs radnr kolnr färg spelplan AI-fn?)
  (set-mcar! (hämta-ruta radnr kolnr spelplan) färg)
  (unless AI-fn?
    (set-piece-at! (- kolnr 1) (- radnr 1) (visa-färg-eng färg))))

; Vänder en spelpjäs
(define (byt-färg radnr kolnr spelplan AI-fn?)
  (cond ((eq? (hämta-värde radnr kolnr spelplan) svart)
         (begin (set-mcar! (hämta-ruta radnr kolnr spelplan) vit)
                (unless AI-fn?
                  (set-piece-at! (- kolnr 1) (- radnr 1) 'WHITE))))
        ((eq? (hämta-värde radnr kolnr spelplan) vit)
         (begin (set-mcar! (hämta-ruta radnr kolnr spelplan) svart)
                (unless AI-fn?
                  (set-piece-at! (- kolnr 1) (- radnr 1) 'BLACK))))
        (else (begin (display "Det finns ingen pjäs här")
                     (newline)))))

; Växla spelare
(define (växla-spelare)
  (if (eq? spelare vit)
      (set! spelare svart)
      (set! spelare vit)))

;; Knappar ------------------------------------------------------------------

; Sätter svårighetsgraden till svår
(set-switch-grid-fn!
 (lambda ()
   (if grid?
       (begin (set! grid? #f)
              (set-highlights! '()))
       (begin (set! grid? #t)
              (set-highlights! (giltiga-drag spelare othello))))))

; Sätter svårighetsgraden till lätt
(set-switch-difficulty-fn!
 (lambda ()
   (if (eq? svårighetsgrad 'svår)
       (begin (set! svårighetsgrad 'lätt)
              (display "AI:n är nu lätt"))
       (begin (set! svårighetsgrad 'svår)
              (display "AI:n är nu svår")))
   (newline)))

; Byt spelare mellan AI och människa
(set-switch-player-fn!
 (lambda ()
   (if AI?
       (begin (display "AI:n stängs av...")
              (newline)
              (set! AI? #f))
       (begin (display "AI:n sätts på...")
              (set! AI? #t)
              (newline)))))

; Restart-funktion
(set-restart-fn!
 (lambda ()
   (display "Startar om...")
   (newline)

   ; Rensar den grafiska spelplanen
   (clear-board!)

   ; Skapar ny spelplan
   (set! othello (skapa-spelplan storlek storlek))
   (set! spelare svart)
   (starta-spel)))

; Stänger spelplanen
(set-quit-fn! hide-board)

; Hjälp-knappen
(set-help-fn!
 (lambda ()
   (message-box "Hjälp"
                "Knappar
Assist På/Av: Sätter på eller av hjälpmedlet
Lätt/Svår: Växla mellan lätt och svår motståndare
Ställning: Visar aktuell ställning
Spelare/AI: Växlar mellan 'manuell vs manuell' och 'manuell vs AI'
Starta om: Startar om spelomgången med spelaren satt till svart
Hjälp: Visar det här meddelandet
Avsluta: Stänger spelplanen" #f '(ok))))

; Ställningsknappen
(set-score-fn!
 (lambda ()
   (message-box "Ställning"
                (string-append "Vit: " (number->string (räkna-pjäser vit othello))
                               "\n"
                               "Svart: " (number->string (räkna-pjäser svart othello)))
                #f '(ok))))

; Ångra
(set-undo-fn!
 (lambda ()
   (if (check-game-over)
       (begin (display "Du kan inte ångra det sista draget")
              (newline))
       (begin (kopiera-spelplan othello tidigare-spelplan #f)
              (when grid?
                (set-highlights! (giltiga-drag spelare othello)))))))


;; Övrigt ------------------------------------------------------------------

; Titel
(set-title! "Othello av Johan och Robin")

; Det finns inget giltigt drag
(define (kan-inte-lägga spelare)
  (display (visa-färg spelare))
  (display " kan inte lägga. Det är ")
  (display (visa-färg-omvänd spelare))
  (display " spelares tur igen.")
  (newline))


; Räknar antalet pjäser för en viss färg
(define (räkna-pjäser färg spelplan)
  (let ((antal-pjäser 0)
        (radnr storlek)
        (kolnr storlek))

    (define (räkna-pjäser-loop)
      (cond ((and (= radnr 1) (= kolnr 1))
             (begin (when (eq? (hämta-värde radnr kolnr spelplan) färg)
                      (set! antal-pjäser (+ antal-pjäser 1)))
                    antal-pjäser))
            ((= kolnr 1)
             (begin (when (eq? (hämta-värde radnr kolnr spelplan) färg)
                      (set! antal-pjäser (+ antal-pjäser 1)))
                    (set! radnr (- radnr 1))
                    (set! kolnr storlek)
                    (räkna-pjäser-loop)))
            (else (begin (when (eq? (hämta-värde radnr kolnr spelplan) färg)
                           (set! antal-pjäser (+ antal-pjäser 1)))
                         (set! kolnr (- kolnr 1))
                         (räkna-pjäser-loop)))))
    (räkna-pjäser-loop)))

; Räknar alla pjäser på en spelplan

(define (räkna-alla-pjäser spelplan)
  (+ (räkna-pjäser vit spelplan) (räkna-pjäser svart spelplan)))

; Placerar pjäserna i mitten och slå på assisten om den är på.
(define (starttillstånd)
  (placera-pjäs (/ storlek 2)
                (/ storlek 2)
                svart othello #f)
  (placera-pjäs (+ (/ storlek 2) 1)
                (+ (/ storlek 2) 1)
                svart othello #f)
  (placera-pjäs (/ storlek 2)
                (+ (/ storlek 2) 1)
                vit othello #f)
  (placera-pjäs (+ (/ storlek 2) 1)
                (/ storlek 2)
                vit othello #f)
  (when grid?
    (set-highlights! (giltiga-drag spelare othello))))

; Funktion som returnerar sannings-värde om draget är dåligt eller inte,
; refererar till platserna runt hörnen. Om man har hörnet är draget inte dåligt,
; om man inte har hörnet eller om motståndaren har hörnet är draget dåligt.
(define (dåligt-drag? radnr kolnr färg spelplan)
  (if (or (if (equal? (hämta-värde 1 1 spelplan) färg)
              #f
              (or (equal? (cons radnr kolnr) (cons 1 2))
                  (equal? (cons radnr kolnr) (cons 2 2))
                  (equal? (cons radnr kolnr) (cons 2 1))))

          (if (equal? (hämta-värde 1 8 spelplan) färg)
              #f
              (or (equal? (cons radnr kolnr) (cons 1 7))
                  (equal? (cons radnr kolnr) (cons 2 7))
                  (equal? (cons radnr kolnr) (cons 2 8))))

          (if (equal? (hämta-värde 8 1 spelplan) färg)
              #f
              (or (equal? (cons radnr kolnr) (cons 7 1))
                  (equal? (cons radnr kolnr) (cons 7 2))
                  (equal? (cons radnr kolnr) (cons 8 2))))

          (if (equal? (hämta-värde 8 8 spelplan) färg)
              #f
              (or (equal? (cons radnr kolnr) (cons 7 8))
                  (equal? (cons radnr kolnr) (cons 7 7))
                  (equal? (cons radnr kolnr) (cons 8 7)))))
      #t
      #f))

;; Startar spelet
(define (starta-spel)
  (show-board)
  (starttillstånd))





;; Giltigt-drag.scm


(define (giltigt-drag? radnr kolnr färg spelplan)

  ; Kolla Väderstreck
  (define (kolla-väderstreck radnr kolnr färg spelplan)
    (if (or (kolla-riktning radnr kolnr 0 -1 färg spelplan)   ;; Kollar nord
            (kolla-riktning radnr kolnr 1 -1 färg spelplan)   ;; Kollar nordost
            (kolla-riktning radnr kolnr 1 0 färg spelplan)    ;; Kollar öst
            (kolla-riktning radnr kolnr 1 1 färg spelplan)    ;; Kollar sydost
            (kolla-riktning radnr kolnr 0 1 färg spelplan)    ;; Kollar syd
            (kolla-riktning radnr kolnr -1 1 färg spelplan)   ;; Kollar sydväst
            (kolla-riktning radnr kolnr -1 0 färg spelplan)   ;; Kollar väst
            (kolla-riktning radnr kolnr -1 -1 färg spelplan)) ;; Kollar nordväst
        #t
        #f))

  ; Kollar giltigt drag
  (cond ((or (> radnr storlek) (> kolnr storlek)) #f)  ;; På spelplanen?
        ((not (eq? (hämta-värde radnr kolnr spelplan) tom)) #f) ;; Tom ruta?
        ((not (kolla-väderstreck radnr kolnr färg spelplan)) #f);;Godkänt drag?
        (else #t)))

;; Kolla riktning
(define (kolla-riktning radnr kolnr x-led y-led färg spelplan)

  (define (kolla-riktning-loop radnr kolnr x-led y-led färg spelplan)
    (cond
      ; Färdig, ogiltigt drag
      ((or (if (= -1 y-led) (= radnr 2) #f)
           (if (= -1 x-led) (= kolnr 2) #f)
           (if (= 1 y-led) (= radnr (- storlek 1)) #f)
           (if (= 1 x-led) (= kolnr (- storlek 1)) #f)) #f)

      ; Färdig, giltigt drag
      ((eq? (hämta-värde (+ radnr (* 2 y-led))
                         (+ kolnr (* 2 x-led)) spelplan) färg) #t)

      ; Tom ruta?
      ((eq? (hämta-värde (+ radnr (* 2 y-led))
                         (+ kolnr (* 2 x-led)) spelplan) tom) #f)

      (else
       (kolla-riktning-loop (+ radnr y-led)
                            (+ kolnr x-led) x-led y-led färg spelplan))))

  (cond

    ; Första eller andra raden?
    ((or (if (= -1 y-led) (<= radnr 2) #f)
         (if (= -1 x-led) (<= kolnr 2) #f)
         (if (= 1 y-led) (>= radnr (- storlek 1)) #f)
         (if (= 1 x-led) (>= kolnr (- storlek 1)) #f)) #f)

    ; Samma färg?
    ((eq? (hämta-värde (+ radnr y-led) (+ kolnr x-led) spelplan) färg) #f)

    ; Tom ruta?
    ((eq? (hämta-värde (+ radnr y-led) (+ kolnr x-led) spelplan) tom) #f)

    ; Två steg eller mer...
    (else (kolla-riktning-loop radnr kolnr x-led y-led färg spelplan))))

;; Kollar alla giltiga drag och returnerar en lista med cons-par,
;; modulerade för den grafiska spelplanen
(define (giltiga-drag färg spelplan)
  (let ((radnr storlek)
        (kolnr storlek)
        (giltiga-drag-lista '()))

    (define (giltiga-drag-loop)

      (cond ((and (= radnr 1) (= kolnr 1))
             (when (giltigt-drag? radnr kolnr färg spelplan)
               (set! giltiga-drag-lista
                     (cons (cons (- kolnr 1) (- radnr 1)) giltiga-drag-lista)))
             giltiga-drag-lista)

            ((= kolnr 1)
             (begin (when (giltigt-drag? radnr kolnr färg spelplan)
                      (set! giltiga-drag-lista
                            (cons (cons (- kolnr 1) (- radnr 1)) giltiga-drag-lista)))
                    (set! radnr (- radnr 1))
                    (set! kolnr storlek)
                    (giltiga-drag-loop)))

            (else
             (begin (when (giltigt-drag? radnr kolnr färg spelplan)
                      (set! giltiga-drag-lista
                            (cons (cons (- kolnr 1) (- radnr 1)) giltiga-drag-lista)))
                    (set! kolnr (- kolnr 1))
                    (giltiga-drag-loop)))))
    (giltiga-drag-loop)))

;; Finns det något giltigt drag?
(define (finns-giltigt-drag? färg spelplan)
  (let ((radnr 1)
        (kolnr 1))

    (define (skanna-spelplan)
      (cond ((and (= radnr storlek) (= kolnr storlek))
             (giltigt-drag? radnr kolnr färg spelplan))
            ((= kolnr (+ storlek 1))
             (begin (set! kolnr 1)
                    (set! radnr (+ radnr 1))
                    (skanna-spelplan)))
            (else (if (giltigt-drag? radnr kolnr färg spelplan)
                      #t
                      (begin (set! kolnr (+ kolnr 1))
                             (skanna-spelplan))))))
    (skanna-spelplan)))




;; Utfor-drag.scm



;; Utför ett drag
(define (utför-drag radnr kolnr färg spelplan AI-fn?)

  (define (vänd-riktning radnr kolnr x-led y-led färg spelplan)
    (unless (eq? (hämta-värde (+ radnr y-led) (+ kolnr x-led) spelplan) färg)
      (begin (byt-färg (+ radnr y-led) (+ kolnr x-led) spelplan AI-fn?)
             (vänd-riktning (+ radnr y-led) (+ kolnr x-led) x-led y-led färg spelplan))))

  (when (kolla-riktning radnr kolnr 0 -1 färg spelplan)
    (vänd-riktning radnr kolnr 0 -1 färg spelplan)) ;; Vänder nord

  (when (kolla-riktning radnr kolnr 1 -1 färg spelplan)
    (vänd-riktning radnr kolnr 1 -1 färg spelplan)) ;; Vänder nordost

  (when (kolla-riktning radnr kolnr 1 0 färg spelplan)
    (vänd-riktning radnr kolnr 1 0 färg spelplan)) ;; Vänder öst

  (when (kolla-riktning radnr kolnr 1 1 färg spelplan)
    (vänd-riktning radnr kolnr 1 1 färg spelplan)) ;; Vänder sydost

  (when (kolla-riktning radnr kolnr 0 1 färg spelplan)
    (vänd-riktning radnr kolnr 0 1 färg spelplan)) ;; Vänder syd

  (when (kolla-riktning radnr kolnr -1 1 färg spelplan)
    (vänd-riktning radnr kolnr -1 1 färg spelplan)) ;; Vänder sydväst

  (when (kolla-riktning radnr kolnr -1 0 färg spelplan)
    (vänd-riktning radnr kolnr -1 0 färg spelplan)) ;; Vänder väst

  (when (kolla-riktning radnr kolnr -1 -1 färg spelplan)
    (vänd-riktning radnr kolnr -1 -1 färg spelplan)) ;; Vänder nordväst

  (placera-pjäs radnr kolnr färg spelplan AI-fn?)

  (unless (or AI-fn? (not grid?))
    (set-highlights! (giltiga-drag (hämta-omvänd-färg färg) spelplan))))




;; AI.scm


(define (AI färg spelplan)

  ;; Prioritera hörnen
  (define (hörn färg spelplan)
    (cond ((giltigt-drag? 1 1 färg spelplan)
           (utför-drag 1 1 färg spelplan #f))
          ((giltigt-drag? 1 storlek färg spelplan)
           (utför-drag 1 storlek färg spelplan #f))
          ((giltigt-drag? storlek 1 färg spelplan)
           (utför-drag storlek 1 färg spelplan #f))
          ((giltigt-drag? storlek storlek färg spelplan)
           (utför-drag storlek storlek färg spelplan #f))
          (else #f)))

  ; Kolla de fyra rutorna i mitten på en kant
  ; radnr, kolnr är listor med koordinater
  ; Exempel: toppen-4:
  ; radnr = (list 1 1 1 1)
  ; kolnr = (list 3 4 5 6)
  (define (sida-4 radnr-lista kolnr-lista)
    (cond ((giltigt-drag? (car radnr-lista) (car kolnr-lista) färg spelplan)
           3)
          ((giltigt-drag? (cadr radnr-lista) (cadr kolnr-lista) färg spelplan)
           4)
          ((giltigt-drag? (caddr radnr-lista) (caddr kolnr-lista) färg spelplan)
           5)
          ((giltigt-drag? (cadddr radnr-lista) (cadddr kolnr-lista) färg spelplan)
           6)
          (else
           #f)))

  ;; Slumpar ett drag
  (define (slumpa-drag)
    (let ((a (+ (random 8) 1))
          (b (+ (random 8) 1)))
      (if (finns-giltigt-drag? färg spelplan)
          (if (giltigt-drag? a b färg spelplan)
              (utför-drag a b färg spelplan #f)
              (slumpa-drag))
          (begin (display "Det finns inga giltiga drag för ")
                 (display (visa-färg färg))))))

  ;; AI:ns prioriteringar
  (define (tänk)

    (if (eq? svårighetsgrad 'svår)
        ; Kan den lägga i ett hörn?
        (unless (hörn färg spelplan)

          ; Kan den lägga på en sida?
          ; Radnr och Kolnr i utför-drag definieras av antingen funktionen sida-4 eller
          ; direkt i inparametern i utför-drag
          (cond
            ; Norra sidan
            ((sida-4 (list 1 1 1 1) (list 3 4 5 6))
             (utför-drag 1 (sida-4 (list 1 1 1 1) (list 3 4 5 6)) färg spelplan #f))
            ; Östra sidan
            ((sida-4 (list 3 4 5 6) (list 8 8 8 8))
             (utför-drag (sida-4 (list 3 4 5 6) (list 8 8 8 8)) 8 färg spelplan #f))
            ; Södra sidan
            ((sida-4 (list 8 8 8 8) (list 3 4 5 6))
             (utför-drag 8 (sida-4 (list 8 8 8 8) (list 3 4 5 6)) färg spelplan #f))
            ; Västra sidan
            ((sida-4 (list 3 4 5 6) (list 1 1 1 1))
             (utför-drag (sida-4 (list 3 4 5 6) (list 1 1 1 1)) 1 färg spelplan #f))

            (else
             ; Om antalet pjäser är mellan 15 och 30 och AI:n ligger under,
             ; eller om antalet pjäser är större än 30,
             ; så prioriterar AI:n drag som ger flest pjäser,
             ; annars drag som ger minst giltiga drag till motspelaren.
             (if (or (and (> (räkna-alla-pjäser spelplan) 15)
                          (> (räkna-pjäser (hämta-omvänd-färg färg) spelplan)
                             (räkna-pjäser färg spelplan)))
                     (> (räkna-alla-pjäser spelplan) 30))
                 (utför-drag (car (bästa-drag färg spelplan 'pjäs))
                             (cdr (bästa-drag färg spelplan 'pjäs)) färg spelplan #f)
                 (utför-drag (car (bästa-drag färg spelplan 'drag))
                             (cdr (bästa-drag färg spelplan 'drag)) färg spelplan #f)))))
        (slumpa-drag)))

  ; Kör AI
  (tänk)

  ; Funktion som gör att AI:n kan göra flera drag i rad om spelaren inte kan lägga.
  ; Om ingen kan lägga skickar game-over.
  (cond ((finns-giltigt-drag? (hämta-omvänd-färg färg) spelplan)
         (begin (växla-spelare)
                (spelloop)))
        ((finns-giltigt-drag? färg spelplan)
         (begin (kan-inte-lägga (hämta-omvänd-färg färg))
                (when grid?
                  (set-highlights! (giltiga-drag färg spelplan)))
                (AI färg spelplan)))
        (else (game-over))))





;; AI-supplement.scm


;; Räknar antal giltiga drag för en färg på en spelplan.
(define (räkna-drag färg spelplan)
  (let ((antal-drag 0)
        (kolnr storlek)
        (radnr storlek))

    (define (räkna-drag-loop)
      (cond ((and (= radnr 1) (= kolnr 1))
             (begin (when (giltigt-drag? radnr kolnr färg spelplan)
                      (set! antal-drag (+ antal-drag 1)))
                    antal-drag))
            ((= kolnr 1)
             (begin (when (giltigt-drag? radnr kolnr färg spelplan)
                      (set! antal-drag (+ antal-drag 1)))
                    (set! radnr (- radnr 1))
                    (set! kolnr storlek)
                    (räkna-drag-loop)))
            (else
             (begin (when (giltigt-drag? radnr kolnr färg spelplan)
                      (set! antal-drag (+ antal-drag 1)))
                    (set! kolnr (- kolnr 1))
                    (räkna-drag-loop)))))
    (räkna-drag-loop)))

;; Kopierar och skriver över en spelplan.
(define (kopiera-spelplan ny-spelplan spelplan AI-fn?)

  (let ((radnr storlek)
        (kolnr storlek))

    (define (kopiera-spelplan-loop)
      (cond ((and (= radnr 1) (= kolnr 1))
             (placera-pjäs radnr kolnr (hämta-värde radnr kolnr spelplan)
                           ny-spelplan AI-fn?))
            ((= kolnr 1)
             (begin (placera-pjäs radnr kolnr (hämta-värde radnr kolnr spelplan)
                                  ny-spelplan AI-fn?)
                    (set! radnr (- radnr 1))
                    (set! kolnr storlek)
                    (kopiera-spelplan-loop)))
            (else
             (begin (placera-pjäs radnr kolnr (hämta-värde radnr kolnr spelplan)
                                  ny-spelplan AI-fn?)
                    (set! kolnr (- kolnr 1))
                    (kopiera-spelplan-loop)))))

    (kopiera-spelplan-loop)))


;; Hittar bästa draget utifrån antal giltiga drag för motspelaren efter det nya draget,
;; eller utifrån vilket drag som vänder flest pjäser
;; (beror på inparametern pjäs/drag), returnerar ett cons-par.
(define (bästa-drag färg spelplan pjäs/drag)

  (let ((temp-spelplan (skapa-spelplan storlek storlek))
        (bra-spelplan (skapa-spelplan storlek storlek)))

    ; Sätt spelplanerna till spelplanen som används.
    (kopiera-spelplan temp-spelplan spelplan #t)
    (kopiera-spelplan bra-spelplan spelplan #t)

    (let ((temp-radnr storlek)
          (temp-kolnr storlek)
          (bra-radnr storlek)
          (bra-kolnr storlek)
          (radnr storlek)
          (kolnr storlek))

      (define (bästa-drag-loop)


        (cond
          ; Om det gjorts ett drag på temp-spelplan
          ; men inte på bra-spelplan.
          ((< (räkna-alla-pjäser bra-spelplan)
              (räkna-alla-pjäser temp-spelplan))
           ; Överför draget från temp-spelplan till bra-spelplan och ändra koordinaterna.
           (begin
             (kopiera-spelplan bra-spelplan temp-spelplan #t)
             (kopiera-spelplan temp-spelplan spelplan #t)
             (set! bra-radnr temp-radnr)
             (set! bra-kolnr temp-kolnr)
             (bästa-drag-loop)))

          ; Om sista rutan.
          ((and (= radnr 1) (= kolnr 1))
           (begin
             ; Om giltigt drag, utför drag och ändra koordinater.
             (when (giltigt-drag? radnr kolnr färg spelplan)
               (begin (utför-drag radnr kolnr färg temp-spelplan #t)
                      (set! temp-radnr radnr)
                      (set! temp-kolnr kolnr)))

             (if
              ; Om en och endast en av spelplanerna har lagt pjäser på en dålig koordinat
              (or (and (dåligt-drag? temp-radnr temp-kolnr färg spelplan)
                       (not (dåligt-drag? bra-radnr bra-kolnr färg spelplan)))
                  (and (dåligt-drag? bra-radnr bra-kolnr färg spelplan)
                       (not (dåligt-drag? temp-radnr temp-kolnr färg spelplan))))

              ; Om bra-spelplan har lagt på dålig koordinat
              (if (dåligt-drag? bra-radnr bra-kolnr färg spelplan)
                  (cons temp-radnr temp-kolnr)
                  (cons bra-radnr bra-kolnr))
              ; Om temp-spelplan är bättre, skicka de koordinaterna.
              ; Annars skicka bra-spelplan:s koordinater.
              (if
               ; Beror på inparametern pjäs/drag
               (if (eq? pjäs/drag 'drag)
                   (< (räkna-drag (hämta-omvänd-färg färg) temp-spelplan)
                      (räkna-drag (hämta-omvänd-färg färg) bra-spelplan))
                   (< (räkna-pjäser färg bra-spelplan)
                      (räkna-pjäser färg temp-spelplan)))
               (cons temp-radnr temp-kolnr)
               (cons bra-radnr bra-kolnr)))))

          ; Om det utförts drag på de båda temporära spelplanerna.
          ((and (< (räkna-alla-pjäser spelplan) (räkna-alla-pjäser temp-spelplan))
                (< (räkna-alla-pjäser spelplan) (räkna-alla-pjäser bra-spelplan)))

           (begin (if
                   ; Om en och endast en av spelplanerna har lagt pjäser på en dålig koordinat
                   (or (and (dåligt-drag? temp-radnr temp-kolnr färg spelplan)
                            (not (dåligt-drag? bra-radnr bra-kolnr färg spelplan)))
                       (and (dåligt-drag? bra-radnr bra-kolnr färg spelplan)
                            (not (dåligt-drag? temp-radnr temp-kolnr färg spelplan))))

                   ; Om bra-spelplan har lagt på dålig koordinat
                   (when (dåligt-drag? bra-radnr bra-kolnr färg spelplan)
                     (begin (kopiera-spelplan bra-spelplan temp-spelplan #t)
                            (set! bra-radnr temp-radnr)
                            (set! bra-kolnr temp-kolnr)))

                   ; Om temp-spelplan är bättre, kopiera och skriv över bra-spelplan.
                   (when
                       ; Beror på inparametern pjäs/drag
                       (if (eq? pjäs/drag 'drag)
                           (< (räkna-drag (hämta-omvänd-färg färg) temp-spelplan)
                              (räkna-drag (hämta-omvänd-färg färg) bra-spelplan))
                           (< (räkna-pjäser färg bra-spelplan)
                              (räkna-pjäser färg temp-spelplan)))

                     (begin (kopiera-spelplan bra-spelplan temp-spelplan #t)
                            (set! bra-radnr temp-radnr)
                            (set! bra-kolnr temp-kolnr))))
                  ; Kopiera och skriv över temp-spelplan med spelplanen.
                  (kopiera-spelplan temp-spelplan spelplan #t)
                  (bästa-drag-loop)))

          ; Om längst till vänster på spelplanen.
          ((= kolnr 1)
           (begin
             ; Om giltigt drag, utför draget och ändra koordinaterna.
             (when (giltigt-drag? radnr kolnr färg spelplan)
               (begin (utför-drag radnr kolnr färg temp-spelplan #t)
                      (set! temp-radnr radnr)
                      (set! temp-kolnr kolnr)))
             ; Fortsätt på raden ovanför.
             (set! radnr (- radnr 1))
             (set! kolnr storlek)
             (bästa-drag-loop)))

          (else
           (begin
             ; Om giltigt drag, utför draget och ändra koordinaterna.
             (when (giltigt-drag? radnr kolnr färg spelplan)
               (begin (utför-drag radnr kolnr färg temp-spelplan #t)
                      (set! temp-radnr radnr)
                      (set! temp-kolnr kolnr)))
             ; Fortsätt på rutan till vänster.
             (set! kolnr (- kolnr 1))
             (bästa-drag-loop)))))

      ; Starta loopen.
      (bästa-drag-loop))))





;; Game-over.scm




;; Visar meddelande, slutställning och vinnare när spelet är slut
(define (game-over)
  (cond ((< (räkna-pjäser vit othello) (räkna-pjäser svart othello))
         (message-box "Spelet är slut!"
                      (string-append "Vit: " (number->string (räkna-pjäser vit othello))
                                     "\n"
                                     "Svart: " (number->string (räkna-pjäser svart othello))
                                     "\n"
                                     "Svart vann!")
                      #f '(ok)))
        ((> (räkna-pjäser vit othello) (räkna-pjäser svart othello))
         (message-box "Spelet är slut!"
                      (string-append "Vit: " (number->string (räkna-pjäser vit othello))
                                     "\n"
                                     "Svart: " (number->string (räkna-pjäser svart othello))
                                     "\n"
                                     "Vit vann!")
                      #f '(ok)))
        (else
         (message-box "Spelet är slut!"
                      (string-append "Vit: " (number->string (räkna-pjäser vit othello))
                                     "\n"
                                     "Svart: " (number->string (räkna-pjäser svart othello))
                                     "\n"
                                     "Det blev oavgjort!")
                      #f '(ok)))))


;; Kollar om spelet är slut, sanningsvärde
(define (check-game-over)
  (if (or (finns-giltigt-drag? vit othello) (finns-giltigt-drag? svart othello))
      #f
      #t))




;; Spelloop.scm




;; Othello av Johan Brandhorst och Robin Carlsson

(define (spelloop)
  ;; Placering är koordinaterna för nästa drag
  (let ((placering (get-next-move)))
    (define färg (if (eq? spelare vit)
                     'WHITE
                     'BLACK))


    ;; Kollar om draget är giltigt (kompenserar för
    ;; skillnader mellan våra koordinater och vektorbaserade koordinater)
    (if (giltigt-drag? (+ (rad placering) 1) (+ (kolumn placering) 1) spelare othello)

        ;; Utför draget på spelplanerna
        (begin
          (kopiera-spelplan tidigare-spelplan othello #t)
          (utför-drag (+ (rad placering) 1) (+ (kolumn placering) 1)
                      spelare othello #f)
          (växla-spelare)

          ;; Finns giltigt drag för nästa spelare?
          (if (finns-giltigt-drag? spelare othello)

              ;; Är AI:n påslagen?
              (when AI?
                (AI spelare othello))

              ;; Nästa spelare kan inte lägga
              (if (check-game-over)
                  (game-over)
                  (begin (kan-inte-lägga spelare)
                         (växla-spelare)
                         (when grid?
                           (set-highlights! (giltiga-drag spelare othello)))))))

        (begin (display "Draget är inte giltigt, försök igen")
               (newline)))
    (spelloop)))

;; Startar spelloopen
(thread spelloop)

(starta-spel)
