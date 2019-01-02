#lang racket
(require "WidgetWorks.rkt")
(require "extras.rkt")
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)

;; CONSTANTS

(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 600)
(define INITIAL-MOUSE -1)
(define SIDE 20)
(define BLOCK-IMG-SELECTED (square SIDE "outline" "red"))
(define BLOCK-IMG-UNSELECTED (square SIDE "outline" "green"))
(define NEW-BLOCK-EVENT "b")
(define INITIAL-MX (/ CANVAS-WIDTH 2))
(define INITIAL-MY (/ CANVAS-HEIGHT 2))



(provide
 SBlock<%>
 make-block
 cubelets-init
 )

;; INTERFACE

(define SBlock<%>
  (interface (SWidget<%>)
    
    ;; get-team : -> ListOfBlock<%>
    ;; RETURNS: the teammates of this block
    get-team
    
    ;; add-teammate: Block<%> -> Void
    ;; EFFECT: adds the given block to this block's team
    add-teammate
    
    ;; sblock-x : -> Integer
    ;; sblock-y : -> Integer
    ;; RETURNS: the x or y coordinates of this block
    sblock-x
    sblock-y
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The Block% class
;; A Block is a
;; (new Block% [x NonNegInt]
;;             [y NonNegInt]
;;             [neighbors ListOfBlock<%>]
;;             [selected? Boolean]
;;             [in-selected-team? Boolean]
;;             [mouse-x NonNegInt]
;;             [mouse-y NonNegInt]
;;             [leader Block<%>]
;;             [updated-blocks ListOfBlock<%>])

;; FOR MAKING A NEW BLOCK, WE ONLY REQUIRE x AND y values to be assigned.


;; INTERPRETATION
;; Represents a square block
;; x is x-coordinate of the center of block.
;; y is y-coordinate of the center of block.
;; neighbors is the list if neighbors of this block.
;; selected? tells us if this block is selected or not.
;; in-selected-team? tells us if this block is in any of selected teams
;; mouse-x is x-coordinate of the center of the mouse.
;; mouse-y is y-coordinate of the center of the mouse.
;; leader tells us the current leader of the team.
;; updated-blocks is the list of updated blocks after movement.


(define Block%
  (class* object% (SBlock<%>)
    
    ;; to be initialized by make-block call
    (init-field x y)
    
    ;; initial value empty
    (init-field [neighbors empty])
    
    ;; initially unselected, hence false
    (init-field [selected? false])
    
    ;; initially not in any team so false initially
    (init-field [in-selected-team? false])
    
    ;; initially mouse co ordinate x set to -1
    (init-field [mouse-x INITIAL-MOUSE])
    
        ;; initially mouse co ordinate y set to -1
    (init-field [mouse-y INITIAL-MOUSE])
    
    
    ;; if none of the blocks are selected then every block is a leader
    (init-field [leader this])
    
    

    ;; initially empty as no team mates
    (init-field [updated-blocks empty])
    
    (super-new)
    
    
    ;; get-block-image : -> image?
    ;; RETURNS : image of the block
    ;; DESIGN STRATEGY :  conditions on if block is selected or not.
    (define (get-block-image)
      (if selected?
          BLOCK-IMG-SELECTED
          BLOCK-IMG-UNSELECTED))
    
    ;; is-selected? : -> Boolean
    ;; RETURNS : true if this block is selected
    (define/public (is-selected?)
      selected?)
    
    ;; get-updated-blocks : -> ListOfBlock<%>
    ;; RETURNS: List of all blocks that are already processed in a team where this is the leader
    ;;          if this is not leader block, updated-blocks will be empty
    (define/public (get-updated-blocks)
      updated-blocks)
    
    ;; is-processed? : Block<%> -> Boolean
    ;; GIVEN : the Block<%> to compare with
    ;; RETURNS : true iff the given block is already processed during drag
    (define (is-processed? other-block)
      (ormap
       ;; Block<%> -> Boolean
       ;; RETURNS: true iff other-block is already processed
       (lambda(processed-block)
         (and (= (send processed-block sblock-x) (send other-block sblock-x))
              (= (send processed-block sblock-y) (send other-block sblock-y))))
       (send leader get-updated-blocks)))
    
    ;; blocks-equal? : Block<%> -> Boolean
    ;; GIVEN : the Block<%> to compare
    ;; RETURNS: true iff the block is same as this block
    (define/public (blocks-equal? other-block)
      (and (equal? x (send other-block sblock-x))
           (equal? y (send other-block sblock-y)))) 
    
    ;; get-team : -> ListOfBlock<%>
    ;; RETURNS: the teammates of this block
    (define/public (get-team)
      (make-my-team empty))
    
    ;; make-my-team : Block<%> -> ListOfBlock<%>
    ;; GIVEN : list of processed blocks
    ;; WHERE : list-of-processed-block is list of blocks which are
    ;;         visited from the leader block
    ;; RETURNS : list of teammate for this block    
    (define/public (make-my-team list-of-processed-block)
      (foldl
       ;; ITERATING ON : Block<%> and ListOfBlock<%>
       ;; RETURNS -> ListOfBlock<%>
       ;; GIVEN: a block & the list of team updated by now.
       ;; RETURNS : updated list of teammates with neigh block teammates
       (lambda(block list-of-teammate)
         (if (in-list? block list-of-processed-block)
             list-of-teammate
             (append
              (send block make-my-team (cons this list-of-processed-block))
              list-of-teammate)))
       (filter
        ;; ITERATING ON : Block<%>
        ;; RETURNS -> Boolean
        ;; RETURNS: true iff neigh block is not already processed
        (lambda(block)
          (not (in-list? block list-of-processed-block)))
        neighbors)
       neighbors))
    
    ;; add-teammate: Block<%> -> Void
    ;; EFFECT: adds the given block to this block's team
    (define/public (add-teammate team-mate)
      (begin
        (set! neighbors
              (cons team-mate neighbors))))
    
    ;; add-processed-block : Block<%> -> Void
    ;; GIVEN : the processed block
    ;; EFFECT : add the processed block to the list
    (define/public (add-processed-block processed-block)
      (set! updated-blocks (cons processed-block updated-blocks)))
    
    ;; set-leader : Block<%> -> Void
    ;; EFFECT : updates the current leader block
    (define/public (set-leader block)
      (set! leader block))
    
    ;; sblock-x : -> Integer
    ;; RETURNS: the x coordinate of this block
    (define/public (sblock-x)
      x)

    ;; sblock-y : -> Integer
    ;; RETURNS: the y coordinate of this block
    (define/public (sblock-y)
      y)
    
    ;; -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: blocks ignore the after tick method.
    (define/public (after-tick)
      this)
    
    ;; in-block? : NonNegInt NonNegInt -> Boolean
    ;; GIVEN : mouse selection coordinates
    ;; RETURNS : true if mouse is in the block else false
    (define (in-block? mx my)
      (and
       (and
        (>= mx (- x (/ SIDE 2)))
        (<= mx (+ x (/ SIDE 2))))
       (and
        (>= my (- y (/ SIDE 2)))
        (<= my (+ y (/ SIDE 2))))))
    
    ;; update-this-block : Boolean NonNegInt NonNegInt -> Void
    ;; GIVEN : if team is selected? & mouse coordinates
    ;; EFFECT : updates this block with the given mouse co-ordinates
    (define (update-this-block selected? mx my)
      (begin
        (set! in-selected-team? selected?)
        (set! mouse-x mx)
        (set! mouse-y my)))
    
    ;; update-neighbors ; (Block<%> -> Void) -> Void
    ;; GIVEN : function to be performed on the neighbors
    ;; EFFECT : Updates all it's neighbors except previous neighbor with the given values
    ;;          Previous neighbor should already be updated.
    (define (update-neighbors fn)
      (begin
        (send leader add-processed-block this)
        (for-each
         ;; ITERATING ON  :Block<%>
         ;; RETURNS -> Void
         ;; EFFECT : Updates each of the teammate
         (lambda (block)
           (and
            (not (is-processed? block)) 
            (begin
              (send block set-leader leader) 
              (fn block)))) 
         neighbors)
        (set! updated-blocks empty)
        (set! leader this)))
    
    ;; update-block-in-selected-team ; Boolean NonNegInt NonNegInt -> Void
    ;; GIVEN : selected? & mouse coordinates
    ;; EFFECT : Updates selected? and mouse co-ordinates of
    ;;          all its team members and itself.
    (define/public (update-block-in-selected-team selected? mx my)
      (update-this-block selected? mx my))
    
    ;; Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this block to the state it should have
    ;; following the specified mouse event at the given location.
    (define/public (after-button-down mx my)
      (begin
        (if (in-block? mx my)
            (set! selected? true)
            this)
        
        (if (or selected?
                in-selected-team?)
            (begin
              (update-this-block true mx my)
              (update-neighbors
               ;; ITERATING ON :Block<%>
               ;; OUTPUT : -> Void
               ;; EFFECT : update the leader & mouse coordinates for all the teammates in its team
               ;;          also call after button-down on all the team mates down the heirarchy.
               (lambda(block)
                 (begin
                   (send block update-block-in-selected-team true mx my)
                   (send block after-button-down mx my)))))
            this)))
    
    ;; Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this block to the state it should have
    ;; following the specified mouse event at the given location.
    (define/public (after-button-up mx my)
      (begin
        (update-neighbors
         ;; ITERATING ON : Block<%>
         ;; RETURNS -> Void
         ;; EFFECT: change the leader to itself and set selected? to false.
         (lambda(neigh-block)
           (begin
             (send neigh-block update-block-in-selected-team
                   false INITIAL-MOUSE INITIAL-MOUSE)
             (send neigh-block after-button-up mx my))))
        (update-this-block false INITIAL-MOUSE INITIAL-MOUSE)
        (set! selected? false)))
    
    ;; after-drag : NonNegInt NonNegInt -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this block to the state it should have
    ;; following the specified mouse event at the given location.
    (define/public (after-drag mx my)
      (and in-selected-team?
           (begin
             (set! x (+ x
                        (- mx mouse-x)))
             (set! y (+ y
                        (- my mouse-y)))
             (update-this-block true mx my)
             (update-neighbors
              ;; Block<%> -> Void
              ;; use after-drag on all it's teammates
              (lambda(neigh-block)
                (send neigh-block after-drag mx my))))))
    
    ;; after-move : NonNegInt NonNegInt -> Void
    ;; GIVEN: a location
    ;; EFFECT: blocks ignore the mouse move event.
    (define/public (after-move mx my)
      this) 
    
    ;; KeyEvent : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates this widget to the state it should have
    ;; following the given key event.
    (define/public (after-key-event kev)
      this)
    
    ;; Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with this object
    ;; painted on it.
    (define/public (add-to-scene scene)
      (place-image (get-block-image)
                   x
                   y
                   scene))
    
    ;; block-overlaps? : NonNegInt NonNegInt -> Boolean
    ;; GIVEN : the x & y co-ordinate of the block to be compared
    ;; RETURNS: true iff this block overlaps with the given block coordinates
    (define (block-overlaps? other-x other-y)
      (not
       (or
        (> (- x (/ SIDE 2))
           (+ other-x (/ SIDE 2)))
        (> (- other-x (/ SIDE 2))
           (+ x (/ SIDE 2)))
        (> (- y (/ SIDE 2))
           (+ other-y (/ SIDE 2)))
        (> (- other-y (/ SIDE 2))
           (+ y (/ SIDE 2))))))
    
    ;; in-list? : Block<%> -> Boolean
    ;; RETURNS : true iff the GIVEN block is already a teammate of this block
    (define (in-list? block list-of-blocks)
      (ormap
       ;; ITERATING ON : Block<%>
       ;; RETURNS: true iff the this-block is already in list, else false
       (lambda (this-block)
         (send block blocks-equal? this-block))
       list-of-blocks))
    
    ;; neighbor? : Block<%> -> Boolean
    ;; RETURNS : true iff the given block is a new neighbor that overlaps with this block
    (define (neighbor? block)
      (and
       (not (blocks-equal? block))
       (not (in-list? block neighbors))
       (block-overlaps? (send block sblock-x) (send block sblock-y))))
    
    ;; handle-block-overlapping : ListofBlock<%> -> Void
    ;; GIVEN : All the blocks present in the world
    ;; EFFECT : adds all the blocks that overlaps with this block as a teammate
    (define/public (handle-block-overlapping all-blocks-in-world)
      (begin
        (for-each
         ;; Block<%> -> Void
         ;; EFFECT: if block is eligible to be a neighbor, adds it as a teammate
         (lambda(block)
           (if (neighbor? block)
               (handle-new-neighbor block)
               this))
         all-blocks-in-world)))
    
    ;; handle-new-neighbor : Block<%> -> Void
    ;; GIVEN : the Block<%> which is a new neighbor
    ;; EFFECT: updates the teammate list of both this block & neighbor block
    (define (handle-new-neighbor new-neighbor)
      (begin
        ;; set the blocks as teammates
        (set! neighbors (cons new-neighbor neighbors))
        (send new-neighbor add-teammate this)
        (send new-neighbor set-leader leader)
        
        ;; set the new neighbor & it's team-mates as part of the block
        (set! updated-blocks (cons this updated-blocks))
        (send new-neighbor update-block-in-selected-team
              true mouse-x mouse-y)
        (send new-neighbor after-button-down mouse-x mouse-y)
        (set! updated-blocks empty)
        
        ))
    
    ;; ------------------------------------------------------------------------------------
    
    ;; FOR TESTING :

    ;; block-mouse-x : -> Int
    ;; RETURNS: last saved x co-ordinate of mouse position.
    (define/public (block-mouse-x)
      mouse-x)

    ;; block-mouse-y : -> Int
    ;; RETURNS: last saved y co-ordinate of mouse position.
    (define/public (block-mouse-y)
      mouse-y)
    
    ;; is-in-selected-team? : -> Boolean
    ;; RETURNS: true iff the block is part of the selected team
    (define/public (is-in-selected-team?)
      in-selected-team?)
    
    ;; get-leader : -> Block<%>
    ;; RETURNS : the leader block of this block
    (define/public (get-leader)
      leader)
    
    
    ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A BlockFactory% class
;; A BlockFactory is
;; (new BlockFactory%
;;                    [available-blocks ListOfBlock<%>]
;;                    [mouse-x NonNegInt]
;;                    [mouse-y NonNegInt])
;; INTERPRETATION
;; Creates all the blocks in the world 
(define BlockFactory%
  (class* object% (SWidget<%>)
    
    
    (init-field [available-blocks empty])
    
    
    (init-field [mouse-x INITIAL-MX])
    
    
    (init-field [mouse-y INITIAL-MY])
    
    
    (super-new)
    
    ;; (Widget or SWidget -> Void) -> Void
    (define (process-block fn)
      (begin
        (for-each fn available-blocks)))
    
    ;; -> Void
    ;; GIVEN: no arguments
    ;; EFFECT: updates this widget to the state it should have
    ;; following a tick.
    (define/public (after-tick)
      (process-block
       (lambda(block) (send block after-tick))))
    
    ;; Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this widget to the state it should have
    ;; following the specified mouse event at the given location.
    (define/public (after-button-down mx my)
      (begin
        (set! mouse-x mx)
        (set! mouse-y my)
        (process-block
         (lambda(block) (send block after-button-down mx my)))))
    
    ;; Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this widget to the state it should have
    ;; following the specified mouse event at the given location.
    (define/public (after-button-up mx my)
      (begin
        (set! mouse-x mx)
        (set! mouse-y my)
        (process-block
         (lambda(block)
           (if (send block is-selected?)
               (send block after-button-up mx my)
               this)))))
    
    ;; Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: updates this widget to the state it should have
    ;; following the specified mouse event at the given location.
    (define/public (after-drag mx my)
      (process-block
       (lambda(block)
         (if (send block is-selected?)
             (begin
               (send block after-drag mx my)
               (send block handle-block-overlapping available-blocks))
             this))))
    
    
    ;; Integer Integer -> Void
    ;; GIVEN: a location
    ;; EFFECT: this widget ignores mouse move event.
    (define/public (after-move mx my)
      this)
    
    ;; KeyEvent : KeyEvent -> Void
    ;; GIVEN: a key event
    ;; EFFECT: updates this widget to the state it should have
    ;; following the given key event
    (define/public (after-key-event kev)
      (begin
        (and
         (string=? kev NEW-BLOCK-EVENT)
         (set! available-blocks
               (cons (make-block
                      mouse-x
                      mouse-y
                      available-blocks) available-blocks)))))        
    
    ;; Scene -> Scene
    ;; GIVEN: a scene
    ;; RETURNS: a scene like the given one, but with this object
    ;; painted on it.
    (define/public (add-to-scene scene)
      (foldr
       (lambda (block scene)
         (send block add-to-scene scene))
       scene
       available-blocks))
    
    ;; get-available-blocks : -> ListofBlock<%>
    (define/public (get-available-blocks)
      available-blocks)
    
    ))





; make-block : NonNegInt NonNegInt ListOfSBlock -> SBlock
; GIVEN: an x and y position, and a list of blocks
; WHERE: the list of blocks is the list of blocks already on the playground.
; RETURNS: a new block, at the given position, with no teammates
(define (make-block x-coord y-coord available-blocks)
  (new Block%
       [x x-coord]
       [y y-coord]))


;; cubelets-init : -> Container
;; GIVEN: no arguments
;; RETURNS: a Container, initially with no blocks,
;;          which when run, will run in a 600x500 canvas and
;;          process the events in the description above.

(define (cubelets-init)
  (local
    ((define the-world
       (container-init CANVAS-WIDTH CANVAS-HEIGHT))
     (define the-factory (new BlockFactory%)))
    (begin
      ;; put the factory in the world
      (send the-world add-stateful-widget the-factory)
      the-world)))
(send (cubelets-init) run 1)



;; =======================================================================================

;; START-TESTS

(begin-for-test
  (local
    ((define block-factory (new BlockFactory%))
     (define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
     (define block-1-image (place-image
                            (square 20 "outline" "red")
                            300 350
                            EMPTY-CANVAS)))
    (send block-factory after-key-event "c")
    (check-equal? (send block-factory get-available-blocks) empty)
    
    (send block-factory after-key-event "b")
    (check-equal? (length (send block-factory get-available-blocks)) 1)
    
    (send block-factory after-button-down 250 300)
    (check-equal? (send (first (send block-factory get-available-blocks)) is-selected?) true)
    
    (send block-factory after-button-up 250 300)
    (check-equal? (send (first (send block-factory get-available-blocks)) is-selected?) false)
    
    (send block-factory after-button-up 250 300)
    (check-equal? (send (first (send block-factory get-available-blocks)) is-selected?) false)
    
    (send block-factory after-drag 300 350)
    (check-equal? (send (first (send block-factory get-available-blocks)) is-selected?) false)
    (check-equal? (send (first (send block-factory get-available-blocks)) sblock-x) 250)
    (check-equal? (send (first (send block-factory get-available-blocks)) sblock-y) 300)
    
    (send block-factory after-button-down 250 300)
    (send block-factory after-drag 300 350)
    (check-equal? (send (first (send block-factory get-available-blocks)) is-selected?) true)
    (check-equal? (send (first (send block-factory get-available-blocks)) sblock-x) 300)
    (check-equal? (send (first (send block-factory get-available-blocks)) sblock-y) 350)
    
    (send block-factory after-tick)
    (check-equal? (send block-factory add-to-scene EMPTY-CANVAS) block-1-image)
    ))

;; ========================================================================================
;; START-TESTS

(begin-for-test
  (local
    ((define EMPTY-CANVAS (empty-scene 500 600))
     (define block-1-image (place-image
                            (square 20 "outline" "green")
                            300 200
                            EMPTY-CANVAS))
     (define block-1-selected-image (place-image
                                     (square 20 "outline" "red")
                                     300 200
                                     EMPTY-CANVAS)) 
     (define block-1 (new Block% [x 300]
                          [y 200]))
     (define block-2 (new Block% [x 100]
                          [y 100]))
     (define block-3 (new Block% [x 100]
                          [y 100]))
     (define world-blocks (cons block-2 (cons block-1 empty)))
     (define world-blocks-three (cons block-3 world-blocks)))
    (check-equal? (send block-1 add-to-scene EMPTY-CANVAS) block-1-image)))


(begin-for-test
  (local
    ((define EMPTY-CANVAS (empty-scene 500 600))
     (define block-1-image (place-image
                            (square 20 "outline" "green")
                            300 200
                            EMPTY-CANVAS))
     (define block-1-selected-image (place-image
                                     (square 20 "outline" "red")
                                     300 200
                                     EMPTY-CANVAS)) 
     (define block-1 (new Block% [x 300]
                          [y 200]))
     (define block-2 (new Block% [x 100]
                          [y 100]))
     (define block-3 (new Block% [x 100]
                          [y 100]))
     (define world-blocks (cons block-2 (cons block-1 empty)))
     (define world-blocks-three (cons block-3 world-blocks)))
    
    (send block-1 after-button-down 300 200)
    (send block-1 after-move 300 200)
    (send block-2 after-button-down 200 200)
    (check-equal? (send block-1 is-selected?) true)
    (check-equal? (send block-1 is-in-selected-team?) true)
    (check-equal? (send block-1 block-mouse-x) 300)
    (check-equal? (send block-1 block-mouse-y) 200)
    (check-equal? (send block-1 add-to-scene EMPTY-CANVAS) block-1-selected-image)


    
    
    (send block-1 after-drag 150 150)
    (send block-2 after-drag 150 150)
    (check-equal? (send block-1 sblock-x) 150)
    (check-equal? (send block-1 sblock-y) 150)
    (send block-1 handle-block-overlapping world-blocks)
    (check-equal? (send block-2 is-in-selected-team?) false)
    (check-equal? (send block-2 sblock-x) 100)
    (check-equal? (send block-2 sblock-y) 100)
    (send block-1 handle-block-overlapping world-blocks)
    (check-equal? (send block-1 is-in-selected-team?) true)
    
    (send block-1 after-drag 120 120)
    (send block-1 handle-block-overlapping world-blocks)
    (check-equal? (send block-2 is-selected?) false)
    (check-equal? (send block-2 is-in-selected-team?) true)
    
    (send block-1 after-drag 80 80)
    (send block-1 handle-block-overlapping world-blocks)
    (check-equal? (send block-1 sblock-x) 80)
    (check-equal? (send block-1 sblock-y) 80)
    (check-equal? (send block-2 sblock-x) 60)
    (check-equal? (send block-2 sblock-y) 60)
    (check-equal? (send (send block-2 get-leader) blocks-equal? block-1) false)
    
    (send block-1 after-button-up 100 100)
    (check-equal? (send block-1 is-selected?) false)
    (check-equal? (send block-1 is-in-selected-team?) false)
    (send block-1 after-key-event "b")
    
    (send block-1 after-button-down 80 80)
    (send block-1 after-tick)
    (check-equal? (send block-1 is-selected?) true)
    (check-equal? (send block-1 is-in-selected-team?) true)
    (check-equal? (send block-2 is-selected?) false)
    (check-equal? (send block-2 is-in-selected-team?) true)
    (check-equal? (send block-2 get-updated-blocks) empty)
    (check-equal? (send (send block-2 get-leader) blocks-equal? block-2) true)
    (check-equal? (send block-1 get-updated-blocks) empty)
    (check-equal? (send (send block-1 get-leader) blocks-equal? block-1) true)
    (check-equal? (send block-1 blocks-equal? (first (send block-2 get-team))) true)
    (check-equal? (send block-2 blocks-equal? (first (send block-1 get-team))) true)
    
    (send block-1 handle-block-overlapping world-blocks-three)
    (check-equal? (length (send block-1 get-team)) 2)
    (check-equal? (length (send block-2 get-team)) 2)
    (check-equal? (length (send block-3 get-team)) 2)
    
    ))