;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname q1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(require 2htdp/universe)
(require 2htdp/image)

(provide initial-world
         run
         world-after-mouse-event
         world-after-key-event
         world-to-trees
         tree-to-root
         tree-to-sons
         node-to-center
         node-to-selected?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Dimensions of the canvas
(define CANVAS-WIDTH 500)
(define CANVAS-HEIGHT 400)
(define EMPTY-CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define CANVAS (empty-scene CANVAS-WIDTH CANVAS-HEIGHT))
(define INITIAL-MX 0)
(define INITIAL-MY 0)
(define RADIUS 20)
(define SIDE 40)
(define INITIAL-X (/ CANVAS-WIDTH 2))
(define INITIAL-Y RADIUS)
(define OFFSET (* 3 RADIUS))


;; CONSTANTS:
(define CIRCLE-UNSELECTED (circle RADIUS "outline" "green"))
(define CIRCLE-SELECTED (circle RADIUS "solid" "green"))
(define SQUARE-UNSELECTED (square SIDE "outline" "green"))
(define SQUARE-SELECTED (square SIDE "solid" "green"))
(define SQUARE "square")
(define CIRCLE "circle")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS:

;; A Shape is either :
;; - CIRCLE-STR
;; - SQUARE-STR

;; template :
;; shape-fn : Shape -> ??
; (define (shape-fn sh)
;   (...
;       (string=? sh CIRCLE)
;       (string=? sh SQUARE)))

;;--------------------------;

(define-struct node(x y shape selected? mx my))
;; A Node is a (make-node NonNegInt NonNegInt Shape Boolean NonNegInt NonNegInt)
;; Interpretation: 
;; x is the x co ordinate of the center of the node.
;; y is the y co ordinate of the center of the node.
;; shape is the Shape of the node.
;; selected? tells us if node is selected
;; mx is the x co ordinate of the mouse.
;; my is the y co ordinate of the mouse.

;; template:
;; node-fn : Node -> ??
;(define (node-fn n)
;  (... (node-x n) (node-y n) (node-shape n)
;       (node-selected? n) (node-mx n) (node-my n)))

;;--------------------------;

(define-struct tree(root children))
;; A Tree is a (make-tree Node ListofTrees)
;; Interpretation: 
;; root is the root Node of the tree.
;; children is the ListofTrees that are children of this node.

;; template:
;; tree-fn : Tree -> ??
;(define (tree-fn t)
;  (... (tree-root t) (tree-children t) ))

;;--------------------------;

;; A ListofTrees (LOT) is either :
;; - empty
;; - (cons Tree LOT)

;; template :
;; listoftrees-fn : LOT -> ??
;; HALTING MEASURE: length of lot
;(define (listoftrees-fn lt)
; (cond
;   [(empty? lt) ...]
;   [(else ...)]))

;;--------------------------;


(define-struct world (trees))
;; A World is a (make-world ListOfTrees)
;; Interpretation: 
;; trees is the ListOfTrees.

;; template:
;; world-fn : World -> ??
;(define (world-fn w)
;  (... (world-trees w) ))


;;--------------------------;

;; A KeyEvent is a KeyEvent, which is one of
;; -- "c"            (interp:add a new circle node)
;; -- "s"            (interp:add a new square node)
;; -- "d"            (interp:delete the node selected)
#|
(define (KeyEvent-fn k)
  (...
   (cond
    [(key? k "c") (...)]
    [(key? k "s") (...)]
    [(key? k "d") (...)])))
|#

;; A  MouseEvent is one of
;; -- "button-down"  (interp: click the mouse button)         
;; -- "drag"         (interp: move mouse while clicking)
;; -- "button-up"    (interp: release the mouse button)
#|
(define (MouseEvent-fn m)
  (...
   (cond
    [(key? m "button-down") (...)]
    [(key? m "drag") (...)]
    [(key? m "button-up") (...)])))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constant defination for testing purposes :

(define CIRCLE-NODE-AT-100-20-UNSELECTED
  (make-node 100 INITIAL-Y CIRCLE false
             INITIAL-MX INITIAL-MY))

(define CIRCLE-NODE-AT-100-20-SELECTED
  (make-node 100 INITIAL-Y CIRCLE true
             100 20))

(define CIRCLE-NODE-AT-100-20-SELECTED-INITIAL
  (make-node 100 INITIAL-Y CIRCLE true
             INITIAL-MX INITIAL-MY))

(define CIRCLE-NODE-AT-400-20-UNSELECTED
  (make-node 400 INITIAL-Y CIRCLE false
             INITIAL-MX INITIAL-MY))

(define CIRCLE-NODE-AT-250-20-UNSELECTED
  (make-node 250 INITIAL-Y CIRCLE false
             INITIAL-MX INITIAL-MY))
(define SQUARE-NODE-AT-250-20-UNSELECTED
  (make-node 250 INITIAL-Y SQUARE false
             INITIAL-MX INITIAL-MY))


(define CIRCLE-NODE-AT-250-80-UNSELECTED
  (make-node 250 80 CIRCLE false
             INITIAL-MX INITIAL-MY))
(define CIRCLE-NODE-AT-250-140-UNSELECTED
  (make-node 250 140 CIRCLE false
             INITIAL-MX INITIAL-MY))
(define CIRCLE-NODE-AT-190-200-UNSELECTED
  (make-node 190 200 CIRCLE false
             INITIAL-MX INITIAL-MY))


(define CIRCLE-NODE-AT-190-80-UNSELECTED
  (make-node 190 80 CIRCLE false
             INITIAL-MX INITIAL-MY))

(define CIRCLE-NODE-AT-190-80-SELECTED
  (make-node 190 80 CIRCLE true
             INITIAL-MX INITIAL-MY))

(define SQUARE-NODE-AT-190-140-UNSELECTED
  (make-node 190 140 SQUARE false
             INITIAL-MX INITIAL-MY))

(define SQUARE-NODE-AT-130-80-UNSELECTED
  (make-node 130 80 SQUARE false
             INITIAL-MX INITIAL-MY))

(define SQUARE-NODE-AT-130-80-SELECTED
  (make-node 130 80 SQUARE true
             INITIAL-MX INITIAL-MY))

(define CIRCLE-NODE-AT-130-140-UNSELECTED
  (make-node 130 140 CIRCLE false
             INITIAL-MX INITIAL-MY))
(define CIRCLE-NODE-AT-70-140-UNSELECTED
  (make-node 70 140 CIRCLE false
             INITIAL-MX INITIAL-MY))



(define TREE-CIRCLE (make-tree CIRCLE-NODE-AT-130-140-UNSELECTED empty))
(define TREE-ROOT1 (make-tree SQUARE-NODE-AT-130-80-SELECTED
                              (list TREE-CIRCLE)))
(define TREE-ROOT1-INITIAL (make-tree SQUARE-NODE-AT-130-80-SELECTED
                                      empty))

(define TREE-SQUARE (make-tree SQUARE-NODE-AT-190-140-UNSELECTED empty))
(define TREE-ROOT2 (make-tree CIRCLE-NODE-AT-190-80-SELECTED
                              (list TREE-SQUARE)))
(define TREE-ROOT2-INITIAL (make-tree CIRCLE-NODE-AT-190-80-SELECTED
                                      empty))


(define TREE1 (make-tree CIRCLE-NODE-AT-130-140-UNSELECTED empty))
(define TREE2 (make-tree CIRCLE-NODE-AT-70-140-UNSELECTED empty))
(define TREE3 (make-tree SQUARE-NODE-AT-130-80-UNSELECTED
                         (list TREE1 TREE2)))

(define TREE4 (make-tree SQUARE-NODE-AT-190-140-UNSELECTED empty))
(define TREE5 (make-tree CIRCLE-NODE-AT-190-80-UNSELECTED
                         (list TREE4)))

(define TREE6 (make-tree CIRCLE-NODE-AT-190-200-UNSELECTED empty))
(define TREE7 (make-tree CIRCLE-NODE-AT-250-140-UNSELECTED empty))
(define TREE8 (make-tree CIRCLE-NODE-AT-250-80-UNSELECTED
                         (list TREE7 TREE6)))

(define TREE9 (make-tree CIRCLE-NODE-AT-250-20-UNSELECTED
                         (list TREE8 TREE5 TREE3)))
(define INITIAL-TREE (make-tree CIRCLE-NODE-AT-250-20-UNSELECTED
                                empty))
(define INITIAL-TREE-SQUARE (make-tree SQUARE-NODE-AT-250-20-UNSELECTED
                                       empty))

(define TREE10 (make-tree CIRCLE-NODE-AT-100-20-UNSELECTED empty))

(define TREE11 (make-tree CIRCLE-NODE-AT-400-20-UNSELECTED empty))

(define TREE-SELECTED (make-tree CIRCLE-NODE-AT-100-20-SELECTED empty))

(define TREE-SELECTED-INITIAL
  (make-tree CIRCLE-NODE-AT-100-20-SELECTED-INITIAL empty))

(define LOT1 (list TREE9 TREE10 TREE11))

(define WORLD-LOT1 (make-world LOT1))
(define WORLD-INITIAL (make-world empty))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HELPER FUNCTIONS :

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-node : Shape -> Node
;; RETURN: a node locate at initial position and given shape
;; DESIGN STRATEGY: use template of node
;; EXAMPLES :
;; (initial-node CIRCLE) = CIRCLE-NODE-AT-250-20-UNSELECTED)
;; (initial-node SQUARE) = SQUARE-NODE-AT-250-20-UNSELECTED)

(define (initial-node shape)
  (make-node INITIAL-X INITIAL-Y shape false INITIAL-MX INITIAL-MY))

;; TEST:
(begin-for-test
  (check-equal? (initial-node CIRCLE)
                CIRCLE-NODE-AT-250-20-UNSELECTED)
  (check-equal? (initial-node SQUARE)
                SQUARE-NODE-AT-250-20-UNSELECTED))


;; initial-tree : Shape -> Tree
;; RETURN: a tree with initial-node with given shape and empty children set
;; DESIGN STRATEGY: use template of tree
;; EXAMPLES : See Test Case

(define (initial-tree shape)
  (make-tree (initial-node shape) empty))

;; TEST:

(begin-for-test
  (check-equal? (initial-tree CIRCLE) INITIAL-TREE))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; initial-world : Any -> World
;; GIVEN: any value
;; RETURNS: an initial world.  The given value is ignored.
;; EXAMPLES : See Test Case
;; DESIGN STRATEGY: use template of work

(define (initial-world n)
  (make-world empty))

;; TEST:

(begin-for-test
  (check-equal? (initial-world 0) WORLD-INITIAL))

;; run :  Any -> World
;; GIVEN: any value
;; EFFECT: runs a copy of an initial world
;; RETURNS: the final state of the world.  The given value is ignored.

(define (run n)
  (big-bang (initial-world 1)
            (on-draw world->scene)
            (on-mouse world-after-mouse-event)
            (on-key world-after-key-event)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add-lot-line : ListOfTrees Image -> Image
;; GIVEN: A ListOfTrees lot and a Image image
;; RETURN: A new image contains all line needed of lot
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: use template of lot
;; HALTING MEASURE : lenght of lot

(define (add-lot-line lot image)
  (cond
    [(empty? lot) image]
    [else (add-lot-line (rest lot)
                        (add-tree-line (first lot)
                                       image))]))

;; TEST:


(begin-for-test
  (check-equal? (add-lot-line empty
                              EMPTY-CANVAS)
                EMPTY-CANVAS)
  (check-equal? (add-lot-line (list (initial-tree CIRCLE))
                              EMPTY-CANVAS)
                EMPTY-CANVAS))


;; add-tree-line : Tree Image -> Image
;; GIVEN: A Tree and a Image
;; RETURN: A new image contains all line needed of a tree
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: combine simpler functions

(define (add-tree-line tree image)
  (add-lot-line (tree-children tree)
                (add-node-lot-line (tree-root tree)
                                   (tree-children tree)
                                   image)))

;; TEST:

(begin-for-test
  (check-equal? (add-tree-line (initial-tree CIRCLE)
                               EMPTY-CANVAS)
                EMPTY-CANVAS))


;; add-node-lot-line : Node ListOfTrees Image -> Image
;; GIVEN: A Node, a ListOfTrees and a image
;; RETURN: A new image contains lines of root node to children
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: use template of lot
;; HALTING MEASURE : lenght of lot

(define (add-node-lot-line node lot image)
  (cond
    [(empty? lot) image]
    [else
     (add-node-lot-line node
                        (rest lot)
                        (add-nodes-line node
                                        (tree-root (first lot))
                                        image))]))

;; TEST:

(begin-for-test
  (check-equal? (add-node-lot-line (initial-node CIRCLE) empty
                                   EMPTY-CANVAS)
                EMPTY-CANVAS))


;; add-nodes-line : Node Node Image -> Image
;; GIVEN: two nodes and a image
;; RETURN: a new image with line between two nodes
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: combine simpler functions

(define (add-nodes-line node1 node2 image)
  (scene+line image
              (node-x node1) (node-y node1)
              (node-x node2) (node-y node2)
              "blue"))

;; TEST:

(begin-for-test
  (check-equal? (add-nodes-line CIRCLE-NODE-AT-250-20-UNSELECTED
                                (initial-node CIRCLE) EMPTY-CANVAS)
                (add-nodes-line (initial-node CIRCLE)
                                CIRCLE-NODE-AT-250-20-UNSELECTED
                                EMPTY-CANVAS)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-lot-images : ListOfTrees -> List
;; GIVEN: A ListOfTrees lot
;; RETURN: The image contains all nodes of this lot
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: use template of lot
;; HALTING MEASURE : lenght of lot
(define (make-lot-images lot)
  (cond
    [(empty? lot) empty]
    [else (append (make-tree-images (first lot))
                  (make-lot-images (rest lot)))]))

;; TEST:

(begin-for-test
  (check-equal? (make-lot-images (list (initial-tree CIRCLE)))
                (list (make-node-images (initial-node CIRCLE)))))


;; make-tree-images : Tree -> List
;; GIVEN : TREE
;; RETURN: A List Of Images the given tree contained
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: combine simpler functions
(define (make-tree-images tree)
  (cons (make-node-images (tree-root tree))
        (make-lot-images (tree-children tree))))

;; TEST:

(begin-for-test
  (check-equal? (make-tree-images (initial-tree CIRCLE))
                (list (make-node-images
                       (initial-node CIRCLE)))))


;; make-node-images : Node -> Image
;; RETURN: The correct image of the given node
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: divide into cases on node-shape and node-selected?
(define (make-node-images node)
  (cond
    [(and (equal? (node-shape node)
                  CIRCLE)
          (node-selected? node)) CIRCLE-SELECTED]
    [(and (equal? (node-shape node)
                  SQUARE)
          (node-selected? node)) SQUARE-SELECTED]
    [(equal? (node-shape node)
             CIRCLE) CIRCLE-UNSELECTED]
    [else SQUARE-UNSELECTED]))

;; TEST:



(begin-for-test
  (check-equal? (make-node-images CIRCLE-NODE-AT-190-80-SELECTED)
                (make-node-images (make-node 190 80 CIRCLE true 0 0)))
  (check-equal? (make-node-images SQUARE-NODE-AT-130-80-UNSELECTED)
                (make-node-images (make-node 130 80 SQUARE false 0 0)))
  (check-equal? (make-node-images SQUARE-NODE-AT-130-80-SELECTED)
                (make-node-images (make-node 130 80 SQUARE true 0 0))))


;; world-to-trees : World -> ListOfTree
;; GIVEN: a World
;; RETURNS: a list of all the trees in the given world.
;; EXAMPLE:see below in test
(define (world-to-trees w)
  (world-trees w))

;; TEST:

(define LOT1-TREE-LIST (list
                        (make-tree
                         (make-node 250 20 "circle" #false 0 0)
                         (list
                          (make-tree (make-node 250 80 "circle" #false 0 0)
                                     (list (make-tree (make-node 250 140 "circle" #false 0 0) '())
                                           (make-tree (make-node 190 200 "circle" #false 0 0) '())))
                          (make-tree (make-node 190 80 "circle" #false 0 0)
                                     (list (make-tree (make-node 190 140 "square" #false 0 0) '())))
                          (make-tree (make-node 130 80 "square" #false 0 0)
                                     (list (make-tree (make-node 130 140 "circle" #false 0 0) '())
                                           (make-tree (make-node 70 140 "circle" #false 0 0) '())))))
                        (make-tree (make-node 100 20 "circle" #false 0 0) '())
                        (make-tree (make-node 400 20 "circle" #false 0 0) '())))

(begin-for-test
  (check-equal? (world-to-trees WORLD-LOT1) LOT1-TREE-LIST))


;; tree-to-root : Tree -> Node
;; GIVEN: a tree
;; RETURNS: the node at the root of the tree
;; EXAMPLE: Consider the tree represented as follows:
#|
                 A
                 |
       +---+-----+-----+
       |   |     |     |
       B   C     D     E
           |           |
         +---+      +-----+
         |   |      |     |
         F   G      H     I
|#
;; If tree-to-root is given the subtree rooted at C, it should return the
;; data structure associated with node C. This data structure may or may
;; not include data associated with rest of the tree, depending on
;; whether you have chosen to represent nodes differently from trees.
(define (tree-to-root t)
  (tree-root t))

;; TEST:

(begin-for-test
  (check-equal? (tree-to-root TREE9)
                (make-node 250 20 "circle" #false 0 0)))

;; tree-to-sons : Tree -> ListOfTree
;; GIVEN: a tree
;; RETURNS: the data associated with the immediate subtrees of the given
;; tree. 
;; EXAMPLE: In the situation above, if tree-to-sons is given the subtree
;; rooted at C, it should return a list consisting of the subtree rooted
;; at F and the subtree rooted at G.
(define (tree-to-sons t)
  (tree-children t))

;; TEST:

(define TREE-TO-LIST (list
                      (make-tree (make-node 250 80 "circle" #false 0 0)
                                 (list (make-tree (make-node 250 140 "circle" #false 0 0) '())
                                       (make-tree (make-node 190 200 "circle" #false 0 0) '())))
                      (make-tree (make-node 190 80 "circle" #false 0 0)
                                 (list (make-tree (make-node 190 140 "square" #false 0 0) '())))
                      (make-tree (make-node 130 80 "square" #false 0 0)
                                 (list (make-tree (make-node 130 140 "circle" #false 0 0) '())
                                       (make-tree (make-node 70 140 "circle" #false 0 0) '())))))


(begin-for-test
  (check-equal? (tree-to-sons TREE9) TREE-TO-LIST) )


;; node-to-center : Node -> Posn
;; RETURNS: the center of the given node as it is to be displayed on the
;; scene.
;; Note: this function returns a Posn (an ISL builtin).  This is for the
;; convenience of the testing framework, and you may or may not wish to
;; represent the center of the node in this way.
(define (node-to-center node)
  (make-posn (node-x node)
             (node-y node)))

;; TEST:

(begin-for-test
  (check-equal? (node-to-center CIRCLE-NODE-AT-100-20-UNSELECTED)
                (make-posn 100 20)))


;; node-to-selected? : Node -> Boolean
;; RETURNS: true iff the given node is selected.
;; EXAMPLE:see below in test
(define (node-to-selected? node)
  (node-selected? node))

;; TEST:

(begin-for-test
  (check-equal? (node-to-selected? CIRCLE-NODE-AT-100-20-UNSELECTED)
                false))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; make-lot-positions : ListOfTrees -> List
;; GIVEN: A ListOfTrees lot
;; RETURN: A List of positions of all nodes in the given lot
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: use template of lot
;; HALTING MEASURE : lenght of lot

(define (make-lot-positions lot)
  (cond
    [(empty? lot) empty]
    [else (append (make-tree-positions (first lot))
                  (make-lot-positions (rest lot)))]))

;; TEST:

(define LOP (list
             (make-posn 250 20)
             (make-posn 250 80)
             (make-posn 250 140)
             (make-posn 190 200)
             (make-posn 190 80)
             (make-posn 190 140)
             (make-posn 130 80)
             (make-posn 130 140)
             (make-posn 70 140)
             (make-posn 100 20)
             (make-posn 400 20)))

(begin-for-test
  (check-equal? (make-lot-positions LOT1) LOP))


;; make-tree-positions : Tree -> List
;; GIVEN : A tree
;; RETURN: A list of positions of nodes in the given tree
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: combine simpler functions
(define (make-tree-positions tree)
  (cons (node-to-center (tree-root tree))
        (make-lot-positions (tree-children tree))))

;; TEST:

(define TREE-POSN (list
                   (make-posn 250 20)
                   (make-posn 250 80)
                   (make-posn 250 140)
                   (make-posn 190 200)
                   (make-posn 190 80)
                   (make-posn 190 140)
                   (make-posn 130 80)
                   (make-posn 130 140)
                   (make-posn 70 140)))

(begin-for-test
  (check-equal? (make-tree-positions TREE9) TREE-POSN))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; world->scene : World -> Image
;; produces a Scene that portrays the given world.
;; EXAMPLES: See test cases
;; (world-to-scene world-trees1) = LOT-SCENE
;; STRATEGY: combine simpler functions

(define (world->scene w)
  (add-lot-line (world-trees w)
                (place-images
                 (make-lot-images (world-trees w))
                 (make-lot-positions (world-trees w))
                 EMPTY-CANVAS)))

;; TEST:

(define WORLD-LOT1-CANVAS (world->scene WORLD-LOT1))

(begin-for-test
  (check-equal? (world->scene WORLD-INITIAL) EMPTY-CANVAS)
  (check-equal? (world->scene WORLD-LOT1) WORLD-LOT1-CANVAS))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-mouse-event : World Integer Integer MouseEvent -> World
;; GIVEN: a World, a location, and a MouseEvent
;; RETURNS: the state of the world as it should be following the given mouse event
;; at that location.
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: combine simpler functions

(define (world-after-mouse-event world mx my mouse-event)
  (make-world (lot-after-mouse-event (world-trees world)
                                     mx my mouse-event)))

;; TEST:

(begin-for-test
  (check-equal? (world-after-mouse-event WORLD-LOT1 0 0 "drag")
                WORLD-LOT1))

;; lot-after-mouse-event : ListOfTrees NonNegInt NonNegInt MouseEvent
;;                                                     -> ListOfTrees
;; GIVEN: A ListOfTrees lot, mouse's coordinate and a mouse-event
;; RETURN: A list of trees after the mouse event
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: use HOF map on LOT
(define (lot-after-mouse-event lot mx my mouse-event)
  (map
   ;; Tree -> Tree
   ;; RETURN: A new tree after mouse event
   (lambda (t) (tree-after-mouse-event t mx my mouse-event))
   lot))

;; TEST:

(begin-for-test
  (check-equal? (lot-after-mouse-event LOT1 10 20 "drag")
                LOT1))


;; tree-after-mouse-event : Tree NonNegInt NonNegInt MouseEvent
;;                                                      -> Tree
;; GIVEN: A Tree tree, x and y coordinate of mouse and the mouse-evnet
;; RETURN: A new tree after the mouse event
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: use template of mouse-event
(define (tree-after-mouse-event tree mx my mev)
  (cond
    [(mouse=? mev "button-down") (tree-after-button-down tree mx my)]
    [(mouse=? mev "drag") (tree-after-drag tree mx my)]
    [(mouse=? mev "button-up") (tree-after-button-up tree mx my)]
    [else tree]))

;; TEST:

(begin-for-test
  (check-equal? (tree-after-mouse-event TREE9 10 20 "drag")
                TREE9)
  (check-equal? (tree-after-mouse-event TREE9 10 20 "button-down")
                TREE9)
  (check-equal? (tree-after-mouse-event TREE9 10 20 "button-up")
                TREE9)
  (check-equal? (tree-after-mouse-event TREE9 10 20 "enter")
                TREE9))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tree-after-button-down : Tree NonNegInt NonNegInt -> Tree
;; GIVEN: A Tree tree, x and y coordinate of mouse
;; RETURN: A new tree after button-down event at (x,y)
;; EXAMPLE:see below in test
;; DESIGN STRATEGY:use template of tree
(define (tree-after-button-down tree mx my)
  (make-tree (node-after-button-down (tree-root tree) mx my)
             (lot-after-mouse-event (tree-children tree)
                                    mx my "button-down")))

;; TEST:

(begin-for-test
  (check-equal? (tree-after-button-down TREE1 0 10) TREE1))


;; node-after-button-down : Node NonNegInt NonNegInt -> Node
;; GIVEN: A Node node, x and y coordinate of mouse 
;; RETURN: A new node after button-down event at (x,y)
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: use template of node
(define (node-after-button-down node mx my)
  (if (in-node? node mx my)
      (make-node (node-x node)
                 (node-y node)
                 (node-shape node)
                 #true
                 mx
                 my)
      node))

;; TEST:

(begin-for-test
  (check-equal? (node-after-button-down
                 CIRCLE-NODE-AT-100-20-UNSELECTED 20 60)
                CIRCLE-NODE-AT-100-20-UNSELECTED)
  (check-equal? (node-after-button-down
                 CIRCLE-NODE-AT-100-20-UNSELECTED 100 20)
                CIRCLE-NODE-AT-100-20-SELECTED))




;; in-node? : Node NonNegInt NonNegInt -> Boolean
;; GIVEN: A Node node, x and y coordinate of mouse
;; RETURN: true if the mouse is inside the node
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: divide into cases on node-shape
(define (in-node? node mx my)
  (if (string=? (node-shape node) CIRCLE)
      (<= (get-distance node mx my) 20)
      (in-square? node mx my)))

;; TEST:

(begin-for-test
  (check-equal? (in-node? CIRCLE-NODE-AT-100-20-UNSELECTED 10 0)
                false)
  (check-equal? (in-node? SQUARE-NODE-AT-250-20-UNSELECTED 250 20)
                true)
  (check-equal? (in-node? CIRCLE-NODE-AT-100-20-UNSELECTED 100 30)
                true))

;; get-distance : Node NonNegInt NonNegInt -> Number
;; GIVEN: A Node node, x and y coordinate of mouse
;; RETURN: The distance from center of the node to mouse
;; EXAMPLE: see below in test
;; DESIGN STRATEGY: combine simpler functions
(define (get-distance node mx my)
  (sqrt (+ (sqr (- (node-x node) mx))
           (sqr (- (node-y node) my)))))

;; TEST:

(begin-for-test
  (check-equal? (get-distance CIRCLE-NODE-AT-100-20-UNSELECTED 100 20)
                0))

;; in-square? : Node NonNegInt NonNegInt -> Boolean
;; GIVEN: A Node , x and y coordinate of mouse
;; RETURN: true if mouse in inside the node
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: combine simpler functions
(define (in-square? node mx my)
  (and (<= (abs (- (node-x node) mx)) 20)
       (<= (abs (- (node-y node) my)) 20)))

;; TEST:
(begin-for-test
  (check-equal?
   (in-square? SQUARE-NODE-AT-250-20-UNSELECTED
               250 10)
   #true))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tree-after-drag : Tree NonNegInt NonNegInt -> Tree
;; GIVEN: A Tree tree, x and y coordinate of mouse
;; RETURN: A new tree after drag to (x,y)
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: use template of tree
(define (tree-after-drag tree mx my)
  (if (node-selected? (tree-root tree))
      (tree-move tree mx my
                 (mouse-move-distance-x (tree-root tree)
                                        mx my)
                 (mouse-move-distance-y (tree-root tree)
                                        mx my))
      (make-tree (tree-root tree)
                 (lot-after-mouse-event (tree-children tree)
                                        mx my "drag"))))

;; TEST:

(begin-for-test
  (check-equal? (tree-after-drag TREE9 0 0) TREE9)
  (check-equal? (tree-after-drag TREE-SELECTED 100 20) TREE-SELECTED))

;; tree-move : Tree NonNegInt NonNegInt Int Int -> Tree
;; GIVEN: A Tree , x and y coordinate of mouse, the distance mouse
;;        has moved in x and y direction
;; RETURN: A new Tree after movement
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: use template of tree and node

(define (tree-move tree mx my x-distance y-distance)
  (make-tree (make-node (+ (root-x tree)
                           x-distance)
                        (+ (root-y tree)
                           y-distance)
                        (node-shape (tree-root tree))
                        (node-selected? (tree-root tree))
                        mx my)
             (lot-move (tree-children tree)
                       mx my
                       x-distance
                       y-distance)))

;; TEST:

(begin-for-test
  (check-equal? (tree-move TREE-SELECTED 0 0 0 0)
                TREE-SELECTED-INITIAL))



;; lot-move : ListOfTrees NonNegInt NonNegInt Int Int -> ListOfTrees
;; GIVEN: A ListOfTree lot, x and y coordinate of mouse, the distance mouse
;;        has moved in x and y direction
;; RETURN:A new list of trees after movement
;; EXAMPLE:see below in test
;; DESIGN STRATEGY:use HOF map on LOT

(define (lot-move lot mx my x-distance y-distance)
  (map
   ;; Tree -> Tree
   ;; RETURN: A new tree after movement
   (lambda (t) (tree-move t mx my x-distance y-distance))
   lot))

;; TEST:

(begin-for-test
  (check-equal? (lot-move LOT1 0 0 0 0) LOT1))


;; mouse-move-distance-x : Node NonNegInt NonNegInt -> Int
;; mouse-move-distance-y : Node NonNegInt NonNegInt -> Int
;; GIVEN: A Node, x and y coordinate of mouse
;; RETURN: The distance mouse has moved in x and y direction
;; EXAMPLE:see below in test
;; DESIGN STRATEGY:combine simpler functions
(define (mouse-move-distance-x node mx my)
  (- mx (node-mx node)))
(define (mouse-move-distance-y node mx my)
  (- my (node-my node)))

;; TEST:

(begin-for-test
  (check-equal? (mouse-move-distance-x
                 CIRCLE-NODE-AT-100-20-UNSELECTED 10 0) 10)
  (check-equal? (mouse-move-distance-y
                 CIRCLE-NODE-AT-100-20-UNSELECTED 0 10) 10))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; tree-after-button-up : Tree NonNegInt NonNegInt -> Tree
;; RETURN: A Tree same as given tree with button-up event
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: use template of tree
(define (tree-after-button-up tree mx my)
  (make-tree (node-after-button-up (tree-root tree))
             (lot-after-mouse-event (tree-children tree)
                                    mx my "button-up")))

;; TEST:

(begin-for-test
  (check-equal? (tree-after-button-up TREE1 10 0) TREE1)
  (check-equal? (tree-after-button-up TREE1 10 10) TREE1))


;; node-after-button-up : Node -> Node
;; RETURN: A Node same as given node with button-up event
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: use template of node
(define (node-after-button-up node)
  (make-node (node-x node)
             (node-y node)
             (node-shape node)
             #false
             0
             0))

;; TEST:

(begin-for-test
  (check-equal? (node-after-button-up CIRCLE-NODE-AT-100-20-UNSELECTED)
                CIRCLE-NODE-AT-100-20-UNSELECTED))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; check-any-node-lot-loop : LOT -> List
;; GIVEN: A LOT
;; RETURNS: a list of true or false stating if node in lot is selected
;; EXAMPLE:see below in test
;; STRATEGY : diving in cases by value of lot
;; HALTING MEASURE : lenght of lot

(define (check-any-node-lot-loop lot)
  (cond
    [(empty? lot) empty]
    [else (append (check-any-node-tree-loop (first lot))
                  (check-any-node-lot-loop (rest lot)))]))


;; TESTS :

(begin-for-test
  (check-equal? (check-any-node-lot-loop (list (initial-tree CIRCLE)))
                (list false)))



;; check-any-node-tree-loop : Tree -> List
;; GIVEN: A Tree
;; RETURNS: a list of true or false stating if node in tree is selected
;; EXAMPLE:see below in test
;; STRATEGY : combining simpler functions

(define (check-any-node-tree-loop tree)
  (cons (check-any-node (tree-root tree))
        (check-any-node-lot-loop (tree-children tree))))

;; TESTS :

(begin-for-test
  (check-equal? (check-any-node-tree-loop (initial-tree CIRCLE))
                (list false)))


;; check-any-node : Node -> Boolean
;; GIVEN: A Node
;; RETURNS: true or false stating if node is selected
;; EXAMPLE:see below in test
;; STRATEGY : diving in cases by if node-selected?

(define (check-any-node node)
  (cond
    [(node-selected? node) true]
    [else false]))

;; TESTS :

(begin-for-test
  (check-equal? (check-any-node (initial-node CIRCLE)) false))



;; check-any-node-selected? : LOT -> Boolean
;; GIVEN: A LOT
;; RETURNS: true or false stating if node is selected in this list
;; EXAMPLE:see below in test
;; STRATEGY : combining simpler functions

(define (check-any-node-selected? lot)
  (member true (check-any-node-lot-loop lot)))

;; TESTS :

(begin-for-test
  (check-equal? (check-any-node-selected? LOT1) false))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; world-after-key-event : World Integer Integer keyEvent -> World
;; GIVEN: a World, a location, and a keyEvent
;; RETURNS: the state of the world as it should be following
;;          the given key event

(define (world-after-key-event world key-event)
  (make-world (lot-after-key-event (world-trees world)
                                   key-event)))

;; TEST:

(begin-for-test
  (check-equal? (world-after-key-event WORLD-LOT1 "a") WORLD-LOT1))


;; lot-after-key-event : ListOfTrees KeyEvent -> ListOfTrees
;; RETURN: A new ListOfTrees after given key event
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: use template of KeyEvent

(define (lot-after-key-event lot  key-event)
  (cond
    [(and (key=? key-event "c") (not (check-any-node-selected? lot)))
     (cons (initial-tree CIRCLE) lot)]
    [(and (key=? key-event "s") (not (check-any-node-selected? lot)))
     (cons (initial-tree SQUARE) lot)]
    
    [(key=? key-event "s") (lot-iterator-c-s-pressed lot SQUARE)]
    [(key=? key-event "c") (lot-iterator-c-s-pressed lot CIRCLE)]
    [(key=? key-event "d") (lot-iterator-d-pressed lot)]
    
    [else lot]))

;; TEST:

(begin-for-test
  (check-equal? (lot-after-key-event empty  "c") (list INITIAL-TREE))
  (check-equal? (lot-after-key-event empty  "d") empty)
  (check-equal? (lot-after-key-event empty  "s") (list INITIAL-TREE-SQUARE))
  (check-equal? (lot-after-key-event (list TREE-ROOT2-INITIAL)  "s")
                (list TREE-ROOT2))
  (check-equal? (lot-after-key-event (list TREE-ROOT1-INITIAL)  "c")
                (list TREE-ROOT1))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; leftmost-x-value : ListOfTrees Int -> Int
;; RETURN: The mininum x-coordinate of tree-root in lot
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: use template of lot
;; HALTING MEASURE : lenght of lot

(define (leftmost-x-value lot min-x)
  (cond
    [(empty? lot) min-x]
    [(< (root-x (first lot)) min-x)
     (leftmost-x-value (rest lot)
                       (root-x (first lot)))]
    [else (leftmost-x-value (rest lot) min-x)]))

;; TEST:

(begin-for-test
  (check-equal? (leftmost-x-value LOT1 100) 100)
  (check-equal? (leftmost-x-value LOT1 500) 100))

;; left-x-value : Tree -> Int
;; RETURN: The leftmost x-coordinate in children of given tree
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: divide into cases on length of tree's children

(define (left-x-value tree)
  (if (empty? (tree-children tree))
      0
      (leftmost-x-value (tree-children tree)
                        500)))

;; TEST:

(begin-for-test
  (check-equal? (left-x-value TREE1) 0)
  (check-equal? (left-x-value TREE9) 130))


;; make-node-by-values : NonNegInt NonNegInt Int Int Shape -> Node
;; RETURN: A new node with given coordinate and movement
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: use template of node

(define (make-node-by-values x y dx dy shape)
  (make-node (- x dx) (+ y dy) shape false
             INITIAL-MX INITIAL-MY))

;; TEST:

(begin-for-test
  (check-equal? (make-node-by-values 100 20 10 10 CIRCLE)
                (make-node 90 30 CIRCLE false 0 0)))

;; make-node-with-offset : Tree -> Shape
;; RETURN: Make a new node at appropriate position
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: divide into cases on length of tree-children

(define (make-node-with-offset tree shape)
  (if (empty? (tree-children tree))
      
      (make-node-by-values
       (root-x tree) (root-y tree)
       0 OFFSET shape)
      
      (make-node-by-values
       (left-x-value tree) (root-y tree)
       OFFSET OFFSET shape)))

;; TEST:


(begin-for-test
  (check-equal? (make-node-with-offset TREE1 CIRCLE)
                (make-node 130 200 "circle" #false 0 0))
  (check-equal? (make-node-with-offset TREE9 CIRCLE)
                (make-node 70 80 "circle" #false 0 0)))


;; make-tree-from-node : Node -> Tree
;; RETURN: A tree with node as its root and empty children
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: use template of tree

(define (make-tree-from-node node)
  (make-tree node empty))

;; TEST:

(begin-for-test
  (check-equal?
   (make-tree-from-node SQUARE-NODE-AT-130-80-UNSELECTED)
   (make-tree SQUARE-NODE-AT-130-80-UNSELECTED empty)))


;; lot-iterator-c-s-pressed : ListOfTrees Shape -> ListOfTrees
;; RETURN: A new list of trees after c or s pressed
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: use HOF map on lot

(define (lot-iterator-c-s-pressed lot shape)
  (map
   ;; Tree -> Tree
   ;; RETURN: a new tree after c/s pressed
   (lambda (t) (tree-iterator-c-s-pressed t shape))
   lot))

;; TEST:
(begin-for-test
  (check-equal? (lot-iterator-c-s-pressed LOT1 CIRCLE)
                LOT1))


;; tree-iterator-c-s-pressed : Tree Shape -> Tree
;; RETURN: A new tree after c/s preesed
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: use template of tree

(define (tree-iterator-c-s-pressed tree shape)
  (if (node-selected? (tree-root tree))
      (add-child tree shape)
      (make-tree (tree-root tree)
                 (lot-iterator-c-s-pressed (tree-children tree) shape))))

;; TEST:


(begin-for-test
  (check-equal? (tree-iterator-c-s-pressed TREE-ROOT2-INITIAL SQUARE)
                TREE-ROOT2))


;; add-child : Tree Shape -> Tree
;; RETURN: Add a child for the given tree
;; EXAMPLE: see below in test
;; DESIGN STRATEGY: use template of tree

(define (add-child tree shape)
  (make-tree (tree-root tree)
             (cons (make-tree-from-node (make-node-with-offset tree shape))
                   (lot-iterator-c-s-pressed (tree-children tree)
                                             shape))))

;; TEST:

(begin-for-test
  (check-equal? (add-child TREE-ROOT2-INITIAL SQUARE)
                TREE-ROOT2))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

;; lot-iterator-d-pressed : ListOfTrees -> ListOfTrees
;; GIVEN: A ListOfTrees lot
;; RETURN: A new list of trees after "d" pressed
;; EXAMPLE:see below in test
;; DESIGN STRATEGY: divide by cases on contents of lot
;; HALTING MEASURE : lenght of lot

(define (lot-iterator-d-pressed lot)
  (cond
    [(empty? lot) empty]
    [(node-selected? (tree-root (first lot)))
     (append (tree-iterator-d-pressed (first lot))
             (lot-iterator-d-pressed (rest lot)))]
    [else
     (cons (tree-iterator-d-pressed (first lot))
           (lot-iterator-d-pressed (rest lot)))]))

;; TEST:

(begin-for-test
  (check-equal? (lot-iterator-d-pressed (list TREE-ROOT2))
                (list TREE-SQUARE)))


;; tree-iterator-d-pressed : Tree -> Tree/ListOfTree
;; RETURN: A tree after "d" pressed
;; EXAMPLE: see below in test
;; DESIGN STRATEGY: divide into cases on node-selected?

(define (tree-iterator-d-pressed tree)
  (if (node-selected? (tree-root tree))
      (lot-iterator-d-pressed (tree-children tree))
      (make-tree (tree-root tree)
                 (lot-iterator-d-pressed (tree-children tree)))))

;; TEST:

(begin-for-test
  (check-equal? (tree-iterator-d-pressed TREE-ROOT2)
                (list TREE-SQUARE)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; root-x : Tree -> Int
;; root-y : Tree -> Int
;; RETURN: X and y coordinate of given tree's root node
;; DESIGN STRATEGY: combine simpler functions
(define (root-x tree)
  (node-x (tree-root tree)))
(define (root-y tree)
  (node-y (tree-root tree)))

;; TEST:
(begin-for-test
  (check-equal?
   (root-x TREE1)
   130
   "The result is wrong.")
  (check-equal?
   (root-y TREE1)
   140
   "The result is wrong."))