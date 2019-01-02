set 01
Introduction
Do the following exercises from HtDP/2e. These should all be easy, but be sure to include all of the items in the design recipe. If there are no data definitions necessary, then say so in your file.

Note: the numbers of these exercises in HtDP2e may change.

Exercise 11. Name this function distance-to-origin.
Exercise 14 (string-last).
Exercise 16 (image-area).
Exercise 19 (string-insert).
Exercise 20 (string-delete).
------------------------------------------------------------------------------------------------------------------------------------
set 02
finite state machine and function design

You are to design a set of functions that illustrate the workings of a finite-state machine for accepting strings that exactly match the regular expression
(q | x)* u* (a | b)* d (e | f)*
. So all of the following should be accepted:
d
de
df
def
dfe
ad
adf
abbde
uabdef
uuabdef
qd
quaade
qxuuadeff
qxuuabbadeff
  
but
que
qaud
quaudefd
qxuuaqdef
should not.
The legal inputs to the machine are precisely the strings "q", "x", "u", "a", "b", "d", "e", and "f". Any other inputs violate the machine's contract, and its behavior on those inputs is unspecified.

First, perform an information analysis and design the data representation for the states of your machine. You may wish to write down a state-transition diagram (like the ones here) to illustrate the meaning of your state. Keep your diagram as simple as possible. You should submit your state-transition diagram either as ascii art in your solution file, or as a jpg or pdf (called "fsm.jpg" or "fsm.pdf"), created in your favorite graphics program.

Then design the following functions and provide them in a file named fsm.rkt

initial-state : Number -> State
GIVEN: a number
RETURNS: a representation of the initial state
of your machine.  The given number is ignored.

next-state : State MachineInput -> State
GIVEN: a state of the machine and a machine input
RETURNS: the state that should follow the given input.

accepting-state? : State -> Boolean
GIVEN: a state of the machine
RETURNS: true iff the given state is a final (accepting) state

error-state? : State -> Boolean
GIVEN: a state of the machine
RETURNS: true iff there is no path (empty or non-empty) from the given
state to an accepting state
You will need to provide data definitions for State and for MachineInput. Be sure to write an interpretation for each state. There is no need to write an interpretation for MachineInput, since the problem is already phrased in terms of strings.

As before, remember that we will be doing automated testing of your solution. So be sure that your solution is in the right place (set02/fsm.rkt in your privatecs5010f16/pdp-YOURUSERNAME repository), and that it provides all the functions listed above. To see if your file is in the right place, insert the following line somewhere near the top of your file:

(check-location "02" "fsm.rkt")
The University has gone on a health campaign and has replaced the candy machine in the basement with a health-food machine. The machine dispenses two kinds of snacks: kale chips and carrot sticks. A bag of kale chips is $0.75, and a bag of carrot sticks is $0.50. The machine accepts only quarters. A customer can put any number of quarters in the machine, and then select an item. If the customer has deposited enough money into the machine, and the machine is not out of the selected item, then the machine will dispense the requested item. If the machine is out of the selected item, the machine will flash "Out of Item". The customer may also press "change", in which case the machine will return any unspent money that the customer has put in during the current transaction. If none of these apply, the machine does nothing.
For example, the customer may put two 25-cent pieces into the machine. If he then selects the carrots, the machine will dispense a bag of carrots. If he tries to select the kale instead, nothing will happen. If the customer then presses "change", the machine will return any unspent money that he has deposited. The customer may request "change" at any time, whether or not he has ordered anything.

The machine has a container, called the bank, that contains all the money it has kept from customers' purchases. The customer's money is added to the bank only after he or she has successfully made a purchase.

The possible inputs from the customer are given by the following data definition:

A CustomerInput is one of
-- a PosInt        interp: insert the specified number of quarters
-- "kale"          interp: request a bag of kale chips
-- "carrots"       interp: request a bag of carrots
-- "change"        interp: return all the unspent money that the
                             customer has inserted

A MachineOutput is one of
-- "kale"           interp: machine dispenses a bag of kale chips
-- "carrots"        interp: machine dispenses a bag of carrot sticks
-- "Out of Item"    interp: machine displays "Out of Item"
-- a PosInt         interp: machine releases the specified number of quarters
-- "Nothing"        interp: the machine does nothing
You are to write a file called snack-machine.rkt that provides the following functions:

initial-machine : NonNegInt NonNegInt -> MachineState
GIVEN: a number of bags of kale chips and carrot sticks
RETURNS: the state of a machine loaded with the given numbers of bags
of kale chips and carrot sticks, with an empty bank.

machine-next-state : MachineState CustomerInput -> MachineState
GIVEN: a machine state and a customer input
RETURNS: the state of the machine that should follow the customer's
input

machine-output : MachineState CustomerInput -> MachineOutput
GIVEN: a machine state and a customer input
RETURNS: a MachineOutput that describes the machine's response to the
customer input

machine-remaining-kale : MachineState -> NonNegInt
GIVEN: a machine state
RETURNS: the number of bags of kale chips left in the machine

machine-remaining-carrots : MachineState -> NonNegInt
GIVEN: a machine state
RETURNS: the number of bags of carrots left in the machine

machine-bank : MachineState -> NonNegInt
GIVEN: a machine state
RETURNS: the amount of money in the machine's bank, in cents
As before, remember that we will be doing automated testing of your solution. So be sure that your solution is in the right place, and that it provides all the functions listed above. You can use check-location to check whether your file is in the right place.

Your space probe to Pluto has just landed. Here's the situation:
At every step, the probe can move forward some number of steps, or it can rotate 90 degrees either right or left. The probe moves by taking steps of exactly 1 cm.
The probe has landed facing north (up).
We will use graphics-style coordinates instead of standard mathematical coordinates. That is, when the probe moves north, its y-coordinate DECREASES. So if it takes one step north from the origin, it is at (0, -1).
The probe's mechanism is a little unreliable. If you send it forward by some number of steps, it may take an extra step, or it may take one step too few. However, it will never move backwards, nor will it ever turn by mistake.
You are to write a file called probe.rkt that provides the following functions:

probe-at: Integer Integer -> Probe
GIVEN: an x-coordinate and a y-coordinate
RETURNS: a probe with its center at those coordinates, facing north.

probe-turned-left : Probe -> Probe
probe-turned-right : Probe -> Probe
GIVEN: a probe
RETURNS: a probe like the original, but turned 90 degrees either left
or right.

probe-direction-equal? : Probe Probe -> Boolean
GIVEN: two probes
RETURNS: true iff the two probes are facing in the same direction,
else false

probe-location-equal? : Probe Probe -> Boolean
GIVEN: two probles
RETURNS: true iff the two probes are at the same location

probe-forward-possible-outcome? : Probe PosInt Probe -> Boolean
GIVEN: two probes and a distance
RETURNS: true iff the first probe, given a move-forward command with
the specified number of steps, could wind up in the state described by
the second probe.
  
------------------------------------------------------------------------------------------------------------------------------------
set 03
iterative system design

(screensaver-1). Your boss has assigned you to a project to build a screensaver. The specifications for the screensaver are as follows:
The screensaver is a universe program that displays two circles that move around a canvas.
Each circle is displayed as an outline blue circle with a radius of 40 pixels. In addition, the circle's current velocity is displayed as a string (vx, vy) in the center of the circle.
The circles bounce smoothly off the edge of the canvas. Bouncing is defined as follows: if the circle in its normal motion would hit or go past one side of the canvas at the next tick, then instead at the next tick it should appear tangent to the edge of the canvas, travelling at the same speed, but in the opposite direction. If the circle would go past a corner, then both the x- and y- velocities are reversed. We call this a perfect bounce.
Here's a clarification (thanks to Professor Clinger):

The x and y coordinates of a circle are constrained to keep the entire circle within the canvas at all times. If a circle's position and velocity would cause any part of a circle to lie outside of the canvas at the end of a tick, then its x and y coordinates at the end of that tick are calculated as follows: first calculate the new x and y coordinates without regard to the boundaries, and then adjust those calculated coordinates (separately) by the smallest adjustments that keep the entire circle within the canvas. If an x or y coordinate is adjusted during that process, then the corresponding velocity component is reversed as well.

Example: A circle whose center is at x=45 and y=80, with velocity components vx=-15 and vy=20 would appear at x=30, if it were unconstrained by the wall. So at the next tick, the center should be at x=40 and y = 100, with velocity components vx=15 and vy=20.

The space bar pauses or unpauses the entire simulation. The simulation is initially paused.
The canvas is 400 pixels wide and 300 pixels high.
The two circles are initially centered at positions (200,100) and (200,200), and have velocities of (-12, 20) and (23, -14), respectively. Remember that we are using computer-graphics coordinates, in which y increases as you go down the page (south) and in which (0,0) is the upper left-hand corner of the canvas.
Here's a demo:


You are to deliver a file named screensaver-1.rkt that provides the following functions:

;; screensaver : PosReal -> WorldState
;; GIVEN: the speed of the simulation, in seconds/tick
;; EFFECT: runs the simulation, starting with the initial state as
;; specified in the problem set.
;; RETURNS: the final state of the world

;; initial-world : Any -> WorldState
;; GIVEN: any value (ignored)
;; RETURNS: the initial world specified in the problem set

;; world-after-tick : WorldState -> WorldState
;; RETURNS: the world state that should follow the given world state
;; after a tick.

;; world-after-key-event : WorldState KeyEvent -> WorldState
;; RETURNS: the WorldState that should follow the given worldstate
;; after the given keyevent

;; world-circ1 : WorldState -> Circle
;; world-circ2 : WorldState -> Circle
;; world-paused? : WorldState -> Boolean
;; RETURNS: the specified attribute of the WorldState
;; NOTE: if these are part of the world struct, you don't need to
;; write any deliverables for these functions.

;; new-circle : NonNegInt NonNegInt Int Int -> Circle
;; GIVEN: 2 non-negative integers x and y, and 2 integers vx and vy
;; RETURNS: a circle centered at (x,y), which will travel with
;; velocity (vx, vy).

;; circ-x : Circle -> NonNegInt
;; circ-y : Circle -> NonNegInt
;; circ-vx : Circle -> Int
;; circ-vy : Circle -> Int
;; RETURNS: the coordinates of the center of the circle and its
;; velocity in the x- and y- directions.
(screensaver-2). Your boss has now decided to build a better screensaver. This one is like the original, except for the following:
Each circle is selectable and draggable. Depressing the mouse button within a circle causes the circle to be "selected". When a circle is selected, it and its velocityare displayed in red instead of blue.
When the mouse button is down, its location should be indicated by a solid red circle with a radius of 5 pixels.

Once a circle has been selected, you should be able to drag it around the Universe canvas with the mouse. As you drag it, the position of the mouse within the circle (as indicated by the red circle), should not change. When the mouse button is released, the circle should go back to its unselected state (outline blue) in its new location.
We refer to this behavior as "smooth dragging." We will be implementing other objects with this behavior in future problem sets.
A selected circle may be dragged in a way that causes some of it to be dragged outside the canvas. If, when the circle is unselected, some or all of it is outside the canvas, its motion is determined by the same rules as in the previous question. Example: A circle is dragged so that its center is at x=25 and y=80, with velocity components vx=-15 and vy=20, and is then released. At the next tick, it should have its center at x=20 x=40 and y=100, while its velocity components would become vx=15 and vy=20.
All of this works whether or not the simulation is paused.
Here's a demo:


You are to deliver a file named screensaver-2.rkt that provides all the functions above, plus the following:

;; world-after-mouse-event
;;  : WorldState Int Int MouseEvent -> WorldState
;; GIVEN: A World, the x- and y-coordinates of a mouse event, and the
;; mouse event
;; RETURNS: the world that should follow the given world after the given mouse
;; event.

;; circ-after-mouse-event :  Circle Int Int MouseEvent -> Circle
;; GIVEN: A circle, the x- and y-coordinates of a mouse event, and the
;; mouse event
;; RETURNS: the circle that should follow the given circle after
;; the given mouse event

;; circ-selected? : Circle -> Boolean
;; RETURNS: true iff the given circle is selected.

;; new-circle
;; as before, but now it returns an UNSELECTED circle.
------------------------------------------------------------------------------------------------------------------------------------
set 04
Computing with lists

(screensaver-3). Your boss has decided that your screensaver needs more features.
The new screensaver will display a list of circles.
Initially, there are no circles. Hitting the "n" key adds a new circle, at the center of the canvas, at rest (velocity is 0).
When a circle is selected, the arrow keys increase the velocity of the circle in the specified direction (up, down, left, or right). Each push of the arrow key increases the velocity by 2 pixels/tick. When the circle is deselected, the circle resumes its motion with the new velocity. Each circle displays its velocity, as in the past.
When a circle is selected, the "d" key drops a pen down. When the pen is down, the circle records on the screen a dot marking its center at each tick. The dot is displayed as a solid black circle of radius 1. No marks are made during a drag.
When a circle is selected, the "u" key lifts the pen up. When the pen is up, the circle does not leave a track on the screen.
When a circle is selected, the "e" key erases all the marks made by that circle.
Here's a demo:


You are to deliver a file named screensaver-3.rkt that provides all the functions of screensaver-2.rkt (except for world-circ1 and world-circ2), plus the following:

;; world-circles : WorldState -> ListOfCircle
;; RETURNS: the specified attribute of the WorldState
;; NOTE: if these are part of the world struct, you don't need to
;; write any deliverables for these functions.

;; circle-after-key-event : Circle KeyEvent -> Circle
;; RETURNS: the state of the circle that should follow the given
;; circle after the given key event

;; circle-pen-down? : Circle -> Boolean
;; RETURNS: true if the pen in the given circle is down
  
In addition, you must turn in a file containing the call graph for your program. This file must show which functions call which, so we (and you) can see the overall structure of your program and find all the recursive calls. You may turn this in as a text file, pdf, jpg, or Racket file. Call your file call-tree with an appropriate suffix, and bring a paper copy to your codewalk.

Remember to use the principle that Program Structure Follows Data Structure, illustrated in Lesson 3.4, to organize your program.

Professor Felleisen and Professor Shivers each keep their class lists on slips of paper, one student on each slip. Professor Felleisen keeps his list on slips of yellow paper. Professor Shivers keeps his list on slips of blue paper.
Unfortunately, both professors are sloppy record-keepers. Sometimes they have more than one slip for the same student. Sometimes they record the student names first-name first; sometimes they record the names last-name first.

One day, Professor Felleisen was walking up the stairs in WVH, talking to one of his graduate students. At the same time, Professor Shivers was walking down the stairs, all the time talking to one of his graduate students. They collided, and dropped all the slips containing their class lists on the stairs, where they got all mixed up.

Your job is to clean up this mess. Deliver a file named class-lists.rkt that provides the following functions:

felleisen-roster : ListOfSlip -> ListOfSlip
GIVEN: a list of slips
RETURNS: a list of slips containing all the students in Professor
Felleisen's class, without duplication.

shivers-roster: ListOfSlip -> ListOfSlip
GIVEN: a list of slips
RETURNS: a list of slips containing all the students in Professor
Shivers' class, without duplication.

possible-roster? : ListOfSlip -> Boolean
GIVEN: a list of slips
RETURNS: true iff all the slips in the list are the same color,
                  and no student is represented twice.

acceptable-felleisen-answer? : ListOfSlip ListOfSlip -> Boolean
GIVEN: two lists of slips, lst1 and lst2
RETURNS: true iff every student on a yellow slip in lst1 appears once
and only once in lst2.
EXAMPLES:
  Let lst1 = (list
              (make-slip "yellow" "Wang" "Xi")
              (make-slip "blue" "Jones" "Tom")
              (make-slip "yellow" "Xi" "Wang")
              (make-slip "yellow" "Shriram" "K."))

This list contains two of Professor Felleisen's students: Wang Xi (or
maybe Xi Wang), and Shriram K.  Therefore the following are acceptable
answers and should return true when given to acceptable-felleisen-answer? 

(list
 (make-slip "yellow" "Wang" "Xi")
 (make-slip "yellow" "Shriram" "K."))

(list
 (make-slip "yellow" "Shriram" "K.")
 (make-slip "yellow" "Wang" "Xi"))

(list
 (make-slip "yellow" "Shriram" "K.")
 (make-slip "yellow" "Xi" "Wang"))

(list
 (make-slip "yellow" "K." "Shriram")
 (make-slip "yellow" "Xi" "Wang"))

Any answer with a blue slip, or any answer that includes both Xi Wang
and Wang Xi, or any answer that does not include both Xi Wang and
Shriram K, is not acceptable.
Here is the beginning of the data definition for ListOfSlip:

(define-struct slip (color name1 name2))
A Slip is a (make-slip Color String String)

A Color is one of
-- "yellow"
-- "blue"
As mentioned before, the professors are confused about first names and last names, so (make-slip "yellow" "Wang" "Xi") and (make-slip "yellow" "Xi" "Wang") represent the same student in Professor Felleisen's class. The phrase "without duplication" in the purpose statements above means that your function should return a list in which this student is represented only once.

Be sure to finish the data definitions I've started above, and to provide make-slip, slip-color, slip-name1 and slip-name2.

------------------------------------------------------------------------------------------------------------------------------------
set 05
Generalizing similar function

(The absent-minded professors, part 2). Reimplement the last problem from last week's problem set (class-lists.rkt), but using HOFs wherever possible and appropriate. Use the filename q1.rkt.
(The nervous registrar) The Registrar has heard about your excellent work with the absent-minded professors and so he asks you to help him solve the following problem:
He needed a program to take a list of (student, class) pairs, and produce a list of class rosters, one roster for each class that has at least one student enrolled (detailed specifications below). He hired several teams of Racket programmers who had never taken PDP, and therefore produced solutions that were late and over-budget. Now he needs your help to test their solutions.

You are to design a tester that will accept any good solution to the registrar's original problem, and reject any incorrect solution. Your solution should be in the form of a file called q2.rkt. Here are more detailed specifications:

An EnrollmentAssertion is a (make-enrollment Student Class).
(make-enrollment s c) represents the assertion that student s is
enrolled in class c.

A ClassRosterAssertion is a (make-roster Class SetOfStudent).
(make-roster c ss) represents the assertion that the students in class
c are exactly the students in set ss.

Student is unspecified, but you may assume that students may be
compared for equality with equal? (Among other things, this means that
we don't have to worry about the first name/last name problems that
Professors Shivers and Felleisen had; the Registrar knows the student
name exactly)

Class is unspecified, but you may assume that classes may be
compared for equality with equal?

Your code should not depend on your choice of data type; that
is, it should work for any definition of Student and Class (so long as
each is testable using equal?, as specified above).

A SetOfX is a list of X's without duplication.  Two SetOfX's are
considered equal if they have the same members.

Example: (list (list 1 2) (list 2 1)) is NOT a SetOfSetOfNumber,
because (list 1 2) and (list 2 1) represent the same set of numbers. 

A ProposedSolution is a function with contract
SetOfEnrollmentAssertion -> SetOfClassRosterAssertion
that is, it is a function that takes a SetOfEnrollmentAssertion and
produces a SetOfClassRosterAssertion

EXAMPLE:
If soln1 is a ProposedSolution, we might have
  (soln1
    (list (make-enrollment "John" "PDP")
          (make-enrollment "Kathryn" "Networks")
          (make-enrollment "Feng" "PDP")
          (make-enrollment "Amy" "PDP")
          (make-enrollment "Amy" "Networks")))
=>
 (list
   (make-roster "PDP" (list "John" "Feng" "Amy"))
   (make-roster "Networks" (list "Kathryn" "Amy")))

This is an example of correct behavior by a ProposedSolution.

In the output of a correct ProposedSolution, the classes may be in any
order, and the students in each class may be in any order, but there
must be no duplication of classes and no duplication of students
within a class.

You are to provide the following functions:

make-enrollment
enrollment-student
enrollment-class
make-roster
roster-classname
roster-students

behavior-correct? : ProposedSolution SetOfEnrollmentAssertion -> Boolean
GIVEN: a ProposedSolution soln-fn and a SetOfEnrollmentAssertion se
RETURNS: true iff the output of soln-fn on se is an example of correct
behavior by a ProposedSolution.
EXAMPLE: See example above

enrollments-to-rosters: SetOfEnrollmentAssertion -> SetOfClassRoster
GIVEN: a set of enrollments
RETURNS: a correct set of class rosters for the given enrollments

enrollments-to-rosters-bad-1: SetOfEnrollmentAssertion -> SetOfClassRoster
enrollments-to-rosters-bad-2: SetOfEnrollmentAssertion -> SetOfClassRoster
enrollments-to-rosters-bad-3: SetOfEnrollmentAssertion -> SetOfClassRoster
GIVEN: a set of enrollment assertions
RETURN: an incorrect set of class rosters for the given enrollments.
The three functions should return DIFFERENT incorrect sets of class
rosters. 
Test your behavior-correct? function by writing a correct enrollments-to-rosters function and several incorrect ones (the 3 shown above are just a start).

The problem requires that your solution work for any choice of representation of Student and Class, so long as they are testable for equality using equal?, so you should also test your solution using at least two different data types for Student and Class. You may assume that any ProposedSolution also works for any such choice of data types.

As elsewhere in this problem set, use HOFs whenever possible and appropriate.

You will find this problem much easier if you follow the slogan "The Structure of the Program Follows the Structure of the Data". To help you with this concept, please turn in a file illustrating the call graph for your program. This file must contain a diagram showing which functions call which, so we (and you) can see the overall structure of your program. You may turn this in as a text file, pdf, jpg, or Racket file. Call your file call-tree with an appropriate suffix

------------------------------------------------------------------------------------------------------------------------------------
set 06
Trees and Branching systems

In this problem, you will design and implement a system for a graphical interface for trees. Your system will allow you to create and manipulate trees on a canvas. Create a file called "q1.rkt" with the following properties:
The canvas starts empty. Its size is 500 pixels wide by 400 pixels high.
Nodes of the tree are rendered either as circles or squares of a fixed size. The circles should be of radius 20, and the squares should appear the same size as the circles. your system should allow you to change the size of the nodes for the next run by changing a single line of your code.
Unselected nodes should be rendered in outline mode; selected nodes should be rendered in solid mode.
When the tree is displayed, there should be a straight blue line from the center of a node to the center of each of its sons.
You can select a node by clicking anwyhere inside it. Selected nodes are displayed as green solid circles. Clicking on a node selects only the node, not any of its subtrees. If the mouse is clicked in the overlap of two or more nodes, all the nodes are selected, even if one node is a son or descendant of the other.
Dragging a selected node causes the entire tree rooted at that node to be dragged, using smooth dragging. The relative positions of all the nodes in the subtree should stay the same. It is ok if this action causes some nodes to be moved off the edge of the canvas; if the node is moved again so that they are now back on the canvas, they should reappear in the proper place.
Hitting "c" when a node is selected adds a new son to the selected node. The new node should be a circle whose center has an x-coordinate that is 3 radii to the left of the center of the currently leftmost son, and a y-coordinate that is 3 radii down from the center of the parent. If the node has no sons, then hitting "c" should create the node's first son, appearing 3 radii down and directly beneath the node.
Hitting "c" when no node is selected creates a new root node that is a circle. The new node should appear in the center of the top of the canvas. The root appears tangent to the top of the canvas and initially has no sons.
Hitting "s" at any time should behave like "c", except that the new node created is a square instead of a circle.
Hitting "d" when a node is selected deletes the selected node. When a node is deleted, its sons become the sons of the parent of the deleted node. If the deleted node is a root node (and therefore has no parent), then its sons become root nodes. See the video for examples.
Here's a demo.


Your solution should provide the following functions:

initial-world : Any -> World
GIVEN: any value
RETURNS: an initial world.  The given value is ignored.

run :  Any -> World
GIVEN: any value
EFFECT: runs a copy of an initial world
RETURNS: the final state of the world.  The given value is ignored.

world-after-mouse-event : World Integer Integer MouseEvent -> World
GIVEN: a World, a location, and a MouseEvent
RETURNS: the state of the world as it should be following the given mouse event at that location.

world-after-key-event : World KeyEvent -> World
GIVEN: a World and a key event
RETURNS: the state of the world as it should be following the given key event
 
world-to-trees : World -> ListOfTree
GIVEN: a World
RETURNS: a list of all the trees in the given world.

tree-to-root : Tree -> Node
GIVEN: a tree
RETURNS: the node at the root of the tree
EXAMPLE: Consider the tree represented as follows:

                 A
                 |
       +---+-----+-----+
       |   |     |     |
       B   C     D     E
           |           |
         +---+      +-----+
         |   |      |     |
         F   G      H     I

If tree-to-root is given the subtree rooted at C, it should return the
data structure associated with node C. This data structure may or may
not include data associated with rest of the tree, depending on
whether you have chosen to represent nodes differently from trees.


tree-to-sons : Tree -> ListOfTree
GIVEN: a tree
RETURNS: the data associated with the immediate subtrees of the given
tree. 
EXAMPLE: In the situation above, if tree-to-sons is given the subtree
rooted at C, it should return a list consisting of the subtree rooted
at F and the subtree rooted at G.

[Note how these examples are expressed.  They are not just tests, but
are constructed to illuminate possible ambiguities or
misunderstandings in the purpose statement.  This is what a good
example does.]


node-to-center : Node -> Posn
RETURNS: the center of the given node as it is to be displayed on the
scene.
Note: this function returns a Posn (an ISL builtin).  This is for the
convenience of the testing framework, and you may or may not wish to
represent the center of the node in this way.

node-to-selected? : Node -> Boolean
RETURNS: true iff the given node is selected.
------------------------------------------------------------------------------------------------------------------------------------
set 07
Generalizing with contexts and invariants

You have taken a job with NextPython, Inc., the makers of GarterSnake. They want to add a better user interface for their development environment, so they want to add a pretty-printer which will convert the GarterSnake syntax-tree representation, from Lesson 7.4, into a nicely indented format.
Recall that the syntax-tree representation of GarterSnake is defined as follows:

;; A Program is a ListOfDefinition.

(define-struct def (name args body))
;; A Definition is a (make-def Variable ListOfVariable Exp)
;; INTERPRETATION:
;; name is the name of the function being defined
;; args is the list of arguments of the function
;; body is the body of the function.

(define-struct varexp (name))
(define-struct appexp (fn args))

;; An Exp is one of
;; -- (make-varexp Variable)
;; -- (make-appexp Variable ListOfExp)
;; INTERPRETATION;
;; (make-varexp v)                   represents a use of the variable v
;; (make-appexp f (list e1 ... en))  represents a call to the function
;;                                   named f, with arguments e1,..,en

;; A Variable is a Symbol.
Your task is to write

;; program-to-strings : Program PosInt -> ListOfString
;; GIVEN: GarterSnake program and a width
;; RETURNS: a representation of the program as a sequence of lines,
;;         following the formatting rules described below.
  
The detailed formatting rules will be given below, but here's an example of the function:

(define sample-program
  (list
   (make-def 'a-very-long-function-name
             (list 'x)
             (make-appexp 'f1 (list (make-varexp 'x))))
   (make-def 'f2 (list 'x 'a-very-long-variable-name 'y)
             (make-appexp 'f1 (list (make-varexp 'y))))
   (make-def 'f3 (list 'x 'z 't 'u)
             (make-appexp
              'f1
              (list (make-appexp
                     'f2 (list (make-varexp 'z)
                               (make-varexp 'y)))
                    (make-varexp 'z)
                    (make-appexp
                     'f1 (list (make-appexp
                                'f2 (list (make-varexp 'z)
                                          (make-varexp 'y)))
                               (make-varexp 'z))))))))

(program-to-strings sample-program 20)
=>
(list
 "def a-very-long-function-name (x) :"
 "    f1(x)"
 "def f2 (x,"
 "        a-very-long-variable-name,"
 "        y) :"
 "    f1(y)"
 "def f3 (x,z,t,u) :"
 "    f1(f2(z, y),"
 "       z,"
 "       f1(f2(z, y),"
 "          z))")
The formatting rules are as follows:

1. Layout of Programs:

The program is formatted by formatting each of definitions in
sequence.

2. Layout of Definitions:

2.1 Definitions are formatted by formatting the header, consisting of
the function name and arguments, followed by the function body,
starting on a new line with 4 additional spaces of indentation.

2.2 The function is header is formatted as

def fnname (arg1,arg2,arg3) :

if it will fit on a line no longer than the given width.

2.3 If the function header as shown in 2.2 will not fit into the given
width, then the function header should be laid out as

def fnname (arg1,
            arg2,
            ..
            argn) :

with each subsequent argument indented to line up under the first
argument.

3. Layout of Expressions:

3.1 For a function application, if the string

fnname(e1, e2, e3)

will fit on a single line, including its indentation and closing
parentheses (and comma if needed), then it is formatted that way.

3.2 If a function application, including its indentation, closing
parentheses and comma (if present), will not fit on a single line, but
the

fnname(e1,

will fit, then that much of the call is placed on a single line and
each of the remaining argument expressions begins on a separate line,
indented to line up under e1, and the closing parenthesis is placed on
the same line as the last argument expression. e.g.

foo(e1,
    e2,
    e3)

The closing paren is always on the last line.  Here "e2,", etc.
stand for a set of lines with a comma at the end of the last line,
and  "e2)" stand for a set of lines with a right parenthesis at the
end of the last line.

3.3 If

fnname(e1,

including its indentation, closing parentheses and comma, will not fit
on a single line, then the function name is placed on a line by
itself, and e1 is formatted beginning on the next line, indented with
1 additional space and a left parenthesis.  Subsequent arguments are
indented to line up under e1, e.g.

fnname
 (e1,
  e2,
  ..
  en)

Here "e2,", etc., stands for a set of lines with a comma at the end of
the last line, and "e2)", etc., stands for a set of lines with a right
parenthesis at the end of the last line.

3.4 If the expression is a variable, then it is placed on a line by
itself with its associated indentation, right parentheses and possible
comma even if that does not fit on one line.

4. General Properties

These properties all follow from the rules above, but are stated here
for reference. 

4.1 There are no blank lines.

4.2 No line ever ends with a space.

4.3 No left parenthesis is ever followed by a space, and no left
parenthesis ever appears at the end of a line.

4.4 No right parenthesis is ever preceded by a space or a comma, and
no right parenthesis ever appears at the start of a line.

4.4 For any given program and width, these rules determine one and only
one layout.

4.5 Following these rules may result in some lines that are longer
than the given width. For example, the name binary-search-helper-2
(besides being a poor function name!) will never fit within a line of
length 10.

To help you debug your program, we have provided in extras.rkt the function:

;; display-strings! : ListOfString -> Void
;; GIVEN: a list of strings
;; EFFECT: displays the strings on separate lines
;; RETURNS: no value
Here is a sample interaction:
> (display-strings!
   (cons "12345678901234567890"
         (program-to-strings sample-program 20)))
12345678901234567890
def a-very-long-function-name (x) :
    f1(x)
def f2 (x,
        a-very-long-variable-name,
        y) :
    f1(y)
def f3 (x,z,t,u) :
    f1(f2(z, y),
       z,
       f1(f2(z, y),
          z))
> 
Turn in your solution is a file named q1.rkt. You also must turn in a call graph for your solution, as in the last few problem sets. Call this file call-graph-1 with an appropriate suffix. Be sure your code includes a halting measure for every function that appears in a cycle in your call graph.

This problem requires careful analysis of the context. For example, when your program generates the string

 "       f1(f2(z, y),"
what information does it need to know about the context? If there is more than one thing that it needs to know, you can introduce multiple context variables to keep track of them.
The new model of the Pluto probe from ps02 is has just come out. The new probe is just like the one from Problem Set 02, except for the following:
It is programmable. The programming language for the robot is defined as follows:
A Program is a ListOfInstruction
Interp: A sequence of instructions, to be executed from left to
right. 

An Instruction is one of
-- (make-turn-left)            Interp: a turn-left instruction
-- (make-turn-right)           Interp: a turn-right instruction
-- (make-move-forward PosInt)  Interp: an instruction to move forward
                                       the given number of steps.
Alas, the new model of the Pluto probe is less reliable than the old one. If you send it forward by some number of steps, it may take up to TWO steps more than you told it to, or TWO steps less than you told it to. As before, the probe never moves backward, nor will it ever turn by mistake.
Thus, if the robot is facing east at x=20, then after a (make-move-forward 10) instruction, it will end still facing east, with its x-coordinate in the interval [28,32] (that is, 30 plus or minus 2), and its y-coordinate unchanged. If the instruction were instead (make-move-forward 1), then afterwards the x coordinate would be in the interval [20,23] (not [19,23]).

You are to write file named q2.rkt that provides the following function:

;; probe-possible-outcome? : Int Int Program Int Int -> Bool
;; GIVEN: starting coordinates x0, y0, a robot program p, and ending
;; coordinates x1, y1.
;; RETURNS: true iff the robot, starting at (x0, y0) and facing north,
;; and executing program p according to the tolerances given above,
;; could end at (x1, y1).
;; EXAMPLES:
;; Let p1 = (list (make-turn-right)
;;                (make-move-forward 10)
;;                (make-turn-right)
;;                (make-move-forward 5))
;; then (probe-possible-outcome 20 100 p1 x1 y1) = true iff
;; x1 is in the interval [28, 32] and y1 is in the interval
;; [103,107].

;; make-turn-right : -> Instruction
;; make-turn-left  : -> Instruction
;; make-move-forward : PosInt -> Instruction
;; As usual, if these are defined by define-struct, you need not
;; write any of the other deliverables (purpose statement, tests, etc.)
For this problem, your program needs to provide acceptable performance. "Acceptable" means that it can provide answers for programs of length 20 in a few seconds-- the time it takes to take a sip of coffee. If you are tempted to go get a fresh cup of coffee, your program is too slow. HINT: Small changes in your code will probably not help. You will need to devise a good representation for the possible states of the robot in the middle of the program execution.

------------------------------------------------------------------------------------------------------------------------------------
set 08
Recursion

Your employers at NextPython, Inc., the makers of GarterSnake, have introduced a new product, which they call Simplified Garter Snake (SGS). Simplified GarterSnake is just like GarterSnake, except for the following:
Function names may only be defined once,
Function names are global: a function may be called in an expression occurring before, after, or within its definition.
Functions may neither be passed as arguments nor returned as values.
Your task is to check SGS programs for potential infinite recursion. Consider the following SGS program:

(define some-loops
  (list
     (make-def 'f1 (list 'x) (make-appexp 'no-loop (list (make-varexp 'x))))
     (make-def 'f2 (list 'u 'y) (make-appexp 'f1 (list (make-varexp 'y))))
     (make-def 'f3 (list 'x 'u)
               (make-appexp 'f1 (list (make-appexp 'f4
                                             (list (make-varexp 'u)
                                                   (make-varexp 'w)))
                                      (make-varexp 'z))))
     (make-def 'f4 (list 'x 'y)
               (make-appexp 'f5
                            (list (make-varexp 'y)
                                  (make-varexp 'u))))
     (make-def 'f5 (list 'u)
               (make-appexp 'f2
                            (list (make-appexp 'f3 empty))))
     (make-def 'no-loop (list 'x) (make-varexp 'x))))
In this program, neither f1 nor f2 can lead to an infinite loop. However, f3 calls f4, which calls f5, which calls f3 again. So any call to f3, f4, or f5 may lead to an infinite recursion.

Note that this program takes advantage of the global scope of function names. For example, f1 calls no-loop, even though the definition of no-loop comes after the definition of f1.

Recall that the syntax-tree representation of GarterSnake is defined as follows:

;; A Program is a ListOfDefinition.

(define-struct def (name args body))
;; A Definition is a (make-def Variable ListOfVariable Exp)
;; INTERPRETATION:
;; name is the name of the function being defined
;; args is the list of arguments of the function
;; body is the body of the function.

(define-struct varexp (name))
(define-struct appexp (fn args))

;; An Exp is one of
;; -- (make-varexp Variable)
;; -- (make-appexp Variable ListOfExp)
;; INTERPRETATION;
;; (make-varexp v)                   represents a use of the variable v
;; (make-appexp f (list e1 ... en))  represents a call to the function
;;                                   named f, with arguments e1,..,en

;; A Variable is a Symbol.
You are to turn in a file named q1.rkt that provides the following functions:

make-def
make-varexp
make-appexp

any-loops? : Program -> Boolean
GIVEN: a valid SGS program p (that is, a GS program that obeys the
restrictions listed above).
RETURNS: true iff there is some function f in p that calls itself
either directly or indirectly, as in the example above.
(Resolution Theorem-Proving) The motivation for this problem is something called the satisfiability problem and resolution theorem-proving.
Here's the set-up:

Imagine that we have a number of boolean variables a, b, c, d, etc., which can either be true or false. You probably know that any boolean formula can be expressed inconjunctive normal form, that is, as a formula that looks like

(a ∨ ¬b ∨ c) & (d ∨ b) & (¬a ∨ c)
We will use conventional terminology for formulas in conjunctive normal form, in which a disjunction like (a ∨ ¬b ∨ c) is called a clause, and each of its disjuncts, which are either variables or their negations, are called literals. For example, the clause (a ∨ ¬b ∨ c) contains the literals a, ¬b, and c.

Given a choice of truth values for each variable, a clause is true if and only if at least one of its literals is true. Thus, the empty clause, which contains no literals, is always false. Similarly, a a choice of truth values for each variable, a formula is true if and only if all of its clauses are true. So the empty formula, which contains no clauses, is always true.

The question we want to answer is: given a formula in conjunctive normal form, is there a choice of truth values for its variables that makes the formula true? For any choice of truth values for the variables, This is called thesatisfiability problem. For our example, the assignment

a = T, b = F, c = T, d = T

makes the formula true, and we say that the formula is satisfiable.

We say that a formula F implies a formula G iff every choice of truth values that makes F true also makes G true. (If you know about truth tables, this is saying that G is true on every line of the truth table that makes F true.)

So, if a formula F implies a contradiction, which is never true, then the original formula F must also never be true. In this case, we say that F is unsatisfiable.

As it turns out, there is a simple method for deriving a contradiction from a formula, if one exists. This is called the rule of resolution. It says:

(a1 ∨ a2 ∨ ... b)         (¬b ∨ c1 ∨ c2 ∨ ...)
-----------------------------------------------
      (a1 ∨ a2 ∨ ... ∨ c1 ∨ c2 ∨ ...)
This rule says that if we have the two formulas above the line, then we can derive the formula below the line. Both the b and the ¬b are deleted in the conclusion.

In our example clauses above, (a ∨ ¬b ∨ c) & (d ∨ b) & (¬a ∨ c) , the first two clauses resolve on the b to get (a ∨ c ∨ d), and the first and third clauses resolve on a to get (¬b ∨ c).

We call b and ¬b complementary literals, Disjunctions like (a1 ∨ a2 ∨ ... b) are treated as sets, so the b and the ¬b may appear anywhere in their clauses. It is possible for a pair of clauses to have more than one pair of complementary literals, in which case the rule applies in two ways, yielding more than one conclusion.

Observe that the conclusion of the rule of resolution, as written in the box above, is implied by its hypotheses: if both of the original clauses are true and b is true, then one of the c's must be true in order to make the second clause true. Similarly, if b is false, then one of the a's must be true in order to make the first clause true. So if both clauses are true, then either one of the a's is true or one of the c's is true and this is what the conclusion says.

So, if you can derive the empty clause (which is always false) from some set of clauses F, then you've shown that your set of clauses F implies the empty clause, and therefore that it's unsatisfiable.

Further, there is a theorem that says that this is an "if and only if": a formula form is unsatisfiable if and only if the empty clause is derivable from the formula.

So to solve the satisfiability problem, we start with a set of clauses, and repeatedly apply the resolution rule until we either derive the empty clause or run out of new clauses to generate. (This always halts. Why? One of your deliverables is a halting measure for this algorithm.) If we derive the empty clause, we know our original set of clauses was unsatisfiable. If we derive all the clauses it is possible to derive, then we know (by the theorem above) that it is satisfiable.

For example, consider the following set of clauses:

(a ∨ ¬b ∨ c) 
(d ∨ b) 
(¬a ∨ c) 
b 
¬c 
From these clauses we can derive the empty clause, as follows:

(a \/ ~b \/ c)    (~a \/ c)
---------------------------
       (~b \/ c)                (b)
       ----------------------------
                    (c)                    (~c)
                    --------------------------
                               ()
In general, you will have to consider all generated clauses at every step, not just ones from the original set.

Most of the preceding discussion is the motivation for this problem. We've presented the problem in terms of for propositional (boolean) logic, but the technique extends to first-order logic. This is all standard material dating back to at least 1965; if you want to understand it better, go look it up on the internet. However, if you rely on anything you found on the internet, or elsewhere, you must cite it in a footnote, just as you would for any other term paper.

Now we can get to specifics:

Here are the specified pieces of the data definitions. You may not need all of these.

A Variable is a Racket Symbol.

A Literal is one of
-- (make-pos Variable)  Interp: a literal containing the variable
-- (make-neg Variable)  Interp: a literal containing the negation of
                                the variable

A Clause is a SetOfLiteral
(Note that make-pos is analogous to make-varexp in GarterSnake: it provides a way of injecting a variable into a boolean expression. make-neg injects the variable and puts a negation on it at the same time.)

You are to turn in a file named q2.rkt that provides the following function:

is-null-derivable? : ListOfClause -> Boolean
GIVEN: a list of clauses
RETURNS: true iff the empty clause is derivable from the given
clauses using the rule of resolution as given above.
In addition, you must provide the following functions that we need in order to test is-null-derivable? :

make-pos : Symbol -> Literal
make-neg : Symbol -> Literal
GIVEN: A Variable v
RETURNS: The literal v or ¬v

make-clause : ListOfLiteral -> Clause
GIVEN: a list of literals, possibly with duplications
RETURNS: a clause containing exactly those literals
You get to design the representation of literals and clauses. In particular, it is possible but not required that you have structs for both make-pos and make-neg. You need only provide the constructors above, but you will need to document your data definitions in the usual way.

------------------------------------------------------------------------------------------------------------------------------------
set 09
Objects, classes and interfaces

Problem:

Your boss at NextPython, Inc. spent the weekend at a wild party in the Valley, where he spent too much time taking drugs and listening to American folk music from the 1960's. He has come out of his hangover just long enough to pull you off your GarterSnake project and tell you to to build a marvelous toy, called the MetaToy. The MetaToy will consist of a canvas on which the child will paint and interact with a variety of toys.

The MetaToy consists of a canvas that is 500 pixels wide and 600 pixels high.
The child interacts with the MetaToy by using the mouse and by typing characters into the system. Each of the characters listed below causes a new mini-toy to be created with its center located at the center of the canvas. Most of the mini-toys are also moveable using smooth drag.
When the child types "t", a new mini-toy called a throbber appears. A throbber starts as a solid green circle of radius 5. At every tick, it expands gradually until it reaches a radius of 20. Once it reaches a radius of 20, it contracts gradually until it reaches a radius of 5, and then resumes its cycle. It can be selected and moved using smooth drag. When it is selected, it appears in outline mode.
When the child types "c", a clock mini-toy appears. This clock displays the number of ticks since it was created. Otherwise the appearance of the clock is unspecified. Like the throbber, the clock can be selected and moved using smooth drag.
When the child types "p", a Politician mini-toy appears. A politician has some interesting behaviors:
The politician appears as an image that is approximately a square 60 pixels on a side. (It doesn't have to be exactly square, or exactly 60 pixels. Choose something that is visually satisfying.)
The politician always moves in a straight line either towards the mouse position or away from it. However, he never reaches or passes the mouse position.
The politician always pays close attention to the polls. In this context, this means that the politician takes notice of every mouse event (button-down, button-up, drag, and move).
A politician will always follow you, at a distance. When the center of the politician is at least 75 pixels away from the mouse position, the politician moves towards the mouse.
However, when the politician gets within 75 pixels of the mouse, he gets frightened and moves away rapidly from the mouse. (So the child can repel the politician by following him around.)
The speed with which the politician moves either towards or away from the mouse is up to you. Choose something that is visually satisfying.
As is well-known, politicians are two-faced. When the politician jumps away from the mouse, he will come back with a different face. Go out on the internet and find pictures of Hillary and Trump to depict the politician. Or you can use pictures of your favorite politicians from your home country, so long as the TA will be able to tell them apart easily.
As usual, you are not responsible for anything that happens after the mouse leaves the canvas.
There are many unspecified parameters in the description above. Choose parameters (like speed, the exact way in which items grow and shrink, etc.) so that the result is visually satisfying.

I believe this problem is easier than the last one, so have some fun with it.

It is acceptable (and even encouraged) to reuse code from the Example files.

Here's a demo.


Your solution should be a file named q1.rkt and should provide the following interfaces and functions:

make-metatoy : ListOfToys -> Metatoy
RETURNS: a Metatoy with the given list of toys.
NOTE: The Metatoy<%> interface extends the World<%> interface, so the
result of make-metatoy is something that big-bang can use as a world.

run : PosNum -> Metatoy
GIVEN: a frame rate (in seconds/tick)
EFFECT: creates a MetaToy with no toys in it, and runs it using big-bang
at the given frame rate.  Returns the final state of the Metatoy.

make-throbber: PosInt PosInt -> Toy
GIVEN: an x and a y position
RETURNS: an object representing a throbber at the given position.

make-clock : PosInt PosInt -> Toy
GIVEN: an x and a y position
RETURNS: an object representing a clock at the given position.

make-politician : PosInt PosInt -> Toy
GIVEN: an x and a y position
RETURNS: an object representing a politician at the given position.

Interfaces:

;; A Metatoy is an object of any class that implements Metatoy<%>.
;; (You will only need one such class)

(define Metatoy<%>
  (interface 
  
   ;; the (World<%>) says that Metatoy<%> inherits from World<%>
   ;; This means that any class that implements Metatoy<%> must
   ;; implement all the methods from World<%> plus all the methods
   ;; defined here. In this case, there is just one additional method,
   ;; called get-toys.
   (World<%>)

    ;; -> ListOfToy
    get-toys

))

;; A Toy is an object of any class that implements Toy<%>
;; You will probably have three such classes, one for each kind of toy. 

(define Toy<%> 
  (interface
  
   ;; The interface Toy<%> inherits from the interface Widget<%>.
   ;; This means that any class that implements Toy<%> must implement
   ;; all the methods from Widget<%> plus all the methods defined here.
   (Widget<%>)


    ;; Note: the Widgets of the space-invader-examples don't respond
    ;; to mouse "move" events, but some of our toys do.  So we add an
    ;; after-move method to the interface.

    ;;  Int Int -> Toy
    ;;  RETURNS: the state of this toy that should follow a mouse-move
    ;;  at the given coordinates
    after-move

 
    ;; -> Int
    ;; RETURNS: the x or y position of the center of the toy
    toy-x
    toy-y

    ;; -> Int
    ;; RETURNS: some data related to the toy.  The interpretation of
    ;; this data depends on the class of the toy.
    ;; for a throbber, it is the current radius of the throbber
    ;; for the clock, it is the current value of the clock
    ;; for a politician, it is the current distance to the mouse
    toy-data


    ))
------------------------------------------------------------------------------------------------------------------------------------
set 10
Objects and states and communication
 
Your boss at the toy factory has been taking PDP, and he has been persuaded to buy a "framework" from WidgetWorks International. The salesman told him that the WidgetWorks framework eliminates the need for your developers to deal with big-bang. You, of course, know that's not so hard, but your boss, still hungover from his party, is pretty gullible. The salesman explains that with WidgetWorks, call you do is create a WidgetWorks Container(TM), load it up with your widgets and swidgets, and send it a 'run' message. You can even add additional widgets and swidgets while it's running. Your boss is convinced, and tells you to reimplement your Metatoy using the WidgetWorks framework. The framework is delivered as a file called WidgetWorks.rkt that provides three interfaces and one function, as follows:
;;; Function:

;; container-init : NonNegInt NonNegInt -> Container
;; GIVEN: the width and height of a canvas
;; RETURNS: a Container, initially empty, but which, when run, will
;; create a canvas of the given width and height and which will send events
;; to the Widgets and SWidgets that are added to it.

;;; Interfaces:

;; A Container is an object of any class that implements Container<%>

(define Container<%>
  (interface ()

   ; Widget -> Void
   ; GIVEN: A widget
   ; EFFECT: adds the given widget to this container
   add-widget

   ; SWidget -> Void
   ; GIVEN: A stateful widget
   ; EFFECT: adds the given swidget to this container
   add-stateful-widget

   ; PosReal -> Void
   ; GIVEN: a framerate, in secs/tick
   ; EFFECT: runs the widgets and stateful widgets in this framework
   ; at the given framerate.
   run

    ))

;; A Widget is an object of any class that implements Widget<%>

(define Widget<%>
  (interface ()

    ; -> Widget
    ; GIVEN: no arguments
    ; RETURNS: the state of this object that should follow at time t+1.
    after-tick          

    ; Integer Integer -> Widget
    ; GIVEN: a location
    ; RETURNS: the state of this object that should follow the
    ; specified mouse event at the given location.
    after-button-down
    after-button-up
    after-drag
    after-move

    ; KeyEvent -> Widget
    ; GIVEN: a key event and a time
    ; RETURNS: the state of this object that should follow the
    ; given key event
    after-key-event     

    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    add-to-scene
    ))

;; An SWidget is an object of any class that implements the SWidget<%>
;; interface.

;; A SWidget is like a Widget, but it is stable (stateful).

(define SWidget<%>
  (interface ()

    ; -> Void
    ; GIVEN: no arguments
    ; EFFECT: updates this widget to the state it should have
    ; following a tick.
    after-tick          

    ; Integer Integer -> Void
    ; GIVEN: a location
    ; EFFECT: updates this widget to the state it should have
    ; following the specified mouse event at the given location.
    after-button-down
    after-button-up
    after-drag
    after-move

    ; KeyEvent -> Void
    ; GIVEN: a key event
    ; EFFECT: updates this widget to the state it should have
    ; following the given key event
    after-key-event     

    ; Scene -> Scene
    ; GIVEN: a scene
    ; RETURNS: a scene like the given one, but with this object
    ; painted on it.
    add-to-scene
    ))
You are relieved to see that these interfaces are much like the ones you've been working with. Just as the salesman said, you run your widgets and swidgets by creating a Container, adding your widgets and swidgets to it, and then calling the run method on your container. You no longer need to call big-bang yourself.

Your job is to reimplement the toy from problem set 9, but using the WidgetWorks framework and stateful objects. The specifications and deliverables are exactly the same, except that:

Metatoy<%> inherits from SWidget<%> instead of World<%>.
Toy<%> inherits from SWidget<%> instead of Widget<%>. Also, after-move is removed from Toy<%> and added to SWidget<%> where it more properly belongs.
The specification for the run function that you provide is now
run : PosNum -> Void
GIVEN: a frame rate (in seconds/tick)
EFFECT: This function creates a Container, and places a MetaToy with
no toys in that Container.  The function may or may not put other
Widgets and SWidgets in the container, depending on the
implementation. The function then runs the Container at the given
frame rate using WidgetWorks.
Note that this change relaxes the previous contract, since now any value is acceptable as a return value from run.
Turn in your solution as a file named "q1.rkt". Put a copy of WidgetWorks.rkt in the directory with your solution. YOU MAY NOT MODIFY WidgetWorks.rkt IN ANY WAY. WE MAY TEST YOUR SOLUTION WITH OUR OWN IMPLEMENTATION OF Container<%>.

Hint: in the Week 10 examples, each SBall took the wall as one of its init-fields, and registered itself with the wall during its initialization. The contracts in ps09 rule out an analogous "self-registration" strategy here. So you will have to make the object that creates the toys subscribe each toy to the relevant notification or notifications. Be sure you can explain your registration strategy at codewalk. If this hint is mysterious when you read it, check back when you try to implement make-throbber, etc. ADDED MON 11/21/16: Now that I look at it, I think this hint is more misleading than helpful. So you can safely ignore it. See @1044. Sorry about that. -Prof. Wand

Your boss at the toy factory asks you to produce a new toy inspired by Cubelets, which are square blocks that stick together. The new toy has the following specification:
The toy consists of a canvas that is 600 pixels wide and 500 pixels high.
When the child types "b", a new block pops up on the screen at the location of the last button-down or button-up. The block appears as a 20x20 outline square. The square is initially green. If the child types a "b" before the first button-down or button-up event, then the first block appears in an unspecified but fixed place on the canvas.
A block does not move by itself, but the child can move it around using Smooth Drag. When the block is selected, it appears as red rather than green.
If a block is dragged so that it contacts or overlaps another block, the two blocks become connected. We say that the blocks are teammates.. The property of being a teammate is symmetric and transitive. So if block A is moved to touch block B, then a new team is formed consisting of A and all its teammates, and B and all its teammates.
Two blocks overlap if they intersect at any point. For this purpose, the edges of the block are considered part of the block.
Once two blocks become teammates, they remain teammates forever.
When a block is moved, all its teammates move along with it. If A and B are teammates, and A is dragged in some direction, then B moves the same way.
Only the selected block accumulates teammates. If A is being dragged, and B is a teammate of A, and A's motion causes B to come into contact with C, C does not become a teammate of A and B. In the video below, we call the selected block the "leader." But you can drag a team by selecting any block in the team, so the leader may be different on different drags.
Here's a demonstration:


Your solution should be a file named q2.rkt that provides the following two functions and one interface:

cubelets-init : -> Container
GIVEN: no arguments
RETURNS: a Container, initially with no blocks, which when run, will
run in a 600x500 canvas and process the events in the description above.

make-block : NonNegInt NonNegInt ListOfSBlock -> SBlock
GIVEN: an x and y position, and a list of blocks
WHERE: the list of blocks is the list of blocks already on the playground.
RETURNS: a new block, at the given position, with no teammates
NOTE: it is up to you as to whether you use the third argument or
not.  Some implementations may use the third argument; others may not.

You must provide an interface named SBlock<%>.  Your SBlock<%>
interface should inherit from the SWidget<%> interface and add AT
LEAST the following methods:

get-team : -> ListOfSBlock
RETURNS: the teammates of this sblock

add-teammate: SBlock -> Void
EFFECT: adds the given sblock to this block's team

sblock-x : -> Integer
sblock-y : -> Integer
RETURNS: the x or y coordinates of this sblock
You may put more methods in the SBlock<%> interface if you so desire. Remember that a method must appear in the interface if and only if it is called from outside this object.

There are several places where information must be disseminated in this problem, either by pushing or pulling. Be prepared to identify these and to discuss your design decisions about each of them.

As in the problem above, you must use WidgetWorks.rkt .

------------------------------------------------------------------------------------------------------------------------------------
set 11
Inheritance

Your task is to simulate a dimensionless particle bouncing in a 150x100 rectangle. For this system, you will produce 5 viewer-controllers:

A position controller, similar to the one in the Examples, but using the arrow keys to move the particle in the x or y direction.
A velocity controller, similar to the one in the Examples, but using the arrow keys to alter the velocity of the particle in the x or y direction.
Both the position and velocity controllers display both the position and velocity of the particle, as in the demo.
An XY controller, which shows a representation of the particle bouncing in the rectangle. With this controller, the user can drag the particle using the mouse. Dragging the mouse causes the particle to follow the mouse pointer via a Smooth Drag.
An X controller, which is like the XY controller, except that it displays only the x coordinate of the particle's motion. Dragging the mouse in the X controller alters the particle's position in the x direction.
A Y controller, which is like the X controller except that it works in the y direction.
Here's a demonstration:


Note that the first time I hit button-down inside the canvas of the XY controller, I accidently did so _at_ the particle location. That was a mousing error. The particle obeys smooth drag, not snap-to-mouse-location. You can also drag the mouse in the X and Y controllers, not demonstrated here.

Here are some more detailed specifications:

The entire system works on a 600 x 500 canvas.
Hitting one of the following keys causes a new controller to appear in the center of the canvas:
"p" : Position controller
"v" : velocity controller
"x" : X controller
"y" : Y controller
"z" : XY controller
Each controller has a 10x10 handle. Dragging on the handle moves the controller around the canvas.
A button-down inside a controller selects the controller for input.
In the position or velocity controller, the arrow keys are used for input. The arrow keys alter the position or velocity of the particle in the indicated direction. Each press of an arrow key alters the appropriate quantity by 5.
In the X, Y, or XY controller, the mouse drags the particle via smooth drag. The mouse need not be in the representation of the particle; it need only be in the controller. However, in such a drag, the particle must remain strictly inside the wall. If the particle gets outside the wall, then PerfectBounce.rkt will complain about a contract violation.
You must use the WidgetWorks.rkt framework, as you did for Problem Set 10.
I don't want you spending time on the geometry of the Perfect Bounce. I've provided a file called PerfectBounce.rkt that calculates this for you.
You must use inheritance to factor out the common parts of the various controllers.
Deliver your solution as a set of files, including WidgetWorks.rkt and PerfectBounce.rkt, and a file top.rkt that provides a function
run : PosReal -> Void
GIVEN: a frame rate, in sec/tick
EFFECT: Creates and runs the MVC simulation with the given frame rate.

