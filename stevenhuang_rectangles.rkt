;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname stevenhuang_rectangles) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #t)))
(require "rectanglelib.rkt")

(define-struct cell (num used?))
;; A Cell is a (make-cell Nat Bool)

;; A Grid is a (listof (listof Cell))
;; requires: the grid contains a non-empty list of non-empty lists,
;;  all the same length.

(define-struct rect (x y w h))
;; A Rect is a (make-rect Nat Nat Nat Nat)

(define-struct state (grid rects))
;; A State is a (make-state Grid (listof Rect))

(define puzz '((0 0 0 0 0 5 0)
               (0 0 0 0 0 2 2)
               (0 3 0 6 3 2 0)
               (4 0 0 0 0 0 0)
               (0 0 0 4 0 4 0)
               (2 0 6 0 2 4 0)
               (0 0 0 0 0 0 0)))

(define big-puzz '((4 0 7 0 0 0 0 0 0 0 0 21 0)
                   (0 3 2 0 0 0 0 0 0 0 0 0 2)
                   (0 0 0 0 0 0 0 2 3 0 0 0 0)
                   (0 0 0 20 0 0 0 0 0 0 0 0 5)
                   (0 2 0 0 0 0 0 4 0 0 0 0 0)
                   (0 0 3 0 0 0 0 0 0 0 0 0 0)
                   (3 0 0 0 0 5 2 4 0 0 0 0 0)
                   (0 0 0 0 0 2 0 6 0 0 0 0 0)
                   (0 0 0 20 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 0 0)
                   (0 0 0 0 0 0 0 0 0 0 0 24 0)
                   (0 0 0 0 4 0 4 0 0 0 4 0 0)
                   (0 0 3 0 0 0 0 0 0 0 8 0 2)))



;;(map2d f lst) produces a new list of lists in which
;;the function f has bene applied to every input in lst
;;map2d: (X -> Y (listof (listof X)) -> (listof (listof Y))
;;Examples:
(check-expect (map2d add1 '((3 4 5) (10 9 8)))
              (list (list 4 5 6) (list 11 10 9)))
(check-expect (map2d (lambda (x) (* 2 x)) '((1 4 5) (2 8 0)))
              '((2 8 10) (4 16 0)))
              
(define (map2d f lst)
 (map (lambda (x) (map f x)) lst))

;;Tests:
(check-expect (map2d (lambda (x) (* 2 x)) '((1 4 5) (2 8 0)))
              '((2 8 10) (4 16 0)))

;;(construct-puzzle listoflistofnat) consumes a list of
;;lists of nats and produces a state representing the initial
;;state of the puzzle
;;construct-puzzle: (listof (listof Nat)) -> State
;;Examples:
(check-expect (construct-puzzle '((0 0) (0 0)))
              (make-state
               (list (list (make-cell 0 false) (make-cell 0 false))
                     (list (make-cell 0 false) (make-cell 0 false))) empty))

(define (construct-puzzle listoflistofnat)
  (make-state (map2d (lambda (x) (make-cell x false)) listoflistofnat)  empty))


;;(solved? mystate) determines whether the puzzle described
;;by mystate is solved
;;solved? State -> Bool
;;Examples:
(check-expect (solved? (make-state
               (list (list (make-cell 0 false) (make-cell 0 false))
                     (list (make-cell 0 false) (make-cell 0 false))) empty))
              false)

(check-expect (solved? (make-state
               (list (list (make-cell 0 true) (make-cell 0 true))
                     (list (make-cell 0 true) (make-cell 0 true))) empty))
              true)
 
(define (solved? mystate)
  (empty? (flatten (flatten (map2d (lambda (x) (filter false?
                                                       (list (cell-used? x))))
                 (state-grid mystate))))))

(define (flatten lst)
  (foldr append empty lst))

;;Tests:
(check-expect (solved? (make-state
               (list (list (make-cell 0 false) (make-cell 0 true))
                     (list (make-cell 0 true) (make-cell 0 true))) empty))
              false)


;;(get-first-ununused mygrid) finds the topmost, leftmost
;;cell in mygrid that isn’t being used
;;get-first-unused: Grid -> (list Nat Nat)
;;Examples:
(check-expect (get-first-unused
               (list (list (make-cell 0 false) (make-cell 0 true))
                     (list (make-cell 0 true) (make-cell 0 true))))
              (list 0 0))
(check-expect (get-first-unused
               (list (list (make-cell 0 true) (make-cell 0 true))
                     (list (make-cell 0 true) (make-cell 0 false))))
              (list 1 1))
               
(define (get-first-unused mygrid)
 (local
   ;;check if a row has an unused cell
   [(define (has-unused? row)
           (not (empty? (filter (lambda (x) (false? (cell-used? x))) row))))]
   (local
     ;;gets the column number of the first cell in a row that is unused
     [(define (get-column-number row)
        (cond [(false? (cell-used? (first row))) 0]
              [else (+ 1 (get-column-number (rest row)))]
              ))]
     ;;accumulates the rownumber we are working on
     (local [(define (get-first/rowacc mygrid rownumber)
        (cond [(has-unused? (first mygrid))
               (cons (get-column-number (first mygrid)) (list rownumber))]
              [else (get-first/rowacc (rest mygrid) (+ 1 rownumber))]))]
    
         (get-first/rowacc mygrid 0)))))

(check-expect (get-first-unused
               (list (list (make-cell 0 true) (make-cell 0 true) (make-cell 0 true))
                     (list (make-cell 0 true) (make-cell 0 true) (make-cell 0 false))))
              (list 2 1))
        


(define example-state (make-state
                    (list (list (make-cell 0 true) (make-cell 0 true) (make-cell 0 true) (make-cell 2 true) (make-cell 0 true) (make-cell 2 true) (make-cell 0 true))
                          (list (make-cell 0 true) (make-cell 0 true) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 4 false) (make-cell 0 true))
                          (list (make-cell 0 true) (make-cell 3 true) (make-cell 6 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true))
                          (list (make-cell 4 true) (make-cell 2 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 4 false) (make-cell 0 true))
                          (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 2 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true))
                          (list (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 10 false) (make-cell 0 false) (make-cell 7 true))
                          (list (make-cell 3 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 false) (make-cell 0 true)))
                    
                     (list (make-rect  0 0 1 4)
                           (make-rect  1 0 1 3)
                           (make-rect  2 0 2 1)
                           (make-rect  4 0 2 1)
                           (make-rect  6 0 1 7))))                 

;;defining max width of the grid
;;(define maxwidth (length (first (state-grid mystate))))

;;defining max height of the grid
;;(define maxheight (length (state-grid mystate)))

                         
;;finds all the possible dimensions given a maxwidth and maxheight
;;possibledimensions: Nat Nat -> (listof (list Nat Nat))
(define (possibledimensions maxwidth maxheight)
 ;;given a width gives all the lists with that width and the heights 
 (local [(define (possibledimensions/heightacc width maxheight lstsofar)
           (cond [(= 0 maxheight) lstsofar]
                 [else (possibledimensions/heightacc width (- maxheight 1)
                                                   (cons (list width maxheight)
                                                         lstsofar))]))]
   
 ;;produces all the possible dimensions for all the widths
 (local [(define (possibledimensions/widthacc maxwidth maxheight lstsofar)
           (cond [(= 0 maxwidth) lstsofar]
                 [else (possibledimensions/widthacc (- maxwidth 1) maxheight
                 (cons (possibledimensions/heightacc maxwidth maxheight empty)
                    lstsofar))]))]
   
   (flatten (possibledimensions/widthacc maxwidth maxheight empty))

   )))
   
;;(possibledimensions 6 5)
                           
;;produces a list of all the possible rectangles given a
;;cells position
;;find-rectangles: Nat Nat Nat Nat -> (list Nat Nat Nat Nat)
(define (find-rectangles x y maxwidth maxheight)
 (map (lambda (k) (append (list x y) k))
      (possibledimensions maxwidth maxheight)))

;;(find-rectangles 2 1 6 5)

;;given a rectangle, finds all the cell coordinates in the rectangle
;;coordinates: (list Nat Nat Nat Nat) -> (listof (list Nat Nat))
(define (coordinates myrectangle)
  (local [(define width (third myrectangle))
         (define height (fourth myrectangle))

          ;;given an y coordinate finds all the possible x coordinates 
          (define (possiblexcoordinates/acc x y lstsofar) 
          (cond [(= (+ width x) 0) lstsofar]
                [else (possiblexcoordinates/acc (- x 1) y
                                (cons (list (- (+ x width) 1) y) lstsofar))]))

         ;;finds all the possible coordinates for all the possible y coordinates
          (define (possibleycoordinates/acc x y lstsofar)
          (cond [(= (+ height y) 0) lstsofar]
                [else (possibleycoordinates/acc x (- y 1)
                 (cons (possiblexcoordinates/acc x ( - (+ y height) 1) empty)
                              lstsofar))]))
        ]
    
(flatten (possibleycoordinates/acc (first myrectangle)
                                   (second myrectangle) empty)))) 

;;(coordinates (list 2 1 3 3))

;;consumes a coordinate and a state, and produces the cells corresponding
;;to that coordinate in the state
;;get-cell: (list Nat Nat) State -> Cell
(define (get-cell coordinates mystate)
  (get-cell2 coordinates (state-grid mystate)))

(define (get-cell2 coordinates mygrid)
  (cond
        [(= (second coordinates) 0) (getxcell
                                      (first coordinates) (first mygrid))]
        [else (get-cell2 (list (first coordinates) (- (second coordinates) 1))
                         (rest mygrid))]))
                               
(define (getxcell x listofcells)
  (cond
        [(= x 0) (first listofcells)]
        [else (getxcell (- x 1) (rest listofcells))]))


;;(get-cell (list 1 1) (make-state
;;               (list (list (make-cell 1 false) (make-cell 2 false))
;;                     (list (make-cell 3 false) (make-cell 4 false))) empty))

;;consumes a rectangle and a state and produces every cell in the rectangle
;;geteverycell (list Nat Nat Nat Nat) State -> (listof Cell)
(define (geteverycell myrectangle mystate)
  (map (lambda (x) (get-cell x mystate)) (coordinates myrectangle)))

;;(geteverycell (list 0 0 2 1)
;;              (make-state 
;;              (list (list (make-cell 1 false) (make-cell 2 false)
;;                          (make-cell 3 false) (make-cell 4 false))
;;                    (list (make-cell 5 false) (make-cell 6 false)
;;                         (make-cell 7 false) (make-cell 8 false))) empty))

;;checks if all the cells in the rectangles are currently unused
;;unused? (list Nat Nat Nat Nat) State -> Bool
(define (unused? myrectangle mystate)
  (empty? (filter (lambda (x) (cell-used? x))
                          (geteverycell myrectangle mystate))))

;;(unused? (list 0 0 2 1)
;;              (make-state 
;;              (list (list (make-cell 2 false) (make-cell 0 false)
;;                          (make-cell 0 false) (make-cell 0 false))
;;                    (list (make-cell 0 false) (make-cell 0 false)
;;                          (make-cell 0 false) (make-cell 0 false))) empty))

;;only1num? determines if the rectangle has only 1
;;non-zero number
;;(list Nat Nat Nat Nat) -> Bool
(define (only1num? myrectangle mystate)
  (equal? (length (filter (lambda (x) (positive? (cell-num x)))
                          (geteverycell myrectangle mystate)))  1))

;;(only1num? (list 0 0 2 1)
;;              (make-state 
;;              (list (list (make-cell 1 true) (make-cell 0 true)
;;                          (make-cell 0 true) (make-cell 0 true))
;;                    (list (make-cell 0 true) (make-cell 0 false)
;;                          (make-cell 0 true) (make-cell 0 false))) empty))

;;area=num? determines if the area of a rectangle is equal
;;to the number inside
(define (area=num? myrectangle mystate)
  (equal? (* (third myrectangle) (fourth myrectangle))
           (cell-num (first (filter (lambda (x) (positive? (cell-num x)))
                          (geteverycell myrectangle mystate))))))

;;(area=num? (list 0 0 2 1)
;;           (make-state 
;;              (list (list (make-cell 2 true) (make-cell 0 true)
;;                          (make-cell 0 true) (make-cell 0 true))
;;                    (list (make-cell 0 true) (make-cell 0 false)
;;                          (make-cell 0 true) (make-cell 0 false))) empty))

;;determines if a rectangle is legal
;;isitlegal?: (list Nat Nat Nat Nat) -> Bool
(define (isitlegal? myrectangle mystate)
  (and (unused? myrectangle mystate)
       (only1num? myrectangle mystate)
       (area=num? myrectangle mystate)))

;;(isitlegal? (list 0 0 2 1)
;;              (make-state 
;;              (list (list (make-cell 2 false) (make-cell 0 false)
;;                          (make-cell 0 false) (make-cell 0 false))
;;                    (list (make-cell 0 false) (make-cell 0 false)
;;                          (make-cell 0 false) (make-cell 0 false))) empty))


;;given the first unused cell in a grid finds all the valid
;;rectangles with that cell in its top left corner
;;findvalidrectangles: (list Nat Nat) -> (listof (list Nat Nat Nat Nat))
(define (findvalidrectangles cell mystate)
 
  (filter (lambda (x) (isitlegal? x mystate))
       (find-rectangles (first cell) (second cell)
                     (length (first (state-grid mystate)))
                     (length (state-grid mystate)))))

;;(findvalidrectangles (list 0 0)
;;              (make-state 
;;              (list (list (make-cell 2 false) (make-cell 0 false)
;;                          (make-cell 0 false) (make-cell 0 false))
;;                    (list (make-cell 0 false) (make-cell 0 false)
;;                          (make-cell 0 false) (make-cell 0 false))) empty))

;;takes a valid rectangle and a grid and changes all the cells used?
;;in the rectangle in the grid in to true
(define (cell-changer myrect mygrid)
 (local [(define (changeused x width listofcells)
           (cond [(and (= x 0) (= width 1))
                  (cons (make-cell (cell-num (first listofcells)) true)
                        (rest listofcells))]
                 [(= x 0) (cons (make-cell (cell-num (first listofcells)) true)
                                (changeused 0 (sub1 width) (rest listofcells)))]
                 [else (cons (first listofcells) (changeused (sub1 x) width (rest listofcells)))]))]

                 
   (cond [(= 0 (fourth myrect)) mygrid]
         [(= 0 (second myrect))
          (cons (changeused (first myrect) (third myrect) (first mygrid))
                (cell-changer (list (first myrect) 0 (third myrect)
                                    (sub1 (fourth myrect))) (rest mygrid)))]
         [else (cons (first mygrid)
                     (cell-changer
                      (list (first myrect)
                            (sub1 (second myrect))
                            (third myrect) (fourth myrect))
                                   (rest mygrid)))])))

;;(cell-changer (list 0 0 2 1)
;;              (list (list (make-cell 2 false) (make-cell 0 false)
;;                          (make-cell 0 false) (make-cell 0 false))
;;                    (list (make-cell 0 false) (make-cell 0 false)
;;                          (make-cell 0 false) (make-cell 0 false))))

;;takes a valid rectangle and a state, and changes the state to
;;account for the valid rectangle
(define (state-changer myrect mystate)
  (make-state (cell-changer myrect (state-grid mystate))
              (cons (make-rect (first myrect)
                               (second myrect)
                               (third myrect)
                               (fourth myrect)) (state-rects mystate))))

;;(state-changer (list 0 0 2 1)
;;               (make-state
;;               (list (list (make-cell 2 false) (make-cell 0 false)
;;                          (make-cell 0 false) (make-cell 0 false))
;;                    (list (make-cell 0 false) (make-cell 0 false)
;;                          (make-cell 0 false) (make-cell 0 false))) empty))


;;(neighbours mystate) consumes a state and produces a list of new
;;states that might legimiately follow from the given state after
;;adding a single new rectangle
;;neighbours: State -> (listof States)

(define (neighbours mystate)
 (cond [(empty? (findvalidrectangles (get-first-unused (state-grid mystate)) mystate)) false]
       [else (map (lambda (x) (state-changer x mystate)) (findvalidrectangles (get-first-unused (state-grid mystate)) mystate))]
       ))
       
;;(neighbours (make-state 
;;              (list (list (make-cell 2 false) (make-cell 0 false)
;;                          (make-cell 0 false) (make-cell 0 false))
;;                    (list (make-cell 0 false) (make-cell 0 false)
;;                          (make-cell 0 false) (make-cell 0 false))) empty))

                 
                                

;;solve-rectangle-puzzle produces a list of rectangles that describe
;;a solution if one exists or false if no solution can be found
;;(listof (listof Nat)) -> (listof Rect)
(define (solve-rectangle-puzzle lolon)
  (cond [(false? (search solved? neighbours (construct-puzzle lolon))) false]
        [else (state-rects (search solved? neighbours (construct-puzzle lolon)))]
        ))

              





        
  
                      



           
