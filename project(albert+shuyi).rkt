#lang racket
(require gigls/unsafe)

;http://stackoverflow.com/questions/13562200/find-the-index-of-element-in-list
;;; Procedure:
;;; get-list-index
;;; Parameters:
;;; l, a list
;;; el, a list element
;;; Purpose:
;;; Get the index of the element el in the list l
;;; Produces:
;;;   index, an integer
;;; Preconditions:
;;;   [No additional]
;;; Postconditions:
;;;   If el is in the list l, index is the index of the first occurence of el in l
;;;   Otherwise, index is -1
(define (get-list-index l el)
  (if (null? l)
      -1
      (if (= (car l) el)
          0
          (let ((result (get-list-index (cdr l) el)))
            (if (= result -1)
                -1
                (+ 1 result))))))

(random-seed 0)

(define size 32)

;;; Procedure:
;;;   pos->ref
;;; Parameters:
;;;   pos, a pair
;;; Purpose:
;;;   Convert a position into a reference
;;; Produces:
;;;   ref, an integer
;;; Preconditions:
;;;   pos is a pair of two integers, both in the range [0, size)
;;; Postconditions:
;;;   pos = (x . y)
;;;   ref = size * x + y
(define pos->ref
  (lambda (pos)
    (+ (* size (car pos)) (cdr pos))))

;;; Procedure:
;;;   ref->pos
;;; Parameters:
;;;   ref, an integer
;;; Purpose:
;;;   Convert a reference into a position
;;; Produces:
;;;   pos, a pair
;;; Preconditions:
;;;   ref is in the range [0, size * size)
;;; Postconditions:
;;;   ref / size = x + y / size (mixed number)
;;;   pos = (x . y)
(define ref->pos
  (lambda (ref)
    (cons (quotient ref size) (remainder ref size))))

(define visited (make-vector (* size size) #f))

;;; Procedure:
;;;   visited?
;;; Parameters:
;;;   pos, a pair
;;; Purpose:
;;;   Find whether a position has been visited by the maze generation algorithm
;;; Produces:
;;;   visited, a boolean
;;; Preconditions:
;;;   pos is a pair of two integers, both in the range [0, size)
;;; Postconditions:
;;;   return whether pos has been visited
(define visited?
  (lambda (pos)
    (vector-ref visited (pos->ref pos))))

;;; Procedure:
;;;   visit!
;;; Parameters:
;;;   pos, a pair
;;; Purpose:
;;;   Visit a position during maze generation
;;; Produces:
;;;   void
;;; Preconditions:
;;;   pos is a pair of two integers, both in the range [0, size)
;;; Postconditions:
;;;   (visited? pos) will return true
(define visit!
  (lambda (pos)
    (vector-set! visited (pos->ref pos) #t)))

;;; Procedure:
;;;   in-maze?
;;; Parameters:
;;;   pos, a pair
;;; Purpose:
;;;   Find whether a position is in the maze bounds
;;; Produces:
;;;   in-maze, a boolean
;;; Preconditions:
;;;   pos is a pair of two integers, both in the range [0, size)
;;; Postconditions:
;;;   if pos is in the maze bounds, return true
;;;   otherwise, return false
(define in-maze?
  (lambda (pos)
    (and
     (>= (car pos) 0)
     (>= (cdr pos) 0)
     (< (car pos) size)
     (< (cdr pos) size))))

; cite filter from racket docs?

;;; Procedure:
;;;   neighbors
;;; Parameters:
;;;   pos, a pair
;;; Purpose:
;;;   Find the neighboring positions of a position
;;; Produces:
;;;   poss, a list
;;; Preconditions:
;;;   pos is a pair of two integers, both in the range [0, size)
;;; Postconditions:
;;;   poss is a list of all positions that are in the maze and a distance of 1 away from pos
(define neighbors
  (lambda (pos)
    (let ([north (cons (- (car pos) 1) (cdr pos))]
          [south (cons (+ (car pos) 1) (cdr pos))]
          [east (cons (car pos) (- (cdr pos) 1))]
          [west (cons (car pos) (+ (cdr pos) 1))])
      (filter in-maze? (list north south east west)))))

;;; Procedure:
;;;   build-maze!
;;; Parameters:
;;;   pos
;;; Purpose:
;;;   Generate a maze by building a tree of nodes
;;; Produces:
;;;   maze-node, a pair
;;; Preconditions:
;;;   pos is a pair of two integers, both in the range [0, size)
;;; Postconditions:
;;;   maze-node is a pair in which the first element is the position
;;;   and the second element is a list of children from that position.
;;;   maze-node makes a tree.

(define build-maze!
  (lambda (pos)
    (when (not (visited? pos))
      (visit! pos)
      (cons pos (map build-maze! (shuffle (neighbors pos)))))))

; unit square with top left corner at origin
(define tile (hshift-drawing 0.5 (vshift-drawing 0.5 drawing-unit-square)))
(define vpath (vscale-drawing 1.9 (hscale-drawing 0.9 tile)))
(define hpath (hscale-drawing 1.9 (vscale-drawing 0.9 tile)))

;;; Procedure:
;;;   drawing-path
;;; Parameters:
;;;   prev, a pair
;;;   next, a pair
;;; Purpose:
;;;   To draw a rectangle between two positions
;;; Produces:
;;;   result, a drawing
;;; Preconditions:
;;;   prev and next are pairs of two integers, both in the range [0, size)
;;;   prev and next represent adjacent positions
;;; Postconditions:
;;;   Result will be a rectangle with its left-top point at 
;;;   the left-most and top-most position between the positions represented
;;;   by prev and next.
(define drawing-path
  (lambda (prev next)
    ;(display (list prev next))
    ;(newline)
    (if (= (car prev) (car next))
        ; vertical
        (hshift-drawing
         (car prev)
         (vshift-drawing 
          (min (cdr prev) (cdr next))
          vpath))
        ; horizontal
        (vshift-drawing
         (cdr prev)
         (hshift-drawing 
          (min (car prev) (car next))
          hpath)))))

;;; Procedure:
;;;   drawing-maze
;;; Parameters:
;;;   maze, a pair
;;; Purpose:
;;;   result, a drawing
;;; Produces:
;;;   maze-drawing, a drawing
;;; Preconditions:
;;;   maze is a valid node in the maze tree
;;; Postconditions:
;;;   maze-drawing will contain a series of rectangles on each path
;;;   the player can go to.
(define drawing-maze
  (lambda (maze)
    (let ([children (filter (o not void?) (cdr maze))])
      (drawing-compose (append
                        (list drawing-blank)
                        (map (o (l-s drawing-path (car maze)) car) children)
                        (map drawing-maze children))))))

(define maze (build-maze! (cons 0 0)))

(define img-nums (cons 0 (shuffle (cdr (iota (* size size))))))

(define drawing-player (recolor-drawing (irgb 254 254 254) (scale-drawing 0.8 (hshift-drawing 0.5 (vshift-drawing 0.5 drawing-unit-circle)))))

;;; Procedure:
;;;   draw-player! 
;;; Parameters:
;;;   image, an image
;;;   hscale, an integer
;;;   vscale, an integer
;;;   img-num, an integer
;;; Purpose:
;;;   To draw a circle on a certain position in the image
;;; Produces:
;;;   Result, an image
;;; Preconditions:
;;;   img-num should be an integer between 0 and 1023
;;; Postconditions:
;;;   Result will be a white circle on image whose position on image depends
;;;   randomly on img-num
(define draw-player!
  (lambda (image hscale vscale img-num)
    (drawing-render! (hscale-drawing hscale (vscale-drawing vscale (hshift-drawing (+ 1 (car (ref->pos img-num))) (vshift-drawing (+ 1 (cdr (ref->pos img-num))) drawing-player)))) image)))

;;; Procedure:
;;;   distance-from-start
;;; Parameters:
;;;   img-num, an integer
;;; Purpose:
;;;   To calculate the distance between a point and the left-top point
;;;   on the drawing
;;; Produces:
;;;   distance, a non-negative number
;;; Preconditions:
;;;   img-num should be an integer between 0 and 1023
;;; Postconditions:
;;;   distance describes how far a point, represented by img-num, is from
;;;   the start of the maze. The farther it is from the start, the bigger distance is.
;;;   Distance should be greater than 0 smaller than 32*(sqrt 2)
(define distance-from-start
  (lambda (img-num)
    (sqrt (+ (expt (car (ref->pos img-num)) 2) (expt (cdr (ref->pos img-num)) 2)))))

;;; Procedure:
;;;   find-bg-color
;;; Parameters:
;;;   distance, a number
;;; Purpose:
;;;   To decide on the color of the background of the image
;;;   when the player is at a certain point
;;; Produces:
;;;   bgcolor, an hsv color
;;; Preconditions:
;;;   Distance is a non-negative number between 0 and 32*(sqrt 2)
;;; Postconditions:
;;;   bgcolor covers a range of color on 1/3 of a color wheel from red to yellow to green.
(define find-bg-color
  (lambda (distance)
    (hsv->rgb (hsv (* 120 (/ distance size (sqrt 2))) 1 0.8))))

;;; Procedure:
;;;   image-series
;;; Parameters:
;;;   n, an integer
;;;   width, an integer
;;;   height, an integer
;;; Purpose:
;;;   To generate an image of a maze with player in the proper position corresponding
;;;   to n, with the proper combination of numbers on sides of the image and the proper
;;;   background color.
;;; Produces:
;;;   image, an image
;;; Preconditions:
;;;   n >= 0
;;;   width > 0
;;;   height > 0
;;; Postconditions:
;;;   image is an image with a maze, a circle and numbers on sides. More information can be found in
;;;   design statement of the final statement.
(define image-series
  (lambda (n width height)
    (let* ([hscale (max 1.25 (floor (/ width (+ size 2))))]
           [vscale (max 1.25 (floor (/ height (+ size 2))))]
           [image (drawing->image (hscale-drawing hscale (vscale-drawing vscale (vshift-drawing 1 (hshift-drawing 1 (drawing-maze maze))))) width height)]
           [unscrambled (get-list-index img-nums n)]
           [bgcolor (find-bg-color (distance-from-start unscrambled))]
           [draw-gradient
            (lambda (color)
              (if (= color (irgb 255 255 255))
                  bgcolor
                  color))])
      (cond [(= unscrambled (- (* size size) 1))
             (draw-smiling-face width height)]
            [(draw-player! image hscale vscale unscrambled)
             (context-set-brush! "2. Hardness 100" 0.17)
             (context-set-fgcolor! "black")
             (draw-nums image (car (ref->pos unscrambled)) (cdr (ref->pos unscrambled)) hscale vscale)
             (image-variant image draw-gradient)]))))

;;; Procedure:
;;;   draw-smiling-face
;;; Parameters:
;;;   width, a positive number
;;;   height, a positive number
;;; Purpose:
;;;   To draw a smiling face
;;; Produces:
;;;   image, an image
;;; Preconditions:
;;;   No additional
;;; Postconditions:
;;;   image will be an image of width width and height height. There will be
;;;   a yellow smiley face with two blue eyes and a red smiley mouth on image.
(define draw-smiling-face
  (lambda (width height)
    (define canvas (image-new width height))
    (image-select-ellipse! canvas REPLACE (/ width 8) (/ height 8) (* width 0.75) (* height 0.75))
    (context-set-fgcolor! "yellow")
    (image-fill-selection! canvas)
    (image-select-ellipse! canvas REPLACE (* width 0.375) (/ height 2) (/ width 4) (/ height 4))
    (image-select-ellipse! canvas SUBTRACT (/ width 10) (* height 0.175) (* 0.8 width) (* 0.4 height))
    (context-set-fgcolor! "red")
    (image-fill-selection! canvas)
    (image-select-ellipse! canvas REPLACE (* 0.325 width) (* 0.35 height) (/ width 10) (/ height 10))
    (context-set-fgcolor! "blue")
    (image-fill-selection! canvas)
    (image-select-ellipse! canvas REPLACE (* 0.575 width) (* 0.35 height) (/ width 10) (/ height 10))
    (context-set-fgcolor! "blue")
    (image-fill-selection! canvas)))

;;; Procedure:
;;;   number->image
;;; Parameters:
;;;   num, a non-negative integer
;;;   x, a number
;;;   y, a number
;;;   image, an image
;;;   hsize, a number
;;;   vsize, a number
;;;   space, a number
;;;   horizontal, a boolean value
;;; Purpose:
;;;   To draw numbers on an image
;;; Produces:
;;;   new-image, an image
;;; Preconditions:
;;;   x, y, hsize, vsize, space should be within the width and height of image
;;; Postconditions:
;;;   The procedure will produce an image with numbers drawn on it representing
;;;   the num that we input. The drawn numbers will be vsize in height and hsize
;;;   in width, with a distance of space between each other. The numbers' first digit will
;;;   start with its left-top point on (x, y). 
(define number->image
  (lambda (num x y image hsize vsize space horizontal)
    (cond [(= num 1)
           (image-draw-line! image (+ x 4) y (+ 4 x) (+ y vsize vsize))]
          [(= num 2)
           (image-draw-line! image x y (+ x hsize) y)
           (image-draw-line! image (+ x hsize) y (+ x hsize) (+ y vsize))
           (image-draw-line! image (+ x hsize) (+ y vsize) x (+ y vsize))
           (image-draw-line! image x (+ y vsize) x (+ y vsize vsize))
           (image-draw-line! image x (+ y vsize vsize) (+ x hsize) (+ y vsize vsize))]
          [(= num 3)
           (image-draw-line! image x y (+ x hsize) y)
           (image-draw-line! image (+ x hsize) (+ y vsize) x (+ y vsize))
           (image-draw-line! image (+ x hsize) y (+ x hsize) (+ y vsize vsize))
           (image-draw-line! image x (+ y vsize vsize) (+ x hsize) (+ y vsize vsize))]
          [(= num 4)
           (image-draw-line! image x y x (+ y vsize))
           (image-draw-line! image x (+ y vsize) (+ x hsize) (+ y vsize))
           (image-draw-line! image (+ x hsize) y (+ x hsize) (+ y vsize vsize))]
          [(= num 5)
           (image-draw-line! image x y (+ x hsize) y)
           (image-draw-line! image x y x (+ y vsize))
           (image-draw-line! image x (+ y vsize) (+ x hsize) (+ y vsize))
           (image-draw-line! image (+ x hsize) (+ y vsize) (+ x hsize) (+ y vsize vsize))
           (image-draw-line! image x (+ y vsize vsize) (+ x hsize) (+ y vsize vsize))]
          [(= num 6)
           (image-draw-line! image x y (+ x hsize) y)
           (image-draw-line! image x y x (+ y vsize vsize))
           (image-draw-line! image x (+ y vsize) (+ x hsize) (+ y vsize))
           (image-draw-line! image (+ x hsize) (+ y vsize) (+ x hsize) (+ y vsize vsize))
           (image-draw-line! image x (+ y vsize vsize) (+ x hsize) (+ y vsize vsize))]
          [(= num 7)
           (image-draw-line! image x y (+ x hsize) y)
           (image-draw-line! image (+ x hsize) y (+ x hsize) (+ y vsize vsize))]
          [(= num 8)
           (image-draw-line! image x y x (+ y vsize vsize))
           (image-draw-line! image x y (+ x hsize) y)
           (image-draw-line! image (+ x hsize) y (+ x hsize) (+ y vsize vsize))
           (image-draw-line! image x (+ y vsize) (+ x hsize) (+ y vsize))
           (image-draw-line! image x (+ y vsize vsize) (+ x hsize) (+ y vsize vsize))]
          [(= num 9)
           (image-draw-line! image x y x (+ y vsize))
           (image-draw-line! image x y (+ x hsize) y)
           (image-draw-line! image (+ x hsize) y (+ x hsize) (+ y vsize vsize))
           (image-draw-line! image x (+ y vsize) (+ x hsize) (+ y vsize))
           (image-draw-line! image x (+ y vsize vsize) (+ x hsize) (+ y vsize vsize))]
          [(= num 0)
           (image-draw-line! image x y x (+ y vsize vsize))
           (image-draw-line! image x y (+ x hsize) y)
           (image-draw-line! image x (+ y vsize vsize) (+ x hsize) (+ y vsize vsize))
           (image-draw-line! image (+ x hsize) y (+ x hsize) (+ y vsize vsize))
           (image-draw-line! image x y (+ x hsize) (+ y vsize vsize))]
          [else
           (number->image (remainder num 10) x y image hsize vsize space horizontal)
           (if horizontal 
               (number->image (quotient num 10) (- x space hsize) y image hsize vsize space horizontal)
               (number->image (quotient num 10) x (- y space vsize) image hsize vsize space horizontal))])))
;             (map (l-s list-ref img-nums) (map pos-ref (neighbors (ref-pos num)))))


;;; Procedure:
;;;   draw-nums
;;; Parameters:
;;;   image, an image
;;;   posx, an integer
;;;   posy, an integer
;;;   hscale, a positive number
;;;   vscale, a positive number
;;; Purpose:
;;;   To draw the numbers on the image after deciding on which number(s) to draw 
;;;   and on which side of the image to draw each of them
;;; Produces:
;;;   image, a revised image
;;; Preconditions:
;;;   0<= posx< 32
;;;   0<= posy< 32
;;; Postconditions:
;;;   result will be an image with numbers drawn on the side to which the player
;;;   is able to go. 
(define draw-nums
  (lambda (image posx posy hscale vscale)
    (let ([x (* hscale (+ 1 posx))]
          [y (* vscale (+ 1 posy))])
      (when ;East
          (equal? (irgb 0 0 0) (image-get-pixel image (- (+ x hscale) 1) y))
        (number->image 
         (list-ref img-nums (pos->ref (cons (+ 1 posx) posy)))
         (* hscale (+ 1 size))
         (+ 4 (/ (* vscale size) 2))
         image
         (/ hscale 2)
         (/ vscale 2)
         hscale
         #f))
      (when;West
          (equal? (irgb 0 0 0) (image-get-pixel image (- x 1) y))
        (number->image 
         (list-ref img-nums (pos->ref (cons (- posx 1) posy)))
         4
         (+ 1 (/ (* vscale size) 2))
         image
         (/ hscale 2)
         (/ vscale 2)
         hscale
         #f))
      (when;North
          (equal? (irgb 0 0 0) (image-get-pixel image x (- y 1)))
        (number->image 
         (list-ref img-nums (pos->ref (cons posx (- posy 1))))
         (+ 1 (/ (* hscale size) 2))
         4
         image
         (/ hscale 2.5)
         (/ vscale 2.5)
         (/ hscale 2)
         #t))
      (when;South
          (equal? (irgb 0 0 0) (image-get-pixel image x (- (+ y vscale) 1)))
        (number->image
         (list-ref img-nums (pos->ref (cons posx (+ 1 posy))))
         (+ 1 (/ (* hscale size) 2))
         (* vscale (+ 1 size))
         image
         (/ hscale 2.5)
         (/ vscale 2.5)
         (/ hscale 2)
         #t))))) 







