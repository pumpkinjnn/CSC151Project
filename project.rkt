#lang racket

(require gigls/unsafe)
;;;Procedure:game
;;;Parameter:num, an positive integer between 1~999
;;;          width, a positive number, width of the image
;;;          height, a positive number, height of the image
;;;Purpose:  apply the game mechinism to the whole procedure
;;;Produce:  nothing



(define game
  (lambda (x width height)
    (cond [(= x 0)
           (draw-magic-circle1 311 width height)]
          [(or (= x 311) 
               (= x 324)
               (= x 339)
               (= x 356)
               (= x 375)
               (= x 396)
               (= x 419)
               (= x 444)
               (= x 471))
           (draw-magic-circle1 (+ 275 (sqr (+ 5 (+ 1 (- (sqrt (- x 275)) 5))))) width height)]
          
          [(= x 500)
           (draw-magic-circle2 28 width height)]
          
          [(or (= x 28)
               (= x 66)
               (= x 114)
               (= x 172)
               (= x 240)
               (= x 318)
               (= x 406)
               (= x 504)
               (= x 612))
           (draw-magic-circle2 (round (* 5 (- (sqr (+ 2.3 (+ 1 (- (sqrt (+ (/ x 5) 5.29)) 2.3)))) 5.29))) width height)]
          
          [(= x 730)
           (draw-magic-circle3 841 width height)]
          
          [(= x 841)
           (draw-magic-circle3 909 width height)]
          [(= x 909)
           (draw-magic-circle3 141 width height)]
          [(= x 141)
           (draw-magic-circle3 756 width height)]
          [(= x 756)
           (draw-magic-circle3 958 width height)]
          [(= x 958)
           (draw-magic-circle3 279 width height)]
          [(= x 279)
           (draw-magic-circle3 656 width height)]
          [(= x 656)
           (draw-magic-circle3 989 width height)]
          [(= x 989)
           (draw-magic-circle3 412 width height)]
          [(= x 412)
           (draw-magic-circle3 544 width height)]
             
          
           [(= x 544)
           (draw-magic-circle4 17 width height)]
           
          [(or (= x 17)
               (= x 53)
               (= x 104)
               (= x 169)
               (= x 247)
               (= x 337)
               (= x 438)
               (= x 549)
               (= x 670))
           (draw-magic-circle4 (floor (+ 1 (* 16 (expt (+ 1 (expt (/ (- x 1) 16) (/ 1 1.7))) 1.7)))) width height)]
          
            [(= x 801)
           (draw-magic-circle5 540 width height)]
          
            
          [(= x 540)
           (draw-magic-circle5 416 width height)]
          [(= x 416)
           (draw-magic-circle5 979 width height)]
          [(= x 979)
           (draw-magic-circle5 653 width height)]
          [(= x 653)
           (draw-magic-circle5 283 width height)]
          [(= x 283)
           (draw-magic-circle5 960 width height)]
          [(= x 960)
           (draw-magic-circle5 753 width height)]
          [(= x 753)
           (draw-magic-circle5 145 width height)]
          [(= x 145)
           (draw-magic-circle5 911 width height)]
          [(= x 911)
           (draw-magic-circle5 839 width height)]
          
          [(= x 839)
           (define source (image-load "/home/chenziwe/Desktop/project/zhihuishu.jpg"))
           (define target (image-new width height))
           (image-copy-paste-block! 
            source 0 0 
            target 0 0 width height)
           (image-show target)]
          
          
          [else (draw-tree x width height)]
          )
    
    
    ))

;;;;;;;;;;;;;;;;;;;general procedures;;;;;;;;;;;;;;;;;;

;;;Procedure:split-num
;;;Parameter:num, a three digits number
;;;Purpose:creat a list of 3 numbers, 
;;;        first is the hundreds digit, next is the tens digit, the last is the unit digit
;;;Produce:splited-num, a list
(define split-num 
  (lambda (num)
    (list (quotient num 100)
          (remainder (quotient num 10) 10)
          (remainder num 10))))

;;;Procedure:draw-bg
;;;Parameter:image, an image
;;;          color, an rgb encoded color
;;;Purpose:draw the background of the image, a halo at the center of the image 
;;;Produce:drawed-bg, an image with a halo at its center
;;;        the inner color is the complement of color inputed with each rgb component substracted by 50
;;;        the outer color is the color inputed with each rgb component substracted by 50
;;;        the outmost strok is the inputed with each rgb component substracted by 240(black if all rgb components is smaller than 240)
(define draw-bg
  (lambda (image color)
    (let* ([bgcolor (irgb (- (irgb-red color) 50)
                          (- (irgb-green color) 50)
                          (- (irgb-blue color) 50))]
           [bgcolor2 (irgb (- (irgb-red color) 240)
                           (- (irgb-green color) 240)
                           (- (irgb-blue color) 240))]
           [icolor (irgb-complement bgcolor)]
           [innercolor (irgb (- (irgb-red icolor) 50)
                             (- (irgb-green icolor) 50)
                             (- (irgb-blue icolor) 50))]
           [center-col (/ (image-width image) 2)]
           [center-row (/ (image-height image) 2)]
           [h-axis (/ (image-width image) 5)]
           [v-axis (/ (image-width image) 10)]
           [a h-axis]
           [b v-axis]
           [r1 (lambda (x y) (floor (/ (* 1.1 a) (sqrt (- 1 (* (- 1 (sqr (/ (* 1.1 a) (* 1.1 b))))
                                                               (/ (sqr (- y center-row))
                                                                  (+ (sqr (- x center-col))
                                                                     (sqr (- y center-row))))))))))]
           [r2 (lambda (x y) (floor (/ (* 2 a) (sqrt (- 1 (* (- 1 (sqr (/ (* 2 a) (* 2 b))))
                                                             (/ (sqr (- y center-row))
                                                                (+ (sqr (- x center-col))
                                                                   (sqr (- y center-row))))))))))]
           [r3 (lambda (x y) (floor (/ (* 0.9 a) (sqrt (- 1 (* (- 1 (sqr (/ (* 0.9 a) (* 0.9 b))))
                                                               (/ (sqr (- y center-row))
                                                                  (+ (sqr (- x center-col))
                                                                     (sqr (- y center-row))))))))))]
           
           [bg (image-compute
                (lambda (x y)
                  (cond 
                    [(and (> (+ (/ (sqr(- x center-col)) (sqr (* 2 a)))
                                (/ (sqr(- y center-row)) (sqr (* 2 b)))) 1)
                          (or (not (= x center-col)) (not (= y center-row))))
                     (irgb (* (irgb-red bgcolor2) (+ 1 (sin (/ (sqrt (+ (sqr (- x center-col)) (sqr (- y center-row)))) (/ a 3)))))
                           (* (irgb-green bgcolor2) (+ 1 (sin (/ (sqrt (+ (sqr (- x center-col)) (sqr (- y center-row)))) (/ a 3)))))
                           (* (irgb-blue bgcolor2) (+ 1 (sin (/ (sqrt (+ (sqr (- x center-col)) (sqr (- y center-row)))) (/ a 3))))))]
                    [(and (> (+ (/ (sqr(- x center-col)) (sqr h-axis))
                                (/ (sqr(- y center-row)) (sqr v-axis))) 1)
                          (<= (+ (/ (sqr(- x center-col)) (sqr (* 2 a)))
                                 (/ (sqr(- y center-row)) (sqr (* 2 b)))) 1)
                          (or (not (= x center-col)) (not (= y center-row))))
                     (irgb (- (irgb-red bgcolor) 
                              (* (/ (- (sqrt (+ (sqr (- x center-col)) 
                                                (sqr (- y center-row))))
                                       (r1 x y))
                                    (- (r2 x y) (r1 x y)))
                                 (irgb-red bgcolor)))
                           (- (irgb-green bgcolor) 
                              (* (/ (- (sqrt (+ (sqr (- x center-col)) 
                                                (sqr (- y center-row))))
                                       (r1 x y))
                                    (- (r2 x y) (r1 x y)))
                                 (irgb-green bgcolor)))
                           (- (irgb-blue bgcolor) 
                              (* (/ (- (sqrt (+ (sqr (- x center-col)) 
                                                (sqr (- y center-row))))
                                       (r1 x y))
                                    (- (r2 x y) (r1 x y)))
                                 (irgb-blue bgcolor))))]
                    [(and (<= (+ (/ (sqr(- x center-col)) (sqr h-axis))
                                 (/ (sqr(- y center-row)) (sqr v-axis))) 1)
                          (or (not (= x center-col)) (not (= y center-row))))
                     (irgb (+ (irgb-red innercolor) 
                              (* (/ (sqrt (+ (sqr (- x center-col)) 
                                             (sqr (- y center-row)))) 
                                    (r3 x y))
                                 (- (irgb-red bgcolor) (irgb-red innercolor))))
                           (+ (irgb-green innercolor) 
                              (* (/ (sqrt (+ (sqr (- x center-col)) 
                                             (sqr (- y center-row)))) 
                                    (r3 x y))
                                 (- (irgb-green bgcolor) (irgb-green innercolor))))
                           (+ (irgb-blue innercolor) 
                              (* (/ (sqrt (+ (sqr (- x center-col)) 
                                             (sqr (- y center-row)))) 
                                    (r3 x y))
                                 (- (irgb-blue bgcolor) (irgb-blue innercolor)))))]
                    [else innercolor]))
                (image-width image) (image-height image))])
      (image-copy-paste-block! bg 0 0 image 0 0 (image-width image) (image-height image)))))


;;;Procedure:generate-color
;;;Parameter:num, an integer between 0~9
;;;Purpose:creat return an rgb encoded color depending on ten conditions(input 0~9)
;;;              with name of "white" "lightsalmon" "lightblue" "gainsboro" "lavender" "lemonchiffon" "pink" "lightcyan" "lightblue" "paleturquoise",respectively       
;;;Produce:generated-colors, an rgb encoded color
(define generate-color
  (lambda (num)
    (cond [(= num 0)
           (irgb 255 255 255)]
          [(= num 1)
           (irgb 255 150 150)]
          [(= num 2)
           (irgb 200 200 255)]
          [(= num 3)
           (irgb 200 255 200)]
          [(= num 4)
           (irgb 255 200 255)]
          [(= num 5)
           (irgb 255 255 200)]
          [(= num 6)
           (irgb 255 192 203)]
          [(= num 7)
           (irgb 200 255 255)]
          [(= num 8)
           (irgb 192 203 255)]
          [(= num 9)
           (irgb 192 255 203)])))


;;;Procedure:25-num
;;;Parameter:seed, an integer
;;;Purpose:produce a list of 25 integers between 0~24
;;;        with a given seed the list in final, that is each time we input the same seed, the random numbers will be the same
;;;Produce:25-nums, a list
(define 25-num
  (lambda (seed)
    (random-seed seed)
    (let kernel ([list-so-far '()]
                 [n 25])
      (cond [(< n 1) list-so-far]
            [else (kernel (cons (random 25) list-so-far) (- n 1))]))))


;;;Purpose:define 4 values, each is a list of 25 integers between 0~24 
;;;        these will be used later to determine the position of the stars in the back ground
(define star-col1 (25-num 1))
(define star-row1 (25-num 2))
(define star-col2 (25-num 3))
(define star-row2 (25-num 4))


;;;Procedure:draw-central-circles
;;;Parameter:image, an image
;;;          circle-num, an positive integer(in this case will only be between 1~9)
;;;          left, a positive number
;;;          top, a positive number
;;;          right, a positive number, must be bigger than left 
;;;          bottom, a positive number, must be bigger than top
;;;Purpose:draw circle-num circles on the image with left, top, right, bottom edges respectively   
;;;Produce:nothing, but will draw circle-num circles on the image
(define draw-central-circles
  (lambda (image circle-num left top right bottom)
    (let* ([x-dis (/ (- right left) circle-num 2)]
           [y-dis (/ (- bottom top) circle-num 2)]
           [lefts (map (l-s + left) (map (l-s * x-dis) (iota circle-num)))]
           [tops  (map (l-s + top) (map (l-s * y-dis) (iota circle-num)))]
           [widths (map (l-s - (- right left)) (map (l-s * (* x-dis 2)) (iota circle-num)))]
           [heights (map (l-s - (- bottom top)) (map (l-s * (* y-dis 2)) (iota circle-num)))]
           [draw-one-circle! (lambda (l t width height)
                               (image-select-ellipse! image REPLACE 
                                                      l t width height)
                               (image-stroke-selection! image)
                               (image-select-nothing! image))])
      (for-each draw-one-circle! lefts tops widths heights))))



;;;Procedure:roman-num
;;;Parameter:image, an image
;;;          h, a positive number, the central col of the roman number
;;;          k, a positive number, the central row of the roman number
;;;          R, a positive number, the radius of the roman number
;;;          ratio, the ratio of width and height of the image
;;;          num, a integer between 0 and 9
;;;Purpose:draw a roman-number on the image centered at (h,k) with R radius if ratio is 1, with width of 2R*ratio and height of 2R  
;;;Produce:roman-numed, a image with a roman number drawed on it, the raman number is same as the integer inputed
(define roman-num
  (lambda (image h k R ratio num)
    (let* ([urow (- k (/ R 1.5))]
           [lrow (+ k (/ R 1.5))]
           [cols (map (l-s + h) (map (r-s * (/ (* R 0.8) 3)) (map (r-s - 3)(iota 7))))]
           [my-cols (map (l-s + (* h (- 1 ratio))) (map (l-s * ratio) cols))])
      (cond [(= num 0)
             (image-draw-line! image (list-ref my-cols 1)
                                     urow
                                     (list-ref my-cols 5)
                                     lrow)
             (image-draw-line! image (list-ref my-cols 1)
                                     lrow
                                     (list-ref my-cols 5)
                                     urow)]
            [(= num 1)
             (image-draw-line! image (list-ref my-cols 3)
                                     urow
                                     (list-ref my-cols 3)
                                     lrow)]
            [(= num 2)
             (image-draw-line! image (list-ref my-cols 2)
                                     urow
                                     (list-ref my-cols 2)
                                     lrow)
             (image-draw-line! image (list-ref my-cols 4)
                                     urow
                                     (list-ref my-cols 4)
                                     lrow)]
            [(= num 3)
             (image-draw-line! image (list-ref my-cols 1)
                                     urow
                                     (list-ref my-cols 1)
                                     lrow)
             (image-draw-line! image (list-ref my-cols 3)
                                     urow
                                     (list-ref my-cols 3)
                                     lrow)
             (image-draw-line! image (list-ref my-cols 5)
                                     urow
                                     (list-ref my-cols 5)
                                     lrow)]
            [(= num 4)
             (image-draw-line! image (list-ref my-cols 1)
                                     urow
                                     (list-ref my-cols 1)
                                     lrow)
             (image-draw-line! image (list-ref my-cols 2)
                                     urow
                                     (list-ref my-cols 3)
                                     lrow)
             (image-draw-line! image (list-ref my-cols 4)
                                     urow
                                     (list-ref my-cols 3)
                                     lrow)]
            [(= num 5)
             (image-draw-line! image (list-ref my-cols 1)
                                     urow
                                     (list-ref my-cols 3)
                                     lrow)
             (image-draw-line! image (list-ref my-cols 5)
                                     urow
                                     (list-ref my-cols 3)
                                     lrow)]
            [(= num 6)
             (image-draw-line! image (list-ref my-cols 1)
                                     urow
                                     (list-ref my-cols 2)
                                     lrow)
             (image-draw-line! image (list-ref my-cols 3)
                                     urow
                                     (list-ref my-cols 2)
                                     lrow)
             (image-draw-line! image (list-ref my-cols 4)
                                     urow
                                     (list-ref my-cols 4)
                                     lrow)]
            [(= num 7)
             (image-draw-line! image (list-ref my-cols 1)
                                     urow
                                     (list-ref my-cols 2)
                                     lrow)
             (image-draw-line! image (list-ref my-cols 3)
                                     urow
                                     (list-ref my-cols 2)
                                     lrow)
             (image-draw-line! image (list-ref my-cols 4)
                                     urow
                                     (list-ref my-cols 4)
                                     lrow)
             (image-draw-line! image (list-ref my-cols 5)
                                     urow
                                     (list-ref my-cols 5)
                                     lrow)]
            [(= num 8)
             (image-draw-line! image (list-ref my-cols 0)
                                     urow
                                     (list-ref my-cols 1)
                                     lrow)
             (image-draw-line! image (list-ref my-cols 2)
                                     urow
                                     (list-ref my-cols 1)
                                     lrow)
             (image-draw-line! image (list-ref my-cols 3)
                                     urow
                                     (list-ref my-cols 3)
                                     lrow)
             (image-draw-line! image (list-ref my-cols 4)
                                     urow
                                     (list-ref my-cols 4)
                                     lrow)
             (image-draw-line! image (list-ref my-cols 5)
                                     urow
                                     (list-ref my-cols 5)
                                     lrow)]
            [(= num 9)
             (image-draw-line! image (list-ref my-cols 2)
                                     urow
                                     (list-ref my-cols 2)
                                     lrow)
             (image-draw-line! image (list-ref my-cols 3)
                                     urow
                                     (list-ref my-cols 5)
                                     lrow)
             (image-draw-line! image (list-ref my-cols 3)
                                     lrow
                                     (list-ref my-cols 5)
                                     urow)]))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;procedures for tree;;;;;;;;;;;;;;;;;;

;;;Procedure:draw-big-circles
;;;Parameter:image, an image
;;;          c-num, an positive integer, the number of the circles
;;;          left, the left edge of the circles
;;;          top, the top edge of the circles
;;;Purpose:draw concentric circles of c-num on the image with radius of width or height/5, if width and height of the image is the same
;;;        draw concentric ellipse of c-num on the image with width of image-width/5, height of image-height/5
;;;Produce:nothing, draw draw concentric circles(ellipses) of c-num on image
(define draw-big-circles
  (lambda (image c-num left top)
    (let* ([width (/ (image-width image) 5)]
           [height (/ (image-height image) 5)]
           [row (+ left (/ width 2))]
           [col (+ top (/ height 2))]
           [b-hdis (/ width 15)]
           [b-vdis (/ height 15)]
           [hdis (map (l-s * b-hdis) (list-take (list 0 1 6 7 11 12 8 9 3 4) (+ c-num 1)))]
           [vdis (map (l-s * b-vdis) (list-take (list 0 1 6 7 11 12 8 9 3 4) (+ c-num 1)))]
           [widths (map (l-s - width) hdis)]
           [heights (map (l-s - height) vdis)]
           [draw-one-circle! (lambda (width height)
                               (image-select-ellipse! image REPLACE 
                                                      (- row (/ width 2)) (- col (/ height 2)) width height)
                               (image-stroke-selection! image)
                               (image-select-nothing! image))])
      (for-each draw-one-circle! widths heights))))

;;;Procedure:draw-small-circles
;;;Parameter:image, an image
;;;          c-num, an positive integer, the number of the circles
;;;          left, the left edge of the circles
;;;          top, the top edge of the circles
;;;Purpose:draw concentric circles of c-num on the image with radius of width or height/10, if width and height of the image is the same
;;;        draw concentric ellipse of c-num on the image with width of image-width/10, height of image-height/10
;;;Produce:nothing, draw draw concentric circles(ellipses) of c-num on image
(define draw-small-circles
  (lambda (image c-num left top)
    (let* ([width (/ (image-width image) 10)]
           [height (/ (image-height image) 10)]
           [row (+ left (/ width 2))]
           [col (+ top (/ height 2))]
           [b-hdis (/ width 15)]
           [b-vdis (/ height 15)]
           [hdis (map (l-s * b-hdis) (list-take (list 0 1 6 7 11 12 8 9 3 4) (+ c-num 1)))]
           [vdis (map (l-s * b-vdis) (list-take (list 0 1 6 7 11 12 8 9 3 4) (+ c-num 1)))]
           [widths (map (l-s - width) hdis)]
           [heights (map (l-s - height) vdis)]
           [draw-one-circle! (lambda (width height)
                               (image-select-ellipse! image REPLACE 
                                                      (- row (/ width 2)) (- col (/ height 2)) (ceiling width) (ceiling height))
                               (image-stroke-selection! image)
                               (image-select-nothing! image))])
      (for-each draw-one-circle! widths heights))))


;;;Procedure:draw-line-same
;;;Parameter:turtle, a turtle 
;;;          left1, the left edge of the first circle
;;;          top1, the top edge of the second circles
;;;          left2, the left edge of the first circle
;;;          top2, the top edge of the second circle 
;;;          hradius, the horizontal radius of the first "circle"
;;;          vradius, the vertical radius of the first "circle"
;;;          hradius2, the horizontal radius of the second "circle"
;;;          vradius2, the vertical radius of the second "circle"
;;;Purpose: draw a line between two circles    
;;;Produce: nothing
(define draw-line-same
  (lambda (turtle left1 top1 left2 top2 hradius vradius hradius2 vradius2)
    (let* ([col1 (+ left1 hradius)]
           [row1 (+ top1 vradius)]
           [col2 (+ left2 hradius2)]
           [row2 (+ top2 vradius2)])
      (turtle-teleport! turtle col1 row1)
      (cond [(and (> col2 col1) (> row2 row1))
             (turtle-face! turtle 45)
             (turtle-forward! turtle (* (sqrt 2) (abs(- col1 col2))))
             (turtle-face! turtle 0)
             (turtle-forward! turtle (- col2 (turtle-col turtle)))]
            [(and (< col2 col1) (> row2 row1))
             (turtle-face! turtle 135)
             (turtle-forward! turtle (* (sqrt 2) (abs(- row1 row2))))
             (turtle-face! turtle -180)
             (turtle-forward! turtle  (- (turtle-col turtle) col2))]
            [(and (> col2 col1) (< row2 row1))
             (turtle-face! turtle -45)
             (turtle-forward! turtle (* (sqrt 2) (abs(- col1 col2))))
             (turtle-face! turtle 0)
             (turtle-forward! turtle  (- col2 (turtle-col turtle)))]
            [(and (< col2 col1) (< row2 row1))
             (turtle-face! turtle -135)
             (turtle-forward! turtle (* (sqrt 2) (abs(- row1 row2))))
             (turtle-face! turtle -90)
             (turtle-forward! turtle  (- (turtle-col turtle) col2))]
            [(and (< col2 col1) (= row2 row1))
             (turtle-face! turtle 180)
             (turtle-forward! turtle  (abs(- col1 col2)) )]
            [(and (> col2 col1) (= row2 row1))
             (turtle-face! turtle 0)
             (turtle-forward! turtle (abs(- col1 col2)) )]
            [(and (= col2 col1) (< row2 row1))
             (turtle-face! turtle -90)
             (turtle-forward! turtle (abs(- row1 row2)) )]
            [(and (= col2 col1) (> row2 row1))
             (turtle-face! turtle 90)
             (turtle-forward! turtle (abs(- row1 row2)) )]
            ))))


;;;Procedure:generate-big-list
;;;Parameter:num, an integer between 0~9
;;;Purpose:generate a list of numbers, later will be used as either the x or y coordinate of big circles in the tree  
;;;Produce:generated-big-list, a list
(define generate-big-list
  (lambda (num)
    (let* ([b-list (map (r-s - 5) (iota 10))]
           [fm1 (lambda (n)
                  (* 4 (+ (sin n) 1)))]
           [fm2 (lambda (n)
                  (* 4 (+ (cos n) 1)))]
           [fm3 (lambda (n)
                  (* (/ 8 25) (sqr n)))]
           [fm4 (lambda (n)
                  (- 8 (* (/ 8 25) (sqr n))))])
    (cond [(= num 0)
           (list 5.5 2.5 0 8 6 2 1 7 4.5 3.5)]
          [(= num 1)
           (map fm1 (map (l-s * (/ pi 5)) b-list))]
          [(= num 2)
           (map fm2 (map (l-s * (/ pi 5)) b-list))]
          [(= num 3)
           (map fm3 b-list)]
          [(= num 4)
           (map fm4 b-list)]
          [(= num 5)
           (list 0.5 0.5 2 2 3 3 5.5 5.5 7.5 7.5)]
          [(= num 6)
           (map (l-s + 0.25) (reverse (map (l-s * 0.8) (iota 10))))]
          [(= num 7)
           (map (l-s + 0.25) (reverse (map (l-s * 0.8) (iota 10))))]
          [(= num 8)
           (map (l-s + 0.25) (map (l-s * 0.8) (iota 10)))]
          [(= num 9)
           (map (l-s + 0.25) (map (l-s * 0.8) (iota 10)))]))))

;;;Procedure:generate-small-list
;;;Parameter:num, an integer between 0~9
;;;Purpose:generate a list of numbers, later will be used as either the x or y coordinate of small circles in the tree  
;;;Produce:generated-small-list, a list
(define generate-small-list
  (lambda (num)
    (let* ([b-list (map (r-s - 5) (iota 10))]
           [fm1 (lambda (n)
                  (* 4.5 (+ (sin n) 1)))]
           [fm2 (lambda (n)
                  (* 4.5 (+ (cos n) 1)))]
           [fm3 (lambda (n)
                  (* (/ 9 25) (sqr n)))]
           [fm4 (lambda (n)
                  (- 9 (* (/ 9 25) (sqr n))))])
    (cond [(= num 0)
           (list 1 8 9 0 4.5 4.5 1 7 2 8)]
          [(= num 1)
           (map fm1 (map (l-s * (/ pi 5)) b-list))]
          [(= num 2)
           (map fm2 (map (l-s + pi) (map (l-s * (/ pi 5)) b-list)))]
          [(= num 4)
           (map fm3 b-list)]
          [(= num 3)
           (map fm4 b-list)]
          [(= num 5)
           (list 0.5 0.5 4 4 4.5 6 8 9 9 8)]
          [(= num 6)
           (map (l-s + 0.5) (map (l-s * 0.9) (iota 10)))]
          [(= num 7)
           (map (l-s + 0.5) (map (l-s * 0.9) (iota 10)))]
          [(= num 8)
           (map (l-s + 0.5) (map (l-s * 0.9) (iota 10)))]
          [(= num 9)
           (map (l-s + 0.5) (map (l-s * 0.9) (iota 10)))]))))



;;;Procedure:draw-tree
;;;Parameter:num, an positive integer of 3 digits
;;;          width, an positive integer, the width of the image
;;;          height, an positive integer, the height of the image
;;;Purpose:to draw a magic tree, the hundreds digit determines the color of the tree    
;;;                              the tens digit determines the positions of the circles on the tree
;;;                              the unit digit determines the number of circles inside each of the biggest circle of the tree  
;;;Produce:drawed-tree, am image
(define draw-tree
  (lambda (num width height)
    (let* ([nums (split-num num)]
          [fnum (car nums)]
          [snum (cadr nums)]
          [tnum (caddr nums)]
          [image (image-new width height)]
          [tommy (turtle-new image)]
          [unit-width (/ width 10)]
          [unit-height (/ height 10)]
          [widths (map (l-s * unit-width) (generate-big-list snum))]
          [heights (map (l-s * unit-height) (generate-big-list (remainder (+ 5 snum) 10)))]
          [s-widths (map (l-s * unit-width) (generate-small-list snum))]
          [s-heights (map (l-s * unit-height) (generate-small-list (remainder (+ 5 snum) 10)))]
          [fgcolor (generate-color fnum)])
      
      (draw-bg image fgcolor)
     
      (context-set-fgcolor! (irgb-lighter (irgb-lighter fgcolor)))
      (context-set-brush! "Star Brush #2")
      (for-each image-blot! (make-list 25 image)
                            (map ceiling (map (l-s * (/ width 25)) star-col1))
                            (map ceiling (map (l-s * (/ height 25)) star-row1)))
      
      (context-set-fgcolor! "white")
      (for-each image-blot! (make-list 25 image)
                            (map ceiling (map (l-s * (/ width 25)) star-col2))
                            (map ceiling (map (l-s * (/ height 25)) star-row2)))
      
      
      (context-set-fgcolor! fgcolor)
      (context-set-brush! "Nova")
      (for-each draw-big-circles (make-list 10 image)
                                   (make-list 10 tnum)
                                   widths
                                   heights)
      
      (for-each draw-small-circles (make-list 10 image)
                                   (make-list 10 tnum)
                                   s-widths
                                   s-heights)
      
      (turtle-set-brush! tommy "Nova")
      (turtle-set-color! tommy fgcolor)
      (for-each draw-line-same (make-list 9 tommy)
                               (map (l-s list-ref widths) (list 0 2 4 6 8 7 5 3 1))
                               (map (l-s list-ref heights) (list 0 2 4 6 8 7 5 3 1))
                               (map (l-s list-ref widths) (list 2 4 6 8 7 5 3 1 0))
                               (map (l-s list-ref heights) (list 2 4 6 8 7 5 3 1 0))
                               (make-list 9 (/ width 10))
                               (make-list 9 (/ height 10))
                               (make-list 9 (/ width 10))
                               (make-list 9 (/ height 10)))
      
      (for-each draw-line-same (make-list 9 tommy)
                               (map (l-s list-ref s-widths) (list 0 2 4 6 8 7 5 3 1))
                               (map (l-s list-ref s-heights) (list 0 2 4 6 8 7 5 3 1))
                               (map (l-s list-ref s-widths) (list 2 4 6 8 7 5 3 1 0))
                               (map (l-s list-ref s-heights) (list 2 4 6 8 7 5 3 1 0))
                               (make-list 9 (/ width 20))
                               (make-list 9 (/ height 20))
                               (make-list 9 (/ width 20))
                               (make-list 9 (/ height 20)))
      
      (for-each draw-line-same (make-list 5 tommy)
                               (map (l-s list-ref widths) (list 3 4 5 6 7))
                               (map (l-s list-ref heights) (list 3 4 5 6 7))
                               (map (l-s list-ref s-widths) (list 1 2 5 8 9))
                               (map (l-s list-ref s-heights) (list 1 2 5 8 9))
                               (make-list 5 (/ width 10))
                               (make-list 5 (/ height 10))
                               (make-list 5 (/ width 20))
                               (make-list 5 (/ height 20)))
      (image-refresh-display! image)
      (image-show image))))



;;;;;;;;;;;;;;;;;;;;;;;;;magic 1;;;;;;;;;;;;;;;;;;;;

;;;Procedure:draw-star
;;;Parameter:image, an image 
;;;          width, the width of the star to be drawn
;;;          height, the height of the star to be drawn
;;;Purpose: to draw a star of width*height on image    
;;;Produce:drawn-star, an image
(define draw-star
  (lambda (image width height)
    (let ([central-col (/ (image-width image)2)]
          [central-row (/ (image-height image)2)]
          [unit-angle (/ (* 4 pi) 5)]
          [start-angle (/ pi 2)])
      (image-draw-line! image  central-col ;col1 row1 col2 row2
                        (- central-row (* (/ height 2)))
                        (+ central-col (* (/ width 2) (cos (+ start-angle unit-angle))));bottomleft
                        (- central-row (* (/ height 2) (sin (+ start-angle unit-angle)))));bottomleft
      (image-draw-line! image  (- central-col (* (/ width 2) (cos (+ start-angle unit-angle))));bottomright
                        (- central-row (* (/ height 2) (sin (+ start-angle unit-angle))));bottomright
                        (+ central-col (* (/ width 2) (cos (+ start-angle (/ unit-angle 2)))));topleft
                        (- central-row (* (/ height 2) (sin (+ start-angle (/ unit-angle 2))))));topleft
      (image-draw-line! image  (+ central-col (* (/ width 2) (cos (+ start-angle (/ unit-angle 2)))));topleft
                        (- central-row (* (/ height 2) (sin (+ start-angle (/ unit-angle 2)))));topleft
                        (+ central-col (* (/ width 2) (cos (- start-angle (/ unit-angle 2)))));topright
                        (- central-row (* (/ height 2) (sin (- start-angle (/ unit-angle 2))))));topright
      (image-draw-line! image (+ central-col (* (/ width 2) (cos (- start-angle (/ unit-angle 2)))));topright
                        (- central-row (* (/ height 2) (sin (- start-angle (/ unit-angle 2)))));topright
                         (+ central-col (* (/ width 2) (cos (+ start-angle unit-angle))));bottomleft
                        (- central-row (* (/ height 2) (sin (+ start-angle unit-angle)))));bottomleft
      (image-draw-line! image  
                         (- central-col (* (/ width 2) (cos (+ start-angle unit-angle))));bottomright
                         (- central-row (* (/ height 2) (sin (+ start-angle unit-angle))));bottomright
                         central-col ;col1 row1 col2 row2
                         (- central-row (* (/ height 2)))))))


;;;Procedure:draw-magic-circle1
;;;Parameter:num, an positive integer between 1~999
;;;          width, a positive number, width of the image
;;;          height, a positive number, height of the image
;;;Purpose: to draw the first magic circle, 
;;;         hundres digit determines the number of roman number showed
;;;         tens digit determines the number of the stars
;;;         unit digit determines the number of the circles
;;;Produce:magic-circle1, an image
(define draw-magic-circle1
  (lambda (num width height)
    (let* ([nums (split-num num)]
          [fnum (car nums)]
          [snum (cadr nums)]
          [tnum (caddr nums)]
          [ratio (/ width height)]
          [image (image-new width height)])
      
      (context-set-fgcolor! "black")
      (image-select-all! image)
      (image-fill-selection! image)
      (image-select-nothing! image)
      (draw-bg image (irgb 200 100 0))
      (draw-star image width height)
      (context-set-fgcolor! "white")
      (for-each image-blot! (make-list 25 image)
                            (map ceiling (map (l-s * (/ width 25)) star-col2))
                            (map ceiling (map (l-s * (/ height 25)) star-row2)))
      (for-each image-blot! (make-list 25 image)
                            (map ceiling (map (l-s * (/ width 25)) star-col1))
                            (map ceiling (map (l-s * (/ height 25)) star-row1)))
      
      (context-set-fgcolor! "orange")
      (context-set-brush! "Nova")
      (image-select-ellipse! image REPLACE 
            (* 0.1 width) (* 0.1 height) (* 0.8 width) (* 0.8 height))
      (image-stroke-selection! image)
      (image-select-ellipse! image REPLACE 
            (* 0.2 width) (* 0.2 height) (* 0.6 width) (* 0.6 height))
      (image-stroke-selection! image)
      (image-select-nothing! image)
  
      
      (cond [(not (= tnum 0))
             (draw-central-circles image tnum (* 0.4 width) (* 0.4 height)(* 0.6 width) (* 0.6 height))])
      
      (cond [(not (= snum 0))
      (for-each draw-star (make-list snum image) (map (l-s * (*  width 0.45)) (map increment (map (r-s / 10) (iota snum))))
                                                 (map (l-s * (*  height 0.45)) (map increment (map (r-s / 10) (iota snum)))))])
      
      (roman-num image (* 0.5 width) (* 0.9 height) (* 0.03 (+ width height)) ratio fnum)
      (image-refresh-display! image)
      (image-show image))))


;;;;;;;;;;;;;;;;;;;;;;;;magic 2;;;;;;;;;;;;;;;;;;;;;;;

;;;Procedure:draw-squares
;;;Parameter:image, an image
;;;          num, an integer, the number of squares
;;;          R, a positive number, the radius of the circle which contain the squares
;;;          ratio, a positive numebr, image's width/height
;;;          c-col, a positive numebr, the central column
;;;          c-row, a positive numebr, the central row
;;;Purpose: draw num sqaures within a given circle with R radius
;;;Produce:nothing
(define draw-squares
  (lambda (image num R ratio c-col c-row)
    (let* ([h c-col]
           [k c-row]
           [dx1 (lambda (n)
                  (+ h (* R (cos (- pi (+ (/ pi 4) (* n (/ pi 5))))))))]
           [dy1 (lambda (n)
                  (+ k (* R (sin (+ (/ pi 4) (* n (/ pi 5)))))))]
           [dx2 (lambda (n)
                  (+ h (* R (cos (- pi (+ (* (/ 3 4) pi) (* n (/ pi 5))))))))]
           [dy2 (lambda (n)
                  (+ k (* R (sin (+ (* (/ 3 4) pi) (* n (/ pi 5)))))))]
           [dx3 (lambda (n)
                  (+ h (* R (cos (- pi (+ (* (/ 5 4) pi) (* n (/ pi 5))))))))]
           [dy3 (lambda (n)
                  (+ k (* R (sin (+ (* (/ 5 4) pi) (* n (/ pi 5)))))))]
           [dx4 (lambda (n)
                  (+ h (* R (cos (- pi (+ (* (/ 7 4) pi) (* n (/ pi 5))))))))]
           [dy4 (lambda (n)
                  (+ k (* R (sin (+ (* (/ 7 4) pi) (* n (/ pi 5)))))))]
           [create-points (lambda (i)
                            (list (cons (dx1 i)  (dy1 i))
                                  (cons (dx2 i)  (dy2 i))
                                  (cons (dx3 i)  (dy3 i))
                                  (cons (dx4 i)  (dy4 i))))]
           [all-points (map create-points (iota 10))]
           [picked-points (list-take all-points num)]
           [my-points (let kernel ([i 0]
                                   [cur-points '()]
                                   )
                        (cond [(>= i num)
                               (reverse cur-points)]
                              [else
                               (kernel (+ i 1)
                                       (cons (list (cons (+ (* h (- 1 ratio)) (* (caar (list-ref picked-points i)) ratio)) (cdar (list-ref picked-points i)) )
                                                   (cons (+ (* h (- 1 ratio)) (* (caadr (list-ref picked-points i)) ratio))  (cdadr (list-ref picked-points i)))
                                                   (cons (+ (* h (- 1 ratio)) (* (caaddr (list-ref picked-points i)) ratio)) (cdaddr (list-ref picked-points i)) )
                                                   (cons (+ (* h (- 1 ratio)) (* (caar (cdddr (list-ref picked-points i))) ratio))  (cdar (cdddr (list-ref picked-points i))) ))
                                             cur-points))]))])
      (let kernel2 ([i 0])
        (when (< i num)
          (image-select-polygon! image REPLACE (list-ref my-points i))
          (image-stroke-selection! image)
          (image-select-nothing! image)
          (kernel2 (+ i 1))
          )))))

;;;Procedure:draw-spikes
;;;Parameter:image, an image
;;;          h, an integer, the x coordinate of the central
;;;          k. an integer, the y coordinate of the central
;;;          R, a positive number, the radius of the circle which contain the squares
;;;          L, a positive number, the half length of the spikes
;;;          ratio, a positive numebr, image's width/height
;;;Purpose: draw 3 spikes 
;;;Produce:nothing
(define draw-spikes
  (lambda (image h k R L ratio)
   (let* ([dx1 (lambda (n)
                 (+ h (* R (cos (- pi (* n (/ pi 4)))))))]
           [dy1 (lambda (n)
                 (+ k (* R (sin (* n (/ pi 4))))))]
           [dx2 (lambda (n)
                 (+ h (* L (cos (- (/ pi 2) (* n (/ pi 4)))))))]
           [dy2 (lambda (n)
                 (+ k (* L (sin  (+ (/ pi 2) (* n (/ pi 4)))))))]
           [dx3 (lambda (n)
                 (+ h (* R (cos (* n (/ pi 4))))))]
           [dy3 (lambda (n)
                 (+ k (* R (sin (+ pi (* n (/ pi 4)))))))]
           [dx4 (lambda (n)
                 (- (* 2 h ) (dx2 n)))]
           [dy4 (lambda (n)
                 (- (* 2 k ) (dy2 n)))]
           [create-points (lambda (i)
                             (list (cons (dx1 i)  (dy1 i))
                                   (cons (dx2 i)  (dy2 i))
                                   (cons (dx3 i)  (dy3 i))
                                   (cons (dx4 i)  (dy4 i))))]
           [all-points (map create-points (iota 4))]
           [my-points (let kernel ([i 0]
                                   [cur-points '()]
                                       )
                        (cond [(>= i 4)
                               (reverse cur-points)]
                            [else
                             (kernel (+ i 1)
                              (cons (list (cons (+ (* h (- 1 ratio)) (* (caar (list-ref all-points i)) ratio)) (cdar (list-ref all-points i)) )
                                  (cons (+ (* h (- 1 ratio)) (* (caadr (list-ref all-points i)) ratio))  (cdadr (list-ref all-points i)))
                                  (cons (+ (* h (- 1 ratio)) (* (caaddr (list-ref all-points i)) ratio)) (cdaddr (list-ref all-points i)) )
                                  (cons (+ (* h (- 1 ratio)) (* (caar (cdddr (list-ref all-points i))) ratio))  (cdar (cdddr (list-ref all-points i))) ))
                                   cur-points 
                                   ))]))])
     (let kernel2 ([i 0])
        (when (< i 4)
          (image-select-polygon! image REPLACE (list-ref my-points i))
          (image-stroke-selection! image)
          (image-select-nothing! image)
          (kernel2 (+ i 1))
          )))))

;;;Procedure:draw-magic-circle2
;;;Parameter:num, an positive integer between 1~999
;;;          width, a positive number, width of the image
;;;          height, a positive number, height of the image
;;;Purpose: to draw the first magic circle, 
;;;         hundres digit determines the number of squares outside
;;;         tens digit determines the number of squares inside
;;;         unit digit determines the number of the circles at the centre
;;;Produce:magic-circle2, an image
(define draw-magic-circle2
  (lambda (num width height)
   (let* ([nums (split-num num)]
          [fnum (car nums)]
          [snum (cadr nums)]
          [tnum (caddr nums)]
          [image (image-new width height)]
          [tommy (turtle-new image)]
          [HR (* 0.8 width)]
          [VR (* 0.8 height)]
          [ratio (/ width height)]
          [c-col (/ width 2)]
          [c-row (/ height 2)]
          [fgcolor (generate-color fnum)])
     
      (draw-bg image fgcolor)
      
      
      (context-set-fgcolor! (irgb-lighter (irgb-lighter fgcolor)))
      (context-set-brush! "Star Brush #2")
      (for-each image-blot! (make-list 25 image)
                            (map ceiling (map (l-s * (/ width 25)) star-col1))
                            (map ceiling (map (l-s * (/ height 25)) star-row1)))
      
      (context-set-fgcolor! "white")
      (for-each image-blot! (make-list 25 image)
                            (map ceiling (map (l-s * (/ width 25)) star-col2))
                            (map ceiling (map (l-s * (/ height 25)) star-row2)))
      
      
     
     
      
      (context-set-brush! "2. Hardness 100")
      (context-set-fgcolor! "black")
      (draw-squares image fnum (/ VR 2.2) ratio c-col c-row)
     
      (context-set-brush! "GIMP Brush #1")
      (context-set-fgcolor! fgcolor)
      (image-select-ellipse! image REPLACE (* 0.1 width) (* 0.1 height) HR VR)
      (image-stroke-selection! image)
      (image-select-nothing! image)
     
      (context-set-brush! "2. Hardness 025" 3)
      (image-select-ellipse! image REPLACE (* 0.14 width) (* 0.14 height) (* HR 0.9) (* VR 0.9))
      (image-stroke-selection! image)
      (image-select-nothing! image)
      
      (context-set-brush! "Nova")
      (context-set-fgcolor! fgcolor)
      (draw-squares image fnum (/ VR 2) ratio c-col c-row)
      (draw-squares image snum (/ VR 4) ratio c-col c-row)
      (draw-spikes image c-col c-row (/ VR 16) (/ VR 3) ratio)
     
     (if (not (= tnum 0))
      (draw-central-circles image tnum (- c-col (/ VR 4)) (- c-row (/ HR 4)) (+ c-col (/ VR 4)) (+ c-row (/ HR 4)))
      (image-refresh-display! image))
     
      (image-refresh-display! image)
      (image-show image))))






;;;;;;;;;;;;;;;;;;;;;;;;;;;magic 3;;;;;;;;;;;;;;;;;;;;;

;;;Procedure:draw-pos-triangle
;;;Parameter:image, an image
;;;          width, a positive number, width fo the ellipse that contains the tirangle
;;;          height, a positive number, height fo the ellipse that contains the tirangle
;;;Purpose:draw a triangle centered at the centre of image     
;;;Produce:drawn-pos-triangle, an image
(define draw-pos-triangle
  (lambda (image width height)
    (let ([central-col (/ (image-width image)2)]
          [central-row (/ (image-height image)2)]
          [unit-angle (/ (* pi 2) 3)]
          [start-angle (/ pi 2)])
      (image-draw-line! image  central-col ;col1 row1 col2 row2
                        (- central-row (* (/ height 2)))
                        (+ central-col (* (/ width 2) (cos (+ start-angle unit-angle))));zuoxia
                        (- central-row (* (/ height 2) (sin (+ start-angle unit-angle)))));zuoxia
      (image-draw-line! image (- central-col (* (/ width 2) (cos (+ start-angle unit-angle))));youxia
                          (- central-row (* (/ height 2) (sin (+ start-angle unit-angle))));youxia
                          (+ central-col (* (/ width 2) (cos (+ start-angle unit-angle))));zuoxia
                         (- central-row (* (/ height 2) (sin (+ start-angle unit-angle)))));zuoxia
      (image-draw-line! image  
                         (- central-col (* (/ width 2) (cos (+ start-angle unit-angle))));youxia
                         (- central-row (* (/ height 2) (sin (+ start-angle unit-angle))));youxia
                         central-col ;col1 row1 col2 row2
                         (- central-row (* (/ height 2))));zuoxia
    )))

;;;Procedure:draw-neg-triangle
;;;Parameter:image, an image
;;;          width, a positive number, width fo the ellipse that contains the tirangle
;;;          height, a positive number, height fo the ellipse that contains the tirangle
;;;Purpose:draw a reversed triangle centered at the centre of image     
;;;Produce:drawn-neg-triangle, an image
(define draw-neg-triangle
  (lambda (image width height)
    (let ([central-col (/ (image-width image)2)]
          [central-row (/ (image-height image)2)]
          [unit-angle (/ (* pi 2) 3)]
          [start-angle (/ pi 2)])

      (image-draw-line! image  central-col ;col1 row1 col2 row2
                        (+ central-row (* (/ height 2)))
                        (- central-col (* (/ width 2) (cos (+ start-angle unit-angle))));zuoxia
                        (+ central-row (* (/ height 2) (sin (+ start-angle unit-angle)))));zuoxia
      (image-draw-line! image (+ central-col (* (/ width 2) (cos (+ start-angle unit-angle))));youxia
                          (+ central-row (* (/ height 2) (sin (+ start-angle unit-angle))));youxia
                          (- central-col (* (/ width 2) (cos (+ start-angle unit-angle))));zuoxia
                         (+ central-row (* (/ height 2) (sin (+ start-angle unit-angle)))));zuoxia
      (image-draw-line! image  
                         (+ central-col (* (/ width 2) (cos (+ start-angle unit-angle))));youxia
                         (+ central-row (* (/ height 2) (sin (+ start-angle unit-angle))));youxia
                         central-col ;col1 row1 col2 row2
                         (+ central-row (* (/ height 2))));zuoxia
    )))



;;;Procedure:draw-magic-circle3
;;;Parameter:num, an positive integer between 1~999
;;;          width, a positive number, width of the image
;;;          height, a positive number, height of the image
;;;Purpose: to draw the first magic circle, 
;;;         hundres digit determines the number of squares on the angles of the triangle
;;;         tens digit determines the number of neg-triangles
;;;         unit digit determines the number of pos-triangles
;;;Produce:magic-circle3, an image
(define draw-magic-circle3
  (lambda (num width height)
    (let* ([nums (split-num num)]
          [fnum (car nums)]
          [snum (cadr nums)]
          [tnum (caddr nums)]
          [image (image-new width height)])
      
      (context-set-fgcolor! "black")
      (image-select-all! image)
      (image-fill-selection! image)
      (image-select-nothing! image)
      (draw-bg image 9662683)
      (draw-pos-triangle image width height)
      (context-set-fgcolor! "white")
      (for-each image-blot! (make-list 25 image)
                            (map ceiling (map (l-s * (/ width 25)) star-col2))
                            (map ceiling (map (l-s * (/ height 25)) star-row2)))
      (for-each image-blot! (make-list 25 image)
                            (map ceiling (map (l-s * (/ width 25)) star-col1))
                            (map ceiling (map (l-s * (/ height 25)) star-row1)))
      (context-set-fgcolor! 16448210)
      (context-set-brush! "Nova")
      (context-set-fgcolor! "mediumpurple")
      (image-select-ellipse! image REPLACE 
            (* 0.1 width) (* 0.1 height) (* 0.8 width) (* 0.8 height))
      (context-set-fgcolor! "mediumpurple")
      (image-stroke-selection! image)

      (image-select-ellipse! image REPLACE 
            (* 0.11 width) (* 0.11 height) (* 0.78 width) (* 0.78 height))

      (image-stroke-selection! image)
      (image-select-ellipse! image REPLACE 
            (* 0.12 width) (* 0.12 height) (* 0.76 width) (* 0.76 height))

      (image-stroke-selection! image)
      (image-select-ellipse! image REPLACE 
            (* 0.13 width) (* 0.13 height) (* 0.74 width) (* 0.74 height))
      (image-stroke-selection! image)
      (image-select-ellipse! image REPLACE 
            (* 0.14 width) (* 0.14 height) (* 0.72 width) (* 0.72 height))
      (image-stroke-selection! image)
      (image-select-ellipse! image REPLACE 
            (* 0.15 width) (* 0.15 height) (* 0.7 width) (* 0.7 height))
      (image-stroke-selection! image)
      (image-select-nothing! image)
      (context-set-brush! "Nova")
      (context-set-fgcolor! "mediumpurple")
      (context-set-fgcolor! "purple")
      (cond [(not (= 0 tnum))
      (for-each draw-pos-triangle (make-list tnum image) (map  (l-s * width) (map (r-s / 10) (map increment (iota tnum))))
                                                 (map  (l-s * height) (map (r-s / 10) (map increment (iota tnum)))))])
      (cond [(not (= 0 snum))
      (for-each draw-neg-triangle (make-list snum image) (map  (l-s * width) (map (r-s / 10) (map increment (iota snum))))
                                                 (map  (l-s * height) (map (r-s / 10) (map increment (iota snum)))))])
     
      (draw-pos-triangle image (* 0.9 width) (* 0.9 height))
      (draw-neg-triangle image (* 0.9 width) (* 0.9 height))
      (cond [(not (= 0 fnum))
      (draw-central-circles image fnum (* 0.45 width) 0 (* 0.55 width) (* 0.1 height))
      (draw-central-circles image fnum (* 0.85 width) (* 0.68 height)(* 0.95 width) (* 0.78 height))
      (draw-central-circles image fnum (* 0.05 width) (* 0.68 height)(* 0.15 width) (* 0.78 height))])
      (image-refresh-display! image)
      (image-show image)
      )))



;;;;;;;;;;;;;;;;;;;;;;;;magic 4;;;;;;;;;;;;;;;;;;;;;;


;;;Procedure:draw-pentagon
;;;Parameter:image, an image
;;;          start-col, a positive numebr, the x coordinate of the first point of the pentagon
;;;          start-row, a positive numebr,the y coordinate of the first point of the pentagon
;;;          R, radius fo the ellipse that contains the tirangle
;;;          ratio, a number 
;;;          draw-or-not, a boolean value
;;;          start-angle, the start angle
;;;Purpose:draw a pentagon an ellipse with R radius that contains the tirangle if draw-or-not is #t
;;;        returns the position of five points of the pentagon if draw-or-not is #f
;;;Produce:draw-pentagon, an image
(define draw-pentagon
  (lambda (image start-col start-row R ratio draw-or-not start-angle)
    (let* ([in (/ (* 2 pi) 5)]
           [create-col-point (map (lambda (n)(+ start-col (* R (cos (+ start-angle (* n in)))))) (iota 5))]
           [create-row-point (map (lambda (n)(- start-row (* R (sin (+ start-angle (* n in)))))) (iota 5))]
           [variant-col (lambda (n) (+ (* start-col (- 1 ratio)) (* n ratio)))]
           [create-point (lambda (n)
                           (cons (+ start-col (* R (cos (+ start-angle (* n in)))))
                                 (- start-row (* R (sin (+ start-angle (* n in)))))))]
           [create-points (map create-point (iota 5))]
           [my-col-point (map variant-col create-col-point)]
           [my-points (let kernel ([i 0]
                                   [cur-points '()]
                                       )
                        (cond [(>= i 5)
                               (reverse cur-points)]
                            [else
                             (kernel (+ i 1)
                              (cons (cons (+ (* start-col (- 1 ratio)) (* (car (list-ref create-points i)) ratio)) 
                                           (cdr (list-ref create-points i)))
                                  cur-points))]))])
           (cond [(equal? draw-or-not #t)
                  (image-select-polygon! image REPLACE my-points)
                   (image-stroke-selection! image)
                    (image-select-nothing! image)]
                 [else (list my-col-point create-row-point)]))))


;;;Procedure:draw-magic-circle4
;;;Parameter:num, an positive integer between 1~999
;;;          width, a positive number, width of the image
;;;          height, a positive number, height of the image
;;;Purpose: to draw the first magic circle, 
;;;         hundres digit determines the number of roman number showed
;;;         tens digit determines the number of the circles on each of angle of outside pentagon
;;;         unit digit determines the number of the circles in the center
;;;Produce:magic-circle4, an image
(define draw-magic-circle4
  (lambda (num width height)
   (let* ([nums (split-num num)]
          [fnum (car nums)]
          [snum (cadr nums)]
          [tnum (caddr nums)]
          [image (image-new width height)]
          [tommy (turtle-new image)]
          [HR (* 0.8 width)]
          [VR (* 0.8 height)]
          [ratio (/ width height)]
          [c-col (/ width 2)]
          [c-row (/ height 2)]
          [fgcolor (generate-color fnum)]
          [in* (/ (* (+ fnum 1) pi) (+ fnum 3))]
          [inner-pentagon-col (car (draw-pentagon image c-col c-row (/ (* (* 0.4 width) (sin (*(/ 54 180) pi))) 2) ratio #f (/ pi 3)))]
          [inner-pentagon-row (cadr (draw-pentagon image c-col c-row (/ (* (* 0.4 width) (sin (*(/ 54 180) pi))) 2) ratio #f (/ pi 3)))]
          [outer-pentagon-col (car (draw-pentagon image c-col c-row (* 0.4 width) ratio #f (/ pi 2)))]
          [outer-pentagon-row (cadr (draw-pentagon image c-col c-row (* 0.4 width) ratio #f (/ pi 2)))]
          )
     
      (draw-bg image fgcolor)
      
      
      (context-set-fgcolor! (irgb-lighter (irgb-lighter fgcolor)))
      (context-set-brush! "Star Brush #2")
      (for-each image-blot! (make-list 25 image)
                            (map ceiling (map (l-s * (/ width 25)) star-col1))
                            (map ceiling (map (l-s * (/ height 25)) star-row1)))
      
      (context-set-fgcolor! "white")
      (for-each image-blot! (make-list 25 image)
                            (map ceiling (map (l-s * (/ width 25)) star-col2))
                            (map ceiling (map (l-s * (/ height 25)) star-row2)))
      
     
      (context-set-fgcolor! "black")
      (context-set-brush! "2. Block 01")
     
      (context-set-fgcolor! fgcolor)
     
     
      (context-set-brush! "Nova")
      (image-select-ellipse! image REPLACE (* 0.1 width) (* 0.1 height) HR VR)
      (image-stroke-selection! image)
      (image-select-nothing! image)
     
      (context-set-brush! "Nova")
      (draw-pentagon image c-col c-row (* 0.4 width) ratio #t (/ pi 2))
      (draw-pentagon image c-col c-row (* (* 0.4 width) (sin (*(/ 54 180) pi))) ratio #t (/ (* 3 pi) 2))
      (for-each image-draw-line! (make-list 5 image)
                        inner-pentagon-col inner-pentagon-row 
                        (append (list (list-ref outer-pentagon-col 3) (list-ref outer-pentagon-col 4)) (list-take outer-pentagon-col 3))
                        (append (list (list-ref outer-pentagon-row 3) (list-ref outer-pentagon-row 4))(list-take outer-pentagon-row 3)))
     (for-each draw-central-circles (make-list 5 image) (make-list 5 2)
                                    (map (r-s - (* width 0.03)) inner-pentagon-col)
                                    (map (r-s - (* height 0.03)) inner-pentagon-row)
                                    (map (r-s + (* width 0.03)) inner-pentagon-col)
                                    (map (r-s + (* height 0.03))inner-pentagon-row))
         (cond [(not (= 0 snum))
           (for-each draw-central-circles (make-list 5 image) (make-list 5 snum)
                                    (map (r-s - (* width 0.06)) outer-pentagon-col)
                                    (map (r-s - (* height 0.06)) outer-pentagon-row)
                                    (map (r-s + (* width 0.06)) outer-pentagon-col)
                                    (map (r-s + (* height 0.06)) outer-pentagon-row))])
      (cond [(not (= 0 tnum))
       (draw-central-circles image tnum (- c-col (* 0.1 width))
                                        (- c-row (* 0.1 height))
                                        (+ c-col (* 0.1 width))
                                        (+ c-row (* 0.1 height)))])
      (roman-num image (* 0.5 width) (* 0.9 height) (* 0.03 (+ width height)) ratio fnum)
      (image-refresh-display! image)
      (image-show image))))



;;;;;;;;;;;;;;;;;;;;;;;magic 5;;;;;;;;;;;;;;;;;;;;;;

;;;Procedure:draw-poly
;;;Parameter:image, an image
;;;          h, an integer, the x coordinate of the central
;;;          k. an integer, the y coordinate of the central
;;;          R, a positive number, the radius of the circle which contain the squares
;;;          m, a positive integer between 0 and 9
;;;          a, a positive number, the start angle
;;;          ratio, a positive numebr, image's width/height       
;;;Purpose: draw-polyons, with m+3 edges and a certain pattern
;;;Produce:nothing
(define draw-poly
  (lambda (image h k R m a ratio)
    (let* ([in (/ (* 2 pi) m)]
           [create-point (lambda (n)
                           (cons (+ h (* R (cos (+ (/ pi 2) (* n in)))))
                                 (- k (* R (sin (+ (/ pi 2) (* n in)))))))]
           [create-points (map create-point (iota m))]
           [my-points (let kernel ([i 0]
                                   [cur-points '()])
                        (cond [(>= i m)
                               (reverse cur-points)]
                              [else
                               (kernel (+ i 1)
                                       (cons (cons (+ (* h (- 1 ratio)) (* (car (list-ref create-points i)) ratio)) 
                                                   (cdr (list-ref create-points i)))
                                             cur-points))]))]
           
           [in* (/ (* (- m 2) pi) m)]
           [r (* 2 R (sin (/ pi m)))]
           [s (* r (/ (sin (- in* a)) (sin (- pi in*))))]
           [create-point* (lambda (n)
                            (cons (+ (car (list-ref create-points n)) 
                                     (* s (cos (+ (* 1.5 pi) (- (/ in* 2)) a (* n (- pi in*))))))
                                  (- (cdr (list-ref create-points n))
                                     (* s (sin (+ (* 1.5 pi) (- (/ in* 2)) a (* n (- pi in*))))))))]
           [create-points* (map create-point* (iota m))]
           [my-points* (let kernel ([i 0]
                                    [cur-points '()]
                                    )
                         (cond [(>= i m)
                                (reverse cur-points)]
                               [else
                                (kernel (+ i 1)
                                        (cons (cons (+ (* h (- 1 ratio)) (* (car (list-ref create-points* i)) ratio)) 
                                                    (cdr (list-ref create-points* i)))
                                              cur-points
                                              ))]))])
      (image-select-polygon! image REPLACE my-points)
      (image-stroke-selection! image)
      (image-select-nothing! image)
      (let kernel2 ([i 0])
        (when (< i m)
          (image-draw-line! image (car (list-ref my-points i))
                            (cdr (list-ref my-points i))
                            (car (list-ref my-points* i))
                            (cdr (list-ref my-points* i)))
          (kernel2 (+ i 1))))
      (let kernel3 ([i 0])
        (when (< i m)
          (image-draw-line! image (car (list-ref my-points i))
                            (cdr (list-ref my-points i))
                            (/ (+ (car (list-ref my-points* (remainder (+ i 1) m)))
                                  (car (list-ref my-points (remainder (+ i 1) m)))) 2)
                            (/ (+ (cdr (list-ref my-points* (remainder (+ i 1) m)))
                                  (cdr (list-ref my-points (remainder (+ i 1) m)))) 2))
          (kernel3 (+ i 1))))
      (let kernel4 ([i 0])
        (when (< i m)
          (image-draw-line! image (car (list-ref my-points i))
                            (cdr (list-ref my-points i))
                            (/ (+ (/ (+ (car (list-ref my-points* i))
                                        (car (list-ref my-points i))) 2)
                                  (car (list-ref my-points (mod (- i 1) m)))) 2)
                            (/ (+ (/ (+ (cdr (list-ref my-points* i))
                                        (cdr (list-ref my-points i))) 2)
                                  (cdr (list-ref my-points (mod (- i 1) m)))) 2))
          (kernel4 (+ i 1))))
      (let kernel5 ([i 0])
        (when (< i m)
          (image-draw-line! image (/ (+ (/ (+ (car (list-ref my-points* i))
                                              (car (list-ref my-points i))) 2)
                                        (car (list-ref my-points (mod (- i 1) m)))) 2)
                            (/ (+ (/ (+ (cdr (list-ref my-points* i))
                                        (cdr (list-ref my-points i))) 2)
                                  (cdr (list-ref my-points (mod (- i 1) m)))) 2)
                            (/ (+ (car (list-ref my-points i)) 
                                  (car (list-ref my-points (mod (- i 1) m)))) 2)
                            (/ (+ (cdr (list-ref my-points i)) 
                                  (cdr (list-ref my-points (mod (- i 1) m)))) 2))
          (kernel5 (+ i 1)))))))



;;;Procedure:draw-magic-circle5
;;;Parameter:num, an positive integer between 1~999
;;;          width, a positive number, width of the image
;;;          height, a positive number, height of the image
;;;Purpose: to draw the first magic circle, 
;;;         hundres digit determines the number of edges of the polygon (N+3)
;;;         tens digit determines the number of the roman number showed at the top of the magic circle
;;;         unit digit determines the number of the roman number showed as the backgroud of the magic circle
;;;Produce:magic-circle5, an image
(define draw-magic-circle5
  (lambda (num width height)
   (let* ([nums (split-num num)]
          [fnum (car nums)]
          [snum (cadr nums)]
          [tnum (caddr nums)]
          [image (image-new width height)]
          [tommy (turtle-new image)]
          [HR (* 0.8 width)]
          [VR (* 0.8 height)]
          [ratio (/ width height)]
          [c-col (/ width 2)]
          [c-row (/ height 2)]
          [fgcolor (generate-color fnum)]
          [in* (/ (* (+ fnum 1) pi) (+ fnum 3))]
          )
     
      (draw-bg image fgcolor)
      
      
      (context-set-fgcolor! (irgb-lighter (irgb-lighter fgcolor)))
      (context-set-brush! "Star Brush #2")
      (for-each image-blot! (make-list 25 image)
                            (map ceiling (map (l-s * (/ width 25)) star-col1))
                            (map ceiling (map (l-s * (/ height 25)) star-row1)))
      
      (context-set-fgcolor! "white")
      (for-each image-blot! (make-list 25 image)
                            (map ceiling (map (l-s * (/ width 25)) star-col2))
                            (map ceiling (map (l-s * (/ height 25)) star-row2)))
      
     
      (context-set-fgcolor! "black")
      (context-set-brush! "2. Block 01")
      (roman-num image c-col c-row  (/ VR 2) ratio tnum)
     
      (context-set-fgcolor! fgcolor)
     
     
      (context-set-brush! "Nova")
      (image-select-ellipse! image REPLACE (* 0.1 width) (* 0.1 height) HR VR)
      (image-stroke-selection! image)
      (image-select-nothing! image)
     
      
      (context-set-brush! "GIMP Brush #1")
      (image-select-ellipse! image REPLACE (* 0.14 width) (* 0.14 height) (* HR 0.9) (* VR 0.9))
      (image-stroke-selection! image)
      (image-select-nothing! image)
      
      (context-set-brush! "Nova")
      (draw-poly image c-col c-row (/ (* 0.7 height) 2) (+ fnum 3) (* snum (/ in* 20)) ratio)
      (roman-num image c-col (- c-row (/ VR 2)) (/ VR 20) ratio snum)
     
     
      (image-refresh-display! image)
      (image-show image))))



