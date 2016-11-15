#lang racket
(require gigls/unsafe)


(define split-num 
  (lambda (num)
    (list (quotient num 100)
          (remainder (quotient num 10) 10)
          (remainder num 10))
    )
  )


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
                               (image-select-nothing! image)
                               )])
      (for-each draw-one-circle! widths heights)
      )
    
    )
  
  )


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
                               (image-select-nothing! image)
                               )])
      (for-each draw-one-circle! widths heights)
      )
    
    )
  
  )


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
             (turtle-forward! turtle (- col2 (turtle-col turtle)) )
             ]
            [(and (< col2 col1) (> row2 row1))
             (turtle-face! turtle 135)
             (turtle-forward! turtle (* (sqrt 2) (abs(- row1 row2))) )
             (turtle-face! turtle -180)
             (turtle-forward! turtle  (- (turtle-col turtle) col2) )
             ]
            [(and (> col2 col1) (< row2 row1))
             (turtle-face! turtle -45)
             (turtle-forward! turtle (* (sqrt 2) (abs(- col1 col2))))
             (turtle-face! turtle 0)
             (turtle-forward! turtle  (- col2 (turtle-col turtle)))
             ]
            [(and (< col2 col1) (< row2 row1))
             (turtle-face! turtle -135)
             (turtle-forward! turtle (* (sqrt 2) (abs(- row1 row2))) )
             (turtle-face! turtle -90)
             (turtle-forward! turtle  (- (turtle-col turtle) col2) )
             ]
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
            )
      
       
      )
    
  ))



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
                         (* (irgb-blue bgcolor2) (+ 1 (sin (/ (sqrt (+ (sqr (- x center-col)) (sqr (- y center-row)))) (/ a 3))))))
                   
                   ]
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
                               (irgb-blue bgcolor))))
                   
                   ]
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
                               (- (irgb-blue bgcolor) (irgb-blue innercolor)))))
                   
                   ]
                  [else innercolor]
                  
                  ))
          (image-width image) (image-height image)
          )]
         
         )
         (image-copy-paste-block! bg 0 0 image 0 0 (image-width image) (image-height image))
         )
    
  ))

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
           (irgb 192 255 203)])
  
  ))

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
                  (- 8 (* (/ 8 25) (sqr n))))]
           )
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
           (map (l-s + 0.25) (map (l-s * 0.8) (iota 10)))]))
    
    )
  )

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
                  (- 9 (* (/ 9 25) (sqr n))))]
           )
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
           (map (l-s + 0.5) (map (l-s * 0.9) (iota 10)))]))
    
    )
  )

(define 25-num
  (lambda ()
    (let kernel ([list-so-far '()]
                 [n 25])
      (cond [(< n 1)
          list-so-far]
          [else (kernel (cons (random 25) list-so-far)
          (- n 1))])
    
    ))
  )

(define star-col1 (25-num))
(define star-row1 (25-num))
(define star-col2 (25-num))
(define star-row2 (25-num))



(define draw-general
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
          
          [fgcolor (generate-color fnum)]
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
                               (make-list 9 (/ height 10))
                               )
      (for-each draw-line-same (make-list 9 tommy)
                               (map (l-s list-ref s-widths) (list 0 2 4 6 8 7 5 3 1))
                               (map (l-s list-ref s-heights) (list 0 2 4 6 8 7 5 3 1))
                               (map (l-s list-ref s-widths) (list 2 4 6 8 7 5 3 1 0))
                               (map (l-s list-ref s-heights) (list 2 4 6 8 7 5 3 1 0))
                               (make-list 9 (/ width 20))
                               (make-list 9 (/ height 20))
                               (make-list 9 (/ width 20))
                               (make-list 9 (/ height 20))
                               )
      (for-each draw-line-same (make-list 5 tommy)
                               (map (l-s list-ref widths) (list 3 4 5 6 7))
                               (map (l-s list-ref heights) (list 3 4 5 6 7))
                               (map (l-s list-ref s-widths) (list 1 2 5 8 9))
                               (map (l-s list-ref s-heights) (list 1 2 5 8 9))
                               (make-list 5 (/ width 10))
                               (make-list 5 (/ height 10))
                               (make-list 5 (/ width 20))
                               (make-list 5 (/ height 20))
                               )
      
      (image-refresh-display! image)
      (image-show image)
      )
    
    
    )
  )