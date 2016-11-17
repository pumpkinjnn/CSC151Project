#lang racket
(require gigls/unsafe)


(define split-num 
  (lambda (num)
    (list (quotient num 100)
          (remainder (quotient num 10) 10)
          (remainder num 10))
    )
  )


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
                               (image-select-nothing! image)
                               )])
      (for-each draw-one-circle! lefts tops widths heights)
      )
    
    )
  
  )


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
                                   cur-points 
                                   ))])
                        )]
           
           )
      (let kernel2 ([i 0])
        (when (< i num)
          (image-select-polygon! image REPLACE (list-ref my-points i))
          (image-stroke-selection! image)
          (image-select-nothing! image)
          (kernel2 (+ i 1))
          )
        )
      
      )
  ))


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
     ;(display all-points)
     (let kernel2 ([i 0])
        (when (< i 4)
          (image-select-polygon! image REPLACE (list-ref my-points i))
          (image-stroke-selection! image)
          (image-select-nothing! image)
          (kernel2 (+ i 1))
          )
        )
     
     )
  ))


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
      (image-show image)
     )
  ))



