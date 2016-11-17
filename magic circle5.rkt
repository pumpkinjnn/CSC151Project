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
           (irgb 230 200 255)]
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



(define draw-poly
  (lambda (image h k R m a ratio)
    (let* ([in (/ (* 2 pi) m)]
           [create-point (lambda (n)
                           (cons (+ h (* R (cos (+ (/ pi 2) (* n in)))))
                                 (- k (* R (sin (+ (/ pi 2) (* n in)))))))]
           [create-points (map create-point (iota m))]
           [my-points (let kernel ([i 0]
                                   [cur-points '()]
                                       )
                        (cond [(>= i m)
                               (reverse cur-points)]
                            [else
                             (kernel (+ i 1)
                              (cons (cons (+ (* h (- 1 ratio)) (* (car (list-ref create-points i)) ratio)) 
                                           (cdr (list-ref create-points i)))
                                  cur-points
                                   ))]))]
                            
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
                                   ))]
                            )
                        )]
           )
      ;(display create-points*)
      ;(display my-points*)
    (image-select-polygon! image REPLACE my-points)
    (image-stroke-selection! image)
    (image-select-nothing! image)
    (let kernel2 ([i 0])
        (when (< i m)
          (image-draw-line! image (car (list-ref my-points i))
                                  (cdr (list-ref my-points i))
                                  (car (list-ref my-points* i))
                                  (cdr (list-ref my-points* i)))
          (kernel2 (+ i 1))
      ))
      (let kernel3 ([i 0])
        (when (< i m)
          (image-draw-line! image (car (list-ref my-points i))
                                  (cdr (list-ref my-points i))
                                  (/ (+ (car (list-ref my-points* (remainder (+ i 1) m)))
                                     (car (list-ref my-points (remainder (+ i 1) m)))) 2)
                                  (/ (+ (cdr (list-ref my-points* (remainder (+ i 1) m)))
                                     (cdr (list-ref my-points (remainder (+ i 1) m)))) 2))
          (kernel3 (+ i 1))
      ))
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
          (kernel4 (+ i 1))
      ))
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
          (kernel5 (+ i 1))
      ))
  
  
  )))


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
      
     
     
      (image-refresh-display! image)
      (image-show image)
     )
  ))



