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
           [b-dis (/ width 15)]
           [dis (map (l-s * b-dis) (list-take (list 0 1 6 7 11 12 8 9 3 4) (+ c-num 1)))]
           [widths (map (l-s - width) dis)]
           [heights (map (l-s - height) dis)]
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



(define draw-example1
  (lambda (num width height)
    (let* ([nums (split-num num)]
          [fnum (car nums)]
          [snum (cadr nums)]
          [tnum (caddr nums)]
          [image (image-new width height)]
          [shorter (min width height)]
          [unit-width (/ width 10)]
          [unit-height (/ height 10)]
          [widths (list-take (map (l-s * unit-width) (list 3 5 0 8 2 6 1 7 3 5)) (+ 1 snum))]
          [heights (list-take (map (l-s * unit-height)(list 1 1 2 2 3 3 5 5 7 7)) (+ 1 snum))]
          
          )
      (context-set-fgcolor! "black")
      (image-select-all! image)
      (image-fill-selection! image)
      (image-select-nothing! image)
      
      (context-set-fgcolor! "pink")
      (context-set-brush! "Nova")
      (for-each draw-big-circles (make-list (+ 1 snum) image)
                                   (make-list (+ 1 snum) tnum)
                                   widths
                                   heights)
      
      
      (image-refresh-display! image)
      (image-show image)
      )
    
    
    )
  )