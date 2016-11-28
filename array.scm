(use coops blas coops-primitive-objects)
;;; S: single 32
;;; D: double 64
;;; C: complex (pair of single)
;;; Z: complex (pair of double)

(define-class <array> ()
  [(dim accessor: dim)
   (data accessor: data)])

(define-method (initialize-instance (a <array>))
  (call-next-method)
  (set! (dim a) (calc-dim (data a))))

(define (calc-dim lst)
  (define (inner lst dim-acc)
    (cond [(atom? lst) (reverse! dim-acc)]
          [else (inner (car lst)
                       (cons (length lst) dim-acc))]))
  (inner lst '()))
;;; dim check
;;; maybe slow
(define (dim-same-dim-check lst)
  (cond [(atom? lst) #t]
        [(atom? (car lst))
         (every atom? lst)]
        [(and (every list? lst) (apply = (map length lst)))
         (every dim-same-dim-check lst)]
        [else #f]))

;; (define-method (element-wise-apply (fn #t) (a1 <array>) (a2 <array>))
;;   )

