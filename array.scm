(use coops blas srfi-4-utils)
;;; S: single 32
;;; D: double 64
;;; C: complex (pair of single)
;;; Z: complex (pair of double)
(define-class <array> ()
  ([dim		accessor: dim
		initform: #f]
   [len         accessor: len]
   [data	accessor: data]      ; f32 vector
   [safe?	accessor: safe?
		initform: #t]))

(define-class <matrix> (<array>))
(define-class <vector> (<array>))

(define-method (initialize-instance (a <array>))
  (call-next-method)
  (set! (len a) (f32vector-length (data a))))

(define-method (initialize-instance (v <vector>))
  (call-next-method)
  (set! (dim v) (list (len v))))

(define (calc-dim lst)
  (define (inner lst dim-acc)
    (cond [(atom? lst) (reverse! dim-acc)]
          [else (inner (car lst)
                       (cons (length lst) dim-acc))]))
  (inner lst '()))
;;; dim check
;;; maybe slow
(define (dim-check lst)
  (cond [(atom? lst) #t]
        [(atom? (car lst))
         (every atom? lst)]
        [(and (every list? lst) (apply = (map length lst)))
         (every dim-same-dim-check lst)]
        [else #f]))

(define (list->array lst)
  (let ([arr (make <array> 'data (list->f32vector (flatten lst)))])
    (set! (dim arr)  (calc-dim lst))
    (set! (len arr)  (f32vector-length (data arr)))
    arr))

(define-method (->list (a <array>))
  (define (inner lst dm)
    (cond [(null? dm) lst]
          [else (inner (chop lst (car dm)) (cdr dm))]))
  (let ([lst (f32vector->list (data a))]
        [dm (reverse (cdr (dim a)))])
    (inner lst dm)))

(define-method (array-map (fn #t) (a <array>) #!rest rest)
  (make <array> 
    'data (apply f32vector-map fn (data a) (map data rest))
    'dim  (dim a)))

(define-method (row (m <matrix>))
  (first  (dim m)))
(define-method (col (m <matrix>))
  (second (dim m)))

;;; initializer
(define (matrix lst nrow ncol)
  (make <matrix>
    'data (list->f32vector lst)
    'dim  (list nrow ncol)))

;;; transpose
(define-method (tr (m <matrix>))
  (let ([r (row m)]
	[c (col m)]
	[m-v  (data m)]
	[mt-v (make-f32vector (len m) 0)])
    (do ([i 0  (+ i 1)])
	[(>= i r) mt-v]
      (do ([j 0 (+ j 1)])
	  [(>= j c)]
	(let ([old (+ (* c i) j)]
	      [new (+ (* r j) i)])
	  (f32vector-set! mt-v new
			  (f32vector-ref m-v old)))))
    (make <matrix> 'data mt-v
	  'dim (list c r))))

;;; BLAS level 1
;;; norm
(define-method (norm (v <vector>))
  (snrm2 (len v) (data v)))

;;; vector dot
(define-method (dot (x <vector>) (y <vector>))
  (sdot (len  x)
        (data x)
        (data y)))
;;; k*a
(define-method (scalar-mul (k #t) (a <array>))
  (sscal (len a) k (data a)))

;;; BLAS level 2
;;; matrix * vector
(define-method (dot (m <matrix>) (v <vector>))
  (sgemv RowMajor NoTrans (row m) (col m) 1
	 (data m) (data v) 0
	 (make-f32vector (row m) 0)))

;;; BLAS level3
;;; matrix * matrix
(define-method (dot (a <matrix>) (b <matrix>))
  (let ([m (row a)]
	[n (col b)]
	[k (col a)])
   (make <matrix>
      'data (sgemm RowMajor NoTrans NoTrans
		   m n k
		   1 (data a) (data b) 0 (make-f32vector (* m n) 0))
      'dim (list m n))))

;;; test data
(define a
  (list->array '(((1   2)   (2   3)   (3   4))
		 ((10  20)  (20  30)  (30  40))
		 ((100 200) (200 300) (300 400)))))

(define m
  (make <matrix>
    'data (f32vector 1 2 3
		     4 5 6)
    'dim '(2 3)))

(define v
  (make <vector> 
    'data (f32vector 5 6 7)))

