(use coops blas srfi-4-utils)
;;; S: single 32
;;; D: double 64
;;; C: complex (pair of single)
;;; Z: complex (pair of double)
(define-class <array> ()
  ([shape	accessor: shape
		initform: #f]
   [len         accessor: len]
   [data	accessor: data]      ; f32 vector
   [safe?	accessor: safe?
		initform: #t]))

(define-class <matrix> (<array>))
(define-class <vector> (<array>))

(define-method (print-object (a <array>) (port #t))
  (with-output-to-port port
    (lambda ()
      (printf "~A ~A:~%"
              (class-name (class-of a))
              (shape a))
      (if (< (len a) 200)
          (pp (->list a))
          (display "large...")))))

(define-method (initialize-instance (a <array>))
  (call-next-method)
  (set! (len a) (f32vector-length (data a))))

(define-method (initialize-instance (v <vector>))
  (call-next-method)
  (set! (shape v) (list (len v))))

(define (calc-shape lst)
  (define (inner lst shape-acc)
    (cond [(atom? lst) (reverse! shape-acc)]
          [else (inner (car lst)
                       (cons (length lst) shape-acc))]))
  (inner lst '()))
;;; shape check
;;; maybe slow
(define (shape-check lst)
  (cond [(atom? lst) #t]
        [(atom? (car lst))
         (every atom? lst)]
        [(and (every list? lst) (apply = (map length lst)))
         (every shape-same-shape-check lst)]
        [else #f]))

(define (list->array lst #!optional (class <array>))
  (let ([arr (make class 'data (list->f32vector (flatten lst)))])
    (set! (shape arr)  (calc-shape lst))
    (set! (len arr)    (f32vector-length (data arr)))
    arr))

(define-method (->list (a <array>))
  (define (inner lst shp)
    (cond [(null? shp) lst]
          [else (inner (chop lst (car shp)) (cdr shp))]))
  (let ([lst (f32vector->list (data a))]
        [shp (reverse (cdr (shape a)))])
    (inner lst shp)))

(define-method (array-map (fn #t) (a <array>) #!rest rest)
  (make <array> 
    'data (apply f32vector-map fn (data a) (map data rest))
    'shape  (shape a)))

;;; deep copy
(define-method (copy (a <array>))
  (let ([newvect (make-f32vector (len a))])
    (f32vector-blit! (data a) 0 (len a) newvect 0)
    (make (class-of a)
      'data newvect
      'shape (list-copy (shape a)))))

(define-method (reshape (a <array>) shp)
  (assert (= (fold * 1 shp) (len a)) "Data length unmatched" shp (len a))
  (let ([ca (copy a)])
    (set! (shape ca) shp)
    ca))

(define-method (row (m <matrix>))
  (first  (shape m)))
(define-method (col (m <matrix>))
  (second (shape m)))

;;; initializer
(define (matrix lst nrow ncol)
  (make <matrix>
    'data (list->f32vector lst)
    'shape  (list nrow ncol)))

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
	  'shape (list c r))))

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
      'shape (list m n))))

;;; test data
(define a
  (list->array '(((1   2)   (2   3)   (3   4))
		 ((10  20)  (20  30)  (30  40))
		 ((100 200) (200 300) (300 400)))))

(define m
  (list->array '((1 2 3)
                 (4 5 6))
               <matrix>))

(define v
  (make <vector> 
    'data (f32vector 5 6 7)))
