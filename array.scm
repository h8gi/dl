(use coops blas srfi-4-utils)
;;; S: single 32
;;; D: double 64
;;; C: complex (pair of single)
;;; Z: complex (pair of double)
(define-class <array> ()
  ([raw-list    accessor: raw-list]     ; only for initialize
   [dim		accessor: dim]
   [len         accessor: len]
   [data	accessor: data]      ; f32 vector
   [safe?	accessor: safe?
		initform: #t]))

(define-class <matrix> (<array>)
  ([row         accessor: row]
   [col         accessor: col]))

(define-class <vector> (<array>))

(define-method (init-dim! (a <array>))
  (define (inner lst dim-acc)
    (cond [(atom? lst) (reverse! dim-acc)]
          [else (inner (car lst)
                       (cons (length lst) dim-acc))]))
  (set! (dim a) (inner (raw-list a) '())))
;;; dim check
;;; maybe slow
;; (define (dim-same-dim-check lst)
;;   (cond [(atom? lst) #t]
;;         [(atom? (car lst))
;;          (every atom? lst)]
;;         [(and (every list? lst) (apply = (map length lst)))
;;          (every dim-same-dim-check lst)]
;;         [else #f]))


(define-method (initialize-instance (a <array>))
  (call-next-method)
  (init-dim! a)
  (set! (len a) (f32vector-length (data a))))

(define-method (initialize-instance (m <matrix>))
  (call-next-method)
  (set! (row m) (first  (dim m)))
  (set! (col m) (second (dim m))))

;;; initializer -> <array>
(define (array lst)
  (let ([data (list->f32vector (flatten lst))] ; RowMajor
        [dm   (calc-dim lst)])
    (make <array> 'data data 'dim dm)))

(define-method (array-map (fn #t) (a <array>) #!rest rest)
  (make <array> 
    'data (apply f32vector-map fn (data a) (map data rest))
    'dim  (dim a)))

(define-method (->list (a <array>))
  (define (inner lst dm)
    (cond [(null? dm) lst]
          [else (inner (chop lst (car dm)) (cdr dm))]))
  (let ([lst (f32vector->list (data a))]
        [dm (reverse (cdr (dim a)))])
    (inner lst dm)))

(define a
  (array '(((1   2)   (2   3)   (3   4))
           ((10  20)  (20  30)  (30  40))
           ((100 200) (200 300) (300 400)))))

;;; BLAS level 1
(define-method (dot (x <array>) (y <array>))
  (sdot (car (dim x))
        (data x)
        (data y)))


