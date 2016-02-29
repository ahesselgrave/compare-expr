;; returns a list for diff output between TCP and UDP
(define (if-TCP-template tcp udp)
  (cons 'if (cons 'TCP (cons tcp (cons udp '())))))

;; compares two things as if they are constants
;; other than the first equal?, it does not recursively check between x and y
;; used as a fallback if we can't match it to a list or special form
(define (compare-constants x y)
  (if (equal? x y)
      x
      (if (and (eq? x #t) (eq? y #f))
	  'TCP
	  (if (and (eq? x #f) (eq? y #t))
	      '(not TCP)
	      (if-TCP-template x y)))))

;; compares lists one element at a time
;; only works if x and y are same length
(define (compare-lists x y)
  (if (equal? x '())
      '()
      (if (equal? y '())
	  '()
	  (cons (compare-expr (car x) (car y)) (compare-lists (cdr x) (cdr y))))))

;; returns true if either x or y is a list of if, quote, lambda, let
(define (is-special-forms x y)
  (if (or (equal? (car x) 'if) (equal? (car y) 'if))
      #t
      (if (or (equal? (car x) 'quote) (equal? (car y) 'quote))
	  #t
	  (if (or (equal? (car x) 'lambda) (equal? (car y) 'lambda))
	      #t
	      (if (or (equal? (car x) 'let) (equal? (car y) 'let))
		  #t
		  #f)))))

;; quote is simple and treats the datum as a constant
(define (compare-quotes x y)
  (compare-constants x y))

;; for lambda, if the formals are the same we treat the body as a list
;; otherwise, the whole lambda expression is a constant
(define (compare-lambda x y)
  (if (equal? (car (cdr x)) (car (cdr y)))
      (compare-lists x y)
      (compare-constants x y)))

;; checks to see if two lets bind the same variables
;; values don't matter as they'll be checked elsewhere
(define (same-let-binding x y)
  (if (and (equal? x '()) (equal? y '()))
      #t
      (if (equal? (car (car x)) (car (car y)))
	  (same-let-binding (cdr x) (cdr y))
	  #f)))
	   

;; similar to lambda. if binding is same, compare as a list.
;; otherwise we treat the whole expression as a constant
(define (compare-let x y)
  (if (same-let-binding (car (cdr x)) (car (cdr y)))
      (compare-lists x y)
      (compare-constants x y)))



;; uses compare-constants to compare x and y unless they are lists of the same length
;; we also check for special cases for special forms that we define above
;; all it does it check when to apply compare-constants or compare-lists
(define (compare-expr x y)
  (if (and (list? x) (list? y))
      (if (eq? (length x) (length y))
	  (if (eq? (car x) (car y))
	      (case (car x)
		('quote (compare-quotes x y))
		('lambda (compare-lambda x y))
		('let (compare-let x y))
		(else (compare-lists x y)))
	      (if (is-special-forms x y)
		  (compare-constants x y)
		  (compare-lists x y)))
	  (compare-constants x y))
      (compare-constants x y)))

;; creates a let binding for TCP and the diff expression of compare-expr
;; assigns TCP to the value of b
(define (eval-expr x b)
  (if b
      (cons 'let (cons '((TCP #t)) (cons x '())))
      (cons 'let (cons '((TCP #f)) (cons x '())))))
  

;; compares the evals of both sides using the raw expression and the diff expression
;; returns #t if both sides pass, #f otherwise
(define (test-compare-expr x y)
  (let ((diff (compare-expr x y)))
    (if (and (equal? (eval (eval-expr diff #t)) (eval x))
	     (equal? (eval (eval-expr diff #f)) (eval y)))
	#t
	#f)))

;; test-x and test-y will test the following comparisons:
;; (1) #t/#f differences being (TCP) or (not TCP) appropriately
;; (2) lists (duh)
;; (3) lets
;; (4) lambdas with same binding
;; (5) lambdas with different binding
;; (6) if/3
;; The test cases will have footnote comments corresponding to the above
(define test-x
  '(if #t ;(1)
       (let ((a 1) (b 2)) ;(3)
	 ((lambda (a b) (cons (+ a b) (- a b))) a b)) ;(2)(4)
       (let ((c 3) (d 4)) ;(3)
	 ((lambda (c d) (list c d)) c d))))

(define test-y
  '(if #f
       (let ((a 1) (b 2))
	 ((lambda (a b) (list (- a b) (+ a b))) a b))
       (let ((c 3) (d 4))
	 ((lambda (d c) (cons c d)) c d))))


;; the ultimate test case! should return #t
(test-compare-expr test-x test-y)
