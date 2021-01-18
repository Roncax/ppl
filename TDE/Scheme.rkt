====2020.09.03====
Scheme, Ex 1 (4 points)
Define a construct variant of call/cc, called call/cc-store, with syntax:

(call/cc-store (k in v) e1 ...)

where k is the current continuation, v is a visible variable in the current
scope, and e1 ... is the body of the construct. The semantics is the same of the
usual call/cc (with a simplified syntax, not requiring a lambda), but the
current continuation must also be stored in v, before executing the body.

(define-syntax call/cc-store
	(syntax rule ()
		((_ (k in v) e1 ...)
			((call/cc 
				(λ (k)
					(set! v k)
					(e1 ...)))))))
					
					
Scheme, Ex 2 (5 points)

Define a pure, tail-recursive function, with O(n) complexity, that, given a list
(e1 e2 ... en), n > 0, returns (en ... e2 e1 e1 e2 ... en). You cannot use
folds, named lets, and reverse.

MY VERSION (?)
(define  (rev-list L1 Lrev Lsav)
	(cond [(= (length L1) 0) (let! Ldef (cons Lrev Lsav))]
		  [else (rev-list (cons (car L1) Lrev) (cdr L1) Lsav)]))

(define (solution L)
	(rev-list L [] L))

SOLUTION (decisamente meglio)
(define (hh-tr L)
  (define (help L out)
    (if (null? L)
        out
        (help (cdr L) (cons (car L) out))))
  (help L L))



====2020.07.17====
Define the verbose construct for folding illustrated by the following
example:

(cobol-fold direction -> from 1 data 1 2 3 4 5 6
            (exec
             (displayln y)
             (+ x y))
             using x y)        
                               
This is a fold-right (->) with initial value 1 on the list (1 2 3 4 5
6), and the fold function is given in the "exec" part.  Of course, <-
is used to select fold-left instead of right.

SOLUTION  
(define-syntax cobol-fold
	(sytax-rules(direction -> <- data using from exec)
		((direction -> from i data d ... (exec e ...) using x y)
			(foldr (lambda (x y) e ...) i '(d ...)))
		((direction <- from i data d ... (exec e ...) using x y)
			(foldl (lambda (x y) e ...) i '(d ...)))
			))



====2020.06.29====
Define the construct define-with-types, that is used to define a procedure 
with type constraints, both for the parameters and for the return value. 
The type constraints are the corresponding type predicates, e.g. number? to check
if a value is a number.
If the type constraints are violated, an error should be issued.

E.g.
(define-with-types (add-to-char : integer? (x : integer?) (y : char?))
  (+ x (char->integer y)))
defines a procedure called add-to-char, which takes an integer and a character, 
and returns an integer.


SOLUTION

(define-syntax define-with-types
  (syntax-rules (:)
    ((_ (f : tf (x1 : t1) ...) e1 ...)
     (define (f x1 ...)
       (if (and (t1 x1) ...)
           (let ((res (begin
                        e1 ...)))
             (if (tf res)
                 res
                 (error "bad return type")))
           (error "bad input types"))))))



====2020.02.07====
Implement this new construct: (each-until var in list until pred : body), 
where keywords are written in
boldface. It works like a for-each with variable var, but it can end 
before finishing all the elements of list
when the predicate pred on var becomes true.

E.g.
(each-until x in '(1 2 3 4)
	until (> x 3) :
	(display (* x 3))
	(display " "))
shows on the screen: 3 6 9

SOLUTION
(define-syntax each-until
 (syntax-rules (in until :)
  ((_ x in L until pred : body ...)
   (let loop ((xs L))
    (unless (null? xs)
     (let ((x (car xs)))
      (unless pred
       (begin
        body ...
         (loop (cdr xs))))))))))



====2020.01.15====
Consider the Foldable and Applicative type classes in Haskell. We want to implement something
analogous in Scheme for vectors. 
Note: you can use the following library functions in your code: vectormap, vector-append.

1) Define vector-foldl and vector-foldr.
2) Define vector-pure and vector-<*>

SOLUTION
(define (vector-foldr f i v)
	(let loop ((cur (- (vector-length v) 1))
		(out i))
	(if (< cur 0)
		out
		(loop (- cur 1) (f (vector-ref v cur) out)))))

(define (vector-foldl f i v)
	(let loop ((cur 0)
		(out i))
	(if (>= cur (vector-length v))
		out
		(loop (+ cur 1) (f (vector-ref v cur) out)))))


(define vector-pure vector)

(define (vector-concatmap f v)
	(vector-foldr vector-append #()(vector-map f v)))

(define (vector-<*> fs xs)
	(vector-concatmap (lambda (f) (vector-map f xs)) fs)) 



====2019.09.03====
Consider the following code:

(define (a-function lst sep)
	(foldl (lambda (el next)
		(if (eq? el sep)
			(cons '() next)
			(cons (cons el (car next))
		(cdr next))))
	 (list '()) lst))
	 
1) Describe what this function does; what is the result of the following call?
(a-function '(1 2 nop 3 4 nop 5 6 7 nop nop 9 9 9) 'nop)
2) Modify a-function so that in the example call the symbols nop are not discarded from the resulting list,
which must also be reversed (of course, without using reverse)

SOLUTION
a-function returns a list of lists, where each list is taken backwards, 
and sep is used for a separator. 
The resulting list is: ((9 9 9) () (7 6 5) (4 3) (2 1))

(define (a-function lst sep)
	(foldr (lambda (el next)
		(if (eq? el sep)
			(cons (list sep) next)
			(cons (cons el (car next))
		(cdr next))))
	 (list '()) lst))
	 


====2019.07.24====
Write a functional, tail recursive implementation of a procedure that takes a list of numbers L and two values
x and y, and returns three lists: one containing all the elements that are less than both x and y, the second one
containing all the elements in the range [x,y], the third one with all the elements bigger than both x and y. It
is not possible to use the named let construct in the implementation.


SOLUTION (mia)
(define (tril L x y)
	(let (l1 (list `()))
		 (l2 (list `()))
		 (l3 (list `())))
	(foldr (lambda (el)
			(cond ((and (> el y) (> el x)) (cons l1 el))
				  ((and (< el y) (< el x)) (cons l3 el))
				  ((and (< el y) (> el x)) (cons l2 el))))
				  `() L))

VERA SOLUZIONE (maaaaah)
(define (3-part L v1 v2)
 (define (3-p L v1 v2 r1 r2 r3)
  (if (null? L)
   (list r1 r2 r3)
   (let ((x (car L))
     (xs (cdr L)))
    (cond
     ((and (< x v1)(< x v2))
      (3-p xs v1 v2 (cons x r1) r2 r3))
     ((and (>= x v1)(<= x v2))
      (3-p xs v1 v2 r1 (cons x r2) r3))
     ((and (> x v1)(> x v2))
      (3-p xs v1 v2 r1 r2 (cons x r3)))))))

(3-p L v1 v2 '() '() '()))


====2019.06.28====
Consider this data definition in Haskell: data Tree a = Leaf a | Branch (Tree a) a (Tree a)
Define an OO analogous of this data structure in Scheme using the technique of "closure as classes" as seen
in class, defining the map and print methods, so that:

(define t1 (Branch (Branch (Leaf 1) -1 (Leaf 2)) -2 (Leaf 3)))
((t1 'map (lambda (x) (+ x 1))) 'print)

should display: (Branch (Branch (Leaf 2) 0 (Leaf 3)) -1 (Leaf 4))

SOLUTION
(define (Branch t1 x t2)
 (define (print)
  (display "(Branch ")
  (t1 'print)
  (display " ")
  (display x)
  (display " ")
  (t2 'print)
  (display ")"))
 (define (map f)
  (Branch (t1 'map f) (f x) (t2 'map f)))
(lambda (message . args)
 (apply
  (case message
  ((print) print)
  ((map) map)
  (else (error "Unknown")))
 args)))

(define (Leaf x)
 (define (print)
  (display "(Leaf ")
  (display x)
  (display ")"))
(define (map f)
 (Leaf (f x)))
(lambda (message . args)
 (apply
  (case message
  ((print) print)
  ((map) map)
  (else (error "Unknown")))
 args)))



====2019.02.08====
Define a pure function multi-merge with a variable number of arguments (all of them must be ordered lists of
numbers), that returns an ordered list of all the elements passed. It is forbidden to use external sort functions.
E.g. when called like:
(multi-merge '(1 2 3 4 8) '(-1 5 6 7) '(0 3 8) '(9 10 12))
it returns: '(-1 0 1 2 3 3 4 5 6 7 8 8 9 10 12)


SOLUTION
(define (merge a1 a2)
	(cond 
		((null? a1) a2)
		((null? a2) a1)
		(else 
			(let ((x (car a1))
			(y (car a2)))
			(if (< x y)
				(cons x (merge (cdr a1) a2))
				(cons y (merge a1 (cdr a2))))))))

(define (multi-merge . data)
	(foldl merge '() data))


====2019.01.16====
Define a pure function f with a variable number of arguments, that, when called like (f x1 x2 .. xn), returns:
(xn (xn-1 ( .. (x1 (xn xn-1 .. x1))..). Function f must be defined using only fold operations for loops.

SOLUTION
(define (strg-fold.args)
	(foldl (lambda (x y)
			((list x y)))) (foldr (lambda (x y) 
											(cons y x)) `() args) args))



====2018.09.05====
Define in a purely functional way a procedure called revlt, which takes three lists, (x1 ... xL) (y1 ... yM) (z1 .. zN)
and returns the list of vectors: (#(xO yO zO) … #(x1 y1 z1)), where O ≥ 1 is the smallest among L, M, and N.
E.g. (revlt '(1 2 3) '(4 5 6 7) '(8 9 10)) is the list '(#(3 6 10) #(2 5 9) #(1 4 8)).

SOLUTION
(define (revlt x y z) 
	(define (revlt-help x y z L)
		(if (or (eq? (size x) 0) (eq? (size y) 0) (eq? (size z) 0)))
		(revlt-help (cdr x) (cdr y) (cdr z) (cons (#((car x) (car y) (car z)) L)))))


REAL SOLUTION
(define (revlt l1 l2 l3)
(let loop ((p1 l1)
(p2 l2)
(p3 l3)
(out '()))
(if (or (null? p1)(null? p2)(null? p3))
out
(let ((x1 (car p1))
(x2 (car p2))
(x3 (car p3)))
(loop (cdr p1) (cdr p2) (cdr p3)
(cons (vector x1 x2 x3) out))))))



