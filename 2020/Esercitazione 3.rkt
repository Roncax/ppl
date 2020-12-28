#lang racket

;; A projection function
(define (proj-f n . rest)
  (lit-ref rest n))

;; Very slow if you call, e.g., (fibonacci 42)
(define (fibonacci n)
  (cond [(<= n 0) 0]
        [(= n 1) 1]
        [else (+ (fibonacci (- n 1)) (fibonacci (- n 2)))]))

;; A better way of making the projection function
(define-syntax proj
  (syntax-rules ()
    ((_ n v)
     v)
    ((_ n v1 vr ...)
     (if (= n 0)
         v1
         (proj (- n 1) vr ...)))))

;; Try both with: (proj 2 0 (+ 1 2) 3 (fibonacci 40))

;; Some fun with continuations
(define (right-now)
  (call/cc
   (λ (cont)
     (cont "Now"))))

(define (test-cc)
  (displayln (call/cc
            (λ (escape)
              (displayln "foo")
              (escape "bar")
              (displayln "baz")))))

;; A factorial generator
(define gen-fact #f)
(define (set-fact-gen)
  (let ((f 1) ;;variables to save 
        (n 1))
    (call/cc
     (λ (cont) 
       (set! gen-fact cont))) ;; così dopo posso chiamare solo gen-fact per avviare la continuazione
    (set! f (* f n)) ;; riprenderà da qua dopo la prima volta 
    (set! n (+ n 1))
    f))

;; We can make while loops with continuations
(define (make-while guard)
  (call/cc
   (λ (back-edge)
     (λ ()
       (when (guard)
         (back-edge (make-while guard)))))))
   
(define (count n)
  (let* ((i 0)
        (mywhile (make-while (λ () (< i n))))) ;; < i n è un guard di check per far partire o no la funzione
    (displayln i)
    (set! i (+ i 1))
    (mywhile)))

;; A break statement
(define (break-negative l)
  (call/cc
   (λ (break) ;;dopo aver chiamato la continuazione arrivo qua
     (for-each (λ (x)
                 (if (>= x 0)
                     (displayln x)
                     (break)))
               l))))

;; A continue statement
(define (skip-negative l)
  (for-each
   (λ (x) ;;salvare la continuazione  dichiarandola dopo la dichiarazione di questa funzione
     (call/cc
      (λ (continue)
        (if (>= x 0)
            (displayln x)
            (continue)))))
   l))

;; A while loop with break and continue
;; Salvo 2 continuazioni, una prima l'esecuzione di tutto e una durante (continue)
(define-syntax while-do
  (syntax-rules (do break: continue:)
    ((_ guard do stmt ... break: br-stmt continue: ct-stmt)
     (call/cc ;; save the loop after the beginning of the loop
      (λ (br-stmt)
        (let loop ((guard-val (guard)))
          (call/cc   ;; Se uso questa continuazione uso il continue
           (λ (ct-stmt)
             (when guard-val
               stmt ...)))
          (when guard-val
            (loop (guard)))))))))

(define (display-positive l)
  (let ((i l))
    (while-do (λ () (pair? i))
              do
              (let ((x (car i)))
                (unless (number? x)
                  (break))
                (set! i (cdr i))
                (unless (> x 0)
                  (continue))
                (displayln x))
              break: break
              continue: continue)))

;; Nondeterministic choices with backtracking
(define *paths* '())
(define fail #f)

(define (choose l)
  (if (null? l)
      (fail)
      (call/cc
       (λ (cc)
         (set! *paths*
               (cons (lambda ()
                       (cc (choose (cdr l))))
                     *paths*))
         (car l)))))

(call/cc
 (λ (exit)
   (set! fail
         (λ ()
           (if (null? *paths*)
               (exit 'end)
               (let ((p1 (car *paths*)))
                 (set! *paths* (cdr *paths*))
                 (p1)))))))

(define (is-the-sum-of n)
  (unless (and (>= n 0) (<= n 10))
    (error "Out of range " n))
  (let ((x (choose '(0 1 2 3 4 5)))
        (y (choose '(0 1 2 3 4 5))))
    (if (= (+ x y) n)
        (list x y)
        (fail))))


;; A queue
(define queue '())

(define (enqueue x)
  (set! queue (append queue (list x))))

(define (dequeue)
  (unless (null? queue)
    (let ((x (car queue)))
      (set! queue (cdr queue))
      x)))

(define (empty-queue?)
  (null? queue))

;; Coroutines
(define (start-coroutine proc)
  (call/cc
   (λ (cc)
     (enqueue cc)
     (proc))))

(define (yield)
  (call/cc
   (λ (cc)
     (enqueue cc)
     ((dequeue)))))

(define (exit-coroutine name)
  (displayln (string-append "Exiting " name))
  (if (empty-queue?)
      (exit)
      ((dequeue))))

(define (say-something name n)
  (λ ()
    (let loop ((i 0))
      (display name)
      (display ": ")
      (displayln i)
      (yield)
      (if (< i n)
          (loop (+ i 1))
          (exit-coroutine name)))))

(define (test-cr)
  (start-coroutine (say-something "A" 3))
  (start-coroutine (say-something "B" 2))
  (exit-coroutine "Main"))
