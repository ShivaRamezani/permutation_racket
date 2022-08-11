#lang racket

(struct bst-node (value left right count) #:mutable #:transparent)

(define (add-value-to-bst tree v)
  (add-value-to-bst-subtree tree tree v))


(define (add-value-to-bst-subtree tree subtree v)
  (cond
    ((and (< v (bst-node-value subtree)) (empty? (bst-node-left subtree))) (set-bst-node-left! subtree (bst-node v empty empty 1)))
    ((and (< v (bst-node-value subtree)) (not (empty? (bst-node-left subtree)))) (add-value-to-bst-subtree tree (bst-node-left subtree) v))
    ((and (> v (bst-node-value subtree)) (empty? (bst-node-right subtree))) (set-bst-node-right! subtree (bst-node v empty empty 1)))
    ((and (> v (bst-node-value subtree)) (not (empty? (bst-node-right subtree)))) (add-value-to-bst-subtree tree (bst-node-right subtree) v))
    ((= v (bst-node-value subtree)) (+ count 1))
    ))




(define (get-bst-value-list-inorder n)
  (cond
    ((empty? (bst-node-value n)) empty)
    ((and (empty? (bst-node-left n)) (empty? (bst-node-right n))) (list (bst-node-value n)))
    ((and (empty? (bst-node-left n)) (not (empty? (bst-node-right n)))) (append (bst-node-value n) (get-bst-value-list-inorder (bst-node-right n))))
    ((and (empty? (bst-node-right n)) (not (empty? (bst-node-left n)))) (append (bst-node-value n) (get-bst-value-list-inorder (bst-node-left n))))
    ((and (not (empty? (bst-node-left n))) (not (empty? (bst-node-right n)))) (append (get-bst-value-list-inorder (bst-node-left n)) (list (bst-node-value n)) (get-bst-value-list-inorder (bst-node-right n))))
    
    ))




(define (get-random-in-range rng)
 (inexact->exact (remainder (floor (* (random) (expt 2 31))) rng))
 )

;(get-random-in-range 15)




(define (get-random-list-in-range rng count)
  (cond
    ((= count 1) (list (get-random-in-range rng)))
    (else (append (list (get-random-in-range rng)) (get-random-list-in-range rng (- count 1)))
    )))

;(get-random-list-in-range 10 5)





(define (contains? x n)
  (cond
    ((empty? x) false)
    ((= (car x) n) true)
    (else (contains? (cdr x) n))
    ))

;(contains? '(10 12 14 16 18 20) 12)




(define (contains-duplicates? x)
  (cond
    ((or (empty? x) (empty? (cdr x))) false)
    ((contains? (cdr x) (car x)) true)
    (else (contains-duplicates? (cdr x)))
    ))

;(contains-duplicates? '(1 2 3 4 5 6 7 7))





(define (add-random-to-list-unique x rng)
 (add-random-to-list-unique-1 x (cons (get-random-in-range rng) x) rng)
 )

 
(define (add-random-to-list-unique-1 x y rng)
 (cond
 ((not (contains-duplicates? y)) y)
 (else (add-random-to-list-unique-1 x (cons (get-random-in-range rng) (cdr y)) rng))
 ))

;(add-random-to-list-unique '(10 12 14 16 18 20) 40)





(define (get-random-list-in-range-unique rng count)
 (cond
 ((= count 1) (add-random-to-list-unique empty rng))
 (else (append (add-random-to-list-unique (get-random-list-in-range-unique rng (- count 1)) rng)))
 ))

;(get-random-list-in-range-unique 25 10)




(define (is-sorted? x)
  (cond
    ((or (empty? x) (empty? (cdr x))) true)
    (else (and (< (car x) (car (cdr x))) (is-sorted? (cdr x))))
    ))

 
;(is-sorted? '(1 3 5 9 25))  



(define (get-path-values p)
  (cond
    ((empty? p) empty)
    
    (else (cons (bst-node-value(car p)) (get-path-values (cdr p))))

  )
)



(define (find-path v n)
  (find-path-1 v n empty))



(define (find-path-1 v n p)
  (cond
    ((empty? n) p)
    ((= v (bst-node-value n)) (cons n p))
    ((< v (bst-node-value n)) (cons n (find-path-1 v (bst-node-left n) p)  ))
    ((> v (bst-node-value n)) (cons n (find-path-1 v (bst-node-right n) p)  ))
    ))



(define (get-child-count tree)
  (cond
    ((null? tree) 0)
    ((and (null? (bst-node-left tree)) (null? (bst-node-right tree))) 0)
    ((null? (bst-node-left tree)) 1)
    ((null? (bst-node-right tree)) 1)
    (else 2)
 ))







(define (find-inorder-prev n)
 (cond
 ((null? (bst-node-right (bst-node-left n))) (list (bst-node-left n) n))
 (else (find-path-inorder-prev (bst-node-right (bst-node-left n)) (list (bst-node-left n) n)))
 ))


(define (find-path-inorder-prev n p)
  (cond
    ((null? (bst-node-right n)) (cons n p))
    (else (find-path-inorder-prev (bst-node-right n) (cons n p))) 
    ))

;(find-path-inorder-prev n2 '(10 15))





(define (delete-value-from-bst v tree)
  (delete-node-from-bst (reverse (find-path v tree)) tree))




(define (delete-node-from-bst nl tree)
  (cond
    ((null? nl) tree)
    ((> (bst-node-count (car nl)) 1) (set-bst-node-count! (car nl) (- (bst-node-count (car nl)) 1)) tree)
    ((= (get-child-count (car nl)) 0)
     (cond
       ((equal? (car nl) tree) null)
       (else
        (cond
          ((equal? (car nl) (bst-node-left (car (cdr nl))))
           (set-bst-node-left! (car (cdr nl)) null) tree)
          (else
           (set-bst-node-right! (car (cdr nl)) null) tree)))))
    ((= (get-child-count (car nl)) 1)
         (cond
          ((equal? (car nl) tree)
           (cond
             ((null? (bst-node-right (car nl)))
              (bst-node-left (car nl)))
             (else
              (bst-node-right (car nl)))))
          (else
           (cond
             ((and (equal? (car nl) (bst-node-left (car (cdr nl)))) (null? (bst-node-right (car nl))))
              (set-bst-node-left! (car (cdr nl)) (bst-node-left (car nl))))
             ((and (equal? (car nl) (bst-node-left (car (cdr nl)))) (null? (bst-node-left (car nl))))
              (set-bst-node-left! (car (cdr nl)) (bst-node-right (car nl))))
             ((and (equal? (car nl) (bst-node-right (car (cdr nl)))) (null? (bst-node-right (car nl))))
              (set-bst-node-right! (car (cdr nl)) (bst-node-left (car nl))))
             (else
              (set-bst-node-right! (car (cdr nl)) (bst-node-right (car nl)))))
           tree)))
    (else
        (delete-inorder-prev (find-inorder-prev (car nl)) (car nl)) tree)
 ))


(define (delete-inorder-prev p n)
  (set-bst-node-value! n (bst-node-value (car p)))
  (cond
    ((and (= (get-child-count (car p)) 0) (equal? (car p) (bst-node-left (car (cdr p)))))
     (set-bst-node-left! (car (cdr p)) null))
    ((and (= (get-child-count (car p)) 0) (equal? (car p) (bst-node-right (car (cdr p)))))
     (set-bst-node-right! (car (cdr p)) null))
    ((and (= (get-child-count (car p)) 1) (equal? (car p) (bst-node-left (car (cdr p)))))
     (set-bst-node-left! (car (cdr p)) (bst-node-left (car p))))
    (else
     (set-bst-node-right! (car (cdr p)) (bst-node-left (car p))))
    ))
    
    

(define (delete-value-list-from-bst x tree)
  (cond
    ((null? x) (void))
    (else
     (set! tree (delete-value-from-bst (car x) tree))
     (delete-value-list-from-bst (cdr x) tree))
    ))





; ===================================================
; test cases
; ===================================================


(define (display-nodes-inorder n)
 (cond
 ((empty? n) (void))
 ((and (empty? (bst-node-left n)) (empty? (bst-node-right n))) (display-node n))
 ((empty? (bst-node-left n)) (display-node n) (display-nodes-inorder (bst-node-right n)))
 ((empty? (bst-node-right n)) (display-nodes-inorder (bst-node-left n)) (display-node n))
 (else (display-nodes-inorder (bst-node-left n)) (display-node n) (display-nodes-inorder (bst-node-right n)))
 ))




(define (inorder n)
 (cond
 ((empty? n) (void))
 ((and (empty? (bst-node-left n)) (empty? (bst-node-right n))) (list (bst-node-value n)))
 ((empty? (bst-node-left n)) (append (list (bst-node-value n)) (inorder (bst-node-right n))))
 ((empty? (bst-node-right n)) (append (inorder (bst-node-left n)) (list (bst-node-value n))))
 (else (append (inorder (bst-node-left n)) (list (bst-node-value n)) (inorder (bst-node-right
n))))
 ))

(define (display-node node)
 (display (format "[~a(" (bst-node-value node)))
 (cond
 ((empty? (bst-node-left node)) (display "N,"))
 (else (display (format "~a," (bst-node-value (bst-node-left node))))))
 (cond
 ((empty? (bst-node-right node)) (display "N)]"))
 (else (display (format "~a)]" (bst-node-value (bst-node-right node))))))
 )






(define n21 (bst-node 93 empty empty 1))
(define n20 (bst-node 44 empty empty 1))
(define n19 (bst-node 94 n21 empty 1))
(define n18 (bst-node 46 empty empty 1))
(define n17 (bst-node 43 empty n20 1))
(define n16 (bst-node 26 empty empty 1))
(define n15 (bst-node 24 empty empty 1))
(define n14 (bst-node 95 n19 empty 1))
(define n13 (bst-node 83 empty empty 1))
(define n12 (bst-node 65 empty empty 1))
(define n11 (bst-node 45 n17 n18 1))
(define n10 (bst-node 35 empty empty 1))
(define n9 (bst-node 25 n15 n16 1))
(define n8 (bst-node 10 empty empty 1))
(define n7 (bst-node 90 n13 n14 1))
(define n6 (bst-node 60 empty n12 1))
(define n5 (bst-node 40 n10 n11 1))
(define n4 (bst-node 20 n8 n9 1))
(define n3 (bst-node 80 n6 n7 1))
(define n2 (bst-node 30 n4 n5 1))
(define n1 (bst-node 50 n2 n3 1))







"inorder traversal"
(inorder n1) 
"-----------------------------------------------------------------------------"
"display nodes in order"
(display-nodes-inorder n1)
(displayln "") 
"-----------------------------------------------------------------------------"
"get path values of node 60"
(define p44 (find-path 60 n1))
(get-path-values p44)
"find path in order predecessor of 20"
(get-path-values (find-inorder-prev (car (find-path 20 n1))))

"-----------------------------------------------------------------------------"
"We are going to delete 20 from the tree"
(define n1-1 (delete-value-from-bst 20 n1))
"Delete completeed"
"-----------------------------------------------------------------------------"
"Display resultant bst inorder of values"
(inorder n1-1)

"-----------------------------------------------------------------------------"
"Display resultant bst inorder of the nodes"
(display-nodes-inorder n1-1)
(displayln "")




  

     





















  