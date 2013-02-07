#lang racket
; Tree representation

(define (make-leaf symbol weight)
  (list `leaf symbol weight))

(define (leaf? object)
  (eq? (car object)`leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left                      
        right 
        (append (symbols left) (symbols right)) 
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


;-----------------------------------------------------------------------------------------------------------------------------------
; Huffman coding algorithm

(define (occurrence x L comp?)
  (length (filter (lambda (current) (comp? current x)) L)))

(define (smart_add x sorted_list comp?)
  (cond 
    ((null? sorted_list)
     (list x))
    ((<= (cadr x) (cadr (car sorted_list)))
     (cons x sorted_list))                 
    (else                                  
     (cons (car sorted_list) (smart_add x (cdr sorted_list) comp?)))))

(define (smart_add_tree x sorted_list comp?)
  (cond 
    ((null? sorted_list)
     (list x))
    ((<= (weight x) (weight (car sorted_list)))
     (cons x sorted_list))
    (else 
     (cons (car sorted_list) (smart_add_tree x (cdr sorted_list) comp?)))))

(define (frequency_list L comp?)
  (define (iterator L result)
    (if (null? L)
        result
        (let ((element (car L))
              (element_occurrence_tuple (list (car L) (occurrence (car L) L comp?))))
          (iterator (filter (lambda (x) (not (comp? element x))) (cdr L)) (smart_add element_occurrence_tuple result comp?)))))
  (iterator L `()))

(define (huffman_tree frequencies comp?)
  (define leaf_list (map (lambda (x) (make-leaf (car x) (cadr x))) frequencies))
  (define (iterator leaf_list tree)
    (if (= (length leaf_list) 1)
        tree
        (let*((left_leaf (car leaf_list))
              (right_leaf (cadr leaf_list))
              (sub_tree (make-code-tree left_leaf right_leaf))
              (new_leaf_list (smart_add_tree sub_tree (cddr leaf_list) comp?)))
          (iterator new_leaf_list sub_tree))))
  (iterator leaf_list `())
  )

(define (codes_list T)
  (define (helper T code)
    (if (leaf? T) 
        (list (list (symbol-leaf T) code))
        (append (helper (left-branch T) (append code `(0))) (helper (right-branch T) (append code `(1))))))
  (helper T `()))

(define (encode_element x codes comp?)
  (cadr (assoc x codes comp?)))

(define (code input codes comp?)
  (apply append (map (lambda (character) (encode_element character codes comp?)) input)))

(define (encode input comp?)
  (let* ((frequencies (frequency_list input comp?))
         (tree (huffman_tree frequencies comp?))
         (codes (codes_list tree))
         (encoded_input (code input codes comp?)))
    (list
     (reverse frequencies)                                               
     (map (lambda (x) (assoc (car x) codes comp?)) (reverse frequencies))
     encoded_input)))                                                    

(define (decode input h_tree)
  (define (iterator input tree result)
    (cond
      ((leaf? tree) (iterator input h_tree (append result (list (symbol-leaf tree)))))
      ((null? input) result)
      ((= (car input) 0) (iterator (cdr input) (left-branch tree) result))
      (else (iterator (cdr input) (right-branch tree) result))))
  (iterator input h_tree `()))
      
(define input `(a a a a b b c d a))
(define frequencies (frequency_list input equal?))
(define tree (huffman_tree frequencies equal?))
(define codes (codes_list tree))
(define encoded_input (code input codes equal?))

(display (encode input equal?))
