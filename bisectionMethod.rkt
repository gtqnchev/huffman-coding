(define (haveRoot? f a b)
  (< (* (f a) (f b)) 0))

(define (average a b)
  (/ (+ a b) 2))

(define (bisect f a b)
  (let (
        (c (average a b))
        (precision 0.00001)
        )
    (cond ((or (< (abs (- a b)) precision) (= (f c) 0)) c)
          ((haveRoot? f a c) (bisect f a c))
          ((haveRoot? f c b) (bisect f c b)))
    )
  )