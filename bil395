
(define (atom? x)
   (ormap (lambda (p) (p x)) (list number? symbol? boolean? string?)))
(define (E3 comp)
		(cond
			((atom? comp) "Atom")
			((list? comp) "list")
			(else "nested list")))


(define (E4 list) (cond ((= number? car(list) #f) #f) ((number? car(list)) E4 cdr(list)) (else #t)) ) 



(define (E5 lis1 lis2)
(cond ((null? lis1) (null? lis2))
((null? lis2) #f)
((eq? (car lis1) (car lis2)) (E5 (cdr lis1) (cdr lis2)))
(else #f)))



(define (E7 lis1 lis2) (cons lis1 lis2))



(define (E8 ls)
  (define (ters2 ls ar)
    (if (null? ls)
        ar
        (if (list? (car ls))
            (ters2 (cdr ls) (cons (E8 (car ls)) ar))
            (ters2 (cdr ls) (cons (car ls) ar)))))
  (ters2 ls '()))




(define E9
  (lambda (reverse cdr)
    ((cdr values) (reverse) (cdr))))



 (define (sort list)
  (cond [(empty? list) empty] [(cons? list) (insert (first list)
                        (sort (rest list)))])) 
 (define (E10 n elmnt)
  (cond [(empty? elmnt) (cons n empty)] [else (cond [(> n (first elmnt)) (cons n elmnt)] [else (cons (first elmnt) (E10 n (rest elmnt)))])]))
  


(define (E12 list) (cond ((null? list) 0) (else (+ s 1) (+ (car list) (E12 (cdr list))))) (cond ((= s 0) (display "error" )) (else (/ list s))  ) ) 



(define (atom? x)
   (ormap (lambda (p) (p x)) (list number? symbol? boolean? string?)))
(define (E13 lis1 lis2)
(cond ((null? lis1) (null? lis2))
((null? lis2) #f)
((atom? lis1) (atom? lis2))
((atom? lis2) #f)
((list? lis1) (list? lis2))
((list? lis2) #f)
((eq? (car lis1) (car lis2)) (E13 (cdr lis1) (cdr lis2)))
(else #f)))



(define (rm x ls)
  (if (null? ls)
      '()
      (if (eqv? x (car ls))
          (rm x (cdr ls))
          (cons (car ls)
                (rm x (cdr ls))))))
(define (E14 list) (cond ((number? car list ) (rm (car list) list ) (else (E14 cdr(list))))))

