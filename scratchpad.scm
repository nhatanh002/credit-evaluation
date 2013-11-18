(load "./schelog.scm")

(%which () %fail)

(%which () %true)

(%which () (%=:= 1 1))

(define %appendo
  (%rel (H L1 L2 T)
        [('() L2 L2)]
        [((cons H L1) L2 (cons H T))
         (%appendo L1 L2 T)]))

(%which-til (L1 L2) (%appendo L1 L2 '(1 2 3 4 5 6)))

;(define %reverse
;  (%rel (A B L1 L2 Ltemp)
;         [('() '())]
;         [(L1 L2)
;          (%= (cons A B) L1)
;          (%reverse B Ltemp)
;          (%appendo Ltemp (cons A '()) L2)]
;         [(L2 L1)
;          (%= (cons A B) L1)
;          (%reverse B Ltemp)
;          (%appendo Ltemp (cons A '()) L2)]))
;(define %reverseo
;  (%rel (X Y Z W)
;        [('() X X)]
;        [((cons X Y) Z W)
;         (%reverseo Y (cons X Z) W)]))
(define %reverse
  (letrec
    ([revaux
       (%rel (x y z w)
	 [('() y y) (%== y y)]
	 [((cons x y) z w)
	   (revaux y
	     (cons x z) w)])])
    (%rel (x y)
      [(x y) (revaux x '() y)])))

;(%which (L) (%reverse '(1 2 3) L))

(define bigger
  (%rel (X Y Z)
        [('elephant 'horse)]
        [('horse 'mouse)]))
(define is-bigger
  (%rel (X Y Z)
        [(X Z)
         (bigger X Z)]
        [(X Z)
         (bigger X Y)
         (is-bigger Y Z)]))

(%which-til (X Z) (is-bigger X Z))
(%which-til (X Y) (%and (%is Y 4) (%is X (+ 3.4 Y))))

(define ls1 (list 1 2 3 4))

(define ls2 ((lambda(x) x) ls1))
(set-car! ls2 'a)

(define (write-a-list ls)
  (call-with-output-file "./list-store"
    (lambda (k) (write ls k))))

(define (read-a-list)
  (call-with-input-file "./list-store"
    (lambda (k) (read k))))



(define %fact
  (%rel (X Y Z T)
        [(0 1)]        
        [(X Y)
         (%> X 0)
         (%is T (- X 1))
         (%fact T Z)
         (%is Y (* X Z))]))
         
