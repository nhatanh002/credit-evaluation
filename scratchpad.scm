(load "./schelog.scm")

(%which () %fail)

(%which () %true)

(%which () (%=:= 1 1))

(define %appendo
  (%rel (H L1 L2 T) ;"construct" new logic variables H, L1, L2, T to use in the relation
        [('() L2 L2)] ;appendo([],L2,L2).
        [((cons H L1) L2 (cons H T)) ;appendo([H|L1], L2, [H|T]) :-
         (%appendo L1 L2 T)]))       ;  appendo(L1,L2,T).

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
	 [('() y y)] ;reverse([],Y,Y). Y = y
	 [((cons x y) z w)     ;reverse([X|Y], Z, W) :- reverse(Y,[X|Z],W).
	   (%/== (cons x y) '()) 
	   (%/= z w)
	   (revaux y           
	     (cons x z) w)])])
    (%rel (x y)                    ;reverse(X,Y) :- reverse(X,[],Y).
      [(x y) (revaux x '() y)])))

;(%which (L) (%reverse '(1 2 3) L))

(define bigger
  (%rel (X Y Z)
        [('elephant 'horse)]
        [('horse 'mouse)]))
(define bigger
  (%rel (X Y Z)        
        [(X Z) 
         (bigger X Y) 
         (bigger Y Z)]))
;fact: a(x) -> positive
;goal: ~a(x) ... == ~a(x) (1)
;rule: a(X) <- c(X) AND d(Y,X) == a(X) OR ~c(X) ~d(Y,X) (2)
;resolve (1) (2) -> mgu(X = x), ~c(X) OR ~ d(Y,X) -> ~c(x) OR ~d(Y,x) -> ~(c(x) AND d(Y,x))
(%which-til (X Z) (is-bigger X Z))
(%which-til (X Y) (%and (%is Y 4) (%is X (+ 3.4 Y))))

(define ls1 (list 1 2 3 4))

(define ls2 ((lambda(x) x) ls1))
(set-car! ls2 'a)

(define (write-a-list ls filename)
  (call-with-output-file filename
    (lambda (k) (write ls k))))

(define (read-a-list filename)
  (call-with-input-file filename
    (lambda (k) (read k))))



(define %fact
  (%rel (X Y Z T)
        [(0 1)]  ;fact(0,1).      
        [(X Y)   ;fact(X,Y) :- X > 0, T is X-1, Y is X*Z, fact(T,Z).
         (%> X 0)
         (%is T (- X 1))
         (%fact T Z)
         (%is Y (* X Z))]))
         

(define %userdb
  (%rel ()
        [('client1 ;a
         '(local_currency_deposits 30000) ;b
         '(foreign_currency_deposits 20000) ;c
         '(bank_guarantees 3000) ;d
         '(negotiate_instruments 5000) ;e
         '(stocks 9000) ;f
         '(mortage 12000) ;g
         '(documents 14000) ;h
         '(bank-yield excellent) ;i
         '(requested-credit 50000) ;k
         'ok-profile ;l
         '(net_worth_per_assets 40) ;m
         '(last_year_sales_growth 20) ;n
         '(gross_profits_on_sales 45) ;p
         '(short_term_debt_per_annual_sales 9) ;q
         )]))

(define %ok-profile
  (%rel (a b c d e f g h i k l m n p q)
        [(a)
         (%userdb a b c d e f g h i k 'ok-profile m n p q)]))

(define %bank-yield
  (%rel (yield= a b c d e f g h i k l m n p q)
        [(a yield=)
         (%userdb a b c d e f g h (cons 'bank-yield yield=) k l m n p q)]))

(define %requested-credit
  (%rel (credit= a b c d e f g h i k l m n p q)
        [(a credit=)
         (%userdb a b c d e f g h i (cons 'requested-credit credit=) l m n p q)]))

(define %value
  (%rel (a b c d e f g h i k l m n p q val)
        [('net_worth_per_assets a val)
         (%userdb a b c d e f g h i k l (cons 'net_worth_per_assets val) n p q)]
        [('last_year_sales_growth a val)
         (%userdb a b c d e f g h i k l m (cons 'last_year_sales_growth val) p q)]
        [('gross_profits_on_sales a val)
                  (%userdb a b c d e f g h i k l m n (cons 'gross_profits_on_sales val) q)]
        [('short_term_debt_per_annual_sales a val)
                           (%userdb a b c d e f g h i k l m n p (cons 'short_term_debt_per_annual_sales val))
         ]))


(define %amount
  (%rel (a b c d e f g h i k l m n p q val)
        [('local_currency_deposits a val)
         (%userdb a (cons 'local_currency_deposits val) c d e f g h i k l m n p q)]
        [('foreign_currency_deposits a val)
         (%userdb a b (cons 'foreign_currency_deposits val) d e f g h i k l m n p q)]
        [('bank_guarantees a val)
         (%userdb a b c (cons 'bank_guarantees val) e f g h i k l m n p q)]
        [('negotiate_instruments a val)
         (%userdb a b c d (cons 'negotiate_instruments val) f g h i k l m n p q)]
        [('stocks a val)
         (%userdb a b c d e (cons 'stocks val) g h i k l m n p q)]
        [('mortage a val)
         (%userdb a b c d e f (cons 'mortage val) h i k l m n p q)]
        [('documents a val)
         (%userdb a b c d e f g (cons 'documents val) i k l m n p q)]))
         

(? (a b c d e f g h i k l m n p q) (%userdb a b c d e f g h i k l m n p q))

(define %userdb %empty-rel)
(define (read-db filename)
  (define (read-a-list filename)
  (call-with-input-file filename
    (lambda (k) (read k))))
  (let ((fun (lambda (ls) (eval (list '%assert '%userdb '() ls))))
        (template (cdr (read-a-list filename))))
    (map fun template)))

(es:query 'client2)

(%which (Client=) (%let (Ok-profile? Requested= Collateral-rating= Financial-rating= Yield= ) 
                              (%credit Client= Ok-profile? Collateral-rating= Financial-rating= Yield= 'give_credit))) 
                                   ;(%is Client= client)
                                   ;(%requested-credit Client= Requested=))))







