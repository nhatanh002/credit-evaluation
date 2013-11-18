;;sample data
(load "schelog.scm")
(define %bank-yield
  (%rel ()
        [('client1 'excellent)]))

(define %requested-credit
  (%rel ()
        [('client1 50000)]))

(define %amount
  (%rel ()
        [('local-currency-deposits 'client1 30000)]
        [('foreign-currency-deposits 'client1 20000)]
        [('bank-guarantees 'client1 3000)]
        [('negotiate_instruments 'client1 5000)]
        [('stocks 'client1 9000)]
        [('mortage 'client1 12000)]
        [('documents 'client1 14000)]))

;(define %ok-profile
;  (%rel () [('client1)]))

(define %value
  (%rel ()
         [('net-worth-per-assets 'client1 40)]
         [('last-year-sales-growth 'client1 20)]
         [('gross-profits-on-sales 'client1 45)]
         [('short-term-debt-per-annual-sales 'client1 9)]))
   

;;;;

;;(evaluate Profile Outcome)
(define (condition Type Test Rating)
  (vector 'condition Type Test Rating))

(define (profile C F Y) (vector 'profile C F Y))

;bank data and rules
(define %rule
  (%rel ()
        [( (list (condition 'collateral '>= 'excellent)
              (condition 'finances '>= 'good)
              (condition 'yield '>= 'reasonable))
           'give_credit)]
        [( (list (condition 'collateral '= 'good)
                 (condition 'finances '= 'good)
                 (condition 'yield '>= 'reasonable))
           'consult_superior)]
        [( (list (condition 'collateral '=< 'moderate)
                 (condition 'finances '=< 'medium))
           'refuse_credit)]))

(define %scale
  (%rel ()
        [('collateral (list 'excellent 'good 'moderate))]
        [('finances (list 'excellent 'good 'medium 'bad))]
        [('yield (list 'excellent 'reasonable 'poor))]))
(define %sumlist              
(let ((%sumlist-aux
    (%rel (x xs temp temp1 sum)
          [('() sum sum)]
          [((cons x xs) temp sum)
           (%is temp1 (+ temp x))
           (%sumlist-aux xs temp1 sum)])))
  (%rel (xs sum)
        [(xs sum) (%sumlist-aux xs 0 sum)])))
        
