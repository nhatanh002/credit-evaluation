(require 'schelog)
(require 'database)
(module selector (%amount %value %ok-profile %bankyield %requested-credit)
	(import database schelog chicken scheme r5rs)
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

(define %ok-profile
  (%rel (a b c d e f g h i k prof m n p q)
        [(a prof)
         (%userdb a b c d e f g h i k (cons 'ok-profile prof) m n p q)]))

(define %bankyield
  (%rel (yield= a b c d e f g h i k l m n p q)
        [(a yield=)
         (%userdb a b c d e f g h (cons 'bank-yield yield=) k l m n p q)]))


(define %requested-credit
  (%rel (credit= a b c d e f g h i k l m n p q)
        [(a credit=)
         (%userdb a b c d e f g h i (cons 'requested-credit credit=) l m n p q)]))

)
