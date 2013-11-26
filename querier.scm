(require 'schelog)
(require 'selector)
(require 'knowledgebase)
(module querier *
	(import chicken scheme extras r5rs schelog selector knowledgebase) 
	  
;utility: query all answers, strip duplicates
(define (query-strip query)
  (define (accum ls val)
    (if (not val) ls
        (accum (cons val ls) (%more))))
  (let ((sols (list query)))
    (strip-duplicates (accum sols (%more)))))

;query procedure. returns the stats and suggestion for 'client'.
;remove duplicates.
(define (es:query client)
  (let ((query (%which (Client Ok-profile Requested Collateral-rating Financial-rating Yield Suggestion)
                             (%and (%credit client Ok-profile Collateral-rating Financial-rating Yield Suggestion)
                                   (%is Client client)
                                   (%requested-credit client Requested))))
        (fun (lambda(x)
	       (display "\t")
               (for-each (lambda(y) (display y) (display " ")) x)
               (newline))))
    (newline)
    (for-each fun (car (query-strip query)))
    (newline)))

(define (es:outcome outcome)
  (let ((query (%which (Client Requested)
		       (%let (?Ok-profile= Collateral-rating= Financial-rating= Yield=)
                             (%and (%credit Client ?Ok-profile= Collateral-rating= Financial-rating= Yield= outcome)
                                   (%requested-credit Client Requested)))))
        (fun (lambda(x)
               (for-each (lambda(y) (display (car y)) (display " = ") (display (cadr y)) (display " ")) x)
               (newline))))
    (newline)
    (for-each fun (query-strip query))
    (newline)))

