(require 'schelog)
(require 'database)
(require 'selector)
(require 'knowledgebase)
(require 'explainer)
(require 'querier)
(import schelog database selector knowledgebase explainer querier)
(use readline)
(current-input-port (make-gnu-readline-port))
(read-db "data")
(display "List of current customers: ")
(display %user-list)
(newline)

(let loop ()  
;  	(display (string? x))
;	(newline)
;	(display (pair? x))
;	(newline)
;	(display (eval x))

	(newline)
	(display "Choose one of the options below:\n")
	(display "[1]: Show the list of customers\n")
	(display "[2]: Ask the expert system for suggestion on a customer request\n")
	(display "[3]: Ask for the expert system's explaination of the suggestion for a customer\n")
	(display "[4]: Show all customers whose requests for credit should be accepted\n") 
	(display "[5]: Show all customers whose requests for credit should NOT be accepted\n")
	(display "[6]: Show all customers that the expert system still hasn't had any definitive decision about their requests for credit\n")
	(display "[0]: exit.\n")
	(newline)
	(let ((x (read)))
	  (if (or (not (number? x)) (< x 0) (> x 6))
	    (display "Invalid choice.\n\n")
	    (cond
	      [(= x 0) (display "\ngoodbye!") (newline) (quit)]
	      [(= x 1) (newline)(display %user-list) (newline)]
	      [(= x 2)
	       (display "\nEnter the name of the customer: \n")
		(let ((name (read)))
		  (if (not (member name %user-list))
			(begin 
			  (display "\nInvalid customer name.\n")
			  (loop))
			(begin
			  (newline)
			  (display "The evaluation customer \[")
			  (display name)
			  (display "\] according to the expert system:\n")
			  (es:query name))))]
	      [(= x 3)
	       (display "\nEnter the name of the customer: \n")
		(let ((name (read)))
		  (if (not (member name %user-list))
			(begin 
			  (display "\nInvalid customer name.\n")
			  (loop))
			(expl:explain name)))]
	      [(= x 4)
	       (display "\nThe customers who are qualified for requesting credit: \n")
	       (es:outcome 'accept-credit-request)
	       (newline)]
	      [(= x 5)
	       (display "\nThe customers who are NOT qualified for requesting credit: \n")
	       (es:outcome 'refuse-credit-request)
	       (newline)]
	      [(= x 6)
	       (display "\nThe customers whom the system is unsure whether to give credit or not, and should be waited for superiors' direction:\n")
	       (es:outcome 'ask-superior-for-direction)
	       (newline)]
	      )))

	(loop)
)
