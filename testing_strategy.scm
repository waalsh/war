(cd "..")
(cd "krebecca")
(load "war/load")

(load "war/strategy")
(define Canada (create-country 'Canada 
			      "From sea to sea"
			      50
			      80
			      30
			      50
			      60
			      90935100
			      51958
			      35))

(define Russia (create-country 'Russia
			       "TV doesth watch you here"
			       70
			       40
			       80
			       84
			       82
			       100000000
			       100000
			       128))

(define USA (create-country 'USA
			    "Capitalizing on you since 1783"
			    45
			    90
			    50
			    90
			    90
			    1000000000
			    100000
			    100))

(set-self-image! Canada '(rational interventionist conceited weak follower))
(image Canada)
(set-diplomatic-opinions! Canada '((Russia aggressive 
					   interventionist 
					   conceited
					   super-power
					   visionary)
				   (USA    rational
					   interventionist
					   realistic
					   super-power
					   visionary)))
(diplomatic-opinions Canada)
(retrieve-opinion (list Canada USA))


(trait-temp-value 'meek)
;;;;;;;;;;;;;;;;;;;;;;;;; country1 country2 perspective
(compare-traits 'strength Canada Russia Canada)
(compare-traits 'intelligence Canada Russia)
(compare-traits 'policies Canada Russia)
(compare-traits 'rationality Canada Russia)
(cd "..")
(load "war/strategy")
(load "war/reasoning")

;; testing reasoning

(define a (cadr (compare-traits 'strength USA Russia Canada)))
(define b (cadr (compare-traits 'intelligence Canada Russia Canada)))

(get-reasoning-train-of-thought a)
(get-subject-of-thought a)
(get-object-of-thought a)
(get-reasoning-question a)
(get-reasoning-conclusion a)


(analyze-strategic-opinion 'status Canada Russia Canada '(strength intelligence))
(hash-table-ref (strategy Canada) 'Russia)

(analyze-strategic-opinion 'status USA Russia Canada '(strength intelligence))
(hash-table-keys (deduction Canada))