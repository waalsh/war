;;;;Testing Resources

;Create simple country, give it some simple and random resources. Re-set its population.

(define USA (create-quick-country 'USA "We're the best")) ;empty resources
(value (car (resources USA)))
;Error, because resources USA returns empty list because its empty

(define some-res (random-resources))
(set-resources! usa some-res) 

(amount (car (resources USA)))
;Value: 17330876242

(set-money! usa 0) ;bankruptcy :( Mass panic :( so sad :(

(amount (car (resources USA)))
;Value: 0

(resources USA)

(+ (car (resources USA)) 3)

countries-in-play




;;;;Testing Traits
(cd "..")
(cd "krebecca")
(load "war/load")
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


(inherent-traits Canada)
(define ba (eq-get (inherent-traits Canada) 'aggression))




(inquire (eq-get ba 'rational))
