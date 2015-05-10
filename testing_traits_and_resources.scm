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

(inherent-traits Canada)
(content (get-diplomacy 'canada-internal-character))
;Value: #(tms (#(value=80,
   premises=(),
   informants=(user))))

(define ba (eq-get (inherent-traits Canada) 'aggression))

(inquire (eq-get (eq-get (inherent-traits country) 'aggression) 'meek))
(inquire (eq-get (eq-get (inherent-traits country) 'aggression) 'passive))
(inquire (eq-get (eq-get (inherent-traits country) 'aggression) 'rational))
(inquire (eq-get (eq-get (inherent-traits country) 'aggression) 'aggressive))
(inquire (eq-get (eq-get (inherent-traits country) 'aggression) 'violent))

(inquire (eq-get (eq-get (inherent-traits country) 'diplomacy) 'isolationist))
(inquire (eq-get (eq-get (inherent-traits country) 'diplomacy) 'interventionist))

(inquire (eq-get (eq-get (inherent-traits country) 'confidence) 'critical))
(inquire (eq-get (eq-get (inherent-traits country) 'confidence) 'realistic))
(inquire (eq-get (eq-get (inherent-traits country) 'confidence) 'conceited))

(inquire (eq-get (eq-get (inherent-traits country) 'strength) 'weak))
(inquire (eq-get (eq-get (inherent-traits country) 'strength) 'strong))
(inquire (eq-get (eq-get (inherent-traits country) 'strength) 'super-power))

(inquire (eq-get (eq-get (inherent-traits country) 'intelligence) 'follower))
(inquire (eq-get (eq-get (inherent-traits country) 'intelligence) 'average-joe))
(inquire (eq-get (eq-get (inherent-traits country) 'intelligence) 'visionary))

;Value: #t

;;Testing Self-image of Inherent traits
(define usa (create-country 'usa 
			      "From sea to sea"
			      50
			      80
			      30
			      50
			      60
			      3.8
			      3.8
			      300))

(inherent-traits usa)
(set-image! usa (content (get-diplomacy 'usa-internal-character)))
(image canada)

(cond (((inquire (eq-get (eq-get (inherent-traits usa) 'aggression) 'meek)))
	 'meek)

(eq? (inquire (eq-get (eq-get (inherent-traits usa) 'aggression) 'meek)) #f)