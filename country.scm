;;; This creates a country object

(define countries-in-play '())

(define-record-type <country>
    (%declare-nation c-name
		     c-motto
		     in-traits    ; list of adjectives
		     dev-traits   ; strategies and image and our opinion of our intraits
		     res          ; list of resources
		     dip-opinions ; in-traits of others
		     ;meta-model      ; opinions of others about our in-traits country
		     actions-t    ; actions taken
		     actions-r)   ; actions received
    country?
  (c-name name)
  (c-motto motto)
  (in-traits inherent-traits)
  (dev-traits developed-traits set-developed-traits!)
  (res resources set-resources!)
  (dip-opinions diplomatic-opinions set-diplomatic-opinions!)
  (actions-t actions-taken set-actions-taken!)
  (actions-r actions-received set-actions-received!))


(define-record-type <resource>
	(%declare-resource name 
			   amount)
    resource?
  (name name)
  (amount amount set-amount!)
  (country set-country!))

(define countries-in-play '())

(define (create-quick-country name motto)
  (append countries-in-play name)
  (let ((in-traits '())
	(dev-traits '())
	(res '())
	(dip-opinions '())
	(actions-t '())
	(actions-r '()))
    (%declare-nation name 
		     motto 
		     in-traits 
		     dev-traits
		     res 
		     dip-opinions
		     actions-t
		     actions-r)))

(define (assign-money value)
  (let ((value value)
	(name 'money))
    (%declare-resource name
		       value)))

(define (assign-land value)
  (let ((value value)
	(name 'land))
    (%declare-resource name
		       value)))

(define (assign-population value)
  (let ((value value)
	(name 'people))
    (%declare-resource name
		       value)))

(define (get-money! country)
  (car (resources country)))

(define (get-land! country)
  (cadr (resources country)))

(define (get-population! country)
  (caddr (resources country)))

(define (set-money! country value)
  (let ((country-resources (resources country)))
    (define new-resources 
      (list (assign-money value)
	    (cdr country-resources)))
    (set-resources! country new-resources)))

(define (set-land! country value)
  (let ((country-resources (resources country)))
    (define new-resources
      (list (car country-resources)
	    (assign-land value)
	    (caddr country-resources)))
    (set-resources! country new-resources)))

(define (set-population! country value)
  (let ((country-resources (resources country)))
    (define new-resources
      (list (car country-resources)
	    (cadr country-resources)
	    (assign-population value)))
    (set-resources! country new-resources)))

(define (random-resources)
  (list (assign-money (random 18000000000))
	(assign-land (random 6600000))
	(assign-population (random 10000000))))

;Testing

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
