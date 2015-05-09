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
			   amount
			   country)
    resource?
  (name name)
  (amount amount set-amount!)
  (country set-country!))

;people land money



(define countries-in-play '())

(define (create-country name
	                motto
	                aggression
	                diplomacy
			confidence
			strength
	                intelligence
			land
			wealth
			population
			)
  (set! countries-in-play (append countries-in-play name))
  (let ((in-traits (symbol-append person name '-internal-character))
	(dev-traits '())
	(res (list (assign-money wealth) (assign-land land) (assign-population population)))
	(dip-opinions '())
	(actions-t '())
	(actions-r '()))

    (declare-national-character! in-traits aggression diplomacy confidence strength intelligence)

    (%declare-nation name 
		     motto 
		     in-traits 
		     dev-traits
		     res 
		     dip-opinions
		     actions-t
		     actions-r)))


(define (create-quick-country name motto)
  (set! countries-in-play (append countries-in-play name))
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
