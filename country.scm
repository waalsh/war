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
