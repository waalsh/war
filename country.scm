;This file creates country and resource objects

(define countries-in-play '())

;;WORLD OBJECTS

;Country object
(define-record-type <country>
    (%declare-nation c-name
		     c-motto
		     in-traits    ; list of adjectives
		     strategies   ; designations of desired relationship with other countries
                     deductions   ; perceived desired relationships between other countries
		     self-image   ; how the country perceives itself
		     res          ; list of resources
		     dip-opinions ; in-traits of others
		     meta-model   ; opinions of others about our in-traits
		     actions-t    ; actions taken
		     actions-r)   ; actions received
   country?
  (c-name country-name)
  (c-motto country-motto)
  (in-traits inherent-traits)
  (strategies strategy %set-strategy!)
  (deductions deduction %set-deduction!)
  (self-image image set-self-image!)
  (res resources set-resources!)
  (dip-opinions diplomatic-opinions set-diplomatic-opinions!)
  (meta-model perception set-perception!)
  (actions-t actions-taken set-actions-taken!)
  (actions-r actions-received set-actions-received!))

;Resource object; attached to countries through assign statements
(define-record-type <resource>
	(%declare-resource r-name 
			   r-amount)
    resource?
  (r-name resource-name)
  (r-amount resource-amount set-resource-amount!))

;; COUNTRY CREATION METHODS

;Empty country
(define (create-quick-country name motto)
  (set! countries-in-play (append countries-in-play name))
  (let ((in-traits '())
	(dev-traits '())
	(res '())
	(dip-opinions '())
	(actions-t '())
	(actions-r '()))
    (%declare-nation name motto in-traits dev-traits res dip-opinions actions-t actions-r)))

;Full-declared country
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
  (set! countries-in-play (append countries-in-play (list name)))
  (let ((in-traits (symbol-append name '-internal-character))
	(strategies (make-hash-table))
        (deductions (make-hash-table))
        (self-image '())
	(res (list (assign-money wealth) (assign-land land) (assign-population population)))
	(dip-opinions '())
        (meta-model '())
	(actions-t '())
	(actions-r '()))
    (declare-national-character! in-traits aggression diplomacy confidence strength intelligence)
    (%declare-nation name 
		     motto 
		     in-traits 
		     strategies
                     deductions
                     self-image
		     res 
		     dip-opinions
                     meta-model
		     actions-t
		     actions-r)))


(define (create-quick-country name motto)
  (set! countries-in-play (append countries-in-play (list name)))
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



(define (strategy-towards strategy)
   (car strategy))

(define (set-strategy! of-country about-country in-mind-of new-strategy)
    (if (eq? in-mind-of of-country)
    	;;we are inside our mind making a judgment about someone
        (hash-table-set! (strategy in-mind-of) (country-name about-country) new-strategy)
        ;; we are inside our mind making judgement about someone else's judgment of someone else
        (hash-table-set! (deduction in-mind-of) `(,(country-name of-country) ,(country-name about-country)) new-strategy))) 


;; RESOURCE DECLARATION, GETTING, and SETTING METHODS

;Declaration
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

;Get
(define (get-money! country)
  (car (resources country)))

(define (get-land! country)
  (cadr (resources country)))

(define (get-population! country)
  (caddr (resources country)))

;Set
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
