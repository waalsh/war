;; Models of other nations


; for country's in the world
	; set counter to 0 for gift counting
	; decide strength based on resources
	; for the actions of those countries
		;increment counter if it's 'give gift'

(define (estimate-strength country resources) ;;we need to improve this
	'weak)

;find-diplomatic-opinions! decides which traits other countries should have
;output: dictionary of a country
;        each entry in list: (country . opinions)
;(define (find-diplomatic-opinions country-with-opinions all-countries)
;  (let ((opinions '()))
;   (let country-loop ((countries all-countries))
;      (if (pair? countries)
;	  (let ((gift-counter 0)
;		(country (car countries))
;		(strength (estimate-strength country (resources country))))
;	    (set! opinions (append opinions strength))
;	    (let action-loop ((actions (taken-actions country)))
;	      (if (pair? countries)
;		  (define counter (+ counter 1))
;		  (set! opinions (append opinions counter))
;		  (action-loop (cdr actions)))
;	      (country-loop (cdr countries))))))
;      opinions))


(define (find-diplomatic-opinions country-with-opinions all-countries)
  (let ((opinions '()))
    (let country-loop ((countries all-countries))
      (cond ((pair? countries)
	    (let action-loop ((actions (actions-taken (car countries))))
	      (cond ((pair? actions)
		    (pp (car actions))
		    (action-loop (cdr actions)))
		    ))
	    (pp (country-name (car countries)))
	    (country-loop (cdr countries)))
	    (else opinions)))))

(cd "..")
(load "war/load")

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
(define canada (create-country 'canada 
			      "From sea to seas"
			      50
			      80
			      30
			      50
			      60
			      3.8
			      3.8
			      300))

(define (past-canada-actions)
	(list (list 'attack canada usa) (list 'gift canada usa) (list 'gift canada usa)))

(define (past-usa-actions)
	(list (list 'attack usa canada) (list 'gift usa canada) (list 'gift usa canada)))

(set-actions-taken! canada (past-canada-actions))
(set-actions-taken! usa (past-usa-actions))

(set-diplomatic-opinions! usa (find-diplomatic-opinions-alpha usa (list usa canada usa usa)))
(set-diplomatic-opinions! usa (find-diplomatic-opinions usa (list usa canada usa usa)))
(diplomatic-opinions usa)
