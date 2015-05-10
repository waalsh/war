;; Models of other nations

(define (estimate-strength country resources) ;;we need to improve this
  (list 'weak))

(define (estimate-diplomacy country actions)
  (let action-loop ((actions actions))
    (cond ((pair? actions)
	   (pp (car (car actions)))
	   (action-loop (cdr actions))))
    (list 'isolationist)))

(define (estimate-aggression country actions) ;;we need to improve this
  (list 'meek))

(define (estimate-confidence country actions) ;;we need to improve this
  (list 'critical))

(define (estimate-intelligence country actions) ;;we need to improve this
  (list 'follower))


;find-diplomatic-opinions! decides which traits other countries should have
;output: dictionary of a country
;        each entry in list: (country . opinions)

(define (find-diplomatic-opinions country-with-opinions all-countries)
  (let ((opinions '()))
    (let country-loop ((countries all-countries)
		       (c-character '()))
      (cond ((pair? countries)
	     (set! c-character (append c-character (list (car countries))))
	     (set! c-character (append c-character (estimate-aggression (car countries) (actions-taken (car countries)))))
	     (set! c-character (append c-character (estimate-diplomacy (car countries) (actions-taken (car countries)))))
	     (set! c-character (append c-character (estimate-confidence (car countries) (actions-taken (car countries)))))
	     (set! c-character (append c-character (estimate-strength (car countries) (actions-taken (car countries)))))
	     (set! c-character (append c-character (estimate-intelligence (car countries) (actions-taken (car countries)))))
	  
	     (pp c-character)
	     (set! opinions (append opinions (list c-character)))
	     (country-loop (cdr countries) '()))
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

(set-diplomatic-opinions! usa (find-diplomatic-opinions usa (list usa canada usa usa)))
(diplomatic-opinions usa)
