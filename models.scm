;; Models of other nations

(define (estimate-strength country resources) ;;we need to improve this
  (list 'weak))

(define (estimate-diplomacy country counter actions-num) ;;we need to improve this
  (list 'isolationist))

(define (estimate-aggression country action) ;;we need to improve this
  (list 'meek))

(define (estimate-confidence country action) ;;we need to improve this
  (list 'critical))

(define (estimate-intelligence country action) ;;we need to improve this
  (list 'follower))


;find-diplomatic-opinions! decides which traits other countries should have
;output: dictionary of a country
;        each entry in list: (country . opinions)

(define (find-diplomatic-opinions country-with-opinions all-countries)
  (let ((opinions '()))
    (let country-loop ((countries all-countries))
      (cond ((pair? countries)
	     (let action-loop ((actions (actions-taken (car countries)))
			       (counter 0)
			       (c-character (list (car countries))))
	      (cond ((pair? actions) ;more than one action
		     ;;deal with the actions
		     (set! c-character (append c-character (estimate-aggression (car countries) (car actions))))
		     (set! c-character (append c-character (estimate-aggression (car countries) (car actions))))
		     (set! c-character (append c-character (estimate-aggression (car countries) (car actions))))
		     (set! c-character (append c-character (estimate-aggression (car countries) (car actions))))
		     (set! c-character (append c-character (estimate-aggression (car countries) (car actions))))
		     (set! c-character (append c-character (list 'happy)))
		     (action-loop (cdr actions) (+ counter 1) c-character))))
	    ;;deal with the country
	    (set! opinions (append opinions (list 'b)))
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

(set-diplomatic-opinions! usa (find-diplomatic-opinions usa (list usa canada usa usa)))
(diplomatic-opinions usa)
