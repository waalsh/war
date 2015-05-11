;; Models of other nations

(define (estimate-strength country resources) ;;we need to improve this
  (list 'weak))

(define (estimate-diplomacy country actions)
  (if (> (gift-counter actions) (/ (length actions) 2))
      (list 'isolationist)
      (list 'interventionist)))

(define (gift-counter actions)
  (let action-loop ((actions actions)
		    (counter 0))
    (cond ((pair? actions)
	   (if (eq? 'gift (car (car actions)))
	       (action-loop (cdr actions) (+ counter 1))
	       (action-loop (cdr actions) counter)))
	 (else counter))))
	
(define (estimate-aggression country country-watching actions)
  (let ((war-count (war-count country actions))
	(war-of-aggression (find-war country country-watching actions)))
    (cond (war-of-aggression
	   (list 'violent))
	  ((> war-count 0)
	   (list 'aggressive))
	  ((> (gift-counter actions) (/ (length actions) 2))
	   (list 'weak))
	  (else (list 'rational)))))

(define (war-count country actions)
  (let action-loop ((actions actions)
		    (counter 0))
    (cond ((pair? actions)
	   (if (eq? 'attack (car (car actions)))
	       (action-loop (cdr actions) (+ counter 1))
	       (action-loop (cdr actions) counter)))
	 (else counter))))

(define (find-war country country-watching actions)
  (let action-loop ((actions actions)
		    (aggression #f))
    (cond ((pair? actions)
	   (if (eq? country-watching (caddr (car actions)))
	       #t
	       (action-loop (cdr actions) #f)))
	  (else aggression))))

(define (estimate-confidence country country-watching actions)
  (let ((gifts-to-weaker (gifts-to-weaker country actions))
	(war-of-aggression (find-bad-war country actions)))
    (cond (war-of-aggression
	   (list 'conceited))
	  ((and gifts-to-weaker (eq? 'aggressive (self-aggression? country-watching))) ;bullies don't understand humanitarian aid
	   (list 'critical))
	  (else (list 'realistic)))))

(define (gifts-to-weaker country actions)
  (let action-loop ((actions actions)
		    (gifts? #f))
    (cond ((pair? actions)
	   (cond ((eq? 'gift (car (car actions)))
		  (if (weaker? (caddr (car actions)) country)
		      #t
		      (else (action-loop (cdr actions) #f)))
		  (else (action-loop (cdr actions) #f)))))
	  (else gifts))))

(define (weaker? country1 country2) ;change after Keke
  #t)

(define (estimate-intelligence country actions) ;;we need to improve this
  (list 'follower))

;find-diplomatic-opinions! decides which traits other countries should have
;output: dictionary of a country
;        each entry in list: (country . opinions)

(define (find-diplomatic-opinions country-with-opinions all-countries)
  (set-diplomatic-opinions! country-with-opinions
  (let ((opinions '()))
    (let country-loop ((countries all-countries)
		       (c-character '()))
       (cond ((pair? countries)
	     (set! c-character (append c-character (list (car countries))))
	     (set! c-character (append c-character (estimate-aggression (car countries) country-with-opinions (actions-taken (car countries)))))
	     (set! c-character (append c-character (estimate-diplomacy (car countries) (actions-taken (car countries)))))
	     (set! c-character (append c-character (estimate-confidence (car countries) country-with-opinions (actions-taken (car countries)))))
	     (set! c-character (append c-character (estimate-strength (car countries) (actions-taken (car countries)))))
	     (set! c-character (append c-character (estimate-intelligence (car countries) (actions-taken (car countries)))))
	  
	     (set! opinions (append opinions (list c-character)))
	     (country-loop (cdr countries) '()))
	    (else opinions))))))

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

;Catching Diplomacy
(define (past-canada-actions)
	(list (list 'gift canada usa) (list 'gift canada usa) (list 'attack canada usa)))



(gift-counter past-usa-actions)
(define (past-usa-actions)
	(list 
	 (list 'gift usa canada) 
	 (list 'gift usa canada)))

(set-actions-taken! canada (past-canada-actions))
(set-actions-taken! usa (past-usa-actions))

(set-diplomatic-opinions! usa (find-diplomatic-opinions usa (list canada usa)))
(diplomatic-opinions usa)

(inherent-traits usa)
(set-image! usa (content (get-diplomacy 'usa-internal-character)))
(image usa)

;Seeing the truth about yourself
(self-aggression? usa)


