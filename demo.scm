;; Sussman Presentation Day Demo

(load "war/load")
(cd "..")
(cd "krebecca")

(define Georgia (create-country 'Georgia
			       "Not just a Soviet satellite"
			       50
			       80
			       30
			       50
			       60
			       90935100
			       51958
			       35))


(define Russia (create-country 'Russia
			       "Here TV watch you"
			       70
			       40
			       80
			       84
			       82
			       100000000
			       100000
			       128))


(define USA (create-country 'USA
			    "Capitalizing on you since 1783"
			    45
			    90
			    50
			    90
			    90
			    1000000000
			    100000
			    100))


(set-image! georgia)
(set-image! usa)
(set-image! russia)

;; Georgia's self-image
;; Displaying how self image stems from character
(self-character georgia)
;Value: (rational interventionist critical strong average-joe)
(image georgia)
;Value: (rational interventionist critical weak follower)


;;Developing models of other countries

;;Russia 'gives gift' to Georgia, and then attacks them
(define (past-russia-actions)
	(list (list 'gift russia georgia) 
	(list 'gift russia georgia) 
	(list 'attack russia georgia)))

(set-actions-taken! russia (past-russia-actions))
(find-diplomatic-opinions usa (list russia georgia))
(find-diplomatic-opinions georgia (list russia usa))
(find-diplomatic-opinions russia (list usa georgia))

;;Let's see what the USA and Georgia think about that
(car (diplomatic-opinions usa))
(car (diplomatic-opinions georgia))

;;Differences:
   ;USA thinks Russia is intelligent ('visionary') because it recognized that Russia deceived Georgia with the gifts. Georgia did not recognize this strategy, so considers Russia of average intelligence.
   ;Georgia thinks Russia's level of aggression is 'violent' because Russia's attack was perpetrated on itself. USA considers Russia only 'aggressive' because they did no undergo an attack.
   ;Strength considerations: Countries perceive strenghth relative to their own strength; both USA and Georgia consider Russia to be greater than or equal to themselves.


(analyze-strategic-opinion 'status USA Russia USA '(strength intelligence))
(analyze-strategic-opinion 'status Georgia Russia USA '(strength intelligence))
(analyze-strategic-opinion 'status Russia Georgia USA '(strength intelligence))
(analyze-strategic-opinion 'status USA Georgia USA '(strength intelligence))



(initiate-QA USA)
(ask '(What is your status with respect to Russia))
(ask '(Really? Interesting! Why do you say that?))
(ask '(I get that - why though))
(ask '(I get that - why though))
(ask '(And why is that))

(ask '(In humble opinion of the Americas what is the status of Georgia in the opinion of Russia))
(ask '(Cool! Can you explain why to me))
(ask '(Why))
(ask '(Why do you think that about russia and georgia))
(ask '(Why do you think russia is a super-power))