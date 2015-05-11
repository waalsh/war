(cd "..")
(load "war/load")

;;Nations to Test with

(define default1 (create-country 'default1 	;strongest state
			      "not a soviet satelite"
			      50
			      80
			      30
			      50
			      60
			      1000
			      1000
			      1000))

(define default2 (create-country 'default2 	;medium state
			      "From sea to seas"
			      50
			      80
			      30
			      50
			      90
			      500
			      500
			      500))


(define default3 (create-country 'default3 	;weaker state
			      "From A to Z"
			      50
			      50
			      20
			      20
			      20
			      5
			      5
			      5))

;;Test Defaults
; should be (rational, interventionist, realistic, *relative*, average-Joe)

(find-diplomatic-opinions default2 (list default1 default3))
(diplomatic-opinions default2)
;Value: ((#[country 14] rational interventionist realistic super-power average-joe) (#[country 16] rational interventionist realistic weak average-joe))

;Default1 has attacked Default3 in the past, so Default3 finds Default1 violent

(define (past-default1-actions)
	(list (list 'gift default1 default3) 
	(list 'gift default1 default3) 
	(list 'attack default1 default3)))

(set-actions-taken! default1 (past-default1-actions))

(find-diplomatic-opinions default3 (list default1))
(diplomatic-opinions default3)
;Value: ((#[country 14] violent isolationist realistic super-power average-joe))

;Meanwhile, default2 notices that default1 used deception on default3, and makes conclusions about default1's intelligence
(find-diplomatic-opinions default2 (list default1))
(diplomatic-opinions default2)
;Value: ((#[country 14] aggressive isolationist realistic super-power visionary))







;Catching Diplomacy
(define (past-default1-actions)
	(list (list 'gift canada usa) (list 'gift canada usa) (list 'attack canada usa)))

(define (past-usa-actions)
	(list 
	 (list 'gift usa canada) 
	 (list 'gift usa canada)))

(set-actions-taken! canada (past-canada-actions))
(set-actions-taken! usa (past-usa-actions))

(set-diplomatic-opinions! usa (find-diplomatic-opinions usa (list canada usa)))
(diplomatic-opinions usa)

;Seeing the truth about yourself
(self-intelligence? usa)
(self-aggression? usa)
(self-diplomacy? usa)
(self-confidence? usa)
(self-strength? usa)

;Testing others' confidence
