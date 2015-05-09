;;; This file describes traits that countries can have in our world
;;; It also displays the actions associated with traits


(define (add-branch! parent child name)
  (eq-put! parent name child)
  (eq-put! child 'parent parent)
  (eq-put! child 'given-name name)
  'done)

(define (name-of thing)
  (let ((n (eq-get thing 'given-name)))
    (if n
	(let ((p (eq-get thing 'parent)))
	  (if p
	      (cons n (name-of p))
	      (list n)))
	(list (name thing)))))


(define (make-personality country-character)

  (let-cells (aggression diplomacy confidence wealth strength)
    (add-branch! country-character aggression 'aggression)
    (add-branch! country-character diplomacy 'diplomacy)
    (add-branch! country-character confidence 'confidence)
    (add-branch! country-character strength 'strength)
    (add-branch! country-character intelligence 'intelligence)

    ((c:bins (named-ranges 'aggression-scale
		           '(meek ,(make-interval 0 20))
                           '(passive ,(make-interval 21 40))
                           '(rational ,(make-interval 41 60))
                           '(aggressive ,(make-interval 61 90))
		           '(violent ,(make-interval 91 100))))
     aggression)


    ((c:bins (named-ranges 'diplomacy-scale
		           '(isolationist ,(make-interval 0 50))
                           '(interventionist ,(make-interval 51 100))))
     diplomacy)


    ((c:bins (named-ranges 'confidence-scale
		           '(critical ,(make-interval 0 30))
                           '(realistic ,(make-interval 31 70))
                           '(conceited ,(make-interval 71 100))))
     confidence)


    ((c:bins (named-ranges 'strength-scale
		           '(weak ,(make-interval 0 40))
                           '(strong ,(make-interval 41 80))
                           '(super-power ,(make-interval 81 100))))
     strength)


    ((c:bins (named-ranges 'intelligence-scale
		           '(follower ,(make-interval 0 40))
                           '(average-joe ,(make-interval 41 80))
                           '(visionary ,(make-interval 81 100))))
     intelligence)
    ))

(define (aggression country-character)
  (eq-get country-character 'aggression))

(define (diplomacy country-character)
  (eq-get country-character 'diplomacy))

(define (confidence country-character)
  (eq-get country-character 'confidence))

(define (strength country-character)
  (eq-get country-character 'strength))

(define (intelligence country-character)
  (eq-get country-character 'intelligence))


(define (declare-national-character! country-character
				    aggression_inherent 
				    diplomacy_inherent
				    confidence_inherent
				    strength_inherent
				    intelligence_inherent)
  (make-personality country-character)
  (tell! (aggression country-character) aggression_inherent)
  (tell! (diplomacy country-character) diplomacy-inherent)
  (tell! (confidence country-character) confidence_inherent)
  (tell! (strength country-character) strength_inherent)
  (tell! (intelligence country-character) intelligence_inherent))


