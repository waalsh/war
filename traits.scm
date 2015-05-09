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

;Create propogator intervals for traits
(define (make-personality country-character)

  (let-cells (aggression diplomacy confidence strength intelligence)
    (add-branch! country-character aggression 'aggression)
    (add-branch! country-character diplomacy 'diplomacy)
    (add-branch! country-character confidence 'confidence)
    (add-branch! country-character strength 'strength)
    (add-branch! country-character intelligence 'intelligence)


    (add-interval-property aggression (make-interval 0 20) 'meek)
    (add-interval-property aggression (make-interval 21 40) 'passive)
    (add-interval-property aggression (make-interval 41 60) 'rational)
    (add-interval-property aggression (make-interval 61 90) 'aggressive)
    (add-interval-property aggression (make-interval 91 100) 'violent)

    (add-interval-property diplomacy (make-interval 0 50) 'isolationist)
    (add-interval-property diplomacy (make-interval 51 100) 'interventionist)

    (add-interval-property confidence (make-interval 0 30) 'critical)
    (add-interval-property confidence (make-interval 31 70) 'realistic)
    (add-interval-property confidence (make-interval 71 100) 'conceited)

    (add-interval-property strength (make-interval 0 40) 'weak)
    (add-interval-property strength (make-interval 41 80) 'strong)
    (add-interval-property strength (make-interval 81 100) 'super-power)

    (add-interval-property intelligence (make-interval 0 40) 'follower)
    (add-interval-property intelligence (make-interval 41 80) 'average-joe)
    (add-interval-property intelligence (make-interval 81 100) 'visionary)
    ))

(define (get-aggression country-character)
  (eq-get country-character 'aggression))

(define (get-diplomacy country-character)
  (eq-get country-character 'diplomacy))

(define (get-confidence country-character)
  (eq-get country-character 'confidence))

(define (get-strength country-character)
  (eq-get country-character 'strength))

(define (get-intelligence country-character)
  (eq-get country-character 'intelligence))

;Assigning traits to a country
(define (declare-national-character! country-character
				     aggression_inherent 
				     diplomacy_inherent
				     confidence_inherent
				     strength_inherent
				     intelligence_inherent)
  (pp 'Declaring)
  (make-personality country-character)

  (tell! (get-aggression country-character) aggression_inherent)
  (tell! (get-diplomacy country-character) diplomacy_inherent)
  (tell! (get-confidence country-character) confidence_inherent)
  (tell! (get-strength country-character) strength_inherent)
  (tell! (get-intelligence country-character) intelligence_inherent))

