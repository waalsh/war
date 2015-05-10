;; This will define how we perceive others

;; This is what country's action box might look like:
;    (list
;        (attack country-name receiver)
;        (demand-tribute country-name receiver)
;        (defend country-name receiver)
;        

;; Here is what our opinion of a country will look like:
;; (list aggression-adj  
;;       diplomacy-adj
;;       confidence-adj... so on.)


(define compare-traits (make-generic-operator 3 'similarity 'default))

(define (strength? comparison) (eq? comparison 'strength))
(define (intelligence? comparison) (eq? comparison 'intelligence))
(define (policies? comarison) (eq? comparison 'policies))
(define (rationality? comparison) (eq? comparison 'rationality))


(define (retrieve-opinion of-country about-country)
  (let lp ((opinion (dip-opinions of-country)))
    (if (eq? (car (car opinion)) (name about-country))
	(cdr (car opinion))
	(lp (cdr opinion)))))

(define (trait-temp-value trait)
  (cond ((eq? trait 'meek) 1)
	((eq? trait 'passive) 2)
	((eq? trait 'rational) 3)
	((eq? trait 'aggressive) 4)
	((eq? trait 'violent) 5)
	((eq? trait 'isolationist) 1)
	((eq? trait 'interventionist) 2)
	((eq? trait 'critical) 1)
	((eq? trait 'realistic) 2)
	((eq? trait 'conceited) 3)
	((eq? trait 'weak) 1)
	((eq? trait 'strong) 2)
	((eq? trait 'super-power) 3)
	((eq? trait 'follower) 1)
	((eq? trait 'average-joe) 2)
	((eq? trait 'visionary) 3)))
  
(defhandler compare-traits
  (lambda (point-of-comparison country1 country2)
    ;;get country1 strength from image
    ;;get country2 strength from dip-opinions
    ;;compare
    ;;output greater than, less than, or same
    (let ((country1-believed-strength (fourth (image country1)))
	  (country2-believed-strength (fourth (retriever-opinion country1 country2))))
      (cond ((< (trait-temp-value country1-believed-strength)
	       (trait-temp-value country2-believed-strength))
	    '(weaker than))
	    ((> (trait-temp-value country1-believed-strength)
	     (trait-temp-value country-2-believed-strength))
	     '(stronger than))
	    (else '(of the same strength as)))
    ))
  strength?)

(defhandler compare-traits
  (lambda (point-of-comparison country1 country2)
    ;;get country1 strength from image
    ;;get country2 strength from dip-opinions
    ;;compare
    ;;output greater than, less than, or same
    (let ((country1-believed-intelligence (fifth (image country1)))
	  (country2-believed-intelligence (fifth (retriever-opinion country1 country2))))
      (cond ((< (trait-temp-value country1-believed-intelligence)
	       (trait-temp-value country2-believed-intelligence))
	    '(less intelligent than))
	    ((> (trait-temp-value country1-believed-intelligence)
	     (trait-temp-value country-2-believed-intelligence))
	     '(more intelligent than))
	    (else '(of about the same intelligence as)))
    ))
  intelligence?)

(defhandler compare-traits
  (lambda (point-of-comparison country1 country2)
    ;;get country1 policies from image
    ;;get country2 policies from dip-opinions
    ;;comare
    ;;output like us or against us
    )
  policies?)

(defhandler compare-traits
  (lambda (point-of-comparison country1 country2)
    ;;get country1 aggression from image
    ;;get country2 aggression from dip-opinions
    ;;output greater than less than or same
    )
  rationality?)


(define (develop-strategy actor receiver)
  (let ((our-image (our image of ourselves))
	(our-view-of-target (or view of them))
	(our-resources (our resources))
	(their-resources (their resources))
