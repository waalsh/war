;;; Strategy is a goal-driven approach to diplomacy
;;; Countries develop strategies towards other nations


;(define develop-strategy (make-generic-operator 3 'strategy 'default-strategy))

;; There are three types of strategy that we can think of at this moment
;; There are strategies towards self
;; There are strategies towards another country you know
;; There are strategies towards countries you just met

;(defhandler strategy self-strategy on-self?)
;(defhandler strategy other-strategy on-other?)
;(defhandler strategy new-strategy on-new?)

;; A strategy is a tree which explores possible strategic options and
;; chooses one that makes sense

;; whenever we choose a strategy, we put it into a country's 
;; developed traits, along with a train of thought (path)
;; that produced said strategy

;; a strategy tree will have the following layers

;; -----------> See my notebook



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
(define (policies? comparison) (eq? comparison 'policies))
(define (rationality? comparison) (eq? comparison 'rationality))

(define (retrieve-opinion of-country about-country)
  (let lp ((opinion (diplomatic-opinions of-country)))
    (if (eq? (car (car opinion)) (country-name about-country))
	(cdr (car opinion))
	(if (and (pair? opinion) (< 1 (length opinion)))
	    (lp (cdr opinion))
            '(What a mysterious place!)))))


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
    ;;get country2 strength  from dip-opinions
    ;;compare
    ;;output greater than, less than, or same
    (let ((country1-believed-strength (fourth (image country1)))
	  (country2-believed-strength (fourth (retrieve-opinion country1 country2))))
      (cond ((< (trait-temp-value country1-believed-strength)
	       (trait-temp-value country2-believed-strength))
	    (list 1 (declare-reason 'Strength 
                                    (country-name country2)
                                    'stronger
                                    `(We are ,country1-believed-strength and they are ,country2-believed-strength))))
	    ((> (trait-temp-value country1-believed-strength)
	     (trait-temp-value country-2-believed-strength))
	     (list -1 (declare-reason 'Strength 
                                      (country-name country2)
                                      'weaker
                                      `(We are ,country1-believed-strength and they are ,country2-believed-strength))))
	    (else (list 0 (declare-reason 'Strength 
                                          (country-name country2)
                                          'weaker
                                          `(We are ,country1-believed-strength and they are ,country2-believed-strength)))))
    ))
  strength?)

(defhandler compare-traits
  (lambda (point-of-comparison country1 country2)
    ;;get country1 strength from image
    ;;get country2 strength from dip-opinions
    ;;compare
    ;;output greater than, less than, or same
    (let ((country1-believed-intelligence (fifth (image country1)))
	  (country2-believed-intelligence (fifth (retrieve-opinion country1 country2))))
      (cond ((< (trait-temp-value country1-believed-intelligence)
	       (trait-temp-value country2-believed-intelligence))
	    (list 1 (declare-reason 'Intelligence 
                                    (country-name country2)
                                    'smarter
                                    `(We are ,country1-believed-intelligence and they are ,country2-believed-intelligence))))
	    ((> (trait-temp-value country1-believed-intelligence)
	     (trait-temp-value country-2-believed-intelligence))
	     (list - 1 (declare-reason 'Intelligence
                                    (country-name country2)
                                    'smarter
                                    `(We are ,country1-believed-intelligence and they are ,country2-believed-intelligence))))
	    (else (list 0 (declare-reason 'Intelligence 
                                    (country-name country2)
                                    'smarter
                                    `(We are ,country1-believed-intelligence and they are ,country2-believed-intelligence)))))
    ))
  intelligence?)

(defhandler compare-traits
  (lambda (point-of-comparison country1 country2)
    ;;get country1 policies from image
    ;;get country2 policies from dip-opinions
    ;;comare
    ;;output like us or against us
    (let ((country1-policy (second (image country1)))
	  (country2-believed-policy (second (retrieve-opinion country1 country2))))
      (let ((compatibility (- (trait-temp-value country1-policy) (trait-temp-value country2-believed-policy))))
	(cond ((and (= compatibility 0) (eq? country1-policy 'isolationist)) '(We like))
	       ((> compatibility 0) '(We like))
	       (else '(We dislike))))))
  policies?)

(defhandler compare-traits
  (lambda (point-of-comparison country1 country2)
    ;;get country1 aggression from image
    ;;get country2 aggression from dip-opinions
    ;;output greater than less than or same
    (let ((country1-believed-aggression (first (image country1)))
	  (country2-believed-aggression (first (retrieve-opinion country1 country2))))
      (cond ((< (trait-temp-value country1-believed-aggression) (trait-temp-value country2-believed-aggression))
	      '(They are irrational))
	     (else '(They are rational)))))
  rationality?)


(define analyze-strategic-opinion (make-generic-operator 4 'analyze-possible-goals 'default))

(define (equals? option) (eq? option 'test-equals))


(defhandler analyze-strategic-opinion
  (lambda (test country1 country2 based-on)
    ;; based-on is a list of comparisons we will issue to make a decision
    (let lp ((considerations based-on)
	     (value-of-judgment 0)
	     (line-of-symbolic-reasoning '()))
      (if (pair? considerations)
	  (let ((evaluation (car (compare-traits (car considerations) country1 country2)))
		(thought (compare-traits (car considerations) country1 country2)))
	    (lp (cdr considerations)
		(+ value-of-judgment evaluation)
		(append line-of-symbolic-reasoning thought)))
	  (let ((final-say-so (choose-strategic-opinion country1 (length based-on) value-of-judgment)))
	    (begin (pp final-say-so) (pp value-of-judgment) (pp line-of-symbolic-reasoning) (pp based-on))))

))
	  
  equals?)
    


(define choose-strategic-opinion
  (make-generic-operator 3 'confidence-based-decision-making 'default))

(define (critical-country? country) 
     (inquire (eq-get (eq-get (inherent-traits country) 'confidence) 'critical)))
(define (realistic-country? country) 
     (inquire (eq-get (eq-get (inherent-traits country) 'confidence) 'realistic))) 
(define (conceited-country? country) 
     (inquire (eq-get (eq-get (inherent-traits country) 'confidence) 'conceited))) 

(defhandler choose-strategic-opinion
  (lambda (country considerations value-of-judgment)
    (let ((threshold (* considerations -0.5))
	  (value value-of-judgment))
      (produce-opinion threshold value)))
  critical-country?)

(defhandler choose-strategic-opinion
  (lambda (country considerations value-of-judgment)
    (let ((threshold (* considerations 0))
	  (value value-of-judgment))
      (produce-opinion threshold value)))
  realistic-country?)

(defhandler choose-strategic-opinion
  (lambda (country considerations value-of-judgment)
    (let ((threshold (* considerations 0.5))
	  (value value-of-judgment))
      (produce-opinion threshold value)))
  conceited-country?)

(define (produce-opinion threshold value)
  (cond ((= threshold value) 'equal)
	((> value threshold) 'superior)
	((< value threshold) 'inferior)))

;(define (develop-strategy actor receiver)
;  (let ((our-image (our image of ourselves))
;	(our-view-of-target (or view of them))
;	(our-resources (our resources))
;	(their-resources (their resources)))))
