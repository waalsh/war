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


(define compare-traits (make-generic-operator 4 'similarity 'default))

(define (strength? comparison) (eq? comparison 'strength))
(define (intelligence? comparison) (eq? comparison 'intelligence))
(define (policies? comparison) (eq? comparison 'policies))
(define (rationality? comparison) (eq? comparison 'rationality))


(define (retrieve-personal-opinion of-country-about-country)
  (let lp ((opinion (diplomatic-opinions (car of-country-about-country))))
    (if (eq? (car (car opinion)) (country-name (cadr of-country-about-country)))
	(cdr (car opinion))
	(if (and (pair? opinion) (< 1 (length opinion)))
	    (lp (cdr opinion))
            '(What a mysterious place!)))))

(define retrieve-opinion (make-generic-operator 1 'get-thoughts-about-entity retrieve-personal-opinion))
(define (my-opinion-about-me? whos-asking) (eq? (car whos-asking) (cadr whos-asking)))

(defhandler retrieve-opinion
  (lambda (of-country-about-country)
    (image (car of-country-about-country)))
 my-opinion-about-me?)

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
  (lambda (point-of-comparison country1 country2 perspective)
    ;;get country1 strength from image
    ;;get country2 strength  from dip-opinions
    ;;compare
    ;;output greater than, less than, or same
    (let ((country1-believed-strength (fourth (retrieve-opinion (list perspective country1))))
	  (country2-believed-strength (fourth (retrieve-opinion (list perspective country2)))))
      (cond ((< (trait-temp-value country1-believed-strength)
		(trait-temp-value country2-believed-strength))
	     (list 1 (declare-reason 'Strength 
				     (country-name country2)
                                     (country-name country1)
				     'stronger
				     `(,(country-name country1) is ,country1-believed-strength and 
                                        ,(country-name country2) is ,country2-believed-strength)
                                       1)))
	    ((> (trait-temp-value country1-believed-strength)
	     (trait-temp-value country2-believed-strength))
	     (list -1 (declare-reason 'Strength 
                                      (country-name country2)
                                      (country-name country1)
                                      'weaker
                                      `(,(country-name country1) is ,country1-believed-strength and 
                                        ,(country-name country2) is ,country2-believed-strength)
                                       -1)))
	    (else (list 0 (declare-reason 'Strength 
                                          (country-name country2)
                                          (country-name country1)
                                          'same
                                          `(,(country-name country1) is ,country1-believed-strength and 
                                        ,(country-name country2) is ,country2-believed-strength)
                                       0))))
    ))
  strength?)

(defhandler compare-traits
  (lambda (point-of-comparison country1 country2 perspective)
    ;;get country1 strength from image
    ;;get country2 strength from dip-opinions
    ;;compare
    ;;output greater than, less than, or same
    (let ((country1-believed-intelligence (fifth (retrieve-opinion (list perspective country1))))
	  (country2-believed-intelligence (fifth (retrieve-opinion (list perspective country2)))))
      (cond ((< (trait-temp-value country1-believed-intelligence)
	       (trait-temp-value country2-believed-intelligence))
	    (list 1 (declare-reason 'Intelligence 
                                    (country-name country2)
                                    (country-name country1)
                                    'greater
                                    `(,(country-name country1) is ,country1-believed-intelligence 
                                       and ,(country-name country2) is ,country2-believed-intelligence)
                                     1)))
	    ((> (trait-temp-value country1-believed-intelligence)
	     (trait-temp-value country2-believed-intelligence))
	     (list - 1 (declare-reason 'Intelligence
                                    (country-name country2)
                                    (country-name country1)
                                    'lesser
                                    `(,(country-name country1) is ,country1-believed-intelligence 
                                       and ,(country-name country2) is ,country2-believed-intelligence)
                                     -1)))
	    (else (list 0 (declare-reason 'Intelligence 
                                    (country-name country2)
                                    (country-name country1)
                                    'same
                                    `(,(country-name country1) is ,country1-believed-intelligence 
                                       and ,(country-name country2) is ,country2-believed-intelligence)
                                     0))))
    ))
  intelligence?)

;; Has to be fixed
(defhandler compare-traits
  (lambda (point-of-comparison country1 country2 perspective)
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


;; Has to be fixed
(defhandler compare-traits
  (lambda (point-of-comparison country1 country2 perspective)
    ;;get country1 aggression from image
    ;;get country2 aggression from dip-opinions
    ;;output greater than less than or same
    (let ((country1-believed-aggression (first (image country1)))
	  (country2-believed-aggression (first (retrieve-opinion country1 country2))))
      (cond ((< (trait-temp-value country1-believed-aggression) (trait-temp-value country2-believed-aggression))
	      '(They are irrational))
	     (else '(They are rational)))))
  rationality?)


(define analyze-strategic-opinion (make-generic-operator 5 'analyze-possible-goals 'default))

(define (status? option) (eq? option 'status))


(defhandler analyze-strategic-opinion
  (lambda (test country1 country2 perspective based-on)
    ;; based-on is a list of comparisons we will issue to make a decision
    (let lp ((considerations based-on)
	     (value-of-judgment 0)
	     (line-of-symbolic-reasoning '()))
      (if (pair? considerations)
	  (let ((evaluation (car (compare-traits (car considerations) country1 country2 perspective)))
		(thought (cadr (compare-traits (car considerations) country1 country2 perspective))))
	    (lp (cdr considerations)
		(+ value-of-judgment evaluation)
		(append line-of-symbolic-reasoning (list thought))))
	  (let ((final-say-so (choose-strategic-opinion perspective country1 (length based-on) value-of-judgment)))
	    (let ((new-strategy (declare-reason test 
                                                (country-name country2)
                                                (country-name country1)
                                                final-say-so
                                                line-of-symbolic-reasoning 1)))
               (set-strategy! country1 country2 perspective new-strategy)
               (pp '(I made up my mind)))))))
  status?)




(define (choose-strategic-opinion perspective country based-on value)
    (if (eq? perspective country)
         (choose-confidence-threshold (third (image country)) country based-on value)
         (choose-confidence-threshold (third (retrieve-opinion (list perspective country))) country based-on value)))

(define choose-confidence-threshold
  (make-generic-operator 4 'confidence-based-internal-decision-making 'default))

(define (critical-country? country)
     (eq? country 'critical))

(define (realistic-country? country) 
     (eq? country 'realistic))

(define (conceited-country? country) 
     (eq? country 'conceited))

(defhandler choose-confidence-threshold
  (lambda (trait country considerations value-of-judgment)
    (let ((threshold (* considerations -0.5))
	  (value value-of-judgment))
      (produce-opinion threshold value)))
  critical-country?)

(defhandler choose-confidence-threshold
  (lambda (trait country considerations value-of-judgment)
    (let ((threshold (* considerations 0))
	  (value value-of-judgment))
      (produce-opinion threshold value)))
  realistic-country?)

(defhandler choose-confidence-threshold
  (lambda (trait country considerations value-of-judgment)
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
