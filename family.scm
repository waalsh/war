;;;; A Small Financial Example 

;;; First, we need a small database mechanism
;;;  Parent and child here do not refer to biological
;;;  things, but rather the relationships of parts
;;;  of a database.

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

;;; e.g. (thing-of Gaggle-salary gross-income Ben)

(define (thing-of name-path)
  (let lp ((path name-path))
    (cond ((= (length path) 1) (car path))
	  (else
	   (eq-get (lp (cdr path))
		   (car path))))))



;;; A financial entity has three cells

(define (make-financial-entity entity)
  (eq-put! entity 'kind-of-entity 'financial)

  (let-cells (gross-income expenses net-income)

    (add-branch! entity gross-income 'gross-income)
    (add-branch! entity net-income 'net-income)
    (add-branch! entity expenses 'expenses)

    (c:+ expenses net-income gross-income)
    'done
    ))

(define (financial-entity? thing)
  (eq? (eq-get thing 'kind-of-entity) 'financial))

(define (gross-income entity)
  (assert (financial-entity? entity))
  (eq-get entity 'gross-income))

(define (net-income entity)
  (assert (financial-entity? entity))
  (eq-get entity 'net-income))

(define (expenses entity)
  (assert (financial-entity? entity))
  (eq-get entity 'expenses))

(define (breakdown sum-node . part-names)
  (for-each (lambda (part-name)
	      (let-cell part
			(add-branch! sum-node part part-name)))
	    part-names)
  (cond ((= (length part-names) 2)
	 (c:+ (eq-get sum-node (car part-names))
	      (eq-get sum-node (cadr part-names))
	      sum-node)
	 'done)
	(else
	 (c:+ (eq-get sum-node (car part-names))
	      (let lp ((pieces (cdr part-names)))
		(ce:+ (eq-get sum-node (car pieces)) 
		      (if (< 2 (length pieces))
			  (lp (cdr pieces))
			  (eq-get sum-node (cadr pieces))))) sum-node) 'done)))
	      
(define (combine-financial-entities compound . parts)
  (assert (every financial-entity? parts))
  (cond ((= (length parts) 2)
	 (let ((p1 (car parts)) (p2 (cadr parts)))
	   (c:+ (gross-income p1) (gross-income p2) (gross-income compound))
	   (c:+ (net-income p1) (net-income p2) (net-income compound))
	   (c:+ (expenses p1) (expenses p2) (expenses compound))
	   'done))
	(else (c:+ (gross-income (car parts)) 
		   (let lp ((pieces (cdr parts))) 
		     (ce:+ (gross-income (car pieces)) (if (< 2 (length pieces)) 
							   (lp (cdr pieces))
							   (gross-income (cadr pieces))))) (gross-income compound))
	      (c:+ (net-income (car parts)) 
		   (let lp ((pieces (cdr parts)))
		     (ce:+ (net-income (car pieces)) (if (< 2 (length pieces)) 
							   (lp (cdr pieces))
							   (net-income (cadr pieces))))) (net-income compound))

	      (c:+ (expenses (car parts)) 
		   (let lp ((pieces (cdr parts)))
		     (ce:+ (expenses (car pieces)) (if (< 2 (length pieces)) 
							   (lp (cdr pieces))
							   (expenses (cadr pieces))))) (expenses compound))
	      'done-combining-many-parts)))



 

#|
(initialize-scheduler)

(make-financial-entity 'Alyssa)
(make-financial-entity 'Ben)

;;; Ben and Alyssa are married
(make-financial-entity 'Ben-Alyssa)
(combine-financial-entities 'Ben-Alyssa 'Ben 'Alyssa)

;;; Ben and Alyssa file income tax jointly
(tell! (gross-income 'Ben-Alyssa) 427000 'IRS)

;;; Ben works at Gaggle as a software engineer.
(breakdown (gross-income 'Ben) 'Gaggle-salary 'investments)

;;; He gets paid a lot to make good apps.
(tell! (thing-of '(Gaggle-salary gross-income Ben)) 200000 'Gaggle)

;;; Alyssa works as a PhD biochemist in big pharma.
(breakdown (gross-income 'Alyssa) 'GeneScam-salary 'investments)

;;; Biochemists are paid poorly.
(tell! (thing-of '(GeneScam-salary gross-income Alyssa)) 70000 'GeneScam)

(tell! (thing-of '(investments gross-income Alyssa))
       (make-interval 30000 40000) 'Alyssa)

(inquire (thing-of '(investments gross-income Ben)))
;Value: #(value=#[interval 117000 127000],
;   premises=(alyssa gaggle genescam irs),
;   informants=((-:p gross-income part)))

;;; Ben is a tightwad
(tell! (expenses 'Ben) (make-interval 10000 20000) 'Ben)

(inquire (thing-of '(net-income Ben)))
;Value: #(value=#[interval 297000 317000],
;   premises=(alyssa ben genescam irs),
;   informants=((-:p gross-income expenses)))


;;; But Alyssa is not cheap.  She likes luxury.
(tell! (thing-of '(expenses Alyssa)) (make-interval 200000 215000) 'Alyssa)

(inquire (thing-of '(net-income Alyssa)))
;Value: #(value=#[interval -115000 -90000],
;   premises=(alyssa genescam),
;   informants=((-:p gross-income expenses)))


;;; But they are doing OK anyway!
(inquire (thing-of '(net-income Ben-Alyssa)))
;Value: #(value=#[interval 192000 217000],
;   premises=(alyssa ben irs),
;   informants=((-:p gross-income expenses)))


;;;;;;;;; Let's introduce Kate - she is our test person ;;;;;;;;;

(make-financial-entity 'Junior)

(tell! (net-income 'Junior) (make-interval 100 200))

(tell! (expenses 'Junior) (make-interval 20 30))

(inquire (thing-of '(gross-income Junior)))
;Value: #[interval 120 230]

(make-financial-entity 'Perla)
(make-financial-entity 'Perla-Junior)
(combine-financial-entities 'Perla-Junior 'Perla 'Junior)

(tell! (gross-income 'Perla-Junior) (make-interval 80 400))
(tell! (expenses 'Perla-Junior) (make-interval 25 45))

(inquire (thing-of '(net-income Perla-Junior)))
;Value: #[interval 35 375]

(inquire (thing-of '(net-income Perla)))
;Value: #[interval -165 275]

(inquire (thing-of '(gross-income Perla)))
;Value: #[interval -150 280]

(inquire (thing-of '(expenses Perla)))
;Value: #[interval -5 25]

(inquire (thing-of '(net-income Junior)))
;Value: #(value=#[interval 100 200],

(inquire (thing-of '(gross-income Junior)))
;Value: #[interval 120 230]

(inquire (thing-of '(expenses Junior)))
;Value: #(value=#[interval 20 30],

(inquire (thing-of '(gross-income Perla-Junior)))
;Value: #(value=#[interval 80 400]                ;;; too wide!

(tell! (gross-income 'Perla) (make-interval 50 300))

(inquire (thing-of '(gross-income Perla-Junior)))

;Value: #(value=#[interval 170 400]          ;;; The other party had to be defined (partial)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Harry and Eva are married

(make-financial-entity 'Harry)
(make-financial-entity 'Eva)
(make-financial-entity 'Harry-Eva)
(combine-financial-entities 'Harry-Eva 'Harry 'Eva)

;;; Harry and Eva make 200,000 together according to the IRS

(tell! (gross-income 'Harry-Eva) 200000 'IRS)

;;; That is what Harry and Eva reported. However, Harry holds a small
;;; business on the side.

(breakdown (gross-income 'Harry) 'Fishing-Salary 'Assassin-Salary 'investments)

;; Accounting for it is a tad fishy

(tell! (thing-of '(Assassin-Salary gross-income Harry)) (make-interval 10000 500000) 'Accountant)
(tell! (thing-of '(Fishing-Salary gross-income Harry)) (make-interval 0 40000) 'Accountant)
(tell! (thing-of '(investments gross-income Harry)) (make-interval 0 10000) 'Broker)

;; These is a smashing two-member club in town

(make-financial-entity 'Club)

;; You can join the club by paying a fee that is at least 300 and coming  with a friend.
;; However, to join your personal net income has to be within a certain bracket, 
;; because you are expected to make voluntarily
;; contributions

(define (join-club club . parts)
  (assert (every financial-entity? parts))
  (cond ((= (length parts) 2)
	 (let ((p1 (car parts)) (p2 (cadr parts)))
	   (c:+ 300 300 (gross-income club))
	   (tell! (expenses p1) (make-interval 300 1000000) 'club-fee)
           (tell! (expenses p2) (make-interval 300 1000000) 'club-fee)
           (tell! (net-income p1) (make-interval 200000 1000000) 'club-assumption)
           (tell! (net-income p2) (make-interval 200000 1000000) 'club-assumption)
           (tell! (expenses club) (make-interval 50 100) 'member-support)))
	(else 
	 (error "This is a two member club"))))

(join-club 'Club 'Harry 'Ben)


(inquire (thing-of '(net-income Harry)))
;Value: #(value=#[interval 200000 549700])

(inquire (thing-of '(gross-income Harry)))   ;; will contradict as soon as we define Eva!
;Value: #(value=#[interval 200300 550000])

(inquire (thing-of '(expenses Harry)))
;Value: #(value=300)


;; Eva is a full-time private investigator

(breakdown (gross-income 'Eva) 'Investogator-salary 'investments)

(tell! (thing-of '(investments gross-income Eva)) 100 'Eva)
(tell! (thing-of '(Investogator-salary  gross-income Eva)) 100000 'B&W)

;; Here, if we assume that IRS is correct, we know that either Eva is not making 1000100 or that
;; Harry is either lying to the club or not reporting his earnings to IRS

(inquire (thing-of '(gross-income Harry)))
;Value: #(value=#[interval 200300 550000],
;   premises=(broker club-assumption club-fee accountant),
;   informants=((+:p expenses net-income) (+:p part cell1)))


;; We have no reason not to believe Eva, so let's investigate Harry

;; Let's first retract the accountant information. 

(retract! 'Accountant)

;; If we retract the accountant, we still have a contradiction.

(retract! 'club-assumption)

;; If we retract the club-assumpion, we notice that Ben now has a contradiction in expenses

(inquire (thing-of '(expenses Ben)))

(contradiction (ben club-fee))
;Value: #(value=#[contradictory-interval 10000 300],
;   premises=(ben club-fee),
;   informants=(user))

;;It looks like ben was not entirely transparent about his expenses. let's retract his statement.

(retract! 'ben)

(inquire (thing-of '(expenses Ben)))

;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Notice that this conclusion does not depend on the details, such
;;; as Gaggle or GeneScam!
|#

