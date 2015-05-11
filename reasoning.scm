(define-record-type <reasoning>
    (%declare-reason reasoning-question
                     object-of-thought
                     subject-of-thought
                     reasoner
		     reasoning-conclusion
	 	     reasoning-train-of-thought
                     reasoning-quick-label)
   reasoning?
  (reasoning-question get-reasoning-question set-reasoning-question!)
  (object-of-thought get-object-of-thought set-object-of-thought!)
  (subject-of-thought get-subject-of-thought set-subject-of-thought!)
  (reasoner get-reasoner set-reasoner!)
  (reasoning-conclusion get-reasoning-conclusion set-reasoning-conclusion!)
  (reasoning-train-of-thought get-reasoning-train-of-thought set-reasoning-train-of-thought!)
  (reasoning-quick-label get-reasoning-quick-lable set-reasoning-quick-lable!))


(define (declare-reason question object subject thinker conclusion train-of-thought label)
	(%declare-reason question
			 object
                         subject
                         thinker
                         conclusion
                         train-of-thought
                         label))


;; I want to be able to ask questions about strategies

(define current-conversationalist 'none)
(define current-musings '())

(define (initiate-QA country)
  (set! current-conversationalist country)
  (set! current-musings '())
  `(Hello - I am Canada. How may I help you?))

;(define (end-QA country))


(define ask (make-generic-operator 1 'default 'ask-me-something)) 
; '(He who knoweth the preachings of GJS doeth have all of the answers.)))

(define (question-about-status? question) (memq 'status question))
(define (question-about-me? question) (or (memq 'you question) (memq 'your question)))
(define (am-i-in-play? country-nick) (memq country-nick countries-in-play))
(define (clarification-question? question) (memq 'why question))

(define (find-question-object question)
  (let lp ((words question))
    (if (pair? words)
	(if (am-i-in-play? (car words))
	    (car words)
	    (lp (cdr words)))
	'none)))

(define (find-question-subject question)
  (let lp ((words question)
	   (counter 0))
    (if (pair? words)
	(cond ((and (am-i-in-play? (car words)) (> counter 0)) (car words))
	      ((am-i-in-play? (car words)) (lp (cdr words) (+ counter 1)))
	      (else (lp (cdr words) counter)))
	'none)))
	    

(defhandler ask
  (lambda (question)
     (if (question-about-me? question)
	 (let ((question-object (find-question-object question)))
	   (let ((musings (hash-table-ref (strategy current-conversationalist) question-object)))
             (set! current-musings musings)
	     (pp `(I think that
		   ,(get-object-of-thought musings) 
		   is 
		   ,(get-reasoning-conclusion musings)
		   with respect to me))))
	 (let ((question-object (find-question-object question))
	       (question-subject (find-question-subject question)))
	   (let ((musings (hash-table-ref (deduction current-conversationalist) 
					  `(,question-subject ,question-object))))
             (set! current-musings musings)
	     (pp `(I think that ,(get-subject-of-thought musings) thinks that
                   ,(get-object-of-thought musings)
		   is
		   ,(get-reasoning-conclusion musings)
		   with respect to
		   it))))))
	 
  question-about-status?)


(define read-musing (make-generic-operator 1 'deliberations 'Dissappear))
(define (musing-list? musing) (and (pair? musing) (reasoning? (car musing))))
(define (musing-reasoning? musing) (reasoning? musing))
(define (musing-symbol? musing) (and (pair? musing) (symbol? (car musing))))

(defhandler read-musing
  (lambda (musings)
    (set! current-musings '())
    (for-each (lambda (musing) (set! current-musings 
                                     (append current-musings 
                                             (get-reasoning-train-of-thought musing)))
		      (pp `(,(get-reasoner musing) thinks that in the question of ,(get-reasoning-question musing)
			       ,(get-object-of-thought musing)
			       is ,(get-reasoning-conclusion musing)
			       than ,(get-subject-of-thought musing))))
	      musings)
    )
  musing-list?)


(defhandler read-musing
  (lambda (musing)
    (set! current-musings (get-reasoning-train-of-thought musing))
    (pp `(,(get-reasoner musing) thinks that in the question of ,(get-reasoning-question musing)
	     ,(get-object-of-thought musing)
	     is ,(get-reasoning-conclusion musing)
	     as compared to ,(get-subject-of-thought musing))))      
    musing-reasoning?)


(defhandler read-musing
  (lambda (musings)
    (set! current-musings (snark-em-up (random 7)))
    (pp musings))
  musing-symbol?)

(defhandler ask
  (lambda (question)
     (read-musing current-musings))
  clarification-question?)

(define (snark-em-up number)
   (cond ((= number 0) '(You are a person of many curiosities))
         ((= number 1) '(My god when is your mother coming.))
         ((= number 2) '(Professor Sussman, can you get this one?))
         ((= number 3) '(Ask God.))
         ((= number 4) '(Is there a reason why you cannot figure it out?))
         ((= number 5) '(I did everything that I could for you.))
         ((= number 6) '(Curiosity killed the cat.))
         ((= number 7) '(Read The Art of War by Sun Tzu. He knows what he is talking about.))))


;; What is your strategy with respect to Russia?
;; ---> Builds tree, reads you the top [current-conversation]

;; I want to be able to ask questions about reasons behind strategies



;(ask '(What is your status with respect to Russia))
;(ask '(What is USA status as compared to Russia))
;(ask '(What status does Russia think it is as compared to USA))







