(define-record-type <reasoning>
    (%declare-reason reasoning-question
                     object-of-thought
		     reasoning-conclusion
	 	     reasoning-train-of-thought
                     reasoning-quick-label)
   reasoning?
  (reasoning-question get-reasoning-question set-reasoning-question!)
  (object-of-thought get-object-of-thought set-object-of-thought!)
  (reasoning-conclusion get-reasoning-conclusion set-reasoning-conclusion!)
  (reasoning-train-of-thought get-reasoning-train-of-thought set-reasoning-train-of-thought!)
  (reasoning-quick-label get-reasoning-quick-lable set-reasoning-quick-lable!))


(define (declare-reason question object conclusion train-of-thought)
	(%declare-reason question
			 object
                         conclusion
                         train-of-thought
                         '()))







