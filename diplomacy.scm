;; This will define how we perceive others


(define (retrieve-opinion of-country about-country)
  (let lp ((opinion (dip-opinions of-country)))
    (if (eq? (car (car opinion)) (name about-country))
	(cdr (car opinion))
	(lp (cdr opinion)))))


