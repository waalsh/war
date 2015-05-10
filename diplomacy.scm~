;; This will define how we perceive others


(define (retrieve-opinion of-country about-country)
  (let lp ((opinion (dip-opinions of-country)))
    (if (eq? (car (car opinion)) (name about-country))
	(cdr (car opinion))
	(if (and (pair? opinion) (< 1 (length opinion)))
	    (lp (cdr opinion))
            (if (eq? (car (car (cdr opinion))) (name about-country))
                (cdr (car (car (cdr opinion))))
                '(You are without opinion))))))


