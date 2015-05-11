;; Sussman Presentation Day Demo

(load "war/load")
(cd "..")
(cd "krebecca")

(define Canada (create-country 'Canada 
			      "From sea to sea"
			      50
			      80
			      30
			      50
			      60
			      90935100
			      51958
			      35))


(define Russia (create-country 'Russia
			       "Here TV watch you"
			       70
			       40
			       80
			       84
			       82
			       100000000
			       100000
			       128))


(define USA (create-country 'USA
			    "Capitalizing on you since 1783"
			    45
			    90
			    50
			    90
			    90
			    1000000000
			    100000
			    100))


(set-image! canada)

(self-confidence? canada)


