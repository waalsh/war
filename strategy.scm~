;;; Strategy is a goal-driven approach to diplomacy
;;; Countries develop strategies towards other nations


(define develop-strategy (make-generic-operator 3 'strategy 'default-strategy))

;; There are three types of strategy that we can think of at this moment
;; There are strategies towards self
;; There are strategies towards another country you know
;; There are strategies towards countries you just met

(defhandler strategy self-strategy on-self?)
(defhandler strategy other-strategy on-other?)
(defhandler strategy new-strategy on-new?)

;; A strategy is a tree which explores possible strategic options and
;; chooses one that makes sense

;; whenever we choose a strategy, we put it into a country's 
;; developed traits, along with a train of thought (path)
;; that produced said strategy

;; a strategy tree will have the following layers

;; -----------> See my notebook