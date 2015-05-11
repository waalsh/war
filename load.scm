;;;; This is the war file load.scm


(cd "war/propagator")
(load "load")
(cd "..")

(define loaded-filenames
  '("ghelper"
    ;"actions"
    "country"
    "strategy"
    "traits"
    "ui"
    "models"
    "extra"
    "utils"
	"reasoning"
))

(for-each load loaded-filenames)
;(cd "..")
