;;;; This is the war file load.scm


(cd "war/propagator")
(load "load")
(cd "..")

(define loaded-filenames
  '("ghelper"
    ;"actions"
    "country"
    ;"strategy"
    "traits"
    "ui"
    "extra"
    "utils"
))

(for-each load loaded-filenames)
;(cd "..")
