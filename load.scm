;;;; This is the war file load.scm

(define loaded-filenames
  '("war/ghelper"
    "war/actions"
    "war/country"
    "war/strategy"
    "war/traits"))

(for-each load loaded-filenames)
