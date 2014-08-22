(require 'ass-mode)

;(ass-get-events-list)
;(defvar test-string)

(ert-deftest ass-timestamp-to-seconds-test()
  ""
  (should
   (equal
    (ass-timestamp-to-seconds "0:24:33.95")
    1473.95
    )
   )
  )


(ert-deftest ass-shift-timestamp-test()
  ""
  (should
   (equal
    (ass-shift-timestamp "0:24:33.95" 100.5)
    "0:26:14.45"
    )
   )
  )
