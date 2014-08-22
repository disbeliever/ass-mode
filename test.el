;; Emacs mode for editing ASS/SSA files
;; Copyright (C) 2011-2014  disbeliever
;; URL: https://github.com/disbeliever/ass-mode

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
