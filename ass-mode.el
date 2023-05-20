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

(require 's)
(require 'seq)

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ssa$" . ass-mode))
(add-to-list 'auto-mode-alist '("\\.ass$" . ass-mode))

;;;###autoload
(define-derived-mode ass-mode fundamental-mode "SSA/ASS"
  "Major mode for editing SSA/ASS ((Advanced) SubStation Alpha) subtitles"
  (setq font-lock-defaults '(ass-font-lock-keywords nil t nil nil))
  )


(make-local-variable 'event-format)

(defvar ass-media-player "mplayer")
(defvar ass-media-player-parameters "-ss")

(defvar ass-font-lock-keywords
  (list
   '("\\;.*" . 'font-lock-comment-face)
   '("^Comment" . 'font-lock-comment-face)
   '("^[ \t]*\\[\\(.+\\)\\]" . 'font-lock-type-face)
   '("^\\(\\w+\\)\:" . 'font-lock-keyword-face)
   ))

(defun mkv-get-tracks (file-name)
  "Получает список дорожек (с целью выцепить потом из них дорожки с сабами)"
  (interactive)
  (process-lines "mkvmerge" "--identify" file-name)
  )

(defun ass-get-buffer-lines ()
  (s-lines (buffer-substring-no-properties (point-min) (point-max)))
  )

(defun ass-get-frame-rate (file-name)
  "Get frame rate of video file. mediainfo is needed"
  (let
      (
       (output (process-lines "mediainfo" file-name))
       )
    (save-match-data
      (let (
            (matched (nth 0 (remove-if (lambda (x) (not (string-match "^Frame rate  .+\:" x))) output)))
            )
        (string-match "^Frame rate  .+\: \\([0-9]+\.[0-9]+\\)" matched)
        (string-to-number (match-string 1 matched))
        )
      )
    )
  )

(defun ass-get-event-parameter-position (parameter)
  "Where is parameter in Format: string"
  (position parameter (ass-get-events-format) :test #'equal)
  )

(defun ass-timestamp-to-seconds (timestamp)
  (let (
        (minutes (string-to-number (nth 1 (split-string (car (split-string timestamp "\\.")) ":" 2))))
        (seconds (string-to-number (nth 2 (split-string (car (split-string timestamp "\\.")) ":" 2))))
        (mseconds (string-to-number (concat "0." (nth 1 (split-string timestamp "\\." 1)))))
        )
    (+
     (* minutes 60)
     seconds
     mseconds
     )
    )
  )

(defun ass-seconds-to-timestamp (sec)
  "Converts number of seconds to timestamp (h:mm:ss.ms)"
  (let* (
        (hours (floor (/ sec 3600)))
        (minutes (floor (/ (- sec (* hours 3600)) 60)))
        (seconds (floor (- sec (* hours 3600) (* minutes 60))))
        (mseconds (nth 1 (split-string (number-to-string sec) "\\.")) )
        )
    (format "%d:%02d:%02d.%s" hours minutes seconds (substring mseconds 0 2))
    )
  )

(defun ass-shift-timestamp (timestamp shift-amount)
  "Calculate new timestamp by shifting it forward/backward"
  (let (
        (shifted-seconds (+ (ass-timestamp-to-seconds timestamp) shift-amount))
        )
    (ass-seconds-to-timestamp shifted-seconds)
    )
  )

(defun ass-change-frame-rate (timestamp fps-old fps-new)
  "Calculate new timestamp considering change in framerate"
  (let* (
         (factor (/ fps-old fps-new))
         (shifted-seconds (* (ass-timestamp-to-seconds timestamp) factor))
         )
    (ass-seconds-to-timestamp shifted-seconds)
    )
)

(defun ass-get-buffer-file-name ()
  (file-name-sans-extension (buffer-file-name))
  )

(defun ass-get-video-name ()
  "Construct the name of the video file for current buffer"
  (let (
        (extensions (list "mp4" "mkv" "avi"))
        )
    (dolist (ext extensions)
      (let ((candidate-filename (concat (ass-get-buffer-file-name) "." ext)))
       (when (file-readable-p candidate-filename)
         (cl-return candidate-filename)
         ))
      )
    )
  )

(defun ass-get-current-start-time ()
  "Get start time of the event under point"
  (nth (ass-get-event-parameter-position "Start") (split-string (thing-at-point 'line) ","))
  )

(defun ass-get-current-end-time ()
  "Get end time of the event under point"
  (nth (ass-get-event-parameter-position "End") (split-string (thing-at-point 'line) ","))
  )


(defun ass-get-styles-format-string ()
  "Format string for style description"
  (interactive)
  (print (nth 1 (split-string (ass-get-styles-list) "\n")))
  )

(defun ass-get-styles-list ()
  "Return styles list (with 'Format:' string)"
  (interactive)
  (save-excursion
    (goto-char 0)
    (let*
        (
         (point-start (search-forward-regexp "\\[V4\\+?.+\\]" nil t))
         (point-end (search-forward-regexp "\\[V4\\+?\\(.*\n\\)*\\[" nil t))
         )
      (buffer-substring-no-properties point-start point-end))
    )
  )

(defun ass-get-styles-names ()
  (seq-filter (apply-partially #'string-match-p "^Style:.+") (ass-get-buffer-lines))
  )

(defun ass-get-events-list ()
  "Return events list (with 'Format:' string)"
  (save-excursion
    (goto-char 0)
    (let*
        (
         (point-start (search-forward-regexp "\\[Events\\]" nil t))
         (point-end (buffer-size))
         )
      (buffer-substring-no-properties point-start point-end)
      )
    )
  )

(defun ass-get-events-format ()
  "Return events format string"
  (let (
        (format-string (nth 1 (split-string (ass-get-events-list) "\n")))
        )
    (mapcar 's-trim (split-string (nth 1 (split-string format-string ":")) ","))
    )
  )

(defun print-events-format ()
  (interactive)
  (print (ass-get-events-format))
  )

(defun print-events-list ()
  (interactive)
  (print (ass-get-events-list))
  )

(defun ass-mplayer ()
  "Run mplayer for event under point"
  (interactive)
  (apply 'start-process
         ass-media-player
         nil
         ass-media-player
         (ass-get-video-name)
         (append
          (split-string ass-media-player-parameters " ")
          (list (ass-get-current-start-time))
          )
         )
  )

(defun ass-shift-time (shift-amount)
  "Calculate new timestamp for event under point by shifting it forward/backward by SHIFT-AMOUNT of seconds"
  (interactive "nEnter shift amount in seconds: ")
  (save-excursion
    (let*
        (
         (start-time (ass-get-current-start-time))
         (end-time (ass-get-current-end-time))
         (shifted-start-time (ass-shift-timestamp start-time shift-amount))
         (shifted-end-time (ass-shift-timestamp end-time shift-amount))
         )
      (beginning-of-line)
      (search-forward start-time)
      (replace-match shifted-start-time)

      (search-forward end-time)
      (replace-match shifted-end-time)
      )
    )
  )

(defun ass-change-fps (fps-old fps-new)
  "Calculate new timestamp for event under point considering change in framerate"
  (interactive
   (list
    (read-number "Old FPS: " (ass-get-frame-rate (ass-get-video-name)))
    (read-number "New FPS: ")
    ))
  (save-excursion
    (let*
      (
       (start-time (ass-get-current-start-time))
       (end-time (ass-get-current-end-time))
       (shifted-start-time (ass-change-frame-rate start-time fps-old fps-new))
       (shifted-end-time (ass-change-frame-rate end-time fps-old fps-new))
       )
      (beginning-of-line)
      (search-forward start-time)
      (replace-match shifted-start-time)

      (search-forward end-time)
      (replace-match shifted-end-time)      
      )
    )
  )

(defun ass-new-entry ()
  (interactive)
  (let*
      (
       (old-start-time (ass-get-current-start-time))
       (old-end-time (ass-get-current-end-time))
       (old-line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
       (new-start-time old-end-time)
       (new-end-time (ass-shift-timestamp new-start-time 2))
       )
    (move-end-of-line 1)
    (newline)
    (princ old-line (current-buffer))
    (save-excursion
      (beginning-of-line)
      (search-forward old-start-time)
      (replace-match new-start-time)

      (search-forward old-end-time)
      (replace-match new-end-time)
      )
    )
  )

(defvar ass-mode-map (make-keymap))
(define-key ass-mode-map "\C-c\C-e" 'print-events-list)
(define-key ass-mode-map "\C-c\C-o" 'ass-mplayer)
(define-key ass-mode-map "\C-c\C-s" 'ass-shift-time)
(define-key ass-mode-map "\C-c\C-f" 'ass-change-fps)
(define-key ass-mode-map "\C-c\C-n" 'ass-new-entry)
(use-local-map ass-mode-map)

(provide 'ass-mode)
