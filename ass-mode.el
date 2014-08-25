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

(define-derived-mode ass-mode fundamental-mode
  (setq mode-name "ASS/SSA")
  (setq font-lock-defaults '(ass-font-lock-keywords nil t nil nil))
  )


(setq ass-hash-event-format (make-hash-table :test 'equal))

(defvar ass-media-player "mplayer2")

(defvar ass-font-lock-keywords
  (list
   '("\\;.*" . 'font-lock-comment-face)
   '("^Comment" . 'font-lock-comment-face)
   '("^[ \t]*\\[\\(.+\\)\\]" . 'font-lock-type-face)
   '("^\\(\\w+\\)\:" . 'font-lock-keyword-face)
   ))

(defun mkv-get-tracks (file-name)
  "Получает список дорожек (с целью выцепить потом из них дорожки с сабами)"
  (start-process "mkvmerge" nil "mkvmerge" "--identify" file-name)
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
        (string-match "^Frame rate  .+\: \\([0-9]+\.[0-9]+\\) fps" matched)
        (string-to-number (match-string 1 matched))
        )
      )
    )
  )

(defun ass-get-timestamp-start-n ()
  "Where is start timestamp in Format: string"
  (print ass-style-get-format)
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

(defun ass-shift-timestamp (timestamp shift-amount)
  (let* (
        (shifted-seconds (+ (ass-timestamp-to-seconds timestamp) shift-amount))
        (hours (floor (/ shifted-seconds 3600)))
        (minutes (floor (/ (- shifted-seconds (* hours 3600)) 60)))
        (seconds (floor (- shifted-seconds (* hours 3600) (* minutes 60))))
        (mseconds (nth 1 (split-string (number-to-string shifted-seconds) "\\.")) )
        )
    (format "%d:%02d:%02d.%s" hours minutes seconds mseconds)
    )
  )

(defun ass-change-frame-rate (old new)
  ""
  ; scaleFactor = from / to
  ; shift_mseconds + m_seconds*scaleFactor + 0.5
)

(defun ass-get-buffer-file-name (ext)
  ""
  (concat (file-name-sans-extension (buffer-file-name))
          ext
          ))

(defun ass-get-subtitles-from-mkv (file-name)
  ; TODO: much work here
  (process-lines "mkvmerge" "--identify" file-name)
  )

(defun ass-get-video-name ()
  "Construct the name of the video file"
  (cond
   ((file-readable-p (ass-get-buffer-file-name ".mp4")) (ass-get-buffer-file-name ".mp4"))
   ((file-readable-p (ass-get-buffer-file-name ".mkv")) (ass-get-buffer-file-name ".mkv"))
   ((file-readable-p (ass-get-buffer-file-name ".avi")) (ass-get-buffer-file-name ".avi"))
   )
  )

(defun ass-get-current-start-time ()
  "Get start time of the event under point"
  (nth 1 (split-string (thing-at-point 'line) ","))
  )

(defun ass-get-current-end-time ()
  "Get end time of the event under point"
  (nth 2 (split-string (thing-at-point 'line) ","))
  )


(defun ass-get-styles-format-string ()
  "Format string for style description"
  (interactive)
  (print (nth 1 (split-string (ass-get-styles-list) "\n")))
  )

(defun ass-get-styles-list ()
  "Вовращает список описания стилей (вместе с форматной строкой сверху)"
  (interactive)
  (save-excursion
    (defvar point-start (search-forward-regexp "\\[V4\\+?.+\\]" nil t))
    (goto-char 0)
    (defvar point-end (search-forward-regexp "\\[V4\\+?\\(.*\n\\)*\\[" nil t))
    (buffer-substring-no-properties point-start point-end))
  )

(defun ass-get-events-list ()
  "Вовращает список описания событий (вместе с форматной строкой сверху)"
  ;(interactive)
  (save-excursion
    (defvar point-start (search-forward-regexp "\\[Events\\]" nil t))
    (goto-char 0)
    (defvar point-end (buffer-size))
    (buffer-substring-no-properties point-start point-end))
  )

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

(defun ass-get-events-format (events-string)
  "Return events format string"
  (mapcar 'chomp (split-string (nth 1 (split-string (nth 1 (split-string events-string "\n")) ":")) ","))
  )

(defun ass-create-list-of-styles-buffer ()
  "Creates buffer with list of ASS styles"
  (with-output-to-temp-buffer "ASS Styles"
    (with-current-buffer "ASS Styles"
      (print "Style name")
      (ass-mode)))
  )

(defun print-debug ()
  (interactive)
  (print
   (ass-get-events-format (ass-get-events-list))
   )
  )

(defun print-events-list ()
  (interactive)
  (print (ass-get-events-list))
  )

(defun mplayer ()
  "Run mplayer"
  (interactive)
  (start-process ass-media-player nil ass-media-player "-ss" (ass-get-current-start-time) (ass-get-video-name))
  )

(defun shift-time (shift-amount)
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

(add-to-list 'auto-mode-alist '("\\.ass$" . ass-mode))

(defvar ass-mode-map (make-keymap))
(define-key ass-mode-map "\C-c\C-e" 'print-events-list)
(define-key ass-mode-map "\C-c\C-o" 'mplayer)
(define-key ass-mode-map "\C-c\C-l" 'print-debug)
(define-key ass-mode-map "\C-c\C-s" 'shift-time)
(define-key ass-mode-map "\C-c\C-f" (lambda () (interactive) (print (ass-get-frame-rate (ass-get-video-name)))))
(use-local-map ass-mode-map)

(provide 'ass-mode)

; TODO: mkvmerge -i filename.mkv | grep subtitles
(defun extract-subtitles-from-mkv
  (let ((lang (getenv "LANG")))
    (setenv "LANG" "C")
    (call-process "mkvextract"
                  nil
                  nil       ; буфер для вывода
                  nil
                  "tracks"
                  "/mnt/home/Strike Witches 2-ki [BD] [720p]/Strike Witches 2 - 01 (BD 1280x720 h264 FLAC) [Coalgirls].mkv"
                  "3:/home/nerevar/1.ass")
    (setenv "LANG" lang)))
