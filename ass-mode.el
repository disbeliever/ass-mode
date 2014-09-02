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
(defvar ass-media-player-parameters "")

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
  (let (
        (shifted-seconds (+ (ass-timestamp-to-seconds timestamp) shift-amount))
        )
    (ass-seconds-to-timestamp shifted-seconds)
    )
  )

(defun ass-change-frame-rate (timestamp fps-old fps-new)
  ""
  (let* (
         (factor (/ fps-old fps-new))
         (shifted-seconds (* (ass-timestamp-to-seconds timestamp) factor))
         )
    (ass-seconds-to-timestamp shifted-seconds)
    )
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
  "Вовращает список описания стилей (вместе с форматной строкой сверху)"
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

(defun ass-get-events-list ()
  "Вовращает список описания событий (вместе с форматной строкой сверху)"
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
    (mapcar 'chomp (split-string (nth 1 (split-string format-string ":")) ","))
    )
  )

(defun chomp (str)
  "Chomp leading and tailing whitespace from STR."
  (let ((s (if (symbolp str) (symbol-name str) str)))
    (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" s)))

;; (defun ass-create-list-of-styles-buffer ()
;;   "Creates buffer with list of ASS styles"
;;   (with-output-to-temp-buffer "ASS Styles"
;;     (with-current-buffer "ASS Styles"
;;       (print "Style name")
;;       (ass-mode)))
;;   )

(defun print-events-format ()
  (interactive)
  (print (ass-get-events-format))
  )

(defun print-events-list ()
  (interactive)
  (print (ass-get-events-list))
  )

(defun ass-mplayer ()
  "Run mplayer"
  (interactive)
  (apply 'start-process ass-media-player nil ass-media-player "-ss" (ass-get-current-start-time) (ass-get-video-name) (split-string ass-media-player-parameters " "))
  )

(defun ass-shift-time (shift-amount)
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

(add-to-list 'auto-mode-alist '("\\.ass$" . ass-mode))

(defvar ass-mode-map (make-keymap))
(define-key ass-mode-map "\C-c\C-e" 'print-events-list)
(define-key ass-mode-map "\C-c\C-o" 'ass-mplayer)
(define-key ass-mode-map "\C-c\C-l" 'print-debug)
(define-key ass-mode-map "\C-c\C-s" 'ass-shift-time)
(define-key ass-mode-map "\C-c\C-f" 'ass-change-fps)
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
