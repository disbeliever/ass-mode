(define-derived-mode ass-mode fundamental-mode
  (setq mode-name "ASS/SSA")
  (setq font-lock-defaults '(ass-font-lock-keywords nil t nil nil))
  )


(setq ass-hash-event-format (make-hash-table :test 'equal))

(defvar ass-media-player "mplayer2")

(defvar ass-font-lock-keywords
  (list
   '("\\;.*" . 'font-lock-comment-face)
   '("^[ \t]*\\[\\(.+\\)\\]" . 'font-lock-type-face)
   '("^.+\:.*" . 'font-lock-keywords-face)
   ))

(defun mkv-get-tracks (file-name)
  "Получает список дорожек (с целью выцепить потом из них дорожки с сабами)"
  (start-process "mkvmerge" nil "mkvmerge" "--identify" file-name)
  )

(defun ass-frame-rate-mkv (file-name)
  "Get frame rate of mkv file. mediainfo is needed"
  (interactive)
  (defvar output (process-lines "mediainfo" file-name))
  (print output)
  )

(defun ass-get-frame-rate ()
  ; Try to get frame-rate from video file (if it exists)
  )

(defun ass-get-timestamp-start-n ()
  "Where is start timestamp in Format: string"
  (print ass-style-get-format)
  )

(defun ass-shift-timestamp (timestamp shift-amount)
  ; (parse-time-string "0:04:27.95")
  ; 1. convert to seconds
  ; 2. convert seconds to time
  ;(seconds-to-time)
  ; 3. add shift
  ; (time-add)
  ; 4. convert to string
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

(defun ass-get-current-time ()
  "Get time of the event under point"
  (nth 1 (split-string (thing-at-point 'line) ","))
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
  (interactive)
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

(defun ass-get-events-format ()
  "Return events format string"
  (mapcar 'chomp (split-string (nth 1 (split-string (nth 1 (split-string (ass-get-events-list) "\n")) ":")) ","))
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
   (ass-get-events-format)   
   )
  )

(defun print-events-list ()
  (interactive)
  (print (ass-get-events-list))
  )

(defun mplayer ()
  "Run mplayer"
  (interactive)
  (start-process ass-media-player nil ass-media-player "-ss" (ass-get-current-time) (ass-get-video-name))
  )
(add-to-list 'auto-mode-alist '("\\.ass$" . ass-mode))

(defvar ass-mode-map (make-keymap))
(define-key ass-mode-map "\C-c\C-e" 'print-events-list)
(define-key ass-mode-map "\C-c\C-o" 'mplayer)
(define-key ass-mode-map "\C-c\C-l" 'print-debug)
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
