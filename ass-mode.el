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
   '("^\\(.+\\)\:" . 'font-lock-keyword-face)
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

(defun ass-timestamp-to-seconds (timestamp)
  ;; (defvar minutes (string-to-number (nth 1 (split-string (car (split-string timestamp "\\.")) ":" 2))))
  ;; (defvar seconds (string-to-number (nth 2 (split-string (car (split-string timestamp "\\.")) ":" 2))))
  ;; (defvar mseconds (string-to-number (concat "0." (nth 1 (split-string timestamp "\\." 1)))))
  ;; (+
  ;;  (* minutes 60)
  ;;  seconds
  ;;  mseconds
  ;;  )
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
  ;(defvar shifted-seconds (+ (ass-timestamp-to-seconds timestamp) shift-amount))
  ;(defvar hours (floor (/ shifted-seconds 3600)))
  ;(defvar minutes (- shifted-seconds (* hours 3600)))
  ;(defvar seconds)
  (let* (
        (shifted-seconds (+ (ass-timestamp-to-seconds timestamp) shift-amount))
        (hours (floor (/ shifted-seconds 3600)))
        (minutes (floor (/ (- shifted-seconds (* hours 3600)) 60)))
        (seconds (floor (- shifted-seconds (* hours 3600) (* minutes 60))))
        (mseconds (nth 1 (split-string (number-to-string shifted-seconds) "\\.")) )
        )
    ;(print hours)
    ;(print minutes)
    ;(print seconds)
    ;(print mseconds)
    ;shifted-seconds
    (format "%d:%d:%d.%s" hours minutes seconds mseconds)
    )
                                        ; 1. convert to seconds
                                        ; 2. add shift
                                        ; (+)
                                        ; 3. convert back to timestamp
                                        ; (ass-seconds-to-timestamp)
  )

(ass-shift-timestamp "0:24:33.95" 100.5)

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
  (start-process ass-media-player nil ass-media-player "-ss" (ass-get-current-time) (ass-get-video-name))
  )

(defun shift-time (shift-amount)
  (interactive "nEnter shift amount: ")
  (print (ass-shift-timestamp "0:04:27.95" shift-amount))
  )

(add-to-list 'auto-mode-alist '("\\.ass$" . ass-mode))

(defvar ass-mode-map (make-keymap))
(define-key ass-mode-map "\C-c\C-e" 'print-events-list)
(define-key ass-mode-map "\C-c\C-o" 'mplayer)
(define-key ass-mode-map "\C-c\C-l" 'print-debug)
(define-key ass-mode-map "\C-c\C-s" 'shift-time)
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
