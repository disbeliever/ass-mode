(define-derived-mode ass-mode fundamental-mode
  (setq mode-name "ASS/SSA")
  (setq font-lock-defaults '(ass-font-lock-keywords nil t nil nil))
  )


;; (modify-syntax-entry ?; "<")

(defvar ass-font-lock-keywords
  (list
   '("\\;.*" . 'font-lock-comment-face)
   '("^[ \t]*\\[\\(.+\\)\\]" . 'font-lock-type-face)
   '("^.+\:.*" . 'font-lock-keywords-face)
   ))

(defun ass-get-buffer-file-name (ext)
  ""
  (concat (file-name-sans-extension (buffer-file-name))
          ext
          ))

(defun ass-get-video-name ()
  "Construct the name of the video file"
  (cond
   ((file-readable-p (ass-get-buffer-file-name ".mp4")) (ass-get-buffer-file-name ".mp4"))
   ((file-readable-p (ass-get-buffer-file-name ".mkv")) (ass-get-buffer-file-name ".mkv"))
   ((file-readable-p (ass-get-buffer-file-name ".avi")) (ass-get-buffer-file-name ".avi"))
   )
  )

(defun ass-get-current-time ()
  "Get time of the event"
  (nth 1 (split-string (thing-at-point 'line) ","))
  )

(defun mplayer ()
  "Run mplayer"
  (interactive)
  (start-process "*mplayer2" nil "mplayer2" "-ss" (ass-get-current-time) (ass-get-video-name))
  )
(add-to-list 'auto-mode-alist '("\\.ass$" . ass-mode))

(defvar ass-mode-map (make-keymap))
(define-key ass-mode-map "\C-c\C-o" 'mplayer)
(use-local-map ass-mode-map)

(provide 'ass-mode)
