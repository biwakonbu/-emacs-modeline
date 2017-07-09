(defvar modeline/icon-height 1.0)

(defun modeline/mode-line ()
  (if (powerline-selected-window-active) 'mode-line 'mode-line-inactive))

(defun modeline/git-icon ()
  (all-the-icons-alltheicon
   "git" :height modeline/icon-height :v-adjust -0.1 :face 'all-the-icons-lred))
(defun modeline/git-branch-icon ()
  (all-the-icons-octicon
   "git-branch" :height modeline/icon-height :v-adjust 0 :face 'all-the-icons-lgreen))
(defun modeline/git-branch ()
  (let* ((branch (replace-regexp-in-string
                  "[\r\n]+\\'" ""
                  (shell-command-to-string "git symbolic-ref -q HEAD")))
         (mode-line-str (if (string-match "^refs/heads/" branch)
                            (format "%s %s" (modeline/git-branch-icon) (substring branch 11))
                          "")))
    (powerline-raw (format " %s" mode-line-str) 'all-the-icons-lgreen)))

(defun modeline/git ()
  (when (and vc-mode buffer-file-name)
    (let ((backend (vc-backend buffer-file-name)))
      (when backend
	(concat (powerline-raw "[" (modeline/mode-line) 'l)
		(powerline-raw (format "%s / %s" backend (vc-working-revision buffer-file-name backend)))
		(powerline-raw "]" (modeline/mode-line)))))))

(defun custom-modeline-buffer-size ()
  (format " %s" (powerline-buffer-size nil 'I)))
(defun custom-modeline-major-mode-icon ()
  (powerline-raw
   (all-the-icons-icon-for-mode major-mode
				'help-echo (format "Major-mode: `%s`" major-mode)
				'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))
(defun custom-modeline-dir/filename ()
  (powerline-raw (format " %s" (buffer-file-name))))
		      

(defun custom-modeline-line-and-column-number ()
  (propertize " %5l:%3c" (line-number-at-pos) (current-column)))

(setq-default mode-line-format '("%e" (:eval
				       (concat
					(custom-modeline-buffer-size)
					(modeline/git-branch)
					(custom-modeline-dir/filename)
					(custom-modeline-line-and-column-number)
					(custom-modeline-major-mode-icon)
					))))

