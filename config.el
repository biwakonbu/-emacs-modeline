(defvar modeline/icon-height 0.8)

(defun modeline/mode-line ()
  (if (powerline-selected-window-active) 'mode-line 'mode-line-inactive))

(defun modeline/git-icon ()
  (all-the-icons-alltheicon
   "git" :height modeline/icon-height :v-adjust 0 :face 'all-the-icons-lred))
(defun modeline/git-branch-icon ()
  (all-the-icons-octicon
   "git-branch" :height modeline/icon-height :v-adjust 0 :face 'all-the-icons-lgreen))
(defun modeline/git-branch ()
  (let* ((branch (replace-regexp-in-string
                  "[\r\n]+\\'" ""
                  (shell-command-to-string "git symbolic-ref -q HEAD")))
         (mode-line-str (if (string-match "^refs/heads/" branch)
                            (format "%s %s %s" (modeline/git-icon) (modeline/git-branch-icon) (substring branch 11))
                          "")))
    (powerline-raw (format " %s" mode-line-str) 'all-the-icons-lgreen)))
(defun modeline/git ()
  (when (and vc-mode buffer-file-name)
    (let ((backend (vc-backend buffer-file-name)))
      (when backend
	(concat (powerline-raw "[" (modeline/mode-line) 'l)
		(powerline-raw (format "%s / %s" backend (vc-working-revision buffer-file-name backend)))
		(powerline-raw "]" (modeline/mode-line)))))))
(defun modeline/filetype ()
  (let ((file-type (cadr (all-the-icons-match-to-alist
			     (buffer-name)
			     all-the-icons-icon-alist))))
    (if (string= "file-o" file-type)
	(format "%s %s"
		(custom-modeline-major-mode-icon)
		(capitalize (replace-regexp-in-string "-mode$" "" (format "%s" major-mode))))
      (format "%s %s"
	      (custom-modeline-major-mode-icon)
	      (capitalize file-type)))))

(defun custom-modeline-buffer-size ()
  (format " %s " (powerline-buffer-size nil 'I)))
(defun custom-modeline-major-mode-icon ()
  (all-the-icons-icon-for-mode
   major-mode :height modeline/icon-height :v-adjust 0))

(defun custom-modeline-buffername ()
  (powerline-raw (format " %s" (buffer-name))))
		 

(defun custom-modeline-line-and-column-number ()
  (propertize " %l:%3c" (line-number-at-pos) (current-column)))

(defface modeline/face-active1
  '((t (:foreground "gray20" :background "#6094ca" :box '(:color "#6094ca") :inherit mode-line :bold t)))
  "modeline face 1."
  :group 'poweline)
(defface modeline/face-active2 '((t (:background "gray40" :inherit mode-line)))
  "modeline face 2."
  :group 'powerline)
(defface modeline/face-inactive1 '((t (:background "gray11" :inherit mode-line-inactive)))
  "modeline face 1."
  :group 'powerline)
(defface modeline/face-inactive2 '((t (:background "gray20" :inherit mode-line-inactive)))
  "modeline face 2."
  :group 'powerline)
(defface modeline/mode-line-buffer-id-inactive
  '((t (:inherit mode-line-buffer-id)))
  "modeline mode-line face"
  :group 'powerline)

(set-face-attribute 'mode-line nil
		    :foreground "gray85"
		    :bold t
		    :background "gray15"
		    :box '(:line-width 4 :color "gray15")
		    :height 105)
(setq-default mode-line-format
	      '("%e"
		(:eval
		 (let* ((active (powerline-selected-window-active))
			(mode-line-buffer-id
			 (if active 'mode-line-buffer-id 'modeline/mode-line-buffer-id-inactive))
			(mode-line (if active 'mode-line 'mode-line-inactive))
			(face1 (if active 'modeline/face-active1 'modeline/face-inactive1))
			(face2 (if active 'modeline/face-active2 'modeline/face-inactive2))
			(separator-left (intern (format "powerline-%s-%s"
							(powerline-current-separator)
							(car powerline-default-separator-dir))))
			(separator-right (intern (format "powerline-%s-%s"
							 (powerline-current-separator)
							 (cdr powerline-default-separator-dir))))
			(lhs (list
			      (powerline-raw (custom-modeline-buffer-size) face1)
			      (powerline-raw (custom-modeline-buffername))
			      (powerline-raw (modeline/git-branch))))
			(center (list
				 (powerline-raw (custom-modeline-line-and-column-number) 'l)))
			(rhs (list
			      (powerline-raw (modeline/filetype)))))
		   (concat
		    (powerline-render lhs)
		    (powerline-fill-center mode-line (powerline-width center))
		    (powerline-render center)
		    (powerline-fill mode-line (powerline-width rhs))
		    (powerline-render rhs))))))
