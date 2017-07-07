(defconst emacs-ui/layers/install-packages
  '(all-the-icons
    powerline))

(layers/install-packages emacs-ui/layers/install-packages)

(use-package poweline
  :init
  (powerline-default-theme))

(use-package all-the-icons-mode)


