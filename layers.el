(defconst emacs-ui/layers/install-packages
  '(all-the-icons
    spaceline))

(layers/install-packages emacs-ui/layers/install-packages)

(use-package all-the-icons)
;;(require 'all-the-icons)
(use-package spaceline-config
  :config (spaceline-emacs-theme))
