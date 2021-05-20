;; Doom One
(use-package doom-themes
  :ensure t
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t))

;; Unicode fonts
(use-package unicode-fonts
  :ensure t
  :config (unicode-fonts-setup))

;; All the icons
(use-package all-the-icons
  :ensure t)

;; Which Key
(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  :custom
  (which-key-idle-delay 0))

;; Company Mode
(use-package company
  :ensure t
  :config
  (global-company-mode +1))

;; Flycheck
(use-package flycheck
  :ensure t
  :defer t)

;; YASnippet
(use-package yasnippet
  :ensure t)

;; Magit
(use-package magit
  :ensure t
  :defer t)

;; Dired
(use-package dired-x
  :after dired)

(use-package diredfl
  :ensure t
  :after dired
  :config
  (diredfl-global-mode +1))

(use-package dired-rsync
  :ensure t
  :after dired
  :config
  (bind-key "C-c C-r" 'dired-rsync dired-mode-map))

(use-package fd-dired
  :ensure t
  :after dired)

;; Selectrum
(use-package selectrum
  :ensure t
  :config
  (selectrum-mode +1))

;; CTRLF
(use-package ctrlf
  :ensure t
  :config
  (ctrlf-mode +1))

;; Prescient
(use-package prescient
  :ensure t
  :config
  (prescient-persist-mode +1)
  :custom
  (prescient-history-length 1000))

;; Selectrum-Prescient
(use-package selectrum-prescient
  :ensure t
  :after selectrum prescient
  :config
  (selectrum-prescient-mode +1))

;; Avy
(use-package avy
  :ensure t
  :bind ("C-t" . avy-goto-char))

;; LSP
(use-package lsp-mode
  :ensure t
  :defer t
  :hook (python-mode . lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode)

(use-package lsp-jedi
  :after lsp-mode
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)))

;; VTerm (needs --with-modules compilation support)
(use-package vterm
  :ensure t)

;; Restart Emacs
(use-package restart-emacs
  :ensure t)

;; Recentf
(use-package recentf
  :defer t
  :config (recentf-mode +1))

;; Terraform Mode
(use-package terraform-mode
  :ensure t)

;; YAML Mode
(use-package yaml-mode
  :ensure t)

;; Sly (SBCL/Common Lisp)
(use-package sly
  :ensure t)

;; Auto-update Packages
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))
