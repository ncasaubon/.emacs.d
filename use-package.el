;; Modus Operandi/Vivendi
(use-package modus-operandi-theme
  :straight t
  :defer t)

(use-package modus-vivendi-theme
  :straight t
  :config
  (load-theme 'modus-vivendi t))

;; Doom One
(use-package doom-themes
  :straight t
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :defer t)

;; Unicode fonts
(use-package unicode-fonts
  :straight t
  :config (unicode-fonts-setup))

;; All the icons
(use-package all-the-icons
  :straight t)

;; Which Key
(use-package which-key
  :straight t
  :config
  (which-key-mode)
  :custom
  (which-key-idle-delay 0.3))

;; Company Mode
(use-package company-mode
  :straight (company-mode
             :host github
             :repo "company-mode/company-mode")
  :hook prog-mode)

;; Flycheck
(use-package flycheck
  :straight t)

;; YASnippet
(use-package yasnippet
  :straight t)

;; Org
(use-package org-mode
  :straight (org-mode
             :repo "https://code.orgmode.org/bzg/org-mode.git")
  :defer t
  :custom
  (org-directory "~/.org/"))

;; Org bullets
(use-package org-bullets
  :straight t
  :after org
  :hook (org-mode . org-bullets-mode))

;; Org roam
(use-package org-roam
  :straight t
  :after org
  :custom
  (org-roam-directory "~/.org/org-roam/"))

;; Org noter
(use-package org-noter
  :straight t
  :after org)

;; Display line numbers
(use-package display-line-numbers-mode
  :straight (:type built-in)
  :hook prog-mode)

;; Magit
(use-package magit
  :straight t
  :defer t)

;; CSV
(use-package csv-mode
  :straight t
  :defer t)

;; Projectile
(use-package projectile
  :straight t
  :config
  (projectile-mode t)
  :custom
  (projectile-completion-system 'selectrum-read)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; Dired
(use-package dired-x
  :straight (:type built-in)
  :after dired)

(use-package all-the-icons-dired
  :straight t
  :after dired)

(use-package diredfl
  :straight t
  :after dired
  :config
  (diredfl-global-mode +1))

(use-package dired-rsync
  :straight t
  :after dired
  :config
  (bind-key "C-c C-r" 'dired-rsync dired-mode-map))

(use-package fd-dired
  :straight t
  :after dired)

;; Selectrum
(use-package selectrum
  :straight t
  :config
  (selectrum-mode +1))

;; CTRLF
(use-package ctrlf
  :straight t
  :config
  (ctrlf-mode +1))

;; Prescient
(use-package prescient
  :straight t)

;; Selectrum-Prescient
(use-package selectrum-prescient
  :straight t
  :config
  (selectrum-prescient-mode +1))

;; Avy
(use-package avy
  :straight t
  :config
  (avy-setup-default))

;; Bufler
(use-package bufler
  :straight t
  :bind ("C-x C-b" . bufler))

;; Ace Window
(use-package ace-window
  :straight t
  :bind
  ("M-o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Olivetti writing mode
(use-package olivetti
  :straight t
  :defer t)

;; ESXML-Query (nov.el dependency)
(use-package esxml
  :straight t
  :defer t)

;; nov.el EPUB reader
(use-package nov.el
  :straight (:type git :repo "https://depp.brause.cc/nov.el.git")
  :after esxml
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; LSP
(use-package lsp-mode
  :straight t
  :defer t
  :hook (python-mode . lsp-deferred))
(use-package lsp-ui
  :straight t
  :after lsp-mode
  :commands lsp-ui-mode)
(use-package lsp-jedi
  :straight (lsp-jedi
             :host github
             :repo "fredcamps/lsp-jedi")
  :after lsp-mode
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)))

;; Doom Modeline
(use-package doom-modeline
  :straight t
  :hook (window-setup . doom-modeline-mode))

;; VTerm (needs --with-modules compilation support)
(use-package vterm
  :straight t)

;; Restart Emacs
(use-package restart-emacs
  :straight t)
