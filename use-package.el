;; Doom One
(use-package doom-themes
  :straight t
  :config
  (load-theme 'modus-operandi t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t))

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
  :straight t
  :defer t)

;; YASnippet
(use-package yasnippet
  :straight t)

;; Org
(use-package org-mode
  :straight (org-mode
             :repo "https://code.orgmode.org/bzg/org-mode.git")
  :defer t
  :custom
  (org-directory "~/.org/")
  (org-ditaa-jar-path "/usr/bin/ditaa"))

;; Display line numbers
(use-package display-line-numbers-mode
  :straight (:type built-in)
  :hook prog-mode)

;; Magit
(use-package magit
  :straight t
  :defer t)

;; Dired
(use-package dired-x
  :straight (:type built-in)
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
  :straight t
  :config
  (prescient-persist-mode +1)
  :custom
  (prescient-history-length 1000))

;; Selectrum-Prescient
(use-package selectrum-prescient
  :straight t
  :after selectrum prescient
  :config
  (selectrum-prescient-mode +1))

;; Avy
(use-package avy
  :straight t
  :bind ("C-t" . avy-goto-char))

;; Bufler
(use-package bufler
  :straight t
  :bind ("C-x C-b" . bufler))

;; LSP
(use-package lsp-mode
  :straight t
  :defer t
  :hook (python-mode . lsp-deferred)
  :custom
  (lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

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

;; VTerm (needs --with-modules compilation support)
(use-package vterm
  :straight t)

;; Restart Emacs
(use-package restart-emacs
  :straight t)

;; Recentf
(use-package recentf
  :straight (:type built-in)
  :config (recentf-mode +1))
