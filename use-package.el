;; Doom One
(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-outrun-electric t)
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
  :config (which-key-mode))

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
;; (use-package org-mode
;;   :straight (:type built-in)
;;   :defer t)
(use-package org-mode
  :straight (org-mode
             :repo "https://code.orgmode.org/bzg/org-mode.git")
  :defer t)

;; Display line numbers
(use-package display-line-numbers-mode
  :hook prog-mode)

;; Magit
(use-package magit
  :straight t)

;; Forge (GitHub, Gitlab, etc. integrations)
(use-package forge
  :straight t
  :after magit)

;; CSV
(use-package csv-mode
  :straight t
  :defer t)

;; Projectile
(use-package projectile
  :straight t
  :config
  (projectile-mode t))

;; Treemacs
(use-package treemacs
  :straight t)

(use-package treemacs-magit
  :after treemacs magit
  :straight t)

(use-package treemacs-projectile
  :after treemacs projectile
  :straight t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :straight t
  :config (treemacs-icons-dired-mode))

;; Ivy
(use-package ivy
  :straight t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (ivy-mode 1)
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)))

;; Swiper
(use-package swiper
  :straight t
  :bind (("C-s" . swiper)))

;; Counsel
(use-package counsel
  :straight t
  :init
  (counsel-mode 1))

;; Avy
(use-package avy
  :straight t
  :config
  (avy-setup-default))

;; Posframe for positioning frames in preferred places.
(use-package posframe
  :straight t)

;; Posframe ivy integration
;; (use-package ivy-posframe
;;   :straight t
;;   :config
;;   (ivy-posframe-mode t)
;;   :custom
;;   (ivy-posframe-display-functions-alist
;;    '((swiper          . ivy-posframe-display-at-frame-top-center)
;;      (complete-symbol . ivy-posframe-display-at-point)
;;      (counsel-M-x     . ivy-posframe-display-at-frame-top-center)
;;      (t               . ivy-posframe-display-at-frame-top-center)))
;;   :after
;;   (ivy posframe))

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
  :straight t)

;; nov.el EPUB reader
(use-package nov.el
  :straight (:type git :repo "https://depp.brause.cc/nov.el.git")
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; Centaur tabs
(use-package centaur-tabs
  :straight t
  :config
  (centaur-tabs-headline-match)
  :hook
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  :bind
  ("C-x <right>" . #'centaur-tabs-forward)
  ("C-x <left>" . #'centaur-tabs-backward)
  ("C-c t t" . centaur-tabs-toggle-groups)
  ("C-c t s" . centaur-tabs-counsel-switch-group)
  ("C-c t p" . centaur-tabs-group-by-projectile-project)
  ("C-c t g" . centaur-tabs-group-buffer-groups)
  :custom
  (uniquify-separator "/")
  (uniquify-buffer-name-style 'forward)
  (centaur-tabs-style "bar")
  (centaur-tabs-height 24)
  (centaur-tabs-set-icons t)
  (centaur-tabs-cycle-scope 'tabs)
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "•")
  (x-underline-at-descent-line t)
  (centaur-tabs-set-bar 'under)
  (centaur-tabs-mode t))

;; LSP basics
(use-package lsp-mode
  :straight t
  :hook (python-mode . lsp-deferred))
(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)
(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)
(use-package lsp-jedi
  :straight (lsp-jedi
             :host github
             :repo "fredcamps/lsp-jedi")
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)))
