;; Doom One
(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-one t)
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
(use-package org-mode
  :straight (:type built-in)
  :defer t)

;; Display line numbers
(use-package display-line-numbers-mode
  :hook prog-mode)

;; Magit
(use-package magit
  :straight t
  :bind (("C-c g" . magit-status)))

;; CSV
(use-package csv-mode
  :straight t
  :defer t)

;; Projectile
(use-package projectile
  :straight t
  :config
  (projectile-mode t))

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
  (avy-setup-default)
  :bind
  ("M-g c" . avy-goto-char))

;; Posframe for positioning frames in preferred places.
(use-package posframe
  :straight t)

;; Posframe ivy integration
(use-package ivy-posframe
  :straight t
  :config
  (ivy-posframe-mode t)
  :custom
  (ivy-posframe-display-functions-alist
   '((swiper          . ivy-posframe-display-at-frame-top-center)
     (complete-symbol . ivy-posframe-display-at-point)
     (counsel-M-x     . ivy-posframe-display-at-frame-top-center)
     (t               . ivy-posframe-display-at-frame-top-center)))
  :after
  (ivy posframe))

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
  (defun centaur-tabs-buffer-groups ()
    "`centaur-tabs-buffer-groups' control buffers' group rules.
      Group centaur-tabs with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
      All buffer name start with * will group to \"Emacs\".
      Other buffer group by `centaur-tabs-get-group-name' with project name."
    (list
     (cond
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
      			magit-status-mode
      			magit-diff-mode
      			magit-log-mode
      			magit-file-mode
      			magit-blob-mode
      			magit-blame-mode
      			)))
       "Emacs")
      ((derived-mode-p 'prog-mode)
       "Editing")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(org-mode
      		    org-agenda-clockreport-mode
      		    org-src-mode
      		    org-agenda-mode
      		    org-beamer-mode
      		    org-indent-mode
      		    org-bullets-mode
      		    org-cdlatex-mode
      		    org-agenda-log-mode
      		    diary-mode))
       "OrgMode")
      (t
       (centaur-tabs-get-group-name (current-buffer))))))
  :hook
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  :bind
  ("<C-tab>" . #'centaur-tabs-forward)
  ("<C-iso-lefttab>" . #'centaur-tabs-backward)
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
  (centaur-tabs-modified-marker "⦿")
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
