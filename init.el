;;;; Pre-startup
;; Prevent GC at startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      site-run-file nil)

;;;; Emacs functionality and defaults
;; pLEaSe ANsWer yEs oR nO.
(fset 'yes-or-no-p 'y-or-n-p)

;; Get rid of menus
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Inhibit startup buffer with the Emacs logo
(setq inhibit-startup-screen t)

;; Upcase/downcase region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Turn off bell (Dracula theme makes this do nothing)
(setq visible-bell t)

;; Scroll conservatively
(setq scroll-conservatively 101)

;; Idle update delay
(setq idle-update-delay 1)

;; Mouse scrolls 1 line/+shift 5 lines/+control full screens
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))

;; Disable bidirectional text rendering for performance reasons
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Matching parentheses
(show-paren-mode t)

;; NO TABS on indent
(setq-default indent-tabs-mode nil)

;; Change scrolling with C-v and M-v to be one line at a time
(global-set-key (kbd "C-v") 'scroll-up-line)
(global-set-key (kbd "M-v") 'scroll-down-line)

;;;; Backups
;; Make backups directory in ~/.emacs.d/
(let ((--backup-directory (concat user-emacs-directory "backups")))
  (unless (file-exists-p --backup-directory)
    (make-directory --backup-directory t))

  ;; Tell Emacs about the directory
  (setq backup-directory-alist `(("." . ,--backup-directory))))

;; Set a few reasonable defaults
(setq make-backup-files   t
      backup-by-copying   t
      version-control     t
      delete-old-versions t)

;;;; Packages, hooks, and bindings
;; straight.el bootstrapping
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; use-package setup
(straight-use-package 'use-package)

;; Dracula
(use-package dracula-theme
  :straight t
  :init
  (unless (custom-theme-enabled-p 'dracula)
    (load-theme 'dracula t)))

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
(use-package yasnippet :straight t :defer t)

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
  :defer t)

;; CSV
(use-package csv-mode
  :straight t
  :defer t)

;; Ivy
(use-package ivy
  :straight t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)))

;; Swiper
(use-package swiper
  :straight t
  :bind (("C-s" . swiper)))

;; Counsel
(use-package counsel
  :straight t
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> o" . counsel-describe-symbol)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)))

;; Olivetti writing mode
(use-package olivetti
  :straight t)

;; Set GC back to default values
(setq gc-cons-threshold 800000
      gc-cons-percentage 0.1)
