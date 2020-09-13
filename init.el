;;;; Pre-startup
;; Prevent GC at startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      site-run-file nil)

;;;; Emacs functionality and defaults
;; pLEaSe ANsWer yEs oR nO.
(fset 'yes-or-no-p 'y-or-n-p)

;; Get rid of all menus
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; Enable pop-up windows
(tooltip-mode 1)

;; Inhibit startup buffer with the Emacs logo, dired in ~
(setq inhibit-startup-screen t)
(setq initial-buffer-choice "~")

;; Upcase/downcase region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Mouse scrolls 1 line/+shift 5 lines/+control full screens
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))

;;; Backups
;; Make backups directory in ~/.emacs.d/
(let ((--backup-directory (concat user-emacs-directory "backups")))
  (if (not (file-exists-p --backup-directory))
      (make-directory --backup-directory t))

  ;; Tell Emacs about the directory
  (setq backup-directory-alist `(("." . ,--backup-directory))))

;; Set a few reasonable defaults
(setq make-backup-files   t
      backup-by-copying   t
      version-control     t
      delete-old-versions t)

;; Turn off bell (Dracula theme makes this do nothing)
(setq visible-bell t)

;; Scroll conservatively
(setq scroll-conservatively 101)

;; Idle update delay
(setq idle-update-delay 1)

;; Disable bidirectional text rendering for performance reasons
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; NO TABS on indent
(setq-default indent-tabs-mode nil)

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
(use-package dracula-theme :straight t)

;; Which Key
(use-package which-key
  :straight t
  :config (which-key-mode))

;; Company Mode
(use-package company-mode
  :straight (company-mode
             :host github
             :repo "company-mode/company-mode")
  :hook prog-mode
  :custom-face
  (company-tooltip ((t (:background "#44475a"
                                    :foreground "#f8f8f2"
                                    :height 0.75)))))

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

(use-package csv-mode
  :straight t
  :defer t)

;; Change scrolling with C-v and M-v to be one line at a time
(global-set-key (kbd "C-v") 'scroll-up-line)
(global-set-key (kbd "M-v") 'scroll-down-line)

;; C-tab to other-window
(global-set-key (kbd "<C-tab>") 'other-window)

;;;; Style
;; Dracula theme
(unless (custom-theme-enabled-p 'dracula)
  (load-theme 'dracula t))

;; No more blinking cursor
(blink-cursor-mode 0)

;; Smaller fringes
(set-fringe-mode '(1 . 1))

;; Matching parentheses
(show-paren-mode t)

;; Box in the whole frame a bit
(set-frame-parameter (selected-frame) 'internal-border-width 5)

;; Tell Emacs the background is dark (default will auto figure it out)
;(setq frame-background-mode 'dark)

;; Default font
(if (find-font (font-spec :name "Iosevka"))
    (set-face-attribute 'default nil
                        :height 140
                        :family "Iosevka"))

;; Set GC back to default values
(setq gc-cons-threshold 800000
      gc-cons-percentage 0.1)
