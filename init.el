;;;; Pre-startup
;; Force lexical bindings
(setq lexical-binding t)

;; Prevent GC at startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      site-run-file nil)

;; Prefer the newer of *.el or corresponding *.elc bytecode elisp
;; files.
(setq load-prefer-newer t)

;;;; Emacs functionality and defaults
;; pLEaSe ANsWer yEs oR nO.
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable all disabled commands
(setq disabled-command-function nil)

;; Get rid of menus
(menu-bar-mode 0)
(if (display-graphic-p)
    (progn (tool-bar-mode 0)
           (scroll-bar-mode 0)))

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Eshell
(setq eshell-cmpl-ignore-case t)

;; SBCL
(setq inferior-lisp-program "sbcl")

;; Turn off bell
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

;; Default Python
(setq python-shell-interpreter "python3")

;; Change scrolling with C-v and M-v to be one line at a time
(global-set-key (kbd "C-v") 'scroll-up-line)
(global-set-key (kbd "M-v") 'scroll-down-line)

;; Other window
(global-set-key (kbd "M-o") 'other-window)

;; Redraw display
(global-set-key (kbd "<f5>") 'redraw-display)

;; Ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

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

;; NRC Mode
(load "~/.emacs.d/nrc-mode.el")

;;;; Packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Bring in all packages
(load "~/.emacs.d/use-package.el")

;; Starting theme
(load-theme 'modus-operandi t)

;; Set GC back to default values
(setq gc-cons-threshold 800000
      gc-cons-percentage 0.1)

(custom-set-variables
 '(package-selected-packages
   '(auto-package-update company unicode-fonts sly yaml-mode terraform-mode restart-emacs vterm lsp-ui lsp-mode avy selectrum-prescient prescient ctrlf selectrum fd-dired dired-rsync diredfl magit org-mode yasnippet flycheck company-mode which-key all-the-icons doom-themes use-package)))
