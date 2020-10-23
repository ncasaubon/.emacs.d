;;;; Pre-startup
;; Force lexical bindings
(setq lexical-binding t)

;; Prevent GC at startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      site-run-file nil)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;;;; Emacs functionality and defaults
;; pLEaSe ANsWer yEs oR nO.
(fset 'yes-or-no-p 'y-or-n-p)

;; Get rid of menus
(menu-bar-mode 0)
(if (display-graphic-p)
    (progn (tool-bar-mode 0)
           (scroll-bar-mode 0)))

;; Inhibit startup buffer with the Emacs logo
(setq inhibit-startup-screen t)

;; Remove scratch message
(setq initial-major-mode nil)
(setq initial-scratch-message nil)

;; Upcase/downcase region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Eshell
(setq eshell-glob-case-insensitive t)
(setq eshell-cmpl-ignore-case t)

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

;; Default Python
(setq python-shell-interpreter "python3")

;; Change scrolling with C-v and M-v to be one line at a time
(global-set-key (kbd "C-v") 'scroll-up-line)
(global-set-key (kbd "M-v") 'scroll-down-line)

;; Redraw display
(global-set-key (kbd "<f5>") 'redraw-display)

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

;;;; Style
;; Set Iosevka as default font
(defun nico/set-iosevka ()
  (interactive)
  (if (find-font (font-spec :name "Iosevka"))
      (set-face-attribute 'default nil
                          :family "Iosevka")))

;; Resize font based on screen height/width sizes
;;  (desktop vs portrait mode phone)
(defun nico/resize-fonts ()
  (interactive)
  (cond ((<= (display-pixel-height) (display-pixel-width))
         (set-face-attribute 'default nil
                             :height 140))
        ((> (display-pixel-height) (display-pixel-width))
         (set-face-attribute 'default nil
                             :height 360))))

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

;; Bring in all packages
(load "~/.emacs.d/use-package.el")

;; Set GC back to default values
(setq gc-cons-threshold 800000
      gc-cons-percentage 0.1)
