;;;; Pre-startup
;; Force lexical bindings
(setq lexical-binding t)

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

;; Remove scratch message
(setq initial-scratch-message nil)

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

;;;; Style
;; Set Iosevka as default font
(cond
 ((string-equal system-type "gnu/linux")
  (if (find-font (font-spec :name "Iosevka"))
      (cond ((<= (display-pixel-height) (display-pixel-width))
             (set-face-attribute 'default nil
                                 :height 120
                                 :family "Iosevka"))
            ((> (display-pixel-height) (display-pixel-width))
             (set-face-attribute 'default nil
                                 :height 275
                                 :family "Iosevka"))))))

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
(load-file "~/.emacs.d/use-package.el")

;; Set GC back to default values
(setq gc-cons-threshold 800000
      gc-cons-percentage 0.1)
