;; Prevent GC at startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      site-run-file nil)

;; Get rid of all menus
(menu-bar-mode 0)
(tool-bar-mode 0)
(tooltip-mode 0)
(scroll-bar-mode 0)

;; Inhibit startup buffer with the Emacs logo
(setq inhibit-startup-screen t)

;; Initial buffer choice
(setq initial-buffer-choice "~/")

;; No more blinking cursor
(blink-cursor-mode 0)

;; Smaller fringes
(set-fringe-mode '(1 . 1))

;; Scroll conservatively
(setq scroll-conservatively 101)

;; Idle update delay
(setq idle-update-delay 1)

;; Disable bidirectional text rendering for performance reasons
(setq-default bidi-display-reordering 'left-to-right
	      bidi-paragraph-direction 'left-to-right)

;; Default font
(set-face-attribute 'default nil
		    :height 105
		    :family "Ubuntu Mono")

;; Matching parentheses
(show-paren-mode t)

;; Upcase/downcase region
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; straight.el
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

;; Enable a maximized frame
(toggle-frame-maximized)

;; Make the frame's default size sensible
(setq default-frame-alist
      (append (list '(width . 90) '(height . 45))))

;;;; Theme
;;; Fonts
;; Use variable width font faces in current buffer
(defun buffer-variable-width-font ()
  "Set font to a variable width (proportional) fonts in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Ubuntu" :height 100 :width semi-condensed))
  (buffer-face-mode))

;; Use mono-spaced font faces in current buffer
(defun buffer-fixed-width-font ()
  "Sets a fixed width (mono-spaced) font in current buffer"
  (interactive)
  (setq buffer-face-mode-face '(:family "Ubuntu Mono" :height 90))
  (buffer-face-mode))

(add-hook 'info-mode-hook 'buffer-variable-width-font)

(set-frame-parameter (selected-frame) 'internal-border-width 5)
(setq x-underline-at-descent-line t)
(unless (eq (car custom-enabled-themes) 'wombat)
  (load-theme 'wombat))

(setq frame-background-mode 'dark)

(setq-default header-line-format
	      '(:eval (format-mode-line
		       (list
			"%b "
			'(:eval (if (and buffer-file-name (buffer-modified-p))
				    (propertize "(modified) "
						'face `(:foreground "#888888"))))
			'(:eval (propertize (format "%s" major-mode)
					    'face `(:foreground "#888888")))))))

(setq-default mode-line-format '(""))

(set-face-attribute 'header-line nil
                    :weight 'extra-light
		    :height 140
		    :background "#242424"
		    :underline "#888888"
		    :family "Ubuntu Condensed")
(set-face-attribute 'mode-line nil
                    :height 3
		    :background "#ffffff"
                    :box nil)
(set-face-attribute 'mode-line-inactive nil
		    :background "#888888"
		    :inherit 'mode-line)
(set-face-attribute 'mode-line-buffer-id nil 
                    :weight 'light)
(set-face-attribute 'fringe nil
		    :background "#242424")

(setq fancy-startup-text '((:face
			    (variable-pitch font-lock-comment-face)
			    "Welcome to Emacs!")))

;; Set GC back to real values
(setq gc-cons-threshold 33554432 ;32mb
      gc-cons-percentage 0.1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(prog-mode-hook
   '(flyspell-prog-mode abbrev-mode linum-mode prettify-symbols-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
