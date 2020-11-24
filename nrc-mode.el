(defun nrc/sanity-check ()
  "Simple message to sanity check functionality."
  (interactive)
  (message "It works!"))

(defun nrc/xterm-mouse-mode ()
  "Set xterm-mouse-mode on with a few extra keybindings."
  (interactive)
  (if (not (display-graphic-p))
      (progn
        (xterm-mouse-mode t)
        (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
        (global-set-key (kbd "<mouse-5>") 'scroll-up-line))))

(defun nrc/find-recent-file ()
  "Find a file in the recentf-list."
  (interactive)
  (find-file (completing-read "Find recent file: " recentf-list)))

(defun nrc/set-iosevka ()
  "If installed on the system, set the default font to Iosevka."
  (interactive)
  (if (find-font (font-spec :name "Iosevka"))
      (set-face-attribute 'default nil
                          :family "Iosevka")))

(defun nrc/resize-fonts ()
  "Resize fonts based on screen proportions.

   This helps when moving from Emacs on the phone screen to the
   monitor, but maintaining the same session throughout,
   especially when using Emacs in daemon mode."
  (interactive)
  (cond ((<= (display-pixel-height) (display-pixel-width))
         (set-face-attribute 'default nil
                             :height 120))
        ((> (display-pixel-height) (display-pixel-width))
         (set-face-attribute 'default nil
                             :height 360))))

(defun nrc/swap-theme (new-theme)
  "Swap all enabled custom themes for a new custom theme."
  (interactive
   (list
    (intern (completing-read "Swap custom theme: "
                             (mapcar #'symbol-name
                                     (custom-available-themes))))))
  (if custom-enabled-themes
      (progn
        (cl-loop for theme in custom-enabled-themes do
                 (disable-theme theme))
        (load-theme new-theme t))
    (load-theme new-theme t)))

(defconst nrc-prefix "C-z"
  "NRC keymap prefix.")

(defvar nrc-keymap
  (let ((map (make-sparse-keymap)))
    (dolist (bind `(("\C-m" . nrc/sanity-check)
                    ("t m" . nrc/xterm-mouse-mode)
                    ("f r" . nrc/find-recent-file)
                    ("\C-z" . suspend-frame)))
      (define-key map (kbd (concat nrc-prefix " " (car bind))) (cdr bind)))
    map)
  "NRC keymap for use in NRC mode.")

;;;###autoload
(define-minor-mode nrc-mode
  "Toggle NRC mode.
NRC mode consists of custom keybindings I like."
  :global t
  :init-value t
  :lighter " NRC"
  :keymap nrc-keymap)

(provide 'nrc-mode)
