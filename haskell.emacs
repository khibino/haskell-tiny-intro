;;;
(set-language-environment "Japanese")

;;;
(set-default 'next-line-add-newlines t)
(setq visible-bell t)
(setq inhibit-startup-screen t)

;;; for color terminal
(custom-set-variables '(frame-background-mode 'dark))
(defvar ansi-color-for-comint-mode t)

;;; paren
(require 'paren)
(set-face-foreground 'show-paren-match "DarkRed")
(set-face-foreground 'show-paren-mismatch "DarkGreen")
(show-paren-mode)

;;; mode-line
(line-number-mode 1)
(column-number-mode 1)

;;; haskell
(add-hook 'haskell-mode-hook
	  (lambda ()
	    (turn-on-haskell-indentation)
	    ;; (interactive-haskell-mode)
	    (set (make-local-variable 'dabbrev-case-fold-search) nil)
	    (set-face-foreground 'font-lock-type-face "PaleGreen")
	    (set-face-foreground 'font-lock-function-name-face "LightSkyBlue")
	    (define-key haskell-mode-map (kbd "C-c a")
	      'HASKELL-CUSTOM:align-record-or-type-signature)
	    ))
