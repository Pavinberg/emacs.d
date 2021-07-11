;;; package --- Summary
;;; Commentary:
;;; This file is themes settings.

;;; Code:

(setq custom-nw-file (expand-file-name "custom-nw.el" user-emacs-directory))
(setq custom-gui-file (expand-file-name "custom-gui.el" user-emacs-directory))

(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; set theme
(if (display-graphic-p)
	(progn
	  (setq custom-file custom-gui-file)
	  (use-package doom-themes
		:ensure t
		:config
		;; Global settings (defaults)
		(setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
			  doom-themes-enable-italic t) ; if nil, italics is universally disabled
		(load-theme 'doom-monokai-octagon t)

		;; Enable flashing mode-line on errors
		;; (doom-themes-visual-bell-config)
		;; Enable custom neotree theme (all-the-icons must be installed!)
		;; (doom-themes-neotree-config)
		;; or for treemacs users
		(setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
		(doom-themes-treemacs-config)
		;; Corrects (and improves) org-mode's native fontification.
		(doom-themes-org-config)))
  (progn
	(setq custom-file custom-nw-file)
	(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))
	(add-to-list 'custom-theme-load-path
				 (expand-file-name "themes" user-emacs-directory))
	(load-theme 'monokai-pro t)))
;; ;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init-themes)
;;; init-themes.el ends here
