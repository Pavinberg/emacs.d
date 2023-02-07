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

(when *is-a-mac*
  (use-package all-the-icons
	:ensure t
	:if (display-graphic-p)
	:config
	(defun custom-modeline-mode-icon ()
	  (format " %s"
			  (propertize icon
						  'help-echo (format "Major-mode: `%s`" major-mode)
						  'face `(:height 1.2 :family ,(all-the-icons-icon-family-for-buffer)))))
	
	(defun -custom-modeline-github-vc ()
	  (let ((branch (mapconcat 'concat (cdr (split-string vc-mode "[:-]")) "-")))
		(concat
		 (propertize (format " %s" (all-the-icons-alltheicon "git")) 'face `(:height 1.2) 'display '(raise -0.1))
		 " · "
		 (propertize (format "%s" (all-the-icons-octicon "git-branch"))
					 'face `(:height 1.3 :family ,(all-the-icons-octicon-family))
					 'display '(raise -0.1))
		 (propertize (format " %s" branch) 'face `(:height 0.9)))))

	(defun -custom-modeline-svn-vc ()
	  (let ((revision (cadr (split-string vc-mode "-"))))
		(concat
		 (propertize (format " %s" (all-the-icons-faicon "cloud")) 'face `(:height 1.2) 'display '(raise -0.1))
		 (propertize (format " · %s" revision) 'face `(:height 0.9)))))

	(defun custom-modeline-icon-vc ()
	  (when vc-mode
		(cond
		 ((string-match "Git[:-]" vc-mode) (-custom-modeline-github-vc))
		 ((string-match "SVN-" vc-mode) (-custom-modeline-svn-vc))
		 (t (format "%s" vc-mode))))))

  ;; ;; Variables configured via the interactive 'customize' interface
  )

(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init-themes)
;;; init-themes.el ends here
