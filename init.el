;;; package --- Summary
;;; Commentary:
;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

;;; Code:
;; (setq debug-on-error t)

(let ((minver "25.1"))
  (when (version< emacs-version minver)
	(error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(setq normal-gc-cons-threshold (* 20 1024 1024))
(let ((init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
			(lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Bootstrap config
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;; Allow users to provide an optional "init-preload-local.el"
(require 'init-preload-local nil t)

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "elpa/use-package-2.4.1/")
  (require 'use-package))

;; ===========================================
;; Basic Customization (in init-preload-local)
;; ===========================================

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  ("C-s" . 'swiper-isearch)
  ("M-x" . 'counsel-M-x)
  ("C-x C-f" . 'counsel-find-file)
  ("M-y" . counsel-yank-pop)
  ("C-x b" . 'ivy-switch-buffer)
  ("C-c v" . 'ivy-push-view)
  ("C-c V" . 'ivy-pop-view)
  ("<f1> f" . 'counsel-describe-function)
  ("<f1> v" . 'counsel-describe-variable)
  ("<f1> i" . 'counsel-info-lookup-symbol))

;; avy
(use-package avy
  :bind (("C-j C-SPC" . avy-goto-word-1))
  :ensure t)

(use-package company
  :ensure t
  :config
  (global-company-mode t)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-show-numbers t) ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence)))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :ensure t)

(use-package flycheck
  :init (global-flycheck-mode)
  :hook
  (prog-mode . flycheck-mode)
  :config
  (setq flycheck-clang-language-standard "c++11")
  :ensure t)

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode))

(use-package projectile
  :init
  (use-package counsel-projectile
		  :ensure t
		  :config (counsel-projectile-mode))
  :bind (("C-c p" . projectile-command-map))
  :ensure t)

;; slime
(setq inferior-lisp-program "sbcl")

;; lisp-mode
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"
		lsp-file-watch-threshold 500
		lsp-prefer-flymake nil)
  :hook ((c-mode . lsp)
		 (c++-mode . lsp)
		 (python-mode . lsp)
		 (rust-mode . lsp)
		 ;; if you want which-key integration
		 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :custom (lsp-headerline-breadcrumb-enable t)
  :ensure t)

(use-package lsp-ui
  :ensure t
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package lsp-ivy
  :ensure t
  :after (lsp-mode))

(use-package rust-mode
  :ensure t
  :bind ("C-c C-c" . rust-run))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package which-key
  :config (which-key-mode)
  :ensure t)

(use-package highlight-symbol
  :ensure t
  :init (highlight-symbol-mode))

(use-package company-tabnine
  :init (add-to-list 'company-backends #'company-tabnine)
  :ensure t)

;; My mode about CALPUFF
;; (load-file "~/.emacs.d/mymode/inp-mode.el")
;; (add-to-list 'auto-mode-alist '("\\.inp\\'" . inp-mode))

;; multiple-cursors
(use-package multiple-cursors
  :bind (("M-s M-e" . mc/edit-lines))
  :ensure t)

;; Python
(require 'init-python)

;; rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :ensure t)

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :config
  (setq dashboard-banner-logo-title "Coding is happening")
  (setq dashboard-projects-backend 'projectile)
  ;; (setq dashboard-startup-banner "/path/to/image")
  (setq dashboard-items '((recents  . 5)
						  (bookmarks . 5)
						  (projects . 10)))
      (dashboard-setup-startup-hook))

;; sml-mode -- smart mode line
(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t)  ; avoid asking when startup
  (sml/setup))

;; SSH remote
;; (defun connect-homeserver ()
;;   (interactive)
;;   (dired "/ssh:pavin@192.168.1.120:/home/pavin/Code/"))
;; (defun connect-ubuntu ()
;;   (interactive)
;;   (dired "/ssh:pavin@172.16.172.133:/home/pavin/Code/"))
;; (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

;; set theme
(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(load-theme 'monokai-pro t)

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;;; init.el ends here
