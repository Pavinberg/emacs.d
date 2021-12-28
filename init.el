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
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages

;; Allow users to provide an optional "init-preload-local.el"
(require 'init-preload-local nil t)
(require 'init-themes)

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  ;; (add-to-list 'load-path "elpa/use-package-2.4.1/")
  (require 'use-package))

(require 'init-latex)
(require 'init-chinese-word-segment)
;; ===========================================
;; Basic Customization (in init-preload-local)
;; ===========================================

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (counsel-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq search-default-mode #'char-fold-to-regexp)
  (setq ivy-count-format "(%d/%d) ")
  :bind
  (("C-s" . 'swiper)
   ("C-x b" . 'ivy-switch-buffer)
   ("C-c v" . 'ivy-push-view)
   ("C-c s" . 'ivy-switch-view)
   ("C-c V" . 'ivy-pop-view)
   ("C-x C-@" . 'counsel-mark-ring); 在某些终端上 C-x C-SPC 会被映射为 C-x C-@，比如在 macOS 上，所以要手动设置
   ("C-x C-SPC" . 'counsel-mark-ring)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)))

(use-package amx
  :ensure t
  :init (amx-mode))

(use-package embark
  :ensure t
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings))) ;; alternative for `describe-bindings'

(use-package avy
  :ensure t
  :config
  (defun avy-action-embark (pt)
	(unwind-protect
		(save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
	t)
  (setf (alist-get ?e avy-dispatch-alist) 'avy-action-embark)
  :bind
  (("C-j C-SPC" . avy-goto-char-timer)))

(use-package marginalia
  :ensure t
  :init (marginalia-mode)
  :bind (:map minibuffer-local-map
			  ("M-A" . marginalia-cycle)))

(use-package exec-path-from-shell
  :ensure t
  :init
  (when (memq window-system '(mac ns x))
	(exec-path-from-shell-initialize)))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . 'ace-window)))

(use-package multiple-cursors
  :bind
  ("C-S-<mouse-1>" . mc/toggle-cursor-on-click))

(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

(use-package company
  :ensure t
  :init (global-company-mode)
  :config
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (setq company-idle-delay 0.0)
  (setq company-show-numbers t) ;; Number the candidates (use M-1, M-2 etc to select completions).
  (setq company-selection-wrap-around t)
  (setq company-transformers '(company-sort-by-occurrence)))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :ensure t
  ;; :init (global-flycheck-mode)
  :hook
  (prog-mode . flycheck-mode)
  (c++-mode-hook . (lambda () (setq flycheck-clang-language-standard "c++17"))))

(use-package yasnippet
  :ensure t
  :init (yas-global-mode))

(use-package dashboard
  :ensure t
  :diminish dashboard-mode
  :config
  (setq dashboard-banner-logo-title "Coding is happening")
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-startup-banner 'official)
  (setq dashboard-items '((recents  . 5)
						  (bookmarks . 5)
						  (projects . 10)))
  (dashboard-setup-startup-hook) ;; TODO some bug here
  )

(use-package projectile
  :ensure t
  :bind (("C-c p" . projectile-command-map))
  :config
  (setq projectile-mode-line "Projectile")
  (defadvice projectile-project-root (around ignore-remote first activate)
	(unless (file-remote-p default-directory) ad-do-it)))

(use-package counsel-projectile
  :ensure t
  :after (projectile)
  :init (counsel-projectile-mode))

;; slime
(setq inferior-lisp-program "sbcl")

(use-package all-the-icons)

(use-package yaml-mode)

(defun enable-lsp-if-not-remote ()
  (unless (file-remote-p default-directory) (lsp)))

;; lisp-mode
(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l"
		lsp-file-watch-threshold 500)
  ;; lsp-prefer-flymake nil)
  :hook ((c-mode . enable-lsp-if-not-remote)
		 (c++-mode . (lambda () (unless (file-remote-p default-directory) (lsp))))
		 (python-mode . enable-lsp-if-not-remote)
		 (rust-mode . lsp)
		 ;; if you want which-key integration
		 (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  ;; :config
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-tramp-connection "pyls")
  ;; 					:major-modes '(python-mode)
  ;; 					:remote? t
  ;; 					:server-id 'pyls-remote))
  :custom (lsp-headerline-breadcrumb-enable t))

(use-package lsp-ui
  :ensure t
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-doc-position 'at-point))

(use-package lsp-ivy
  :ensure t
  :after (lsp-mode))

(use-package dap-mode
  :ensure t
  :commands dap-debug
  :config
  (require 'dap-gdb-lldb)
  (dap-ui-mode 1))

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ;; ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package lsp-treemacs
  :ensure t
  :after (treemacs lsp))

(use-package rust-mode
  :ensure t
  :bind ("C-c C-c" . rust-run))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

(use-package which-key
  :ensure t
  :init (which-key-mode))

(use-package highlight-symbol
  :ensure t
  :init (highlight-symbol-mode)
  :bind ("<f3>" . highlight-symbol))

(use-package company-tabnine
  :ensure t
  :init (add-to-list 'company-backends #'company-tabnine))

;; My mode about CALPUFF
;; (load-file "~/.emacs.d/mymode/inp-mode.el")
;; (add-to-list 'auto-mode-alist '("\\.inp\\'" . inp-mode))

;; multiple-cursors
(use-package multiple-cursors
  :ensure t
  :bind (("M-s M-e" . mc/edit-lines)))

;; Python
(require 'init-python)

;; rainbow delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package undo-tree
  :ensure t
  :init (global-undo-tree-mode))

;; sml-mode -- smart mode line
(use-package smart-mode-line
  :ensure t
  :init
  ; (setq sml/no-confirm-load-theme t)  ; avoid asking when startup
  (sml/setup))

(use-package google-this
  :ensure t
  :init
  (google-this-mode 1))

(use-package smooth-scroll
  :ensure t
  :config
  (smooth-scroll-mode))

;; SSH remote
;; (defun connect-homeserver ()
;;   (interactive)
;;   (dired "/ssh:pavin@192.168.1.120:/home/pavin/Code/"))
;; (defun connect-ubuntu ()
;;   (interactive)
;;   (dired "/ssh:pavin@172.16.172.133:/home/pavin/Code/"))
;; (eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))

(provide 'init)

;;; init.el ends here
