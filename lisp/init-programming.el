;;; init-programming.el --- Programming settings -*- lexical-binding: t -*-
;;; Commentary:
;;;     lsp-mode and dap-mode should be installed and loaded first.
;;; Code:

;; C/C++
(use-package c++-mode
  :functions 			; suppress warnings
  c-toggle-hungry-state
  :hook
  (c-mode . lsp-deferred)
  (c++-mode . lsp-deferred)
  (c++-mode . c-toggle-hungry-state)
  :bind
  ("C-c o" . ff-find-other-file)
  ("C-c 4 o" . ff-find-other-file-other-window))

(use-package clang-format
  :ensure t)

(use-package cmake-mode
  :ensure t)

;; debug
(use-package dap-cpptools
  :after dap-mode)

(when *is-a-mac*
  (use-package dap-lldb
										;:disabled
	:after dap-mode
	:config
	(setq dap-lldb-debug-program '("/usr/local/opt/llvm/bin/lldb-vscode"))
	;; ask user for executable to debug if not specified explicitly (c++)
	(setq dap-lldb-debugged-program-function
		  (lambda () (read-file-name "Select file to debug: "))))
  ;; default debug template for (c++)
  ;; (dap-register-debug-template
  ;;  "LLDB:vscode"
  ;;  (list :type "lldb-vscode"
  ;;        :cwd nil
  ;;        :args nil
  ;;        :request "launch"
  ;;        :program nil)))
  )

;; Python
(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("~/miniconda3/bin/python3" . python-mode)
  :config
  ;; for debug
  (require 'dap-python))

(use-package pyvenv
  :ensure t
  :config
  (setenv "WORKON_HOME" (expand-file-name "~/miniconda3/envs"))
  ;; (setq python-shell-interpreter "python3")
  (pyvenv-mode t)
  :hook
  (python-mode . (lambda () (pyvenv-workon ".."))))

(use-package lsp-pyright
  :ensure t
  :config
  :hook
  (python-mode . (lambda ()
				   (require 'lsp-pyright)
				   (lsp-deferred))))

;; Rust
(use-package rust-mode
  :ensure t
  :functions dap-register-debug-template
  :bind
  ("C-c C-c" . rust-run)
  :hook
  (rust-mode . lsp-deferred)
  :config
  ;; debug
  (require 'dap-gdb-lldb)
  (dap-register-debug-template "Rust::LLDB Run Configuration"
                               (list :type "lldb"
									 :request "launch"
									 :name "rust-lldb::Run"
									 :gdbpath "rust-lldb"
									 :target nil
									 :cwd nil)))

;; Add keybindings for interacting with Cargo
(use-package cargo
  :ensure t
  :hook
  (rust-mode . cargo-minor-mode))

;; For ns-3
(load-file (expand-file-name "~/.emacs.d/mymode/ns3-mode.el"))
(require 'ns3-mode)

;; Print ANSI colors in compilation mode buffer
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

(defun shell-other-window ()
  "Open shell in other window."
  (interactive)
  (other-window 1)
  (shell))

(provide 'init-programming)
;;; init-programming.el ends here
