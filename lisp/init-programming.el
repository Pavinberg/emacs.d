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
  (c++-mode . c-toggle-hungry-state))

;; debug
(use-package dap-cpptools
  :after dap-mode)

(use-package dap-lldb
  :disabled
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

;; Python
(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python3" . python-mode)
  :config
  ;; for debug
  (require 'dap-python))

(use-package lsp-pyright
  :ensure t
  :config
  :hook
  (python-mode . (lambda ()
				   (require 'lsp-pyright)
				   (lsp-deferred))))

(use-package pyvenv
  :ensure t
  :config
  (setenv "WORKON_HOME" "~/miniconda3/envs")
  ;; (setq python-shell-interpreter "python3")
  (pyvenv-mode t))

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


(provide 'init-programming)
;;; init-programming.el ends here
