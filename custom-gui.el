(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(codeium/metadata/api_key "2d487642-54f2-4cfa-936d-ed07baec760b")
 '(custom-safe-themes
   '("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "14850c68376012a083ed7ec9d36179962b165cd8f7536f021ee3b6f5cb68aa3c" "3a2f8087795a6a06d5a57cec6569dbbb98211f86ae3ad9ce931a5a3340b32569" default))
 '(ivy-mode t)
 '(package-selected-packages
   '(chatgpt-shell org-ref org-roam org-ref-ivy org-ref-bib helpful meow flycheck-clang-tidy csv-mode queue cider cmake-mode clang-format pyvenv-mode pyvenv lsp-pyright tiny yasnippet-snippets dirvish good-scroll use-package-hydra embark marginalia exec-path-from-shell gnu-elpa-keyring-update fullframe seq amx dap-mode projectile-ripgrep dashboard rainbow-mode pdf-tools auctex))
 '(ring-bell-function 'ignore)
 '(safe-local-variable-values
   '((TeX-master quote dwim)
	 (TeX-master concat
				 (projectile-project-root)
				 "bifrost")
	 (TeX-engine . pdflatex)
	 (eval dap-register-debug-template "NS3::Debug"
		   (list :type "lldb-vscode" :cwd
				 (projectile-project-root)
				 :request "launch" :program
				 (lambda nil
				   (read-file-name "Select file to debug: "))
				 :name "NS3::Debug"))
	 (eval dap-register-debug-template "NS3::Debug"
		   (list :type "lldb-vscode" :cwd
				 (projectile-project-root)
				 :request "launch" :program "build/scratch/ns3.36.1-dumbell-debug" :name "NS3::Debug"))
	 (flycheck-clang-include-path list
								  (concat "-I"
										  (projectile-project-root)
										  "build/include/"))
	 (py-indent-offset . 4)
	 (flycheck-clang-include-path list concat "-I"
								  (projectile-project-root)
								  "include")
	 (flycheck-clang-include-path list
								  (concat "-I"
										  (projectile-project-root)
										  "build/include"))
	 (c-file-style . gnu)
	 (flycheck-clang-include-path list
								  (concat "-I"
										  (projectile-project-root)
										  "build"))))
 '(visible-bell nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-selection ((t (:extend t :background "gray45" :weight normal))))
 '(dashboard-heading ((t (:foreground "light slate blue"))))
 '(dashboard-items-face ((t (:foreground "light gray"))))
 '(dashboard-no-items-face ((t (:foreground "light gray"))))
 '(dashboard-text-banner ((t (:foreground "magenta"))))
 '(ivy-current-match ((t (:extend t :background "#7a7d8b"))))
 '(ivy-org ((t (:foreground "sienna1" :slant italic))))
 '(lsp-face-highlight-read ((t (:background "#6a6d7b"))))
 '(lsp-face-highlight-textual ((t (:background "gray21"))))
 '(org-agenda-date ((t (:foreground "SkyBlue1" :weight normal))))
 '(org-agenda-date-today ((t (:foreground "CadetBlue1" :weight bold))))
 '(org-agenda-date-weekend ((t (:foreground "SteelBlue3" :weight normal))))
 '(org-agenda-structure ((t (:foreground "MediumPurple2" :weight normal))))
 '(org-warning ((t (:foreground "IndianRed1"))))
 '(region ((t (:extend t :background "gray24"))))
 '(show-paren-match ((t (:background "gray89" :foreground "light slate blue" :weight normal))))
 '(tooltip ((t (:background "gray30" :foreground "#eaf2f1")))))
