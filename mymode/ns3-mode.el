;;; ns3-mode --- Summary

;; Copyright 2022, by Paviberg
;; Author: Pavinberg(pavin0702@gmail.com)
;; Version: 0.0.1
;; Created: 33 Sep 2022

;;; Commentary:
;; A minor mode to do some configurations for ns-3 simulator.

;;; Code:

(require 'cc-vars)
(require 'projectile)
(require 'flycheck)
(require 'dap-mode)
(require 'dap-lldb)

(defun get-string-from-file (filePath)
  "Return file content of FILEPATH as a string."
  (with-temp-buffer
	(insert-file-contents filePath)
	(buffer-string)))

(defun ns3-get-version ()
  "Get version of the ns-3."
  (string-trim-right
   (get-string-from-file (concat (projectile-project-root) "VERSION"))))

(defvar ns3-version nil
  "The version of ns-3.")

(defvar ns3-cache-last-program-name ""
  "A cache of the last program.")

(defun ns3-set-c++-mode-variables ()
  "Set variables of c++-mode."
  (setq c-file-style "gnu")
  (setq c-basic-offset 2)
  (setq flycheck-clang-include-path (list
									 (concat "-I"
											 (projectile-project-root)
	   										 "build/include/")))
  (setq flycheck-clang-language-standard "c++17"))

(defun ns3-set-compile-command ()
  "Set the compile command of ns-3."
  (setq compile-command (concat "cd "
								(projectile-project-root)
								" && ./ns3 build")))

(defun ns3-init-dap ()
  "Initiate the DAP configuration template of ns3 with a version of NS3-VERSION."
  (setq dap-lldb-debugged-program-function
		(lambda () (read-file-name "Select file to debug: ")))
  (dap-register-debug-template
   "NS3::Debug"
   (list :type "lldb-vscode"
		 :cwd (projectile-project-root)
		 :request "launch"
		 ;:program (concat "build/scratch/ns" ns3-version "-" "dumbell" "-debug")
		 :name "NS3::Debug")))

(defun ns3-read-string-or-default (prompt default)
  "Read user input with PROMPT or return DEFAULT if no input."
  (let ((prompt-line (if (zerop (length (string-trim default)))
						 (format "%s: " prompt)
					   (format "%s [%s]: " prompt default))))
	(read-string prompt-line nil nil default)))

(defun ns3-debug ()
  "Debug the ns-3."
  (interactive)
  (let ((prog-name (ns3-read-string-or-default "ns-3 program name" ns3-cache-last-program-name)))
	(setq ns3-cache-last-program-name prog-name)
	(let ((program
		   (concat "build/scratch/ns" ns3-version "-"
				   prog-name "-debug")))
										; (call-interactively 'dap-debug)
	  (dap-debug (list :type "lldb-vscode"
					   :cwd (projectile-project-root)
					   :request "launch"
					   :program program
					   :name "NS3::Debug")))))

(define-minor-mode ns3-mode
  "Minor mode for ns-3 simulator."
  :global global
  :lighter "ns3"
  (if (file-exists-p (concat (projectile-project-root) "ns3"))
	  (progn
		(setq ns3-version (ns3-get-version))
		(ns3-set-c++-mode-variables)
		(ns3-set-compile-command)
		; (ns3-init-dap)
		)))

(provide 'ns3-mode)
;;; ns3-mode.el ends here
