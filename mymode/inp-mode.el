;;; inp-mode --- Summary

;; Copyright 2020, by Paviberg
;; Author: Pavinberg(pavin0702@gmail.com)
;; Version: 0.0.1
;; Created: 10 Feb 2020

;;; Commentary:
;; inp-mode for CALPUFF model INP file.

;;; Code:
(setq inp-highlights
      '(("!\\([A-Za-z0-9_ ]+\\)=" . font-lock-keyword-face)
        ("#\\([A-Za-z0-9_ ]+\\)=" . font-lock-constant-face)
		("\\([A-Za-z0-9-_,+/\\.\\ ]+\\)!" . font-lock-string-face)
        ("\\([A-Za-z0-9-_:,/\\.\\ ]+\\)#" . font-lock-string-face)
		("\\*.*\\*" . font-lock-function-name-face)))

 (define-derived-mode inp-mode fundamental-mode "inp"
   "major mode for editing mymath language code."
   (setq font-lock-defaults '(inp-highlights)))

(provide 'inp-mode)
;;; inp-mode.el ends here
