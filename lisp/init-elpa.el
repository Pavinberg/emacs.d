;;; init-elpa.el --- Settings and helpers for package.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'package)
(require 'cl-lib)

;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
;; (setq package-user-dir
;;       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
;;                         user-emacs-directory))


;;; Standard package repositories
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq url-proxy-services '(("no_proxy" . "^\\(192\\.168\\..*\\)")
                           ("http" . "localhost:10108")
						   ("https" . "localhost:10108")))
(setq package-archives '(("gnu-mir"   . "http://mirrors.cloud.tencent.com/elpa/gnu/")
                         ("melpa-mir" . "http://mirrors.cloud.tencent.com/elpa/melpa/")))
;; (add-to-list 'package-archives '("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/") t)
;; (add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '( "melpa_tsinghua" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/") t)
;; (add-to-list 'package-archives
;;              '("MELPA Stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("MELPA Stable" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/") t)
;; (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
;;(add-to-list 'load-path "~/.emacs.d/elpa/org-9.3.6/lisp")

;; Official MELPA Mirror, in case necessary.
;; (add-to-list 'package-archives (cons "melpa-mirror" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/")) t)


;; ;; Work-around for https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
;; ;; (when (and (version< emacs-version "26.3") (boundp 'libgnutls-version) (>= libgnutls-version 30604))
;; ;;   (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

;;; Fire up package.el

;; (setq package-enable-at-startup nil)
(package-initialize)

(provide 'init-elpa)
;;; init-elpa.el ends here
