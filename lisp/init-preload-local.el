;;; init-preload-local -- Configure some basic function and keyboard shortcuts
;;; Commentary:
;;; Code:

(setq-default tab-width 4)
(electric-pair-mode t)
(column-number-mode t)
(global-auto-revert-mode t)
(setq inhibit-startup-message t)    ;; Hide the startup message
(setq-default python-indent 4)
(setq c-basic-offset 4)
;;(setq-default indent-tabs-mode nil)
(setq make-backup-files nil) ; stop creating backup~ files
(add-hook 'prog-mode-hook #'hs-minor-mode)
(add-hook 'prog-mode-hook #'show-paren-mode)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(menu-bar-mode -1)
;; (add-hook 'rust-mode-hook
;; 		  (lambda () (setq indent-tabs-mode nil)))

;;; keyboard setting
(global-set-key (kbd "RET") 'newline-and-indent)

;; Set home to Hyper
(global-set-key (kbd "<home>") nil)
(define-key function-key-map (kbd "<home>") 'event-apply-hyper-modifier)

;; Set end to super
(global-set-key (kbd "<end>") nil)
(define-key function-key-map (kbd "<end>") 'event-apply-super-modifier)

;; Shortcuts
(global-set-key (kbd "C-j") nil)
(global-set-key (kbd "M-w") 'kill-region)
(global-set-key (kbd "C-w") 'kill-ring-save)
(global-set-key (kbd "C-c '") 'comment-or-uncomment-region)
(global-set-key (kbd "C-j C-k") 'kill-whole-line)
(global-set-key (kbd "C-a") 'back-to-indentation) ;; swap C-a and M-m
(global-set-key (kbd "M-m") 'move-beginning-of-line)
(global-set-key (kbd "H-o") 'other-window)
;;(global-set-key (kbd "H-k") 'kill-buffer)
(global-set-key (kbd "H-h") 'hs-hide-block)
(global-set-key (kbd "H-s") 'hs-show-block)

;; Faster move cursor
(defun next-ten-lines()
  "Move cursor to next 10 lines."
  (interactive)
  (next-line 10))

(defun previous-ten-lines()
  "Move cursor to previous 10 lines."
  (interactive)
  (previous-line 10))
(global-set-key (kbd"M-n") 'next-ten-lines)
(global-set-key (kbd"M-p") 'previous-ten-lines)

;; Move text in a line
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "M-P") 'move-line-up)
(global-set-key (kbd "M-N") 'move-line-down)

;; pretty paste and copy
(defun pbpaste ()
  "Paste data from pasteboard."
  (interactive)
  (shell-command-on-region
   (point)
   (if mark-active (mark) (point))
   "pbpaste" nil t))

(defun pbcopy ()
  "Copy region to pasteboard."
  (interactive)
  (print (mark))
  (when mark-active
	(shell-command-on-region
	 (point) (mark) "pbcopy")
	(kill-buffer "*Shell Command Output*")))
(global-set-key (kbd "C-x C-y") 'pbpaste)
(global-set-key (kbd "C-x C-w") 'pbcopy)

;; switch windows-spliting between horizontally and vertically
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
	  (let* ((this-win-buffer (window-buffer))
			 (next-win-buffer (window-buffer (next-window)))
			 (this-win-edges (window-edges (selected-window)))
			 (next-win-edges (window-edges (next-window)))
			 (this-win-2nd (not (and (<= (car this-win-edges)
										 (car next-win-edges))
									 (<= (cadr this-win-edges)
										 (cadr next-win-edges)))))
			 (splitter
			  (if (= (car this-win-edges)
					 (car (window-edges (next-window))))
				  'split-window-horizontally
				'split-window-vertically)))
		(delete-other-windows)
		(let ((first-win (selected-window)))
		  (funcall splitter)
		  (if this-win-2nd (other-window 1))
		  (set-window-buffer (selected-window) this-win-buffer)
		  (set-window-buffer (next-window) next-win-buffer)
		  (select-window first-win)

		  (if this-win-2nd (other-window 1))))))

;; bookmark
(global-set-key (kbd "H-x m") 'bookmark-set)
(global-set-key (kbd "H-x b") 'bookmark-jump)
(global-set-key (kbd "H-x l") 'bookmark-bmenu-list)

;; lisp-mode
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-region)

;; shell remove escape characters
(defun preamble-regexp-alternatives (regexps)
  "Return the alternation of a list of regexps."
  (mapconcat (lambda (regexp)
               (concat "\\(?:" regexp "\\)"))
             regexps "\\|"))

(defvar non-sgr-control-sequence-regexp nil
  "Regexp that matches non-SGR control sequences.")

(defun regexp-alternatives (regexps)
  (mapconcat (lambda (regexp) (concat "\\(" regexp "\\)")) regexps "\\|"))

(setq non-sgr-control-sequence-regexp
      (regexp-alternatives
       '(;; icon name escape sequences
         "\033\\][0-2];.*?\007"
         ;; non-SGR CSI escape sequences
         "\033\\[\\??[0-9;]*[^0-9;m]"
         ;; noop
         "\012\033\\[2K\033\\[1F"
         )))

(defun filter-non-sgr-control-sequences-in-region (begin end)
  (save-excursion
    (goto-char begin)
    (while (re-search-forward
            non-sgr-control-sequence-regexp end t)
      (replace-match ""))))

(defun filter-non-sgr-control-sequences-in-output (ignored)
  (let ((start-marker
         (or comint-last-output-start
             (point-min-marker)))
        (end-marker
         (process-mark
          (get-buffer-process (current-buffer)))))
    (filter-non-sgr-control-sequences-in-region
     start-marker
     end-marker)))

(add-hook 'comint-output-filter-functions
          'filter-non-sgr-control-sequences-in-output)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(provide 'init-preload-local)
;;; init-preload-local.el ends here
