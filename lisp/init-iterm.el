;;; init-iterm.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun iterm-goto-dir (dirname)
  (do-applescript
	 (concat
      " tell application \"iTerm2\"\n"
      "   tell the current session of current window\n"
      (format "     write text \"cd %s\" \n" dirname)
      "   end tell\n"
      " end tell\n"
      " do shell script \"open -a iTerm\"\n")))

(defun iterm-goto-filedir-or-home ()
  "Go to present working dir and focus iterm."
  (interactive)
  (let ((working-directory
		 (replace-regexp-in-string "\\\\" "\\\\\\\\"
								   (shell-quote-argument (or default-directory "~")))))
	(iterm-goto-dir working-directory)))

(defun iterm-goto-project-root ()
  (interactive)
  (iterm-goto-dir (projectile-project-root)))


(defun iterm-run-command-in-current-directory (cmd)
  "Run command CMD in current directory."
  (interactive "sCommand: ")
  (let ((working-directory
		 (replace-regexp-in-string "\\\\" "\\\\\\\\"
								   (shell-quote-argument default-directory))))
	(message "%s %s" working-directory cmd)
	(do-applescript
	 (concat
      " tell application \"iTerm2\"\n"
      "   tell the current session of current window\n"
      (format "     write text \"cd %s && %s\" \n" working-directory cmd)
      "   end tell\n"
      " end tell\n"
      " do shell script \"open -a iTerm\"\n"))))

(provide 'init-iterm)
;;; init-iterm.el ends here
