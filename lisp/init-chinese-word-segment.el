;;; init-chinese-word-segment.el -- Chinses word segemtation
;;; Commentary:
;;; Code:

;; (setq chinese-word-segment-dir (expand-file-name "emacs-chinese-word-segemtation" user-emacs-directory))
(setq chinese-word-segment-dir "/Users/pavin/.emacs.d/site-lisp/emacs-chinese-word-segmentation/")
(require 'cns)
(setq cns-prog (expand-file-name "chinese-word-segmentation" chinese-word-segment-dir))
(setq cns-dict-directory (expand-file-name "dict" chinese-word-segment-dir))
(setq cns-recent-segmentation-limit 10) ; default is 10
(setq cns-debug nil) ; disable debug output, default is t
;; (when (featurep 'cns)
;;   (add-hook 'find-file-hook 'cns-auto-enable))

(provide 'init-chinese-word-segment)
;;; init-chinese-word-segment ends here
