;;; org-roam-download.el -- Org roam download to manage images
;;; Commentary:
;;;    

;;; Code:

(require 'org-roam)
(require 'org-download)

(when (eq system-type 'darwin)
  (setq org-download-screenshot-method "screencapture"))

(defgroup org-roam-download nil
  "Image drag-and-drop for org-roam."
  :group 'org-download
  :prefix "org-roam-download-")

(defun org-roam-download-create-image-dir ()
  "Create image directory for current org-roam node."
  (interactive)
  (let ((dirname (concat org-roam-download-image-dir (org-roam-id-at-point))))
	(unless (file-directory-p dirname)
	  (make-directory dirname))
	dirname))

(with-eval-after-load "org-roam"
  (setq org-roam-download-image-dir (concat org-roam-directory "images/")))

(defun org-roam-download-yank ()
  "Call `org-download-yank' after setting correct directory."
  (interactive)
  (setq org-download-image-dir (org-roam-download-create-image-dir))
  (org-download-yank))

(defun org-roam-download-clipboard ()
  "Call `org-download-clipboard' after setting correct directory."
  (interactive)
  (setq org-download-image-dir (org-roam-download-create-image-dir))
  (org-download-clipboard))

(defun org-roam-download-dnd (uri action)
    "When in `org-mode' and URI points to image, download it.
Otherwise, pass URI and ACTION back to dnd dispatch."
  (setq org-download-image-dir (org-roam-download-create-image-dir))
  (org-download-dnd uri action))

(defun org-roam-download-dnd-base64 (uri _action)
  "When in `org-mode' and URI points to image in base64, download it.
Otherwise, pass URI and ACTION back to dnd dispatch."
  (setq org-download-image-dir (org-roam-download-create-image-dir))
  (org-download-dnd-base64 uri _action))

;;;###autoload
(defun org-roam-download-enable ()
  "Enable org-roam-download."
  (unless (eq (cdr (assoc "^\\(https?\\|ftp\\|file\\|nfs\\):" dnd-protocol-alist))
              'org-roam-download-dnd)
    (setq dnd-protocol-alist
          `(("^\\(https?\\|ftp\\|file\\|nfs\\):" . org-roam-download-dnd)
            ("^data:" . org-roam-download-dnd-base64)
            ,@dnd-protocol-alist))))

(org-roam-download-enable)

(provide 'org-roam-download)

;;; org-roam-download.el ends here

