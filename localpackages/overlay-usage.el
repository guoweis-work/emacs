;;; overlay-usage.el --- ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Package for showing usage of function.

;;; Code:

(require 'rect)
(require 'project)

(defvar overlay-usage-mode nil)

(defvar-local overlays-list nil
  "List of overlays.")

(defgroup overlay-usage nil
  "Plugin shows complexity information."
  :prefix "overlay-usage-"
  :group 'comm)

(defface overlay-usage-default-face
  '((t :height 0.7 :foreground "#999999"))
  "Face added to code-usage display."
  :group 'overlay-usage)

(defface overlay-usage-symbol-face
  '((t :inherit font-lock-function-name-face :height 0.7))
  "Face added to code-usage display."
  :group 'overlay-usage)

(define-minor-mode overlay-usage-mode
  "Toggle overlay-usage-mode."
  :group overlay-usage
  :init-value nil
  :lighter "OverlayUsage"
  (if overlay-usage-mode
      (overlay-usage-enable)
    (overlay-usage-disable)))

(defun project-root-dir ()
  "Get the root directory of the current project."
  (let ((project (project-current)))
    (when project
      (project-root project))))

(defun overlay-usage-enable ()
  "Enable overlay-usage."
  (add-hook 'after-save-hook #'overlay-add-to-functions nil t)
  (overlay-add-to-functions)
  ;; (setq scroll-preserve-screen-position 'always)
  ;; (setq inhibit-field-text-motion t)
  )

(defun overlay-usage-disable ()
  "Disable overlay-usage-mode."
  (overlay-recenter (point-max))
  (remove-hook 'after-save-hook #'overlay-add-to-functions t)
  (overlay-usage-remove-overlays))

(defun overlay-usage-remove-overlays ()
  "Clean up all overlays."
  (mapc #'delete-overlay overlays-list))

(cl-defun add-overlay (&key position spaces extension)
  "Add overlay (as POSITION with SPACES and search EXTENSION)."
  (goto-char position)
  (let* ((function-name (thing-at-point 'symbol))
         (extension extension)
         (count (string-to-number
                 (shell-command-to-string
                  (shell-command-from
                   :extension extension
                   :function function-name)))))

    (let ((ov (make-overlay (line-beginning-position 0) (line-beginning-position 0) (current-buffer) t t)))
      (overlay-put ov 'before-string "\n")
      (overlay-put ov 'after-string
                   (concat spaces
                           (concat (propertize "λ︎ " 'face 'overlay-usage-symbol-face)
                                   (propertize (concat "Found " (number-to-string (if (> count 0)
                                                                                      (- count 1) 0)) " references")
                                               'face 'overlay-usage-default-face))))
      (overlay-put ov 'overlay-usage t)
      (push ov overlays-list))))

(defun extension-from-file ()
  "Get file extension."
  (file-name-extension buffer-file-name))

(cl-defun shell-command-from (&key extension function)
  "Shell command from EXTENSION and FUNCTION."
  (cond
   ((string-suffix-p "swift" extension t)
    (format "rg --glob '*.%s' -e '%s\\(' | wc -l" extension function))
   ((string-suffix-p "el" extension t)
    (format "rg --glob '*.%s' -e '%s' | wc -l" extension function))))

(defun regex-for-file-type (extension)
  "Detect what the function start with from the (EXTENSION)."
  (cond
   ((string-match-p (regexp-quote "swift") extension) "func")
   ((string-match-p (regexp-quote "el") extension) "defun")))

(defun overlay-add-to-functions ()
  "Add overlay to functions."
  (overlay-usage-remove-overlays)
  (save-excursion
    (let* ((extension (extension-from-file))
           (func-regex (regex-for-file-type extension))
           (default-directory (project-root-dir)))

      (while (search-forward-regexp (concat func-regex " \\([a-zA-Z0-9_-\(]+\\)") nil t)
        (let ((position (match-beginning 1))
              (column (save-excursion
                        (back-to-indentation)
                        (current-column))))
          (add-overlay
           :position position
           :spaces (spaces-string column)
           :extension (format "%s" extension)))))))

(provide 'overlay-usage)
;;; overlay-usage.el ends here
