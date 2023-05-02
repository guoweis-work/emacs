;;; welcome.el --- Simple welcome screen -*- lexical-binding: t; -*-

;;; Commentary:
;; Welcome screen

;;; code:

(require 'recentf)

(defvar welcome-mode nil)
(defvar welcome-recentfiles '()
  "Recent list.")

(defgroup welcome nil
  "Welcome group."
  :group 'applications)

(defconst welcome-buffer "*welcome*"
  "Welcome buffer name.")

(defvar welcome-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'welcome--open-recent-file)
    (define-key map (kbd "<return>") 'welcome--open-recent-file)
    (define-key map (kbd "o") 'welcome--open-recent-file)

    ;; Add shortcuts for file indexes
    (dolist (i (number-sequence 1 9))
      (define-key map (kbd (concat "C-" (number-to-string i)))
        `(lambda ()
           (interactive)
           (welcome--open-recent-file-at-index ,i))))
    (message "welcome-mode-map initialized")
    map)
  "Keymap for `welcome-mode'.")

(define-derived-mode welcome-mode fundamental-mode "Welcome"
  "Major mode for the welcome screen."
  :group 'welcome
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (setq-local display-line-numbers nil)
  (setq-local truncate-lines t)
  (setq-local mode-line-format nil)
  (setq-local global-hl-line-mode nil)
  (use-local-map welcome-mode-map))

(defface welcome-title-face
  '((t :inherit link :height 1.1 :bold t))
  "Face added to code-usage display."
  :group 'welcome)

(defface welcome-info-face
  '((t :foreground "#F66D86" :height 0.9 :bold t :italic t))
  "Face added to code-usage display."
  :group 'welcome)

(defface welcome-text-info-face
  '((t :foreground "#ADB5D0" :height 0.9 :bold nil))
  "Face added to code-usage display."
  :group 'welcome)

(defface welcome-path-face
  '((t :foreground "#63677D" :height 0.9 :weight thin :bold nil :italic nil))
  "Face for the file path."
  :group 'welcome)

(defface welcome-filename-face
  '((t :inherit default :bold t :italic nil))
  "Face for the file name."
  :group 'welcome)

(defun welcome--insert-centered (text)
  "Insert TEXT at the center of the current line."
  (let ((width (window-width)))
    (insert (make-string (/ (- width (length text)) 2) ?\ ))
    (insert text)))

(defun welcome--open-recent-file ()
  "Open the recent file on the current line."
  (interactive)
  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         (prop-pos (next-single-property-change line-start 'path nil line-end)))
    (when prop-pos
      (let ((file (get-text-property prop-pos 'path)))
        (message file)
        (if (file-exists-p file)
            (find-file file)
          (error "File %s does not exist" file))))))

(defun welcome--open-recent-file-at-index (index)
  "Open the recent file at the given INDEX in the list."
  (interactive "nIndex: ")
  (let ((files (seq-take welcome-recentfiles 9)))
    (when (<= 1 index (length files))
      (find-file (nth (1- index) files)))))

(defun welcome--insert-recent-files ()
  "Insert the first 9 recent files with icons in the welcome buffer."
  (recentf-mode)
  (insert "\n")
  (let* ((files (seq-take welcome-recentfiles 9))
         (max-length (apply 'max (mapcar 'length files)))
         (left-margin (/ (- (window-width) max-length) 2)))
    (dolist (file files)
      (let* ((index (cl-position file files :test #'equal))
             (full-path (file-truename file))
             (shortcut (format "%d" (+ index +1)))
             (file-name (file-name-nondirectory file))
             (file-dir (file-name-directory file))
             (title (format "%s %s%s"
                    (propertize (all-the-icons-icon-for-file file :v-adjust -0.05) 'face '(:family "all-the-icons" :height 1.0))
                    (propertize file-dir 'face 'welcome-path-face)
                    (propertize file-name 'face 'welcome-filename-face)))
             (title-with-path (propertize title 'path full-path))
             (title-with-path-and-shortcut (concat title-with-path (propertize (format " [%s]" shortcut) 'face '(:height 0.9 :inherit font-lock-constant-face))))
        (right-margin (- (window-width) max-length left-margin)))
        (insert (format "%s%s%s\n" (make-string left-margin ?\s) title-with-path-and-shortcut (make-string right-margin ?\s)))))))

(defun welcome--insert-text (text)
  "Insert (as TEXT)."
  (let* ((max-length (apply 'max (mapcar 'length welcome-recentfiles)))
         (left-margin (/ (- (window-width) max-length) 2)))
    (insert (format "%s%s\n" (make-string left-margin ?\s) text ))))

(defun welcome--redisplay-buffer-on-resize (&rest _)
  "Resize current buffer."
  (when (equal (buffer-name) welcome-buffer)
    (welcome--refresh-screen)))

;;;###autoload
(defun welcome-create-welcome-hook ()
  "Setup welcome screen."
  (when (< (length command-line-args) 2)
    (add-hook 'window-size-change-functions 'welcome--redisplay-buffer-on-resize)
    (add-hook 'emacs-startup-hook (lambda () (welcome--refresh-screen)))))

(defun welcome--refresh-screen ()
  "Show the welcome screen."
  ;; (setq welcome--timer
  ;;       (run-at-time "0 sec" 1 'welcome--refresh-screen))
  (setq welcome-recentfiles recentf-list)
  (with-current-buffer (get-buffer-create welcome-buffer)
    (let* ((buffer-read-only)
           (image-path "~/.emacs.d/themes/true.png")
           (image (create-image image-path 'png nil :width 200 :height 169))
           (size (image-size image))
           (width (car size))
           (left-margin (floor (/ (- (window-width) width) 2)))
           (packages (format "%d" (length package-activated-list))))
      (erase-buffer)
      (goto-char (point-min))
      (let ((inhibit-read-only t))
        (insert "\n")
        (welcome--insert-text (propertize "Quick acccess [C-x to open file]" 'face 'welcome-title-face))
        (welcome--insert-recent-files)
        (setq cursor-type nil)
        (insert "\n")
        (welcome--insert-text (format "%s %s %s"
                                      (propertize "Startup time:" 'face 'welcome-text-info-face)
                                      (propertize (emacs-init-time "%.2f") 'face 'welcome-info-face)
                                      (propertize "seconds" 'face 'welcome-text-info-face)))
        (welcome--insert-text (format "%s %s"
                                      (propertize packages 'face 'welcome-info-face)
                                      (propertize "packages loaded" 'face 'welcome-text-info-face)))
        (insert "\n\n\n")
        (insert (make-string left-margin ?\ ))
        (insert-image image)
        (switch-to-buffer (current-buffer))
        (read-only-mode +1)
        (welcome-mode)
        (goto-char (point-min))
        (forward-line 3)
        ))))

(provide 'welcome)
;;; welcome.el ends here
