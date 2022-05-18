;;; Periphery --- A simple package to parse output from compile commands

;;; Commentary: --- A simple package


;;; Code:
(require 'dash)
(require 'transient)
(require 'evil)

(defface periphery-info-face
  '((t (:inherit compilation-info :bold t)))
  "Warning."
  :group 'periphery)

(defface periphery-warning-face
  '((t (:inherit compilation-warning :bold t)))
  "Warning."
  :group 'periphery)

(defface periphery-error-face
  '((t (:inherit compilation-error :bold t)))
  "Warning."
  :group 'periphery)

(defface periphery-filename-face
  '((t (:inherit font-lock-builtin-face)))
  "Warning."
  :group 'periphery)

(defface periphery-linenumber-face
  '((t (:inherit font-lock-constant-face)))
  "Warning."
  :group 'periphery)

(defface periphery-identifier-face
  '((t (:inherit font-lock-type-face :italic t :weight semi-bold)))
  "Warning."
  :group 'periphery)

(defface periphery-message-face
  '((t (:inherit font-lock-comment-face)))
  "Warning."
  :group 'periphery)

(defvar periphery-mode-map nil "Keymap for periphery.")
(setq periphery-mode-map (make-sparse-keymap))

(define-key periphery-mode-map (kbd "RET") #'open-current-line-id)
(define-key periphery-mode-map (kbd "<return>") #'open-current-line-id)
(define-key periphery-mode-map (kbd "o") #'open-current-line-id)

(defconst periphery-regex-parser "\\(^\/[^:]+\\):\\([0-9]+\\):\\([0-9]+\\):\w?\\([^:]+\\).\\(.*\\)")
(defconst periphery-parse-line-regex "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$")
(defconst periphery-remove-unicode-regex "[^\x00-\x7F]+"
  "Remove unicode-characters.")
(defconst periphery-note-and-errors-regex "\\(^[^\s:]+\\):\s\\(.+\\)$"
  "When we fail because of other errors than compilation errors.")
(defconst periphery-buffer-name "*Periphery*")
(defconst periphery-regex-mark-quotes "\\('[^']+'\\)")

(defvar periphery-errorList '())
(defvar periphery-directory-root nil "DirectoryRoot for localizeable.")

(define-derived-mode periphery-mode tabulated-list-mode "Periphery-mode"
  "Periphery mode.  A mode to show compile errors like Flycheck."
  (setq tabulated-list-format [
                               ("File" 30 t)
                               ("Line" 5 nil)
                               ("Severity" 8 nil)
                               ("Message" 80 nil)
                               ])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key (cons "Line" nil))
  (turn-off-evil-mode)
  (use-local-map periphery-mode-map)
  (tabulated-list-init-header))

(defun open-current-line-id ()
  "Open current row."
  (interactive)
  (save-match-data
    (let* ((data (tabulated-list-get-id))
           (matched (string-match periphery-parse-line-regex data))
           (file (match-string 1 data))
           (linenumber (string-to-number (match-string 2 data)))
           (column (string-to-number (match-string 3 data))))
        (with-current-buffer (find-file file)
          (when (> linenumber 0)
            (goto-char (point-min))
            (forward-line (1- linenumber))
            (when (> column 0)
              (forward-char (1- column))))))))

(defun periphery-listing-command (errorList)
 "Create a ERRORLIST for current mode."
  (pop-to-buffer periphery-buffer-name nil)
  (periphery-mode)
  (setq tabulated-list-entries (-non-nil errorList))
  (tabulated-list-print t))


(defun mark-all-quoted-symbols (input)
  "Highlight all quoted symbols (as INPUT)."
  (save-match-data
    (let* ((position 0)
           (normalizedInput (replace-regexp-in-string "’" "'"  input)))
      (while (string-match periphery-regex-mark-quotes normalizedInput position)
        (let* ((ref (match-string 1 normalizedInput))
               (startPosition (string-match periphery-regex-mark-quotes normalizedInput position)))
          (setq position (match-end 1))
          (put-text-property startPosition position 'face 'periphery-error-face normalizedInput)))
  normalizedInput)))

(defun parse-periphery-output-line (line)
  "Run regex over curent LINE."
  (save-match-data
    (and (string-match periphery-regex-parser line)
         (let* ((file (match-string 1 line))
                (linenumber (match-string 2 line))
                (column (match-string 3 line))
                (type (match-string 4 line))
                (message (match-string 5 line))
                (fileWithLine (format "%s:%s:%s" file linenumber column)))

             (list fileWithLine (vector
                                 (propertize (file-name-sans-extension (file-name-nondirectory file)) 'face 'periphery-filename-face)
                                 (propertize linenumber 'face 'periphery-linenumber-face)
                                 (propertize-severity type (string-trim-left type))
                                 (mark-all-quoted-symbols
                                    (propertize (string-trim-left message) 'face 'periphery-message-face))
                                 ;; (mark-all-quoted-symbols (string-trim-left message))
                                 ))))))


(defun propertize-message (text)
  "Colorize TEXT based on type."
  (cond
   ((string-match-p (regexp-quote "Function") text)
    (propertize text 'face 'font-lock-function-name-face))
   ((string-match-p (regexp-quote "Class") text)
    (propertize text 'face 'font-lock-keyword-face))
   ((string-match-p (regexp-quote "Enum") text)
    (propertize text 'face 'font-lock-keyword-face))
   ((string-match-p (regexp-quote "Struct") text)
    (propertize text 'face 'font-lock-keyword-face))
   ((string-match-p (regexp-quote "Parameter") text)
    (propertize text 'face 'font-lock-type-face))
   ((string-match-p (regexp-quote "Property") text)
    (propertize text 'face 'font-lock-variable-name-face))
   ((string-match-p (regexp-quote "Initializer") text)
    (propertize text 'face 'font-lock-constant-face))
   ((string-match-p (regexp-quote "Protocol") text)
    (propertize text 'face 'font-lock-builtin-face))
  (t (propertize text 'face 'periphery-message-face))))
  

(defun propertize-severity (severity text)
  "Colorize TEXT using SEVERITY."
  (let ((type (string-trim-left severity)))
    (cond
     ((string= type "info")
      (propertize text 'face 'compilation-info-face))
     ((string= type "note")
      (propertize text 'face 'periphery-warning-face))
     ((string= type "warning")
      (propertize text 'face 'periphery-warning-face))
     ((string= type "error")
      (propertize text 'face 'periphery-error-face))
     (t (propertize text 'face 'periphery-info-face)))))

(defun periphery-run-parser (input)
  "Run parser (as INPUT)."
  (setq periphery-errorList nil)
  (dolist (line (split-string input "\n"))
    (let (
          (entry (parse-periphery-output-line (string-trim-left (replace-regexp-in-string periphery-remove-unicode-regex "" line))))
          (secondEntry (parse-xcodebuild-notes-and-errors (replace-regexp-in-string periphery-remove-unicode-regex "" line))))
      (if entry
          (push entry periphery-errorList))
      (unless entry (and secondEntry
          (push secondEntry periphery-errorList))
      )))
  (if periphery-errorList
      (periphery-listing-command periphery-errorList)
    (message-with-color "[Complete]" "No errors or warnings found" '(:inherit success))))

(defun periphery-mode-all ()
  "Show all."
  (interactive)
  (progn
    (setq tabulated-list-entries (-non-nil periphery-errorList))
    (tabulated-list-print t)))

(defun periphery-mode-build-filter (filter)
  "Show only FILTER."
  (interactive "P")
  (progn
    (setq tabulated-list-entries
          (--filter
           (string-match-p (regexp-quote filter)
                           (aref (car( cdr it)) 3)) (-non-nil periphery-errorList)))
    (tabulated-list-print t)))

(defun periphery-mode-list-functions ()
  "Filter on fucntions."
  (interactive)
  (periphery-mode-build-filter "Function"))

(defun periphery-mode-list-unused ()
  "Filter on fucntions."
  (interactive)
  (periphery-mode-build-filter "unused"))

(defun periphery-mode-list-initializer ()
  "Filter on fucntions."
  (interactive)
  (periphery-mode-build-filter "Initializer"))

(defun periphery-mode-list-protocol ()
  "Filter on protocol."
  (interactive)
  (periphery-mode-build-filter "Protocol"))

(defun periphery-mode-list-parameter ()
  "Filter on parameter."
  (interactive)
  (periphery-mode-build-filter "Parameter"))

(defun periphery-mode-list-property ()
  "Filter on property."
  (interactive)
  (periphery-mode-build-filter "Property"))

(defvar periphery-mode-map nil
  "Keymap for periphery.")

(setq periphery-mode-map (make-sparse-keymap))
(define-key periphery-mode-map (kbd "?") 'periphery-mode-help)
(define-key periphery-mode-map (kbd "a") 'periphery-mode-all)
(define-key periphery-mode-map (kbd "f") 'periphery-mode-list-functions)
(define-key periphery-mode-map (kbd "u") 'periphery-mode-list-unused)
(define-key periphery-mode-map (kbd "i") 'periphery-mode-list-initializer)
(define-key periphery-mode-map (kbd "I") 'periphery-mode-list-protocol)
(define-key periphery-mode-map (kbd "P") 'periphery-mode-list-parameter)
(define-key periphery-mode-map (kbd "p") 'periphery-mode-list-property)
(define-key periphery-mode-map (kbd "<return>") 'open-current-line-id)
(define-key periphery-mode-map (kbd "o") 'open-current-line-id)

(transient-define-prefix periphery-mode-help ()
"Help for periphery mode."
["Periphery mode help"
    ("a" "All" periphery-mode-all)
    ("f" "Functions" periphery-mode-list-functions)
    ("u" "Unused" periphery-mode-list-unused)
    ("i" "Initializer" periphery-mode-list-initializer)
    ("I" "Protocol" periphery-mode-list-protocol)
    ("P" "Parameter" periphery-mode-list-parameter)
    ("p" "Property" periphery-mode-list-property)
    ("o" "Open" open-current-line-id)
 ])

(defun periphery-kill-buffer ()
  "Kill the periphery buffer."
  (interactive)
  (when (get-buffer periphery-buffer-name)
    (kill-buffer periphery-buffer-name)))

(defun periphery-show-buffer ()
  "Show current periphery buffer."
  (interactive)
  (periphery-listing-command periphery-errorList))

;;; - Bartycrouch parsing
(defconst bartycrouch-regex-parser "\\(\/+[^:]+\\):\\([0-9]+\\):[^:]+.\s[^:]+.\s+\\([^']+\\)\\('[^']+'\\)\\([^:]+:\\)\s\\(\[[0-9]+\]\\)")

(defun parse-bartycrouch-output-line (line)
  "Run regex over curent LINE."
  (message line)
  (save-match-data
    (and (string-match bartycrouch-regex-parser line)
         (let* ((file (match-string 1 line))
                (linenumber (match-string 2 line))
                (message (match-string 3 line))
                (failingAttribute (match-string 4 line))
                (messageRest (match-string 5 line))
                (otherEntries (match-string 6 line))
                (fileWithLine (format "%s:%s:%s" file linenumber "0")))
           
             (list fileWithLine (vector
                                 (propertize
                                  (format "%s/%s"
                                          (file-name-sans-extension (file-name-nondirectory (directory-file-name (file-name-directory file))))
                                          (file-name-nondirectory file)
                                          ) 'face 'periphery-filename-face)
                                 (propertize linenumber 'face 'periphery-linenumber-face)
                                 (propertize "warning" 'face 'periphery-warning-face)
                                 (format "%s%s%s %s"
                                         (propertize message 'face 'periphery-message-face)
                                         (propertize failingAttribute 'face 'periphery-identifier-face)
                                         (propertize messageRest 'face 'periphery-message-face)
                                         (propertize otherEntries 'face 'periphery-linenumber-face)
                                         )
                                 ))))))

(defun periphery-run-bartycrouch-parser (input directory)
  "Run bartycrouchparsing as INPUT DIRECTORY."
  (setq periphery-directory-root directory)
  (setq periphery-errorList nil)
  (dolist (line (split-string input "\n"))
    (when-let ((entry (parse-bartycrouch-output-line (string-trim-left line))))
      (push entry periphery-errorList)))
  (when periphery-errorList
      (periphery-listing-command periphery-errorList)))
  
(defun parse-xcodebuild-notes-and-errors (line)
  "Parse error and notes (as LINE)."
  (save-match-data
    (and (string-match periphery-note-and-errors-regex line)
         (let* ((note (match-string 1 line))
                (message (match-string 2 line)))
             (list "" (vector
                                 (propertize "Buildinfo" 'face 'periphery-filename-face)
                                 (propertize "" 'face 'periphery-message-face)
                                 (propertize (if note note "error") 'face 'periphery-warning-face)
                                 (propertize message 'face 'periphery-message-face)))))))


(defun message-with-color (tag text attributes)
  "Print a TAG and TEXT with ATTRIBUTES."
  (interactive)
  (setq-local inhibit-message nil)
  (message "%s %s" (propertize tag 'face attributes) text)
  (setq-local inhibit-message t))

(provide 'periphery)
;;; periphery.el ends here

