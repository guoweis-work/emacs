(delete 'Hg vc-handled-backends)
    (setq locate-command "mdfind")
(defvar locate-make-command-line 'nt-locate-make-command-line)

(defun nt-locate-make-command-line (arg)
 (list "mdfind" "-onlyin" "/Users/guoweis/fbsource" "-name" arg locate-fcodes-file))


(defun my-find-and-open-file ()
  "Use mdfind to search for a file and open it in a new buffer."
  (interactive)
  (let ((query (read-string "Enter a Spotlight query: ")))
    (message "Query: %s" query)
    (let ((cmdline (format "mdfind -name %s.swift 2>/dev/null" query)))
      (message "Command: %s" cmdline)
      (let ((result-list (split-string (shell-command-to-string cmdline) "\n")))
        (setq result-list (remove-if #'(lambda (x) (string= x "")) result-list))
        (if (equal (length result-list) 1)
            (find-file (car result-list))
        (message "Found %d files" (length result-list)))))))
