;;; init.el --- My init.el

;;; Code:

(setq gc-cons-threshold (* 100 1024 1024))

(setq comp-deferred-compilation nil
      create-lockfiles nil
      display-time-24hr-format t
      display-time-default-load-average nil
      frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      inhibit-compacting-font-caches t
      ns-pop-up-frames nil
      inhibit-startup-message t
      package-enable-at-startup nil
      site-run-file nil
      visible-bell nil
      window-resize-pixelwise t
      backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/backups")))

(display-battery-mode t)	; Show battery
(display-time-mode t)		; Show time
(global-hl-line-mode)		; Show current line
(menu-bar-mode -1)			; Disable the menu bar
(recentf-mode t)			; Recent file mode
(scroll-bar-mode -1)		; Disable scrollbar
(set-fringe-mode 4)			; Give us some space
(tool-bar-mode -1)			; Disable toolbar
(tooltip-mode -1)			; Disable tooltip
(show-paren-mode t)			; Enable show paren matching mode

(setq-default display-line-numbers-width 4
			  c-basic-offset 4
			  tab-width 4
			  indent-tabs-mode t
              indent-line-function 'insert-tab)

;; Window
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(when (boundp 'read-process-output-max)
  ;; 1MB in bytes, default 4096 bytes
  (setq read-process-output-max 1048576))

;; dont word wrap
(add-hook 'prog-mode-hook #'(lambda ()
			                  (setq company-mode t					;; Use auto-completion
											electric-pair-mode t			;; Auto insert pairs {} () [] etc
											highlight-indent-guides-mode t	;; Turn on indent-guides
											indicate-empty-lines t			;; Show empty lines
											indicate-unused-lines t			;; Show unused lines
											semantic-mode t					;; Get a little extra help for autocompletion
											show-trailing-whitespace t		;; Show trailing whitespaces
											word-wrap nil					;; Dont word wrap when we are coding
											truncate-lines nil 
											column-number-mode t			;; Show current line number highlighted
											display-line-numbers t			;; Show line numbers
											)))

(fset 'yes-or-no-p 'y-or-n-p)	;; Set yes or no to y/n
(global-font-lock-mode 1)		;; always highlight codex
(global-auto-revert-mode 1)		;; refresh a buffer if changed on disk

;; saving
(desktop-save-mode 1)			;; Save desktop
(savehist-mode 1)				;; Save autocompletions

(add-to-list 'savehist-additional-variables 'kill-ring)

;; Setup fonts
(set-face-attribute 'default nil :font "Source Code Pro" :height 148)
(set-face-attribute 'fixed-pitch nil :font "Source Code Pro" :height 148)
(set-face-attribute 'variable-pitch nil :font "Noto Sans" :height 148 :weight 'regular)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-verbose nil)

;; Make sure we are up to date, atleast once a week
(use-package auto-package-update
  :custom
  (setq auto-package-update-interval 7
		auto-package-update-prompt-before-update t
		auto-package-update-hide-results nil))

(use-package no-littering)	;; Clean up all those temporary files

(setq custom-file (concat user-emacs-directory "/var/custom.el"))

;; Dont leave #file autosaves everywhere I go
(defvar my-auto-save-folder "~/.emacs.d/var/auto-save/")
(setq auto-save-list-file-prefix "~/.emacs.d/var/auto-save/.saves-"); set prefix for auto-saves 
(setq auto-save-file-name-transforms `((".*", my-auto-save-folder t))); location for all auto-save files

(use-package dired
  :ensure nil
  :commands dired dired-jump
  :bind (("C-x C-j" . dired-jump))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single
  :after dired)

;; (use-package tree-sitter
;;     :defer t)

;; (use-package tree-sitter-langs
;;     :after tree-sitter)

;; (add-hook 'swift-mode-hook #'tree-sitter-mode)
;; (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(use-package autothemer)

;;  theming
(add-hook 'minibuffer-setup-hook
          (lambda ()
            (make-local-variable 'face-remapping-alist)
            (add-to-list 'face-remapping-alist '(default (:background "#15121C")))))

(load-theme 'catppuccin t)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  :init (progn
		  (setq dashboard-startup-banner "~/.emacs.d/dashimage/catppuccin.png")
		  (setq dashboard-path-style 'truncate-beginning)
		  (setq dashboard-banner-logo-title "Mikaels dashboard!"
				dashboard-set-file-icons t
				dashboard-set-init-info t
				dashboard-center-content t
				dashboard-set-heading-icons t
				dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name
				dashboard-filter-agenda-entry 'dashboard-filter-agenda-by-time
				dashboard-week-agenda t
				dashboard-items '(
								  (projects . 5)
								  (agenda)))))

;; Which key
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-sort-order 'which-key-key-order-alpha
        which-key-idle-delay 0.3
		which-key-min-display-lines 4
		which-key-max-display-columns 5))

; Use evil mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

  ;; (define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
  ;; (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  ;; (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  ;; (evil-set-initial-state 'messages-buffer-mode 'evil)
  ;; (normal-set-initial-state 'dashboard-mode 'normal))

(use-package evil-tutor
  :commands evil-tutor)

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Theming
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t  ; if nil, italics is universally disabled
	    doom-themes-treemacs-theme "doom-atom")

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package all-the-icons
  :after doom-modeline)

(use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode))

;; Config and install modeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-env-version t)
  :config
  (setq doom-modeline-height 38)
  (set-face-attribute 'mode-line nil :family "Source Code Pro" :height 136)
  (set-face-attribute 'mode-line-inactive nil :family "Source Code Pro" :height 128))

(use-package auto-dim-other-buffers
  :defer t
  :config
  (set-face-background 'auto-dim-other-buffers-face nil)
  (set-face-foreground 'auto-dim-other-buffers-face "#988BA2"))

(add-hook 'after-init-hook (lambda ()
  (when (fboundp 'auto-dim-other-buffers-mode)
    (auto-dim-other-buffers-mode t))))

;; rainbow-delimieters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ivy
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-height 12)
  (setq ivy-display-style nil)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (define-key ivy-mode-map       (kbd "<escape>") nil)
  (define-key ivy-minibuffer-map (kbd "<escape>") #'minibuffer-keyboard-quit))

;; counsel
(use-package counsel
  :hook (ivy-mode . counsel-mode))

(use-package swiper
  :after ivy
  :config
  (setq swiper-goto-start-of-match t))

;; ;; Ivy rich
(use-package ivy-rich
  :hook (ivy-mode . ivy-rich-mode))

(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

;; company --------------------------------------------
(use-package company
  :after lsp-mode
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
              ("<tab>" . company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist #'company-box-icons-all-the-icons))

(use-package lsp-mode
  :commands (lsp lsp-deffered)
  :init
  (setq lsp-keymap-prefix "m-l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (setq lsp-ui-doc-border (face-foreground 'nil)))

(use-package lsp-treemacs
  :after lsp)

(use-package treemacs-projectile
  :hook (treemacs-mode-hook))

(use-package flycheck
  :hook ((prog-mode . flycheck-mode))
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enabled newline))
  (setq flycheck-display-errors-delay 0.1))

(use-package nyan-mode
  :hook doom-modeline-mode
  :init
  (nyan-mode)
  (setq nyan-animate-nyancat t))

; On macos use our custom settings ---------------------
(when (eq system-type 'darwin)
  (use-package ns-auto-titlebar)
  (use-package swift-mode
    :hook (swift-mode . lsp-deferred)
    :config
    (setq swift-mode:parenthesized-expression-offset 4
	  swift-mode:multiline-statement-offset 4))
  (use-package exec-path-from-shell)
  (use-package lsp-sourcekit
    :after lsp-mode
    :config
    (setq lsp-sourcekit-executable (string-trim (shell-command-to-string "Xcrun --find sourcekit-lsp"))))
  (exec-path-from-shell-initialize)
  (ns-auto-titlebar-mode)
  (setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none))

; helpful
(use-package helpful
  :defer t)

(defun my-vterm/split-horizontal ()
  "Create a new vterm window under of the current one."
  (interactive)
  (let* ((ignore-window-parameters t)
         (dedicated-p (window-dedicated-p)))
    (split-window-vertically)
    (other-window 1)
    (vterm default-directory)))

(defun mk/browser-split-vertically ()
  "Create a new browser window to the right of the current one."
  (interactive)
  (let* ((ignore-window-parameters t)
         (dedicated-p (window-dedicated-p)))
    (split-window-horizontally)
    (other-window 1)
    (xwidget-webkit-browse-url "https://google.com")))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom
    (projectile-project-root-files-functions
     '(projectile-root-local
       projectile-root-top-down
       projectile-root-bottom-up
       projectile-root-top-down-recurring))
   (setq projectile-completion-system 'ivy)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Documents/git")
    (setq projectile-project-search-path '("~/Documents/git")))
  (setq projectile-switch-project-action #'projectile-dired))

;; counsel-projectile
(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

;; Restart emacs
(use-package restart-emacs
  :commands restart-emacs)

; Hydra
(use-package hydra
  :defer t)

(defhydra hydra-text-scale (:timeout 4)
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; Winum - select windows easy
(use-package winum
  :defer t
  :init
  (winum-mode 1))

;; darkroom (go to focus mode)
(use-package darkroom
  :commands darkroom)

;; Use git
(use-package magit
  :commands magit-status
  :custom (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package vterm
  :commands vterm)

;; Kill all other buffers
(defun kill-other-buffers ()
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

 ;; general
(use-package general
  :defer t
  :config
  (general-create-definer mk/leader-keys
   :keymaps '(normal insert emacs visual operator hybrid)
   :prefix "SPC"
   :non-normal-prefix "M-SPC")

  (mk/leader-keys
	"T" '(:ignore t :which-key "toggle")
	"Tt" '(counsel-load-theme :which-key "choose theme")
	"Ts" '(hydra-text-scale/body :which-key "scale text")
	;; Screen
	"Tf" '(:ignore t :which-key "screen/frame")
	"Tff" '(toggle-frame-fullscreen :which-key "fullscreen")
	"Tfm" '(toggle-frame-maximized :which-key "maximized"))

  (mk/leader-keys
	"TAB" '((lambda () (interactive) (switch-to-buffer nil)) :which-key "toggle buffers")
	"SPC" '(counsel-M-x :which-key "M-x")
	"0" '(treemacs-select-window :which-key "treemacs")
	"1" '(winum-select-window-1 :which-key "window 1")
	"2" '(winum-select-window-2 :which-key "window 2")
	"3" '(winum-select-window-3 :which-key "window 3")
	"4" '(winum-select-window-4 :which-key "window 4")
	"5" '(winum-select-window-5 :which-key "window 5")
	"6" '(winum-select-window-6 :which-key "window 6")
	"s" 'swiper
	"P" 'package-install
	"'" '((lambda () (interactive) (my-vterm/split-horizontal)) :which-key "term")
	"!" 'shell-command
	":" 'eval-expression)

  (mk/leader-keys
	"e" '(:ignore t :which-key "eval")
	"ee" '(eval-expression :which-key "eval expression")
	"eb" '(eval-buffer :which-key "eval buffer")
	"el" '(eval-last-sexp :which-key "eval before point")
	"er" '(eval-region :which-key "eval region"))

  (mk/leader-keys
	"a" '(:ignore t :which-key "agenda")
   	"aa" '(org-agenda :which-key "show agenda")
   	"as" '(org-agenda-schedule :which-key "show schedule")
	"al" '(org-agenda-list :which-key "show agenda list")
	"aF" '(org-agenda-file-to-front :which-key "bring file to front")
	"at" '(:ignore t:which-key "time/date")
	"att" '(org-time-stamp :which-key "add time")
	"atd" '(org-deadline :which-key "add deadline"))

  (mk/leader-keys
	"A" '(:ignore t :which-key "applications")
	"Af" '(:ignore t :which-key "feed")
	"Afu" '(elfeed-update :which-key "update feed")
	"Afs" '(elfeed :which-key "show feed"))

  (mk/leader-keys
	"f" '(:ignore t :which-key "files")
	"fs" '(save-buffer :which-key "save file")
	"fo" '(dired :which-key "open file")
	"ff" '(counsel-find-file :which-key "find file")
	"fr" '(counsel-recentf :which-key "recent files")
	"fn" '(create-file-buffer :which-key "new file")
	"fR" '(dired-rename-file :which-key "rename file")
	"fD" '(delete-file :which-key "delete file")
	"fe" '(lambda () (interactive) (find-file user-init-file) :which-key "user configuration"))

  (mk/leader-keys
    "c" '(:ignore t :which-key "code")
    "cp" 'check-parens
    "co" 'projectile-find-other-file
    "cl" '(comment-line :which-key "comment line")
    "cr" '(comment-region :which-key "comment region")
    "cu" '(lsp-ui-imenu :which-key "lsp-ui-menu")
    "ce" '(lsp-treemacs-errors-list :which-key "treemacs errors")
    "ct" '(lsp-treemacs-symbols :which-key "treemacs symbols")
    "cf" '(lsp-ivy-global-workspace-symbol :which-key "find symbol in workspace"))

  (mk/leader-keys
    "q" '(:ignore t :which-key "quit")
    "qq" 'save-buffers-kill-terminal
    "qr" 'restart-emacs)

  (mk/leader-keys
    "t" '(:ignore t :which-key "text")
    "ts" '(sort-lines :which-key "sort lines")
    "tw" '(:ignore t :which-key "whitespace")
    "twx" '(delete-trailing-whitespace :which-key "delete trailing whitespace"))

   (mk/leader-keys
     "b" '(:ignore t :which-key "buffer")
     "bb" '(ivy-switch-buffer :which-key "list buffers")
     "bx" '(evil-delete-buffer :which-key "delete buffer")
     "bk" '((lambda () (interactive) (kill-other-buffers)) :which-key "kill other buffers")
     "bd" '(kill-current-buffer :which-key "kill current buffer")
     "bp" '(previous-buffer :which-key "previous buffer")
     "bn" '(next-buffer :which-key "next buffer")
     "be" '(eval-buffer :which-key "eval buffer")
     "bE" '(eval-last-sexp :which-key "eval to point")
     "br" '(revert-buffer :which-key "revert buffer")
     "bC" '((lambda () (interactive) (switch-to-buffer "*Compile-Log*")) :which-key "Compile log-buffer")
     "bD" '((lambda () (interactive) (switch-to-buffer "*dashboard*")) :which-key "dashboard-buffer")
     "bm" '((lambda () (interactive) (switch-to-buffer "*Messages*")) :which-key "messages-buffer")
     "bs" '((lambda () (interactive) (switch-to-buffer "*scratch*")) :which-key "scratch-buffer"))

   (mk/leader-keys
     "h" '(:ignore t :which-key "help")
     "hc" '(helpful-command :which-key "describe command")
     "hk" '(helpful-key :which-key "describe key")
     "hf" '(counsel-describe-function :which-key "describe function")
     "hv" '(counsel-describe-variable :which-key "describe variable")
     "ht" '(evil-tutor-start :which-key "evil tutorial")
     "hp" '(helpful-at-point :which-key "describe at-point"))

   (mk/leader-keys
     "w" '(:ignore t :which-key "windows")
	 "wb" '((lambda () (interactive) (mk/browser-split-vertically)) :which-key "start a browser")
     "wp" '(previous-window-any-frame :which-key "previous window")
     "wx" '(delete-window :which-key "delete window")
	 "wk" '(delete-window-internal :which-key "delete window")
	 "w-" '((lambda () (interactive) (split-window-below) (other-window 1)) :which-key "split window horizontally")
	 "w/" '((lambda () (interactive) (split-window-right) (other-window 1)) :which-key "split window vertically")
     "wn" '(next-window-any-frame :which-key "next window")
	 )

   (mk/leader-keys
     "p" '(:ignore t :which-key "project")
     "pp" '(:ignore t :which-key "project management")
     "ppa" '(treemacs-add-project-to-workspace :which-key "add project")
     "ppr" '(treemacs-remove-project-from-workspace :which-key "remove project")
     "pf" '(projectile-find-file :which-key "find file")
     "pt" '(projectile-find-tag :which-key "find tag")
     "pF" '(projectile-project-files :which-key "project files")
     "pk" '(projectile-kill-buffers :which-key "kill buffers")
     "ps" '(projectile-switch-project :which-key "switch project")
     "pS" '(projectile-switch-open-project :which-key "switch open project"))

   (mk/leader-keys
     "v" '(:ignore t :which-key "version control")
     "vs" '(magit-status :which-key "status"))

   (mk/leader-keys
     "g" '(:ignore t :which-key "games")
     "gt" '(tetris :which-key "tetris")
     "gh" '(hanoi :which-key "tower of hanoi"))

   (mk/leader-keys
     "T" '(:ignore t :which-key "tabs")
     "Tn" '(tab-new :which-key "new")
     "Tl" '(tab-list :which-key "list")
     "Tg" '(tab-close-group :which-key "close group")
     "Td" '(tab-detach :which-key "detach")
     "Tx" '(tab-close :which-key "close")
     "Tk" '(tab-close-other :which-key "close other"))
   )

;; Forge - Git PR, Issues, etc
;;(use-package forge)

(defun mk/org-mode-setup()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . mk/org-mode-setup)
  :config
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (setq org-ellipsis " ▾"
	org-hide-emphasis-markers t
	org-hide-leading-stars t
	org-agenda-start-with-log-mode t
	org-log-into-drawer t
	org-log-done 'time
	org-agenda-files '("~/Library/Mobile Documents/com~apple~CloudDocs/orgfiles/")))

(with-eval-after-load 'org
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((emacs-lisp t)))
  (setq org-confirm-babel-evaluate nil)
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun mk/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . mk/org-mode-visual-fill))

(use-package elfeed
  :commands elfeed
  :config
  (setq elfeed-feeds '(
		       ("https://news.ycombinator.com/rss")
		       ("https://www.reddit.com/r/emacs.rss")
		       ("https://www.reddit.com/r/swift.rss")
			   ("https://www.osnews.com/feed/")
			   ("https://www.feber.se/rss/")
		       ("https://www.reddit.com/r/haikuos.rss"))))

(setq-default elfeed-search-filter "@2-days-ago +unread")
(setq-default elfeed-search-title-max-width 100)
(setq-default elfeed-search-title-min-width 100)

(use-package highlight-indent-guides
  :defer t
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom (highlight-indent-guides-method 'bitmap))

(use-package highlight-symbol
  :hook (prog-mode . highlight-symbol-mode)
  :config
  (setq highlight-symbol-idle-delay 0.3))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

(use-package emojify
  :config
  :hook (after-init . global-emojify-mode))

;; Reset memory for Garbage collection
(setq gc-cons-threshold (* 5 1024 1024))

(provide 'init)

;;; init.el ends here
