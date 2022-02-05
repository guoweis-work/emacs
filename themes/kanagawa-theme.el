;;; commentary: Kanagawa based theme

;;; Code:
(eval-when-compile
  (require 'cl-lib))

(require 'autothemer)

(unless (>= emacs-major-version 24)
  (error "Requires Emacs 24 or later"))


(autothemer-deftheme
	kanagawa "A theme based on kanagawa color scheme"

 ((((class color) (min-colors #xFFFFFF))        ; col 1 GUI/24bit
   ((class color) (min-colors #xFF)))           ; col 2 Xterm/256

  ;; Define our color palette
  (fujiWhite		"#DCD7BA" "#ffffff")
  (old-white		"#C8C093" "#ffffff")
  (black			"#1E1F28" "#080808")

  (sumiInk-0		"#16161D" "#000000")
  (sumiInk-1		"#1F1F28" "#080808")
  (sumiInk-2		"#2A2A37" "#121212")
  (sumiInk-2		"#2A2A37" "#121212")
  (sumiInk-3		"#363646" "#303030")
  (sumiInk-4		"#54546D" "#303030")

  (waveBlue-1		"#223249" "#4e4e4e")
  (waveBlue-2		"#2D4F67" "#585858")

  (winterGreen		"#2B3328" "#585858")
  (winterYellow		"#49443C" "#585858")
  (winterRed		"#43242B" "#585858")
  (winterBlue		"#252535" "#585858")

  (autumnGreen		"#76946A" "#585858")
  (autumnRed		"#C34043" "#585858")
  (autumnYellow		"#DCA561" "#585858")

  (samuraiRed		"#E82424" "#585858")
  (roninYellow		"#FF9E3B" "#585858")

  (roninYellow		"#FF9E3B" "#585858")

  (waveAqua1		"#6A9589" "#6a9589")
  (dragonBlue		"#658594" "#658594")
  (fujiGray			"#727169" "#717C7C")
  (springViolet1	"#938AA9" "#717C7C")
  (oniViolet		"#957FB8" "#717C7C")
  (crystalBlue		"#7E9CD8" "#717C7C")
  (springViolet2	"#9CABCA" "#717C7C")
  (springBlue		"#7FB4CA" "#717C7C")
  (lightBlue		"#A3D4D5" "#717C7C")
  (waveAqua2		"#7AA89F" "#717C7C")
  (springGreen		"#98BB6C" "#717C7C")
  (boatYellow1		"#938056" "#717C7C")
  (boatYellow2		"#C0A36E" "#717C7C")
  (carpYellow		"#E6C384" "#717C7C")
  (sakuraPink		"#D27E99" "#717C7C")
  (waveRed			"#E46876" "#717C7C")
  (peachRed			"#FF5D62" "#717C7C")
  (surimiOrange		"#FFA066" "#717C7C")
  (katanaGray		"#717C7C" "#717C7C")
  (comet			"#54536D" "#4e4e4e")
  )

 ;; Customize faces
  (
  (border                               (:background sumiInk-2		:foreground black))
  (button                               (:foreground waveAqua2))
  (child-frame		                    (:background sumiInk-0		:foreground sumiInk-1))
  (child-frame-border                   (:background sumiInk-1		:foreground sumiInk-1))
  (cursor                               (:background springViolet2	:foreground black))
  (default								(:background sumiInk-1		:foreground fujiWhite ))
  (error                                (:foreground peachRed))
  (fringe                               (:background sumiInk-2		:foreground waveBlue-2))
  (glyph-face                           (:background peachRed))
  (glyphless-char                       (:foreground waveBlue-2))
  (header-line							(:background black))
  (highlight                            (:background comet :foreground springViolet1))
  (hl-line                              (:background sumiInk-1))
  (homoglyph                            (:foreground lightBlue))
  (internal-border                      (:background sumiInk-2 :foreground sumiInk-2))
  (line-number                          (:foreground sumiInk-4))
  (line-number-current-line             (:foreground springViolet2 :background sumiInk-3 :bold t))
  (lv-separator                         (:foreground waveBlue-2 :background sumiInk-2))
  (match                                (:background carpYellow :foreground black))
  (menu                                 (:background black :foreground fujiWhite))
  (mode-line                            (:background sumiInk-0))
  (mode-line-inactive                   (:background nil :foreground dragonBlue :bold nil))
  (mode-line-active		                (:background sumiInk-0 :foreground old-white :bold nil))
  (mode-line-highlight                  (:foreground boatYellow2))
  (mode-line-buffer-id                  (:foreground waveAqua2 :bold t))
  (numbers                              (:background sakuraPink))
  (region                               (:background waveBlue-1))
  (separator-line                       (:background sumiInk-2))
  (shadow                               (:background sumiInk-0))
  (success                              (:foreground waveAqua2))
  (vertical-border                      (:foreground sumiInk-3 :background nil))
  (warning                              (:foreground springGreen))
  (window-border                        (:background sumiInk-1 :foreground sumiInk-2))
  (window-divider-first-pixel           (:foreground sumiInk-1))
  (window-divider-last-pixel            (:background sumiInk-1))

  ;; Font lock
  (font-lock-type-face                  (:foreground waveAqua2))
  (font-lock-regexp-grouping-backslash  (:foreground boatYellow2))
  (font-lock-keyword-face               (:bold t :foreground oniViolet))
  (font-lock-warning-face               (:foreground roninYellow))
  (font-lock-constant-face              (:foreground carpYellow))
  (font-lock-string-face                (:foreground springGreen))
  (font-lock-builtin-face               (:foreground springBlue))
  (font-lock-reference-face				(:foreground peachRed))
  (font-lock-constant-face              (:foreground surimiOrange))
  (font-lock-function-name-face         (:foreground crystalBlue))
  (font-lock-variable-name-face         (:foreground peachRed))
  (font-lock-negation-char-face         (:foreground peachRed))
  (font-lock-comment-face               (:foreground fujiGray))
  (font-lock-comment-delimiter-face     (:foreground fujiGray))
  (font-lock-doc-face                   (:foreground comet))
  (font-lock-doc-markup-face            (:foreground comet))
  (font-lock-preprocessor-face	   		(:foreground boatYellow2))
  (elisp-shorthand-font-lock-face       (:foreground fujiWhite))

  (info-xref                            (:foreground carpYellow))
  (highlight-quoted-symbol              (:foreground springGreen))
  (minibuffer-prompt-end                (:foreground fujiWhite :background sumiInk-1))
  (minibuffer-prompt                    (:foreground fujiWhite :background sumiInk-1))
  (epa-mark                             (:foreground waveRed))
  (dired-mark                           (:foreground waveRed))

  (trailing-whitespace                  (:background comet))

  (mode-line (:background black :foreground fujiWhite :bold t))

  ;; ;; Battery colors
  (doom-modeline-battery-critical       (:foreground peachRed))
  (doom-modeline-battery-warning        (:foreground springGreen))
  (doom-modeline-battery-charging       (:foreground sumiInk-4))
  (doom-modeline-battery-error          (:foreground peachRed))
  (doom-modeline-battery-normal         (:foreground sumiInk-4))
  (doom-modeline-battery-full           (:foreground waveAqua2))

  ;; Doom visual state
  (doom-modeline-evil-motion-state      (:foreground lightBlue))
  (doom-modeline-evil-emacs-state       (:foreground crystalBlue))
  (doom-modeline-evil-insert-state      (:foreground springViolet2))
  (doom-modeline-evil-normal-state      (:foreground fujiWhite))
  (doom-modeline-evil-visual-state      (:foreground springBlue))
  (doom-modeline-evil-replace-state     (:foreground peachRed))
  (doom-modeline-evil-operator-state    (:foreground crystalBlue))

  (doom-modeline-project-dir            (:bold t :foreground waveAqua2))
  (doom-modeline-buffer-path            (:inherit 'bold :foreground waveAqua2))
  (doom-modeline-buffer-file            (:inherit 'bold :foreground springGreen))
  (doom-modeline-buffer-modified        (:inherit 'bold :foreground carpYellow))
  (doom-modeline-error                  (:background peachRed))
  (doom-modeline-buffer-major-mode      (:foreground waveAqua2 :bold t))
  (doom-modeline-info                   (:bold t :foreground lightBlue))
  (doom-modeline-project-dir            (:bold t :foreground surimiOrange))
  (doom-modeline-bar                    (:bold t :background springViolet1))
  (doom-modeline-panel                  (:inherit 'bold :background boatYellow2 :foreground sumiInk-2))

  ;;elfeed
  (elfeed-search-feed-face              (:foreground springViolet1))
  (elfeed-search-tag-face               (:foreground waveAqua2))

  ;; message colors
  (message-header-name                  (:foreground sumiInk-4))
  (message-header-other                 (:foreground surimiOrange))
  (message-header-subject               (:foreground carpYellow))
  (message-header-to                    (:foreground old-white))
  (message-header-cc                    (:foreground waveAqua2))
  (message-header-xheader               (:foreground old-white))
  (custom-link                          (:foreground crystalBlue))
  (link                                 (:foreground crystalBlue))

  ;; org-mode
  (org-done                             (:foreground dragonBlue))
  (org-code                             (:background black))
  (org-block                            (:background black))
  (org-block-begin-line                 (:background black :foreground dragonBlue))
  (org-block-end-line	                (:background black :foreground dragonBlue))
  (org-headline-done                    (:foreground dragonBlue :strike-through t))
  (org-todo                             (:foreground waveAqua2 :bold t))
  (org-headline-todo                    (:foreground sumiInk-2))
  (org-upcoming-deadline                (:foreground peachRed))
  (org-footnote                         (:foreground waveAqua2))
  (org-indent                           (:background sumiInk-2 :foreground sumiInk-2))
  (org-hide	                            (:background sumiInk-2 :foreground sumiInk-2))
  (org-date                             (:foreground waveBlue-2))
  (org-ellipsis                         (:foreground waveBlue-2 :bold t))
  (org-level-1                          (:foreground peachRed :height 1.3 :bold t))
  (org-level-2                          (:foreground springViolet2 :height 1.15 :bold t))
  (org-level-3                          (:foreground boatYellow2 :height 1.05))
  (org-level-4                          (:foreground fujiWhite))
  (org-level-5                          (:foreground fujiWhite))
  (org-level-6                          (:foreground carpYellow))
  (org-level-7                          (:foreground surimiOrange))
  (org-level-8                          (:foreground springGreen))

  ;; which-key
  (which-key-key-face                   (:inherit 'font-lock-variable-name-face))
  (which-func							(:inherit 'font-lock-function-name-face :bold t))
  (which-key-group-description-face     (:foreground waveRed))
  (which-key-command-description-face   (:foreground crystalBlue))
  (which-key-local-map-description-face (:foreground carpYellow))
  (which-key-posframe					(:background sumiInk-0))
  (which-key-posframe-border			(:background sumiInk-0))

  ;; swiper
  (swiper-line-face                     (:foreground carpYellow))
  (swiper-background-match-face-1       (:background surimiOrange :foreground black))
  (swiper-background-match-face-2       (:background crystalBlue :foreground black))
  (swiper-background-match-face-3       (:background boatYellow2 :foreground black))
  (swiper-background-match-face-4       (:background peachRed :foreground black))
  (swiper-match-face-1					(:inherit 'swiper-background-match-face-1))
  (swiper-match-face-2					(:inherit 'swiper-background-match-face-2))
  (swiper-match-face-3					(:inherit 'swiper-background-match-face-3))
  (swiper-match-face-4					(:inherit 'swiper-background-match-face-4))

  (counsel-outline-default              (:foreground carpYellow))
  (info-header-xref                     (:foreground carpYellow))
  (xref-file-header                     (:foreground carpYellow))
  (xref-match		                    (:foreground carpYellow))

  ;; rainbow delimiter
  (rainbow-delimiters-mismatched-face   (:foreground peachRed))
  (rainbow-delimiters-unmatched-face    (:foreground waveAqua2))
  (rainbow-delimiters-base-error-face   (:foreground peachRed))
  (rainbow-delimiters-base-face         (:foreground sumiInk-4))

  (rainbow-delimiters-depth-1-face      (:foreground springViolet2))
  (rainbow-delimiters-depth-2-face      (:foreground dragonBlue))
  (rainbow-delimiters-depth-3-face      (:foreground springViolet1))
  (rainbow-delimiters-depth-4-face      (:foreground springGreen))
  (rainbow-delimiters-depth-5-face      (:foreground waveAqua2))
  (rainbow-delimiters-depth-6-face      (:foreground carpYellow))
  (rainbow-delimiters-depth-7-face      (:foreground waveRed))
  (rainbow-delimiters-depth-8-face      (:foreground lightBlue))
  (rainbow-delimiters-depth-9-face      (:foreground springViolet2))

  ;; show-paren
  (show-paren-match								(:background surimiOrange :foreground sumiInk-0 :bold t))
  (show-paren-match-expression					(:background surimiOrange :foreground sumiInk-0 :bold t))
  (show-paren-mismatch							(:background peachRed :foreground old-white))

  (tooltip (:background black))

  ;; company-box  (company-tooltip            :inherit 'tooltip)
  (company-tooltip			  (:background black))
  (company-box				  (:background black))
  (company-tooltip-common     (:foreground carpYellow))
  (company-tooltip-search     (:background peachRed :foreground black :distant-foreground fujiWhite))
  (company-tooltip-selection  (:background comet :foreground peachRed :bold t))
  (company-tooltip-mouse      (:background peachRed :foreground black :distant-foreground fujiWhite))
  (company-tooltip-annotation (:foreground waveAqua2))
  (company-scrollbar-bg       (:inherit 'tooltip))
  (company-scrollbar-fg       (:background peachRed))
  (company-preview            (:foreground carpYellow))
  (company-preview-common     (:background peachRed :foreground black))
  (company-preview-search     (:inherit 'company-tooltip-search))
  (company-template-field     (:inherit 'match))

  (flycheck-posframe-background-face	(:background black))
  (flycheck-posframe-face				(:background black))
  (flycheck-posframe-info-face  		(:background black :foreground lightBlue))
  (flycheck-posframe-warning-face  		(:background black :foreground carpYellow))
  (flycheck-posframe-error-face  		(:background black :foreground springViolet2))
  (flycheck-fringe-warning				(:foreground carpYellow :background sumiInk-2))
  (flycheck-fringe-error				(:foreground peachRed :background sumiInk-2))
  (flycheck-fringe-info					(:foreground crystalBlue :background sumiInk-2))
  (flycheck-error-list-warning          (:foreground carpYellow :bold t))
  (flycheck-error-list-error            (:foreground peachRed :bold t))
  (flycheck-error-list-info             (:foreground crystalBlue :bold t))

  ;; indent dots
  (highlight-indent-guides-character-face       (:foreground sumiInk-3))
  (highlight-indent-guides-stack-character-face (:foreground sumiInk-3))
  (highlight-indent-guides-stack-odd-face       (:foreground sumiInk-3))
  (highlight-indent-guides-stack-even-face      (:foreground comet))
  (highlight-indent-guides-stack-character-face (:foreground sumiInk-3))
  (highlight-indent-guides-even-face            (:foreground sumiInk-2))
  (highlight-indent-guides-odd-face             (:foreground comet))

   ;;;; ivy
  (ivy-current-match                            (:background crystalBlue :foreground black :bold t))
  (ivy-action                                   (:background nil :foreground fujiWhite))
  (ivy-grep-line-number                         (:background nil :foreground springGreen))
  (ivy-minibuffer-match-face-1                  (:background nil :foreground waveRed))
  (ivy-minibuffer-match-face-2                  (:background nil :foreground springGreen))
  (ivy-minibuffer-match-highlight               (:foreground lightBlue))
  (ivy-grep-info                                (:foreground lightBlue))
  (ivy-grep-line-number                         (:foreground springViolet2))
  (ivy-confirm-face                             (:foreground waveAqua2))

  ;; posframe's
  (ivy-posframe                                 (:background black))
  (ivy-posframe-border                          (:inherit 'ivy-posframe))

  (treemacs-directory-collapsed-face			(:foreground fujiWhite))
  (treemacs-directory-face						(:foreground fujiWhite))
  (treemacs-file-face							(:foreground fujiWhite))

  (treemacs-git-added-face						(:foreground surimiOrange))
  (treemacs-git-renamed-face				   	(:foreground fujiWhite))
  (treemacs-git-ignored-face				   	(:foreground sumiInk-4))
  (treemacs-git-unmodified-face		   			(:foreground fujiWhite))
  (treemacs-git-renamed-face		   			(:foreground fujiWhite))
  (treemacs-git-modified-face		   			(:foreground springGreen))

  (tree-sitter-hl-face:constant					(:background peachRed))
  (tree-sitter-hl-face:constant.builtin			(:background peachRed))
  (tree-sitter-hl-face:function.call			(:background springViolet2))

  ;; lets support solaire mode
  (solaire-default-face (:background sumiInk-1))
  ;; lsp
  (lsp-headerline-breadcrumb-path-error-face (:underline (:color springGreen :style 'wave)
                                                         :foreground sumiInk-4 :background black))
  (lsp-headerline-breadcrumb-path-face				(:background black))
  (lsp-headerline-breadcrumb-path-hint-face	   		(:background black))
  (lsp-headerline-breadcrumb-path-info-face	   		(:background black))
  (lsp-headerline-breadcrumb-separator-face			(:background black))
  (lsp-headerline-breadcrumb-symbols-face			(:background black))
  (lsp-headerline-breadcrumb-project-prefix-face	(:background black))
  (lsp-headerline-breadcrumb-symbols-error-face     (:foreground peachRed))

  (lsp-ui-doc-background							(:background black :foreground peachRed))
  (lsp-ui-doc-header								(:background black :foreground peachRed))
  (lsp-ui-doc-border								(:background nil :foreground nil))
  (lsp-ui-peek-filename								(:foreground lightBlue))
  (lsp-ui-sideline-code-action			   			(:foreground carpYellow))
  (lsp-ui-sideline-current-symbol					(:foreground springBlue))
  (lsp-ui-sideline-symbol							(:foreground dragonBlue))

  ;; dashboard
  (dashboard-heading								(:foreground springViolet2 :bold t))
  (dashboard-items-face								(:bold nil :foreground fujiWhite))
  (dashboard-banner-logo-title						(:bold t :height 200))
  (dashboard-no-items-face							(:foreground sumiInk-4))

  ;; all-the-icons
  (all-the-icons-dgreen							(:foreground waveAqua2))
  (all-the-icons-green							(:foreground waveAqua2))
  (all-the-icons-dpurple						(:foreground springViolet2))
  (all-the-icons-purple							(:foreground springViolet2))

  ;; evil
  (evil-ex-substitute-replacement (:foreground surimiOrange :strike-through nil :inherit 'evil-ex-substitute-matches))
  (evil-search-highlight-persist-highlight-face (:background carpYellow))

  (term (:background sumiInk-0 :foreground sumiInk-0))
  (term-color-blue (:background crystalBlue :foreround crystalBlue))
  (term-color-bright-blue (:inherit 'term-color-blue))
  (term-color-green (:background waveAqua2 :foreround waveAqua2))
  (term-color-bright-green (:inherit 'term-color-green))
  (term-color-black (:background sumiInk-0 :foreground black))
  (term-color-bright-black (:background sumiInk-1 :foreground sumiInk-1))
  (term-color-white (:background fujiWhite :foreground fujiWhite))
  (term-color-bright-white (:background old-white :foreground old-white))
  (term-color-red (:background peachRed :foreground peachRed))
  (term-color-bright-red (:background springGreen :foreground springGreen))
  (term-color-yellow (:background carpYellow :foreground carpYellow))
  (term-color-bright-yellow (:background carpYellow :foreground carpYellow))
  (term-color-cyan (:background springBlue :foreground springBlue))
  (term-color-bright-cyan (:background springBlue :foreground springBlue))
  (term-color-magenta (:background springViolet2 :foreground springViolet2))
  (term-color-bright-magenta (:background springViolet2 :foreground springViolet2))

  (popup-face (:inherit 'tooltip))
  (popup-selection-face (:inherit 'tooltip))
  (popup-tip-face (:inherit 'tooltip))


  (anzu-match-1 (:foreground waveAqua2 :background sumiInk-2))
  (anzu-match-2 (:foreground carpYellow :background sumiInk-2))
  (anzu-match-3 (:foreground lightBlue :background sumiInk-2))

  (anzu-mode-line		(:foreground black :background springViolet2))
  (anzu-mode-no-match	(:foreground fujiWhite :background peachRed))
  (anzu-replace-to		(:foreground carpYellow :background winterRed))

  (ace-jump-face-background (:foreground waveBlue-2))
  (ace-jump-face-foreground (:foreground peachRed :background black :bold t))

  (vertico-current (:background sumiInk-3 :foreground springViolet2 :bold t))

 ))

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory
                   (file-name-directory load-file-name))))

(provide-theme 'kanagawa)
;;; kanagawa-theme.el ends here
