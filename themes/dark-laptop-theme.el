;;; dark-laptop-theme.el --- dark-laptop

(deftheme dark-laptop
  "dark-laptop")

(custom-theme-set-faces 'dark-laptop
			'(default ((t (:stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "apple" :family "Monaco"))))
			'(bold ((t (:weight bold))))
			'(italic ((t (:slant italic))))
			'(bold-italic ((t (:slant italic :weight bold))))
			'(underline ((t (:underline t))))
			'(fixed-pitch ((t (:family "Monospace"))))
			'(variable-pitch ((t (:family "Sans Serif"))))
			'(shadow ((t (:foreground "grey70"))))
			'(link ((t (:foreground "cyan1" :underline t))))
			'(link-visited ((t (:foreground "violet"))))
			'(highlight ((t (:background "darkolivegreen"))))
			'(region ((t (:background "blue"))))
			'(secondary-selection ((t (:background "darkslateblue"))))
			'(trailing-whitespace ((t (:background "red1"))))
			'(escape-glyph ((t (:foreground "cyan"))))
			'(nobreak-space ((t (:underline t))))
			'(mode-line ((t (:background "white" :foreground "black" :box (:line-width -1 :style released-button)))))
			'(mode-line-inactive ((t (:background "grey30" :foreground "grey80" :box (:line-width -1 :color "grey40" :style nil) :weight light))))
			'(mode-line-highlight ((t (:box (:line-width 2 :color "grey40" :style released-button)))))
			'(mode-line-emphasis ((t (:weight bold))))
			'(mode-line-buffer-id ((t (:background "white" :foreground "black" :weight bold))))
			'(header-line ((t (:background "grey20" :foreground "grey90" :box nil))))
			'(minibuffer-prompt ((t (:foreground "cyan"))))
			'(fringe ((t (:background "grey10"))))
			'(cursor ((t (:background "yellow"))))
			'(tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))
			'(menu ((t (:inverse-video t))))
			'(button ((t (:underline t))))
			'(font-lock-comment-face ((t (:foreground "OrangeRed"))))
			'(font-lock-string-face ((t (:foreground "LightSalmon"))))
			'(font-lock-keyword-face ((t (:foreground "Cyan"))))
			'(font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
			'(font-lock-function-name-face ((t (:foreground "LightSkyBlue"))))
			'(font-lock-variable-name-face ((t (:foreground "LightGoldenrod"))))
			'(font-lock-type-face ((t (:foreground "PaleGreen"))))
			'(font-lock-constant-face ((t (:foreground "Aquamarine"))))
			'(font-lock-warning-face ((t (:foreground "Pink" :weight bold))))
			'(font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
			'(isearch ((t (:background "blue"))))
			'(isearch-fail ((t (:background "red4"))))
			'(lazy-highlight ((t (:background "paleturquoise4"))))
			'(match ((t (:background "RoyalBlue3"))))
			'(buffer-menu-buffer ((t (:weight bold))))
			'(ns-working-text-face ((t (:underline t))))
			'(tooltip ((t (:background "lightyellow" :foreground "black"))))
			'(widget-documentation ((t (:foreground "lime green"))))
			'(widget-button ((t (:weight bold))))
			'(widget-field ((t (:background "dim gray"))))
			'(widget-single-line-field ((t (:background "dim gray"))))
			'(widget-inactive ((t (:foreground "light gray"))))
			'(widget-button-pressed ((t (:foreground "red"))))
			'(zmacs-region ((t (:background "blue"))))
			'(primary-selection ((t (:background "blue"))))
			'(modeline-mousable-minor-mode ((t (:background "white" :foreground "black"))))
			'(modeline-mousable ((t (:background "white" :foreground "black"))))
			'(message-separator-face ((t (:foreground "blue3"))))
			'(message-mml-face ((t (:background "Green3" :weight bold))))
			'(message-header-xheader-face ((t (:foreground "light blue" :weight bold))))
			'(message-header-to-face ((t (:foreground "cyan" :weight bold))))
			'(message-header-subject-face ((t (:foreground "yellow" :weight bold))))
			'(message-header-other-face ((t (:foreground "chocolate" :weight bold))))
			'(message-header-newsgroups-face ((t (:foreground "violet" :weight bold))))
			'(message-header-name-face ((t (:foreground "orange" :weight bold))))
			'(message-header-cc-face ((t (:foreground "green4" :weight bold))))
			'(message-cited-text-face ((t (:foreground "red" :weight bold))))
			'(gnus-summary-selected-face ((t (:underline t))))
			'(gnus-summary-normal-ticked-face ((t (:foreground "pink"))))
			'(gnus-summary-normal-read-face ((t (:foreground "PaleGreen"))))
			'(gnus-summary-normal-ancient-face ((t (:foreground "SkyBlue"))))
			'(gnus-summary-low-unread-face ((t (:slant italic))))
			'(gnus-summary-low-ticked-face ((t (:foreground "pink" :slant italic))))
			'(gnus-summary-low-read-face ((t (:foreground "PaleGreen" :slant italic))))
			'(gnus-summary-low-ancient-face ((t (:foreground "SkyBlue" :slant italic))))
			'(gnus-summary-high-unread-face ((t (:weight bold))))
			'(gnus-summary-high-ticked-face ((t (:foreground "pink" :weight bold))))
			'(gnus-summary-high-read-face ((t (:foreground "PaleGreen" :weight bold))))
			'(gnus-summary-high-ancient-face ((t (:foreground "SkyBlue" :weight bold))))
			'(gnus-summary-cancelled-face ((t (:background "black" :foreground "yellow"))))
			'(gnus-splash-face ((t (:foreground "Brown"))))
			'(gnus-signature-face ((t (:foreground "khaki" :weight bold))))
			'(gnus-header-subject-face ((t (:foreground "orange" :weight bold))))
			'(gnus-header-newsgroups-face ((t (:foreground "purple" :slant italic :weight bold))))
			'(gnus-header-name-face ((t (:foreground "deep sky blue"))))
			'(gnus-header-from-face ((t (:foreground "spring green" :weight bold))))
			'(gnus-header-content-face ((t (:foreground "forest green" :slant italic))))
			'(gnus-group-news-low-face ((t (:foreground "DarkTurquoise" :weight bold))))
			'(gnus-group-news-low-empty-face ((t (:foreground "DarkTurquoise"))))
			'(gnus-group-news-6-face ((t (:weight bold))))
			'(gnus-group-news-5-face ((t (:weight bold))))
			'(gnus-group-news-4-face ((t (:weight bold))))
			'(gnus-group-news-3-face ((t (:weight bold))))
			'(gnus-group-news-2-face ((t (:foreground "turquoise" :weight bold))))
			'(gnus-group-news-2-empty-face ((t (:foreground "turquoise"))))
			'(gnus-group-news-1-face ((t (:foreground "PaleTurquoise" :weight bold))))
			'(gnus-group-news-1-empty-face ((t (:foreground "PaleTurquoise"))))
			'(gnus-group-mail-low-face ((t (:foreground "aquamarine4" :weight bold))))
			'(gnus-group-mail-low-empty-face ((t (:foreground "aquamarine4"))))
			'(gnus-group-mail-3-face ((t (:foreground "aquamarine3" :weight bold))))
			'(gnus-group-mail-3-empty-face ((t (:foreground "aquamarine3"))))
			'(gnus-group-mail-2-face ((t (:foreground "aquamarine2" :weight bold))))
			'(gnus-group-mail-2-empty-face ((t (:foreground "aquamarine2"))))
			'(gnus-group-mail-1-face ((t (:foreground "aquamarine1" :weight bold))))
			'(gnus-group-mail-1-empty-face ((t (:foreground "aquamarine1"))))
			'(gnus-emphasis-underline-italic ((t (:underline t :slant italic))))
			'(gnus-emphasis-underline-bold-italic ((t (:underline t :slant italic :weight bold))))
			'(gnus-emphasis-underline-bold ((t (:underline t :weight bold))))
			'(gnus-emphasis-underline ((t (:underline t))))
			'(gnus-emphasis-italic ((t (:slant italic))))
			'(gnus-emphasis-highlight-words ((t (:background "black" :foreground "yellow"))))
			'(gnus-emphasis-bold-italic ((t (:slant italic :weight bold))))
			'(gnus-emphasis-bold ((t (:weight bold))))
			'(gnus-cite-face-9 ((t (:foreground "violet"))))
			'(gnus-cite-face-8 ((t (:foreground "magenta"))))
			'(gnus-cite-face-7 ((t (:foreground "orange"))))
			'(gnus-cite-face-6 ((t (:foreground "chocolate" :weight bold))))
			'(gnus-cite-face-5 ((t (:foreground "pale green"))))
			'(gnus-cite-face-4 ((t (:foreground "light pink"))))
			'(gnus-cite-face-3 ((t (:foreground "gold" :weight bold))))
			'(gnus-cite-face-2 ((t (:foreground "cyan" :weight bold))))
			'(gnus-cite-face-11 ((t (:foreground "turquoise"))))
			'(gnus-cite-face-10 ((t (:foreground "medium purple"))))
			'(gnus-cite-face-1 ((t (:foreground "deep sky blue" :weight bold))))
			'(gnus-cite-attribution-face ((t (:slant italic))))
			'(font-lock-reference-face ((t (:foreground "LightSteelBlue"))))
			'(font-lock-doc-string-face ((t (:foreground "LightSalmon"))))
			'(fl-type-face ((t (:foreground "yellow"))))
			'(fl-string-face ((t (:foreground "green"))))
			'(fl-keyword-face ((t (:foreground "cyan"))))
			'(fl-function-name-face ((t (:foreground "red"))))
			'(fl-doc-string-face ((t (:foreground "purple"))))
			'(fl-comment-face ((t (:foreground "pink"))))
			'(custom-variable-tag-face ((t (:foreground "light blue" :underline t))))
			'(custom-variable-button-face ((t (:underline t :weight bold))))
			'(custom-state-face ((t (:foreground "lime green"))))
			'(custom-set-face ((t (:background "white" :foreground "blue"))))
			'(custom-saved-face ((t (:underline t))))
			'(custom-rogue-face ((t (:background "black" :foreground "pink"))))
			'(custom-modified-face ((t (:background "blue" :foreground "white"))))
			'(custom-invalid-face ((t (:background "red" :foreground "yellow"))))
			'(custom-group-tag-face-1 ((t (:foreground "pink" :underline t))))
			'(custom-group-tag-face ((t (:foreground "light blue" :underline t))))
			'(custom-face-tag-face ((t (:underline t))))
			'(custom-changed-face ((t (:background "blue" :foreground "white")))))

(provide-theme 'dark-laptop)

;;; dark-laptop-theme.el ends here

