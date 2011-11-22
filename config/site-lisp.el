;===============================================================================
;; site-lisp
;===============================================================================
;; カラーテーマ
(require 'color-theme)
(eval-after-load "color-theme"
  (color-theme-dark-laptop))

;; physica-line
;; http://www.scythe.jp/lab/physical-line.html
(require 'physical-line)
(physical-line-mode t)
(global-set-key (kbd "C-e") 'end-of-visual-line)
(global-set-key (kbd "C-S-e") 'end-of-line)

;; markdown-mode
;; http://jblevins.org/projects/markdown-mode/
;; (package-install 'url "http://jblevins.org/projects/markdown-mode/markdown-mode.el" 'markdown-mode)
;; (package-install-from-url "http://jblevins.org/projects/markdown-mode/markdown-mode.el")
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; actionscript-mode
;; (package-install 'emacswiki "actionscript-mode-haas-7.0.el" 'actionscript-mode)
(require 'actionscript-mode)
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))

;; rst-mode
;; Emacs起動時にrst.elを読み込み
(require 'rst)
;; 拡張子の*.rst, *.restのファイルをrst-modeで開く
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))
;; 背景が黒い場合はこうしないと見出しが見づらい
(setq frame-background-mode 'dark)
;; 全部スペースでインデントしましょう
(add-hook 'rst-mode-hook '(lambda() (setq indent-tabs-mode nil)))

;; htmlize
(require 'htmlize)

;; lua-mode
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;; htmlize
(require 'mediawiki)

;; php-mode
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; js-mode : apply to .json
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;; jaspace.el
;; http://openlab.dino.co.jp/2008/08/29/230500336.html
;; タブ, 全角スペース、改行直前の半角スペースを表示する
(when (require 'jaspace nil t)
  (when (boundp 'jaspace-modes)
    (setq jaspace-modes (append jaspace-modes
                                (list 'php-mode
                                      'yaml-mode
                                      'javascript-mode
                                      'js-mode
                                      'js2-mode
                                      'ruby-mode
                                      'text-mode
                                      'fundamental-mode))))
  (when (boundp 'jaspace-alternate-jaspace-string)
    (setq jaspace-alternate-jaspace-string "□"))
  (when (boundp 'jaspace-highlight-tabs)
    (setq jaspace-highlight-tabs ?^))
  (add-hook 'jaspace-mode-off-hook
            (lambda()
              (when (boundp 'show-trailing-whitespace)
                (setq show-trailing-whitespace nil))))
  (add-hook 'jaspace-mode-hook
            (lambda()
              (progn
                (when (boundp 'show-trailing-whitespace)
                  (setq show-trailing-whitespace t))
                (face-spec-set 'jaspace-highlight-jaspace-face
                               '((((class color) (background light))
                                  (:foreground "blue"))
                                 (t (:foreground "green"))))
                (face-spec-set 'jaspace-highlight-tab-face
                               '((((class color) (background light))
                                  (:foreground "red"
                                               :background "unspecified"
                                               :strike-through nil
                                               :underline t))
                                 (t (:foreground "red"
                                                 :background "unspecified"
                                                 :strike-through nil
                                                 :underline t))))
                (face-spec-set 'trailing-whitespace
                               '((((class color) (background light))
                                  (:foreground "red"
                                               :background "unspecified"
                                               :strike-through nil
                                               :underline t))
                                 (t (:foreground "red"
                                                 :background "unspecified"
                                                 :strike-through nil
                                                 :underline t))))))))
