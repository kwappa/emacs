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
