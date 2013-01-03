;*******************************************************************************
;;;; -*- mode: lisp-interaction; syntax: elisp; coding: utf-8 -*-
;*******************************************************************************
;;; ロードパスの追加
(setq load-path (append
                 '("~/.emacs.d"
                   "~/.emacs.d/packages"
                   "~/.emacs.d/site-lisp")
                 load-path))

;;; Localeに合わせた環境の設定
(set-locale-environment nil)

;; デフォルトはUTF-8
(set-default-coding-systems 'utf-8)

;; 行 / 列番号を表示
(column-number-mode t)
(line-number-mode   t)

;; タブ幅を4に / インデントをタブに固定
(setq-default default-tab-width 4)
(setq-default indent-tabs-mode  nil)
(setq tab-stop-list
  '(      4   8  12  16  20  24  28  32  36  40  44  48  52  56  60
     64  68  72  80  84  88  92  96 100 104 108 112 116 120 124 128
    132 136 140 144 148 152 156 160 164 168 172 176 180 184 188 192
    196 200 204 208 212 216 220 224 228 232 236 240 244 248 252 256))

;; ツールバー / メニューバーを表示しない
(if window-system
    (progn
      (tool-bar-mode  0)
      (menu-bar-mode -1)
      ))

;; ビープ音,画面フラッシュなし
(setq ring-bell-function 'ignore)

;; [#]のつくバックアップファイルを作らない
(setq auto-save-default nil)

;===============================================================================
;; key bindings
;===============================================================================
;; ^hをBackSpaceに
(global-set-key "\C-h" 'delete-backward-char)

;; Ctrl + , / TAB : ウインドウ間移動
(global-set-key [?\C-,] 'other-window)

;; スクロール設定
(setq scroll-conservatively 35
      scroll-margin          0
      scroll-step            1)
(setq comint-scroll-show-maximum-output t)

;; M-g : 指定行にジャンプ
(global-set-key "\M-g" 'goto-line)

;; メール作成を無効に
(global-unset-key "\C-xm")

;; suspend-frameを無効に
(global-unset-key "\C-z")

;; 括弧をハイライト
(show-paren-mode t)

;; ファンクションキーでキーボードマクロ
(global-set-key [f3] 'start-kbd-macro)
(global-set-key [f4] 'end-kbd-macro)
(global-set-key [f5] 'call-last-kbd-macro)

;; 行番号表示
(setq linum-format "%4d ")
(global-set-key [M-f8] 'global-linum-mode)

;; コメント / アンコメント
(global-set-key [?\C-\;]    'comment-region)
(global-set-key [?\C-\M-\;] 'uncomment-region)

;; 単語移動を単語の先頭に
(defun next-word (&optional n)
  (interactive "p")
  (and (forward-word (1+ n)) (backward-word 1)))
(global-set-key "\M-f" 'next-word)

;===============================================================================
;; builtins.elのfunctionへのキーバインド
;===============================================================================
;; 折り返しのトグル
(global-set-key [f8] 'my-toggle-truncate)
;; WZ風page-up / down
(global-set-key "\C-v" 'my-scroll-up)
(global-set-key "\M-v" 'my-scroll-down)
;; 1行スクロール up / down
(global-set-key "\M-n" 'my-online-up)
(global-set-key "\M-p" 'my-online-down)
;; 入力文字で80桁までfill
(global-set-key "\C-cf" 'my-draw-line)

;===============================================================================
;;; 追加の設定
;===============================================================================
;; 標準Elispの設定
(load "config/builtins")
;; auto-installの設定
(load "config/auto-install")

;; 非標準Elispの設定
(load "config/packages")
;; パッケージマネージャで管理しないelisp
(load "config/site-lisp.el")

;===============================================================================
;;  環境判別
;===============================================================================
;; system-type predicates
(setq darwin-p  (eq system-type 'darwin)
      ns-p      (eq window-system 'ns)
      carbon-p  (eq window-system 'mac)
      linux-p   (eq system-type 'gnu/linux)
      cygwin-p  (eq system-type 'cygwin)
      nt-p      (eq system-type 'windows-nt)
      meadow-p  (featurep 'meadow)
      windows-p (or cygwin-p nt-p meadow-p))

;; 画面サイズ
(setq my-screen-font-size 14)

;; Mac - cocoa
(when ns-p
  (progn
    (load "env/cocoa.el")
  ))
;; Windows - ntemacs
(when nt-p
  (progn
    (load "env/ntemacs.el")
  ))
