;;; my-keys.el --- グローバルキーバインド -*- lexical-binding: t -*-

;;; Commentary:
;; パッケージに紐づかないグローバルキーバインド。
;; パッケージ固有のバインドは各モジュールの use-package :bind に書く。
;; ターミナル専用のため C-, や C-; のような端末が送れないキーは使わない。

;;; Code:

;; C-h は Backspace (ヘルプは F1 または M-x help-for-help)
(keymap-global-set "C-h" #'delete-backward-char)

;; ウインドウ間移動
(keymap-global-set "M-o" #'other-window)

;; 1行スクロール up / down
(keymap-global-set "M-n" #'scroll-up-line)
(keymap-global-set "M-p" #'scroll-down-line)

;; 単語移動は次の単語の先頭へ
(autoload 'forward-to-word "misc")
(keymap-global-set "M-f" #'forward-to-word)

;; メール作成と suspend を無効化
(keymap-global-unset "C-x m")
(keymap-global-unset "C-z")

;; キーボードマクロ: F3 で記録開始 / F4 で終了・再生 (Emacs 標準のまま)
;; F5 でも再生できるようにしておく
(keymap-global-set "<f5>" #'kmacro-end-and-call-macro)

;; 折り返しのトグル
(keymap-global-set "<f8>" #'toggle-truncate-lines)

(provide 'my-keys)
;;; my-keys.el ends here
