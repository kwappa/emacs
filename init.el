;;; init.el --- kwappa's Emacs configuration -*- lexical-binding: t -*-

;;; Commentary:
;; ターミナル専用のモダン Emacs 設定 (Emacs 30+ 前提)。
;; 設定本体は lisp/ 以下に目的別のモジュールとして分割している。

;;; Code:

;; M-x customize の書き出し先を隔離する (リポジトリにはコミットしない)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; パッケージ管理: 組み込みの package.el + use-package (Emacs 30 同梱)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq use-package-always-ensure t)

;; batch 実行 (emacs --batch -l init.el での動作検証) では起動時の
;; 自動 activate が走らないため、明示的に初期化する
(when noninteractive
  (package-initialize))

;; モジュール読み込み
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'my-defaults)
(require 'my-keys)
(require 'my-completion)
(require 'my-ui)
(require 'my-lang)

;;; init.el ends here
