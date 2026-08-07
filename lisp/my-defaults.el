;;; my-defaults.el --- 編集の基本設定 -*- lexical-binding: t -*-

;;; Commentary:
;; 文字コード・インデント・スクロールなど、パッケージに依存しない基本設定。

;;; Code:

(eval-when-compile
  (require 'dired)
  (require 'ls-lisp))
(declare-function dired-mark "dired")
(declare-function dired-up-directory "dired")

;; 文字コードは UTF-8 を基本に
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; インデントはスペース・幅4
(setq-default indent-tabs-mode nil
              tab-width 4)

;; モードラインに行・桁番号を表示
(column-number-mode 1)

;; スクロールは1行ずつ / C-v・M-v はバッファ端で止まらず先頭・末尾まで移動
(setq scroll-conservatively 101
      scroll-margin 0
      scroll-error-top-bottom t)

;; バックアップ・自動保存・ロックファイルを作らない
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; undo したら redo 側の履歴は捨てる (C-M-_ で redo)
(setq undo-no-redo t)

;; ミニバッファ履歴・最近使ったファイル・カーソル位置を永続化
(savehist-mode 1)
(recentf-mode 1)
(save-place-mode 1)

;; 外部で変更されたファイルを自動で読み直す
(global-auto-revert-mode 1)

;; プレフィクスキーの続きを表示 (Emacs 30 組み込み)
(which-key-mode 1)

;; dired
(with-eval-after-load 'dired
  ;; ディレクトリを先頭に配置する
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil
        ls-lisp-dirs-first t)
  ;; 再帰コピー / 削除
  (setq dired-recursive-copies 'always
        dired-recursive-deletes 'always)
  ;; 2窓で開いているときはもう片方をコピー先の初期値に
  (setq dired-dwim-target t)
  ;; Delete で親ディレクトリに
  (keymap-set dired-mode-map "DEL" #'dired-up-directory)
  ;; スペースでマークをトグルする (FD like)
  (keymap-set dired-mode-map "SPC" #'my/dired-toggle-mark))

(defun my/dired-toggle-mark (arg)
  "ポイント位置 (または続く ARG 個) のファイルのマークをトグルする。"
  (interactive "P")
  (let ((dired-marker-char
         (if (save-excursion (beginning-of-line) (looking-at-p " "))
             dired-marker-char ?\s)))
    (dired-mark arg)))

(provide 'my-defaults)
;;; my-defaults.el ends here
