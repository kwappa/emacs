;;; early-init.el --- 起動前設定 -*- lexical-binding: t -*-

;;; Commentary:
;; init.el より先に読まれる。GC 抑制と表示まわりの最低限だけを置く。

;;; Code:

;; 起動中は GC を抑制し、起動後に現実的な値へ戻す
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 16 1024 1024))))

;; メニューバーを表示しない
(menu-bar-mode -1)

;; ビープ音・画面フラッシュなし
(setq ring-bell-function 'ignore)

;;; early-init.el ends here
