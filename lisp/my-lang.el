;;; my-lang.el --- 言語・ファイル種別ごとの設定 -*- lexical-binding: t -*-

;;; Commentary:
;; 言語モードは使うものから都度追加していく。いまは markdown のみ。

;;; Code:

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode))
  :custom
  (markdown-fontify-code-blocks-natively t))

(provide 'my-lang)
;;; my-lang.el ends here
