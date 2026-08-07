;;; my-ui.el --- 見た目まわり -*- lexical-binding: t -*-

;;; Commentary:
;; テーマ・行の強調・不可視文字の可視化。

;;; Code:

;; ダーク系の組み込みテーマ
(load-theme 'modus-vivendi t)

;; 現在行を強調
(global-hl-line-mode 1)

;; 対応する括弧をハイライト (標準で有効だがスタイルだけ指定)
(setq show-paren-style 'mixed)

;; 行番号は C-c n でトグル
(setq display-line-numbers-width 4)
(keymap-global-set "C-c n" #'display-line-numbers-mode)

;; 全角スペース・タブ・行末スペースの可視化 (旧 jaspace の後継)
(use-package whitespace
  :ensure nil
  :hook ((prog-mode text-mode conf-mode) . whitespace-mode)
  :custom
  (whitespace-style '(face tabs tab-mark spaces space-mark trailing))
  ;; 可視化するスペースは全角スペースのみ
  (whitespace-space-regexp "\\(　+\\)")
  (whitespace-display-mappings '((space-mark ?　 [?□])
                                 (tab-mark ?\t [?^ ?\t])))
  :custom-face
  (whitespace-space ((t (:foreground "green"))))
  (whitespace-tab ((t (:foreground "red" :underline t))))
  (whitespace-trailing ((t (:foreground "red" :underline t)))))

(provide 'my-ui)
;;; my-ui.el ends here
