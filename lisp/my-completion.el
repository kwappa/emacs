;;; my-completion.el --- ミニバッファ補完・検索 -*- lexical-binding: t -*-

;;; Commentary:
;; vertico + orderless + marginalia + consult のスタック。
;; 旧設定の anything.el / browse-kill-ring の後継ポジション。

;;; Code:

;; ミニバッファ補完 UI
(use-package vertico
  :init
  (vertico-mode 1))

;; 順不同の部分一致でマッチング
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; 補完候補に説明を添える
(use-package marginalia
  :init
  (marginalia-mode 1))

;; 検索・ジャンプ系コマンド
(use-package consult
  :bind (("C-x b"   . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("M-y"     . consult-yank-pop)     ; kill-ring を一覧から選ぶ
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-s l"   . consult-line)         ; バッファ内検索
         ("M-s r"   . consult-ripgrep)))    ; プロジェクト横断 grep

(provide 'my-completion)
;;; my-completion.el ends here
