;===============================================================================
;; key mapping
;===============================================================================
;; Command-Key and Option-Key
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;; Backslashes
(define-key global-map [165] nil)
(define-key global-map [67109029] nil)
(define-key global-map [134217893] nil)
(define-key global-map [201326757] nil)
(define-key function-key-map [165] [?\\])
(define-key function-key-map [67109029] [?\C-\\])
(define-key function-key-map [134217893] [?\M-\\])
(define-key function-key-map [201326757] [?\C-\M-\\])

;===============================================================================
;; font setting
;;      あああああいいいいいうううううえええええおおおおお
;;      00112233440011223344001122334400112233440011223344
;===============================================================================
;; フォントセットを作る
(let* ((fontset-name "myfonts") ; フォントセットの名前
       ;; (size 14) ; ASCIIフォントのサイズ [9/10/12/14/15/17/19/20/...]
       ;; (asciifont "Droid Sans Mono Slashed") ; ASCIIフォント
       ;; (jpfont "Hiragino Maru Gothic ProN") ; 日本語フォント
       (size my-screen-font-size) ; ASCIIフォントのサイズ [9/10/12/14/15/17/19/20/...]
       (asciifont "Ricty") ; ASCIIフォント
       (jpfont "Ricty") ; 日本語フォント
       (font (format "%s-%d:weight=normal:slant=normal" asciifont size))
       (fontspec (font-spec :family asciifont))
       (jp-fontspec (font-spec :family jpfont)) 
       (fsn (create-fontset-from-ascii-font font nil fontset-name)))
  (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
  (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
  (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec) ; 半角カナ
  (set-fontset-font fsn '(#x0080 . #x024F) fontspec) ; 分音符付きラテン
  (set-fontset-font fsn '(#x0370 . #x03FF) fontspec) ; ギリシャ文字
  )

(add-to-list 'default-frame-alist '(font . "fontset-myfonts"))

;; フォントサイズの比を設定
(dolist (elt '(("^-apple-hiragino.*" . 1.2)
               (".*osaka-bold.*" . 1.2)
               (".*osaka-medium.*" . 1.2)
               (".*courier-bold-.*-mac-roman" . 1.0)
               (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
               (".*monaco-bold-.*-mac-roman" . 0.9)))
  (add-to-list 'face-font-rescale-alist elt))

;; デフォルトのフレームパラメータでフォントセットを指定
;; # これは起動時に default-frame-alist に従ったフレームが
;; # 作成されない現象への対処
(set-face-font 'default "fontset-myfonts")

;; drag & dropでウインドウを開かない
(setq ns-pop-up-frames nil)
