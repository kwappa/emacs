;===============================================================================
;; face
;===============================================================================
;; 現在行を強調
(defface hlline-face  '((t (:background "dark slate gray"))) nil)
(setq hl-line-face 'hlline-face)
(global-hl-line-mode)

;===============================================================================
;; diredの機能拡張
;===============================================================================
(load "dired-x")
;; ディレクトリは先頭に配置する
(load-library "ls-lisp")
(setq ls-lisp-use-insert-directory-program nil)
(setq ls-lisp-dirs-first t)

;; スペースでマークする (FD like)
(define-key dired-mode-map " " 'dired-toggle-mark)
(defun dired-toggle-mark (arg)
  "Toggle the current (or next ARG) files."
  ;; S.Namba Sat Aug 10 12:20:36 1996
  (interactive "P")
  (let ((dired-marker-char
         (if (save-excursion (beginning-of-line)
                             (looking-at " "))
             dired-marker-char "*")))
    (dired-mark arg)
;;    (dired-previous-line 1)
))
;; 再帰コピー / 削除
(setq dired-recursive-copies  'always)
(setq dired-recursive-deletes 'always)

;;==============================================================================
;; プログラミング言語設定
;;==============================================================================
(add-hook 'c-mode-common-hook
          '(lambda ()
             (c-set-style "k&r")
             (c-set-offset 'case-label     '+)
             (c-set-offset 'inline-open     0)
             (c-set-offset 'statement-cont  0)
             (c-set-offset 'arglist-close   0)
             (c-set-offset 'inexpr-class    0)
             (setq c-basic-offset 4 indent-tabs-mode nil)
             ))

;*******************************************************************************
;; my functions
;*******************************************************************************
;===============================================================================
;; 一行スクロール up / down
;===============================================================================
(defun my-online-up()
  "my online up"
  (interactive)
  (condition-case err
      (scroll-up 1)
    (end-of-buffer
     (forward-line 1))))
(defun my-online-down()
  "my online down"
  (interactive)
  (condition-case err
      (scroll-down 1)
    (beginning-of-buffer
     (forward-line -1))))

;===============================================================================
;; Page up / downをWZ風味に
;===============================================================================
(defun my-scroll-up()
  "my scroll up"
  (interactive)
  (condition-case err                   ; scroll-upに失敗したらバッファ末尾へ
      (scroll-up)
    (end-of-buffer                      ; エラーの種類
     (goto-char (point-max))))
  )
(defun my-scroll-down()
  "my scroll down"
  (interactive)
  (condition-case err                   ; scroll-downに失敗したらバッファ先頭へ
      (scroll-down)
    (beginning-of-buffer                ; エラーの種類
     (goto-char (point-min))))
  )

;===============================================================================
;; 入力文字で80桁までfill
;===============================================================================
(defun my-draw-line ()
  "Fill characters until column less than 80."
  (interactive)
  (setq fill-string (read-from-minibuffer "fill character : "))
  (if (> (length fill-string) 0)
      (progn
        (setq col (current-column))     ; 現在のカラム位置を取得
        (setq eol (- (progn (end-of-line) (current-column)) col)) ; 行末を取得
        (move-to-column col)            ; 元の位置に戻す
        (while (< (current-column) (- 80 eol))
          (insert fill-string))         ; 行末が80桁になるまでfill
        )))

;===============================================================================
;; 折り返しのトグル
;===============================================================================
(defun my-toggle-truncate()
  "toggle truncate"
  (interactive)
  (progn
    (if truncate-lines
        (progn
          (setq truncate-lines nil)
          (setq truncate-partial-width-windows nil))
      (progn
        (setq truncate-lines t)
        (setq truncate-partial-width-windows t)))
    (message "truncate-mode : %s" truncate-lines)))

;===============================================================================
;; フレームの位置を設定
;;      pos_x, pos_y  : pixel
;;      width, height : character
;===============================================================================
(defun my-adjust-frame(pos_x pos_y width height)
  "adjusst frame position."
  (interactive)
  (progn
    (set-frame-position (selected-frame) pos_x pos_y)
    (set-frame-width    (selected-frame) width)
    (set-frame-height   (selected-frame) height)))

;; 画面左半分
(global-set-key [f11]     '(lambda() (interactive) (my-adjust-frame    0  0 (/ my-screen-columns 2) my-screen-rows)))
;; 画面右半分
(global-set-key [f12]     '(lambda() (interactive) (my-adjust-frame (/ my-screen-width 2)  0 (/ my-screen-columns 2) my-screen-rows)))
;; 全画面
(global-set-key [C-f12]   '(lambda() (interactive) (my-adjust-frame    0  0 my-screen-columns my-screen-rows)))
