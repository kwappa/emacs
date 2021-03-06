;===============================================================================
;; site-lisp
;===============================================================================
;; physica-line
;; http://www.scythe.jp/lab/physical-line.html
(require 'physical-line)
(physical-line-mode t)
(global-set-key (kbd "C-e") 'end-of-visual-line)
(global-set-key (kbd "C-S-e") 'end-of-line)

;; markdown-mode
;; http://jblevins.org/projects/markdown-mode/
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(setq markdown-command "perl /usr/local/bin/markdown")

;; rst-mode
;; Emacs起動時にrst.elを読み込み
(require 'rst)
;; 拡張子の*.rst, *.restのファイルをrst-modeで開く
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))
;; 背景が黒い場合はこうしないと見出しが見づらい
(setq frame-background-mode 'dark)
;; 全部スペースでインデントしましょう
(add-hook 'rst-mode-hook '(lambda() (setq indent-tabs-mode nil)))

;; htmlize
(require 'htmlize)

;; lua-mode
(setq auto-mode-alist (cons '("\\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;; htmlize
(require 'mediawiki)

;; php-mode
(autoload 'php-mode "php-mode" "Major mode for editing php code." t)
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))

;; js-mode : apply to .json
(require 'js2-mode)
(eval-when-compile
  (require 'espresso))

(eval-after-load 'js2-mode
  '(progn
     (require 'espresso)))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; http://d.hatena.ne.jp/gengar/20110522/1306029785
(defun my-js-indent-line ()
  (interactive)
  (let* ((parse-status (save-excursion (syntax-ppss (point-at-bol))))
         (offset (- (current-column) (current-indentation)))
         (indentation (espresso--proper-indentation parse-status)))
    (back-to-indentation)

    ;; switch の中は余分に一つインデント
    (cond ((let ((pos (nth 1 parse-status)))
             (and pos
                  (not (looking-at "}"))
                  (save-excursion
                    (goto-char pos)
                    (back-to-indentation)
                    (looking-at "switch\\W"))))
           (indent-line-to (+ indentation espresso-indent-level)))
          ;; consecutive declarations in a var statement are nice if
          ;; properly aligned, i.e:
          ;;
          ;; var foo = "bar",
          ;;     bar = "foo";
          ((let ((node (js2-node-at-point)))
             (and node
                  (= js2-NAME (js2-node-type node))
                  (= js2-VAR (js2-node-type (js2-node-parent node)))))
           (indent-line-to (+ 4 indentation)))
          (t
           (espresso-indent-line)))
    (when (> offset 0) (forward-char offset))))
(add-hook 'js2-mode-hook
          '(lambda ()
             (setq espresso-indent-level 4
                   espresso-expr-indent-offset 4)
             (set (make-local-variable 'indent-line-function) 'my-js-indent-line)))

;; ruby-mode : apply to Rakefile, .ru
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile"  . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$"   . ruby-mode))

;; rhtml-mode : erb
;;; rhtml mode
(add-to-list 'load-path "~/.emacs.d/site-lisp/rhtml")
(require 'rhtml-mode)
(add-to-list 'auto-mode-alist '("\\.erb$"   . rhtml-mode))

;; slim-mode
(autoload 'slim-mode "slim-mode" "Major mode for editing slim template." t)
(add-to-list 'auto-mode-alist '("\\.slim$" . slim-mode))

;; jaspace.el
;; http://openlab.dino.co.jp/2008/08/29/230500336.html
;; タブ, 全角スペース、改行直前の半角スペースを表示する
(when (require 'jaspace nil t)
  (when (boundp 'jaspace-modes)
    (setq jaspace-modes (append jaspace-modes
                                (list 'php-mode
                                      'yaml-mode
                                      'javascript-mode
                                      'js-mode
                                      'js2-mode
                                      'ruby-mode
                                      'text-mode
                                      'fundamental-mode))))
  (when (boundp 'jaspace-alternate-jaspace-string)
    (setq jaspace-alternate-jaspace-string "□"))
  (when (boundp 'jaspace-highlight-tabs)
    (setq jaspace-highlight-tabs ?^))
  (add-hook 'jaspace-mode-off-hook
            (lambda()
              (when (boundp 'show-trailing-whitespace)
                (setq show-trailing-whitespace nil))))
  (add-hook 'jaspace-mode-hook
            (lambda()
              (progn
                (when (boundp 'show-trailing-whitespace)
                  (setq show-trailing-whitespace t))
                (face-spec-set 'jaspace-highlight-jaspace-face
                               '((((class color) (background light))
                                  (:foreground "blue"))
                                 (t (:foreground "green"))))
                (face-spec-set 'jaspace-highlight-tab-face
                               '((((class color) (background light))
                                  (:foreground "red"
                                               :background "unspecified"
                                               :strike-through nil
                                               :underline t))
                                 (t (:foreground "red"
                                                 :background "unspecified"
                                                 :strike-through nil
                                                 :underline t))))
                (face-spec-set 'trailing-whitespace
                               '((((class color) (background light))
                                  (:foreground "red"
                                               :background "unspecified"
                                               :strike-through nil
                                               :underline t))
                                 (t (:foreground "red"
                                                 :background "unspecified"
                                                 :strike-through nil
                                                 :underline t))))))))
