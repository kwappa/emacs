;;; パッケージ管理システム
;; 2011-03-02
;; original from :
;; https://github.com/clear-code/emacs.d/blob/master/config/packages.el

(require 'cl)

(defvar package-base-dir "~/.emacs.d/packages")

(defun package-path-basename (path)
  (file-name-sans-extension (file-name-nondirectory path)))

(defun package-directory (files)
  (concat package-base-dir "/"
          (package-path-basename (car files))))

(defun package-run-shell-command (command)
  (message (format "running...: %s" command))
  (shell-command command))

(defun package-install-from-emacswiki (files)
  (make-directory (package-directory files) t)
  (package-run-shell-command
   (format "wget --directory-prefix %s %s"
           (package-directory files)
           (mapconcat (lambda (name)
                        (concat "http://www.emacswiki.org/emacs/download/"
                                name))
                      files
                      " "))))

(defun package-install-from-url (url)
  (let ((files (package-path-basename url))))
  (make-directory (package-directory files) t)
  (package-run-shell-command
   (format "wget --directory-prefix %s %s"
           (package-directory files)
           url)))

(defun package-install-from-github (files)
  (package-run-shell-command
   (format (concat "git clone https://github.com/%s.git %s")
           (car files)
           (package-directory files))))

(defun package-install-from-repo.or.cz (files)
  (package-run-shell-command
   (format (concat "git clone git://repo.or.cz/%s.git %s")
           (car files)
           (package-directory files))))

(defun package-alist-value (alist key default-value)
  (if (listp alist)
      (let ((alist-item (assoc key alist)))
        (if alist-item
            (cdr alist-item)
          default-value))
    default-value))

(defun package-install (type package-spec require-name &optional force)
  (let ((files (package-alist-value package-spec 'files
                                    (if (listp package-spec)
                                        package-spec
                                      (list package-spec))))
        (base-path (package-alist-value package-spec 'base-path "."))
        (additional-paths (package-alist-value package-spec 'additional-paths
                                               nil))
        (install-proc (case type
                        (emacswiki
                         'package-install-from-emacswiki)
                        (github
                         'package-install-from-github)
                        (repo.or.cz
                         'package-install-from-repo.or.cz)
                        (url
                         'package-install-from-url)
                        (t
                         (error "unknown package type: <%s>(%s)"
                                type package)))))
    (add-to-list 'load-path
                 (format "%s/%s"
                         (package-directory files)
                         base-path))
    (dolist (additional-path additional-paths)
      (add-to-list 'load-path (format "%s/%s"
                                      (package-directory files)
                                      additional-path)))
    (condition-case err
        (require require-name)
      (error
       (message (format "installing %s..." files))
       (funcall install-proc files)))
    (require require-name)))

;===============================================================================
;; packages
;===============================================================================
;; anything.el
;; <f5> a というバインドがかぶるので変更
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(anything-command-map-prefix-key "\C-c\C-f"))

(package-install 'repo.or.cz '((files . ("anything-config"))
                               (additional-paths . ("extensions")))
                 'anything-startup)
(global-set-key [?\C-\:] 'anything)

;; auto-complete
(package-install 'github "m2ym/auto-complete" 'auto-complete-config)
(add-to-list 'ac-dictionary-directories
             (format "%s/auto-complete/dict" package-base-dir))
(ac-config-default)
(add-hook 'auto-complete-mode-hook
          (lambda ()
            (define-key ac-completing-map (kbd "C-n") 'ac-next)
            (define-key ac-completing-map (kbd "C-p") 'ac-previous)))

;; redo
(package-install 'emacswiki "redo+.el" 'redo+)
(global-set-key (kbd "C-M-/") 'redo)
(setq undo-no-redo t)

;; haml-mode
(package-install 'github "nex3/haml-mode" 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

;; yaml-mode
(package-install 'github "yoshiki/yaml-mode" 'yaml-mode)

;; browse-kill-ring
(package-install 'emacswiki "browse-kill-ring.el" 'browse-kill-ring)
(global-set-key (kbd "C-M-y") 'browse-kill-ring)

;; simplenote
(package-install 'github "cefstat/simplenote.el" 'simplenote)
; https://gist.github.com/458545
(add-hook 'before-save-hook
          (lambda()
            (when (equal (buffer-name) "*Simplenote*")
              (simplenote-sync-notes)
              (simplenote-browser-refresh))
            ))
