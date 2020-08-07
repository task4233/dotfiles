(require 'package)
(setq package-archives
  '(("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
    t
    (if (or (assoc package package-archive-contents) no-refresh)
      (if (boundp 'package-selected-packages)
        ;; Record this as a package the user installed explicitly
        (package-install package nil)
        (package-install package))
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.
In the event of failure, return nil and print a warning message.
Optionally require MIN-VERSION.  If NO-REFRESH is non-nil, the
available package lists will not be re-downloaded in order to
locate PACKAGE."
  (condition-case err
    (require-package package min-version no-refresh)
    (error
      (message "Couldn't install optional package `%s': %S" package err)
      nil)))

;; show line-num
(global-linum-mode)
(setq linum-format "%4d")
(setq linum-delay t)
(defadvice linum-schedule (around my-linum-schedule () activate)
  (run-with-idle-timer 0.2 nil #'linum-update-current))

;; set tab with
(setq default-tab-width 2)

;; hl-linemode
(defface my-hl-line-face
  ;; 背景がdarkなら背景色を紺に
  '((((class color) (background dark))
     (:background "NavyBlue" t))
    ;; 背景がlightならば背景色を青に
    ((( class color) (background light))
     (:background "LightSkyBlue" t))
    (t (:bold t)))
  "hl-line's my face")
(setq hl-line-face 'my-hl-line-face)
(global-hl-line-mode t)

;; コメントアウト
(set-face-foreground 'font-lock-comment-face "red")

;; color theme
(load-theme 'atom-one-dark t)

;; None startup-message
(setq inhibit-startup-message 1)

;; None menu-bar
(if (eq window-system 'x)
    (menu-bar-mode 1) (menu-bar-mode 0))
(menu-bar-mode nil)

;; None scratch-message
(setq initial-scratch-message "")

;; Back-files-config(All backup files to ~/.emacs.d/backups/)
(add-to-list 'backup-directory-alist
         (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))
(setq auto-save-timeout 15)
(setq auto-save-interval 60)

;;
;; Key-Config
;;

;; バックスペース
(global-set-key (kbd "C-h") 'delete-backward-char)

;; ディスプレイ切り替え
(define-key global-map (kbd "C-t") 'other-window)

;;; C-n / C-p で選択
(setq ac-use-menu-map t)

;;; yasnippetのbindingを指定するとエラーが出るので回避する方法。
(setf (symbol-function 'yas-active-keys)
      (lambda ()
        (remove-duplicates (mapcan #'yas--table-all-keys (yas--get-snippet-tables)))))

;;
;; undo-tree
;;
(require-package 'undo-tree)
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)

;; rainbow-delimiters を使うための設定
(require-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;
;; flycheck(vue, c++)
;;
(require-package 'flycheck)
(require 'flycheck)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(eval-after-load 'vue-mode
  '(add-hook 'vue-mode-hook #'add-node-modules-path))
(flycheck-add-mode 'javascript-eslint 'vue-mode)
(flycheck-add-mode 'javascript-eslint 'vue-html-mode)
(flycheck-add-mode 'javascript-eslint 'css-mode)
(add-hook 'vue-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook 'company-mode) ; 補完用
(add-hook 'c++-mode-hook 'flycheck-mode) ; チェック用
(add-hook 'c++-mode-hook #'lsp)

;;
;; go mode & auto-complete
;;
(add-to-list 'exec-path (expand-file-name "/usr/local/go/bin"))
(add-to-list 'exec-path (expand-file-name "/root/go/bin"))
(add-to-list 'load-path "~/.emacs.d/lisp")

(require-package 'auto-complete)
(require-package 'go-autocomplete)
(require 'auto-complete)
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)
(ac-set-trigger-key "TAB")
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'vue-mode)
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook (lambda()
			  (add-hook 'before-save-hook' 'gofmt-before-save)
			  (local-set-key (kbd "M-.") 'godef-jump)
			  (set (make-local-variable 'company-backends) '(company-go))
			  (setq indent-tabs-mode nil) ; using tab
			  (setq c-basic-offset 4)     ; set indent 4
			  (setq tab-width 4)))

;;
;; company-go
;;
(require-package 'company-go)
(add-hook 'go-mode-hook (lambda()
			  (company-mode)
			  (setq company-transformers '(company-sort-by-backend-importance)) ; sorted
			  (setq company-idle-delay 0)                                       ; no delay
			  (setq company-minimum-prefix-length 3)
			  (setq company-selection-wrap-around t)                            ; loop choice
			  (setq completion-ignore-case t)
			  (setq company-dabbrev-downcase nil)
			  (global-set-key (kbd "C-M-i") 'company-complete)
			  ;; key-config
			  (define-key company-active-map (kbd "C-n") 'company-select-next)
			  (define-key company-active-map (kbd "C-p") 'company-select-previous)
			  (define-key company-active-map (kbd "C-s") 'company-filter-candidates)
			  (define-key company-active-map [tab] 'company-complete-selection)
			  ))


(setq gofmt-command "goimports")
(add-hook 'before-save-hook 'gofmt-before-save)


;; 括弧の色を強調する設定
(require-package 'cl-lib)
(require-package 'color)
(require 'cl-lib)
(require 'color)
(defun rainbow-delimiters-using-stronger-colors ()
  (interactive)
  (cl-loop
   for index from 1 to rainbow-delimiters-max-face-count
   do
   (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
    (cl-callf color-saturate-name (face-foreground face) 30))))
(add-hook 'emacs-startup-hook 'rainbow-delimiters-using-stronger-colors)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (clang-format company-lsp lsp-ui markdown-mode lsp-mode go-complete yaml-mode add-node-modules-path vue-mode js2-mode neotree web-mode yatex company-go flycheck go-mode undo-tree rainbow-delimiters auto-complete atom-one-dark-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
