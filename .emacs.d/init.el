(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")))

;; show line-num
(global-linum-mode)
(setq linum-format "%4d")

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
(require 'undo-tree)
(global-undo-tree-mode t)
(global-set-key (kbd "M-/") 'undo-tree-redo)

;; rainbow-delimiters を使うための設定
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;;
;; vue mode
;;
(require 'flycheck)
(add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
(eval-after-load 'vue-mode
  '(add-hook 'vue-mode-hook #'add-node-modules-path))
(flycheck-add-mode 'javascript-eslint 'vue-mode)
(flycheck-add-mode 'javascript-eslint 'vue-html-mode)
(flycheck-add-mode 'javascript-eslint 'css-mode)
(add-hook 'vue-mode-hook 'flycheck-mode)

;;
;; go mode & auto-complete
;;
(add-to-list 'exec-path (expand-file-name "/usr/local/go/bin"))
(add-to-list 'exec-path (expand-file-name "/root/go/bin"))
(add-to-list 'load-path "/root/.emacs.d/lisp")

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
(require 'company-go)
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

;;
;; YaTeX
;;
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
                ("\\.ltx$" . yatex-mode)
                ("\\.cls$" . yatex-mode)
                ("\\.sty$" . yatex-mode)
                ("\\.clo$" . yatex-mode)
                ("\\.bbl$" . yatex-mode)) auto-mode-alist))
(setq YaTeX-inhibit-prefix-letter t)
(setq YaTeX-kanji-code nil)
(setq YaTeX-latex-message-code 'utf-8)
(setq YaTeX-use-LaTeX2e t)
(setq YaTeX-use-AMS-LaTeX t)
(setq YaTeX-dvi2-command-ext-alist
      '(("TeXworks\\|texworks\\|texstudio\\|mupdf\\|SumatraPDF\\|Preview\\|Skim\\|TeXShop\\|evince\\|atril\\|xreader\\|okular\\|zathura\\|qpdfview\\|Firefox\\|firefox\\|chrome\\|chromium\\|MicrosoftEdge\\|microsoft-edge\\|Adobe\\|Acrobat\\|AcroRd32\\|acroread\\|pdfopen\\|xdg-open\\|open\\|start" . ".pdf")))
(setq tex-command "ptex2pdf -u -l -ot '-synctex=1'")
;(setq tex-command "lualatex -synctex=1")
;(setq tex-command "latexmk")
;(setq tex-command "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
;(setq tex-command "latexmk -e '$lualatex=q/lualatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -norc -gg -pdflua")
(setq bibtex-command "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
(setq makeindex-command  "latexmk -e '$latex=q/uplatex %O -synctex=1 %S/' -e '$bibtex=q/upbibtex %O %B/' -e '$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/' -e '$makeindex=q/upmendex %O -o %D %S/' -e '$dvipdf=q/dvipdfmx %O -o %D %S/' -norc -gg -pdfdvi")
;(setq dvi2-command "xdg-open")
(setq dvi2-command "evince")
;(setq dvi2-command "atril")
;(setq dvi2-command "okular --unique")
;(setq dvi2-command "zathura -x \"emacsclient --no-wait +%{line} %{input}\"")
;(setq dvi2-command "qpdfview --unique")
;(setq dvi2-command "texworks")
;(setq dvi2-command "texstudio --pdf-viewer-only")
;(setq tex-pdfview-command "xdg-open")
(setq tex-pdfview-command "evince")
;(setq tex-pdfview-command "atril")
;(setq tex-pdfview-command "okular --unique")
;(setq tex-pdfview-command "zathura -x \"emacsclient --no-wait +%{line} %{input}\"")
;(setq tex-pdfview-command "qpdfview --unique")
;(setq tex-pdfview-command "texworks")
;(setq tex-pdfview-command "texstudio --pdf-viewer-only")
(setq dviprint-command-format "wine cmd /c start AcroRd32.exe `echo %s | sed -e \"s/\\.[^.]*$/\\.pdf/\"`")
(require 'dbus)
(defun un-urlify (fname-or-url)
  "A trivial function that replaces a prefix of file:/// with just /."
  (if (string= (substring fname-or-url 0 8) "file:///")
      (substring fname-or-url 7)
    fname-or-url))
(defun evince-inverse-search (file linecol &rest ignored)
  (let* ((fname (decode-coding-string (url-unhex-string (un-urlify file)) 'utf-8))
         (buf (find-file fname))
         (line (car linecol))
         (col (cadr linecol)))
    (if (null buf)
        (message "[Synctex]: %s is not opened..." fname)
      (switch-to-buffer buf)
      (goto-line (car linecol))
      (unless (= col -1)
        (move-to-column col))
      (x-focus-frame (selected-frame)))))
(dbus-register-signal
 :session nil "/org/gnome/evince/Window/0"
 "org.gnome.evince.Window" "SyncSource"
 'evince-inverse-search)
(with-eval-after-load 'yatexprc
  (defun YaTeX-preview-jump-line ()
    "Call jump-line function of various previewer on current main file"
    (interactive)
    (save-excursion
      (save-restriction
        (widen)
        (let*((pf (or YaTeX-parent-file
                      (save-excursion (YaTeX-visit-main t) (buffer-file-name))))
              (pdir (file-name-directory pf))
              (bnr (substring pf 0 (string-match "\\....$" pf)))
              ;(cf (file-relative-name (buffer-file-name) pdir))
              (cf (buffer-file-name)) ;2016-01-08
              (buffer (get-buffer-create " *preview-jump-line*"))
              (line (count-lines (point-min) (point-end-of-line)))
              (previewer (YaTeX-preview-default-previewer))
              (cmd (cond
                    ((string-match "Skim" previewer)
                     (format "%s %d '%s.pdf' '%s'"
                             YaTeX-cmd-displayline line bnr cf))
                    ((string-match "evince" previewer)
                     (format "%s '%s.pdf' %d '%s'"
                             "fwdevince" bnr line cf))
                    ((string-match "sumatra" previewer)
                     (format "%s \"%s.pdf\" -forward-search \"%s\" %d"
                             previewer bnr cf line))
                    ((string-match "zathura" previewer)
                     (format "%s --synctex-forward '%d:0:%s' '%s.pdf'"
                             previewer line cf bnr))
                    ((string-match "qpdfview" previewer)
                     (format "%s '%s.pdf#src:%s:%d:0'"
                             previewer bnr cf line))
                    ((string-match "okular" previewer)
                     (format "%s '%s.pdf#src:%d %s'"
                             previewer bnr line (expand-file-name cf)))
                    )))
          (YaTeX-system cmd "jump-line" 'noask pdir))))))
(add-hook 'yatex-mode-hook
          '(lambda ()
             (auto-fill-mode -1)))
;;
;; RefTeX with YaTeX
;;
;(add-hook 'yatex-mode-hook 'turn-on-reftex)
(add-hook 'yatex-mode-hook
          '(lambda ()
             (reftex-mode 1)
             (define-key reftex-mode-map (concat YaTeX-prefix ">") 'YaTeX-comment-region)
             (define-key reftex-mode-map (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))


;; 括弧の色を強調する設定
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
    (yaml-mode add-node-modules-path vue-mode js2-mode neotree web-mode yatex company-go flycheck go-mode undo-tree rainbow-delimiters auto-complete atom-one-dark-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
