;; -*- coding:utf-8 -*-
;; from http://curiosity-drives.me/programming/rails/rails-emacs/
;; set load path
;; (setq load-path (cons “~/.emacs.d/” load-path))
;; (setq load-path (cons (expand-file-name "~/lisp/") load-path))


;; set language Japanese
(set-language-environment 'Japanese)
;; UTF-8
;; (prefer-coding-system 'utf-8)
(prefer-coding-system 'shift_jis)

;; set window status
(if window-system
    (progn
      ;; (frame-width)
      ;; (frame-height)
      ;; (setq initial-frame-alist '((width . 235)(height . 64)(top . 0)(left . 1921)))
      (setq initial-frame-alist '((width . 235)(height . 62)(top . 0)(left . 0)))
      ;; (set-background-color "Black")
      (set-background-color "#0f000f")
      (set-foreground-color "White")
      (set-cursor-color "Gray")

      (set-face-font 'default "-outline-Source Code Pro-normal-normal-normal-mono-14-*-*-*-c-*-iso8859-1")
      (global-hl-line-mode)
      ))

;; make window transparent
(set-frame-parameter nil 'alpha 80)
(defun set-alpha (per)
  (interactive "sAlpha: ")
  (set-frame-parameter nil 'alpha (car (read-from-string per)))
)

;; show line and column number
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(line-number-mode t)
 '(safe-local-variable-values (quote ((encoding . utf-8)))))

;; show filename in title bar
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))

;; Stop Auto Backup
(setq make-backup-files nil)
(setq auto-save-default nil)

;; newline and indent
(global-set-key "\C-m" 'newline-and-indent)

;;その他もろもろ

;; Base settings
(setq read-file-name-completion-ignore-case t) ;; 補完で大文字小文字無視
(global-font-lock-mode t) ;;文字の色つけ
(display-time) ;;時計を表示
(auto-compression-mode t) ;;日本語infoの文字化け防止
(setq inhibit-startup-message t) ;;起動時のメッセージは消す
(setq-default tab-width 4 indent-tabs-mode nil);;tabは4文字分、改行後に自動インデント
(setq visible-bell t) ;; 警告音を消す
(show-paren-mode 1) ;; 対応する括弧を光らせる。
;; (global-hl-line-mode) ;; 編集行のハイライト
(setq require-final-newline t) ;; ファイル末の改行がなければ追加
;(menu-bar-mode -1) ;;メニューバーを消す
(tool-bar-mode 0) ;;ツールバーを表示しない
(setq truncate-partial-width-windows nil) ;; ウインドウ分割時に画面外へ出る文章を折り返す

;;その他拡張
;;全角スペース、タブ、改行を可視化する

;
;====================================
;;jaspace.el を使った全角空白、タブ、改行表示モード
;;切り替えは M-x jaspace-mode-on or -off
;====================================
;;(require 'jaspace)
;; 全角空白を表示させる。
(setq jaspace-alternate-jaspace-string "□")
;; 改行記号を表示させる。
(setq jaspace-alternate-eol-string "↓\n")
;; タブ記号を表示。
;;(setq jaspace-highlight-tabs t) ; highlight tabs

;; EXPERIMENTAL: On Emacs 21.3.50.1 (as of June 2004) or 22.0.5.1, a tab
;; character may also be shown as the alternate character if
;; font-lock-mode is enabled.
;; タブ記号を表示。
(setq jaspace-highlight-tabs ?^ ) ; use ^ as a tab marker


;; from http://kagawacss.wiki.fc2.com/wiki/Emacs

;; bison-modeとflex-mode
;; bison
(autoload 'bison-mode "bison-mode" "bison" t)
(setq auto-mode-alist
      (append '(("\\.y$" . bison-mode))
              auto-mode-alist))
;; flex
(autoload 'flex-mode "flex-mode" "flex" t)
(setq auto-mode-alist
      (append '(("\\.l$" . flex-mode))
              auto-mode-alist))

;; クリップボード
(setq x-select-enable-clipboard t)
;; これを.emacsに追加しておくと他のソフト(Firefoxとか)でコピーした文字列をyankできるようになる。

;; インデント
;; C/C++のソースでタブ幅4にする。微妙におかしな設定かもしれないけど、意図した通りに動いてるのでよしとする。

; C
(add-hook 'c-mode-hook
          '(lambda()
             (c-set-style "linux")
             (setq c-basic-offset tab-width)
             ))

; C++
(add-hook 'c++-mode-hook
          '(lambda()
             (c-set-style "linux")
             (c-set-offset 'innamespace 0)
             (setq c-basic-offset tab-width)
             ))

; タブ幅
(setq-default tab-width 4)
; タブ幅の倍数
(setq tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))

;; for CUDA Program
(setq auto-mode-alist (cons (cons "\\.cu$" 'c++-mode) auto-mode-alist))

(setq load-path (cons (expand-file-name "~/.emacs.d/") load-path))

;;backtab
(defun backtab()
  "Do reverse indentation"
  (interactive)
  (back-to-indentation)
  (delete-backward-char
   (if (< (current-column) (car tab-stop-list)) 0
     (- (current-column)
        (car (let ((value (list 0)))
               (dolist (element tab-stop-list value) 
                 (setq value (if (< element (current-column)) (cons element value) value)))))))))

(global-set-key [backtab] 'backtab)

;;auto
;;(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")
(require 'auto-complete-config)
(ac-config-default)
(ac-set-trigger-key "TAB")
(setq ac-auto-start nil)

; C-tab でウィンドウを変更
(define-key global-map[C-tab] 'other-window)
(define-key global-map [(C x) (C x)] 'other-window)
(define-key global-map [S-C-tab] (lambda () (interactive) (other-window -1)))

;;; 現在の関数名をモードラインに表示
(which-function-mode 1)

(define-key global-map [(C x) (C tab)] 'buffer-menu)
(define-key global-map [(C x) (o)] 'buffer-menu)

;;CAL
(setq cal-dir "~/.emacs.d/CAL/11/")
(autoload 'cal (concat cal-dir "cal-mode/cal.elc") nil t)


(require 'matlab)
(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))

;; auto-complete-modeの自動起動
(add-to-list 'ac-modes 'matlab-mode)

;; (define-key global-map [(C x) (C c)]
;; (lambda (commandp
;; (end-of-defun)
;; (comint-delchar-or-maybe-eof)
;; (save-buffers-kill-terminal))))

;; white space
(require 'whitespace)
(global-whitespace-mode 1)
(setq whitespace-style
      '(face tabs tab-mark spaces space-mark trailing))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '((space-mark ?\x3000 [?\□])
        (tab-mark   ?\t   [?\xBB ?\t])
        (newline-mark ?\n     [?\u21B5 ?\n] [?$ ?\n])
        ))
(set-face-attribute 'whitespace-trailing nil
                    :background nil
                    ;; :foreground "LightSkyBlue"
                    :foreground "DarkSlateGray"
                    :underline t)
(set-face-attribute 'whitespace-tab nil
                    :background nil
                    :foreground "DarkSlateGray"
                    :underline t)
(set-face-attribute 'whitespace-space nil
                    :background nil
                    :foreground "DarkSlateGray"
                    :underline t)

(require 'dired)
(require 'cssh)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
