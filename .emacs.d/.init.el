;; from http://curiosity-drives.me/programming/rails/rails-emacs/
;; set load path
;; (setq load-path (cons “~/.emacs.d/” load-path))
;; (setq load-path (cons (expand-file-name "~/lisp/") load-path))


;; set language Japanese
(set-language-environment 'Japanese)
;; UTF-8
(prefer-coding-system 'utf-8)

;;Windowサイズと色を設定

;; set window status
(if window-system (progn
(setq initial-frame-alist '((width . 200)(height . 45)(top . 0)(left . 0)))
(set-background-color "Black")
(set-foreground-color "White")
(set-cursor-color "Gray")
))

;;Windowを透明にする

;; make window transparent
(set-frame-parameter nil 'alpha 80)

;;行番号と何文字目か情報を表示

;; show line and column number
(custom-set-variables '(line-number-mode t)
'(column-number-mode t))

;;タイトルバーにファイル名を表示

;; show filename in title bar
(setq frame-title-format (format "%%f - Emacs@%s" (system-name)))

;;自動的にバックアップファイルが作られるのをやめる(filename.txt~みたいなやつ)

;; Stop Auto Backup
(setq make-backup-files nil)
(setq auto-save-default nil)

;;改行時に自動でインデント

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
(global-hl-line-mode) ;; 編集行のハイライト
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
(setq tab-stop-list

    '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))

;; for CUDA Program
(setq auto-mode-alist (cons (cons "\\.cu$" 'c++-mode) auto-mode-alist))



(setq load-path (cons (expand-file-name "~/.emacs.d/") load-path))


