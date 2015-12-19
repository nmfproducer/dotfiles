
(defun cal ()
  (interactive)
  (if (get-buffer "*cal*")
      (progn
	(switch-to-buffer "*cal*")
	(delete-other-windows))
    ;; we first call cal-news to check if the news file is downloadable
    (if (or (not cal-interactive) (cal-get-news))
	(progn
	  (cal-mode)
	  (switch-to-buffer "*cal*")
	  (delete-other-windows))
      ;; report error (without actually calling error)
      (message "ファイルのダウンロードに失敗しました! 教官かTAに連絡してください．")
      (ding))))

(defun cal-mode ()
  "Major mode: Computation and Logic."
  (setq cal-exit-flag nil
	;; the value of the variables below will be set to the latest value
	;; by loading cal-record-file
	cal-uer-id nil
	cal-solved-list nil
	*csl* nil
	cal-session-count 0
	cal-news-list nil
	cal-def-list nil
	cal-thm-list nil
	cal-new-thm-list nil
	cal-thm-name-list nil
	cal-old-thm-name-list nil
	cal-current-game nil
	cal-demo-on nil
	)
  (if (file-exists-p cal-record-file)
      (if (cal-correct-file cal-record-file)
	  (progn
	    (load cal-record-file nil t t)
	    ;; set *csl*
	    (setq *csl* cal-solved-list)
	    (if *csl*
		;; Q[1] is solved, so we upload record file. 
		(unless (cal-upload cal-record-file cal-upload-url)
		  (message "ファイルのアップロードに失敗しました! 教官かTAに連絡してください．")
		  (sit-for 3)
		  (error ""))
	      ;; Q[1] is not solved, so do nothing
	      ))
	;; cal-record-file is broken!
	(cal-upload cal-record-file cal-upload-url 
		    (concat "Broken-cal-record-"
			    (replace-regexp-in-string " " "-" 
						      (current-time-string))))
	(message "CAL の記録ファイルが正しくありません．教官かTAに連絡してください.")
	(sit-for 3)
	(message "")
	(error ""))
    (error "CAL の記録ファイルがありません. 教官かTAに連絡してください."))
  (setq cal-session-count (1+ cal-session-count))
  (set-buffer (get-buffer-create "*cal*"))
  (if (= (buffer-size) 0)
      (let ((current-time (current-time-string)))
	(save-excursion
	  (set-buffer (get-buffer-create " *CALLOG*"))
	  (erase-buffer)
	  (insert "CAL session started\nat: " current-time
		  "\non: " (system-name)
		  "\nby: " (user-login-name)
		  " (" (user-full-name) ")\n"))
	(cal-insert
	 "\t\t***********************************************\n\n"
	 "\t\t        ようこそ「計算と論理」の世界へ!   \n\n"
	 "\t\t      著作者:  佐藤 雅彦,  五十嵐 淳\n\n"
	 "\t\t    京都大学大学院 情報学研究科 知能情報学専攻\n"
	 "\t\t************************************************\n\n"
	 (cal-mode-version) "\n"
	 "質問は cal@sato.kuis.kyoto-u.ac.jp でも受け付けます.\n\n"
	 )))
  (if (= cal-session-count 1)
      (progn
	(cal-insert "今回は最初の CAL のセッションです.\n"
		    "カーソルがプロンプト【CAL <1> 】の直後にあることを確認して,\n"
		    "『news[1]』とタイプしてください."))
    ;; check new news
    (setq cal-no-of-news (cal-news nil))
    (unless (= cal-no-of-news (length cal-news-list))
      (cal-insert "未読のニュースがあります.\n"
		  "『news[]』で未読のニュースを全部読むことができます."))
    ;; check if cal-user-id is set
    (unless cal-user-id
      (cal-insert "成績評価に必要なデータを記録するために, 必ず Q[1]\n"
		  "を解答してください!")))
  (cal-insert)
  (setq cal-prompt-count 0)
  (cal-increment-prompt-count)
  (use-local-map cal-mode-map)
  (cal-set-syntax)
  (set-syntax-table cal-mode-syntax-table)
  (setq parse-sexp-ignore-comments t)
  (setq major-mode 'cal-mode)
  (setq mode-name "計算と論理")
  ;; may be added sometime later ...
  (add-hook 'after-change-functions 'cal-after-change-function nil t)
  ;; make the values of the following two variables large enough
  (setq max-lisp-eval-depth 5000)
  (setq max-specpdl-size 5000)
  ;; call cal-Q to set cal-no-of-problems
  (cal-Q)
  ;; define basic tokens
  (deftoken
    (comment ";;")
    (white " \t\n")
    (open  "(" "<" "{" "[" "《")
    (close ")" ">" "}" "]" "》")
    (var "[a-zA-Z][a-zA-Z0-9_-]*")
    (svar "\\^[a-zA-Z][a-zA-Z0-9_-]*")
    (op 
     "WILD_CARD_OP" ;; for internal use
     "DUMMY_CARD" ;; for internal use
     "WILD_CARD" ;; for internal use
     "since"
     ;;"triv"
     "conj"
     "nil" 
     "by" 
     "id" 
     "in" 
     "::"
     ":"
     "::="
     ":-"
     "├"
     "Π"  ;; for universal judgment
     "⇒"
     "#"
     "^"
     ","
     ";"
     "|"
     "&"
     "'"
     ))
  ;; load the basic file for defining Games
  (load (concat cal-game-dir "Def.el") nil t t)
  ;; we can now load .gm files, so we load the Basic definitions
  (cal-load-game "Basic")
  (cal-load-game "Common")
  (cal-load-game "TeX")
  ;; setup definitions
  (when cal-def-list
    (message "定義を読み込んでいます...") 
    (let ((cdl cal-def-list))
      (while cdl
	(let* ((def (first cdl))
	       (full-name (first def))
	       (vars (second def))
	       (body (third def))
	       (g-m (cal-split-str full-name ":"))
	       (game (first g-m))
	       (pred (second g-m))) 
	  (put (intern full-name) 'cal-def (list vars body))
	  (cal-update-list 'cal-game-ops game pred)
	  (setq cdl (cdr cdl))))) 
    (message ""))
  ;; setup theorems
  (when cal-thm-list 
    (let ((list cal-thm-list)) 
      (message "定理を読み込んでいます...")
      (while list
	(let* ((item (first list))
	       (game (first item))
	       (thm-list (second item)))
	  (put (intern game) 'thm-list thm-list)
	  (while thm-list
	    (let* ((item (first thm-list))
		   (name (first item)))
	      (cal-update-list 'cal-game-ops game name)
	      (setq thm-list (cdr thm-list))))
	  (setq list (cdr list))))))
  ;; call cal-play non-interactively
  (when cal-current-game (cal-play cal-current-game t))
  (run-hooks 'cal-mode-hook)
  (message "")
  )

