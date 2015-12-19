(setq cal-version "7.04")
(setq cal-version-string
      (concat "CAL version " cal-version " of Wed Oct 22 12:07:06 2008"))

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
	)
  (if (file-exists-p cal-record-file)
      (if (cal-correct-file cal-record-file)
	  (progn
	    (load cal-record-file nil t t)
	    ;; set *csl*
	    (setq *csl* cal-solved-list)
	    (if *csl*
		(progn
		  ;; Q[1] is solved, so we upload record file. 
		  ;; But, first check cookie and re-initialize cookie
		  ;; information if necessary
		  (unless (cal-check-cookie)
		    ;; cookie is expired, so re-initialize cookie.
		    ;; this works only for local users.
		    (cal-initial-setup "9999"))
		  (unless (cal-upload cal-record-file cal-upload-url)
		    (message "ファイルのアップロードに失敗しました! 教員かTAに連絡してください．")
		    (sit-for 3)
		    (error "")))
	      ))
	;; cal-record-file is broken!
	(cal-upload cal-record-file cal-upload-url 
		    (concat "Broken-cal-record-"
			    (replace-regexp-in-string " " "-" 
						      (current-time-string))))
	(message "CAL の記録ファイルが正しくありません．教員かTAに連絡してください.")
	(sit-for 3)
	(message "")
	(error ""))
    (error "CAL の記録ファイルがありません. 教員かTAに連絡してください."))
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
	 (format "\t\t        ようこそ「計算と論理」の世界へ!   \n\n")
	 (format "\t\t      著作者:  佐藤 雅彦,  五十嵐 淳\n\n")
	 (format "\t\t    京都大学大学院 情報学研究科 知能情報学専攻\n\n")
	 "\t\t************************************************\n\n"
	 (cal-mode-version) "\n"
	 (format "質問は cal@sato.kuis.kyoto-u.ac.jp でも受け付けます.\n\n")
	 )))
  (if (= cal-session-count 1)
      (progn
	(cal-insert 
	 (format "今回は最初の CAL のセッションです.\n")
	 (format "カーソルがプロンプト【CAL <1> 】の直後にあることを確認して,\n")
	 (format "『news[1]』とタイプしてください.")))
    ;; check new news
    (setq cal-no-of-news (cal-news nil))
    (unless (= cal-no-of-news (length cal-news-list))
      (cal-insert 
       (format "未読のニュースがあります.\n")
       (format "『news[]』で未読のニュースを全部読むことができます.")))
    ;; check if cal-user-id is set
    (unless cal-user-id
      (cal-insert 
       (format "成績評価に必要なデータを記録するために, 必ず Q[1]\n")
       (format "を解答してください!"))))
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
  (setq cal-game-dirs
	(if cal-private-game-dir
	    (list cal-private-game-dir cal-game-dir)
	  (list cal-game-dir)))
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

(defun cal-exit ()
  "Exit from cal-mode."
  (let (;; inhibit quit!
	(inhibit-quit t)
	(time (current-time-string)))
    (when (or *csl* (not cal-use-w3m))
      ;; we save the log buffer and upload record file etc.
      ;; only when *csl* is non-nil OR when not using w3m.
      (cal-save-and-upload-logfile t)
      (cal-save-and-upload-record))
    (if (get-buffer " *CALLOG*") (kill-buffer " *CALLOG*"))
    (if (get-buffer " *CALMAIL*") (kill-buffer " *CALMAIL*"))
    (if (get-buffer " *PROBLEMS*") (kill-buffer " *PROBLEMS*"))
    (if (get-buffer " *ANSWERS*") (kill-buffer " *ANSWERS*"))
    (if (get-buffer " *HELP*") (kill-buffer " *HELP*"))
    (if (get-buffer " *NEWS*") (kill-buffer " *NEWS*"))
    (if (get-buffer " *CALWORK*") (kill-buffer " *CALWORK*"))
    (if (get-buffer " *RECORD*") (kill-buffer " *RECORD*"))
    (if (get-buffer " *MYRECORD*") (kill-buffer " *MYRECORD*"))
    (end-of-line)
    (insert "\nBye,")
    (sit-for 1)
    (insert "\nBye!\n")
    (sit-for 0.5)
    (message "")
    (if (get-buffer " *CALMAIL*") (kill-buffer " *CALMAIL*"))
    (setq cal-exit-flag t
	  cal-session-count 0
	  cal-solved-list nil
	  cal-current-game nil
	  *csl* nil
	  cal-def-list nil
	  cal-thm-list nil
	  cal-game-ops nil)
    (set-buffer-modified-p nil)
    (kill-buffer (current-buffer))))

(defun cal-save-and-upload-record ()
  "Save and upload the latest cal-record-file."
  (cal-save-record)
  (sit-for 0.1)
  ;(message "Uploading cal-record...")
  (cal-upload cal-record-file cal-upload-url)
  ;(message "Uploading cal-record...done")
  )
  
(defun cal-save-record ()
  "Save cal-record-file."
  (let (;; inhibit quit!
	(inhibit-quit t)
	(time (current-time-string)))
    (save-excursion
      (with-temp-buffer
	;; now insert data for .cal
	(if cal-user-id
	    (insert
	     (format
	      "(setq cal-user-id \"%s\")\n"
	      cal-user-id)))
	(insert
	 (format
	  "(setq cal-session-count %s)\n"
	  cal-session-count))
	(insert
	 (format
	  "(setq cal-solved-list '%s)\n"
	  *csl*))
	(if cal-current-game
	    (insert
	     (format
	      "(setq cal-current-game \"%s\")\n"
	      cal-current-game)))
	(insert "(setq cal-def-list\n      '(\n")
	(let ((list cal-def-list))
	  (while list
	    (cal-print-tree (car list) 2)
	    (insert "\n")
	    (setq list (cdr list))))
	(insert "))\n")
	(insert "(setq cal-def-str-list\n      '(\n")
	(let ((list cal-def-str-list))
	  (while list
	    (cal-print-tree (car list) 2)
	    (insert "\n")
	    (setq list (cdr list))))
	(insert "))\n")
	(insert "(setq cal-thm-list\n      '(\n")
	(let ((list cal-thm-list))
	  (while list
	    (cal-print-tree (car list) 2)
	    (insert "\n")
	    (setq list (cdr list))))
	(insert "))\n")
	(insert "(setq cal-thm-str-list\n      '(\n")
	(let ((list cal-thm-str-list))
	  (while list
	    (cal-print-tree (car list) 2)
	    (insert "\n")
	    (setq list (cdr list))))
	(insert "))\n")
	(insert
	 (format
	  "(setq cal-news-list '%s)\n"
	  cal-news-list))
	(cal-encrypt-buffer)
	(write-region (point-min) (point-max) cal-record-file nil 0) ))))

(defun cal-A (str1 str2)
  "check if STR2 is a correct answer to the quetstion no STR1." 
  (let (p
	astr ;; answer string
	cstr ;; commentary string
	(correct nil) ;; will be set to t if answer is correct
	(noprob nil) ;; will be set to t if the problem not found
	val ;; resulting value of eval-region
	)
    (when (> (string-to-number str1) 1)
	;; check if Q[1] has been solved 
	(unless (or (member 1 *csl*) (not cal-use-w3m))
	    (error
	     "Q[1] に解答しないと他の問題は解答できません．")) )
    (save-excursion
      (set-buffer (get-buffer-create " *ANSWERS*"))
      (if (= (buffer-size) 0) 
	  (unless (cal-download cal-A-file "answers ends here")
	    (error "解答ファイルの取得に失敗しました．教員かTAに連絡してください．")))
      (goto-char (point-min))
      (if (search-forward (concat "-----\n[" str1 "]") nil t)
	  (progn
	    (forward-char 1)
	    (if (looking-at ";; ")
		(progn
		  ;; in this case, we compute the answer by
		  ;; evaluating the program written in this line.
		  (forward-char 3)
		  (setq p (point))
		  (end-of-line)
		  (eval-region p (point))
		  (setq correct val))
	      (setq p (point))
	      (end-of-line)
	      (setq correct
		    (string= str2 (buffer-substring p (point)))))
	    (or (looking-at "\n-----")
		;; we have commentary lines
		(progn
		  (forward-char 1)
		  (setq p (point))
		  (end-of-line)
		  (search-forward "\n-----")
		  (setq cstr
			(buffer-substring
			 p
			 (1+ (match-beginning 0)))))))
	(setq noprob t)))
    (if noprob
	(progn
	  (goto-char (cal-prompt-point))
	  (search-forward "\[" nil t)
	  (error "番号 %s の問題はありません!" str1))
      (if correct
	  (progn
	    ;; save logfile
	    (cal-save-and-upload-logfile)
	    ;; check if already solved the same problem
	    (or (member (string-to-number str1)
			*csl*)
		(progn
		  (setq *csl*
			(cons (string-to-number str1)
			      *csl*))
		  ))
	    ;; save and upload record
	    (cal-save-and-upload-record)
	    (if cstr (cal-insert-lines cstr)
	      (cal-insert "正解!")))
	(error "間違いです!")))))

(defun cal-save-and-upload-logfile (&optional terminate)
  (cal-save-logfile terminate)
  (sit-for 0.1)
  (cal-upload-logfile))

(defun cal-save-logfile (&optional terminate)
  "Save the current log."
  (save-excursion
    (set-buffer (get-buffer-create " *CALLOG*"))
    (goto-char (point-max))
    (if terminate
	(progn
	  (unless (bolp) (insert "\n"))
	  (insert (format "-----\nCAL session terminated\nat: %s\n"
			  (current-time-string)))))
    (write-region (point-min) (point-max) 
		  (concat my-cal-session-log-file
			  (int-to-string cal-session-count))
		  nil 0)))

(defun cal-upload-logfile ()
  "Upload log file."
  (let ((file 
	 (concat my-cal-session-log-file
		 (int-to-string cal-session-count))))
    (cal-upload file cal-upload-url)))

(require 'advice)

(defadvice write-region (around cal-ad first activate)
  "Make sure that the file will be written in euc-japan-unix."
  (let ((buffer-file-coding-system 'euc-japan-unix))
    ad-do-it))

;; parser.el : use English error message for ReadToken

(defun ReadToken (beg end &optional pos-info)
  "convert region (BEG END) into token list. information on position 
is added in case POS-INFO and tokens are OP-tokens or VAR-tokens."
  (let ((case-fold-search nil) (l nil))
    (goto-char beg)
    (save-restriction
      (narrow-to-region beg end)
      (while (< (point) end)
	(let ((p (point)) (cont t))
	  ;; skip white chars and comments
	  (while cont
	    (if WhiteExp (skip-chars-forward WhiteExp))
	    (if (and CommentExp (looking-at CommentExp))
		(progn
		  (end-of-line)
		  (if (< (point) end) (forward-char 1))
		  ))
	    (if (< p (point))
		(setq p (point))
	      (setq cont nil))))
	(cond ((eobp) nil)
	      ((looking-at OpenExp)
	       (let ((open (match-string 0))
		     p)
		 (setq p (match-end 0))
		 (parser-forward-sexp open)
		 (if (> (point) p)
		     ;; we have a matching pair of parens, so we parse the
		     ;; region in between these parens.
		     (let ((len (length (MatchingParen open))))
		       (setq l (cons
				(cons 'paren 
				      (cons open 
					    (ReadToken
					     p
					     (- (point) len)
					     pos-info)))
				l))
		       (forward-char len))
		   (error "Cannot find the matching open!"))))
	      ((and (stringp SvarExp) (posix-looking-at SvarExp))
	       (setq l 
		     (cons (list 'svar 
				 ;; we chop off the first character, which
				 ;; is used to only to make the expression
				 ;; a schematic variable.  (typically the
				 ;; character is ^ )
				 (buffer-substring (1+ (match-beginning 0))
						   (match-end 0)))
			   l))
	       (goto-char (match-end 0)))
	      ((looking-at ElispStart)
	       (forward-char 1)
	       (let ((p (point)))
		 (with-syntax-table emacs-lisp-mode-syntax-table
		   (parser-forward-sexp)
		   (setq l
			 (cons
			  (list 'sexp 
				(car
				 (read-from-string 
				  (buffer-substring-no-properties p (point)))))
			  l)))))
	      ((looking-at ElispDquote)
	       (let ((p (point)))
		 (with-syntax-table emacs-lisp-mode-syntax-table
		   (parser-forward-sexp)
		   (setq l
			 (cons
			  (list 'sexp 
				(car
				 (read-from-string 
				  (buffer-substring-no-properties p (point)))))
			  l)))))
	      ((looking-at UnitQuote)
	       (forward-char 1)
	       ;; we call ReadAToken with POS-INFO
	       (setq l
		     (cons (list 'unit (ReadAToken pos-info)) l)))
	      (t
	       (let* ((var-string
		       (if (and
			    (posix-looking-at VarExp)
			    ;; just to be sure
			    (< (match-beginning 0) (match-end 0)))
			   (buffer-substring (match-beginning 0) (match-end 0))
			 nil))
		      (var-string-end (if var-string (match-end 0) nil))
		      (var-string-len (length var-string))
		      (string
		       (if (and
			    (posix-looking-at PrimExpPat)
			    ;; just to be sure
			    (< (match-beginning 0) (match-end 0)))
			   (buffer-substring (match-beginning 0) (match-end 0))
			 nil))
		      (string-end (if string (match-end 0) nil))
		      (string-len (length string)))
		 (if var-string
		    ;; we have a candidate for variable
		    (if string
			;; in this case we are looking at either a
			;; variable or an op word (reserved word)
			(if (> var-string-len string-len)
			    ;; we got a variable!
			    (progn
			      (setq l 
				    (cons 
				     (if pos-info 
					 (list 'var var-string (point))
				       (list 'var var-string))
				     l))
			      (goto-char var-string-end))
			  ;; in this case var-string and string must be
			  ;; the same string, since var-string cannot be
			  ;; shorter than string
			  ;; so, we have a reserved word here.
			  (if (looking-at CloseExp)
			      (error "Cannot find the matching open!")
			    (setq l 
				  (cons 
				   (if pos-info
				       (list 'op string (point))
				     (list 'op string))
				   l)))
			  (goto-char string-end))
		      ;; we have a variable here too!
		      (setq l 
			    (cons 
			     (if pos-info (list 'var var-string (point))
			       (list 'var var-string))
			    l))
		      (goto-char var-string-end))
		  ;; we should be looking at an op word here
		  (if string
		      ;; yes, we are looking at an op word
		      (progn
			(if (looking-at CloseExp)
			    (error "Cannot find the matching close!")
			  ;;(setq l (cons (list 'token 'op string) l)))
			  (setq l 
				(cons 
				 (if pos-info
				     (list 'op string (point))
				   (list 'op string))
				 l)))
			(goto-char string-end))
		    ;; no, we are NOT looking at an op word.
		    ;; we must report an error!
		    ;; we use global OFFSET to set epos correctly.
		    ;; (see ParseSTring)
		    (if (and (boundp 'offset) offset)
			(cal-set-epos (+ offset (1- (point)))
				      (+ offset (point)))
		      (cal-set-epos (1- (point)) (point)))
		    (parse-fail (format "「%s」はトークンでありません!"
				  (char-to-string (following-char)))
			  nil))
		  )))))
	(reverse l))))

(defun cal-string-closure (str beg-end)
  "Given a string STR and a region BEG-END=(BEG END) which specifies a
substring of STR, compute the smallest region with balanced
parentheses containing the given region.  The result is
given as a region."
  (let ((beg (first beg-end)) (end (second beg-end))
	(cont t) error-condition (dead-end (length str)))
    (while cont
      (setq error-condition nil)
      (condition-case condition
	  (ReadString (substring str beg end))
	(error
	 (setq error-condition condition)))
      (if error-condition
	  (let ((error-str (second error-condition)))
	    (cond ((string= error-str "Cannot find the matching open!")
		   ;; extend region to the left
		   (if (= beg 0)
		       ;; cannot extend to the left!
		       (setq cont nil)
		       ;(error "Implementation error: cal-string-closure")
		     (setq beg (1- beg))))
		  ((string= error-str "Cannot find the matching close!")
		   ;; extend region to the right
		   (if (= end dead-end)
		       ;; cannot extend to the right!
		       (setq cont nil)
		       ;(error "Implementation error: cal-string-closure")
		     (setq end (1+ end))))
		  ((string= error-str "Unbalanced parentheses")
		   ;; this case is same as above, since the error should
		   ;; have been caused by forward-sexp.
		   ;; extend region to the right
		   (if (= end dead-end)
		       ;; cannot extend to the right!
		       (setq cont nil)
		       ;(error "Implementation error: cal-string-closure")
		     (setq end (1+ end))))
		  (t
		   (setq cont nil)
		   ;(error "Implementation error: cal-string-closure")
		   )))
	(setq cont nil)))
    (list beg end)))
