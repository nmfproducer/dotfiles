
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
      (message "$B%U%!%$%k$N%@%&%s%m!<%I$K<:GT$7$^$7$?(B! $B6541$+(BTA$B$KO"Mm$7$F$/$@$5$$!%(B")
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
		  (message "$B%U%!%$%k$N%"%C%W%m!<%I$K<:GT$7$^$7$?(B! $B6541$+(BTA$B$KO"Mm$7$F$/$@$5$$!%(B")
		  (sit-for 3)
		  (error ""))
	      ;; Q[1] is not solved, so do nothing
	      ))
	;; cal-record-file is broken!
	(cal-upload cal-record-file cal-upload-url 
		    (concat "Broken-cal-record-"
			    (replace-regexp-in-string " " "-" 
						      (current-time-string))))
	(message "CAL $B$N5-O?%U%!%$%k$,@5$7$/$"$j$^$;$s!%6541$+(BTA$B$KO"Mm$7$F$/$@$5$$(B.")
	(sit-for 3)
	(message "")
	(error ""))
    (error "CAL $B$N5-O?%U%!%$%k$,$"$j$^$;$s(B. $B6541$+(BTA$B$KO"Mm$7$F$/$@$5$$(B."))
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
	 "\t\t        $B$h$&$3$=!V7W;;$HO@M}!W$N@$3&$X(B!   \n\n"
	 "\t\t      $BCx:n<T(B:  $B:4F#(B $B2mI'(B,  $B8^==Mr(B $B=_(B\n\n"
	 "\t\t    $B5~ETBg3XBg3X1!(B $B>pJs3X8&5f2J(B $BCNG=>pJs3X@l96(B\n"
	 "\t\t************************************************\n\n"
	 (cal-mode-version) "\n"
	 "$B<ALd$O(B cal@sato.kuis.kyoto-u.ac.jp $B$G$b<u$1IU$1$^$9(B.\n\n"
	 )))
  (if (= cal-session-count 1)
      (progn
	(cal-insert "$B:#2s$O:G=i$N(B CAL $B$N%;%C%7%g%s$G$9(B.\n"
		    "$B%+!<%=%k$,%W%m%s%W%H!Z(BCAL <1> $B![$ND>8e$K$"$k$3$H$r3NG'$7$F(B,\n"
		    "$B!X(Bnews[1]$B!Y$H%?%$%W$7$F$/$@$5$$(B."))
    ;; check new news
    (setq cal-no-of-news (cal-news nil))
    (unless (= cal-no-of-news (length cal-news-list))
      (cal-insert "$BL$FI$N%K%e!<%9$,$"$j$^$9(B.\n"
		  "$B!X(Bnews[]$B!Y$GL$FI$N%K%e!<%9$rA4ItFI$`$3$H$,$G$-$^$9(B."))
    ;; check if cal-user-id is set
    (unless cal-user-id
      (cal-insert "$B@.@SI>2A$KI,MW$J%G!<%?$r5-O?$9$k$?$a$K(B, $BI,$:(B Q[1]\n"
		  "$B$r2rEz$7$F$/$@$5$$(B!")))
  (cal-insert)
  (setq cal-prompt-count 0)
  (cal-increment-prompt-count)
  (use-local-map cal-mode-map)
  (cal-set-syntax)
  (set-syntax-table cal-mode-syntax-table)
  (setq parse-sexp-ignore-comments t)
  (setq major-mode 'cal-mode)
  (setq mode-name "$B7W;;$HO@M}(B")
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
    (open  "(" "<" "{" "[" "$B!T(B")
    (close ")" ">" "}" "]" "$B!U(B")
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
     "$B('(B"
     "$B&0(B"  ;; for universal judgment
     "$B"M(B"
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
    (message "$BDj5A$rFI$_9~$s$G$$$^$9(B...") 
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
      (message "$BDjM}$rFI$_9~$s$G$$$^$9(B...")
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

