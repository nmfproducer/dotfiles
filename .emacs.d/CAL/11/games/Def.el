;; -*-Emacs-Lisp-*-
;; version 2 of Sun Feb  9 13:19:27 2003

;; variable を明示的に定義しないときには，DefaultVarExp を用いる．
;; constant，operator は CommonConstExp, CommonOpExp を拡張する．
;; open，close は CommonOpenExp, CommonCloseExp を拡張する．

(defclass deftokens
  (or
   (and 
    (postfix dummy ";")
    (eval
     (let ((debug-on-error t)
	   var-pat const-pat op-pat open-pat close-pat)
       ;; TOKEN-LIST is dynamically bound by the function Parse in parser.el
       (Parse token-list 
	      '(or 
		(postfix (list deftoken ";") ";")
		(throw "トークン定義集合でありません")))
       (setq VarExp
	     (if var-pat
		 (MkOrPattern var-pat)
	       DefaultVarExp))
       (setq ConstExp
	     (if const-pat
		 (concat (regexp-quote name-of-the-game) "\\|"
			 CommonConstExp "\\|" (MkOrPattern const-pat))
	       (concat (regexp-quote name-of-the-game) "\\|" CommonConstExp)))
       (setq OpExp
	     (if op-pat
		 (concat CommonOpExp "\\|" (MkOrPattern op-pat))
	       CommonOpExp))
       (setq OpenExp
	     (if open-pat
		 (concat CommonOpenExp "\\|" (MkOrPattern open-pat))
	       CommonOpenExp))
       (setq CloseExp
	     (if close-pat
		 (concat CommonCloseExp "\\|" (MkOrPattern close-pat))
	       CommonCloseExp))
       (setq PEP
	     (concat ConstExp "\\|" OpExp "\\|" OpenExp "\\|" CloseExp))
       (save-excursion
	 (set-buffer " *GAMECOMPILE*")
	 (goto-char (point-min))
	 (insert "(let (var-pat const-pat op-pat open-pat close-pat)")
	 (goto-char (point-max))
	 (insert "\n   (cal-deftokens))\n"))
       nil) ))
   (throw "トークン定義の最後に必要な「;」がありません")))

(defclass deftoken
  (or
   (and
    (infix "variable" "::=" (list token-pattern "|"))
    (eval 
     (save-excursion
       (setq var-pat (third TREE))
       (set-buffer " *GAMECOMPILE*")
       (goto-char (point-max))
       (insert "\n   (setq var-pat '")
       (cal-print-tree (third TREE) 0)
       (insert ")"))))
   (and
    (infix "constant" "::=" (list token-pattern "|"))
    (eval
     (save-excursion
       (setq const-pat (third TREE))
       (set-buffer " *GAMECOMPILE*")
       (goto-char (point-max))
       (insert "\n   (setq const-pat '")
       (cal-print-tree (third TREE) 0)
       (insert ")"))))
   (and
    (infix "operator" "::=" (list token-pattern "|"))
    (eval
     (save-excursion
       (setq op-pat (third TREE))
       (set-buffer " *GAMECOMPILE*")
       (goto-char (point-max))
       (insert "\n   (setq op-pat '")
       (cal-print-tree (third TREE) 0)
       (insert ")"))))
   (throw "トークン定義でありません")
   ))

(defclass token-pattern
  (or
   (and
    sexp
    (eval
     (let ((exp (second TREE)))
       (if (stringp exp)
	   (let ((reg-quote (regexp-quote exp)))
	     ;; regxep-quote the EXP if necessary
	     (if (string= exp reg-quote) exp reg-quote))
	 (throw 'err 
		(list 'err (list exp)
		      "「%s」 should be a string"))))))
   (and
    (prefix "regexp" sexp)
    (eval
     (let ((exp (second (second TREE))))
       (if (stringp exp)
	   ;; return the EXP as it is
	   exp
	 (throw 'err 
		(list 'err (list exp)
		      "「%s」 should be a string"))))))))

(defclass defclasses
  (or
   (and 
    (postfix dummy ";")
    (eval
     (let ((debug-on-error t))
       ;; TOKEN-LIST is dynamically bound by the function Parse in parser.el
       (Parse token-list 
	      '(or 
		(postfix (list defclass ";") ";")
		(throw "構文定義集合でありません")))
       nil)))
   (throw "構文定義集合の最後に必要な「;」がありません")))

(defclass defclass
  (or
   (and
    (infix var "::=" pattern)
    (eval
     (save-excursion
       (DefClass (intern (second (second TREE))) (second (third TREE)))
       (set-buffer " *GAMECOMPILE*")
       (goto-char (point-max))
       (insert "\n\n")
       (insert "(DefClass '")
       (cal-print-tree (intern (second (second TREE))) 0) 
       (insert " '")
       (cal-print-tree (second (third TREE)) 0)
       (insert ")\n"))))
   (and
    sexp
    ;; we evaluate by Lisp the SEXP just read in for effects.
    (eval 
     (save-excursion
      (eval (second TREE))
       (set-buffer " *GAMECOMPILE*")
       (goto-char (point-max))
       (insert "\n\n")
       (cal-print-tree (second TREE) 0)
       (insert "\n"))))
   (throw "構文定義でありません")
   ))

(defclass pattern
  (or
   sexp
   (paren "(" pattern)
   (and
    (infix dummy "|" dummy)
    (commit
     (and
      (list pattern "|")
      (eval
       (let ((list TREE))
	 (list 'sexp
	       (cons 'or
		     (mapcar
		      (lambda (item) (second item)) list))))))))
   (and
    (infix dummy "&" dummy)
    (commit
     (and
      (infix-i pattern "&" pattern)
      (eval
       (let ((arg1 (second (first TREE)))
	     (arg2 (second (second TREE))))
	 (list 'sexp
	       (list 'and arg1 arg2)))))))
   (and
    (prefix "prefix" 
	    (paren "(" (infix-i pattern "," pattern)))
    (eval
     (let ((arg1 (second (first (second TREE))))
	   (arg2 (second (second (second TREE)))))
       (list 'sexp
	     (list 'prefix arg1 arg2)))))
   (and
    (prefix "prefix-c" 
	    (paren "(" (infix-i pattern "," pattern)))
    (eval
     (let ((arg1 (second (first (second TREE))))
	   (arg2 (second (second (second TREE)))))
       (list 'sexp
	     (list 'prefix-c arg1 arg2)))))
   (and
    (prefix "prefix-i" 
	    (paren "(" (infix-i pattern "," pattern)))
    (eval
     (let ((arg1 (second (first (second TREE))))
	   (arg2 (second (second (second TREE)))))
       (list 'sexp
	     (list 'prefix-i arg1 arg2)))))
   (and
    (prefix "tex-prefix" 
	    (paren "(" (infix-i pattern "," pattern)))
    (eval
     (let ((arg1 (second (first (second TREE))))
	   (arg2 (second (second (second TREE)))))
       (list 'sexp
	     (list 'tex-prefix arg1 arg2)))))
   (and
    (prefix "postfix" 
	    (paren "(" (infix-i pattern "," pattern)))
    (eval
     (let ((arg1 (second (first (second TREE))))
	   (arg2 (second (second (second TREE)))))
       (list 'sexp
	     (list 'postfix arg1 arg2)))))
   (and
    (prefix "tex-postfix" 
	    (paren "(" (infix-i pattern "," pattern)))
    (eval
     (let ((arg1 (second (first (second TREE))))
	   (arg2 (second (second (second TREE)))))
       (list 'sexp
	     (list 'tex-postfix arg1 arg2)))))
   (and
    (prefix "paren" 
	    (paren "(" (infix-i pattern "," pattern)))
    (eval
     (let ((arg1 (second (first (second TREE))))
	   (arg2 (second (second (second TREE)))))
       (list 'sexp
	     (list 'paren arg1 arg2)))))
   (and
    (prefix "tex-paren" 
	    (paren "(" (infix-i pattern "," pattern)))
    (eval
     (let ((arg1 (second (first (second TREE))))
	   (arg2 (second (second (second TREE)))))
       (list 'sexp
	     (list 'tex-paren arg1 arg2)))))
   (and
    (prefix "list" 
	    (paren "(" (infix-i pattern "," pattern)))
    (eval
     (let ((arg1 (second (first (second TREE))))
	   (arg2 (second (second (second TREE)))))
       (list 'sexp
	     (list 'list arg1 arg2)))))
   (and
    (prefix "list+" 
	    (paren "(" (infix-i pattern "," pattern)))
    (eval
     (let ((arg1 (second (first (second TREE))))
	   (arg2 (second (second (second TREE)))))
       (list 'sexp
	     (list 'list+ arg1 arg2)))))
   (and
    (prefix "tex-list" 
	    (paren "(" (infix-i pattern "," pattern)))
    (eval
     (let ((arg1 (second (first (second TREE))))
	   (arg2 (second (second (second TREE)))))
       (list 'sexp
	     (list 'tex-list arg1 arg2)))))
   (and
    (prefix (or "infixL" "infix")
	    (paren "(" 
		   (infix-ic pattern ","
			  (infix-i pattern "," pattern))))
    (eval
     (let ((arg1 (second (first (second TREE))))
	   (arg2 (second (second (second TREE))))
	   (arg3 (second (third (second TREE)))))
       (list 'sexp
	     (list 'infix arg1 arg2 arg3)))))
   (and
    (prefix "infixR"
	    (paren "(" 
		   (infix-ic pattern ","
			  (infix-i pattern "," pattern))))
    (eval
     (let ((arg1 (second (first (second TREE))))
	   (arg2 (second (second (second TREE))))
	   (arg3 (second (third (second TREE)))))
       (list 'sexp
	     (list 'infixR arg1 arg2 arg3)))))
   (and
    (prefix "infix-i"
	    (paren "(" 
		   (infix-ic pattern ","
			  (infix-i pattern "," pattern))))
    (eval
     (let ((arg1 (second (first (second TREE))))
	   (arg2 (second (second (second TREE))))
	   (arg3 (second (third (second TREE)))))
       (list 'sexp
	     (list 'infix-i arg1 arg2 arg3)))))
   (and
    (prefix "infix-ic"
	    (paren "(" 
		   (infix-ic pattern ","
			  (infix-i pattern "," pattern))))
    (eval
     (let ((arg1 (second (first (second TREE))))
	   (arg2 (second (second (second TREE))))
	   (arg3 (second (third (second TREE)))))
       (list 'sexp
	     (list 'infix-ic arg1 arg2 arg3)))))
   (and
    (prefix (or "tex-infixL" "tex-infix")
	    (paren "(" 
		   (infix-ic pattern ","
			  (infix-i pattern "," pattern))))
    (eval
     (let ((arg1 (second (first (second TREE))))
	   (arg2 (second (second (second TREE))))
	   (arg3 (second (third (second TREE)))))
       (list 'sexp
	     (list 'tex-infix arg1 arg2 arg3)))))
   (and
    (prefix "tex-infixR"
	    (paren "(" 
		   (infix-ic pattern ","
			  (infix-i pattern "," pattern))))
    (eval
     (let ((arg1 (second (first (second TREE))))
	   (arg2 (second (second (second TREE))))
	   (arg3 (second (third (second TREE)))))
       (list 'sexp
	     (list 'tex-infixR arg1 arg2 arg3)))))
   (and
    (prefix "or" (paren "(" (list pattern ",")))
    (eval
     (let ((list (second TREE)))
       (list 'sexp
	     (cons 'or
		   (mapcar
		    (lambda (item) (second item)) list))))))
   (and
    (prefix "and" 
	    (paren "(" (infix-i pattern "," pattern)))
    (eval
     (let ((arg1 (second (first (second TREE))))
	   (arg2 (second (second (second TREE)))))
       (list 'sexp
	     (list 'and arg1 arg2)))))
   (and var (eval (list 'sexp (intern (second TREE)))))
   (and
    (prefix "abs" (paren "(" pattern))
    (eval
     (let ((item (second (second TREE))))
       (list 'sexp
	     (list 'abs item)))))
   (and
    (prefix "commit" (paren "(" pattern))
    (eval
     (let ((item (second (second TREE))))
       (list 'sexp
	     (list 'commit item)))))
   (and
    (prefix "eval" sexp)
    (eval
     (let ((item (second (second TREE))))
       (list 'sexp
	     (list 'eval item)))))
   (and
    (prefix "catch" (paren "(" pattern))
    (eval
     (let ((item (second (second TREE))))
       (list 'sexp
	     (list 'catch item)))))
   (and
    (prefix "throw" sexp)
    (eval
     (let ((item (second (second TREE))))
       (list 'sexp
	     (list 'throw item)))))
   (and
    (prefix (or var op) (paren "(" dummy))
    (throw "「%s」は構文解析のコマンドではありません"))
   (and
    op
    (throw "予約語「%s」はパターンではありません"))
   (throw "「%s」はパターンではありません")
   ))

(defclass defrules
  (or
   (and 
    (postfix dummy ";")
    (eval
     (let ((rule-list nil))
       (Parse token-list
	      '(or
		(postfix (list defrule ";") ";")
		(throw "導出ゲームの規則集合でありません")))
       (DefGame (intern name-of-the-game) rule-list t)
       (save-excursion
	 (set-buffer " *GAMECOMPILE*")
	 (goto-char (point-max))
	 (insert "\n\n")
	 (insert (format "(DefGame '%s '" name-of-the-game))
	 (cal-print-tree rule-list 0)
	 (insert " t)\n"))
       nil)))
   (throw "推論規則の最後に必要な「;」がありません")))

(defclass defrule
  (or
   (and
    (infix-i (prefix sexp-string (paren "(" (list var ",")))
	     ":"
	     (infix-ic judgment ":-" (list judgment ",")))
    (eval
     (let ((rule-name (second (first (first TREE))))
	   (args (mapcar
		  (lambda (item)
		    (intern (second item)))
		  (second  (first TREE))))
	   (clause (second TREE)))
       (setq rule-list 
	     (cons
	      (cons rule-name (cons args clause))
	      rule-list))
       nil)))
   (throw "「%s」は推論規則でありません")))

(defclass sexp-string
  (or
   (and
    sexp
    (eval
     (if (stringp (second TREE))
	 TREE
       (Parse token-list
	      '(throw "「%s」はS式文字列ではありません")))))
   (throw "「%s」はS式文字列ではありません")))
