;; tex support within cal

(defvar caltex-size "6"
  "Specifies the -s option of xdvi command.  It must be \"6\" or \"5\".")

(defvar caltex-redo nil
  "Set this variablet to t if reevaluation is needed.")

(defvar caltex-tt-var nil
  "Set this variable to T if variables should be set in typewriter font.")

(defvar META-VARS nil)

(defconst caltex-nil-pad-list
  '("#" "Π" "∀" "∃" "S")
  "List of op names used in unitlist which do not require
pad for the following unit.")

(defun cal-eval-string (str)
  "Evaluate STR as elisp sexp."
  (let (v)
    (with-temp-buffer
      (insert str)
      (insert ")")
      (goto-char (point-min))
      (insert "(setq v ")
      (eval-region (point-min) (point-max)))
    v))

(defun caltex-geometry ()
  "Return the appropriate geometry for the current CALTEX-SIZE."
  (if (string= caltex-size "6")
      "660x500"
    "792x600"))

(defun caltex-play (game)
  "Set up CAL and play GAME.  Also create a buffer containing the GAME file."
  (or (get-buffer "*cal*") (cal-mode))
  (unless (string= cal-current-game game)
    (cal-play game t))
  (save-excursion
    (set-buffer (get-buffer-create "*caltex*"))
    (erase-buffer)
    (insert-file-contents (cal-game-file game t))))

(defun caltex-parse (str class)
  "First ParseString wit args STR and CLASS.  The result must be of the
form (sexp STRING), and returns the STRING."
  (second (ParseString str class)))

(defun caltex-parse-rule (rule &optional ITALIC RULE-ARG)
  "Parse RULE into tex form."
  (let ((str
	 (save-excursion
	   (let ((case-fold-search nil) p q)
	     (set-buffer "*caltex*")
	     (goto-char (point-max))
	     (search-backward (concat "\"" rule "\""))
	     (setq p (point))
	     ;; we search another rule, which if exists must be
	     ;; on differnet line
	     (end-of-line)
	     (if (re-search-forward 
		  ;; the line below is the pattern of a general rule.
		  "\"[^\"]*\"[ \n\t]*([^)]*)[ \n\t]*:" nil t)
		 ;; we got it
		 (setq q (point))
	       ;; this is the last rule
	       (setq q (point-max)))
	     (goto-char q)
	     (search-backward ";") ;; let's hope this is what we want!
	     (buffer-substring-no-properties p (point))))) 
	;; we use META-VARS
	META-VARS)
    ;; optional argumet ITALIC is dynamically given to tex-defrule2
    (caltex-parse str 'tex-defrule2)))

(defun caltex-defrule2-fun (TREE)
  "Aux fun called from within the syntax class TEX-DEFRULE2"
  (let ((rule-name (second (first (first TREE))))
	(concl (second (second TREE)))
	(prems (third TREE)))
    ;; for debug
    ;;(setq RT TREE)
    (list 'sexp
	  (if RULE-ARG
	      (format "\\infer[\\mbox{\\small $%s\\mytt{(}\\boldsymbol{%s}\\mytt{)}$}]{%s}{%s}"
		      rule-name RULE-ARG concl 
		      (caltex-list-to-and-list prems))
	    (format "\\infer[\\mbox{\\small $%s$}]{%s}{%s}"
		    rule-name concl (caltex-list-to-and-list prems))))))

(defun caltex-check-tt (deriv)
  "Check DERIV, which is a string, if it is a correct derivation, and if so
print it in alltt."
  (let (newstr)
    (save-excursion
      (if (cal-check-derivation deriv)
	  (with-temp-buffer
	    (insert deriv)
	    (untabify (point-min) (point-max))
	    (goto-char (point-min))
	    (while (search-forward "{" nil t)
	      (delete-char -1)
	      (insert "\\leftbrace"))
	    (goto-char (point-min))
	    (while (search-forward "}" nil t)
	      (delete-char -1)
	      (insert "\\rightbrace"))
	    ;; we can now put brace around \leftbrace and \rightbrace
	    (goto-char (point-min))
	    (while (search-forward "\\rightbrace" nil t)
	      (goto-char (match-beginning 0))
	      (insert "{")
	      (goto-char (1+ (match-end 0)))
	      (insert "}"))
	    (goto-char (point-min))
	    (while (search-forward "\\leftbrace" nil t)
	      (goto-char (match-beginning 0))
	      (insert "{")
	      (goto-char (1+ (match-end 0)))
	      (insert "}"))
	    (goto-char (point-min))
	    (while (not (eobp))
	      (insert "{\\hskip 1cm}")
	      (while (looking-at " ")
		(insert "\\")
		(forward-char 1))
	      (end-of-line)
	      (insert "\\\\")
	      (when (not (eobp))
		(forward-char 1)))
	    ;; remove the last two backslashes
	    (goto-char (point-max))
	    (when (search-backward "\\\\" nil t)
	      (delete-char 2))
	    (goto-char (point-min))
	    (insert "\\mbox{}\\vskip -1.5cm\\mbox{}\\begin{alltt}")
	    (goto-char (point-max))
	    (insert "\\end{alltt}")
	    (setq newstr 
		  (buffer-substring-no-properties (point-min) (point-max))))))
    newstr))

(defun caltex-check-deriv (deriv &optional caltex-tt-var vertical)
  "Check DERIV, which is a string, if it is a correct derivation, and if so
print it in proof style.  If optional CALTEX-TT-VAR is given, variables
will be set in typewriter font.  (see, the constant CALTEX-TT-VAR)
If optional VERTICAL is given, ABSv is used when hypothetical derivation
is typeset.  (see, TEX-DERIV in TeX.gm)"
  (let ((newstr ""))
    (save-excursion
      (when (cal-check-derivation deriv)
	(setq newstr (caltex-parse deriv 'tex-derivation))
	(when (string= (substring newstr 0 5) "\\ABS{")
	  ;; replace it with the following command. see kougi.sty.
	  ;;(setq N newstr)
	  (setq newstr (concat "\\ABStop{"
			       (substring newstr 5))))))
    newstr))

(defun caltex-hyp (str)
  (second (ParseString str 'tex-hyp)))

(defun caltex-hyp-j (str)
  (second (ParseString str 'tex-hyp-judg)))

(defun caltex-j (str)
  (second (ParseString str 'tex-judgment)))

(defun caltex-term (str)
  (second (ParseString str 'tex-term)))

(defun caltex-op (str)
  (second (ParseString str 'tex-op)))

(defun caltex-hyp-seq (str) 
  (caltex-list-to-comma-list (ParseString str 'tex-hyp-seq)))

(defun caltex-hyp-seq-tt (str)
  (let ((caltex-tt-var t))
    (caltex-list-to-comma-list (ParseString str 'tex-hyp-seq))))

(defun caltex-hyp-j-tt (str)
  (let ((caltex-tt-var t))
    (second (ParseString str 'tex-hyp-judg))))

(defun caltex-j-tt (str)
  (let ((caltex-tt-var t))
    (second (ParseString str 'tex-judgment))))

(defun caltex-term-tt (str)
 (let ((caltex-tt-var t))
   (second (ParseString str 'tex-term))))

(defun caltex-term-sch (str)
  "Parse STR as a schematic term."
  ;; we use NF-VARS2 since STR may contain meta-application.
 (let ((META-VARS (mapcar 'second (nf-vars2 (ParseString str 'term)))))
   (second (ParseString str 'tex-term))))

(defun caltex-unitlist-tt (str)
 (let ((caltex-tt-var t))
   (second (ParseString str 'tex-unitlist))))

(defun caltex-unitlist-main (tree)
  "Convert parse TREE of unitlist into tex commands."
  ;; for debug
  ;;(setq Tree (cons tree Tree))
  (when (eq 'qq (car tree)) (setq tree (second tree)))
  (let ((result "")
	;; previous unit
	(prev nil))
    (while tree
      (let* ((item (if (eq 'unit (car (car tree)))
		       (second (car tree))
		     ;; svar
		     (car tree)))
	     (kind (car item))
	     (pad "\\ "))
	(cond ((eq kind 'var)
	       (if (null prev) (setq pad ""))
	       ;; we use \mytt for variables
	       (setq result 
		     (concat result pad "\\mytt{" (second item) "}")
		     prev 'unit))
	      ((eq kind 'svar)
	       (if (null prev) (setq pad ""))
	       (setq result 
		     (concat result pad "\\boldsymbol{" 
			     (caltex-greek-char (second item)) "}")
		     prev 'unit))
	      ((eq kind 'op)
	       (if (or (null prev) 
		       (string= "," (second item))
		       (string= ";" (second item))
		       (string= "#" (second item)))
		   (setq pad ""))
	       (setq result 
		     (let* ((name (second item))
			    (greek (caltex-greek-char name)))
		       (if (string= name greek)
			   (concat result pad "\\mytt{" greek  "}")
			 (concat result pad "{" greek "}")))
		     prev (if (member (second item) caltex-nil-pad-list)
			      nil 
			    'punc)))
	      ((eq kind 'paren)
	       (setq pad (if (eq prev 'punc) "\\ " ""))
	       (let* ((par (second item))
		      (match-par (MatchingParen par)))
		 (when (string= par "{") (setq par "\\leftbrace"))
		 (when (string= match-par "}") (setq match-par "\\rightbrace"))
		 (setq result 
		       (concat result pad "\\mytt{" par "}"
			       (caltex-unitlist-main (cddr item)) 
			       "\\mytt{" match-par "}")
		       prev 'paren)))
	      (t (error "Implementation error: caltex-unitlist-main")))
	(setq tree (cdr tree))))
    result))

(defun caltex-unitlist (tree)
  (list 'sexp (caltex-unitlist-main tree)))

(defun caltex-greek-char (name)
  "Convert NAME to greek or math symbol."
  (let (val)
    (if (setq val
	      (assoc name
		     '(("al" "\\alpha")
		       ("be" "\\beta")
		       ("ga" "\\gamma")
		       ("de" "\\delta")
		       ("ph" "\\phi")
		       ("ps" "\\psi")
		       ("ka" "\\kappa")
		       ("mu" "\\mu")
		       ("nu" "\\nu")
		       ("rh" "\\rho")
		       ("pi" "\\pi")
		       ("si" "\\sigma")
		       ("ta" "\\tau")
		       ("Ga" "\\Gamma")
		       ("De" "\\Delta")
		       ("Pi" "\\Pi")
		       ("⇒" "\\Rightarrow")
		       ("→" "\\rightarrow")
		       ("⊥" "\\false")
		       ("⊃" "\\imp")
		       ("∧" "\\conj")
		       ("∨" "\\disj")
		       ("¬" "\\neg")
		       ("∀" "\\forall")
		       ("∃" "\\exists")
		       ("Π" "\\Pi")
		       ("Σ" "\\Sigma")
		       ("λ" "\\lambda")
		       ("∈" "\\in")
		       ("#" "\\mytt{\\symbol{\"23}}"))))
	(second val)
      name)))

(defun caltex-str-to-tex (str)
  "Convert STR to tex command appopriately.  For exaple,
a1⊃b2 will be converted to \mytt{a1}{\lambda}\mytt{b2}."
  (if (find-multibyte-characters str 0)
      ;; we should convert STR
      (let ((result "") (ascii nil) (multi nil))
	;; ascii and multi are both nil only initially.
	;; after seeing a character always one of them is t and
	;; the other is nil.
	(while (not (string= str ""))
	  (let ((str1 (substring str 0 1)))
	    (if (find-multibyte-characters str1 0)
		;; we are looking at a multi char.
		(if ascii
		    ;; previous char was ascii
		    (setq result (concat result "}{" (caltex-greek-char str1))
			  multi t
			  ascii nil)
		  (if multi
		      ;; previous char is also multi, so just concatenate
		      (setq result (concat result (caltex-greek-char str1)))
		    ;; this is the first char of the string 
		    (setq result (concat "{" (caltex-greek-char str1))
			  multi t)))
	      (if ascii
		  ;; previous char is also ascii, so just concatenate 
		  (setq result (concat result (caltex-greek-char str1)))
		(if multi
		    ;; previous char was multi
		    (setq result (concat result "}\\mytt{" str1)
			  multi nil
			  ascii t) 
		  ;; this is the first char of the string 
		  (setq result (concat "\\mytt{"  str1) 
			ascii t))))
	    (setq str (substring str 1))))
	;; don't forget to close the brace
	(concat result "}"))
    ;; just put mytt around the STR
    (concat "\\mytt{" str "}")))

(defun caltex-str-to-tex-small (str)
  "Convert STR to tex command appopriately in small size.  For exaple,
a1⊃b2 will be converted to mbox small ..."
  (if (find-multibyte-characters str 0)
      ;; we should convert STR
      (let ((result "") (ascii nil) (multi nil))
	;; ascii and multi are both nil only initially.
	;; after seeing a character always one of them is t and
	;; the other is nil.
	(while (not (string= str ""))
	  (let ((str1 (substring str 0 1)))
	    (if (find-multibyte-characters str1 0)
		;; we are looking at a multi char.
		(if ascii
		    ;; previous char was ascii
		    (setq result (concat result "}$" (caltex-greek-char str1))
			  multi t
			  ascii nil)
		  (if multi
		      ;; previous char is also multi, so just concatenate
		      (setq result (concat result (caltex-greek-char str1)))
		    ;; this is the first char of the string 
		    (setq result (concat "\\mbox{\\small$" 
					 (caltex-greek-char str1))
			  multi t)))
	      ;; we are looking at an ascii char.
	      (if ascii
		  ;; previous char is also ascii, so just concatenate 
		  (setq result (concat result (caltex-greek-char str1)))
		(if multi
		    ;; previous char was multi
		    (setq result (concat result "$\\mytt{" str1)
			  multi nil
			  ascii t) 
		  ;; this is the first char of the string 
		  (setq result (concat "\\mbox{\\small\\mytt{"  str1) 
			ascii t))))
	    (setq str (substring str 1))))
	;; don't forget to close the brace
	(concat result (if ascii "}}" "$}")))
    ;; put appropriate commands around STR
    (concat "\\mbox{\\small\\mytt{" str "}}")))

(defun caltex-meta-apply (token-list)
  "Convert meta application into proper form.  See, LambdaTerm.gm, for
instance."
  (list 'sexp
	(concat "\\boldsymbol{(}"
		(second (Parse (cddr (first token-list))
			       '(tex-list tex-term ",")))
		"\\boldsymbol{)}")))

(defun caltex-var (name)
  "convert NAME into appropriate tex command."
  (if (member name META-VARS)
      (list 'sexp (concat "\\boldsymbol{" (caltex-greek-char name) "}"))
    (if caltex-tt-var
	(cond ((and (= (length name) 2)
		 ;; first char is ?i or ?I
		 (or (= ?i (aref name 0)) (= ?I (aref name 0))))
	       (list 'sexp (concat "{\\it " (substring name 1) "\\/}")))
	      ((and (= (length name) 3)
		 ;; first char is ?g
		 (= ?g (aref name 0)))
	       (list 'sexp (caltex-greek-char (substring name 1))))
	      (t
	       (list 'sexp (concat "\\mytt{" name "}"))))
      (list 'sexp (concat "{\\it " name "\\/}")))))

(defun caltex-varref (varref)
  (if (eq 'svar (car varref))
      ;; meta-variable
      (list 'sexp (concat "{\\it " (second varref) "}"))
    (let ((str (exp-sharp-var-dest varref))
	  (no (exp-sharp-var-sharps varref)))
      (list 'sexp (caltex-varref-to-tex str no)))))

(defun caltex-varref-to-tex (str no)
  "Convert varref of the form #...#x into tex form.  NO is the
number of sharps."
  (let ((name (substring str no)) (sharps ""))
    (when (> no 0)
      (while (> no 0)
	;; the hexadecimal code for # is 23
	(setq sharps (concat "\\symbol{\"23}" sharps)
	      no (1- no)))
      (setq sharps (concat "\\mytt{" sharps "}")))
    (if (member str META-VARS)
	(concat "\\boldsymbol{" (caltex-greek-char str) "}")
      (if caltex-tt-var
	  (cond ((and (= (length name) 2)
		      ;; first char is ?i or ?I
		      (or (= ?i (aref name 0)) (= ?I (aref name 0))))
		 (concat sharps "{\\it " (substring name 1) "\\/}"))
		((and (= (length name) 3)
		      ;; first char is ?g
		      (= ?g (aref name 0)))
		 (concat sharps (caltex-greek-char (substring name 1))))
		(t
		 (concat sharps "\\mytt{" name "}"))) 
	(concat sharps "{\\it " name "\\/}")))))

(defun caltex-list-to-and-list (list)
  "LIST is of the form ((sexp ...) ... (sexp ...)). convert this
LIST into appropriate string.  This function is called from tex-deriv
defined in TeX.gm."
  (let ((str ""))
    (while list
      (let* ((newstr (second (car list)))
	     (match (string-match "\\ABStop" newstr)))
	(when (and match (= match 1))
	  ;; in this case, replace it as follows
	  ;; the variable vertical should be available
	  (if (and (boundp 'vertical) vertical)
	      (setq newstr (concat "\\ABSv" (substring newstr 8)))
	    (setq newstr (concat "\\ABS" (substring newstr 7)))))
	(setq str
	      (if (= (length list) 1)
		  (concat str newstr)
		(concat str newstr "&")))
	(setq list (cdr list))))
    str))

(defun caltex-list-to-comma-list (list)
  "LIST is of the form ((sexp ...) ... (sexp ...)). convert this
     LIST into appropriate string."
  (let ((str ""))
    (while list
      (let ((newstr (second (car list))))
	(setq str
	      (if (= (length list) 1)
		  (concat str newstr)
		(concat str newstr "\\mytt{,}{\\hskip 0.1cm}")))
	(setq list (cdr list))))
    str))

(provide 'caltex)

