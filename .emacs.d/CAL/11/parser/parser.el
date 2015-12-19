(defun parser-version ()
  (interactive)
  (message (concat "parser.el: version 57 of 2004.05.08")))

;; version 57 of 2004.05.08
;; version 56 of 2003.11.27
;; version 55 of 2003.10.07
;; version 54 of 2003.10.06
;; version 53 of 2003.07.09
;; version 52 of 2003.01.14
;; version 51 of 2003.01.08
;; version 50 of 2003.01.01
;; version 49 of 2002.12.30
;; version 48 of 2002.12.13
;; version 47 of 2002.12.01
;; version 46 of 2002.11.25
;; version 45 of 2002.11.24
;; version 44 of 2002.11.22
;; version 43 of 2002.11.20
;; version 42 of 2002.11.16
;; version 41 of 2002.10.02
;; version 40 of 2002.09.14
;; version 39 of 2002.08.27
;; version 38 of 2002.08.03
;; version 37 of 2002.07.08
;; version 36 of 2002.07.07
;; version 35 of 2002.07.05
;; last renamed to dt-list-get-last.

;; version 34 of 2001.11.14
;; Parse: token-list を大域変数 TOKL に bind するようにした．

;; version 33 of 2000.12.23
;; Parse: category dummy を追加．nil に parse する．
;; DefToken: PrimClass の初期値に dummy を追加．
;; ParseOk: 関数を追加

;; version 32 of 2000.11.15
;; ParseToken: エラーメッセージを変更

;; version 31 of 2000.10.26
;; Parse: category に commit を追加．
;; ParseCommit: 新しい関数

;; version 30 of 2000.10.19
;; match-string, defun-maybe を追加: borrowed from poe.el

;; version 29 of 2000.10.16
;; ParsePostfixC, ParsePostfix, ParseParen: name -> (list 'op name) に修正

;; version 28 of 2000.10.04
;; ReadToken: keyword による括弧構造を parse できるようにした．
;; "begin", "end" 等の組で括弧を指定できる．
;;  (open "(" "[" "{" "<" "if" "while")
;;  (close ")" "]" "}" ">" "fi" "od") のようにする．
;; DefToken: PrimExpList を keyword の長さでソートするようにした．
;; PrimClass の初期値 '(any) とした．
;; MatchingParenList を計算するようにした．
;; MatchingParen: MatchingParenList を用いて計算するように変更．
;; Parse: ParseNonEmpty を追加，PrimClass any を parse できるようにした．

;; version 27 of 2000.06.21
;; DefToken: token-type が op のときに PrimClass に op を一回だけ追加する
;; ように変更した。
;; MatchingParen: ... を追加
;; LexString: (set-syntax-table parser-syntax-table) を追加。

;; version 26 of 1999.11.14
;; ParseOr: parse の途中の fail message を error-list にためるように変更．
;; 「適用できる構文解析の規則がありません」をなるべくでないようにした．
;; ConsIfNew を追加: string を list に，すでに要素としてなければ cons する
;; ParsePostfix: unit が括弧構造のときには，たしかに必要な括弧が token-list
;; にあるかを最初のチェックするように変更．
;; Parse: category 「eval」を追加
;; ParseAnd: 最初の parse tree を変数 TREE に bind するようにした．あとで
;; eval の中で参照できるようになる．
;; category postfix-c とこれを parse する関数 ParsePostfixC を追加．

;; version 25 of 1999.10.28
;; ReadString: parser-syntax-table を用いて set-syntax-table するようにした．
;; parser-syntax-table は文法を記述するファイルで set する．

;; version 24 of 1999.10.24
;; ReadToken: 括弧構造を含めて token list を生成するように変更した. emacs 
;; の forward-sexp で対応する括弧を見つけるようにした.
;; 文法に依存して syntax-table を用意する必要がある.
;; ParseString: ReadToken を直接呼ぶようにした.
;; ParseToken: ReadToken の読み込み形式の変更に対応した.
;; TokenListToString: ReadToken の読み込み形式の変更に対応した.
;; Parse: var, op を pattern で表現するとき, これまでの, (token op ),
;; (token var ) でなく (op), (var) に変更
;; ParseList: 上記変更に合わせて修正.

;; version 23 of 1999.10.22
;; ReadToken; primtive expression のパターンをあらかじめ計算して
;; looking-at に渡すように変更した．
;; ParseString: ClassName を catch で囲むようにした．
;; ParsePostfix: pre が nil のときに fail するようにした．
;; ParsePretfix, ParsePrefixI, ParsePrefixC : unit が nil のときに fail
;; するようにした．

;; version 22 of 1999.10.19
;; Parse: ParseAnd, ParseEmpty, ParseCatch, ParseThrow を追加
;; ReadToken: token の位置情報を追加 (使用するか未定)
;; DefToken: op を PrimClass に追加
;; ParseToken: エラーメッセージを改良

;; version 21 of 1999.01.28
;; 関数 ParseInfix+X を追加

;; version 20 of 1999.01.28
;; 関数 ParseInfix+ を追加. token-list の infix-operator を左から順に
;; 成功するまで parse する.

;; version 19 of 1998.12.28
;; ReadToken: エラーの point を global variable EP にセットするようにした.

;; version 18 of 1998.11.21
;; ParseString: save-excursion を追加.

;; version 17 of 1998.11.16
;; 関数 ParsePrefixI を追加. prefix を ignore して捨てる.

;; version 16 of 1998.11.05
;; ParseInfixC での fail の引数が足りないことを修正.

;; version 15 of 1998.10.26
;; ParseParen: エラーメッセージを日本語にし, より詳細にした.

;; version 14 of 1998.10.25
;; ParseList: エラーメッセージを日本語で表示し, 区切り文字を示すように
;; した.

;; version 13 of 1998.10.24
;; ReadToken: エラーメッセージを「計算と論理」の構文に特化してわかり
;; やすくした.

;; version 12 of 1998.10.18
;; 構文解析のルールに prefix-neg を ad hoc に追加. (ParsePrefixNeg)
;; ¬A ≡ A ⊃ ⊥ として解析するため.

;; parser version 11 of 1998.10.09
;; postfix の parse のとき option で名前をつけることを可能にした.
;; 例: (postfix-n "apply" p-lam-term (paren "(" p-lam-term))
;; とすると (ParseString "f(0)" 'p-lam-term) → ("apply" (var "f") (op "0"))
;; のように parse される.

;; 空語を受理しない syntactic class を宣言できるようにした.
;; 例 : (def-non-nil p-lam-term)

;; parser version 10 of 1998.10.05
;; 変数へのマクロ定義を可能にした. token reader が変数を読みこむときに
;; 変数がマクロ定義を持つ場合は, マクロに展開して読みこむ. マクロの定義
;; は logic-mode で行う.
;; parser version 9 of 1998.09.26
;; 自然な abstract syntax にしたがう parse tree を生成するようにした.
;; parser version 8 of 1998.09.24
;; parse tree の最初の要素はその木が属する syntax class
;; ParseParen: default では括弧は grouping だけに使われているとして 
;; parse するようにした. したがって余分な括弧があっても parse tree は
;; 同じになる. 括弧が grouping 以外に使われているときは optional な引
;; 数 t を渡して parse すればよい.
;; parser version 7 of 1998.09.16
;; added ParseInfixLast
;; parser version 6 of 1998.09.01
;; parser version 5 of 1998.08.31
;; parser version 4 of 1998.08.18
;; parser version 3 of 1998.08.16
;; parser version 2 of 1998.05.26

;; we have to make the value of max-specpdl-size larger to parse
;; a meaningful derivation.
;; 640 is the default value.

(setq max-lisp-eval-depth 5000)
(setq max-specpdl-size 5000)

;; borrowed from poe.el

(defmacro defun-maybe (name &rest everything-else)
  "Define NAME as a function if NAME is not defined.
See also the function `defun'."
  (or (and (fboundp name)
	   (not (get name 'defun-maybe)))
      (` (or (fboundp (quote (, name)))
	     (prog1
		 (defun (, name) (,@ everything-else))
	       ;; This `defun' will be compiled to `fset', which does
	       ;; not update `load-history'.
	       (setq current-load-list
		     (cons (quote (, name)) current-load-list))
	       (put (quote (, name)) 'defun-maybe t))))))

;; imported from emacs-19.34/lisp/subr.el.
(defun-maybe match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING.
\[Emacs 19.29 emulating function]"
  (if (match-beginning num)
      (if string
	  (substring string (match-beginning num) (match-end num))
	(buffer-substring (match-beginning num) (match-end num)))))

;; imported from emacs-21.2/lisp/subr.el
(defmacro with-syntax-table (table &rest body)
  "Evaluate BODY with syntax table of current buffer set to a copy of TABLE.
The syntax table of the current buffer is saved, BODY is evaluated, and the
saved table is restored, even in case of an abnormal exit.
Value is what BODY returns."
  (let ((old-table (make-symbol "table"))
	(old-buffer (make-symbol "buffer")))
    `(let ((,old-table (syntax-table))
	   (,old-buffer (current-buffer)))
       (unwind-protect
	   (progn
	     (set-syntax-table (copy-syntax-table ,table))
	     ,@body)
	 (save-current-buffer
	   (set-buffer ,old-buffer)
	   (set-syntax-table ,old-table))))))


(defun MkOrPattern (list)
  "LIST is a list of strings, and the result is a reg-exp pattern that
matches with any string in the LIST."
  ;; list must be nonempty
  (if (null (cdr list))
      ;; single element list
      (car list)
    (let ((list (cdr list)) (pat (car list)))
      (while list
	(setq pat (concat (car list) "\\|" pat))
	(setq list (cdr list)))
      pat)))

(defvar OpenExp nil)

(defvar CloseExp nil)

;; constants

(defconst PrimClass 
  '(var svar op sexp unit any dummy empty unitlist toklist)
  "primitive syntactic classes")

(defconst DefPrimExpPat
  "variable\\|constant\\|operator\\|closepar\\|openpar\\|postfix\\|regexp\\|define\\|infixL\\|infixR\\|commit\\|prefix\\|prefix-c\\|prefix-i\\|since\\|throw\\|catch\\|infix\\|infix-i\\|infix-ic\\|tex-infix\\|tex-infixL\\|tex-infixR\\|tex-prefix\\|tex-postfix\\|tex-paren\\|paren\\|eval\\|list\\|list\\+\\|tex-list\\|nil\\|and\\|abs\\|::=\\|or\\|(\\|<\\|{\\|\\[\\|《\\|)\\|>\\|}\\|\\]\\|》\\|,\\|;\\||\\|&"
  "PrimExpPat used in Game definitions")

(defconst DefaultVarExp
  "[a-zA-Z][a-zA-Z0-9_-]*"
  "default reg-exp for VarExp")

(defconst CommonConstExp
  "nil"
  "Regular expression for constants commonly used in any game.")

(defconst CommonOpExp
  (MkOrPattern
   '(
     "WILD_CARD_OP" ;; for internal use
     "DUMMY_CARD" ;; for internal use
     "WILD_CARD" ;; for internal use
     "since"
     ;;"triv"
     "def-E"
     "def-I"
     "conj"
     "by" 
     "id" 
     "in" 
     ":-"
     "├"
     "Π"  ;; for universal judgment
     "⇒"
     "#"
     "^"
     "::"
     ":"
     ","
     ";"
     "|"
     ))
  "Regular expression for operators commonly used in any game.")

(defconst CommonOpenExp
  (MkOrPattern
   (list (regexp-quote "(")
	 (regexp-quote "<")
	 (regexp-quote "{")
	 (regexp-quote "[")
	 (regexp-quote "《")))
  "Regular expression for open pars commonly used in any game.")

(defconst CommonCloseExp
  (MkOrPattern
   (list (regexp-quote ")")
	 (regexp-quote ">")
	 (regexp-quote "}")
	 (regexp-quote "]")
	 (regexp-quote "》")))
  "Regular expression for close pars commonly used in any game.")

(defconst CommentExp
  ";;"
  "Regular expression for comments.")

(defconst WhiteExp
  " \t\n"
  "Regular expression for white chars.")

;; macros for syntax definitions

(defun parser-cons-if-new (item list)
  "cons ITEM onto LIST if ITEM is not already in the LIST."
  (if (member item list)
      list
    (cons item list)))

(defmacro defclass (name rule)
  (list 'DefClass (list 'quote name) (list 'quote rule)))

(defun DefClass (name rule)
  (put name 'rule rule)
  (setq ClassNameList (parser-cons-if-new name ClassNameList)))

(defmacro def-non-nil (name)
  (list 'DefNonNil (list 'quote name)))

(defun DefNonNil (name)
  "declare NAME as a class that does not accept empty string."
  (put name 'non-nil t))

(defvar ClassNameList nil)

(defun ResetSyntaxRules ()
  (let ((l ClassNameList))
    (while l
      (put (car l) 'rule nil)
      (setq l (cdr l)))
    (setq ClassNameList nil)))

(defmacro deftoken (&rest list)
  (list 'DefToken (list 'quote list)))

(defun DefToken (list)
  (setq CommentExp nil
	WhiteExp nil
	VarExp nil
	SvarExp nil
	NumExp nil
	PrimExpList nil
	OpenExp nil
	CloseExp nil
	TokenTypes nil
	MatchingTokens nil
	MatchingParenList nil)
  (let ((l list) (opens nil) (closes nil)
	(open-list nil) (close-list nil))
    (ResetSyntaxRules)
    (while l
      (let* ((item (car l))
	     (token-type (car item))
	     (args (cdr item))
	     (exp (second item)))
	(cond ((eq token-type 'comment)
	       (setq CommentExp (regexp-quote exp)))
	      ((eq token-type 'white)
	       (setq WhiteExp exp))
	      ((eq token-type 'var)
	       (setq VarExp exp))
	      ((eq token-type 'svar)
	       (setq SvarExp exp))
	      ((eq token-type 'open)
	       (setq opens args)
	       (if closes (ComputeMatchingTokens opens closes))
	       (while args
		 (setq PrimExpList
		       (cons (list 'open (car args)) PrimExpList))
		 (setq open-list
		       (cons (list 'open (car args)) open-list))
		 (setq args (cdr args)))
	       (setq TokenTypes (cons token-type TokenTypes)))
	      ((eq token-type 'close)
	       (setq closes args)
	       (if opens (ComputeMatchingTokens opens closes))
	       (while args
		 (setq PrimExpList
		       (cons (list 'close (car args)) PrimExpList))
		 (setq close-list
		       (cons (list 'close (car args)) close-list))
		 (setq args (cdr args)))
	       (setq TokenTypes (cons token-type TokenTypes)))
	      ((eq token-type 'op)
	       (while args
		 (setq PrimExpList (cons (list 'op (car args)) PrimExpList))
		 (setq args (cdr args)))
	       (setq TokenTypes (cons token-type TokenTypes)))
	      (t
	       (setq PrimExpList (cons item PrimExpList)
		     TokenTypes (cons token-type TokenTypes))))
	(setq l (cdr l))))
    ;; now compute several constants; sort PrimExpList by the length
    ;; of keywords.
    (setq PrimExpList (sort PrimExpList 'ExpLt))
    (setq PrimExpPat (MkRegExpPattern PrimExpList))
    (if open-list (setq OpenExp (MkRegExpParenPattern open-list)))
    (if close-list (setq CloseExp (MkRegExpParenPattern close-list)))
    (while open-list
      (setq MatchingParenList
	    (cons (list (second (car open-list)) (second (car close-list)))
		  MatchingParenList))
      (setq open-list (cdr open-list)
	    close-list (cdr close-list)))
    ))

(defun ExpLt (x y)
  (< (length (second x)) (length (second y))))

(defun MkRegExpParenPattern (list)
  ;; list must be nonempty
  (if (null (cdr list))
      ;; single element list
      (regexp-quote (second (car list)))
    (let ((list (cdr list)) (pat (MkRegExp (second (car list)))))
      (while list
	(setq pat (concat (MkRegExp (second (car list))) "\\|" pat))
	(setq list (cdr list)))
      pat)))

(defun MkRegExp (str)
  "Regexp-quote STR if its length is 1, otherwise construct reg-exp that
matches STR as a word."
  (if (= (length str) 1)
      (regexp-quote str)
    (concat "\\b" str "\\b")))

(defun ComputeMatchingTokens (opens closes)
  (let ((o opens) (c closes))
    (while o
      (setq MatchingTokens
	    (cons (list (car o) (car c)) MatchingTokens))
      (setq o (cdr o) c (cdr c)))))

(defun MkRegExpPattern (list)
  ;; list must be nonempty
  (if (null (cdr list))
      ;; single element list
      (regexp-quote (second (car list)))
    (let ((list (cdr list)) (pat (regexp-quote (second (car list)))))
      (while list
	(setq pat (concat (regexp-quote (second (car list))) "\\|" pat))
	(setq list (cdr list)))
      pat)))

;; lexical analyzer

(defun ReadBufferToken ()
  (interactive)
  (ReadToken (point-min) (point-max) t))

;; grammar dependent.  should modify below to compute them
;; when reading the syntax definition file.
;; the lines below work for LOGIC.DEF

;;(setq OpenExp "\\((\\|<\\|{\\)")
;;(setq CloseExp "\\()\\|>\\|}\\)")

;; PrimExp のカテゴリーは open, close, op のいずれか．

(defconst UnitQuote "'"
  "The character used to quote a unit token.")

(defconst ElispStart
  (regexp-quote "$")
  "The character which marks the start point of an Emacs Lisp sexp which
follows the character.")

(defconst ElispDquote
  (regexp-quote "\"")
  "The character which marks the start point of an Emacs Lisp string.")

(defun parser-skip-white-and-comments ()
  (let ((p (point)) (cont t))
    ;; skip white chars and comments
    (while cont
      (if WhiteExp (skip-chars-forward WhiteExp))
      (if (and CommentExp (looking-at CommentExp))
	  (progn
	    (end-of-line)
	    (if (< (point) (point-max)) (forward-char 1))
	    ))
      (if (< p (point))
	  (setq p (point))
	(setq cont nil)))))

(defun ReadAToken (&optional pos-info)
  "read a single token. this function is called from ReadToken.
pos info added to OP's and VAR'S if POS-INFO is positively given."
  (let ((v nil))
    (let ((p (point)) (cont t))
      ;; skip white chars and comments
      (while cont
	(if WhiteExp (skip-chars-forward WhiteExp))
	(if (and CommentExp (looking-at CommentExp))
	    (progn
	      (end-of-line)
	      (if (< (point) (eobp)) (forward-char 1))
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
		 ;; we call ReadToken with POS-INFO nil.
		 (let ((len (length (MatchingParen open))))
		   (setq v 
			 (cons 'paren 
			       (cons open 
				     (ReadToken
				      p
				      (- (point) len)
				      pos-info))))
		   (forward-char len))
	       (error "対応する閉括弧がありません!"))))
	  ((and (stringp SvarExp) (posix-looking-at SvarExp))
	   (setq v 
		 (list 'svar 
		       ;; we chop off the first character, which
		       ;; is used only to make the expression
		       ;; a schematic variable.  (typically the
		       ;; character is ^ )
		       (buffer-substring (1+ (match-beginning 0))
					 (match-end 0))))
	   (goto-char (match-end 0)))
	  ((looking-at ElispStart)
	   (forward-char 1)
	   (let ((p (point)))
	     (with-syntax-table emacs-lisp-mode-syntax-table
	       (parser-forward-sexp)
	       (setq v
		     (list 'sexp 
			   (car
			    (read-from-string 
			     (buffer-substring-no-properties p (point)))))))))
	  ((looking-at ElispDquote)
	   (let ((p (point)))
	     (with-syntax-table emacs-lisp-mode-syntax-table
	       (parser-forward-sexp)
	       (setq v
		     (list 'sexp 
			   (car
			    (read-from-string 
			     (buffer-substring-no-properties p (point)))))))))
	  ((looking-at UnitQuote)
	   (forward-char 1)
	   (setq v (list 'unit (ReadAToken pos-info))))
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
			   (setq v 
				 (if pos-info 
				     (list 'var var-string (point))
				   (list 'var var-string)))
			   (goto-char var-string-end))
		       ;; in this case var-string and string must be
		       ;; the same string, since var-string cannot be
		       ;; shorter than string
		       ;; so, we have a reserved word here.
		       (if (looking-at CloseExp)
			   (error "対応する開括弧がありません!")
			 (setq v 
			       (if pos-info 
				   (list 'op string (point))
				 (list 'op string))))
		       (goto-char string-end))
		   ;; we have a variable here too!
		   (setq v 
			 (if pos-info
			     (list 'var var-string (point))
			   (list 'var var-string)))
		   (goto-char var-string-end))
	       ;; we should be looking at an op word here
	       (if string
		   ;; yes, we are looking at an op word
		   (progn
		     (if (looking-at CloseExp)
			 (error "対応する開括弧がありません!")
		       (setq v 
			     (if pos-info 
				 (list 'op string (point))
			       (list 'op string))))
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
	       ))))
    (if v 
	v 
      ;;(setq EP (point))
      (parse-fail "空列「」はトークンでありません" nil))))

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
		   (error "対応する開括弧がありません!"))))
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
			      (error "対応する開括弧がありません!")
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
			    (error "対応する開括弧がありません!")
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

;; extension of forward-sexp

(defun parser-forward-sexp (&optional open)
  "Using externnal OPEN, find matching CLOSE."
  (if (or (null open) (= (length open) 1))
      ;; use ordinary forward-sexp, but capture error and just return (point)
      (condition-case condition
	  (forward-sexp 1)
	(error
	 (point)))
    (let ((cont t) 
	  (count 0) 
	  (pat (format "\\b%s\\b\\|\\b%s\\b" open (MatchingParen open)))
	  (p (point))
	  (found nil))
      (while cont
	(if (re-search-forward pat nil t)
	    (let ((str (match-string 0)))
	      (goto-char (match-end 0))
	      (if (string= str open)
		  (setq count (1+ count))
		;; found the close
		(if (= count 1)
		    ;; ok, done!
		    (setq cont nil
			  found t)
		  (setq count (- count 1)))))
	  (setq cont nil)
	  ))
      (if found
	  (goto-char (match-end 0))
	(goto-char p)
	(if (and (boundp 'offset) offset)
	    (cal-set-epos (+ offset (1- (point)))
			  (+ offset (point)))
	  (cal-set-epos (1- (point)) (point)))
	(parse-fail (format "「%s」はトークンでありません!"
			    (char-to-string (following-char)))
		    nil)))))

(defun old-parser-forward-sexp (&optional open)
  "Using externnal OPEN, find matching CLOSE."
  (if (or (null open) (= (length open) 1))
      ;; use ordinary forward-sexp, but capture error and convert it
      ;; to fail
      (condition-case condition
	  (forward-sexp 1)
	(error
	 ;; must be scan error like:
	 ;; (scan-error "Unbalanced parentheses" 2151 3442)
	 (let ((p (third condition)))
	   (if (and (boundp 'offset) offset)
	       (cal-set-epos (+ offset (1- p))
			     (+ offset point))
	     (cal-set-epos (1- p) p))
	   (parse-fail (format "「%s」に対応する括弧がありません"
			       (char-to-string (following-char)))
		       nil))))
    (let ((cont t) 
	  (count 0) 
	  (pat (format "\\b%s\\b\\|\\b%s\\b" open (MatchingParen open)))
	  (p (point))
	  (found nil))
      (while cont
	(if (re-search-forward pat nil t)
	    (let ((str (match-string 0)))
	      (goto-char (match-end 0))
	      (if (string= str open)
		  (setq count (1+ count))
		;; found the close
		(if (= count 1)
		    ;; ok, done!
		    (setq cont nil
			  found t)
		  (setq count (- count 1)))))
	  (setq cont nil)
	  ))
      (if found
	  (goto-char (match-end 0))
	(goto-char p)
	(if (and (boundp 'offset) offset)
	    (cal-set-epos (+ offset (1- (point)))
			  (+ offset (point)))
	  (cal-set-epos (1- (point)) (point)))
	(parse-fail (format "「%s」はトークンでありません!"
			    (char-to-string (following-char)))
		    nil)))))


;; example
;; ((abc+23))
;; ;; UVW
;; abc
;;
;; ((open "(")
;;  (open "(")
;;  (var "abc")
;;  (op "+")
;;  (num "23")
;;  (close ")")
;;  (close ")")
;;  (var "abc"))

;; We deal with lists of token strings.
;; If the NetLength of LIST is 0, then return LIST as the value.

;; Otherwise, search the first close;

;; If found, then search the last open before point:
;;    if found, then check if the open-close pair matches:
;;         if matches, then make a paren of the pair and continue
;;         recursively.
;;         if does not match, then report error of non-matching pair
;;         of parentheses.
;;    if not found, then report error of no corresponding open for the close.

;; If the search fails, then search for an open.
;;    if found, then report error since this open does not have a
;;    matching close
;;    if not found, then return LIST.

;; (Enlist 2 5 '(0 1 (open "(") 3 4 (close ")") 6 7))
;; -> (0 1 (paren "(" 3 4) 6 7)

(defun Enlist (i j list)
  "items between i-th and j-th in the list is made into a paren list,
where i-th element is open and j-th element is close.
changes open to paren"
  (let ((k 0) (l list) (left nil) (center nil))
    (while (< k i)
      (setq left (append left (list (car l)))
	    l (cdr l)
	    k (1+ k)))
    (while (<  k j)
      (setq center (append center (list (car l)))
	    l (cdr l)
	    k (1+ k)))
    (append left
	    (cons (cons 'paren
			(cons (second (car center))
			      (cdr center)))
		  (cdr l)))))

;;(SearchType 'close '((open "(") (num "123") (close ")")))
;;(2 (close ")"))

(defun TypeOf (item) (car item))

(defun SearchType (type list)
  "Search for an element in the list whose type is TYPE.  If found returns
the index and item in the list"
  (let ((found nil) (l list) (k 0))
    (while (and (not found) l)
      (let ((item (car l)))
	(if (eq (TypeOf item) type)
	    (setq found (list k item))
	  (setq l (cdr l)
		k (1+ k)))))
    found))

;; (SearchLast 'open 2
;; 	       '((open "(") (open "{") (close "}") (close ")")
;; 	         (open "{") (close "}")))
;;
;; (1 (open "{"))

(defun SearchLast (type index list)
  "Search item of type TYPE before INDEX in LIST, if found returns the
index and item in the item-list."
  (let ((found nil) (l list) (k 0))
    (while (and (< k index) l)
      (let ((item (car l)))
	(if (eq (TypeOf item) type) (setq found (list k item)))
	(setq l (cdr l)
	      k (1+ k))))
    found))

(defun Parenp (token)
  (let ((type (TypeOf token)))
    (memq type '(open close))))
    
(defun NetLength (list)
  "the number of top-level open or closed tokens in the list."
  (let ((len 0) (l list))
    (while l
      (if (Parenp (car l)) (setq len (1+ len)))
      (setq l (cdr l)))
    len))

(defun MatchingPair (token1 token2)
  (member (list (second token1) (second token2)) MatchingTokens))

(defun MkTokenList (list)
  "convert simple-token-list into token list."
  (let ((v nil) (w nil))
    (if (= (NetLength list) 0) list
      (if (setq v (SearchType 'close list))
	  (if (setq w (SearchLast 'open (car v) list))
	      (if (MatchingPair (cadr w) (cadr v))
		  (MkTokenList (Enlist (car w) (car v) list))
		(error
		 "括弧の対 %s と %s は対応がとれていません!"
		 (second (cadr w)) (second (cadr v)))
		)
	    (error "対応する開括弧がありません!"))
	(if (setq w (SearchType 'open list))
	    (error "対応する閉括弧がありません!")
	  ;; no parenthesis
	  list)))))

(defun Lex (beg end)
  (MkTokenList (ReadToken beg end)))

;; parser part

(defun parse-fail (str token-list)
  "Throw error message STR together with TOKEN-LIST when
parsing failed for TOKEN-LIST."
  (throw 'fail (list 'fail str token-list)))

(defun parse-failed (result)
  "Check if the RESULT of parsing is a failure."
  (and (listp result) (eq (car result) 'fail)))

(defun parse-failed-str (result)
  (second result))

(defun parse-failed-toklist (result)
  (third result))

(defun parse-errorp (result)
  "Check if the RESULT of parsing is a failure."
  (and (listp result) (eq (car result) 'err)))

(defun parse-errorp-str (result)
  (third result))

(defun parse-errorp-toklist (result)
  (second result))


(defun parser-cut (list op)
  "Cut LIST by finding the first OP in LIST, where OP in
LIST may contain information on position."
  (let ((op-str (second op)) (left nil) (right list))
    (while (not (let ((token (car right)))
		  (and (eq 'op (first token))
		       (equal op-str (second token)))))
      (setq left (append left (list (car right))))
      (setq right (cdr right)))
    (list left (cdr right))))
  
(setq ParseL nil)

(defun Parse (token-list pattern)
  "Parse TOKEN-LIST against PATTERN.  TOKEN-LIST is globally bound to
TOKL."
  (if nf-debug (setq ParseL (cons (list token-list pattern) ParseL)))
  (if (stringp pattern)
      (setq pattern (list 'op pattern)))
  (if (and (atom pattern) (memq pattern PrimClass))
      (setq pattern (list pattern)))
  (if (atom pattern)
      ;; pattern is a defined class name
      (let (tree (pat (get pattern 'rule)))
	(if (null pat)
	    (error "「%s」は構文のクラスとしての定義がありません!" (symbol-name pattern))
	  (if (and (get pattern 'non-nil) (null token-list))
	      ;; PATTERN does not accept empty string!
	      (parse-fail (format "構文クラス「%s」は空列を受理しません!"
			    (symbol-name pattern))
		    token-list)
	  (Parse token-list pat))))
    (let ((category (car pattern)) (pat-list (cdr pattern)))
      (cond ((eq category 'op) (ParseToken token-list pattern))
	    ((eq category 'var) (ParseToken token-list pattern))
	    ((eq category 'svar) (ParseToken token-list pattern))
	    ((eq category 'num) (ParseToken token-list pattern))
	    ((eq category 'sexp) (ParseToken token-list pattern))
	    ((eq category 'unit) (ParseToken token-list pattern))
	    ((eq category 'any) (list 'string (TokenListToString token-list)))
	    ((eq category 'dummy) nil)
	    ((eq category 'toklist) (TokenListToToklist token-list))
	    ((eq category 'unitlist) (TokenListToUnitlist token-list))
	    ((eq category 'atom) (ParseToken token-list pattern))
	    ((eq category 'eval) (eval (car pat-list)))
	    ((eq category 'or) (ParseOr token-list pat-list))
	    ((eq category 'and) (ParseAnd token-list pat-list))
	    ((eq category 'empty) (ParseEmpty token-list))
	    ((eq category 'nonempty) (ParseNonEmpty token-list))
	    ((eq category 'paren) (ParseParen token-list pat-list))
	    ((eq category 'paren-n)
	     (ParseParen token-list (cdr pat-list) (car pat-list)))
	    ((eq category 'list) (ParseList token-list pat-list))
	    ((eq category 'list+) (ParseList token-list pat-list t))
	    ((eq category 'infix) (ParseInfix token-list pat-list))
	    ((eq category 'infixL) (ParseInfix token-list pat-list))
	    ((eq category 'infix-i) (ParseInfixI token-list pat-list))
	    ((eq category 'infix-c) (ParseInfixC token-list pat-list))
	    ((eq category 'infix-ic) (ParseInfixIC token-list pat-list))
	    ((eq category 'infixR) (ParseInfixLast token-list pat-list))
	    ((eq category 'postfix) (ParsePostfix token-list pat-list))
	    ((eq category 'prefix) (ParsePrefix token-list pat-list))
	    ((eq category 'prefix-i) (ParsePrefixI token-list pat-list))
	    ((eq category 'prefix-c) (ParsePrefixC token-list pat-list))
	    ((eq category 'conc) (ParseConc token-list pat-list))
	    ((eq category 'catch) (ParseCatch token-list pat-list))
	    ((eq category 'throw) (ParseThrow token-list pat-list))
	    ((eq category 'commit) (ParseCommit token-list pat-list))
	    ((eq category 'tex-paren) (ParseTeXParen token-list pat-list))
	    ((eq category 'tex-infix) (ParseTeXInfix token-list pat-list))
	    ((eq category 'tex-infixR) (ParseTeXInfixR token-list pat-list))
	    ((eq category 'tex-prefix) (ParseTeXPrefix token-list pat-list))
	    ((eq category 'tex-postfix) (ParseTeXPostfix token-list pat-list))
	    ((eq category 'tex-list) (ParseTeXList token-list pat-list))
	    (t (error "unknown category: %s" (symbol-name category)))
	    ))))

(defun ParseToken (token-list pat-list)
  ;; for debug
  ;;(setq TL token-list PL pat-list)
  (if (and (= (length token-list) 1)
	   (memq (first (first token-list)) '(svar var sexp unit op)))
      (let* ((item (first token-list))
	     ;; chop off position info if ITEM is an OP
	     (item-core
	      (if (eq 'op (car item))
		  (list 'op (second item))
		item))
	     (name (first item)))
	(if (= (length pat-list) 1)
	    (let ((token-name (first pat-list)))
	      (cond ((eq name token-name) item)
		    ((equal token-name 'svar)
		     (parse-fail
		      (format
		       "超変数が必要なところに超変数でない「%s」があります."
		       (second item))
		      nil))
		    ((equal token-name 'var)
		     (parse-fail
		      (format
		       "変数が必要なところに変数でない「%s」があります."
		       (second item))
		      nil))
		    ((equal token-name 'sexp)
		     (parse-fail
		      (format
		       "S式が必要なところにS式でない「%s」があります."
		       (second item))
		      nil))
		    ((equal token-name 'unit)
		     (parse-fail
		      (format
		       "ユニットが必要なところにユニットでない「%s」があります."
		       (second item))
		      nil))
		    ((equal token-name 'op)
		     (parse-fail
		      (format
		       "予約語が必要なところに予約語でない「%s」があります."
		       (second item))
		      nil))
		    (t (parse-fail (format "必要なトークン「%s」が見つかりません!"
				     (second pat-list))
			     token-list))))
	  ;; length of pat-list is 2 or 3 if OP
	  (if (eq (car pat-list) 'op)
	      (setq pat-list (list 'op (second pat-list))))
	  (if (equal item-core pat-list)
	      item
	    (parse-fail (format "必要なトークン「%s」が見つかりません!"
			  (second pat-list))
		  token-list))))
    (let ((token-name (first pat-list)))
      (cond ((eq token-name 'var) 
	     (parse-fail (format "「%s」は超変数ではありません!"
			   (TokenListToString token-list)) token-list))
	    ((eq token-name 'var) 
	     (parse-fail (format "「%s」は変数ではありません!"
			   (TokenListToString token-list)) token-list))
	    ((eq token-name 'sexp) 
	     (parse-fail (format "「%s」はS式ではありません!"
			   (TokenListToString token-list)) token-list))
	    ((eq token-name 'unit) 
	     (parse-fail (format "「%s」はユニットではありません!"
			   (TokenListToString token-list)) token-list))
	    ((eq token-name 'op) 
	     (parse-fail (format "「%s」は予約語ではありません!"
			   (TokenListToString token-list)) token-list))
	    (t (parse-fail (format "「%s」は必要なトークン「%s」と一致しません!" 
			     (TokenListToString token-list)
			     (second pat-list))
		     token-list))))))

(defun ParseNonEmpty (token-list)
  (if (null token-list)
      (parse-fail (format "「%s」は空列です!" (TokenListToString token-list)) nil)
    nil))

(defun ParseAnd (token-list pat-list)
  (let ((pat1 (car pat-list)) (pat2 (cadr pat-list)) TREE)
    (setq TREE (Parse token-list pat1))
    (Parse token-list pat2)))

(defun TokenListToToklist (list)
  "Convert TOKEN-LIST into list of tokens which corresponds to the
original word sequence which was parsed into TOKEN-LIST.  If the result
contains schematic variables, then we cons QQ to the result, yielding
a meta level object."
  (let ((result nil) (svar-free t) item)
    (while list
      (setq item (car list))
      (cond ((eq (car item) 'paren)
	     ;; in this case ITEM = (paren PAR-STR . TokenList)
	     (let* ((paren (cadr item)) (TL (cddr item))
		    (sublist (TokenListToToklist TL)))
	       (if (eq (car sublist) 'qq)
		   (setq svar-free nil
			 sublist (second sublist)))
	       (setq result 
		     (append 
		      result 
		      (list (list 'tok (list 'op paren)))
		      sublist
		      (list (list 'tok (list 'op (MatchingParen paren))))))))
	    ((eq (car item) 'svar)
	     (setq svar-free nil)
	     ;; keep schematic variable as it is
	     (setq result
		   (append result (list item))))
	    (t
	     (setq result
		   (append result (list (list 'tok item))))))
      (setq list (cdr list)))
    (if svar-free 
	result 
      (list 'qq result))))

(defun TokenListToUnitlist (list)
  "Convert TOKEN-LIST into list of unit exps which corresponds to the
original word sequence which was parsed into TOKEN-LIST.  If the result
contains schematic variables, then we cons QQ to the result, yielding
a meta level object."
  (let ((result nil) (svar-free t) item)
    (while list
      (setq item (car list))
      (cond ((eq (car item) 'paren)
	     (when svar-free (setq svar-free (parser-scan-svar (cddr item))))
	     (setq result
		   (append result (list (list 'unit item)))))
	    ((eq (car item) 'svar)
	     (setq svar-free nil)
	     ;; keep schematic variable as it is
	     (setq result
		   (append result (list item))))
	    (t
	     (setq result
		   (append result (list (list 'unit item))))))
      (setq list (cdr list)))
    (if svar-free 
	result 
      (list 'qq result))))

(defun parser-scan-svar (toklist)
  "scan TOKLIST and return T if it is svar free."
  (let ((result t) (cont t))
    (while (and toklist cont)
      (let ((item (car toklist)))
	(cond ((eq (car item) 'paren)
	       (if (parser-scan-svar (cddr item))
		   ;; ITEM is svar free, so continue to scan the rest of
		   ;; the list
		   (setq toklist (cdr toklist))
		 (setq result nil
		       cont nil)))
	      ((eq (car item) 'svar)
	       (setq result nil
		     cont nil))
	      ((eq (car item) 'unit)
	       (if (parser-scan-svar (cdr item))
		   ;; ITEM is svar free
		   (setq toklist (cdr toklist))
		 (setq result nil
		       cont nil)))
	      (t (setq toklist (cdr toklist))))))
    result))

(defun TokenListToString (list)
  (let ((str "") item)
    (while list
      (setq item (car list))
      (if (equal (car item) 'paren)
	  ;; in this case ITEM = (paren PAR-STR . TokenList)
	  (let ((paren (cadr item)) (TL (cddr item)))
	    (setq str (concat str " " paren (TokenListToString TL)
			      (MatchingParen paren))))
	(setq str 
	      (concat str " " 
		      (cond ((eq (car item) 'sexp) 
			     (format "$%s" (second item)))
			    ((eq (car item) 'unit)
			     (format "'%s" (TokenListToString (cdr item))))
			    (t (cadr item))))))
      (setq list (cdr list)))
    (if (string= str "")
	""
      (substring str 1))))

(defun MatchingParen (paren)
  (let ((v nil))
    (if (setq v (assoc paren MatchingParenList))
	(second v))))

(defun ParseEmpty (token-list)
  (if (null token-list)
      nil
    (parse-fail (format "「%s」は空列ではありません!" (TokenListToString token-list))
	  nil)))
  
(defun ParseCatch (token-list pat-list)
  ;; A typical form is: (catch (or ...)), so PAT-LIST is typically
  ;; ((or ...)), hence (car PAT-LIST) is (or ...)
  (let ((tree (catch 'err (Parse token-list (car pat-list)))))
    (if (and (listp tree) (eq (car tree) 'err))
	(parse-fail 
	 (format (third tree) (TokenListToString (second tree))) nil)
      tree)))
  
(defun ParseThrow (token-list pat-list)
  "Throw an error, where car of PAT-LIST should be a string."
  (throw 'err (list 'err token-list (car pat-list))))

(defun ParseCommit (token-list pat-list)
  "Catch FAIL and throw it as ERR"
  (let ((tree (catch 'fail (Parse token-list (car pat-list)))))
    (if (parse-failed tree)
	(throw 'err (list 'err token-list (parse-failed-str tree)))
      tree)))

(defun ParseOr (token-list pat-list)
  "Does what it says."
  (let (tree (cont t))
    (while (and pat-list cont)
      (let* ((pat (car pat-list)))
	(if (parse-failed (catch 'fail (setq tree (Parse token-list pat))))
	    ;; couldn't parse pat
	    (setq pat-list (cdr pat-list))
	  ;; parse succeeded!
	  (setq cont nil))))
    (if cont
	(parse-fail "適用できる構文解析の規則がありません" token-list)
      tree)))

(defun ConsIfNew (string list)
  (if (member string list)
      list
    (cons string list)))

(defun ParseParen (token-list pat-list &optional name)
  ;; here pat-list == (<string> <pat>) and token-list should be of
  ;; the form ((paren <string> . <token-list>))
  ;; if NAME, use it as the name of the operator.
  (let ((open (first pat-list))
	(pat (second pat-list))
	tree)
    (if (and (= (length token-list) 1)
	     (eq (car (car token-list)) 'paren)
	     (string= (second (first token-list)) open))
	(progn 
	  (setq tree (Parse (cdr (cdr (car token-list))) pat))
	  (if name (cons (list 'op name) tree) tree))
      (parse-fail
       (format "括弧「%s」が必要なところに「%s」があります"
	       open (second (first token-list)))
       token-list))))

(setq PS nil)

(defun ParseList (token-list pat-list &optional positive)
  "Parse TOKEN-LIST possibly containing the operator in PAT-LIST.
If POSITIVE, second element of pat-list is an operator, but operator
will be ignored and throwed away in the output tree."
  ;; pat-list == (<pat> {<op>})
  (if nf-debug (setq PS (cons (list token-list pat-list) PS)))
  (if (and positive (null token-list))
      (parse-fail "Empty token list cannot be a postitive list!" token-list))
  (let ((pat (first pat-list))
	(op (second pat-list))
	orig-op)
    (if (stringp op)
	(setq orig-op op
	      ;;op (list 'token 'op op)))
	      op (list 'op op)))
    (if (null op)
	;; in this case we parse each member of token-list
	(let ((v nil) tree)
	  (while token-list
	    (setq tree (Parse (list (car token-list)) pat)
		  v (cons tree v)
		  token-list (cdr token-list)))
	  (reverse v))
      ;; in this case we cut the token list by op
      (let ((v nil))
	(while token-list
	  (if (parser-op-member op token-list)
	      (let* ((cut-list (parser-cut token-list op))
		     (token-list1 (first cut-list))
		     (token-list2 (second cut-list)))
		(if (null token-list1)
		    (if orig-op
			(parse-fail
			 (format "区切文字「%s」が正しくないところにあります!"
				 orig-op) nil)
		    (parse-fail "punc operator should not be at the beginning!"
			  token-list)))
		(if (null token-list2)
		    (if orig-op
			(parse-fail
			 (format "区切文字「%s」が正しくないところにあります!"
				 orig-op) nil)
		      (parse-fail "punc operator should not be at the end!"
			    token-list)))
		(setq v (cons (Parse token-list1 pat) v)
		      token-list token-list2))
	    (setq v (cons (Parse token-list pat) v)
		  token-list nil)))
	(reverse v)))))

(setq POM nil)

(defun parser-op-member (op list)
  "check if OP is a member of token LIST."
  (if nf-debug (setq POM (cons (list op list) POM)))
  (let ((result nil) (cont t) (op-str (second op)))
    (while (and list cont)
      (let ((item (car list)))
	(if (eq (car item) 'op)
	    (if (equal op-str (second item))
		;; we found the OP.  note that this is the case even if
		;; ITEM contains info on position.
		(setq result t
		      cont nil)
	      (setq list (cdr list)))
	  (setq list (cdr list)))))
    result))

(defun PatSearch (pat token-list)
  "Try to match PAT against each element of TOKEN-LIST and returns a list
\(A B C\), where B is the parsed tree of first match, A is the list of
elements  before the match and C is the list of elements after the match.
Returns NIL if the match fails."
  (let (A B C (list token-list) (cont t))
    (while (and cont list)
      (let ((item (car list)) tree)
	(if (parse-failed (catch 'fail (setq tree (Parse (list item) pat))))
	    ;; didn't match, try the rest of list
	    (setq list (cdr list)
		  A (cons item A))
	  (setq cont nil
		B tree
		C (cdr list)))))
    (if cont nil (list (reverse A) B C))))

(defun PatSearchLast (pat token-list)
  "Try to match PAT against each element of TOKEN-LIST and returns a list
\(A B C\), where B is the parsed tree of the *last* match, A is the list of
elements  before the match and C is the list of elements after the match.
Returns NIL if the match fails."
  (let (A head B C (list token-list) (found nil))
    (while list
      (let ((item (car list)) tree)
	(if (parse-failed (catch 'fail (setq tree (Parse (list item) pat))))
	    ;; didn't match, try the rest of list
	    (setq list (cdr list)
		  head (cons item head))
	  (setq found t
		A head
		B tree
		C (cdr list)
		head (cons item head)
		list (cdr list)))))
    (if found (list (reverse A) B C) nil)))

(defun ParseInfix (token-list pat-list)
  "Match TOKEN-LIST against PAT-LIST, which is of the form
\(<pat> <unit> <pat>\).  <unit> will be moved to the front of the list."
  (let ((pat1 (first pat-list))
	(unit (second pat-list))
	(pat3 (third pat-list))
	triple
	(first (list (first token-list)))
	(last (list (exp-list-get-last token-list))))
    (if (setq triple (PatSearch unit token-list))
	(let* ((tree2 (second triple))
	       (token-list1 (first triple))
	       (token-list3 (third triple))
	       (tree1 (Parse token-list1 pat1))
	       (tree3 (Parse token-list3 pat3)))
	  (list tree2 tree1 tree3))
      (parse-fail
       (format "中置演算子「%s」がありません!" unit)
       token-list))))

(defun ParseInfixI (token-list pat-list)
  "Match TOKEN-LIST against PAT-LIST, which is of the form
\(<pat> <unit> <pat>\). Ignores and throws away tree2"
  (let ((pat1 (first pat-list))
	(unit (second pat-list))
	(pat3 (third pat-list))
	triple
	(first (list (first token-list)))
	(last (list (exp-list-get-last token-list))))
    (if (setq triple (PatSearch unit token-list))
	(let* (
	       (token-list1 (first triple))
	       (token-list3 (third triple))
	       (tree1 (Parse token-list1 pat1))
	       (tree3 (Parse token-list3 pat3)))
	  (list tree1 tree3))
      (parse-fail (format "演算子「%s」がありません!" unit) token-list))))

(defun ParseInfixC (token-list pat-list)
  "Match TOKEN-LIST against PAT-LIST, which is of the form
\(<pat> <unit> <pat>\). Use Cons instead of list."
  (let ((pat1 (first pat-list))
	(unit (second pat-list))
	(pat3 (third pat-list))
	triple
	(first (list (first token-list)))
	(last (list (exp-list-get-last token-list))))
    (if (setq triple (PatSearch unit token-list))
	(let* ((tree2 (second triple))
	       (token-list1 (first triple))
	       (token-list3 (third triple))
	       (tree1 (Parse token-list1 pat1))
	       (tree3 (Parse token-list3 pat3)))
	  (cons tree2 (cons tree1 tree3)))
      (parse-fail (format "演算子「%s」がありません!" unit) nil))))

(defun ParseInfixIC (token-list pat-list)
  "Match TOKEN-LIST against PAT-LIST, which is of the form
\(<pat> <unit> <pat>\). Igonore and throws away tree2,
and use Cons instead of list."
  (let ((pat1 (first pat-list))
	(unit (second pat-list))
	(pat3 (third pat-list))
	triple
	(first (list (first token-list)))
	(last (list (exp-list-get-last token-list))))
    (if (setq triple (PatSearch unit token-list))
	(let* (
	       (token-list1 (first triple))
	       (token-list3 (third triple))
	       (tree1 (Parse token-list1 pat1))
	       (tree3 (Parse token-list3 pat3)))
	  (cons tree1 tree3))
      (parse-fail (format "演算子「%s」がありません!" unit) nil))))

(defun ParseInfixLast (token-list pat-list)
  "Match TOKEN-LIST against PAT-LIST, which is of the form
\(<pat> <unit> <pat>\) where <unit> is the *last* match."
  (let ((pat1 (first pat-list))
	(unit (second pat-list))
	(pat3 (third pat-list))
	triple
	(first (list (first token-list)))
	(last (list (exp-list-get-last token-list))))
    (if (setq triple (PatSearchLast unit token-list))
	(let* ((tree2 (second triple))
	       (token-list1 (first triple))
	       (token-list3 (third triple))
	       (tree1 (Parse token-list1 pat1))
	       (tree3 (Parse token-list3 pat3)))
	  (list tree2 tree1 tree3))
      (parse-fail (format "演算子「%s」がありません!" unit) nil))))

(defun SearchInfix (pat token-list)
  "Search the leftmost unit that matches PAT in TOKEN-LIST."
  (let (head center tail (cont t))
    (while (and cont token-list)
      (if (parse-failed (catch 'fail (Parse (list (car token-list)) pat)))
	  (setq cont nil)
	(setq head (cons (car token-list) head)
	      token-list (cdr token-list))))
    (setq token-list (reverse token-list)
	  cont t)
    (while (and cont token-list)
      (if (parse-failed (catch 'fail (Parse (list (car token-list)) pat)))
	  (setq cont nil)
	(setq tail (cons (car token-list) tail)
	      token-list (cdr token-list))))
    (setq center (reverse token-list)
	  tail (reverse tail)
	  cont t)
    (while (and cont center)
      (let ((item (car center)) tree)
	(if (parse-failed (catch 'fail
		       (setq tree (Parse (list (car center)) pat))))
	    (setq head (cons item head)
		  center (cdr center))
	  (setq cont nil
		tail (append (cdr center) tail)
		center tree
		))))
    (if cont
	;; search failed, so return nil
	nil
      (list (reverse head) center tail))))

(defun ParsePrefix (token-list pat-list)
  "Parse TOKEN-LIST containing the prefix operator in PAT-LIST,
which must be of the form \(<unit> <pat>\)."
  (let ((unit (first pat-list))
	(pat (second pat-list)))
    (if (null token-list)
	(parse-fail "cannot parse an empty token list as prefix" token-list))
    (if (null unit)
	(parse-fail "prefix must be non-nil" token-list))
    (let ((tree1 (Parse (list (car token-list)) unit))
	  (tree2 (Parse (cdr token-list) pat)))
      (list tree1 tree2))))

(defun ParsePrefixI (token-list pat-list)
  "Parse TOKEN-LIST containing the prefix operator in PAT-LIST,
which must be of the form \(<unit> <pat>\).  ignores prefix"
  (let ((unit (first pat-list))
	(pat (second pat-list)))
    (if (null token-list)
	(parse-fail "cannot parse an empty token list as prefix" token-list))
    (if (null unit)
	(parse-fail "prefix must be non-nil" token-list))
    (let ((tree1 (Parse (list (car token-list)) unit))
	  (tree2 (Parse (cdr token-list) pat)))
      tree2)))

(defun ParsePrefixC (token-list pat-list)
  "Parse TOKEN-LIST containing the prefix operator in PAT-LIST,
which must be of the form \(<unit> <pat>\).  The result will be
given by Consing two trees rather than making a list of two
trees."
  (let ((unit (first pat-list))
	(pat (second pat-list)))
    (if (null token-list)
	(parse-fail "cannot parse an empty token list as prefix" token-list))
    (if (null unit)
	(parse-fail "prefix must be non-nil" token-list))
    (let ((tree1 (Parse (list (car token-list)) unit))
	  (tree2 (Parse (cdr token-list) pat)))
      (cons tree1 tree2))))

(defun ParsePostfix (token-list pat-list &optional name)
  "Parse TOKEN-LIST containing the postfix operator in PAT-LIST,
which must be of the form \(<pat> <unit>\)."
  (let ((pat (first pat-list))
	(unit (second pat-list))
	(list token-list)
	(cont t)
	pre
	post)
    (if (null token-list)
	(parse-fail "cannot parse an empty token list as postfix" token-list)
      (while (and cont list)
	(if (null (cdr list))
	    (setq cont nil
		  post list)
	  (setq pre (cons (car list) pre)
		list (cdr list))))
      (setq pre (reverse pre)))
    (if (and (listp unit) (eq (car unit) 'paren))
	;; we check if POST has necessary parenthesis
	(or (and (eq (first (car post)) 'paren)
		 (equal (second (car post)) (second unit)))
	    (parse-fail (format "必要な括弧「%s」がありません!" (second unit))
		  token-list)))
    (let (;; parse TREE2 first
	  (tree2 (Parse post unit))
	  (tree1 (Parse pre pat)))
      (if name (list (list 'op name) tree1 tree2)
	(list tree1 tree2)))))

(defun ParseConc (token-list pat-list &optional name)
  "Match TOKEN-LIST against PAT-LIST, which is of the form
\(<pat> <pat>\).  NAME is used as the name of the operator."
  (if (null token-list)
      (parse-fail "couldn't parse empty token-list as conc!" token-list)
    (let ((pat1 (first pat-list))
	  (pat2 (second pat-list))
	  (head (list (first token-list)))
	  (tail (cdr token-list))
	  (cont t)
	  tree1
	  tree2)
      (while (and cont tail)
	(if (parse-failed (catch 'fail (setq tree1 (Parse head pat1))))
	    ;; couldn't parse head
	    (progn
	      (setq head (append head (list (car tail)))
		    tail (cdr tail)))
	  (if (parse-failed (catch 'fail (setq tree2 (Parse tail pat2))))
	      ;; couldn't parse tail
	      (progn
		(setq head (append head (list (car tail)))
		      tail (cdr tail)))
	    ;; successfully parsed!
	    (setq cont nil))))
      (if cont
	  (parse-fail "couldn't parse as conc!" token-list)
	(if name
	    (list (list 'op name) tree1 tree2)
	  (list tree1 tree2))))))

;; TeX related parsing functions.

(defconst parser-tex-default-skip
  "1pt")

(defconst parser-tex-skip-alist
  '(("," "0pt" "2pt")
    (";" "0pt" "2pt")
    (":" "2pt" "2pt")
    ("::" "0pt" "0pt")
    (":-" "3pt" "3pt")
    ("├" "3pt" "3pt")
    ("∧" "2pt" "2pt")
    ("∨" "2pt" "2pt")
    ("⊃" "3pt" "3pt")
    ("⇒" "3pt" "3pt")
    ("→" "2pt" "2pt")
    ("≡" "3pt" "3pt")
    ("=" "3pt" "3pt")
    ("λ" "0pt" "0pt")
    ("∈" "2pt" "2pt")
    ))

(defconst parser-tex-symbol-alist
  '(("├" "\\proves")
    ("⊥" "\\false")
    ("∧" "\\conj")
    ("∨" "\\disj")
    ("¬" "\\neg")
    ("⊃" "\\imp")
    ("⇒" "\\Rightarrow")
    ("Π" "\\Pi")
    ("Σ" "\\Sigma")
    ("→" "\\rightarrow")
    ("∀" "\\forall")
    ("∃" "\\exists")
    ("≡" "\\equiv")
    ("λ" "\\lambda")
    ("∈" "\\in")
    ))

(defun parser-keyword-p (op)
  (let ((char (string-to-char (substring op 0 1))))
    (or (and (<= ?a char) (<= char ?z))
	(and (<= ?A char) (<= char ?Z)))))

(defun parser-tex-skip (op)
  (let* ((v (or (assoc op parser-tex-skip-alist)
		(if (parser-keyword-p op) '(nil "4pt" "4pt"))))
	 (lskip (or (second v) parser-tex-default-skip))
	 (rskip (or (third v) parser-tex-default-skip)) 
	 (w (assoc op parser-tex-symbol-alist)))
    (if w 
	(concat "{\\hskip " lskip "}{" (second w) "}{\\hskip " rskip "}") 
      (concat "{\\hskip " lskip "}\\mytt{" op "}{\\hskip " rskip "}"))))

(defun parser-op-to-tex (tree)
  (if (and (listp tree) (eq 'op (first tree)))
      (let* ((op (second tree))
	     (w (assoc op parser-tex-symbol-alist)))
	(if w 
	    (list 'sexp (concat "{" (second w) "}"))
	  (list 'sexp (concat "\\mytt{" (second tree) "}"))))
    tree))

(defun ParseTeXInfix (token-list pat-list)
  "Match TOKEN-LIST against PAT-LIST, which is of the form
\(<pat> <unit> <pat>\)."
  (let ((pat1 (first pat-list))
	(unit (second pat-list))
	(pat3 (third pat-list))
	triple
	(first (list (first token-list)))
	(last (list (exp-list-get-last token-list))))
    (if (setq triple (PatSearch unit token-list))
	(let* ((tree2 (second triple))
	       (token-list1 (first triple))
	       (token-list3 (third triple))
	       (tree1 (parser-op-to-tex (Parse token-list1 pat1)))
	       (tree3 (parser-op-to-tex (Parse token-list3 pat3))))
	  (list 'sexp (concat (second tree1) (parser-tex-skip (second tree2))
			      (second tree3))))
      (parse-fail
       (format "中置演算子「%s」がありません!" unit)
       token-list))))

(defun ParseTeXInfixR (token-list pat-list)
  "Match TOKEN-LIST against PAT-LIST, which is of the form
\(<pat> <unit> <pat>\) where <unit> is the Right-most match."
  (let ((pat1 (first pat-list))
	(unit (second pat-list))
	(pat3 (third pat-list))
	triple
	(first (list (first token-list)))
	(last (list (exp-list-get-last token-list))))
    (if (setq triple (PatSearchLast unit token-list))
	(let* ((tree2 (second triple))
	       (token-list1 (first triple))
	       (token-list3 (third triple))
	       (tree1 (parser-op-to-tex (Parse token-list1 pat1)))
	       (tree3 (parser-op-to-tex (Parse token-list3 pat3))))
	  (list 'sexp (concat (second tree1) (parser-tex-skip (second tree2))
			      (second tree3))))
      (parse-fail (format "演算子「%s」がありません!" unit) nil))))

(defun ParseTeXPrefix (token-list pat-list)
  "Parse TOKEN-LIST containing the prefix operator in PAT-LIST,
which must be of the form \(<unit> <pat>\)."
  (let ((unit (first pat-list))
	(pat (second pat-list)))
    (if (null token-list)
	(parse-fail "cannot parse an empty token list as prefix" token-list))
    (if (null unit)
	(parse-fail "prefix must be non-nil" token-list))
    (let ((tree1 (Parse (list (car token-list)) unit))
	  (tree2 (Parse (cdr token-list) pat)))
      (list 'sexp (concat (parser-tex-op (second tree1)) (second tree2))))))

(defun parser-tex-op (op) 
  (let* ((w (assoc op parser-tex-symbol-alist))) 
    (if w (concat "{" (second w) "}") op)))

(defun ParseTeXPostfix (token-list pat-list)
  "Parse TOKEN-LIST containing the postfix operator in PAT-LIST,
which must be of the form \(<pat> <unit>\)."
  (let ((pat (first pat-list))
	(unit (second pat-list))
	(list token-list)
	(cont t)
	pre
	post)
    (if (null token-list)
	(parse-fail "cannot parse an empty token list as postfix" token-list)
      (while (and cont list)
	(if (null (cdr list))
	    (setq cont nil
		  post list)
	  (setq pre (cons (car list) pre)
		list (cdr list))))
      (setq pre (reverse pre)))
    (if (and (listp unit) (eq (car unit) 'paren))
	;; we check if POST has necessary parenthesis
	(or (and (eq (first (car post)) 'paren)
		 (equal (second (car post)) (second unit)))
	    (parse-fail (format "必要な括弧「%s」がありません!" (second unit))
		  token-list)))
    (let (;; parse TREE2 first
	  (tree2 (Parse post unit))
	  (tree1 (Parse pre pat)))
      (list 'sexp (concat (second tree1) (parser-tex-op (second tree2)))))))

(defun ParseTeXParen (token-list pat-list)
  ;; here pat-list == (<string> <pat>) and token-list should be of
  ;; the form ((paren <string> . <token-list>))
  (let* ((open (first pat-list))
	 (close (MatchingParen open))
	 (pat (second pat-list)))
    (if (and (= (length token-list) 1)
	     (eq (car (car token-list)) 'paren)
	     (string= (second (first token-list)) open))
	(list 'sexp (concat (parser-tex-tt open)
			    (second (Parse (cdr (cdr (car token-list))) pat))
			    (parser-tex-tt close)))
      (parse-fail
       (format "括弧「%s」が必要なところに「%s」があります"
	       open (second (first token-list)))
       token-list))))

(defun parser-tex-tt (str)
  (cond ((string= str "{") "\\mytt{\\leftbrace}")
	((string= str "}") "\\mytt{\\rightbrace}")
	(t (concat "\\mytt{" str "}"))))

(defun ParseTeXList (token-list pat-list &optional positive)
  "Parse TOKEN-LIST possibly containing the operator in PAT-LIST.
If POSITIVE, second element of pat-list is an operator, but operator
will be ignored and throwed away in the output tree."
  ;; pat-list == (<pat> {<op>})
  (if nf-debug (setq PS (cons (list token-list pat-list) PS)))
  (if (and positive (null token-list))
      (parse-fail "Empty token list cannot be a postitive list!" token-list))
  (let ((pat (first pat-list))
	(op (second pat-list))
	orig-op)
    (if (stringp op)
	(setq orig-op op
	      ;;op (list 'token 'op op)))
	      op (list 'op op)))
    (if (null op)
	;; in this case we parse each member of token-list
	(let ((v "") tree)
	  (while token-list
	    (setq tree (Parse (list (car token-list)) pat)
		  v (concat v (second tree))
		  token-list (cdr token-list)))
	  (list 'sexp v))
      ;; in this case we cut the token list by op
      (let ((v ""))
	(while token-list
	  (if (parser-op-member op token-list)
	      (let* ((cut-list (parser-cut token-list op))
		     (token-list1 (first cut-list))
		     (token-list2 (second cut-list)))
		(if (null token-list1)
		    (if orig-op
			(parse-fail
			 (format "区切文字「%s」が正しくないところにあります!"
				 orig-op) nil)
		    (parse-fail "punc operator should not be at the beginning!"
			  token-list)))
		(if (null token-list2)
		    (if orig-op
			(parse-fail
			 (format "区切文字「%s」が正しくないところにあります!"
				 orig-op) nil)
		      (parse-fail "punc operator should not be at the end!"
			    token-list)))
		(setq v 
		      (if (string= v "")
			  (second (parser-op-to-tex (Parse token-list1 pat)))
			(concat v 
				(parser-tex-skip orig-op)
				(second 
				 (parser-op-to-tex (Parse token-list1 pat)))))
		      token-list token-list2))
	    (setq v 
		  (if (string= v "")
		      (second (parser-op-to-tex (Parse token-list pat)))
		    (concat v
			    (parser-tex-skip orig-op)
			    (second 
			     (parser-op-to-tex (Parse token-list pat)))))
		  token-list nil)))
	(list 'sexp v)))))

(defun ParseBuffer ()
  (interactive)
  (let* ((token-list (LexBuffer))
	 (v (Parse token-list ClassName)))
    (goto-char (point-max))
    (insert "\n;; ")
    ;;(PrintTree v)
    (insert (prin1-to-string v))
    (insert "\n")
    v
    ))

(defun LexString (string)
  (set-buffer (get-buffer-create " *parse-buffer*"))
  (set-syntax-table parser-syntax-table)
  (erase-buffer)
  (insert string)
  (LexBuffer))

;; set parser syntax table
(setq parser-syntax-table (make-syntax-table))
(modify-syntax-entry ?_ "w   " parser-syntax-table)
(modify-syntax-entry ?  "    " parser-syntax-table)
(modify-syntax-entry ?\t "    " parser-syntax-table)
(modify-syntax-entry ?\n ">   " parser-syntax-table)
(modify-syntax-entry ?\^m ">   " parser-syntax-table)
(modify-syntax-entry ?\( "()  " parser-syntax-table)
(modify-syntax-entry ?\) ")(  " parser-syntax-table)
(modify-syntax-entry ?\{ "(}  " parser-syntax-table)
(modify-syntax-entry ?\} "){  " parser-syntax-table)
(modify-syntax-entry ?\[ "(]  " parser-syntax-table)
(modify-syntax-entry ?\] ")[  " parser-syntax-table)
(modify-syntax-entry ?\< "(>  " parser-syntax-table)
(modify-syntax-entry ?\> ")<  " parser-syntax-table)
(modify-syntax-entry ?《 "(《  " parser-syntax-table)
(modify-syntax-entry ?》 ")》  " parser-syntax-table)
(modify-syntax-entry ?\; ". 12" parser-syntax-table)

(defun ReadString (string)
  (set-buffer (get-buffer-create " *parse-buffer*"))
  (set-syntax-table parser-syntax-table)
  (erase-buffer)
  (insert string)
  (ReadToken (point-min) (point-max) t))

(defun ParseString (string pattern &optional offset)
  "Parse STRING according to the PATTERN.  The result, if successful,
is a valid NF expression or an NF meta expression.  If optional
OFFSET, which is an integer, is given, it is added to error positions."
  (save-excursion
    (let* ((result (catch 'fail
		    (catch 'err
		      (Parse (ReadString string) pattern)))))
      ;;(setq R result STR string P pattern)
      ;; dynamically scoped variable.  set META-VARS to nil only when
      ;; it is unbound
      ;; (or (boundp 'META-VARS) (setq META-VARS nil))
      (cond ((parse-failed result)
	     (let ((toklist (parse-failed-toklist result)) r)
	       (when (and toklist (setq r (cal-toklist-region toklist)))
		 (let ((region (cal-string-closure string r)))
		   (cal-set-epos 
		    (+ (first region) (or offset 0))
		    (+ (second region) (or offset 0)))))
	       (error (parse-failed-str result))))
	    ((parse-errorp result)
	     (let ((toklist (parse-errorp-toklist result)) r)
	       (when (and toklist (setq r (cal-toklist-region toklist)))
		 (let ((region (cal-string-closure string r)))
		   (cal-set-epos 
		    (+ (first region) (or offset 0))
		    (+ (second region) (or offset 0)))))
	       (error (parse-errorp-str result)
		      ;; we must call cal-quote-percent
		      (parser-quote-percent
		       (TokenListToString toklist)))))
	    (t 
	     (when (eq 'derivation pattern) (setq cal-deriv-str string))
	      result)))))

(defun parser-quote-percent (str)
  "Convert %s in STR into %%s, to avoid getting [not enough arguments
for format string] error."
  (let (newstr)
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (search-forward "%" nil t)
	(if (= ?% (following-char))
	    ;; % is quoted, so just skip it
	    (forward-char 1)
	  ;; we must quote it
	  (insert "%")
	  (forward-char 1)))
      (setq newstr (buffer-substring (point-min) (point-max))))
    newstr))

(defun ParseOk (string class-name)
  "parse STRING as CLASS-NAME. returns the parse tree if successfully parsed,
returns nil otherwise."
  (interactive)
  (let ((v (catch 'fail
	     (ParseString string class-name))))
    (if (parse-failed v)
	nil
      v)))

(provide 'parser)

