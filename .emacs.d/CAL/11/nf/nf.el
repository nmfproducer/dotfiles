;; !!! This file should always be loaded indirectly by calling 
;; from cal.el

;; NF version 0.14 of Mon Nov 17 08:54:15 2003
;; NF version 0.13 of Thu Jan  2 22:42:10 2003
;; NF version 0.12 of Wed Jan  1 16:20:28 2003
;; NF version 0.11 of Sat Dec 28 14:12:40 2002
;; NF version 0.10 of Thu Dec 12 22:04:38 2002
;; NF version 0.9 of Sun Dec  1 21:26:18 2002
;; NF version 0.8 of Tue Nov 26 00:10:27 2002
;; NF version 0.7 of Fri Nov  8 23:35:00 2002
;; NF version 0.6 of Sun Oct 13 18:22:07 2002
;; NF version 0.5 of Mon Oct  7 14:14:07 2002
;; moved some obsolete functions to nf-obsolete.el
;; NF version 0.4 of Sun Sep  1 23:58:04 2002
;; NF version 0.3 of Mon Jul  1 23:12:54 2002
;; nf-assoc: unified with exp-varref-assoc, which is now obsolete
;; snoc: removed all snoc related codes
;; NF version 0.2 of Sun Jun 30 21:05:46 2002
;; NF version 0.1 of Fri Jun 28 23:56:40 2002

;; Removal Candidate
;; (defvar nf-name-of-the-game "")
;; (defvar nf-def-dir "~/system/nf/def/")
;; (defvar nf-basic-exp-list nil)

;; list of NF game names.  each item of the list is a string representing
;; a name of NF game.
;;(defvar nf-game-list nil)

;; (setq nf-basic-exp-list PrimExpList)

;; parser related functions

;; PrimExpList に (op "NAME") を追加する．NAME の長さに応じて適当な
;; 位置に挿入して追加する．

;; Removal Candidate
;; modified from cal.el
;;(defun nf-extend-prim-exp-list (name)
  ;;(or (member (list 'op name) PrimExpList)
      ;;(setq PrimExpList (cal-insert-by-length name nil PrimExpList) 
	    ;;PrimExpPat (MkRegExpPattern (sort PrimExpList 'ExpLt)))))

;;(defun cal-insert-by-length (name list1 list2)
  ;;(if (null list2)
      ;;;; append item to list1
      ;;(append list1 (list (list 'op name)))
    ;;(if (<= (length name) (length (second (car list2))))
	;;(append list1 (list (list 'op name)) list2)
      ;;(cal-insert-by-length 
       ;;name 
       ;;(append list1 (list (car list2)))
       ;;(cdr list2)))))

;;(defmacro defop (&rest list)
  ;;(list 'DefOp (list 'quote list)))

;;(defun DefOp (list)
  ;;;; first reset PrimExpList
  ;;(setq PrimExpList nf-basic-exp-list)
  ;;(while list
    ;;(let ((op (car list)))
      ;;(nf-extend-prim-exp-list op)
      ;;(setq list (cdr list)))))
;; End of Removal Candidate

;;;;;;;;;;;;;;;;
;; match 変数 ;;
;;;;;;;;;;;;;;;;

(defun exp-svar-p (exp)
  (and (listp exp)
       (eq (length exp) 2) 
       (eq (1st exp) 'svar)))

;; <- 文字列

(defun exp-svar-cons (str)
  (list 'svar str))

;; -> 文字列 (文字列のリストではない！）
(defun exp-svar-dest (var)
  (2nd var))

;;;;;;;;;;
;; 導出 ;; (derivation)
;;;;;;;;;;

;; derivation == ((├ context concl) game deriv)

;; 導出 -> context

(defun exp-derivation-context (derivation)
  (second (first derivation)))

;; 導出 -> concl

(defun exp-derivation-concl (derivation)
  (third (first derivation)))

;; 導出 -> game の名前 (文字列)

(defun exp-derivation-game (derivation)
  (second (second derivation)))

;; 導出 -> deriv

(defun exp-derivation-deriv (derivation)
  (third derivation))

;; (CD conc (rule-name . rule-args) subderivs)
;; deriv == (CD conc (rule-name . rule-args) subderivs)
;;        | (HD hyp (Abs var map body))
;;        | (Abs var map body)

(defun exp-CD-p (deriv)
  (and (listp deriv) (nf-eq (first deriv) exp-CD)))

(defun exp-CD-concl (deriv)
  (second deriv))

(defun exp-CD-rule-name (deriv)
  (let ((rule (third deriv)))
    (if (exp-const-p rule)
	(second rule)
      (second (first rule)))))

(defun exp-CD-rule-pos (deriv)
  (let ((rule (third deriv)))
    (if (exp-const-p rule) 
	;; rule without explicit parameters
	(third rule)
      ;; rule with parameters, i.e., (rule param-list)
      (third (first rule)))))

(defun exp-CD-rule-args (deriv)
  (let ((rule (third deriv)))
    (if (exp-const-p rule)
	nil
      (second rule))))

(defun exp-CD-subderivs (deriv)
  (fourth deriv))

;; (HD hyp (Abs (var) map body))

(defun exp-HD-p (deriv)
  (and (listp deriv) (nf-eq (first deriv) exp-HD)))

(defun exp-HD-hyp (deriv)
  (cons (car (second (third deriv))) (second deriv)))

(defun exp-HD-hypj (deriv)
  (second deriv))

(defun exp-HD-var (deriv)
  (car (second (third deriv))))

(defun exp-HD-map (deriv)
  (third (third deriv)))

(defun exp-HD-body (deriv)
  (fourth (third deriv)))

;; univarsal derivation: (Abs (var) map body)

(defun exp-UD-p (deriv)
  (and (listp deriv) (eq (first deriv) 'Abs)))

(defun exp-rule-params (rule)
  (first rule))

(defun exp-rule-conc (rule)
  (second rule))

;;;;;;;;;;;;;;
;; 導出仮定 ;;
;;;;;;;;;;;;;;

;; 証明変数宣言 -> 判断

(defun exp-dhyp-judg (tree) (cdr tree))

;; 証明変数宣言 -> 変数

(defun exp-dhyp-var (tree) (car tree))

(defun exp-add-sharp (v)
  (if (exp-var-p v)
      (list 'varref "#" (exp-var-dest v))
    (cons 'varref (cons "#" (cdr v)))))

;; dummy variable representing abstracted position
(setq dt-dummy-var '(var 0))

(setq one 1)

(defun oneP (x)
  (eq x 1))

(defun constP (x)
  (or
   (memq x '(Abs CD HD))
   (and (listp x) (eq (car x) 'op))))

(defun dt-map-atom-p (map)
  "check if MAP is an atomic position for an argument."
  (integerp map))

;;(defun exp-to-String (exp class)
  ;;(let ((context nil))
    ;;(exp-to-string exp class)))

(defun exp-to-string (exp class)
  (cond ((eq class 'var) (exp-to-string-var exp))
	((eq class 'varlist) (exp-to-string-varlist exp))
	((eq class 'varref) (exp-to-string-varref exp))
	((eq class 'op) (exp-to-string-const exp))
	((eq class 'Abs) (exp-to-string-Abs exp))
	((eq class 'exp) (exp-to-string-exp exp))
	((eq class 'prop-form) (exp-to-string-prop-form exp))
	((eq class 'arith-prop) (exp-to-string-arith-prop exp))
	((eq class 'PropProof-deriv) (exp-to-string-PropProof-deriv exp))
	((eq class 'lambda-term) (exp-to-string-lambda-term exp))
	(t "Sorry!")))

(defun exp-to-string-Abs (exp)
  (if (exp-Abs-p exp)
      (let* ((list (exp-Abs-dest exp))
	     (var (1st list))
	     (map (2nd list))
	     (body (3rd list)))
	;;(setq context (cons var context))
	(concat "(" (exp-to-string-var var) ")[" 
		(exp-to-string-exp (exp-map-body-inst1 map (exp-push-var body var) var)) "]"))
    (throw 'exp-to-string "抽象体でありません．")))

;;;;;;;;;;;;
;; 抽象式 ;;
;;;;;;;;;;;;

;; 抽象式 <- (変数 式)

(defun exp-Abs-cons (var exp)
  (mAbs var exp))

;; 抽象式 -> (変数 式)

(defun exp-Abs-dest (aexp) (but-1st aexp))

(defun exp-Abs-dest1 (aexp) (1st (exp-abs-dest aexp)))

(defun exp-Abs-dest2 (aexp) (2nd (exp-abs-dest aexp)))

(defun exp-Abs-dest3 (aexp) (3rd (exp-abs-dest aexp)))

(defun exp-to-string-exp (exp)
  (cond ((exp-varref-p exp) (exp-to-string-varref exp))
	((exp-const-p exp) (exp-to-string-const exp))
	((exp-Abs-p exp) (exp-to-string-Abs exp))
	(t ;; it must be a struct!, check that dynamically
	 (exp-to-string-struct exp))))

(defun nf-non-disjoint (listA listB)
  "Check if some variable in LISTA is declared in LISTB"
  (let ((result nil) (cont t))
    (while (and listA cont)
      (if (nf-contained (car listA) listB)
	  (setq result t
		cont nil)
	(setq listA (cdr listA))))
    result))

;; defgame

(defmacro defgame (name &rest rule-list)
  (list 'DefGame (list 'quote name) (list 'quote rule-list)))
  ;;(format "導出ゲーム「%s」を定義しました" name))

(defun DefGame (name rule-list &optional direct) 
  "define a game NAME with RULE-LIST.  DIRECT means rule clauses are
already parsed."
  (if nf-debug (setq DGLIST (list name rule-list direct)))
  (let ((list nil) (debug-on-error nf-debug)
	(name-str (symbol-name name)))
    ;;(if (member name-str nf-game-list)
	;;(error "導出ゲーム「%s」はすでに定義されています" name))
    ;; first load grammatical defs for the game
    ;;(setq PrimExpList nf-basic-exp-list)
    ;;(load-file (concat nf-def-dir name-str ".def"))
    ;;(setq nf-name-of-the-game name-str)
    (while rule-list
      (let* ((rule (car rule-list))
	     (rule-name (first rule))
	     (params (second rule))
	     (items (cdr (cdr rule)))
	     (newitems nil)) 
	;; each rule is of the form:
	;; (<rule-name> <params> <item> ... <item>)
	;; where <params> = (<var-name> ... <var-name>), each <var-name>
	;; is a symbol representing the name of the parameter.
	;; each <item> is usually a string representing a conclusion or
	;; a premise of the rule.  it is also possible that a rule has
	;; only one premise which is a Lisp program that can be used
	;; to represent a meta-level rule scheme.
	;;
	;; add NAME to the list of reserved words. 
	;;(nf-extend-prim-exp-list rule-name)
	(parser-extend-prim-exp-pat rule-name)
	(while items
	  (let ((item (car items)))
	    (if (or direct (stringp item))
		(setq newitems 
		      ;; check if each item is a schematic judgment
		      (cons (nf-schematize 
			     (if direct
				 ;; no need to parse if DIRECT
				 item
			       (ParseString item 'sjudgment)) params)
			    newitems))
	      ;; in this case, ITEM is a program to be used to check
	      ;; the correctness of the derivation at meta-level.
	      ;; so we signal this by consing the symbol META to the item
	      (setq newitems (cons (cons 'META item) newitems)))
	    (setq items (cdr items))))
	;; check if all the parameters of the rule are declared
	(setq list 
	      (cons (cons rule-name (cons params (reverse newitems))) list))
	(setq rule-list (cdr rule-list))))
    (put name 'rule-list list) 
    ;; add NAME to the list of reserved words. 
    ;;(nf-extend-prim-exp-list name-str)
    (parser-extend-prim-exp-pat name-str)
    ;; add NAME-STR to the list of NF game names
    ;;(setq nf-game-list (cons name-str nf-game-list))
    (format "導出ゲーム「%s」を定義しました" name)
))

(defun nf-get-rule (rule-name game)
  "Get the rule RULE-NAME in GAME.  Rules in Common.gm is also tried."
  (let ((rule (or (assoc rule-name (get (intern game) 'rule-list))
		  (assoc rule-name (get 'Common 'rule-list)))))
    (if rule
	(cdr rule)
      ;; try theorem
      (if (setq rule (assoc rule-name (get (intern game) 'thm-list)))
	  (let ((body (cdr rule)))
	    (if (= (length body) 2)
		;; ordinary rules
		body
	      ;; theorem
	      ;; body == (vars (cons concl hyps) deriv)
	      (cons (first body) (second body))))
	nil))))

;; derivation context

(defun nf-valid-context (context)
  (let ((dc nil) (gc nil) (ct context))
    (while ct
      (let ((hyp (car ct)))
	(if (exp-var-p hyp)
	    (if (assoc hyp dc)
		(error "「%s」は導出変数なので一般変数として使えません" 
		       (exp-var-dest hyp))
	      (setq gc (cons hyp gc)))
	  (if (nf-member (first hyp) gc) 
	      (let* ((e-token (exp-dhyp-var hyp))
		     (e-region (cal-exp-region e-token)))
		(cal-set-epos (first e-region) (second e-region))
		(error "「%s」は一般変数なので導出変数として使えません"
		       (exp-var-dest e-token)))
	    ;; check if vars in the judgment of hyp are declared in GC
	    (nf-sublist (nf-vars (exp-dhyp-judg hyp)) gc)
	    (setq dc (cons hyp dc))))
	(setq ct (cdr ct))))
    ;; just return original context
    context))

;;;;;;;;;;;;;
;; checker ;;
;;;;;;;;;;;;;

(setq nf-debug t)
(setq nf-debug nil)

(defun NF (string)
  (if nf-debug (setq CS nil))
  (let ((debug-on-error nf-debug))
    (catch 'fail
      (nf-check-derivation
       (ParseString string 'derivation)))))

(defun nf-check-derivation (derivation)
  "Check if DERIVATION is a correct derivation. Don't call this function
directly, but use cal-check-derivation, so that the original string
of the DERIVATION is accessible as STR."
  (let ((deriv (exp-derivation-deriv derivation))
	(context (nf-valid-context (exp-derivation-context derivation)))
	(concl (exp-derivation-concl derivation))
	(game (exp-derivation-game derivation)))
    ;; for debug
    ;;(setq DE derivation CCL concl CT context STR str)
    ;; first check vars in CONCL are declared in CONTEXT.
    (message "導出をチェックしています．．．")
    (nf-sublist (nf-vars concl) (nf-gc context))
    (nf-sublist (nf-vars deriv) (nf-decl-vars context))
    (let ((dconcl (nf-check-deriv deriv (reverse context) game)))
      ;; for debug
      ;;(setq DC dconcl)
      (message "")
      (unless (nf-eq dconcl concl)
	(let* ((d-region (cal-string-closure str (cal-exp-region dconcl)))
	       (c-region (cal-string-closure str (cal-exp-region concl))))
	  (cal-set-epos (first d-region) (second d-region))
	  (cal-set-epos2 (first c-region) (second c-region))
	  (error "導出すべき判断(青)と導出された判断(赤)が一致しません"))))))

(setq NCD nil)

(defun nf-check-deriv (deriv context game)
  "Check if DERIV is a correct deriv under CONTEXT in GAME.
Return conclusion of the DERIV or report an error."
  (if nf-debug (setq NCD (cons (list deriv context game) NCD)))
  (cond ((exp-varref-p deriv)
	 ;; this case covers both X-type and #X-type variables.
	 (let ((v (nf-assoc deriv context)))
	   (if v (exp-dhyp-judg v)
	     (let ((pos (exp-varref-pos deriv)))
	       (cal-set-epos pos (+ pos (length (exp-varref-dest deriv)))))
	     (error "「%s」は仮定されていません" 
		    (exp-to-string-varref deriv)))))
	((exp-CD-p deriv)
	 (let ((conc (exp-CD-concl deriv))
	       (subderivs (exp-CD-subderivs deriv))
	       (rule-name (exp-CD-rule-name deriv))
	       (rule-pos (exp-CD-rule-pos deriv))
	       (rule-args (exp-CD-rule-args deriv)))
	   (let* ((rule (nf-get-rule rule-name game)) 
		  (rule-params (exp-rule-params rule))
		  (prem-scheme-list (cdr (cdr rule)))
		  ;; we count number of non-meta premises in PREM-SCHEME-LIST
		  ;; since it may contain a program for handling
		  ;; meta-level rule
		  (rule-arity (nf-count-premises prem-scheme-list))
		  ;;(rule-arity (length prem-scheme-list))
		  (env nil)
		  )
	     (when (null rule)
	       (cal-set-epos)
	       (error "「%s」は導出ゲーム %s の規則の名前ではありません" 
		      rule-name game))
	     ;; check lengths of premises 
	     (unless (= (length subderivs) rule-arity)
	       (cal-set-epos)
	       (error "規則「%s」の前提は %s 個必要です" rule-name rule-arity))
	     (when rule-args
	       ;; check deriv by instantiating the deriv under the
	       ;; environment determined by RULE-ARGS
	       ;; check lengths of args and params
	       (when (> (length rule-args) (length rule-params)) 
		 (cal-set-epos) 
		 (error "規則 %s のパラメータは %s 個です．パラメータが多すぎます．" 
			rule-name (length rule-params)))
	       (setq env (nf-mk-env rule-params rule-args)) )
	     ;; check deriv by pattern matching
	     (let ((meta-check nil))
	       (setq env 
		     (catch 'fail
		       (nf-match
			;; construct equations
			(let ((eqs 
			       (list (list (exp-rule-conc rule) conc 0)))
			      (psl prem-scheme-list)
			      (subds subderivs))
			  (while psl
			    (let ((dc (nf-deriv-concl (car subds) context))
				  (item (car psl)))
			      (if (eq (car item) 'sexp)	;; META !!
				  (setq meta-check (second item)
					psl nil)
				(setq eqs (cons (list (car psl) dc 0)
						eqs)
				      psl (cdr psl)
				      subds (cdr subds)))))
			  eqs)
			nil nil env)))
	       (when (stringp env)
		 ;; matching failed, so report an error with pos info
		 (cal-set-epos)
		 (error "規則「%s」の適用に誤りがあります" rule-name))
	       (when meta-check
		 ;; evaluate the META-CHECK program.  the program
		 ;; may refer to ENV, which we have just computed
		 ;; above.
		 ;; (setq MC meta-check ENV env)
		 (unless
		     (eval
		      (let ((decl 
			     (mapcar 
			      (lambda (item) 
				(list (first item) 
				      (list 'quote (second item))))
			      env))
			    (exp meta-check))
			`(let ,decl ,exp)))
		   (cal-set-epos)
		   (error "規則「%s」の適用に誤りがあります" rule-name)
		   ))))
	   ;; finally check subderivations
	   (while subderivs
	     (nf-check-deriv (car subderivs) context game)
	     (setq subderivs (cdr subderivs)))
	   conc))
	((exp-HD-p deriv)
	 (let* ((hyp (exp-HD-hyp deriv))
		(hypj (exp-HD-hypj deriv))
		(var (exp-HD-var deriv))
		(context (cons hyp context)))
	   ;;(message "here") (sit-for 1)
	   ;;(setq De deriv H hyp)
	   ;; we must first check that HYPJ is a valid expression
	   ;; under CONTEXT
	   (nf-sublist (nf-vars hypj) (nf-gc context))
	   (list op-hj hypj 
		 (nf-check-deriv
		  (exp-map-body-inst1 (exp-HD-map deriv) 
				      (exp-push-var (exp-HD-body deriv) var)
				      var)
		  context game))))
	((exp-UD-p deriv)
	 (let* ((var (exp-Abs-var deriv))
		;;(map (exp-Abs-map deriv))
		;;(body (exp-Abs-body deriv))
		(context (cons var context)))
	   ;; for debug
	   ;;(setq D deriv Var var)
	   (cons 'Abs 
		 (cons (list var)
		       (mabs (list var)
			     (nf-check-deriv
			      (exp-map-body-inst1 (exp-Abs-map deriv) 
						  (exp-push-var 
						   (exp-Abs-body deriv) var)
						  var)
			      context game))))))
	   ))

(defun nf-count-premises (list)
  "Count no of ordinary premises in LIST.  An item whose car is META
is a special premiss and not counted."
  (let ((count 0))
    (while list
      (unless (eq 'sexp (car (car list)))
	(setq count (1+ count)))
      (setq list (cdr list)))
    count))

(defun nf-deriv-concl (deriv context)
  "Returns the conclusion of DERIV under CONTEXT."
  (cond ((exp-varref-p deriv)
	 ;; this case covers both X-type and #X-type variables.
	 (let ((v (nf-assoc deriv context)))
	   (if v (exp-dhyp-judg v) 
	     (let ((pos (exp-varref-pos deriv)))
	       (cal-set-epos pos (+ pos (length (exp-varref-dest deriv)))))
	     (error "「%s」は仮定されていません" 
		    (exp-to-string-varref deriv)))))
	((exp-CD-p deriv) (exp-CD-concl deriv))
	((exp-HD-p deriv)
	 (let* ((hyp (exp-HD-hyp deriv))
		(hypj (exp-HD-hypj deriv))
		(var (exp-HD-var deriv))
		(context (cons hyp context)))
	   (list op-hj hypj 
		 (nf-deriv-concl
		  (exp-map-body-inst1 (exp-HD-map deriv) 
				      (exp-push-var (exp-HD-body deriv) var)
				      var)
		  context))))
	((exp-UD-p deriv)
	 (let* ((var (exp-Abs-var deriv))
		;;(map (exp-Abs-map deriv))
		;;(body (exp-Abs-body deriv))
		(context (cons var context)))
	   (cons 'Abs 
		 (cons (list var)
		       (mabs (list var)
			     (nf-deriv-concl
			      (exp-map-body-inst1 (exp-Abs-map deriv) 
						  (exp-push-var 
						   (exp-Abs-body deriv) var)
						  var)
			      context))))))
	))

(defun nf-mk-env (listA listB)
  ;; we know that two the length of listB >= that of listB
  ;; we first check args are OK under gc
  ;; refer to the global variable CONTEXT
  (nf-sublist (nf-vars listB) (nf-gc context))
  (let ((list nil))
    (while listB
      (setq list (cons (list (car listA) (car listB)) list))
      (setq listA (cdr listA)
	    listB (cdr listB)))
    list))

(defun nf-mk-match-env (list)
  (let ((env nil))
    (while list
      (let ((var (car list)))
	(setq env (cons (list var (list 'svar (symbol-name var))) env))
	(setq list (cdr list))))
    env))

(defun nf-gc (context)
  "compute the general vars list of CONTEXT"
  (let ((val nil))
    (while context
      (let ((var (car context)))
	(if (exp-var-p var)
	    (setq val (cons var val))))
      (setq context (cdr context)))
    val))

(defun nf-gc-intern (context)
  "compute the general vars list of CONTEXT. variables are interned to
atoms."
  (let ((val nil))
    (while context
      (let ((item (car context)))
	(if (exp-var-p item)
	    (setq val (cons (intern (exp-var-dest item)) val))))
      (setq context (cdr context)))
    (reverse val)))

(defun nf-dc (context)
  "compute the derivation context part of CONTEXT.
order of elements are reversed."
  ;;(setq C context)
  (let ((val nil))
    (while context
      (let ((item (car context)))
	(or (exp-var-p item)
	    (setq val (cons item val))))
      (setq context (cdr context)))
    val))

(defun nf-hyps (context)
  "compute the hypotheses of CONTEXT."
  (let ((val nil))
    (while context
      (let ((item (car context)))
	(or (exp-var-p item)
	    (setq val (cons (cdr item) val))))
      (setq context (cdr context)))
    (reverse val)))

(defun nf-decl-vars (context)
  "list of variables declared in CONTEXT."
  (let ((val nil))
    (while context
      (let ((item (car context)))
	(if (exp-var-p item) 
	    (setq val (cons item val)) 
	  ;; item == var :: judgment, in this case
	  (setq val (cons (car item) val)))
	(setq context (cdr context))))
    val))

(defun nf-vars (e)
  "Compute list of free variables in E."
  (cond ((equal e dt-dummy-var) nil)
	((exp-var-p e) (list e))
	((exp-sharp-var-p e) (list e))
	((exp-atom-p e) nil)
	((exp-Abs-p e) (let ((e1 (fourth e))) (nf-vars e1)))
	(t 
	 (let ((e1 (car e)) (e2 (cdr e))) 
	   (nf-append (nf-vars e1) (nf-vars e2))))))

(defun nf-vars2 (e)
  "Compute list of free variables in E.  E may contain meta-application."
  (cond ((equal e dt-dummy-var) nil)
	((exp-var-p e) (list e))
	((exp-sharp-var-p e) (list e))
	((exp-atom-p e) nil)
	((exp-sapp-p e)
	 (nf-append (list (exp-sapp-var e))
		    (nf-vars2 (exp-sapp-args e))))
	((exp-Abs-p e) (let ((e1 (fourth e))) (nf-vars2 e1)))
	(t 
	 (let ((e1 (car e)) (e2 (cdr e))) 
	   (nf-append (nf-vars2 e1) (nf-vars2 e2))))))

(defun nf-append (listA listB)
  "Prepend elements in LISTA not in LISTB to LISTB."
  (let ((result listB))
    (while listA
      (let ((item (car listA)))
	(or (nf-member item result)
	    (setq result (cons item result)))
	(setq listA (cdr listA))))
    result))

(defun nf-intersect-p (e list)
  "Test if the mexp E contains an op in LIST, where LIST is a list of
ops."
  (cond ((null e) nil)
	((equal e dt-dummy-var) nil)
	((exp-var-p e) nil)
	((exp-sharp-var-p e) nil)
	((exp-svar-p e) nil)
	((exp-const-p e) (nf-member e list))
	((exp-Abs-p e) (let ((e1 (fourth e))) (nf-intersect-p e1 list)))
	(t 
	 (let ((e1 (car e)) (e2 (cdr e))) 
	   (or (nf-intersect-p e1 list) (nf-intersect-p e2 list))))))

;; Removal candidate
;;(defun nf-distinct-vars-p (list)
  ;;"Check that members of LIST are all distinct."
  ;;(let ((result t) (cont t))
    ;;(while (and list cont)
      ;;(let ((var (car list)))
	;;(if (or (eq (car var) 'svar) (member var (cdr list)))
	    ;;(setq result nil cont nil)
	  ;;(setq list (cdr list)))))
    ;;result))

;; Removal candidate
;;(defun nf-declared-p (listA listB)
  ;;"Check each member of listA is decared in listB. Return T or NIL.  Also
;;check that members of listA are all distinct."
  ;;(let ((result t) (cont t))
    ;;(while (and listA cont)
      ;;(let ((var (car listA)))
	;;(if (member var (cdr listA))
	    ;;(setq result nil cont nil)
	  ;;(if (exp-sharp-var-p var)
	      ;;(if (nf-contained var listB)
		  ;;(setq listA (cdr listA))
		;;(setq result nil cont nil))
	    ;;(if (member var listB)
		;;(setq listA (cdr listA)) 
	      ;;(setq result nil cont nil))))))
    ;;result))

(defun nf-sublist (listA listB)
  (while listA
    (let ((var (car listA)))
      (if (exp-sharp-var-p var)
	  (if (nf-contained var listB)
	      (setq listA (cdr listA))
	    (let* ((min-max (cal-varref-region var))
		   (min (first min-max))
		   (max (second min-max)))
	      (cal-set-epos min max))
	    (error "変数「%s」は宣言されていません" (exp-to-string-varref var)))
	(if (nf-member var listB)
	    (setq listA (cdr listA))
	  (let* ((min-max (cal-varref-region var))
		 (min (first min-max))
		 (max (second min-max)))
	    (cal-set-epos min max))
	  (error "変数「%s」は宣言されていません" (exp-to-string-varref var))))))
  t)

(defun nf-contained (var list)
  "Check if #-var VAR is an expression under the variable declaration
LIST.  E.g., ##x is contained in (x x x) but not in (x x)."
  (let ((n (if (exp-varref-with-pos-info-p var)
	       (- (length var) 2)
	     (- (length var) 1)))
	(core (exp-sharp-var-to-var var))
	(m 0))
    (while list
      (if (nf-eq core (car list))
	  (setq m (1+ m)))
      (setq list (cdr list)))
    (<= n m)))

(defun nf-schematize (e &optional params)
  "Schematize E by converting free variables in E to schematic variables.
We assume that free variables in E do not occur as local variables.
If optional PARAMS is given, check if each free variable in E is a
member of PARAMS.  We use OFFSET which is set by the function
PARSE-STRING which calls this function."
  (cond ((exp-var-p e) 
	 (if params
	     (let ((name (second e)))
	       (cond ((integerp name)
		      ;; this is a bound variable so leave it as it is
		      e)
		     ((member (intern name) params) (list 'svar name))
		     (t 
		      (let ((pos (+ (1- offset) (third e))))
			(cal-set-epos pos (+ pos (length name)))
			(error "%s は規則のパラメータとして宣言されていません" name)))))
	   (list 'svar (second e))))
	((exp-sharp-var-p e) (error "Error in rule!"))
	((exp-atom-p e) e)
	((exp-qq-p e) 
	 ;; in this case, we just return E, since schematic variables
	 ;; are explicitly given in the body of quasi-quotation
	 e)
	((exp-Abs-p e) (let ((v (second e)) (m (third e)) (e1 (fourth e))) 
			 (list 'Abs v m (nf-schematize e1 params))))
	((exp-sapp-p e)
	 (let ((x (exp-sapp-var e)) (args (exp-sapp-args e)))
	   (exp-mk-sapp (list 'svar (second x)) (nf-schematize args params))))
	(t (let ((e1 (car e)) (e2 (cdr e)))
	     (cons (nf-schematize e1 params) (nf-schematize e2 params))))))

(defun nf-push-env (env v)
  (let ((newenv nil))
    (while env
      (let* ((item (car env))
	     (var (first item))
	     (exp (second item)))
	(setq newenv (cons (list var (exp-push-var exp v)) newenv)
	      env (cdr env))))
    newenv))

(defun Push (abs)
  (let ((v (exp-Abs-var abs)) 
	(map (exp-Abs-map abs)) 
	(body (exp-Abs-body abs)))
    (list 'Abs v map (exp-push-var body v))
    ))

;; <context> ::= (<decl> ... <decl>)
;; <decl> ::= <var> | (<var> . <judgment>)

(defun nf-assoc (var context)
  "try to find a derivation variable declaration for VAR in CONTEXT, 
and return the result by pushing through the surrounding context."
  (let ((vars nil) ;; accumulate genral variables surrounding VAR
	(result nil) (cont t) (list nil))
    (while (and cont context)
      (let ((item (car context)))
	(if (exp-var-p item)
	    ;; ITEM is a general variable, so cons it to L.
	    (setq vars (cons item vars)
		  context (cdr context))
	  (let ((dvar (car item)))
	    (if (exp-var-p var)
		(if (nf-eq var dvar)
		    ;; found it!
		    (setq result item
			  cont nil)
		  (setq context (cdr context))) 
	      ;; var is #var 
	      (let ((core-var (exp-sharp-var-to-var var)))
		(if (nf-eq core-var dvar)
		    ;; found one var!
		    (setq var (exp-pull-sharp-var var)))
		(setq context (cdr context)))))))) 
    (if result
	(if vars (exp-push-vars result vars) result)
      nil)))

;;;;;;;;;;;;;;;;;;
;;;            ;;;
;;; 定理と定義 ;;;
;;;            ;;;
;;;;;;;;;;;;;;;;;;

(defun NFTH (deriv-str name)
  (let ((debug-on-error nf-debug))
    (setq CS nil)
    (or (stringp (catch 'fail (ParseString name 'op)))
	(error "「%s」は定理の名前に使えません" name))
    (nf-mk-theorem (ParseString deriv-str 'derivation) name)))

(defun nf-mk-theorem (derivation name)
  "Convert DERIVATION into a theorem with a given NAME."
  ;; first check that NAME is not a reserved word.
  (or (stringp (catch 'fail (ParseString name 'op)))
      (error "「%s」は定理の名前に使えません" name))
  ;; check DERIVATION
  (nf-check-derivation derivation)
  (let* ((context (exp-derivation-context derivation))
	 (concl (exp-derivation-concl derivation))
	 (game (intern (exp-derivation-game derivation)))
	 (list (get game 'rule-list))
	 (vars (nf-gc-intern context))
	 (premises (nf-hyps context))
	 (items (cons concl premises))
	 (newitems nil)) 
    (while items
      (let ((item (car items)))
	;; for debug
	;;(setq I item)
	(setq newitems (cons (nf-schematize item) newitems))
	(setq items (cdr items))))
    (setq list (cons (cons name (cons vars (reverse newitems))) list))
    (put game 'rule-list list) 
    ;; add NAME to the list of reserved words. 
    (nf-extend-prim-exp-list name)
    (format "定理「%s」を登録しました" name) ))

(defun nf-mk-def (derivation name)
  "Convert DERIVATION whose conclusion is of the form e:Prop
into a definition with a give name.  Rules for introducing and elminating
defined propostion are added to the game."
  ;; check DERIVATION
  (nf-check-derivation derivation)
  (let* ((nameF (concat name "_F"))
	 (nameI (concat name "_I"))
	 (nameE (concat name "_E"))
	 (context (exp-derivation-context derivation))
	 (concl (nf-schematize (exp-derivation-concl derivation)))
	 newconcl newtruth oldtruth
	 (game (intern (exp-derivation-game derivation)))
	 (list (get game 'rule-list))
	 (args (nf-schematize (nf-gc context)))
	 (vars (nf-gc-intern context))
	 (premises (nf-hyps context))
	 (newitems nil)) 
    ;; first check that NAME is not a reserved word. 
    (or (stringp (catch 'fail (ParseString name 'op)))
	(error "「%s」は定義の名前に使えません" name))
    (or (stringp (catch 'fail (ParseString nameF 'op)))
	(error "「%s」は定義の名前に使えません" name))
    (or (stringp (catch 'fail (ParseString nameI 'op)))
	(error "「%s」は定義の名前に使えません" name))
    (or (stringp (catch 'fail (ParseString nameE 'op)))
	(error "「%s」は定義の名前に使えません" name))
    (or (and (listp concl) (= (length concl) 3)
	     (equal (first concl) '(op ":"))
	     (equal (third concl) '(op "Prop")))
	(error "この導出は定義に使えません．"))
    ;; it is enough only to schematize ARGS
    (setq newconcl (list '(op ":") (cons (list 'op name) args) '(op "Prop")))
    (setq newtruth (list '(op ":") (cons (list 'op name) args) '(op "True")))
    (setq oldtruth (list '(op ":") (second concl) '(op "True")))
    (while premises
      (let ((item (car premises)))
	(setq newitems (cons (nf-schematize item) newitems))
	(setq premises (cdr premises))))
    (setq list 
	  (cons 
	   (cons nameF (cons vars (cons newconcl (reverse newitems))))
	   list))
    (setq list (cons (list nameI vars newtruth oldtruth) list))
    (setq list (cons (list nameE vars oldtruth newtruth) list))
    (put game 'rule-list list) 
    ;; add NAME to the list of reserved words. 
    (nf-extend-prim-exp-list name)
    (nf-extend-prim-exp-list nameF)
    (nf-extend-prim-exp-list nameI)
    (nf-extend-prim-exp-list nameE)
    (format "定義「%s」を登録しました" name) ))

;; NF pattern matcher.

;; <exp> ::= nil
;;        |  <varref>
;;        |  <const>
;;        |  (Abs <var> <map> <exp>)
;;        |  (<exp> . <exp>)

;; <pat> ::= nil
;;        |  <svar>
;;        |  <varref>
;;        |  <atom>
;;        |  (Abs <var> <map> <pat>)
;;        |  <svar>[<pat>, ... , <pat>]
;;        |  (<pat> . <pat>)

;; <map> ::= nil
;;        |   1  |  2  |  3  | ... 
;;        |  (<map> . <map>)

;; <mpat> ::= nil
;;        |   ?  ; map variable
;;        |   1  |  2  |  3  | ... 
;;        |  (<mpat> . <mpat>)

;; X[P1, ... , Pn] = E =>
;; we create a schematic variable equation:
;;    X = (? E ((P1 ... Pn) (() E)))
;; where mX is a map variable uniquely determined by the schematic
;; variable X. This equation can be extended to the following three
;; types of equations:
;;   (1) X = (nil E) ((P1 ... Pn) (nil E)))
;;   (2) X = (i E) where 1 <= i <= n, in this case we also create the
;;       equation: Pi = E
;;   (3) X = ((mXL . mXR) E ((P1 ... Pn) (mXL EL)) ((P1 ... Pn) (mXR ER)))
;;           where E = (EL . ER)
;; General form of a schematic variable equation is:
;;   X = (<mpat> <exp> (<lvars> (P1 ... Pn) (<mvar> <exp>)) ...)

;; ((X = (SAbs <mpat> E)) P1 ... Pn)
;; (or (Y = nil) (Y = 1) ... (Y = n) (Y = (YL . YR)))
;; Y = nil -> X = (SAbs nil E)
;; Y = k   -> X = (SAbs k nil), Pk = E
;; Y = (YL . YR) -> X = (SAbs (YL . YR) (EL . ER))

;; solve <eqs>, <seqs> and <meqs> under <env> and <senv>.

;; <eqs> ::= (<eq> ... <eq>)
;; <eq>  ::= (<pat> <exp>)
;;        |  (<pat> <exp> <mvar>) ; P = E/X
;;        |  (or2 (<eqs> <env>) (<eqs> <env>))
;;        |  (or3 (<eqs> <env>) (<eqs> <env>) (<eqs> <env>))

;;;;;;;;;;;;;
;;;       ;;;
;;;  seq  ;;;
;;;       ;;;
;;;;;;;;;;;;;

;; data type for equations for schematic variable
;; we wish to solve a system of equations:
;;
;;    X[P1, ... , Pn] = E
;;    ...
;;    X[Q1, ... , Qn] = F
;;
;; the system is solvable if we can find a map M over {1, ... , n} such that
;; for each p*i in M, the system of equations:
;;
;;    Pi = E/p, ... , Qi = F/p
;;
;; is solvable.
;; we represent seq by the list
;;
;;    (X ((P1 ... Pn) ... (Q1 ... Qn)) exp List Map)
;;
;; where each member of List is of the form:
;;
;;    (<path> (E <lvars>) ... (F <lvars>))
;;
;; and each <path> is a sequence of L(eft) and R(ight).
;; Map is a linear representation of a map
;; over {1, ..., n} and this represents the solved (committed) part of
;; the map M we are constructing.

(defun dt-mk-seq (svar plist exp list map)
  (list svar plist exp list map))

(defun dt-seq-get-svar (seq)
  (first seq))

(defun dt-seq-get-plist (seq)
  (second seq))

(defun dt-seq-get-exp (seq)
  (third seq))

(defun dt-seq-get-list (seq)
  (fourth seq))

(defun dt-seq-get-map (seq)
  "Assuming that SEQ is solved, this function returns the MAP part of
SEQ."
  (fifth seq))

(defun dt-seq-get-path (seq)
  "Get the shortest path in the list-part of SEQ."
  (let ((list (dt-seq-get-list seq)))
    (first (first list))))

(defun dt-seq-get-arity (seq)
  "return the arity of the main schematic variable of SEQ."
  (length (first (second seq))))

(defun dt-seq-solved-p (seq)
  "Check if SEQ is already solved."
  (null (fourth seq)))

;;;;;;;;;;;;;;
;;;        ;;;
;;;  pel  ;;;
;;;        ;;;
;;;;;;;;;;;;;;
;; pel = path, (expression, lvars) list
;; (<path> (<exp> <lvars>) ... (<exp> <lvars>))

(defun dt-pel-get-path (pel)
  (car pel))

(defun dt-pel-get-els (pel)
  (cdr pel))

;;;;;;;;;;;;;
;;;       ;;;
;;; list1 ;;;
;;;       ;;;
;;;;;;;;;;;;;
;; list1 = pel list

(defun dt-list1-get-pel (list)
  "get the first pel in LIST."
  (car list))

;;;;;;;;;;
;;;    ;;;
;;; eq ;;;
;;;    ;;;
;;;;;;;;;;
;; eq = (lhs rhs lvars) , lvars is an integer representing the depth
;; of local variables

(defun dt-mk-eq (l r lvars)
  (list l r lvars))

;;;;;;;;;;;;;;
;;;        ;;;
;;;  svar  ;;;
;;;        ;;;
;;;;;;;;;;;;;;
;; <svar> ::= (svar <string>) | (svar <integer>)

(defun dt-svar-intern (svar)
  "construct an atom from the name of SVAR."
  (intern (second svar)))

(defun exp-destruct (e path)
  "destruct E along PATH, where PATH is a list of L or R."
  (let ((result e))
    (while path
      (let ((p (car path)))
	(setq path (cdr path))
	(cond ((eq p 'L)
	       (if (exp-pair-p result)
		   (setq result (car result))
		 (throw 'fail (format "cannot destruct %s" result))))
	      ((eq p 'R)
	       (if (exp-pair-p result)
		   (setq result (car result))
		 (throw 'fail (format "cannot destruct %s" result))))
	      ((eq p 'M)
	       (if (exp-Abs-p result)
		   (setq result (exp-Abs-map result))
		 (throw 'fail (format "%s should be an abstract" result))))
	      ((eq p 'B)
	       (if (exp-Abs-p result)
		   (setq result (exp-Abs-body result))
		 (throw 'fail (format "%s should be an abstract" result))))
	      (t (error (format "illegal path component: %s" p))))))
    result))

;; Given a map over {1, ... , n} and an exression, abstraction of
;; the expression wrt the map is defined.

(defun exp-local-p (exp)
  "check if EXP contains local variables."
  (cond ((exp-var-p exp) (integerp (exp-var-dest exp)))
	((exp-sharp-var-p exp) nil)
	((exp-const-p exp) nil)
	((exp-Abs-p exp) (exp-local-p (exp-Abs-body exp)))
	(t 
	 ;; exp is a cons pair
	 (or (exp-local-p (car exp)) (exp-local-p (cdr exp))))))

;; the NF pattern matching function
;; for debug
(setq CS nil) ;; call stack

(defun nf-match-p (mexp exp)
  "Match MEXP against EXP.  Return NIL if the matching failed, and
T if successful."
  (if (stringp
       (catch 'fail
	 (nf-match (list (list mexp exp 0)) nil nil nil)))
      ;; match failed
      nil
    ;; match was successful
    t))

(defun nf-match-simple (mexp exp)
  "match MEXP agains EXP"
  (nf-match (list (list mexp exp 0)) nil nil nil))

(defun nf-match (eqs seqs qeqs env)
  "Solve mathcing equations EQS, SEQS and QEQS under given environment
ENV."
  (let ((fail-message nil))
    (if nf-debug (setq CS (cons (list eqs seqs qeqs env) CS)))
    (if (null eqs)
	(if (null seqs)
	    (if (null qeqs)
		;; since EQS, SEQS and QEQS are all empty, we have 
		;; successfully solved the equations 
		env
	      ;; we solve QEQS
	      (nf-match-qq qeqs env))
	  ;; take the first equation in SEQS
	  (let ((seq (first seqs)))
	    (if nf-debug (setq SEQS seqs))
	    ;; check if SEQ is already solved
	    (if (dt-seq-solved-p seq)
		;; since SEQ is solved, we add the solution to ENV
		(let ((map (dt-seq-get-map seq))
		      (exp (dt-seq-get-exp seq))
		      (svar (dt-seq-get-svar seq)))
		  (nf-match eqs (cdr seqs) qeqs
			    (cons (list (dt-svar-intern svar) 
					(exp-mk-abs-from-lmap map exp)) env)))
	      ;; we try to solve the first equation in SEQS.
	      (let ((svar (dt-seq-get-svar seq))
		    (n (dt-seq-get-arity seq))
		    (exp (dt-seq-get-exp seq)))
		;; create new seqs by considering possible instantiations of
		;; the MVAR (map variable)
		(let ((result 
		       ;; first, consider the case where MVAR is nil
		       ;; we create new equations for the newly commited
		       ;; position.
		       (catch 'fail 
			 (nf-match nil
				   (cons (nf-inst-to-nil seq)
					 (cdr seqs))
				   qeqs env))))
		  (if (stringp result)
		      ;; first case failed! so, consider the case where
		      ;; MVAR is an integer I such that 1 <= I <= N, where
		      ;; N is the arity of X
		      (let ((i 1) (cont t) result)
			(while (and (<= i n) cont)
			  (setq result
				;; consider the case where MVAR is I
				(catch 'fail 
				  (nf-match 
				   (nf-inst-to-neweqs i seq)
				   (cons (nf-inst-to i seq) 
					 (cdr seqs))
				   qeqs env)))
			  (if (stringp result)
			      ;; since this case failed, we increment I
			      (setq i (1+ i))
			    ;; we now found a solution!
			    (setq cont nil)))
			(if (stringp result)
			    ;; we must consider the final case where
			    ;; PATH is extended to a pair of paths
			    (nf-match
			     eqs
			     (cons (nf-inst-to-pair seq) (cdr seqs))
			     qeqs env)
			  ;; return RESULT as our solution
			  result))
		    result))))))
      ;; eqs are non-empty, so try to solve the first equation
      (let* ((eq (first eqs))
	     (lhs (first eq))
	     (rhs (second eq))
	     (lvars (third eq)))
	;(setq EQ eq SEQS seqs)
	(cond ((exp-atom-p lhs)
	       ;; object level vars are constants essentially
	       (if (nf-eq lhs rhs)
		   (nf-match (cdr eqs) seqs qeqs env)
		 (throw 'fail (format "%s and %s do not match" lhs rhs))))
	      ((exp-qq-p lhs)
	       ;; add the current equation to QEQS. this implementation
	       ;; assume that lvars does not matter.
	       (nf-match (cdr eqs) seqs
			 (cons (list (exp-qq-body lhs) rhs) qeqs) env))
	      ((exp-svar-p lhs)
	       (let ((val (assoc (dt-svar-intern lhs) env)))
		 (if val
		     ;; the variable has a value in ENV, so create a new
		     ;; eq.
		     (let* ((newlhs (second val))
			    (neweqs
			     ;; if the new equation is trivial, we don't add
			     ;; it
			     (if (nf-eq newlhs rhs)
				 (cdr eqs)
			       (cons (list newlhs rhs 0) (cdr eqs)))))
		       (nf-match neweqs seqs qeqs env))
		   (if (and (exp-var-p rhs)
			    (exp-local-or-dummy-var-p rhs))
		       (throw 'fail 
			      (format "%s contains local or dummy variables" 
				      rhs))
		     ;; extend env and continue to solve the rest of EQS
		     (nf-match (cdr eqs) seqs qeqs
			       (cons (list (intern (exp-svar-dest lhs)) rhs) 
				     env)))))) 
	      ((exp-Abs-p lhs)
	       (if (exp-Abs-p rhs)
		   ;; instantiate abstractions and continue to match
		   (let ((lvar (list 'var (1+ lvars))))
		     (nf-match 
		      (cons 
		       (list (Inst lhs lvar) 
			     (Inst rhs lvar)
			     (1+ lvars))
		       (cdr eqs))
		      seqs qeqs
		      env))
		 (throw 'fail (format "%s and %s do not match" lhs rhs)) ))
	      ((exp-sapp-p lhs)
	       (let* ((svar (exp-sapp-var lhs))
		      (plist (exp-sapp-args lhs))
		      (val (assoc (intern (exp-svar-dest svar)) env)))
		 (if val
		     ;; SVAR is already solved, so we construct
		     ;; a new equation
		     (nf-match
		      (cons (list (exp-abs-app (second val) plist) rhs lvars)
			    (cdr eqs))
		      seqs qeqs env)
		   (let ((val2 (assoc svar seqs)))
		     (if val2
			 ;; extend seq for SVAR.  also add equations for
			 ;; the committed parts of the seq.
			 (let ((map (dt-seq-get-map (second val))))
			   (nf-match
			    (append (nf-mk-eqs-for-map map plist rhs lvars) 
				    (cdr eqs))
			    (nf-extend-seqs seqs svar plist rhs lvars)
			    qeqs env))
		       ;; create a new seq
		       (nf-match 
			(cdr eqs) 
			(cons (nf-mk-seq svar plist rhs lvars) seqs)
			qeqs env))))))
	      (t 
	       ;; LHS is a cons pair
	       (if (or (null rhs) (exp-atom-p rhs) (exp-svar-p rhs)
		       (exp-const-p rhs) (exp-Abs-p rhs))
		   (throw 'fail (format "%s and %s do not match" lhs rhs))
		 (let ((lhs1 (car lhs)) (rhs1 (car rhs))
		       (lhs2 (cdr lhs)) (rhs2 (cdr rhs)))
		   ;; avoid adding trivial equations.
		   (if (nf-eq lhs1 rhs1)
		       (if (nf-eq lhs2 rhs2)
			   (nf-match (cdr eqs) seqs qeqs env)
			 (nf-match
			  (cons (list lhs2 rhs2 lvars) (cdr eqs)) 
			  seqs qeqs env))
		     (if (nf-eq lhs2 rhs2)
			 (nf-match
			  (cons (list lhs1 rhs1 lvars) (cdr eqs)) 
			  seqs qeqs env)
		       (nf-match
			(cons (list lhs1 rhs1 lvars)
			      (cons (list lhs2 rhs2 lvars) (cdr eqs)))
			seqs qeqs env)))))))))))

(setq QQ nil)

(defun nf-match-qq (qeqs env)
  "Solve matching equations QEQS which are equations of quasi-quoted
patterns under ENV."
  (if nf-debug (setq QQ (cons (list qeqs env) QQ)))
  (let* ((item (car qeqs))
	 (lhs (first item))
	 (rhs (second item))
	 (unit (car lhs)))
    (cond ((null qeqs) 
	   ;; we are done, luckily
	   env)
	  ((parser-scan-svar lhs)
	   (if (nf-eq lhs rhs)
	       (nf-match-qq (cdr qeqs) env)
	     (throw 'fail (format "%s and %s do not match" lhs rhs))))
	  ((exp-svar-p unit)
	   (if (null (cdr lhs))
	       ;; we can now create a new equation
	       (nf-match
		(list (list unit rhs))
		nil
		(cdr qeqs)
		env)
	     ;; we invesitigate the second item of LHS
	     (let ((unit2 (second lhs)))
	       (cond ((exp-svar-p unit2)
		      (error "Sorry, NF-matcher does not take care of the case where two meta variables appear in a row in QQ"))
		     ((exp-unit-paren-p unit2)
		      (let ((cont t)
			    (listL nil) unit-par 
			    (listR rhs)
			    triple result)
			(while cont
			  (if (setq triple
				    (nf-search-unit-paren 
				     (exp-unit-paren-string unit2) listR))
			      ;; search was successful, so try to match
			      ;; using the result
			      (progn
				(setq listL (append listL (first triple))
				      ;; (second TRIPLE) 
				      ;; == (unit (paren "(" ... ))
				      unit-par (second triple)
				      listR (third triple))
				(let ((paren-listL
				       (nf-map-unit 
					(exp-unit-paren-list unit2)))
				      (paren-listR 
				       (nf-map-unit
					(exp-unit-paren-list unit-par)))
				      (lhsR (cddr lhs)))
				  (setq result
					(catch 'fail
					  (nf-match
					   (list (list unit listL))
					   nil
					   (cons (list paren-listL paren-listR)
						 (cons (list lhsR listR)
						       (cdr qeqs)))
					   env)))
				  (if (stringp result)
				      ;; match failed, so try another possibility
				      (if listR
					  ;; since LISTR is nonempty, we
					  ;; continue the while loop.
					  ;; we must also extend listL by
					  ;; UNIT-PAR
					  (setq listL 
						(append listL (list unit-par)))
					;; we can now fail
					(throw 'fail
					       (format "%s and %s do not match"
						       lhs rhs)))
				    ;; match was successful, so return the
				    ;; result, by turning off CONT
				    (setq cont nil))))
			    ;; search failed
			    (throw 'fail
				   (format "%s and %s do not match" lhs rhs))))
			result))
		     (t 
		      ;; unit2 is an ordinary unit, i.e., a token.
		      ;; this case is similar to the above case.
		      (let ((cont t)
			    (listL nil) unit-part
			    (listR rhs)
			    double result)
			(while cont
			  ;; for debug
			  ;;(setq U2 unit2 LR listR)
			  (if (setq double (nf-search-unit-part unit2 listR))
			      ;; search was successful, so try to match
			      ;; using the result. unlike the above case,
			      ;; no need to match unit2.
			      (progn
				(setq listL (append listL (first double))
				      listR (second double))
				(let ((lhsR (cddr lhs)))
				  (setq result
					(catch 'fail
					  (nf-match
					   (list (list unit listL))
					   nil
					   (cons (list lhsR listR) (cdr qeqs))
					   env))))
				(if (stringp result)
				    ;; match failed, so try another possibility
				    (if listR
					;; since LISTR is nonempty, we
					;; continue the while loop.
					;; we must also extend listL by unit2
					(setq listL
					      (append listL (list unit2)))
				      ;; we can now fail
				      (throw 'fail
					     (format "%s and %s do not match"
						     lhs rhs)))
				  ;; match was successful, so return the
				  ;; result, by turning off CONT
				  (setq cont nil)))
			    ;; search failed
			    (throw 'fail
				   (format "%s and %s do not match" lhs rhs))))
			result))))))
	  ((exp-unit-paren-p unit)
	   (when (null rhs)
	     (throw 'fail
		    (format "%s and %s do not match" lhs rhs)))
	   (let ((head-rhs (car rhs)))
	     (if (and (exp-unit-paren-p head-rhs)
		      (string= (exp-unit-paren-string unit) 
			       (exp-unit-paren-string head-rhs)))
		 ;; divide and conquer
		 (let ((paren-listL (nf-map-unit (exp-unit-paren-list unit)))
		       (paren-listR 
			(nf-map-unit (exp-unit-paren-list head-rhs)))
		       (tail-lhs (cdr lhs))
		       (tail-rhs (cdr rhs)))
		   (nf-match-qq
		    (cons (list paren-listL paren-listR)
			  (cons (list tail-lhs tail-rhs) 
				(cdr qeqs)))
		    env))
	       (throw 'fail
		      (format "paren strings %s and %s do not match" 
			      (exp-unit-paren-string unit)
			      (exp-unit-paren-string head-rhs))))))
	  (t
	   ;; now, UNIT is an ordinary unit
	   (when (null rhs)
	     (throw 'fail
		    (format "%s and %s do not match" lhs rhs)))
	   (let ((head-rhs (car rhs)))
	     (if (exp-unit-paren-p head-rhs)
		 ;; fail, in this case
		 (throw 'fail
			(format "%s and %s do not match" lhs rhs)))
	     ;; divide and conquer
	     (if (nf-eq unit head-rhs)
		 (let ((tail-lhs (cdr lhs))
		       (tail-rhs (cdr rhs)))
		   (nf-match-qq
		    (cons (list tail-lhs tail-rhs) (cdr qeqs))
		    env))
	       ;; fail!
	       (throw 'fail
		      (format "%s and %s do not match" unit head-rhs))))))))

(defun nf-map-unit (unitlist)
  (mapcar
   (lambda (x)
     (if (eq (car x) 'svar)
	 x
       (list 'unit x)))
   unitlist))

(defun nf-search-unit-paren (paren-str unitlist)
  "Search for the unit-paren whose paren-string is PAREN-STR in 
UNITLIST.  Returns (LIST1 UNIT LIST2) if search succeeds."
  (let ((running-list unitlist)
	(list1 nil) unit-paren list2
	(cont t))
    (while (and running-list cont)
      (let ((unit (car running-list)))
	(if (and (exp-unit-paren-p unit)
		 (string= paren-str (exp-unit-paren-string unit)))
	    ;; got it!
	    (setq unit-paren unit
		  list2 (cdr running-list)
		  cont nil)
	  ;; no, must continue
	  (setq list1 (append list1 (list unit))
		running-list (cdr running-list)))))
    (if cont
	;; search failed
	nil
      (list list1 unit-paren list2))))

(defun nf-search-unit-part (unit-part unitlist)
  "Search for UNIT-PART in UNITLIST.  Returns (LIST1 LIST2) if 
search succeeds."
  (let ((running-list unitlist)
	(list1 nil) list2
	(cont t))
    (while (and running-list cont)
      (let ((unit (car running-list)))
	(if (nf-eq unit-part unit)
	    ;; got it!
	    (setq list2 (cdr running-list)
		  cont nil)
	  ;; no, must continue
	  (setq list1 (append list1 (list unit))
		running-list (cdr running-list)))))
    (if cont
	;; search failed
	nil
      (list list1 list2))))


(setq CS2 nil)

(defun nf-match2 (eqs seqs env envs)
  "This function returns the first two solutions for the matching
equations given by EQS, SEQS and ENV.  The function fails if
there is at most one solution."
  (let ()
    (if nf-debug (setq CS2 (cons (list eqs seqs env) CS2)))
    (if (null eqs)
	(if (null seqs)
	    ;; since both eqs and seqs are empty, we have successfully
	    ;; solved the equations
	    (cons env envs)
	  ;; take the first equation in SEQS
	  (let ((seq (first seqs)))
	    (if nf-debug (setq SEQS seqs))
	    ;; check if SEQ is already solved
	    (if (dt-seq-solved-p seq)
		;; since SEQ is solved, we add the solution to ENV
		(let ((map (dt-seq-get-map seq))
		      (exp (dt-seq-get-exp seq))
		      (svar (dt-seq-get-svar seq)))
		  (nf-match2 eqs (cdr seqs)
			    (cons 
			     (list (dt-svar-intern svar) 
				   (exp-mk-abs-from-lmap map exp)) env) envs))
	      ;; we try to solve the first equation in SEQS.
	      (let ((svar (dt-seq-get-svar seq))
		    (n (dt-seq-get-arity seq))
		    (exp (dt-seq-get-exp seq)))
		;; create new seqs by considering possible instantiations of
		;; the MVAR (map variable)
		(let ((result 
		       ;; first, consider the case where MVAR is nil
		       ;; we create new equations for the newly commited
		       ;; position.
		       (catch 'fail 
			 (nf-match2 nil
				   (cons (nf-inst-to-nil seq)
					 (cdr seqs))
				   env envs))))
		  (if (or (stringp result)
			  (and
			   (< (length result) 2)
			   (setq envs result)))
		      ;; first case failed! so, consider the case where
		      ;; MVAR is an integer I such that 1 <= I <= N, where
		      ;; N is the arity of X
		      (let ((i 1) (cont t) result)
			(while (and (<= i n) cont)
			  (setq result
				;; consider the case where MVAR is I
				(catch 'fail 
				  (nf-match2 
				   (nf-inst-to-neweqs i seq)
				   (cons (nf-inst-to i seq) 
					 (cdr seqs))
				   env envs)))
			  (if (or (stringp result)
				  (and
				   (< (length result) 2)
				   (setq envs result)))
			      ;; since this case failed, we increment I
			      (setq i (1+ i))
			    ;; we now found a solution!
			    (setq cont nil)))
			(if (or (stringp result)
				(and
				 (< (length result) 2)
				 (setq envs result)))
			    ;; we must consider the final case where
			    ;; PATH is extended to a pair of paths
			    (nf-match2
			     eqs
			     (cons (nf-inst-to-pair seq) (cdr seqs))
			     env envs)
			  ;; return RESULT as our solution
			  result))
		    ;; found a solution
		    result))))))
      ;; eqs are non-empty, so try to solve the first equation
      (let* ((eq (first eqs))
	     (lhs (first eq))
	     (rhs (second eq))
	     (lvars (third eq)))
	;(setq EQ eq SEQS seqs)
	(cond ((exp-atom-p lhs)
	       ;; object level vars are constants essentially
	       (if (nf-eq lhs rhs)
		   (nf-match (cdr eqs) seqs env envs)
		 (throw 'fail (format "%s and %s do not match" lhs rhs))))
	      ((exp-svar-p lhs)
	       (let ((val (assoc (dt-svar-intern lhs) env)))
		 (if val
		     ;; the variable has a value in ENV, so create a new
		     ;; eq.
		     (let* ((newlhs (second val))
			    (neweqs
			     ;; if the new equation is trivial, we don't add
			     ;; it
			     (if (nf-eq newlhs rhs)
				 (cdr eqs)
			       (cons (list newlhs rhs 0) (cdr eqs)))))
		       (nf-match2 neweqs seqs env envs))
		   (if (and (exp-var-p rhs)
			    (exp-local-or-dummy-var-p rhs))
		       (throw 'fail 
			      (format "%s contains local or dummy variables" 
				      rhs))
		     ;; extend env and continue to solve the rest of EQS
		     (nf-match2 (cdr eqs) seqs
			       (cons (list (intern (exp-svar-dest lhs)) rhs) 
				     env envs)))))) 
	      ((exp-Abs-p lhs)
	       (if (exp-Abs-p rhs)
		   ;; instantiate abstractions and continue to match
		   (let ((lvar (list 'var (1+ lvars))))
		     (nf-match2 
		      (cons 
		       (list (Inst lhs lvar) 
			     (Inst rhs lvar)
			     (1+ lvars))
		       (cdr eqs))
		      seqs 
		      env envs))))
	      ((exp-sapp-p lhs)
	       (let* ((svar (exp-sapp-var lhs))
		      (plist (exp-sapp-args lhs))
		      (val (assoc (intern (exp-svar-dest svar)) env)))
		 (if val
		     ;; SVAR is already solved, so we construct
		     ;; a new equation
		     (nf-match2
		      (cons (list (exp-abs-app (second val) plist) rhs lvars)
			    (cdr eqs))
		      seqs env envs)
		   (let ((val2 (assoc svar seqs)))
		     (if val2
			 ;; extend seq for SVAR.  also add equations for
			 ;; the committed parts of the seq.
			 (let ((map (dt-seq-get-map (second val))))
			   (nf-match2
			    (append (nf-mk-eqs-for-map map plist rhs lvars) 
				    (cdr eqs))
			    (nf-extend-seqs seqs svar plist rhs lvars)
			    env envs))
		       ;; create a new seq
		       (nf-match2 
			(cdr eqs) 
			(cons (nf-mk-seq svar plist rhs lvars) seqs)
			env envs))))))
	      (t 
	       ;; LHS is a cons pair
	       (if (or (null rhs) (exp-atom-p rhs) (exp-svar-p rhs)
		       (exp-const-p rhs) (exp-Abs-p rhs))
		   (throw 'fail (format "%s and %s do not match" lhs rhs))
		 (let ((lhs1 (car lhs)) (rhs1 (car rhs))
		       (lhs2 (cdr lhs)) (rhs2 (cdr rhs)))
		   ;; avoid adding trivial equations.
		   (if (nf-eq lhs1 rhs1)
		       (if (nf-eq lhs2 rhs2)
			   (nf-match2 (cdr eqs) seqs env)
			 (nf-match2
			  (cons (list lhs2 rhs2 lvars) (cdr eqs)) 
			  seqs env envs))
		     (if (nf-eq lhs2 rhs2)
			 (nf-match2
			  (cons (list lhs1 rhs1 lvars) (cdr eqs)) 
			  seqs env envs)
		       (nf-match2
			(cons (list lhs1 rhs1 lvars)
			      (cons (list lhs2 rhs2 lvars) (cdr eqs)))
			seqs env envs)))))))))))

(defun nf-mk-eqs-for-map (map plist exp lvars)
  "construct equations for each committed part in MAP."
  (setq MPEL (list map plist exp lvars))
  (let ((eqs nil))
    (while map
      (let* ((path (first-but-last (car map)))
	     (item (exp-list-get-last (car map)))
	     (rhs (exp-destruct exp path)))
	(if (integerp item)
	    ;; construct an equation for pattern matching
	    (setq eqs
		  (cons (list (nth (1- item) plist)
			      rhs
			      lvars)
			eqs))
	  ;; check if the commited part is equal to the corresponding
	  ;; part of EXP
	  (or (nf-eq item rhs)
	      (throw 'fail (format "%s and %d do not match" item rhs))))))
    eqs))

(defun nf-inst-to-nil (seq)
  "Instantiate the lowest rank variable position in SEQ to nil and
construct a new seq."
  (let ((svar (dt-seq-get-svar seq))
	(plist (dt-seq-get-plist seq))
	(exp (dt-seq-get-exp seq))
	(list (dt-seq-get-list seq))
	(map (dt-seq-get-map seq)))
    (setq SQ seq L1 list)
    ;; each element of LIST is of the form: 
    ;; (<path> (<exp> <lvars>) ... (<exp> <lvars>))
    (let* ((el-list (cdr (car list)))
	   (path (first (car list)))
	   (exp1 (first (car el-list))))
      (while el-list
	(let ((exp (first (car el-list)))
	      (el2 (second el-list)))
	  ;; check that EXP is free from local variables
	  ;; it is ok for EXP to have DT-DUMMY-VAR
	  (nf-lvars-free exp)
	  ;; if the length of EL-LIST > 1, then the EXP must be
	  ;; equal to the expression of the second element of EL-LIST
	  (if el2
	      (or (nf-eq exp (first el2))
		  (throw 'fail 
			 (format "%s and %s do not match" exp (first el2)))))
	  (setq el-list (cdr el-list))))
      ;; extend map by adding the newly committed part
      (setq map (cons (append path (list exp1)) map)))
    (dt-mk-seq svar plist exp (cdr list) map)))

(defun exp-local-or-dummy-var-p (var)
  "Check if VAR is a local or dummy variable."
  (integerp (exp-var-dest var)))

(defun exp-local-var-p (var)
  "Assuming that VAR is a variable, check if it is a local variable.
DT-DUMMY-VAR is not considered to be a local variable, since it should
be invisible."
  (let ((var-name (exp-var-dest var)))
    (and (integerp var-name)
	 (> var-name 0))))

(defun nf-lvars-free (e)
  "Check if E is free from local variables. Fail, if E contains a
local variable"
  (cond ((exp-var-p e) 
	 (if (exp-local-var-p e)
	     (throw 'fail (format "%s is a local variable" e))))
	((exp-sharp-var-p e) nil)
	((exp-const-p e) nil)
	((exp-Abs-p e) 
	 (let ((e1 (fourth e))) (nf-lvars-free e1)))
	(t (let ((e1 (car e)) (e2 (cdr e)))
	     (nf-lvars-free e1)
	     (nf-lvars-free e2)))))

(defun nf-inst-to-neweqs (i seq)
  "Instantiate the lowest rank position in SEQ to the I-th pattern
and construct new eqs."
  (let ((plist (dt-seq-get-plist seq))
	(exp (dt-seq-get-exp seq))
	(list (dt-seq-get-list seq)))
    ;; each element of LIST is of the form: 
    ;; (<path> (<exp> <lvars>) ... (<exp> <lvars>))
    ;; for debug
    (setq SEQ seq)
    (let ((el-list (cdr (car list))) eqs)
      (while el-list
	(let ((exp (first (car el-list)))
	      (lhs (nth (1- i) (first plist)))
	      (lvars (second (car el-list))))
	  (or (nf-eq lhs exp) ;; equation already solved!
	      (setq eqs (cons (list lhs exp lvars) eqs)))
	  (setq el-list (cdr el-list)
		plist (cdr plist))))
      eqs)))

(defun nf-inst-to (i seq)
  "Instantiate the lowest rank position in SEQ to the I-th pattern and
construct a new seq."
  (let* ((svar (dt-seq-get-svar seq))
	 (plist (dt-seq-get-plist seq))
	 (exp (dt-seq-get-exp seq))
	 (list (dt-seq-get-list seq))
	 ;; each element of LIST is of the form: 
	 ;; (<path> (<exp> <lvars>) ... (<exp> <lvars>))
	 (map (cons (append (car (car list)) (list i))
		      (dt-seq-get-map seq))))
    (dt-mk-seq svar plist exp (cdr list) map)))

(defun nf-inst-to-pair (seq)
  "Instantiate the first item of the list-part in SEQ to a pair and 
construct a new seq.  The case where the postion to be decomposed is an 
abstract is handled separately."
  (let ((svar (dt-seq-get-svar seq))
	(plist (dt-seq-get-plist seq))
	(exp (dt-seq-get-exp seq))
	(list (dt-seq-get-list seq))
	(map (dt-seq-get-map seq)))
    ;; each element of LIST is of the form: 
    ;; (<path> (<exp1> <lvars>) ... (<expn> <lvars>))
    ;; hard coding
    (let* ((el-list (cdr (car list))) 
	   (path (car (car list)))
	   (exp1 (first (first el-list)))
	   (lvars1 (second (first el-list))))
      (if (exp-atom-p exp1)
	  (throw 'fail (format "cannot destruct an atom: %s" exp1)))
      (if (exp-Abs-p exp1)
	  ;; EXPi must all be abstractions, and should match as abstractions
	  (let ((new-el-list nil))
	    (while el-list
	      (let ((abs1 (first (car el-list)))
		    (el2 (second el-list)))
		(if el2
		    (let ((abs2 (first el2))
			  (map1 (exp-Abs-map abs1)))
		      ;; the conditions below must all be satisfied.
		      (and
		       (or (exp-Abs-p abs2)
			   (throw 'fail
				  (format "%s should be an abstract" abs2)))
		       (let ((map2 (exp-Abs-map abs2)))
			 (or (equal map1 map2)
			     (throw 'fail
				    (format "Maps %s and %s do not match"
					    map1 map2)))))))
		;; now extend NEW-EL-LIST by making a new constraint for the
		;; body-part of ABS1. we note that ABS1 = (Abs var map body)
		;; and we can access the body-part by the path (B).
		(setq new-el-list
		      (cons (list (exp-Abs-body abs1) lvars1)
			    new-el-list))
		(setq el-list (cdr el-list))))
	    ;; construct new SEQ by adding the new constraint at the end of
	    ;; LIST
	    (dt-mk-seq svar plist exp
		       (append (cdr list)
			       (list
				(cons (append path '(B)) 
				      ;; don't forget to reverse the list
				      (reverse new-el-list))))
		       map))
	;; EXP1 is a cons-pair, so extend LIST by adding constraints for
	;; the left and right parts of EXP1
	(dt-mk-seq svar plist exp
		   (append (cdr list)
			   (list
			    (cons (append path '(L))
				  (mapcar (function nf-inst-to-left) el-list))
			    (cons (append path '(R))
				  (mapcar (function nf-inst-to-right) el-list))
			    ))
		   map)))))

(defun nf-inst-to-left (item)
  "this function refers to the dynamically surrounding variable SEQ."
  ;; for debug
  ;;(setq SEQ seq)
  (let ((exp (first item))
	(lvars (second item)))
    (if (exp-atom-p exp)
	(throw 'fail (format "%s: atom cannot be decomposed" exp))
      (list (car exp) lvars))))

(defun nf-inst-to-right (item)
  (let ((exp (first item))
	(lvars (second item)))
    (list (cdr exp) lvars)))

(defun nf-mk-seq (svar args exp lvars)
  "Make an equation for the schematic variablle SVAR which when applied
to ARGS yields EXP."
  (list svar (list args) exp (list (list () (list exp lvars))) nil nil))

(defun nf-extend-seqs (seqs svar args rhs lvars)
  "Find an seq in SEQS for SVAR and extend the seq."
  (let ((cont t) result)
    (while cont
      (let ((seq (first seqs)))
	(if (equal svar (first seq))
	    (let* ((plist (dt-seq-get-plist seq))
		   (exp (dt-seq-get-exp seq))
		   (list (dt-seq-get-list seq))
		   (map (dt-seq-get-map seq))
		   (extend-list 
		    (lambda (item)
		      (let ((path (car item))
			    (el-list (cdr item)))
			(cons path
			      ;; extract sub-expression from RHS
			      (cons (list (exp-destruct rhs path) lvars)
				    el-list)))))
		   (check-list 
		    (lambda (item) 
		      (exp-destruct exp (first-but-last item))
		      item)))
	      (setq result
		    (cons
		     (dt-mk-seq
		      svar
		      (cons args plist)
		      exp
		      (mapcar extend-list list)
		      (mapcar check-list map))
		     (append result (cdr seqs))))
	      (setq cont nil))
	  (setq result (cons seq result)
		seqs (cdr seqs)))))
    result))

;; debugging tools

(defun nf-CS-assoc (var)
  (let ((list CS) (cont t) v)
    (while (and cont list)
      (if (and (setq v (assoc var (third (car list))))
	       (eq 'sabs (car (second v))))
	  (setq cont nil)
	(setq list (cdr list))))
    list))

(defun PS (exp cat)
  (let ((debug-on-error nil))
    (ParseString exp cat)))

(provide 'nf)

