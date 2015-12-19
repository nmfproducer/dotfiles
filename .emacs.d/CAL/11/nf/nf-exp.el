;; this file defines the data structure of NF expressions and functions
;; handling them.

;; selector functions for Lisp lists

(defun but-1st (x) (cdr x))
(defun 1st (x) (nth 0 x))
(defun 2nd (x) (nth 1 x))
(defun 3rd (x) (nth 2 x))
(defun 4th (x) (nth 3 x))
(defun 5th (x) (nth 4 x))

(defun but-last (x) 
  (if (null x) x
    (if (null (but-1st x)) ()
      (cons (1st x) (but-last (but-1st x))))))

;; special constant

(defconst op-hj '(op "⇒"))

;;;;;;;;;;;;;;;;
;;            ;;
;; expression ;;
;;            ;;
;;;;;;;;;;;;;;;;
;; <exp> ::= <atom>
;;        |  <abstraction>
;;        |  <pair>

;; the following function should rarely be needed, but, we supply it
;; for the sake of completeness

(defun exp-p (x)
  (or (exp-atom-p x)
      (exp-Abs-p x)
      (exp-pair-p x)))

;;;;;;;;;;
;;      ;;
;; pair ;;
;;      ;;
;;;;;;;;;;

(defun exp-pair-p (x)
  (and (not (exp-atom-p x)) (not (exp-Abs-p x))))

;;;;;;;;;;
;;      ;;
;; atom ;;
;;      ;;
;;;;;;;;;;
;; <atom> ::= <varref> | <const>

(defun exp-atom-p (exp)
  (or (exp-varref-p exp) 
      (exp-const-p exp)
      ))

;;;;;;;;;;;;;;;
;;           ;;
;; variable  ;;
;; reference ;;
;;           ;;
;;;;;;;;;;;;;;;
;; <varref> ::= <var> | <#var>

(defun exp-varref-p (exp)
  "Check if EXP is a variable reference."
  (or (exp-var-p exp) (exp-sharp-var-p exp)))

(defun exp-varref-with-pos-info-p (varref)
  "Check if #-var VAR contains position information."
  (integerp (car (last varref))))

(defun exp-varref-dest (varref)
  (if (exp-var-p varref)
      (exp-var-dest varref)
    (exp-sharp-var-dest varref)))

(defun exp-varref-var-part (varref)
  "Returns the variable which VARREF refers to."
  (if (exp-var-p varref) varref
    (if (exp-varref-with-pos-info-p varref)
	(list 'var (exp-list-get-last2 varref))
    (list 'var (exp-list-get-last varref)))))

(defun exp-varref-pos (varref)
  "Return the position of VARREF, if VARREF contains info on pos,
return NIL otherwise."
  (let ((pos (car (last varref))) (len (length varref)))
    (if (integerp pos)
	;; VARREF = (var name pos) or (varref # ... # name pos),
	;; and for #X we return pos-1, for ##2 we return pos-2, etc.
	(- pos (- len 2))
      nil)))

;;;;;;;;;;;;;;
;;          ;;
;; variable ;;
;;          ;;
;;;;;;;;;;;;;;
;; <var> ::= (var <name>)

(defun exp-var-p (exp)
  (and (listp exp)
       ;; since we assume that parser is reliable, we don't check below
       ;; (also, if VAR is with info, its length is 3
       ;;(eq (length exp) 2) 
       (eq (1st exp) 'var)))

;; <- 文字列

(defun exp-var-cons (str)
  (list 'var str))

;; -> 文字列 (文字列のリストではない！）
(defun exp-var-dest (var)
  (2nd var))

(defun exp-varseq-p (list)
  "Check if LIST is a list of variables."
  (let ((cont t))
    (while (and cont list)
      (if (exp-var-p (first list))
	  ;; ok, continue
	  (setq list (cdr list))
	;; discontinue
	(setq cont nil)))
    cont))

;;;;;;;;;;;;;;;;
;;            ;;
;; # variable ;;
;;            ;;
;;;;;;;;;;;;;;;;
;; <#var> ::= (varref "#" ... "#" <name>) ;; must contain at least one #
;;         |  (varref "#" ... "#" <name> pos) ;; must contain at least one #

(defun exp-sharp-var-p (exp)
  (and (listp exp)
       (> (length exp) 2) 
       (eq (1st exp) 'varref)
       ;; we don't check this
       ;; (exp-sharplist-butlast-p (but-1st exp))
       ))

(defun exp-remove-pos-info (sharpvar)
  "Remove position information from SHARPVAR."
  (if (integerp (car (last sharpvar)))
      (reverse (cdr (reverse sharpvar)))
    sharpvar))

(defun exp-sharp-var-to-var (sharpvar)
  (list 'var (exp-list-get-last (exp-remove-pos-info sharpvar))))

(defun exp-sharp-var-dest (sharpvar)
  (apply (function concat) (cdr (exp-remove-pos-info sharpvar))))

(defun exp-sharp-var-sharps (sharpvar)
  "return the number of #'s in SHARPVAR"
  (- (length (exp-remove-pos-info sharpvar)) 2))

(defun exp-pull-sharp-var (exp)
  "Remove a sharp from sharp var EXP."
  (setq exp (exp-remove-pos-info exp))
  (if (= (length exp) 3)
      ;; we must make a variable
      (cons 'var (cdr (cdr exp)))
    (cons 'varref (cdr (cdr exp)))))

;; Remove candidate
;;(defun exp-sharplist-butlast-p (tree)
  ;;(let ((x (1st tree))
	;;(y (but-1st tree)))
    ;;(if (null y) t
      ;;(and (equal x "#")
	   ;;(exp-sharplist-butlast-p y)))))

;;;;;;;;;;;;;;
;;          ;;
;; constant ;;
;;          ;;
;;;;;;;;;;;;;;
;; a constant is a non-variable atom
;; <constant> ::= <nil> | (op <string>) | (tok <token>)

(defun exp-const-p (x)
  (or
   (exp-null x)
   (and (listp x) 
	(or (eq (car x) 'op) 
	    (eq (car x) 'sexp)
	    (eq (car x) 'unit)))))

(defun exp-op-p (x)
  "check if X is an OP."
  (and (listp x)
       (eq (car x) 'op)))

(defun exp-op-eq (x y)
  "Assuming that X, Y are both OP, compare them (by ignoring the
position information)."
  (equal (second x) (second y)))

(defun exp-unit-p (x)
  "check if X is a UNIT."
  (and (listp x)
       (eq (car x) 'unit)))

(defun exp-unit-eq (x y)
  "Assuming that X, Y are both UNIT, compare them (by ignoring the
position information)."
  ;; UNIT ::= (unit UNIT-CORE)
  ;; UNIT-CORE ::= (var STR &optional POS)
  ;;            |  (op  STR &optional POS)
  ;;            |  (sexp  SEXP &optional POS)
  ;;            |  (paren STR UNIT-CORE ... UNIT-CORE)
  (exp-unit-core-eq (second x) (second y)))

(defun exp-unit-core-eq (l r)
  "Compare UNIT-CORE's L and R."
  (let* ((l-head (car l)) (r-head (car r)))
    (if (or (eq l-head 'var) (eq l-head 'op) (eq l-head 'sexp))
	(and (eq l-head r-head) (equal (second l) (second r)))
      ;; L-HEAD = paren
      (and (eq r-head 'paren)
	   ;; paren name must be equal
	   (equal (second l) (second r))
	   ;; the rest of the two lists must be equal
	   (let ((l-list (cddr l)) (r-list (cddr r))
		 (cont t) (result t))
	     (if (= (length l-list) (length r-list))
		 (progn
		   (while (and cont l-list)
		     (let ((l-item (car l-list)) (r-item (car r-list)))
		       (if (exp-unit-core-eq l-item r-item)
			   (setq l-list (cdr l-list)
				 r-list (cdr r-list))
			 (setq cont nil
			       result nil))))
		   result)
	       ;; two lists are of different length, so return NIL
	       nil))))))

(defun exp-sexp-p (x)
  "check if X is an SEXP."
  (and (listp x)
       (eq (car x) 'sexp)))

(defun exp-sexp-eq (x y)
  "Assuming that X, Y are both SEXP, compare them (by ignoring the
position information).  Currently, we assume that SEXP's do not
contain pos info."
  (equal (second x) (second y)))

;; we declare special constants, which are used to construct derivations.

(defconst exp-CD
  ;; constant for compositional derivation
  '(op "CD"))

(defconst exp-HD
  ;; constant for hypothetical derivation
  '(op "HD"))

;;;;;;;;;
;;     ;;
;; nil ;;
;;     ;;
;;;;;;;;;
;; <nil> ::= nil
;; we implement NF expression <nil> by Lisp object nil.

(defun exp-null (x)
  (null x))

;;;;;;;;;;;;;;;;;
;;             ;;
;; abstraction ;;
;;             ;;
;;;;;;;;;;;;;;;;;
;; (Abs (<var> ... <var>) <map> <body>)

(defun exp-mk-Abs (vars map body)
  (list 'Abs vars map body))

(defun exp-Abs-p (x)
  (and (listp x) 
       (eq (car x) 'Abs)))

(defun exp-Abs-vars (x)
  "Return the list of bound variable of X."
  (second x))

(defun exp-Abs-var (x)
  "Assuming that X is a unary abstraction, the function returns the 
bound variable of X."
  (car (second x)))

(defun exp-Abs-map (x)
  (3rd x))

(defun exp-Abs-body (x)
  (4th x))

;;;;;;;;;
;;     ;;
;; map ;;
;;     ;;
;;;;;;;;;
;; <map> ::= <nil> | <int> | (<map> . <map>)

(defun map-p (m)
  (or (map-null m)
      (map-int m)
      (and (listp m)
	   (map-p (car m))
	   (map-p (cdr m)))))

(defconst map-nil nil)

(defun map-null (m)
  (equal m map-nil))

(defun map-int (m)
  (and (integerp m) (>= m 0)))

(defun map-cons (x y)
  (if (and (map-null x) (map-null y))
      nil
    (cons x y)))

(defun map-mk-holes (e map)
  "Replace the positions in Exp specified by MAP to DT-DUMMY-VAR."
  (cond ((map-int map) dt-dummy-var)
	((map-null map) e)
	(t (let ((e1 (car e)) (e2 (cdr e))
		 (m1 (car map)) (m2 (cdr map)))
	     (cons (map-mk-holes e1 m1) (map-mk-holes e2 m2))))))

;; abstractions
;; example
;; (PS "(x,x)[<x,#x,##x>]" 'exp)
;; (Abs ((var "x") (var "x")) (2 1) ((var 0) (var 0) (var "x")))

(setq MA nil)

(defun mabs (x e)
  "Abstract E with respect to list X of variables.  This function is
sometimes called when reading in NF rules, so E may contain schematic
applications."
  (if nf-debug (setq MA (cons (list x e) MA)))
  (cond ((or (exp-var-p e) (exp-sharp-var-p e))
	 (let ((i 0) (n (length x)) (found nil) (x (reverse x)) (exp e))
	   (while (and x (not found))
	     (let ((var (car x)))
	       (if (nf-eq var exp)
		   (setq found t)
		 (if (nf-eq var (exp-varref-var-part e))
		     ;; remove one # from EXP
		     (setq exp (exp-pull-sharp-var exp)))
		 (setq i (1+ i)
		       x (cdr x)))))
	   (if found 
	       (list (- n i) dt-dummy-var)
	     (list map-nil exp))))
	((exp-sapp-p e)
	 ;; in this case, the expression to be abstarcted is of the form
	 ;; (sapp <svar> <args>)
	 (let* ((svar (exp-sapp-var e))
		(args (exp-sapp-args e))
		(map-body (mabs x args))
		(map (first map-body))
		(body (second map-body)))
	   (list
	    (map-cons nil (map-cons nil (map-cons map map-nil)))
	    (list 'sapp svar body))))
	((exp-atom-p e) (list map-nil e))
	((exp-Abs-p e) 
	 (let* ((v (second e)) (m (third e)) (e1 (fourth e))
		(a1 (mabs x e1)) (m1 (first a1)) (b1 (second a1)))
	   (list 
	    (map-cons map-nil 
		      (map-cons map-nil 
				(map-cons map-nil (map-cons m1 map-nil))))
	    (list 'Abs v m b1))))
	(t (let* ((e1 (car e)) (e2 (cdr e))
			  (a1 (mabs x e1)) (a2 (mabs x e2))
			  (m1 (first a1)) (m2 (first a2))
			  (b1 (second a1)) (b2 (second a2)))
		     (list (map-cons m1 m2) (cons b1 b2))))))

(defun mAbs (x e)
  "X is a variable."
  (let ((map-body (mabs (list x) e)))
    (list 'Abs (list x) (first map-body) (second map-body))))

(defun Inst (a e)
  ;; a == (Abs X M B) 
  (exp-map-body-inst1 (third a) (fourth a) e))

(defun abs-inv (m b v) 
  (exp-map-body-inst1 m (exp-push-var b v) v))

(defun abs-Inv (a v)
  "Compute inverse of an abstract A w.r.t. the variable V."
  ;; a == (Abs X M B) 
  (let ((map (third a))
	(body (fourth a)))
    (exp-map-body-inst1 map (exp-push-var body v) v)))

(defun exp-push-var (e var)
  "Push expression E through variable VAR.  The function is also called
when instantiating schematic expression, so E is, in general,
a schematic expression."
  (cond ((exp-var-p e) 
	 (if (nf-eq var e) 
	     ;; add one sharp
	     (exp-add-sharp e)
	   e))
	((exp-sharp-var-p e)
	 (if (nf-eq var (exp-varref-var-part e))
	     ;; add one sharp
	     (exp-add-sharp e)
	   e))
	((exp-svar-p e)
	 ;; meta variables are not affected by pushing
	 e)
	((exp-sapp-p e)
	 (let ((svar (exp-sapp-var e))
	       (args (exp-sapp-args e)))
	   (exp-mk-sapp svar (exp-push-var args var))))
	((exp-atom-p e) e)
	((exp-Abs-p e) 
	 (let ((v (second e)) (m (third e)) (e1 (fourth e))) 
	   (list 'Abs v m (exp-push-var e1 var))))
	(t (let ((e1 (car e)) (e2 (cdr e)))
	     (cons (exp-push-var e1 var) (exp-push-var e2 var))))))

(defun nf-seq (e f)
  "Compare schematic expressions E and F."
  (cond ((exp-varref-p e) 
	 (if (exp-varref-p f)
	     (exp-varref-eq e f)
	   nil))
	((exp-null e) (exp-null f))
	;; an OP may have position information
	((exp-op-p e) (and (exp-op-p f) (exp-op-eq e f)))
	((exp-sexp-p e) (and (exp-sexp-p f) (exp-sexp-eq e f)))
	((exp-unit-p e) (and (exp-unit-p f) (exp-unit-eq e f)))
	((exp-Abs-p e)
	 (if (exp-Abs-p f)
	     (let ((m (third e)) (e1 (fourth e)) 
		   (n (third f)) (f1 (fourth f)))
	       ;; we don't care about names of bound variables
	       (and (equal m n)
		    (nf-seq e1 f1)))
	   nil))
	((exp-svar-p e)
	 (if (exp-svar-p f)
	     (equal (exp-svar-dest e) (exp-svar-dest f))
	   nil))
	((or (exp-sapp-p e)
	     (exp-qq-p e)) 
	 nil)
	(t
	 ;; in this case, E is an NF pair
	 (if (or (exp-atom-p f) (exp-Abs-p f))
		 nil
	     (let ((e1 (car e)) (f1 (car f)) 
		   (e2 (cdr e)) (f2 (cdr f)))
	       (and (nf-seq e1 f1) (nf-seq e2 f2)))))))

(defun exp-push-vars (e vars)
  "Push expression E through list of variables VARS."
  (let ((result e))
    (while vars
      (setq result (exp-push-var result (car vars))
	    vars (cdr vars)))
    result))

(defun exp-mk-abs-from-lmap (lmap exp)
  "construct a schematic abstraction from LMAP, which is a linear 
representation of map, and EXP. we don't replace
the abstracted part of EXP by 0 since that part is irrelevant in the
computation of instantiation."
  (if nf-debug (setq LM lmap EX exp))
  (let ((map (exp-lmap-to-map lmap)))
    (list 'Abs nil map (map-mk-holes exp map))))

(defun exp-abs-app (abs args)
  "apply abstract ABS to list of arguments ARGS"
  (let ((m (exp-Abs-map abs)) ; map
	(b (exp-Abs-body abs))) ; body 
    (exp-map-body-inst m b args)))

(defun exp-map-body-inst (m b args)
  "Instantiate Body abstracted by Map wrt ARGS"
  (cond ((map-null m) b)
	((map-int m) (nth (1- m) args))
	(t (let ((m1 (car m)) (m2 (cdr m)) 
		 (b1 (car b)) (b2 (cdr b)))
	     (cons (exp-map-body-inst m1 b1 args) 
		   (exp-map-body-inst m2 b2 args))))))

(defun exp-map-body-inst1 (m b arg)
  "Instantiate Body abstracted by Map wrt an ARG"
  (exp-map-body-inst m b (list arg)))

(defun exp-lmap-to-map (lmap)
  "conver LMAP to ordinary map, wher LMAP is a list whose elements are
of the form <path>*(<int> | <exp>).
For example, ((L R 1) (R 2)) is converted to the map ((nil . 1) . 2)."
  (cond ((null lmap) nil)
	((and (= (length lmap) 1) (= (length (first lmap)) 1))
	 ;; in this case PMAP = ((<item>)), where <item> is an interger
	 ;; or an expression.
	 (let ((item (first (first lmap))))
	   (if (integerp item) item nil)))
	(t
	 (let ((cut (exp-lmap-cut lmap)))
	   (cons (exp-lmap-to-map (first cut)) 
		 (exp-lmap-to-map (second cut)))))))

(defun exp-lmap-cut (lmap)
  "cut LMAP into left part and right part."
  (let ((list lmap) left right)
    (while list
      (let* ((pi (first list))
	     (LRB (first pi))
	     (rest (cdr pi)))
	(cond ((eq LRB 'L) (setq left (cons rest left)))
	      ((eq LRB 'R) (setq right (cons rest right)))
	      ((eq LRB 'B) 
	       ;; B = R R R L, is the body part of an abstraction
	       (setq right (cons (append '(R R L) rest) right)))
	      (t (error "dt-lmap-cut error")))
	(setq list (cdr list))))
    (list left right)))

;;;;;;;;;;;;;;;;;;;;;;;;
;;                    ;;
;; Schema application ;;
;;                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;
;; <sapp> ::= (sapp <svar> <args>)

(defun exp-sapp-p (tree)
  (and (listp tree) (eq (car tree) 'sapp)))

(defun exp-sapp-var (tree)
  (2nd tree))

(defun exp-sapp-args (tree)
  (3rd tree))

(defun exp-mk-sapp (S T)
  (list 'sapp S T))

;;;;;;;;;;;;;;;;;;;;;
;;                 ;;
;; Quasi Quotation ;;
;;                 ;;
;;;;;;;;;;;;;;;;;;;;;
;; <qq> ::= (qq <body>)

(defun exp-qq-p (sexp)
  (and (listp sexp) (eq (car sexp) 'qq)))

(defun exp-qq-body (sexp)
  (second sexp))

;; (unit (paren PAREN-STR . LIST))
(defun exp-unit-paren-p (exp)
  (and (listp exp)
       (eq 'unit (first exp))
       (eq 'paren (first (second exp)))))

(defun exp-unit-paren-string (exp)
  (second (second exp)))

(defun exp-unit-paren-list (exp)
  (cddr (second exp)))



;;;;;;;;;;;;;;
;;          ;;
;; equality ;;
;;          ;;
;;;;;;;;;;;;;;
;; equality of NF expressions.

(defun nf-eq (e f)
  "Compare schematic expression E against an expression F.
If E is not an expression, then the result is always NIL.  If both
E and F are ordinary expressions, the result is T if and only if
they are definitionally equal.  So, for example, alpha equivalent expressions
are equal."
  (cond ((exp-varref-p e) 
	 (if (exp-varref-p f)
	     (exp-varref-eq e f)
	   nil))
	((exp-null e) (exp-null f))
	;; an OP may have position information
	((exp-op-p e) (and (exp-op-p f) (exp-op-eq e f)))
	((exp-sexp-p e) (and (exp-sexp-p f) (exp-sexp-eq e f)))
	((exp-unit-p e) (and (exp-unit-p f) (exp-unit-eq e f)))
	((exp-Abs-p e)
	 (if (exp-Abs-p f)
	     (let ((m (third e)) (e1 (fourth e)) 
		   (n (third f)) (f1 (fourth f)))
	       ;; we don't care about names of bound variables
	       (and (equal m n)
		    (nf-eq e1 f1)))
	   nil))
	((or (exp-svar-p e) 
	     (exp-sapp-p e)
	     (exp-qq-p e)) 
	 nil)
	(t
	 ;; in this case, E is an NF pair
	 (if (or (exp-atom-p f) (exp-Abs-p f))
		 nil
	     (let ((e1 (car e)) (f1 (car f)) 
		   (e2 (cdr e)) (f2 (cdr f)))
	       (and (nf-eq e1 f1) (nf-eq e2 f2)))))))

(defun exp-varref-eq (e f)
  "compare varrefs E and F, by ignoring possible info on
position, which is stored as the last element of the varref."
  (let ((cont t) result)
    (while cont
      (let ((l (car e)) (r (car f)))
	(if (or (eq l 'var)
		(eq l 'varref)
		(equal l "#"))
	    (if (equal l r) 
		;; we just continue while
		(setq e (cdr e)
		      f (cdr f))
	      ;; E and F are distinct
	      (setq result nil ;; this is not necessary, but...
		    cont nil))
	  ;; now, L must be the name of the variable, and
	  ;; must be equal to R, otherwise, E and F are distinct
	  (if (equal l r)
	      (setq result t cont nil)
	    (setq result nil cont nil)))))
    result))

(setq NI nil)

(defun nf-inst (e env &optional context)
  "Instantiate schematic expression E under environment ENV.  Optional
argument CONTEXT is a list of local variables surrounding E, through
which ENV is visible."
  (if nf-debug (setq NI (cons (list e env context) NI)))
  (cond ((exp-svar-p e) 
	 ;; the variable E must have a value in ENV.
	 (let ((v (assoc (intern (exp-var-dest e)) env)))
	   (if v (exp-push-vars (second v) context)
	     (error "Schematic variable %s doesn't have a value" e))))
	((exp-sapp-p e)
	 (let ((abs (nf-inst (exp-sapp-var e) env context))
	       (args (nf-inst (exp-sapp-args e) env context))
	       )
	   (if (exp-Abs-p abs)
	       (exp-abs-app abs args)
	     (error "「%s」は抽象式でありません" abs))))
	((exp-qq-p e)
	 (let ((toklist (exp-qq-body e)) (result nil))
	   (while toklist
	     (let ((item (car toklist)))
	       (if (exp-svar-p item)
		   (setq result
			 (append result
				 (nf-inst item env context)))
		 ;; ITEM = (unit TOKEN) or (unit (paren PAREN . LIST))
		 (if (exp-unit-paren-p item)
		     (let ((paren-string (exp-unit-paren-string item))
			   (list (exp-unit-paren-list item)))
		       (setq result
			     (append result
				     (list
				      (list
				       'unit
				       (cons 
					'paren
					(cons 
					 paren-string
					 (mapcar
					  (function second)
					  ;; since each element of the
					  ;; following list is of the form
					  ;; (unit ...), we take the second
					  ;; of the item by mapcar
					  (nf-inst
					   (list 'qq
						 (mapcar
						  (lambda (x)
						    (if (eq 'svar (car x))
							x
						      (list 'unit x)))
						  ;; LIST
						  list))
					   env)))))))))
		   (setq result
			 (append result (list item))))))
	       (setq toklist (cdr toklist)))
	   result))
	((exp-atom-p e) e)
	((exp-Abs-p e)
	 ;; push ENV thru the bound variable, instantiate body, 
	 ;; and abstract again
	 (let ((v (exp-Abs-var e)))
	   (mAbs v (nf-inst (abs-Inv e v) env (cons v context)))))
	(t (let ((e1 (car e)) (e2 (cdr e)))
	     (cons (nf-inst e1 env context) (nf-inst e2 env context))))))

;;;;;;;;;;;;;;;;;;;
;;               ;;
;; list handlers ;;
;;               ;;
;;;;;;;;;;;;;;;;;;;

(defun nf-append (listA listB)
  "Prepend elements in LISTA not in LISTB to LISTB."
  (let ((result listB))
    (while listA
      (let ((item (car listA)))
	(unless (nf-member item result)
	  (setq result (cons item result)))
	(setq listA (cdr listA))))
    result))

(defun nf-member (item list)
  "Check if ITEM is a membe of LIST, using NF-EQ."
  (let ((cont t) (result nil))
    (while (and cont list)
      (if (nf-eq item (car list))
	  (setq result t
		cont nil)
	(setq list (cdr list))))
    result))

;;;;;;;;;;;;;;;;;;;
;;               ;;
;; exp to string ;;
;;               ;;
;;;;;;;;;;;;;;;;;;;

(defun exp-to-string-varref (exp)
  (if (exp-varref-p exp)
      (exp-varref-dest exp)
    (throw 'exp-to-string "変数参照でありません．")))

(provide 'nf-exp)
