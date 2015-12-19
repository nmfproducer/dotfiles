;; utility functions for derivation games

(defun cal-q-var-p (x)
  "check if X is of the form ((unit (var ...)))."
  (and (listp x)
       (= (length x) 1)
       (let ((u (car x)))
	 (and (listp u)
	      (= (length u) 2)
	      (eq 'unit (car u))
	      (let ((v (second u)))
		(and (listp v)
		     (eq 'var (car v))))))))

(defun cal-q-varref-p (x)
  "check if X is a list whose last elem is (unit (var ...)) preceded by 
a sequence of (unit (op #))"
  (and (listp x)
       (not (null x))
       (if (= (length x) 1)
	   (cal-q-var-p x)
	 (if (nf-eq '(unit (op "#")) (car x))
	     (cal-q-varref-p (cdr x))
	   nil))))

(defun cal-q-exp-const-p (x list)
  "check if X is of the form ((unit (op XX))), and XX is
a constant in LIST."
  (and (listp x)
       (= (length x) 1)
       (let ((u (car x)))
	 (and (listp u)
	      (= (length u) 2)
	      (eq 'unit (car u))
	      (let ((v (second u)))
		(and (listp v)
		     (eq 'op (car v))
		     (member (second v) list)))))))

(defun cal-q-nonvoid-p (e)
  (if  (null e) nil t))

(defun cal-q-in (item list)
  "Check if ITEM is a member of LIST, where ITEM is a unitlist of
length 1."
  (nf-member (car item) list))

(defun cal-q-is-in (item list)
  "Check if ITEM refers to a member of LIST, where ITEM is a unitlist of
the form #...# UNIT, where # occurs k times.  The result is T if LIST
contains UNIT at least (k+1) times."
  (let ((k 1) unit (cont t) (go-on t) (result nil))
    (while cont
      (if (null item)
	  (setq cont nil
		go-on nil)
	(let ((u (first item)))
	  (if (nf-eq '(unit (op "#")) u)
	      (setq k (1+ k)
		    item (cdr item))
	    (if (= (length item) 1)
		(setq unit (first item)
		      cont nil)
	      ;; length must be 1
	      (setq cont nil
		    go-on nil))))))
    (if go-on
	(progn
	  (setq cont t)
	  (while (and cont list)
	    (if (nf-eq unit (first list))
		(if (= k 1)
		    ;; ok!
		    (setq result t
			  cont nil)
		  (setq k (1- k)
			list (cdr list)))
	      (setq list (cdr list))))
	  result)
      nil)))

(defun cal-q-is-core (var varref)
  "Check if VAR is the core of VARREF"
  (and (cal-q-var-p var)
       (cal-q-varref-p varref)
       (nf-eq (second (first var))
	      (second (first (last varref))))))

(defun cal-q-is-not-core (var varref)
  "Check if VAR is not the core of VARREF"
  (and (cal-q-var-p var)
       (cal-q-varref-p varref)
       (not (nf-eq (second (first var))
		   (second (first (last varref)))))))

(defun cal-check-def-I (p q)
  "Check if Q is the result of applying def-I to P.  So, Q must be
of the form (PRED ...)."
  (if (and (listp q) (exp-op-p (first q)))
      (let* ((pred (second (first q)))
	     (def (cal-get-def pred)))
	(if def
	    (nf-match-p (nf-schematize def) 
			(list (second q) p))
	  nil))
    nil))

(defun cal-check-def-E (p q)
  "Check if Q is the result of applying def-E to P.  So, P must be
of the form (PRED ...)."
  (if (and (listp p) (exp-op-p (first p)))
      (let* ((pred (second (first p)))
	     (def (cal-get-def pred)))
	(if def
	    (nf-match-p (nf-schematize def) 
			(list (second p) q))
	  nil))
    nil))

(defun cal-deftokens ()
  "Define tokens by using the given patterns.  This function is called only
from the compiled game files.  See, also DEFTOKENS in Def.el."
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
	(concat ConstExp "\\|" OpExp "\\|" OpenExp "\\|" CloseExp)))
