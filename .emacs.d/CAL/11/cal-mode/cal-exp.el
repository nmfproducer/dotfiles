;; CAL-EXP version 3.2 of Sat Apr  7 17:08:35 2001

;; 式の処理をする関数をあつめてある．
;; some functions are stolen from data.el and renamed
;; require data.el

(setq op-not '(op "¬"))
(setq op-id '(op "id"))
(setq op-exist-fun '(op "exist"))

;; list processing

(defun cadr (x) (car (cdr x)))

(defun caar (x) (car (car x)))

(defun cddr (list) (cdr (cdr list)))

(defun caddr (list) (car (cdr (cdr list))))

(defun cdddr (list) (cdr (cdr (cdr list))))

(defun first (x) (car x))

(defun second (x) (car (cdr x)))

(defun third (x) (car (cdr (cdr x))))

(defun fourth (x) (car (cdr (cdr (cdr x)))))

(defun fifth (x) (car (cdr (cdr (cdr (cdr x))))))

(defun exp-list-get-last (list)
  "Return the last element of LIST, assuming that LIST has
at least one elements."
  (nth (- (length list) 1) list))

(defun exp-list-get-last2 (list)
  "Return the second last element of LIST, assuming that LIST has
at least two elements."
  (nth (- (length list) 2) list))

(defun first-but-last (list)
  (let ((result nil) (cont t))
    (while (and cont list)
      (if (null (cdr list))
	  (setq cont nil)
	(setq result (cons (car list) result)
	      list (cdr list))))
    (reverse result)))

(defun center (list)
  (cdr (first-but-last list)))

;; =====================================================================
;; 命題論理式
;; =====================================================================

;;;;;;;;;;
;; 変数 ;;
;;;;;;;;;;

(defun exp-var-p (exp)
  (and (eq (length exp) 2) 
       (eq (1st exp) 'var)))

;; <- 文字列

(defun exp-var-cons (str)
  (list 'var str))

;; -> 文字列 (文字列のリストではない！）
(defun exp-var-dest (var)
  (2nd var))

;;;;;;;;;;;
;; #変数 ;;
;;;;;;;;;;;

(defun exp-sharp-var-p (exp)
  (and (> (length exp) 2) 
       (eq (1st exp) 'varref)
       (is-sharplist-butlast (but-1st exp))))

(defun exp-sharp-var-dest (sharpvar)
  (apply (function concat) (cdr sharpvar)))

(defun exp-pull-sharp-var (exp)
  "Remove a sharp from sharp var EXP."
  (if (= (length exp) 3)
      ;; we must make a variable
      (cons 'var (cdr (cdr exp)))
    (cons 'varref (cdr (cdr exp)))))

;;;;;;;;;;;;;;
;; 変数参照 ;;
;;;;;;;;;;;;;;

(defun exp-varref-p (exp)
  (or (exp-var-p exp) (exp-sharp-var-p exp)))

(defun exp-varref-dest (varref)
  (if (exp-var-p varref)
      (exp-var-dest varref)
    (exp-sharp-var-dest varref)))

(defun exp-varref-var-part (varref)
  "Returns the variable which VARREF refers to."
  (if (exp-var-p varref) varref
    (list 'var (last varref))))

;;;;;;;;;;
;; 仮定 ;;
;;;;;;;;;;

(defun exp-hyp-p (tree)
  (and (eq (length tree) 3)
       (equal (1st tree) op-colon)))

;; 仮定 <- (変数 型)

(defun exp-hyp-cons (x P) (list op-colon x P))

;; 仮定 -> (変数 型)

(defun exp-hyp-dest (tree) (but-1st tree))

;;;;;;;;;;
;; 定数 ;;
;;;;;;;;;;

(defun exp-const-p (exp)
  (and (eq (length exp) 2) 
       (eq (1st exp) 'op)))

;; <- 文字列

(defun exp-const-cons (str)
  (list 'op str))

;; -> 文字列 (文字列のリストではない！）
(defun exp-const-dest (exp)
  (2nd exp))

;;;;;;;;;;;;
;; 抽象式 ;;
;;;;;;;;;;;;

(defun exp-abs-p (exp)
  (and (eq (length exp) 3)
       (eq 'abs (1st exp))))

;; 抽象式 <- (変数 式)

(defun exp-abs-cons (var exp)
  (list 'abs var exp))

;; 抽象式 -> (変数 式)

(defun exp-abs-dest (aexp) (but-1st aexp))

(defun exp-abs-dest1 (aexp) (1st (exp-abs-dest aexp)))

(defun exp-abs-dest2 (aexp) (2nd (exp-abs-dest aexp)))

;;;;;;;;;;;;
;; assume ;;
;;;;;;;;;;;;

(defun exp-assume-p (exp)
  (and (eq (length exp) 3)
       (equal op-assume (1st exp))))

;; assume <- (変数 式 証明)

(defun exp-assume-cons (prop var proof)
  (list op-assume prop (exp-abs-cons var proof)))

;; assume -> (変数 式 証明)

(defun exp-assume-dest (exp)
  (let* ((prop (2nd exp))
	 (abs (3rd exp))
	 (var (exp-abs-dest1 abs))
	 (proof (exp-abs-dest2 abs)))
    (list prop var proof)))

(defun exp-assume-dest1 (exp)
  (1st (exp-assume-dest exp)))

(defun exp-assume-dest2 (exp)
  (2nd (exp-assume-dest exp)))

(defun exp-assume-dest3 (exp)
  (3rd (exp-assume-dest exp)))

;;;;;;;;;;
;; 矛盾 ;;
;;;;;;;;;;

(defun exp-btm-p (exp)
  (equal exp op-btm))

;; -> 文字列 (文字列のリストではない！）
(defun exp-btm-dest (var)
  (2nd var))

;;;;;;;;;;;;;;;;;;;;;;
;; atomic prop form ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun exp-atomic-prop-form-p (exp)
  (or (exp-var-p exp) (exp-btm-p exp)))

;; -> 文字列 (文字列のリストではない！）
(defun exp-atomic-prop-form-dest (var)
  (2nd var))

;;;;;;;;;;;;;;;;;;;;
;; unit prop form ;;
;;;;;;;;;;;;;;;;;;;;

(defun exp-unit-prop-form-p (exp)
  (or (exp-atomic-prop-form-p exp)
      (and (exp-not-p exp) 
	   (exp-unit-prop-form-p (exp-not-dest exp)))))

;;;;;;;;;;
;; 否定 ;;
;;;;;;;;;;

(defun exp-not-p (exp)
  (and (eq (length exp) 3) 
       (equal (1st exp) op-imp)
       (equal (3rd exp) op-btm)))

(defun exp-not-dest (exp)
  (2nd exp))

;;;;;;;;;;
;; 仮言 ;;
;;;;;;;;;;

(defun exp-imp-p (exp)
  (and (eq (length exp) 3) 
       (equal (1st exp) op-imp)
       ;; must not be a negation!
       (not (exp-not-p exp))))

(defun exp-imp-cons (exp1 exp2)
  (list op-imp exp1 exp2))

(defun exp-imp-dest (exp)
  (but-1st exp))

(defun exp-imp-dest1 (exp)
  (1st (exp-imp-dest exp)))

(defun exp-imp-dest2 (exp)
  (2nd (exp-imp-dest exp)))

;;;;;;;;;;
;; 連言 ;;
;;;;;;;;;;

(defun exp-and-p (exp)
  (and (eq (length exp) 3) 
       (equal (1st exp) op-and)))

(defun exp-and-cons (exp1 exp2)
  (list op-and exp1 exp2))

(defun exp-and-dest (exp)
  (but-1st exp))

(defun exp-and-dest1 (exp)
  (1st (but-1st exp)))

(defun exp-and-dest2 (exp)
  (2nd (but-1st exp)))

;;;;;;;;;;
;; 選言 ;;
;;;;;;;;;;

(defun exp-or-p (exp)
  (and (eq (length exp) 3) 
       (equal (1st exp) op-or)))

(defun exp-or-cons (exp1 exp2)
  (list op-or exp1 exp2))

(defun exp-or-dest (exp)
  (but-1st exp))

(defun exp-or-dest1 (exp)
  (1st (but-1st exp)))

(defun exp-or-dest2 (exp)
  (2nd (but-1st exp)))

;; =====================================================================
;; 命題論理の証明
;; =====================================================================

(defun exp-concl-of (proof hyp-seq)
  "Returns the conclusion of PROOF under HYP-SEQ"
  (if (exp-varref-p proof)
      (exp-type-of-varref proof hyp-seq)
    (1st proof)))

(defun exp-imp-intro-cons (var prop proof hyp-seq)
  (list
   (exp-imp-cons prop 
		 (exp-concl-of proof 
			      (cons (exp-hyp-cons var prop) hyp-seq)))
   rule-imp-intro
   (list (exp-assume-cons prop var proof))))

(defun exp-imp-elim-cons (proof1 proof2 hyp-seq)
  (let ((prop (exp-imp-dest2 (exp-concl-of proof1 hyp-seq))))
    (list
     prop
     rule-imp-elim
     (list proof1 proof2))))

(defun exp-and-intro-cons (proof1 proof2 hyp-seq)
  (let ((prop (exp-and-cons (exp-concl-of proof1 hyp-seq)
			    (exp-concl-of proof2 hyp-seq))))
    (list
     prop
     rule-and-intro
     (list proof1 proof2))))

(defun exp-and-elim-left-cons (proof hyp-seq)
  (let ((prop (exp-and-dest1 (exp-concl-of proof hyp-seq))))
    (list
     prop
     rule-and-elim-left
     (list proof))))

(defun exp-and-elim-right-cons (proof hyp-seq)
  (let ((prop (exp-and-dest2 (exp-concl-of proof hyp-seq))))
    (list
     prop
     rule-and-elim-right
     (list proof))))

(defun exp-or-intro-left-cons (prop-r proof hyp-seq)
  (let ((prop (exp-or-cons (exp-concl-of proof hyp-seq) prop-r)))
    (list
     prop
     rule-or-intro-left
     (list proof))))

(defun exp-or-intro-right-cons (prop-l proof hyp-seq)
  (let ((prop (exp-or-cons prop-l (exp-concl-of proof hyp-seq))))
    (list
     prop
     rule-or-intro-right
     (list proof))))

(defun exp-or-elim-cons (proof var-l proof-l var-r proof-r hyp-seq)
  (let* ((prop (exp-concl-of proof hyp-seq))
	 (list (exp-or-dest prop))
	 (prop-l (1st list))
	 (prop-r (2nd list))
	 (concl (exp-concl-of 
		 proof-l (cons (exp-hyp-cons var-l prop-l) hyp-seq))))
    (list
     concl
     rule-or-elim
     (list proof
	   (exp-assume-cons prop-l var-l proof-l)
	   (exp-assume-cons prop-r var-r proof-r)))))

(defun exp-abort-cons (prop proof)
  (list prop rule-abort (list proof)))

;; =====================================================================
;; λ項 (lambda term)
;; =====================================================================

;;;;;;;;
;; λ ;;
;;;;;;;;

(defun exp-lamterm-p (tree)
  (and (eq (length tree) 3)
       (equal (1st tree) op-lambda)
       (exp-abs-p (3rd tree))))

;; λ項 <-

(defun exp-lamterm-cons (x P M) 
  (list op-lambda P (exp-abs-cons x M)))

;; λ項 -> (変数 型 本体)

(defun exp-lamterm-dest (tree) 
  (let ((tree1 (exp-abs-dest (3rd tree))))
    (list (1st tree1) (2nd tree) (2nd tree1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; １引数項か？
(defun exp-uterm-p (tree)
  (eq (length tree) 2))
;; １引数項 <- (op 項)
(defun exp-uterm-cons (op M) 
  (list op M))
;; １引数項 -> (op 項)
(defun exp-uterm-dest (tree) tree)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ２引数項か？
(defun exp-bterm-p (tree)
  (eq (length tree) 3))
;; ２引数項 <- (op 項 項)
(defun exp-bterm-cons (op M N) 
  (list op M N))
;; ２引数項 -> (op 項 項)
(defun exp-bterm-dest (tree) tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; case項か？

(defun exp-caseterm-p (tree)
  (and (eq (length tree) 4)
       (equal (1st tree) op-case)
       (exp-abs-p (3rd tree))
       (exp-abs-p (4th tree))))

;; case項 <- (項 変数 項 変数 項)

(defun exp-caseterm-cons (term1 var2 term2 var3 term3)
  (list op-case
	term1 
	(exp-abs-cons var2 term2)
	(exp-abs-cons var3 term3)))

;; case項 -> (項 変数 項 変数 項)

(defun exp-caseterm-dest (tree) 
  (let ((term2 (exp-abs-dest (3rd tree)))
	(term3 (exp-abs-dest (4th tree))))
  (cons (2nd tree) (append term2 term3))))

;;;;;;;;;;;;
;; apply 項
;;;;;;;;;;;;

(defun exp-appterm-p (exp)
  (and (eq (length exp) 3)
       (equal (1st exp) op-apply)))

;; apply 項 <- (項 項)

(defun exp-appterm-cons (M N) 
  (list op-apply M N))

;; apply項 -> (項 項)

(defun exp-appterm-cons (fun arg)
  (list op-apply fun arg))

;;;;;;;;;;;
;; pair 項
;;;;;;;;;;;

(defun exp-pairterm-p (exp)
  (and (eq (length exp) 3)
       (equal (1st exp) op-pair)))

;; pair 項 <- (項 項)

(defun exp-pairterm-cons (left right)
  (list op-pair left right))

;; pair 項 -> (項 項)

(defun exp-pairterm-dest (exp)
  (list (2nd exp) (3rd exp)))

;;;;;;;;;;;
;; left 項
;;;;;;;;;;;

(defun exp-leftterm-p (exp)
  (and (eq (length exp) 2)
       (equal (1st exp) op-left)))

;; left 項 <- (項)

(defun exp-leftterm-cons (proof)
  (list op-left proof))

;; left 項 -> (項)

(defun exp-leftterm-dest (exp)
  (list (2nd exp)))

;;;;;;;;;;;
;; right 項
;;;;;;;;;;;

(defun exp-rightterm-p (exp)
  (and (eq (length exp) 2)
       (equal (1st exp) op-right)))

;; right 項 <- (項)

(defun exp-rightterm-cons (proof)
  (list op-right proof))

;; right 項 -> (項)

(defun exp-rightterm-dest (exp)
  (list (2nd exp)))

;;;;;;;;;;;
;; inl 項
;;;;;;;;;;;

(defun exp-inlterm-p (exp)
  (and (eq (length exp) 3)
       (equal (1st exp) op-inl)))

;; inl 項 <- (項 項)

(defun exp-inlterm-cons (prop proof)
  (list op-inl prop proof))

;; inl 項 -> (項 項)

(defun exp-inlterm-dest (exp)
  (list (2nd exp) (3rd exp)))

;;;;;;;;;;;
;; inr 項
;;;;;;;;;;;

(defun exp-inrterm-p (exp)
  (and (eq (length exp) 3)
       (equal (1st exp) op-inr)))

;; inr 項 <- (項 項)

(defun exp-inrterm-cons (prop proof)
  (list op-inr prop proof))

;; inr 項 -> (項 項)

(defun exp-inrterm-dest (exp)
  (list (2nd exp) (3rd exp)))

;; =====================================================================
;; derivation in derivation games
;; =====================================================================

;; derivation == ((├ hyp-seq concl) game-name deriv)

(defun exp-get-hyp-seq (derivation)
  "Get the hyp-seq of DERIVATION"
  (second (first derivation)))

(defun exp-get-game-name (derivation)
  "Get the name of the game of DERIVATION as a string"
  (second (second derivation)))

(defun exp-get-concl (derivation)
  "Get the conclusion of DERIVATION."
  (first (third derivation)))

(defun exp-get-deriv (derivation)
  "Get the deriv-part of DERIVATION."
  (third derivation))

(defun exp-get-prop-of-concl (derivation)
  "Get the proposition from the conclusion of DERIVATION."
  (second (first (third derivation))))

(defun exp-get-vars-of-hyp-seq (hyp-seq)
  "compute the var-list from HYP-SEQ"
  (if (null hyp-seq) nil
    (cons (second (car hyp-seq)) (exp-get-vars-of-hyp-seq (cdr hyp-seq)))))

;; translation from exp to string
;; =====================================================================
;; 式 -> 文字列
;; =====================================================================

(defun exp-to-string (exp class)
  (cond ((eq class 'var) (exp-to-string-var exp))
	((eq class 'varlist) (exp-to-string-varlist exp))
	((eq class 'varref) (exp-to-string-varref exp))
	((eq class 'op) (exp-to-string-const exp))
	((eq class 'abs) (exp-to-string-abs exp))
	((eq class 'exp) (exp-to-string-exp exp))
	((eq class 'prop-form) (exp-to-string-prop-form exp))
	((eq class 'arith-prop) (exp-to-string-arith-prop exp))
	((eq class 'PropProof-deriv) (exp-to-string-PropProof-deriv exp))
	((eq class 'lambda-term) (exp-to-string-lambda-term exp))
	(t "Sorry!")))

(defun exp-to-string-var (exp)
  (if (exp-var-p exp)
      (exp-var-dest exp)
    (throw 'exp-to-string "変数でありません．")))

(defun exp-to-string-varlist (list)
  (cond ((null list) "")
	((= (length list) 1) (exp-to-string-var (1st list)))
	(t (concat (exp-to-string-var (1st list)) ", "
		   (exp-to-string-varlist (but-1st list))))))

(defun exp-to-string-varref (exp)
  (if (exp-varref-p exp)
      (exp-varref-dest exp)
    (throw 'exp-to-string "変数参照でありません．")))

(defun exp-to-string-const (exp)
  (if (exp-const-p exp)
      (exp-const-dest exp)
    (throw 'exp-to-string "定数でありません．")))

(defun exp-to-string-abs (exp)
  (if (exp-abs-p exp)
      (let* ((list (exp-abs-dest exp))
	     (var (1st list))
	     (body (2nd list)))
	(concat "(" (exp-to-string-var var) ")[" 
		(exp-to-string-exp body) "]"))
    (throw 'exp-to-string "抽象体でありません．")))

(defun exp-to-string-struct (exp)
  (if (listp exp)
      (concat "<" (exp-to-string-explist exp) ">")
    (throw 'exp-to-string "構造体でありません")))

(defun exp-to-string-explist (list)
  (cond ((null list) "")
	((= (length list) 1) (exp-to-string-exp (1st list)))
	(t (concat (exp-to-string-exp (1st list)) ", "
		   (exp-to-string-explist (but-1st list))))))

(defun exp-to-string-exp (exp)
  (cond ((exp-varref-p exp) (exp-to-string-varref exp))
	((exp-const-p exp) (exp-to-string-const exp))
	((exp-abs-p exp) (exp-to-string-abs exp))
	(t ;; it must be a struct!, check that dynamically
	 (exp-to-string-struct exp))))

;; =====================================================================
;; 命題論理式
;; =====================================================================

(defun exp-to-string-prop-form (exp)
  (cond ((exp-var-p exp) (exp-to-string-var exp))
	((exp-btm-p exp) (exp-to-string-btm exp))
	((exp-not-p exp)
	 (let ((body (exp-not-dest exp)))
	   (if (exp-unit-prop-form-p body)
	       (concat (exp-to-string-const op-not)
		       (exp-to-string-prop-form body))
	     (concat (exp-to-string-const op-not)
			 "(" (exp-to-string-prop-form body) ")"))))
	((exp-imp-p exp)
	 (let* ((list (exp-imp-dest exp))
		(left (1st list))
		(right (2nd list)))
	   ;; since imp is weakest in binding power and right associative,
	   ;; we have the following two cases
	   (if (exp-imp-p left)
	       ;; in this case, we must parethesize the left
	       (concat (exp-to-string-prop-form-paren left)
		       (exp-to-string-const op-imp)
		       (exp-to-string-prop-form right))
	     (concat (exp-to-string-prop-form left)
		     (exp-to-string-const op-imp)
		     (exp-to-string-prop-form right)))))
	((exp-or-p exp)
	 (let* ((list (exp-or-dest exp))
		(left (1st list))
		(right (2nd list)))
	   (cond ((exp-unit-prop-form-p left)
		  (cond ((exp-unit-prop-form-p right)
			 (concat (exp-to-string-prop-form left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form right)))
			((exp-not-p right)
			 (concat (exp-to-string-prop-form left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form-paren right)))
			((exp-and-p right)
			 (concat (exp-to-string-prop-form left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form right)))
			((exp-or-p right)
			 (concat (exp-to-string-prop-form left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form-paren right)))
			((exp-imp-p right)
			 (concat (exp-to-string-prop-form left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form-paren right)))
			))
		 ((exp-not-p left)
		  (cond ((exp-unit-prop-form-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form right)))
			((exp-not-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form-paren right)))
			((exp-and-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form right)))
			((exp-or-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form-paren right)))
			((exp-imp-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form-paren right)))
			))
		 ((exp-and-p left)
		  (cond ((exp-unit-prop-form-p right)
			 (concat (exp-to-string-prop-form left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form right)))
			((exp-not-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form-paren right)))
			((exp-and-p right)
			 (concat (exp-to-string-prop-form left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form right)))
			((exp-or-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form-paren right)))
			((exp-imp-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form-paren right)))
			))
		 ((exp-or-p left)
		  (cond ((exp-unit-prop-form-p right)
			 (concat (exp-to-string-prop-form left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form right)))
			((exp-not-p right)
			 (concat (exp-to-string-prop-form left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form-paren right)))
			((exp-and-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form right)))
			((exp-or-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form-paren right)))
			((exp-imp-p right)
			 (concat (exp-to-string-prop-form left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form-paren right)))
			))
		 ((exp-imp-p left)
		  (cond ((exp-unit-prop-form-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form right)))
			((exp-not-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form-paren right)))
			((exp-and-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form right)))
			((exp-or-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form-paren right)))
			((exp-imp-p right)
			 (concat (exp-to-string-prop-form left)
				 (exp-to-string-const op-or)
				 (exp-to-string-prop-form right)))
			))
		 )))
	((exp-and-p exp)
	 (let* ((list (exp-or-dest exp))
		(left (1st list))
		(right (2nd list)))
	   (cond ((exp-unit-prop-form-p left)
		  (cond ((exp-unit-prop-form-p right)
			 (concat (exp-to-string-prop-form left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form right)))
			((exp-not-p right)
			 (concat (exp-to-string-prop-form left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form-paren right)))
			((exp-and-p right)
			 (concat (exp-to-string-prop-form left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form-paren right)))
			((exp-or-p right)
			 (concat (exp-to-string-prop-form left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form-paren right)))
			((exp-imp-p right)
			 (concat (exp-to-string-prop-form left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form-paren right)))
			))
		 ((exp-not-p left)
		  (cond ((exp-unit-prop-form-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form right)))
			((exp-not-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form-paren right)))
			((exp-and-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form-paren right)))
			((exp-or-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form-paren right)))
			((exp-imp-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form-paren right)))
			))
		 ((exp-and-p left)
		  (cond ((exp-unit-prop-form-p right)
			 (concat (exp-to-string-prop-form left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form right)))
			((exp-not-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form-paren right)))
			((exp-and-p right)
			 (concat (exp-to-string-prop-form left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form-paren right)))
			((exp-or-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form-paren right)))
			((exp-imp-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form-paren right)))
			))
		 ((exp-or-p left)
		  (cond ((exp-unit-prop-form-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form right)))
			((exp-not-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form-paren right)))
			((exp-and-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form right)))
			((exp-or-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form-paren right)))
			((exp-imp-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form-paren right)))
			))
		 ((exp-imp-p left)
		  (cond ((exp-unit-prop-form-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form right)))
			((exp-not-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form-paren right)))
			((exp-and-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form right)))
			((exp-or-p right)
			 (concat (exp-to-string-prop-form-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form-paren right)))
			((exp-imp-p right)
			 (concat (exp-to-string-prop-form left)
				 (exp-to-string-const op-and)
				 (exp-to-string-prop-form-paren right)))
			))
		 )))
	(t (throw 'exp-to-string-prop-form 
		  "命題論理式でありません")))
  )

(defun exp-to-string-prop-form-paren (exp)
  (concat "(" (exp-to-string-prop-form exp) ")"))

(defun exp-to-string-btm (exp)
  (if (exp-btm-p exp)
      (exp-btm-dest exp)
    (throw 'exp-to-string "矛盾でありません．")))

;; =====================================================================
;; 命題論理式の証明 (PropProof)
;; =====================================================================

(defun exp-to-string-PropProof-deriv (exp)
  (cond ((exp-varref-p exp) (exp-to-string-varref exp))
	((exp-assume-p exp)
	 (let* ((prop (exp-assume-dest1 exp))
		(var (exp-assume-dest2 exp))
		(body (exp-assume-dest3 exp)))
	   (concat "(" (exp-to-string-var var) ":" 
		   (exp-to-string prop 'prop-form) ")"
		   "[" (exp-to-string-PropProof-deriv body) "]")))
	(t
	 (let* ((list (exp-match exp
				 (ParseString
				  "WILD_CARD by WILD_CARD WILD_CARD"
				  'PropProof-deriv)))
		(judgment (1st list))
		(rule-name (2nd list))
		(deriv-list (3rd list)))
	   (concat (exp-to-string-PropProof-judgment judgment) " by "
		   (exp-to-string-const rule-name) " {"
		   (exp-to-string-PropProof-deriv-list deriv-list) "}")))))

(defun exp-to-string-PropProof-deriv-list (deriv-list)
  (cond ((null deriv-list) "")
	((= (length deriv-list) 1) 
	 (exp-to-string-PropProof-deriv (1st deriv-list)))
	(t (concat (exp-to-string-PropProof-deriv (1st deriv-list)) "; "
		   (exp-to-string-PropProof-deriv-list (but-1st deriv-list))))
	))

(defun exp-to-string-PropProof-judgment (judgment)
  (exp-to-string-prop-form judgment))

;; =====================================================================
;; λ項 (lambda-term)
;; =====================================================================

(defun exp-to-string-lambda-term (exp)
  (if (exp-varref-p exp) 
      (exp-to-string-varref exp)
    (let ((op (1st exp))
	  (body (but-1st exp)))
      (cond ((equal op-lambda op)
	     (let* ((prop (1st body))
		    (abs (2nd body))
		    (list (exp-abs-dest abs))
		    (var (1st list))
		    (body (2nd list)))
	      (concat (2nd op-lambda) 
		      "(" (exp-to-string-var var) ":" 
		      (exp-to-string prop 'prop-form) ")"
		      "[" (exp-to-string-lambda-term body) "]")))
	    ((equal op-apply op)
	     (let ((fun (1st body))
		   (arg (2nd body)))
	       (concat (exp-to-string-lambda-term fun)
		       "(" (exp-to-string-lambda-term arg) ")")))
	    ((equal op-pair op)
	     (let ((left (1st body))
		   (right (2nd body)))
	       (concat "[" (exp-to-string-lambda-term left) ", "
		       (exp-to-string-lambda-term right) "]")))
	    ((equal op-left op)
	     (let ((arg (1st body)))
	       (concat "left(" (exp-to-string-lambda-term arg) ")")))
	    ((equal op-right op)
	     (let ((arg (1st body)))
	       (concat "right(" (exp-to-string-lambda-term arg) ")")))
	    ((equal op-inl op)
	     (let ((prop (1st body))
		   (term (2nd body)))
	       (concat "inl(" (exp-to-string-prop-form prop) ", "
		       (exp-to-string-lambda-term term) ")")))
	    ((equal op-inr op)
	     (let ((prop (1st body))
		   (term (2nd body)))
	       (concat "inr(" (exp-to-string-prop-form prop) ", "
		       (exp-to-string-lambda-term term) ")")))
	    ((equal op-case op)
	     (let ((term (1st body))
		   (abs1 (2nd body))
		   (abs2 (3rd body)))
	       (concat "case(" (exp-to-string-lambda-term term) ", "
		       (exp-to-string-abs-lambda-term abs1) ", "
		       (exp-to-string-abs-lambda-term abs2) ")")))
	    ((equal op-abort op)
	     (let ((prop (1st body))
		   (term (2nd body)))
	       (concat "abort(" (exp-to-string-prop-form prop) ", "
		       (exp-to-string-lambda-term term) ")")))
	    (t (throw 'exp-to-string-lambda-term "sorry"))))))

(defun exp-to-string-abs-lambda-term (abs)
  (let* ((list (exp-abs-dest abs))
	 (var (1st list))
	 (body (2nd list)))
    (concat "(" (exp-to-string-var var) ")"
	    "[" (exp-to-string-lambda-term body) "]")))

;; =====================================================================
;; λ項の導出 (LambdaTerm)
;; =====================================================================

(defun exp-to-string-LambdaTerm-deriv (exp)
  (cond ((exp-varref-p exp) (exp-to-string-varref exp))
	((exp-assume-p exp)
	 (let* ((prop (exp-assume-dest1 exp))
		(var (exp-assume-dest2 exp))
		(body (exp-assume-dest3 exp)))
	   (concat "(" (exp-to-string-var var) ":" 
		   (exp-to-string prop 'prop-form) ")"
		   "[" (exp-to-string-LambdaTerm-deriv body) "]")))
	(t
	 (let* ((list (exp-match exp
				 (ParseString
				  "WILD_CARD by WILD_CARD WILD_CARD"
				  ;; PropProof で parse する
				  'PropProof-deriv)))
		(judgment (1st list))
		(rule-name (2nd list))
		(deriv-list (3rd list)))
	   (concat (exp-to-string-LambdaTerm-judgment judgment) " by "
		   (exp-to-string-const rule-name) " {"
		   (exp-to-string-LambdaTerm-deriv-list deriv-list) "}")))))

(defun exp-to-string-LambdaTerm-deriv-list (deriv-list)
  (cond ((null deriv-list) "")
	((= (length deriv-list) 1) 
	 (exp-to-string-LambdaTerm-deriv (1st deriv-list)))
	(t (concat (exp-to-string-LambdaTerm-deriv (1st deriv-list)) "; "
		   (exp-to-string-LambdaTerm-deriv-list (but-1st deriv-list))))
	))

(defun exp-to-string-LambdaTerm-judgment (judgment)
  (let ((term (2nd judgment))
	(prop (3rd judgment)))
    (concat (exp-to-string-lambda-term term) ":"
	    (exp-to-string-prop-form prop))))

;; =====================================================================
;; 算術項
;; =====================================================================

(defun exp-zero-p (exp)
  (equal exp op-0))

(defun exp-zero-dest (exp)
  (2nd exp))

(defun exp-succ-p (exp)
  (and (= (length exp) 2)
       (equal (1st exp) op-s)))

(defun exp-succ-dest1 (exp)
  (2nd (1st exp)))

(defun exp-succ-dest2 (exp)
  (2nd exp))

(defun exp-unit-arith-term-p (exp)
  (or (exp-varref-p exp) (exp-zero-p exp) (exp-succ-p exp)))

(defun exp-plus-p (exp)
  (equal (1st exp) op-+))

(defun exp-plus-dest1 (exp)
  (2nd exp))

(defun exp-plus-dest2 (exp)
  (3rd exp))

(defun exp-times-p (exp)
  (equal (1st exp) op-*))

(defun exp-times-dest1 (exp)
  (2nd exp))

(defun exp-times-dest2 (exp)
  (3rd exp))

(defun exp-to-string-arith-term (exp)
  (cond ((exp-varref-p exp) (exp-to-string-varref exp))
	((exp-zero-p exp) (exp-zero-dest exp))
	((exp-succ-p exp)
	 (concat (exp-succ-dest1 exp)
		 "(" (exp-to-string-arith-term (exp-succ-dest2 exp)) ")"))
	((exp-plus-p exp)
	 (let* ((left (exp-plus-dest1 exp))
		(right (exp-plus-dest2 exp)))
	   (cond ((exp-unit-arith-term-p left)
		  (cond ((exp-unit-arith-term-p right)
			 (concat (exp-to-string-arith-term left)
				 (exp-to-string-const op-+)
				 (exp-to-string-arith-term right)))
			((exp-times-p right)
			 (concat (exp-to-string-arith-term left)
				 (exp-to-string-const op-+)
				 (exp-to-string-arith-term right)))
			((exp-plus-p right)
			 (concat (exp-to-string-arith-term left)
				 (exp-to-string-const op-+)
				 (exp-to-string-arith-term-paren right)))
			))
		 ((exp-times-p left)
		  (cond ((exp-unit-arith-term-p right)
			 (concat (exp-to-string-arith-term left)
				 (exp-to-string-const op-+)
				 (exp-to-string-arith-term right)))
			((exp-times-p right)
			 (concat (exp-to-string-arith-term left)
				 (exp-to-string-const op-+)
				 (exp-to-string-arith-term right)))
			((exp-plus-p right)
			 (concat (exp-to-string-arith-term-paren left)
				 (exp-to-string-const op-+)
				 (exp-to-string-arith-term-paren right)))
			))
		 ((exp-plus-p left)
		  (cond ((exp-unit-arith-term-p right)
			 (concat (exp-to-string-arith-term left)
				 (exp-to-string-const op-+)
				 (exp-to-string-arith-term right)))
			((exp-times-p right)
			 (concat (exp-to-string-arith-term left)
				 (exp-to-string-const op-+)
				 (exp-to-string-arith-term right)))
			((exp-plus-p right)
			 (concat (exp-to-string-arith-term-paren left)
				 (exp-to-string-const op-+)
				 (exp-to-string-arith-term-paren right)))
			))
		 )))
	((exp-times-p exp)
	 (let* ((left (exp-times-dest1 exp))
		(right (exp-times-dest2 exp)))
	   (cond ((exp-unit-arith-term-p left)
		  (cond ((exp-unit-arith-term-p right)
			 (concat (exp-to-string-arith-term left)
				 (exp-to-string-const op-*)
				 (exp-to-string-arith-term right)))
			((exp-times-p right)
			 (concat (exp-to-string-arith-term left)
				 (exp-to-string-const op-*)
				 (exp-to-string-arith-term-paren right)))
			((exp-plus-p right)
			 (concat (exp-to-string-arith-term left)
				 (exp-to-string-const op-*)
				 (exp-to-string-arith-term-paren right)))
			))
		 ((exp-times-p left)
		  (cond ((exp-unit-arith-term-p right)
			 (concat (exp-to-string-arith-term left)
				 (exp-to-string-const op-*)
				 (exp-to-string-arith-term right)))
			((exp-times-p right)
			 (concat (exp-to-string-arith-term left)
				 (exp-to-string-const op-*)
				 (exp-to-string-arith-term-paren right)))
			((exp-plus-p right)
			 (concat (exp-to-string-arith-term-paren left)
				 (exp-to-string-const op-*)
				 (exp-to-string-arith-term-paren right)))
			))
		 ((exp-plus-p left)
		  (cond ((exp-unit-arith-term-p right)
			 (concat (exp-to-string-arith-term-paren left)
				 (exp-to-string-const op-*)
				 (exp-to-string-arith-term right)))
			((exp-times-p right)
			 (concat (exp-to-string-arith-term-paren left)
				 (exp-to-string-const op-*)
				 (exp-to-string-arith-term right)))
			((exp-plus-p right)
			 (concat (exp-to-string-arith-term-paren left)
				 (exp-to-string-const op-*)
				 (exp-to-string-arith-term-paren right)))
			))
		 )))
	(t (throw 'exp-to-string-arith-term
		  (format "「%s」は算術項でありません" exp)))))

(defun exp-to-string-arith-term-paren (exp)
  (concat "(" (exp-to-string-arith-term exp) ")"))

(defun exp-to-string-arith-term-list (term-list)
  (cond ((null term-list) "")
	((= (length term-list) 1) 
	 (exp-to-string-arith-term (1st term-list)))
	(t (concat (exp-to-string-arith-term (1st term-list)) ", "
		   (exp-to-string-arith-term-list (but-1st term-list))))
	))

;; =====================================================================
;; 算術論理式
;; =====================================================================

;;;;;;;;;;
;; 等式 ;;
;;;;;;;;;;

(defun exp-equality-p (exp)
  (equal (1st exp) op-=))

(defun exp-equality-dest1 (exp)
  (2nd exp))

(defun exp-equality-dest2 (exp)
  (3rd exp))

;;;;;;;;;;;;;;
;; 定義述語 ;;
;;;;;;;;;;;;;;

(defun exp-defpred-p (exp)
  (and (listp exp)
       (let ((op (1st exp)) (item (2nd exp)))
	 (and
	  (equal op-apply op)
	  (exp-const-p item)
	  (cal-get-def (intern (2nd item)))))))

(defun exp-defpred-dest1 (exp)
  "returns the name of the predicate as a string."
  (2nd (2nd exp)))

(defun exp-defpred-dest2 (exp)
  "returns the list of the arguments to the predicate."
  (cdr (cdr exp)))

;;;;;;;;;;;;
;; ∀, ∃ ;;
;;;;;;;;;;;;

(defun exp-univ-p (exp)
  (equal (1st exp) op-univ))

(defun exp-exist-p (exp)
  (equal (1st exp) op-exist))

;;;;;;;;;;;;;;;;;;;;;;;;
;; unitary arith form ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun exp-unitary-arith-prop-p (exp)
  (or (exp-var-p exp)
      (exp-btm-p exp)
      (exp-equality-p exp)
      (exp-univ-p exp)
      (exp-exist-p exp)
      (exp-defpred-p exp)
      ))

;;;;;;;;;;;;;;;;;;;;;
;; unit arith prop ;;
;;;;;;;;;;;;;;;;;;;;;

(defun exp-unit-arith-prop-p (exp)
  (or (exp-unitary-arith-prop-p exp)
      (and (exp-not-p exp) 
	   (exp-unit-arith-prop-p (exp-not-dest exp)))))


;; Nat も算術論理式とみなす．

(defun exp-to-string-arith-prop (exp)
  (cond ((equal op-nat exp) "Nat")
	((exp-varref-p exp) (exp-to-string-varref exp))
	((exp-btm-p exp) (exp-to-string-btm exp))
	((exp-equality-p exp)
	 (let ((arg1 (exp-equality-dest1 exp))
	       (arg2 (exp-equality-dest2 exp)))
	   (concat (exp-to-string-arith-term arg1)
		   "="
		   (exp-to-string-arith-term arg2))))
	((exp-defpred-p exp)
	 (let ((pred (exp-defpred-dest1 exp))
	       (list (exp-defpred-dest2 exp)))
	   (concat pred "("
		   (exp-to-string-arith-term-list list)
		   ")")))
	((exp-univ-p exp) (exp-to-string-univ exp))
	((exp-exist-p exp) (exp-to-string-exist exp))
	((exp-not-p exp)
	 (let ((body (exp-not-dest exp)))
	   (if (exp-unit-arith-prop-p body)
	       (concat (exp-to-string-const op-not)
		       (exp-to-string-arith-prop body))
	     (concat (exp-to-string-const op-not)
			 "(" (exp-to-string-arith-prop body) ")"))))
	((exp-imp-p exp)
	 (let* ((list (exp-imp-dest exp))
		(left (1st list))
		(right (2nd list)))
	   ;; since imp is weakest in binding power and right associative,
	   ;; we have the following two cases
	   (if (exp-imp-p left)
	       ;; in this case, we must parethesize the left
	       (concat (exp-to-string-arith-prop-paren left)
		       (exp-to-string-const op-imp)
		       (exp-to-string-arith-prop right))
	     (concat (exp-to-string-arith-prop left)
		     (exp-to-string-const op-imp)
		     (exp-to-string-arith-prop right)))))
	((exp-or-p exp)
	 (let* ((list (exp-or-dest exp))
		(left (1st list))
		(right (2nd list)))
	   (cond ((exp-unit-arith-prop-p left)
		  (cond ((exp-unit-arith-prop-p right)
			 (concat (exp-to-string-arith-prop left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop right)))
			((exp-not-p right)
			 (concat (exp-to-string-arith-prop left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop-paren right)))
			((exp-and-p right)
			 (concat (exp-to-string-arith-prop left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop right)))
			((exp-or-p right)
			 (concat (exp-to-string-arith-prop left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop-paren right)))
			((exp-imp-p right)
			 (concat (exp-to-string-arith-prop left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop-paren right)))
			))
		 ((exp-not-p left)
		  (cond ((exp-unit-arith-prop-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop right)))
			((exp-not-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop-paren right)))
			((exp-and-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop right)))
			((exp-or-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop-paren right)))
			((exp-imp-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop-paren right)))
			))
		 ((exp-and-p left)
		  (cond ((exp-unit-arith-prop-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop right)))
			((exp-not-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop-paren right)))
			((exp-and-p right)
			 (concat (exp-to-string-arith-prop left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop right)))
			((exp-or-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop-paren right)))
			((exp-imp-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop-paren right)))
			))
		 ((exp-or-p left)
		  (cond ((exp-unit-arith-prop-p right)
			 (concat (exp-to-string-arith-prop left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop right)))
			((exp-not-p right)
			 (concat (exp-to-string-arith-prop left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop-paren right)))
			((exp-and-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop right)))
			((exp-or-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop-paren right)))
			((exp-imp-p right)
			 (concat (exp-to-string-arith-prop left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop-paren right)))
			))
		 ((exp-imp-p left)
		  (cond ((exp-unit-arith-prop-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop right)))
			((exp-not-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop-paren right)))
			((exp-and-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop right)))
			((exp-or-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop-paren right)))
			((exp-imp-p right)
			 (concat (exp-to-string-arith-prop left)
				 (exp-to-string-const op-or)
				 (exp-to-string-arith-prop right)))
			))
		 )))
	((exp-and-p exp)
	 (let* ((list (exp-or-dest exp))
		(left (1st list))
		(right (2nd list)))
	   (cond ((exp-unit-arith-prop-p left)
		  (cond ((exp-unit-arith-prop-p right)
			 (concat (exp-to-string-arith-prop left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop right)))
			((exp-not-p right)
			 (concat (exp-to-string-arith-prop left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop-paren right)))
			((exp-and-p right)
			 (concat (exp-to-string-arith-prop left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop-paren right)))
			((exp-or-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop right)))
			((exp-imp-p right)
			 (concat (exp-to-string-arith-prop left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop-paren right)))
			))
		 ((exp-not-p left)
		  (cond ((exp-unit-arith-prop-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop right)))
			((exp-not-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop-paren right)))
			((exp-and-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop-paren right)))
			((exp-or-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop-paren right)))
			((exp-imp-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop-paren right)))
			))
		 ((exp-and-p left)
		  (cond ((exp-unit-arith-prop-p right)
			 (concat (exp-to-string-arith-prop left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop right)))
			((exp-not-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop-paren right)))
			((exp-and-p right)
			 (concat (exp-to-string-arith-prop left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop-paren right)))
			((exp-or-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop-paren right)))
			((exp-imp-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop-paren right)))
			))
		 ((exp-or-p left)
		  (cond ((exp-unit-arith-prop-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop right)))
			((exp-not-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop-paren right)))
			((exp-and-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop right)))
			((exp-or-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop-paren right)))
			((exp-imp-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop-paren right)))
			))
		 ((exp-imp-p left)
		  (cond ((exp-unit-arith-prop-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop right)))
			((exp-not-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop-paren right)))
			((exp-and-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop right)))
			((exp-or-p right)
			 (concat (exp-to-string-arith-prop-paren left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop-paren right)))
			((exp-imp-p right)
			 (concat (exp-to-string-arith-prop left)
				 (exp-to-string-const op-and)
				 (exp-to-string-arith-prop-paren right)))
			))
		 )))
	(t (throw 'exp-to-string-arith-prop 
		  "算術論理式でありません"))))

(defun exp-to-string-arith-prop-paren (exp)
  (concat "(" (exp-to-string-arith-prop exp) ")"))

(defun exp-to-string-univ (exp)
  (if (exp-univ-p exp)
      (concat
       (exp-to-string-const op-univ)
       (exp-to-string-abs-arith-prop (2nd exp)))
    (throw 'exp-to-string "算術論理式でありません．")))

(defun exp-to-string-exist (exp)
  (if (exp-exist-p exp)
      (concat
       (exp-to-string-const op-exist)
       (exp-to-string-abs-arith-prop (2nd exp)))
    (throw 'exp-to-string "算術論理式でありません．")))

(defun exp-to-string-abs-arith-prop (abs)
  (let* ((list (exp-abs-dest abs))
	 (var (1st list))
	 (body (2nd list)))
    (concat "(" (exp-to-string-var var) ")" 
	    "[" (exp-to-string-arith-prop body) "]")))

(defun exp-to-string-arith-hyp-seq (exp)
  (cond ((null exp) "")
	((= (length exp) 1) (exp-to-string-arith-hyp (1st exp)))
	(t (concat (exp-to-string-arith-hyp (1st exp)) ", "
		   (exp-to-string-arith-hyp-seq (but-1st exp))))))

(defun exp-to-string-arith-hyp (hyp)
  (concat (exp-to-string-var (2nd hyp)) ":"
	  (let ((type (3rd hyp)))
	    (if (equal type op-nat) 
		(exp-const-dest op-nat)
	      (exp-to-string-arith-prop type)))))

;; =====================================================================
;; 算術の証明項
;; =====================================================================

(defun exp-to-string-arith-proof-term (exp)
  (if (exp-varref-p exp) 
      (exp-to-string-varref exp)
    (let ((op (1st exp))
	  (body (but-1st exp)))
      (cond ((equal op-lambda op)
	     (if (= (length exp) 3)
		 (let* ((prop (1st body))
			(abs (2nd body))
			(list (exp-abs-dest abs))
			(var (1st list))
			(body (2nd list)))
		   (concat (2nd op-lambda) 
			   "(" (exp-to-string-var var) ":" 
			   (exp-to-string prop 'prop-form) ")"
			   "[" (exp-to-string-arith-proof-term body) "]"))
	       (let* ((abs (1st body))
		      (list (exp-abs-dest abs))
		      (var (1st list))
		      (body (2nd list)))
		 (concat (2nd op-lambda) 
			 "(" (exp-to-string-var var) ")" 
			 "[" (exp-to-string-arith-proof-term body) "]"))))
	    ((equal op-pair op)
	     (let ((left (1st body))
		   (right (2nd body)))
	       (concat "[" (exp-to-string-arith-proof-term left) ", "
		       (exp-to-string-arith-proof-term right) "]")))
	    ((equal op-left op)
	     (let ((arg (1st body)))
	       (concat "left(" (exp-to-string-arith-proof-term arg) ")")))
	    ((equal op-right op)
	     (let ((arg (1st body)))
	       (concat "right(" (exp-to-string-arith-proof-term arg) ")")))
	    ((equal op-inl op)
	     (let ((prop (1st body))
		   (term (2nd body)))
	       (concat "inl(" (exp-to-string-prop-form prop) ", "
		       (exp-to-string-arith-proof-term term) ")")))
	    ((equal op-inr op)
	     (let ((prop (1st body))
		   (term (2nd body)))
	       (concat "inr(" (exp-to-string-prop-form prop) ", "
		       (exp-to-string-arith-proof-term term) ")")))
	    ((equal op-case op)
	     (let ((term (1st body))
		   (abs1 (2nd body))
		   (abs2 (3rd body)))
	       (concat "case(" (exp-to-string-arith-proof-term term) ", "
		       (exp-to-string-abs-arith-proof-term abs1) ", "
		       (exp-to-string-abs-arith-proof-term abs2) ")")))
	    ((equal op-abort op)
	     (let ((prop (1st body))
		   (term (2nd body)))
	       (concat "abort(" (exp-to-string-prop-form prop) ", "
		       (exp-to-string-arith-proof-term term) ")")))
	    ((equal op-exist-fun op)
	     (let ((term1 (1st body))
		   (term2 (2nd body))
		   (prop (3rd body)))
	       (concat "exist(" (exp-to-string-arith-term term1) ", "
		       (exp-to-string-arith-proof-term term2) ", "
		       (exp-to-string-arith-prop prop) ")")))
	    ((equal op-split op)
	     (let ((term (1st body))
		   (dabs-term (2nd body)))
	       (concat "split(" (exp-to-string-arith-proof-term term) ", "
		       (exp-to-string-dabs-arith-proof-term dabs-term) ")")))
	    ((equal op-apply op)
	     (let ((fun (1st body)))
	       (if (and (exp-const-p fun) (get (intern (2nd fun)) 'cal-thm))
		   (concat (exp-const-dest fun) "("
			   (exp-to-string-term-list (but-1st body)) ")")
		 (let ((arg (2nd body)))
		   (concat (exp-to-string-arith-proof-term fun) "("
			   (exp-to-string-term arg) ")")))))
	    ((equal op-id op)
	     (let ((term (1st body)))
	       (concat "id(" (exp-to-string-arith-proof-term term) ")")))
	    ((equal op-eq op)
	     (let ((term (1st body)))
	       (concat "eq(" (exp-to-string-arith-term term) ")")))
	    ((equal op-repl op)
	     (let ((prop1 (1st body))
		   (prop2 (2nd body))
		   (prop3 (3rd body)))
	       (concat "repl(" (exp-to-string-prop-form prop1) ", "
		       (exp-to-string-prop-form prop2) ", "
		       (exp-to-string-prop-form prop3)
		       ")")))
	    ((equal op-succ-e op)
	     (let ((term (1st body)))
	       (concat "succ_e(" (exp-to-string-arith-term term) ")")))
	    ((equal op-zero-e op)
	     (let ((term (1st body)))
	       (concat "zero_e(" (exp-to-string-arith-term term) ")")))
	    ((equal op-plus op)
	     (let ((term1 (1st body))
		   (term2 (2nd body)))
	       (concat "plus(" (exp-to-string-arith-term term1) ", "
		       (exp-to-string-arith-term term2) ")")))
	    ((equal op-times op)
	     (let ((term1 (1st body))
		   (term2 (2nd body)))
	       (concat "times(" (exp-to-string-arith-term term1) ", "
		       (exp-to-string-arith-term term2) ")")))
	    ((equal op-rec op)
	     (let ((term1 (1st body))
		   (term2 (2nd body)))
	       (concat "times(" (exp-to-string-arith-proof-term term1) ", "
		       (exp-to-string-arith-proof-term term2) ")")))
	    (t (throw 'exp-to-string-arith-proof-term "sorry"))))))

(defun exp-to-string-abs-arith-proof-term (abs)
  (let* ((list (exp-abs-dest abs))
	 (var (1st list))
	 (body (2nd list)))
    (concat "(" (exp-to-string-var var) ")"
	    "[" (exp-to-string-arith-proof-term body) "]")))

(defun exp-to-string-term (term)
  "TERM is either an arith-term or an arith-proof-term."
  (if (exp-arith-term-p term)
      (exp-to-string-arith-term term)
    (exp-to-string-arith-proof-term term)))

(defun exp-to-string-term-list (term-list)
  (cond ((null term-list) "")
	((= (length term-list) 1) 
	 (exp-to-string-term (1st term-list)))
	(t (concat (exp-to-string-term (1st term-list)) ", "
		   (exp-to-string-term-list (but-1st term-list))))
	))

(defun exp-to-string-dabs-arith-proof-term (abs)
  (let ((var1 (2nd abs))
	(var2 (2nd (3rd abs)))
	(body (3rd (3rd abs))))
    (concat "(" (exp-to-string-var var1) ", " (exp-to-string-var var2) ")"
	    "[" (exp-to-string-arith-proof-term body) "]")))

(defun exp-arith-term-p (term)
  (or (exp-varrref-p term)
      (exp-zero-p term)
      (exp-succ-p term)
      (exp-plus-p term)
      (exp-times-p term)))

(defun exp-to-string-arith-judge (judge)
  (concat (exp-to-string-arith-proof-term (2nd judge)) ":"
	  (exp-to-string-arith-prop (3rd judge))))

;; =====================================================================
;; 算術の証明 (ArithProof)
;; =====================================================================

(defun exp-to-string-ArithProof-deriv (exp)
  (cond ((exp-varref-p exp) (exp-to-string-varref exp))
	((exp-assume-p exp)
	 (let* ((prop (exp-assume-dest1 exp))
		(var (exp-assume-dest2 exp))
		(body (exp-assume-dest3 exp)))
	   (concat "(" (exp-to-string-var var) ":" 
		   (exp-to-string-arith-prop prop) ")"
		   "[" (exp-to-string-ArithProof-deriv body) "]")))
	(t
	 (let* ((list (exp-match exp
				 (ParseString
				  "WILD_CARD by WILD_CARD WILD_CARD"
				  'ArithProof-deriv)))
		(judgment (1st list))
		(rule-name (2nd list))
		(deriv-list (3rd list)))
	   (concat (exp-to-string-ArithProof-judgment judgment) " by "
		   (exp-to-string-const rule-name) " {"
		   (exp-to-string-ArithProof-deriv-list deriv-list) "}")))))

(defun exp-to-string-ArithProof-deriv-list (deriv-list)
  (cond ((null deriv-list) "")
	((= (length deriv-list) 1) 
	 (exp-to-string-ArithProof-deriv (1st deriv-list)))
	(t (concat (exp-to-string-ArithProof-deriv (1st deriv-list)) "; "
		   (exp-to-string-ArithProof-deriv-list (but-1st deriv-list))))
	))

(defun exp-to-string-ArithProof-judgment (judgment)
  (exp-to-string-arith-prop judgment))

;; =====================================================================
;; 算術の証明項 (ArithProofTerm)
;; =====================================================================

(defun exp-to-string-ArithProofTerm-deriv (exp)
  (cond ((exp-varref-p exp) (exp-to-string-varref exp))
	((exp-assume-p exp)
	 (let* ((prop (exp-assume-dest1 exp))
		(var (exp-assume-dest2 exp))
		(body (exp-assume-dest3 exp)))
	   (concat "(" (exp-to-string-var var) ":" 
		   (exp-to-string-arith-prop prop) ")"
		   "[" (exp-to-string-ArithProofTerm-deriv body) "]")))
	(t
	 (let* ((list (exp-match exp
				 (ParseString
				  "WILD_CARD by WILD_CARD WILD_CARD"
				  'ArithProof-deriv)))
		(judgment (1st list))
		(rule-name (2nd list))
		(deriv-list (3rd list)))
	   (concat (exp-to-string-ArithProofTerm-judgment judgment) " by "
		   (exp-to-string-const rule-name) " {"
		   (exp-to-string-ArithProofTerm-deriv-list deriv-list)
		   "}")))))

(defun exp-to-string-ArithProofTerm-deriv-list (deriv-list)
  (cond ((null deriv-list) "")
	((= (length deriv-list) 1) 
	 (exp-to-string-ArithProofTerm-deriv (1st deriv-list)))
	(t (concat (exp-to-string-ArithProofTerm-deriv (1st deriv-list)) "; "
		   (exp-to-string-ArithProofTerm-deriv-list 
		    (but-1st deriv-list))))
	))

(defun exp-to-string-ArithProofTerm-judgment (judgment)
  (let ((term (2nd judgment))
	(prop (3rd judgment)))
    (concat (exp-to-string-arith-proof-term term) ":"
	    (exp-to-string-arith-prop prop))))

;; data handling functions

(setq exp-wild-card '(op "WILD_CARD"))
(setq exp-dummy-card '(op "DUMMY_CARD"))

(defun exp-wild-card-p (exp)
  (equal exp-wild-card exp))

(defun exp-dummy-card-p (exp)
  (equal exp-dummy-card exp))

(defun exp-atom-p (exp)
  (and (listp exp)
       (= (length exp) 2)
       (memq (1st exp) '(op var varref))))

(defun exp-match (exp pat)
  "match EXP against PATtern. return a list of matched EXP.
return NO-MATCH if the matching fails."
 (catch 'match-fail
   (exp-match-body exp pat)))

(defun exp-match-body (exp pat)
  (cond ((null pat) (if (null exp) () (throw 'match-fail 'no-match)))
	((exp-wild-card-p pat) (list exp))
	((exp-dummy-card-p pat) ())
	((exp-atom-p pat) 
	 (if (equal pat exp) () (throw 'match-fail 'no-match)))
	((exp-abs-p pat)
	 (if (exp-abs-p exp)
	     (let* ((list-p (exp-abs-dest pat))
		    (var-p (1st list-p))
		    (term-p (2nd list-p))
		    (list-t (exp-abs-dest exp))
		    (var-t (1st list-t))
		    (term-t (2nd list-t)))
	       (append (exp-match-body var-t var-p)
		       (exp-match-body term-t term-p)))
	   (throw 'match-fail 'no-match)))
	((null exp) (throw 'match-fail 'no-match))
	((exp-atom-p exp) (throw 'match-fail 'no-match))
	(t (append (exp-match-body (car exp) (car pat))
		   (exp-match-body (cdr exp) (cdr pat))))))

(defun exp-match-string (str pat &optional class)
  "match STR against PATtern. return a list of matched EXP.
return NO-MATCH if the matching fails.  CLASS is the syntactic class
used to parse strings."
  (setq class (or class 'form))
  (exp-match (ParseString str class) (ParseString pat class)))

(defun exp-leaf (atom exp)
  "check if ATOM is a leaf of EXP."
  (cond ((null exp) nil)
	((exp-atom-p exp) (equal atom exp))
	((exp-leaf atom (car exp)) t)
	(t (exp-leaf atom (cdr exp)))))

;; =====================================================================
;; Curry-Howard Isomorphism
;; =====================================================================

(defun exp-type-of-varref (varref hyp-seq)
  "Compute type of VARREF under HYP-SEQ."
  (if (null hyp-seq)
      (error "Unbound variable")
    (let* ((hyp (1st hyp-seq))
	   (list (exp-hyp-dest hyp))
	   (var (1st list))
	   (type (2nd list))
	   (hyp-seq-rest (but-1st hyp-seq)))
      (if (exp-var-p varref)
	  (if (equal varref var) type
	    (exp-type-of-varref varref hyp-seq-rest))
	(if (equal (exp-varref-var-part varref) var)
	    (exp-type-of-varref (exp-pull-sharp-var varref) hyp-seq-rest)
	  (exp-type-of-varref varref hyp-seq-rest))))))

(defun exp-type-of (exp hyp-seq)
  "Compute the type of EXP, which is a lambda-term, under HYP-SEQ."
  (if (exp-varref-p exp) 
      (exp-type-of-varref exp hyp-seq)
    (let ((op (1st exp))
	  (body (but-1st exp)))
      (cond ((equal op-lambda op)
	     (let* ((prop (1st body))
		    (abs (2nd body))
		    (list (exp-abs-dest abs))
		    (var (1st list))
		    (body (2nd list)))
	       (exp-imp-cons 
		prop 
		(exp-type-of body (cons (exp-hyp-cons var prop) hyp-seq)))))
	    ((equal op-apply op)
	     (let* ((fun (1st body))
		    (list (exp-imp-dest (exp-type-of fun hyp-seq))))
	       (2nd list)))
	    ((equal op-pair op)
	     (let ((left (1st body))
		   (right (2nd body)))
	       (exp-and-cons 
		(exp-type-of left hyp-seq) (exp-type-of right hyp-seq))))
	    ((equal op-left op)
	     (let* ((arg (1st body))
		    (list (exp-and-dest (exp-type-of arg hyp-seq))))
	       (1st list)))
	    ((equal op-right op)
	     (let* ((arg (1st body))
		    (list (exp-and-dest (exp-type-of arg hyp-seq))))
	       (2nd list)))
	    ((equal op-inl op)
	     (let ((prop (1st body))
		   (term (2nd body)))
	       (exp-or-cons (exp-type-of term hyp-seq) prop)))
	    ((equal op-inr op)
	     (let ((prop (1st body))
		   (term (2nd body)))
	       (exp-or-cons prop (exp-type-of term hyp-seq))))
	    ((equal op-case op)
	     (let* ((term (1st body))
		    (list-or (exp-or-dest (exp-type-of term hyp-seq)))
		    (prop-left (1st list-or))
		    (abs1 (2nd body))
		    (list-abs (exp-abs-dest abs1))
		    (var1 (1st list-abs))
		    (term1 (2nd list-abs)))
	       (exp-type-of 
		term1 
		(cons (exp-hyp-cons var1 prop-left) hyp-seq))))
	    ((equal op-abort op)
	     (let ((prop (1st body))) prop))
	    (t (throw 'exp-P2L "sorry"))))))

;; =====================================================================
;; λ項 -> 命題論理の証明
;; =====================================================================

(defun exp-L2P (exp hyp-seq)
  "Convert lambda-term EXP to prop-proof under HYP-SEQ."
  (if (exp-varref-p exp) 
      exp
    (let ((op (1st exp))
	  (body (but-1st exp)))
      (cond ((equal op-lambda op)
	     (let* ((prop (1st body))
		    (abs (2nd body))
		    (list (exp-abs-dest abs))
		    (var (1st list))
		    (body (2nd list)))
	      (exp-imp-intro-cons
	       var
	       prop
	       (exp-L2P body (cons (exp-hyp-cons var prop) hyp-seq))
	       hyp-seq)))
	    ((equal op-apply op)
	     (let* ((fun (1st body))
		    (arg (2nd body)))
	       (exp-imp-elim-cons 
		(exp-L2P fun hyp-seq) (exp-L2P arg hyp-seq) hyp-seq)))
	    ((equal op-pair op)
	     (let ((left (1st body))
		   (right (2nd body)))
	       (exp-and-intro-cons 
		(exp-L2P left hyp-seq) (exp-L2P right hyp-seq) hyp-seq)))
	    ((equal op-left op)
	     (let ((arg (1st body)))
	       (exp-and-elim-left-cons (exp-L2P arg hyp-seq)  hyp-seq)))
	    ((equal op-right op)
	     (let ((arg (1st body)))
	       (exp-and-elim-right-cons (exp-L2P arg hyp-seq)  hyp-seq)))
	    ((equal op-inl op)
	     (let ((prop (1st body))
		   (term (2nd body)))
	       (exp-or-intro-left-cons prop (exp-L2P term hyp-seq)  hyp-seq)))
	    ((equal op-inr op)
	     (let ((prop (1st body))
		   (term (2nd body)))
	       (exp-or-intro-right-cons prop (exp-L2P term hyp-seq)  hyp-seq)))
	    ((equal op-case op)
	     (let* ((term (1st body))
		    (list (exp-or-dest (exp-type-of term hyp-seq)))
		    (prop-l (1st list))
		    (prop-r (2nd list))
		    (abs1 (2nd body))
		    (list1 (exp-abs-dest abs1))
		    (var1 (1st list1))
		    (body1 (2nd list1))
		    (abs2 (3rd body))
		    (list2 (exp-abs-dest abs2))
		    (var2 (1st list2))
		    (body2 (2nd list2)))
	       (exp-or-elim-cons
		(exp-L2P term hyp-seq)
		var1
		(exp-L2P body1 (cons (exp-hyp-cons var1 prop-l) hyp-seq))
		var2
		(exp-L2P body2 (cons (exp-hyp-cons var2 prop-r) hyp-seq))
		hyp-seq)))
	    ((equal op-abort op)
	     (let ((prop (1st body))
		   (term (2nd body)))
	       (exp-abort-cons prop (exp-L2P term hyp-seq))))
	    (t (throw 'exp-L2P "sorry"))))))

;; =====================================================================
;; 命題論理の証明 -> λ項 
;; =====================================================================

(defun exp-P2L (exp hyp-seq)
  "Convert a PropProof-deriv EXP into corresponding lambda-term
under HYP-SEQ."
  (if (exp-varref-p exp) 
      exp
    (let ((concl (1st exp))
	  (rule-name (2nd exp))
	  (subproofs (3rd exp)))
      (cond ((equal rule-imp-intro rule-name)
	     (let* ((assume-proof (1st subproofs))
		    (prop (exp-assume-dest1 assume-proof))
		    (var (exp-assume-dest2 assume-proof))
		    (body (exp-assume-dest3 assume-proof)))
	       (exp-lamterm-cons 
		var 
		prop 
		(exp-P2L body (cons (exp-hyp-cons var prop) hyp-seq)))))
	    ((equal rule-imp-elim rule-name)
	     (let ((proof1 (1st subproofs))
		   (proof2 (2nd subproofs)))
	       (exp-appterm-cons (exp-P2L proof1 hyp-seq)
				 (exp-P2L proof2 hyp-seq))))
	    ((equal rule-and-intro rule-name)
	     (let ((proof1 (1st subproofs))
		   (proof2 (2nd subproofs)))
	       (exp-pairterm-cons (exp-P2L proof1 hyp-seq)
				  (exp-P2L proof2 hyp-seq))))
	    ((equal rule-and-elim-left rule-name)
	     (let ((proof (1st subproofs)))
	       (exp-leftterm-cons (exp-P2L proof hyp-seq))))
	    ((equal rule-and-elim-right rule-name)
	     (let ((proof (1st subproofs)))
	       (exp-rightterm-cons (exp-P2L proof hyp-seq))))
	    ((equal rule-or-intro-left rule-name)
	     (let ((prop (exp-or-dest2 concl))
		   (proof (1st subproofs)))
	       (exp-inlterm-cons prop (exp-P2L proof hyp-seq))))
	    ((equal rule-or-intro-right rule-name)
	     (let ((prop (exp-or-dest1 concl))
		   (proof (1st subproofs)))
	       (exp-inrterm-cons prop (exp-P2L proof hyp-seq))))
	    ((equal rule-or-elim rule-name)
	     (let* ((proof (1st subproofs))
		    (proof-l (2nd subproofs))
		    (prop-l (exp-assume-dest1 proof-l))
		    (var-l (exp-assume-dest2 proof-l))
		    (body-l (exp-assume-dest3 proof-l))
		    (proof-r (3rd subproofs))
		    (prop-r (exp-assume-dest1 proof-r))
		    (var-r (exp-assume-dest2 proof-r))
		    (body-r (exp-assume-dest3 proof-r)))
	       (exp-caseterm-cons
		(exp-P2L proof hyp-seq)
		var-l 
		(exp-P2L body-l (cons (exp-hyp-cons var-l prop-l) hyp-seq))
		var-r 
		(exp-P2L body-r (cons (exp-hyp-cons var-r prop-r) hyp-seq)))))
	    ((equal op-abort rule-name)
	     (let ((prop (1st body))) prop))
	    (t (throw 'exp-P2L "sorry"))))))

;;;;;;;;;;;;;;;;;;;;;
;; 正規形のチェック
;;;;;;;;;;;;;;;;;;;;;

(defun exp-normal-lambda-term-p (exp)
  "check if the given TREE is a normal form in LambdaTerm."
  (cond ((exp-atom-p exp) t)
	((exp-lamterm-p exp)
	 (exp-normal-lambda-term-p (3rd (exp-lamterm-dest exp))))
	((exp-appterm-p exp)
	 (let* ((list (exp-appterm-dest exp))
		(fun (1st list))
		(arg (2nd list)))
	   (if (exp-lamterm-p fun)
	       ;; it's a redex!
	       nil
	     (and (exp-normal-lambda-term-p fun)
		  (exp-normal-lambda-term-p arg)))))
	((exp-pairterm-p exp)
	 (let* ((list (exp-pairterm-dest exp))
		(left (1st list))
		(right (2nd list)))
	   (and (exp-normal-lambda-term-p left)
		(exp-normal-lambda-term-p right))))
	((exp-leftterm-p exp)
	 (let* ((list (exp-leftterm-dest exp))
		(term (1st list)))
	   (if (exp-pairterm-p term)
	       ;; it'a redex!
	       nil
	     (exp-normal-lambda-term-p term))))
	((exp-rightterm-p exp)
	 (let* ((list (exp-rightterm-dest exp))
		(term (1st list)))
	   (if (exp-pairterm-p term)
	       ;; it'a redex!
	       nil
	     (exp-normal-lambda-term-p term))))
	((exp-inlterm-p exp)
	 (let* ((list (exp-inlterm-dest exp))
		(term (2nd list)))
	   (exp-normal-lambda-term-p term)))
	((exp-inrterm-p exp)
	 (let* ((list (exp-inrterm-dest exp))
		(term (2nd list)))
	   (exp-normal-lambda-term-p term)))
	((exp-caseterm-p exp)
	 (let* ((list (exp-caseterm-dest exp))
		(case (1st list))
		(left (3rd list))
		(right (5th list)))
	   (if (or (exp-inlterm-p case) (exp-inrterm-p case))
	       ;; it's a redex
	       nil
	     (and (exp-normal-lambda-term-p left)
		  (exp-normal-lambda-term-p right)))))
	((exp-abs-p exp)
	 (let* ((list (exp-abs-dest exp))
		(term (2nd list)))
	   (exp-normal-lambda-term-p term)))
	(t 
	 ;; exp is a struct
	 (exp-normal-lambda-struct-term-p exp))
	))

(defun exp-normal-lambda-struct-term-p (list)
  (if (null list)
      ;; it is normal!
      t
    (and (exp-normal-lambda-term-p (1st list))
	 (exp-normal-lambda-struct-term-p (but-1st list)))))
