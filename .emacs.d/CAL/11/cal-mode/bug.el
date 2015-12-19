
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
		  (message "�ե�����Υ��åץ��ɤ˼��Ԥ��ޤ���! ������TA��Ϣ���Ƥ���������")
		  (sit-for 3)
		  (error ""))
	      ;; Q[1] is not solved, so do nothing
	      ))
	;; cal-record-file is broken!
	(cal-upload cal-record-file cal-upload-url 
		    (concat "Broken-cal-record-"
			    (replace-regexp-in-string " " "-" 
						      (current-time-string))))
	(message "CAL �ε�Ͽ�ե����뤬����������ޤ��󡥶�����TA��Ϣ���Ƥ�������.")
	(sit-for 3)
	(message "")
	(error ""))
    (error "CAL �ε�Ͽ�ե����뤬����ޤ���. ������TA��Ϣ���Ƥ�������."))
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
	 "\t\t        �褦�����ַ׻��������פ�������!   \n\n"
	 "\t\t      �����:  ��ƣ ��ɧ,  �޽��� ��\n\n"
	 "\t\t    ���������ر� ����ظ���� ��ǽ������칶\n"
	 "\t\t************************************************\n\n"
	 (cal-mode-version) "\n"
	 "����� cal@sato.kuis.kyoto-u.ac.jp �Ǥ�����դ��ޤ�.\n\n"
	 )))
  (if (= cal-session-count 1)
      (progn
	(cal-insert "����Ϻǽ�� CAL �Υ��å����Ǥ�.\n"
		    "�������뤬�ץ��ץȡ�CAL <1> �ۤ�ľ��ˤ��뤳�Ȥ��ǧ����,\n"
		    "��news[1]�٤ȥ����פ��Ƥ�������."))
    ;; check new news
    (setq cal-no-of-news (cal-news nil))
    (unless (= cal-no-of-news (length cal-news-list))
      (cal-insert "̤�ɤΥ˥塼��������ޤ�.\n"
		  "��news[]�٤�̤�ɤΥ˥塼���������ɤळ�Ȥ��Ǥ��ޤ�."))
    ;; check if cal-user-id is set
    (unless cal-user-id
      (cal-insert "����ɾ����ɬ�פʥǡ�����Ͽ���뤿���, ɬ�� Q[1]\n"
		  "��������Ƥ�������!")))
  (cal-insert)
  (setq cal-prompt-count 0)
  (cal-increment-prompt-count)
  (use-local-map cal-mode-map)
  (cal-set-syntax)
  (set-syntax-table cal-mode-syntax-table)
  (setq parse-sexp-ignore-comments t)
  (setq major-mode 'cal-mode)
  (setq mode-name "�׻�������")
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
    (open  "(" "<" "{" "[" "��")
    (close ")" ">" "}" "]" "��")
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
     "��"
     "��"  ;; for universal judgment
     "��"
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
    (message "������ɤ߹���Ǥ��ޤ�...") 
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
      (message "�������ɤ߹���Ǥ��ޤ�...")
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
