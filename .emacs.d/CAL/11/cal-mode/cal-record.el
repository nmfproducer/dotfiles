;; version 0.8 Tue Feb 18 16:02:20 2003 for media
;; version 0.7 Wed Oct 30 10:43:23 2002 for media
;; version 0.6 Sat Apr 21 00:36:27 2001
;; private install に対応。
;; record file の生成と管理。
;; ~msatou/logifiles/ユーザ名 の directory を owner msatou でつくり、
;; その下に cal-record を owner msatou でつくる。
;; ユーザのセッション終了時には、user の directory に一旦 cal-record
;; を作成し、それを logfiles に copy する。

;; version: 0.5 Thu Oct 26 16:22:09 2000
;; 問題別正解者数を整理して出力するようにした.

;; version: 0.4 Thu Oct 26 16:22:09 2000
;; cal-mk-myrecord: 書きだしたファイルの owner をそれぞれのユーザに
;; chown で変更するようにした.
;; 変数 max-problems-number を導入. 問題の最大数

;; version: 0.3 Sat Dec 18 16:10:28 1999
;; process cal-record files

;; site dependent set up
;; set up for io
(setq cal-dir "~masahiko/system/cal/03/")
(setq cal-system-log-dir "~igarashi/callog/")
(setq cal-topten-record-file "~masahiko/public_html/cal/03/cal-mode/record")

(load-file (concat cal-dir "cal-mode/cal.elc"))

(setq max-problems-number 300) ;; at most 300 problems

(defun cal-remove-esc ()
  "Remove backslash char used for encoding."
  (while (search-forward "\\" nil t)
    (delete-char -1)
    (forward-char 1)))

(defun cal-mk-user-list (date)
  (let* ((dir (expand-file-name cal-system-log-dir))
	 (record-dir (concat dir "dotcal/record/"))
	 (topten-dir (concat dir "dotcal/topten/"))
	 (passwd-file (concat dir "passwd"))
	 p id list vector (plist nil)
	 (total-solved-problems 0)
	 total-no-of-users
	 (problems (cal-Q nil))
	 average
	 cal-user-id-list 
	 cal-user-name-list
	 cal-user-id ;; localize this variable
	 cal-solved-list ;; localize this variable
	 )
    (save-excursion
      (set-buffer (get-buffer-create " *CALPASSWD*"))
      (insert-file-contents passwd-file))
    (save-excursion
      (setq cal-user-id-list nil)
      (set-buffer (get-buffer-create " *CALRECORD*"))
      (erase-buffer)
      (call-process cal-ls nil t nil "-l" (concat dir "logfiles/"))
      (goto-char (point-min))
      (end-of-line)
      (delete-region (point-min) (1+ (point)))
      (while (not (eobp))
	(end-of-line)
	(setq p (point))
	(search-backward " ")
	(setq id (buffer-substring (1+ (point)) p))
	(or (member id cal-avoid-list)
	    (setq cal-user-id-list (cons id cal-user-id-list)))
	(end-of-line)
	(forward-char 1))
      (erase-buffer)
      ;; we use vector of length MAX-PROBLEMS-NUMBER to hold the list of users
      ;; who solved problems 1..MAX-PROBLEMS-NUMBER
      (setq vector (make-vector max-problems-number nil))
      (setq list cal-user-id-list)
      (setq total-no-of-users 0)
      (while list
	(let* ((user (car list))
	       (user-record-file
		(expand-file-name
		 (concat dir "logfiles/" user "/cal-record")))
	       string)
	  (erase-buffer)
	  (if (and
	       (file-exists-p user-record-file)
	       (file-readable-p user-record-file))
	      (progn
		(setq total-no-of-users (1+ total-no-of-users))
		(insert-file-contents user-record-file)))
	  (if (= (buffer-size) 0)
	      ;; forget about the current user and continue with the rest
	      (setq list (cdr list))
	    (progn
	      ;; decode contents of cal-record
	      (cal-remove-esc)
	      ;; compute the value of cal-solved-list
	      (goto-char (point-min))
	      (search-forward "cal-solved-list")
	      (beginning-of-line)
	      (delete-region (point-min) (point))
	      (end-of-line)
	      (delete-region (1+ (point)) (point-max))
	      (eval-current-buffer)
	      ;; we now have the value of cal-solved-list.
	      (setq cal-user-name-list
		    (cons 
		     (cal-get-name-part user)
		     cal-user-name-list))
	      (let ((solved-list cal-solved-list))
		(setq total-solved-problems
		      (+ total-solved-problems (length cal-solved-list)))
		(while solved-list
		  (let* ((index (1- (car solved-list)))
			 ;; ulist is the list of users who solved
			 ;; problem (index + 1)
			 (ulist (aref vector index)))
		    (setq ulist
			  (cons (cal-get-name-part user) ulist))
		    (aset vector index ulist)
		    (setq solved-list (cdr solved-list)))))
	      (setq list (cdr list))))))
      ;; now make table
      (erase-buffer)
      (let ((index 0) ulist)
	(while (< index max-problems-number)
	  (if (setq ulist (aref vector index))
	      (progn
		(insert
		 (format
		  "Problem[%3d] solved by the following %s students.\n"
		  (1+ index) (length ulist)))
		(while ulist
		  (insert (car ulist) "\n")
		  (setq ulist (cdr ulist)))
		(insert "\C-l")))
	  (setq index (1+ index))))
      ;; we no longer use this, but simply call write-region
      ;;(cal-write-region (point-min) (point-max) (concat record-dir date))
      (write-region (point-min) (point-max) (concat record-dir date))
      (setq list cal-user-name-list)
      ;; compute total points and number of problems solved
      ;; of each user, and make a list of them
      (while list
	(let ((user (car list)) (index 0) (points 0) (solved 0) ulist)
	  (while (< index max-problems-number)
	    (setq ulist (aref vector index))
	    (if (member user ulist)
		(setq points (+ points (/ 1000 (length ulist)))
		      solved (1+ solved)))
	    (setq index (1+ index)))
	  (setq plist
		(cons (format "%d  %6d  %s" points solved user) plist))
	  (setq list (cdr list))))
      (erase-buffer)
      (while plist
	;; we attach - to make a negative number
	(insert "-" (car plist) "\n")
	(setq plist (cdr plist)))
      (cal-sort)
      (goto-char (point-min))
      (insert (format "演習参加者数:   %3d名\n" total-no-of-users))
      (insert (format "総問題数:       %3d題\n" problems))
      (insert (format "平均正解数:    %s題\n\n"
		      (decimal-one
		       (/ (* 10 total-solved-problems)
			  total-no-of-users))))
      ;; next we make a list of (problem-no, no-of-solvers)
      (insert "問題別正解者数\n\n")
      (insert "    ----------------------------------------------------------------------\n")
      (insert "    問題番号 |  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19\n")
      (insert "    ----------------------------------------------------------------------")
      (let ((index 1) rem)
	(while (<= index problems)
	  (if (or (= index 1) (= (mod index 20) 0))
	      (insert (format "\n         %3d |" (* 20 (/ index 20)))))
	  (if (= index 1) (insert "   "))
	  (insert (format "%3d" (length (aref vector (1- index)))))
	  (setq index (1+ index)))
	(insert "\n"))
      (insert "    ----------------------------------------------------------------------\n")
      (insert "\n")
      (insert "        --------------------------------\n"
	      "        順位  総得点  正解数  氏名\n"
	      "        --------------------------------\n")
      (goto-char (point-max))
      (insert "        --------------------------------\n\n\n")
      ;;(cal-write-region (point-min) (point-max) (concat topten-dir date))
      (write-region (point-min) (point-max) (concat topten-dir date))
      ;; now make top ten list and save it.
      ;; we list users who solved >= (+ 3 average) problems
      (goto-char (point-min))
      (insert (format "[%s]\n;; CAL record: %s\n\n"
		      date (current-time-string)))
      (goto-char (point-max))
      (search-backward "----------")
      (beginning-of-line)
      (backward-char 1)
      (beginning-of-line)
      (let ((cont t)
	    (boundary (max (+ 5 average) (+ 4 average (/ average 5)))))
	(while cont
	  (goto-char (+ (point) 25))
	  (if (or
	       (looking-at "---");; list of users exhausted
	       (>= (string-to-int (buffer-substring (point) (+ 3 (point))))
		   boundary))
	      (setq cont nil)
	    ;; otherwise continue
	    (beginning-of-line)
	    (backward-char 1)
	    (beginning-of-line))))
      (end-of-line)
      (insert "\n        --------------------------------\n-----\n")
      (delete-region (point) (point-max))
      ;; we no longer use this, but just call write-region
      ;; (cal-append-buffer (concat cal-dir "cal-mode/record"))
      (write-region (point-min) (point-max) cal-topten-record-file t)
      ;; call chmod to make the file group readable and writable.
      (cal-call-process cal-chmod "664" cal-topten-record-file)
      (kill-buffer " *CALPASSWD*")
      (kill-buffer " *CALRECORD*")
      )))

;; STR is something like: "佐藤雅彦,1998,1234"
;; obsolete
;;(defun cal-get-name-part (str)
  ;;(string-match "[^,]+" str)
  ;;(substring str 0 (match-end 0)))

(defun cal-get-name-part (user)
  "get real name of USER by using PASSWD file"
  (save-excursion
    ;;(set-buffer (get-buffer-create " *CALPASSWD*"))
    (goto-char (point-min))
    (if (re-search-forward 
	 (concat "^" (regexp-quote user) ".*name=\"\\([^\"]*\\)\"")
	 nil t)
	(buffer-substring (match-beginning 1) (match-end 1))
      (error ""))))

(defun decimal-one (num)
  (let ((int (/ num 10))
	(dec (mod num 10)))
    ;; compute average as side effect
    (setq average int)
    (format "%d.%d" int dec)))

(defun cal-sort ()
  (let ((rank 0) (line 0) p (oldnum -1))
    (sort-numeric-fields 1 (point-min) (point-max))
    (goto-char (point-min))
    (while (not (eobp))
      ;; delete - sign
      (delete-char 1)
      (setq p (point))
      (skip-chars-forward "[0-9]")
      (setq num (string-to-int (buffer-substring p (point))))
      (delete-region p (point))
      (if (= num oldnum)
	  ;; this one has the same points as the previous, so
	  ;; just increment line
	  (setq line (1+ line))
	(setq oldnum num)
	(setq line (1+ line))
	(setq rank line))
      (beginning-of-line)
      (insert (format "          %2d  %6d" rank num))
      (end-of-line)
      (forward-char 1))))

;; 演習参加者数:    57名
;; 総問題数:        54題
;; 平均正解数:    11.1題
;;        --------------------------------
;;        順位  総得点  正解数  login name
;;        --------------------------------
;;           1   35660      54  7atarasi

(defun cal-mk-myrecord (date &optional remove)
  (let ((dir (expand-file-name cal-system-log-dir))
	(topten-dir (concat cal-dir "dotcal/topten/"))
	p q
	solved-problems
	total-no-of-users
	problems
	average
	order
	points
	login-name)
    (set-buffer (get-buffer-create " *CALWORK*"))
    (erase-buffer)
    (insert-file-contents (concat topten-dir date))
    (goto-char (point-min))
    (search-forward ":")
    (skip-chars-forward " ")
    (setq p (point))
    (end-of-line)
    (backward-char 1)
    (setq total-no-of-users (buffer-substring p (point)))
    (search-forward ":")
    (skip-chars-forward " ")
    (setq p (point))
    (end-of-line)
    (backward-char 1)
    (setq problems (buffer-substring p (point)))
    (search-forward ":")
    (skip-chars-forward " ")
    (setq p (point))
    (end-of-line)
    (backward-char 1)
    (setq average (buffer-substring p (point)))
    (search-forward "login name")
    (next-line 2)
    (beginning-of-line)
    (save-excursion
     (search-forward "-----")
     (beginning-of-line)
     (setq p (point-marker)))
    (while (< (point) p)
      (skip-chars-forward " ")
      (setq q (point))
      (search-forward " ")
      (setq order (buffer-substring q (1- (point))))
      (skip-chars-forward " ")
      (setq q (point))
      (search-forward " ")
      (setq points (buffer-substring q (1- (point))))
      (skip-chars-forward " ")
      (setq q (point))
      (search-forward " ")
      (setq solved-problems (buffer-substring q (1- (point))))
      (skip-chars-forward " ")
      (setq q (point))
      (end-of-line)
      (setq login-name (buffer-substring q (point)))
      ;; now write record. first prepare data string in the current buffer.
      (setq q (point))
      (insert
       (format "%s  %4s/%2s       %3s/%3s     (%10s)  %6s  %s\n"
	       date order total-no-of-users solved-problems
	       problems average points login-name))
      (setq myrecord-file
	    (if nil ;; t for testing
		(concat dir "masahiko" "/myrecord")
	      (concat dir login-name "/myrecord")))
      (if remove
	  (if (file-exists-p myrecord-file)
	      (call-process cal-rm nil t nil myrecord-file))
	(or (file-exists-p myrecord-file)
	    (call-process cal-touch nil t nil myrecord-file))
	(append-to-file q (point) myrecord-file)
	(cal-call-process cal-chmod "644" myrecord-file)
	;; change the owner 
	(cal-call-process cal-chown login-name myrecord-file)
	)
      (delete-region q (point))
      (forward-char 1))))

(defun doit ()
  (let ((error-occurred nil))
    (condition-case conditions
	(let ((date (cal-today)))
	  (cal-mk-user-list date)
	  ;;(cal-mk-myrecord date)
	  )
      (error
       (setq error-occurred t)))
    (if error-occurred
	(cal-mail-to-kougi
	 "Error in dot-cal"
	 "Couldn't make CAL user list!"))))

(doit)
