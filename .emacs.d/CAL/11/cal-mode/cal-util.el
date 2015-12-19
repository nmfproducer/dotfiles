;; cal-util: version 7.00 of Wed Dec 31 23:17:46 2003
;; cal-util: version 4.04 of Mon Nov 17 22:53:15 2003
;; cal-util: version 4.03 of 2001.06.25
;; cal-util: version 4.02 of 2001.06.06
;; cal-util: version 4.01 of 2001.05.29
;; cal-util: version 2.03 of October 18, 1999.
;; cal-send-mail: cal-use-mail が nil のときの動作を修正．
;; cal-util: version 2.02 of October 16, 1999.
;; cal-user-name を encryption の key にした.
;; cal-utii: version 2.01 of October 13, 1999.
;; (user-name) を key にせずに encrypt するようにした．
;; domainname が isle かをチェックするようにした．
;; cal-utii: version 1.03 of October 29, 1998.
;; cal-utii: version 1.02 of October 26, 1998.
;; cal-utii: version 1.01 of October 22, 1998.
;; utitity functions

;; taken from skk
;; create file

(defun cal-create-file (file &optional message)
  "Create an empty file named FILE if the file FILE does not exist already.
Return t if created."
  (let ((file (expand-file-name file)))
    (if (file-exists-p file)
	nil
      (write-region (point-min) (point-min) file nil 0)
      (if message
	  (progn
	    (message message)
	    (sit-for 3)))
      t)))

(defun cal-load-file (file)
  (cal-call-process cal-chmod "644" file)
  (sit-for 0.1)
  (load file nil t t)
  (cal-call-process cal-chmod "600" file)
  )

(defun cal-eval-file (file)
  (let (val)
    (save-excursion
      (set-buffer (get-buffer-create " *CALTMP*"))
      (erase-buffer)
      (insert-file-contents file)
      (goto-char (point-min))
      (insert "(setq val ")
      (goto-char (point-max))
      (insert ")")
      (eval-current-buffer)
      val)))

;; encryption

;; ファイルの変更や他人のファイルのコピーを防ぐためにファイルの先頭に固有の
;; 文字列を挿入する.
;; 
;; (length (current-time-string)) = 24 で以下のフォーマットであること
;; を仮定する. "Sat Oct 10 19:54:30 1998"

(defun cal-encrypt-number (number n1 n2 n3)
  "simple encryption function"
  (let ((key cal-user-name)
	(init (mod (+ number 77) 128))
	(step 71)
	(str "")
	(count 0)
	key2
	)
    (if (string= "" key) (setq key "Who are you?"))
    (while (< (length key) 20)
      (setq key (concat key key)))
    (while (< count 20)
      (setq step (mod (+ step count init) 128))
      (setq key2
	    (cond ((= count 0) n3)
		  ((= count 7) n2)
		  ((= count 14) n1)
		  (t 17)))
      (setq str (concat
		 (char-to-string
		  (cal-convert (mod (+ (aref key count) step key2) 128))
		  )
		 str))
      (setq count (1+ count)))
    str))

(defun cal-encrypt-buffer ()
  "insert encrypted string at the beginning of current buffer."
  (interactive)
  (let* (number
	 (time (current-time-string))
	 (n1 (string-to-number (substring time 11 13)))
	 (n2 (string-to-number (substring time 14 16)))
	 (n3 (string-to-number (substring time 17 19))))
    (setq number (cal-count))
    (goto-char (point-min))
    (insert ";; " time "\n")
    (goto-char (point-min))
    (insert ";; " (cal-encrypt-number number n1 n2 n3) "\n")))

(defun cal-correct-buffer ()
  "check if buffer is correct."
  (interactive)
  (let (number n1 n2 n3)
    (if (< (buffer-size) 52)
	;; buffer is too small!
	nil
      (goto-char 53);; this should be the beginning of the real content
      (setq number (cal-count (point)))
      (setq n1 (string-to-number (buffer-substring 39 41)))
      (setq n2 (string-to-number (buffer-substring 42 44)))
      (setq n3 (string-to-number (buffer-substring 45 47)))
      (string= (cal-encrypt-number number n1 n2 n3)
	       (buffer-substring 4 24)))))

(defun cal-correct-file (file)
  "check if FILE is correct.  If correct, return the size of file."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((size (buffer-size)))
      (if (= size  0)
	  0
	(if (cal-correct-buffer) size nil)))))

(defun cal-count (&optional num)
  (let ((c 0))
    (goto-char (or num (point-min)))
    (while (not (eobp))
      (setq c (mod (+ c (following-char)) 128))
      (forward-char 1))
    c))

(defun cal-save-buffer ()
  "save buffer to cal-record-file file"
  (cal-encrypt-buffer)
  (write-region (point-min) (point-max) cal-record-file))

(defun cal-convert (num)
  "convert num from 0 to 127 to alpha-numeric char."
  (if (< num 32) (+ num 32) num))

;; encode-decode

(defun cal-enc-str (str &optional seed)
  (let ((len (length str))
	(result "")
	(i 0)
	(seed (or seed (user-login-name))))
    (while (< i len)
      (setq result
	    (concat result (char-to-string (cal-enc (aref str i) seed i))))
      (setq i (1+ i)))
    result))

(defun cal-dec-str (str &optional seed)
  (let ((len (length str))
	(result "")
	(i 0)
	(seed (or seed (user-login-name))))
    (while (< i len)
      (setq result
	    (concat result (char-to-string (cal-dec (aref str i) seed i))))
      (setq i (1+ i)))
    result))

(defun cal-enc (n seed i)
  (let* ((seed-len (length seed))
	 (index (mod i seed-len))
	 (shift (+ 57 index (* 13 (aref seed index)))))
  (+ 32 (mod (+ (- n 32) i shift 77) 96))))


(defun cal-dec (n seed i)
  (let* ((seed-len (length seed))
	 (index (mod i seed-len))
	 (shift (+ 57 index (* 13 (aref seed index)))))
  (+ 32 (mod (- (- n 32) (+ i shift 77)) 96))))


;; cal today

(defun cal-today ()
  "compute date string for today.  e.g., if today is Nov 7, then returns
\"11.07\""
  (let* ((str (current-time-string))
	 (mon (cdr (assoc (substring str 4 7)
		      '(("Jan" . "01") ("Feb" . "02") ("Mar" . "03")
			("Apr" . "04") ("May" . "05") ("Jun" . "06")
			("Jul" . "07") ("Aug" . "08") ("Sep" . "09")
			("Oct" . "10") ("Nov" . "11") ("Dec" . "12")))))
	 (day (if (string= (substring str 8 9) " ")
		  (concat "0" (substring str 9 10))
		(substring str 8 10))))
    (concat mon "." day)))

;; unix commands

(defun cal-remove-file (file)
  (when (and file (file-exists-p file))
    (cal-call-process cal-rm file)))

(defun cal-copy-file (from to)
  (cal-call-process cal-cp from to))

(defun cal-diff (from to diff)
  (with-temp-buffer
    (call-process cal-diff nil t nil "-c" from to)
    (write-file diff)))


