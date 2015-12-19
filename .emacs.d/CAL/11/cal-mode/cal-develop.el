;; utilities for developping the CAL system

(defun cal-kill-problem-buffers ()
  (interactive)
  (if (get-buffer " *PROBLEMS*") (kill-buffer " *PROBLEMS*"))
  (if (get-buffer " *ANSWERS*") (kill-buffer " *ANSWERS*"))
  )

;; utility function for renumbering problems.

(defun cal-renumber (num)
  (interactive "nStart Number: ")
  (let (p q)
  (while (search-forward "---\n[" nil t)
    (setq p (point))
    (search-forward "]")
    (setq q (1- (point)))
    (goto-char p)
    (delete-region p q)
    (insert (format "%s" num))
    (setq num (1+ num)))))

(defun cal-quote-string ()
  "Quote double quotes symbols in the current buffer."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\"" nil t)
    (backward-char 1)
    (insert "\\")
    (forward-sexp 1)
    (backward-char 1)
    (insert "\\")
    (forward-char 1)))

;; version up

(defun cal-version-up (&optional inc-v-no)
  "version up .el file, save, compile and make diff file.
version no is kept when this function is called on the same day as the
current version.  This feature can be overridden by giving non-nil
INC-V-NO."
  (interactive)
  (goto-char (point-min))
  (when
      (re-search-forward
       "^;; \\(.* version [0-9]+\\.\\)\\([0-9][0-9]\\) of \\(.*\\)$" nil t)
    (let* ((str (buffer-substring-no-properties 
		 (match-beginning 0) (match-end 0)))
	   (init-str (buffer-substring-no-properties 
		      (match-beginning 1) (match-end 1)))
	   (v-str (buffer-substring-no-properties 
		   (match-beginning 2) (match-end 2)))
	   (v-no (string-to-number v-str))
	   (new-v-str (format (if (< v-no 9) "0%d" "%d") (1+ v-no)))
	   (old-v-str 
	    (if (= v-no 0) nil (format (if (< v-no 10) "0%d" "%d") (1- v-no))))
	   (cts (current-time-string))
	   (date (substring cts 4 10))
	   (year (substring cts -4))
	   (time-str2 (buffer-substring-no-properties
		       (match-beginning 3) (match-end 3)))
	   (date2 (substring time-str2 4 10))
	   (year2 (substring time-str2 -4))
	   (file (buffer-file-name))
	   (dir (file-name-directory file))
	   ;; FILE-NAME without .el extension
	   (file-name (file-name-sans-extension
		       (file-name-nondirectory file)))
	   ;; files in old directory
	   (new-file (concat dir "old/" file-name "-" new-v-str ".el"))
	   (current-file (concat dir "old/" file-name "-" v-str ".el"))
	   (old-file (if old-v-str
			 (concat dir "old/" file-name "-" old-v-str ".el")
		       nil))
	   (new-version nil)
	   )
      (if (and (string= date date2) (string= year year2) (not inc-v-no))
	  (progn
	    ;; rewrite only the time-string
	    (delete-region (match-beginning 3) (match-end 3))
	    (goto-char (match-beginning 3))
	    (insert cts))
	;; version up!
	(setq new-version t)
	(goto-char (match-beginning 0))
	(insert ";; " init-str new-v-str " of " cts "\n") )
      ;; rewrite version-string
      (goto-char (point-min))
      (when (re-search-forward "(setq[ \t\n]+.+version-string[ \t\n]+" nil t)
	(goto-char (match-beginning 0))
	(forward-sexp 1)
	(delete-region (match-end 0) (1- (point)))
	(goto-char (1- (point)))
	(insert "\"" init-str new-v-str " of " cts "\""))
      ;; now save and compile, make diffs
      (write-file file)
      (byte-compile-file file)
      (if new-version
	  (progn
	    (cal-copy-file file new-file)
	    (cal-diff current-file new-file 
		      (concat (file-name-sans-extension current-file) "-diff"))
	    (cal-remove-file old-file))
	(cal-copy-file file current-file)
	(cal-diff old-file current-file
		  (concat (file-name-sans-extension old-file) "-diff"))))))

