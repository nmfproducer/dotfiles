;; cal interface to w3m

(defun cal-fur ()
  "Apply Form-Url-Encode on the current buffer."
  (goto-char (point-min))
  (while (not (eobp))
    (let ((char (following-char)))
      (if (or (< char 32) (> char 127)) 
	  (forward-char)
	;; delete and replace
	(cond ((= char 32) (delete-char 1) (insert "+"))
	      ((= char 33) (delete-char 1) (insert "%21"))
	      ((= char 34) (delete-char 1) (insert "%22"))
	      ((= char 35) (delete-char 1) (insert "%23"))
	      ((= char 36) (delete-char 1) (insert "%24"))
	      ((= char 37) (delete-char 1) (insert "%25"))
	      ((= char 38) (delete-char 1) (insert "%26"))
	      ((= char 39) (delete-char 1) (insert "%27"))
	      ((= char 40) (delete-char 1) (insert "%28"))
	      ((= char 41) (delete-char 1) (insert "%29"))
	      ((= char 43) (delete-char 1) (insert "%2B"))
	      ((= char 44) (delete-char 1) (insert "%2C"))
	      ((= char 47) (delete-char 1) (insert "%2F"))
	      ((= char 58) (delete-char 1) (insert "%3A"))
	      ((= char 59) (delete-char 1) (insert "%3B"))
	      ((= char 60) (delete-char 1) (insert "%3C"))
	      ((= char 61) (delete-char 1) (insert "%3D"))
	      ((= char 62) (delete-char 1) (insert "%3E"))
	      ((= char 63) (delete-char 1) (insert "%3F"))
	      ((= char 64) (delete-char 1) (insert "%40"))
	      ((= char 91) (delete-char 1) (insert "%5B"))
	      ((= char 92) (delete-char 1) (insert "%5C"))
	      ((= char 93) (delete-char 1) (insert "%5D"))
	      ((= char 94) (delete-char 1) (insert "%5D"))
	      ((= char 96) (delete-char 1) (insert "%60"))
	      ((= char 123) (delete-char 1) (insert "%7B"))
	      ((= char 124) (delete-char 1) (insert "%7C"))
	      ((= char 125) (delete-char 1) (insert "%7D"))
	      ((= char 126) (delete-char 1) (insert "%7E"))
	      ((= char 127) (delete-char 1) (insert "%7F"))
	      (t (forward-char))) ))))

(defun cal-download  (url key &optional code)
  "Download URL.  Return T if successful, NIL otherwise.  Downloading
will be successful if the content of the donwloaded file contains KEY.
CODE determines the coding system.  The default coding system is
euc-japan."
  (save-excursion
    (let (
	  ;;(coding-system-for-read (or code 'iso-2022-jp-unix))
	  ;;(coding-system-for-write (or code 'iso-2022-jp-unix))
	  (coding-system-for-read (or code 'euc-japan))
	  (coding-system-for-write (or code 'euc-japan))
	  )
    (message "ファイルをダウンロードしています．．．")
    (erase-buffer)
    (apply 'call-process cal-w3m-command nil (current-buffer) nil
	   (append cal-w3m-command-arguments
		   (list "-header" "Cache-Control: no-cache" 
			 "-header" "Pragma: no-cache" 
			 "-dump_source" url)) ) 
    (goto-char (point-min)) 
    (message "")
    (if (search-forward key nil t) t nil))))

;;SUBMIT=hoge&fname=<ファイル名>&content=....
;;
;;とポストすると(認証済であれば) 
;;
;;io:/home/lab8/igarashi/callog/logfiles/<ユーザ名>/<ファイル名>
;;
;;としてファイルができる

(defun cal-upload  (file url &optional name) t)
;; comment-out
;; (defun cal-upload  (file url &optional name)
;;   "Upload FILE to URL.  Return T if KEY is found in the buffer, NIL otherwise.
;; If NAME is given, it is used as the name of FILE."
;;   (let (result)
;;     (message "ファイル %sをアップロードしています．．．" (or name file))
;;     (with-temp-file
;; 	cal-temp-file
;;       (erase-buffer)
;;       (insert-file-contents file)
;;       ;; we do not upload cal-thm-list.  we should control the
;;       ;; execution of this code by optional argument.
;;       (when (string= "cal-record" (file-name-nondirectory file))
;; 	(goto-char (point-min))
;; 	(when (search-forward "(setq cal-thm-list")
;; 	  (let ((p (match-beginning 0)))
;; 	    (goto-char p)
;; 	    (forward-sexp 1)
;; 	    (delete-region p (point)))))
;;       (cal-fur)
;;       (goto-char (point-min))
;;       (insert "SUBMIT=hoge&fname=" 
;; 	      (or name (file-name-nondirectory (expand-file-name file)))
;; 	      "&content="))
;;     (with-temp-buffer
;;       (apply 'call-process cal-w3m-command nil (current-buffer) nil
;; 	     (append cal-w3m-command-arguments
;; 		     (list "-cookie" "-post" cal-temp-file url)))
;;       (goto-char (point-min))
;;       ;; KEY = "uploaded"
;;       (setq result (if (search-forward "uploaded" nil t) t nil)))
;;     (cal-call-process cal-rm cal-temp-file)
;;     (message "")
;;     result))

(defun cal-download-test  (url key &optional code)
  "Download URL.  Return T if successful, NIL otherwise.  Downloading
will be successful if the content of the donwloaded file contains KEY.
CODE determines the coding system.  The default coding system is
euc-japan."
  (save-excursion
    (let (
	  ;;(coding-system-for-read (or code 'iso-2022-jp-unix))
	  ;;(coding-system-for-write (or code 'iso-2022-jp-unix))
	  (coding-system-for-read (or code 'euc-japan))
	  (coding-system-for-write (or code 'euc-japan))
	  )
    (erase-buffer)
    (apply 'call-process cal-w3m-command nil (current-buffer) nil
	   (append cal-w3m-command-arguments
		   (list "-header" "Cache-Control: no-cache" 
			 "-header" "Pragma: no-cache" 
			 "-dump_extra" url)) ) 
    (goto-char (point-min)) 
    (if (search-forward key nil t) t nil))))

(defun cal-check-cookie ()
  "Check cookie file, and return non-nil if it contains cookie information."
  (let ((file (expand-file-name "~/.w3m/cookie")))
    (when
	(file-exists-p file)
      (with-temp-buffer
       (insert-file-contents file)
       (goto-char (point-min))
       (search-forward cal-welcome-url nil t)))))
