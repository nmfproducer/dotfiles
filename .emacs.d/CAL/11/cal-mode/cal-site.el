;; site-specific data
;; on media, this file must be updated each year
;; media center and sato lab (venus or moon).
;; emacs on media: /usr/bin/emacs
;; emacs on moon:  /usr/bin/emacs21

;; site map
;; user's directory:
;;
;;    my-cal-dir = "~/cal/"
;;
;; CAL files
;;
;;    cal-dir = "~a00xxxxx(teacher's id)/CAL/11/" on media
;;            = "~masahiko/system/cal/07/" on moon
;;
;; web site: www.sato.kuis.kyoto-u.ac.jp = io = 130.54.22.201
;;
;;    cal-remote-dir = "~masahiko/cal"
;;        news, record, problems, help are stored here
;;
;;    cal-welcome-url = "~igarashi/cal/03/welcome.php"
;;        for initial setup
;;
;;    cal-upload-url = "~igarashi/cal/03/upload.php"
;;        each user's cal-record and CALLOG files are stored here
;;
;; CAL record
;;
;;    io:~masahiko/system/cal/11/dotcal/cal-record.el

;; local installation or not

(defvar cal-use-w3m t
  "T means that CAL is installed using w3m.")

;; w3m

(defvar cal-use-w3m)
(defvar cal-hostname)
(defvar cal-w3m-command)
(defvar cal-w3m-command-arguments)

(when cal-use-w3m
  (setq cal-hostname
	(with-temp-buffer
	  (call-process "hostname" nil (current-buffer) nil)
	  (buffer-substring (point-min) (1- (point-max)))))

  (setq cal-w3m-command
	(cond ((string-match "unix[1-7]" cal-hostname)
               (concat cal-dir "../bin/w3m"))
	      (t "/usr/bin/w3m")))

  (setq cal-w3m-command-arguments
	(cond ((member cal-hostname 
		       '("venus" "moon" "vega" "libra" "leo"))
	       '("-o" 
		 "http_proxy=http://wwwproxy.sato.kuis.kyoto-u.ac.jp:3128/"))
	      ((string-match "unix[1-7]" cal-hostname)
	       nil
	       ;;'("-o" 
		 ;;"http_proxy=http://squid.ecs.kyoto-u.ac.jp:3128/"))
	       )
	      (t nil)))
  )

;; we use "whoami" command result instead of (getenv "USER"),
;; since environment variables are not reliable.
;; (setq cal-user-name (getenv "USER"))
(setq cal-user-name
      (with-temp-buffer
        (call-process "whoami" nil (current-buffer) nil)
        (buffer-substring (point-min) (1- (point-max)))))

(defvar cal-debugger-list)
(setq cal-debugger-list
      '(
	;;"masahiko"
	;;"a0017249"
	))

;; urls
(defvar cal-welcome-url)
(defvar cal-upload-url)
(setq cal-welcome-url 
      "http://www.sato.kuis.kyoto-u.ac.jp/~igarashi/cal/03/welcome.php"
      cal-upload-url 
      "http://www.sato.kuis.kyoto-u.ac.jp/~igarashi/cal/03/upload.php")

;; cal-remote-dir is the directory where CAL system files reside.
(defvar cal-remote-dir)
(setq cal-remote-dir "http://www.sato.kuis.kyoto-u.ac.jp/~masahiko/cal/11/")

(defvar cal-N-file)
(defvar cal-R-file)
(defvar cal-P-file)
(defvar cal-A-file)
(defvar cal-H-file)
(defvar cal-message-file)
(setq cal-N-file (concat cal-remote-dir "cal-mode/news"))
(setq cal-R-file (concat cal-remote-dir "cal-mode/record"))
(setq cal-P-file (concat cal-remote-dir "cal-mode/problems"))
(setq cal-A-file (concat cal-remote-dir "cal-mode/answers"))
(setq cal-H-file (concat cal-remote-dir "cal-mode/help"))
(setq cal-message-file (concat cal-remote-dir "message"))

;; we use different files for cal-debuggers
(when (member (getenv "USER") cal-debugger-list)
  (setq cal-P-file (concat cal-remote-dir "debug/problems"))
  (setq cal-A-file (concat cal-remote-dir "debug/answers")))

;; moved to cal-record.el
;; cal-system-log-dir: この下にすべてのユーザの記録が保存される.
;; この directory は既に存在する directory でないといけない.
;; moreover, we need following subdirectories:
;;  cal-system-log-dir logfiles/
;;  cal-system-log-dir dotcal/record/
;;  cal-system-log-dir dotcal/topten/
;;(setq cal-system-log-dir ...)

(defvar my-cal-dir)
(setq my-cal-dir (expand-file-name "~/cal/"))
;; each user should create the following dir if he/she wishes to
;; have a private games directory
(defvar cal-private-game-dir)
(setq cal-private-game-dir (concat my-cal-dir "games/"))

(defvar cal-temp-file)
(setq cal-temp-file (concat my-cal-dir "temp"))
(defvar cal-record-file)
(setq cal-record-file (concat my-cal-dir "cal-record"))

(defvar my-cal-session-log-file)
(setq my-cal-session-log-file (expand-file-name (concat my-cal-dir "CALLOG")))
;; set this, if you wish to keep CALLOG system wide
(defvar cal-session-log-file)
(setq cal-session-log-file my-cal-session-log-file)

;; game directory

(defvar cal-game-dir)
(setq cal-game-dir (concat cal-dir "games/"))
(defvar my-cal-game-dir)
(setq my-cal-game-dir (concat my-cal-dir "games/"))

(defconst cal-my-touch  "touch")
(defconst cal-my-mkdir  "mkdir")
(defconst cal-my-chmod  "chmod")
(defconst cal-my-rm  "rm")
(defconst cal-ls  "ls")

(defconst cal-touch  "touch")
(defconst cal-mkdir  "mkdir")
(defconst cal-chmod  "chmod")
(defconst cal-chown  "chown")
(defconst cal-cp  "cp")
(defconst cal-rm "rm")

(or (file-exists-p my-cal-dir)
    (progn
      (cal-call-process cal-my-mkdir my-cal-dir)
      (cal-call-process cal-my-mkdir my-cal-game-dir)
      (cal-call-process cal-my-chmod "700" my-cal-dir)))

(or (file-exists-p cal-record-file)
    (progn
      (cal-call-process cal-my-touch cal-record-file)
      (cal-call-process cal-my-chmod "600" cal-record-file)))

;; moved from cal-record.el

(defvar cal-avoid-list
      '(
	"masahiko"
	"kam"
	"msatou"
	"itakeuti"
	;;"tmiki"
	;;"aokamoto"
	"root"
	"cal"
	"i30a0281"
	"i60y0033"
	"a0018186" ;; nakazawa
	)
      )

;; directory for archiving theorems

(defvar cal-thm-dir)
(setq cal-thm-dir (concat my-cal-dir "theorems/"))
(or (file-exists-p cal-thm-dir)
    (progn
      (cal-call-process cal-my-mkdir cal-thm-dir)
      (cal-call-process cal-my-chmod "700" cal-thm-dir)))
