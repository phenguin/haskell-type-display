(require 'ghc)
(require 'eieio)
(require 'shm)
(require 'dash)
(require 's)

(defvar hdt/default-run-delay
  0.5
  "How much idle time must pass before updating the types display.")

(defvar hdt/timer
  nil
  "The timer for the (maybe) currently running idle timer task.")

(defvar hdt/current-einfos
  nil
  "The current list of expressions around point")

(defun hdt/timer-enable (&optional secs)
  (interactive)
  (unless hdt/timer
    (setq
     hdt/timer
     (run-with-idle-timer
      (or secs hdt/default-run-delay)
      t
      'hdt/show-types-around-point))))

(defun hdt/timer-disable ()
  (interactive)
  (if hdt/timer
      (progn
	(cancel-timer hdt/timer)
	(setq hdt/timer nil))))

(defun hdt/timer-toggle ()
  (interactive)
  (if hdt/timer
      (hdt/timer-disable)
    (hdt/timer-enable)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Setup class that holds information on haskell expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass einfo ()
  ((expr :accessor einfo-expr
	 :initarg :expr)
   (type :accessor einfo-type
	 :initarg :type)
   (start :accessor einfo-start
	  :initarg :start
	  :initform (point-marker))
   (end :accessor einfo-end
	:initarg :end)))

(defun make-einfo (expr type &optional start end)
  (let ((sp (or start (point-marker))))
    (make-instance 'einfo :expr expr :type type
		   :start sp
		   :end (or end (copy-marker (+ sp (length expr)))))))

(defun make-einfo-from-lc (expr type startline startcol endline endcol &optional buffer)
  (let ((start nil))
    (with-current-buffer buffer
      (goto-line startline)
      (forward-char startcol)
      (setq start (point-marker))
      (goto-line endline)
      (forward-char endcol)
      (make-einfo expr type start (point-marker)))))

(defmethod einfo-buffer ((x einfo))
  (marker-buffer (einfo-start x)))

(defmethod einfo-start-lc ((x einfo))
  (letf (((point-marker) (einfo-start x)))
    (cons (line-number-at-pos) (current-column))))

(defmethod einfo-end-lc ((x einfo))
  (letf (((point-marker) (einfo-end x)))
    (cons (line-number-at-pos) (current-column))))

;; Code modified slightly from shm-case-split
(defun shm/case-split-parenthesized (name &optional expr-string)
  "Prompt for a type then do a case split based on it."
  (interactive (list (read-from-minibuffer "Type: ")))
  (save-excursion
    (let ((column (current-column))
	  (case-expr (if expr-string
			 expr-string
		       "undefined")))
      (insert "(")
      (insert (concat "case " case-expr " "))
      (if (not expr-string)
	  (shm-evaporate (- (point) (+ 1 (length "undefined")))
			 (- (point) 1)))
      (insert "of\n")
      (indent-to (+ column 2))
      (shm-case-split-insert-alts
       (shm-case-split-alts-from-data-decl
        (haskell-process-get-data-type name)))
      (insert ")"))))

(defmethod einfo-case-split ((x einfo))
  (letf (((point-marker) (einfo-start x)))
    (einfo-replace-string x "")
    (shm/case-split-parenthesized
     (shm-cleanup-type-string-for-case (einfo-type x))
     (einfo-expr x))))

(defmethod einfo-replace-string ((x einfo) val)
  (with-current-buffer (einfo-buffer x)
    (setf (buffer-substring (- (einfo-start x) 1) (einfo-end x)) val)))

(defun hdt/prompt-case-split (n)
  (interactive (list (- (read-number "Case split which number expression?: " 1) 1)))
  (if hdt/current-einfos
      (einfo-case-split (nth n hdt/current-einfos))
    (error "No expressions to choose from.  Call hdt/show-types-around-point or hdt/timer-enable first.")))

;;;###autoload
(define-minor-mode hdt-mode
  "Automatically display types of expressions surrounding point."
  :lighter "HDT"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c C-- t") 'hdt/show-types-around-point)
	    (define-key map (kbd "C-c C-- z") 'hdt/show-display-buffer)
	    (define-key map (kbd "C-c C-- T") 'hdt/timer-toggle)
	    (define-key map (kbd "C-c C-- s") 'hdt/prompt-case-split)
	    map)
  (setup-hdt-mode))

(defun setup-hdt-mode ()
  (set (make-local-variable 'hdt/timer-running) t)
  (set (make-local-variable 'hdt/timer) nil)
  (set (make-local-variable 'hdt/current-einfos) nil)
  (set (make-local-variable 'hdt/default-run-delay) hdt/default-run-delay))

(define-minor-mode hdt-display-buffer-mode
  "Mode for the buffer displaying the types."
  :lighter "HDTDisplay"
  :keymap (make-sparse-keymap)
  (setup-hdt-display-buffer-mode))

(defun setup-hdt-display-buffer-mode ()
  (turn-on-haskell-font-lock))


(defun hdt:buffer-substring-by-linecol (startline startcol endline endcol &optional buffer)
  (let* ((buf (or buffer (current-buffer)))
	 ;; TODO: handle case of startcol==0
	 (sl startline)
	 (sc (- startcol 1))
	 (el endline)
	 (ec (- endcol 1))
	 (spoint nil)
	 (epoint nil))
    (save-excursion
      (with-current-buffer buf
	(goto-line sl)
	(forward-char sc)
	(setq spoint (point))
	(goto-line el)
	(forward-char ec)
	(setq epoint (point))
	(buffer-substring-no-properties spoint epoint)))))


(defun hdt:tinfo->einfo (tinfo &optional buffer)
  (let* ((buf (or buffer (current-buffer)))
	 (posinfo (-slice tinfo 0 4))
	 (startpos (-slice posinfo 0 2))
	 (startcol (cadr startpos))
	 (endpos (-slice posinfo 2 4))
	 (type (nth 4 tinfo))
	 (typestring (apply 'hdt:buffer-substring-by-linecol (-concat posinfo (list buf))))
	 (splitts (s-lines typestring))
	 (cleaned (s-join "\n"
			  (cons (car splitts)
				(--map (substring it startcol) (cdr splitts))))))
    (make-einfo-from-lc cleaned type (car startpos) (cadr startpos) (car endpos) (cadr endpos) buf)))

;; Why didn't this already exist..?
(defun ghc-run-async-command (cmd callback &optional numresults hook)
  (setq ghc-process-rendezvous nil)
  (setq ghc-process-results nil)
  (setq ghc-process-num-of-results (or numresults 1))
  (ghc-with-process
   cmd
   (lexical-let ((cb callback)
		 (initialbuf (current-buffer))
		 (initialpoint (point)))
     (lambda (status)
       (ghc-process-callback status)
       (unwind-protect
	   (letf* (((current-buffer) initialbuf)
		   ((point) initialpoint))
	     (funcall cb ghc-process-results))
	 (setq ghc-process-running nil))))
   nil hook))

(defun ghc-type-obtain-tinfos-async (callback)
  (let* ((ln (int-to-string (line-number-at-pos)))
	(cn (int-to-string (1+ (current-column))))
	(file (buffer-file-name))
	(cmd (format "type %s %s %s\n" file ln cn)))
    (ghc-run-async-command cmd callback nil 'ghc-type-fix-string)))

(defun hdt:to-display-string (einfo &optional n buffer)
  (let* ((exp (einfo-expr einfo))
	 (type (einfo-type einfo))
	 (exp (if (string-match "\s+=\s+" exp)
		  (car (split-string exp)) exp))
	 (sepchar (if (> (+ (length exp) (length type)) 80) "\n" " "))
	 (s (concat exp " ::" sepchar type)))
    (if n (concat (number-to-string n) ": " s) s)))

(defun hdt:get-display-buffer (&optional buffer)
  (let* ((name (concat "*HDT:" (buffer-name (or buffer (current-buffer))) "*"))
	 (buf (get-buffer name)))
    (if buf
	buf
      (setq buf (get-buffer-create name))
      (with-current-buffer buf
	(hdt-display-buffer-mode)
	buf))))

(defun hdt/show-display-buffer ()
  (interactive)
  (display-buffer (hdt:get-display-buffer)))

(defun hdt/display-type-info (tinfos &optional outputbuffer)
  (interactive)
  (let ((einfos (-map 'hdt:tinfo->einfo tinfos)))
    (setq hdt/current-einfos einfos)
    (if tinfos
	(let* ((buf (or outputbuffer (hdt:get-display-buffer)))
	       (tstrings (-map-indexed (lambda (i x) (hdt:to-display-string x (+ i 1))) einfos)))
	  (with-current-buffer (get-buffer-create buf)
	    (setf (buffer-string)
		  (s-join "\n\n" tstrings)))))))

(defun hdt/show-types-around-point ()
  (interactive)
  (ghc-type-obtain-tinfos-async 'hdt/display-type-info))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test area ----------------------------
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(provide 'haskell-type-display)
