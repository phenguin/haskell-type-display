(require 'ghc)
(require 'shm)
(require 'dash)
(require 's)

(defvar hdt/run-delay
  0.5
  "How much idle time must pass before updating the types display.")

(defun hdt-mode-setup ()
  (set (make-local-variable 'hdt-test) 0))

;;;###autoload
(define-minor-mode hdt-mode
  "Automatically display types of expressions surrounding point."
  :lighter "HDT"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c k") 'hdt-show-types-around-point)
	    map)
  (hdt-mode-setup))

(define-key hdt-mode-map (kbd "C-c k") 'hdt-show-types-around-point)

(defun hdt-buffer-substring-by-linecol (startline startcol endline endcol &optional buffer)
  (let* ((buf (or buffer (current-buffer)))
	 ;; TODO: handle case of startcol==0
	 (sl startline)
	 (sc (- startcol 1))
	 (el endline)
	 (ec (- endcol 1))
	 (spoint nil)
	 (epoint nil))
    (with-current-buffer buf
      (goto-line sl)
      (forward-char sc)
      (setq spoint (point))
      (goto-line el)
      (forward-char ec)
      (setq epoint (point))
      (buffer-substring-no-properties spoint epoint))))

(defun hdt-to-type-annotation (tinfo &optional buffer)
  (let* ((posinfo (-slice tinfo 0 4))
	 (type (nth 4 tinfo)))
    `(,(apply 'hdt-buffer-substring-by-linecol (-concat posinfo (list buffer))) . ,type)))

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
       (letf* (((current-buffer) initialbuf)
	       ((point) initialpoint))
	 (funcall cb ghc-process-results))))
   nil hook))

(defun ghc-type-obtain-tinfos-async (callback)
  (let* ((ln (int-to-string (line-number-at-pos)))
	(cn (int-to-string (1+ (current-column))))
	(file (buffer-file-name))
	(cmd (format "type %s %s %s\n" file ln cn)))
    (ghc-run-async-command cmd callback nil 'ghc-type-fix-string)))

(defun hdt-to-display-string (tinfo &optional buffer)
  (let* ((type-pair (hdt-to-type-annotation tinfo buffer))
	 (exp (car type-pair))
	 (type (cdr type-pair))
	 (sepchar (if (> (+ (length exp) (length type)) 80)
		      "\n" " ")))
    (concat (car type-pair) " ::" sepchar (cdr type-pair))))

(defun hdt-display-type-info (tinfos &optional outputbuffer)
  (interactive)
  (let* ((buf (or outputbuffer "testbuf"))
	 (tstrings (-map 'hdt-to-display-string tinfos)))
    (with-current-buffer (get-buffer-create buf)
      (setf (buffer-string)
	    (s-join "\n\n" tstrings)))))

(provide 'haskell-type-display)
