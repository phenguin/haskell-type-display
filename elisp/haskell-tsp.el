;;; haskell-tsp.el --- Haskell Types Surrounding Point Mode

;; Copyright (c) 2014 Justin Cullen. All rights reserved.

;; Author:    Justin Cullen <zenguine@gmail.com>
;; Created:   23-Sep-2014
;; Version:   0.1.0
;; Keywords:  haskell, development
;; Stability: unstable

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'ghc)
(require 'eieio)
(require 'dash)
(require 's)

(defvar tsp/default-run-delay
  0.5
  "How much idle time must pass before updating the types display.")

(defvar tsp/timer
  nil
  "The timer for the (maybe) currently running idle timer task.")

(defvar tsp/current-einfos
  nil
  "The current list of expressions around point")

(defun tsp/timer-enable (&optional secs)
  (interactive)
  (unless tsp/timer
    (setq
     tsp/timer
     (run-with-idle-timer
      (or secs tsp/default-run-delay)
      t
      'tsp/show-types-around-point))))

(defun tsp/timer-disable ()
  (interactive)
  (if tsp/timer
      (progn
	(cancel-timer tsp/timer)
	(setq tsp/timer nil))))

(defun tsp/timer-toggle ()
  (interactive)
  (if tsp/timer
      (tsp/timer-disable)
    (tsp/timer-enable)))

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
(eval-after-load 'shm
  '(defun tsp:shm-case-split-parenthesized (name &optional expr-string)
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
	 (insert ")")))))

(defmethod einfo-case-split ((x einfo))
  (if (require 'shm nil 'noerror)
    (letf (((point-marker) (einfo-start x)))
      (einfo-replace-string x "")
      (tsp:shm-case-split-parenthesized
       (shm-cleanup-type-string-for-case (einfo-type x))
       (einfo-expr x)))
    (error "Structured haskell mode is required to use case splitting.")))

(defmethod einfo-replace-string ((x einfo) val)
  (with-current-buffer (einfo-buffer x)
    (setf (buffer-substring (- (einfo-start x) 1) (einfo-end x)) val)))

(defun tsp/prompt-case-split (n)
  (interactive (list (- (read-number "Case split which number expression?: " 1) 1)))
  (if tsp/current-einfos
      (einfo-case-split (nth n tsp/current-einfos))
    (error "No expressions to choose from.  Call tsp/show-types-around-point or tsp/timer-enable first.")))

;;;###autoload
(define-minor-mode tsp-mode
  "Automatically display types of expressions surrounding point."
  :lighter "TSP"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c C-- t") 'tsp/show-types-around-point)
	    (define-key map (kbd "C-c C-- z") 'tsp/show-display-buffer)
	    (define-key map (kbd "C-c C-- T") 'tsp/timer-toggle)
	    (define-key map (kbd "C-c C-- s") 'tsp/prompt-case-split)
	    map)
  (setup-tsp-mode)
  (hdt/timer-enable))

(defun setup-tsp-mode ()
  (set (make-local-variable 'tsp/timer-running) t)
  (set (make-local-variable 'tsp/timer) nil)
  (set (make-local-variable 'tsp/current-einfos) nil)
  (set (make-local-variable 'tsp/default-run-delay) tsp/default-run-delay))

(define-minor-mode tsp-display-buffer-mode
  "Mode for the buffer displaying the types."
  :lighter "TSP-Display"
  :keymap (make-sparse-keymap)
  (setup-tsp-display-buffer-mode))

(defun setup-tsp-display-buffer-mode ()
  (turn-on-haskell-font-lock))

(defun tsp:buffer-substring-by-linecol (startline startcol endline endcol &optional buffer)
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


(defun tsp:tinfo->einfo (tinfo &optional buffer)
  (let* ((buf (or buffer (current-buffer)))
	 (posinfo (-slice tinfo 0 4))
	 (startpos (-slice posinfo 0 2))
	 (startcol (cadr startpos))
	 (endpos (-slice posinfo 2 4))
	 (type (nth 4 tinfo))
	 (typestring (apply 'tsp:buffer-substring-by-linecol (-concat posinfo (list buf))))
	 (splitts (s-lines typestring))
	 (cleaned (s-join "\n"
			  (cons (car splitts)
				(--map (substring it startcol) (cdr splitts))))))
    (make-einfo-from-lc cleaned type (car startpos) (cadr startpos) (car endpos) (cadr endpos) buf)))

;; Why didn't this already exist..?
(defun tsp:ghc-run-async-command (cmd callback &optional numresults hook)
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

(defun tsp:ghc-type-obtain-tinfos-async (callback)
  (let* ((ln (int-to-string (line-number-at-pos)))
	(cn (int-to-string (1+ (current-column))))
	(file (buffer-file-name))
	(cmd (format "type %s %s %s\n" file ln cn)))
    (tsp:ghc-run-async-command cmd callback nil 'ghc-type-fix-string)))

(defun tsp:to-display-string (einfo &optional n buffer)
  (let* ((exp (einfo-expr einfo))
	 (type (einfo-type einfo))
	 (exp (if (string-match "\s+=\s+" exp)
		  (car (split-string exp)) exp))
	 (sepchar (if (> (+ (length exp) (length type)) 80) "\n" " "))
	 (s (concat exp " ::" sepchar type)))
    (if n (concat (number-to-string n) ": " s) s)))

(defun tsp:get-display-buffer (&optional buffer)
  (let* ((name (concat "*TSP:" (buffer-name (or buffer (current-buffer))) "*"))
	 (buf (get-buffer name)))
    (if buf
	buf
      (setq buf (get-buffer-create name))
      (with-current-buffer buf
	(tsp-display-buffer-mode)
	buf))))

(defun tsp/show-display-buffer ()
  (interactive)
  (display-buffer (tsp:get-display-buffer)))

(defun tsp/display-type-info (tinfos &optional outputbuffer)
  (interactive)
  (let ((einfos (-map 'tsp:tinfo->einfo tinfos)))
    (setq tsp/current-einfos einfos)
    (if tinfos
	(let* ((buf (or outputbuffer (tsp:get-display-buffer)))
	       (tstrings (-map-indexed (lambda (i x) (tsp:to-display-string x (+ i 1))) einfos)))
	  (with-current-buffer (get-buffer-create buf)
	    (setf (buffer-string)
		  (s-join "\n\n" tstrings)))))))

(defun tsp/show-types-around-point ()
  (interactive)
  (tsp:ghc-type-obtain-tinfos-async 'tsp/display-type-info))

(provide 'haskell-tsp)
