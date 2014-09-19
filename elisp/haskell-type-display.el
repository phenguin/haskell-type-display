(require 'ghc-mod)
(require 'shm)

(defun test-fn ()
  (message "First minor mode working.."))

(defun hdt-inc ()
  (interactive)
  (setq hdt-test (+ 1 (or hdt-test 5))))

(defun hdt-mode-setup ()
  (set (make-local-variable 'hdt-test) 0))

;;;###autoload
(define-minor-mode hdt-mode
  "Automatically display types of expressions surrounding point."
  :lighter "HDT"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-c c-q a") 'test-fn)
	    map)
  (hdt-mode-setup))

(provide 'haskell-type-display)
