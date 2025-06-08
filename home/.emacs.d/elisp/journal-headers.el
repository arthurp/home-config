(provide 'journal-headers)

(defun bar-timestamp (n c)
  (insert (concat (make-string n c) "\n" (current-time-string) "\n\n"))
  )
 
(defun bar-timestamp-dash ()
  (interactive)
  (bar-timestamp 10 ?-))
 
(defun bar-timestamp-eq ()
  (interactive)
  (bar-timestamp 40 ?=))
 
 
(define-key text-mode-map (kbd "C-c C--") 'bar-timestamp-dash)
(define-key text-mode-map (kbd "C-c C-=") 'bar-timestamp-eq)
 
