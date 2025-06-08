(provide 'auctex-extensions)

(defun TeX-dwim-master-buffer-search ()
  (let ((dir default-directory))
    (dolist (buf (buffer-list))
      (when (with-current-buffer buf
	      (and (equal dir default-directory)
		   (stringp TeX-master)))
	(return (with-current-buffer buf TeX-master))))))

(defun TeX-dwim-master ()
  "Find a likely `TeX-master'."
  (let ((buffer-based (TeX-dwim-master-buffer-search)))
    (if buffer-based
        buffer-based
      (save-excursion
        (beginning-of-buffer)
        (re-search-forward "^[[:space:]]*%[[:space:]]*!TEX[[:space:]]*root[[:space:]]*=[[:space:]]*\\(.*\\)[[:space:]]*$")
        (match-string-no-properties 1))
      )))
