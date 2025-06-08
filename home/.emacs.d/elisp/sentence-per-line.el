;;; The original value is "\f\\|[      ]*$", so we add the bullets (-), (+), and (*).
;;; There is no need for "^" as the regexp is matched at the beginning of line.
;; (setq paragraph-start "\f\\|[ \t]*$\\|[ \t]*\\\\[a-zA-Z]")
;; (setq paragraph-start "\f\\|[ \t]*$")

(setq sentence-end-double-space nil)

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph    
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

;; perform-replace from-string replacements query-flag regexp-flag delimited-flag &optional repeat-count map start end

(defun unfill-paragraph-mark-nl (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (save-excursion
    (mark-paragraph)
    (perform-replace "\r?\n" " \\\\snl " nil t nil nil nil (+ 1 (region-beginning)) (- (region-end) 1))
    ))

(defun fill-paragraph-sentence-per-line-mark-nl (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (unfill-paragraph-mark-nl region)
  (save-excursion 
    (let ((end (save-excursion
                 (forward-paragraph 1)
                 (backward-char)
                 (point-marker))))  ;; remember where to stop
      (beginning-of-line)
      (while (progn (forward-sentence)
                    (re-search-forward "^\\|[^[:space:]]")
                    (backward-char)
                    (if (string= (buffer-substring-no-properties (point) (+ 4 (point))) "\\snl")
                        (forward-word))
                    (re-search-forward "^\\|[^[:space:]]")
                    (backward-char)
                    (<= (point) (marker-position end)))
        (newline)       
        )))
  )

(defun revert-paragraph-nl (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (save-excursion
    (mark-paragraph)
    (perform-replace "\r?\n" "" nil t nil nil nil (+ 1 (region-beginning)) (- (region-end) 1))
    (mark-paragraph)
    (perform-replace " ?\\\\snl ?" "\n" nil t nil nil nil (+ 1 (region-beginning)) (- (region-end) 1))
    ))

;;; Aleš Bizjak <abizjak@cs.au.dk> (http://www.cs.au.dk/~abizjak/emacs/2016/03/06/latex-fill-paragraph.html)
(defun fill-paragraph-sentence-per-line (&optional P)
  "When called with prefix argument call `fill-paragraph'.
Otherwise split the current paragraph into one sentence per line."
  (interactive "P")
  (if (not P)
      (save-excursion 
        (unfill-paragraph)
        (let ((end (save-excursion
                     (forward-paragraph 1)
                     (backward-sentence)
                     (point-marker))))  ;; remember where to stop
          (beginning-of-line)
          (while (progn (forward-sentence)
                        (<= (point) (marker-position end)))
            (just-one-space) ;; leaves only one space, point is after it
            (delete-char -1) ;; delete the space
            (newline)        ;; and insert a newline
            )))
    ;; otherwise do ordinary fill paragraph
    (fill-paragraph P)))

(provide 'sentence-per-line)
