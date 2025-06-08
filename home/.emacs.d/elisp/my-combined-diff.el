(defconst combined-diff-hunk-header-re-unified
  "^@@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@@")

(defconst combined-diff-hunk-header-re
  (concat "^\\(?:" combined-diff-hunk-header-re-unified ".*\\|\\*\\{15\\}.*\n\\*\\*\\* .+ \\*\\*\\*\\*\\|[0-9]+\\(,[0-9]+\\)?[acd][0-9]+\\(,[0-9]+\\)?\\)"))

(defconst combined-diff-file-header-re (concat "^\\(--- .+\n\\+\\+\\+ \\|\\*\\*\\* .+\n--- \\|[^-+!<>0-9@* \n]\\)\\(.+\\)\n" (substring combined-diff-hunk-header-re 1)))

(defun combined-diff-kill-hunk ()
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((p (search-backward-regexp combined-diff-hunk-header-re nil nil 1))
          (end (min (save-excursion (if (search-forward-regexp combined-diff-file-header-re nil t 1)
                                        (match-beginning 0)
                                      (point-max)))
                    (save-excursion (if (search-forward-regexp combined-diff-hunk-header-re nil t 2)
                                        (match-beginning 0)
                                      (point-max))))))
      (kill-region p end)
      )
    ))

(defun combined-diff-kill-file ()
  (interactive)
  (save-excursion
    (forward-line 2)
    (end-of-line)
    (let ((p (search-backward-regexp combined-diff-file-header-re nil nil 1)))
      (search-forward-regexp combined-diff-file-header-re nil nil 2)
      (beginning-of-line)
      (kill-region p (match-beginning 0))
      )
    ))

(defun combined-diff-goto-hunk ()
  (interactive)
  (letrec ((file-name
         (save-excursion
           (forward-line 2)
           (end-of-line)
           (search-backward-regexp combined-diff-file-header-re nil nil 1)
           (match-string 2)
           ))
        (line
         (save-excursion
           (end-of-line)
           (search-backward-regexp combined-diff-hunk-header-re nil nil 1)
           (list (match-string 5) (match-string 6))
           ))
        (start (string-to-number (first line)))
        (end (+ start (string-to-number (second line)))))
    ;;(insert file-name (number-to-string start) (number-to-string end))
    (find-file-other-window (concat "/home/amp/shared/orc/" file-name))
    (goto-char (point-min))
    (forward-line (1- end))
    (push-mark nil nil t)
    (goto-char (point-min))
    (forward-line (1- start))
    ))

(defvar combined-diff-mode-hook nil)

(defvar combined-diff-mode-map
  (let ((map (make-keymap)))
    ;;(define-key map "\C-j" 'newline-and-indent)
    (define-key map "\C-c\C-c" 'combined-diff-goto-hunk)
    (define-key map "\C-c\C-d" 'combined-diff-kill-hunk)
    (define-key map "\C-c\C-f" 'combined-diff-kill-file)
    map)
  "Keymap for combined diff major mode")

;;;;###autoload
;(add-to-list 'auto-mode-alist '("\\.cdiff\\'" . combined-diff-mode))

;; (regexp-opt '("@@@" "@@" "--" "-+" "+-" "++") t)

(defconst combined-diff-font-lock-keywords
  (list
   '("\\(^[+-][+-]\\|@@@?\\)" . font-lock-keyword-face))
  "Minimal highlighting expressions for combined diff mode")

(define-derived-mode combined-diff-mode fundamental-mode "Combined Diff"
  "Major mode for browsing combined diff files."
  (set (make-local-variable 'font-lock-defaults) '(combined-diff-font-lock-keywords)))
