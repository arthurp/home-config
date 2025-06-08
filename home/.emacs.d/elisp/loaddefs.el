;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "curry-doc" "curry-doc.el" (19870 50332 0 0))
;;; Generated autoloads from curry-doc.el

(defvar curry-doc-mode nil "\
*If non-nil, show the type of the function near point or a related comment.

If the identifier near point is a Curry keyword and the variable
`curry-doc-show-reserved' is non-nil show a one line summary
of the syntax.

If the identifier near point is a Prelude or one of the standard library 
functions and `curry-doc-show-prelude' is non-nil show its type. Currently 
only Curry 1.4 functions are supported. In future versions the 
`curry-doc-show-prelude' variable should determine which prelude/library
to use for type lookup.

If the identifier near point is local (i.e. defined in this module) check
the `imenu' list of functions for the type. This obviously requires that
your language mode uses `imenu' (`pakcs-mode' 0.6 for example).

If the identifier near point is global (i.e. defined in an imported module) 
and the variable `curry-doc-show-global-types' is non-nil show the type of its 
function.

If the identifier near point is a standard strategy or a function, type related
related to strategies and `curry-doc-show-strategy' is non-nil show the type
of the function. Strategies are special to the parallel execution of Curry.
If you're not interested in that just turn it off.

If the identifier near point is a user defined function that occurs as key
in the alist `curry-doc-user-defined-ids' and the variable 
`curry-doc-show-user-defined' is non-nil show the type of the function.

This variable is buffer-local.")

(autoload 'curry-doc-mode "curry-doc" "\
Enter curry-doc-mode for showing fct types in the echo area (see variable docstring).

\(fn &optional PREFIX)" t nil)

(autoload 'turn-on-curry-doc-mode "curry-doc" "\
Unequivocally turn on curry-doc-mode (see variable documentation).

\(fn)" t nil)

(autoload 'turn-off-curry-doc-mode "curry-doc" "\
Unequivocally turn off curry-doc-mode (see variable documentation).

\(fn)" t nil)

(autoload 'curry-doc-show-type "curry-doc" "\
Show the type of the function near point.
For the function under point, show the type in the echo area.
This information is extracted from the `curry-doc-prelude-types' alist of prelude functions and their types, or from the local functions in the current buffer.

\(fn &optional SYMBOL)" t nil)

;;;***

;;;### (autoloads nil "orc-mode" "orc-mode.el" (22725 27651 659142
;;;;;;  684000))
;;; Generated autoloads from orc-mode.el

(add-to-list 'auto-mode-alist '("\\.orc\\'" . orc-mode))

;;;***

;;;### (autoloads nil "rainbow-delimiters" "rainbow-delimiters.el"
;;;;;;  (22326 35339 0 0))
;;; Generated autoloads from rainbow-delimiters.el

(autoload 'rainbow-delimiters-mode "rainbow-delimiters" "\
Highlight nested parentheses, brackets, and braces according to their depth.

\(fn &optional ARG)" t nil)

(autoload 'rainbow-delimiters-mode-enable "rainbow-delimiters" "\
Enable `rainbow-delimiters-mode'.

\(fn)" nil nil)

(autoload 'rainbow-delimiters-mode-disable "rainbow-delimiters" "\
Disable `rainbow-delimiters-mode'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil "wikidot-mode" "wikidot-mode.el" (19829 29914
;;;;;;  0 0))
;;; Generated autoloads from wikidot-mode.el

(autoload 'wikidot-mode "wikidot-mode" "\
Major mode to edit Wikidot markup.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("acl2.el" "auctex-extensions.el" "bison-mode.el"
;;;;;;  "curry-decl-scan.el" "curry-font-lock.el" "curry-indent.el"
;;;;;;  "curry-mode.el" "curry-pakcs.el" "curry-simple-indent.el"
;;;;;;  "flex-mode.el" "journal-headers.el" "kerboscript-mode.el"
;;;;;;  "make-regexp.el" "my-combined-diff.el" "my-synctex.el" "ottmode.el"
;;;;;;  "sentence-per-line.el" "word-count.el") (22725 28081 810407
;;;;;;  523000))

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here
