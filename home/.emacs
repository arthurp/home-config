;; -*- lisp -*-

(savehist-mode 1)

(global-unset-key (kbd "C-z"))

(setq load-path (cons (expand-file-name "~/.emacs.d/elisp") load-path))

(autoload 'word-count-mode "word-count"
           "Minor mode to count words." t nil)
(global-set-key "\M-+" 'word-count-mode)

(autoload 'wikidot-mode "wikidot-mode"
  "Major mode for editing WikiDot markup." t nil)

;(proofgeneral)

(server-start)

(column-number-mode)
(display-time-mode)

(require 'epa-file)
(epa-file-enable)

(add-hook 'text-mode-hook (lambda ()
                            (flyspell-mode) (flyspell-buffer) (visual-line-mode)))
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

;; Curry mode

(if (file-exists-p "~/LocalInstalls/kics2/emacs/")
    (progn
      (setq load-path (cons (expand-file-name "~/LocalInstalls/kics2/emacs/") load-path))

      (setq auto-mode-alist
            (append auto-mode-alist
                    '(("\\.curry$"  . curry-mode)
                      ("\\.lcurry$"  . literate-curry-mode))))
      (autoload 'curry-mode "curry-mode"
        "Major mode for editing Curry programs." t)
      (autoload 'literate-curry-mode "curry-mode"
        "Major mode for editing literate Curry scripts." t)

      (add-hook 'curry-mode-hook 'turn-on-curry-font-lock)
      (add-hook 'curry-mode-hook 'turn-on-curry-decl-scan)
      (add-hook 'curry-mode-hook 'turn-on-curry-pakcs)
      ))

;; Scala Mode

;; (add-to-list 'load-path "/home/amp/LocalInstalls/scala2_9/misc/scala-tool-support/emacs/")
;; (require 'scala-mode-auto)
;; ;; scala mode hooks
;; (defun scala-turnoff-indent-tabs-mode ()
;;   (setq indent-tabs-mode nil))
;; (add-hook 'scala-mode-hook 'scala-turnoff-indent-tabs-mode)

;; OpenCL mode
(setq auto-mode-alist (cons '("\.cl$" . c-mode) auto-mode-alist))

;; OCaml config

;; (require 'flymake)

;; (defun flymake-ocaml-init ()
;;   (flymake-simple-make-init-impl
;;    'flymake-create-temp-with-folder-structure nil nil
;;    (file-name-nondirectory buffer-file-name)
;;    'flymake-get-ocaml-cmdline))
;; (defun flymake-get-ocaml-cmdline (source base-dir)
;;   (list "ocaml_flycheck.pl"
;;         (list source base-dir)))

;; (push '(".+\\.ml[yilp]?$" flymake-ocaml-init flymake-simple-java-cleanup)
;;       flymake-allowed-file-name-masks)
;; (push
;;  '("^\\(\.+\.ml[yilp]?\\|\.lhs\\):\\([0-9]+\\):\\([0-9]+\\):\\(.+\\)"
;;    1 2 3 4) flymake-err-line-patterns)


(setq mouse-drag-copy-region nil)  ; stops selection with a mouse being immediately injected to the kill ring
(setq x-select-enable-primary nil)  ; stops killing/yanking interacting with primary X11 selection
(setq x-select-enable-clipboard t)  ; makes killing/yanking interact with clipboard X11 selection

(autoload 'bison-mode "bison-mode.el")
(setq auto-mode-alist (cons '("\\.y$" . bison-mode) auto-mode-alist))

(add-to-list 'auto-mode-alist '("\\.\\(pl\\|pro\\|lgt\\)" . prolog-mode))

(autoload 'flex-mode "flex-mode")
(setq auto-mode-alist (cons '("\\.l$" . flex-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.lex$" . flex-mode) auto-mode-alist))

(require 'rainbow-delimiters)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

(put 'downcase-region 'disabled nil)

(require 'journal-headers)

(defun keyword-bracket-word ()
  (interactive)
  (backward-word)
  (insert "\\<")
  (forward-word)
  (insert ">")
  )

(require 'tex-mode)
(require 'sentence-per-line)

(define-key tex-mode-map (kbd "C-c C-k") 'keyword-bracket-word)
;;(define-key tex-mode-map (kbd "C-c C-u") 'fill-paragraph-sentence-per-line-mark-nl)
;;(define-key tex-mode-map (kbd "C-c C-i") 'revert-paragraph-nl)

;;(define-key latex-mode-map (kbd "C-c C-u") 'fill-paragraph-sentence-per-line-mark-nl)
;;(define-key latex-mode-map (kbd "C-c C-i") 'revert-paragraph-nl)


(setq default-tab-width 4)

(defun ask-before-closing ()
  "Ask whether or not to close, and then close if y was pressed"
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to exit Emacs? "))
      (if (< emacs-major-version 22)
          (save-buffers-kill-terminal)
        (save-buffers-kill-emacs))
    (message "Canceled exit")))

(when window-system
  (global-set-key (kbd "C-x C-c") 'ask-before-closing))

(require 'my-synctex)

; use IPython
(setq-default py-shell-name "ipython3")
(setq-default py-which-bufname "IPython")
; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p nil)
(setq py-switch-buffers-on-execute-p nil)
(setq py-split-windows-on-execute-p nil)
(setq py-force-py-shell-name-p t)
; try to automagically figure out indentation
(setq py-smart-indentation t)

; (load-file "~/shared/UTexas/cs109_python/present.el")
; (load-file "~/shared/UTexas/cs109_python/tests/grading.el")

;; '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inversxe-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 113 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))))
;; '(unicode-tokens-symbol-font-face ((t (:slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "STIX"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((((background light)) (:foreground "#306132"))))
 '(rainbow-delimiters-depth-2-face ((((background light)) (:foreground "#4368f5"))))
 '(rainbow-delimiters-depth-3-face ((((background light)) (:foreground "#d0d102"))))
 '(rainbow-delimiters-depth-4-face ((((background light)) (:foreground "#50f851"))))
 '(rainbow-delimiters-depth-5-face ((((background light)) (:foreground "#f06363"))))
 '(rainbow-delimiters-depth-6-face ((((background light)) (:foreground "#5266f9"))))
 '(rainbow-delimiters-depth-7-face ((((background light)) (:foreground "#858581"))))
 '(rainbow-delimiters-depth-8-face ((((background light)) (:foreground "#50d851"))))
 '(rainbow-delimiters-depth-9-face ((((background light)) (:foreground "#d86061"))))
 '(rainbow-delimiters-unmatched-face ((((background light)) (:foreground "#ff0000"))))
 '(whitespace-line ((t (:foreground "dark red"))))
 '(whitespace-space ((t (:foreground "lightgray"))))
 '(whitespace-tab ((t (:foreground "lightgray")))))



(require 'package)
(setq
 package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                    ("org" . "http://orgmode.org/elpa/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("melpa-stable" . "http://stable.melpa.org/packages/")))

(package-initialize)
;; (package-refresh-contents)
;; (package-install 'use-package)

(require 'use-package)

(use-package ensime
  :ensure t
  :pin melpa)

(use-package sbt-mode
  :pin melpa)

(use-package scala-mode
  :pin melpa)

(use-package poly-markdown
  :ensure t)

(use-package cython-mode
  :pin melpa)
;; (use-package flycheck-cython
;;   :pin melpa)



(require 'unicode-escape)

(require 'auctex-extensions)

(setq-default TeX-master 'dwim)

;(require 'auctex-latexmk)
;(auctex-latexmk-setup)
;(setq auctex-latexmk-inherit-TeX-PDF-mode t)
(put 'upcase-region 'disabled nil)

;; (require 'ks)
;; (require 'flymd)






;; Automatic Options

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-source-correlate-mode t)
 '(TeX-source-correlate-start-server t)
 '(TeX-view-program-selection (quote ((output-pdf "EvinceDbus"))) t)
 '(c-basic-offset 4)
 '(c-default-style
   (quote
    ((c-mode . "linux")
     (c++-mode . "linux")
     (java-mode . "java")
     (awk-mode . "awk")
     (other . "gnu"))))
 '(coq-compile-before-require nil)
 '(coq-compile-parallel-in-background nil)
 '(coq-double-hit-enable t)
 '(coq-maths-menu-enable nil)
 '(coq-shortcut-alist
   (quote
    (("<>" . "⋄")
     ("|>" . "⊳")
     ("\\/" . "∨")
     ("/\\" . "∧")
     ("+O" . "⊕")
     ("-O" . "⊖")
     ("xO" . "⊗")
     ("/O" . "⊘")
     ("|+" . "†")
     ("|++" . "‡")
     ("<=" . "≤")
     (">=" . "≥")
     ("-|" . "⊣")
     ("||" . "∥")
     ("==" . "≡")
     ("~=" . "≃")
     ("~~~" . "≍")
     ("~~" . "≈")
     ("~==" . "≅")
     ("|<>|" . "⋈")
     ("|=" . "⊨")
     ("=." . "≐")
     ("_|_" . "⊥")
     ("</" . "≮")
     (">=/" . "≱")
     ("=/" . "≠")
     ("==/" . "≢")
     ("~/" . "≁")
     ("~=/" . "≄")
     ("~~/" . "≉")
     ("~==/" . "≇")
     ("<-" . "←")
     ("<=" . "⇐")
     ("->" . "→")
     ("<->" . "↔")
     ("<=>" . "⇔")
     ("|->" . "↦")
     ("<--" . "⟵")
     ("<==" . "⟸")
     ("-->" . "⟶")
     ("==>" . "⟹")
     ("<==>" . "⟷")
     ("|-->" . "⟼")
     ("<-->" . "⟷")
     ("[|" . "⟦")
     ("|]" . "⟧")
     ("``" . "”")
     ("''" . "“")
     ("--" . "–")
     ("---" . "—")
     ("\\int" . "ℤ")
     ("\\rat" . "ℚ")
     ("\\complex" . "ℂ")
     ("\\euro" . "€")
     ("\\yen" . "¥")
     ("\\cent" . "¢")
     ("\\nat" . "ℕ")
     ("!!" . "∀")
     ("??" . "∃")
     ("[=" . "⊑")
     ("]=" . "⊒")
     ("\\~" . "¬")
     ("\\eta" . "η"))))
 '(coq-token-symbol-map
   (quote
    (("alpha" "α")
     ("beta" "β")
     ("gamma" "γ")
     ("delta" "δ")
     ("epsilon" "ε")
     ("zeta" "ζ")
     ("eta" "η")
     ("eta'" "η'")
     ("eta''" "η''")
     ("theta" "θ")
     ("iota" "ι")
     ("kappa" "κ")
     ("lambda" "λ")
     ("mu" "μ")
     ("nu" "ν")
     ("xi" "ξ")
     ("pi" "π")
     ("rho" "ρ")
     ("sigma" "σ")
     ("tau" "τ")
     ("upsilon" "υ")
     ("phi" "ϕ")
     ("chi" "χ")
     ("psi" "ψ")
     ("omega" "ω")
     ("Gamma" "Γ")
     ("Delta" "Δ")
     ("Theta" "Θ")
     ("Lambda" "Λ")
     ("Xi" "Ξ")
     ("Pi" "Π")
     ("Sigma" "Σ")
     ("Upsilon" "Υ")
     ("Phi" "Φ")
     ("Psi" "Ψ")
     ("Omega" "Ω")
     ("forall" "∀")
     ("exists" "∃")
     ("nat" "ℕ" type)
     ("complex" "ℂ" type)
     ("real" "ℝ" type)
     ("int" "ℤ" type)
     ("rat" "ℚ" type)
     ("bool" "B" underline type)
     ("false" "false" sans)
     ("true" "true" sans)
     ("WHILE" "WHILE" bold sans)
     ("DO" "DO" bold sans)
     ("END" "END" bold sans)
     ("SKIP" "SKIP" bold sans)
     ("THEN" "THEN" bold sans)
     ("ELSE" "ELSE" bold sans)
     ("IFB" "IFB" bold sans)
     ("FI" "FI" bold sans)
     ("{{" "⦃" bold)
     ("}}" "⦄" bold)
     ("TODO" "TODO" underline sans)
     ("lhd" "⊲")
     ("rhd" "⊳")
     ("<=" "≤")
     (">=" "≥")
     ("=>" "⇒")
     ("->" "→")
     ("<-" "←")
     ("<->" "↔")
     ("++" "⧺")
     ("===" "≡")
     ("=/=" "≢")
     ("=~=" "≅")
     ("==b" "≡")
     ("<>b" "≢")
     ("-->" "⟹-")
     ("++>" "⟹+")
     ("==>" "⟹")
     (":=" "≔")
     ("|-" "⊢")
     ("<>" "≠")
     ("-|" "⊣")
     ("\\/" "∨")
     ("/\\" "∧")
     ("~" "¬")
     ("============================" "⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯⎯" bold tactical))))
 '(coq-unicode-tokens-enable t)
 '(flymd-close-buffer-delete-temp-files t)
 '(flymd-output-directory "/tmp/")
 '(indent-tabs-mode nil)
 '(markdown-code-lang-modes
   (quote
    (("ocaml" . tuareg-mode)
     ("elisp" . emacs-lisp-mode)
     ("ditaa" . artist-mode)
     ("asymptote" . asy-mode)
     ("dot" . fundamental-mode)
     ("sqlite" . sql-mode)
     ("calc" . fundamental-mode)
     ("C" . c-mode)
     ("cpp" . c++-mode)
     ("C++" . c++-mode)
     ("screen" . shell-script-mode)
     ("shell" . sh-mode)
     ("bash" . sh-mode)
     ("Python" . python-mode))))
 '(package-selected-packages
   (quote
    (textx-mode jinja2-mode yaml-mode clang-format+ mmm-jinja2 cython-mode poly-markdown use-package ascii-art-to-unicode flymd markdown-mode unicode-escape ensime auctex)))
 '(proof-disappearing-proofs nil)
 '(proof-electric-terminator-enable nil)
 '(proof-output-tooltips t)
 '(proof-sticky-errors nil)
 '(python-shell-interpreter "python3")
 '(standard-indent 2)
 '(warning-suppress-types (quote ((undo discard-info))))
 '(whitespace-line-column 155)
 '(whitespace-style
   (quote
    (face trailing tabs spaces lines empty indentation space-after-tab space-before-tab))))

(tool-bar-mode -1)

(global-set-key (kbd "C-/") 'comment-line)

;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
(put 'narrow-to-region 'disabled nil)


;; (require 'python-cpp-mode)
(add-hook 'c-mode-common-hook #'clang-format+-mode)

  (defun uniquify-all-lines-region (start end)
    "Find duplicate lines in region START to END keeping first occurrence."
    (interactive "*r")
    (save-excursion
      (let ((end (copy-marker end)))
        (while
            (progn
              (goto-char start)
              (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
          (replace-match "\\1\n\\2")))))
  
  (defun uniquify-all-lines-buffer ()
    "Delete duplicate lines in buffer and keep first occurrence."
    (interactive "*")
    (uniquify-all-lines-region (point-min) (point-max)))
