(require 'polymode)
(require 'polymode-compat)

(defcustom pm-host/python
  (pm-host-chunkmode :name "python"
                     :mode 'python-mode)
  "Python hostmode"
  :group 'poly-hostmodes
  :type 'object)


(defcustom  pm-inner/python-fenced-code
  (pm-inner-chunkmode :name "python-fenced-code"
                      :head-matcher (cons "^[ \t]*[^ \t].*[ (]f?\\(\"\"\"\\)" 1)
                      :tail-matcher (cons "\\(\"\"\"\\)[^\"]*$" 1)
                      :mode 'c++-mode
                      :head-mode 'host
                      :tail-mode 'host)
  "Python/C++ fenced code block."
  :group 'poly-innermodes
  :type 'object)


(defcustom  pm-inner/python-inline-code
  (pm-inner-chunkmode :name "python-inline-code"
                      :head-matcher (cons "^[^\"]* \\(f\"\\)[^\"]" 1)
                      :tail-matcher (cons "[^\"\\]\\(\"\\)\\([^\"]\\|$\\)" 1)
                      :mode 'c++-mode
                      :head-mode 'host
                      :tail-mode 'host)
  "Python/C++ inline code."
  :group 'poly-innermodes
  :type 'object)

(define-polymode poly-python-mode
  :hostmode 'pm-host/python
  :innermodes '(pm-inner/python-fenced-code pm-inner/python-inline-code))

(provide 'python-cpp-mode)
