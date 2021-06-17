
(require 'nlinum)
(require 'nlinum-relative)

(defcustom serenade-enable-double-line-numbers t 
  "if t, serenade mode shows both relative and absolute line numbers")

(defun serenade--double-line-numbers-on () 
  (global-display-line-numbers-mode +1) 
  (nlinum-mode +1) 
  (nlinum-relative-mode -1))

(defun serenade-double-line-numbers-on () 
  (interactive) 
  (serenade--double-line-numbers-on))

(defun serenade--double-line-numbers-off () 
  (nlinum-mode -1))

(defun serenade-double-line-numbers-off () 
  (interactive) 
  (serenade--double-line-numbers-off))

;; #0f1011

(provide 'serenade-lines)
