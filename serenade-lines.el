
(defcustom serenade-enable-double-line-numbers nil 
  "if t, serenade mode shows both relative and absolute line numbers")

(defun serenade-double-line-numbers-on ()
  r  
  (interactive) 
  (global-display-line-numbers-mode +1) 
  (nlinum-mode +1) 
  (nlinum-relative-off))

(defun serenade-double-line-numbers-off () 
  (interactive) 
  (nlinum-mode -1))

;; (diminish 'serenade-mode
;;           '(:propertize "Ⓢ"
;;                         face
;;                         '(:foreground "plum3")))

(provide 'serenade-lines)
