(require 'yasnippet)

(defun serenade--get-snippet (name) 
  (let* ((found-snippet (yas-lookup-snippet name))) 
    (message (prin1-to-string (yas--template-content found-snippet))) found-snippet))

(defun serenade--insert-yasnippet (name) 
  (evil-insert-state) 
  (let* ((snippet  (serenade--get-snippet name))) 
    (if snippet (yas-expand-snippet snippet))))

;; (defun serenade--test-snippet ()
;;   (interactive)
;;   (serenade--insert-yasnippet "defun"))
