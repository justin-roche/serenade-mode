(require 'yasnippet)

(defun serenade--get-snippet (name) 
  (ignore-errors (let* ((found-snippet (yas-lookup-snippet name))) 
                   (message (prin1-to-string (yas--template-content found-snippet))) found-snippet)))

(defun serenade--insert-yasnippet (name) 
  (if serenade-evil (evil-insert-state)) 
  (let* ((snippet  (serenade--get-snippet name))) 
    (if snippet (yas-expand-snippet snippet) 
      (message (concat "no snippet found for \"" name "\"" )))))

(defun serenade--insert-yasnippet-with-args (name) 
  (if serenade-evil (evil-insert-state)) 
  (let* ((snippet  (serenade--get-snippet name))) 
    (if snippet (yas-expand-snippet snippet) 
      (message (concat "no snippet found for \"" name "\"" )))))

(provide 'serenade-snippet)
