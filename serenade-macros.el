(defmacro serc (fn &rest args) 
  "Curry the function FN with ARGS, and add the resulting function to the global namespace with a descriptive name and docstring"
  (let* ((formatted-args (-map '(lambda (item) 
                                  (cond ((eq (type-of item) 'string) item) 
                                        ((eq (type-of item) 'integer) 
                                         (number-to-string item)) 
                                        ((eq (type-of item) 'symbol) 
                                         (symbol-name item)))) args )) 
         (curried-name (intern (concat "serenade-curried->" (symbol-name fn) "->"(mapconcat
                                                                                  'identity
                                                                                  formatted-args
                                                                                  "--")))))
    (defalias  curried-name 
      `(lambda 
         (&rest 
          speech-args) 
         ,(format "Run %s with arguments: %s" fn (mapconcat 'identity formatted-args ", "))
         ;; (interactive)
         ;; TODO: no error caught here? (condition-case nil)
         (apply ',fn (append ',args speech-args)))) 
    `(intern-soft ',curried-name )))

(defmacro seri (fn ) 
  "Call the function FN interactively, and add the resulting function to the global namespace with a descriptive name and docstring. Return the new symbol."
  (let* ((curried-name (intern (concat "serenade-interactive->" (symbol-name fn) )))) 
    (defalias  curried-name 
      `(lambda 
         (&rest 
          speech-args) 
         ,(format "Call %s interactively." fn ) 
         (call-interactively ',fn))) 
    `(intern-soft ',curried-name )))

(defmacro* serd (name args   body &optional &keys pre )
  "Define the function NAME which executes, and add the resulting function to the global namespace."
  `(defun ,(intern-soft (symbol-name name))  ,args 
     (interactive)
     ,body
     (if ,pre (put ',name 'serenade-pre-hook ,pre)) 
     (intern-soft ',name )))

(provide 'serenade-macros)
