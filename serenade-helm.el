
(defcustom serenade-helm-M-x nil 
  "if true, display speech bindings in helm M-x")

(require 's)
(setq serenade-helm-map (ht))

(defun serenade--update-helm-map (speech command) 
  (if-let* ((current (ht-get* serenade-helm-map (symbol-name command)))) 
      (if  (not (member speech (s-split "|" current t))) 
          (ht-set serenade-helm-map (symbol-name command) 
                  (concat speech " | " current))) 
    (ht-set serenade-helm-map (symbol-name command) speech)))

(defun serenade-map-helm (cand) 
  (or (ht-get* serenade-helm-map cand) 
      nil))

;; see spacemacs-motion-face/ plum3
(defface helm-serenade-command '((t :foreground "#0f1011" 
                                    :underline t)) 
  "Face for serenade helm." 
  :group 'serenade-mode)

(defun helm-M-x-transformer-1 (candidates &optional sort) 
  "Transformer function to show bindings in emacs commands.
Show global bindings and local bindings according to current `major-mode'.
If SORT is non nil sort list with `helm-generic-sort-fn'.
Note that SORT should not be used when fuzzy matching because
fuzzy matching is running its own sort function with a different algorithm." 
  (with-helm-current-buffer 
    (cl-loop with local-map = (helm-M-x-current-mode-map-alist) for cand in candidates for local-key
             = (car (rassq cand local-map)) for key = (substitute-command-keys (format "\\[%s]"
                                                                                       cand)) for
                                                                                       voice-command
                                                                                       =
                                                                                       (serenade-map-helm
                                                                                        cand) unless
                                                                                        (get (intern
                                                                                              (if
                                                                                                  (consp
                                                                                                   cand)
                                                                                                  (car
                                                                                                   cand)
                                                                                                cand))
                                                                                             'helm-only)
                                                                                        collect
                                                                                        (cons (cond
                                                                                               ((and
                                                                                                 voice-command
                                                                                                 (string-match
                                                                                                  "^M-x"
                                                                                                  key)
                                                                                                 local-key) 
                                                                                                (format
                                                                                                 "%s (%s) (%s)"
                                                                                                 cand
                                                                                                 (propertize
                                                                                                  local-key
                                                                                                  'face
                                                                                                  'helm-M-x-key)
                                                                                                 (propertize
                                                                                                  (serenade-map-helm
                                                                                                   cand)
                                                                                                  'face
                                                                                                  'helm-serenade-command)))
                                                                                               ((and 
                                                                                                 (string-match
                                                                                                  "^M-x"
                                                                                                  key)
                                                                                                 local-key) 
                                                                                                (format
                                                                                                 "%s (%s)"
                                                                                                 cand
                                                                                                 (propertize
                                                                                                  local-key
                                                                                                  'face
                                                                                                  'helm-M-x-key)))
                                                                                               ((and 
                                                                                                 voice-command
                                                                                                 (string-match
                                                                                                  "^M-x"
                                                                                                  key)) 
                                                                                                (format
                                                                                                 "%s (%s)"
                                                                                                 cand
                                                                                                 (propertize
                                                                                                  (serenade-map-helm
                                                                                                   cand)
                                                                                                  'face
                                                                                                  'helm-serenade-command)))
                                                                                               ((string-match
                                                                                                 "^M-x"
                                                                                                 key)
                                                                                                cand)
                                                                                               (voice-command
                                                                                                (format
                                                                                                 "%s (%s) (%s)"
                                                                                                 cand
                                                                                                 (propertize
                                                                                                  key
                                                                                                  'face
                                                                                                  'helm-M-x-key)
                                                                                                 (propertize
                                                                                                  (serenade-map-helm
                                                                                                   cand)
                                                                                                  'face
                                                                                                  'helm-serenade-command)))
                                                                                               (t
                                                                                                (format
                                                                                                 "%s (%s)"
                                                                                                 cand
                                                                                                 (propertize
                                                                                                  key
                                                                                                  'face
                                                                                                  'helm-M-x-key))))
                                                                                              cand)
                                                                                        into ls
                                                                                        finally
                                                                                        return (if
                                                                                                   sort
                                                                                                   (sort
                                                                                                    ls
                                                                                                    #'helm-generic-sort-fn)
                                                                                                 ls))))

(provide 'serenade-helm)
