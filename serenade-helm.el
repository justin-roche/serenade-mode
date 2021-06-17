(require 's)
(require 'serenade-log)
(defcustom serenade-helm-M-x t 
  "if true, display speech bindings in helm M-x")

(setq serenade-helm-map (ht))

(defun serenade--update-helm-map (speech command) 
  (if command (if-let* ((current (ht-get* serenade-helm-map (symbol-name command)))) 
                  (progn (if  (not (member speech (s-split "|" current t))) 
                             (ht-set serenade-helm-map (symbol-name command) 
                                     (concat speech " | " current)))) 
                (progn (ht-set serenade-helm-map (symbol-name command) speech)))))

(defun serenade--helm-match (cand) 
  (message "matching..") 
  (if-let* ((cand  (ht-get* serenade-helm-map cand))) 
      (serenade--info (concat "matched: " cand))) 
  (or (ht-get* serenade-helm-map cand) 
      nil))

(defun serenade--clear-helm-map () 
  (setq serenade-helm-map (ht)))

(defun serenade--get-helm-candidates (voice-maps) 
  (setq serenade--helm-candidates '()) 
  (ht-each '(lambda (key value) 
              (ht-each '(lambda (speech binding) 
                          (let* ((command (ht-get* binding "command"))) 
                            (setq serenade--helm-candidates (append serenade--helm-candidates (list
                                                                                               (format
                                                                                                "%s %s"
                                                                                                command
                                                                                                (propertize
                                                                                                 speech
                                                                                                 'face
                                                                                                 'helm-serenade-command))))))) value)) voice-maps)
  serenade--helm-candidates)

(defun serenade--get-helm-candidates-restricted (voice-maps) 
  (setq serenade--helm-candidates '()) 
  (ht-each '(lambda (key value) 
              (ht-each '(lambda (speech binding) 
                          (let* ((command (ht-get* binding "command"))) 
                            (setq serenade--helm-candidates (append serenade--helm-candidates (list
                                                                                               (format
                                                                                                "%s %s"
                                                                                                command
                                                                                                (propertize
                                                                                                 speech
                                                                                                 'face
                                                                                                 'helm-serenade-command))))))) value)) voice-maps)
  serenade--helm-candidates)

(defun serenade--helm-M-x-transformer (candidates &optional sort) 
  (with-helm-current-buffer 
    (cl-loop with local-map = (helm-M-x-current-mode-map-alist) for cand in candidates for local-key
             = (car (rassq cand local-map)) for key = (substitute-command-keys (format "\\[%s]"
                                                                                       cand)) for
                                                                                       voice-command
                                                                                       =
                                                                                       (serenade--helm-match
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
                                                                                                  (serenade--helm-match
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
                                                                                                  (serenade--helm-match
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
                                                                                                  (serenade--helm-match
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

(defun serenade--advise-helm-transformer () 
  (advice-add 'helm-M-x-transformer-1 
              :override #'serenade--helm-M-x-transformer))

(defun serenade--unadvise-helm-transformer () 
  (advice-remove 'helm-M-x-transformer-1 #'serenade--helm-M-x-transformer))

(provide 'serenade-helm)
