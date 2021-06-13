(require 's)
(require 'serenade-log)
(defcustom serenade-helm-M-x t 
  "if true, display speech bindings in helm M-x")

(setq serenade-helm-map (ht))

;;   :group 'serenade-mode)

;; (defface helm-serenade-command
;;   '((t
;;      ;; :extend t
;;      :foreground  "plum3" ;;"#0f1011"
;;      :underline nil))
;;   "Face for serenade helm.")
(defface helm-serenade-command '((t :foreground "plum3" 
                                    :underline t)) 
  "Face for serenade helm.")

(defun serenade--update-helm-map (speech command) 
  (serenade--info "adding helm map") 
  (if-let* ((current (ht-get* serenade-helm-map (symbol-name command)))) 
      (progn ;; (message current (ht-get* serenade-helm-map))
        (if  (not (member speech (s-split "|" current t))) 
            (ht-set serenade-helm-map (symbol-name command) 
                    (concat speech " | " current)))) 
    (progn (ht-set serenade-helm-map (symbol-name command) speech))))

(defun serenade--helm-match (cand) 
  (message "matching..") 
  (let* ((cand  (ht-get* serenade-helm-map cand))) 
    (message cand)) 
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

;; (serenade--get-helm-candidates)
;; (message (prin1-to-string serenade--helm-candidates))

;; see spacemacs-motion-face/ plum3

(defun seranade--bind-helm-transformer () 
  (defun serenade--helm-M-x-transformer-1 (candidates &optional sort) 
    "xxx" 
    (with-helm-current-buffer 
      (cl-loop with local-map = (helm-M-x-current-mode-map-alist) for cand in candidates for
               local-key = (car (rassq cand local-map)) for key = (substitute-command-keys (format
                                                                                            "\\[%s]"
                                                                                            cand))
               for voice-command = (serenade--helm-match cand) unless (get (intern (if (consp cand)
                                                                                       (car cand)
                                                                                     cand))
                                                                           'helm-only) collect (cons
                                                                                                (cond
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
                                                                                                    'serenade-helm-command)))
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
                                                                                                    'serenade-helm-command)))
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
                                                                                                    'serenade-helm-command)))
                                                                                                 (t
                                                                                                  (format
                                                                                                   "%s (%s)"
                                                                                                   cand
                                                                                                   (propertize
                                                                                                    key
                                                                                                    'face
                                                                                                    'helm-M-x-key))))
                                                                                                cand)
                                                                           into ls finally return
                                                                           (if sort (sort ls
                                                                                          #'helm-generic-sort-fn)
                                                                             ls)))))

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

(provide 'serenade-helm)
