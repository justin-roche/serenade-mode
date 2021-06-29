(defcustom serenade-helm-M-x t 
  "if true, display speech bindings in helm M-x")

(setq serenade-helm-M-x-map (ht))
;; the map consulted by helm for displaying speech bindings next to commands"

(setq serenade--add-helm-candidates '())
;; the current helm candidates"

(defun serenade--update-helm-M-x-map (speech command) 
  "This updates the serenade-helm-M-x-map with SPEECH and COMMAND, combinng synonmys and excluding identical speech patterns."
  (if command (if-let* ((current (ht-get* serenade-helm-M-x-map (symbol-name command)))) 
                  (progn (if  (not (member speech (s-split "|" current t))) 
                             (ht-set serenade-helm-M-x-map (symbol-name command) 
                                     (concat speech " | " current)))) 
                (progn (ht-set serenade-helm-M-x-map (symbol-name command) speech)))))

(defun serenade--clear-helm-M-x-map () 
  (setq serenade-helm-M-x-map (ht)))

(defun serenade--helm-M-x-match (cand) 
  (if-let* ((cand  (ht-get* serenade-helm-M-x-map cand))) 
      (serenade--info (concat "matched: " cand))) 
  (or (ht-get* serenade-helm-M-x-map cand) 
      nil))

(defun serenade--add-helm-candidate (speech &optional command) 
  "add a single helm candidate to the helm source"
  (let* ((item (if command (format "%s %s" command (propertize speech 'face 'helm-serenade-command)) 
                 (format "%s" (propertize speech 'face 'helm-serenade-info))))) 
    (setq serenade--helm-candidates (append serenade--helm-candidates (list item)))))

(defun serenade--get-helm-candidates (voice-maps) 
  (setq serenade--helm-candidates '())

  ;; loop over speech maps
  (ht-each '(lambda (key value)
              ;; loop over bindings in speech map
              (ht-each '(lambda (speech binding) 
                          (let* ((command (ht-get* binding "command"))) 
                            (serenade--add-helm-candidate speech command))) value)) voice-maps)
  serenade--helm-candidates)

(defun serenade--get-helm-active-candidates (voice-maps) 
  "like serenade--get-helm-candidates, except return only candidates for active maps" 
  (setq serenade--helm-candidates '())
  ;; loop over speech maps
  (ht-each '(lambda (speech-map-name speech-map) 
              (let* ((mode-symbol (intern-soft speech-map-name))) 
                (if (or (string-equal speech-map-name "global") 
                        (and (boundp mode-symbol) 
                             (symbol-value mode-symbol)))
                    ;; loop over bindings in speech map
                    (ht-each '(lambda (speech binding) 
                                (let* ((command (ht-get* binding "command"))) 
                                  (serenade--add-helm-candidate speech command))) ;;
                             speech-map))) ) ;;
           voice-maps)
  serenade--helm-candidates)

(defun serenade--get-helm-selectors (selectors) 
  (setq serenade--helm-candidates '()) 
  (-each selectors '(lambda ( value) 
                      (serenade--add-helm-candidate value)))
  serenade--helm-candidates)

(defun serenade--helm-M-x-transformer (candidates &optional sort) 
  (with-helm-current-buffer 
    (cl-loop with local-map = (helm-M-x-current-mode-map-alist) for cand in candidates for local-key
             = (car (rassq cand local-map)) for key = (substitute-command-keys (format "\\[%s]"
                                                                                       cand)) for
                                                                                       voice-command
                                                                                       =
                                                                                       (serenade--helm-M-x-match
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
                                                                                                  (serenade--helm-M-x-match
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
                                                                                                  (serenade--helm-M-x-match
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
                                                                                                  (serenade--helm-M-x-match
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
