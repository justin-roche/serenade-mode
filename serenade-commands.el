(require 'helm)
(require 'ht)
(require 'cl)
(require 'dash)
(require 's)
(require 'serenade-helm)
(require 'serenade-defaults)

(defvar serenade-speech-maps (ht("global" (ht)) ) 
  "hashtable of Serenade voice maps")

(setq serenade--add-builtin-global-defaults t )
(setq serenade--add-custom-global-defaults t )

(defun serenade--initialize-mode-maps ()
  ;; This function clears the SERENADE-SPEECH-MAPS and sets them according to the default binding.
  (serenade--clear-mode-maps) 
  (if serenade--add-builtin-global-defaults (serenade--add-builtin-global-defaults)) 
  (if serenade--add-custom-global-defaults (serenade--add-custom-global-defaults)) 
  (run-hooks 'serenade-speech-maps-hook))

(defun serenade--clear-mode-maps () 
  (setq serenade-speech-maps (ht("global" (ht)) ) ) 
  (serenade--clear-helm-M-x-map))

(defun serenade--get-global-map () 
  (ht-get serenade-speech-maps "global"))

(defun serenade--add-builtin-global-defaults () 
  (serenade-global-set-speech serenade--builtin-global-defaults))

(defun serenade--add-custom-global-defaults () 
  (serenade-global-set-speech serenade--custom-global-defaults))

(cl-defun 
    serenade-global-set-speech
    (speech &optional command )
  ;; Convenience function for adding speech bindings to the global serenade speech map. Possible inputs are an association list of speech-command bindings, a single command from which the asociated speech is automatically generated, or a pair of SPEECH and COMMAND.
  (if (and (listp speech)) 
      (dolist (item speech ) 
        (serenade-global-set-speech (car item) 
                                    (cdr item)))) 
  (serenade-define-speech 'global speech command))

(defun serenade-auto-define-speech (mode command-or-list) 
  (if (listp command-or-list) 
      (dolist (command command-or-list ) 
        (serenade-auto-define-speech mode command)) 
    (let* ((split-command  (s-replace  "-" " " (symbol-name command-or-list)))) 
      (serenade-define-speech mode split-command command-or-list))))

(cl-defun 
    serenade-define-speech
    (mode speech &optional command)
  ;; this function associates speech pattern SPEECH with an 8lisp function COMMAND for the symbol MODE. If the speech-map provided by MODE does not exist a speech-map is created. If mode is the special symbol 'global then the binding is created for the global speech map. If a previous binding exists for the speech pattern it is overwritten.
  (if (listp speech) 
      (dolist (item speech ) 
        (serenade-define-speech mode (car item) 
                                (cdr item))) 
    (let* ((name (symbol-name mode)) 
           (voice-map (ht-get serenade-speech-maps name ))) 
      (if (string-equal name "global") 
          (serenade--set-speech-bindings "global" speech command) 
        (progn (if (not voice-map) 
                   (ht-set serenade-speech-maps name (ht))) 
               (serenade--set-speech-bindings name speech command))) 
      (if serenade-helm-M-x (serenade--update-helm-M-x-map speech command)))))

(defun serenade--set-speech-bindings (map-name pattern command) 
  (ht-set (ht-get serenade-speech-maps name ) speech (ht ("command" command))))

(defun serenade--find-voice-binding (speech) 
  (or (serenade--find-in-active-minor-maps speech) 
      (serenade--find-in-active-major-map speech) 
      (serenade--find-in-global-map speech)))

(defun serenade--find-in-active-minor-maps (speech)
  ;; search speech map applicable to the current minor-mode-map-alist. If any contain the speech patterns SPEECH return the command for the speech.
  (catch 'result 
    (mapc (lambda (mode-and-map) 
            (if (and (boundp (car mode-and-map)) 
                     (symbol-value (car mode-and-map))) 
                (if-let* ((mode (symbol-name (car mode-and-map))) 
                          (voice-map (ht-get* serenade-speech-maps mode)) 
                          (command (ht-get* voice-map speech))) 
                    (throw 'result command)))) minor-mode-map-alist)
    nil))

(defun serenade--find-in-active-major-map (speech) 
  (if-let* ((current-mode-map (ht-get serenade-speech-maps (symbol-name major-mode)))) 
      (ht-get* current-mode-map speech)))

(defun serenade--find-in-global-map (speech) 
  (ht-get* serenade-speech-maps "global" speech))

(cl-defun 
    serenade-helm-commands
    ()
  ;; This function provides all current speech bindings in a helm buffer.
  (interactive) 
  (helm :sources (helm-build-sync-source "serenade" 
                   :candidates (serenade--get-helm-candidates serenade-speech-maps)) 
        :buffer "*helm serenade*"))

(cl-defun 
    serenade-helm-active-commands
    ()
  ;; This function provides all current active speech bindings in a helm buffer.
  (interactive) 
  (helm :sources (helm-build-sync-source "serenade" 
                   :candidates (serenade--get-helm-active-candidates serenade-speech-maps)) 
        :buffer "*helm serenade*"))

(cl-defun 
    serenade-helm-selectors
    ()
  ;; This function provides all current active speech bindings in a helm buffer.
  (interactive) 
  (helm :sources (helm-build-sync-source "serenade" 
                   :candidates (serenade--get-helm-selectors serenade--selectors)) 
        :buffer "*helm serenade*"))

(defmacro serc (fn &rest args)
  ;; Curry the function FN with ARGS, and add the resulting function to the global namespace with a descriptive name and docstring
  (let* ((formatted-args (-map '(lambda (item) 
                                  (cond ((eq (type-of item) 'string) item) 
                                        ((eq (type-of item) 'number) 
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
         (interactive) 
         (apply ',fn (append ',args speech-args)))) 
    `(intern-soft ',curried-name )))

(defmacro* serd (name args   body &optional &keys pre )
  ;; Define the function NAME which executes, and add the resulting function to the global namespace.
  `(defun ,(intern-soft (symbol-name name))  ,args 
     (interactive)
     ,body
     (if ,pre (put ',name 'serenade-pre-hook ,pre)) 
     (intern-soft ',name )))

;; (serenade-define-speech 'global `(("a" . ,(serd custom-lambdas()
;;                                                 (setq test-val 1)
;;                                                 :pre 1))))
;; (serenade--call-function-with-args (ht-get* (serenade--find-voice-binding "a") "command" )nil)

(provide 'serenade-commands)
