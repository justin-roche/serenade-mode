(require 'ht)
(require 'cl)
(require 's)
(require 'serenade-helm)

(defvar serenade-speech-maps (ht("global" (ht)) ) 
  "hashtable of Serenade voice maps")

;; Serenade itself will handle "copy <target>" and "cut <target>", sending a diff message and populating the system clipboard. Serenades built-in "paste" command is also sufficient for sending a diff with the correct result.

(setq serenade--global-defaults '(;;
                                  ( "copy" .   serenade--copy-selection ) 
                                  ( "cut" .   serenade--cut-selection ) 
                                  ( "select <target>" .   serenade--select-target ) 
                                  ( "undo" .   serenade--undo ) 
                                  ( "redo" .   serenade--redo ) 
                                  ( "open <file>" . serenade--open-file  ) 
                                  ( "<nth> tab" . serenade--switch-tab  ) 
                                  ( "close tab" . delete-window  ) 
                                  ( "save"  . save-buffer  ) 
                                  ( "create tab" . split-window-right-and-focus  ) 
                                  ( "next tab" . next-buffer  ) 
                                  ( "previous tab" . previous-buffer) 
                                  ( "switch tab" . nil  ) 
                                  ( "open file list" . nil) 
                                  ( "scroll" . nil  ) 
                                  ( "style" . nil) 
                                  ( "go to definition" . nil) 
                                  ( "add breakpoint" . nil) 
                                  ( "remove breakpoint" . nil) 
                                  ( "toggle breakpoint" . nil) 
                                  ( "start debug" . nil  ) 
                                  ( "start debugging" . nil  ) 
                                  ( "stop debug" . nil  ) 
                                  ( "stop debugging" . nil  ) 
                                  ( "pause debug" . nil  ) 
                                  ( "show hover" . nil) 
                                  ( "continue debug" . nil  ) 
                                  ( "step into" . nil  ) 
                                  ( "step out" . nil  ) 
                                  ( "step over" . nil) 
                                  ( "continue" . nil)))

(defun serenade--initialize-mode-maps ()
  ;; This function clears the SERENADE-SPEECH-MAPS and sets them according to the default binding.
  (serenade--clear-mode-maps) 
  (serenade--add-default-bindings) 
  (run-hooks 'serenade-speech-maps-hook))

(defun serenade--clear-mode-maps () 
  (setq serenade-speech-maps (ht("global" (ht)) ) ) 
  (serenade--clear-helm-map))

(defun serenade--get-global-map () 
  (ht-get serenade-speech-maps "global"))

(defun serenade--add-default-bindings () 
  (serenade-global-set-speech serenade--global-defaults))

(cl-defun 
    serenade-global-set-speech
    (speech &optional command )
  ;; Convenience function for adding speech bindings to the global serenade speech map. Possible inputs are an association list of speech-command bindings, a single command from which the asociated speech is automatically generated, or a pair of SPEECH and COMMAND.
  (if (and (listp speech)) 
      (dolist (item speech ) 
        (serenade-global-set-speech (car item) 
                                    (cdr item)))) 
  (if (and (symbolp speech) 
           (not command)) 
      (let* ((split-command  (s-replace  "-" " " (symbol-name speech)))) 
        (serenade-global-set-speech split-command speech))) 
  (if (and (stringp speech) 
           (symbolp command)) 
      (serenade-define-speech 'global speech command)))

(defun serenade-define-speech (mode speech command)
  ;; this function associates speech pattern SPEECH with an 8lisp function COMMAND for the symbol MODE. If the speech-map provided by MODE does not exist a speech-map is created. If mode is the special symbol 'global then the binding is created for the global speech map. If a previous binding exists for the speech pattern it is overwritten.
  (let* ((name (symbol-name mode)) 
         (voice-map (ht-get serenade-speech-maps name ))) 
    (if (string-equal name "global") 
        (ht-set (ht-get serenade-speech-maps "global") speech (ht("command" command))  ) 
      (if voice-map (ht-set voice-map speech (ht ("command" command))) 
        (progn (ht-set serenade-speech-maps name (ht)) 
               (ht-set (ht-get serenade-speech-maps name ) speech (ht ("command" command)))))) 
    (if serenade-helm-M-x (serenade--update-helm-map speech command))))

(defun serenade--find-voice-binding (speech) 
  (or (serenade--find-in-active-minor-maps speech) 
      (serenade--find-in-active-major-map speech) 
      (serenade--find-in-global-map speech)))

(defun serenade--find-in-active-minor-maps (speech)
  ;; search speech map applicable to the current minor-mode-map-alist. If any contain the speech patterns SPEECH return the command for the speech.
  (catch 'result 
    (mapc (lambda (mode-and-map) 
            (let* ((mode (symbol-name (car mode-and-map))) 
                   (voice-map (ht-get* serenade-speech-maps mode))) 
              (if voice-map (let* ((command (ht-get* voice-map speech))) 
                              (if command 
                                  (throw 'result command)))))) minor-mode-map-alist)
    nil))

(defun serenade--find-in-active-major-map (speech) 
  (if-let* ((current-mode-map (ht-get serenade-speech-maps (symbol-name major-mode)))) 
      (ht-get* current-mode-map speech)))

(defun serenade--find-in-global-map (speech) 
  (ht-get* serenade-speech-maps "global" speech))

(defun serenade-helm-commands () 
  ;; This function provide all current speech bindings in a helm buffer.
  (interactive) 
  (helm :sources (helm-build-sync-source "serenade" 
                   :candidates (serenade--get-helm-candidates serenade-speech-maps)) 
        :buffer "*helm serenade*"))

(provide 'serenade-commands)
