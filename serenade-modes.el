(defcustom serenade-mode-filetypes 
  '("js" "py" "c" "h" "cpp" "cc" "cxx" "c++" "hpp" "hh" "hxx" "h++""cs""css" "scss""dart" "go"
    "html" "vue" "svelte" "java" "js" "jsx" "jsx" "js""jsx" "js" "kt" "py" "rb" "rs" "scss" "sh"
    "bash" "ts" "tsx" "tsx" "ts""vue" "html" "el")
  "The filetypes that can be used as serenade buffers, which are buffers subject to the diff operation.")

(defvar serenade-mode-config-map (ht ) 
  "The list of mode configurations.")

(setq serenade-active-mode-configuration nil )

(defun serenade--initialize-mode-config-map () 
  "This function clears the SERENADE-MODE-CONFIG-MAP and sets only the global (default) mode config"
  (serenade--clear-mode-config-map) 
  (serenade--configure-mode :mode 'global ))

(defun serenade--clear-mode-config-map () 
  (setq serenade-mode-config-map (ht ) ))

(defun serenade--set-active-mode-configuration () 
  "Set the active mode configuration based on the major-mode. If none is found, use the global default."
  (let* ((mode-name (symbol-name major-mode )) 
         (active-config  (ht-get* serenade-mode-config-map mode-name))) 
    (setq serenade-active-mode-configuration (or active-config 
                                                 (ht-get* serenade-mode-config-map "global")))))

(cl-defstruct 
    serenade-mode-configuration
  mode
  get-editor-state
  diff
  post-edit
  pre-edit)

(cl-defun 
    serenade--configure-mode 
    (&optional 
     &keys
     mode
     get-editor-state
     diff
     post-edit
     pre-edit)
  "This function is used to configure specific mode behavior for modes which does not relate to specific speech bindings. It accepts five optional keyword arguments:

:MODE symbol which names the major mode the configuration applies to.
:PRE-EDIT function which will run before every serenade edit in the mode.
:POST-EDIT function which will run after every serenade edit in the mode.
:GET-EDITOR-STATE function which returns a list of items of form  '(FILENAME SOURCE CURSOR), where filename is the name of the file, source is the contents for serenade to change, and cursor is the current location of the cursor).
:DIFF function accepting two parameters, source and cursor, which updates the buffer with the new source and cursor position."
  (let* ((config  (make-serenade-mode-configuration ;;
                   :mode (or mode 
                             nil) 
                   :get-editor-state (or get-editor-state 
                                         'serenade--get-editor-state) 
                   :diff (or diff 
                             'serenade--diff) 
                   :post-edit (or post-edit 
                                  nil) 
                   :pre-edit (or pre-edit 
                                 nil)))) 
    (ht-set serenade-mode-config-map (symbol-name mode) config)))

(defun serenade--set-serenade-buffer () 
  "Determines if the current buffers file extension is a valid member of SERENADE-MODE-FILE-TYPES. If it is set SERENADE-BUFFER to the current buffer, otherwise set it to nil."
  (if (and (buffer-file-name) 
           (file-name-extension (buffer-file-name))) 
      (let* ((ext (file-name-extension (buffer-file-name)))) 
        (if (member ext serenade-mode-filetypes) 
            (setq serenade-buffer (current-buffer) ) 
          (setq serenade-buffer nil ))) 
    (setq serenade-buffer nil )))
;; (serenade--configure-mode :mode 'global )
;; (serenade--initialize-mode-config-map)

(defun serenade-shell/diff (source cursor) 
  (let ((proc (get-buffer-process ( current-buffer )))) 
    (goto-char (process-mark proc)) 
    (kill-whole-line) 
    (insert (or contents 
                "<contents>")) 
    (goto-char cursor)))

(defun serenade--shell/get-editor-state () 
  (let ((proc (get-buffer-process ( current-buffer )))) 
    (goto-char (process-mark proc)) 
    (let* (( line-contents (thing-at-point 'line t)) 
           (cursor (point)) 
           (filename "active-shell.sh")) 
      (list filename line-contents cursor))))

(serenade--configure-mode :mode 'shell-mode 
                          :get-editor-state 'serenade--shell/get-editor-state 
                          :diff 'serenade-shell/diff)

(serenade--configure-mode :mode 'rjsx-mode 
                          :post-edit 'js2-reparse)

(provide 'serenade-modes)
