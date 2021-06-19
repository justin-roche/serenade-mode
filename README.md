# Serenade Mode

Serenade mode is a minor mode allowing voice control of Emacs through integration of [Serenade](http://www.serenade.ai). It features:

- Voice-command mapping using Elisp 
- Three layers of speech maps (global, minor mode, and major mode)
- Helm and Helm-M-x integration to aid in discoverability of commands 

## Installation

First, download [Serenade](http://www.serenade.ai) and start the application.

Download the source code here, add the directory to the load path, and call:

```elisp
(use-package serenade-mode)
```

## Usage

To start the the mode call:

```elisp
(serenade-mode)
```

If all goes well, you should see the active application change to "Emacs" in the Serenade applications overlay window.

### Default bindings

Default bindings are those that specify bindings for Serenade's built-in commands. The map for these is found in serenade-commands.el.

```elisp
 '(
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
    ( "scroll" . scroll-up-command  ) 
    ( "scroll down" . scroll-up-command  ) 
    ( "scroll up" . scroll-down-command  ) 
    ( "open file list" . nil) 
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
```


## Customization

### Speech Bindings

To add a speech binding to a voice map, call serenade-define-speech with the map, the speech pattern, and the associated command. If a map does not exist, it will be created.

```elisp

   (serenade-define-speech 'global "treemacs" 'treemacs) 
```

```elisp

   (serenade-define-speech 'org-mode "promote" 'org-do-promote) 
```

To add variables to the speech pattern, enclose them in brackets: 

```elisp

   (serenade-define-speech 'global "open buffer <name>" 'switch-to-buffer) 
```

It is possible to use an alist as the second argument to define-speech:

```elisp
(serenade-define-speech 'org-mode '(("promote" . org-do-promote) 
                                    ("demote" . org-do-demote)))
```
### Variables

#### serenade-mode-filetypes 

This variable specifies filetypes that can be used as serenade buffers, which are buffers subject to Serenade's editing operations. By default this includes all the filetypes Serenade handles, with the addition of Elisp files. 

```elisp
  '("js" "py" "c" "h" "cpp" "cc" "cxx" "c++" "hpp" "hh" "hxx" "h++""cs""css" "scss""dart" "go" "html" "vue" "svelte" "java" "js" "jsx" "jsx" "js""jsx" "js" "kt" "py" "rb" "rs" "scss" "sh" "bash" "ts" "tsx" "tsx" "ts""vue" "html" "el")
```

#### serenade-directory

To manage custom commands, serenade-mode autogenerates javascript stored in the serenade scripts directory. The location of this directory is specfied by serenade directory with the default value:
```elisp
"~/.serenade/scripts/"
```

#### serenade-sync-on-start

This variable specifies whether the autogeneration of cutom javascript

#### serenade-evil

####  serenade-prompt-for-application-start 

#### serenade-enable-double-line-numbers 

#### serenade-helm-M-x

### Hooks

#### serenade-double-line-numbers-on/serenade-double-line-numbers-on

```elisp
(defun serenade--double-line-numbers-on () 
  (global-display-line-numbers-mode +1) 
  (global-nlinum-mode +1) 
  (global-nlinum-relative-mode -1))
  
(defun serenade--double-line-numbers-off () 
  (global-nlinum-mode -1))

  ```
  
## Other functions

### (serenade-helm-commands) 

### (serenade--log-open-log)

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
GNU General Public License

<!-- [MIT](https://choosealicense.com/licenses/mit/) -->
