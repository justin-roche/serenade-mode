# Serenade Mode

Serenade mode is a minor mode allowing voice control of Emacs through integration of [Serenade](http://www.serenade.ai), a tool for voice based structural code editing. Serenade-mode features:

- Voice-command mapping using Elisp 
- Configurable speech maps for global, major, and minor modes  
- Helm and Helm-M-x integration to aid in discoverability of commands 
- Basic Yasnippet integration
- Evil integration

The guiding principles for the design of the mode are:

- All speech recognition is handled by the third-party tool
- Robust configurations are made in Elisp, and then saved as autogenerated javascript for the Serenade App
- The API should as much as possible mirror the Emacs keybindings API 
- Speech bindings should be discoverable through helm or other tools

## Installation

First, download Serenade and start the application.

Download the serenade-mode source code here, add the serenade-mode directory to your Emacs load path, and call:

```elisp
(use-package serenade-mode)
```

## Getting Started

To start the the mode call:

```elisp
(serenade-mode)
```

If all goes well, you should see the active application change to Emacs in the Serenade application's overlay window.

### Terminology
- A __speech binding__ is the association of a speech pattern and a command
- A __speech pattern__ is the speech input, such as "save" or "switch to buffer \<name\>"
- A __command__ is the command associated with a speech pattern 
- A __speech map__ is a grouping of speech bindings active for a given major or minor mode (the global map is always active)

Multiple speech patterns can be associated with a command, but a single speech pattern can be associated with only one command.

### Builtin default voice bindings
Builtin bindings are those that specify bindings for Serenade's built-in commands. Serenade already recognizes these commands and sends specific messages to the plugin. The map for these is found in serenade-defaults.el. All bindings besides these involve the configuration of javascript in the Serenade directory.

| Pattern | Command                  |
|:--------------------|:-----------------------------|
| close tab         | delete-window                |
| undo              | serenade--undo               |
| copy              | serenade--copy-selection     |
| cut               | serenade--cut-selection      |
| select <target>   | serenade--select-target      |
| undo              | serenade--undo               |
| redo              | serenade--redo               |
| open <file>       | serenade--open-file          |
| <nth> tab         | serenade--switch-tab         |
| close tab         | delete-window                |
| save              | save-buffer                  |
| create tab        | split-window-right-and-focus |
| next tab          | next-buffer                  |
| previous tab      | previous-buffer              |
| scroll            | scroll-up-command            |
| scroll down       | scroll-up-command            |
| scroll up         | scroll-down-command          |
| open file list    | list-buffers                 |
| style             | nil                          |

### Generated default voice bindings

The __snippet \<name\>__ command inserts a Yasnippet snippet of that name. __snippet \<name\> of \<arg\>__ autofills the first field of the snippet with \<arg\>.

| Pattern | Command                  |
|:--------------------|:-----------------------------|
| show commands | serenade-commands  | 
| serenade log | serenade-commands-log-open-log  | 
| snippet \<name\> of \<arg\> | serenade--insert-yasnippet-with-args  |
| snippet \<name\> | serenade--insert-yasnippet  |

# Customization

## Speech Bindings

To add a speech binding to a speech map, call __serenade-define-speech__ with the symbol for the map, the speech pattern, and the associated command. If a map does not exist, it will be created. Serenade mode must be restarted or the function __serenade-generate__ must be called for these customizations to take effect.

```elisp

   (serenade-define-speech 'global "treemacs" 'treemacs) 
```

```elisp

   (serenade-define-speech 'org-mode "promote" 'org-do-promote) 
```

To add variables to the speech pattern, enclose them in brackets. Arguments are passed in the order specified in the speech pattern. 

```elisp

   (serenade-define-speech 'global "open buffer <name>" 'switch-to-buffer) 
```

To reorder the input of arguments to the command, include __%n__ as an argument transformer in the pattern definition:

```elisp

   (serenade-define-speech 'global "open buffer <%2 name> <%1 direction>" 'another-switch-buffer-fn) 

```

It is possible to use an alist as the second argument to define-speech:

```elisp
(serenade-define-speech 'org-mode '(("promote" . org-do-promote) 
                                    ("demote" . org-do-demote)))
```

The convenience function __serenade-global-set-speech__ has the same signature as define-speech, but applies to the global map only:

```elisp

(serenade-global-set-speech '( ("a" . b) ))

```

## Macros

To help ensure discoverability, speech maps do not allow lambdas as bound commands. You can instead use the provided currying macro __serc__ within a backquoted list, shown here with its macro expansion:


```elisp
(serenade-define-speech 'org-mode `(("switch to special buffer" . ,(serc switch-to-buffer "special"))))
                                    
=>

(serenade-define-speech 'org-mode `(("switch to special buffer" . 'serenade-curried->switch-to-buffer->special)))

                                    
```

The currying macro is compatabile with speech pattern variables, which are applied as the final arguments to the curried function.

There is also the provided __serd__ macro, which acts like a defun call but returns the symbol of the new function, allowing you to inline function definitions in the speech map.


```elisp

(serenade-define-speech 'global `(("a <n>" . ,(serd custom-fn(a) 
                                                    (setq test-val a) )))) 

=>

(serenade-define-speech 'global `(("a <n>" . ,(defun custom-fn
                                                  (a)
                                                (interactive)
                                                (setq test-val a)
                                                (intern-soft 'custom-fn))))) 
                                    
```

### Mode configuration

For some modes it can be useful to specify configurations that are not specific to particular speech bindings. For this there is the function __serenade-configure-mode__. This can be used to expose only a portion of the buffer as editable (as with shell-mode), or for cleanup:  

```elisp

(serenade--configure-mode :mode 'rjsx-mode 
                          :post-edit 'js2-reparse)                                    
```

For more about the mode configuration options, see the documentation in the source code.
## Variables

### serenade-mode-filetypes 

This variable specifies filetypes that can be used as serenade buffers, which are buffers subject to Serenade's editing operations. By default this includes all the filetypes Serenade handles, with the addition of Elisp files. 

```elisp
  '("js" "py" "c" "h" "cpp" "cc" "cxx" "c++" "hpp" "hh" "hxx" "h++""cs""css" "scss""dart" "go" "html" "vue" "svelte" "java" "js" "jsx" "jsx" "js""jsx" "js" "kt" "py" "rb" "rs" "scss" "sh" "bash" "ts" "tsx" "tsx" "ts""vue" "html" "el")
```

### serenade-directory

To manage custom commands, serenade-mode autogenerates javascript stored in the serenade scripts directory. The location of this directory is specfied by serenade-directory with the default value:

```elisp
"~/.serenade/scripts/"
```

### serenade-sync-on-start

This variable specifies whether the autogeneration of custom javascript should happen each time the mode is started. Default is true.

### serenade-evil

This specifies whether certain buffer editing commands integrate with evil or default Emacs editing commands. Default is nil.

### serenade-helm-M-x

If true, serenade-mode advices helm-M-x so that speech patterns appear beside the keybinding for M-x commands.

### serenade--add-builtin-global-defaults 

this variable determines whether the global defaults are added when the mode loads.

### serenade--add-generated-global-defaults 

this variable determines whether the generated global defaults are added when the mode loads.

## Hooks

### serenade-mode-init-hook  

The list of functions to be called after `serenade-mode' has initialized all variables, before connecting fer for the first time. 

### serenade-mode-after-edit-hook 

The list of functions to be called after an edit has been made in response to a speech command. 

### serenade-double-line-numbers-on/serenade-double-line-numbers-on

For voice coding it can be useful to display both relative and absolute line numbers simultaneously. Associated hooks are provided to allow customization of this operation. These hooks run if serenade--enable-double-line-numbers is true. 

# Functions

## (serenade-generate) 
Convert custom commands added to serenade speech bindings to javascript.

## (serenade-helm-commands) 

This function displays all the currently bound serenade-mode commands in a helm buffer.

## (serenade-helm-active-commands) 

This function displays all the currently bound and active serenade-mode commands in a helm buffer.

## (serenade-helm-selectors) 

This function displays displays a reference list of serenade selectors in a helm buffer.

## (serenade--log-open-log)

This function displays the log for serenade-mode.

# Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

# License
GNU General Public License
