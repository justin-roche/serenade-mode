https://upload.wikimedia.org/wikipedia/commons/f/f9/Faravahar-Gold.svg
# Serenade Mode

Serenade mode is a minor mode allowing voice control of Emacs through integration of [Serenade](http://www.serenade.ai). It features:

- Voice-command mapping using Elisp 
- Three layers of speech maps (global, minor mode, and major mode)
- Helm and Helm-M-x integration to aid in discoverability of commands 
- Basic Yasnippet integration

## Installation

First, download Serenade and start the application.

Download the serenade-mode source code here, add the serenade-mode directory to your Emacs load path, and call:

```elisp
(use-package serenade-mode)
```

## Usage

To start the the mode call:

```elisp
(serenade-mode)
```

If all goes well, you should see the active application change to Emacs in the Serenade application's overlay window.

### Default voice bindings
Default bindings are those that specify bindings for Serenade's built-in commands. The map for these is found in serenade-commands.el.

| Voice Binding       | Description                  |
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
| open file list    | nil                          |
| style             | nil                          |
| go to definition  | nil                          |
| add breakpoint    | nil                          |
| remove breakpoint | nil                          |
| toggle breakpoint | nil                          |
| start debug       | nil                          |
| start debugging   | nil                          |
| stop debug        | nil                          |
| stop debugging    | nil                          |
| pause debug       | nil                          |
| show hover        | nil                          |
| continue debug    | nil                          |
| step into         | nil                          |
| step out          | nil                          |
| step over         | nil                          |
| continue          | nil                          |

### Additional voice bindings

The __snippet \<name\>__ command inserts a Yasnippet snippet of that name. __snippet \<name\> of \<arg\>__ autofills the first field of the snippet with \<arg\>.

| Voice Binding       | Description                  |
|:--------------------|:-----------------------------|
| show commands | serenade-commands  | 
| serenade log | serenade-commands-log-open-log  | 
| snippet \<name\> of <arg> | serenade--insert-yasnippet-with-args  |
| snippet <\name\> | serenade--insert-yasnippet  |

## Customization

### Speech Bindings

To add a speech binding to a voice map, call __serenade-define-speech__ with the symbol for the map, the speech pattern, and the associated command. If a map does not exist, it will be created. Serenade mode must be restarted for these customizations to take effect.

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

It is possible to use an alist as the second argument to define-speech:

```elisp
(serenade-define-speech 'org-mode '(("promote" . org-do-promote) 
                                    ("demote" . org-do-demote)))
```

To help ensure discoverability, speech maps do not allow lambdas as bound commands. You can instead use the provided currying macro __serc__ within a backquoted list:


```elisp
(serenade-define-speech 'org-mode `(("switch to special buffer" . ,(serc switch-to-buffer "special"))))
                                    
=>

(serenade-define-speech 'org-mode `(("switch to special buffer" . 'serenade-curried->switch-to-buffer->special)))

                                    
```

The currying macro is compatabile with speech pattern variables, which are applied as the final arguments to the curried function.

### Variables

#### serenade-mode-filetypes 

This variable specifies filetypes that can be used as serenade buffers, which are buffers subject to Serenade's editing operations. By default this includes all the filetypes Serenade handles, with the addition of Elisp files. 

```elisp
  '("js" "py" "c" "h" "cpp" "cc" "cxx" "c++" "hpp" "hh" "hxx" "h++""cs""css" "scss""dart" "go" "html" "vue" "svelte" "java" "js" "jsx" "jsx" "js""jsx" "js" "kt" "py" "rb" "rs" "scss" "sh" "bash" "ts" "tsx" "tsx" "ts""vue" "html" "el")
```

#### serenade-directory

To manage custom commands, serenade-mode autogenerates javascript stored in the serenade scripts directory. The location of this directory is specfied by serenade-directory with the default value:

```elisp
"~/.serenade/scripts/"
```

#### serenade-sync-on-start

This variable specifies whether the autogeneration of custom javascript should happen each time the mode is started. Default is true.

#### serenade-evil

This specifies whether certain buffer editing commands integrate with evil or default Emacs editing commands. Default is nil.

#### serenade-helm-M-x

If true, serenade-mode advices helm-M-x so that speech patterns appear beside the keybinding for M-x commands.

### Hooks

#### serenade-double-line-numbers-on/serenade-double-line-numbers-on

For voice coding it can be useful to display both relative and absolute line numbers simultaneously. Associated hooks are provided to allow customization of this operation. These hooks run if serenade--enable-double-line-numbers is true. 

## Other functions

### (serenade-helm-commands) 

This function displays all the currently bound serenade-mode commands in a helm buffer.

### (serenade--log-open-log)

This function displays the log for serenade-mode.

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
GNU General Public License
