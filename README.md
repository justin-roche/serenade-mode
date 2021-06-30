# Serenade Mode

Serenade mode is a minor mode allowing voice-based [structure editing](https://en.wikipedia.org/wiki/Structure_editor) and control of Emacs through integration of [Serenade](http://www.serenade.ai). Serenade-mode features:

- Speech-command mapping using Elisp 
- Configurable speech maps for global, major, and minor modes  
- Helm and Helm-M-x integration to aid in discoverability of commands 
- Basic Yasnippet integration
- Evil integration

## Why another Emacs voice control mode?

Emacs has a long and distinguished history of voice control modes, summarized at [EmacsWiki](https://www.emacswiki.org/emacs/SpeechToText). However, many are no longer accessible, and those that are suffer from one or more the following drawbacks: 

- Dependence on platform dependent speech-recognition engines (VrMode).
- No support for structure editing (Talon, VoiceKey), or support only for combinatorial speech grammars (EmacsListen) 
- Support for structure editing which is tightly coupled with unmaintained or platform-dependent parsers (VoiceCode)

In contrast to these, Serenade mode aims to be a light wrapper allowing interaction with a third-party voice-recognition and structure editing tool, so that the heavy lifting is done outside of Emacs, and Emacs can focus on doing what it does best: providing a fully customizable user interface for text-based workflows. 

The guiding principles for the design of the mode are:

- All speech recognition and structure editing is handled by the third-party tool.
- Robust configurations are made in Elisp, and then saved as Serenade custom commands in the form of autogenerated Javascript.
- The API for user-created commands should as much as possible mirror the Emacs keybindings API.
- The algorithm of mapping speech to user-defined commands should mirror the built-in Emacs algorithm that maps keybindings based on mode priority.
- Speech bindings should be discoverable through helm or other tools.

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

### Command Taxonomy
- __Serenade commands__ are commands entirely handled by Serenade. These are commands like __"add function \<name\>"__ and __"delete second parameter"__. They are documented on the Serenade homepage. 
- __Built-In commands__ are commands sent by Serenade to be handled by the plugin: commands such as __"cut"__ and __"copy"__ are implemented by the mode but are not registered by the mode as new speech patterns. 
- __Generated commands__ are commands which are registered as Serenade "custom commands". This is the class for which you can create a new speech pattern associated with some behavior of Emacs.
- __Default commands__ are either built-in or generated commands included as part as Serenade-mode by default.
- __Custom commands__ are commands you add. 

The implementation of a generated command includes the following components:

- A __speech binding__ is the association of a speech pattern and a command.
- A __speech pattern__ is the speech input, such as "save" or "switch to buffer \<name\>".
- A __command__ is the elisp command associated with a speech pattern, a function such as 'next-buffer. 
- A __speech map__ is a grouping of speech bindings active for a given major or minor mode (the global map is always active).

Multiple speech patterns can be associated with a command (synonyms), but a single speech pattern can be associated with only one command per speech map.

### Built-In default speech bindings
Built-In bindings are those that specify bindings for Serenade's built-in commands. Serenade already recognizes these commands and sends specific messages to the plugin. The map for these is found in serenade-defaults.el. All bindings besides these involve the configuration of javascript in the Serenade directory.

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

### Generated default speech bindings

The __snippet \<name\>__ command inserts a Yasnippet snippet of that name. __snippet \<name\> of \<arg\>__ autofills the first field of the snippet with \<arg\>.

| Pattern | Command                  |
|:--------------------|:-----------------------------|
| show commands | serenade-commands  | 
| serenade log | serenade-commands-log-open-log  | 
| snippet \<name\> of \<arg\> | serenade--insert-yasnippet-with-args  |
| snippet \<name\> | serenade--insert-yasnippet  |

# Customization

## Speech Bindings

To add a speech binding to a speech map, call __serenade-define-speech__ with the symbol for the map, the speech pattern, and the associated command. If a map does not exist, it will be created. Serenade mode must be restarted or the function __serenade-generate__ must be called for these customizations to take effect. It is possible to bind any speech pattern that does not conflict with Serenade commands.

```elisp

   (serenade-define-speech 'global "treemacs" 'treemacs) 
```

```elisp

   (serenade-define-speech 'org-mode "promote" 'org-do-promote) 
```

To add variables to the speech pattern, enclose them in arrow brackets. Arguments are passed in the order specified in the speech pattern. 

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

To inline interactive function calls, use the __seri__ macro, which simply returns a symbol for the interactive invocation of the function:


```elisp

("switch workspace" . ,(seri treemacs-switch-workspace)) 

=>

("switch workspace" . ,(intern-soft 'serenade-interactive->treemacs-switch-workspace)) 
                                    
```

## Mode configuration

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

## Functions

### (serenade-generate) 
Convert custom commands added to serenade speech bindings to javascript.

### (serenade-helm-commands) 

This function displays all the currently bound serenade-mode commands in a helm buffer.

### (serenade-helm-active-commands) 

This function displays all the currently bound and active serenade-mode commands in a helm buffer.

### (serenade-helm-selectors) 

This function displays displays a reference list of serenade selectors in a helm buffer.

### (serenade--log-open-log)

This function displays the log for serenade-mode.

## Contributing
Pull requests, issues, and feature requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

# License
GNU General Public License
