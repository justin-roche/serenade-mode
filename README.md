# Serenade Mode



## Installation

[Serenade](http://www.serenade.ai)

```elisp
(use-package serenade-mode)
```

## Usage

```elisp
(serenade-mode)
```


### logging 

### (serenade--log-open-log)

## Customization

### Speech Bindings

```elisp

   (serenade-define-speech 'global "treemacs" 'treemacs) 
```


```elisp

   (serenade-define-speech 'org-mode "promote" 'org-do-promote) 
```

```elisp

   (serenade-define-speech 'global "open buffer <name>" 'switch-to-buffer) 
```

### Variables

#### serenade-mode-filetypes 

```elisp
  '("js" "py" "c" "h" "cpp" "cc" "cxx" "c++" "hpp" "hh" "hxx" "h++""cs""css" "scss""dart" "go" "html" "vue" "svelte" "java" "js" "jsx" "jsx" "js""jsx" "js" "kt" "py" "rb" "rs" "scss" "sh" "bash" "ts" "tsx" "tsx" "ts""vue" "html" "el")
```
  
The filetypes that can be used as serenade buffers, which are buffers subject to the diff operation."

#### serenade-directory

```elisp
"~/.serenade/scripts/"
```

#### serenade-sync-on-start

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

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
GNU General Public License

<!-- [MIT](https://choosealicense.com/licenses/mit/) -->
