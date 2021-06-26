;; The list of default mappings corresponding to built in Serenade commands. These are exluded from autogenerated js because Serenade already contains the speech patterns. Serenade itself will handle "copy <target>" and "cut <target>", sending a diff message and populating the system clipboard, so these are not included. Serenades built-in "paste" command is also sufficient for sending a diff with the correct result.

(setq serenade--builtin-global-defaults '(;;
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
                                          ( "open file list" . list-buffers) 
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

;; The list of default mappings not corresponding to built in Serenade commands. These are included in autogenerated js because Serenade does not contain the speech patterns.
(setq serenade--generated-global-defaults '(;;
                                         ( "show commands" . serenade-commands  ) 
                                         ( "open buffer <name>" . switch-to-buffer  ) 
                                         ("add bookmark <name>" . bookmark-set) 
                                         ("bookmark <name>" . bookmark-jump) 
                                         ( "serenade log" . serenade-commands-log-open-log  ) 
                                         ( "snippet <name> of <arg>" .
                                           serenade--insert-yasnippet-with-args  )
                                         ( "snippet <name>" . serenade--insert-yasnippet  )))

(setq serenade--selectors '(;;
                            "argument" "list"	"argument"	"assert"	"assignment" "assignment"
                            "value"	"assignment" "variable"	"attribute"	"attribute" "value"
                            "attribute" "name"	"attribute" "text"	"body"	"break"
                            "call"	"case"	"catch"	"class" "close" "tag"	"comment"	"comment"
                            "text"	"condition" "constructor"	"content"	"continue"	"debugger"
                            "declaration"	"decorator"	"default"	"dictionary" "do"	"do" "while"
                            "element"	"else" "if" "else"	"enum"	"except"	"export" "for"
                            "finally"	"function"	"generator" "getter"	"if"	"import"	"interface"
                            "interface" "list"	"keyword" "argument"	"keyword" "parameter"	"key"
                            "entry"	"lambda"	"list"	"method" "mixin"	"modifier"	"modifier"
                            "list"	"name" "named" "parameter"	"named" "parameter" "list"
                            "namespace"	"object" "open" "tag"	"operator"	"parameter" "list"
                            "parameter" "value" "parameter"	"parent" "list"	"parent"	"pass"
                            "positional" "parameter"	"positional" "parameter" "list"	"property"
                            "prototype" "return"	"return" "type"	"return" "value"	"ruleset"
                            "set"	"setter"	"statement"	"string" "string"
                            "text"	"struct"	"switch"	"symbol" "synchronized"	"tag"	"throw"	"top"
                            "level" "statement" "tuple"	"try"	"type"	"type" "alias"
                            "using"	"with"	"with" "list"	"with" "alias" "with" "item"	"while"
                            "value"	"vertical"))

(provide 'serenade-defaults)
