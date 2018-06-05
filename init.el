;; -*- no-byte-compile: t; -*-
;;; init.el --- description -*- lexical-binding: t; -*-

(doom! :feature
       ;debugger          ; FIXME stepping through code, to help you add bugs
       eval              ; run code, run (also, repls)
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       (lookup           ; helps you navigate your code and documentation
        +devdocs         ; ...on devdocs.io online
        +docsets)        ; ...or in Dash docsets locally
       ;services          ; TODO managing external services & code builders
       snippets          ; my elves. They type so I don't have to
       spellcheck        ; tasing you for misspelling mispelling
       syntax-checker    ; tasing you for every semicolon you forget
       version-control   ; remember, remember that commit in November
       workspaces        ; tab emulation, persistence & separate workspaces

       :completion
       (company +childframe +auto) ; the ultimate code completion backend
       ivy                ; a search engine for love and life
       ;helm              ; the *other* search engine for love and life
       ;ido               ; the other *other* search engine...

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-modeline     ; a snazzy Atom-inspired mode-line
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       nav-flash         ; blink the current line after jumping
       evil-goggles      ; display visual hints when editing in evil
      ;unicode           ; extended unicode support for various languages
      ;tabbar            ; FIXME an (incomplete) tab bar for Emacs
       neotree           ; a project drawer, like NERDTree for vim
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows

       :emacs
       dired             ; making dired pretty [functional]
       electric-indent   ; smarter, keyword-based electric-indent
       eshell            ; a consistent, cross-platform shell (WIP)
       imenu             ; an imenu sidebar and searchable code index
       term              ; terminals in Emacs

       :tools
       editorconfig      ; let someone else argue about tabs vs spaces
       gist              ; interacting with github gists
       macos             ; MacOS-specific commands
       make              ; run make tasks from Emacs
       magit             ;
       (password-store +auth) ; password manager for nerds
       pdf               ; pdf enhancements
       rotate-text       ; cycle region at point between text candidates
       tmux              ; an API for interacting with tmux
       ;upload            ; map local to remote projects via ssh/ftp

       :lang
       assembly          ; assembly for fun or debugging
       (cc +irony)       ; C/C++/Obj-C madness
      ;crystal           ; ruby at the speed of c
       clojure           ; java with a lisp
       ;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       elixir            ; erlang done right
       elm               ; care for a cup of TEA?
       erlang            ;
       emacs-lisp        ; drown in parentheses
       ;ess               ; emacs speaks statistics
       go                ; the hipster dialect
      ;(haskell +intero) ; a language that's lazier than I am
       ;hy                ; readability of scheme w/ speed of python
       ;(java +meghanada) ; the poster child for carpal tunnel syndrome
       javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;julia             ; a better, faster MATLAB
       (latex             ; writing papers in Emacs has never been so fun
        +latexmk
        +pdf-tools
        +preview-pane)
       ;ledger            ; an accounting system in Emacs
       lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;ocaml             ; an objective camel
       (org              ; organize your plain life in plain text
        +attach          ; custom attachment system
        +babel           ; running code in org
        +capture         ; org-capture in and outside of Emacs
        +export          ; Exporting org to whatever you want
        +present         ; Emacs for presentations
        +publish)        ; Emacs+Org as a static site generator
       perl              ; write code no one else can comprehend
       ;php               ; perl's insecure younger brother
       plantuml          ; diagrams for confusing people more
       ;purescript        ; javascript, but functional
       python            ; beautiful is better than ugly
       rest              ; Emacs as a REST client
       ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;scala             ; java, but good
       sh                ; she sells (ba|z)sh shells on the C xor
       ;swift             ; who asked for emoji variables?
       web               ; the tubes

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
       ;(email +gmail)    ; emacs as an email client
      irc                 ; how neckbeards socialize
      ;(rss +org)        ; emacs as an RSS reader
      twitter           ; twitter client https://twitter.com/vnought
      ;(write            ; emacs as a word processor (latex + org + markdown)
      ; +wordnut         ; wordnet (wn) search
      ; +langtool)       ; a proofreader (grammar/style check) for Emacs

      :config
      (default +bindings +snippets +evil-commands))

(provide 'init)

;;; init.el ends here
