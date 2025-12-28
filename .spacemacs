;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. "~/.mycontribs/")
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(vimscript
     ;; Core
     auto-completion
     better-defaults
     emacs-lisp
     helm
     treemacs

     ;; Git (magit + git-gutter)
     git
     (version-control :variables
                      version-control-diff-tool 'git-gutter
                      version-control-global-margin t)

     ;; Languages
     (rust :variables rust-format-on-save t)
     (haskell :variables haskell-completion-backend 'lsp)
     (python :variables
             python-backend 'lsp
             python-formatter 'black
             python-sort-imports-on-save t)
     (latex :variables latex-build-command "LatexMk")
     html
     markdown
     (org :variables
          org-enable-github-support t)

     ;; Tools
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-shell 'vterm)
     ranger
     syntax-checking
     spell-checking
     lsp
     pandoc             ; vim-pandoc equivalent

     ;; Undo tree (like vim-mundo)
     (spacemacs-defaults :variables
                         undo-tree-auto-save-history t))


   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(gruvbox-theme
     evil-args              ; argument text objects (like argtextobj.vim)
     evil-indent-plus       ; indent text objects
     key-chord              ; for jk escape
     ;; Missing plugins from nvim config:
     drag-stuff             ; vim-schlepp - drag visual blocks
     ;; evil-little-word not on MELPA; using subword-mode instead
     evil-owl               ; vim-peekaboo - register preview
     evil-visual-mark-mode  ; vim-signature - marks in gutter
     string-inflection      ; vim-abolish - case coercion (crs, crm, etc)
     evil-lion              ; tabular - align text
     imenu-list             ; tagbar - symbol sidebar
     company-math           ; latex-unicoder - unicode math input
     lorem-ipsum            ; loremipsum
     calfw                  ; calendar.vim
     calfw-org              ; calendar org integration
     highlight-symbol       ; vim-codepainter - highlight words
     posframe               ; dependency for evil-owl popups
     default-text-scale     ; frame-wide font scaling
     )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; Scale factor controls the scaling (size) of the startup banner. Default
   ;; value is `auto' for scaling the logo automatically to fit all buffer
   ;; contents, to a maximum of the full image height and a minimum of 3 line
   ;; heights. If set to a number (int or float) it is used as a constant
   ;; scaling factor for the default logo size.
   dotspacemacs-startup-banner-scale 'auto

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "nerd-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light). A theme from external
   ;; package can be defined with `:package', or a theme can be defined with
   ;; `:location' to download the theme package, refer the themes section in
   ;; DOCUMENTATION.org for the full theme specifications.
   dotspacemacs-themes '(gruvbox-dark-hard
                         gruvbox-light-hard
                         spacemacs-dark)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme 'doom

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. This setting has no effect when
   ;; running Emacs in terminal. The font set here will be used for default and
   ;; fixed-pitch faces. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("Terminus"
                               :size 16.0
                               :weight normal
                               :width normal)

   ;; Default icons font, it can be `all-the-icons' or `nerd-icons'.
   dotspacemacs-default-icons-font 'nerd-icons

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "M-<return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "M-<return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; It is also possible to use a posframe with the following cons cell
   ;; `(posframe . position)' where position can be one of `center',
   ;; `top-center', `bottom-center', `top-left-corner', `top-right-corner',
   ;; `top-right-corner', `bottom-left-corner' or `bottom-right-corner'
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; Make consecutive tab key presses after commands such as
   ;; `spacemacs/alternate-buffer' (SPC TAB) cycle through previous
   ;; buffers/windows/etc. Please see the option's docstring for more information.
   ;; Set the option to t in order to enable cycling for all current and
   ;; future cycling commands. Alternatively, choose a subset of the currently
   ;; supported commands: '(alternate-buffer alternate-window). (default nil)
   dotspacemacs-enable-cycling nil

   ;; Whether side windows (such as those created by treemacs or neotree)
   ;; are kept or minimized by `spacemacs/toggle-maximize-window' (SPC w m).
   ;; (default t)
   dotspacemacs-maximize-window-keep-side-windows t

   ;; If nil, no load-hints enabled. If t, enable the `load-hints' which will
   ;; put the most likely path on the top of `load-path' to reduce walking
   ;; through the whole `load-path'. It's an experimental feature to speedup
   ;; Spacemacs on Windows. Refer the FAQ.org "load-hints" session for details.
   dotspacemacs-enable-load-hints nil

   ;; If t, enable the `package-quickstart' feature to avoid full package
   ;; loading, otherwise no `package-quickstart' attemption (default nil).
   ;; Refer the FAQ.org "package-quickstart" section for details.
   dotspacemacs-enable-package-quickstart nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default t) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' to obtain fullscreen
   ;; without external boxes. Also disables the internal border. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes the
   ;; transparency level of a frame background when it's active or selected. Transparency
   ;; can be toggled through `toggle-background-transparency'. (default 90)
   dotspacemacs-background-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling nil

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers nil

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode t

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server t

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server t

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `ack' and `grep'.
   ;; (default '("rg" "ag" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "ack" "grep")

   ;; The backend used for undo/redo functionality. Possible values are
   ;; `undo-redo', `undo-fu' and `undo-tree' see also `evil-undo-system'.
   ;; Note that saved undo history does not get transferred when changing
   ;; your undo system from or to undo-tree. (default `undo-redo')
   dotspacemacs-undo-system 'undo-tree

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Color highlight trailing whitespace in all prog-mode and text-mode derived
   ;; modes such as c++-mode, python-mode, emacs-lisp, html-mode, rst-mode etc.
   ;; (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; The variable `global-spacemacs-whitespace-cleanup-modes' controls
   ;; which major modes have whitespace cleanup enabled or disabled
   ;; by default.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env)
  )

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  ;; Gruvbox hard contrast (like g:gruvbox_contrast_dark="hard")
  (setq gruvbox-dark-hard-contrast t)
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  ;; ============================================================
  ;; Emoji font support (matching mlterm config)
  ;; ============================================================
  (defun my/set-emoji-fonts ()
    (set-fontset-font t 'unicode "Noto Color Emoji" nil 'prepend)
    (set-fontset-font t '(#x1F300 . #x1F9FF) "Noto Color Emoji" nil 'prepend)
    (set-fontset-font t '(#x2600 . #x26FF) "Noto Color Emoji" nil 'prepend)
    (set-fontset-font t '(#x2700 . #x27BF) "Noto Color Emoji" nil 'prepend))
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'my/set-emoji-fonts)
    (my/set-emoji-fonts))

  ;; Scale emoji font to match Terminus
  (add-to-list 'face-font-rescale-alist '("Noto Color Emoji" . 0.85))

  ;; Tab-bar-mode with M-S-h/l navigation
  (tab-bar-mode 1)
  (global-set-key (kbd "M-H") 'tab-previous)
  (global-set-key (kbd "M-L") 'tab-next)
  (global-set-key (kbd "M-T") 'tab-new)
  (global-set-key (kbd "M-W") 'tab-close)

  ;; M-c to close window (like nvim <A-c> = <C-w>c)
  (global-set-key (kbd "M-c") 'delete-window)

  ;; M-n to open new terminal in tab (like nvim <A-n> = :tabnew term://zsh)
  (defun my/new-terminal-tab ()
    "Open a new tab with a new vterm."
    (interactive)
    (tab-new)
    (let ((buf (generate-new-buffer-name "vterm")))
      (vterm buf)))
  (global-set-key (kbd "M-n") #'my/new-terminal-tab)

  ;; Make tab/window nav work in vterm
  (with-eval-after-load 'vterm
    (define-key vterm-mode-map (kbd "M-H") #'tab-previous)
    (define-key vterm-mode-map (kbd "M-L") #'tab-next)
    (define-key vterm-mode-map (kbd "M-T") #'tab-new)
    (define-key vterm-mode-map (kbd "M-W") #'tab-close)
    (define-key vterm-mode-map (kbd "M-n") #'my/new-terminal-tab)
    (define-key vterm-mode-map (kbd "M-c") #'delete-window)
    (define-key vterm-mode-map (kbd "M-h") #'evil-window-left)
    (define-key vterm-mode-map (kbd "M-j") #'evil-window-down)
    (define-key vterm-mode-map (kbd "M-k") #'evil-window-up)
    (define-key vterm-mode-map (kbd "M-l") #'evil-window-right))

  ;; Install nerd-icons fonts for modeline
  (use-package nerd-icons
    :config
    (unless (member "Symbols Nerd Font Mono" (font-family-list))
      (nerd-icons-install-fonts t)))

  ;; ============================================================
  ;; Zoom frame font size (frame-wide, not buffer-local)
  ;; ============================================================
  (use-package default-text-scale
    :config
    (default-text-scale-mode 1)
    (global-set-key (kbd "s-=") 'default-text-scale-increase)
    (global-set-key (kbd "s--") 'default-text-scale-decrease)
    (global-set-key (kbd "s-0") 'default-text-scale-reset))

  ;; ============================================================
  ;; Basic Settings (matching init.vim)
  ;; ============================================================
  (setq-default
   tab-width 2
   indent-tabs-mode nil              ; expandtab
   evil-shift-width 2                ; shiftwidth=2
   require-final-newline t)          ; add newlines to ends of files

  ;; Persistent undo (like undofile)
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
  (unless (file-exists-p "~/.emacs.d/undo")
    (make-directory "~/.emacs.d/undo" t))

  ;; Show whitespace characters (like listchars)
  (setq whitespace-style '(face tabs tab-mark trailing))
  (global-whitespace-mode t)

  ;; ============================================================
  ;; jk to escape (like Arpeggio jk)
  ;; ============================================================
  (use-package key-chord
    :config
    (key-chord-mode 1)
    (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
    (key-chord-define evil-insert-state-map "kj" 'evil-normal-state))

  ;; ============================================================
  ;; Keybindings matching init.vim
  ;; ============================================================

  ;; Y to yank to end of line (like Y y$)
  (define-key evil-normal-state-map "Y" (kbd "y$"))

  ;; Enter for command line in normal mode
  (define-key evil-normal-state-map (kbd "RET") 'evil-ex)

  ;; Visual mode < > reselect after indent
  (define-key evil-visual-state-map "<" (lambda ()
                                          (interactive)
                                          (evil-shift-left (region-beginning) (region-end))
                                          (evil-normal-state)
                                          (evil-visual-restore)))
  (define-key evil-visual-state-map ">" (lambda ()
                                          (interactive)
                                          (evil-shift-right (region-beginning) (region-end))
                                          (evil-normal-state)
                                          (evil-visual-restore)))

  ;; Alt+hjkl for window navigation (like nvim)
  (define-key evil-normal-state-map (kbd "M-h") 'evil-window-left)
  (define-key evil-normal-state-map (kbd "M-j") 'evil-window-down)
  (define-key evil-normal-state-map (kbd "M-k") 'evil-window-up)
  (define-key evil-normal-state-map (kbd "M-l") 'evil-window-right)
  (define-key evil-insert-state-map (kbd "M-h") 'evil-window-left)
  (define-key evil-insert-state-map (kbd "M-j") 'evil-window-down)
  (define-key evil-insert-state-map (kbd "M-k") 'evil-window-up)
  (define-key evil-insert-state-map (kbd "M-l") 'evil-window-right)

  ;; ============================================================
  ;; Leader key mappings (SPC prefix) matching init.vim
  ;; Use SPC o prefix for user bindings to avoid conflicts
  ;; ============================================================

  ;; Clipboard: use "+y and "+p in evil, or SPC o y/p
  (spacemacs/set-leader-keys
    "oy" 'spacemacs/copy-to-clipboard
    "op" 'spacemacs/paste-from-clipboard)

  ;; Save: SPC o w (Spacemacs native: SPC f s)
  (spacemacs/set-leader-keys "ow" 'save-buffer)

  ;; SPC o u for undo-tree visualize (like <Leader>u for Mundo)
  (spacemacs/set-leader-keys "ou" 'undo-tree-visualize)

  ;; Edit config: SPC f e d is Spacemacs native

  ;; SPC o c for toggle cursor crosshairs
  (spacemacs/set-leader-keys "oc" 'crosshairs-mode)

  ;; SPC RET for clear search highlight (like <leader><cr>)
  (spacemacs/set-leader-keys "RET" 'evil-search-highlight-persist-remove-all)

  ;; Spell: SPC t S in spacemacs

  ;; ============================================================
  ;; Text objects (matching vim plugins)
  ;; ============================================================

  ;; evil-args for argument text objects (ia, aa)
  (use-package evil-args
    :config
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

  ;; evil-indent-plus for indent text objects
  (use-package evil-indent-plus
    :config
    (evil-indent-plus-default-bindings))

  ;; i, / a, as aliases for argument text objects (vim-swap style)
  (define-key evil-inner-text-objects-map "," 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "," 'evil-outer-arg)

  ;; ============================================================
  ;; Clause text objects (ix / ax) and motions ([x / ]x)
  ;; Ported from init.vim - selects text between punctuation
  ;; ============================================================
  (evil-define-text-object evil-inner-clause (count &optional beg end type)
    "Select inner clause (between punctuation, excluding punctuation)."
    (let* ((punct "[,;:.?!()]")
           (start (save-excursion
                    (if (re-search-backward punct nil t)
                        (progn (forward-char 1) (skip-chars-forward " \t\n") (point))
                      (point-min))))
           (end (save-excursion
                  (if (re-search-forward punct nil t)
                      (progn (backward-char 1) (skip-chars-backward " \t\n") (1+ (point)))
                    (point-max)))))
      (evil-range start end)))

  (evil-define-text-object evil-outer-clause (count &optional beg end type)
    "Select outer clause (between punctuation, including trailing punctuation)."
    (let* ((punct "[,;:.?!()]")
           (start (save-excursion
                    (if (re-search-backward punct nil t)
                        (point)
                      (point-min))))
           (end (save-excursion
                  (if (re-search-forward punct nil t)
                      (point)
                    (point-max)))))
      (evil-range start end)))

  (define-key evil-inner-text-objects-map "x" 'evil-inner-clause)
  (define-key evil-outer-text-objects-map "x" 'evil-outer-clause)

  ;; Clause motions [x / ]x
  (evil-define-motion evil-forward-clause (count)
    "Move forward to next clause (after punctuation)."
    :type exclusive
    (let ((punct "[,;:.?!()]"))
      (dotimes (_ (or count 1))
        (when (re-search-forward punct nil t)
          (skip-chars-forward " \t\n")))))

  (evil-define-motion evil-backward-clause (count)
    "Move backward to previous clause."
    :type exclusive
    (let ((punct "[,;:.?!()]"))
      (dotimes (_ (or count 1))
        (when (re-search-backward punct nil t)
          (skip-chars-backward " \t\n")))))

  (define-key evil-motion-state-map "]x" 'evil-forward-clause)
  (define-key evil-motion-state-map "[x" 'evil-backward-clause)

  ;; ============================================================
  ;; Org mode settings (matching orgmode config)
  ;; ============================================================
  (with-eval-after-load 'org
    (setq org-agenda-files '("~/org/")))

  ;; ============================================================
  ;; LaTeX settings (matching vimtex)
  ;; ============================================================
  (setq TeX-engine 'default)

  ;; ============================================================
  ;; drag-stuff (vim-schlepp equivalent)
  ;; ============================================================
  (use-package drag-stuff
    :config
    (drag-stuff-global-mode 1)
    ;; Arrow keys in visual mode to drag selection (like vim-schlepp)
    (define-key evil-visual-state-map (kbd "<up>") 'drag-stuff-up)
    (define-key evil-visual-state-map (kbd "<down>") 'drag-stuff-down)
    (define-key evil-visual-state-map (kbd "<left>") 'drag-stuff-left)
    (define-key evil-visual-state-map (kbd "<right>") 'drag-stuff-right))

  ;; ============================================================
  ;; evil-little-word (camelcasemotion equivalent)
  ;; NOTE: Package not available on MELPA. Use subword-mode instead:
  ;; ============================================================
  (global-subword-mode 1)  ; Makes M-f, M-b, etc. stop at camelCase boundaries

  ;; ============================================================
  ;; evil-owl (vim-peekaboo equivalent)
  ;; ============================================================
  (use-package evil-owl
    :config
    (setq evil-owl-display-method 'posframe
          evil-owl-extra-posframe-args '(:width 60 :height 20)
          evil-owl-idle-delay 0.3)
    (evil-owl-mode 1))

  ;; ============================================================
  ;; evil-visual-mark-mode (vim-signature equivalent)
  ;; ============================================================
  (use-package evil-visual-mark-mode
    :config
    (evil-visual-mark-mode 1))

  ;; ============================================================
  ;; string-inflection (vim-abolish coercion equivalent)
  ;; ============================================================
  (use-package string-inflection
    :config
    ;; Mimic vim-abolish coercion: cr + s/m/c/u/k/-/_
    (spacemacs/set-leader-keys
      "xis" 'string-inflection-underscore        ; crs - snake_case
      "xic" 'string-inflection-lower-camelcase   ; crc - camelCase
      "xim" 'string-inflection-camelcase         ; crm - MixedCase/PascalCase
      "xiu" 'string-inflection-upcase            ; cru - UPPER_CASE
      "xik" 'string-inflection-kebab-case        ; cr- - kebab-case
      "xii" 'string-inflection-cycle))           ; cycle through all

  ;; ============================================================
  ;; evil-lion (tabular/vim-easy-align equivalent)
  ;; ============================================================
  (use-package evil-lion
    :config
    (evil-lion-mode)
    ;; gl<textobj><char> to align, e.g., glip= aligns paragraph on =
    )

  ;; ============================================================
  ;; imenu-list (tagbar equivalent)
  ;; ============================================================
  (use-package imenu-list
    :config
    (setq imenu-list-focus-after-activation t
          imenu-list-auto-resize t)
    (spacemacs/set-leader-keys "bi" 'imenu-list-smart-toggle))

  ;; ============================================================
  ;; company-math (latex-unicoder equivalent)
  ;; ============================================================
  (use-package company-math
    :config
    (add-to-list 'company-backends 'company-math-symbols-unicode)
    (add-to-list 'company-backends 'company-math-symbols-latex))

  ;; ============================================================
  ;; lorem-ipsum
  ;; ============================================================
  (use-package lorem-ipsum
    :commands (lorem-ipsum-insert-paragraphs
               lorem-ipsum-insert-sentences
               lorem-ipsum-insert-list)
    :config
    (spacemacs/set-leader-keys
      "ilp" 'lorem-ipsum-insert-paragraphs
      "ils" 'lorem-ipsum-insert-sentences
      "ill" 'lorem-ipsum-insert-list))

  ;; ============================================================
  ;; calfw (calendar.vim equivalent)
  ;; ============================================================
  (use-package calfw
    :commands cfw:open-calendar-buffer
    :config
    (use-package calfw-org))
  (spacemacs/set-leader-keys "aC" 'cfw:open-org-calendar)

  ;; ============================================================
  ;; highlight-symbol (vim-codepainter equivalent)
  ;; ============================================================
  (use-package highlight-symbol
    :config
    (spacemacs/set-leader-keys
      "sH" 'highlight-symbol-remove-all
      "sn" 'highlight-symbol-next
      "sN" 'highlight-symbol-prev))

  ;; Smart highlight - word at point or visual selection
  (defvar my/hi-lock-face-index 0 "Current index into hi-lock faces.")
  (defvar my/hi-lock-faces
    '(hi-yellow hi-pink hi-green hi-blue hi-salmon hi-aquamarine)
    "Faces to cycle through for highlighting.")

  (defun my/highlight-dwim ()
    "Highlight visual selection or symbol at point, cycling colors."
    (interactive)
    (if (use-region-p)
        (let ((regexp (regexp-quote (buffer-substring-no-properties
                                      (region-beginning) (region-end))))
              (face (nth my/hi-lock-face-index my/hi-lock-faces)))
          (hi-lock-face-buffer regexp face)
          (setq my/hi-lock-face-index
                (mod (1+ my/hi-lock-face-index) (length my/hi-lock-faces)))
          (deactivate-mark))
      (highlight-symbol)))
  (spacemacs/set-leader-keys "sh" 'my/highlight-dwim)
  (spacemacs/set-leader-keys "sV" 'hi-lock-unface-buffer)

  ;; ============================================================
  ;; Eshell visual commands (run in term-mode)
  ;; ============================================================
  (with-eval-after-load 'eshell
    (add-to-list 'eshell-visual-commands "claude")
    (add-to-list 'eshell-visual-commands "ranger"))

  )


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ace-link aggressive-indent all-the-icons attrap auctex-latexmk auto-compile
              auto-highlight-symbol auto-yasnippet avy-jump-helm-line blacken
              browse-at-remote calfw-org centered-cursor-mode clean-aindent-mode
              cmm-mode code-cells code-review column-enforce-mode company-auctex
              company-cabal company-math company-reftex company-web cython-mode
              dactyl-mode dante define-word devdocs diminish dired-quick-sort
              disable-mouse doom-modeline dotenv-mode drag-stuff dumb-jump eat
              edit-indirect elisp-def elisp-demos elisp-slime-nav emmet-mode emr
              esh-help eshell-prompt-extras eshell-z eval-sexp-fu evil-anzu
              evil-args evil-cleverparens evil-collection evil-easymotion
              evil-escape evil-evilified-state evil-exchange evil-goggles
              evil-iedit-state evil-indent-plus evil-lion evil-lisp-state
              evil-matchit evil-mc evil-nerd-commenter evil-numbers evil-org
              evil-owl evil-surround evil-tex evil-textobj-line evil-tutor
              evil-unimpaired evil-visual-mark-mode evil-visualstar
              expand-region eyebrowse fancy-battery flycheck-elsa
              flycheck-haskell flycheck-package flycheck-pos-tip
              flyspell-correct-helm gh-md git-gutter-fringe git-link
              git-messenger git-modes git-timemachine gitignore-templates
              gnuplot golden-ratio google-translate gruvbox-theme
              haskell-snippets helm-ag helm-c-yasnippet helm-comint helm-company
              helm-css-scss helm-descbinds helm-hoogle helm-ls-git helm-lsp
              helm-make helm-mode-manager helm-org helm-org-rifle
              helm-projectile helm-purpose helm-pydoc helm-swoop helm-xref
              hide-comnt highlight-indentation highlight-numbers
              highlight-parentheses highlight-symbol hl-todo hlint-refactor
              holy-mode hungry-delete hybrid-mode impatient-mode indent-guide
              info+ inspector key-chord link-hint live-py-mode lorem-ipsum
              lsp-haskell lsp-latex lsp-origami lsp-treemacs lsp-ui macrostep
              markdown-toc multi-line multi-term multi-vterm mwim nameless
              nerd-icons open-junk-file org-cliplink org-contrib org-download
              org-mime org-pomodoro org-present org-projectile org-rich-yank
              org-superstar orgit-forge overseer ox-gfm ox-pandoc
              page-break-lines pandoc-mode paradox password-generator pcre2el
              pip-requirements pipenv pippel poetry popwin prettier-js pug-mode
              py-isort pydoc pyenv-mode pylookup python-pytest quickrun
              rainbow-delimiters ranger restart-emacs ron-mode rustic sass-mode
              scss-mode shell-pop shrink-path slim-mode smeargle space-doc
              spaceline spacemacs-purpose-popwin spacemacs-whitespace-cleanup
              sphinx-doc string-edit-at-point string-inflection symbol-overlay
              symon tagedit term-cursor terminal-here toc-org toml-mode
              treemacs-evil treemacs-icons-dired treemacs-magit treemacs-persp
              treemacs-projectile undo-tree unfill vi-tilde-fringe vimrc-mode
              volatile-highlights web-beautify web-mode wgrep winum
              writeroom-mode ws-butler yasnippet-snippets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
