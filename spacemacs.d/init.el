;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers '(
        auto-completion
        ansible
        (colors :variables
                colors-enable-nyan-cat-progress-bar t
                colors-enable-rainbow-identifiers t)
        clojure
        dash
        deft
        docker
        emacs-lisp
        elixir
        elm
        fasd
        (git :variables
             git-gutter-use-fringe t)
        github
        ;; magit-gh-issues
        html
        javascript
        jenkins
        markdown
        org
        osx
        restclient
        ruby
        (scala :variables
               flycheck-scalastyle-jar "/usr/local/bin/scalastyle")
        (shell :variables shell-default-shell 'eshell)
        slack
        spotify
        sql
        syntax-checking
        version-control
        ;; (wakatime :variables
        ;;           wakatime-api-key "d405f27b-007d-411f-9e0f-e1e1545c9d6b"
        ;;           wakatime-cli-path "/usr/local/Cellar/wakatime-cli/6.0.9/libexec/lib/python2.7/site-packages/wakatime/cli.py")
        )

   ;; List of additional packages that will be installed wihout being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '()

   ;; A list of packages and/or extensions that will not be install and loaded.
   ;; dotspacemacs-excluded-packages '()
   ;; ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to a .PNG file.
   ;; If the value is nil then no banner is displayed.
   ;; dotspacemacs-startup-banner 'official
   dotspacemacs-startup-banner nil
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(gotham
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Pragmata Pro"
                               :size 19
                               :weight normal
                               :width expanded
                               :powerline-scale 1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil the paste micro-state is enabled. While enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil ;; to boost the loading time.
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 95
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 95
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil)
  ;; User initialization goes here
  (setq-default ruby-version-manager 'rvm)
  (define-key global-map (kbd "C-+") 'text-scale-increase)
  (define-key global-map (kbd "C--") 'text-scale-decrease)
  )

(defun dotspacemacs/user-init ()
  (push '("melpa-stable" . "stable.melpa.org/packages/") configuration-layer--elpa-archives)
  (push '(ensime . "melpa-stable") package-pinned-packages))

(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (setq powerline-default-separator 'slant)
  (global-linum-mode t)
  (linum-relative-toggle)
  (global-prettify-symbols-mode t)
  (add-hook 'hack-local-variables-hook (lambda () (setq truncate-lines t)))
  (setq-default line-spacing 6)
  (golden-ratio-mode)
  ;; (global-wakatime-mode)
  (indent-guide-global-mode)
  ;;enable transparency by default
  (spacemacs/toggle-transparency)

  ;; Keep server alive
  (setq-default dotspacemacs-persistent-server t)

  ;; Make screen grey when using avy
  (setq avy-background t)
  (setq avy-highlight-first nil)
  (setq avy-all-windows t)

  ;; ruby mode
  (setq-default ruby-version-manager 'rvm)

  ;; deft notes directory
  (setq deft-directory "~/Dropbox/Documents/notes")
  (setq deft-recursive t)

  ;; private config
  (add-to-list 'load-path "~/.spacemacs.d/peel/")
  ;; general
  (require 'peel)
  (peel/init)

  ;; scala-related
  (evil-leader/set-key "n s" 'ensime)
  (setq ensime-startup-snapshot-notification nil)
  ;; sbt-worksheet-like experience
  (add-to-list 'load-path "~/.spacemacs.d/sbt-worksheet-mode/")
  (require 'sbt-worksheet-mode)
  (autoload 'sbt-worksheet-mode "sbt-worksheet-mode")

  ;; blogging
  (add-to-list 'load-path "~/.spacemacs.d/ox-jekyll-subtree/")
  (require 'ox-jekyll-subtree)
  (autoload 'endless/export-to-blog "ox-jekyll-subtree")
  (setq org-jekyll-use-src-plugin t)
  (setq endless/blog-base-url "https://codearsonist.com/")
  (setq endless/blog-dir (expand-file-name "~/wrk/blog/"))

  ;; enable ligatures
  (mac-auto-operator-composition-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (minimap zonokai-theme zenburn-theme zen-and-art-theme underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme seti-theme reverse-theme railscasts-theme purple-haze-theme professional-theme planet-theme phoenix-dark-pink-theme phoenix-dark-mono-theme pastels-on-dark-theme organic-green-theme omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme naquadah-theme mustang-theme monokai-theme monochrome-theme molokai-theme moe-theme minimal-theme material-theme majapahit-theme lush-theme light-soap-theme jbeans-theme jazz-theme ir-black-theme inkpot-theme heroku-theme hemisu-theme hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gandalf-theme flatui-theme flatland-theme firebelly-theme farmhouse-theme espresso-theme dracula-theme django-theme darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme cyberpunk-theme color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme cherry-blossom-theme busybee-theme bubbleberry-theme birds-of-paradise-plus-theme badwolf-theme apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes afternoon-theme dichromancy-theme yaml-mode jenkins wakatime-mode spotify helm-spotify multi ob-elixir minitest hide-comnt seq restclient ranger pdf-tools ob-http docker tablist docker-tramp clojure-snippets clj-refactor inflections edn paredit peg cider-eval-sexp-fu cider queue clojure-mode slack emojify circe oauth2 websocket sql-indent org goto-chg undo-tree diminish uuidgen rake pug-mode osx-dictionary org-projectile org-download livid-mode skewer-mode simple-httpd link-hint jinja2-mode github-search flycheck-mix eyebrowse evil-visual-mark-mode evil-unimpaired evil-ediff eshell-z dumb-jump column-enforce-mode color-identifiers-mode xterm-color ws-butler window-numbering web-mode web-beautify volatile-highlights vi-tilde-fringe toc-org tagedit speed-type spacemacs-theme spaceline powerline smooth-scrolling smeargle slim-mode shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode ruby-end rubocop rspec-mode robe reveal-in-osx-finder restart-emacs rbenv rainbow-mode rainbow-identifiers rainbow-delimiters popwin persp-mode pcre2el pbcopy paradox hydra spinner page-break-lines osx-trash orgit org-repo-todo org-present org-pomodoro alert log4e gntp org-plus-contrib org-bullets open-junk-file noflet neotree multi-term move-text mmm-mode markdown-toc markdown-mode magit-gitflow magit-gh-pulls macrostep lorem-ipsum linum-relative leuven-theme less-css-mode launchctl json-mode json-snatcher json-reformat js2-refactor multiple-cursors js2-mode js-doc jade-mode info+ indent-guide ido-vertical-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation help-fns+ helm-themes helm-swoop helm-projectile helm-mode-manager helm-make projectile helm-gitignore request helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag haml-mode google-translate golden-ratio gnuplot gitignore-mode github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gist gh marshal logito pcache ht gh-md flycheck-pos-tip flycheck-elm flycheck flx-ido flx floobits fill-column-indicator fasd grizzl fancy-battery expand-region exec-path-from-shell evil-visualstar evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit magit magit-popup git-commit with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-args evil-anzu anzu eval-sexp-fu highlight eshell-prompt-extras esh-help ensime sbt-mode scala-mode emmet-mode elm-mode elisp-slime-nav dockerfile-mode diff-hl deft define-word dash-at-point company-web web-completion-data company-tern dash-functional tern company-statistics company-quickhelp pos-tip coffee-mode clean-aindent-mode chruby bundler inf-ruby buffer-move bracketed-paste auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed ansible-doc ansible f s alchemist company dash elixir-mode pkg-info epl aggressive-indent adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup quelpa package-build use-package which-key bind-key bind-map evil gotham-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
