;;;; init.el --- Peel's Essential Emacs Lisp

;;; Commentary:
;;; Work in progress.  An attempt for a clean start of nix-managed config.

;;; Code:
;;; ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

;; package.el ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(require 'package)
(package-initialize 'noactivate)

(eval-when-compile
  (require 'use-package))

(use-package diminish
  :ensure t
  :init (require 'diminish))

(use-package exec-path-from-shell
    :ensure t
    :custom
    (exec-path-from-shell-variables
     '("PATH"
       "SHELL"
       "NIX_PATH"
       "NIX_PROFILES"
       "NIX_REMOTE"
       "NIX_SSL_CERT_FILE"
       "NIX_USER_PROFILE_DIR"
       "JAVA_HOME"
       ))
    :config
    (when (eq system-type 'darwin) (exec-path-from-shell-initialize)))

;; navigation ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(use-package winner
  :ensure nil
  :config (winner-mode 1)
  :init
  (defvar peel--window-configuration nil
    "Store the window configuration before zooming.")

  (defun peel--toggle-window-zoom ()
    "Toggle between zoomed window and previous window configuration."
    (interactive)
    (if peel--window-configuration
        (progn
          (set-window-configuration peel--window-configuration)
          (setq peel--window-configuration nil))
      (setq peel--window-configuration (current-window-configuration))
      (delete-other-windows)))

  (global-set-key (kbd "s-1") 'peel--toggle-window-zoom))

;; pragmata pro ui borders ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(use-package window
  :ensure nil
  :init
  (defun my/setup-window-borders ()
    "Setup window borders with Pragmata Pro box drawing characters."
    ;; Only set window border characters, don't touch global display table
    (set-face-attribute 'vertical-border nil :foreground "#404040" :background nil)
    (set-face-attribute 'window-divider nil :foreground "#404040" :background nil)
    (set-face-attribute 'window-divider-first-pixel nil :foreground "#404040" :background nil)
    (set-face-attribute 'window-divider-last-pixel nil :foreground "#404040" :background nil)
    (unless standard-display-table
      (setq standard-display-table (make-display-table)))
    (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?\u2502)))
  :hook (after-init . my/setup-window-borders)
  :custom
  (window-divider-default-bottom-width 1)
  (window-divider-default-right-width 1)
  (window-divider-default-places 'right-only)
  (window-resize-pixelwise nil)
  (frame-resize-pixelwise nil)
  :config
  (window-divider-mode 1)
  (unless window-system
    (require 'mouse)
    (xterm-mouse-mode t)
    (global-set-key [mouse-4] 'scroll-down-line)
    (global-set-key [mouse-5] 'scroll-up-line))

  ;; Better mouse wheel scrolling
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
  (setq mouse-wheel-progressive-speed nil)
  (setq mouse-wheel-follow-mouse 't)

  ;; Enable track-mouse for better mouse support
  (setq track-mouse t))

;; ivy ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(use-package vertico
  :ensure t
  :bind (:map vertico-map
         ("C-j" . vertico-next)
         ("C-k" . vertico-previous)
         ("C-f" . vertico-exit)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

(use-package savehist
  :init
  (savehist-mode))

(use-package marginalia
  :after vertico
  :ensure t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  (marginalia-separator " \u2502 ")
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))
  (completion-category-overrides nil))

(use-package corfu
  :ensure t
  :bind (:map corfu-map
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous))
  :custom
  (corfu-separator ?\u2502)
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-commit-predicate nil)
  (corfu-cycle t)
  (corfu-quit-no-match nil)
  :init (global-corfu-mode))

(use-package consult
  :ensure t
  :bind (("C-x C-b" . consult-buffer)
         ("C-s"     . consult-line)
         ("C-c C-r" . consult-ripgrep)
         ("C-c f"   . consult-flymake)
         ("M-g i"   . consult-imenu)
         ([remap switch-to-buffer] . consult-buffer))
  :config
  (recentf-mode +1)
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  (consult-async-input-debounce 0.1)
  (consult-async-input-throttle 0.2)
  (consult-async-refresh-delay  0.15)
  (consult-line-numbers-widen t)
  (consult-narrow-key "<"))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


;; git ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(use-package magit
  :ensure t
  :if (executable-find "git")
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-dispatch))
  :custom
  (magit-completing-read-function 'magit-builtin-completing-read "Use vertico")
  (defun peel/change-commit-author (arg)
    "Change the commit author during an interactive rebase in Magit.
With a prefix argument, insert a new change commit author command
even when there is already another rebase command on the current
line.  With empty input, remove the change commit author action
on the current line, if any."
    (interactive "P")
    (let ((author
           (magit-transient-read-person "Select a new author for this commit"
                                        nil
                                        nil)))
      (git-rebase-set-noncommit-action
       "exec"
       (lambda (_) (if author
                  (format "git commit --amend --author='%s'" author)
                ""))
       arg)))
  (define-key git-rebase-mode-map (kbd "h") #'peek/change-commit-author))

(use-package git-link
  :ensure t
  :after magit
  :commands (git-link git-link-commit git-link-homepage)
  :bind (("C-c C-g l" . git-link)
         ("C-c C-g c" . git-link-commit)
         ("C-c C-g h" . git-link-homepage)))

(use-package forge
  :ensure t)

;; ui ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

;; files ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)
  (dired-recursive-deletes t)
  (dired-use-ls-dired nil)
  (delete-by-moving-to-trash t))

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :bind ("C-x C-n" . dired-sidebar-toggle-sidebar)
  :custom 
  (dired-sidebar-subtree-line-prefix "\u251c\u2500"))

;; bindings ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

;; ....................................................................... popup
(use-package which-key
  :ensure t
  :defer 1
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0
        which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-add-key-based-replacements
   "C-c f" "consult-flymake"
   "C-c i" "unicode")
  (setq which-key-separator " \u2502 ")
  (which-key-mode))


;; ....................................................................... setup

;; languages ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

;; ..................................................................... generic
;; indentation
(setq-default indent-tabs-mode nil
	            tab-width 2
	            fill-column 80)
;; rainbow
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :ensure t
  :hook (prog-mode . rainbow-identifiers-mode))

(use-package diff-hl
  :ensure t
  :hook (prog-mode . diff-hl-mode)
  :diminish diff-hl-mode)

(use-package dumb-jump
  :ensure t
  :bind ("s-." . dumb-jump-go))

(use-package puni
  :defer t
  :ensure t
  :bind (:map puni-mode-map
              ("M-s" . puni-splice)
              ("C-|" . puni-split)
              ("C-<right>" . puni-slurp-forward)
              ("C-<left>" . puni-barf-forward)
              ("C-M-<right>" . puni-barf-backward)
              ("C-M-<left>" . puni-slurp-backward)
              ("C-M-p" . puni-syntactic-backward-punct)
              ("C-M-n" . puni-syntactic-forward-punct)
              ("C-M-t" . puni-transpose)
              ("C-c v" . puni-expand-region))
  :hook
  (after-init . puni-global-mode)
  (vterm-mode . puni-disable-puni-mode)
  (minibuffer-mode . puni-disable-puni-mode))

(use-package elec-pair
  :ensure nil
  :init
  (electric-pair-mode))

(use-package project
  :ensure nil
  :defer 10
  :custom
  (project-vc-ignores '("result/" ".direnv/" ".node_modules/" ".bloop/" ".metals/" "target/" ".DS_Store")))

(use-package prog-mode
  :defer 3
  :ensure nil
  :hook ((prog-mode . prettify-symbols-mode)
         (prog-mode . hs-minor-mode))
  :init
  ;; disable showing compilation *buffer*
  (add-to-list
   'display-buffer-alist
   '("\\*compilation\\*"
     (display-buffer-no-window)))
  :config
  (setq compilation-finish-functions
        (append compilation-finish-functions
                '(peel/compilation-finish)))

  (defun peel/compilation-finish (buffer status)
    ;; ('require alert)
    ;; (alert status :title "Compilation" :severity 'trivial)
    (call-process "osascript" nil 0 nil "-e"
                  (concat "display notification \"Compilation: " status "\" with title \"Emacs\""))))

(use-package fancy-narrow
  :ensure t
  :bind (("C-x n n" . peel/fancy-narrow-dwim)
         ("C-x n d" . fancy-narrow-to-defun)
         ("C-x n w" . fancy-widen))
  :config
  (defun peel/fancy-narrow-dwim ()
                  (interactive)
                  (if fancy-narrow--beginning
                      (fancy-widen)
                    (unless (region-active-p)
                      (mark-paragraph -1))
                    (call-interactively 'fancy-narrow-to-region))))

(use-package flyover
  :ensure t
  :diminish
  :after flymake
  :hook (flymake-mode . flyover-mode)
  :custom
  (flyover-checkers '(flymake))
  (flyover-virtual-line-type 'line-no-arrow)
  (flyover-show-at-eol nil)
  (flyover-info-icon "")
  (flyover-warning-icon "")
  (flyover-error-icon ""))

(use-package eglot
  :demand
  :bind (("C-c C-l s" . eglot)
         (:map eglot-mode-map
               ("C-c C-l f" . eglot-format)
               ("C-c C-l a" . eglot-code-actions)
               ("C-c C-l r" . eglot-rename)
               ("C-c C-l i" . peel/eglot-imports)
               ("C-c C-l S" . eglot-reconnect)
               ("C-c C-l q" . eglot-shutdown)
               ("C-c C-l h" . flymake-show-project-diagnostics)))
  :after (:all envrc inheritenv)
  :custom
  (eldoc-echo-area-use-multiline-p 5)
  :config
  (advice-add 'eglot :before #'envrc-reload)
  (defun peel/eglot-imports ()
    (interactive)
    (call-interactively #'eglot-code-action-organize-imports))
  (add-to-list 'eglot-server-programs
               '((go-mode go-ts-mode) .
                 ("gopls" :initializationOptions
                  (:hints (:parameterNames t
                                           :rangeVariableTypes t
                                           :functionTypeParameters t
                                           :assignVariableTypes t
                                           :compositeLiteralFields t
                                           :compositeLiteralTypes t
                                           :constantValues t)))))
  (add-to-list 'eglot-server-programs
               '((rust-ts-mode rust-mode) .
                 ("rust-analyzer" :initializationOptions (:check (:command "clippy")))))
  (add-to-list 'eglot-server-programs
               '(scala-mode .
                 ("metals" :initializationOptions
                  (:metals (:isHttpEnabled t
                            :automaticImportBuild t
                            :inlayHints
                            (:typeParameters (:enable t)
                             :hintsInPatternMatch (:enable t)
                             :inferredTypes (:enable t)))))))
  :hook ((scala-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (haskell-mode . eglot-ensure)
         (rust-mode . eglot-ensure)
         (rustic-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (go-ts-mode . eglot-ensure)))

(use-package eglot-booster
  :ensure t
	:after eglot
	:config	(eglot-booster-mode))

(use-package agent-shell
  :ensure t
  :after (:all envrc inheritenv)
  :config
  (setq agent-shell-anthropic-claude-command '("claude-agent-acp"))
  (agent-shell-anthropic-authentication
   (agent-shell-anthropic-make-authentication :login t)))

 (use-package claude-code-ide
    :ensure t
    :after (:all envrc inheritenv)
    :bind ("C-c c" . claude-code-ide-menu)
    :config
    (claude-code-ide-emacs-tools-setup)
    (advice-add 'claude-code-ide--start-session :before
                (lambda (&rest _)
                  (when-let ((path (executable-find "claude")))
                    (setq-local claude-code-ide-cli-path path)))))

(use-package transient
  :demand
  :ensure t)

;; ..................................................................... Haskell
(use-package haskell-mode
  :ensure t
  :mode ("\\.hs\\'")
  :preface
  (load "haskell-mode-autoloads" t t)
  :mode (("\\.hs\\(c\\|-boot\\)?\\'" . haskell-mode)
         ("\\.lhs\\'" . haskell-literate-mode)
         ("\\.cabal\\'" . haskell-cabal-mode))
  :hook (haskell-mode . subword-mode)
  :functions xref-push-marker-stack
  :commands (haskell-session-maybe
             haskell-mode-find-def
             haskell-ident-at-pointoh
             haskell-mode-handle-generic-loc))
(use-package haskell-interactive-mode)

;; ......................................................................... nix
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package nix-buffer
  :ensure t
  :commands nix-buffer)

(use-package nix-sandbox
  :ensure t
  :defer 2)

(use-package envrc
  :ensure t
  :diminish
  :init (envrc-global-mode))

(use-package inheritenv
  :demand
  :ensure t
  :diminish
  :init
  (inheritenv-add-advice 'shell-command)
  (inheritenv-add-advice 'shell-command-to-string)
  (inheritenv-add-advice 'async-shell-command)
  (inheritenv-add-advice 'compile)
  (inheritenv-add-advice 'project-compile)
  (inheritenv-add-advice 'claude-code--term-make)
  (inheritenv-add-advice 'claude-code-vterm)
  (inheritenv-add-advice 'claude-code-ide--create-vterm-session))

;; ....................................................................... dhall
(use-package dhall-mode
  :ensure nil ;; disabled as it fails to build
  :mode "\\.dhall\\'"
  :custom (dhall-format-at-save . -1))

;; ...................................................................... docker
(use-package dockerfile-mode
  :ensure t)

;; ........................................................................ yaml
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

;; .......................................................................... go
(use-package go-mode
  :ensure t
  :commands go-mode
  :mode ("\\.go?\\'" . go-mode)
  :defer t
  :hook (eglot-format-buffer))

;; ........................................................................ rust

(use-package rustic
  :ensure t
  :mode ("\\.rs?\\'" . rustic-mode)
  :after inheritenv
  :config
  (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))
  :custom
  (rustic-format-on-save t)
  (rustic-lsp-client 'eglot))


;; ....................................................................... scala
(use-package scala-mode
  :ensure t
  :mode ("\\.scala\\'" "\\.sc\\'" "\\.sbt\\'")
  :hook (scala-mode . subword-mode)
  :interpreter ("scala" . scala-mode)
  :config
  (setq scala-indent:align-forms t)
  (setq scala-indent:align-parameters t)
  (setq scala-indent:default-run-on-strategy scala-indent:operator-strategy))


;; .......................................................................... js
(use-package js2-mode
  :ensure t
  :hook (js-mode . js2-minor-mode))

(use-package prettier-js
  :ensure t
  :hook ((web-mode . prettier-js-mode)
         (js-mode . prettier-js-mode)))

(use-package typescript-mode
  :ensure t
  :mode "\\.tsx?\\'")

;; .................................................................. restclient
(use-package restclient
  :ensure t
  :diminish (restclient-mode . " ")
  :mode (("\\.http\\'" . restclient-mode)
	       ("\\.rest\\'" . restclient-mode)
	       ("\\.restclient\\'" . restclient-mode)))

;; ................................................................... terraform
(use-package terraform-mode
  :ensure t
  :mode ("\\.tf\\'"))

;; .................................................................... markdown
(use-package markdown-mode
  :ensure t
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  :diminish (markdown-mode . " "))

;; ;; ai  ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
;; (use-package gptel
;;   :ensure t )

;; org ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(setq papers-dir (expand-file-name "~/Dropbox/Documents/roam/")
      papers-pdfs (concat papers-dir "lib/")
      papers-notes (concat papers-dir "books/")
      papers-refs (concat papers-dir "index.bib"))

(use-package org
  :ensure nil
  :defer nil
  :hook ((org-mode . visual-line-mode)
         (org-mode . auto-fill-mode)
         (org-mode . org-indent-mode)
         (org-mode . writeroom-mode))
  :config
  (require 'org-protocol)
  ;; todo paths
  (setq org-directory "~/Dropbox/Documents/roam/")
  (setq org-agenda-files '("~/Dropbox/Documents/roam/journal"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell      . t)
     (js         . t)
     (emacs-lisp . t)
     (haskell    . t)
     (dot        . t)
     (python     . t))))

(use-package org-transclusion
  :ensure t
  :after org)

(use-package ob-restclient
  :ensure t)

(use-package org-capture
  :bind ("C-C n n" . org-capture)
  :init
  (setq org-capture-templates `(
    ("p" "Protocol" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
     "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
    ("L" "Protocol Link" entry (file+headline ,(concat org-directory "notes.org") "Inbox")
     "* %? [[%:link][%:description]] \nCaptured On: %U"))))

(use-package org-present
  :ensure t
  :hook (org-present-mode . (lambda ()
                (org-present-big)
                (org-display-inline-images)
                (org-present-hide-cursor)
                (org-present-read-only)))
         (org-present-mode-quit . (lambda ()
                 (org-present-small)
                 (org-remove-inline-images)
                 (org-present-show-cursor)
                 (org-present-read-write))))

(use-package org-roam
  :ensure t
  :after org
  :hook (after-init-hook . org-roam-db-autosync-enable)
  :init
  ;; (defun peel/org-roam-capture ()
  ;;   (interactive)
  ;;   (org-roam-capture)
  ;;   (defun peel/delete-if-capture-frame ()
  ;;   (let (this-frame-name (substring-no-properties
  ;;                     (cdr (assoc 'name (frame-parameters)))))
  ;;     (message this-frame-name)
  ;;     (if (string-equal this-frame-name "org-capture")
  ;;         'delete-frame
  ;;       nil)))
  ;;   (advice-add 'org-roam-capture--finalize :after 'peel/delete-if-capture-frame))

  :custom
  (org-roam-autosync-enable t)
  (org-roam-db-location "~/.config/emacs/org-roam.db")
  (org-roam-directory "~/Dropbox/Documents/roam/")
  (org-roam-link-auto-replace)
  (add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)))

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :bind ("C-c n g" . org-roam-ui-open)
  :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package readwise
  :ensure t
  :autoload readwise-pull
  :after org-roam
  :custom
  (readwise-sync-db-path "~/Dropbox/Documents/roam/highlights.org")
  (readwise-api-token (auth-source-pick-first-password :host "readwise.io" :user "credentials")))

(use-package auth-source-1password
  :ensure t
  :custom
  (auth-source-1password-vault "Private")
  (auth-source-1password-executable "op"))
  ;; :init
  ;; (auth-source-1password-enable))

(use-package gnuplot
  :ensure t
  :defer t)

(use-package gnuplot-mode
  :ensure t)

(use-package org-roam-bibtex
  :ensure t
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (:map org-mode-map
              (("C-c n a" . orb-note-actions))))

(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode)
  :hook ((peel/nov-ui)
         (nov-mode . org-noter))
  :config
  (defun peel/nov-ui ()
    (face-remap-add-relative 'variable-pitch :family "Georgia"
                                 :height 1.05)))

(use-package pdf-tools
  :ensure t
  :hook (pdf-view-mode . pdf-view-midnight-minor-mode)
  :config
  (require 'pdf-occur)
  (pdf-tools-install))

(use-package org-ref
  :ensure t
  :config
  ;; (setq reftex-default-bibliography (list papers-refs))
  (setq org-ref-bibliography-notes papers-notes
        org-ref-default-bibliography (list papers-refs)
        org-ref-pdf-directory papers-pdfs))

(use-package org-noter
  :ensure t
  :commands org-noter
  :custom
  (org-noter-default-notes-file-names '("index-org"))
	(org-noter-notes-search-path (list "~/Dropbox/Documents/roam/"))
	(org-noter-auto-save-last-location t)
	(org-noter-doc-split-fraction '(0.8 . 0.8))
	(org-noter-always-create-frame nil)
	(org-noter-insert-note-no-questions t)
	(org-noter-notes-window-location 'horizontal-split))

;; todo paths
(use-package org-static-blog
  :ensure t
  :load-path "~/Dropbox/Documents/roam/"
  :custom
  (org-export-with-section-numbers nil)
  (org-export-with-toc nil)
  (org-static-blog-enable-tags t)
  (org-static-blog-drafts-directory "/tmp/drafts")
  (org-static-blog-posts-directory "~/Dropbox/Documents/roam/")
  (org-static-blog-publish-directory "~/Dropbox/Documents/roam-html/")
  (org-static-blog-publish-title "notes.codearsonist.com")
  (org-static-blog-publish-url "notes.codearsonist.com")
  (org-static-blog-use-preview t)
  (org-static-blog-index-file "landing.html")
  (org-static-blog-page-preamble nil)
  (org-static-blog-page-postamble nil)
  (org-static-blog-page-header "<link href= \"export/html/style.css\" rel=\"stylesheet\" type=\"text/css\" />"))

;; terminal ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(defun lambda-term-keys-want-key-p-def (key mods)
  "Lambda implementation for `term-keys/want-key-p-func'.

This function controls which key combinations are to be encoded
and decoded using the term-keys protocol extension.
KEY is the KeySym name as listed in `term-keys/mapping'; MODS is
a 6-element bool vector representing the modifiers Shift /
Control / Meta / Super / Hyper / Alt respectively, with t or nil
representing whether they are depressed or not.  Returns non-nil
if the specified key combination should be encoded.

Note that the ALT modifier rarely actually corresponds to the Alt
key on PC keyboards; the META modifier will usually be used
instead."
  (let ((shift   (elt mods 0))
        (control (elt mods 1))
        (meta    (elt mods 2))
        (super   (elt mods 3))
        (hyper   (elt mods 4))
        (alt     (elt mods 5)))
    (and

     ;; We don't care about Super/Hyper/Alt modifiers
     (not super)
     (not hyper)
     (not alt)

     (or
      ;; Navigation keys and Control/Alt
      (and (member key '("Left" "Right")) control)
      ))))

(use-package term-keys
  :ensure t
  :custom
  (term-keys/want-key-p-func 'lambda-term-keys-want-key-p-def)
  :config
  (term-keys-mode t))

(use-package vterm
  :demand
  :ensure t
  :hook (vterm-mode . (lambda ()
                        (setq-local global-hl-line-mode nil)
                        (setq-local line-spacing nil)))
  (add-hook 'vterm-mode-hook #'my/setup-window-borders)
  :bind ("C-c C-d" . peel/vterm-cd)
  :custom
  (vterm-max-scrollback 100000)
  (vterm-kill-buffer-on-exit t)
  :config
  (add-to-list 'vterm-keymap-exceptions "C-b")
  ;; (defun vterm-counsel-yank-pop-action (orig-fun &rest args)
  ;;   "Use vterm-yank-pop to make counsel-yank-pop work in vterm"
  ;;   (if (equal major-mode 'vterm-mode)
  ;;       (let ((inhibit-read-only t)
  ;;             (yank-undo-function #'(lambda(_start _end) (vterm-undo))))
  ;;         (cl-letf (((symbol-function 'insert-for-yank)
  ;;                    #'(lambda(str) (vterm-send-string str t))))
  ;;           (apply orig-fun args)))
  ;;     (apply orig-fun args)))
  ;; (advice-add 'counsel-yank-pop-action :around #'vterm-counsel-yank-pop-action)

  (defun peel/vterm-inherit-environment ()
    "Ensure vterm gets the full environment including direnv."
    (when (and (bound-and-true-p envrc-mode)
               (string-prefix-p "*claude:" (buffer-name)))
      (setq vterm-environment process-environment)))
  
  (add-hook 'vterm-mode-hook #'peel/vterm-inherit-environment)
  (defun peel/vterm-cd (dir)
    "Prompt for directory and cd"
    (interactive "Dcd ")
    (let* ((inhibit-read-only t))
      (vterm-send-string (concat "cd " dir))
      (vterm-send-return)
      (vterm-clear))))

(use-package multi-vterm
  :ensure t
  :bind ("C-!" . multi-vterm-project))

;; builtins ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
;; .................................................................. autorevert
(use-package autorevert
  :ensure nil
  :commands global-auto-revert-mode
  :demand
  :config (global-auto-revert-mode t))

;; ............................................................ delete selection
(use-package delsel
  :ensure nil
  :demand
  :commands delete-selection-mode
  :config (delete-selection-mode t))

;; ....................................................................... folds
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :diminish hs-minor-mode
  :custom (hs-isearch-opean t "Automatically open a block if it matches a search")
  :bind (("M-<tab>" . hs-toggle-hiding)
         ("M-+" . hs-show-all)
         ("M--" . hs-hide-level)))

(use-package ediff
  :ensure nil
  :init
  (defun peel/kill-ediff-buffers ()
    (kill-buffer ediff-buffer-A)
    (kill-buffer ediff-buffer-B)
    (kill-buffer ediff-buffer-C))
  (add-hook 'ediff-quit-hook 'peel/kill-ediff-buffers))

;; ui ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
;; ...................................................................... themes
(use-package emacs
  :ensure nil
  :config
  (defvar dark-theme 'gotham)
  (defvar light-theme 'solo-jazz)
  (defvar current-theme dark-theme)

  (use-package auto-dark
    :ensure t
    :custom
    (auto-dark-dark-theme 'gotham)
    (auto-dark-light-theme 'solo-jazz)
    :config
    (auto-dark-mode t))

  (use-package gotham-theme
    :ensure t
    :preface
    (add-to-list 'custom-theme-load-path
                 (file-name-directory (locate-library "gotham-theme"))))

  (use-package solo-jazz-theme
    :ensure t
    :preface
    (add-to-list 'custom-theme-load-path
                 (file-name-directory (locate-library "solo-jazz-theme")))
    :config
    (load-theme 'solo-jazz))

  (defun peel/load-theme ()
    (load-theme dark-theme t)
    (setq current-theme dark-theme))

  (defun peel/load-font ()
    "Load default font."
    (add-to-list 'initial-frame-alist '(font . "PragmataPro Liga"))
    (add-to-list 'default-frame-alist '(font . "PragmataPro Liga"))
    (set-face-attribute 'default nil :height 220)
    (setq-default line-spacing 9))

  (defun peel/load-ui ()
    "Remove UI bars."
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (blink-cursor-mode -1)
    (menu-bar-mode -1)
    (unless (display-graphic-p)
        (progn
          (setenv "COLORTERM" "truecolor")
          (set-terminal-parameter nil 'background-mode 'dark)))
    (setq ns-use-srgb-colorspace nil)
    (if (memq window-system '(mac ns))
        (progn
          (setq frame-title-format '("%b"))
          (setq ns-use-proxy-icon t)
          (add-to-list 'default-frame-alist '(ns-transparent-titlebar t))
          (add-to-list 'default-frame-alist '(ns-appearance dark)))
      (menu-bar-mode -1)))

  (defun peel/load-glitter (&optional frame)
    (unless frame
      (setq frame (selected-frame)))
    (when frame
      (with-selected-frame frame
        (when (display-graphic-p)
          (peel/load-theme)
          (peel/load-font)
          (peel/load-ui)))))

  (if (daemonp)
      (add-hook 'after-make-frame-functions #'peel/load-glitter t)
    (peel/load-glitter)))

(use-package writeroom-mode
  :ensure t
  :custom
  (writeroom-fullscreen-effect "maximized")
  (writeroom-width 126)
  (writeroom-bottom-divider-width 0)
  (global-writeroom-mode))

(use-package clipetty
  :ensure t
  :hook (after-init . global-clipetty-mode))

;; .................................................................... unclutter
(use-package emacs
  :defer 0
  :init
  (setq gc-cons-threshold (* 2 100000000)
        inhibit-startup-screen t
        initial-scratch-message nil
        make-backup-files nil
        frame-resize-pixelwise t
        pop-up-windows nil
        column-number-mode t
        confirm-kill-emacs 'yes-or-no-p
        echo-keystrokes 0.1
        visible-bell nil
        hl-line-mode t
        xwidget-webkit-enable-plugins nil
        tab-always-indent 'complete)
  (setq-default show-trailing-whitespace t)
  (setq-default indicate-empty-lines t)
  (setq-default indicate-buffer-boundaries 'left)
  :bind (("C-z" . kill-whole-line)
         ("M-n" . forward-paragraph)
         ("M-p" . backward-paragraph)))

(use-package modeline
  :defer 0
  :init
  (setq auto-revert-check-vc-info t)
  (defun ww/diff-hl-reducer (acc it)
    "A reducer to count added, removed, and modified lines for diff-hl."
    (cl-destructuring-bind (added removed modified) acc
      (let ((lines (nth 1 it))
            (type (nth 2 it)))
        (pcase type
          ('insert (list (+ added lines) removed modified))
          ('delete (list added (+ removed lines) modified))
          ('change (list added removed (+ modified lines)))))))


  (defun vc-status-mode-line ()
    "Builds a source control string or nil."
    (when vc-mode
      `(" "
        ,(concat (s-trim (substring-no-properties vc-mode 5)) )
        " ")))


  (setq-default
   mode-line-format
   ;; Enhanced mode line with box drawing
   (list
    '(:eval (propertize "\u2524 %b " 'face 'font-lock-keyword-face)) ;; buffer
    '(:eval (propertize "%* " 'face 'font-lock-warning-face)) ;; ! ro | * mod | - clean
    "\u251c %l:%c \u2502 "
    '(:eval (propertize "%m" 'face 'font-lock-comment-face)) ;; major
    " \u2502"
    '(:eval (vc-status-mode-line))
    '(global-mode-string global-mode-string))))

(setq backup-by-copying t
      backup-directory-alist '((".*" . "~/.emacs.d/saves/"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(setq create-lockfiles nil)

;; ............................................................. fix awkwardness
(fset 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;;; ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂
;;; default.el ends here
