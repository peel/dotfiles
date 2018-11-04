;;;; init.el --- Peel's Essential Emacs Lisp

;;; Commentary:
;;; Work in progress.  An attempt for a clean start of nix-managed config.

;;; Code:
;;; ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂

(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook (lambda ()
                             ;; restore after startup
                             (setq gc-cons-threshold 800000)))

;; package.el ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(require 'package)
(package-initialize 'noactivate)

(eval-when-compile
  (require 'use-package))

(setq-default use-package-always-defer t
              use-package-always-ensure t)

(use-package diminish
  :init (require 'diminish))

;; darwin-specific
;; Graphical applications in macOS inherit their process environment from
;; launchd, not from a shell process which loads a profile.
(use-package exec-path-from-shell
  :defer nil
  :if (memq window-system '(mac ns))
  :config
  (defconst exec-path-from-shell-variables
    '("PATH"
      "NIX_PATH"
      "NIX_PROFILES"
      "NIX_REMOTE"
      "NIX_SSL_CERT_FILE"
      "NIX_USER_PROFILE_DIR"
      ))
  :init (exec-path-from-shell-initialize))

;; navigation ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(use-package winner
  :config (winner-mode 1))

(use-package windmove
  :ensure nil
  :defer nil
  :bind (("C-|" . split-window-right)
         ("C--" . split-window-below)
         ("C-x w r" . peel/rotate-windows))
  :config
  (windmove-default-keybindings 'meta)
  :init
  (defun peel/rotate-windows (arg)
  "Rotate your windows; use the prefix argument to rotate the other direction"
  (interactive "P")
  (if (not (> (count-windows) 1))
      (message "Nope, can't rotate a single window.")
    (let* ((rotate-times (prefix-numeric-value arg))
           (direction (if (or (< rotate-times 0) (equal arg '(4)))
                          'reverse 'identity)))
      (dotimes (_ (abs rotate-times))
        (dotimes (i (- (count-windows) 1))
          (let* ((w1 (elt (funcall direction (window-list)) i))
                 (w2 (elt (funcall direction (window-list)) (+ i 1)))
                 (b1 (window-buffer w1))
                 (b2 (window-buffer w2))
                 (s1 (window-start w1))
                 (s2 (window-start w2))
                 (p1 (window-point w1))
                 (p2 (window-point w2)))
            (set-window-buffer-start-and-point w1 b2 s2 p2)
            (set-window-buffer-start-and-point w2 b1 s1 p1))))))))

;; completion ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(use-package company
  :bind ("<C-tab>" . company-complete)
  :diminish company-mode
  :commands (company-mode global-company-mode)
  :defer 1
  :config
  (setq company-require-match nil
	    company-selection-wrap-around t)
  (global-company-mode))


;; ivy ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

;; ........................................................................ core
(use-package ivy
  :defer 1
  :bind (("C-c C-r" . ivy-resume)
	     ([remap list-buffers] . ivy-switch-buffer)
	     ([remap switch-to-buffer] . ivy-switch-buffer)
	     ([remap switch-to-buffer-other-window] . ivy-switch-buffer-other-window)
         ("C-c C-f" . counsel-recentf)
         :map ivy-minibuffer-map
         ("C-j" . ivy-call))
  :diminish ivy-mode
  :commands ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t    ;; highlight recent
        ivy-count-format "%d/%d "    ;; current / total
        ivy-initial-inputs-alist nil ;; do not preset ^ in buffer
        ivy-re-builders-alist '((t . ivy--regex-fuzzy))))

;; ...................................................................... counsel
(use-package counsel
  :commands (counsel-descbinds)
  :bind (([remap execute-extended-command] . counsel-M-x)
	     ([remap find-file] . counsel-find-file)
	     ([remap find-library] . counsel-find-library)
	     ([remap describe-function] . counsel-describe-function)
	     ([remap describe-variable] . counsel-describe-variable)
	     ([remap describe-bindings] . counsel-descbinds)
	     ([remap describe-face]  . counsel-describe-faces)
	     ([remap list-faces-display] . counsel-faces)
	     ([remap imenu] . counsel-imenu)
	     ([remap load-library] . counsel-load-library)
	     ([remap load-theme] . counsel-load-theme)
	     ([remap yank-pop] . counsel-yank-pop)
	     ([remap info-lookup-symbol] . counsel-info-lookup-symbol)
	     ([remap pop-to-mark-command] . counsel-mark-ring)
	     ([remap bookmark-jump] . counsel-bookmark)
         ("C-x j" . counsel-imenu)
	     ("C-c g" . counsel-git)
	     ("C-c j" . counsel-git-grep)
	     ("M-y" . counsel-yank-pop)
	     ("C-c i 8" . counsel-unicode-char)
         ("C-c r" . counsel-rg)
	     ("C-c d" . counsel-descbinds)))

;; ...................................................................... search
(use-package swiper
  :bind ("C-s" . swiper))

;; ........................................................................ smex
(use-package smex
  :after (ivy counsel)
  :init
  (setq-default smex-history-length 32))

;; syntax checking ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :diminish flycheck-mode " ✓")

;; git ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(use-package magit
  :if (executable-find "git")
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-dispatch-popup))
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  :config
  (use-package gitignore-mode))

(use-package git-link
  :commands (git-link git-link-commit git-link-homepage)
  :bind (("C-c C-g l" . git-link)
         ("C-c C-g c" . git-link-commit)
         ("C-c C-g h" . git-link-homepage)))

;; TODO git-timemachine

;; project management ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(use-package projectile
  :commands projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :defer 5
  :diminish projectile-mode
  :custom
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode))

;; ui ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

;; ................................................................... highlight
(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  :bind ("s-h" . highlight-symbol))

;; ................................................................. visual undo
(use-package undo-tree
  :defer 5
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t)
  :bind ("s-/" . undo-tree-visualize))


;; other ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(use-package editorconfig
  :defer 1
  :diminish editorconfig-mode
  :config (editorconfig-mode 1))

;; files ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(use-package dired
  :ensure nil
  :config
  (setq dired-dwim-target t
        dired-recursive-deletes t
        dired-use-ls-dired nil
        delete-by-moving-to-trash t))

(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar)
  :bind ("C-x C-n" . dired-sidebar-toggle-sidebar)
  :config
  (setq dired-sidebar-subtree-line-prefix " ."))

;; bindings ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

;; ....................................................................... popup
(use-package which-key
  :defer 1
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5
        which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-add-key-based-replacements
    "C-c !" "flycheck"
    "C-c p" "projectile"
    "C-c i" "unicode"
    "C-c &" "yas")
  (which-key-mode))


;; ....................................................................... setup
;; hydra
(use-package hydra
  :ensure t
  :demand t)

;; languages ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

;; ..................................................................... generic
;; indentation
(setq-default indent-tabs-mode nil
	          tab-width 2
	          fill-column 80)
;; rainbow
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :hook (prog-mode . rainbow-identifiers-mode))

;; git-gutter
(use-package diff-hl
  :hook (prog-mode . diff-hl-mode)
  :diminish diff-hl-mode)

;; gtags
(use-package ggtags
  :if (executable-find "global")
  :hook ((prog-mode . ggtags-mode))
  :diminish (ggtags-mode . " "))

(use-package counsel-gtags
  :after hydra
  :defer 1
  :diminish counsel-gtags-mode
  :bind ("C-." . counsel-gtags-hydra/body)
  :hook (ggtags-mode . counsel-gtags-mode)
  :init (require 'hydra)
  :config
  (defhydra counsel-gtags-hydra (:color blue :columns 3)
    "ggtags"
    ("." counsel-gtags-dwim "dwim")
    ("," counsel-gtags-go-backward "back")
    ("t" counsel-gtags-find-definition "definition")
    ("r" counsel-gtags-find-reference "reference")
    ("s" counsel-gtags-find-symbol "symbol")
    ("c" counsel-gtags-create-tags "create tags")
    ("u" counsel-gtags-update-tags "update tags")
    ("j" counsel-gtags-go-forward "forward")))

(use-package dumb-jump
  :after hydra
  :bind ("s-." . dumb-jump-hydra/body)
  :init (require 'hydra)
  :config
  (defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back")))

(use-package expand-region
  :bind (("C-c v" . er/expand-region)))

(use-package smartparens
  :after hydra
  :hook (prog-mode . smartparens-strict-mode)
  :diminish smartparens-mode
  :bind (("C-c k" . peel-smartparens/body)
         :map smartparens-strict-mode-map
         ;; A fill paragraph in strict mode
         ("M-q" . sp-indent-defun))
  :init
  (require 'hydra)
  :config
  (setq sp-highlight-wrap-overlay t
        sp-highlight-pair-overlay t
        sp-highlight-wrap-tag-overlay t)
  ;; Hydra for Smartparens
  (defhydra peel-smartparens (:hint nil)
    "
Sexps (quit with _q_)

^Nav^            ^Barf/Slurp^                 ^Depth^
^---^------------^----------^-----------------^-----^-----------------
_f_: forward     _→_:         slurp forward   _R_: splice
_b_: backward    _←_:         barf forward    _r_: raise
_u_: backward ↑  _C-<right>_:  slurp backward  _↑_: raise backward
_d_: forward ↓   _C-<left>_:   barf backward   _↓_: raise forward
_p_: backward ↓
_n_: forward ↑

^Kill^           ^Misc^                       ^Wrap^
^----^-----------^----^-----------------------^----^------------------
_w_: copy        _j_: join                    _(_: wrap with ( )
_k_: kill        _s_: split                   _{_: wrap with { }
^^               _t_: transpose               _'_: wrap with ' '
^^               _c_: convolute               _\"_: wrap with \" \"
^^               _i_: indent defun"
    ("q" nil)
    ;; Wrapping
    ("(" (lambda (_) (interactive "P") (sp-wrap-with-pair "(")))
    ("{" (lambda (_) (interactive "P") (sp-wrap-with-pair "{")))
    ("'" (lambda (_) (interactive "P") (sp-wrap-with-pair "'")))
    ("\"" (lambda (_) (interactive "P") (sp-wrap-with-pair "\"")))
    ;; Navigation
    ("f" sp-forward-sexp )
    ("b" sp-backward-sexp)
    ("u" sp-backward-up-sexp)
    ("d" sp-down-sexp)
    ("p" sp-backward-down-sexp)
    ("n" sp-up-sexp)
    ;; Kill/copy
    ("w" sp-copy-sexp)
    ("k" sp-kill-sexp)
    ;; Misc
    ("t" sp-transpose-sexp)
    ("j" sp-join-sexp)
    ("s" sp-split-sexp)
    ("c" sp-convolute-sexp)
    ("i" sp-indent-defun)
    ;; Depth changing
    ("R" sp-splice-sexp)
    ("r" sp-splice-sexp-killing-around)
    ("<up>" sp-splice-sexp-killing-backward)
    ("<down>" sp-splice-sexp-killing-forward)
    ;; Barfing/slurping
    ("<right>" sp-forward-slurp-sexp)
    ("<left>" sp-forward-barf-sexp)
    ("C-<left>" sp-backward-barf-sexp)
    ("C-<right>" sp-backward-slurp-sexp))
  (use-package smartparens-config
    :ensure nil
    :demand))

(use-package prog-mode
  :ensure nil
  :diminish pragmata-pro-mode
  :hook ((prog-mode . prettify-symbols-mode)
         (prog-mode . display-line-numbers-mode)
         (prog-mode . pragmata-pro-mode))
  :preface (load (locate-file "pragmata-pro.el" load-path) 'noerror))

(use-package dash-at-point
  :commands dash-at-point
  :bind ("C-c h" . dash-at-point))


;; ..................................................................... Haskell
(use-package direnv
  :config (direnv-mode))

(use-package haskell-mode
  :diminish (haskell-mode . " ")
  :init
  (use-package shm
    :hook (haskell-mode . structured-haskell-mode))
  (use-package hindent
    :hook (haskell-mode . hindent-mode))
  (use-package attrap
    :bind ("C-x /" . attrap-attrap))
  (use-package dante
    :after (direnv nix-buffer)
    :commands dante-mode
    :hook ((haskell-mode . dante-mode)
           (haskell-mode . flycheck-mode))
    :config
    (add-hook 'dante-mode-hook
              '(lambda () (flycheck-add-next-checker 'haskell-dante
                                                '(warning . haskell-hlint))))))

;; ......................................................................... nix
(use-package nix-mode
  :mode "\\.nix\\'")

(use-package nix-buffer
  :commands nix-buffer)

(use-package direnv
  :config (direnv-mode))

;; ....................................................................... dhall
(use-package dhall-mode
  :disabled
  :mode "\\.dhall\\'")

;; ...................................................................... docker
(use-package dockerfile-mode)

;; ...................................................................... elixir
(use-package elixir-mode
  :init
  (use-package alchemist))

;; ...................................................................... shells
;; TODO

;; ..................................................................... clojure
;; TODO

;; ........................................................................ yaml
(use-package yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'"))

;; ....................................................................... scala
(use-package sbt-mode
  :config
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package scala-mode
  :mode ("\\.scala\\'" "\\.sc\\'" "\\.sbt\\'")
  :diminish (scala-mode . " ")
  :interpreter
  ("scala" . scala-mode)
  :bind ("C-c C-v f" . scalafmt/format-file)
  :config
  (setq scala-indent:align-forms t
        scala-indent:align-parameters t
        scala-indent:default-run-on-strategy scala-indent:operator-strategy)
  :preface
  (defun scalafmt/format-region (beg end)
    "Run scalafmt on selected region"
    (interactive "r")
    (call-process-region beg end
                         "scalafmt"
                         t t nil
                         "--non-interactive"
                         "--config" (expand-file-name "~/.scalafmt.conf")
                         "--stdin"
                         "--assume-filename" (file-name-nondirectory buffer-file-name)))
  
  (defun scalafmt/format-file ()
    "Run scalafmt on the current file"
    (interactive)
    (let ((default-directory (projectile-project-root))
          (scalafmt-cmd (format "/usr/bin/env scalafmt -i --config ~/.scalafmt.conf -f %s"
                                (shell-quote-argument (buffer-file-name)))))
      (message "Running %s..." scalafmt-cmd)
      (shell-command scalafmt-cmd)
      (revert-buffer t t t))))


;; .......................................................................... js
;;(use-package tide)
(use-package rjsx-mode
  :config
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode))

(use-package prettier-js
  :hook (web-mode . prettier-js-mode)
  :config
  (setq prettier-js-args '("--print-width" "90")))

(use-package web-mode
  :ensure smartparens
  :ensure rainbow-delimiters
  :ensure prettier-js
  :ensure rjsx-mode
  :mode ("\\.html?\\'" "\\.jsx?" "\\.css\\'" "\\.scss\\'")
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        css-indent-offset 2
        js-switch-indent-offset 2
        js-indent-level 2
        js-indent-switch-body t))


;; .................................................................. restclient
(use-package restclient
  :diminish (restclient-mode . " ")
  :mode (("\\.http\\'" . restclient-mode)
	     ("\\.rest\\'" . restclient-mode)
	     ("\\.restclient\\'" . restclient-mode)))


;; ................................................................... terraform
(use-package terraform-mode
  :mode ("\\.tf\\'"))


;; .................................................................... markdown
(use-package markdown-mode
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode))
  :diminish (markdown-mode . " "))


;; org ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(setq papers-dir (expand-file-name "~/Dropbox/Documents/notes/reading/")
      papers-pdfs (concat papers-dir "lib/")
      papers-notes (concat papers-dir "index.org")
      papers-refs (concat papers-dir "index.bib"))

(use-package org
  :ensure nil
  :defer nil
  :config
  ;; jekyll 
  (autoload 'endless/export-to-blog "jekyll-once")
  (setq org-jekyll-use-src-plugin t)
  (setq endless/blog-base-url "https://codearsonist.com/")
  (setq endless/blog-dir (expand-file-name "~/wrk/blog/"))
  ;; disabled for now: (require 'ox-jekyll-subtree)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell      . t)
     (js         . t)
     (emacs-lisp . t)
     (clojure    . t)
     (scala      . t)
     (haskell    . t)
     (dot . t)))
  :init
  (use-package ob-async))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install))

(use-package org-ref
  :init
  ;; (setq reftex-default-bibliography (list papers-refs))
  (setq org-ref-completion-library 'org-ref-ivy-cite
        org-ref-bibliography-notes papers-notes
        org-ref-default-bibliography (list papers-refs)
        org-ref-pdf-directory papers-pdfs))

(use-package org-noter
  :after org-mode
  :hook ((pdf-view-mode . org-noter-mode)
         (nov-mode . org-noter-mode))
  :config
  (setq org-noter-notes-search-path (list papers-dir)))

(use-package ivy-bibtex
  :after ivy
  :config
  (setq bibtex-completion-bibliography papers-refs
        bibtex-completion-library-path papers-pdfs
        bibtex-completion-notes-path papers-notes))


;; builtins ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

;; ....................................................................... eldoc
(use-package eldoc
  :ensure nil
  :diminish (eldoc-mode . " ")
  :demand
  :hook ((emacs-lisp-mode . eldoc-mode)
	     (eshell-mode . eldoc-mode)
	     (ggtags-mode . eldoc-mode))
  :config (eldoc-mode t))

;; ...................................................................... eshell
(use-package eshell
  :ensure nil
  :after hydra
  :bind ("C-c e" . eshell-hydra/body)
  :config
  (setq eshell-banner-message "")
  (require 'vterm)
  (setq vterm-keymap-exceptions '("C-x" "C-u" "C-g" "C-h" "M-x" "M-o" "C-v" "M-v" "s-v" "s-c"))
  
  (use-package shell-pop
    :requires eshell
    :init (setq shell-pop-window-size 45
                shell-pop-shell-type '("eshell" "*eshell*" (lambda () (eshell)))
                shell-pop-window-position "bottom"))
  
  (use-package xterm-color
    :hook (eshell-before-prompt-hook . (setq xterm-color-preserve-properties))
    :config
    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
    (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions)))
  
  (use-package esh-autosuggest
    :hook (eshell-mode . esh-autosuggest-mode))
  
  (use-package eshell-prompt-extras
    :after eshell-visual-options
    :config
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-lambda)
    (autoload 'epe-theme-lambda "eshell-prompt-extras"))
  
  :init
  (require 'hydra)
  (defalias 'emacs 'find-file)
  (defalias 'gs 'magit-status)
  (defun eshell-new()
    "Open a new instance of eshell."
    (interactive)
    (eshell 'N))
  (defun vterm-pop()
    "Open existing vterm if exists."
    (interactive)
    (if (get-buffer "vterm")
        (switch-to-buffer "vterm")
      (vterm)))
  :config
  (defhydra eshell-hydra (:color blue :columns 3)
    "Eshell"
    ("e" eshell "Open eshell")
    ("E" eshell-new "Eshell new window")
    ("t" shell-pop "Pop")
    ("s" vterm-pop "Open vterm")
    ("S" vterm "Vterm new window"))
  
  (defun peel/truncate-eshell-buffers ()
    "Truncates all eshell buffers"
    (interactive)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (eq major-mode 'eshell-mode)
          (eshell-truncate-buffer)))))

  ;; After being idle for 5 seconds, truncate all the eshell-buffers if
  ;; needed. If this needs to be canceled, you can run `(cancel-timer
  ;; my/eshell-truncate-timer)'
  (setq eshell-buffer-maximum-lines 20000
        peel/eshell-truncate-timer
        (run-with-idle-timer 5 t #'peel/truncate-eshell-buffers)))

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


;; ui ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

;; ...................................................................... themes
(use-package peel-frame
  :if window-system
  :ensure nil
  :requires gotham-theme
  :requires apropospriate-theme
  :requires nord-theme
  :preface
  (add-hook 'focus-in-hook #'peel/load-font)
  (add-hook 'focus-in-hook #'peel/load-theme)
  (add-hook 'focus-in-hook #'peel/load-ui)
  
  (defvar dark-theme 'gotham)
  (defvar semi-dark-theme 'nord)
  (defvar light-theme 'apropospriate-light)
  (defvar default-theme semi-dark-theme)
  
  (defun peel/load-theme ()
    (load-theme default-theme t)
    (remove-hook 'focus-in-hook #'peel/load-theme))

  (defun peel/load-font ()
    "Load default font."
    (defvar default-font "PragmataPro")
    (set-face-attribute 'default nil :height 210)
    (setq-default line-spacing 8)
    (set-frame-font default-font))
  
  (defun peel/load-ui ()
    "Remove UI bars."
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (blink-cursor-mode -1)
    
    ;; (setq default-frame-alist '((undecorated . t)))
    ;; and the workaround for the above thats's broken
    (when (memq window-system '(mac ns))
      (progn
        ;; (setq frame-title-format nil)
        ;; (setq ns-use-proxy-icon nil)
        (add-to-list 'default-frame-alist '(ns-transparent-titlebar t))
        (add-to-list 'default-frame-alist '(ns-appearance dark))))
    
    (when (not (memq window-system '(mac ns)))
      (menu-bar-mode -1))
    
    (remove-hook 'focus-in-hook #'peel/load-ui))
  
  (defun dark-theme ()
    "Load dark theme."
    (interactive)
    (mapcar #'disable-theme custom-enabled-themes)
    (load-theme dark-theme t))

  (defun semi-dark-theme ()
    "Load semi-dark theme."
    (interactive)
    (mapcar #'disable-theme custom-enabled-themes)
    (load-theme semi-dark-theme t))

  (defun light-theme ()
    "Load light theme."
    (interactive)
    (mapcar #'disable-theme custom-enabled-themes)
    (load-theme light-theme t))
  
  :config
  (use-package nord-theme
    :preface
    (add-to-list 'custom-theme-load-path
                 (file-name-directory (locate-library "nord-theme"))))

  (use-package gotham-theme
    :preface
    (add-to-list 'custom-theme-load-path
                 (file-name-directory (locate-library "gotham-theme"))))

  (use-package apropospriate-theme
    :preface
    (add-to-list 'custom-theme-load-path
                 (file-name-directory (locate-library "apropospriate-theme")))))

;; .................................................................... unclutter
(use-package emacs
  :defer 0
  :bind (("C-z" . kill-whole-line)))

(setq inhibit-startup-screen t
      initial-scratch-message nil
      make-backup-files nil
      frame-resize-pixelwise t
      pop-up-windows nil
      column-number-mode t
      confirm-kill-emacs 'yes-or-no-p
      echo-keystrokes 0.1
      apropos-do-all t
      visible-bell nil)

(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.saves/"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; ............................................................. fix awkwardness
(fset 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;;; ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂
;;; default.el ends here
