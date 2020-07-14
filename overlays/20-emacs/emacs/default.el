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

(use-package diminish
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
  (exec-path-from-shell-initialize))

;; navigation ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(use-package winner
  :config (winner-mode 1))

(use-package windmove
  :ensure nil
  :bind (("C-|" . split-window-right)
         ("C--" . split-window-below))
  :init
  (windmove-default-keybindings 'meta)
  :config
  (setq windmove-wrap-around t))

;; completion ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(use-package company
  :bind ("<C-tab>" . company-complete)
  :diminish company-mode
  :commands (company-mode global-company-mode)
  :defer 1
  :custom
  (company-require-match nil)
	(company-selection-wrap-around t)
  (global-company-mode)
  (company-idle-delay 0))
;; TODO company-posframe

(use-package dash-at-point
  :commands (dash-at-point dash-at-point-with-docset)
  :bind (("C-c h" . dash-at-point)
         ("C-c H" . dash-at-point-with-docset)))

;; ivy ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
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
        ivy-re-builders-alist '((counsel-rg            . ivy--regex-plus)
                                (counsel-projectile-rg . ivy--regex-plus)
                                (swiper                . ivy--regex-plus)
                                (t                     . ivy--regex-fuzzy)))
  (use-package request)
  (use-package swiper
    :bind (("C-s" . swiper)
           ("C-r" . swiper-backward)))
  ;; TODO move po prescient.el
  ;; https://github.com/raxod502/prescient.el
  ;; https://github.com/ianpan870102/.personal-emacs.d/blob/master/init.el#L391-L412
  (use-package smex
    :after (ivy counsel)
    :init
    (setq-default smex-history-length 32)))

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
	         ("C-c d" . counsel-descbinds))
    :config
    ;; disable matching ^+ in M-x
    (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) ""))

;; syntax checking ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(use-package flycheck
  :after nix-sandbox
  :hook (prog-mode . flycheck-mode)
  :diminish flycheck-mode " ✓"
  ;; :config
  ;; (setq flycheck-command-wrapper-function
  ;;       (lambda (command) (apply 'nix-shell-command (nix-current-sandbox) command))
  ;;     flycheck-executable-find
  ;;     (lambda (cmd) (nix-executable-find (nix-current-sandbox) cmd)))
  )

;; git ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(use-package magit
  :if (executable-find "git")
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-dispatch))
  :custom
  (magit-completing-read-function 'ivy-completing-read "Use ivy with magit")
  :config
  (use-package gitignore-mode))

(use-package git-link
  :after magit
  :commands (git-link git-link-commit git-link-homepage)
  :bind (("C-c C-g l" . git-link)
         ("C-c C-g c" . git-link-commit)
         ("C-c C-g h" . git-link-homepage)))
(use-package forge
  :after magit)
  

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

;; files ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)
  (dired-recursive-deletes t)
  (dired-use-ls-dired nil)
  (delete-by-moving-to-trash t))

(use-package dired-sidebar
  :commands (dired-sidebar-toggle-sidebar)
  :bind ("C-x C-n" . dired-sidebar-toggle-sidebar)
  :custom
  (dired-sidebar-subtree-line-prefix " ."))

;; bindings ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

;; ....................................................................... popup
(use-package which-key
  :defer 1
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0
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
  :diminish diff-hl-mode
  :bind (("M-n" . diff-hl-next-hunk)
         ("M-p" . diff-hl-previous-hunk)))

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
  :defer 3
  :ensure nil
  :diminish pragmatapro-lig-mode
  :hook ((prog-mode . prettify-symbols-mode)
         (prog-mode . pragmatapro-lig-mode)
         (prog-mode . hs-minor-mode))
  :init
  (load (locate-file "pragmatapro-lig.el" load-path) 'noerror)
  ;; disable showing compilation *buffer*
  (add-to-list
   'display-buffer-alist
   '("\\*compilation\\*"
     (display-buffer-no-window))))

(use-package lsp-mode
  :bind ("C-c l" . lsp-hydra/body)
  :hook ((scala-mode . lsp)
         (js-mode . lsp)
         (typescript-mode . lsp))
  :after hydra
  :config
  (setq lsp-eldoc-render-all t)
  (defhydra lsp-hydra (:exit t :hint nil)
  "
 Buffer^^               Server^^                   Symbol
-------------------------------------------------------------------------------------
 [_f_] format           [_M-r_] restart            [_d_] declaration  [_i_] implementation  [_o_] documentation
 [_m_] imenu            [_S_]   shutdown           [_D_] definition   [_t_] type            [_r_] rename
 [_x_] execute action   [_M-s_] describe session   [_R_] references   [_s_] signature       [_w_] workspace"
  ("d" lsp-find-declaration)
  ("D" lsp-ui-peek-find-definitions)
  ("R" lsp-ui-peek-find-references)
  ("i" lsp-ui-peek-find-implementation)
  ("t" lsp-find-type-definition)
  ("s" lsp-signature-help)
  ("o" lsp-describe-thing-at-point)
  ("r" lsp-rename)
  ("w" lsp-ivy-workspace-symbol)

  ("f" lsp-format-buffer)
  ("m" lsp-ui-imenu)
  ("x" lsp-execute-code-action)

  ("M-s" lsp-describe-session)
  ("M-r" lsp-workspace-restart)
  ("S" lsp-workspace-shutdown)))

(use-package lsp-ui)
(use-package lsp-ivy)
(use-package company-lsp)

;; ..................................................................... Haskell
(use-package haskell-mode
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
             haskell-ident-at-point
             haskell-mode-handle-generic-loc))
(use-package haskell-interactive-mode)
(use-package ormolu
 :hook (haskell-mode . ormolu-format-on-save-mode)
 :bind
 (:map haskell-mode-map
   ("C-c r" . ormolu-format-buffer)))
(use-package attrap
    :bind ("C-x /" . attrap-attrap))
(use-package dante
    :commands dante-mode
    :hook ((haskell-mode . dante-mode)
           (haskell-mode . flycheck-mode))
    :config
    (setq haskell-process-wrapper-function
        (lambda (args) (apply 'nix-shell-command (nix-current-sandbox) args)))
    (add-hook 'dante-mode-hook
              '(lambda () (flycheck-add-next-checker 'haskell-dante
                                                     '(warning . haskell-hlint)))))

;; ......................................................................... nix
(use-package nix-mode
  :mode "\\.nix\\'")

(use-package nix-buffer
  :commands nix-buffer)

(use-package nix-sandbox
  :defer 2)

(use-package envrc
  :diminish
  :init (envrc-global-mode))

;; ....................................................................... dhall
(use-package dhall-mode
  :disabled
  :mode "\\.dhall\\'")

;; ...................................................................... docker
(use-package dockerfile-mode)

;; ...................................................................... elixir
(use-package elixir-mode
  :config
  (use-package alchemist))

;; ........................................................................ yaml
(use-package yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'"))

;; .......................................................................... go
(use-package go-mode
  :commands go-mode
  :mode ("\\.go?\\'" . go-mode)
  :defer t
  :hook (lsp-format-buffer))

;; .......................................................................... go
(use-package rust-mode
  :commands go-mode
  :mode ("\\.rs?\\'" . rust-mode)
  :defer t)

;; ....................................................................... scala
(use-package scala-mode
  :mode ("\\.scala\\'" "\\.sc\\'" "\\.sbt\\'")
  :hook (scala-mode . subword-mode)
  :interpreter ("scala" . scala-mode)
  :config
  (setq scala-indent:align-forms t)
  (setq scala-indent:align-parameters t)
  (setq scala-indent:default-run-on-strategy scala-indent:operator-strategy)
  (setq projectile-globally-ignored-directories (append '(".metals" ".bloop"))))

(use-package lsp-metals)

;; .......................................................................... js
(use-package js2-mode
  :hook (js-mode . js2-minor-mode))

(use-package prettier-js
  :hook ((web-mode . prettier-js-mode)
         (js-mode . prettier-js-mode)))

(use-package typescript-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode)))
  
(use-package web-mode
  :mode ("\\.html?\\'" "\\.css\\'" "\\.scss\\'")
  :custom
  (web-mode-markup-indent-offset 2 "2 spaces")
  (web-mode-css-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (css-indent-offset 2)
  (js-switch-indent-offset 2)
  (js-indent-level 2)
  (js-indent-switch-body t))

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
  :hook ((org-mode . visual-line-mode)
         (org-mode . auto-fill-mode)
         (org-mode . org-indent-mode))
  :config
  (setq org-agenda-files '("~/Dropbox/Documents/notes/"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell      . t)
     (js         . t)
     (emacs-lisp . t)
     (clojure    . t)
     (haskell    . t)
     (dot . t))))

(use-package org-roam
  :hook after-init-hook
  :custom
  (org-roam-directory "~/Dropbox/Documents/roam/")
  (org-roam-graph-viewer "/usr/bin/open"))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

(use-package pdf-tools
  :hook (pdf-view-mode . pdf-view-midnight-minor-mode)
  :config
  (require 'pdf-occur)
  (pdf-tools-install))

(use-package org-ref
  :config
  ;; (setq reftex-default-bibliography (list papers-refs))
  (setq org-ref-completion-library 'org-ref-ivy-cite
        org-ref-bibliography-notes papers-notes
        org-ref-default-bibliography (list papers-refs)
        org-ref-pdf-directory papers-pdfs))

(use-package org-noter
  :commands org-noter
  :custom
  (org-noter-default-notes-file-names '("index-org"))
	(org-noter-notes-search-path (list papers-dir))
	(org-noter-auto-save-last-location t)
	(org-noter-doc-split-fraction '(0.8 . 0.8))
	(org-noter-always-create-frame nil)
	(org-noter-insert-note-no-questions t)
	(org-noter-notes-window-location 'vertical-split))

(use-package ivy-bibtex
  :after ivy
  :custom
  (bibtex-completion-bibliography papers-refs)
  (bibtex-completion-library-path papers-pdfs)
  (bibtex-completion-notes-path papers-notes))

;; terminal ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(use-package vterm
  :hook (vterm-mode . (lambda ()
                        (setq-local global-hl-line-mode nil)
                        (setq-local line-spacing nil)))
  :bind (("C-!" . peel/vterm)
         ("s-!" . peel/vterm-force)
         ("C-c C-d" . peel/vterm-cd))
  :config
  (setq vterm-kill-buffer-on-exit t)
  (add-to-list 'vterm-keymap-exceptions "C-b")
  (defun vterm-counsel-yank-pop-action (orig-fun &rest args)
    "Use vterm-yank-pop to make counsel-yank-pop work in vterm"
    (if (equal major-mode 'vterm-mode)
        (let ((inhibit-read-only t)
              (yank-undo-function #'(lambda(_start _end) (vterm-undo))))
          (cl-letf (((symbol-function 'insert-for-yank)
                     #'(lambda(str) (vterm-send-string str t))))
            (apply orig-fun args)))
      (apply orig-fun args)))
  (advice-add 'counsel-yank-pop-action :around #'vterm-counsel-yank-pop-action)

  (defun peel/vterm-cd (dir)
    "Prompt for directory and cd"
    (interactive "Dcd ")
    (let* ((inhibit-read-only t))
      (vterm-send-string (concat "cd " dir))
      (vterm-send-return)
      (vterm-clear)))
  
  (defun peel/vterm--new (&optional force)
    "Starts or switches to vterm. If forced starts a new instance"
    (let* ((buffer-name "vterm")
           (vterm-buffer (get-buffer "vterm"))
           (in-buffer (string-match-p (regexp-quote buffer-name) (buffer-name))))
      (cond (force (vterm))
            (in-buffer (switch-to-buffer (other-buffer (current-buffer) 1)))
            (vterm-buffer (switch-to-buffer vterm-buffer))
            (t (vterm)))))

  (defun peel/vterm-force ()
    "Starts new vterm"
    (interactive)
    (peel/vterm--new t))
  
  (defun peel/vterm ()
    "Starts or switches to vterm"
    (interactive)
    (peel/vterm--new)))

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
  :hook (prog-mode . hs-minor-mode)
  :diminish hs-minor-mode
  :custom (hs-isearch-opean t "Automatically open a block if it matches a search")
  :bind (("M-<tab>" . hs-toggle-hiding)
         ("M-+" . hs-show-all)
         ("M--" . hs-hide-level)))

(use-package hide-comnt
  :commands hide/show-comments-toggle)

;; ui ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
;; ...................................................................... themes
(use-package emacs
  :ensure nil
  :config
  (defvar dark-theme 'gotham)
  (defvar light-theme 'apropospriate-light)
  (defvar current-theme dark-theme)

  (use-package gotham-theme
    :preface
    (add-to-list 'custom-theme-load-path
                 (file-name-directory (locate-library "gotham-theme"))))

  (use-package apropospriate-theme
    :preface
    (add-to-list 'custom-theme-load-path
                 (file-name-directory (locate-library "apropospriate-theme"))))

  (defun peel/load-theme ()
    (load-theme dark-theme t)
    (setq current-theme dark-theme))

  (defun peel/load-font ()
    "Load default font."
    (add-to-list 'initial-frame-alist '(font . "PragmataPro"))
    (add-to-list 'default-frame-alist '(font . "PragmataPro"))
    (set-face-attribute 'default nil :height 220)
    (setq-default line-spacing 9))
  
  (defun peel/load-ui ()
    "Remove UI bars."
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (blink-cursor-mode -1)
    (if (memq window-system '(mac ns))
        (progn
          (setq frame-title-format '("%b"))
          (setq ns-use-proxy-icon t)
          (add-to-list 'default-frame-alist '(ns-transparent-titlebar t))
          (add-to-list 'default-frame-alist '(ns-appearance dark)))
      (menu-bar-mode -1)))
  
  (defun peel/lights ()
    "Toggles dark mode on Darwin."
    (interactive)
    (defun switch (theme)
      (counsel-load-theme-action theme)
      (load-theme theme)
      (setq current-theme theme))
    (if (eq current-theme dark-theme)
        (switch light-theme)
      (switch dark-theme))
    (shell-command "osascript -e 'tell app \"System Events\" to tell appearance preferences to set dark mode to not dark mode'"))
  
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
  :custom
  (writeroom-fullscreen-effect "maximized")
  (writeroom-width 126)
  (writeroom-bottom-divider-width 0)
  (writeroom-major-modes '(prog-mode))
  :config
  (use-package focus)

  (defun peel/work ()
    "Trigger work env"
    (interactive)
    (shell-command "$HOME/wrk/dotfiles/setup/bin/bin/work on"))

  (defun peel/chill ()
    "Trigger work env"
    (interactive)
    (shell-command "$HOME/wrk/dotfiles/setup/bin/bin/work off"))

  (global-writeroom-mode))


;; .................................................................... unclutter
(use-package emacs
  :defer 0
  :init
  (setq inhibit-startup-screen t
      initial-scratch-message nil
      make-backup-files nil
      frame-resize-pixelwise t
      pop-up-windows nil
      column-number-mode t
      confirm-kill-emacs 'yes-or-no-p
      echo-keystrokes 0.1
      visible-bell nil
      hl-line-mode t)
  :bind ("C-z" . kill-whole-line))

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
