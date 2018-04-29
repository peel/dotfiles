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


;; window management ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
(use-package winner
  :config (winner-mode 1))
;; TODO winner-mode bindings


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
         :map ivy-minibuffer-map
         ("C-j" . ivy-call))
  :diminish ivy-mode
  :commands ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-re-builders-alist
	'((t . ivy--regex-fuzzy))))

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
	 ("C-c g" . counsel-git)
	 ("C-c j" . counsel-git-grep)
	 ("M-y" . counsel-yank-pop)
	 ("C-c i 8" . counsel-unicode-char)
         ("C-c k" . counsel-rg)
	 ("C-c d" . counsel-descbinds)))

;; ...................................................... projectile integration
(use-package counsel-projectile
  :requires (counsel projectile)
  :commands counsel-projectile
  :config
  (counsel-projectile-mode))

;; ...................................................................... search
(use-package swiper
  :bind ("C-s" . swiper))


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
  (magit-completing-read-function 'ivy-completing-read))

;; TODO git-timemachine
;; TODO gitignore-mode

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

;; bindings ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

;; ....................................................................... popup
(use-package which-key
  :defer 1
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.5)
  (which-key-mode))


;; ....................................................................... setup
;; TODO hydra
;; TODO bind-key


;; languages ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

;; ..................................................................... generic
;; indentation
(setq-default indent-tabs-mode nil
	      tab-width 4
	      fill-column 80)

(use-package aggressive-indent
  :hook (prog-mode . aggressive-indent-mode)
  :diminish (aggressive-indent-mode . " "))

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
  :hook ((nix-mode . ggtags-mode) (scala-mode . ggtags-mode))
  :diminish (ggtags-mode . " "))

(use-package counsel-gtags
  :defer 1
  :diminish counsel-gtags-mode
  :hook (ggtags-mode . counsel-gtags-mode))

(use-package show-paren
  :ensure nil
  :init (show-paren-mode 1))

(use-package smartparens
  :hook ((prog-mode . smartparens-strict-mode))
  :diminish smartparens-mode
  :config
  (use-package smartparens-config
    :ensure nil
    :demand))

(use-package prog-mode
  :ensure nil
  :hook (prog-mode . prettify-symbols-mode)
  :preface (load (locate-file "pragmata.el" load-path) 'noerror))

;; ..................................................................... haskell
;; TODO

;; ......................................................................... nix
(use-package nix-mode
  :mode "\\.nix\\'")

;; ....................................................................... dhall
(use-package dhall-mode
  :disabled
  :mode "\\.dhall\\'")

;; ...................................................................... docker
;; TODO

;; ...................................................................... elixir
;; TODO

;; ...................................................................... shells
;; TODO

;; ..................................................................... clojure
;; TODO

;; ....................................................................... scala
(use-package ensime
  :mode ("\\.scala\\'" "\\.sc\\'" "\\.sbt\\'")
  :custom
  (setq ensime-search-interface 'ivy
	    ensime-startup-notification nil)
  :preface
  (defun ensime-gen-and-restart()
    "Regenerate `.ensime' file and restart the ensime server."
    (interactive)
    (progn
      (sbt-command ";ensimeConfig;ensimeConfigProject")
      (ensime-shutdown)
      (ensime))))

(use-package scala-mode
  :mode ("\\.scala\\'" "\\.sc\\'" "\\.sbt\\'")
  :diminish (scala-mode . " ")
  :interpreter
  ("scala" . scala-mode))


;; .......................................................................... js
;; TODO



;; .................................................................. restclient
(use-package restclient
  :diminish (restclient-mode . " ")
  :mode (("\\.http\\'" . restclient-mode)
	     ("\\.rest\\'" . restclient-mode)
	     ("\\.restclient\\'" . restclient-mode)))

;; ................................................................... terraform
;; todo

;; org ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁
;; TODO


;; builtins ▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁▁

;; ...................................................................... server
(use-package server
  :disabled
  :init (server-start))

;; ....................................................................... eldoc
(use-package eldoc
  :ensure nil
  :diminish (eldoc-mode . " ")
  :demand
  :hook ((emacs-lisp-mode . eldoc-mode)
	 (eshell-mode . eldoc-mode)
	 (gtags-mode . eldoc-mode))
  :config (eldoc-mode t))

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

;; ....................................................................... theme
(use-package gotham-theme
  :if window-system
  :init
  (add-to-list 'custom-theme-load-path
               (file-name-directory (locate-library "gotham-theme")))
  (load-theme 'gotham t))

(defun dark-theme ()
  "Load dark theme."
  (interactive)
  (load-theme 'gotham t))

(defun light-theme ()
  "Load light theme."
  (interactive)
  (load-theme 'whiteboard t))


;; ......................................................................... font
(when (display-graphic-p)
  (set-face-attribute 'default nil :height 180)
  (setq-default line-spacing 7)
  (set-frame-font "PragmataPro"))

;; .................................................................... unclutter
(setq  inhibit-startup-screen t
       initial-scratch-message nil
       make-backup-files nil
       frame-resize-pixelwise t
       pop-up-windows nil
       column-number-mode t
       confirm-kill-emacs 'yes-or-no-p
       epg-gpg-program "/run/current-system/sw/bin/gpg"
       echo-keystrokes 0.1
       visible-bell nil)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
;;(set-frame-parameter nil 'undecorated t)

;; ............................................................. fix awkwardness
(fset 'yes-or-no-p 'y-or-n-p)

(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))

;;; ▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂▂
;;; default.el ends here
