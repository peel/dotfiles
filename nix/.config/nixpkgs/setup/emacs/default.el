;; -*- lexical-binding: t; -*-
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook (lambda ()
                             ;; restore after startup
                             (setq gc-cons-threshold 800000)))

;; initialize package
(require 'package)
(package-initialize 'noactivate)

(eval-when-compile
  (require 'use-package))

(setq-default use-package-always-defer t
              use-package-always-ensure t)

(fset 'yes-or-no-p 'y-or-n-p)

;; load packages
(use-package ace-window
  :bind (("M-o" . ace-window)
	 ("C-x o" . ace-window))
  :defer t)

(use-package company
  :bind ("<C-tab>" . company-complete)
  :diminish company-mode
  :commands (company-mode global-company-mode)
  :defer 1
  :config
  (global-company-mode))

(use-package counsel
  :commands (counsel-descbinds)
  :bind (([remap execute-extended-command] . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-rg)
         ("M-y" . counsel-yank-pop)))

(use-package ivy
  :defer 1
  :bind (("C-c C-r" . ivy-resume)
         ("C-x C-b" . ivy-switch-buffer)
         :map ivy-minibuffer-map
         ("C-j" . ivy-call))
  :diminish ivy-mode
  :commands ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy))))

(use-package swiper
  :defer t
  :bind ("C-s" . swiper))

(use-package editorconfig
  :defer 1
  :config (editorconfig-mode 1))

(use-package flycheck
  :defer 2
  :config (global-flycheck-mode))

(use-package highlight-symbol
  :defer t
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  :bind ("s-h" . highlight-symbol))

(use-package magit
  :defer
  :if (executable-find "git")
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-dispatch-popup))
  :init
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package projectile
  :commands projectile-mode
  :bind-keymap ("C-c p" . projectile-command-map)
  :defer 5
  :config
  (projectile-global-mode))

(use-package undo-tree
  :defer 5
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t)
  (setq undo-tree-visualizer-timestamps t)
  :bind ("s-/" . undo-tree-visualize))

(use-package which-key
  :defer 1
  :config (which-key-mode))

(setq dired-dwim-target t
      dired-recursive-deletes t
      dired-use-ls-dired nil
      delete-by-moving-to-trash t)

;; ui
(use-package gotham-theme
  :if window-system
  :init
  (add-to-list 'custom-theme-load-path
               (file-name-directory (locate-library "gotham-theme")))
  (load-theme 'gotham t))

;; font
(when (display-graphic-p)
  (set-face-attribute 'default nil :height 180)
  (setq-default line-spacing 7)
  (set-frame-font "PragmataPro"))

(setq inhibit-splash-screen t
      confirm-kill-emacs 'yes-or-no-p
      epg-gpg-program "/run/current-system/sw/bin/gpg"
      echo-keystrokes 0.1
      visible-bell nil)

(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
;;(set-frame-parameter nil 'undecorated t)
(setq frame-resize-pixelwise t)
;; always match parens
(show-paren-mode t)
(setq show-paren-delay 0)

(setq ring-bell-function (lambda ()
                           (invert-face 'mode-line)
                           (run-with-timer 0.1 nil 'invert-face 'mode-line)))
