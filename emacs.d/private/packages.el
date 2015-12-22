(setq magit-gh-issues-packages
    '(
      (magit-gh-issues :location (recipe
                                       :fetcher github
                                       :repo "domtronn/magit-gh-issues.el"))
     )
)

(defun magit-gh-issues-excluded-packages '())


(defun magit-gh-issues/init-magit-gh-issues ()
  (spacemacs|use-package-add-hook magit
      :pre-config
      (progn
        (use-package magit-gh-issues
          :init
          (progn
              (defun turn-on-magit-gh-issues ()
                                            (magit-gh-issues-mode 1))
              (add-hook 'magit-mode-hook 'turn-on-magit-gh-issues))
          :config
          (spacemacs|diminish magit-gh-issues-mode "#")))))
