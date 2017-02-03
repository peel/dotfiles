(defun scalafmt/format-file ()
  "Run scalafmt on the current file"
  (interactive)
  (let ((default-directory (projectile-project-root))
        (scalafmt-cmd (format "scalafmt --config %s -i -f %s"
                              (shell-quote-argument ".scalafmt.conf")
                              (shell-quote-argument (buffer-file-name)))))
    (message "Running %s..." scalafmt-cmd)
    (shell-command scalafmt-cmd)
    (revert-buffer t t t))
  (spacemacs/set-leader-keys-for-major-mode 'scala-mode
    "rf" 'scalafmt/format-file))

(provide scalafmt)
