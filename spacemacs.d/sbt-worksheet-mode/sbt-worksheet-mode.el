;;;###autoload
(define-minor-mode sbt-worksheet-mode
  "Minor mode for Scala worksheet-like experience.
Syncs buffer's contents to initiated sbt console session."
  :lighter "~w"
  (if sbt-worksheet-mode
      (progn (make-local-variable 'after-save-hook)
             (add-hook 'after-save-hook 'sbt-worksheet-sync nil t))
      (kill-local-variable 'after-save-hook)))

(defun sbt-worksheet-sync ()
  "Pass current buffer to SBT REPL. The REPL has to be started first."
  (progn (message "Syncing...")
         (sbt:paste-region (point-min)(point-max) nil)))

(provide 'sbt-worksheet-mode)
