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
  (message "Syncing...")
  (sbt:paste-region (point-min)(point-max) nil))

(defun sbt-worksheet-paste (rStart rEnd)
  "Pass marked region to console"
  (interactive "r")
  (message "Syncing: L%d-L%d" rStart rEnd)
  (sbt:paste-region rStart rEnd))

(spacemacs/set-leader-keys-for-minor-mode 'sbt-worksheet-mode
  ";" 'sbt-worksheet-paste)

(provide 'sbt-worksheet-mode)
