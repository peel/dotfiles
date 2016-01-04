;;; magit-gh-issues.el --- GitHub issues extension for Magit

;; Copyright (C) 2015  Dom Charlesworth

;; Author: Dom Charlesworth <dgc336@gmail.com>
;; Keywords: git tools
;; Package-Version: 20150926.2354
;; Version: 0.0.1
;; URL: https://github.com/domtronn/magit-gh-issues
;; Package-Requires: ((emacs "24") (gh "0.9.1") (magit "2.1.0") (pcache "0.2.3") (s "1.6.1") (markdown-mode "2.0.0"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'magit)
(require 'gh)
(require 'gh-issues)
(require 'gh-issue-comments)
(require 's)
(require 'browse-url)
(require 'markdown-mode)
(require 'auto-complete)
(require 'gravatar)

;;; Group Definitions
(defgroup magit-gh-issues nil
  "Customise the creation and display of GitHub issues in Magit."
  :group 'tools
  :group 'convenience)

(defcustom magit-gh-issues-ghi-executable (executable-find "ghi")
  "The executable with path for ghi.

By default, it performs `executable-find` to try and find ghi on your PATH."
  :group 'magit-gh-issues
  :type 'string)

(defcustom magit-gh-issues--id-max-width 2
  "The maximum width of the issue numbers as a string without the #."
  :group 'magit-gh-issues
  :type 'number)

(defcustom magit-gh-issues--comments-max-width 1
  "The maximum width of the comments number string."
  :group 'magit-gh-issues
  :type 'number)

(defcustom magit-gh-issues--assignee-max-width 3
  "The maximum width of the assignee name string.

E.g.  A max width of 3 and a username of `John_Doe` will display `@Joh`"
  :group 'magit-gh-issues
  :type 'number)

(defcustom magit-gh-issues--use-avatars nil
  "Whether or not to use the avatars for assignees.

When non-nil this displays the users GitHub avatar as an indicator
that they're assigned to an issue.  When nil, an '@' symbol will be
displayed instead to indicate this issue is assigned to someone.

This requires making another network request for avatars,
and although they get cached, you may want to avoid this.

*THIS VARIABLE IS CURRENTLY NOT SUPPORTED FULLY*
It is dependent on an update to `gh.el`"
  :group 'magit-gh-issues
  :type 'boolean)

;;; Face Definitions
(defface magit-gh-issues-login-face
  '((t :weight bold))
  "Face for GitHub logins/usernames"
  :group 'magit-faces)

;;; Variable Definitions
(defvar magit-gh-issues-body-format-hook '(magit-gh-issues--unmarkdown-body)
  "A list of hooks to run to format the body text of issues and comments.

By default it calls `magit-gh-issues--unmarkdown-body` which removes markdown
tags like underscores for italics and stars for bold to make the body
more readable")

(defcustom magit-gh-issues-title-template-string "%id% %comments% %assignee% %title% %labels%"
  "A template string for the title issues.

Items in '%..%' get replaced by the relevant GitHub item, they have to
match exactly.  Currently we support the following
  %id%       - The issue number
  %assignee% - The users GitHub login or avatar icon
  %comments% - The number of comments on an issue
  %title%    - The title of the issue
  %labels%   - The labels on the issue")

(defvar magit-gh-issues-format-alist
  '((id . (magit-gh-issues--make-id-string magit-gh-issues--id-max-width))
    (comments . (magit-gh-issues--make-comments-string magit-gh-issues--comments-max-width))
    (title . (identity))
    (labels . (magit-gh-issues--make-label-string))))

(defvar ac-source-gh-issues '((candidates . ac-gh-issues--candidates)
                               (prefix . "#\\S-*"))
  "An `auto-complete-mode` source that is built of the issues.")

(defvar ac-gh-issues--candidates '()
  "A list of `popup-menu` items used for `ac-source-gh-issues`.
It is built during the rendering/creation
of the issues section in `magit` and gets reset between renders.")

(defun magit-gh-issues--get-api ()
  "Get the `gh-issues-api` object."
  (gh-issues-api "api" :sync t :num-retries 1 :cache (gh-cache "cache")))

(defun magit-gh-issues--get-comments-api ()
  "Get the `gh-issue-comments-api` object."
  (gh-issue-comments-api "api" :sync t :num-retries 1 :cache (gh-cache "cache")))

(defun magit-gh-issues--get-users-api ()
  "Get the `gh-users-api` object."
  (gh-users-api "api" :sync t :num-retries 1 :cache (gh-cache "cache")))

(defun magit-gh-issues--parse-url (url)
  "Parse a remote URL into a cons cell of ( username . reponame )."
  (let* ((fixed-url (if (and (not (s-matches? "^[a-zA-Z_-]+://" url))
                             (s-matches? ":" url))
                        (concat "ssh://" (s-replace ":" "/" url))
                      url))
         (parsed-url (url-generic-parse-url fixed-url)))
    (let ((repo (s-match "/\\(.+\\)/\\([^/]+\\)/?$" (url-filename parsed-url))))
      (cons (cadr repo) (s-chop-suffix ".git" (cadr (cdr repo)))))))

(defun magit-gh-issues--guess-repo ()
  "Attempt to guess the repo object using remote."
  (let* ((remote (car (magit-git-lines "remote")))
         (url (magit-get "remote" remote "url")))
    (magit-gh-issues--parse-url url)))

(defmacro magit-gh-issues--make-face (name col &optional box)
  "Macro to make a face called NAME with foreground and box COL.

If BOX is non-nil then it will display a box around the face."
  (let ((face-name (intern name)))
    `(defface ,face-name
       '((t :foreground ,col :box ,box))
       ,(concat "Face for GitHub label " name ".")
       :group 'magit-faces)))

(defun magit-gh-issues--build-login-face-name (login)
  "Build the face name for a user LOGIN."
  (replace-regexp-in-string "[. ]" "-" (format "magit-gh-%s-face" login)))

(defun magit-gh-issues--make-login-face (login)
  "Build the LOGIN name face to have unique colours."
  (let* ((name (split-string login ""))
         (n (-reduce '+ (--map (* 5 (string-to-char it)) name)))
         (i (mod n 2000))
				 (f (/ 5.0 2000))
				 (r (mod (+ (* (sin (* f i)) 127) 128) 256))
				 (g (mod (+ (* (sin (+ (* f i) 2)) 127) 128) 256))
				 (b (mod (+ (* (sin (+ (* f i) 4)) 127) 128) 256))
         (col (format "#%02x%02x%02x" r g b)))
    (eval `(magit-gh-issues--make-face ,(magit-gh-issues--build-login-face-name login) ,col nil))))

(defun magit-gh--build-face-name (user proj name)
  "Build the name for a label face from USER PROJ and NAME.

The format should be `magit-gh-user-repo-label-name-face`"
  (replace-regexp-in-string
   "[. ]" "-"
   (format "magit-gh-%s-%s-label-%s-face" user proj name)))

(defun magit-gh-issues-get-avatar (url)
  "Get, and cache, the users avatar from URL."
  (if url
      (let ((url (concat url "&size=" (number-to-string (/ (face-attribute 'default :height) 10))))
            (result nil))
        (if (gravatar-cache-expired url)
            (with-current-buffer (url-retrieve-synchronously url)
              (url-store-in-cache (current-buffer))
              (let ((img (gravatar-data->image)))
                (propertize "*" 'display `((,@img :ascent center :relief 1)))))
          (with-temp-buffer
            (mm-disable-multibyte)
            (url-cache-extract (url-cache-create-filename url))
            (let ((img (gravatar-data->image)))
              (propertize "*" 'display `((,@img :ascent center :relief 1)))))))
    "  "))

(defun magit-gh-issues-get-at-assignee (assignee)
  "Create an indicator for the assignees from ASSIGNEE.

User names have a face created for them to uniquely identify them."
  (if (and assignee (not (string= assignee "unbound")))
        (progn
          (magit-gh-issues--make-login-face assignee)
          (propertize (format "@%s" (substring assignee 0 (min (- (length assignee) 1) magit-gh-issues--assignee-max-width)))
                     'face (intern (magit-gh-issues--build-login-face-name assignee))))
      (concat " " (make-string magit-gh-issues--assignee-max-width ? ))))

(defun magit-gh-issues--make-assignee-string (assignee)
  "Create the string for use in the ASSIGNEE part of the title."
  (if magit-gh-issues--use-avatars
      (magit-gh-issues-get-avatar (oref assignee :avatar-url))
    (magit-gh-issues-get-at-assignee (oref assignee :login))))

(defun magit-gh-issues-get-labels ()
  "Get the raw labels data from the gh library for repo."
  (let* ((api (magit-gh-issues--get-api))
         (repo (magit-gh-issues--guess-repo))
         (user (car repo))
         (proj (cdr repo)))
    (oref (gh-issues-label-list api user proj) :data)))

(defun magit-gh-issues-get-issue-comments (id)
  "Get the raw comments data from the gh library for issue #ID."
  (let* ((api (magit-gh-issues--get-comments-api))
         (repo (magit-gh-issues--guess-repo))
         (user (car repo))
         (proj (cdr repo)))
    (oref (gh-issue-comments-list api user proj id) :data)))

(defun magit-gh-issues-get-assignees ()
  "Get the list of assignees for a repo."
  (let* ((api (magit-gh-issues--get-users-api))
         (repo (magit-gh-issues--guess-repo))
         (result (magit-gh-issues--api-get-assignees api (car repo) (cdr repo))))
    (--mapcat (list (oref it :login)) (oref result :data))))

(defun magit-gh-issues-get-issues ()
  "Get the raw issues data from the gh library for repo."
  (let* ((api (magit-gh-issues--get-api))
         (repo (magit-gh-issues--guess-repo))
         (user (car repo))
         (proj (cdr repo)))
    (oref (gh-issues-issue-list api user proj) :data)))

(defun magit-gh-issues-purge-cache (&optional only)
  "Purge the cache of items matching a repos user and project name.

If ONLY is provided, then this is appended to the string match check
of the cache key."
  (mapc
   (lambda (api)
     (let* ((cache (oref api :cache))
            (repo (magit-gh-issues--guess-repo))
            (only (or only "")))
       (pcache-map cache
                   (lambda (k v)
                     (when (string-match
                            (format "/repos/%s/%s/%s" (car repo) (cdr repo) only)
                            (car k))
                       (pcache-invalidate cache k))))))
   `(,(magit-gh-issues--get-api) ,(magit-gh-issues--get-comments-api))))

(defun magit-gh--cached-p (type api user proj)
  "Check whether a repo has a cache for TYPE in API under USER & PROJ."
  (let ((cache-repo (format "/repos/%s/%s/%s" user proj type))
        (cached? nil))
    (pcache-map (oref api :cache)
                (lambda (key _) (when (equal (car key) cache-repo)
                                  (setq cached? t))))
    cached?))

(defun magit-gh-issues-cached-p (api user proj)
  "Check whether issues have been cached in API for USER & PROJ."
  (magit-gh--cached-p "issues" api user proj))

(defun magit-gh-labels-cached-p (api user proj)
  "Check whether labels have been cached in API for USER & PROJ."
  (magit-gh--cached-p "labels" api user proj))

(defun magit-gh-issues--make-label-faces (labels user proj)
  "Create a face for each label in LABELS namespaced by USER & PROJ."
  (dolist (label labels)
    (let* ((name (magit-gh--build-face-name user proj (oref label :name)))
           (color (oref label :color)))
      (eval `(magit-gh-issues--make-face ,name ,(concat "#" color) t)))))

(defun magit-gh-issues--label-list (labels)
  "Create a propertized list of LABELS using faces namespaced by USER & PROJ."
  (let* ((repo (magit-gh-issues--guess-repo))
         (user (car repo))
         (proj (cdr repo))
         (result '()))
    (dolist (label labels)
      (let* ((name (cdr (assoc 'name label)))
             (s (format "%s"
                        (propertize
                         (format " %s " name)
                         'face (intern (magit-gh--build-face-name user proj name))))))
        (setq result (append result (list s)))))
    result))

(defun magit-gh-issues--make-label-string (labels)
  "Create a propertized string of LABELS."
  (mapconcat 'format (magit-gh-issues--label-list labels) " "))

(defun magit-gh-issues--unmarkdown-body (body)
  "Remove markdown decoration like underscores from BODY."
  (let ((regexps '(("```.*$" . "")
                   ("\\[\\(.*?\\)\\](.*?)" . "\\1")
                   ("[_`*]\\(.*?\\)[*`_]" . "\\1")
                   ("
" . ""))))
    (mapc (lambda (regexp) (setq body (replace-regexp-in-string (car regexp) (cdr regexp) body))) regexps))
  (format "%s\n\n" body))

(defun magit-gh-issues--make-comments-string (comments)
  "Create the comment number string from COMMENTS.

COMMENTS should be a list of objects."
  (let* ((comments-s (if comments (number-to-string (length comments)) nil))
         (comments-padding (make-string (max (- magit-gh-issues--comments-max-width (length comments-s)) 0) ? ))
         (comments-p (propertize (if comments (format "%s" (length comments)) "") 'face 'magit-cherry-equivalent)))
    (concat comments-p comments-padding)))

(defun magit-gh-issues--make-id-string (id)
  "Create the propertized ID string."
  (let* ((id-s (number-to-string id))
         (id-padding (make-string (max (- magit-gh-issues--id-max-width (length id-s)) 0) ? ))
         (id-p (propertize (format "#%s" id-s) 'face 'magit-tag)))
    (concat id-p id-padding)))

(defun magit-gh-issues--make-heading-string (template id title comments labels)
  "Create the propertized string used for issue headers.

This function maps each of the arguments provided to a placeholder
in TEMPLATE.  It uses the `magit-gh-issues-format-alist` to create the string for
each element applied with the argument.  ID should be a number,
TITLE is a string title of the issue, COMMENTS is a list of the
comments and LABELS is a list of `gh-label` objects relating to the
issue.

The logic behind creating each of these strings should be found in a
`magit-gh-issues--make-variable-string` function.  TITLE is an exception in that it
just uses the `identity` function"
  (with-temp-buffer
    (insert template)
    (goto-char (point-min))
    (while (re-search-forward "%\\([a-z]+?\\)%" nil t)
      (let* ((m (match-data))
             (format (assoc (intern (match-string 1)) magit-gh-issues-format-alist))
             (default-value (symbol-value (caddr format)))
             (args (symbol-value (car format)))
             (f (cadr format))

             (result (if args (funcall f args) (make-string (or default-value 0) ? ))))
        (set-match-data m)
        (replace-match result)))
    (buffer-string)))

(defun magit-gh-issues--make-comment-heading-string (login time)
  "Create the propertized string used for comment headers.

It displays @LOGIN - TIME string and formats it from the create at label
of the comment."
  (magit-gh-issues--make-login-face login)
  (let* ((login-p (propertize (format "@%s" login) 'face (intern (magit-gh-issues--build-login-face-name login))))
         (time-string (format-time-string "%H:%M, %a %d %b, %Y" (date-to-time time)))
         (time-p (propertize time-string 'face 'magit-tag)))
    (format "  %s - %s" login-p time-string)))

(defun magit-gh-issues--make-body-string (body &optional offset)
  "Create the propertized string used for the issue BODY.
Providing an OFFSET will indent the region in the block."
  (mapc (lambda (f) (setq body (funcall f body)))
        magit-gh-issues-body-format-hook)
  (propertize
   (magit-gh-issues-format-text-in-rectangle body
     (min 100 (- (window-width) 5)) offset)
   'face 'magit-dimmed))

(defun magit-gh-issues-visit-issue ()
  "Get the URL meta data of the current issue and visit it in a browser."
  (interactive)
  (let* ((issue (cdr (assoc 'issue (magit-section-value (magit-current-section)))))
         (url (oref issue html-url)))
    (when (y-or-n-p (format "Would you like to open %s in a browser? " url))
      (browse-url url))))

;;;###autoload
(defun ac-gh-issues-setup ()
  "Setup the GitHub issues auto completion by add sources."
  (interactive)
  (add-to-list 'ac-sources 'ac-source-gh-issues))

(defun magit-gh-issues--reset-ac-candidates ()
  "Reset the variable `ac-gh-issues--candidates` to an empty list."
  (setq ac-gh-issues--candidates '()))

(defun magit-gh-issues--append-ac-candidate (s summary)
  "Add a `popup-menu-item` S with SUMMARY to the list of ac candidates."
  (add-to-list 'ac-gh-issues--candidates (popup-make-item s :summary summary)))

(defun magit-gh-issues-reload ()
  "Reload the issues by updating the cache of issues and labels.
It refreshes magit status to re-render the issues section."
  (interactive)
  (let ((repo (magit-gh-issues--guess-repo)))
    (if (not (and repo (car repo) (cdr repo)))
        (message "Remote repository is not configured or incorrect.")
      (magit-gh-issues-purge-cache "issues")
      (magit-gh-issues-purge-cache "labels")
      (magit-gh-issues-get-labels)
      (magit-gh-issues-get-issues)
      (magit-refresh))))

(defmethod magit-gh-issues--api-add-label ((api gh-issues-api) user repo
                            issue-or-issue-id label)
  (let ((issue-id (gh-issues--issue-id issue-or-issue-id)))
    (gh-api-authenticated-request
     api (gh-object-list-reader (oref api label-cls)) "POST"
     (format "/repos/%s/%s/issues/%s/labels" user repo issue-id)
     (list label))))

(defmethod magit-gh-issues--api-remove-label ((api gh-issues-api) user repo
                               issue-or-issue-id label)
  (let ((issue-id (gh-issues--issue-id issue-or-issue-id)))
    (gh-api-authenticated-request
     api (gh-object-list-reader (oref api label-cls)) "DELETE"
     (format "/repos/%s/%s/issues/%s/labels/%s" user repo issue-id label))))

(defmethod magit-gh-issues--api-add-assignee ((api gh-issues-api) user repo
                               issue-or-issue-id login)
  (let ((issue-id (gh-issues--issue-id issue-or-issue-id))
        (to-update `(("assignee" . ,login))))
    (gh-api-authenticated-request
     api (gh-object-list-reader (oref api label-cls)) "PATCH"
     (format "/repos/%s/%s/issues/%s" user repo issue-id)
     to-update)))

(defmethod magit-gh-issues--api-get-assignees ((api gh-users-api) user repo)
  (gh-api-authenticated-request
   api (gh-object-list-reader (oref api users-cls)) "GET"
   (format "/repos/%s/%s/assignees" user repo)))

(defun magit-gh-issues-process-sentinel (process event)
  "Default sentinel used by `magit-start-process' around PROCESS and EVENT."
  (when (memq (process-status process) '(exit signal))
    (setq event (substring event 0 -1))
    (when (string-match "^finished" event)
        (message (concat (capitalize (process-name process)) " finished"))
        (magit-process-finish process)
        (when (eq process magit-this-process)
          (setq magit-this-process nil))
        (unless (process-get process 'inhibit-refresh)
          (let ((inhibit-magit-revert (process-get process 'inhibit-revert))
                (command-buf (process-get process 'command-buf)))
            (if (buffer-live-p command-buf)
                (with-current-buffer command-buf
                  (magit-gh-issues-reload))
              (with-temp-buffer
                (setq default-directory (process-get process 'default-dir))
                (magit-gh-issues-reload))))))))

(define-derived-mode ghi-issue-mode ghi-comment-mode "Git Issue"
  "Major mode for editing GHI Issue files."
  :group 'magit
  (git-commit-setup-font-lock))

(define-derived-mode ghi-comment-mode markdown-mode "Git Comment"
  "Major mode for editing GHI Comment files."
  :group 'magit
  (add-hook 'with-editor-pre-cancel-hook
            '(lambda () (with-current-buffer (buffer-name) (goto-char (point-min)) (flush-lines ""))) nil t)
  (with-editor-mode 1))

(defun magit-gh-issues--reload-when-in-magit ()
  "Print message when back in magiit."
  (remove-hook 'after-change-major-mode-hook
               'magit-gh-issues--reload-when-in-magit))

(add-to-list 'auto-mode-alist '("GHI_ISSUE" . ghi-issue-mode))
(add-to-list 'auto-mode-alist '("GHI_COMMENT" . ghi-comment-mode))

(defun magit-gh-issues-close-issue ()
  "Close the current highlighted issue."
  (interactive)
  (magit-gh-issues-check-in-section '(issue comments comment))
  (when (eq 'issue (magit-section-type (magit-current-section)))
    (let* ((issue (cdr (assoc 'issue (magit-section-value (magit-current-section)))))
           (repo (magit-gh-issues--guess-repo))
           (id (oref issue :number)))
      (when (y-or-n-p (format "Are you sure you want to close issue #%s? " id))
        (oset issue :state "closed")
        (gh-issues-issue-update (magit-gh-issues--get-api) (car repo) (cdr repo) id issue)
        (magit-gh-issues-reload)))))

(magit-define-section-jumper issues "Issues")

(defun magit-gh-issues-collapse-issues ()
  "Collapse the issues section."
  (interactive)
  (magit-jump-to-issues)
  (magit-section-hide-children (magit-current-section)))

(defun magit-gh-issues--start-ghi (&rest args)
  "Start an editor process by calling `magit-gh-issues-start-ghi` with ARGS."
  (if magit-gh-issues-ghi-executable
      (let ((process
             (with-editor "GIT_EDITOR"
               (let ((magit-process-popup-time -1))
                 (run-hooks 'magit-pre-start-git-hook)
                 (apply #'magit-start-process magit-gh-issues-ghi-executable nil (-flatten args))))))
        (set-process-sentinel process 'magit-gh-issues-process-sentinel))
    (error "Could not find the `ghi` executable.  Have you got it installed?")))

(defun magit-gh-issues-check-in-section (sections)
  "Check that the user is in one of SECTIONS."
  (let ((section (magit-section-type (magit-current-section))))
    (when (--none? (eq it section) sections)
      (error "You are currently not in the right section"))))

(defun magit-gh-issues-open-issue ()
  "Open an issue using ghi."
  (interactive)
  (magit-gh-issues--start-ghi "open"))

(defun magit-gh-issues-comment-issue ()
  "Comment on the current issue using ghi."
  (interactive)
  (magit-gh-issues-check-in-section '(issue comments comment))
  (let* ((issue (cdr (assoc 'issue (magit-section-value (magit-current-section)))))
         (id (oref issue :number)))
    (magit-gh-issues--start-ghi "comment" (format "%s" id))))

(defun magit-gh-issues-add-label ()
  "Add a label from a popup menu to the current issue and refresh."
  (interactive)
  (let ((labels (mapcar
                 (lambda (l) (list (cons 'name (format "%s" (gh-issues--label-name l)))))
                 (magit-gh-issues-get-labels))))
    (magit-gh-issues--call-label-api labels "Add" 'magit-gh-issues--api-add-label)))

(defun magit-gh-issues-remove-label ()
  "Remove a label from a popup menu to the current issue and refresh."
  (interactive)
  (let ((labels (cdr (assoc 'labels (magit-section-value (magit-current-section))))))
    (when labels
      (magit-gh-issues--call-label-api labels "Remove" 'magit-gh-issues--api-remove-label))))

(defun magit-gh-issues--call-label-api (labels prompt-prefix f)
  "Prompt LABELS with PROMPT-PREFIX and perform the API call F for the labels of the current issue."
  (magit-gh-issues-check-in-section '(issue comments comment))
  (let* ((repo (magit-gh-issues--guess-repo))
         (prompt (magit-gh-issues--label-list labels)))
    (let* ((label-p (completing-read (format "%s Label: " prompt-prefix) prompt))
           (label (replace-regexp-in-string "^ \\(.*\\) $" "\\1" (format "%s" label-p)))
           (issue (cdr (assoc 'issue (magit-section-value (magit-current-section)))))
           (url (oref issue :url))
           (id (oref issue :number)))
      (when label
        (funcall f (magit-gh-issues--get-api) (car repo) (cdr repo) id label)
        (magit-gh-issues-reload)))))

(defun magit-gh-issues--call-assign-api (assignee)
  "Perform the call to the assign with the ASSIGNEE and refresh."
  (let* ((repo (magit-gh-issues--guess-repo))
         (issue (cdr (assoc 'issue (magit-section-value (magit-current-section)))))
         (id (oref issue :number)))
    (oset issue :assignee (make-instance gh-user :login assignee))
    (gh-issues-issue-update (magit-gh-issues--get-api) (car repo) (cdr repo) id issue)
    (magit-gh-issues-reload)))

(defun magit-gh-issues-assign ()
  "Assign an assignee to the current issue and refresh."
  (interactive)
  (magit-gh-issues-check-in-section '(issue comments comment))
  (let* ((assignees (magit-gh-issues-get-assignees))
         (assignee-p (completing-read "Assign To: " assignees)))
    (magit-gh-issues--call-assign-api assignee-p)))

(defun magit-gh-issues-unassign ()
  "Unassign all assignees from the current issue and refresh."
  (interactive)
  (magit-gh-issues-check-in-section '(issue comments comment))
  (magit-gh-issues--call-assign-api nil))

(defun magit-gh-issues-insert-issues ()
  "Insert the actual issues sections into magit."
  (let* ((repo (magit-gh-issues--guess-repo))
         (user (car repo))
         (proj (cdr repo))
         (api (magit-gh-issues--get-api))
         (issues-cached? (magit-gh-issues-cached-p api user proj))
         (issues (when issues-cached? (magit-gh-issues-get-issues)))
         (labels-cached? (magit-gh-labels-cached-p api user proj))
         (labels (when labels-cached? (magit-gh-issues-get-labels))))

    (magit-gh-issues--reset-ac-candidates)

    (when (> (length labels) 0)
      (magit-gh-issues--make-label-faces labels user proj))

    (when (or (not issues-cached?) (> (length issues) 0))
      (magit-insert-section (issues)
        (magit-insert-heading "Issues:")
        (dolist (issue issues)
          (let* ((id (oref issue :number))
                 (body (oref issue :body))
                 (labels (oref issue :labels))
                 (assignee (oref issue :assignee))
                 (comments (magit-gh-issues-get-issue-comments id)))
            (magit-insert-section (issue `((issue . ,issue) (labels . ,labels)) t)
              (let ((templates (split-string magit-gh-issues-title-template-string "%assignee%")))
                (insert (magit-gh-issues--make-heading-string (car templates) id (oref issue :title) comments labels))
                (when (cadr templates)
                  (insert (magit-gh-issues--make-assignee-string assignee))
                  (insert (magit-gh-issues--make-heading-string (cadr templates) id (oref issue :title) comments  labels))))

              (magit-insert-heading)

              (magit-gh-issues--append-ac-candidate (format "#%s" id) (oref issue :title))

              (when body
                (magit-insert (magit-gh-issues--make-body-string body)))
              (when comments
                (magit-insert-section (comments comments t)
                  (magit-insert-heading "Comments:")
                  (dolist (comment comments)
                    (let ((body (oref comment :body))
                          (user (oref (oref comment :user) :login))
                          (time (oref comment :created_at))
                          (comment-indent (make-string 4 ? )))
                      (magit-insert-section (comment `((issue . ,issue)))
                        (magit-insert-heading (magit-gh-issues--make-comment-heading-string user time))
                        (magit-insert (concat comment-indent (magit-gh-issues--make-body-string body comment-indent)))))))
                (magit-insert "\n")))))
        (when (> (length issues) 0)
          (insert "\n") t)
        (when (not issues-cached?)
          (insert "Fetch issues by pressing `I g`\n\n") t)))))

(defun magit-gh-issues-format-text-in-rectangle (text width &optional indent)
  "Wrap a block of TEXT with a maximum WIDTH and INDENT."
  (with-temp-buffer
    (insert
     (replace-regexp-in-string "    $" ""
                               (replace-regexp-in-string "\n" (concat "\n" indent)
                                       text)))
    (goto-char (+ (point-min) width))
    (while (< (point) (point-max))
      (backward-word)
      (newline)
      (when indent (insert indent))
      (search-forward "\n" (+ (point) width) t)
      (goto-char (+ (point) width)))
    (format "%s" (buffer-substring (point-min) (point-max)))))

;;; Keybinding definitions
(defvar magit-issue-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-gh-issues-visit-issue)
    (define-key map [C-return] 'magit-gh-issues-visit-issue)
    (define-key map "l" 'magit-gh-issues-popup-label)
    (define-key map "a" 'magit-gh-issues-popup-assign)
    (define-key map "c" 'magit-gh-issues-comment-issue)
    (define-key map "k" 'magit-gh-issues-close-issue)
    map)
  "Keymap for `issues` section.")

(defvar magit-comment-section-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\r" 'magit-gh-issues-visit-issue)
    (define-key map [C-return] 'magit-gh-issues-visit-issue)
    (define-key map "c" 'magit-gh-issues-comment-issue)
    map)
  "Keymap for `comment` section.")

(define-key magit-status-mode-map (kbd "I") 'magit-gh-issues-popup)

;;;###autoload (autoload 'magit-gh-issues-popup "magit" nil t)
(magit-define-popup magit-gh-issues-popup
  "Popup console for GitHub Issues commands."
  'magit-commands nil nil
  :actions  '("Global Commands"
              (?o "Open a new Issue"    magit-gh-issues-open-issue)
              (?z "Collapse Issues section"   magit-gh-issues-collapse-issues)
              (?g "Reload all issues from GitHub"  magit-gh-issues-reload)
              "Local Commands (run whilst focusing on an issue)"
              (?c "Comment on current Issue" magit-gh-issues-comment-issue)
              (?k "Close current Issue" magit-gh-issues-close-issue)
              (?l "Label Commands" magit-gh-issues-popup-label)
              (?a "Assignment Commands" magit-gh-issues-popup-assign)
              "\
 RET    Visit the current issue" nil)
  :max-action-columns 4)

(magit-define-popup magit-gh-issues-popup-label
  "Popup console for GitHub Issues label commands."
  'magit-commands nil nil
  :actions '("Label Commands"
             (?a "Add Label" magit-gh-issues-add-label)
             (?k "Remove Label" magit-gh-issues-remove-label)))

(magit-define-popup magit-gh-issues-popup-assign
  "Popup console for GitHub Issues Assignment commands."
  'magit-commands nil nil
  :actions '("Assignment Commands"
             (?a "Assign / Edit Assignees" magit-gh-issues-assign)
             (?k "Unassign" magit-gh-issues-unassign)))

;;;###autoload
(define-minor-mode magit-gh-issues-mode "GitHub Issues support for Magit using gh"
  :lighter " ghi"
  :require 'magit-gh-issues
  (or (derived-mode-p 'magit-mode)
      (error "This mode only makes sense when used with magit"))
  (if magit-gh-issues-mode
      (magit-add-section-hook
       'magit-status-sections-hook
       'magit-gh-issues-insert-issues
       'magit-insert-stashes)
    (remove-hook 'magit-status-sections-hook 'magit-gh-issues-insert-issues))
  (when (called-interactively-p 'any)
    (magit-refresh)))

(provide 'magit-gh-issues)
;; Local Variables:
;; indent-tabs-mode: nil
;; eval: (nameless-mode 1)
;; End:
;;; magit-gh-issues.el ends here
