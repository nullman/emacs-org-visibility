;;; org-visibility.el --- Persistent org tree visibility -*- lexical-binding: t; -*-
;;
;;; Copyright (C) 2021 Kyle W T Sherman
;;
;; Author:   Kyle W T Sherman <kylewsherman at gmail dot com>
;; Created:  2021-07-17
;; Version:  1.0
;; Keywords: org-mode outline visibility persistence
;;
;; This file is not part of GNU Emacs.
;;
;; This is free software; you can redistribute it and/or modify it under the
;; terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Org Visibility is an Emacs package that adds the ability to persist (save
;; and load) the state of the visible sections of `org-mode' files.  The state
;; is saved when the file is saved or killed, and restored when the file is
;; loaded.
;;
;; Hooks are used to persist and restore org tree visibility upon loading and
;; saving org files.  Files are only considered if they meet one of the
;; following requirements:
;;
;; - They have the buffer local variable `org-visibility' set to t.
;;
;; - They are contained within one of the directories listed in
;;   `org-visibility-include-paths'.
;;
;; - They match one of the regular expressions listed in
;;   `org-visibility-include-regexps'.
;;
;; Files are removed from consideration if they meet one of the following
;; requirements (overriding the above include logic):
;;
;; - They have the buffer local variable `org-visibility' set to never.
;;
;; - They are contained within one of the directories listed in
;;   `org-visibility-exclude-paths'.
;;
;; - They match one of the regular expressions listed in
;;   `org-visibility-exclude-regexps'.
;;
;; Provides the following interactive functions:
;;
;;   `org-visibility-save'             - Save visibility state for current buffer
;;   `org-visibility-force-save'       - Save even if buffer has not been modified
;;   `org-visibility-save-all-buffers' - Save all buffers that qualify
;;   `org-visibility-load'             - Load a file and restore its visibility state
;;   `org-visibility-clean'            - Clean up `org-visibility-state-file'
;;   `org-visibility-enable-hooks'     - Enable all hooks
;;   `org-visibility-disable-hooks'    - Disable all hooks
;;
;;; Installation:
;;
;; Put `org-visibility.el' where you keep your elisp files and add something
;; like the following to your .emacs file:
;;
;;   ;; optionally change the location of the state file (not recommended)
;;   ;;(setq org-visibility-state-file `,(expand-file-name "/some/path/.org-visibility"))
;;
;;   ;; list of directories and files to automatically persist and restore visibility state of
;;   (setq org-visibility-include-paths `(,(file-truename "~/.emacs.d/init-emacs.org")
;;                                        ,(file-truename "~/org"))
;;
;;   ;; list of directories and files to not persist and restore visibility state of
;;   (org-visibility-exclude-paths `(,(file-truename "~/org/old")))
;;
;;   (require 'org-visibility)
;;
;;   ;; optionally set a keybinding to force save
;;   (bind-keys :map org-mode-map
;;                   ("C-x C-v" . org-visibility-force-save)) ; defaults to `find-alternative-file'
;;
;; Or, if using `use-package', add something like this instead:
;;
;;   (use-package org-visibility
;;     :bind (:map org-mode-map
;;                 ("C-x C-v" . org-visibility-force-save)) ; defaults to `find-alternative-file'
;;     :custom
;;     ;; list of directories and files to automatically persist and restore visibility state of
;;     (org-visibility-include-paths `(,(file-truename "~/.emacs.d/init-emacs.org")
;;                                     ,(file-truename "~/org"))))
;;     ;; list of directories and files to not persist and restore visibility state of
;;     (org-visibility-exclude-paths `(,(file-truename "~/org/old")))
;;
;;; Usage:
;;
;; Visibiliy state is automatically persisted on file save or kill, and
;; restored when loaded.  No user intervention is needed.  The user can,
;; however, call `org-visibility-force-save' to save the current visibiliy
;; state of a buffer before a file save or kill would automatically trigger it
;; next.
;;
;; Interactive commands:
;;
;; The `org-visibility-save' function saves the current buffer's visibility
;; state if it has been modified or had an `org-cycle' change, and either has
;; local variable `org-visibility' set to t, is in one of the
;; `org-visibility-include-paths' paths, or matches one of the
;; `org-visibility-include-regexps' regular expressions.
;;
;; The `org-visibility-force-save' function saves the current buffer's
;; visibility state if it either has local variable `org-visibility' set to t
;; or is in one of the `org-visibility-include-paths' paths.
;;
;; The `org-visibility-save-all-buffers' function saves the visibility state
;; for any modified buffers.
;;
;; The `org-visibility-load' function loads a file and restores its visibility
;; state if it either has local variable `org-visibility' set to t or is in
;; one of the `org-visibility-include-paths' paths.
;;
;; The `org-visibility-clean' function removes all missing or untracked files
;; from `org-visibility-state-file'.
;;
;; The `org-visibility-enable-hooks' function enables all `org-visibility'
;; hooks so that it works automatically.
;;
;; The `org-visibility-disable-hooks' function disables all `org-visibility'
;; hooks so that it is effectively turned off unless functions are manually
;; called.

;;; Code:

(defcustom org-visibility-state-file `,(expand-file-name ".org-visibility" user-emacs-directory)
  "File used to store org visibility state."
  :type 'string
  :group 'org-visibility)

(defcustom org-visibility-include-paths '()
  "List of directories and files to automatically persist and
restore visibility state of when saved and loaded."
  :type 'list
  :group 'org-visibility)

(defcustom org-visibility-exclude-paths '()
  "List of directories and files to not persist and restore
visibility state of when saved and loaded."
  :type 'list
  :group 'org-visibility)

(defcustom org-visibility-include-regexps '()
  "List of regular expressions that, when matched, will
automatically persist and restore visibility state of matching
directories and files when saved and loaded."
  :type 'list
  :group 'org-visibility)

(defcustom org-visibility-include-regexps '()
  "List of regular expressions that, when matched, will
automatically persist and restore visibility state of matching
directories and files when saved and loaded."
  :type 'list
  :group 'org-visibility)

(defvar-local org-visibility
  nil
  "File local varible to determine if buffer file visibility
state should be automatically persisted and restored.

If nil, this setting has no effect on determining buffer file
visibility state persistence.

If t, buffer file should have its visibility state automatically
persisted and restored.

If 'never, buffer file should never have its visibility state
automatically persisted and restored.  (This overrides the
behavior of `org-visibility-include-paths' and
`org-visibility-include-regexps'.)")

(defvar-local org-visibility-dirty
  nil
  "Non-nil if buffer has been modified since last visibility save.")

(defun buffer-file-checksum (&optional buffer)
  "Return checksum for BUFFER file or nil if file does not exist."
  (let* ((buffer (or buffer (current-buffer)))
         (file-name (buffer-file-name buffer)))
    (ignore-errors
      (car (split-string
            (if window-system-mac
                (shell-command-to-string (concat "md5 -r " file-name))
              (shell-command-to-string (concat "md5sum " file-name))))))))

(defun org-visibility-set (buffer visible-lines)
  "Set visibility state for BUFFER to VISIBLE-LINES and update
`org-visibility-state-file' with new state."
  (let ((data (and (file-exists-p org-visibility-state-file)
                   (ignore-errors
                     (with-temp-buffer
                       (insert-file-contents org-visibility-state-file)
                       (read (buffer-substring-no-properties (point-min) (point-max)))))))
        (file-name (buffer-file-name buffer))
        (checksum (buffer-file-checksum buffer)))
    (when file-name
      (setq data (delq (assoc file-name data) data)) ; remove previous value
      (setq data (append (list (list file-name checksum visible-lines)) data)) ; add new value
      (with-temp-file org-visibility-state-file
        (insert (format "%S\n" data)))
      (message "Set visibility state for %s" file-name))))

(defun org-visibility-get (buffer)
  "Return visibility state for BUFFER if found in `org-visibility-state-file'."
  (let ((data (and (file-exists-p org-visibility-state-file)
                   (ignore-errors
                     (with-temp-buffer
                       (insert-file-contents org-visibility-state-file)
                       (read (buffer-substring-no-properties (point-min) (point-max)))))))
        (file-name (buffer-file-name buffer))
        (checksum (buffer-file-checksum buffer)))
    (when file-name
      (let ((state (assoc file-name data)))
        (when (string= (cadr state) checksum)
          (message "Restored visibility state for %s" file-name)
          (caddr state))))))

(defun org-visibility-save-internal (&optional buffer noerror force)
  "Save visibility snapshot of org BUFFER.

If NOERROR is non-nil, do not throw errors.

If FORCE is non-nil, save even if file is not marked as dirty."
  (let ((buffer (or buffer (current-buffer)))
        (file-name (buffer-file-name buffer))
        (visible-lines '()))
    (with-current-buffer buffer
      (if (not (eq major-mode 'org-mode))
          (unless noerror
            (error "This function only works with `org-mode' files"))
        (if (not file-name)
            (unless noerror
              (error "There is no file associated with this buffer: %S" buffer))
          (when (or force org-visibility-dirty)
            (save-mark-and-excursion
              (goto-char (point-min))
              (while (not (eobp))
                (when (not (invisible-p (point)))
                  (push (point) visible-lines))
                (forward-visible-line 1)))
            (org-visibility-set buffer (nreverse visible-lines))
            (setq org-visibility-dirty nil)))))))

(defun org-visibility-load-internal (&optional buffer noerror)
  "Load visibility snapshot of org BUFFER.

If NOERROR is non-nil, do not throw errors."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (if (not (eq major-mode 'org-mode))
          (unless noerror
            (error "This function only works with `org-mode' files"))
        (if (not (buffer-file-name buffer))
            (unless noerror
              (error "There is no file associated with this buffer: %S" buffer))
          (let ((visible-lines (org-visibility-get buffer)))
            (save-mark-and-excursion
              (outline-hide-sublevels 1)
              (dolist (x visible-lines)
                (ignore-errors
                  (goto-char x)
                  (when (invisible-p (point))
                    (forward-char -1)
                    (org-cycle)))))
            (setq org-visibility-dirty nil)))))))

(defun org-visibility-check-file-path (file paths)
  "Return whether FILE is in one of the PATHS."
  (cl-do ((paths paths (cdr paths))
          (match nil))
      ((or (null paths) match) match)
    (let ((path (car paths))
          (file (file-truename file)))
      (when (>= (length file) (length path))
        (let ((part (substring file 0 (length path))))
          (when (string= part path)
            (setq match t)))))))

(defun org-visibility-check-file-include-exclude-paths (file)
  "Return whether FILE is in one of the paths in
`org-visibility-include-paths' and not in one of the paths in
`org-visibility-exclude-paths'."
  (and (org-visibility-check-file-path file org-visibility-include-paths)
       (not (org-visibility-check-file-path file org-visibility-exclude-paths))))

(defun org-visibility-check-buffer-file-path (buffer)
  "Return whether BUFFER's file is in one of the paths in
`org-visibility-include-paths' and not in one of the paths in
`org-visibility-exclude-paths'."
  (let ((file (buffer-file-name buffer)))
    (if file
        (org-visibility-check-file-include-exclude-paths file)
      nil)))

(defun org-visibility-check-buffer-file-persistance (buffer)
  "Return whether BUFFER's file's visibility should be persisted
and restored."
  (with-current-buffer buffer
    (case (if (boundp 'org-visibility) org-visibility nil)
      ('nil (org-visibility-check-buffer-file-path buffer))
      ('never nil)
      (t t))))

;;;###autoload
(defun org-visibility-clean ()
  "Remove any missing files from `org-visibility-state-file'."
  (interactive)
  (let ((data (and (file-exists-p org-visibility-state-file)
                   (ignore-errors
                     (with-temp-buffer
                       (insert-file-contents org-visibility-state-file)
                       (read (buffer-substring-no-properties (point-min) (point-max))))))))
    (setq data (cl-remove-if-not
                (lambda (x)
                  (let ((file (car x)))
                    (and (file-exists-p file)
                         (org-visibility-check-file-include-exclude-paths file))))
                data))

    (with-temp-file org-visibility-state-file
      (insert (format "%S\n" data)))
    (message "Visibility state file has been cleaned")))

;;;###autoload
(defun org-visibility-save (&optional noerror force)
  "Save visibility state if buffer has been modified."
  (interactive)
  (when (org-visibility-check-buffer-file-persistance (current-buffer))
    (org-visibility-save-internal (current-buffer) noerror force)))

;;;###autoload
(defun org-visibility-force-save ()
  "Save visibility state even if buffer has not been modified."
  (interactive)
  (org-visibility-save nil :force))

;;;###autoload
(defun org-visibility-save-all-buffers (&optional force)
  "Save visibility state for any modified buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (org-visibility-check-buffer-file-persistance buffer)
      (org-visibility-save-internal buffer :noerror force))))

;;;###autoload
(defun org-visibility-load (&optional file)
  "Load FILE or `current-buffer' and restore its visibility state."
  (interactive)
  (let ((buffer (if file (get-file-buffer file) (current-buffer))))
    (when (and buffer (org-visibility-check-buffer-file-persistance buffer))
      (org-visibility-load-internal buffer :noerror))))

(defun org-visibility-dirty ()
  "Set visibility dirty flag."
  (when (and (org-visibility-check-buffer-file-persistance (current-buffer))
             (eq major-mode 'org-mode))
    (setq org-visibility-dirty t)))

(defun org-visibility-dirty-org-cycle (state)
  "Set visibility dirty flag when `org-cycle' is called."
  ;; dummy check to prevent compiler warning
  (when (not (eq state 'INVALID-STATE))
    (org-visibility-dirty)))

;;;###autoload
(defun org-visibility-enable-hooks ()
  "Helper function to enable all `org-visibility' hooks."
  (interactive)
  (add-hook 'after-save-hook #'org-visibility-save :append)
  (add-hook 'kill-buffer-hook #'org-visibility-save :append)
  (add-hook 'kill-emacs-hook #'org-visibility-save-all-buffers :append)
  (add-hook 'find-file-hook #'org-visibility-load :append)
  (add-hook 'first-change-hook #'org-visibility-dirty :append)
  (add-hook 'org-cycle-hook #'org-visibility-dirty-org-cycle :append))

;;;###autoload
(defun org-visibility-disable-hooks ()
  "Helper function to disable all `org-visibility' hooks."
  (interactive)
  (remove-hook 'after-save-hook #'org-visibility-save)
  (remove-hook 'kill-buffer-hook #'org-visibility-save)
  (remove-hook 'kill-emacs-hook #'org-visibility-save-all-buffers)
  (remove-hook 'find-file-hook #'org-visibility-load)
  (remove-hook 'first-change-hook #'org-visibility-dirty)
  (remove-hook 'org-cycle-hook #'org-visibility-dirty-org-cycle))

;; enable all hooks
(org-visibility-enable-hooks)

(provide 'org-visibility)

;;; org-visibility.el ends here
