;;; org-visibility-test.el --- Test org-visibility.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;;; Copyright (C) 2021 Kyle W T Sherman
;;
;; Author: Kyle W T Sherman <kylewsherman@gmail.com>
;; Created: 2021-07-17
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc.,
;; 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
;;
;;; Commentary:
;;
;; Run all tests via:
;;
;;   (ert-run-tests-batch)

;;; Code:

(require 'org-visibility)
(require 'ert)

(defun org-visibility-test-run-test (test files)
  "Setup test environment, run TEST using FILES, then restore environment."
  (org-visibility-enable-hooks)
  (let ((org-startup-folded 'showeverything)
        (org-odd-levels-only t)
        (enable-local-variables :all)
        (enable-local-eval t)
        (state-file org-visibility-state-file)
        (include-paths org-visibility-include-paths)
        (exclude-paths org-visibility-exclude-paths)
        (temp-state-file (make-temp-file "org-visibility-test-state-file-"))
        errors)
    (setq org-visibility-state-file temp-state-file
          org-visibility-include-paths '()
          org-visibility-exclude-paths '())
    (unwind-protect
        (setq errors (remove nil (apply #'append (nreverse (funcall test)))))
      (progn
        (setq org-visibility-state-file state-file
              org-visibility-include-paths include-paths
              org-visibility-exclude-paths exclude-paths)
        (mapc
         (lambda (x) (when (file-exists-p x) (delete-file x)))
         files)
        (delete-file temp-state-file)))
    (should (not errors))))

(defun org-visibility-test-create-org-file (&optional local-var-visbility)
  "Create temporary `org-mode' file to test with.

If LOCAL-VAR-VISBILITY is non-nil, set local variable
`org-visibility' to LOCAL-VAR-VISBILITY."
  (let ((file (make-temp-file "org-visibility-test-" nil ".org")))
    (with-temp-file file
      (insert "* Heading 1")
      (newline)
      (insert "*** Heading 1.2")
      (newline)
      (insert "Body text 1.2")
      (newline)
      (insert "And some more")
      (newline)
      (insert "* Heading 2")
      (newline)
      (insert "*** Heading 2.1")
      (newline)
      (insert "***** Heading 2.1.1")
      (newline)
      (insert "Body text 2.1.1")
      (newline)
      (insert "*** Heading 2.2")
      (newline)
      (insert "Body text 2.2")
      (newline)
      (insert "* Heading 3")
      (newline)
      (insert "Body text 3")
      (newline)
      (when local-var-visbility
        (newline)
        ;; concat is used to prevent emacs from trying to set local variables on this file
        (insert (concat ";; Local " "Variables:"))
        (newline)
        (insert (format ";; org-visibility: %s" local-var-visbility))
        (newline)
        (insert ";; End:")
        (newline)))
    file))

(defun org-visibility-test-cycle-outline ()
  "Hide all sublevels then cycle Heading 2."
  (outline-hide-sublevels 1)
  (goto-char (point-min))
  (forward-line 4)
  (org-cycle))

(defun org-visibility-test-check-visible-lines (lines)
  "Test that all LINES are visible, and no others, in current
buffer.

Return list of errors, or nil, if none."
  (let (errors)
    (goto-char (point-min))
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
        (if (member (line-number-at-pos) lines)
            (unless (not (invisible-p (point)))
              (push (format "Line not visible: %s" line) errors))
          (unless (invisible-p (point))
            (push (format "Line visible: %s" line) errors)))
        (forward-line 1)))
    (nreverse errors)))

(defun org-visibility-test-check-state-file-entries (count)
  "Test that `org-visibility-state-file' has COUNT entries.

Return a list of one error, or nil, if correct."
  (with-temp-buffer
    (insert-file-contents org-visibility-state-file)
    (let ((entries (condition-case nil (length (read (buffer-string))) ('error 0))))
      (if (= entries count)
          nil
        (list (format "State file entry count: %s (expected %s)" entries count))))))

(defun org-visibility-test-decrease-state-file-timestamps (days)
  "Subtract DAYS from all timestamps in `org-visibility-state-file'."
  (let ((data (and (file-exists-p org-visibility-state-file)
                   (ignore-errors
                     (with-temp-buffer
                       (insert-file-contents org-visibility-state-file)
                       (read (buffer-substring-no-properties (point-min) (point-max))))))))
    (when data
      (dolist (x data)
        (let ((ts (format-time-string
                   "%FT%T%Z"
                   (encode-time
                    (decoded-time-add (decode-time (date-to-time (cadr x)))
                                      (make-decoded-time :day (- days)))))))
          (setcdr x (cons ts (cddr x)))))
      (with-temp-file org-visibility-state-file
        (insert (format "%S\n" data))))))

(defun org-visibility-test-check-dirty-status (is-dirty)
  "Test that `org-visibility-dirty' is IS-DIRTY.

Return a list of one error, or nil, if correct."
  (if (eq org-visibility-dirty is-dirty)
      nil
    (list (format "Dirty flag: %s (expect %s)" org-visibility-dirty is-dirty))))

;;; Tests

(ert-deftest org-visibility-test-test-no-persistence ()
  "Test no visibility persistence."
  (let ((file (org-visibility-test-create-org-file))
        errors)
    (org-visibility-test-run-test
     (lambda ()
       (find-file file)
       (org-visibility-test-cycle-outline)
       (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
       (kill-buffer (current-buffer))
       (find-file file)
       (push (org-visibility-test-check-visible-lines '(1 2 3 4 5 6 7 8 9 10 11 12)) errors)
       (kill-buffer (current-buffer))
       errors)
     (list file))))

(ert-deftest org-visibility-test-test-no-persistence-with-local-var-nil ()
  "Test no visibility persistence using local var
`org-visibility' set to nil."
  (let ((file (org-visibility-test-create-org-file "nil"))
        errors)
    (org-visibility-test-run-test
     (lambda ()
       (find-file file)
       (org-visibility-test-cycle-outline)
       (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
       (kill-buffer (current-buffer))
       (find-file file)
       (push (org-visibility-test-check-visible-lines '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18)) errors)
       (kill-buffer (current-buffer))
       errors)
     (list file))))

(ert-deftest org-visibility-test-test-no-persistence-with-local-var-never ()
  "Test no visibility persistence using local var
`org-visibility' set to never."
  (let ((file (org-visibility-test-create-org-file "never"))
        errors)
    (org-visibility-test-run-test
     (lambda ()
       (let ((org-visibility-include-paths (list file)))
         (find-file file)
         (org-visibility-test-cycle-outline)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (find-file file)
         (push (org-visibility-test-check-visible-lines '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18)) errors)
         (kill-buffer (current-buffer)))
       errors)
     (list file))))

(ert-deftest org-visibility-test-test-persistence-with-local-var-t ()
  "Test visibility persistence using local var `org-visibility'
set to t."
  (let ((file (org-visibility-test-create-org-file "t"))
        errors)
    (org-visibility-test-run-test
     (lambda ()
       (find-file file)
       (org-visibility-test-cycle-outline)
       (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
       (kill-buffer (current-buffer))
       (push (org-visibility-test-check-state-file-entries 1) errors)
       (find-file file)
       (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
       (kill-buffer (current-buffer))
       errors)
     (list file))))

(ert-deftest org-visibility-test-test-persistence-with-include-paths ()
  "Test visibility persistence using include paths."
  (let ((file (org-visibility-test-create-org-file))
        errors)
    (org-visibility-test-run-test
     (lambda ()
       (let ((org-visibility-include-paths (list file)))
         (find-file file)
         (org-visibility-test-cycle-outline)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (push (org-visibility-test-check-state-file-entries 1) errors)
         (find-file file)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer)))
       errors)
     (list file))))

(ert-deftest org-visibility-test-test-persistence-with-include-regexps ()
  "Test visibility persistence using include regular expressions."
  (let ((file (org-visibility-test-create-org-file))
        errors)
    (org-visibility-test-run-test
     (lambda ()
       (let ((org-visibility-include-regexps (list "\\.org\\'")))
         (find-file file)
         (org-visibility-test-cycle-outline)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (push (org-visibility-test-check-state-file-entries 1) errors)
         (find-file file)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer)))
       errors)
     (list file))))

(ert-deftest org-visibility-test-test-no-persistence-with-include-exclude-paths ()
  "Test no visibility persistence using include and exclude paths."
  (let ((file (org-visibility-test-create-org-file))
        errors)
    (org-visibility-test-run-test
     (lambda ()
       (let ((org-visibility-include-paths (list (file-name-directory file)))
             (org-visibility-exclude-paths (list file)))
         (find-file file)
         (org-visibility-test-cycle-outline)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (find-file file)
         (push (org-visibility-test-check-visible-lines '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18)) errors)
         (kill-buffer (current-buffer)))
       errors)
     (list file))))

(ert-deftest org-visibility-test-test-no-persistence-with-include-exclude-regexps ()
  "Test no visibility persistence using include and exclude regular expressions."
  (let ((file (org-visibility-test-create-org-file))
        errors)
    (org-visibility-test-run-test
     (lambda ()
       (let ((org-visibility-include-regexps (list "\\.org\\'"))
             (org-visibility-exclude-regexps (list "\\.org\\'")))
         (find-file file)
         (org-visibility-test-cycle-outline)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (find-file file)
         (push (org-visibility-test-check-visible-lines '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18)) errors)
         (kill-buffer (current-buffer)))
       errors)
     (list file))))

(ert-deftest org-visibility-test-test-no-persistence-with-fundamental-mode-and-local-var-t ()
  "Test no visibility persistence using `fundamental-mode' and
local var `org-visibility' set to t."
  (let ((file (org-visibility-test-create-org-file "t"))
        errors)
    (org-visibility-test-run-test
     (lambda ()
       (find-file file)
       (org-visibility-test-cycle-outline)
       (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
       (fundamental-mode)
       (kill-buffer (current-buffer))
       (find-file file)
       (push (org-visibility-test-check-visible-lines '(1 5 11)) errors)
       (kill-buffer (current-buffer))
       errors)
     (list file))))

(ert-deftest org-visibility-test-test-no-persistence-with-fundamental-mode-and-include-paths ()
  "Test no visibility persistence using `fundamental-mode' and
include paths."
  (let ((file (org-visibility-test-create-org-file))
        errors)
    (org-visibility-test-run-test
     (lambda ()
       (let ((org-visibility-include-paths (list file)))
         (find-file file)
         (org-visibility-test-cycle-outline)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (fundamental-mode)
         (kill-buffer (current-buffer))
         (find-file file)
         (push (org-visibility-test-check-visible-lines '(1 5 11)) errors)
         (kill-buffer (current-buffer)))
       errors)
     (list file))))

(ert-deftest org-visibility-test-test-no-persistence-with-fundamental-mode-and-include-regexps ()
  "Test no visibility persistence using `fundamental-mode' and
include regular expressions."
  (let ((file (org-visibility-test-create-org-file))
        errors)
    (org-visibility-test-run-test
     (lambda ()
       (let ((org-visibility-include-regexps (list "\\.org\\'")))
         (find-file file)
         (org-visibility-test-cycle-outline)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (fundamental-mode)
         (kill-buffer (current-buffer))
         (find-file file)
         (push (org-visibility-test-check-visible-lines '(1 5 11)) errors)
         (kill-buffer (current-buffer)))
       errors)
     (list file))))

(ert-deftest org-visibility-test-test-maximum-tracked-files ()
  "Test visibility persistence expires when
`org-visibility-maximum-tracked-files' is exceeded."
  (let ((file1 (org-visibility-test-create-org-file))
        (file2 (org-visibility-test-create-org-file))
        errors)
    (org-visibility-test-run-test
     (lambda ()
       (let ((org-visibility-maximum-tracked-files 1)
             (org-visibility-include-paths (list file1 file2)))
         (find-file file1)
         (org-visibility-test-cycle-outline)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (push (org-visibility-test-check-state-file-entries 1) errors)
         (find-file file2)
         (org-visibility-test-cycle-outline)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (push (org-visibility-test-check-state-file-entries 1) errors)
         (find-file file2)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (find-file file1)
         (push (org-visibility-test-check-visible-lines '(1 5 11)) errors)
         (kill-buffer (current-buffer)))
       errors)
     (list file1 file2))))

(ert-deftest org-visibility-test-test-maximum-tracked-files-2 ()
  "Test visibility persistence expires when
`org-visibility-maximum-tracked-files' is exceeded."
  (let ((file1 (org-visibility-test-create-org-file))
        (file2 (org-visibility-test-create-org-file))
        (file3 (org-visibility-test-create-org-file))
        errors)
    (org-visibility-test-run-test
     (lambda ()
       (let ((org-visibility-maximum-tracked-files 2)
             (org-visibility-include-paths (list file1 file2 file3)))
         (find-file file1)
         (org-visibility-test-cycle-outline)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (push (org-visibility-test-check-state-file-entries 1) errors)
         (find-file file2)
         (org-visibility-test-cycle-outline)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (push (org-visibility-test-check-state-file-entries 2) errors)
         (find-file file3)
         (org-visibility-test-cycle-outline)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (push (org-visibility-test-check-state-file-entries 2) errors)
         (find-file file3)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (find-file file2)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (org-visibility-force-save)
         (kill-buffer (current-buffer))
         (push (org-visibility-test-check-state-file-entries 2) errors)
         (find-file file1)
         (push (org-visibility-test-check-visible-lines '(1 5 11)) errors)
         (org-visibility-test-cycle-outline)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (push (org-visibility-test-check-state-file-entries 2) errors)
         (find-file file1)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (find-file file2)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (find-file file3)
         (push (org-visibility-test-check-visible-lines '(1 5 11)) errors)
         (kill-buffer (current-buffer)))
       errors)
     (list file1 file2 file3))))

(ert-deftest org-visibility-test-test-maximum-tracked-days ()
  "Test visibility persistence expires when
`org-visibility-maximum-tracked-days' is exceeded."
  (let ((file1 (org-visibility-test-create-org-file))
        (file2 (org-visibility-test-create-org-file))
        errors)
    (org-visibility-test-run-test
     (lambda ()
       (let ((org-visibility-maximum-tracked-days 3)
             (org-visibility-include-paths (list file1 file2)))
         (find-file file1)
         (org-visibility-test-cycle-outline)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (push (org-visibility-test-check-state-file-entries 1) errors)
         (org-visibility-test-decrease-state-file-timestamps 2)
         (find-file file2)
         (org-visibility-test-cycle-outline)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (push (org-visibility-test-check-state-file-entries 2) errors)
         (org-visibility-test-decrease-state-file-timestamps 2)
         (find-file file2)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (org-visibility-force-save)
         (kill-buffer (current-buffer))
         (push (org-visibility-test-check-state-file-entries 1) errors)
         (find-file file1)
         (push (org-visibility-test-check-visible-lines '(1 5 11)) errors)
         (kill-buffer (current-buffer)))
       errors)
     (list file1 file2))))

(ert-deftest org-visibility-test-test-maximum-tracked-days-2 ()
  "Test visibility persistence expires when
`org-visibility-maximum-tracked-days' is exceeded."
  (let ((file1 (org-visibility-test-create-org-file))
        (file2 (org-visibility-test-create-org-file))
        (file3 (org-visibility-test-create-org-file))
        errors)
    (org-visibility-test-run-test
     (lambda ()
       (let ((org-visibility-maximum-tracked-days 5)
             (org-visibility-include-paths (list file1 file2 file3)))
         (find-file file1)
         (org-visibility-test-cycle-outline)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (push (org-visibility-test-check-state-file-entries 1) errors)
         (org-visibility-test-decrease-state-file-timestamps 2)
         (find-file file2)
         (org-visibility-test-cycle-outline)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (push (org-visibility-test-check-state-file-entries 2) errors)
         (org-visibility-test-decrease-state-file-timestamps 2)
         (find-file file3)
         (org-visibility-test-cycle-outline)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (push (org-visibility-test-check-state-file-entries 3) errors)
         (org-visibility-test-decrease-state-file-timestamps 2)
         (find-file file3)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (find-file file2)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (org-visibility-force-save)
         (kill-buffer (current-buffer))
         (push (org-visibility-test-check-state-file-entries 2) errors)
         (find-file file1)
         (push (org-visibility-test-check-visible-lines '(1 5 11)) errors)
         (org-visibility-test-cycle-outline)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (push (org-visibility-test-check-state-file-entries 3) errors)
         (org-visibility-test-decrease-state-file-timestamps 4)
         (find-file file1)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (org-visibility-force-save)
         (kill-buffer (current-buffer))
         (push (org-visibility-test-check-state-file-entries 2) errors)
         (find-file file2)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (find-file file3)
         (push (org-visibility-test-check-visible-lines '(1 5 11)) errors)
         (kill-buffer (current-buffer)))
       errors)
     (list file1 file2 file3))))

(ert-deftest org-visibility-test-test-clean-remove-file ()
  "Test `org-visibility-clean'."
  (let ((file1 (org-visibility-test-create-org-file))
        (file2 (org-visibility-test-create-org-file))
        errors)
    (org-visibility-test-run-test
     (lambda ()
       (let ((org-visibility-include-paths (list file1 file2)))
         (find-file file1)
         (org-visibility-test-cycle-outline)
         (kill-buffer (current-buffer))
         (find-file file2)
         (org-visibility-test-cycle-outline)
         (kill-buffer (current-buffer))
         (push (org-visibility-test-check-state-file-entries 2) errors)
         (delete-file file1)
         (org-visibility-clean)
         (push (org-visibility-test-check-state-file-entries 1) errors)
         (delete-file file2))
       errors)
     (list file1 file2))))

(ert-deftest org-visibility-test-test-clean-remove-include-path ()
  "Test `org-visibility-clean'."
  (let ((file1 (org-visibility-test-create-org-file))
        (file2 (org-visibility-test-create-org-file))
        errors)
    (org-visibility-test-run-test
     (lambda ()
       (let ((org-visibility-include-paths (list file1 file2)))
         (find-file file1)
         (org-visibility-test-cycle-outline)
         (kill-buffer (current-buffer))
         (find-file file2)
         (org-visibility-test-cycle-outline)
         (kill-buffer (current-buffer))
         (push (org-visibility-test-check-state-file-entries 2) errors)
         (setq org-visibility-include-paths (list file1))
         (org-visibility-clean)
         (push (org-visibility-test-check-state-file-entries 1) errors))
       errors)
     (list file1 file2))))

(ert-deftest org-visibility-test-test-force-save ()
  "Test `org-visibility-force-save'."
  (let ((file1 (org-visibility-test-create-org-file))
        (file2 (org-visibility-test-create-org-file))
        errors)
    (org-visibility-test-run-test
     (lambda ()
       (let ((org-visibility-include-paths (list file1 file2)))
         (find-file file1)
         (org-visibility-test-cycle-outline)
         (kill-buffer (current-buffer))
         (push (org-visibility-test-check-state-file-entries 1) errors)
         (find-file file2)
         (org-visibility-force-save)
         (push (org-visibility-test-check-state-file-entries 2) errors)
         (kill-buffer (current-buffer)))
       errors)
     (list file1 file2))))

(ert-deftest org-visibility-test-test-save-all-buffers ()
  "Test `org-visibility-save-all-buffers'."
  (let ((file1 (org-visibility-test-create-org-file))
        (file2 (org-visibility-test-create-org-file))
        errors)
    (org-visibility-test-run-test
     (lambda ()
       (let ((org-visibility-include-paths (list file1 file2)))
         (find-file file1)
         (org-visibility-test-cycle-outline)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (find-file file2)
         (org-visibility-test-cycle-outline)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (org-visibility-save-all-buffers)
         (push (org-visibility-test-check-state-file-entries 2) errors)
         (find-file file1)
         (kill-buffer (current-buffer))
         (find-file file2)
         (kill-buffer (current-buffer))
         (find-file file1)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer))
         (find-file file2)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer)))
       errors)
     (list file1 file2))))

(ert-deftest org-visibility-test-test-save-error-with-fundamental-mode-and-include-paths ()
  "Test save error thrown using `fundamental-mode' and include paths"
  (let ((file (org-visibility-test-create-org-file))
        errors)
    (org-visibility-test-run-test
     (lambda ()
       (let ((org-visibility-include-paths (list file))
             (error '("Expected `org-visibility-save' to throw an error")))
         (find-file file)
         (org-visibility-test-cycle-outline)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (fundamental-mode)
         (condition-case err
             (org-visibility-force-save)
           ('error
            (setq error nil)))
         (kill-buffer (current-buffer))
         (if error
             (nreverse (push error errors))
           errors)))
     (list file))))

(ert-deftest org-visibility-test-test-dirty ()
  "Test file dirty state from change."
  (let ((file (org-visibility-test-create-org-file))
        errors)
    (org-visibility-test-run-test
     (lambda ()
       (let ((org-visibility-include-paths (list file)))
         (find-file file)
         (push (org-visibility-test-check-dirty-status nil) errors)
         (goto-char (point-at-eol))
         (push (org-visibility-test-check-dirty-status nil) errors)
         (insert "A")
         (push (org-visibility-test-check-dirty-status t) errors)
         (save-buffer)
         (kill-buffer (current-buffer)))
       errors)
     (list file))))

(ert-deftest org-visibility-test-test-dirty-org-cycle ()
  "Test file dirty state from `org-cycle'."
  (let ((file (org-visibility-test-create-org-file))
        errors)
    (org-visibility-test-run-test
     (lambda ()
       (let ((org-visibility-include-paths (list file)))
         (find-file file)
         (push (org-visibility-test-check-dirty-status nil) errors)
         (outline-hide-sublevels 1)
         (push (org-visibility-test-check-dirty-status nil) errors)
         (forward-line 4)
         (push (org-visibility-test-check-dirty-status nil) errors)
         (org-cycle)
         (push (org-visibility-test-check-dirty-status t) errors)
         (save-buffer)
         (kill-buffer (current-buffer))
         (find-file file)
         (push (org-visibility-test-check-visible-lines '(1 5 6 9 11)) errors)
         (kill-buffer (current-buffer)))
       errors)
     (list file))))

;;; org-visibility-test.el ends here
