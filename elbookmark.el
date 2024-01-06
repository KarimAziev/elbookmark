;;; elbookmark.el --- Bookmark utils -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/elbookmark
;; Version: 0.1.0
;; Keywords: convenience
;; Package-Requires: ((emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Bookmark utils

;;; Code:

(require 'bookmark)
(require 'transient)


(defun elbookmark-cleanup-nonexisting ()
  "Remove nonexisting bookmarks."
  (interactive)
  (require 'bookmark)
  (dolist (cell bookmark-alist)
    (when-let ((filename (alist-get 'filename (cdr cell)))
               (bookmark-name (car cell)))
      (unless (file-exists-p filename)
        (when (yes-or-no-p (format "Removing bookmark %s in %s?" bookmark-name
                                   filename))
          (bookmark-delete bookmark-name))))))


(defun elbookmark-get-minibuffer-completion ()
  "Return the cons with completion candidate in the minibuffer and category."
  (pcase completing-read-function
    ('ivy-completing-read
     (when (and
            (fboundp 'ivy-state-current)
            (boundp 'ivy-last))
       (cons (ivy-state-current ivy-last)
             (when (minibufferp)
               (ignore-errors (completion-metadata-get
                               (completion-metadata
                                (buffer-substring-no-properties
                                 (minibuffer-prompt-end)
                                 (max
                                  (minibuffer-prompt-end)
                                  (point)))
                                minibuffer-completion-table
                                minibuffer-completion-predicate)
                               'category))))))
    (_ (when (and (minibufferp) minibuffer-completion-table)
         (pcase-let* ((`(,category . ,candidates)
                       (when (minibufferp)
                         (let* ((all (completion-all-completions
                                      (minibuffer-contents)
                                      minibuffer-completion-table
                                      minibuffer-completion-predicate
                                      (max 0 (- (point)
                                                (minibuffer-prompt-end)))))
                                (last (last all)))
                           (when last (setcdr last nil))
                           (cons
                            (ignore-errors (completion-metadata-get
                                            (completion-metadata
                                             (buffer-substring-no-properties
                                              (minibuffer-prompt-end)
                                              (max (minibuffer-prompt-end)
                                                   (point)))
                                             minibuffer-completion-table
                                             minibuffer-completion-predicate)
                                            'category))
                            all))))
                      (contents (minibuffer-contents))
                      (top (if
                               (test-completion contents
                                                minibuffer-completion-table
                                                minibuffer-completion-predicate)
                               contents
                             (let ((completions
                                    (completion-all-sorted-completions)))
                               (if (null completions)
                                   contents
                                 (concat
                                  (substring contents
                                             0
                                             (or
                                              (cdr (last completions)) 0))
                                  (car completions)))))))
           (let ((result
                  (cons (or (car (member top candidates)) top) category)))
             result))))))


(defun elbookmark-minibuffer-switch-to-transient ()
  "Switch to transient mode from minibuffer after a delay."
  (interactive)
  (when (minibufferp)
    (run-with-timer 0.1 nil
                    'elbookmark-transient-dwim)
    (abort-minibuffers)))

(defun elbookmark-preview ()
  "Preview selected bookmark before opening it."
  (interactive)
  (when-let ((x
              (or (car-safe (elbookmark-get-minibuffer-completion)))))
    (when (member x (when (fboundp 'bookmark-all-names)
                      (bookmark-all-names)))
      (if (minibuffer-selected-window)
          (with-minibuffer-selected-window
            (if (file-directory-p (when (fboundp 'bookmark-location)
                                    (bookmark-location x)))
                (message "Bookmark %s  is directory" x)
              (bookmark-jump x)))
        (bookmark-jump x)))))

(defvar elbookmark-other-wind nil)

(defvar elbookmark-minibuffer-keymap
  (let ((map
         (make-sparse-keymap)))
    (define-key map
                (kbd "C-c C-o")
                #'elbookmark-jump-in-other-window)
    (define-key map
                (kbd "C-.")
                #'elbookmark-minibuffer-switch-to-transient)
    (define-key map (kbd "C-j")
                #'elbookmark-preview)
    map))

(defun elbookmark-jump-in-other-window ()
  "Set flag for jump in other window and exit minibuffer."
  (interactive)
  (setq elbookmark-other-wind t)
  (when-let ((key (where-is-internal
                   'exit-minibuffer
                   minibuffer-mode-map
                   t t
                   t)))
    (execute-kbd-macro key)))

(defun elbookmark-setup-minibuffer ()
  "Setup minibuffer."
  (when (minibufferp)
    (use-local-map
     (let ((map
            (copy-keymap elbookmark-minibuffer-keymap)))
       (set-keymap-parent map
                          (current-local-map))
       map))))

;;;###autoload
(defun elbookmark-jump (&optional bookmark display-func)
  "Same as `bookmark-jump' but with additional command to jump in other window.
This command is bound to `C-c o'.

Jump to bookmark BOOKMARK (a point in some file).
You may have a problem using this function if the value of variable
`bookmark-alist' is nil.  If that happens, you need to load in some
bookmarks.  See help on function `bookmark-load' for more about
this.

If the file pointed to by BOOKMARK no longer exists, you will be asked
if you wish to give the bookmark a new location, and `bookmark-jump'
will then jump to the new location, as well as recording it in place
of the old one in the permanent bookmark record.

BOOKMARK is usually a bookmark name (a string).  It can also be a
bookmark record, but this is usually only done by programmatic callers.

If DISPLAY-FUNC is non-nil, it is a function to invoke to display the
bookmark."
  (interactive)
  (require 'bookmark)
  (setq elbookmark-other-wind nil)
  (setq bookmark
        (or bookmark
            (when (fboundp 'bookmark-completing-read)
              (minibuffer-with-setup-hook
                  (:append #'elbookmark-setup-minibuffer)
                (bookmark-completing-read "Jump to bookmark"
                                          bookmark-current-bookmark)))))
  (when bookmark
    (when-let ((loc (expand-file-name
                     (when (fboundp 'bookmark-location)
                       (bookmark-location bookmark)))))
      (if (and (file-directory-p loc))
          (let* ((tramp-archive-enabled nil))
            (funcall (if elbookmark-other-wind 'find-file
                       'find-file-other-window)
                     (completing-read "File: " (directory-files-recursively
                                                loc "[.]" nil))))
        (bookmark-maybe-historicize-string bookmark)
        (when elbookmark-other-wind
          (let ((wind (selected-window)))
            (select-window (or
                            (window-right wind)
                            (window-left wind)
                            (split-window-right)))))
        (when (fboundp 'bookmark--jump-via)
          (bookmark--jump-via
           bookmark
           (or display-func 'pop-to-buffer-same-window)))))))

;;;###autoload (autoload 'elbookmark-transient-bmenu-mode "elbookmark.el" nil t)
(transient-define-prefix elbookmark-transient-bmenu-mode ()
  "Menu for `bookmark-bmenu-mode'."
  :transient-suffix #'transient--do-call
  [:if-derived
   bookmark-bmenu-mode
   ["Bookmarks"
    ("/" "Search" bookmark-bmenu-search :transient nil)
    ("n" "Next bookmark" next-line)
    ("p" "Previous bookmark" previous-line)
    ("s" "Select Bookmark in This Window"
     bookmark-bmenu-this-window
     :transient nil)
    ("f" "Select Bookmark in Full-Frame Window"
     bookmark-bmenu-1-window
     :transient nil)
    ("e" "Select Bookmark in Other Window"
     bookmark-bmenu-other-window
     :transient nil)
    ("I" "Select Bookmark in Other Frame"
     bookmark-bmenu-other-frame
     :transient nil)
    ("c" "Select Marked Bookmarks" bookmark-bmenu-select
     :transient nil)]
   [""
    ("m" "Mark Bookmark" bookmark-bmenu-mark)
    ("a" "Mark all Bookmarks" bookmark-bmenu-mark-all)
    ("u" "Unmark Bookmark" bookmark-bmenu-unmark)
    ("U" "Unmark Backwards" bookmark-bmenu-backup-unmark)
    ("r" "Unmark all Bookmarks" bookmark-bmenu-unmark-all)
    ("t" "Toggle Display of Filenames"
     bookmark-bmenu-toggle-filenames)
    ("d" "Display Location of Bookmark" bookmark-bmenu-locate)]]
  [:if-derived
   bookmark-bmenu-mode
   ["Edit Bookmarks"
    ("b" "Rename Bookmark" bookmark-bmenu-rename)
    ("o" "Relocate Bookmark's File" bookmark-bmenu-relocate)
    ("k" "Mark Bookmark for Deletion" bookmark-bmenu-delete)
    ("i" "Mark all Bookmarks for Deletion"
     bookmark-bmenu-delete-all)
    ("D" "Delete Marked Bookmarks"
     bookmark-bmenu-execute-deletions)]
   ["Annotations"
    ("h" "Show Annotation for Current Bookmark"
     bookmark-bmenu-show-annotation)
    ("w" "Show Annotations for All Bookmarks"
     bookmark-bmenu-show-all-annotations)
    ("." "Edit Annotation for Current Bookmark."
     bookmark-bmenu-edit-annotation)]
   ["Other"
    ("v" "Save Bookmarks" bookmark-bmenu-save)
    ("L" "Load Bookmarks" bookmark-bmenu-load)]])

;;;###autoload (autoload 'elbookmark-transient "elbookmark.el" nil t)
(transient-define-prefix elbookmark-transient ()
  "Menu for `bookmark-bmenu'."
  [["Bookmarks"
    ("l" "List" (lambda ()
                  (interactive)
                  (call-interactively 'bookmark-bmenu-list)))
    ("j" "Jump" elbookmark-jump)
    ("s" "Save" bookmark-save)
    ("w" "Write" bookmark-write)
    ("L" "Load" bookmark-load)
    ("D" "Delete all" bookmark-delete-all)
    ("d" "Delete" bookmark-delete)
    ("r" "Rename" bookmark-rename)
    ("f" "Insert location" bookmark-insert-location)
    ("i" "Insert" bookmark-insert)
    ("F" "Frame" bookmark-jump-other-frame)
    ("o" "Jump other window" bookmark-jump-other-window)
    ("M" "Set no overwrite" bookmark-set-no-overwrite)
    ("x" "Set" bookmark-set)]])

;;;###autoload
(defun elbookmark-transient-dwim ()
  "Invoke either `elbookmark-transient' or `elbookmark-transient-bmenu-mode'."
  (interactive)
  (if (derived-mode-p 'bookmark-bmenu-mode)
      (elbookmark-transient-bmenu-mode)
    (elbookmark-transient)))


(provide 'elbookmark)
;;; elbookmark.el ends here