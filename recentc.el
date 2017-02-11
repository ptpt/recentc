;; -*- lexical-binding: t -*-

;;; recentc.el --- list recently closed files

;; Copyright (C) 2012-2017 Tao Peng <pt@taopeng.me>

;; Author:   Tao Peng <pt@taopeng.me>
;; Keywords: files

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or \(at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(defvar recentc-list nil
  "List of recently closed files.")

(defvar recentc-save-file (eval-when-compile
                            (locate-user-emacs-file "recentc"))
  "A file used to save the recently closed file list.")

(defvar recentc-max-saved-items 1024
  "Maximum number of items of recently closed list that will be saved.
nil means to save the whole list")

(defvar recentc-mode-map (make-sparse-keymap)
  "Keymap for recentc-mode.")

(defun recentc-clear-non-existent-files ()
  "Clear non-exisitent files from `recentc-list'."
  (setq recentc-list
        (remove-if (lambda (file)
                     (or (file-remote-p file)
                         (not (file-exists-p file))))
                   recentc-list)))

(defun recentc-reopen-last-closed-file (&optional arg)
  "Reopen the last closed file."
  (interactive "p")
  (recentc-clear-non-existent-files)
  (let ((filename (nth (- arg 1) recentc-list)))
    (if filename
        (find-file filename))))

(defun recentc-remove-file (&optional filename)
  "Remove FILENAME or current buffer file from `recentc-list'."
  (setq filename (or filename (buffer-file-name)))
  (when filename
    (setq recentc-list (remove filename recentc-list))))

(defvar recentc-exclude-conditions nil)

(defvar recentc-include-conditions nil)

(let ((test (lambda (filename)
              (lambda (condition)
                (cond ((stringp condition) (string-match-p condition filename t))
                      ((functionp condition) (funcall condition filename))
                      (t (error "Invalid condition")))))))

  (defun recentc-exclude-p (&optional filename)
    (some (funcall test filename) recentc-exclude-conditions))

  (defun recentc-include-p (&optional filename)
    (some (funcall test filename) recentc-include-conditions)))

(defun recentc-add-file (&optional filename)
  "Add FILENAME or current buffer file to `recentc-list'."
  (setq filename (or filename (buffer-file-name)))
  (when (and (stringp filename)
             ;; include or not exclude
             (or (recentc-include-p filename)
                 (not (recentc-exclude-p filename))))
    ;; always put the closed file at the front of the list
    (setq recentc-list (cons filename (remove filename recentc-list)))
    ;; keep the number of recently closed files not to exceed `recentc-max-saved-items'
    ;; todo: need improve
    (when (and (numberp recentc-max-saved-items)
               (> recentc-max-saved-items 0))
      (let ((tails (nthcdr (- recentc-max-saved-items 1) recentc-list)))
        (if (consp tails)
            (setcdr tails nil))))))

(defun recentc-save-all ()
  "Add all files to `recentc-list' and save the list to `recentc-save-file'.
This function only can be used in `kill-emacs-hook'."
  ;; add all files to list
  (mapc (lambda (buffer)
          (if (buffer-file-name buffer)
              (recentc-add-file (buffer-file-name buffer))))
        (reverse (buffer-list)))
  ;; save list to file
  (when recentc-save-file
    (recentc-clear-non-existent-files)
    (with-temp-file recentc-save-file
      (prin1 recentc-list (current-buffer)))))

(defun recentc-load-list-from-file ()
  "Load data from `recentc-save-file' into `recentc-list'."
  (when (file-exists-p recentc-save-file)
    (setq recentc-list
          (with-temp-buffer
            (insert-file-contents recentc-save-file)
            (goto-char (point-min))
            (read (current-buffer))))
    (recentc-clear-non-existent-files)))

(define-minor-mode recentc-mode
  "Toggle `recentc-mode'."
  :global t
  :group 'recentc
  :keymap recentc-mode-map
  (if recentc-mode
      (progn
        (recentc-load-list-from-file)
        (add-hook 'kill-emacs-hook 'recentc-save-all)
        (add-hook 'kill-buffer-hook 'recentc-add-file)
        (add-hook 'find-file-hook 'recentc-remove-file))
    (progn
      ;; (recentc-save-all)
      (remove-hook 'kill-emacs-hook 'recentc-save-all)
      (remove-hook 'kill-buffer-hook 'recentc-add-file)
      (remove-hook 'find-file-hook 'recentc-remove-file))))

(defun recentc--ido-keymap-setup ()
  (define-key ido-completion-map [?\C-k] 'recentc-ido-kill-recentc))

;; should be deprecated
(defvar recentc--ido-candidates nil
  "ido candidates that correspond to recently closed files.")

(defun recentc-ido-find-closed-file ()
  "Find recently opened files using ido-mode."
  (interactive)
  (setq recentc--ido-candidates nil)
  (recentc-clear-non-existent-files)
  (let ((records nil) (suffix nil))
    (dolist (file recentc-list)
      (let* ((f (file-name-nondirectory file))
             (match (assoc f records)))
        (if match
            (setcdr match (1+ (cdr match)))
          ;; `add-to-list' doesn't work with lexical binding
          (pushnew (cons f 1) records))
        (setq suffix
              (if (cdr match)
                  (format "<%d>" (cdr match))
                ""))
        (add-to-list 'recentc--ido-candidates
                     (cons (concat f suffix) file) t))))
  (add-hook 'ido-setup-hook 'recentc--ido-keymap-setup)
  (let* ((choice
          (condition-case err
              (ido-completing-read
               "Recently closed files: "
               (mapcar #'car recentc--ido-candidates)
               nil t)
            ;; remove keymap
            (quit (remove-hook 'ido-setup-hook 'recentc--ido-keymap-setup))
            (error (remove-hook 'ido-setup-hook 'recentc--ido-keymap-setup)
                   (signal (car err) (cdr err)))))
         (filename (and choice (assoc choice recentc--ido-candidates))))
    (remove-hook 'ido-setup-hook 'recentc--ido-keymap-setup)
    (when filename
      (find-file (cdr filename)))))

(defun recentc-ido-kill-recentc ()
  "Kill the recently closed file at the head of `ido-matches'
and remove it from `recentc-list'. If cursor is not at
the end of the user input, delete to end of input."
  (interactive)
  (if (not (eobp))
      (delete-region (point) (line-end-position)) ;why?
    (let ((enable-recursive-minibuffers t) ;why?
          (file (ido-name (car ido-matches))))
      (when file
        (setq recentc-list
              (delq (cdr (assoc file recentc--ido-candidates)) recentc-list))
        (setq recentc--ido-candidates
              (delq (assoc file recentc--ido-candidates) recentc--ido-candidates))
        (setq ido-cur-list
              (delq file ido-cur-list))))))

(defun recentc-find-candidates ()
  (recentc-clear-non-existent-files)
  (let (counts suffix candidates)
    (dolist (file recentc-list)
      (let* ((basename (file-name-nondirectory file))
             (match (assoc basename counts)))

        (if match
            (setcdr match (1+ (cdr match)))
          ;; `add-to-list' doesn't work with lexical binding
          (pushnew (cons basename 1) counts))

        (pushnew (cons (if (cdr match)
                           (concat basename (format "<%d>" (cdr match)))
                         basename)
                       file)
                 candidates)))
    (reverse candidates)))

(defun recentc-ivy-find-closed-file ()
  "Find recently opened files using ido-mode."
  (interactive)
  (ivy-read "Recently closed files: "
            (recentc-find-candidates)
            :require-match t
            :action #'find-file
            :caller 'recentc-ivy-find-closed-file))

(defun recentc-find-closed-file ()
  (interactive)
  (cond ((functionp #'ivy-read)
         (recentc-ivy-find-closed-file))
        ((functionp #'ido-completing-read)
         (recentc-ido-find-closed-file))
        (t (message "enable ivy-mode or ido-mode first"))))

(define-key recentc-mode-map "\C-x\C-r" #'recentc-find-closed-file)
(define-key recentc-mode-map "\C-x\C-u" #'recentc-reopen-last-closed-file)

(provide 'recentc)
