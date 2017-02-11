# recentc
List recently closed files in Emacs

## Installation

```lisp
(require 'recentc)
(recentc-mode 1)
```

## Usage

Press `C-c C-r` to list recently closed files with `ivy-mode`
(preferred) or `ido-mode`. Press `C-c C-u` to open the most recently
closed file.

## Configuration

Customize key bindings:

```lisp
(define-key recentc-mode-map "\C-x\C-r" #'recentc-find-closed-file)
(define-key recentc-mode-map "\C-x\C-u" #'recentc-reopen-last-closed-file)
```


Exclude closed files under `~/.emacs.d/` (`user-emacs-directory`):

```lisp
(add-hook 'recentc-exclude-conditions
          (lambda (filename)
            (string-prefix-p (expand-file-name user-emacs-directory)
                             (file-name-directory (expand-file-name filename)))))
```


Exclude closed files in `.git`:

```lisp
(defun pt/folder-name (filename)
  (file-name-base (directory-file-name (file-name-directory filename))))

(add-hook 'recentc-exclude-conditions
          (lambda (filename)
            (string-equal ".git" (pt/folder-name (expand-file-name filename)))))
```
