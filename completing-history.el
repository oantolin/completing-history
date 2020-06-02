;;; completing-history.el --- Insert history items with completion  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Omar Antolín Camarena

;; Author: Omar Antolín Camarena <omar@matem.unam.mx>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a command `completing-history-insert-item' to
;; insert an item from history using completion. It calls
;; `completing-read' to do this, meaning it will work with your
;; preferred completion framework. The package supports: the
;; minibuffer, eshell, comint and term.

;; There is also a function `completing-history-setup-keybinding' to
;; setup a keybinding for you in all supported contexts. By default it
;; binds `completing-history-insert-item' to M-r (usually bound to a
;; command that gets the previous history item matching a regexp) and
;; unbinds the companion M-s command (usually bound to a command that
;; searches forward in the history).

;; To customize the binding of `completing-history-insert-item', use
;; `completing-history-binding'. To keep M-s from being unbound
;; customize `completing-history-unbind-M-s'.

;;; Code:

(defgroup completing-history nil
  "Insert history items chosen with completion."
  :group 'completion)

(defcustom completing-history-input-rings
  '((eshell-mode . eshell-history-ring)
    (comint-mode . comint-input-ring)
    (term-mode   . term-input-ring))
  "Alist of (mode . ring) pairs of input rings."
  :type '(list (cons symbol symbol))
  :group 'completing-history)

(defun completing-history--items-for-buffer ()
  "Get history relevant for current buffer."
  (cond
   ((eq last-command 'repeat-complex-command)
    (mapcar #'prin1-to-string command-history))
   ((minibufferp)
    (if (version< emacs-version "27")
        (symbol-value minibuffer-history-variable)
      (minibuffer-history-value)))
   (t (cl-loop
       for (mode . ring) in completing-history-input-rings
       when (and (boundp ring) (derived-mode-p mode))
       return (ring-elements (symbol-value ring))))))

(defun completing-history--completion-table ()
  "Relevant history items in reverse chronological order."
  (let ((history (completing-history--items-for-buffer)))
    (lambda (string pred action)
      (if (eq action 'metadata)
          '(metadata (display-sort-function . identity)
                     (cycle-sort-function . identity))
        (complete-with-action action history string pred)))))

(defun completing-history-insert-item ()
  "Insert an item from history, selected with completion."
  (interactive)
  (let ((item (let ((enable-recursive-minibuffers t))
                (completing-read
                 "Item: " (completing-history--completion-table) nil t))))
    (when (minibufferp)
      (delete-minibuffer-contents))
    (when item
      (let ((inhibit-read-only t))
        (insert item)))))

(defcustom completing-history-keymaps
  `((minibuffer . minibuffer-local-map)
    ,(if (version< emacs-version "27")
         '(esh-mode . eshell-mode-map)
       '(em-hist . eshell-hist-mode-map))
    (shell . shell-mode-map)
    (term . term-mode-map)
    (term . term-raw-map)
    (comint . comint-mode-map)
    (slime-repl . slime-repl-mode-map))
  "Alist of (mode . keymap) pairs where M-r should insert history items."
  :type '(list (cons symbol symbol))
  :group 'completing-history)

(defcustom completing-history-binding "M-r"
  "Keybinding to use for `completing-history-insert-item'."
  :type 'string
  :group 'completing-history)

(defcustom completing-history-unbind-M-s t
  "Unbind M-s in the keymaps where we bind `completing-history-insert-item'?"
  :type 'boolean
  :group 'completing-history)

(defun completing-history-setup-keybinding ()
  "Free M-s and bind M-r to do history completion in various modes."
  (cl-loop   
   for (feature . keymap) in completing-history-keymaps
   for body = `((define-key ,keymap (kbd ,completing-history-binding)
                  #'completing-history-insert-item)
                ,@(when completing-history-unbind-M-s
                    `((define-key ,keymap (kbd "M-s") nil))))
   do (eval-after-load feature
        (if (and (version< emacs-version "27") (eq feature 'esh-mode))
            `(add-hook 'eshell-mode-hook (lambda () ,@body))
          `(progn ,@body)))))

(provide 'completing-history)
;;; completing-history.el ends here
