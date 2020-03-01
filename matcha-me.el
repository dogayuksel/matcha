;;; matcha.el --- Integration with Hydra. -*- lexical-binding: t -*-

;; Copyright (C) 2019 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/matcha
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: hydra, emacs
;; HomePage: https://github.com/jojojames/matcha

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;;  USAGE:
;;;
;;;  (general-define-key
;;;   :states '(normal visual motion)
;;;   :keymaps 'override
;;;   "SPC" 'matcha-me-space)
;;;

;;; Code:
(require 'matcha-base)

(defun matcha-me-find-init ()
  "Visit init file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun matcha-me-vc-dir (dir &optional backend)
  "Reimplementation of `vc-dir' without popping to another window."
  (interactive
   (list
    ;; When you hit C-x v d in a visited VC file,
    ;; the *vc-dir* buffer visits the directory under its truename;
    ;; therefore it makes sense to always do that.
    ;; Otherwise if you do C-x v d -> C-x C-f -> C-c v d
    ;; you may get a new *vc-dir* buffer, different from the original
    (file-truename (read-directory-name "VC status for directory: "
                                        (vc-root-dir) nil t
                                        nil))
    (if current-prefix-arg
        (intern
         (completing-read
          "Use VC backend: "
          (mapcar (lambda (b) (list (symbol-name b)))
                  vc-handled-backends)
          nil t nil nil)))))
  (unless backend
    (setq backend (vc-responsible-backend dir)))
  (switch-to-buffer
   (vc-dir-prepare-status-buffer "*vc-dir*" dir backend))
  (if (derived-mode-p 'vc-dir-mode)
      (vc-dir-refresh)
    (defvar use-vc-backend)
    (let ((use-vc-backend backend))
      (ignore use-vc-backend)
      (vc-dir-mode))))

(defun matcha-me-find-file-dwim ()
  "Find file DWIM."
  (interactive)
  (cond
   ((or (eq major-mode 'dired-mode)
        (eq major-mode 'dired-sidebar-mode))
    (let ((default-directory (dired-current-directory)))
      (call-interactively #'find-file)))
   ((derived-mode-p 'magit-mode)
    (let ((magit-file (magit-file-at-point)))
      (if magit-file
          (let ((default-directory
                  (file-name-directory
                   (concat (magit-toplevel) magit-file))))
            (call-interactively #'find-file))
        (call-interactively #'find-file))))
   (:default
    (call-interactively #'find-file))))

(defun matcha-me-recentf-dwim ()
  "Find recent files DWIM."
  (interactive)
  (cond
   ((bound-and-true-p ivy-mode)
    (counsel-recentf))
   ((bound-and-true-p helm-mode)
    (helm-recentf))
   ((bound-and-true-p ido-mode)
    (ido-recentf-open))
   (:else
    (recentf-open-files))))

(defun matcha-me-buffers-dwim ()
  "List buffers DWIM."
  (interactive)
  (cond
   ((bound-and-true-p ivy-mode)
    (ivy-switch-buffer))
   ((bound-and-true-p helm-mode)
    (helm-buffers-list))
   ((bound-and-true-p ido-mode)
    (ido-switch-buffer))
   (:else
    (call-interactively #'switch-to-buffer))))

(defun matcha-me-save-all-buffers ()
  "Save all buffers without confirming."
  (interactive)
  (save-some-buffers :all-buffers-no-confirm))

;; File Related

(defun matcha-copy-current-filename-to-clipboard ()
  "Copy `buffer-file-name' to system clipboard."
  (interactive)
  (if (not buffer-file-name)
      (message "Not a file...")
    (message (format "Copying %s to clipboard..." buffer-file-name))
    (kill-new buffer-file-name)))

(defalias 'copy-current-filename-to-clipboard 'matcha-copy-current-filename-to-clipboard)

(defun matcha-revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed.

https://emacs.stackexchange.com/questions/24459/revert-all-open-buffers-and-ignore-errors"
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (when (file-readable-p filename)
          ;; If the file exists and is readable, revert the buffer.
          (with-current-buffer buf
            (revert-buffer :ignore-auto :noconfirm :preserve-modes))))))
  (message "Finished reverting buffers containing unmodified files."))

(defalias 'revert-all-file-buffers 'matcha-revert-all-file-buffers)

(defvar matcha-saved-files-file "~/.emacs.d/saved-files")

(defun matcha-save-files-to-saved-files-list ()
  "Save list of open files in Emacs to `matcha-saved-files-file'."
  (interactive)
  (let ((text-to-write ""))
    (dolist (buffer (buffer-list))
      (when-let* ((buffer-name (buffer-file-name buffer)))
        (setq text-to-write (concat text-to-write buffer-name "\n"))))
    (unless (string-equal text-to-write "")
      (message (format "Writing to %s..." matcha-saved-files-file))
      (delete-file matcha-saved-files-file)
      (write-region text-to-write nil matcha-saved-files-file))))

(defalias 'save-files-to-saved-files-list 'matcha-save-files-to-saved-files-list)

(defun matcha-open-files-from-saved-files-list ()
  "Open saved files stored at `matcha-saved-files-file'."
  (interactive)
  (let ((files (with-temp-buffer
                 (insert-file-contents matcha-saved-files-file)
                 (split-string (buffer-string) "\n" t))))
    (message (format "Reading from %s..." matcha-saved-files-file))
    (mapc (lambda (file)
            (if (file-exists-p file)
                (ignore-errors
                  (message (format "Finding file %s..." file))
                  (find-file file))
              (message (format "File %s doesn't exist anymore." file))))
          files)
    (message "Finish opening saved files.")))

(defalias 'open-files-from-saved-files-list 'matcha-open-files-from-saved-files-list)

(defun matcha-rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))

(defalias 'rename-current-buffer-file 'matcha-rename-current-buffer-file)

;; Transients

(define-transient-command matcha-org-space
  "Org"
  [["Org"
    ("a" "Agenda" org-agenda)
    ("c" "Capture" org-capture)
    ("r" "Refile" org-refile)
    ("o" "Todo" org-todo)]
   ["Links"
    ("l" "Store" org-store-link)
    ("i" "Insert" org-insert-link)]])

(define-transient-command matcha-me-space ()
  "Space"
  [["Find"
    ("f" "File" counsel-find-file)
    ("b" "Quick File" counsel-buffer-or-recentf)
    ("B" "Buffers" switch-to-buffer)
    ("r" "Bookmarks" bookmark-jump)
    ("n" "NeoTree" neotree)
    ("SPC" "Jump" avy-goto-char-timer)]
   ["Manage"
    ("w" "Window..." matcha-me-window)
    ("g" "Git..." matcha-magit)
    ("p" "Project..." matcha-projectile)
    ("y" "System..." matcha-me-system)
    ("R" "Bookmarks..." matcha-me-bookmark)]
   ["Do"
    ("s" "Search..." matcha-me-search)
    ("i" "Save buffer" save-buffer)
    ("S" "Save all Buffers" matcha-me-save-all-buffers)
    ("=" "Format" matcha-run-format-command)
    ("o" "Org..." matcha-org-space)]
   ["Mode"
    ("m" "Mode" matcha-run-mode-command)
    ("d" "Debug" matcha-run-debug-command)
    ("v" "Eval" matcha-run-eval-command)
    ("t" "Test" matcha-run-test-command)
    ("a" "Refactor..." matcha-run-refactor-command)]]
  [:hide (lambda () t)
         ("c" org-capture)
         ("e" er/expand-region)
         ("-" split-window-below)
         ("|" split-window-right)
         ("\\" split-window-right)
         ("h" evil-window-left)
         ("l" evil-window-right)
         ("k" evil-window-up)
         ("j" evil-window-down)
         ("." evil-next-buffer)
         ("," evil-prev-buffer)
         (";" eval-expression)
         ("'" shell-command)
         ("x" kill-this-buffer)
         ("/" evil-mc-hydra/body)]
  (interactive)
  (let ((transient-show-popup -.2))
    (transient-setup 'matcha-me-space)))

(define-transient-command matcha-me-profiler
  "Profiler"
  [["Profiler"
    ("s" "Start" profiler-start)
    ("r" "Report" profiler-report)
    ("x" "Stop" profiler-stop)]])

(define-transient-command matcha-me-bookmark
  "Bookmark"
  [["Bookmark"
    ("b" "Set" bookmark-set)
    ("d" "Delete" bookmark-delete)
    ("j" "Jump" bookmark-jump)
    ("l" "List" bookmark-bmenu-list)
    ("s" "Save" bookmark-save)]])

(defun open-finder ()
  "Open finder in current directory."
  (interactive)
  (shell-command "open ."))

(define-transient-command matcha-me-system ()
  "System"
  [["System"
    ("f" "Finder" open-finder)
    ("t" "Open iTerm" my/open-iterm-here)
    ("s" "Send to iTerm" my/send-line-to-iterm)]
   ["Processes"
    ("p" "Profiler..." matcha-me-profiler)
    ("L" "List Processes" list-processes)]])

(define-transient-command matcha-me-search ()
  "Search"
  [["Swiper"
    ("s" "Swiper" swiper-thing-at-point)
    ("a" "Without Pre-input" swiper)
    ("S" "Swiper All" swiper-all)]
   ["Occur"
    ("o" "Occur" occur)
    ("O" "Multi Occur" multi-occur)
    ("P" "Occur in Project" projectile-multi-occur)]
   ["Ripgrep"
    ("r" "Do" counsel-rg)
    ("g" "Set..." counsel-rg-transient)]])

(define-transient-command matcha-me-window ()
  "Window"
  [["Narrow/Widen"
    ("n" "Narrow" narrow-to-region)
    ("W" "Widen" widen)
    ("ND" "Narrow to Defun" narrow-to-defun)
    ("NP" "Narrow to Page" narrow-to-page)]
   ["Workspace"
    ("j" "New" eyebrowse-create-window-config)
    ("l" "Next" eyebrowse-next-window-config)
    ("h" "Previous" eyebrowse-prev-window-config)
    ("k" "Close" eyebrowse-close-window-config)]
   ["Text"
    ("+" "Increase" text-scale-increase :transient t)
    ("_" "Decrease" text-scale-decrease :transient t)]]
  [["Frame"
    ("m" "Maximize" toggle-frame-maximized)
    ("f" "Toggle Fullscreen" toggle-frame-fullscreen)
    ("0" "Delete Frame" delete-frame)
    ("1" "Delete other Frames" delete-other-frames)
    ("2" "Make Frame" make-frame-command)
    ("o" "Other Frame" other-frame)]
   ["Window"
    ("=" "Balance" balance-windows)
    ("t" "Rotate Windows" evil-window-rotate-downwards)
    ("x" "Delete Window" delete-window)
    ("w" "Delete Others" delete-other-windows)]
   ["Resize"
    ("<right>" "->" shrink-window-horizontally :transient t)
    ("<left>" "<-" enlarge-window-horizontally :transient t)
    ("<down>" "Down" shrink-window :transient t)
    ("<up>" "Up" enlarge-window :transient t)]]
  [:hide (lambda () t)
         ("-" split-window-below)
         ("|" split-window-right)
         ("\\" split-window-right)])

(define-transient-command matcha-me-files ()
  "Files"
  [["Current File"
    ("y" "Copy Filename to Clipboard" matcha-copy-current-filename-to-clipboard)
    ("r" "Rename Current File" matcha-rename-current-buffer-file)]
   ["All Files"
    ("S" "Save All to SavedFile" matcha-save-files-to-saved-files-list)
    ("O" "Open All from SavedFile" matcha-open-files-from-saved-files-list)
    ("R" "Revert/Refresh All" matcha-revert-all-file-buffers)]])

(provide 'matcha-me)
;;; matcha-me.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
