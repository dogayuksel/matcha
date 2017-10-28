;;; hydra-lua.el --- Integration with Hydra. -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; URL: https://github.com/jojojames/hydra-integrations
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: hydra, emacs
;; HomePage: https://github.com/jojojames/hydra-integrations

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
;;; Integration with Hydra.

;;; Code:
(require 'hydra-integration-base)
(require 'lua-mode)

(defun +lua-run-test-suite ()
  "Run test_suite.lua."
  (interactive)
  (let ((default-directory (locate-dominating-file
                            (file-name-directory buffer-file-name)
                            "main.lua")))
    (compilation-start
     (format "lua tests/test_suite.lua -v")
     'compilation-mode
     (lambda (_mode-name)
       "*lua test results*")
     t)))

(defun +lua-run-test-file ()
  "Run test file using buffer as file."
  (interactive)
  (if-let (buffer-file (buffer-file-name))
      (let ((default-directory (locate-dominating-file
                                (file-name-directory buffer-file-name)
                                "main.lua")))
        (compilation-start (format "lua %s -v" buffer-file)
                           'compilation-mode
                           (lambda (_mode-name)
                             "*lua test results*")
                           t))
    (message "`buffer-file-name' is nil.")))

(defun +lua-run-test-at-point ()
  "Run test at point."
  (interactive)
  (if-let (buffer-file (buffer-file-name))
      (let ((function-name
             (let ((current-line (thing-at-point 'line t)))
               (progn
                 (unless (string-match-p "function" current-line)
                   (search-backward "function"))
                 (let ((new-current-line (s-trim (thing-at-point 'line t))))
                   (s-trim
                    (s-chop-suffix
                     "()"
                     (s-chop-prefix "function" new-current-line))))))))
        (if function-name
            (let ((default-directory (locate-dominating-file
                                      (file-name-directory buffer-file-name)
                                      "main.lua")))
              (compilation-start
               (format "lua %s %s -v" buffer-file (s-replace ":" "." function-name))
               'compilation-mode
               (lambda (_mode-name)
                 "*lua test results*")
               t))
          (message "Couldn't find `function-name'.")))
    (message "`buffer-file-name' is nil.")))

(defhydra hydra-lua-test (:color blue :columns 4)
  "Lua Test"
  ("p" +lua-run-test-at-point "Test at Point")
  ("f" +lua-run-test-file "Test File")
  ("t" +lua-run-test-suite "Test Suite"))

(defhydra hydra-lua-eval (:color blue :columns 4)
  "Lua Eval"
  ("b" lua-send-buffer "Send Buffer")
  ("l" lua-send-current-line "Send Current Line")
  ("e" lua-send-defun "Send Defun")
  ("r" lua-send-region "Send Region")
  ("s" lua-show-process-buffer "Show Process Buffer")
  ("h" lua-hide-process-buffer "Hide Process Buffer")
  ("z" lua-start-process "Start Repl")
  ("x" lua-kill-process "Kill Repl")
  ("R" lua-restart-with-whole-file "Restart Repl with File"))

(defhydra hydra-lua-mode (:color blue :columns 4)
  "Lua"
  ("l" hydra-love-mode/body "Love")
  ("e" hydra-lua-eval/body "Eval")
  ("t" hydra-lua-test/body "Test")
  ("z" lua-show-process-buffer "Show Repl")
  ("h" lua-search-documentation "Search Documentation"))

(defcustom love/documentation-url
  "https://love2d.org/wiki/"
  "URL pointing to the Love wiki."
  :type 'string
  :group 'lua)

(defun love/search-documentation ()
  "Search Love documentation for the word at the point."
  (interactive)
  (let ((url (concat love/documentation-url (lua-funcname-at-point))))
    (funcall lua-documentation-function url)))

(defhydra hydra-love-mode ()
  "Love"
  ("k" love/search-documentation "Search Documentation")
  ("p" love/create-project-configuration "Create Project")
  ("f" love/search-forums "Search Forums")
  ("h" love/browse-documentation "Browse Documentation"))

(+add-mode-command #'hydra-lua-mode/body '(lua-mode))
(+add-eval-command #'hydra-lua-eval/body '(lua-mode))
(+add-test-command #'hydra-lua-test/body '(lua-mode))

(provide 'hydra-lua)
;;; hydra-lua.el ends here
;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved noruntime cl-functions obsolete)
;; End:
