;;; name.el --- summary -*- lexical-binding: t -*-

;; Author: adam bob
;; Maintainer: adam bob
;; Version: 1.0
;; Package-Requires: ((require 'dash) (require 's))
;; Homepage: TBD
;; Keywords: ammonite, scala


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary
;; inf-ammonite is a simple mode for running "Ammonite" insdie Emacs using an inferior process.
;;; Code:
(require 'dash)
(require 's)

(defconst inf-amm-mode-syntax-table
  (-let [table
         (copy-syntax-table scala-syntax:syntax-table)]
    ;; syntax modifications...
    
    table)
  "inf-amm mode syntax table.")

(setq-local indent-line-function 'scala-indent:indent-line)

(define-derived-mode inf-amm-mode comint-mode "Inferior Amm"
  "Major mode for Ammonite inferior process."
  (setq-local indent-tabs-mode nil)

  ;; How to dispaly the process status in the mode-line
  (setq mode-line-process '(":%s"))
  ;; This disables editing and traversing the "=>" prompts
  (setq-local comint-prompt-read-only t)
  ;; Lets comint mode recognize the prompt
  (setq-local comint-prompt-regexp (rx bol "@" space))

  ;; ... other specialized config introduced later ...
  )

;;; Configuration

(defconst amm-shell-interpreter "amm"
  "Default Ammm interpreter name.")

(defvar amm-shell-interpreter-args ""
  "Default arguments for Amm interpreter.")

;;; Internalp

(defconst amm-shell-buffer-name "Amm"
  "Default buffer name for Amm interpreter.")

(defconst amm-shell-internal-buffer-name "Amm Internal"
  "Default buffer name for the internal Amm process.")

(defvar amm-shell-buffer nil
  "The current shell buffer for Amm.")

(defvar amm--shell-output-filter-in-progress nil
  "Whether we are waiting for output in `amm-shell-send-string-no-output'.")

(defvar amm--shell-font-lock-enable t
  "Whether the shell should font-lock the current line.")

(defun amm--shell-format-process-name (proc-name)
  (format "*%s*" proc-name))

(defun amm-shell-get-process (&optional internal)
  "Get process corr. to `amm-shell-buffer-name'/`amm-shell-internal-buffer-name'."
  (if (eq internal 'internal)
    amm-shell-internal-buffer-name
    amm-shell-buffer-name))

(defun amm--shell-current-buffer-process ()
  "Get process associated with current buffer.")

(defun amm--shell-current-buffer-a-process? ()
  "Is `current-buffer' a live process?")

(defun amm--shell-get-or-create-buffer ()
  "Get or create `amm-shell-buffer' buffer for current amm shell process.")

(defun amm--shell-buffer? ()
  "Is `amm-shell-buffer' set and does it exist?")

(defun amm--shell-kill-buffer ()
  "Kill `amm-shell-buffer'."
  (when amm-shell-buffer
    (kill-buffer amm-shell-buffer)))

(defun amm--shell-calculate-command (&optional internal)
  "Calculate the string used to execute the inferior Amm process."
  (if (eq internal 'internal)
    "amm"
  "amm")) ;; Currently no difference between the internal or not

;; Straightforward string formatting - see: `shell-quote-argument'

(defun run-amm (&optional cmd)
  "Run an inferior Amm process.
CMD defaults to the result of `amm--shell-calculate-command'."
  (interactive)
  (unless (executable-find "amm")
    (message "Amm not found."))
  (-> (or cmd (amm--shell-calculate-command))
      (amm--shell-make-comint amm-shell-buffer-name 'show)
      get-buffer-process))

(defun run-amm-internal ()
  "Start an inferior hy process in the background for autocompletion."
  (interactive)
  (unless (executable-find "amm")
    (message "Amm executable not found."))

  (when (and (not (amm-shell-get-process 'internal))
             (executable-find "amm"))
    (-let [amm--shell-font-lock-enable nil]
      (prog1
          (-> (amm--shell-calculate-command 'internal)
              (amm--shell-make-comint amm-shell-internal-buffer-name nil 'internal)
              get-buffer-process)
        (amm--shell-send-internal-setup-code)
        (message "Amm internal process successfully started")))))

(defun amm--shell-make-comint (cmd proc-name &optional show internal)
  "Create and return comint process PROC-NAME with CMD, opt. INTERNAL and SHOW."
  (-when-let* ((proc-buffer-name
                (amm--shell-format-process-name proc-name))
               (_
                (not (comint-check-proc proc-buffer-name)))
               (cmdlist
                (split-string-and-unquote cmd))
               (buffer
                (apply 'make-comint-in-buffer proc-name proc-buffer-name
                       (car cmdlist) nil (cdr cmdlist)))
               (process
                (get-buffer-process buffer)))
    (with-current-buffer buffer
      (inferior-amm-mode))
    (when show
      (display-buffer buffer))
    (if internal
        (set-process-query-on-exit-flag process nil)
      (setq amm-shell-buffer buffer))
    proc-buffer-name))

(provide 'inf-amm)
;;; inf-amm.el ends here
