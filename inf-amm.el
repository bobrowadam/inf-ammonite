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
(require 'scala-mode)
(require 'cl-lib)
(require 'company)

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
  (-let [buffer-name (if (eq internal 'internal)
             amm-shell-internal-buffer-name
             amm-shell-buffer-name)]
    (get-process buffer-name)))

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
    (shell-quote-argument "amm")
  (shell-quote-argument "amm"))) ;; Currently no difference between the internal or not

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
      (inf-amm-mode))
    (when show
      (display-buffer buffer))
    (if internal
        (set-process-query-on-exit-flag process nil)
      (setq amm-shell-buffer buffer))
    proc-buffer-name))

(defun amm--shell-end-of-output? (string)
  "Return non-nil if STRING ends with the prompt."
  (s-matches? comint-prompt-regexp string))

(defun amm--shell-output-filter (string)
  "If STRING ends with input prompt then set filter in progress done."
  (when (amm--shell-end-of-output? string)
    (setq amm--shell-output-filter-in-progress nil))
  "\n@ ")

(defun amm--shell-send-string (string &optional process internal)
  "Internal implementation of shell send string functionality."
  (let ((process (or process (amm-shell-get-process internal)))
        (amm--shell-output-filter-in-progress t))
    (comint-send-string process string)
    (while amm--shell-output-filter-in-progress
      (accept-process-output process))))

(defun amm-shell-send-string-no-output (string &optional process internal)
  "Send STRING to amm PROCESS and inhibit printing output."
  (-let [comint-preoutput-filter-functions
         '(amm--shell-output-filter)]
    (amm--shell-send-string string process internal)))

(defun amm-shell-send-string (string &optional process)
  "Send STRING to amm PROCESS."
  (-let [comint-output-filter-functions
         '(amm--shell-output-filter)]
    (amm--shell-send-string string process)))

(defun amm--shell-send-async (string)
  "Send STRING to internal amm process asynchronously."
  (let ((output-buffer "*Comint Amm Redirect Work Buffer*")
        (proc (amm-shell-get-process 'internal)))
    (with-current-buffer (get-buffer-create output-buffer)
      (erase-buffer)
      (comint-redirect-send-command-to-process string output-buffer proc nil t)
      (set-buffer (process-buffer proc))
      (while (and (null comint-redirect-completed)
                  (accept-process-output proc nil 100 t)))
      (set-buffer output-buffer)
      (buffer-string))))

;; (defun amm--shell-send-async (string)
;;   "Send STRING to internal amm process asynchronously."
;;   (let ((output-buffer "*Comint Amm Redirect Work Buffer*")
;;         (internal-proc (amm-shell-get-process 'internal))
;;         (amm-internal-buffer ))
;;     (with-current-buffer (get-buffer-create output-buffer)
;;       (erase-buffer)
;;       (comint-redirect-setup output-buffer (current-buffer) comint-prompt-regexp nil)
;;       (comint-send-input t t)
;;       (process-send-string (current-buffer) string))
;;     ;; (comint-redirect-send-command-to-process string  proc nil t)
;;     ;; (set-buffer (process-buffer proc))
;;       (while (and (null comint-redirect-completed)
;;                   (accept-process-output internal-proc nil 100 t)))
;;       (set-buffer output-buffer)
;;       (buffer-string)))

(defun amm--company-format-str (string)
  "Format STRING to send to amm for completion candidates."
  (when string
    (format "%s\t"
            string)))

(defun amm--company-candidates (string)
  "Get candidates for completion of STRING."
  (-when-let* ((command (amm--company-format-str string))
               (candidates (amm--shell-send-async command))
               (matches (split-string (replace-regexp-in-string "@" "" (ansi-color-filter-apply candidates)))))
    matches))

(defun company-amm-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-amm-backend))
    (prefix (company-grab-symbol))
    (candidates (amm--company-candidates arg))))

;; (add-to-list 'company-backends 'company-amm-backend)

(provide 'inf-amm)
;;; inf-amm.el ends here
