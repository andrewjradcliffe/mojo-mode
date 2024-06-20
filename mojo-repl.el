;;; mojo-repl.el --- A minor mode for a Mojo REPL -*- lexical-binding:t; no-byte-compile:t -*-

;;; Commentary:

;; Run a mojo REPL inside a terminal in Emacs.
;; Based on `julia-repl' (https://github.com/tpapp/julia-repl)

;;; Code:

(require 'vterm)
(require 'subr-x)

(defgroup mojo-repl nil
  "A minor mode for a Mojo REPL"
  :group 'mojo)

(defcustom mojo-repl-inferior-buffer-name-base "mojo"
  "Prefix for the names for inferior REPL buffers.

See ‘mojo-repl--inferior-buffer-name’."
  :type 'string
  :group 'mojo-repl)

(defvar mojo-repl-inferior-buffer-name-suffix nil
  "Suffix for the Mojo REPL buffer; can be a symbol or number.")

(defcustom mojo-repl-pop-to-buffer t
  "When non-nil pop to mojo repl"
  :type 'symbol
  :group 'mojo-repl)

(defcustom mojo-repl-skip-comments nil
  "Make some send commands (currently `mojo-repl-send-line' and `mojo-repl-send-region-or-line') skip comments."
  :type 'boolean
  :group 'mojo-repl)

(defvar mojo-repl-switches nil
  "Command line switches for the Mojo executable.

Valid values are NIL or a string. These take effect the next time
a new Mojo process is started.")

;;; vterm

(defun mojo-repl--locate-live-buffer (name)
  (if-let ((inferior-buffer (get-buffer name)))
      (with-current-buffer inferior-buffer
        (when (vterm-check-proc inferior-buffer)
          inferior-buffer))))

(defun mojo-repl--make-buffer (name executable-path switches)
  (let ((vterm-buffer (get-buffer-create name))
        (inhibit-read-only t))
    (with-current-buffer vterm-buffer
      (let ((vterm-shell (string-join (cons executable-path switches) " ")))
        (vterm-mode)
        (local-set-key (kbd "C-c C-z") #'mojo-repl--switch-back)))
    vterm-buffer))

(defun mojo-repl--send-to-backend (buffer string paste-p ret-p)
  (with-current-buffer buffer
    (vterm-send-string string paste-p)
    (when ret-p
      ;; Send 1 or 2 newlines? -- 2 seems more natural.
      ;; (vterm-send-return)
      (progn (vterm-send-return) (vterm-send-return))
      )))

(defun mojo-repl--executable-path (executable)
  "Return the path of the Mojo executable."
  (if (file-name-absolute-p executable)
      executable
    (executable-find executable)))

(defun mojo-repl--inferior-buffer-name (suffix)
  "Name for a Mojo REPL inferior buffer.

Suffix formatting:
- integer: “<suffix>”
- symbol: “-suffix.”
"
  (let* ((end (cond
               ((null suffix) "")
               ((integerp suffix) (format "<%d>" suffix))
               ((symbolp suffix) (format "-%s" suffix))
               (t (error
                   "Inferior name suffix should be an integer or a symbol")))))
    (concat "*" mojo-repl-inferior-buffer-name-base end "*")))

(defun mojo-repl-inferior-buffer ()
  "Return the Mojo REPL inferior buffer, creating one if it does not exist."
  (let* ((suffix mojo-repl-inferior-buffer-name-suffix)
         (name (mojo-repl--inferior-buffer-name suffix))
         (live-buffer (mojo-repl--locate-live-buffer name)))
    (if live-buffer
        live-buffer
      (let ((executable-path (mojo-repl--executable-path "mojo"))
            (switches mojo-repl-switches))
        (let* ((inferior-buffer
                (mojo-repl--make-buffer name executable-path
                                        (when switches
                                          (split-string switches)))))
          inferior-buffer)))))

(defun mojo-repl ()
  "Raise the Mojo REPL inferior buffer, creating one if it does not exist."
  (interactive)
  (let ((script-buffer (current-buffer))
	    (inferior-buffer (mojo-repl-inferior-buffer)))
    (with-current-buffer inferior-buffer
      (setq mojo-repl--script-buffer script-buffer))
    (if mojo-repl-pop-to-buffer
	    (pop-to-buffer inferior-buffer)
      (switch-to-buffer inferior-buffer))))

(defun mojo-repl--switch-back ()
  "Switch to the buffer that was active before last call to `mojo-repl'."
  (interactive)
  (when (buffer-live-p mojo-repl--script-buffer)
    (if mojo-repl-pop-to-buffer
	    (switch-to-buffer-other-window mojo-repl--script-buffer)
      (switch-to-buffer mojo-repl--script-buffer))))


;;; sending to the REPL

(defun mojo-repl--send-string (string &optional no-newline no-bracketed-paste)
  "Send STRING to the Mojo REPL vterm buffer.

The string is trimmed, then a closing newline is sent according to NO-NEWLINE:

  1. NIL sends the newline,
  2. 'PREFIX sends it according to ‘current-prefix-arg’,
  3. otherwise no newline.

Unless NO-BRACKETED-PASTE, bracketed paste control sequences are used."
  (when (eq no-newline 'prefix)
    (setq no-newline current-prefix-arg))
  (let ((inferior-buffer (mojo-repl-inferior-buffer)))
    (if mojo-repl-pop-to-buffer
	    (display-buffer inferior-buffer))
    (mojo-repl--send-to-backend inferior-buffer
                                (string-trim string)
                                (not no-bracketed-paste)
                                (not no-newline))))

(defun mojo-repl--forward-skip-comments ()
  "Move one line forward, then skip any comments when `mojo-repl-skip-comments' is set."
  (forward-line)
  (when mojo-repl-skip-comments
    (forward-comment (buffer-size))))

(defun mojo-repl-send-line ()
  "Send the current line to the Mojo REPL term buffer.

Closed with a newline, unless used with a prefix argument.

This is the only REPL interaction function that does not use
bracketed paste.  Unless you want this specifically, you should
probably be using `mojo-repl-send-region-or-line'."
  (interactive)
  (mojo-repl--send-string (thing-at-point 'line t) 'prefix t)
  (mojo-repl--forward-skip-comments))

(defun mojo-repl-send-region (start end)
  "Send active region to the inferior buffer."
  (interactive "r")
  (mojo-repl--send-string (buffer-substring-no-properties start end) 'prefix))

(defun mojo-repl-send-buffer ()
  "Send the contents of the current buffer to the Mojo REPL."
  (interactive)
  (mojo-repl-send-region (point-min) (point-max)))

;; This could implemented in a more subtle way, but, it works.
(defun mojo-repl-send-definition ()
  "Send the contents of the current buffer to the Mojo REPL."
  (interactive)
  (save-mark-and-excursion
    (mark-defun)
    (mojo-repl-send-region (region-beginning) (region-end))))

;;; buffer name

(defun mojo-repl--unused-inferior-buffer-name-index ()
  "Find first positive integer not currently in use as a buffer suffix."

  (let ((n 2))
    (while (get-buffer (mojo-repl--inferior-buffer-name n))
      (message (number-to-string n))
      (setq n (+ n 1)))
    n))


(defun mojo-repl-prompt-set-inferior-buffer-name-suffix (arg)
  "Prompt for and set a Mojo REPL inferior buffer name for the current buffer.

A prefix argument ARG modifies the behavior:

- \\[negative-argument] selects the next unused number for the suffix
- a numerical prefix selects that integer for the suffix.

Both of these happen without prompting."
  (interactive "P")
  (let* ((suffix (cond
                  ((null arg)
                   (intern (read-from-minibuffer "Mojo REPL suffix: " "")))
                  ((eq arg '-)
                   (mojo-repl--unused-inferior-buffer-name-index))
                  ((integerp arg)
                   arg))))
    (setq mojo-repl-inferior-buffer-name-suffix suffix)
    (message "mojo-repl-inferior-buffer-name-suffix set to %s" suffix)))

;;;###autoload
(define-minor-mode mojo-repl-mode
  "Minor mode for interacting with a Mojo REPL running inside a term.

\\{mojo-repl-mode-map}"
  :init-value nil
  :lighter " 🔥"
  :keymap
  `(
    (,(kbd "C-c C-b")    . mojo-repl-send-buffer)
    (,(kbd "C-c C-r")    . mojo-repl-send-region)
    ;; (,(kbd "C-c C-s")    . mojo-repl-prompt-set-inferior-buffer-name-suffix)
    (,(kbd "C-c C-z")    . mojo-repl)
    (,(kbd "<C-return>") . mojo-repl-send-line))
  (when-let ((filename (buffer-file-name)))
    (setq-local default-directory (file-name-directory filename))))

(provide 'mojo-repl)
;;; mojo-repl.el ends here
