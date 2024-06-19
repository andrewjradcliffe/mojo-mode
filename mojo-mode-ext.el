;;; mojo-mode-ext.el --- Mojo based commands -*-lexical-binding: t-*-
;;; Commentary:

;; This library implements support for `mojo-mode'.

;;; Code:

(require 'compile)

(defvar mojo-run-arguments ""
  "Holds arguments for 'mojo run', similar to `compilation-arguments`.")
(defvar mojo-build-arguments ""
  "Holds arguments for 'mojo build', similar to `compilation-arguments`.")

(defvar mojo-package-arguments ""
  "Holds arguments for 'mojo package', similar to `compilation-arguments`.")

(defvar mojo-format-arguments ""
  "Holds arguments for 'mojo format', similar to `compilation-arguments`."
  )
(defvar mojo-test-arguments ""
  "Holds arguments for 'mojo test', similar to `compilation-arguments`."
  )

(defun mojo-run (&optional arg)
  "Run 'mojo run' for the current file.

If ARG is not nil, use the value as argument and store it in `mojo-run-arguments'.
"
  (interactive "P")
  (when arg
    (setq mojo-run-arguments (read-from-minibuffer "Mojo run arguments: " mojo-run-arguments)))
  (if (eq mojo-run-arguments "")
      (compile (concat "mojo run " buffer-file-name))
    (compile (concat "mojo run " mojo-run-arguments " " buffer-file-name))))


(defun mojo-build (&optional arg)
  "Run 'mojo build' for the current file."
  (interactive "P")
  (when arg
    (setq mojo-build-arguments (read-from-minibuffer "Mojo build arguments: " "")))
  (if (eq mojo-build-arguments "")
      (compile (concat "mojo build " buffer-file-name))
    (compile (concat "mojo build " mojo-build-arguments " " buffer-file-name))))

(defun mojo-package (&optional arg)
  "Run 'mojo package'.

If options are provided via prefix argument, it is assumed that a path is included.
Without prefix argument, `default-directory' is used as the path."
  (interactive "P")
  (when arg
    (setq mojo-package-arguments (read-from-minibuffer "Mojo package arguments: " "")))
  (if (eq mojo-package-arguments "")
      (compile (concat "mojo package "
                       ;; (string-trim-right (expand-file-name default-directory) "/?")
                       (directory-file-name (expand-file-name default-directory))
                       ))
    (compile (concat "mojo package " mojo-package-arguments))))

(defun mojo-format (&optional arg)
  "Run 'mojo format' for the current file."
  (interactive "P")
  (when arg
    (setq mojo-format-arguments
          (read-from-minibuffer "Mojo format arguments: " mojo-format-arguments)))
  (if (eq mojo-format-arguments "")
      (compile (concat "mojo format " buffer-file-name))
    (compile (concat "mojo format " mojo-format-arguments " " buffer-file-name))))

(defun mojo-test (&optional arg)
  "Run 'mojo test'."
  (interactive "P")
  (when arg
    (setq mojo-format-arguments
          (read-from-minibuffer "Mojo test arguments: " mojo-test-arguments)))
  (if (eq mojo-test-arguments "")
      (compile "mojo test")
    (compile (concat "mojo test " mojo-test-arguments))))

;;; Tentative keymap
;; (defvar-keymap mojo-mode-map
;;   :doc "Keymap for `mojo-mode'."
;;   "C-c C-c C-r" #'mojo-run
;;   "C-c C-c C-b" #'mojo-build
;;   "C-c C-c C-f" #'mojo-format
;;   "C-c C-c C-t" #'mojo-test
;;   )

(provide 'mojo-mode-ext)
;;; mojo-mode-ext.el ends here
