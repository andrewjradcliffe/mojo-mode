;;; mojo-mode.el --- Mojo support for Emacs -*- lexical-binding: t -*-

;;; Commentary:

;; Major mode for editing Mojo source code, based on Fabián E. Gallina's
;; python.el found in GNU Emacs.

;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 'seq)

;;;###autoload🔥
;; (add-to-list 'auto-mode-alist
;;              (cons (purecopy (concat (rx "." (or "mojo" "🔥")) "\\'")) 'mojo-mode))
(add-to-list 'auto-mode-alist `(,(concat (rx "." (or "mojo" "🔥")) "\\'") . mojo-mode))

(defgroup mojo-mode nil
  "Mojo support for Emacs."
  :group 'languages
  ;; :version "29.3"
  ;; :link '(emacs-commentary-link "mojo")
  )

;;; Bindings

(defvar mojo-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Movement
    (define-key map [remap backward-sentence] #'mojo-nav-backward-block)
    (define-key map [remap forward-sentence] #'mojo-nav-forward-block)
    (define-key map [remap backward-up-list] #'mojo-nav-backward-up-list)
    (define-key map [remap up-list] #'mojo-nav-up-list)
    (define-key map [remap mark-defun] #'mojo-mark-defun)
    (define-key map "\C-c\C-j" #'imenu)
    ;; Indent specific
    (define-key map "\177" #'mojo-indent-dedent-line-backspace)
    (define-key map (kbd "<backtab>") #'mojo-indent-dedent-line)
    (define-key map "\C-c<" #'mojo-indent-shift-left)
    (define-key map "\C-c>" #'mojo-indent-shift-right)
    ;; Skeletons
    (define-key map "\C-c\C-tc" #'mojo-skeleton-class)
    (define-key map "\C-c\C-td" #'mojo-skeleton-def)
    (define-key map "\C-c\C-tf" #'mojo-skeleton-for)
    (define-key map "\C-c\C-ti" #'mojo-skeleton-if)
    (define-key map "\C-c\C-tm" #'mojo-skeleton-import)
    (define-key map "\C-c\C-tt" #'mojo-skeleton-try)
    (define-key map "\C-c\C-tw" #'mojo-skeleton-while)
    ;; Shell interaction
    (define-key map "\C-c\C-p" #'run-mojo)
    (define-key map "\C-c\C-s" #'mojo-shell-send-string)
    (define-key map "\C-c\C-e" #'mojo-shell-send-statement)
    (define-key map "\C-c\C-r" #'mojo-shell-send-region)
    (define-key map "\C-\M-x"  #'mojo-shell-send-defun)
    (define-key map "\C-c\C-c" #'mojo-shell-send-buffer)
    (define-key map "\C-c\C-l" #'mojo-shell-send-file)
    (define-key map "\C-c\C-z" #'mojo-shell-switch-to-shell)
    ;; Some util commands
    (define-key map "\C-c\C-v" #'mojo-check)
    (define-key map "\C-c\C-f" #'mojo-eldoc-at-point)
    (define-key map "\C-c\C-d" #'mojo-describe-at-point)
    ;; Import management
    (define-key map "\C-c\C-ia" #'mojo-add-import)
    (define-key map "\C-c\C-if" #'mojo-fix-imports)
    (define-key map "\C-c\C-ir" #'mojo-remove-import)
    (define-key map "\C-c\C-is" #'mojo-sort-imports)
    ;; Utilities
    (substitute-key-definition #'complete-symbol #'completion-at-point
                               map global-map)
    (easy-menu-define mojo-menu map "Mojo Mode menu"
      '("Mojo"
        :help "Mojo-specific Features"
        ["Shift region left" mojo-indent-shift-left :active mark-active
         :help "Shift region left by a single indentation step"]
        ["Shift region right" mojo-indent-shift-right :active mark-active
         :help "Shift region right by a single indentation step"]
        "-"
        ["Start of def/class" beginning-of-defun
         :help "Go to start of outermost definition around point"]
        ["End of def/class" end-of-defun
         :help "Go to end of definition around point"]
        ["Mark def/class" mark-defun
         :help "Mark outermost definition around point"]
        ["Jump to def/class" imenu
         :help "Jump to a class or function definition"]
        "--"
        ("Skeletons")
        "---"
        ["Start interpreter" run-mojo
         :help "Run inferior Mojo process in a separate buffer"]
        ["Switch to shell" mojo-shell-switch-to-shell
         :help "Switch to running inferior Mojo process"]
        ["Eval string" mojo-shell-send-string
         :help "Eval string in inferior Mojo session"]
        ["Eval buffer" mojo-shell-send-buffer
         :help "Eval buffer in inferior Mojo session"]
        ["Eval statement" mojo-shell-send-statement
         :help "Eval statement in inferior Mojo session"]
        ["Eval region" mojo-shell-send-region
         :help "Eval region in inferior Mojo session"]
        ["Eval defun" mojo-shell-send-defun
         :help "Eval defun in inferior Mojo session"]
        ["Eval file" mojo-shell-send-file
         :help "Eval file in inferior Mojo session"]
        ["Debugger" pdb :help "Run pdb under GUD"]
        "----"
        ["Check file" mojo-check
         :help "Check file for errors"]
        ["Help on symbol" mojo-eldoc-at-point
         :help "Get help on symbol at point"]
        ["Complete symbol" completion-at-point
         :help "Complete symbol before point"]
        "-----"
        ["Add import" mojo-add-import
         :help "Add an import statement to the top of this buffer"]
        ["Remove import" mojo-remove-import
         :help "Remove an import statement from the top of this buffer"]
        ["Sort imports" mojo-sort-imports
         :help "Sort the import statements at the top of this buffer"]
        ["Fix imports" mojo-fix-imports
         :help "Add missing imports and remove unused ones from the current buffer"]
        ))
    map)
  "Keymap for `mojo-mode'.")

;;; Mojo specialized rx

(defmacro mojo-rx (&rest regexps)
  "Mojo mode specialized rx macro.
This variant of `rx' supports common Mojo named REGEXPS."
  `(rx-let ((sp-bsnl (or space (and ?\\ ?\n)))
            (block-start       (seq symbol-start
                                    (or "fn" "struct" "trait"
                                        ;; Python
                                        "def" "class" "if" "elif" "else" "try"
                                        "except" "finally" "for" "while" "with"
                                        ;; Python 3.10+ PEP634
                                        "match" "case"
                                        ;; Python 3.5+ PEP492
                                        (and "async" (+ space)
                                             (or "fn"
                                                 ;; Python
                                                 "def" "for" "with")))
                                    symbol-end))
            (dedenter          (seq symbol-start
                                    (or "elif" "else" "except" "finally" "case")
                                    symbol-end))
            (block-ender       (seq symbol-start
                                    (or
                                     "break" "continue" "pass" "raise" "return")
                                    symbol-end))
            (decorator         (seq line-start (* space) ?@ (any letter ?_)
                                    (* (any word ?_))))
            (defun             (seq symbol-start
                                    (or "fn" "struct" "trait"
                                        ;; Python
                                        "def" "class"
                                        ;; Python 3.5+ PEP492
                                        (and "async" (+ space) (or "fn" "def")))
                                    symbol-end))
            (if-name-main      (seq line-start "if" (+ space) "__name__"
                                    (+ space) "==" (+ space)
                                    (any ?' ?\") "__main__" (any ?' ?\")
                                    (* space) ?:))
            (symbol-name       (seq (any letter ?_) (* (any word ?_))))
            (assignment-target (seq (? ?*)
                                    (* symbol-name ?.) symbol-name
                                    (? ?\[ (+ (not ?\])) ?\])))
            (grouped-assignment-target (seq (? ?*)
                                            (* symbol-name ?.) (group symbol-name)
                                            (? ?\[ (+ (not ?\])) ?\])))
            (open-paren        (or "{" "[" "("))
            (close-paren       (or "}" "]" ")"))
            (simple-operator   (any ?+ ?- ?/ ?& ?^ ?~ ?| ?* ?< ?> ?= ?%))
            (not-simple-operator (not (or simple-operator ?\n)))
            (operator          (or "==" ">="
                                   "**" "//" "<<" ">>" "<=" "!="
                                   "+" "-" "/" "&" "^" "~" "|" "*" "<" ">"
                                   "=" "%"))
            (assignment-operator (or "+=" "-=" "*=" "/=" "//=" "%=" "**="
                                     ">>=" "<<=" "&=" "^=" "|="
                                     "="))
            (string-delimiter  (seq
                                ;; Match even number of backslashes.
                                (or (not (any ?\\ ?\' ?\")) point
                                    ;; Quotes might be preceded by an
                                    ;; escaped quote.
                                    (and (or (not (any ?\\)) point) ?\\
                                         (* ?\\ ?\\) (any ?\' ?\")))
                                (* ?\\ ?\\)
                                ;; Match single or triple quotes of any kind.
                                (group (or  "\"\"\"" "\"" "'''" "'"))))
            (coding-cookie (seq line-start ?# (* space)
                                (or
                                 ;; # coding=<encoding name>
                                 (: "coding" (or ?: ?=) (* space)
                                    (group-n 1 (+ (or word ?-))))
                                 ;; # -*- coding: <encoding name> -*-
                                 (: "-*-" (* space) "coding:" (* space)
                                    (group-n 1 (+ (or word ?-)))
                                    (* space) "-*-")
                                 ;; # vim: set fileencoding=<encoding name> :
                                 (: "vim:" (* space) "set" (+ space)
                                    "fileencoding" (* space) ?= (* space)
                                    (group-n 1 (+ (or word ?-)))
                                    (* space) ":"))))
            (bytes-escape-sequence
             (seq (not "\\")
                  (group (or "\\\\" "\\'" "\\a" "\\b" "\\f"
                             "\\n" "\\r" "\\t" "\\v"
                             (seq "\\" (** 1 3 (in "0-7")))
                             (seq "\\x" hex hex)))))
            (string-escape-sequence
             (or bytes-escape-sequence
                 (seq (not "\\")
                      (or (group-n 1 "\\u" (= 4 hex))
                          (group-n 1 "\\U" (= 8 hex))
                          (group-n 1 "\\N{" (*? anychar) "}"))))))
     (rx ,@regexps)))

;;; Font-lock and syntax

(eval-and-compile
  (defun mojo-syntax--context-compiler-macro (form type &optional syntax-ppss)
    (pcase type
      (''comment
       `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
          (and (nth 4 ppss) (nth 8 ppss))))
      (''string
       `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
          (and (nth 3 ppss) (nth 8 ppss))))
      (''single-quoted-string
       `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
          (and (characterp (nth 3 ppss)) (nth 8 ppss))))
      (''triple-quoted-string
       `(let ((ppss (or ,syntax-ppss (syntax-ppss))))
          (and (eq t (nth 3 ppss)) (nth 8 ppss))))
      (''paren
       `(nth 1 (or ,syntax-ppss (syntax-ppss))))
      (_ form))))

(defun mojo-syntax-context (type &optional syntax-ppss)
  "Return non-nil if point is on TYPE using SYNTAX-PPSS.
TYPE can be `comment', `string', `single-quoted-string',
`triple-quoted-string' or `paren'.  It returns the start
character address of the specified TYPE."
  (declare (compiler-macro mojo-syntax--context-compiler-macro))
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (pcase type
      ('comment (and (nth 4 ppss) (nth 8 ppss)))
      ('string (and (nth 3 ppss) (nth 8 ppss)))
      ('single-quoted-string (and (characterp (nth 3 ppss)) (nth 8 ppss)))
      ('triple-quoted-string (and (eq t (nth 3 ppss)) (nth 8 ppss)))
      ('paren (nth 1 ppss))
      (_ nil))))

(defun mojo-syntax-context-type (&optional syntax-ppss)
  "Return the context type using SYNTAX-PPSS.
The type returned can be `comment', `string' or `paren'."
  (let ((ppss (or syntax-ppss (syntax-ppss))))
    (cond
     ((nth 8 ppss) (if (nth 4 ppss) 'comment 'string))
     ((nth 1 ppss) 'paren))))

(defsubst mojo-syntax-comment-or-string-p (&optional ppss)
  "Return non-nil if PPSS is inside comment or string."
  (nth 8 (or ppss (syntax-ppss))))

(defsubst mojo-syntax-closing-paren-p ()
  "Return non-nil if char after point is a closing paren."
  (eql (syntax-class (syntax-after (point)))
       (syntax-class (string-to-syntax ")"))))

(defun mojo-font-lock-syntactic-face-function (state)
  "Return syntactic face given STATE."
  (if (nth 3 state)
      (if (mojo-info-docstring-p state)
          font-lock-doc-face
        font-lock-string-face)
    font-lock-comment-face))

(defconst mojo--f-string-start-regexp
  (rx bow
      (or "f" "F" "fr" "Fr" "fR" "FR" "rf" "rF" "Rf" "RF")
      (or "\"" "\"\"\"" "'" "'''"))
  "A regular expression matching the beginning of an f-string.

See URL `https://docs.python.org/3/reference/lexical_analysis.html#string-and-bytes-literals'.")

(defun mojo--f-string-p (ppss)
  "Return non-nil if the pos where PPSS was found is inside an f-string."
  (and (nth 3 ppss)
       (let* ((spos (1- (nth 8 ppss)))
              (before-quote
               (buffer-substring-no-properties (max (- spos 4) (point-min))
                                               (min (+ spos 2) (point-max)))))
         (and (string-match-p mojo--f-string-start-regexp before-quote)
              (or (< (point-min) spos)
                  (not (memq (char-syntax (char-before spos)) '(?w ?_))))))))

(defun mojo--font-lock-f-strings (limit)
  "Mark {...} holes as being code.
Remove the (presumably `font-lock-string-face') `face' property from
the {...} holes that appear within f-strings."
  ;; FIXME: This will fail to properly highlight strings appearing
  ;; within the {...} of an f-string.
  ;; We could presumably fix it by running
  ;; `font-lock-default-fontify-syntactically-region' (as is done in
  ;; `sm-c--cpp-fontify-syntactically', for example) after removing
  ;; the `face' property, but I'm not sure it's worth the effort and
  ;; the risks.
  (let ((ppss (syntax-ppss)))
    (while
        (progn
          (while (and (not (mojo--f-string-p ppss))
                      (re-search-forward mojo--f-string-start-regexp limit 'move))
            (setq ppss (syntax-ppss)))
          (< (point) limit))
      (cl-assert (mojo--f-string-p ppss))
      (let ((send (save-excursion
                    (goto-char (nth 8 ppss))
                    (condition-case nil
                        (progn (let ((forward-sexp-function nil))
                                 (forward-sexp 1))
                               (min limit (1- (point))))
                      (scan-error limit)))))
        (while (re-search-forward "{" send t)
          (if (eq ?\{ (char-after))
              (forward-char 1)          ;Just skip over {{
            (let ((beg (match-beginning 0))
                  (end (condition-case nil
                           (progn (up-list 1) (min send (point)))
                         (scan-error send))))
              (goto-char end)
              (put-text-property beg end 'face nil))))
        (goto-char (min limit (1+ send)))
        (setq ppss (syntax-ppss))))))

(defconst mojo--not-raw-bytes-literal-start-regexp
  (rx (or bos (not alnum)) (or "b" "B") (or "\"" "\"\"\"" "'" "'''") eos)
  "A regular expression matching the start of a not-raw bytes literal.")

(defconst mojo--not-raw-string-literal-start-regexp
  (rx bos (or
           ;; Multi-line string literals
           (seq (? (? (not alnum)) (or "u" "U" "F" "f")) (or "\"\"\"" "'''"))
           (seq (? anychar) (not alnum) (or "\"\"\"" "'''"))
           ;; Single line string literals
           (seq (? (** 0 2 anychar) (not alnum)) (or "u" "U" "F" "f") (or "'" "\""))
           (seq (? (** 0 3 anychar) (not (any "'\"" alnum))) (or "'" "\"")))
      eos)
  "A regular expression matching the start of a not-raw string literal.")

(defun mojo--string-bytes-literal-matcher (regexp start-regexp)
  "Match REGEXP within a string or bytes literal whose start matches START-REGEXP."
  (lambda (limit)
    (cl-loop for result = (re-search-forward regexp limit t)
             for result-valid = (and
                                 result
                                 (when-let* ((pos (nth 8 (syntax-ppss)))
                                             (before-quote
                                              (buffer-substring-no-properties
                                               (max (- pos 4) (point-min))
                                               (min (+ pos 1) (point-max)))))
                                   (backward-char)
                                   (string-match-p start-regexp before-quote)))
             until (or (not result) result-valid)
             finally return (and result-valid result))))

(defvar mojo-font-lock-keywords-level-1
  `((,(mojo-rx symbol-start "def" (1+ space) (group symbol-name))
     (1 font-lock-function-name-face))
    (,(mojo-rx symbol-start "class" (1+ space) (group symbol-name))
     (1 font-lock-type-face))
    (,(mojo-rx symbol-start "fn" (1+ space) (group symbol-name))
     (1 font-lock-function-name-face))
    (,(mojo-rx symbol-start "struct" (1+ space) (group symbol-name))
     (1 font-lock-type-face))
    (,(mojo-rx symbol-start "trait" (1+ space) (group symbol-name))
     (1 font-lock-type-face))
    )
  "Font lock keywords to use in `mojo-mode' for level 1 decoration.

This is the minimum decoration level, including function and
class declarations.")

(defvar mojo-font-lock-keywords-level-2
  `(,@mojo-font-lock-keywords-level-1
    ,(rx symbol-start
         (or
          "fn" "struct" "trait"
          ;; Python
          "and" "del" "from" "not" "while" "as" "elif" "global" "or" "with"
          "assert" "else" "if" "pass" "yield" "break" "except" "import" "class"
          "in" "raise" "continue" "finally" "is" "return" "def" "for" "lambda"
          "try"
          ;; False, None, and True are listed as keywords on the Python 3
          ;; documentation, but since they also qualify as constants they are
          ;; fontified like that in order to keep font-lock consistent between
          ;; Python versions.
          "nonlocal"
          ;; Python 3.5+ PEP492
          (and "async" (+ space) (or "fn"
                                     ;; Python
                                     "def" "for" "with"))
          "await"
          ;; Python 3.10+
          "match" "case"
          ;; Extra:
          "self")
         symbol-end)
    ;; Builtins
    (,(rx symbol-start
          (or
           "abs" "all" "any" "bin" "bool" "callable" "chr" "classmethod"
           "compile" "complex" "delattr" "dict" "dir" "divmod" "enumerate"
           "eval" "filter" "float" "format" "frozenset" "getattr" "globals"
           "hasattr" "hash" "help" "hex" "id" "input" "int" "isinstance"
           "issubclass" "iter" "len" "list" "locals" "map" "max" "memoryview"
           "min" "next" "object" "oct" "open" "ord" "pow" "print" "property"
           "range" "repr" "reversed" "round" "set" "setattr" "slice" "sorted"
           "staticmethod" "str" "sum" "super" "tuple" "type" "vars" "zip"
           "__import__"
           ;; Mojo 2:
           "basestring" "cmp" "execfile" "file" "long" "raw_input" "reduce"
           "reload" "unichr" "unicode" "xrange" "apply" "buffer" "coerce"
           "intern"
           ;; Mojo 3:
           "ascii" "breakpoint" "bytearray" "bytes" "exec"
           ;; Special attributes:
           ;; https://docs.python.org/3/reference/datamodel.html
           "__annotations__" "__closure__" "__code__"
           "__defaults__" "__dict__" "__doc__" "__globals__"
           "__kwdefaults__" "__name__" "__module__" "__package__"
           "__qualname__"
           ;; Extras:
           "__all__")
          symbol-end) . font-lock-builtin-face))
  "Font lock keywords to use in `mojo-mode' for level 2 decoration.

This is the medium decoration level, including everything in
`mojo-font-lock-keywords-level-1', as well as keywords and
builtins.")

(defun mojo-font-lock-assignment-matcher (regexp)
  "Font lock matcher for assignments based on REGEXP.
Search for next occurrence if REGEXP matched within a `paren'
context (to avoid, e.g., default values for arguments or passing
arguments by name being treated as assignments) or is followed by
an '=' sign (to avoid '==' being treated as an assignment.  Set
point to the position one character before the end of the
occurrence found so that subsequent searches can detect the '='
sign in chained assignment."
  (lambda (limit)
    (cl-loop while (re-search-forward regexp limit t)
             unless (or (mojo-syntax-context 'paren)
                        (equal (char-after) ?=))
             return (progn (backward-char) t))))

(defvar mojo-font-lock-keywords-maximum-decoration
  `((mojo--font-lock-f-strings)
    ,@mojo-font-lock-keywords-level-2
    ;; Constants
    (,(rx symbol-start
          (or
           "Ellipsis" "False" "None" "NotImplemented" "True" "__debug__"
           ;; copyright, license, credits, quit and exit are added by the site
           ;; module and they are not intended to be used in programs
           "copyright" "credits" "exit" "license" "quit")
          symbol-end)
     . font-lock-constant-face)
    ;; Decorators.
    (,(rx line-start (* (any " \t")) (group "@" (1+ (or word ?_))
                                            (0+ "." (1+ (or word ?_)))))
     (1 font-lock-type-face))
    ;; Builtin Exceptions
    (,(rx symbol-start
          (or
           ;; Mojo 2 and 3:
           "ArithmeticError" "AssertionError" "AttributeError" "BaseException"
           "BufferError" "BytesWarning" "DeprecationWarning" "EOFError"
           "EnvironmentError" "Exception" "FloatingPointError" "FutureWarning"
           "GeneratorExit" "IOError" "ImportError" "ImportWarning"
           "IndentationError" "IndexError" "KeyError" "KeyboardInterrupt"
           "LookupError" "MemoryError" "NameError" "NotImplementedError"
           "OSError" "OverflowError" "PendingDeprecationWarning"
           "ReferenceError" "RuntimeError" "RuntimeWarning" "StopIteration"
           "SyntaxError" "SyntaxWarning" "SystemError" "SystemExit" "TabError"
           "TypeError" "UnboundLocalError" "UnicodeDecodeError"
           "UnicodeEncodeError" "UnicodeError" "UnicodeTranslateError"
           "UnicodeWarning" "UserWarning" "ValueError" "Warning"
           "ZeroDivisionError"
           ;; Mojo 2:
           "StandardError"
           ;; Mojo 3:
           "BlockingIOError" "BrokenPipeError" "ChildProcessError"
           "ConnectionAbortedError" "ConnectionError" "ConnectionRefusedError"
           "ConnectionResetError" "FileExistsError" "FileNotFoundError"
           "InterruptedError" "IsADirectoryError" "NotADirectoryError"
           "PermissionError" "ProcessLookupError" "RecursionError"
           "ResourceWarning" "StopAsyncIteration" "TimeoutError"
           ;; OS specific
           "VMSError" "WindowsError"
           )
          symbol-end)
     . font-lock-type-face)
    ;; multiple assignment
    ;; (note that type hints are not allowed for multiple assignments)
    ;;   a, b, c = 1, 2, 3
    ;;   a, *b, c = 1, 2, 3, 4, 5
    ;;   [a, b] = (1, 2)
    ;;   (l[1], l[2]) = (10, 11)
    ;;   (a, b, c, *d) = *x, y = 5, 6, 7, 8, 9
    ;;   (a,) = 'foo'
    ;;   (*a,) = ['foo', 'bar', 'baz']
    ;;   d.x, d.y[0], *d.z = 'a', 'b', 'c', 'd', 'e'
    ;; and variants thereof
    ;; the cases
    ;;   (a) = 5
    ;;   [a] = 5,
    ;;   [*a] = 5, 6
    ;; are handled separately below
    (,(mojo-font-lock-assignment-matcher
       (mojo-rx (? (or "[" "(") (* space))
                grouped-assignment-target (* space) ?, (* space)
                (* assignment-target (* space) ?, (* space))
                (? assignment-target (* space))
                (? ?, (* space))
                (? (or ")" "]") (* space))
                (group assignment-operator)))
     (1 font-lock-variable-name-face)
     (2 'font-lock-operator-face)
     (,(mojo-rx grouped-assignment-target)
      (progn
        (goto-char (match-end 1))       ; go back after the first symbol
        (match-beginning 2))            ; limit the search until the assignment
      nil
      (1 font-lock-variable-name-face)))
    ;; single assignment with type hints, e.g.
    ;;   a: int = 5
    ;;   b: Tuple[Optional[int], Union[Sequence[str], str]] = (None, 'foo')
    ;;   c: Collection = {1, 2, 3}
    ;;   d: Mapping[int, str] = {1: 'bar', 2: 'baz'}
    (,(mojo-font-lock-assignment-matcher
       (mojo-rx (or line-start ?\;) (* space)
                grouped-assignment-target (* space)
                (? ?: (* space) (+ not-simple-operator) (* space))
                (group assignment-operator)))
     (1 font-lock-variable-name-face)
     (2 'font-lock-operator-face))
    ;; special cases
    ;;   (a) = 5
    ;;   [a] = 5,
    ;;   [*a] = 5, 6
    (,(mojo-font-lock-assignment-matcher
       (mojo-rx (or line-start ?\; ?=) (* space)
                (or "[" "(") (* space)
                grouped-assignment-target (* space)
                (or ")" "]") (* space)
                (group assignment-operator)))
     (1 font-lock-variable-name-face)
     (2 'font-lock-operator-face))
    ;; Operators.
    (,(mojo-rx operator) . 'font-lock-operator-face)
    ;; escape sequences within bytes literals
    ;;   "\\" "\'" "\a" "\b" "\f" "\n" "\r" "\t" "\v"
    ;;   "\ooo" character with octal value ooo
    ;;   "\xhh" character with hex value hh
    (,(mojo--string-bytes-literal-matcher
       (mojo-rx bytes-escape-sequence)
       mojo--not-raw-bytes-literal-start-regexp)
     (1 font-lock-constant-face t))
    ;; escape sequences within string literals, the same as appear in bytes
    ;; literals in addition to:
    ;;   "\uxxxx" Character with 16-bit hex value xxxx
    ;;   "\Uxxxxxxxx" Character with 32-bit hex value xxxxxxxx
    ;;   "\N{name}" Character named name in the Unicode database
    (,(mojo--string-bytes-literal-matcher
       (mojo-rx string-escape-sequence)
       mojo--not-raw-string-literal-start-regexp)
     (1 'font-lock-constant-face t)))
  "Font lock keywords to use in `mojo-mode' for maximum decoration.

This decoration level includes everything in
`mojo-font-lock-keywords-level-2', as well as constants,
decorators, exceptions, and assignments.")

(defvar mojo-font-lock-keywords
  '(mojo-font-lock-keywords-level-1   ; When `font-lock-maximum-decoration' is nil.
    mojo-font-lock-keywords-level-1   ; When `font-lock-maximum-decoration' is 1.
    mojo-font-lock-keywords-level-2   ; When `font-lock-maximum-decoration' is 2.
    mojo-font-lock-keywords-maximum-decoration ; When `font-lock-maximum-decoration'
                                        ; is more than 1, or t (which it is,
                                        ; by default).
    )
  "List of font lock keyword specifications to use in `mojo-mode'.

Which one will be chosen depends on the value of
`font-lock-maximum-decoration'.")


(defconst mojo-syntax-propertize-function
  (syntax-propertize-rules
   ((rx (or "\"\"\"" "'''"))
    (0 (ignore (mojo-syntax-stringify))))))

(define-obsolete-variable-alias 'mojo--prettify-symbols-alist
  'mojo-prettify-symbols-alist "26.1")

(defvar mojo-prettify-symbols-alist
  '(("lambda"  . ?λ)
    ("and" . ?∧)
    ("or" . ?∨))
  "Value for `prettify-symbols-alist' in `mojo-mode'.")

(defsubst mojo-syntax-count-quotes (quote-char &optional point limit)
  "Count number of quotes around point (max is 3).
QUOTE-CHAR is the quote char to count.  Optional argument POINT is
the point where scan starts (defaults to current point), and LIMIT
is used to limit the scan."
  (let ((i 0))
    (while (and (< i 3)
                (or (not limit) (< (+ point i) limit))
                (eq (char-after (+ point i)) quote-char))
      (setq i (1+ i)))
    i))

(defun mojo-syntax-stringify ()
  "Put `syntax-table' property correctly on single/triple quotes."
  (let* ((ppss (save-excursion (backward-char 3) (syntax-ppss)))
         (string-start (and (eq t (nth 3 ppss)) (nth 8 ppss)))
         (quote-starting-pos (- (point) 3))
         (quote-ending-pos (point)))
    (cond ((or (nth 4 ppss)             ;Inside a comment
               (and string-start
                    ;; Inside of a string quoted with different triple quotes.
                    (not (eql (char-after string-start)
                              (char-after quote-starting-pos)))))
           ;; Do nothing.
           nil)
          ((nth 5 ppss)
           ;; The first quote is escaped, so it's not part of a triple quote!
           (goto-char (1+ quote-starting-pos)))
          ((null string-start)
           ;; This set of quotes delimit the start of a string.  Put
           ;; string fence syntax on last quote. (bug#49518)
           ;; FIXME: This makes sexp-movement a bit suboptimal since """a"""
           ;; is now treated as 3 strings.
           ;; We could probably have our cake and eat it too by
           ;; putting the string fence on the first quote and then
           ;; convincing `syntax-ppss-flush-cache' to flush to before
           ;; that fence when any char of the 3-char delimiter
           ;; is modified.
           (put-text-property (1- quote-ending-pos) quote-ending-pos
                              'syntax-table (string-to-syntax "|")))
          (t
           ;; This set of quotes delimit the end of a string.  Put
           ;; string fence syntax on first quote. (bug#49518)
           (put-text-property quote-starting-pos (1+ quote-starting-pos)
                              'syntax-table (string-to-syntax "|"))))))

(defvar mojo-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Give punctuation syntax to ASCII that normally has symbol
    ;; syntax or has word syntax and isn't a letter.
    (let ((symbol (string-to-syntax "_"))
          (sst (standard-syntax-table)))
      (dotimes (i 128)
        (unless (= i ?_)
          (if (equal symbol (aref sst i))
              (modify-syntax-entry i "." table)))))
    (modify-syntax-entry ?$ "." table)
    (modify-syntax-entry ?% "." table)
    ;; exceptions
    (modify-syntax-entry ?# "<" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?` "$" table)
    table)
  "Syntax table for Mojo files.")

(defvar mojo-dotty-syntax-table
  (let ((table (make-syntax-table mojo-mode-syntax-table)))
    (modify-syntax-entry ?. "w" table)
    (modify-syntax-entry ?_ "w" table)
    table)
  "Dotty syntax table for Mojo files.
It makes underscores and dots word constituent chars.")

;;; Indentation

(defcustom mojo-indent-offset 4
  "Default indentation offset for Mojo."
  :type 'integer
  :safe 'integerp)

(defcustom mojo-indent-guess-indent-offset t
  "Non-nil tells Mojo mode to guess `mojo-indent-offset' value."
  :type 'boolean
  :safe 'booleanp)

(defcustom mojo-indent-guess-indent-offset-verbose t
  "Non-nil means to emit a warning when indentation guessing fails."
  :version "25.1"
  :type 'boolean
  :safe' booleanp)

(defcustom mojo-indent-trigger-commands
  '(indent-for-tab-command yas-expand yas/expand)
  "Commands that might trigger a `mojo-indent-line' call."
  :type '(repeat symbol))

(defcustom mojo-indent-def-block-scale 2
  "Multiplier applied to indentation inside multi-line def blocks."
  :version "26.1"
  :type 'integer
  :safe 'natnump)

(defvar mojo-indent-current-level 0
  "Deprecated var available for compatibility.")

(defvar mojo-indent-levels '(0)
  "Deprecated var available for compatibility.")

(make-obsolete-variable
 'mojo-indent-current-level
 "The indentation API changed to avoid global state.
The function `mojo-indent-calculate-levels' does not use it
anymore.  If you were defadvising it and or depended on this
variable for indentation customizations, refactor your code to
work on `mojo-indent-calculate-indentation' instead."
 "24.5")

(make-obsolete-variable
 'mojo-indent-levels
 "The indentation API changed to avoid global state.
The function `mojo-indent-calculate-levels' does not use it
anymore.  If you were defadvising it and or depended on this
variable for indentation customizations, refactor your code to
work on `mojo-indent-calculate-indentation' instead."
 "24.5")

(defun mojo-indent-guess-indent-offset ()
  "Guess and set `mojo-indent-offset' for the current buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((block-end))
        (while (and (not block-end)
                    (re-search-forward
                     (mojo-rx line-start block-start) nil t))
          (when (and
                 (not (mojo-syntax-context-type))
                 (progn
                   (goto-char (line-end-position))
                   (mojo-util-forward-comment -1)
                   (if (equal (char-before) ?:)
                       t
                     (forward-line 1)
                     (when (mojo-info-block-continuation-line-p)
                       (while (and (mojo-info-continuation-line-p)
                                   (not (eobp)))
                         (forward-line 1))
                       (mojo-util-forward-comment -1)
                       (when (equal (char-before) ?:)
                         t)))))
            (setq block-end (point-marker))))
        (let ((indentation
               (when block-end
                 (goto-char block-end)
                 (mojo-util-forward-comment)
                 (current-indentation))))
          (if (and indentation (not (zerop indentation)))
              (setq-local mojo-indent-offset indentation)
            (when mojo-indent-guess-indent-offset-verbose
              (message "Can't guess mojo-indent-offset, using defaults: %s"
                       mojo-indent-offset))))))))

(defun mojo-indent-context ()
  "Get information about the current indentation context.
Context is returned in a cons with the form (STATUS . START).

STATUS can be one of the following:

keyword
-------

:after-comment
 - Point is after a comment line.
 - START is the position of the \"#\" character.
:inside-string
 - Point is inside string.
 - START is the position of the first quote that starts it.
:no-indent
 - No possible indentation case matches.
 - START is always zero.

:inside-paren
 - Fallback case when point is inside paren.
 - START is the first non space char position *after* the open paren.
:inside-paren-at-closing-nested-paren
 - Point is on a line that contains a nested paren closer.
 - START is the position of the open paren it closes.
:inside-paren-at-closing-paren
 - Point is on a line that contains a paren closer.
 - START is the position of the open paren.
:inside-paren-newline-start
 - Point is inside a paren with items starting in their own line.
 - START is the position of the open paren.
:inside-paren-newline-start-from-block
 - Point is inside a paren with items starting in their own line
   from a block start.
 - START is the position of the open paren.

:after-backslash
 - Fallback case when point is after backslash.
 - START is the char after the position of the backslash.
:after-backslash-assignment-continuation
 - Point is after a backslashed assignment.
 - START is the char after the position of the backslash.
:after-backslash-block-continuation
 - Point is after a backslashed block continuation.
 - START is the char after the position of the backslash.
:after-backslash-dotted-continuation
 - Point is after a backslashed dotted continuation.  Previous
   line must contain a dot to align with.
 - START is the char after the position of the backslash.
:after-backslash-first-line
 - First line following a backslashed continuation.
 - START is the char after the position of the backslash.

:after-block-end
 - Point is after a line containing a block ender.
 - START is the position where the ender starts.
:after-block-start
 - Point is after a line starting a block.
 - START is the position where the block starts.
:after-line
 - Point is after a simple line.
 - START is the position where the previous line starts.
:at-dedenter-block-start
 - Point is on a line starting a dedenter block.
 - START is the position where the dedenter block starts."
  (let ((ppss (save-excursion
                (beginning-of-line)
                (syntax-ppss))))
    (cond
     ;; Beginning of buffer.
     ((= (line-number-at-pos) 1)
      (cons :no-indent 0))
     ;; Inside a string.
     ((let ((start (mojo-syntax-context 'string ppss)))
        (when start
          (cons (if (mojo-info-docstring-p)
                    :inside-docstring
                  :inside-string) start))))
     ;; Inside a paren.
     ((let* ((start (mojo-syntax-context 'paren ppss))
             (starts-in-newline
              (when start
                (save-excursion
                  (goto-char start)
                  (forward-char)
                  (not
                   (= (line-number-at-pos)
                      (progn
                        (mojo-util-forward-comment)
                        (line-number-at-pos))))))))
        (when start
          (cond
           ;; Current line only holds the closing paren.
           ((save-excursion
              (skip-syntax-forward " ")
              (when (and (mojo-syntax-closing-paren-p)
                         (progn
                           (forward-char 1)
                           (not (mojo-syntax-context 'paren))))
                (cons :inside-paren-at-closing-paren start))))
           ;; Current line only holds a closing paren for nested.
           ((save-excursion
              (back-to-indentation)
              (mojo-syntax-closing-paren-p))
            (cons :inside-paren-at-closing-nested-paren start))
           ;; This line starts from an opening block in its own line.
           ((save-excursion
              (goto-char start)
              (when (and
                     starts-in-newline
                     (save-excursion
                       (back-to-indentation)
                       (looking-at (mojo-rx block-start))))
                (cons
                 :inside-paren-newline-start-from-block start))))
           (starts-in-newline
            (cons :inside-paren-newline-start start))
           ;; General case.
           (t (cons :inside-paren
                    (save-excursion
                      (goto-char (1+ start))
                      (skip-syntax-forward "(" 1)
                      (skip-syntax-forward " ")
                      (point))))))))
     ;; After backslash.
     ((let ((start (when (not (mojo-syntax-comment-or-string-p ppss))
                     (mojo-info-line-ends-backslash-p
                      (1- (line-number-at-pos))))))
        (when start
          (cond
           ;; Continuation of dotted expression.
           ((save-excursion
              (back-to-indentation)
              (when (eq (char-after) ?\.)
                ;; Move point back until it's not inside a paren.
                (while (prog2
                           (forward-line -1)
                           (and (not (bobp))
                                (mojo-syntax-context 'paren))))
                (goto-char (line-end-position))
                (while (and (search-backward
                             "." (line-beginning-position) t)
                            (mojo-syntax-context-type)))
                ;; Ensure previous statement has dot to align with.
                (when (and (eq (char-after) ?\.)
                           (not (mojo-syntax-context-type)))
                  (cons :after-backslash-dotted-continuation (point))))))
           ;; Continuation of block definition.
           ((let ((block-continuation-start
                   (mojo-info-block-continuation-line-p)))
              (when block-continuation-start
                (save-excursion
                  (goto-char block-continuation-start)
                  (re-search-forward
                   (mojo-rx block-start (* space))
                   (line-end-position) t)
                  (cons :after-backslash-block-continuation (point))))))
           ;; Continuation of assignment.
           ((let ((assignment-continuation-start
                   (mojo-info-assignment-continuation-line-p)))
              (when assignment-continuation-start
                (save-excursion
                  (goto-char assignment-continuation-start)
                  (cons :after-backslash-assignment-continuation (point))))))
           ;; First line after backslash continuation start.
           ((save-excursion
              (goto-char start)
              (when (or (= (line-number-at-pos) 1)
                        (not (mojo-info-beginning-of-backslash
                              (1- (line-number-at-pos)))))
                (cons :after-backslash-first-line start))))
           ;; General case.
           (t (cons :after-backslash start))))))
     ;; After beginning of block.
     ((let ((start (save-excursion
                     (back-to-indentation)
                     (mojo-util-forward-comment -1)
                     (when (equal (char-before) ?:)
                       (mojo-nav-beginning-of-block)))))
        (when start
          (cons :after-block-start start))))
     ;; At dedenter statement.
     ((let ((start (mojo-info-dedenter-statement-p)))
        (when start
          (cons :at-dedenter-block-start start))))
     ;; After normal line, comment or ender (default case).
     ((save-excursion
        (back-to-indentation)
        (skip-chars-backward " \t\n")
        (if (bobp)
            (cons :no-indent 0)
          (mojo-nav-beginning-of-statement)
          (cons
           (cond ((mojo-info-current-line-comment-p)
                  :after-comment)
                 ((save-excursion
                    (goto-char (line-end-position))
                    (mojo-util-forward-comment -1)
                    (mojo-nav-beginning-of-statement)
                    (looking-at (mojo-rx block-ender)))
                  :after-block-end)
                 (t :after-line))
           (point))))))))

(defun mojo-indent--calculate-indentation ()
  "Internal implementation of `mojo-indent-calculate-indentation'.
May return an integer for the maximum possible indentation at
current context or a list of integers.  The latter case is only
happening for :at-dedenter-block-start context since the
possibilities can be narrowed to specific indentation points."
  (save-excursion
    (pcase (mojo-indent-context)
      (`(:no-indent . ,_) (prog-first-column)) ; usually 0
      (`(,(or :after-line
              :after-comment
              :inside-string
              :after-backslash) . ,start)
       ;; Copy previous indentation.
       (goto-char start)
       (current-indentation))
      (`(,(or :inside-paren-at-closing-paren
              :inside-paren-at-closing-nested-paren) . ,start)
       (goto-char (+ 1 start))
       (if (looking-at "[ \t]*\\(?:#\\|$\\)")
           ;; Copy previous indentation.
           (current-indentation)
         ;; Align with opening paren.
         (current-column)))
      (`(:inside-docstring . ,start)
       (let* ((line-indentation (current-indentation))
              (base-indent (progn
                             (goto-char start)
                             (current-indentation))))
         (max line-indentation base-indent)))
      (`(,(or :after-block-start
              :after-backslash-first-line
              :after-backslash-assignment-continuation
              :inside-paren-newline-start) . ,start)
       ;; Add one indentation level.
       (goto-char start)
       (+ (current-indentation) mojo-indent-offset))
      (`(:after-backslash-block-continuation . ,start)
       (goto-char start)
       (let ((column (current-column)))
         (if (= column (+ (current-indentation) mojo-indent-offset))
             ;; Add one level to avoid same indent as next logical line.
             (+ column mojo-indent-offset)
           column)))
      (`(,(or :inside-paren
              :after-backslash-dotted-continuation) . ,start)
       ;; Use the column given by the context.
       (goto-char start)
       (current-column))
      (`(:after-block-end . ,start)
       ;; Subtract one indentation level.
       (goto-char start)
       (- (current-indentation) mojo-indent-offset))
      (`(:at-dedenter-block-start . ,_)
       ;; List all possible indentation levels from opening blocks.
       (let ((opening-block-start-points
              (mojo-info-dedenter-opening-block-positions)))
         (if (not opening-block-start-points)
             (prog-first-column) ; if not found default to first column
           (mapcar (lambda (pos)
                     (save-excursion
                       (goto-char pos)
                       (current-indentation)))
                   opening-block-start-points))))
      (`(,(or :inside-paren-newline-start-from-block) . ,start)
       (goto-char start)
       (+ (current-indentation)
          (* mojo-indent-offset mojo-indent-def-block-scale))))))

(defun mojo-indent--calculate-levels (indentation)
  "Calculate levels list given INDENTATION.
Argument INDENTATION can either be an integer or a list of
integers.  Levels are returned in ascending order, and in the
case INDENTATION is a list, this order is enforced."
  (if (listp indentation)
      (sort (copy-sequence indentation) #'<)
    (nconc (number-sequence (prog-first-column) (1- indentation)
                            mojo-indent-offset)
           (list indentation))))

(defun mojo-indent--previous-level (levels indentation)
  "Return previous level from LEVELS relative to INDENTATION."
  (let* ((levels (sort (copy-sequence levels) #'>))
         (default (car levels)))
    (catch 'return
      (dolist (level levels)
        (when (funcall #'< level indentation)
          (throw 'return level)))
      default)))

(defun mojo-indent-calculate-indentation (&optional previous)
  "Calculate indentation.
Get indentation of PREVIOUS level when argument is non-nil.
Return the max level of the cycle when indentation reaches the
minimum."
  (let* ((indentation (mojo-indent--calculate-indentation))
         (levels (mojo-indent--calculate-levels indentation)))
    (if previous
        (mojo-indent--previous-level levels (current-indentation))
      (if levels
          (apply #'max levels)
        (prog-first-column)))))

(defun mojo-indent-line (&optional previous)
  "Internal implementation of `mojo-indent-line-function'.
Use the PREVIOUS level when argument is non-nil, otherwise indent
to the maximum available level.  When indentation is the minimum
possible and PREVIOUS is non-nil, cycle back to the maximum
level."
  (let ((follow-indentation-p
         ;; Check if point is within indentation.
         (and (<= (line-beginning-position) (point))
              (>= (+ (line-beginning-position)
                     (current-indentation))
                  (point)))))
    (save-excursion
      (indent-line-to
       (mojo-indent-calculate-indentation previous))
      (mojo-info-dedenter-opening-block-message))
    (when follow-indentation-p
      (back-to-indentation))))

(defun mojo-indent-calculate-levels ()
  "Return possible indentation levels."
  (mojo-indent--calculate-levels
   (mojo-indent--calculate-indentation)))

(defun mojo-indent-line-function ()
  "`indent-line-function' for Mojo mode.
When the variable `last-command' is equal to one of the symbols
inside `mojo-indent-trigger-commands' it cycles possible
indentation levels from right to left."
  (mojo-indent-line
   (and (memq this-command mojo-indent-trigger-commands)
        (eq last-command this-command))))

(defun mojo-indent-dedent-line ()
  "De-indent current line."
  (interactive "*")
  (when (and (not (bolp))
             (not (mojo-syntax-comment-or-string-p))
             (= (current-indentation) (current-column)))
    (mojo-indent-line t)
    t))

(defun mojo-indent-dedent-line-backspace (arg)
  "De-indent current line.
Argument ARG is passed to `backward-delete-char-untabify' when
point is not in between the indentation."
  (interactive "*p")
  (unless (mojo-indent-dedent-line)
    (backward-delete-char-untabify arg)))

(put 'mojo-indent-dedent-line-backspace 'delete-selection 'supersede)

(defun mojo-indent-region (start end)
  "Indent a Mojo region automagically.

Called from a program, START and END specify the region to indent."
  (let ((deactivate-mark nil))
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp) (forward-line 1))
      (while (< (point) end)
        (or (and (bolp) (eolp))
            (when (and
                   ;; Skip if previous line is empty or a comment.
                   (save-excursion
                     (let ((line-is-comment-p
                            (mojo-info-current-line-comment-p)))
                       (forward-line -1)
                       (not
                        (or (and (mojo-info-current-line-comment-p)
                                 ;; Unless this line is a comment too.
                                 (not line-is-comment-p))
                            (mojo-info-current-line-empty-p)))))
                   ;; Don't mess with strings, unless it's the
                   ;; enclosing set of quotes or a docstring.
                   (or (not (mojo-syntax-context 'string))
                       (equal
                        (syntax-after
                         (+ (1- (point))
                            (current-indentation)
                            (mojo-syntax-count-quotes (char-after) (point))))
                        (string-to-syntax "|"))
                       (mojo-info-docstring-p))
                   ;; Skip if current line is a block start, a
                   ;; dedenter or block ender.
                   (save-excursion
                     (back-to-indentation)
                     (not (looking-at
                           (mojo-rx
                            (or block-start dedenter block-ender))))))
              (mojo-indent-line)))
        (forward-line 1))
      (move-marker end nil))))

(defun mojo-indent-shift-left (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the left.
COUNT defaults to `mojo-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie.  An error is signaled if
any lines in the region are indented less than COUNT columns."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (if count
      (setq count (prefix-numeric-value count))
    (setq count mojo-indent-offset))
  (when (> count 0)
    (let ((deactivate-mark nil))
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (if (and (< (current-indentation) count)
                   (not (looking-at "[ \t]*$")))
              (user-error "Can't shift all lines enough"))
          (forward-line))
        (indent-rigidly start end (- count))))))

(defun mojo-indent-shift-right (start end &optional count)
  "Shift lines contained in region START END by COUNT columns to the right.
COUNT defaults to `mojo-indent-offset'.  If region isn't
active, the current line is shifted.  The shifted region includes
the lines in which START and END lie."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end) current-prefix-arg)
     (list (line-beginning-position) (line-end-position) current-prefix-arg)))
  (let ((deactivate-mark nil))
    (setq count (if count (prefix-numeric-value count)
                  mojo-indent-offset))
    (indent-rigidly start end count)))

(defun mojo-indent-post-self-insert-function ()
  "Adjust indentation after insertion of some characters.
This function is intended to be added to `post-self-insert-hook.'
If a line renders a paren alone, after adding a char before it,
the line will be re-indented automatically if needed."
  (when (and electric-indent-mode
             (eq (char-before) last-command-event)
             (not (mojo-syntax-context 'string))
             (save-excursion
               (beginning-of-line)
               (not (mojo-syntax-context 'string (syntax-ppss)))))
    (cond
     ;; Electric indent inside parens
     ((and
       (not (bolp))
       (let ((paren-start (mojo-syntax-context 'paren)))
         ;; Check that point is inside parens.
         (when paren-start
           (not
            ;; Filter the case where input is happening in the same
            ;; line where the open paren is.
            (= (line-number-at-pos)
               (line-number-at-pos paren-start)))))
       ;; When content has been added before the closing paren or a
       ;; comma has been inserted, it's ok to do the trick.
       (or
        (memq (char-after) '(?\) ?\] ?\}))
        (eq (char-before) ?,)))
      (save-excursion
        (goto-char (line-beginning-position))
        (let ((indentation (mojo-indent-calculate-indentation)))
          (when (and (numberp indentation) (< (current-indentation) indentation))
            (indent-line-to indentation)))))
     ;; Electric colon
     ((and (eq ?: last-command-event)
           (memq ?: electric-indent-chars)
           (not current-prefix-arg)
           ;; Trigger electric colon only at end of line
           (eolp)
           ;; Avoid re-indenting on extra colon
           (not (equal ?: (char-before (1- (point)))))
           (not (mojo-syntax-comment-or-string-p)))
      ;; Just re-indent dedenters
      (let ((dedenter-pos (mojo-info-dedenter-statement-p)))
        (when dedenter-pos
          (let ((start (copy-marker dedenter-pos))
                (end (point-marker)))
            (save-excursion
              (goto-char start)
              (mojo-indent-line)
              (unless (= (line-number-at-pos start)
                         (line-number-at-pos end))
                ;; Reindent region if this is a multiline statement
                (mojo-indent-region start end))))))))))

;;; Mark

(defun mojo-mark-defun (&optional allow-extend)
  "Put mark at end of this defun, point at beginning.
The defun marked is the one that contains point or follows point.

Interactively (or with ALLOW-EXTEND non-nil), if this command is
repeated or (in Transient Mark mode) if the mark is active, it
marks the next defun after the ones already marked."
  (interactive "p")
  (when (mojo-info-looking-at-beginning-of-defun)
    (end-of-line 1))
  (mark-defun allow-extend))


;;; Navigation

(defcustom mojo-forward-sexp-function #'mojo-nav-forward-sexp
  "Function to use when navigating between expressions."
  :version "28.1"
  :type '(choice (const :tag "Mojo blocks" mojo-nav-forward-sexp)
                 (const :tag "CC-mode like" nil)
                 function))

(defvar mojo-nav-beginning-of-defun-regexp
  (mojo-rx line-start (* space) defun (+ sp-bsnl) (group symbol-name))
  "Regexp matching class or function definition.
The name of the defun should be grouped so it can be retrieved
via `match-string'.")

(defvar mojo-nav-beginning-of-block-regexp
  (mojo-rx line-start (* space) block-start)
  "Regexp matching block start.")

(defun mojo-nav--beginning-of-defun (&optional arg)
  "Internal implementation of `mojo-nav-beginning-of-defun'.
With positive ARG search backwards, else search forwards."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (let* ((re-search-fn (if (> arg 0)
                           #'re-search-backward
                         #'re-search-forward))
         (line-beg-pos (line-beginning-position))
         (line-content-start (+ line-beg-pos (current-indentation)))
         (pos (point-marker))
         (min-indentation (if (mojo-info-current-line-empty-p)
                              most-positive-fixnum
                            (current-indentation)))
         (body-indentation
          (and (> arg 0)
               (or (and (mojo-info-looking-at-beginning-of-defun nil t)
                        (+ (save-excursion
                             (mojo-nav-beginning-of-statement)
                             (current-indentation))
                           mojo-indent-offset))
                   (save-excursion
                     (while
                         (and
                          (mojo-nav-backward-block)
                          (or (not (mojo-info-looking-at-beginning-of-defun))
                              (>= (current-indentation) min-indentation))
                          (setq min-indentation
                                (min min-indentation (current-indentation)))))
                     (or (and (mojo-info-looking-at-beginning-of-defun)
                              (+ (current-indentation) mojo-indent-offset))
                         0)))))
         (found
          (progn
            (when (and (mojo-info-looking-at-beginning-of-defun nil t)
                       (or (< arg 0)
                           ;; If looking at beginning of defun, and if
                           ;; pos is > line-content-start, ensure a
                           ;; backward re search match this defun by
                           ;; going to end of line before calling
                           ;; re-search-fn bug#40563
                           (and (> arg 0)
                                (or (mojo-info-continuation-line-p)
                                    (> pos line-content-start)))))
              (mojo-nav-end-of-statement))

            (while (and (funcall re-search-fn
                                 mojo-nav-beginning-of-defun-regexp nil t)
                        (or (mojo-syntax-context-type)
                            ;; Handle nested defuns when moving
                            ;; backwards by checking indentation.
                            (and (> arg 0)
                                 (not (= (current-indentation) 0))
                                 (>= (current-indentation) body-indentation)))))
            (and (mojo-info-looking-at-beginning-of-defun nil t)
                 (or (not (= (line-number-at-pos pos)
                             (line-number-at-pos)))
                     (and (>= (point) line-beg-pos)
                          (<= (point) line-content-start)
                          (> pos line-content-start)))))))
    (if found
        (progn
          (when (< arg 0)
            (mojo-nav-beginning-of-statement))
          (beginning-of-line 1)
          t)
      (and (goto-char pos) nil))))

(defun mojo-nav-beginning-of-defun (&optional arg)
  "Move point to `beginning-of-defun'.
With positive ARG search backwards else search forward.
ARG nil or 0 defaults to 1.  When searching backwards,
nested defuns are handled with care depending on current
point position.  Return non-nil if point is moved to
`beginning-of-defun'."
  (when (or (null arg) (= arg 0)) (setq arg 1))
  (let ((found))
    (while (and (not (= arg 0))
                (let ((keep-searching-p
                       (mojo-nav--beginning-of-defun arg)))
                  (when (and keep-searching-p (null found))
                    (setq found t))
                  keep-searching-p))
      (setq arg (if (> arg 0) (1- arg) (1+ arg))))
    found))

(defun mojo-nav-end-of-defun ()
  "Move point to the end of def or class.
Returns nil if point is not in a def or class."
  (interactive)
  (let ((beg-defun-indent)
        (beg-pos (point)))
    (when (or (mojo-info-looking-at-beginning-of-defun)
              (mojo-nav-beginning-of-defun 1)
              (mojo-nav-beginning-of-defun -1))
      (setq beg-defun-indent (current-indentation))
      (while (progn
               (mojo-nav-end-of-statement)
               (mojo-util-forward-comment 1)
               (and (> (current-indentation) beg-defun-indent)
                    (not (eobp)))))
      (mojo-util-forward-comment -1)
      (forward-line 1)
      ;; Ensure point moves forward.
      (and (> beg-pos (point)) (goto-char beg-pos))
      ;; Return non-nil if we did something (because then we were in a
      ;; def/class).
      (/= beg-pos (point)))))

(defun mojo-nav--syntactically (fn poscompfn &optional contextfn)
  "Move point using FN avoiding places with specific context.
FN must take no arguments.  POSCOMPFN is a two arguments function
used to compare current and previous point after it is moved
using FN, this is normally a less-than or greater-than
comparison.  Optional argument CONTEXTFN defaults to
`mojo-syntax-context-type' and is used for checking current
point context, it must return a non-nil value if this point must
be skipped."
  (let ((contextfn (or contextfn 'mojo-syntax-context-type))
        (start-pos (point-marker))
        (prev-pos))
    (catch 'found
      (while t
        (let* ((newpos
                (and (funcall fn) (point-marker)))
               (context (funcall contextfn)))
          (cond ((and (not context) newpos
                      (or (and (not prev-pos) newpos)
                          (and prev-pos newpos
                               (funcall poscompfn newpos prev-pos))))
                 (throw 'found (point-marker)))
                ((and newpos context)
                 (setq prev-pos (point)))
                (t (when (not newpos) (goto-char start-pos))
                   (throw 'found nil))))))))

(defun mojo-nav--forward-defun (arg)
  "Internal implementation of mojo-nav-{backward,forward}-defun.
Uses ARG to define which function to call, and how many times
repeat it."
  (let ((found))
    (while (and (> arg 0)
                (setq found
                      (mojo-nav--syntactically
                       (lambda ()
                         (re-search-forward
                          mojo-nav-beginning-of-defun-regexp nil t))
                       '>)))
      (setq arg (1- arg)))
    (while (and (< arg 0)
                (setq found
                      (mojo-nav--syntactically
                       (lambda ()
                         (re-search-backward
                          mojo-nav-beginning-of-defun-regexp nil t))
                       '<)))
      (setq arg (1+ arg)))
    found))

(defun mojo-nav-backward-defun (&optional arg)
  "Navigate to closer defun backward ARG times.
Unlikely `mojo-nav-beginning-of-defun' this doesn't care about
nested definitions."
  (interactive "^p")
  (mojo-nav--forward-defun (- (or arg 1))))

(defun mojo-nav-forward-defun (&optional arg)
  "Navigate to closer defun forward ARG times.
Unlikely `mojo-nav-beginning-of-defun' this doesn't care about
nested definitions."
  (interactive "^p")
  (mojo-nav--forward-defun (or arg 1)))

(defun mojo-nav-beginning-of-statement ()
  "Move to start of current statement."
  (interactive "^")
  (forward-line 0)
  (let* ((ppss (syntax-ppss))
         (context-point
          (or
           (mojo-syntax-context 'paren ppss)
           (mojo-syntax-context 'string ppss))))
    (cond ((bobp))
          (context-point
           (goto-char context-point)
           (mojo-nav-beginning-of-statement))
          ((save-excursion
             (forward-line -1)
             (mojo-info-line-ends-backslash-p))
           (forward-line -1)
           (mojo-nav-beginning-of-statement))))
  (back-to-indentation)
  (point-marker))

(defun mojo-nav-end-of-statement (&optional noend)
  "Move to end of current statement.
Optional argument NOEND is internal and makes the logic to not
jump to the end of line when moving forward searching for the end
of the statement."
  (interactive "^")
  (let (string-start bs-pos (last-string-end 0))
    (while (and (or noend (goto-char (line-end-position)))
                (not (eobp))
                (cond ((setq string-start (mojo-syntax-context 'string))
                       ;; The condition can be nil if syntax table
                       ;; text properties and the `syntax-ppss' cache
                       ;; are somehow out of whack.  This has been
                       ;; observed when using `syntax-ppss' during
                       ;; narrowing.
                       (when (>= string-start last-string-end)
                         (goto-char string-start)
                         (if (mojo-syntax-context 'paren)
                             ;; Ended up inside a paren, roll again.
                             (mojo-nav-end-of-statement t)
                           ;; This is not inside a paren, move to the
                           ;; end of this string.
                           (goto-char (+ (point)
                                         (mojo-syntax-count-quotes
                                          (char-after (point)) (point))))
                           (setq
                            last-string-end
                            (or (if (eq t (nth 3 (syntax-ppss)))
                                    (re-search-forward
                                     (rx (syntax string-delimiter)) nil t)
                                  (ignore-error scan-error
                                    (goto-char string-start)
                                    (mojo-nav--lisp-forward-sexp)
                                    (point)))
                                (goto-char (point-max)))))))
                      ((mojo-syntax-context 'paren)
                       ;; The statement won't end before we've escaped
                       ;; at least one level of parenthesis.
                       (condition-case err
                           (goto-char (scan-lists (point) 1 -1))
                         (scan-error (goto-char (nth 3 err)))))
                      ((setq bs-pos (mojo-info-line-ends-backslash-p))
                       (goto-char bs-pos)
                       (forward-line 1))))))
  (point-marker))

(defun mojo-nav-backward-statement (&optional arg)
  "Move backward to previous statement.
With ARG, repeat.  See `mojo-nav-forward-statement'."
  (interactive "^p")
  (or arg (setq arg 1))
  (mojo-nav-forward-statement (- arg)))

(defun mojo-nav-forward-statement (&optional arg)
  "Move forward to next statement.
With ARG, repeat.  With negative argument, move ARG times
backward to previous statement."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (mojo-nav-end-of-statement)
    (mojo-util-forward-comment)
    (mojo-nav-beginning-of-statement)
    (setq arg (1- arg)))
  (while (< arg 0)
    (mojo-nav-beginning-of-statement)
    (mojo-util-forward-comment -1)
    (mojo-nav-beginning-of-statement)
    (setq arg (1+ arg))))

(defun mojo-nav-beginning-of-block ()
  "Move to start of current block."
  (interactive "^")
  (let ((starting-pos (point)))
    ;; Go to first line beginning a statement
    (while (and (not (bobp))
                (or (and (mojo-nav-beginning-of-statement) nil)
                    (mojo-info-current-line-comment-p)
                    (mojo-info-current-line-empty-p)))
      (forward-line -1))
    (if (progn
          (mojo-nav-beginning-of-statement)
          (looking-at (mojo-rx block-start)))
        (point-marker)
      (let ((block-matching-indent
             (- (current-indentation) mojo-indent-offset)))
        (while
            (and (mojo-nav-backward-block)
                 (> (current-indentation) block-matching-indent)))
        (if (and (looking-at (mojo-rx block-start))
                 (= (current-indentation) block-matching-indent))
            (point-marker)
          (and (goto-char starting-pos) nil))))))

(defun mojo-nav-end-of-block ()
  "Move to end of current block."
  (interactive "^")
  (when (mojo-nav-beginning-of-block)
    (let ((block-indentation (current-indentation)))
      (mojo-nav-end-of-statement)
      (while (and (forward-line 1)
                  (not (eobp))
                  (or (and (> (current-indentation) block-indentation)
                           (or (mojo-nav-end-of-statement) t))
                      (mojo-info-current-line-comment-p)
                      (mojo-info-current-line-empty-p))))
      (mojo-util-forward-comment -1)
      (point-marker))))

(defun mojo-nav-backward-block (&optional arg)
  "Move backward to previous block of code.
With ARG, repeat.  See `mojo-nav-forward-block'."
  (interactive "^p")
  (or arg (setq arg 1))
  (mojo-nav-forward-block (- arg)))

(defun mojo-nav-forward-block (&optional arg)
  "Move forward to next block of code.
With ARG, repeat.  With negative argument, move ARG times
backward to previous block."
  (interactive "^p")
  (or arg (setq arg 1))
  (let ((block-start-regexp
         (mojo-rx line-start (* whitespace) block-start))
        (starting-pos (point))
        (orig-arg arg))
    (while (> arg 0)
      (mojo-nav-end-of-statement)
      (while (and
              (re-search-forward block-start-regexp nil t)
              (mojo-syntax-context-type)))
      (setq arg (1- arg)))
    (while (< arg 0)
      (mojo-nav-beginning-of-statement)
      (while (and
              (re-search-backward block-start-regexp nil t)
              (mojo-syntax-context-type)))
      (setq arg (1+ arg)))
    (mojo-nav-beginning-of-statement)
    (if (or (and (> orig-arg 0) (< (point) starting-pos))
            (not (looking-at (mojo-rx block-start))))
        (and (goto-char starting-pos) nil)
      (and (not (= (point) starting-pos)) (point-marker)))))

(defun mojo-nav--lisp-forward-sexp (&optional arg)
  "Standard version `forward-sexp'.
It ignores completely the value of `forward-sexp-function' by
setting it to nil before calling `forward-sexp'.  With positive
ARG move forward only one sexp, else move backwards."
  (let ((forward-sexp-function)
        (arg (if (or (not arg) (> arg 0)) 1 -1)))
    (forward-sexp arg)))

(defun mojo-nav--lisp-forward-sexp-safe (&optional arg)
  "Safe version of standard `forward-sexp'.
When at end of sexp (i.e. looking at an opening/closing paren)
skips it instead of throwing an error.  With positive ARG move
forward only one sexp, else move backwards."
  (let* ((arg (if (or (not arg) (> arg 0)) 1 -1))
         (paren-regexp
          (if (> arg 0) (mojo-rx close-paren) (mojo-rx open-paren)))
         (search-fn
          (if (> arg 0) #'re-search-forward #'re-search-backward)))
    (condition-case nil
        (mojo-nav--lisp-forward-sexp arg)
      (error
       (while (and (funcall search-fn paren-regexp nil t)
                   (mojo-syntax-context 'paren)))))))

(defun mojo-nav--forward-sexp (&optional dir safe skip-parens-p)
  "Move to forward sexp.
With positive optional argument DIR direction move forward, else
backwards.  When optional argument SAFE is non-nil do not throw
errors when at end of sexp, skip it instead.  With optional
argument SKIP-PARENS-P force sexp motion to ignore parenthesized
expressions when looking at them in either direction."
  (setq dir (or dir 1))
  (unless (= dir 0)
    (let* ((forward-p (if (> dir 0)
                          (and (setq dir 1) t)
                        (and (setq dir -1) nil)))
           (context-type (mojo-syntax-context-type)))
      (cond
       ((memq context-type '(string comment))
        ;; Inside of a string, get out of it.
        (let ((forward-sexp-function))
          (forward-sexp dir)))
       ((and (not skip-parens-p)
             (or (eq context-type 'paren)
                 (if forward-p
                     (eq (syntax-class (syntax-after (point)))
                         (car (string-to-syntax "(")))
                   (eq (syntax-class (syntax-after (1- (point))))
                       (car (string-to-syntax ")"))))))
        ;; Inside a paren or looking at it, lisp knows what to do.
        (if safe
            (mojo-nav--lisp-forward-sexp-safe dir)
          (mojo-nav--lisp-forward-sexp dir)))
       (t
        ;; This part handles the lispy feel of
        ;; `mojo-nav-forward-sexp'.  Knowing everything about the
        ;; current context and the context of the next sexp tries to
        ;; follow the lisp sexp motion commands in a symmetric manner.
        (let* ((context
                (cond
                 ((mojo-info-beginning-of-block-p) 'block-start)
                 ((mojo-info-end-of-block-p) 'block-end)
                 ((mojo-info-beginning-of-statement-p) 'statement-start)
                 ((mojo-info-end-of-statement-p) 'statement-end)))
               (next-sexp-pos
                (save-excursion
                  (if safe
                      (mojo-nav--lisp-forward-sexp-safe dir)
                    (mojo-nav--lisp-forward-sexp dir))
                  (point)))
               (next-sexp-context
                (save-excursion
                  (goto-char next-sexp-pos)
                  (cond
                   ((mojo-info-beginning-of-block-p) 'block-start)
                   ((mojo-info-end-of-block-p) 'block-end)
                   ((mojo-info-beginning-of-statement-p) 'statement-start)
                   ((mojo-info-end-of-statement-p) 'statement-end)
                   ((mojo-info-statement-starts-block-p) 'starts-block)
                   ((mojo-info-statement-ends-block-p) 'ends-block)))))
          (if forward-p
              (cond ((and (not (eobp))
                          (mojo-info-current-line-empty-p))
                     (mojo-util-forward-comment dir)
                     (mojo-nav--forward-sexp dir safe skip-parens-p))
                    ((eq context 'block-start)
                     (mojo-nav-end-of-block))
                    ((eq context 'statement-start)
                     (mojo-nav-end-of-statement))
                    ((and (memq context '(statement-end block-end))
                          (eq next-sexp-context 'ends-block))
                     (goto-char next-sexp-pos)
                     (mojo-nav-end-of-block))
                    ((and (memq context '(statement-end block-end))
                          (eq next-sexp-context 'starts-block))
                     (goto-char next-sexp-pos)
                     (mojo-nav-end-of-block))
                    ((memq context '(statement-end block-end))
                     (goto-char next-sexp-pos)
                     (mojo-nav-end-of-statement))
                    (t (goto-char next-sexp-pos)))
            (cond ((and (not (bobp))
                        (mojo-info-current-line-empty-p))
                   (mojo-util-forward-comment dir)
                   (mojo-nav--forward-sexp dir safe skip-parens-p))
                  ((eq context 'block-end)
                   (mojo-nav-beginning-of-block))
                  ((eq context 'statement-end)
                   (mojo-nav-beginning-of-statement))
                  ((and (memq context '(statement-start block-start))
                        (eq next-sexp-context 'starts-block))
                   (goto-char next-sexp-pos)
                   (mojo-nav-beginning-of-block))
                  ((and (memq context '(statement-start block-start))
                        (eq next-sexp-context 'ends-block))
                   (goto-char next-sexp-pos)
                   (mojo-nav-beginning-of-block))
                  ((memq context '(statement-start block-start))
                   (goto-char next-sexp-pos)
                   (mojo-nav-beginning-of-statement))
                  (t (goto-char next-sexp-pos))))))))))

(defun mojo-nav-forward-sexp (&optional arg safe skip-parens-p)
  "Move forward across expressions.
With ARG, do it that many times.  Negative arg -N means move
backward N times.  When optional argument SAFE is non-nil do not
throw errors when at end of sexp, skip it instead.  With optional
argument SKIP-PARENS-P force sexp motion to ignore parenthesized
expressions when looking at them in either direction (forced to t
in interactive calls)."
  (interactive "^p")
  (or arg (setq arg 1))
  ;; Do not follow parens on interactive calls.  This hack to detect
  ;; if the function was called interactively copes with the way
  ;; `forward-sexp' works by calling `forward-sexp-function', losing
  ;; interactive detection by checking `current-prefix-arg'.  The
  ;; reason to make this distinction is that lisp functions like
  ;; `blink-matching-open' get confused causing issues like the one in
  ;; Bug#16191.  With this approach the user gets a symmetric behavior
  ;; when working interactively while called functions expecting
  ;; paren-based sexp motion work just fine.
  (or
   skip-parens-p
   (setq skip-parens-p
         (memq real-this-command
               (list
                #'forward-sexp #'backward-sexp
                #'mojo-nav-forward-sexp #'mojo-nav-backward-sexp
                #'mojo-nav-forward-sexp-safe #'mojo-nav-backward-sexp))))
  (while (> arg 0)
    (mojo-nav--forward-sexp 1 safe skip-parens-p)
    (setq arg (1- arg)))
  (while (< arg 0)
    (mojo-nav--forward-sexp -1 safe skip-parens-p)
    (setq arg (1+ arg))))

(defun mojo-nav-backward-sexp (&optional arg safe skip-parens-p)
  "Move backward across expressions.
With ARG, do it that many times.  Negative arg -N means move
forward N times.  When optional argument SAFE is non-nil do not
throw errors when at end of sexp, skip it instead.  With optional
argument SKIP-PARENS-P force sexp motion to ignore parenthesized
expressions when looking at them in either direction (forced to t
in interactive calls)."
  (interactive "^p")
  (or arg (setq arg 1))
  (mojo-nav-forward-sexp (- arg) safe skip-parens-p))

(defun mojo-nav-forward-sexp-safe (&optional arg skip-parens-p)
  "Move forward safely across expressions.
With ARG, do it that many times.  Negative arg -N means move
backward N times.  With optional argument SKIP-PARENS-P force
sexp motion to ignore parenthesized expressions when looking at
them in either direction (forced to t in interactive calls)."
  (interactive "^p")
  (mojo-nav-forward-sexp arg t skip-parens-p))

(defun mojo-nav-backward-sexp-safe (&optional arg skip-parens-p)
  "Move backward safely across expressions.
With ARG, do it that many times.  Negative arg -N means move
forward N times.  With optional argument SKIP-PARENS-P force sexp
motion to ignore parenthesized expressions when looking at them in
either direction (forced to t in interactive calls)."
  (interactive "^p")
  (mojo-nav-backward-sexp arg t skip-parens-p))

(defun mojo-nav--up-list (&optional dir)
  "Internal implementation of `mojo-nav-up-list'.
DIR is always 1 or -1 and comes sanitized from
`mojo-nav-up-list' calls."
  (let ((context (mojo-syntax-context-type))
        (forward-p (> dir 0)))
    (cond
     ((memq context '(string comment)))
     ((eq context 'paren)
      (let ((forward-sexp-function))
        (up-list dir)))
     ((and forward-p (mojo-info-end-of-block-p))
      (let ((parent-end-pos
             (save-excursion
               (let ((indentation (and
                                   (mojo-nav-beginning-of-block)
                                   (current-indentation))))
                 (while (and indentation
                             (> indentation 0)
                             (>= (current-indentation) indentation)
                             (mojo-nav-backward-block)))
                 (mojo-nav-end-of-block)))))
        (and (> (or parent-end-pos (point)) (point))
             (goto-char parent-end-pos))))
     (forward-p (mojo-nav-end-of-block))
     ((and (not forward-p)
           (> (current-indentation) 0)
           (mojo-info-beginning-of-block-p))
      (let ((prev-block-pos
             (save-excursion
               (let ((indentation (current-indentation)))
                 (while (and (mojo-nav-backward-block)
                             (>= (current-indentation) indentation))))
               (point))))
        (and (> (point) prev-block-pos)
             (goto-char prev-block-pos))))
     ((not forward-p) (mojo-nav-beginning-of-block)))))

(defun mojo-nav-up-list (&optional arg)
  "Move forward out of one level of parentheses (or blocks).
With ARG, do this that many times.
A negative argument means move backward but still to a less deep spot.
This command assumes point is not in a string or comment."
  (interactive "^p")
  (or arg (setq arg 1))
  (while (> arg 0)
    (mojo-nav--up-list 1)
    (setq arg (1- arg)))
  (while (< arg 0)
    (mojo-nav--up-list -1)
    (setq arg (1+ arg))))

(defun mojo-nav-backward-up-list (&optional arg)
  "Move backward out of one level of parentheses (or blocks).
With ARG, do this that many times.
A negative argument means move forward but still to a less deep spot.
This command assumes point is not in a string or comment."
  (interactive "^p")
  (or arg (setq arg 1))
  (mojo-nav-up-list (- arg)))

(defun mojo-nav-if-name-main ()
  "Move point at the beginning the __main__ block.
When \"if __name__ == \\='__main__\\=':\" is found returns its
position, else returns nil."
  (interactive)
  (let ((point (point))
        (found (catch 'found
                 (goto-char (point-min))
                 (while (re-search-forward
                         (mojo-rx line-start
                                  "if" (+ space)
                                  "__name__" (+ space)
                                  "==" (+ space)
                                  (group-n 1 (or ?\" ?\'))
                                  "__main__" (backref 1) (* space) ":")
                         nil t)
                   (when (not (mojo-syntax-context-type))
                     (beginning-of-line)
                     (throw 'found t))))))
    (if found
        (point)
      (ignore (goto-char point)))))

;;; Shell integration: start=2594, end=4596

;;; PDB Track integration: start=4598, end=4807

;;; Symbol completion

;; (defun mojo-completion-at-point ()
;;   "Function for `completion-at-point-functions' in `mojo-mode'.
;; For this to work as best as possible you should call
;; `mojo-shell-send-buffer' from time to time so context in
;; inferior Mojo process is updated properly."
;;   (let ((process (mojo-shell-get-process)))
;;     (when (and process
;;                (mojo-shell-with-shell-buffer
;;                  (mojo-util-comint-end-of-output-p)))
;;       (mojo-shell-completion-at-point process))))

;; (define-obsolete-function-alias
;;   'mojo-completion-complete-at-point
;;   #'mojo-completion-at-point
;;   "25.1")

;;; Fill paragraph: start=4826, end=5060

;;; Skeletons: start=5063, end=5208

;;; FFAP: start=5212, end=5252

;;; Code check: start=5257, end=5294

;;; ElDoc: start=5299, end=5433

;;; Hideshow: start=5439, end=5474

;;; Imenu: start=5479, end=5628

;;; Misc helpers

(defun mojo-info-current-defun (&optional include-type)
  "Return name of surrounding function with Mojo compatible dotty syntax.
Optional argument INCLUDE-TYPE indicates to include the type of the defun.
This function can be used as the value of `add-log-current-defun-function'
since it returns nil if point is not inside a defun."
  (save-restriction
    (widen)
    (save-excursion
      (end-of-line 1)
      (let ((names)
            (starting-indentation (current-indentation))
            (starting-pos (point))
            (first-run t)
            (last-indent)
            (type))
        (catch 'exit
          (while (mojo-nav-beginning-of-defun 1)
            (when (save-match-data
                    (and
                     (or (not last-indent)
                         (< (current-indentation) last-indent))
                     (or
                      (and first-run
                           (save-excursion
                             ;; If this is the first run, we may add
                             ;; the current defun at point.
                             (setq first-run nil)
                             (goto-char starting-pos)
                             (mojo-nav-beginning-of-statement)
                             (beginning-of-line 1)
                             (looking-at-p
                              mojo-nav-beginning-of-defun-regexp)))
                      (< starting-pos
                         (save-excursion
                           (let ((min-indent
                                  (+ (current-indentation)
                                     mojo-indent-offset)))
                             (if (< starting-indentation  min-indent)
                                 ;; If the starting indentation is not
                                 ;; within the min defun indent make the
                                 ;; check fail.
                                 starting-pos
                               ;; Else go to the end of defun and add
                               ;; up the current indentation to the
                               ;; ending position.
                               (mojo-nav-end-of-defun)
                               (+ (point)
                                  (if (>= (current-indentation) min-indent)
                                      (1+ (current-indentation))
                                    0)))))))))
              (save-match-data (setq last-indent (current-indentation)))
              (if (or (not include-type) type)
                  (setq names (cons (match-string-no-properties 1) names))
                (let ((match (split-string (match-string-no-properties 0))))
                  (setq type (car match))
                  (setq names (cons (cadr match) names)))))
            ;; Stop searching ASAP.
            (and (= (current-indentation) 0) (throw 'exit t))))
        (and names
             (concat (and type (format "%s " type))
                     (mapconcat #'identity names ".")))))))


(defun mojo-info-current-symbol (&optional replace-self)
  "Return current symbol using dotty syntax.
With optional argument REPLACE-SELF convert \"self\" to current
parent defun name."
  (let ((name
         (and (not (mojo-syntax-comment-or-string-p))
              (with-syntax-table mojo-dotty-syntax-table
                (let ((sym (symbol-at-point)))
                  (and sym
                       (substring-no-properties (symbol-name sym))))))))
    (when name
      (if (not replace-self)
          name
        (let ((current-defun (mojo-info-current-defun)))
          (if (not current-defun)
              name
            (replace-regexp-in-string
             (mojo-rx line-start word-start "self" word-end ?.)
             (concat
              (mapconcat #'identity
                         (butlast (split-string current-defun "\\."))
                         ".")
              ".")
             name)))))))

(defun mojo-info-statement-starts-block-p ()
  "Return non-nil if current statement opens a block."
  (save-excursion
    (mojo-nav-beginning-of-statement)
    (looking-at (mojo-rx block-start))))

(defun mojo-info-statement-ends-block-p ()
  "Return non-nil if point is at end of block."
  (let ((end-of-block-pos (save-excursion
                            (mojo-nav-end-of-block)))
        (end-of-statement-pos (save-excursion
                                (mojo-nav-end-of-statement))))
    (and end-of-block-pos end-of-statement-pos
         (= end-of-block-pos end-of-statement-pos))))

(defun mojo-info-beginning-of-statement-p ()
  "Return non-nil if point is at beginning of statement."
  (= (point) (save-excursion
               (mojo-nav-beginning-of-statement)
               (point))))

(defun mojo-info-end-of-statement-p ()
  "Return non-nil if point is at end of statement."
  (= (point) (save-excursion
               (mojo-nav-end-of-statement)
               (point))))

(defun mojo-info-beginning-of-block-p ()
  "Return non-nil if point is at beginning of block."
  (and (mojo-info-beginning-of-statement-p)
       (mojo-info-statement-starts-block-p)))

(defun mojo-info-end-of-block-p ()
  "Return non-nil if point is at end of block."
  (and (mojo-info-end-of-statement-p)
       (mojo-info-statement-ends-block-p)))

(define-obsolete-function-alias
  'mojo-info-closing-block
  #'mojo-info-dedenter-opening-block-position "24.4")

(defun mojo-info-dedenter-opening-block-position ()
  "Return the point of the closest block the current line closes.
Returns nil if point is not on a dedenter statement or no opening
block can be detected.  The latter case meaning current file is
likely an invalid mojo file."
  (let ((positions (mojo-info-dedenter-opening-block-positions))
        (indentation (current-indentation))
        (position))
    (while (and (not position)
                positions)
      (save-excursion
        (goto-char (car positions))
        (if (<= (current-indentation) indentation)
            (setq position (car positions))
          (setq positions (cdr positions)))))
    position))

(defun mojo-info-dedenter-opening-block-positions ()
  "Return points of blocks the current line may close sorted by closer.
Returns nil if point is not on a dedenter statement or no opening
block can be detected.  The latter case meaning current file is
likely an invalid mojo file."
  (save-excursion
    (let ((dedenter-pos (mojo-info-dedenter-statement-p)))
      (when dedenter-pos
        (goto-char dedenter-pos)
        (let* ((cur-line (line-beginning-position))
               (pairs '(("elif" "elif" "if")
                        ("else" "if" "elif" "except" "for" "while")
                        ("except" "except" "try")
                        ("finally" "else" "except" "try")
                        ("case" "case")))
               (dedenter (match-string-no-properties 0))
               (possible-opening-blocks (cdr (assoc-string dedenter pairs)))
               (collected-indentations)
               (opening-blocks))
          (catch 'exit
            (while (mojo-nav--syntactically
                    (lambda ()
                      (cl-loop while (re-search-backward (mojo-rx block-start) nil t)
                               if (save-match-data
                                    (looking-back (rx line-start (* whitespace))
                                                  (line-beginning-position)))
                               return t))
                    #'<)
              (let ((indentation (current-indentation)))
                (when (and (not (memq indentation collected-indentations))
                           (or (not collected-indentations)
                               (< indentation
                                  (apply #'min collected-indentations)))
                           ;; There must be no line with indentation
                           ;; smaller than `indentation' (except for
                           ;; blank lines) between the found opening
                           ;; block and the current line, otherwise it
                           ;; is not an opening block.
                           (save-excursion
                             (mojo-nav-end-of-statement)
                             (forward-line)
                             (let ((no-back-indent t))
                               (save-match-data
                                 (while (and (< (point) cur-line)
                                             (setq no-back-indent
                                                   (or (> (current-indentation) indentation)
                                                       (mojo-info-current-line-empty-p)
                                                       (mojo-info-current-line-comment-p))))
                                   (forward-line)))
                               no-back-indent)))
                  (setq collected-indentations
                        (cons indentation collected-indentations))
                  (when (member (match-string-no-properties 0)
                                possible-opening-blocks)
                    (setq opening-blocks (cons (point) opening-blocks))))
                (when (zerop indentation)
                  (throw 'exit nil)))))
          ;; sort by closer
          (nreverse opening-blocks))))))

(define-obsolete-function-alias
  'mojo-info-closing-block-message
  #'mojo-info-dedenter-opening-block-message "24.4")

(defun mojo-info-dedenter-opening-block-message  ()
  "Message the first line of the block the current statement closes."
  (let ((point (mojo-info-dedenter-opening-block-position)))
    (when point
        (message "Closes %s" (save-excursion
                               (goto-char point)
                               (buffer-substring
                                (point) (line-end-position)))))))

(defun mojo-info-dedenter-statement-p ()
  "Return point if current statement is a dedenter.
Sets `match-data' to the keyword that starts the dedenter
statement."
  (save-excursion
    (mojo-nav-beginning-of-statement)
    (when (and (not (mojo-syntax-context-type))
               (looking-at (mojo-rx dedenter))
               ;; Exclude the first "case" in the block.
               (not (and (string= (match-string-no-properties 0)
                                  "case")
                         (save-excursion
                           (back-to-indentation)
                           (mojo-util-forward-comment -1)
                           (equal (char-before) ?:)))))
      (point))))

(defun mojo-info-line-ends-backslash-p (&optional line-number)
  "Return non-nil if current line ends with backslash.
With optional argument LINE-NUMBER, check that line instead."
  (save-excursion
      (when line-number
        (mojo-util-goto-line line-number))
      (while (and (not (eobp))
                  (goto-char (line-end-position))
                  (mojo-syntax-context 'paren)
                  (not (equal (char-before (point)) ?\\)))
        (forward-line 1))
      (when (equal (char-before) ?\\)
        (point-marker))))

(defun mojo-info-beginning-of-backslash (&optional line-number)
  "Return the point where the backslashed line starts.
Optional argument LINE-NUMBER forces the line number to check against."
  (save-excursion
      (when line-number
        (mojo-util-goto-line line-number))
      (when (mojo-info-line-ends-backslash-p)
        (while (save-excursion
                 (goto-char (line-beginning-position))
                 (mojo-syntax-context 'paren))
          (forward-line -1))
        (back-to-indentation)
        (point-marker))))

(defun mojo-info-continuation-line-p ()
  "Check if current line is continuation of another.
When current line is continuation of another return the point
where the continued line ends."
  (save-excursion
      (let* ((context-type (progn
                             (back-to-indentation)
                             (mojo-syntax-context-type)))
             (line-start (line-number-at-pos))
             (context-start (when context-type
                              (mojo-syntax-context context-type))))
        (cond ((equal context-type 'paren)
               ;; Lines inside a paren are always a continuation line
               ;; (except the first one).
               (mojo-util-forward-comment -1)
               (point-marker))
              ((member context-type '(string comment))
               ;; move forward an roll again
               (goto-char context-start)
               (mojo-util-forward-comment)
               (mojo-info-continuation-line-p))
              (t
               ;; Not within a paren, string or comment, the only way
               ;; we are dealing with a continuation line is that
               ;; previous line contains a backslash, and this can
               ;; only be the previous line from current
               (back-to-indentation)
               (mojo-util-forward-comment -1)
               (when (and (equal (1- line-start) (line-number-at-pos))
                          (mojo-info-line-ends-backslash-p))
                 (point-marker)))))))

(defun mojo-info-block-continuation-line-p ()
  "Return non-nil if current line is a continuation of a block."
  (save-excursion
    (when (mojo-info-continuation-line-p)
      (forward-line -1)
      (back-to-indentation)
      (when (looking-at (mojo-rx block-start))
        (point-marker)))))

(defun mojo-info-assignment-statement-p (&optional current-line-only)
  "Check if current line is an assignment.
With argument CURRENT-LINE-ONLY is non-nil, don't follow any
continuations, just check the if current line is an assignment."
  (save-excursion
    (let ((found nil))
      (if current-line-only
          (back-to-indentation)
        (mojo-nav-beginning-of-statement))
      (while (and
              (re-search-forward (mojo-rx not-simple-operator
                                            assignment-operator
                                            (group not-simple-operator))
                                 (line-end-position) t)
              (not found))
        (save-excursion
          ;; The assignment operator should not be inside a string.
          (backward-char (length (match-string-no-properties 1)))
          (setq found (not (mojo-syntax-context-type)))))
      (when found
        (skip-syntax-forward " ")
        (point-marker)))))

;; TODO: rename to clarify this is only for the first continuation
;; line or remove it and move its body to `mojo-indent-context'.
(defun mojo-info-assignment-continuation-line-p ()
  "Check if current line is the first continuation of an assignment.
When current line is continuation of another with an assignment
return the point of the first non-blank character after the
operator."
  (save-excursion
    (when (mojo-info-continuation-line-p)
      (forward-line -1)
      (mojo-info-assignment-statement-p t))))

(defun mojo-info-looking-at-beginning-of-defun (&optional syntax-ppss
                                                            check-statement)
  "Check if point is at `beginning-of-defun' using SYNTAX-PPSS.
When CHECK-STATEMENT is non-nil, the current statement is checked
instead of the current physical line."
  (save-excursion
    (when check-statement
      (mojo-nav-beginning-of-statement))
    (beginning-of-line 1)
    (and (not (mojo-syntax-context-type (or syntax-ppss (syntax-ppss))))
         (looking-at mojo-nav-beginning-of-defun-regexp))))

(defun mojo-info-looking-at-beginning-of-block ()
  "Check if point is at the beginning of block."
  (let ((pos (point)))
    (save-excursion
      (mojo-nav-beginning-of-statement)
      (beginning-of-line)
      (and
       (<= (point) pos (+ (point) (current-indentation)))
       (looking-at mojo-nav-beginning-of-block-regexp)))))

(defun mojo-info-current-line-comment-p ()
  "Return non-nil if current line is a comment line."
  (char-equal
   (or (char-after (+ (line-beginning-position) (current-indentation))) ?_)
   ?#))

(defun mojo-info-current-line-empty-p ()
  "Return non-nil if current line is empty, ignoring whitespace."
  (save-excursion
    (beginning-of-line 1)
    (looking-at
     (mojo-rx line-start (* whitespace)
                (group (* not-newline))
                (* whitespace) line-end))
    (string-equal "" (match-string-no-properties 1))))

(defun mojo-info-docstring-p (&optional syntax-ppss)
  "Return non-nil if point is in a docstring.
When optional argument SYNTAX-PPSS is given, use that instead of
point's current `syntax-ppss'."
  ;;; https://www.python.org/dev/peps/pep-0257/#what-is-a-docstring
  (save-excursion
    (when (and syntax-ppss (mojo-syntax-context 'string syntax-ppss))
      (goto-char (nth 8 syntax-ppss)))
    (mojo-nav-beginning-of-statement)
    (let ((counter 1)
          (indentation (current-indentation))
          (backward-sexp-point)
          (re "[uU]?[rR]?[\"']"))
      (when (and
             (not (mojo-info-assignment-statement-p))
             (looking-at-p re)
             ;; Allow up to two consecutive docstrings only.
             (>=
              2
              (let (last-backward-sexp-point)
                (while (and (<= counter 2)
                            (save-excursion
                              (mojo-nav-backward-sexp)
                              (setq backward-sexp-point (point))
                              (and (= indentation (current-indentation))
                                   ;; Make sure we're always moving point.
                                   ;; If we get stuck in the same position
                                   ;; on consecutive loop iterations,
                                   ;; bail out.
                                   (prog1 (not (eql last-backward-sexp-point
                                                    backward-sexp-point))
                                     (setq last-backward-sexp-point
                                           backward-sexp-point))
                                   (looking-at-p re))))
                  ;; Previous sexp was a string, restore point.
                  (goto-char backward-sexp-point)
                  (cl-incf counter))
                counter)))
        (mojo-util-forward-comment -1)
        (mojo-nav-beginning-of-statement)
        (cond ((bobp))
              ((mojo-info-assignment-statement-p) t)
              ((mojo-info-looking-at-beginning-of-defun))
              (t nil))))))

(defun mojo-info-triple-quoted-string-p ()
  "Check if point is in a triple quoted string including quotes.
It returns the position of the third quote character of the start
of the string."
  (save-excursion
    (let ((pos (point)))
      (cl-loop
       for offset in '(0 3 -2 2 -1 1)
       if (let ((check-pos (+ pos offset)))
            (and (>= check-pos (point-min))
                 (<= check-pos (point-max))
                 (mojo-syntax-context
                  'triple-quoted-string (syntax-ppss check-pos))))
       return it))))

(defun mojo-info-encoding-from-cookie ()
  "Detect current buffer's encoding from its coding cookie.
Returns the encoding as a symbol."
  (let ((first-two-lines
         (save-excursion
           (save-restriction
             (widen)
             (goto-char (point-min))
             (forward-line 2)
             (buffer-substring-no-properties
              (point)
              (point-min))))))
    (when (string-match (mojo-rx coding-cookie) first-two-lines)
      (intern (match-string-no-properties 1 first-two-lines)))))

(defun mojo-info-encoding ()
  "Return encoding for file.
Try `mojo-info-encoding-from-cookie', if none is found then
default to utf-8."
  ;; If no encoding is defined, then it's safe to use UTF-8: Mojo 2
  ;; uses ASCII as default while Mojo 3 uses UTF-8.  This means that
  ;; in the worst case scenario mojo.el will make things work for
  ;; Mojo 2 files with unicode data and no encoding defined.
  (or (mojo-info-encoding-from-cookie)
      'utf-8))

;;; Utility functions

(defun mojo-util-goto-line (line-number)
  "Move point to LINE-NUMBER."
  (goto-char (point-min))
  (forward-line (1- line-number)))

;; Stolen from org-mode
(defun mojo-util-clone-local-variables (from-buffer &optional regexp)
  "Clone local variables from FROM-BUFFER.
Optional argument REGEXP selects variables to clone and defaults
to \"^mojo-\"."
  (mapc
   (lambda (pair)
     (and (consp pair)
          (symbolp (car pair))
          (string-match (or regexp "^mojo-")
                        (symbol-name (car pair)))
          (set (make-local-variable (car pair))
               (cdr pair))))
   (buffer-local-variables from-buffer)))

(defvar comint-last-prompt-overlay)     ; Shut up, byte compiler.

(defun mojo-util-comint-last-prompt ()
  "Return comint last prompt overlay start and end.
This is for compatibility with Emacs < 24.4."
  (cond ((bound-and-true-p comint-last-prompt-overlay)
         (cons (overlay-start comint-last-prompt-overlay)
               (overlay-end comint-last-prompt-overlay)))
        ((bound-and-true-p comint-last-prompt)
         comint-last-prompt)
        (t nil)))

(defun mojo-util-comint-end-of-output-p ()
  "Return non-nil if the last prompt matches input prompt."
  (when-let ((prompt (mojo-util-comint-last-prompt)))
    (mojo-shell-comint-end-of-output-p
     (buffer-substring-no-properties
      (car prompt) (cdr prompt)))))

(defun mojo-util-forward-comment (&optional direction)
  "Mojo mode specific version of `forward-comment'.
Optional argument DIRECTION defines the direction to move to."
  (let ((comment-start (mojo-syntax-context 'comment))
        (factor (if (< (or direction 0) 0)
                    -99999
                  99999)))
    (when comment-start
      (goto-char comment-start))
    (forward-comment factor)))

(defun mojo-util-list-directories (directory &optional predicate max-depth)
  "List DIRECTORY subdirs, filtered by PREDICATE and limited by MAX-DEPTH.
Argument PREDICATE defaults to `identity' and must be a function
that takes one argument (a full path) and returns non-nil for
allowed files.  When optional argument MAX-DEPTH is non-nil, stop
searching when depth is reached, else don't limit."
  (let* ((dir (expand-file-name directory))
         (dir-length (length dir))
         (predicate (or predicate #'identity))
         (to-scan (list dir))
         (tally nil))
    (while to-scan
      (let ((current-dir (car to-scan)))
        (when (funcall predicate current-dir)
          (setq tally (cons current-dir tally)))
        (setq to-scan (append (cdr to-scan)
                              (mojo-util-list-files
                               current-dir #'file-directory-p)
                              nil))
        (when (and max-depth
                   (<= max-depth
                       (length (split-string
                                (substring current-dir dir-length)
                                "/\\|\\\\" t))))
          (setq to-scan nil))))
    (nreverse tally)))

(defun mojo-util-list-files (dir &optional predicate)
  "List files in DIR, filtering with PREDICATE.
Argument PREDICATE defaults to `identity' and must be a function
that takes one argument (a full path) and returns non-nil for
allowed files."
  (let ((dir-name (file-name-as-directory dir)))
    (apply #'nconc
           (mapcar (lambda (file-name)
                     (let ((full-file-name
                            (expand-file-name file-name dir-name)))
                       (when (and
                              (not (member file-name '("." "..")))
                              (funcall (or predicate #'identity)
                                       full-file-name))
                         (list full-file-name))))
                   (directory-files dir-name)))))

(defun mojo-util-list-packages (dir &optional max-depth)
  "List packages in DIR, limited by MAX-DEPTH.
When optional argument MAX-DEPTH is non-nil, stop searching when
depth is reached, else don't limit."
  (let* ((dir (expand-file-name dir))
         (parent-dir (file-name-directory
                      (directory-file-name
                       (file-name-directory
                        (file-name-as-directory dir)))))
         (subpath-length (length parent-dir)))
    (mapcar
     (lambda (file-name)
       (replace-regexp-in-string
        (rx (or ?\\ ?/)) "." (substring file-name subpath-length)))
     (mojo-util-list-directories
      (directory-file-name dir)
      (lambda (dir)
        (file-exists-p (expand-file-name "__init__.py" dir)))
      max-depth))))

(defun mojo-util-popn (lst n)
  "Return LST first N elements.
N should be an integer, when negative its opposite is used.
When N is bigger than the length of LST, the list is
returned as is."
  (let* ((n (min (abs n)))
         (len (length lst))
         (acc))
    (if (> n len)
        lst
      (while (< 0 n)
        (setq acc (cons (car lst) acc)
              lst (cdr lst)
              n (1- n)))
      (reverse acc))))

(defun mojo-util-strip-string (string)
  "Strip STRING whitespace and newlines from end and beginning."
  (replace-regexp-in-string
   (rx (or (: string-start (* (any whitespace ?\r ?\n)))
           (: (* (any whitespace ?\r ?\n)) string-end)))
   ""
   string))

(defun mojo-util-valid-regexp-p (regexp)
  "Return non-nil if REGEXP is valid."
  (ignore-errors (string-match regexp "") t))


;;; Major mode
(defun mojo-electric-pair-string-delimiter ()
  (when (and electric-pair-mode
             (memq last-command-event '(?\" ?\'))
             (let ((count 0))
               (while (eq (char-before (- (point) count)) last-command-event)
                 (cl-incf count))
               (= count 3))
             (eq (char-after) last-command-event))
    (save-excursion (insert (make-string 2 last-command-event)))))

(defvar electric-indent-inhibit)
(defvar prettify-symbols-alist)

;;;###autoload
(define-derived-mode mojo-base-mode prog-mode "Mojo"
  "Generic major mode for editing Mojo files.

This is a generic major mode intended to be inherited by
concrete implementations.  Currently there are two concrete
implementations: `mojo-mode' and `mojo-ts-mode'."
  (setq-local tab-width 8)
  (setq-local indent-tabs-mode nil)

  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")

  (setq-local parse-sexp-lookup-properties t)
  (setq-local parse-sexp-ignore-comments t)

  (setq-local forward-sexp-function mojo-forward-sexp-function)

  (setq-local indent-line-function #'mojo-indent-line-function)
  (setq-local indent-region-function #'mojo-indent-region)
  ;; Because indentation is not redundant, we cannot safely reindent code.
  (setq-local electric-indent-inhibit t)
  (setq-local electric-indent-chars
              (cons ?: electric-indent-chars))

  ;; Add """ ... """ pairing to electric-pair-mode.
  (add-hook 'post-self-insert-hook
            #'mojo-electric-pair-string-delimiter 'append t)

  (setq-local paragraph-start "\\s-*$")
  (setq-local fill-paragraph-function #'mojo-fill-paragraph)
  (setq-local normal-auto-fill-function #'mojo-do-auto-fill)

  (setq-local beginning-of-defun-function #'mojo-nav-beginning-of-defun)
  (setq-local end-of-defun-function #'mojo-nav-end-of-defun)

  ;; (add-hook 'completion-at-point-functions
  ;;           #'mojo-completion-at-point nil 'local)

  (add-hook 'post-self-insert-hook
            #'mojo-indent-post-self-insert-function 'append 'local)

  (setq-local add-log-current-defun-function
              #'mojo-info-current-defun)

  (setq-local skeleton-further-elements
              '((abbrev-mode nil)
                (< '(backward-delete-char-untabify (min mojo-indent-offset
                                                        (current-column))))
                (^ '(- (1+ (current-indentation))))))

  ;; (with-no-warnings
  ;;   ;; suppress warnings about eldoc-documentation-function being obsolete
  ;;   (if (null eldoc-documentation-function)
  ;;       ;; Emacs<25
  ;;       (setq-local eldoc-documentation-function #'mojo-eldoc-function)
  ;;     (if (boundp 'eldoc-documentation-functions)
  ;;         (add-hook 'eldoc-documentation-functions #'mojo-eldoc-function nil t)
  ;;       (add-function :before-until (local 'eldoc-documentation-function)
  ;;                     #'mojo-eldoc-function))))

  ;; TODO: Use tree-sitter to figure out the block in `mojo-ts-mode'.
  (dolist (mode '(mojo-mode))
    (add-to-list
     'hs-special-modes-alist
     `(,mode
       ,mojo-nav-beginning-of-block-regexp
       ;; Use the empty string as end regexp so it doesn't default to
       ;; "\\s)".  This way parens at end of defun are properly hidden.
       ""
       "#"
       mojo-hideshow-forward-sexp-function
       nil
       mojo-nav-beginning-of-block
       mojo-hideshow-find-next-block
       mojo-info-looking-at-beginning-of-block)))

  (setq-local outline-regexp (mojo-rx (* space) block-start))
  (setq-local outline-level
              (lambda ()
                "`outline-level' function for Mojo mode."
                (1+ (/ (current-indentation) mojo-indent-offset))))

  (setq-local prettify-symbols-alist mojo-prettify-symbols-alist)

  ;; (make-local-variable 'mojo-shell-internal-buffer)

  ;; (add-hook 'flymake-diagnostic-functions #'mojo-flymake nil t)
  )

;;;###autoload
(define-derived-mode mojo-mode mojo-base-mode "Mojo"
  "Major mode for editing Mojo files.

\\{mojo-mode-map}"
  (setq-local font-lock-defaults
              `(,mojo-font-lock-keywords
                nil nil nil nil
                (font-lock-syntactic-face-function
                 . mojo-font-lock-syntactic-face-function)))
  (setq-local syntax-propertize-function
              mojo-syntax-propertize-function)
  ;; (setq-local imenu-create-index-function
  ;;             #'mojo-imenu-create-index)

  (add-hook 'which-func-functions #'mojo-info-current-defun nil t)

  ;; (mojo-skeleton-add-menu-items)

  (when mojo-indent-guess-indent-offset
    (mojo-indent-guess-indent-offset)))

;;; Completion predicates for M-x
;; Commands that only make sense when editing Mojo code
(dolist (sym '(mojo-add-import
               mojo-check
               mojo-fill-paragraph
               mojo-fix-imports
               mojo-indent-dedent-line
               mojo-indent-dedent-line-backspace
               mojo-indent-guess-indent-offset
               mojo-indent-shift-left
               mojo-indent-shift-right
               mojo-mark-defun
               mojo-nav-backward-block
               mojo-nav-backward-defun
               mojo-nav-backward-sexp
               mojo-nav-backward-sexp-safe
               mojo-nav-backward-statement
               mojo-nav-backward-up-list
               mojo-nav-beginning-of-block
               mojo-nav-beginning-of-statement
               mojo-nav-end-of-block
               mojo-nav-end-of-defun
               mojo-nav-end-of-statement
               mojo-nav-forward-block
               mojo-nav-forward-defun
               mojo-nav-forward-sexp
               mojo-nav-forward-sexp-safe
               mojo-nav-forward-statement
               mojo-nav-if-name-main
               mojo-nav-up-list
               ;; mojo-remove-import
               ;; mojo-shell-send-buffer
               ;; mojo-shell-send-defun
               ;; mojo-shell-send-statement
               ;; mojo-sort-imports
               ))
  (put sym 'completion-predicate #'mojo--completion-predicate))

(defun mojo-shell--completion-predicate (_ buffer)
  (provided-mode-derived-p
   (buffer-local-value 'major-mode buffer)
   'mojo-base-mode 'inferior-mojo-mode))

;; Commands that only make sense in the Mojo shell or when editing
;; Mojo code.
(dolist (sym '(mojo-describe-at-point
               mojo-eldoc-at-point
               mojo-shell-completion-native-toggle
               mojo-shell-completion-native-turn-off
               mojo-shell-completion-native-turn-on
               mojo-shell-completion-native-turn-on-maybe
               mojo-shell-font-lock-cleanup-buffer
               mojo-shell-font-lock-toggle
               mojo-shell-font-lock-turn-off
               mojo-shell-font-lock-turn-on
               mojo-shell-package-enable
               mojo-shell-completion-complete-or-indent  ))
  (put sym 'completion-predicate #'mojo-shell--completion-predicate))



(provide 'mojo-mode)

;;; mojo.el ends here

