# Major mode for editing Mojo source code

It would be rather dull if Emacs did not have a major mode (for
editing) and a REPL minor mode for Mojo. This is an attempt at
providing both, albeit, by hacking on `python.el`.

Current working features:
- font-locking
- indentation
- movement
- REPL

At time of writing, a person writing Mojo code will likely have the
Mojo language server available to them. This integrates nicely with
`eglot`, yielding effortless ElDoc and imenu support.

In addition to the above, by analogy with `rustic` and `cargo`,
support for `mojo` commands are included in `mojo-mode`'s keymap.

``` emacs-lisp
(define-key map (kbd "C-c C-c C-r") #'mojo-run)
(define-key map (kbd "C-c C-c C-b") #'mojo-build)
(define-key map (kbd "C-c C-c C-f") #'mojo-format)
(define-key map (kbd "C-c C-c C-t") #'mojo-test)
```

The powerful convenience of using `compile` to run and display common
commands be familiar to any user of `rustic`. Combined with a REPL,
this yields a very enjoyable development experience.

## Installation

Clone this repository

``` shell
git clone https://github.com/andrewjradcliffe/mojo-mode.git
```

Then, add the following to your `.emacs`, `init.el` or however you
configure your Emacs initialization.

``` emacs-lisp
(add-to-list 'load-path "/path/to/mojo-mode")
(require 'mojo-mode)
(require 'mojo-repl)
(add-hook 'mojo-mode-hook 'mojo-repl-mode)
```

### `eglot` support

For the best experience, you should use `eglot` (`lsp-mode` may also
work, I haven't tested it!) to provide language server support, which
will include ElDoc, imenu and a host of other nice things.

Configuration looks something like:

``` emacs-lisp
;; Only if you need to activate eglot
(use-package eglot
  :ensure t)

;; Otherwise, just these two lines
(add-to-list 'eglot-server-programs '(mojo-mode . ("mojo-lsp-server")))
(add-hook 'mojo-mode-hook 'eglot-ensure)
```

### REPL support

For the REPL to work, you will need
[vterm](https://github.com/akermu/emacs-libvterm). `vterm` provides a
high-performance virtual terminal, in contrast to the traditional
alternatives -- though, I hear good things about
[eat](https://codeberg.org/akib/emacs-eat).

If you don't have it already, add something like this to your init file:
``` emacs-lisp
(use-package vterm
  :ensure t
  :init
  (setq vterm-always-compile-module t)
  :config
  (setq vterm-max-scrollback 100000)
  (setq vterm-kill-buffer-on-exit nil)
  ;; If you want color support, configure
  (setq vterm-term-environment-variable "eterm-256color")
  )
```

Color support and font-locking with `vterm` works best with
[eterm-256color](https://github.com/dieggsy/eterm-256color) (or
[xterm-color](https://github.com/atomontage/xterm-color)). In my
experience, minor adjustments to colors may be necessary, depending on
the theme one uses.

## Compatibility

At time of writing, the author has only tested this package with Emacs
29.3, compiled from source with essentially all features enabled; OS
is Ubuntu 22.04 LTS.

## Contributing

Contributions are welcome. Notably, many fancy features can be added;
the current implementation is simple, but provides most of what one
needs to get started.

Future improvements:
- fix some minor issues around font-locking for variable declarations
  and assignment
- font-locking for types and parameters
- `mojo debug` integration

## Stability and roadmap

As the syntax and semantics of Mojo evolve, so must this package. Let
us tentatively set an objective of being compatible with the latest
stable release of Mojo.
