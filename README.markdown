This directory contains _mythryl-mode_, an _Emacs_ major mode for the _Mythryl_ programming language.

- Copyright (C) 2009 Phil Rand <philrand@gmail.com>
- Copyright (C) 2010, 2011, 2012 Michele Bini <michele.bini@gmail.com>
- Current maintainer: Michele Bini <michele.bini@gmail.com>

Mythryl is a safe, functional programming language, derived by Cynbe ru Taren from SML/NJ, but featuring a clear, more programmer-friendly syntax.  See http://mythryl.org for more.


# Usage

Activate mythryl-mode for an Emacs buffer:

    M-x mythryl-mode

Execute an interactive Mythryl session:

    M-x run-mythryl

Customize indentation and fontification of Mythryl code:

    M-x customize-group mythryl


# Installation

To use this mode, install this file in your elisp load path; then insert the expression:

    (load "mythryl-mode")

in your `.emacs` file.

Other useful lines for your .emacs (to automatically activate mythryl-mode for .pkg .api and .mythryl files)

    ;; for .pkg and .api files
    (setq auto-mode-alist
          (append '(("\\.pkg$" . mythryl-mode)
                    ("\\.api$" . mythryl-mode)
                    ("\\.mythryl$" . mythryl-mode))
                  auto-mode-alist))
    
    ;; for scripts starting with #!/.../mythryl
    (setq interpreter-mode-alist
          (append '(("mythryl" . mythryl-mode))
                  interpreter-mode-alist))

Alternatively, installation via a Debian package is possible on compatible Linux distributions (see repository below)


# Repositories

You can find versions of mythryl-mode at:

- https://github.com/rev22/mythryl-mode
  - Forked to https://github.com/spiralofhope/mythryl-mode
- [EmacsWiki](http://www.emacswiki.org/emacs/MythrylMode)
- [Debian package](https://launchpad.net/~michele-bini/+archive/ppa-mbxxii)


# History

_Mythryl-mode_ was derived by Phil Rand from Stefan Monnier's _[sml-mode](https://github.com/emacsmirror/sml-mode)_.

After the tragic loss of the original author and maintainer Phil Rand in 2011, I (Michele Bini) decided to enlist as a maintainer.

I (spiralofhope) forked it to keep a copy for reference; I don't use emacs.
