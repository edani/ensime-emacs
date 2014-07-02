# ENSIME

ENhanced Scala Interaction Mode for [Emacs](http://www.gnu.org/software/emacs/).

This project provides the Emacs support for the ENSIME server and
currently shares the same issue tracker. This project is actively
community maintained, and we are very pleased to see contributions
from new members. Please visit the
[server's github page](/ensime/ensime-server)
to find out more about how you can help.

ENSIME brings IDE-like features to your favourite text editor, such as:

- Show the `type` of the symbol under the cursor.
- Contextual completion for `var`s, `val`s and `def`s.
- Add an import for the symbol under the cursor.
- Fast classpath search (types and members).
- Jump to source code or documentation.
- Browse packages and type hierarchies.
- Find all references to a symbol.
- Refactorings (rename, organize imports, extract method).
- REPL with stack trace highlighting.
- Errors and warnings in your code: *red squigglies*.
- Debugging

and many more.


# Quick Start

There are two ways to install this extension. You can use MELPA (**recommended**):

```elisp
;; if you're new to the MELPA package manager, this is how to add it
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
;; and then do a M-x package-install RET "ensime" RET
```

Or fork and clone this repository into a directory of your choice and
add it explicitly into your `~/.emacs` (developer front-end install, manual dependency management):

```elisp
;; assuming you put the repository in ~/.emacs.d/ensime
(add-to-list 'load-path (concat user-emacs-directory "ensime"))
```


In either case, enable ensime with the following:

```elisp
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
```

Much of the actual text editing is provided by the excellent
[scala-mode2](https://github.com/hvesalai/scala-mode2), which can
be customised.



## Getting Started

A project needs to have a `.ensime` configuration file. Luckily we
have a [plugin for SBT projects](https://github.com/ensime/ensime-sbt/)
and [`maker` provides out-of-the-box support](https://github.com/cage433/maker),
which will automatically create an appropriate `.ensime` config.
The [example `.ensime` configuration documents the available entries and options](https://github.com/ensime/ensime-server/wiki/Example-Configuration-File).


We would love to receive user-contributed
[maven](https://github.com/ensime/ensime-server/issues/481)
and [ivy](https://github.com/ensime/ensime-server/issues/482) generators.

For best behaviour, the ENSIME server needs to be running the same
version of scala that your project uses. The default version of scala
is defined by `ensime-default-scala-version` but you can specify
this per-project with `:scala-version "2.9.3"` in `.ensime`.

Then simply `M-x ensime` and point it at your project config, which may download the latest ENSIME server for the specified version of scala.
If the download fails, or you want to use a specific version of the server, install the developer version following the instructions on
[the ENSIME server github page](http://github.com/ensime/ensime-server#quick-start).

Once the server is available, wait for the analyzer to complete and
enjoy editing with the ENSIME commands that are conveniently
summarised in our
[ENSIME Quick command reference](http://github.com/ensime/ensime-emacs/wiki/Quick-command-reference)
(or [read it straight from the source](http://github.com/ensime/ensime-emacs/blob/master/ensime-mode.el#L49)).

Emacs-wide customisations are defined in [ensime-vars.el](http://github.com/ensime/ensime-emacs/blob/master/ensime-vars.el) and will appear in `M-x customize`.

Keeping up to date with releases is recommended. Melpa manages upgrading of
packages and if you're running from source you will need to
`git pull --rebase upstream master` regularly.


Before reporting any problems with ENSIME, please:

* check the [tickets flagged as FAQ](https://github.com/ensime/ensime-server/issues?labels=FAQ).
* check the [most recently updated tickets](http://github.com/ensime/ensime-server/issues?direction=desc&sort=updated) (others are probably talking about it already with workarounds).
* do a few keyword searches using the github search (top of the page) to see if anybody has reported this already.



## Further Information

Although the ENSIME's options are fully documented in the emacs
customization pages (`M-x customize RET "ensime"`), you may also wish to read the [ENSIME User
Manual](http://ensime.github.io/).

[Older releases](https://www.dropbox.com/sh/ryd981hq08swyqr/V9o9rDvxkS/ENSIME%20Releases)
are bundled with the server.
