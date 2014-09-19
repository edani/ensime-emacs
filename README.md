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


# Getting Started

See our [Quick Start Guide](http://github.com/ensime/ensime-server/wiki/Quick-Start-Guide) to learn how to install and start ENSIME.

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
