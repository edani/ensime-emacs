# Before Opening an Issue

* if the problem is related to the update stage or a dodgy ENSIME classpath, try nuking your ivy cache and `ensime-reload`.
* check the [tickets flagged as FAQ on the server](https://github.com/ensime/ensime-server/issues?labels=FAQ) and [tickets flagged as FAQ for Emacs](https://github.com/ensime/ensime-emacs/issues?labels=FAQ).
* check the [most recently updated tickets](http://github.com/ensime/ensime-emacs/issues?direction=desc&sort=updated).
* do a few keyword searches using the github search (top of the page) to see if anybody has reported this already.

# Contributors

If you use this software you are already well placed, as a Scala
developer, to make it even better.

All interactions within the ENSIME community have been friendly and
open: we want this to continue, formalised by our
[Code of Conduct](https://github.com/ensime/ensime-server/wiki/Code-of-Conduct).

Please help out by:

* [Picking up some low hanging fruit](https://github.com/ensime/ensime-emacs/issues?labels=Low+Hanging+Fruit) (also [for the server](https://github.com/ensime/ensime-server/issues?labels=Low+Hanging+Fruit))
* [Fixing a bug](http://github.com/ensime/ensime-emacs/issues?labels=Bug) (also [for the server](http://github.com/ensime/ensime-emacs/issues?labels=Bug))
* [Helping with the current Milestone](http://github.com/ensime/ensime-emacs/issues/milestones) (also [for the server](http://github.com/ensime/ensime-server/issues/milestones))
* Sending an unsolicited pull request with a new feature
* Joining the conversation on [![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/ensime/ensime-emacs) (also [for the server](https://gitter.im/ensime/ensime-emacs))
* Telling your co-workers!

We are using some great technologies to automate our build and testing process:

* Kanban project planning from [waffle.io](https://waffle.io/ensime/ensime-server)
* Continuous Integration from [shippable.com](https://app.shippable.com/subscriptions/5504572d5ab6cc13529ad105)
* Coverage reports from coveralls.io [coveralls.io](https://coveralls.io/r/ensime/ensime-server)
* Binary distribution from [sonatype.org](http://www.sonatype.org/)
* Emacs distributions from [MELPA](http://melpa.milkbox.net/#/ensime)

Along with unit testing, we have automated coverage checks and code
formatting as part of our build process. Pull requests will only be
accepted if the build and tests are successful, coverage has not
decreased, and the formatting style has not been changed (it is
automated, so you don't need to think about it).

Pull requests will be reviewed and should not be merged by the person
who created the request (except for trivial changes and hotfixes).

We have branches for older versions of scala, which are merged regularly.
