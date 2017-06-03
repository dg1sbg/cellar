# CELLAR LIBRARY - Gönninger B&T's Common Lisp Foundation Library

## NOTE:  June 3, 2017 - This contains a pre-release of Cellar version A.01.00.

## What Is This?
CELLAR is a set of classes, methods and functions in the Common Lisp language. The library is the foundation of all GBT Common Lisp applications.

It provides (not a complete list):
* A base class that ensures each instance gets associated a UUID
* A logging mechanism based on Log4CL
* Most of the functions described in the book On Lisp by Paul Graham
* An implementation of Finite State Machines
and much more ;-) - you are invited to explore ...

## Latest Release
The upcoming release is [TRIANGULUM - CELLAR X.01.00](https://github.com/dg1sbg/cellar/releases/tag/). [Changes](https://github.com/drmeister/clasp/milestone/3)

See the [RELEASE-NOTES](RELEASE-NOTES)

### Getting CELLAR
At the moment, Clasp is supported on Linux and Mac OS X. On these systems, you should be able to build it from source. See the [wiki entry](https://github.com/drmeister/clasp/wiki/Clasp-0.5-Build-Instructions) for instructions. In case things go wrong, the quickest way to get help is to either [file an issue](#reporting-problems), or to [chat with us directly](#irc).

### Required Common Lisp Packages
CELLAR builds on the following Common Lisp packages:

* [Bordeaux-Threads](https://common-lisp.net/project/bordeaux-threads/)
* [Closer MOP](https://common-lisp.net/project/closer/)
* [Alexandria](https://common-lisp.net/project/alexandria/)
* [Babel](https://common-lisp.net/project/babel/)
* [Net Telent Date](http://www.cliki.net/net-telent-date)
* [UUID](http://www.cliki.net/uuid)
* [UIOP](http://www.cliki.net/uiop)
* [trivial-backtrace](http://www.cliki.net/trivial-backtrace)
* [cells](https://github.com/kennytilton/cells/wiki)
* [log4cl](https://github.com/7max/log4cl)

All of them a loadable via [Quicklisp](https://www.quicklisp.org/).

## Reporting Problems
Generally you can report problems in two fashions, either by [opening an issue ticket](https://github.com/dg1sbg/cellar/issues/new) or by [emailing us](#Contact Us). In both cases, though, you should have the following pieces handy in order for us to be able to help you out as quickly and painlessly as possible:

* Your operating system name and version.
* Your Common Lisp implementation name and version.
* The branch of CELLAR that you're using.
* A paste of the build log or failure point that you reached.
* Patience.

## More on CELLAR
For more information on CELLAR and the discussion around it, see the following sites:

* [Frank Gönninger's blog](http://ham-and-eggs-from-frgo.blogspot.de)

## Contact Us
You may contact us via email at [cellar-info@goenninger.net](cellar-info@goenninger.net) or via our website [www.goenninger.net](https://www.goenninger.net).
