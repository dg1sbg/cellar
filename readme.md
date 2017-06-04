# <img src="https://github.com/dg1sbg/cellar/raw/X.01.00/root-cellar.jpg"/>  THE CELLAR LIBRARY

<p><h2><b>Gönninger B&T's Common Lisp Application Foundation Library</b></h2>

## NOTE:  June 3, 2017 - This contains pre-release X.01.00 of Cellar version A.01.00.

## What Is This?
CELLAR is a set of classes, methods and functions in the Common Lisp language. The library is the foundation of all GBT Common Lisp applications.

It provides (not a complete list):

* A base class that ensures each instance gets associated a UUID
* A logging mechanism based on Log4CL
* Most of the functions described in the book On Lisp by Paul Graham
* An implementation of Finite State Machines

and much more 😉 - you are invited to explore ...

## Latest Release
The upcoming release is [TRIANGULUM - CELLAR X.01.00](https://github.com/dg1sbg/cellar/releases/tag/CELLAR_PREREL_X0100). See the [changes](https://github.com/dg1sbg/cellar/milestone/1) for this release.

## Required Common Lisp Packages
CELLAR builds on the following Common Lisp packages:

* [bordeaux-threads](https://common-lisp.net/project/bordeaux-threads/)
* [closer mop](https://common-lisp.net/project/closer/)
* [alexandria](https://common-lisp.net/project/alexandria/)
* [babel](https://common-lisp.net/project/babel/)
* [net.telent.date](http://www.cliki.net/net-telent-date)
* [uuid](http://www.cliki.net/uuid)
* [uiop](http://www.cliki.net/uiop)
* [trivial-backtrace](http://www.cliki.net/trivial-backtrace)
* [cells](https://github.com/kennytilton/cells/wiki)
* [log4cl](https://github.com/7max/log4cl)

All of them are loadable via [quicklisp](https://www.quicklisp.org/).

## License
<a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/"><img alt="Creative Commons License" style="border-width:0" src="https://i.creativecommons.org/l/by-nc/4.0/88x31.png" /></a><br />CELLAR is licensed under a <a rel="license" href="http://creativecommons.org/licenses/by-nc/4.0/">Creative Commons Attribution-NonCommercial 4.0 International License</a>. Any violation of this license will be prosecuted. German law applies in all cases and under all circumstances.

## Reporting Problems
Generally you can report problems in two fashions, either by [opening an issue ticket](https://github.com/dg1sbg/cellar/issues/new) or by [emailing to CELLAR support](mailto:cellar-support@goenninger.net). In both cases, though, you should have the following pieces handy in order for us to be able to help you out as quickly and painlessly as possible:

* Your operating system name and version.
* Your Common Lisp implementation name and version.
* The branch of CELLAR that you're using.
* A paste of the build log or failure point that you reached.
* Patience.

## Contact
You may contact us via email at [cellar-info@goenninger.net](mailto:cellar-info@goenninger.net) or via our website [www.goenninger.net](https://www.goenninger.net). See also [frgo's blog](http://ham-and-eggs-from-frgo.blogspot.de) for an occasional post about CELLAR.
