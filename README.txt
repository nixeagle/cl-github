                               cl-github
                               =========



Important! This library is not API stable! In general functions and
objects exported from [file:package.lisp] will not have their interfaces
changed without a major version increment. Anything else is subject to
my whim and fancy. Of course the idea is all functions end up exported
;).

As of this writing [2010-02-01 Mon], all current v2 github APIs are
implemented if not release ready or vetted for an interface I'm
comfortable with maintaining.

Table of Contents
=================
1 Dependencies 
    1.1 Some thanks 
2 Overview 
3 Testing 
4 Bugs 


1 Dependencies 
~~~~~~~~~~~~~~~
  - [CL-JSON] - Translate github's api messages to CLOS or simple lists.
  - [DRAKMA]  - HTTP support to talk to github.
  - [Iterate] - A better loop.


  [CL-JSON]: http://common-lisp.net/project/cl-json/
  [DRAKMA]: http://weitz.de/drakma/
  [Iterate]: http://common-lisp.net/project/iterate/

1.1 Some thanks 
================
    This is not a direct dependency, however I made use of some encoding
    functions in [hunchentoot] and its only right that I mention
    Dr. Edmund Weitz's work which I selected 3 functions from and placed
    in [file:url-utils.lisp] along with his copyright statement and
    release under a BSD like license.


    [hunchentoot]: http://weitz.de/hunchentoot

2 Overview 
~~~~~~~~~~~
  The whole public api can be used as of this release, however not all
  of the function names or return results are finalized. Please see
  [file:package.lisp] for functions that are currently exported and won't
  change in terms of input or output without a depreciation cycle.

  The major principle we follow is functions either return a *single*
  object or they return a list of objects. In some extremely simple
  cases, we return lists of strings or lists of other non CLOS objects.

3 Testing 
~~~~~~~~~~
  This is known to work on sbcl 1.0.34, however this library is not
  using any sbcl specific features. Anything that can run drakma,
  cl-json, and iterate ought to run this library without any issues.

4 Bugs 
~~~~~~~
  Please report all bugs to github's tracker, found at
  [http://github.com/nixeagle/cl-github/issues].

