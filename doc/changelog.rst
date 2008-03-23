=========
ChangeLog
=========

:Author: pluskid
:Contact: pluskid@gmail.com
:Date: 2008-03-22

0.4.3 / 2008-03-23
==================

* Bug fix: failed to recognize user customized yas/trigger-key.

0.4.2 / 2008-03-22
==================

* Make a separate document package for release. Also make document
  available online.

0.4.1 / 2008-03-21
==================

* Make sure ``yas/minor-mode``'s key bindings always take priority to
  other minor modes.

0.4.0 / 2008-03-20
==================

* Document refinement and released with YASnippet. Most of the Online
  wiki document will be deprecated soon.
* Powerful condition system added to yasnippet!
* Incorporate ``dropdown-list.el`` and make it default way for
  selecting multiple candidates. Thanks to `Jaeyoun Chung
  <http://groups.google.com/group/smart-snippet/browse_thread/thread/c869158b76addeb3/e7c6372ba457189e>`_.
* yas/before-expand-snippet-hook

0.3.2 / 2008-03-19
==================

* Enhancement: A better way to define minor-mode. Thanks to Kentaro
  Kuribayashi. See `this thread
  <https://groups.google.com/group/smart-snippet/browse_thread/thread/65cb3b5583eda887?hl=en>`_
  for more details.

0.3.1 / 2008-03-17
==================

* Bug fix: Emacs get confused when a field is deleted. See `issue 10
  <http://code.google.com/p/yasnippet/issues/detail?id=10>`_.

0.3.0 / 2008-03-16
==================

* Add a ``yas/after-exit-snippet-hook`` so that you can do something like
  ``indent-region`` or ``fill-region`` after finish the snippet.
* Use minor-mode instead of ``global-set-key`` to bind the trigger
  key. Now the trigger key and fall-back behavior can be more
  flexible. Not constrained to ``<tab>``. Thanks to Trey Jackson. See
  this `thread
  <https://groups.google.com/group/smart-snippet/browse_thread/thread/937f32a2a6dea4f2?hl=en>`_
  for more details.
* Now user can customize the popup function for selecting multiple
  candidate for the same snippet key.
* Support ``dropdown-list.el`` to be a better way to select multiple
  candidate when in text mode.

0.2.3 / 2008-03-15
==================

* Bug in non-window (-nw) mode when there's multiple candidate to
  expand. See `issue 7
  <http://code.google.com/p/yasnippet/issues/detail?id=7>`_.
* Allow expanding another snippet as long as not currently inside a
  field. 

0.2.2 / 2008-03-13
==================

* Added customized face for fields and mirrors. Better in dark
  background. And users can customize it.

0.2.1 / 2008-03-10
==================

* Fix the insert-behind problem under both Emacs 22 and Emacs 23. 

0.2.0 / 2008-03-10
==================

* Use big keymap overlay to detect ``insert-behind`` event manually to
  avoid sometimes missed hook calls. See `issue 3
  <http://code.google.com/p/yasnippet/issues/detail?id=3>`_ for more
  details.
* Support parent snippet table. Now you can set (for example)
  ``cc-mode`` as common mode for ``c++-mode``, ``c-mode`` and
  ``java-mode``. They'll share snippets defined for ``cc-mode``.

0.1.1 / 2008-03-08
==================

* Add a rake task to upload to google code.
* Use elisp compile-bundle function instead of python scrip

0.1.0 / 2008-03-07
==================

* Embedded elisp support.
* Fields navigation support.
* Mirror of fields support.
* Menu-bar support.
* Multiple snippets with same name support.
* Popup menu for multiple snippet with same name support.
* Transformation of fields support.
* Load directory support.
* Compile bundle support. 
