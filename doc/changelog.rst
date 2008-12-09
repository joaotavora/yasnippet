=========
ChangeLog
=========

:Author: pluskid
:Contact: pluskid@gmail.com
:Date: 2008-03-22

0.5.7 / 2008-12-03
==================

* Fixed `Issue 28
  <http://code.google.com/p/yasnippet/issues/detail?id=28>`_ of
  properly clean up snippet (by joaotavora).
* Added a new section "Field-level undo functionality" to correct
  `Issue 33 <http://code.google.com/p/yasnippet/issues/detail?id=33>`_
  (by joaotavora).
* Added some snippets from users for sql, erlang, scala, html, xml, latex, etc.
* Fixed `Issue 16
  <http://code.google.com/p/yasnippet/issues/detail?id=16>`_ by adding
  ``$>`` support. Here's the `doc for $> indenting
  <http://pluskid.lifegoo.com/upload/project/yasnippet/doc/define_snippet.html#indenting>`_.

0.5.6 / 2008-08-07
==================

* Added a buffer local variable ``yas/dont-activate`` to turn off
  ``yas/minor-mode`` in some major modes. See `Issue 29
  <http://code.google.com/p/yasnippet/issues/detail?id=29>`_.
* Make the environment of elisp evaluation more friendly to
  ``(current-column)``.
* Fixed the regular expression bug in python-mode snippets.
* Use filename or full key extension for snippet name if no ``name``
  property is defined.

0.5.5 / 2008-05-29
==================

* Tweak ``yas/extra-mode-hooks`` so that it can be more easily
  customized.
* Add an entry in FAQ about why ``TAB`` key doesn't work in some
  modes.

0.5.4 / 2008-05-15
==================

* Added ``ox-mode-hook`` and ``python-mode-hook`` to
  ``yas/extra-mode-hooks`` to fix the problem YASnippet is not enabled
  in those modes.

0.5.3 / 2008-05-07
==================

* Fix indent of python-mode snippets.
* Fix a bug of dropdown-list: conflicts with color-theme (`Issue 23
  <http://code.google.com/p/yasnippet/issues/detail?id=23>`_). Thanks
  Mike.
* Fix a bug of condition system.

0.5.2 / 2008-04-20
==================

* Fix a bug for comparing string to symbol using ``string=`` (which
  will fire an error).

0.5.1 / 2008-04-14
==================

* Use a beautiful css style in the document.

0.5.0 / 2008-04-10
==================

* Integrate with hippie-expand. Just add ``yas/hippie-try-expand`` to
  ``hippie-expand-try-functions-list``.
* If you set ``yas/fall-back-behavior`` to ``'return-nil``, YASnippet
  will return nil when it can't find a snippet to expand.
* Defect fix: the condition of a snippet was evaluated twice in
  earlier version.
* Deleting snippet (using ``C-w`` or ``C-k``) won't cause serious
  problem now.
* Several complex snippet for python-mode from Yasser included in the
  distribution.

0.4.5 / 2008-04-07
==================

* Merge the latest dropdown-list.el.
* Add snippets for f90-mode from Li Zhu.
* Bug fix: l-safe-expr-p: Lisp nesting exceeds ``max-lisp-eval-depth``
  error when several (more than two) snippets overlaps. Thanks
  sunwaybupt@newsmth for reporting this bug.

0.4.4 / 2008-03-24
==================

* Bug fix: dropdown-list.el doesn't recognize [return] properly.

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
