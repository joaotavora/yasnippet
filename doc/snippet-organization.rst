===================
Organizing snippets
===================

:Author: pluskid, joaotavora
:Contact: pluskid@gmail.com
:Date: 2009-08-18

.. contents::

Loading snippets
================

Snippet definitions are stored in files in the filesystem and you have
to arrange for YASnippet to load them (unless you use a `YASnippet
bundle <index.html@bundle-install>`_ (see `No storage (bundle)`_),

The non-bundle version of YASsnippet, once unpacked, comes with a full
directory of snippets, which you can copy somewhere and use. You can
also create or download, one or more directories.

Once these are in place reference them in the variable
``yas/root-directory`` and then load them with ``yas/load-directory``:

.. sourcecode:: common-lisp

  ;; Develop and keep personal snippets under ~/emacs.d/mysnippets
  (setq yas/root-directory "~/emacs.d/mysnippets")

  ;; Load the snippets
  (yas/load-directory yas/root-directory)

The point in using ``yas/root-directory`` (as opposed to calling
``yas/load-directory`` directly) is considering "~/emacs.d/mysnippets"
for snippet development, so you can use commands like
``yas/new-snippet`` and others described `here
<snippet-development.html>`_)

If you make this variable a list and store more items into it...

.. sourcecode:: common-lisp
   :align: right

  ;; Develop in ~/emacs.d/mysnippets, but also
  ;; try out snippets in ~/Downloads/interesting-snippets
  (setq yas/root-directory '("~/emacs.d/mysnippets"
                             "~/Downloads/interesting-snippets"))

  ;; Map `yas/load-directory' to every element
  (mapc 'yas/load-directory yas/root-directory)

, the directories after the first are loaded, their snippets
considered for expansion, but development still happens in
"~/emacs.d/mysnippets"

Organizing snippets
===================

Once you've setup ``yas/root-directory`` , you can store snippets
inside subdirectories of these directories.

Common to *both* cases, snippet definitions are put in plain text
files. They are arranged by subdirectories, and the name of these
directories correspond to the Emacs mode where you want expansion to
take place. For example, snippets for ``c-mode`` are put in the
``c-mode`` subdirectory. You can also skip snippet storage altogether
and use the bundle (see `No storage (bundle)`_).

Nested organization
-------------------

Here is an excerpt of a directory hierarchy containing snippets
for some modes:

.. sourcecode:: text

  $ tree
  .
  `-- text-mode
      |-- cc-mode
      |   |-- c-mode
      |   |   `-- printf
      |   |-- for
      |   |-- java-mode
      |   |   `-- println
      |   `-- while
      |-- email
      |-- perl-mode
      |   |-- cperl-mode
      |   `-- for
      `-- time

The parent directory acts as the *parent mode*. This is the way of
YASnippet to share snippet definitions among different modes. As you
can see above, ``c-mode`` and ``java-mode`` share the same parents
``cc-mode``, while all modes are derived from ``text-mode``. This can
be also used to as an *alias* -- ``cperl-mode`` is an empty directory
whose parent is ``perl-mode``.

The ``.yas-parents`` file
------------------------------

If you place a plain text file ``.yas-parents`` inside one of the
subdirectories you can bypass nesting and still have parent modes. In
this file you just write whitespace-separated names of modes. This
allows more flexibility and readability of your snippet hierarchy.

.. sourcecode:: text

  $ tree
  .
  |-- c-mode
  |   |-- .yas-parents    # contains "cc-mode text-mode" 
  |   `-- printf
  |-- cc-mode
  |   |-- for
  |   `-- while
  |-- java-mode
  |   |-- .yas-parents    # contains "cc-mode text-mode"
  |   `-- println
  `-- text-mode
      |-- email
      `-- time

The ``.yas-make-groups`` file
-----------------------------

.. image:: images/group.png
   :align: right

If you place an empty plain text file ``.yas-make-groups`` inside one
of the mode directories, the names of these subdirectories are
considered groups of snippets and the `YASsnippet menu` is organized
much more cleanly, as you can see in the image.

Another alternative way to achieve this is to place a ``# group:``
directive inside the snippet definition. See `Writing snippets
<snippet-development.html>`_

.. sourcecode:: text

  $ tree ruby-mode/
  ruby-mode/
  |-- .yas-make-groups
  |-- collections
  |   |-- each
  |   `-- ...
  |-- control structure
  |   |-- forin
  |   `-- ...
  |-- definitions
  |   `-- ...
  `-- general
      `-- ...


Using plain file names
----------------------

Normally, file names act as the snippet trigger *key*, see `Expanding
snippets <snippet-expansion.html>`_. However, if you customize the
variable ``yas/ignore-filenames-as-triggers`` to be true *or* place an
empty file ``.yas-ignore-filename-triggers`` you can use much more
descriptive file names. This is useful (but not mandatory) if many
snippets within a mode share the same trigger key.

.. sourcecode:: text

  $ tree rails-mode/
  rails-mode/
  |-- .yas-make-groups
  |-- .yas-ignore-filename-triggers
  |-- Insert ERb's <% __ %> or <%= __ %>.yasnippet
  |-- asserts
  |   |-- assert(var = assigns(%3Avar)).yasnippet
  |   |-- assert_difference.yasnippet
  |   |-- assert_no_difference.yasnippet
  |   |-- assert_redirected_to (nested path plural).yasnippet
  |   |-- assert_redirected_to (nested path).yasnippet
  |   |-- assert_redirected_to (path plural).yasnippet
  |   |-- assert_redirected_to (path).yasnippet
  |   |-- assert_rjs.yasnippet
  |   `-- assert_select.yasnippet



No storage (bundle)
===================

The most convenient way to define snippets for YASnippet is to put
them in a directory arranged by the mode and use
``yas/load-directory`` to load them.

However, this might slow down the Emacs startup speed if you have many
snippets. You can use ``yas/define-snippets`` to define a bunch of
snippets for a particular mode in an emacs-lisp file.

Since this is hard to maintain, there's a better way: define your
snippets in directory and then call ``M-x yas/compile-bundle`` to
compile it into a bundle file when you modified your snippets.

The release bundle of YASnippet is produced by
``yas/compile-bundle``. The bundle uses ``yas/define-snippets`` to
define snippets. This avoids the IO and parsing overhead when loading
snippets.

Further more, the generated bundle is a stand-alone file not depending
on ``yasnippet.el``. The released bundles of YASnippet are all
generated this way.

See the internal documentation for the functions
``yas/define-snippets`` and ``yas/compile-bundle``.
