=========================
How to define a snippet ?
=========================

:Author: pluskid
:Contact: pluskid@gmail.com
:Date: 2008-03-20

.. contents::

The most convenient way to define snippets for YASnippet is to put
them in a directory arranged by the mode and use
``yas/load-directory`` to load them. 

However, this might slow down the Emacs startup speed if you have many
snippets. You can use ``yas/define-snippets`` to define a bunch of
snippets for a perticular mode. But this is hard to maintain! So,
there's a better way: define your snippets in directory and use
``yas/compile-bundle`` to compile it into a bundle file when you
modified your snippets.

The release bundle of YASnippet is produced by
``yas/compile-bundle``. The bundle use ``yas/define-snippets`` to
define snippets. This avoid the IO and parsing overhead when loading
snippets.

Finally, you can use ``yas/define`` to define a single snippet at your
convenience. I ofthen use this to do some testing.

Define snippets in files
========================

Directory hierarchy
-------------------

Here's the directory hierarchy of the ``snippets`` directory comes
with YASnippet:

.. sourcecode:: text

  snippets
  `-- text-mode/
      |-- cc-mode/
      |   |-- c++-mode/
      |   |   |-- beginend
      |   |   |-- class
      |   |   `-- using
      |   |-- c-mode/
      |   |   `-- fopen
      |   |-- do
      |   |-- for
      |   |-- if
      |   |-- inc
      |   |-- inc.1
      |   |-- main
      |   |-- once
      |   `-- struct
      |-- css-mode/
      |   |-- background
      |   |-- background.1
      |   `-- border
      |-- email
      |-- html-mode/
      |   |-- div
      |   |-- doctype
      |   |-- doctype.xhml1
      |   |-- doctype.xhtml1_1
      |   |-- doctype.xhtml1_strict
      |   `-- doctype.xhtml1_transitional
      |-- objc-mode/
      |   `-- prop
      |-- perl-mode/
      |   |-- cperl-mode/
      |   |-- eval
      |   |-- for
      |   |-- fore
      |   |-- if
      |   |-- ife
      |   |-- ifee
      |   |-- sub
      |   |-- unless
      |   |-- while
      |   |-- xfore
      |   |-- xif
      |   |-- xunless
      |   `-- xwhile
      |-- python-mode/
      |   |-- __
      |   |-- class
      |   |-- def
      |   |-- for
      |   |-- ifmain
      |   `-- while
      |-- rst-mode/
      |   |-- chapter
      |   |-- section
      |   `-- title
      |-- ruby-mode/
      |   |-- #
      |   |-- =b
      |   |-- Comp
      |   |-- all
      |   |-- am
      |   |-- any
      |   |-- app
      |   |-- bm
      |   |-- case
      |   |-- cla
      |   |-- classify
      |   |-- cls
      |   |-- collect
      |   |-- dee
      |   |-- deli
      |   |-- det
      |   |-- ea
      |   |-- eac
      |   |-- eai
      |   |-- eav
      |   |-- eawi
      |   |-- forin
      |   |-- if
      |   |-- ife
      |   |-- inject
      |   |-- mm
      |   |-- r
      |   |-- rb
      |   |-- reject
      |   |-- req
      |   |-- rreq
      |   |-- rw
      |   |-- select
      |   |-- w
      |   |-- y
      |   `-- zip
      `-- time

Snippet definitions are put in plain text files. They are arranged by
subdirectories. For example, snippets for ``c-mode`` are put in the
``c-mode`` directory.

The parent directory acts as the *parent mode*. This is the way of
YASnippet to share snippet definitions among different modes. As you
can see above, ``c-mode`` and ``c++-mode`` share the same parents
``cc-mode``, while all modes are derived from ``text-mode``. This can
be also used to as an *alias* -- ``cperl-mode`` is an empty directory
whose parent is ``perl-mode``.

File names act as the snippet trigger key. Note files starting with a
dot (``.``) are ignored.

File content
------------

A file defining a snippet may just contain the template for the
snippet. Optionally it can also contains some meta data for the
snippet as well as comments.

Generally speaking, if the file contains a line of ``# --``, then all
contents above that line are considered as meta data and comments;
below are template. Or else the whole file content is considered as
the template.

Here's a typical example:

.. sourcecode:: text

  #contributor : pluskid <pluskid@gmail.com>
  #name : __...__
  # --
  __${init}__

Meta data are specified in the syntax of

.. sourcecode:: text

  #data-name : data value

Any other text above ``# --`` is considered as comment and
ignored. You might want to refer to the list of currently `supported
meta data`_ .

Define snippets using elisp code
--------------------------------

As I mentioned above, you can define snippets directly by writing
elisp code.

yas/define-snippets
~~~~~~~~~~~~~~~~~~~

The basic syntax of ``yas/define-snippets`` is

.. sourcecode:: common-lisp

  (yas/define-snippets MODE SNIPPETS &optional PARENT)

The parameters are self-descriptive. If you specify a ``PARENT``, then
the snippets of the parents may be shared by ``MODE``. Note if you use
this function several times, the later specified ``PARENT`` will
overwrite the original one. However, not specifying a ``PARENT`` won't
erase the original parent.

The ``SNIPPETS`` parameter is a list of snippet definitions. Each
element should have the following form:

.. sourcecode:: common-lisp

  (KEY TEMPLATE NAME CONDITION)

The ``NAME`` and ``CONDITION`` can be omitted if you don't want to
provide one. Here's an example:

.. sourcecode:: common-lisp

  (yas/define-snippets 'c++-mode
  '(
    ("using" "using namespace ${std};
  $0" "using namespace ... " nil)
    ("class" "class ${1:Name}
  {
  public:
      $1($2);
      virtual ~$1();
  };" "class ... { ... }" nil)
    ("beginend" "${1:v}.begin(), $1.end" "v.begin(), v.end()" nil)
    )
  'cc-mode)

The example above is auto-generated code by ``yas/compile-bundle``.

yas/compile-bundle
~~~~~~~~~~~~~~~~~~

``yas/compile-bundle`` can be used to parse the snippets from a
directory hierarchy and translate them into the elisp form. The
translated code is faster to load. Further more, the generated bundle
is a stand-alone file not depending on ``yasnippet.el``. The released
bundles of YASnippet are all generated this way.

The basic syntax of ``yas/compile-bundle`` is

.. sourcecode:: common-lisp

  (yas/compile-bundle &optional yasnippet yasnippet-bundle snippet-roots code)

As you can see, all the parameters are optional. The default values
for those parameters are convenient for me to produce the default
release bundle:

.. sourcecode:: common-lisp

  (yas/compile-bundle "yasnippet.el"
                      "./yasnippet-bundle.el"
                      '("snippets")
                      "(yas/initialize)")

The ``snippet-roots`` can be a list of root directories. This is
useful when you have multiple snippet directories (maybe from other
users). The ``code`` parameter can be used to specify your own
customization code instead of the default ``(yas/initialize)``. For
example, you can set ``yas/trigger-key`` to ``(kbd "SPC")`` here if
you like.

The Syntax of the Template
==========================

.. _supported meta data:

* ``name``: The name of the snippet. This is a one-line description of
  the snippet. It will be displayed in the menu. So it's a good idea
  to select a descriptive name fo a snippet -- especially
  distinguishable among similar snippets.
* ``contributor``: The contributor of the snippet.
* ``condition``: The condition of the snippet. This is a piece of
  elisp code. If a snippet has a condition, then it will only be
  expanded when the condition code evaluate to some non-nil value.
