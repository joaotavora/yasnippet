===================
Organizing snippets
===================

:Author: pluskid, joaotavora
:Contact: pluskid@gmail.com
:Date: 2009-08-18

.. contents::

There are three ways to keep your snippets:


New-style storage (recommended)
===============================

Hehe

Old-style storage (pre 0.6)
===========================

Blabla

No storage (bundle)
===================

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

  (KEY TEMPLATE NAME CONDITION GROUP)

The ``NAME``, ``CONDITION`` and ``GROUP`` can be omitted if you don't
want to provide one. Here's an example:

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


yas/define
~~~~~~~~~~

The basic syntax for ``yas/define`` is

.. sourcecode:: common-lisp

  (yas/define mode key template &optional name condition group)

This is only a syntax sugar for

.. sourcecode:: common-lisp

  (yas/define-snippets mode
                       (list (list key template name condition group)))

yas/compile-bundle
~~~~~~~~~~~~~~~~~~

``yas/compile-bundle`` can be used to parse the snippets from a
directory hierarchy and translate them into the elisp form. The
translated code is faster to load. Further more, the generated bundle
is a stand-alone file not depending on ``yasnippet.el``. The released
bundles of YASnippet are all generated this way.



The Menu
~~~~~~~~

YASnippet will setup a menu just after the *Buffers* Menu in the
menubar. The snippets for all *real* modes are listed there under the
menu. You can select a snippet from the menu to expand it. Since you
select manually from the menu, you can expand any snippet. For
example, you can expand a snippet defined for ``python-mode`` in a
``c-mode`` buffer by selecting it from the menu:

.. image:: images/menubar.png
   :align: right

* Condition system is ignored since you select to expand it
  explicitly.
* There will be no muliple candidates since they are listed in the
  menu as different items.

This can be convenient sometimes. However, if you don't like the
menubar of Emacs and never use it. You can tell YASnippet don't boring
to build a menu by setting ``yas/use-menu`` to nil.

Another thing to note is that only *real* modes are listed under the
menu. As you know, common snippets can be shared by making up a
*virtual* parent mode. It's too bad if the menu is floored by those
*virtual* modes. So YASnippet only show menus for those *real*
modes. But the snippets fo the *virtual* modes can still be accessed
through the ``parent`` submenu of some *real* mode.

YASnippet use a simple way to check whether a mode is *real* or
*virtual*: ``(fboundp mode)``. For example, the symbol ``c-mode`` is
bound to a function while ``cc-mode`` is not. But this is not enough,
some modes aren't part of Emacs, and maybe when initializing
YASnippet, those modes haven't been initialized. So YASnippet also
maintain a list of known modes (``yas/known-modes``). You can add item
to that list if you need.

The basic syntax of ``yas/compile-bundle`` is

.. sourcecode:: common-lisp

  (yas/compile-bundle &optional yasnippet yasnippet-bundle snippet-roots code dropdown)

As you can see, all the parameters are optional. The default values
for those parameters are convenient for me to produce the default
release bundle:

.. sourcecode:: common-lisp

  (yas/compile-bundle "yasnippet.el"
                      "./yasnippet-bundle.el"
                      '("snippets")
                      "(yas/initialize)"
		      "dropdown-list.el")

The ``snippet-roots`` can be a list of root directories. This is
useful when you have multiple snippet directories (maybe from other
users). The ``code`` parameter can be used to specify your own
customization code instead of the default ``(yas/initialize)``. For
example, you can set ``yas/trigger-key`` to ``(kbd "SPC")`` here if
you like.

From release 0.6 you have to specify the ``dropdown-list.el`` file if
you want it to be a part of the generated bundle.
