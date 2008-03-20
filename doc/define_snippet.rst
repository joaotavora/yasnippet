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

yas/define
~~~~~~~~~~

The basic syntax for ``yas/define`` is

.. sourcecode:: common-lisp

  (yas/define mode key template &optional name condition)

This is only a syntax sugar for

.. sourcecode:: common-lisp

  (yas/define-snippets mode
                       (list (list key template name condition)))

The strategy to select a snippet
================================

When user press the ``yas/trigger-key``, YASnippet try to find a
proper snippet to expand. The strategy to find such a snippet is
explained here.

Finding the key
---------------

YASnippet search from current point backward trying to find the
snippet to be expanded. The default searching strategy is quite
powerful. For example, in ``c-mode``, ``"bar"``, ``"foo_bar"``,
``"#foo_bar"`` can all be recognized as a template key. Further more,
the searching is in that order. In other words, if ``"bar"`` is found
to be a key to some *valid* snippet, then ``"foo_bar"`` and
``"#foobar"`` won't be searched.

However, this strategy can also be customized easily from the
``yas/key-syntaxes`` variable. It is a list of syntax rules, the
default value is ``("w" "w_" "w_." "^ ")``. Which means search the
following thing until found one:

* a word.
* a symbol. In lisp, ``-`` and ``?`` can all be part of a symbol.
* a sequence of characters of either word, symbol or punctuation.
* a sequence of characters of non-whitespace characters.

But you'd better keep the default value unless you understand what
Emacs's syntax rule mean.

The condition system
--------------------

I write forked snippet.el to make the smart-snippet.el. I call it
*smart*-snippet because a condition can be attached to a snippet. This
is really a good idea. However, writing condition for a snippet
usually needs good elisp and Emacs knowledge, so it is strange to many
user.

Later I write YASnippet and persuade people to use it instead of
smart-snippet.el. However, some user still love smart-snippet because
it is smart. So I make YASnippet smart. Even smarter than
smart-snippet.el. :p

Consider this scenario: you are an old Emacs hacker. You like the
abbrev-way and set ``yas/trigger-key`` to ``(kbd "SPC")``. However,
you don't want ``if`` to be expanded as a snippet when you are typing
in a comment block or a string (e.g. in ``python-mode``). 

It's OK, just specify the condition for ``if`` to be ``(not
(python-in-string/comment))``. But how about ``while``, ``for``,
etc. ? Writing the same condition for all the snippets is just
boring. So YASnippet introduce a buffer local variable
``yas/buffer-local-condition``. You can set this variable to ``(not
(python-in-string/comment))`` in ``python-mode-hook``. There's no way
to do this in smart-snippet.el!

Then, what if you really want some snippet even in comment? This is
also possible! But let's stop telling the story and look at the rules:

* If ``yas/buffer-local-condition`` evaluate to nil, snippet won't be
  expanded.
* If it evaluate to the a cons cell where the ``car`` is the symbol
  ``require-snippet-condition`` and the ``cdr`` is a symbol (let's
  call it ``requirement``):

  * If the snippet has no condition, then it won't be expanded.
  * If the snippet has a condition but evaluate to nil or error
    occured during evaluation, it won't be expanded.
  * If the snippet has a condition that evaluate to non-nil (let's
    call it ``result``):

    * If ``requirement`` is ``t``, the snippet is ready to be
      expanded.
    * If ``requirement`` is ``eq`` to ``result``, the snippet is ready
      to be expanded.
    * Otherwise the snippet won't be expanded.

* If it evaluate to other non-nil value:

  * If the snippet has no condition, or has a condition that evaluate
    to non-nil, it is ready to be expanded.
  * Otherwise, it won't be expanded.

So set ``yas/buffer-local-condition`` like this

.. sourcecode:: common-lisp

  (add-hook 'python-mode-hook
            '(lambda ()
               (setq yas/buffer-local-condition
                     '(if (python-in-string/comment)
                          '(require-snippet-condition . force-in-comment)
                        t))))

And specify the condition for a snippet that you're going to expand in
comment to be evaluated to the symbol ``force-in-comment``. Then it
can be expanded as you expected, while other snippets like ``if``
still can't expanded in comment. 

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
