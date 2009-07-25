=========================
How to define a snippet ?
=========================

:Author: pluskid, joaotavora
:Contact: pluskid@gmail.com
:Date: 2009-07-24

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
ignored. Here's a list of currently supported meta data:

.. image:: images/group.png
   :align: right

* ``name``: The name of the snippet. This is a one-line description of
  the snippet. It will be displayed in the menu. So it's a good idea
  to select a descriptive name fo a snippet -- especially
  distinguishable among similar snippets.
* ``contributor``: The contributor of the snippet.
* ``condition``: The condition of the snippet. This is a piece of
  elisp code. If a snippet has a condition, then it will only be
  expanded when the condition code evaluate to some non-nil value.
* ``key``: The key to expand the snippet. Sometimes the key of a
  snippet is non-ASCII or not valid filename (e.g. contains
  ``/``). One can then define the ``key`` property which will
  overwrite the filename as the key to expand the snippet.
* ``group``: The snippets for a mode can be grouped. Grouped snippets
  will be grouped in sub-menu. This is useful if one has too many
  snippets for a mode which will make the menu too long. ``group``
  property only affect menu construction (See `The Menu`_). Refer to
  the snippets for ``ruby-mode`` for examples. Group can also be
  nested, e.g.  ``control structure.loops`` tells that the snippet is
  under the ``loops`` group which is under the ``control structure``
  group.

Quickly finding/defining snippets
---------------------------------

From version 0.6 upwards there are two ways you can quickly find a
snippet file. Once you find this file it will be set to
``snippet-mode`` (see ahead)

* ``M-x yas/find-snippets``

  Lets you find the snippet file in the directory the snippet was
  loaded from (if it exists) like ``find-file-other-window``. 

* ``M-x yas/visit-snippet-file``

  Prompts you for possible snippet expansions like
  ``yas/insert-snippet``, but instead of expanding it, takes you
  directly to the snippet definition's file, if it exists.


Using the ``snippet-mode`` major mode
-------------------------------------

From version 0.6 upwards there is a major mode ``snippet-mode`` to
edit snippets. You can set the buffer to this mode with ``M-x
snippet-mode``. It provides reasonably useful syntax highlighting.

Two commands are defined in this mode:

* ``M-x yas/load-snippet-buffer``                                     
                                                                
    When editing a snippet, this loads the snippet into the correct
    mode and menu. Bound to ``C-c C-c`` by default while in
    ``snippet-mode``.
                                                                
* ``M-x yas/tryout-snippet``                                          
                                                                
    When editing a snippet, this opens a new empty buffer, sets it to
    the appropriate major mode and inserts the snippet there, so you
    can see what it looks like. This is bound to ``C-c C-t`` while in
    ``snippet-mode``.
    
There are also snippets for making snippets: ``vars``, ``field`` and
``mirror``.

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

yas/compile-bundle
~~~~~~~~~~~~~~~~~~

``yas/compile-bundle`` can be used to parse the snippets from a
directory hierarchy and translate them into the elisp form. The
translated code is faster to load. Further more, the generated bundle
is a stand-alone file not depending on ``yasnippet.el``. The released
bundles of YASnippet are all generated this way.

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

yas/define
~~~~~~~~~~

The basic syntax for ``yas/define`` is

.. sourcecode:: common-lisp

  (yas/define mode key template &optional name condition group)

This is only a syntax sugar for

.. sourcecode:: common-lisp

  (yas/define-snippets mode
                       (list (list key template name condition group)))

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

Multiple snippet with the same key
----------------------------------

There can be multiple snippet bind to the same key. If you define a
snippet with a key that is already used, you'll overwrite the original
snippet definition. However, you can add a different *postfix* to the
key.

In general, the *extension* (consider a file name) is *ignored* when
defining a snippet. So ``def``, ``def.1`` and ``def.mine`` will all be
valid candidates when the key is ``def``.

When there are multiple candidates, YASnippet will let you select
one. The UI for selecting multiple candidate can be
customized. There're two variable related:

From version 0.6 of YASnippet this has changed significantly. A new
customization variable, called ``yas/prompt-functions`` defines your
preferred method of being prompted for snippets.

You can customize it with ``M-x customize-variable RET
yas/prompt-functions RET``. Alternatively you can put in your
emacs-file:

.. sourcecode:: common-lisp
   
   (setq yas/prompt-functions '(yas/x-prompt yas/dropdown-prompt))

Currently there are some alternatives solution with YASnippet.

.. image:: images/popup-menu.png
   :align: right

Use the X window system
~~~~~~~~~~~~~~~~~~~~~~~

The function ``yas/x-prompt`` can be used to show a popup menu for you
to select. This menu will be part of you native window system widget,
which means:

* It usually looks beautiful. E.g. when you compile Emacs with gtk
  support, this menu will be rendered with your gtk theme.
* Emacs have little control over it. E.g. you can't use ``C-n``,
  ``C-p`` to navigate.
* This function can't be used when in a terminal.

Use built-in Emacs selection methods
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can use functions ``yas/completing-prompt`` for the classic emacs
completion method or ``yas/ido-prompt`` for a much nicer looking
method. The best way is to try it. This works in a terminal.

.. image:: images/idrop-menu.png
   :align: center

Use ``dropdown-menu.el``
~~~~~~~~~~~~~~~~~~~~~~~~

The function ``yas/dropdown-prompt`` can also be placed in the
``yas/prompt-functions`` list.

.. image:: images/dropdown-menu.png
   :align: right

Originally, only the above two function is available in
YASnippet. They are difficult to use -- especially in a
terminal. Until later Jaeyoun Chung show me his ``dropdown-menu.el``,
I say wow! It's wonderful!

* It works in both window system and terminal.
* It is customizable, you can use ``C-n``, ``C-p`` to navigate, ``q``
  to quite and even press ``6`` as a shortcut to select the 6th
  candidate.

So I added ``yas/dropdown-list-popup-for-template`` to support
``dropdown-list.el``. And upload ``dropdown-list.el`` to YASnippet
hompage for an optional download (since Jaeyoun didn't provide a URL).

Then finally, in 0.4.0, I included a copy of the content of
``dropdown-list.el`` [1]_ in ``yasnippet.el`` and made it the default
way for selecting multiple candidates.

However, the original functions are still there, you can still use this

.. sourcecode:: common-lisp

  (setq yas/window-system-popup-function
        'yas/x-popup-menu-for-template)

if you prefer a *modern* UI. :)

The Trigger Key
---------------

YASnippet is implemented as a minor-mode (``yas/minor-mode``). The
trigger key ``yas/trigger-key`` is defined in ``yas/minor-mode-map``
to call ``yas/expand`` to try to expand a snippet.

The Minor Mode
~~~~~~~~~~~~~~

.. image:: images/minor-mode-indicator.png
   :align: left

When ``yas/minor-mode`` is enabled, the trigger key will take
effect. The default key is ``(kbd "TAB")``, however, you can freely
set it to some other key. 

In version 0.5, YASnippet add a hook to
``after-change-major-mode-hook`` to enable ``yas/minor-mode`` [2]_ in
every buffer. This works fine for most modes, however, some mode
doesn't follow the Emacs convention and doens't call this hook. You
can either explicitly hook for those mode or just add it to
``yas/extra-mode-hooks`` to let YASnippet do it for you:

.. sourcecode:: common-lisp

  (require 'yasnippet)
  (add-to-list 'yas/extra-mode-hooks
               'ruby-mode-hook)
  (yas/initialize)

Note that **should** be put after ``(require 'yasnippet)`` and before
``(yas/initialize)``. Further more, you may report it to me, I'll add
that to the default value.

In version 0.6, just use ``yas/global-mode`` to enable YASnippet in
all major modes. Or put ``yas/minor-mode-on`` in that modes hook. See
the `FAQ <faq.html>`_.

The Fallback
~~~~~~~~~~~~

If ``yas/expand`` failed to find any suitable snippet to expand, it
will disable the minor mode temporarily and find if there's any other
command bind the ``yas/trigger-key``. If found, the command will be
called. Usually this works very well -- when there's a snippet, expand
it, otherwise, call whatever command originally bind to the trigger
key.

However, you can change this behavior by customizing the
``yas/fallback-behavior`` variable. If you set this variable to
``'return-nil``, it will return ``nil`` instead of trying to call the
*original* command when no snippet is found. This is useful when you
would like YASnippet to work with other extensions,
e.g. ``hippie-expand``. I'm also glad to tell you that integration
with ``hippie-expand`` is already included in YASnippet.

Integration with ``hippie-expand``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To integrate with ``hippie-expand``, just put
``yas/hippie-try-expand`` in
``hippie-expand-try-functions-list``. Personally I would like to put
in front of the list, but it can be put anywhere you prefer.

Other way to select a snippet
-----------------------------

When you use the trigger key (so ``yas/expand``) to expand a snippet,
the key for the snippet is deleted before the template for the snippet
is inserted. 

However, there're other ways to insert a snippet.

``yas/insert-snippet``
~~~~~~~~~~~~~~~~~~~~~~

The command ``M-x yas/insert-snippet`` lets you insert snippets at
point *for you current major mode*. It prompts you for the snippet
key first, and then for a snippet template if more than one template
exists for the same key.

The list presented contains the snippets that can be inserted at
point, according to the condition system. If you want to see all
applicable snippets for the major mode, prefix this command with
``C-u``.

The prompting methods used are again controlled by
``yas/prompt-functions``.

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

Expanding From Elisp Code
~~~~~~~~~~~~~~~~~~~~~~~~~

Sometimes you might want to expand a snippet directly by calling a
functin from elisp code. You should call ``yas/expand-snippet``
instead of ``yas/expand`` in this case.

As with expanding from the menubar, condition system and multiple
candidates won't exists here. In fact, expanding from menubar has the
same effect of evaluating the follow code:

.. sourcecode:: common-lisp

  (yas/expand-snippet (point) (point) template)

Where ``template`` is the template of a snippet. It is never required
to belong to any snippet -- you can even make up it on the fly. The
1st and 2nd parameter defines the region to be deleted after YASnippet
inserted the template. It is used by ``yas/expand`` to indicate the
region of the key. There's usually no need to delete any region when
we are expanding a snippet from elisp code, so passing two ``(point)``
is fine. Note only ``(point)`` will be fine because the 1st parameter
also indicate where to insert and expand the ``template``.

The Syntax of the Template
==========================

The syntax of the snippet template is simple but powerful, very
similar to TextMate's.

Plain Text
----------

Arbitrary text can be included as the content of a template. They are
usually interpreted as plain text, except ``$`` and `````. You need to
use ``\`` to escape them: ``\$`` and ``\```. The ``\`` itself may also
needed to be escaped as ``\\`` sometimes.

Embedded elisp code
-------------------

Elisp code can be embedded inside the template. They are written
inside back-quotes (`````):

They are evaluated when the snippet is being expanded. The evaluation
is done in the same buffer as the snippet being expanded. Here's an
example for ``c-mode`` to calculate the header file guard dynamically:

.. sourcecode:: text

  #ifndef ${1:_`(upcase (file-name-nondirectory (file-name-sans-extension (buffer-file-name))))`_H_}
  #define $1
  
  $0
  
  #endif /* $1 */

From version 0.6.0, snippets expansions are run with some special
emacs-lisp variables bound. One of this is ``yas/selected-text``. You
can therefore define a snippet like:

.. sourcecode:: text

   for ($1;$2;$3) {
     `yas/selected-text`$0
   }

to "wrap" the selected region inside your recently inserted
snippet. Alternatively, you can also customize the variable
``yas/wrap-around-region`` to ``t`` which will do this automatically.

Tab stop fields
---------------

Tab stops are fields that you can navigate back and forth by ``TAB``
and ``S-TAB`` [3]_. They are written by ``$`` followed with a
number. ``$0`` has the special meaning of the *exit point* of a
snippet. That is the last place to go when you've traveled all the
fields. Here's a typical example:

.. sourcecode:: text

  <div$1>
      $0
  </div>

Placeholder fields
------------------

Tab stops can have default values -- a.k.a placeholders. The syntax is
like this:

.. sourcecode:: text

  ${N:default value}

They acts as the default value for a tab stop. But when you firstly
type at a tab stop, the default value will be replaced by your
typing. The number can be omitted if you don't want to create
`mirrors`_ or `transformations`_ for this field.

.. _mirrors:

Mirrors
-------

We refer the tab stops with placeholders as a *field*. A field can have
mirrors. Its mirrors will get updated when you change the text of a
field. Here's an example:

.. sourcecode:: text

  \begin{${1:enumerate}}
      $0
  \end{$1}

When you type ``"document"`` at ``${1:enumerate}``, the word
``"document"`` will also be inserted at ``\end{$1}``. The best
explanation is to see the screencast(`YouTube
<http://www.youtube.com/watch?v=vOj7btx3ATg>`_ or `avi video
<http://yasnippet.googlecode.com/files/yasnippet.avi>`_).

The tab stops with the same number to the field act as its mirrors. If
none of the tab stops has an initial value, the first one is selected
as the field and others mirrors.

.. _transformations:

Mirrors with transformations
----------------------------

If the default value of a field starts with ``$``, then it is interpreted
as the transformation code instead of default value. A transformation
is some arbitrary elisp code that will get evaluated in an environment
when the variable text is bind to the inputted text of the
field. Here's an example for Objective-C:

.. sourcecode:: text

  - (${1:id})${2:foo}
  {
      return $2;
  }
  
  - (void)set${2:$(capitalize text)}:($1)aValue
  {
      [$2 autorelease];
      $2 = [aValue retain];
  }
  $0

Look at ``${2:$(capitalize text)}``, it is a transformation instead of
a placeholder. The actual placeholder is at the first line:
``${2:foo}``. When you type text in ``${2:foo}``, the transformation
will be evaluated and the result will be placed there as the
transformated text. So in this example, if you type baz in the field,
the transformed text will be Baz. This example is also available in
the screencast.

Another example is for ``rst-mode``. In reStructuredText, the document
title can be some text surrounded by "===" below and above. The "==="
should be at least as long as the text. So

.. sourcecode:: text

  =====
  Title
  =====

is a valid title but

.. sourcecode:: text

  ===
  Title
  ===

is not. Here's an snippet for rst title: 

.. sourcecode:: text

  ${1:$(make-string (string-width text) ?\=)}
  ${1:Title}
  ${1:$(make-string (string-width text) ?\=)}
  
  $0

.. [1] With some minor change, mainly for fixing some trivial bugs.
.. [2] This is done when you call ``yas/initialize``.
.. [3] Of course, this can be customized.

Fields with transformations
---------------------------

From version 0.6 on, you can also have lisp transformation inside
fields. These work mostly mirror transformations but are evaluated
when you first enter the field, after each change you make to the
field and also just before you exit the field.

The syntax is also a tiny bit different, so that the parser can
distinguish between fields and mirrors. In the following example

.. sourcecode:: text

  #define "${1:mydefine$(upcase yas/text)}"

``mydefine`` gets automatically upcased to ``MYDEFINE`` once you enter
the field. As you type text, it gets filtered through the
transformation every time.

Note that this is differentiated from a mirror with a transformation
by the existance of extra text between the ``:`` and the
transformation's ``$``. If you don't want this extra-text, you can use
two ``$``'s instead.

.. sourcecode:: text

  #define "${1:$$(upcase yas/text)}"

Please note that as soon as a transformation takes place, it changes
the value of the field and sets it its internal modification state to
``true``. As a consequence, the auto-deletion behaviour of normal
fields does not take place. This is by design.

Choosing fields value from a list
---------------------------------

As mentioned, the field transformation is invoked just after you enter
the field, and with some useful variables bound, notably
``yas/field-modified-p`` and ``yas/moving-away-p``. Because of this
feature you can place a transformation in the primary field that lets
you select default values for it. 

The ``yas/choose-value`` does this work for you. For example:
 
.. sourcecode:: text

   <div align="${2:$$(yas/choose-value '("right" "center" "left"))}">
     $0
   </div>
  
See the definition of ``yas/choose-value`` to see how it was written
using the two variables. Also check out ``yas/verify-value`` for
another neat trick.

Nested placeholder fields
-------------------------

From version 0.6 on, you can also have nested placeholders of the type:

.. sourcecode:: text

   <div${1: id="${2:some_id}"}>$0</div>

This allows you to choose if you want to give this ``div`` an ``id``
attribute. If you tab forward after expanding it will let you change
"some_id" to whatever you like. Alternatively, you can just press
``C-d`` (which executes ``yas/skip-and-clear-or-delete-char``) and go
straight to the exit marker. 

By the way, ``C-d`` will only clear the field if you cursor is at the
beginning of the field *and* it hasn't been changed yet. Otherwise, it
performs the normal Emacs ``delete-char`` command.

Indenting
---------

Many people miss the indenting feature of smart-snippet: when you
place a ``$>`` in your snippet, an ``(indent-according-to-mode)`` will
be executed there to indent the line. So you'll not need to hard-code
the indenting in the snippet template, and it will be very convenient
when you need to work with several different project where coding
styles are different.

The reason why this feature wasn't added to YASnippet until after
0.5.6 is that it doesn't work well for all modes. In some cases
(e.g. python-mode), calling ``indent-according-to-mode`` will break
the overlays created by YASnippet.

However, since many people asked for this feature, I finally added
this to YASnippet. Here's an example of the usage:

.. sourcecode:: text

  for (${int i = 0}; ${i < 10}; ${++i})
  {$>
  $0$>
  }$>

In 0.6.0 You should **not** need to use this feature although it's
supported for backward compatibility. Just set ``yas/indent-line`` to
``'auto``.

