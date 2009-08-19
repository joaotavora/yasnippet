================
Writing snippets
================

:Author: pluskid, joaotavora
:Contact: pluskid@gmail.com
:Date: 2009-08-18

.. contents::

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
