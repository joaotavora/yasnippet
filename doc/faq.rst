=========================
How to define a snippet ?
=========================

:Author: pluskid
:Contact: pluskid@gmail.com
:Date: 2008-03-20

Why there's an extra newline?
=============================

If you have a newline at the end of the snippet definition file, then
YASnippet will add a newline when you expanding a snippet. Please
don't add a newline at the end if you don't want it when you saving
the snippet file.

Note some editors will automatically add a newline for you. In Emacs,
if you set ``require-final-newline`` to ``t``, it will add the final
newline for you automatically.

Why TAB key doesn't expand a snippet?
=====================================

First check the mode line to see if there's ``yas``. If no, then try
``M-x yas/minor-mode-on`` to manually turn on ``yas/minor-mode`` and
try to expand the snippet again. If it works, then, you can add the
following code to your ``.emacs`` *before* loading YASnippet:

.. sourcecode:: lisp

  (setq yas/extra-mode-hooks '(the-major-mode))

where ``the-major-mode`` is the major mode in which ``yas/minor-mode``
isn't enabled by default.

If ``yas/minor-mode`` is on but the snippet still not expanded. Then
try to see what command is bound to the ``TAB`` key: press ``C-h k``
and then press ``TAB``. Emacs will show you the result. 

You'll see a buffer prompted by Emacs saying that ``TAB runs the
command ...``. Alternatively, you might see ``<tab> runs the command
...``, note the difference between ``TAB`` and ``<tab>`` where the
latter has priority. If you see ``<tab>`` bound to a command other
than ``yas/expand``, (e.g. in ``org-mode``) you can try the following
code to work around:

.. sourcecode:: lisp

  (add-hook 'org-mode-hook
            '(lambda ()
               (make-variable-buffer-local 'yas/trigger-key)
               (setq yas/trigger-key [tab])))

replace ``org-mode-hook`` with the major mode hook you are dealing
with (``C-h m`` to see what major mode you are in).

If it says ``TAB`` but YASnippet still doesn't work, check your
configuration and you may also ask for help on the `discussion group
<http://groups.google.com/group/smart-snippet>`_. Don't forget to
attach the information on what command is bound to TAB as well as the
mode information (Can be obtained by ``C-h m``).

How to define snippets with named by characters not supported by the filesystem?
================================================================================
For example, you want to define a snippet by the key ``<`` which is not a
valid character for filename on Windows. In this case, you may use
``yas/define`` to define the snippet. If you want to enjoy defining
snippets in a file, you can use the ``key`` property to specify the key of
the defined snippet explicitly.

Just name your snippet with an arbitrary valid filename, ``lt`` for
example. and specify ``<`` for the ``key`` property:

.. sourcecode:: text

  #key: <
  #name: <...></...>
  # --
  <${1:div}>$0</$1>

