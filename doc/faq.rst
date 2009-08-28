==========================
Frequently Asked Questions
==========================

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

First check the mode line to see if there's ``yas``. If not, then try
``M-x yas/minor-mode`` to manually turn on the minor mode and try to
expand the snippet again. If it works, then, you can add the following
code to your ``.emacs`` *before* loading YASnippet:

.. sourcecode:: lisp

  (add-hook 'the-major-mode-hook 'yas/minor-mode-on)

where ``the-major-mode`` is the major mode in which ``yas/minor-mode``
isn't enabled by default.

From YASnippet 0.6 you can also use the command ``M-x
yas/global-mode`` to turn on YASnippet automatically for *all* major
modes.

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
            #'(lambda ()
                (local-set-key [tab] 'yas/expand))) 

replace ``org-mode-hook`` with the major mode hook you are dealing
with (``C-h m`` to see what major mode you are in).

If this doesn't work, you can also try

.. sourcecode:: lisp

  (defun yas/advise-indent-function (function-symbol)
    (eval `(defadvice ,function-symbol (around yas/try-expand-first activate)
             ,(format
               "Try to expand a snippet before point, then call `%s' as usual"
               function-symbol)
             (let ((yas/fallback-behavior nil))
               (unless (and (interactive-p)
                            (yas/expand))
                 ad-do-it)))))

  (yas/advise-indent-function 'ruby-indent-line)

To *advise* the modes indentation function bound to TAB, (in this case
``ruby-indent-line``) to first try to run ``yas/expand``.

If The output of ``C-h k RET <tab>`` tells you that ``<tab>`` is
indeed bound to ``yas/expand`` but YASnippet still doesn't work, check
your configuration and you may also ask for help on the `discussion
group <http://groups.google.com/group/smart-snippet>`_. 

Don't forget to attach the information on what command is bound to TAB
as well as the mode information (Can be obtained by ``C-h m``).

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

