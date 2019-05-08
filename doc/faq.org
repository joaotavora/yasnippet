#+SETUPFILE: org-setup.inc

#+TITLE: Frequently Asked Questions

-  *Note*: In addition to the questions and answers presented here,
  you might also with to visit the list of [[https://github.com/joaotavora/yasnippet/issues?q=label%3Asupport][solved support issues]] in
  the Github issue tracker.  It might be more up-to-date than this
  list.

* Why are my snippet abbrev keys triggering when I don't want them too?
Expansion of abbrev keys is controlled by [[sym:yas-key-syntaxes][=yas-key-syntaxes=]].  Try
removing entries which correspond to the abbrev key character syntax.
For example, if you have a snippet with abbrev key "bar", that you
don't want to trigger when point follows the text =foo_bar=, remove
the ="w"= entry (since "bar" has only word syntax characters).

* Why aren't my snippet abbrev keys triggering when I want them too?
See previous question, but in reverse.

* Why is there an extra newline?

If there is a newline at the end of a snippet definition file,
YASnippet will add a newline when expanding that snippet. When editing
or saving a snippet file, please be careful not to accidentally add a
terminal newline.

Note that some editors will automatically add a newline for you. In
Emacs, if you set =require-final-newline= to =t=, it will add the
final newline automatically.

* Why doesn't TAB navigation work with flyspell

This is [[https://debbugs.gnu.org/26672][Emacs Bug#26672]], so you should upgrade to version 25.3 or
better.  Otherwise, a workaround is to inhibit flyspell overlays while
the snippet is active:

#+BEGIN_SRC emacs-lisp
  (add-hook 'flyspell-incorrect-hook
            #'(lambda (&rest _)
                (and yas-active-field-overlay
                     (overlay-buffer yas-active-field-overlay))))
#+END_SRC

* How do I use alternative keys, i.e. not TAB?

Edit the keymaps [[sym:yas-minor-mode-map][=yas-minor-mode-map=]] and [[sym:yas-keymap][=yas-keymap=]] as you would
any other keymap, but use [[sym:yas-filtered-definition][=yas-filtered-definition=]] on the definition
if you want to respect [[sym:yas-keymap-disable-hook][=yas-keymap-disable-hook=]]:

#+begin_src emacs-lisp :exports code
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (define-key yas-minor-mode-map (kbd "<the new key>") yas-maybe-expand)

  ;;keys for navigation
  (define-key yas-keymap [(tab)]       nil)
  (define-key yas-keymap (kbd "TAB")   nil)
  (define-key yas-keymap [(shift tab)] nil)
  (define-key yas-keymap [backtab]     nil)
  (define-key yas-keymap (kbd "<new-next-field-key>")
    (yas-filtered-definition 'yas-next-field-or-maybe-expand))
  (define-key yas-keymap (kbd "<new-prev-field-key>")
    (yas-filtered-definition 'yas-prev-field))
#+end_src

* How do I define an abbrev key containing characters not supported by the filesystem?

-  *Note*: This question applies if you're still defining snippets
   whose key /is/ the filename. This is behavior still provided by
   version 0.6 for backward compatibilty, but is somewhat
   deprecated...

For example, you want to define a snippet by the key =<= which is not a
valid character for filename on Windows. This means you can't use the
filename as a trigger key in this case.

You should rather use the =# key:= directive to specify the key of the
defined snippet explicitly and name your snippet with an arbitrary valid
filename, =lt.YASnippet= for example, using =<= for the =# key:=
directive:

#+BEGIN_SRC snippet
  # key: <
  # name: <...></...>
  # --
  <${1:div}>$0</$1>
#+END_SRC
