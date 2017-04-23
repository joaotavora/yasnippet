# Submitting Bug Reports or Patches

As a GNU ELPA package, bugs or patches may be submitted to the main
Emacs bug list, bug-gnu-emacs@gnu.org.  Alternatively, you may use the
[Github issue tracker][issues].

Please read [Important note regarding bug reporting][bugnote].

# Contributing to Yasnippet

## Copyright Assignment

Yasnippet is part of GNU ELPA, so it falls under the same copyright
assignment policy as the rest of Emacs (see "Copyright Assignment" in
https://www.gnu.org/software/emacs/CONTRIBUTE).  A copyright assignment
for Emacs also covers Yasnippet.

## Commit message format

The commit message format roughly follows Emacs conventions.  There is
no separate Changelog file.

    Capitalize the first sentence, no period at the end

    Please make sure the summary line can be understood without having
    to lookup bug numbers.  It may be followed by a paragraph with a
    longer explanation.  The changelog style entry goes at the end of
    the message.
    * foo.el (a-function): Terse summary of per-function changes.  Use
    double spacing between sentences (set `sentence-end-double-space'
    to t).

For trivial changes, a message consisting of just the changelog entry
(e.g., `* foo.el (a-function): Fix docstring typo.`) is fine.

[bugnote]: https://github.com/joaotavora/yasnippet#important-note-regarding-bug-reporting
[issues]: https://github.com/joaotavora/yasnippet/issues
