[![Build Status](https://travis-ci.org/joaotavora/yasnippet.png)](https://travis-ci.org/joaotavora/yasnippet)

# Intro

**YASnippet** is a template system for Emacs. It allows you to
type an abbreviation and automatically expand it into function
templates. Bundled language templates include: C, C++, C#, Perl,
Python, Ruby, SQL, LaTeX, HTML, CSS and more. The snippet syntax
is inspired from [TextMate's][textmate-snippets] syntax, you can
even [import](#import) most TextMate templates to
YASnippet. Watch [a demo on YouTube][youtube-demo].

[textmate-snippets]: http://manual.macromates.com/en/snippets
[youtube-demo]: http://www.youtube.com/watch?v=ZCGmZK4V7Sg

# Installation

## Install the most recent version

Clone this repository somewhere

    $ cd ~/.emacs.d/plugins
    $ git clone --recursive https://github.com/joaotavora/yasnippet

Add the following in your `.emacs` file:

    (add-to-list 'load-path
                  "~/.emacs.d/plugins/yasnippet")
    (require 'yasnippet)
    (yas-global-mode 1)

Add your own snippets to `~/.emacs.d/snippets` by placing files there or invoking `yas-new-snippet`.

## Install with `package-install`

In a recent emacs `M-x list-packages` is the recommended way to list and install packages.
[MELPA][melpa] keeps a very recent snapshot of YASnippet, see http://melpa.org/#installing.

## Install with el-get

El-get is a nice way to get the most recent version, too. See
https://github.com/dimitri/el-get for instructions.

## Use `yas-minor-mode` on a per-buffer basis

To use YASnippet as a non-global minor mode, don't call
`yas-global-mode`; instead call `yas-reload-all` to load the snippet
tables and then call `yas-minor-mode` from the hooks of major-modes
where you want YASnippet enabled.

    (yas-reload-all)
    (add-hook 'prog-mode-hook #'yas-minor-mode)

# Where are the snippets?

<a name="import"></a>

YASnippet no longer bundles snippets directly, but it's very easy to
get some!

1. [yasnippet-snippets] - a snippet collection package maintained by
    [AndreaCrotti](https://github.com/AndreaCrotti).

    It can be installed with `M-x package-install RET
    yasnippet-snippets` if you have added MELPA to your package
    sources.

2. [yasmate] a tool which is dedicated to converting textmate bundles
    into yasnippet snippets.

    To use these snippets you have to run the tool first, so
    [see its doc][yasmate]), and then point the `yas-snippet-dirs`
    variable to the `.../yasmate/snippets` subdir.

    If you have a working ruby environment, you can probably get lucky
    directly with `rake convert-bundles`.

3.  [textmate-to-yas.el]

    This is another textmate bundle converting tool using Elisp
    instead of Ruby.

Naturally, you can point `yas-snippet-dirs` to good snippet collections out
there. If you have created snippets for a mode, or multiple modes,
consider creating a repository to host them, then tell users that it
should be added like this to `yas-snippet-dirs`:

    (setq yas-snippet-dirs
          '("~/.emacs.d/snippets"                 ;; personal snippets
            "/path/to/some/collection/"           ;; foo-mode and bar-mode snippet collection
            "/path/to/yasnippet/yasmate/snippets" ;; the yasmate collection
            ))

    (yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.

# Manual, issues etc

There's comprehensive [documentation][docs] on using and customising
YASnippet.

There's a [list of support issues][support-issues], with solutions to
common problems and practical snippet examples.

The [Github issue tracker][issues] is where most YASnippet-related
discussion happens.  Nevertheless, since YASnippet is a part of Emacs,
you may alternatively report bugs to the main Emacs bug list,
bug-gnu-emacs@gnu.org, putting "yasnippet" somewhere in the subject.

## Important note regarding bug reporting

Your bug reports are very valuable.

The most important thing when reporting bugs is making sure that we have
a way to reproduce the problem exactly like it happened to you.

To do this, we need to rule out interference from external factors
like other Emacs extensions or your own customisations.

Here's an example report that "sandboxes" an Emacs session just for
reproducing a bug.

```
$ emacs --version
Emacs 24.3
$ cd /tmp/
$ git clone https://github.com/joaotavora/yasnippet.git yasnippet-bug
$ cd yasnippet-bug
$ git log -1 --oneline
6053db0 Closes #527: Unbreak case where yas-fallback-behaviour is a list
$ HOME=$PWD emacs -L . # This "sandboxes" your emacs, melpa configuration, etc

(require 'yasnippet)
(yas-global-mode 1)

When I open a foo-mode file I don't see foo-mode under the "YASnippet" menu!
OR
When loading yasnippet I see "Error: failed to frobnicate"!
```

Using `emacs -Q` or temporarily moving your `.emacs` init file to the side
is another way to achieve good reproducibility.

Here's a
[another example](https://github.com/joaotavora/yasnippet/issues/318)
of a bug report. It has everything needed for a successful analysis
and speedy resolution.

Also, don't forget to state the Emacs version (use `M-x emacs-version`) and
the yasnippet version you are using (if using the latest from github,
do `git log -1` in the dir).

Any more info is welcome, but don't just paste a backtrace or an error
message string you got, unless we ask for it.

Finally, thank you very much for using YASnippet!

[docs]: http://joaotavora.github.io/yasnippet/
[issues]: https://github.com/joaotavora/yasnippet/issues
[support-issues]: https://github.com/joaotavora/yasnippet/issues?q=label%3Asupport
[googlecode tracker]: http://code.google.com/p/yasnippet/issues/list
[forum]: http://groups.google.com/group/smart-snippet
[melpa]: http://melpa.milkbox.net/
[yasmate]: http://github.com/joaotavora/yasmate
[textmate-to-yas.el]: https://github.com/mattfidler/textmate-to-yas.el
[yasnippet-snippets]: http://github.com/AndreaCrotti/yasnippet-snippets
