=============================
Yet Another Snippet extension
=============================

:Author: pluskid
:Contact: pluskid@gmail.com
:Date: 2008-03-20

.. contents::

YASnippet is a re-design and re-write of my original extension
`smart-snippet`_. It is much cleaner and more powerful than
smart-snippet.

.. _smart-snippet: http://code.google.com/p/smart-snippet/

Getting Started
===============

For the busy or impatient people
--------------------------------

Watch the `screencast at YouTube
<http://www.youtube.com/watch?v=vOj7btx3ATg>`_ or download `the one
with a higher resolution
<http://yasnippet.googlecode.com/files/yasnippet.avi>`_.

For lazy poeple or beginners
----------------------------

.. _downloads page: http://code.google.com/p/yasnippet/downloads/list

1. Download the latest bundle release [1]_ from the `downloads page`_.
2. Create a directory ``~/emacs/plugins``.
3. Unpack the downloaded bundle to that directory.
4. Add the following code to your ``~/.emacs`` file:
   
   .. sourcecode:: common-lisp

     (add-to-list 'load-path
                   "~/emacs/plugins")
     (require 'yasnippet-bundle)

For you
-------

1. Download the latest YASnippet release package [2]_ from the
   `downloads page`_.
2. Unpack it to a directory and add that directory to your
   ``load-path``.
3. Add the following code to your ``~/.emacs`` file:

   .. sourcecode:: common-lisp

     (require 'yasnippet) ;; not yasnippet-bundle
     (yas/initialize)
     (yas/load-directory "/path/to/the/snippets/directory/")

4. You can inspect into the ``snippets`` directory for adding your own
   snippets. 
5. Detailed document can be found at the ``doc`` directory.

For geeks
---------

If you want to always follow the latest code. You can check out it
from the svn repository:

.. sourcecode:: bash

  svn checkout http://yasnippet.googlecode.com/svn/trunk/ yasnippet

However, I try to release a new version as soon as I made some changes
that will affect the normal use or added some new features. So there's
usually no need to follow the svn repository. Except that you might
find ``svn up`` is more convenient than downloading and unpacking the
release package. :D

How to contribute ?
===================

If you like YASnippet, you can recommendate it to your friends.

Issues
------

If you find a bug you can create a new issue at the `issue list
<http://code.google.com/p/yasnippet/issues/list>`_. Please describe
the problem as clear as possible. 

Suggestion, Feature Request
---------------------------

There's a `discussion group`_ for both smart-snippet and yasnippet. If
you have any suggesion, you can post to there and discuss with other
members. 

Especially, there's a `wish list`_ wiki page. I'll collect ideas from
the `discussion group`_ to the `wish list`_. So you might want to look
at the `wish list`_ before you post something.

Snippets
--------

YASnippet comes with some default snippet definitions. However, they
are far from complete. So I'm calling users to share their
snippets. If you have some good snippet definitions, you can post them
to the `discussion group`_. You can specify the ``contributor``
property of the snippet like:

.. sourcecode:: text

  #contributor : pluskid <pluskid@gmail.com>
  #name : __...__
  # --
  __${init}__

I'll incorporate (some of) them in the release if suitable. However,
if you have *many* snippets (especially when they need to be
maintained and updated constantly), it is not suitable to put them in
the YASnippet release package. A better way is to make your snippets
publicly available and tell me the URL. I'll try to keep a list of
them on the wiki page.

.. _discussion group: http://groups.google.com/group/smart-snippet
.. _wish list: http://code.google.com/p/yasnippet/wiki/WishList

Detailed Documentation
======================

* See `this page <define_snippet.html>`_ on how to define a snippet by
  yourself.
* Here's the `FAQ <faq.html>`_ page.
* Here's the `ChangeLog <changelog.html>`_.

.. [1] They usually named like ``yasnippet-bundle-x.y.z.el.tgz`` where
   ``x.y.z`` is the version number.
.. [2] They usually named like ``yasnippet.x.y.z.tar.bz2``.
