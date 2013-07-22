Small & useful Emacs functions.


Smart Compile
-------------

I like to have a key bound to compile, and this key should do the right thing:
if there is a Makefile in the current dir or upward, use `make`, else use
`rake`. Or, if I\'m in an elisp file, just run elk tests. And saving the current
buffer before doing anything avoids most `yes-or-no-p` questions.


Flymake Lua
-----------

Flymake for Lua...


Jekyll
------

I use this to quickly create new posts, automatically using the current date.


Smart Tab
---------

Note : this is the first and outdated version. Please have a look
[here](https://github.com/genehack/smart-tab) instead.

I like to have word completion near the homeline, and TAB feels natural to me.
But I like to have automatic indentation bound to TAB as well. Fortunately,
most of the time it is easy to decide which one I need. Smart Tab does that for
me.
