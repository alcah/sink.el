<!-- readme.md -- readme for sink.el -->

# Introduction
Sink.el provides a global minor mode allowing Emacs to receive and respond to
messages from the Plan9 plumber via its ports interface along with a small set
of utilities for handling messages. Out of the box, messages are received on the
edit port and display the plumbed file to the user in an intelligent way. The
standard address format is supported, for example, plumbing "notes.org:20" will
open the file notes.org at line 20.

sink-mode is currently intended as basic library to support further user
customisation via elisp.

# Use
1. Install [Plan9Port](https://9fans.github.io/plan9port/), and ensure that its
   programs are in $PATH
2. Start the plan9 plumber by running `plumber`
2. M-x `sink-mode`
3. Try plumbing a file name from the shell with `plumb <filename>` or from Emacs
   with `sink-plumb-dwim`
4. See source for implementation details and plumb(7) for information
   about defining rules and sending messages

# User Variables and Functions
* `sink-mode` - activate/deactivate sink-mode
* `sink-port-alist` - Alist of port-name . function pairs. While sink-mode is
  active function will be called with a parsed plumbing message whenever a
  message is received on the associated port. This is the main point of
  customisation, see inbuilt help for further details.
* `sink-plumb-dwim` - utility function for plumbing messages from emacs.

# Customisation
User customisation is intended through the `sink-port-alist`
variable. Handler functions listed in this variable are called with a
plumbmsg structure as their only argument whenever a message is
received on the associated port. Ports are named without any leading
directories or namespaces; "edit", not "plumb/edit".

A plumbmsg's components can be accessed through the following functions:

* `plumbmsg-src` - source of message - typically the sending program.
* `plumbmsg-dst` - destination (port) of message.
* `plumbmsg-wdir` - working directory.
* `plumbmsg-type` - data type. Usually "text", see the plumb documentation for others.
* `plumbmsg-attr` - alist of attribute-value pairs. Attribute values are either strings or nil.
* `plumbmsg-ndata` - number of bytes in data segment.
* `plumbmsg-data` - message data.

In addition, the function `plumbmsg-attr-addr` is provided to return the
value of the addr attribute as an integer (or nil), since it's often used.

# Contributing
 I use sink heavily as part of my own workflow with a lot of additional
functionality. However that also involves custom plumbing rules which are hard
to distribute. If others are interested I'm very open to PRs to improve the
default behaviour and build sink into more of a complete package.

# Known Issues
Since plumbing messages are received through asynchronous processes there's no
guarantee any function call will receive the complete message. I use this daily
and it's never been an issue, but it theoretically may be if the message is
particularly large or arrives under heavy load.

# Non-uses
* Sink does not implement the 9p protocol in Elisp.  All communication
with the plumber is via plan9port's 9p utility.
* Sink does not provide any facilities for running GNUEmacs on
Plan9/9Front.
