<!-- readme.md -- readme for sink.el -->

# Introduction
Sink.el provides a global minor mode allowing Emacs to receive and
respond to messages from the Plan9 plumber via its ports interface.
Users can easily define functions to handle messages in any way they
choose. Out of the box, messages are received on the edit port and display
the plumbed file to the user in an intelligent way. The standard
address format is supported, for example, plumbing "notes.org:20" will
open the file notes.org at line 20.

# Use
1. Install [Plan9Port](https://9fans.github.io/plan9port/), and ensure that its programs are in $PATH
2. Start the plan9 plumber by running `plumber`
2. M-x `sink-mode`
3. Try plumbing a file name from the shell with `plumb <filename>`
4. See source for implementation details and plumb(7) for information
   about defining rules and sending messages

# User Variables
* `sink-port-alist` - Alist of port-name . function pairs. Sink-mode
  inspects this variable upon starting and opens each port-name for
  reading. Any messages received are passed to the associated
  function. The default value is `'(("edit"
  . sink-default-edit-handler))`
* `sink-use-frames` - Used by the default edit handler to determine
  whether to open buffers in a new frame or window. Defaults to t.

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

In addition, the function `sink-pmsg-addr` is provided to return the
value of the addr attribute as an integer (or nil), since it's often used.

# Known Issues
Since plumbing messages are received through asynchronous processes
there's no guarantee any function call will receive the complete
message. In testing this hasn't been an issue, but it may arise if the
message is particularly large or arrives under heavy load. This'll be
my priority if sink-mode because a part of my (or your) workflow.

# Non-uses
* Sink does not implement the 9p protocol in Elisp.  All communication
with the plumber is via plan9port's 9p utility.
* Sink cannot plumb messages. This is an obvious avenue for further
work, but it's outside the strict scope of this minor-mode and better
handled by a separate package.
* Sink does not provide any facilities for running GNUEmacs on
Plan9/9Front.  This would be a truly mammoth task and of questionable
value since Emacs will never be at home on that system.
