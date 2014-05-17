Journal Entry 3
===============

### Overall

*Server works!* Inter-thread communication is flawless. Messages now get relayed between users. Server accepts a port number to listen on as a command line argument. Client currently unchanged, but updates are in progress.

### Server

ITC (As opposed to IPC) is very, very clean in Haskell. It took a while to figure out, but after a bit of testing channels became trivial to work with. Channels are basically queues implemented as thread-safe linked lists. They have three basic interactions: Push an item on the channel, shift an item off the channel, and duplicate the channel. Duplicates of a channel start empty, but whenever an item is pushed on a channel it is also pushed on all duplicate channels. Tada! Getting the chat server working was as trivial as giving two duplicate channels to each client handler - one to read from to display messages to the user, and one to write to whenever the user sends some text.

Getting the port number as a command line argument was a much more complicated task. Haskell does many things nicely, but type casting and string handling are not its strongest suites. Converting the command line argument to an int wasn't that bad now that I understand how it works. But getting the PortNumber constructor to take it as an argument was just a nightmare. Spent most of the day on that problem, finally found that the constructor I was using was expecting a strange 16-byte word encoding. I bypassed the constructor and did some implicit typecasting later on. It works, but isn't as readable as I'd like.

### Client

I'm working on giving the client a setup window, so you can specify the port number and host IP at runtime. This is essential. Right now I'm having some disagreements with Swing and can't get my UI elements to go where I want them, but I'm sure I'll figure it out. Once I do I'll commit the changes to the repo.

I think I need to drop the colored usernames feature. I've looked into what resources are available in Swing, and it looks like the most straight-forward way of only coloring parts of every line is to use HTML inside a text element. *shudder* That sounds painful, and a security nightmare since we're reading text from an insecure socket, and really not worth it.
