Journal Entry 2
===============

### Overall

I/O works on both ends! The server acts as an echo server now, I haven't tackled IPC yet. The client is basically done, other than a GUI for choosing server and port. Now the easy work is out of the way.

### Server

The Server can now read input from the user a line at a time, and echo it back to the user again. I'm using infinite recursion in a few places, but as it turns out that's alright in Haskell. As I understand it because there's no mutable state Haskell doesn't have the same concept of a "stack" from other languages, so there's no threat of a stack overflow or similar performance overhead associated with recursion in other languages. I haven't tackled getting multiple threads to talk to one another, I'm a little afraid of what IPC will look like in this language.

### Client

The client has a GUI now! Actually, it's just about finished. The client has a GUI that displays messages received from the server, and lets the user enter messages which are sent to the server. It's missing some finishing touches, but the main functionality is now tried and true. I thought this would be much more challenging, I was expecting reading from the socket and the GUI would require multi-threading or polling or some similar pain. I forgot GUI elements in Java are interrupt driven, so the entire problem is bypassed. Neat!
