Journal Entry 1
===============

### Overall

I have some basic sockets functionality working for both the client and the server. Developing the client and server simultaneously may not be the best idea flexibility-wise, but gives me a break from writing in Haskell when I need it.

### Server

The server currently listens on a port and forks to handle new clients. For each new client it displays a little message and closes the connection. Simple, pretty, and functional. I have concerns about how Haskell optimizes recursion, but I don't *think* the listening loop will make a buffer overflow like it would in C. Next up is reading from the socket, and then figuring out how to forward those messages to other threads. Whee!

### Client
	
The client isn't too complex right now. It has the address and port number hard-coded in, connects to the server, and prints whatever it reads to the screen. Simple again, but verifies that I won't have unforeseen surprises down the road like host to network byte order problems.
