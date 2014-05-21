Journal Entry 4
===============

### Overall

All basic functionality complete. The client now accepts the host / portnumber arguments from the user, and has been verified to work on several platforms. The server has received only minor changes, mostly refactoring and fixing one memory leak.

### Server

The server was pretty solid as of journal entry 3, and the only significant improvement was fixing a memory leak with the channels code. For each client we kick off two threads (for reading and writing), each of which receives a duplicate of the chat channel. Silly me, nothing was ever reading the original chat channel, and so messages would pile up endlessly and take up space over time. There is now a thread devoted to clearing the main chat channel, so the problem is resolved.

### Client

The client has received some major upgrades and bug fixes. Of primary importance, it now receives the hostname and port from the user instead of hardcoded values. This involved making a second class that collects setup data and has a public method for extracting the host / port data. This is where I ran into the nastiest bug of the project, a block of code that performs differently on OSX and Linux, despite both machines sharing the same version of Java.

Host and port data does not become available to the program until the user enters it and presses "Connect". The event handler for the connect button checks if both entry boxes are filled out, and if they are sets the boolean "ready" to true. Therefore when we want to read the host and port data from setup in a blocking method we do:

	while( !ready );
	return new String [] {host, port};

Which was fine on Linux. On OSX however, the thread would never execute the while loop. Replacing the semi-colon with {} also didn't work. Finally, giving it a useless task made it perform as intended:

	while( !ready )
		System.out.print("");
	return new String [] {host, port};

So that's more than a little odd. Other than that progress was pretty smooth. Took a while to grapple Swing into submission, but I finally got the UI looking more or less like I want. Client can be considered done.
