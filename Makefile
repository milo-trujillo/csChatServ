# Set compilers
HC= ghc
JAVAC= javac

server:
	$(HC) Server.hs

client:
	$(JAVAC) Client.java

all:
	$(HC) Server.hs
	$(JAVAC) Client.java

clean:
	rm *.o *.hi *.class Server
