{-
	This Server acts like a stripped-down IRCd. It relays messages and
	handles usernames, but doesn't have channels or many other features.
-}

import System.IO
import Network.Socket

-- Port for the server to bind to
LISTEN_PORT = 8888
MAX_CONNECTIONS = 30

main :: IO ()
main = do
	-- Make new socket
	sock <- socket AF_INET Stream 0
	-- Set it up as a reusable listening socket
	setSocketOption sock ReuseAddr 1
	-- And bind it to all devices on our port
	bindSocket sock (SockAddrInet LISTEN_PORT iNADDR_ANY)
	-- Set the maximum number of connections 
	listen sock MAX_CONNECTIONS
	listenLoop sock

-- This may cause a stack overflow given enough time, I need to look into
-- how Haskell optimizes recursion
listenLoop :: Socket -> IO ()
listenLoop servSock = do
	client <- accept servSock
	handle client -- Need to make this multithreaded later
	listenLoop servSock

-- 'accept' returns a tuple of a socket and the address it's connected on
-- We're only interested in the socket right now
handle :: (Socket, SockAddr) -> IO ()
handle (sock, _) = do
	send sock "Testing\n"
	sClose sock -- Close connection	
