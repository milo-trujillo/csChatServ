{-
	This Server acts like a stripped-down IRCd. It relays messages and
	handles usernames, but doesn't have channels or many other features.
-}

import System.IO			-- For handles
import Network.Socket		-- For sockets
import Control.Concurrent	-- For threads

-- Global vars for configuration
listen_port = 8888
max_connections = 30

-- We'll define messages as (Username, Text)
type Msg = (String, String)

main :: IO ()
main = do
	sock <- socket AF_INET Stream 0		-- Make new socket
	setSocketOption sock ReuseAddr 1	-- Make it a reusable listening socket
	bindSocket sock (SockAddrInet listen_port iNADDR_ANY) -- Bind to dev / port
	listen sock max_connections 		-- Set max connections
	listenLoop sock

-- This may cause a stack overflow given enough time, I need to look into
-- how Haskell optimizes recursion
listenLoop :: Socket -> IO ()
listenLoop servSock = do
	client <- accept servSock
	forkIO (handle client) -- Run 'handle' on a background thread
	listenLoop servSock

-- 'accept' returns a tuple of a socket and the address it's connected on
-- We're only interested in the socket right now
handle :: (Socket, SockAddr) -> IO ()
handle (sock, _) = do
	s <- socketToHandle sock ReadWriteMode -- convert the socket to a handle
	hSetBuffering s NoBuffering -- Write byte by byte over the network
	-- Now let's test IO
	hPutStr s "Your name: "
	name <- hGetLine s
	hPutStrLn s ("Hello " ++ name)
	hPutStrLn s "We're echoing you now."
	echo s
	hClose s -- This closes the handle _and_ the socket

echo :: Handle -> IO ()
echo sock = do
	msg <- hGetLine sock
	hPutStrLn sock msg
	echo sock
	
