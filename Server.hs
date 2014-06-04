{-
	This Server acts like a stripped-down IRCd. It relays messages and
	handles usernames, but doesn't have channels or many other features.
-}

import System.IO			-- For handles
import System.Environment	-- For getArgs
import Network.Socket		-- For sockets
import Control.Concurrent	-- For threads and channels
import Control.Exception	-- For exceptions

-- Global vars for configuration
max_connections = 30
announce_name = "Server" -- Name used by all server announcements

-- We'll define messages as (Username, Text)
type Msg = (String, String)

-- Does some initial setup, then turns over to listenLoop
main :: IO ()
main = do
	args <- getArgs
	if length args == 1 && isInteger (args !! 0)
	then
		do
		let portno = (read (head args) :: Integer) -- Convert first arg to Int
		msgs <- newChan						-- Stores all messages
		sock <- socket AF_INET Stream 0		-- Make new socket
		setSocketOption sock ReuseAddr 1	-- Set reusable listening socket
		-- Bind the socket to the listen port on every interface
		bindSocket sock (SockAddrInet (fromIntegral portno) iNADDR_ANY)
		listen sock max_connections 		-- Set max connections
		forkIO (clearChannel msgs)			-- Prevent memory leak in msgs
		listenLoop sock msgs
	else
		putStrLn "Usage: Server <port number>"

-- Listens for a new client, then forks off a handler
listenLoop :: Socket -> Chan Msg -> IO ()
listenLoop servSock msgs = do
	client <- Network.Socket.accept servSock
	forkIO (handleClient client msgs) -- Run 'handle' on a background thread
	listenLoop servSock msgs

-- 'accept' returns a tuple of a socket and the address it's connected on
-- We're only interested in the socket right now
handleClient :: (Socket, SockAddr) -> Chan Msg -> IO ()
handleClient (sock, _) msgs = do
	s <- socketToHandle sock ReadWriteMode -- convert the socket to a handle
	hSetBuffering s NoBuffering -- Write byte by byte over the network
	hPutStr s "Your name: "
	name <- hGetLine s
	if (name == announce_name) then do
		hPutStrLn s "Sorry, that's a forbidden name"
		hClose s
	else do
		hPutStrLn s ("Hello, " ++ name)
		writeChan msgs (announce_name, name ++ " has entered the server")
		write <- dupChan msgs
		read <- dupChan msgs
		forkIO (readUser name s write)
		readMsgs s read -- Do _not_ fork this line! We don't want hClose to run!
		hClose s -- This closes the handle _and_ the socket

-- This reads from the user and appends new messages to the global queue
readUser :: String -> Handle -> Chan Msg -> IO ()
readUser user sock msgs = do
	eof <- hIsEOF sock -- Check if there's data to read before we try
	if eof then do
		hClose sock
		writeChan msgs (announce_name, user ++ " has left the server")
	else do
		msg <- hGetLine sock
		writeChan msgs (user, msg)
		readUser user sock msgs

-- This reads from the message queue and prints results over socket to user
readMsgs :: Handle -> Chan Msg -> IO ()
readMsgs sock msgs = do
	(user, msg) <- readChan msgs
	-- hIsOpen blocks, so we use exceptions instead
	handle (\(SomeException _) -> return ()) $ do
		hPutStrLn sock ("<" ++ user ++ "> " ++ msg)
		readMsgs sock msgs

-- This function constantly empties a channel, and never returns.
-- This prevents a memory leak from the original channel never getting emptied.
clearChannel :: Chan Msg -> IO ()
clearChannel chan = do
	(_, _) <- readChan chan
	clearChannel chan

-- Checks if a string contains only an integer
isInteger s = case reads s :: [(Integer, String)] of
	[(_, "")] -> True
	_         -> False
