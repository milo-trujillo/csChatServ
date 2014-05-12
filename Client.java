/**
 *		Client is a chat client for the accompanying server.
 *		It's mostly just a GUI frontend to some sockets.
 */

// For networking
import java.net.*;
import java.io.*;
import java.util.*;

// For GUI
import javax.swing.*;
import java.awt.*;

public class Client
{
	public static void main( String [] args ) throws IOException
	{
		// For now we'll hardcode in the address and port number
		// but eventually we'll want to get them from the user
		InetAddress host = InetAddress.getByName("localhost");
		int port = 8888;
		try
		{
			Socket sock = new Socket(host, port);
			InputStream in = sock.getInputStream();
			// The server writes a byte at a time, so we'll read byte by byte
			byte[] buffer = new byte[1024];
			int length;
			while( (length = in.read(buffer)) != -1 ) // -1 == EOF
			{
				System.out.write(buffer, 0, length);
			}
		}
		catch( IOException e )
		{
			System.out.println("Something went wrong: " + e.getMessage());
		}
	}
}
