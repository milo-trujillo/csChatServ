/**
 *		Client is a chat client for the accompanying server.
 *		It gets the hostname and port to connect to from the user, and then acts
 *		as a GUI frontend to some sockets.
 */

// For networking
import java.net.*;
import java.io.*;
import java.util.*;

// For GUI
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class Client extends JFrame implements ActionListener
{
	// IO elements that need to be accessed in several methods
	private static JTextArea display;
	private static JTextField input;
	private static PrintWriter outs;

	// This is the window where the user enters port number and hostname
	private static ClientSetup setup;

	// This is true when we're ready to read / write data to the network
	private static boolean initialized = false;

	// Sets up the GUI elements, called once during setup
	public Client(String host)
	{
		setTitle("Chat Client");
		setSize(300, 220);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		display = new JTextArea("Connecting to host: " + host);
		JScrollPane scroll = new JScrollPane(display);
		input = new JTextField();
		input.addActionListener(this);

		display.setEditable(false);

		this.setLayout(new BorderLayout());
		this.add(scroll, BorderLayout.CENTER); // Center makes it use all space
		this.add(input, BorderLayout.SOUTH);
	}

	// This is called whenever the user presses return in the input box
	public void actionPerformed(ActionEvent event)
	{
		Object source = event.getSource();
		if( !source.equals(input) || initialized == false )
			return;
		String text = input.getText(); 	// Get the text user has entered
		input.setText(new String("")); 	// Clear text box
		outs.println(text);				// Send text across the network
	}

	// This is called to connect to a server and read from the socket
	private static void connectToServer( InetAddress host, int port )
	{
		try
		{
			// Open the socket and set up some handles for it
			Socket sock = new Socket(host, port);
			InputStream in = sock.getInputStream();
			OutputStream out = sock.getOutputStream();
			InputStreamReader ins = new InputStreamReader(in);
			outs = new PrintWriter(out, true);

			// Now ready to read and write data
			initialized = true;

			// Read from socket and print to GUI, until socket closes
			while(true)
			{
				try
				{
					int recv = ins.read();
					if( recv == -1 ) // If EOF
						break;
					String line = Character.toString((char)recv);
					display.append(line);
				}
				catch(IOException e)
				{
					display.append("Error reading from socket: " + 
						e.getMessage());
				}
			}

			// Now clean up
			display.append("--- Server Closed Connection ---\n");
			initialized = false;
			sock.close();
		}
		catch( IOException e )
		{
			display.append("Something went wrong in setup: " + 
				e.getMessage());
		}
	}

	public static void main( String [] args ) throws IOException
	{
		// Default values, we will not create 
		InetAddress host = null;
		int port = 0;

		// Set up UI for getting the host / port number
		ClientSetup setup = new ClientSetup();
		setup.setVisible(true);

		// Get valid input from user
		boolean validHost = false;
		while( !validHost )
		{
			String [] address = setup.getAddress();
			try
			{
				host = InetAddress.getByName(address[0]);
				port = Integer.parseInt(address[1]);
				if( port != 0 )
				{
					validHost = true;
					setup.dispose();	
				}
			}
			catch(Exception e) // Something went wrong, their input is botched
			{
				JOptionPane.showMessageDialog(null,
					"Please provide a valid hostname and port", 
					"Invalid Input",
					JOptionPane.ERROR_MESSAGE);
				validHost = false;
			}
		}

		// This should always be true, but Java wants to be sure the vars
		// are initialized
		if( host != null && port != 0 )
		{
			// Set up the chat GUI
			JFrame frame = new Client(host.getHostName() + ":" + port + "\n");
			frame.setVisible(true);
			connectToServer(host, port);
		}
	}
}
