/**
 *		ClientSetup provides basic UI for entering a hostname and port.
 *		It provides a blocking method for outside classes to extract that data.
 */

// For GUI
import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class ClientSetup extends JFrame implements ActionListener
{
	// IO elements that need to be accessed in several methods
	private static JTextField hostInput;
	private static JTextField portInput;
	private static JButton connect;

	// Set to 'true' when we have input from the user and are ready to read it
	private static boolean ready = false;

	// Sets up the GUI elements, called once during setup
	public ClientSetup()
	{
		// Main window
		setTitle("Choose Server");
		setSize(150, 140);
		setResizable(false);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

		// Hostname stuff
		JPanel hostPanel = new JPanel();
		JLabel hostTitle = new JLabel("Enter hostname:");
		hostInput = new JTextField();
		hostInput.addActionListener(this);
		hostPanel.setLayout(new BorderLayout());
		hostPanel.add(hostTitle, BorderLayout.NORTH);
		hostPanel.add(hostInput, BorderLayout.CENTER);

		// Port number stuff
		JPanel portPanel = new JPanel();
		JLabel portTitle = new JLabel("Enter port:");
		portInput = new JTextField();
		portInput.addActionListener(this);
		portPanel.setLayout(new BorderLayout());
		portPanel.add(portTitle, BorderLayout.NORTH);
		portPanel.add(portInput, BorderLayout.CENTER);

		// Connect button
		connect = new JButton("Connect");
		connect.addActionListener(this);

		// Vomit it all onto the main window
		this.setLayout(new FlowLayout());
		this.add(hostPanel);
		this.add(portPanel);
		this.add(connect);
	}

	// When Connect or Return is pressed we check if there's text in both
	// boxes, and if there is we set 'ready' to True
	public void actionPerformed(ActionEvent event)
	{
		if( hostInput.getText().length() > 0 ||
			portInput.getText().length() > 0 )
			ready = true;
	}

	// This effectively returns a tuple of (hostname, port), but using arrays
	public String [] getAddress()
	{
		// Wait on the ready variable
		while( !ready )
			System.out.print(""); // On OSX the loop never exited without this
			// Write once, run everywhere my ass.
		ready = false; // Reset the status
		return new String [] {hostInput.getText(), portInput.getText()};
	}
}
