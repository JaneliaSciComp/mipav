package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import javax.swing.*;
import java.awt.event.*;
import java.awt.image.*;
import javax.swing.event.*;
import java.awt.*;
import java.io.*;

/**
*	Dialog for creating and saving a screen capture to a TIFF file.  The
*	user can draw a rectangle around the region to save, or can choose a window
*	and the object in the window will be saved.  Anything drawn on the image will
*	also be saved.  This class uses the glass panes of the JFrames created by
*	MIPAV to draw the bounding boxes.  Therefore anything created by MIPAV is
*	fair game to draw upon, but clicking elsewhere on the screen will make MIPAV
*	inactive and the rectangle drawn invalid.
*/
public class JDialogCaptureScreen extends JDialogBase implements MouseListener {

    private static final int NONE   = -1;
    private static final int REGION =  0;
    private static final int WINDOW =  1;

    /** Button for selecting region mode.                 */
    private JRadioButton      regionButton;

    /** Button for selecting window mode.                 */
    private JRadioButton      windowButton;

    /** Checkbox for selecting display in window.         */
    private JCheckBox		  saveCheck;

    /** Instructions on how to use screen capture, line 1.*/
    private JLabel            instructions;

    /** Instructions on how to use screen capture, line 2.*/
    private JLabel            instructions2;

    /** Pointer to the user interface.                    */
    private ViewUserInterface userInterface;

    /** Special glass panes for all the valid frames in the GUI so that
        the user can draw rectangles on top of objects in the GUI.*/
    private MyGlassPane[]	myGlassPanes;

	/** Old glass panes to reset to after drawing is done.*/
    private Component[]		oldPanes;

    /** All the valid JFrames in the GUI.*/
    private JFrame[]		oldFrames;

    /** The rectangle that will be captured from the screen to save to a file.*/
    private Rectangle		currentRectangle;

    /** Flag that indicates whether to save capture as file or put it in new frame on screen. */
	private boolean			save;

    /** Mode - region, window, or none.*/
    private int             mode;

    /**
    *	Creates new dialog for screen capture.  Sets up glass panes of all the
    *	JFrames currently in the user interface so that we can draw on the frame
    *	to delineate a bounding box for a capture.
    *	@param parent	Parent frame of this dialog.
    *	@param UI		User interface, needed for saving TIFF image.
    */
    public JDialogCaptureScreen(JFrame parent, ViewUserInterface UI) {
        super(parent, false);
        userInterface = UI;
        save          = true;
        mode          = NONE;
        Frame[] frames = Frame.getFrames();

        // Get count of JFrames, frames with GlassPanes that we can write on
        // for the bounding box of a screen capture.
        int count = 0;
        for (int i=0; i<frames.length; i++) {
            if (frames[i] instanceof JFrame) {
                count++;
            }
        }
        myGlassPanes	= new MyGlassPane[count];
        oldPanes		= new Component[count];
        oldFrames		= new JFrame[count];

        // Save old frames so we can reset their glass panes at the end
		int j = 0;
        for (int i=0; i<frames.length; i++) {
			try {
        		JFrame test = (JFrame)frames[i];
        		test.addWindowListener(this);
        		test.addMouseListener(this);
        		oldPanes[j] = test.getGlassPane();
        		oldFrames[j] = test;
        		myGlassPanes[j] = new MyGlassPane();
        		test.setGlassPane(myGlassPanes[j]);
        		j++;
        	}
        	catch (ClassCastException error) { }
        }

        init();
    }

	/**
	*	Initialize GUI.  This is a very simple dialog, with
	*	two radio buttons to indicate the mode (drawing a region or
	*	selecting a window), a label for instructions, and
	*	an OK and Cancel button.
	*/
    private void init() {
    	setTitle("Capture screen");
        ButtonGroup group = new ButtonGroup();

        regionButton = new JRadioButton("Region");
        regionButton.setFont(MipavUtil.font12);
        regionButton.setForeground(Color.black);
        regionButton.addActionListener(this);
        regionButton.setActionCommand("Region");

        windowButton = new JRadioButton("Window");
        windowButton.setFont(MipavUtil.font12);
        windowButton.setForeground(Color.black);
        windowButton.addActionListener(this);
        windowButton.setActionCommand("Window");

        group.add(regionButton);
        group.add(windowButton);

		instructions = new JLabel("Draw a rectangle with the mouse around the");
		instructions.setFont(MipavUtil.font12);
		instructions.setForeground(Color.black);
		instructions2 = new JLabel("region you want to save.  Then press OK.");
		instructions2.setFont(MipavUtil.font12);
		instructions2.setForeground(Color.black);

		saveCheck = new JCheckBox("Display in window instead of save");
		saveCheck.setFont(MipavUtil.font12);
		saveCheck.setForeground(Color.black);
		saveCheck.setActionCommand("Save");
		saveCheck.addItemListener(this);

		buildOKButton();
        buildCancelButton();

		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridwidth = 1; gbc.gridheight = 1; gbc.anchor = gbc.WEST; gbc.weightx = 1;

		JPanel instruction = new JPanel();
		instruction.setLayout(new BoxLayout(instruction, BoxLayout.Y_AXIS));
		instruction.add(instructions);
		instruction.add(instructions2);

		JPanel panel = new JPanel();
		panel.setLayout(new GridBagLayout());
        gbc.gridx = 0; gbc.gridy = 0; gbc.weighty = 1; gbc.fill = gbc.VERTICAL;
		panel.add(instruction, gbc);
		gbc.gridy = 2; gbc.weighty = 0; gbc.fill = gbc.NONE;
		panel.add(regionButton, gbc);
		gbc.gridy = 3;
		panel.add(windowButton, gbc);
		gbc.gridy = 4;
		panel.add(saveCheck, gbc);
    	panel.setBorder(buildTitledBorder("Capture options"));

		JPanel buttonPanel = new JPanel();
		buttonPanel.add(OKButton);
		buttonPanel.add(cancelButton);

		JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new BorderLayout());
		mainPanel.add(panel);
		mainPanel.add(buttonPanel, BorderLayout.SOUTH);

		getContentPane().add(mainPanel);
		pack();
		instructions.setText("Choose type of screen capture.");
		instructions2.setText("");
    }

	/**
	*	Perfoms the following actions based on the command:<br>
	*	<ul>
	*	<li>OK		- uses the "current rectangle" set either by a listener
	*				of a glass pane (region mode) or by the window listener
	*				(window mode)</li>
	*	<li>Cancel	- cleans up and disposes the dialog</li>
	*	<li>Region	- sets all the glass panes visible to enable drawing on top of objects
	*				without selecting them</li>
	*	<li>Window	- sets the glass panes invisible; then when a window is selected,
	*				the listener will save that rectangle.</li>
	*	</ul>
	*	@param event	Event that triggered this function.
	*/
	public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();
		if (command.equals("OK")) {
			if (mode != NONE && currentRectangle != null && !currentRectangle.isEmpty() &&
			                    currentRectangle.x > -1  &&  currentRectangle.y > -1) {
				if (writeImage()) {
				    for (int i=0; i<oldFrames.length; i++) {
					    myGlassPanes[i].setVisible(false);
					    oldFrames[i].setGlassPane(oldPanes[i]);
					    oldFrames[i].removeWindowListener(this);
					    oldFrames[i].removeMouseListener(this);
					    myGlassPanes[i] = null;
				    }
				    myGlassPanes = null;
				    dispose();
				    System.gc();
			    }
			}
			else {
				MipavUtil.displayError("You must choose a region or window to capture.");
			}
		}
		else if (command.equals("Cancel")) {
			for (int i=0; i<oldFrames.length; i++) {
				myGlassPanes[i].setVisible(false);
				oldFrames[i].setGlassPane(oldPanes[i]);
				oldFrames[i].removeWindowListener(this);
				oldFrames[i].removeMouseListener(this);
				myGlassPanes[i] = null;
			}
			myGlassPanes = null;
			dispose();
			System.gc();
		}
		else if (command.equals("Region")) {
			for (int i=0; i<myGlassPanes.length; i++) {
				myGlassPanes[i].setVisible(true);
			}
			instructions.setText("Draw a rectangle with the mouse around the");
			instructions2.setText("region you want to save.  Then press OK.");
			mode = REGION;
		}
		else if (command.equals("Window")) {
			for (int i=0; i<myGlassPanes.length; i++)
				myGlassPanes[i].setVisible(false);
			instructions.setText("Select the window you want to save.");
			instructions2.setText("Then press OK.");
			mode = WINDOW;
		}

	}

	/**
	*	Writes the image captured from the screen, using currentRectangle as the
	*	bounding box of the capture.  Converts pixels grabbed using a Robot to an
	*	RGB TIFF file.
	*/
	private boolean writeImage() {
    	int pixels[];
	    int bufferSize, xDim, yDim;
    	short buffer[] = null;
    	ModelImage testImage = null;
		Robot robot;

    	try {
        	robot = new Robot();
        	Image imagePix = robot.createScreenCapture(currentRectangle);
        	xDim = currentRectangle.width;
        	yDim = currentRectangle.height;
        	bufferSize = 4*xDim*yDim;
        	pixels = new int[xDim*yDim];
        	PixelGrabber pgTest = new PixelGrabber(imagePix, 0, 0, xDim, yDim, pixels, 0, xDim);
        	pgTest.grabPixels();
    	}
    	catch (InterruptedException e) {
        	Preferences.debug("Interrupted waiting for pixels!");
        	return false;
    	}
    	catch (OutOfMemoryError error){
        	MipavUtil.displayError("ViewFrameImage: unable to allocate enough memory for RGB image");
        	return false;
    	}
    	catch (AWTException error) {
        	MipavUtil.displayError("Platform doesn't support screen capture.");
        	return false;
    	}


    	try {
        	int extents[] = new int[2];
        	extents[0] = xDim; // RGB
        	extents[1] = yDim;
        	testImage = new ModelImage(ModelStorageBase.ARGB, extents, "Screen capture", userInterface);
        	buffer    = new short[bufferSize];
    	}
    	catch (OutOfMemoryError error){
        	MipavUtil.displayError("JDialogScreenCapture: unable to allocate enough memory for RGB image");
        	return false;
    	}

    	int i,k;
    	for (i = 0, k = 0; i < xDim*yDim; i++, k+=4) {
        	buffer[k]   = (short)(255);                     // alpha
        	buffer[k+1] = (short)(pixels[i] >> 16 & 0xFF);  // Red
        	buffer[k+2] = (short)(pixels[i] >> 8  & 0xFF);  // Green
        	buffer[k+3] = (short)(pixels[i] & 0xFF);        // Blue
    	}

    	try {
        	testImage.importData( 0, buffer, true);
    	}
    	catch (IOException error) {
        	MipavUtil.displayError("JDialogScreenCapture: Problems grabbing image!");
    	}
    	testImage.getFileInfo()[0].setPhotometric((short)2); // Indicates RGB tiff file format

    	if (save) {
    		String fileName;
    		String directory;
    		FileIO fileIO = new FileIO();

	    	JFileChooser chooser = new JFileChooser();
        	if (userInterface.getDefaultDirectory() != null)
            	chooser.setCurrentDirectory(new File(userInterface.getDefaultDirectory()));
        	else
            	chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
    		chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        	int returnVal = chooser.showSaveDialog(this);

        	if(returnVal == JFileChooser.APPROVE_OPTION) {
            	fileName  = chooser.getSelectedFile().getName();
            	directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;

            	fileIO.writeImage(testImage, new FileWriteOptions(fileName, directory, true));
            	userInterface.setDefaultDirectory(String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar);
        	}
        	else return false;
    	}
    	else {
    		new ViewJFrameImage(testImage, null, new Dimension(610,200));
    	}
    	return true;
	}

	/**
	*	If in Window mode, captures the content pane of the window activated and sets
	*	the current rectangle to the bounds of the content pane.
	*	@param event	Event that triggered this method.
	*/
    public void windowActivated(WindowEvent	event){
    	// not in window mode
    	if (mode != WINDOW) return;
    	// don't save the dialog as the screen capture -
    	// and the dialog will be the last window activated
    	if (event.getWindow().equals(this)) return;
    	else {
    		try {
    			JFrame frame = (JFrame)event.getWindow();

    			Point p = new Point();
    		//  These ought to have been (0,0) in all cases
    		//  but sometimes were not.  As a result the
    		//  window mode sometimes looked wrong (offset
    		//  vertically).  Hardcoding it to (0,0) fixed
    		//  this problem.
    		//	p.x = frame.getContentPane().getX();
    		//	p.y = frame.getContentPane().getY();

    			p.x = 0;
    			p.y = 0;
    			SwingUtilities.convertPointToScreen(p, frame.getContentPane());
                p.x++; //must correct this slightly
                p.y++; //""
    			Dimension d = new Dimension();
    			d.width = frame.getContentPane().getWidth() - 3; // the -3 is a correction
    			d.height = frame.getContentPane().getHeight() - 3; // ""
    			currentRectangle = new Rectangle(p, d);
            }
            catch (ClassCastException error) { }
        }
    }

	/**
	*	Unchanged.
	*/
    public void mouseClicked(MouseEvent e) {}

	/**
	*	Unchanged.
	*/
    public void mouseEntered(MouseEvent e) {}

	/**
	*	Unchanged.
	*/
    public void mouseExited(MouseEvent e) {}

    /**
    *	When the user presses the mouse in any valid frame of
    *	the GUI, causes all the glass panes to repaint.  This
    *	is so that multiple rectangles in different windows
    *	won't show up on the screen.
    *	@param e	Event that triggered this function.
    */
    public void mousePressed(MouseEvent e) {
    	for (int i=0; i<myGlassPanes.length; i++)
    		myGlassPanes[i].repaint();
    }

	/**
	*	Unchanged.
	*/
    public void mouseReleased(MouseEvent e) {}

	/**
	*   Sets save flag based on whether or not the
	*   save checkbox is checked.
	*   @param event    Event that triggered this function.
	*/
	public void itemStateChanged(ItemEvent event) {
	    if (event.getSource() == saveCheck) {
	        save = !(saveCheck.isSelected());
	    }
	}

	/**
	*	Our special glass panes draw rectangles based on how the user
	*	traces them using the mouse.  The mouse events aren't captured
	*	unless the glass pane is visible.
	*/
	class MyGlassPane extends JComponent  {
		/** Dimensions of rectangle to draw. */
		int x=-1, y=-1, width=-1, height=-1;
		/** When we're done drawing the rectangle, change the color. */
		boolean complete = false;

		/**
		*	Creates new glass pane and adds the listeners.
		*/
		public MyGlassPane() {
			GlassPaneListener listener = new GlassPaneListener(this);
			addMouseListener(listener);
			addMouseMotionListener(listener);
		}

		/**
		*	Paints a yellow rectangle based on the
		*	dimensions if not complete; if complete,
		*	paints a red rectangle.
		*	@param g	Graphics component for this paint.
		*/
		public void paint(Graphics g) {
			if (x > 0) {
				if (!complete)	g.setColor(Color.yellow);
				else			g.setColor(Color.red);
				g.drawRect(x, y, width, height);
		    }
		}

		/**
		*	Accessor to set the x value of the painted rectangle.
		*	@param x	X value of painted rectangle.
		*/
		public void setX(int x) { this.x = x; }

		/**
		*	Accessor to set the y value of the painted rectangle.
		*	@param y	Y value of painted rectangle.
		*/
		public void setY(int y) { this.y = y; }

		/**
		*	Accessor to set the width value of the painted rectangle.
		*	@param w	Width value of painted rectangle.
		*/
		public void setW(int w) { this.width = w; }

		/**
		*	Accessor to set the height value of the painted rectangle.
		*	@param h	Height value of painted rectangle.
		*/
		public void setH(int h) { this.height = h; }

		/**
		*	Accessor to tell the glass pane that the rectangle drawing is complete.
		*	If so, sets the currentRectangle appropriately.
		*	@param comp	<code>true</code> if drawing is complete, <code>false</code> otherwise.
		*/
		public void setComplete(boolean comp) {
			this.complete = comp;
			if (complete) {
				// the +1 accounts for the size of the line
				Point p = new Point(x+1, y+1);
				SwingUtilities.convertPointToScreen(p, this);
				// the -1 accounts for the size of the line
				Dimension d = new Dimension(width-1, height-1);
				currentRectangle = new Rectangle(p, d);
			}
		}
	}

	/**
	*	Listener for the glass panes.
	*/
	class GlassPaneListener extends MouseInputAdapter {
		/** Pane of glass this listener is tied to. */
		MyGlassPane glassPane;
		/** X and Y coordinates where drawing started. */
		int x, y;

		/**
		*	Creates new listener that captures mouse and mouse
		*	motion events and sends them to the glass pane
		*	to draw a partial or completed rectangle.
		*	@param glassPane	Glass pane tied to this listener.
		*/
		public GlassPaneListener (MyGlassPane glassPane) {
			this.glassPane = glassPane;
		}

		/**
		*	Sets the width and height variables in the glass pane
		*	based upon how far the mouse has been dragged and
		*	repaints the glass pane.
		*	@param e	Event that triggered this function.
		*/
    	public void mouseDragged(MouseEvent e) {
			glassPane.setW(e.getX() - x);
			glassPane.setH(e.getY() - y);
			glassPane.repaint();
    	}

    	/**
    	*	Sets the x and y variables in the glass pane to
    	*	indicate that we've begun drawing a rectangle.
    	*	Resets all the glass panes first so only one
    	*	has any drawing in it at one time.
    	*	@param e	Event that triggered this function.
    	*/
    	public void mousePressed(MouseEvent e) {
			for (int i=0; i<myGlassPanes.length; i++) {
				myGlassPanes[i].setX(-1);
				myGlassPanes[i].repaint();
			}
			glassPane.setComplete(false);
			x = e.getX();
			y = e.getY();
			glassPane.setX(x);
			glassPane.setY(y);
			glassPane.setW(0);
			glassPane.setH(0);
			glassPane.repaint();
    	}

    	/**
    	*	Sets the width and height variables appropriately,
    	*	and lets the glass pane know that we are done
    	*	drawing the rectangle.
    	*	@param e	Event that triggered this function.
    	*/
    	public void mouseReleased(MouseEvent e) {
			glassPane.setW(e.getX() - x);
			glassPane.setH(e.getY() - y);
			glassPane.setComplete(true);
			glassPane.repaint();
    	}
		/**
		*	Unchanged.
		*/
    	public void mouseClicked(MouseEvent e) {}

		/**
		*	Unchanged.
		*/
    	public void mouseEntered(MouseEvent e) {}

		/**
		*	Unchanged.
		*/
    	public void mouseExited(MouseEvent e) {}

		/**
		*	Unchanged.
		*/
    	public void mouseMoved(MouseEvent e) {}

    }


}
