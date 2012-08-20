package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.reportbug.ReportBugBuilder;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.*;
import java.awt.datatransfer.*;

import java.io.*;

import javax.imageio.ImageIO;
import javax.swing.*;
import javax.swing.TransferHandler.TransferSupport;
import javax.swing.event.*;


/**
 * Dialog for creating and saving a screen capture to a TIFF file. The user can draw a rectangle around the region to
 * save, or can choose a window and the object in the window will be saved. Anything drawn on the image will also be
 * saved. This class uses the glass panes of the JFrames created by MIPAV to draw the bounding boxes. Therefore anything
 * created by MIPAV is fair game to draw upon, but clicking elsewhere on the screen will make MIPAV inactive and the
 * rectangle drawn invalid.
 */
public class JDialogCaptureScreen extends JDialogBase implements MouseListener, ComponentListener{

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2364847865242983737L;

    public enum WindowProperties{
    	/** Selected region of the window */
    	REGION,
    	/** The entire image window */
    	WINDOW;
    }

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The rectangle that will be captured from the screen to save to a file. */
    private Rectangle currentRectangle;

    /** Instructions on how to use screen capture, line 1. */
    private JLabel instructions;

    /** Instructions on how to use screen capture, line 2. */
    private JLabel instructions2;

    /** Mode - region or window */
    private WindowProperties mode;

    /**
     * Special glass panes for all the valid frames in the GUI so that the user can draw rectangles on top of objects in
     * the GUI.
     */
    private MyGlassPane[] myGlassPanes;

    /** All the valid JFrames in the GUI. */
    private JFrame[] oldFrames;

    /** Old glass panes to reset to after drawing is done. */
    private Component[] oldPanes;

    /** Button for selecting region mode. */
    private JRadioButton regionButton;

    /** Flag that indicates whether to save capture as file. */
    private boolean save;
    
    /** Flag that indicates whether to copy capture to clipboard*/
    private boolean copy;
    
    /** Flag that indicates whether to put capture in a new frame on screen. */
    private boolean display;

    /** Button for selecting display in window. */
    private JRadioButton displayCheck;
    
    /** Button for selecting copy to clipboard. */
    private JRadioButton copyCheck;
    
    /** Button for selecting save to computer. */
    private JRadioButton saveCheck;

    /** Pointer to the user interface. */
    private ViewUserInterface userInterface;

    /** Button for selecting window mode. */
    private JRadioButton windowButton;
    
    /** Active frame **/
    public ViewJFrameImage activeFrame;
    
    /** Boolean to indicate whether or not this class is being used by the ImageAttacher class */
    private boolean imageAttacher;
    
    /** JTextField for the name of the file being attached */
    private JTextField fileField = new JTextField(25);
    
    public boolean ok = false;

	public static String fileName;

	private File attachment;
	
	public static int test;
	
	private int tester;

	private JLabel instructions3;

	public static BufferedImage currImage;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for screen capture. Sets up glass panes of all the JFrames currently in the user interface so
     * that we can draw on the frame to delineate a bounding box for a capture.
     *
     * @param  parent  Parent frame of this dialog.
     */
    public JDialogCaptureScreen(ViewJFrameImage parent) {
        this(parent, false);
    }
    
    public JDialogCaptureScreen(ViewJFrameImage parent, boolean bugReport) {
        super(parent, false);

        userInterface = ViewUserInterface.getReference();
        save = false;
        copy = false;
        display = false;
        mode = WindowProperties.REGION;
        imageAttacher = bugReport;
        
        

	        Frame[] frames = Frame.getFrames();
	
	        // Get count of JFrames, frames with GlassPanes that we can write on
	        // for the bounding box of a screen capture.
	        int count = 0;
	
	        for (int i = 0; i < frames.length; i++) {
	
	            if (frames[i] instanceof JFrame) {
	                count++;
	            }
	        }
	
	        myGlassPanes = new MyGlassPane[count];
	        oldPanes = new Component[count];
	        oldFrames = new JFrame[count];
	
	        // Save old frames so we can reset their glass panes at the end
	        int j = 0;
	
	        for (int i = 0; i < frames.length; i++) {
	
	            try {
	                JFrame test = (JFrame) frames[i];
	                test.addWindowListener(this);
	                test.addMouseListener(this);
	                oldPanes[j] = test.getGlassPane();
	                oldFrames[j] = test;
	                myGlassPanes[j] = new MyGlassPane();
	                test.setGlassPane(myGlassPanes[j]);
	                j++;
	            } catch (ClassCastException error) { }
	        }
	        if (imageAttacher == false) {
	        	init();
	        } else {
	        	init(true);
	        }
	        
    }


    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Performs the following actions based on the command:<br>
     *
     * <ul>
     *   <li>OK - uses the "current rectangle" set either by a listener of a glass pane (region mode) or by the window
     *     listener (window mode)</li>
     *   <li>Cancel - cleans up and disposes the dialog</li>
     *   <li>Region - sets all the glass panes visible to enable drawing on top of objects without selecting them</li>
     *   <li>Window - sets the glass panes invisible; then when a window is selected, the listener will save that
     *     rectangle.</li>
     * </ul>
     *
     * @param  event  Event that triggered this function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        if (command.equals("OK")) {

            if ((currentRectangle != null) && !currentRectangle.isEmpty() &&
                    (currentRectangle.x > -1) && (currentRectangle.y > -1)) {
            	
            	writeImage();
            	if(imageAttacher) {

                	for (int i = 0; i < oldFrames.length; i++) {
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
            	ok = true;
            		
            		
                	//commented the code below out in order to leave dialog up so that you can 
                	//select multiple regions one after the other
                	
                    //for (int i = 0; i < oldFrames.length; i++) {
                    //   myGlassPanes[i].setVisible(false);
                    //   oldFrames[i].setGlassPane(oldPanes[i]);
                    //   oldFrames[i].removeWindowListener(this);
                    //   oldFrames[i].removeMouseListener(this);
                    //   myGlassPanes[i] = null;
                    //}

                    //myGlassPanes = null;
                    //dispose();
                    //System.gc();
            } else {
                MipavUtil.displayError("You must choose a region or window to capture.");
            }
            
        } else if (command.equals("Cancel")) {

            for (int i = 0; i < oldFrames.length; i++) {
                myGlassPanes[i].setVisible(false);
                oldFrames[i].setGlassPane(oldPanes[i]);
                oldFrames[i].removeWindowListener(this);
                oldFrames[i].removeMouseListener(this);
                myGlassPanes[i] = null;
            }
            
            myGlassPanes = null;
            dispose();
            System.gc();
        } else if (command.equals("Region")) {

        	currentRectangle = null;
            for (int i = 0; i < myGlassPanes.length; i++) {
                myGlassPanes[i].setVisible(true);
            }

            instructions.setText("Draw a rectangle with the mouse around the");
            instructions2.setText("region you want to save.  Then press OK.");
            mode = WindowProperties.REGION;
        } else if (command.equals("Window")) {

        	currentRectangle = null;
            for (int i = 0; i < myGlassPanes.length; i++) {
                myGlassPanes[i].setVisible(false);
            }

            instructions.setText("Select the window you want to save.");
            instructions2.setText("Then press OK.");
            mode = WindowProperties.WINDOW;
        }
            

    }

    /**
     * Sets save, copy, and display flags based on whether or not they are selected.
     *
     * @param  event  Event that triggered this function.
     */
    public void itemStateChanged(ItemEvent event) {

            save = saveCheck.isSelected();
        	copy = copyCheck.isSelected();
        	display = displayCheck.isSelected();
        
    }

    /**
     * Unchanged.
     *
     * @param  e  DOCUMENT ME!
     */
    public void mouseClicked(MouseEvent e) { }

    /**
     * Unchanged.
     *
     * @param  e  DOCUMENT ME!
     */
    public void mouseEntered(MouseEvent e) { }

    /**
     * Unchanged.
     *
     * @param  e  DOCUMENT ME!
     */
    public void mouseExited(MouseEvent e) { }

    /**
     * When the user presses the mouse in any valid frame of the GUI, causes all the glass panes to repaint. This is so
     * that multiple rectangles in different windows won't show up on the screen.
     *
     * @param  e  Event that triggered this function.
     */
    public void mousePressed(MouseEvent e) {

        for (int i = 0; i < myGlassPanes.length; i++) {
            myGlassPanes[i].repaint();
        }
    }

    /**
     * Unchanged.
     *
     * @param  e  DOCUMENT ME!
     */
    public void mouseReleased(MouseEvent e) { }
    
   /**
     * If in Window mode, captures the content pane of the window activated and sets the current rectangle to the bounds
     * of the content pane.
     *
     * @param  event  Event that triggered this method.
     */
    public void windowActivated(WindowEvent event) {
        // not in window mode
    	switch(mode){
        case REGION:
        	if(event.getWindow() instanceof ViewJFrameImage) {
	        	ViewJFrameImage frame = (ViewJFrameImage) event.getWindow();
	            activeFrame = frame;
	            activeFrame.addComponentListener(this);
        	}
        	break;
        case WINDOW:
        	if (!event.getWindow().equals(this)) {
	            try {
	                ViewJFrameImage frame = (ViewJFrameImage) event.getWindow();
	                activeFrame = frame;
	                activeFrame.addComponentListener(this);

	                Point p = new Point();
	                // These ought to have been (0,0) in all cases
	                // but sometimes were not.  As a result the
	                // window mode sometimes looked wrong (offset
	                // vertically).  Hardcoding it to (0,0) fixed
	                // this problem.
	                // p.x = frame.getContentPane().getX();
	                // p.y = frame.getContentPane().getY();
	
	                p.x = 0;
	                p.y = 0;
	                SwingUtilities.convertPointToScreen(p, frame.getContentPane());
	                p.x++; // must correct this slightly
	                p.y++; // ""
	
	                Dimension d = new Dimension();
	                d.width = frame.getContentPane().getWidth() - 3; // the -3 is a correction
	                d.height = frame.getContentPane().getHeight() - 3; // ""
	                currentRectangle = new Rectangle(p, d);
	            } catch (ClassCastException error) { }
	        }
        	break;
        }
    
    }

    /**
     * Initialize GUI. This is a very simple dialog, with two radio buttons to indicate the mode (drawing a region or
     * selecting a window), a label for instructions, and an OK and Cancel button.
     */
    private void init() {
        setTitle("Capture screen");
        
        currentRectangle = null;
        for (int i = 0; i < myGlassPanes.length; i++) {
            myGlassPanes[i].setVisible(true);
        }

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
        
        ButtonGroup sampleImage = new ButtonGroup();

        sampleImage.add(regionButton);
        sampleImage.add(windowButton);

        instructions = new JLabel("Draw a rectangle with the mouse around the");
        instructions.setFont(MipavUtil.font12);
        instructions.setForeground(Color.black);
        instructions2 = new JLabel("region you want to save.  Then press OK.");
        instructions2.setFont(MipavUtil.font12);
        instructions2.setForeground(Color.black);

        saveCheck = new JRadioButton("Save selection");
        saveCheck.setFont(MipavUtil.font12);
        saveCheck.setForeground(Color.black);
        saveCheck.setActionCommand("Save");
        saveCheck.addItemListener(this);
        
        copyCheck = new JRadioButton("Copy to clipboard");
        copyCheck.setFont(MipavUtil.font12);
        copyCheck.setForeground(Color.black);
        copyCheck.setActionCommand("Copy");
        copyCheck.addItemListener(this);
        
        displayCheck = new JRadioButton("Display in new window");
        displayCheck.setFont(MipavUtil.font12);
        displayCheck.setForeground(Color.black);
        displayCheck.setActionCommand("Display");
        displayCheck.addItemListener(this);
        
        ButtonGroup saveOptions = new ButtonGroup();
        
        saveOptions.add(saveCheck);
        saveOptions.add(copyCheck);
        saveOptions.add(displayCheck);

        OKButton = buildOKButton();
        cancelButton = buildCancelButton();

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;

        JPanel instruction = new JPanel();
        instruction.setLayout(new BoxLayout(instruction, BoxLayout.Y_AXIS));
        instruction.add(instructions);
        instruction.add(instructions2);

        JPanel panel = new JPanel();
        panel.setLayout(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.VERTICAL;
        panel.add(instruction, gbc);
        gbc.gridy = 2;
        gbc.weighty = 0;
        gbc.fill = GridBagConstraints.NONE;
        panel.add(regionButton, gbc);
        gbc.gridy = 3;
        panel.add(windowButton, gbc);
        panel.setBorder(buildTitledBorder("Image options"));
        regionButton.setSelected(true);
        
        JPanel options = new JPanel();
        options.setLayout(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weighty = 1;
        gbc.gridy = 2;
        options.add(saveCheck, gbc);
        gbc.gridy = 3;
        options.add(copyCheck, gbc);
        gbc.gridy = 4;
        options.add(displayCheck, gbc);
        options.setBorder(buildTitledBorder("Capture options"));
        saveCheck.setSelected(true);

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());
        mainPanel.add(panel, BorderLayout.WEST);
        mainPanel.add(buttonPanel, BorderLayout.SOUTH);
        
        mainPanel.add(options, BorderLayout.EAST);
        
        getContentPane().add(mainPanel);
        try {
			setIconImage(MipavUtil.getIconImage("divinci.gif"));
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
        //resizer.addComponentListener(this);
        pack();
        setVisible(true);
    }
    
    public void init(boolean imageAttacher) {
    	
    	
    	setTitle("Create New Image");
        
        currentRectangle = null;
        for (int i = 0; i < myGlassPanes.length; i++) {
            myGlassPanes[i].setVisible(true);
        }

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

        instructions = new JLabel("Draw a rectangle with the mouse around the");
        instructions.setFont(MipavUtil.font12);
        instructions.setForeground(Color.black);
        instructions2 = new JLabel("region you want to save.  Then press OK.");
        instructions2.setFont(MipavUtil.font12);
        instructions2.setForeground(Color.black);
        instructions3 = new JLabel("(Regions must originate within a MIPAV window.)");
        instructions3.setFont(MipavUtil.font12);
        instructions3.setForeground(Color.black);

        OKButton = buildOKButton();
        cancelButton = buildCancelButton();

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;

        JPanel instruction = new JPanel();
        instruction.setLayout(new BoxLayout(instruction, BoxLayout.Y_AXIS));
        instruction.add(instructions);
        instruction.add(instructions2);
        instruction.add(instructions3);

        JPanel panel = new JPanel();
        panel.setLayout(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.VERTICAL;
        panel.add(instruction, gbc);
        gbc.gridy = 2;
        gbc.weighty = 0;
        gbc.fill = GridBagConstraints.NONE;
        panel.add(regionButton, gbc);
        gbc.gridy = 3;
        panel.add(windowButton, gbc);
        panel.setBorder(buildTitledBorder("Image options"));
        regionButton.setSelected(true);

        JPanel buttonPanel = new JPanel();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        JPanel attachmentOptions = new JPanel();
        attachmentOptions.setLayout(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weighty = 1;
        gbc.gridy = 2;
        JLabel attachmentInstruction = new JLabel("Select the region you would like to attach. Copy the image and ");
        attachmentInstruction.setFont(MipavUtil.font12);
        attachmentInstruction.setForeground(Color.black);
        
        JLabel attachmentInstruction2 = new JLabel("paste the image to the description area, or enter a file name");
        attachmentInstruction2.setFont(MipavUtil.font12);
        attachmentInstruction2.setForeground(Color.black);
        
        JLabel attachmentInstruction3 = new JLabel("below and press the OK button to attach the image as a file. \n");
        attachmentInstruction3.setFont(MipavUtil.font12);
        attachmentInstruction3.setForeground(Color.black);
        
        attachmentOptions.add(attachmentInstruction, gbc);
        gbc.gridy = 3;
        attachmentOptions.add(attachmentInstruction2, gbc);
        gbc.gridy = 4;
        attachmentOptions.add(attachmentInstruction3, gbc);
        gbc.gridy = 6;
        fileField.setPreferredSize(new Dimension(320, 20));
        attachmentOptions.add(fileField, gbc);
        attachmentOptions.setBorder(buildTitledBorder("Image Selection"));
        
        
        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());
//        mainPanel.add(instruction, BorderLayout.NORTH);
        mainPanel.add(buttonPanel, BorderLayout.SOUTH);
        mainPanel.add(attachmentOptions, BorderLayout.NORTH);

        
        getContentPane().add(mainPanel);
        try {
			setIconImage(MipavUtil.getIconImage("divinci.gif"));
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
        //resizer.addComponentListener(this);
        pack();
        setVisible(true);
    }

    /**
     * Writes the image captured from the screen, using currentRectangle as the bounding box of the capture. Converts
     * pixels grabbed using a Robot to an RGB TIFF file.
     *
     * @return  DOCUMENT ME!
     */
    private boolean writeImage() {
        int[] pixels;
        int bufferSize, xDim, yDim;
        short[] buffer = null;
        ModelImage testImage = null;
        Robot robot;
        String imageName;
        Image imagePix;

        try {
            robot = new Robot();

            imagePix = robot.createScreenCapture(currentRectangle);
            xDim = currentRectangle.width;
            yDim = currentRectangle.height;
            bufferSize = 4 * xDim * yDim;
            pixels = new int[xDim * yDim];

            PixelGrabber pgTest = new PixelGrabber(imagePix, 0, 0, xDim, yDim, pixels, 0, xDim);
            pgTest.grabPixels();
        } catch (InterruptedException e) {
            Preferences.debug("Interrupted waiting for pixels!");

            return false;
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("ViewFrameImage: unable to allocate enough memory for RGB image");

            return false;
        } catch (AWTException error) {
            MipavUtil.displayError("Platform doesn't support screen capture.");

            return false;
        }


        if (!imageAttacher) {
	        try {
	            int[] extents = new int[2];
	            extents[0] = xDim; // RGB
	            extents[1] = yDim;
	
	            imageName = activeFrame.getImageNameA() + "_screen_capture.tif";
	            testImage = new ModelImage(ModelStorageBase.ARGB, extents, activeFrame.getImageNameA() + "_screen_capture");
	            testImage.setImageName(imageName);
	            testImage.getFileInfo()[0].setFileDirectory(activeFrame.getImageA().getFileInfo(0).getFileDirectory());
	            buffer = new short[bufferSize];
	        } catch (OutOfMemoryError error) {
	            MipavUtil.displayError("JDialogScreenCapture: unable to allocate enough memory for RGB image");
	
	            return false;
	        }

	        int i, k;
	
	        for (i = 0, k = 0; i < (xDim * yDim); i++, k += 4) {
	            buffer[k] = (short) (255); // alpha
	            buffer[k + 1] = (short) ((pixels[i] >> 16) & 0xFF); // Red
	            buffer[k + 2] = (short) ((pixels[i] >> 8) & 0xFF); // Green
	            buffer[k + 3] = (short) (pixels[i] & 0xFF); // Blue
	        }
	
	        try {
	            testImage.importData(0, buffer, true);
	        } catch (IOException error) {
	            MipavUtil.displayError("JDialogScreenCapture: Problems grabbing image!");
	        }

	        testImage.getFileInfo()[0].setPhotometric((short) 2); // Indicates RGB tiff file format
       	
	        if (save) {
	            String fileName;
	            String directory;
	            FileIO fileIO = new FileIO();
	
	            JFileChooser chooser = new JFileChooser();
	
	            //if (userInterface.getDefaultDirectory() != null) {
	             //   chooser.setCurrentDirectory(new File(userInterface.getDefaultDirectory()));
	            //} else {
	            //    chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
	            //}
	
	            
	            if (testImage.getFileInfo(0).getFileDirectory() != null) {
	                chooser.setSelectedFile(new File(testImage.getFileInfo(0).getFileDirectory() + testImage.getImageFileName()));
	            }
	            else {
	                chooser.setSelectedFile(new File(testImage.getImageFileName()));
	            }
	            
	            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
	
	            int returnVal = chooser.showSaveDialog(this);
	
	            if (returnVal == JFileChooser.APPROVE_OPTION) {
	                fileName = chooser.getSelectedFile().getName();
	                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
	
	                fileIO.writeImage(testImage, new FileWriteOptions(fileName, directory, true));
	                userInterface.setDefaultDirectory(String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar);
	            } else {
	
	                return false;
	            }
	            
	            testImage.disposeLocal();
	        } 
	        if (copy) {
	        	try{
	        		Clipboard clipboard = Toolkit.getDefaultToolkit().getSystemClipboard();
	        		Transferable imagePixTransferable = new ImageConverter(imagePix);
	            	clipboard.setContents(imagePixTransferable, null); 
	        	} catch(Exception e) {
	        		MipavUtil.displayError("Cannot access system clipboard");
	        	}
	        }
	        if (display) {
	            new ViewJFrameImage(testImage, null, new Dimension(610, 200));
	        } 
        } else {
        	fileName = fileField.getText();
        	try {
	        	currImage = (BufferedImage) imagePix;
	        } catch (IllegalArgumentException e) {
	           	
	        }
	    }
        return true;
    }
    
    /** changes an image to a Transferable*/
	public static class ImageConverter implements Transferable{
		private Image temp;
		public ImageConverter(Image image){
			temp = image;
		}
		public Object getTransferData(DataFlavor flavor) throws UnsupportedFlavorException, IOException {
			if (DataFlavor.imageFlavor.equals(flavor)){
				return temp;
			}
			throw new UnsupportedFlavorException(flavor);
		}
		public DataFlavor[] getTransferDataFlavors() {
			return new DataFlavor[] {DataFlavor.imageFlavor};
		}
		public boolean isDataFlavorSupported(DataFlavor flavor) {
			return DataFlavor.imageFlavor.equals(flavor);
		}
	}
    
    
    /** 
	 * window closing
	 */
	public void windowClosing(WindowEvent event) {
		for (int i = 0; i < oldFrames.length; i++) {
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

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Listener for the glass panes.
     */
    class GlassPaneListener extends MouseInputAdapter {

        /** Pane of glass this listener is tied to. */
        MyGlassPane glassPane;

        /** X and Y coordinates where drawing started. */
        int x, y;

        /**
         * Creates new listener that captures mouse and mouse motion events and sends them to the glass pane to draw a
         * partial or completed rectangle.
         *
         * @param  glassPane  Glass pane tied to this listener.
         */
        public GlassPaneListener(MyGlassPane glassPane) {
            this.glassPane = glassPane;
        }

        /**
         * Unchanged.
         *
         * @param  e  DOCUMENT ME!
         */
        public void mouseClicked(MouseEvent e) { }

        /**
         * Sets the width and height variables in the glass pane based upon how far the mouse has been dragged and
         * repaints the glass pane.
         *
         * @param  e  Event that triggered this function.
         */
        public void mouseDragged(MouseEvent e) {
            glassPane.setW(e.getX() - x);
            glassPane.setH(e.getY() - y);
            glassPane.repaint();
        }

        /**
         * Unchanged.
         *
         * @param  e  DOCUMENT ME!
         */
        public void mouseEntered(MouseEvent e) { }

        /**
         * Unchanged.
         *
         * @param  e  DOCUMENT ME!
         */
        public void mouseExited(MouseEvent e) { }

        /**
         * Unchanged.
         *
         * @param  e  DOCUMENT ME!
         */
        public void mouseMoved(MouseEvent e) { }

        /**
         * Sets the x and y variables in the glass pane to indicate that we've begun drawing a rectangle. Resets all the
         * glass panes first so only one has any drawing in it at one time.
         *
         * @param  e  Event that triggered this function.
         */
        public void mousePressed(MouseEvent e) {

            for (int i = 0; i < myGlassPanes.length; i++) {
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
         * Sets the width and height variables appropriately, and lets the glass pane know that we are done drawing the
         * rectangle.
         *
         * @param  e  Event that triggered this function.
         */
        public void mouseReleased(MouseEvent e) {
            glassPane.setW(e.getX() - x);
            glassPane.setH(e.getY() - y);
            glassPane.setComplete(true);
            glassPane.repaint();
        }

        
        
        
    }

    /**
     * Our special glass panes draw rectangles based on how the user traces them using the mouse. The mouse events
     * aren't captured unless the glass pane is visible.
     */
    class MyGlassPane extends JComponent {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -1351237665634271651L;

        /** When we're done drawing the rectangle, change the color. */
        boolean complete = false;

        /** Dimensions of rectangle to draw. */
        int x = -1, y = -1, width = -1, height = -1;

        /**
         * Creates new glass pane and adds the listeners.
         */
        public MyGlassPane() {
            GlassPaneListener listener = new GlassPaneListener(this);
            addMouseListener(listener);
            addMouseMotionListener(listener);
        }

        /**
         * Paints a yellow rectangle based on the dimensions if not complete; if complete, paints a red rectangle.
         *
         * @param  g  Graphics component for this paint.
         */
        public void paint(Graphics g) {

            if (x > 0) {

                if (!complete) {
                    g.setColor(Color.yellow);
                } else {
                    g.setColor(Color.red);
                }

                g.drawRect(x, y, width, height);
            }
        }

        /**
         * Accessor to tell the glass pane that the rectangle drawing is complete. If so, sets the currentRectangle
         * appropriately.
         *
         * @param  comp  <code>true</code> if drawing is complete, <code>false</code> otherwise.
         */
        public void setComplete(boolean comp) {
            this.complete = comp;

            if (complete) {

                // the +1 accounts for the size of the line
                Point p = new Point(x + 1, y + 1);
                SwingUtilities.convertPointToScreen(p, this);

                // the -1 accounts for the size of the line
                Dimension d = new Dimension(width - 1, height - 1);
                currentRectangle = new Rectangle(p, d);
            }
        }

        /**
         * Accessor to set the height value of the painted rectangle.
         *
         * @param  h  Height value of painted rectangle.
         */
        public void setH(int h) {
            this.height = h;
        }

        /**
         * Accessor to set the width value of the painted rectangle.
         *
         * @param  w  Width value of painted rectangle.
         */
        public void setW(int w) {
            this.width = w;
        }

        /**
         * Accessor to set the x value of the painted rectangle.
         *
         * @param  x  X value of painted rectangle.
         */
        public void setX(int x) {
            this.x = x;
        }

        /**
         * Accessor to set the y value of the painted rectangle.
         *
         * @param  y  Y value of painted rectangle.
         */
        public void setY(int y) {
            this.y = y;
        }
    }


	@Override
	public void componentHidden(ComponentEvent e) {}

	/** Moves the currentRectangle to the location of the window on the screen to maintain the correct image */
	public void componentMoved(ComponentEvent event) {
		if(windowButton.isSelected()){
			ViewJFrameImage frame = (ViewJFrameImage) SwingUtilities.getRoot(event.getComponent());
            activeFrame = frame;
            Point p = new Point();
            p.x = 0;
            p.y = 0;
            SwingUtilities.convertPointToScreen(p, frame.getContentPane());
            p.x++; // must correct this slightly
            p.y++; // ""

            Dimension d = new Dimension();
            d.width = frame.getContentPane().getWidth() - 3; // the -3 is a correction
            d.height = frame.getContentPane().getHeight() - 3; // ""
            currentRectangle = new Rectangle(p, d);

		} else if(regionButton.isSelected()){
			currentRectangle = null;			
		}
	}

	/** Resizes the currentRectangle to the dimensions of the changed window */
	public void componentResized(ComponentEvent event) {
		
		if(windowButton.isSelected()){
			ViewJFrameImage frame = (ViewJFrameImage) SwingUtilities.getRoot(event.getComponent());
            activeFrame = frame;
            Point p = new Point();
            p.x = 0;
            p.y = 0;
            SwingUtilities.convertPointToScreen(p, frame.getContentPane());
            p.x++; // must correct this slightly
            p.y++; // ""

            Dimension d = new Dimension();
            d.width = frame.getContentPane().getWidth() - 3; // the -3 is a correction
            d.height = frame.getContentPane().getHeight() - 3; // ""
            currentRectangle = new Rectangle(p, d);

		} else if(regionButton.isSelected()){
			currentRectangle = null;
		}
	}

	@Override
	public void componentShown(ComponentEvent e) {}
	
//	public class ImageCopier extends TransferHandler implements ClipboardOwner{
//		
//		private JTextPane parent;
//		
//		public void lostOwnership(Clipboard clipboard, Transferable contents) {}
//		
//		public ImageCopier(JTextPane parent) {
//	        this.parent = parent;
//	    }
//		
//		public boolean canImport(JComponent description, DataFlavor[] transferFlavors) {
//			if(description.equals(parent)) {
//				for(int i=0; i<transferFlavors.length; i++) {
//	                if(!transferFlavors[i].equals(DataFlavor.imageFlavor)) {
//	                    return false;
//	                }
//	            }
//	            return true;
//			}
//			return false;
//			
//		}
//		
//		protected Transferable createTransferable(JComponent c) {
//			if(c.equals(parent)) {
//				return new ImageConverter();
//			}
//		}
//		
//		public void exportAsDrag(JComponent comp, InputEvent e, int action) {
//	         // TODO Auto-generated method stub
//	         super.exportAsDrag(comp, e, action);
//	    }
//		
//		 protected void exportDone(JComponent source, Transferable data, int action) {
//	         // TODO Auto-generated method stub
//	         super.exportDone(source, data, action);
//	     }
//
//	     /* (non-Javadoc)
//	      * @see javax.swing.TransferHandler#exportToClipboard(javax.swing.JComponent, java.awt.datatransfer.Clipboard, int)
//	      */
//	     public void exportToClipboard(JComponent comp, Clipboard clip, Image imagePix) throws IllegalStateException {
//	         try {
//	             if(comp.equals(parent)) {
//	            	 Transferable pic = new ImageConverter(imagePix);
//	                 clip.setContents(pic, null);
//	             }
//	         } catch(Exception e) {
//	             e.printStackTrace();
//	         }
//	     }
//
//	     /* (non-Javadoc)
//	      * @see javax.swing.TransferHandler#getSourceActions(javax.swing.JComponent)
//	      */
//	     @Override
//	     public int getSourceActions(JComponent c) {
//	         // TODO Auto-generated method stub
//	         return super.getSourceActions(c);
//	     }
//
//	     /* (non-Javadoc)
//	      * @see javax.swing.TransferHandler#importData(javax.swing.JComponent, java.awt.datatransfer.Transferable)
//	      */
//	     @Override
//	     public boolean importData(JComponent comp, Transferable t) {
//			return false;
//
//	     }
//
//	     /* (non-Javadoc)
//	      * @see javax.swing.TransferHandler#importData(javax.swing.TransferHandler.TransferSupport)
//	      */
//	     @Override
//	     public boolean importData(TransferSupport support) {
//	         // TODO Auto-generated method stub
//	         return super.importData(support);
//	     }
//	}
}
