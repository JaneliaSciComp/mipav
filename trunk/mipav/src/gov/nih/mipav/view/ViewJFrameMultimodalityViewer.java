package gov.nih.mipav.view;

import gov.nih.mipav.util.*;

import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.renderer.WildMagic.VOI.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.*;

import javax.imageio.ImageIO;
import javax.swing.*;

import javax.swing.event.*;

import WildMagic.LibFoundation.Mathematics.*;


public class ViewJFrameMultimodalityViewer extends ViewJFrameTriImage
		implements ItemListener, ChangeListener, KeyListener, MouseListener, MouseMotionListener, MouseWheelListener, VOIManagerInterfaceListener {

	
	
	// ~ Instance fields
	// ------------------------------------------------------------------------------------------------
	/** target image variables. */
	private JFileChooser imagesChooser = new JFileChooser();
	
	/** The main user interface. */
	private ViewUserInterface UI;

	private String[] imageNames = new String[4];
	private int imageNamesIndex = 0;
	private ModelImage[] images = new ModelImage[4];

	private ViewJFrameImage currentFrame;
	private ViewJFrameImage[] imageFrame = new ViewJFrameImage[4];
	private ViewJComponentEditImage[] imageComp = new ViewJComponentEditImage[4];
	private JScrollPane[] imageScroll = new JScrollPane[4];
	private Point origin;

	private JPanel[] quadImagePanel = new JPanel[4];
	private JSplitPane splitPaneUpper;
	private JSplitPane splitPaneLower;
	private JSplitPane splitPaneCenter;
	private int zOffset;
	private int image1Slice;
	private int zDim;
	
	
	
	// ~ Constructors
	// ---------------------------------------------------------------------------------------------------

	public ViewJFrameMultimodalityViewer(final ModelImage _imageA, ViewJFrameImage frame) {
		super(_imageA, null);
		currentFrame = frame;
		UI = ViewUserInterface.getReference();
		readMultlmodalImages();
		getFramesInfo();
		initLayout();
	}


	// ~ Methods
	// --------------------------------------------------------------------------------------------------------

	private void getFramesInfo() {
		
		int[] extents0 = images[0].getExtents();
		int[] extents1 = images[1].getExtents();
		int[] extents2 = images[2].getExtents();
		int[] extents3 = images[3].getExtents();
		
		int minZ1 = Math.min(extents0[2], extents1[2]);
		int minZ2 = Math.min(extents2[2], minZ1);
		zDim = Math.min(minZ2, extents3[2]);
		
		
	}
	
	private void initLayout() {

		final GridBagLayout gbLayout = new GridBagLayout();
		final GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.fill = GridBagConstraints.NONE;
		gbc.anchor = GridBagConstraints.CENTER;
		gbc.weightx = 0;
		gbc.weighty = 0;

		ImageIcon cornerImage;
		cornerImage = MipavUtil.getIcon("WhiteCircle_550.png");
		ImageIcon blackImage;
		blackImage = MipavUtil.getIcon("BlackCircle_550.png");

		JPanel leftPanel = new JPanel(new BorderLayout());
		leftPanel.setBackground(Color.black);
		JLabel label1 = new JLabel();
		label1.setBackground(Color.black);
		label1.setIcon(blackImage);
		JLabel label2 = new JLabel();
		label2.setBackground(Color.black);
		label2.setIcon(blackImage);
		leftPanel.add(label1, BorderLayout.NORTH);
		leftPanel.add(label2, BorderLayout.SOUTH);

		JPanel rightPanel = new JPanel(new BorderLayout());
		rightPanel.setBackground(Color.black);
		JLabel label3 = new JLabel();
		label3.setBackground(Color.black);
		label3.setIcon(blackImage);
		JLabel label4 = new JLabel();
		label4.setBackground(Color.black);
		label4.setIcon(blackImage);
		rightPanel.add(label3, BorderLayout.NORTH);
		rightPanel.add(label4, BorderLayout.SOUTH);

		JPanel topPanel = new JPanel(new BorderLayout());
		topPanel.setBackground(Color.black);
		JLabel label5 = new JLabel();
		label5.setBackground(Color.black);
		label5.setIcon(cornerImage);
		JLabel label6 = new JLabel();
		label6.setBackground(Color.black);
		label6.setIcon(cornerImage);
		topPanel.add(label5, BorderLayout.WEST);
		topPanel.add(label6, BorderLayout.EAST);

		JPanel lowerPanel = new JPanel(new BorderLayout());
		lowerPanel.setBackground(Color.black);
		JLabel label7 = new JLabel();
		label7.setBackground(Color.black);
		label7.setIcon(cornerImage);
		JLabel label8 = new JLabel();
		label8.setBackground(Color.black);
		label8.setIcon(cornerImage);
		lowerPanel.add(label7, BorderLayout.WEST);
		lowerPanel.add(label8, BorderLayout.EAST);

		int screenWidth = Toolkit.getDefaultToolkit().getScreenSize().width;
		int screenHeight = Toolkit.getDefaultToolkit().getScreenSize().height;
		// int actualFrameWidth = screenWidth - 160;
		// int actualFrameHeight = screenHeight - 158;

		int actualFrameWidth = screenWidth;
		int actualFrameHeight = screenHeight;

		int panelWidth = actualFrameHeight / 2;
		int panelHeight = actualFrameHeight / 2;

		int currentSlice0 = imageComp[0].getSlice();
		int currentSlice1 = imageComp[1].getSlice();
		// System.err.println("currentSlice1 = " + currentSlice1);
		image1Slice = currentSlice1;
		int currentSlice2 = imageComp[2].getSlice();
		int currentSlice3 = imageComp[3].getSlice();

		int compW = imageComp[0].getWidth();
		int compH = imageComp[0].getHeight();

		float zoomFactorX = panelWidth / compH;
		float zoomFactorY = panelHeight / compH;

		float zoomX0 = imageComp[0].getZoomX();
		float zoomY0 = imageComp[0].getZoomY();

		float zoomX = zoomX0 * zoomFactorX;
		float zoomY = zoomY0 * zoomFactorY;

		imageComp[0].setZoomExact(zoomY, zoomY);
		imageComp[0].show(0, currentSlice0, true);
		imageComp[1].setZoomExact(zoomY, zoomY);
		imageComp[1].show(0, currentSlice1, true);
		imageComp[2].setZoomExact(zoomY, zoomY);
		imageComp[2].show(0, currentSlice2, true);

		float zoomX3 = imageComp[3].getZoomX();
		float zoomY3 = imageComp[3].getZoomY();
		zoomX = zoomX3 * zoomFactorX;
		zoomY = zoomY3 * zoomFactorY;
		imageComp[3].setZoomExact(zoomY, zoomY);
		imageComp[3].show(0, currentSlice3, true);


		quadImagePanel[0] = new JPanel();
		quadImagePanel[0].setLayout(gbLayout);
		quadImagePanel[0].setBackground(Color.black);
		quadImagePanel[0].add(imageComp[0], gbc);
		
		
		imageScroll[0] = new JScrollPane(quadImagePanel[0],
	                ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		imageScroll[0].getVerticalScrollBar().setSize(new Dimension(2, 0));
		imageScroll[0].getHorizontalScrollBar().setSize(new Dimension(0, 2));

		quadImagePanel[1] = new JPanel();
		quadImagePanel[1].setLayout(gbLayout);
		quadImagePanel[1].setBackground(Color.black);
		quadImagePanel[1].add(imageComp[1], gbc);
		imageScroll[1] = new JScrollPane(quadImagePanel[1],
                ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		imageScroll[1].getVerticalScrollBar().setSize(new Dimension(2, 0));
		imageScroll[1].getHorizontalScrollBar().setSize(new Dimension(0, 2));
		
		quadImagePanel[2] = new JPanel();
		quadImagePanel[2].setLayout(gbLayout);
		quadImagePanel[2].setBackground(Color.black);
		quadImagePanel[2].add(imageComp[2], gbc);
		imageScroll[2] = new JScrollPane(quadImagePanel[2],
                ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		imageScroll[2].getVerticalScrollBar().setSize(new Dimension(2, 0));
		imageScroll[2].getHorizontalScrollBar().setSize(new Dimension(0, 2));
		
		quadImagePanel[3] = new JPanel();
		quadImagePanel[3].setLayout(gbLayout);
		quadImagePanel[3].setBackground(Color.black);
		quadImagePanel[3].add(imageComp[3], gbc);
		imageScroll[3] = new JScrollPane(quadImagePanel[3],
                ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		imageScroll[3].getVerticalScrollBar().setSize(new Dimension(2, 0));
		imageScroll[3].getHorizontalScrollBar().setSize(new Dimension(0, 2));

		JSplitPane splitPaneUpper = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, imageScroll[0], imageScroll[1]);
		splitPaneUpper.setDividerSize(2);
		splitPaneUpper.setDividerLocation(0.5);
		splitPaneUpper.setResizeWeight(0.5);
		splitPaneUpper.setOneTouchExpandable(false);
		splitPaneUpper.setEnabled(false);

		JSplitPane splitPaneLower = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, imageScroll[2], imageScroll[3]);
		splitPaneLower.setDividerSize(2);
		splitPaneLower.setDividerLocation(0.5);
		splitPaneLower.setResizeWeight(0.5);
		splitPaneLower.setOneTouchExpandable(false);
		splitPaneLower.setEnabled(false);

		JSplitPane splitPaneCenter = new JSplitPane(JSplitPane.VERTICAL_SPLIT, splitPaneUpper, splitPaneLower);
		splitPaneCenter.setDividerSize(2);
		splitPaneCenter.setDividerLocation(0.5);
		splitPaneCenter.setResizeWeight(0.5);
		splitPaneCenter.setEnabled(false);

		getContentPane().add(splitPaneCenter, BorderLayout.CENTER);

		getContentPane().add(leftPanel, BorderLayout.WEST);
		getContentPane().add(rightPanel, BorderLayout.EAST);
		getContentPane().add(topPanel, BorderLayout.NORTH);
		getContentPane().add(lowerPanel, BorderLayout.SOUTH);
        getContentPane().setName("MultiViewConentPane");
        
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setExtendedState(JFrame.MAXIMIZED_BOTH);
		setUndecorated(true);

		
		imageComp[0].addMouseWheelListener(this);
		imageComp[1].addMouseWheelListener(this);
		imageComp[2].addMouseWheelListener(this);
		imageComp[3].addMouseWheelListener(this);
		// addMouseListener(this);
		
		imageComp[0].addMouseListener(this);
		imageComp[1].addMouseListener(this);
		imageComp[2].addMouseListener(this);
		imageComp[3].addMouseListener(this);
		
		imageComp[0].addMouseMotionListener(this);
		imageComp[1].addMouseMotionListener(this);
		imageComp[2].addMouseMotionListener(this);
		imageComp[3].addMouseMotionListener(this);
		 
		addKeyListener(this);
		
		GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
		// GraphicsDevice gs = ge.getDefaultScreenDevice();
		// gs.setFullScreenWindow(this);
		GraphicsDevice[] gs = ge.getScreenDevices();
		gs[0].setFullScreenWindow(this);
		pack();
		this.validate();
		setSize(screenWidth, screenHeight);
	}
	
	void captureComponent(Component component) {
	    Rectangle rect = component.getBounds();
	 
	    try {
	        String format = "png";
	        String fileName = component.getName() + "." + format;
	        BufferedImage captureImage =
	                new BufferedImage(rect.width, rect.height,
	                                    BufferedImage.TYPE_INT_ARGB);
	        component.paint(captureImage.getGraphics());
	 
	        ImageIO.write(captureImage, format, new File(fileName));
	 
	        System.err.printf("The screenshot of %s was saved!", component.getName());
	    } catch (IOException ex) {
	        System.err.println(ex);
	    }
	}
	
	public void mouseWheelMoved(final MouseWheelEvent mouseWheelEvent) {
		
		    int wheelRotation = mouseWheelEvent.getWheelRotation();
	        int xCoord = mouseWheelEvent.getX();
	    	int yCoord = mouseWheelEvent.getY();
	    	
	    	
	    	int currentSlice0 = imageComp[0].getSlice();
			int currentSlice1 = imageComp[1].getSlice();
			// System.err.println("currentSlice1 = " + currentSlice1);
			int currentSlice2 = imageComp[2].getSlice();
			int currentSlice3 = imageComp[3].getSlice();
	    	
			int zDim1 = images[1].getExtents()[2];
		    // System.err.println("zDim1 = " + zDim1 + " zDim = " + zDim);
		    // System.err.println("zDim/zDim1 = " + (float)((float)zDim/(float)zDim1));
	         
	    	 if (((wheelRotation < 0) && (zOffset < zDim - 1)) ||  ((wheelRotation > 0) && (zOffset > 0))) {
	         	if (wheelRotation < 0) {
	         	    // Increment slice
	         		//  System.err.println("Mouse wheel moved UP");
	         	    zOffset++;
	         	   image1Slice++;
	             }
	         	else {
	         		// Decrement slice
	         		// System.err.println("Mouse wheel moved DOWN");
	             	zOffset--;
	             	image1Slice--;
	         	}
	    	 }
	    	 
	    	 // System.err.println("zOffset = " + zOffset);
	    	 
	    	 
	    	 if ( imageFrame[0].isActive() || imageFrame[2].isActive() || imageFrame[3].isActive()) {
	    		 currentSlice1 = (int)(((float)zOffset / (float)zDim) * zDim1);
	    		 imageComp[0].show(0, zOffset, true);
	    		 imageComp[1].show(0, currentSlice1, true);
	    		 imageComp[2].show(0, zOffset, true);
	    		 imageComp[3].show(0, zOffset, true);
	    	 } else if ( imageFrame[1].isActive() ) {
	    		 // System.err.println("image1Slice = " + image1Slice + " zDim1 = " + zDim1);
	    		 zOffset = (int)( (float)image1Slice / (float)zDim1 * zDim );  
	    		 // System.err.println("zOffset = " + zOffset + " zDim = " + zDim);
	    		 imageComp[0].show(0, zOffset, true);
	    		 imageComp[1].show(0, image1Slice, true);
	    		 imageComp[2].show(0, zOffset, true);
	    		 imageComp[3].show(0, zOffset, true);
	    	 }
	    	  
	    	
	}
	
	/**
	 * File chooser to select target image directory.
	 */
	private void readMultlmodalImages() {
		imagesChooser.setDialogTitle("Open Target Images");

		if (UI.getDefaultDirectory() != null) {
			final File file = new File(UI.getDefaultDirectory());

			if (file != null) {
				imagesChooser.setCurrentDirectory(file);
			}
		} else {
			imagesChooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
		}

		imagesChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
		final int returnValue = imagesChooser.showOpenDialog(UI.getMainFrame());

		if (returnValue == JFileChooser.APPROVE_OPTION) {
			String imageDirectory = String.valueOf(imagesChooser.getSelectedFile()) + File.separatorChar;
			// System.err.println("imageDirectory = " + imageDirectory);
			File fileDir = new File(imageDirectory);
			readImages(fileDir);

		} else {
			return;
		}

	}

	private void readImages(File dir) {
		int i;
		FileIO imageIO = null;
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (i = 0; i < children.length; i++) {
				traverse(new File(dir, children[i]));
			}

			try {
				// read target images
				imageIO = new FileIO();

				for (i = 0; i < imageNamesIndex; i++) {
					System.err.println(imageNames[i]);
					images[i] = imageIO.readImage(imageNames[i]);
					// new ViewJFrameImage(images[i]);
				}

				equalScaleImage();

				// for ( i = 0; i < imageNamesIndex; i++ ) {
				// new ViewJFrameImage(images[3]);
				// }

			} catch (Exception e) {
				e.printStackTrace();
			}

		}
	}

	public void equalScaleImage() {

		float newZoom;
		int currentSlice;
		int i;
		int compW;
		int compH;
		int frameW;
		int frameH;
		int diffW;
		int diffH;
		
		int screenWidth = Toolkit.getDefaultToolkit().getScreenSize().width;
	    int screenHeight = Toolkit.getDefaultToolkit().getScreenSize().height;
		
		for ( i = 0; i < 4; i++ ) {
			
			imageFrame[i] = new ViewJFrameImage(images[i]);
			// imageFrame[i].setVisible(false);
			imageFrame[i].setLocation(screenWidth, screenHeight);
			imageComp[i] = imageFrame[i].getComponentImage();
			
			if ( i == 3 ) {
				
				compW = imageComp[i].getWidth();
				compH = imageComp[i].getHeight();

				frameW = imageFrame[i].getWidth();
				frameH = imageFrame[i].getHeight();

				diffW = frameW - compW;
				diffH = frameH - compH;
				
				currentSlice = imageComp[3].getSlice();
				newZoom = 0.5f * imageComp[3].getZoomX();
				
			    imageComp[3].setZoomExact(newZoom, newZoom);
			    imageComp[3].show(0, currentSlice, true);

				imageFrame[3].setSize(compW / 2 + diffW, compH / 2 + diffH);
				imageFrame[3].updateImages();
			}
		}
		
		

	}

	private void traverse(File file) {
		String dirName = file.toString();
		// System.err.println(dirName);
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();

		if (dirName.substring(begin, end).startsWith("adc") && dirName.substring(begin, end).endsWith(".xml")) {
			imageNames[imageNamesIndex++] = file.toString();
		}

		if (dirName.substring(begin, end).startsWith("dce") && dirName.substring(begin, end).endsWith(".xml")) {
			imageNames[imageNamesIndex++] = file.toString();
		}

		if (dirName.substring(begin, end).startsWith("dwi") && dirName.substring(begin, end).endsWith(".xml")) {
			imageNames[imageNamesIndex++] = file.toString();
		}

		if (dirName.substring(begin, end).startsWith("t2w") && dirName.substring(begin, end).endsWith(".xml")) {
			imageNames[imageNamesIndex++] = file.toString();
		}

	}

	// ************************************************************************
	// **************************** Action Events *****************************
	// ************************************************************************

	/**
	 * Calls various methods depending on the action.
	 * 
	 * @param event
	 *            event that triggered function
	 */
	public void actionPerformed(final ActionEvent event) {}

	/**
	 * Should be called when window is closing to perform cleanup.
	 */
	public void disposeLocal() {

	}

		
	/**
	 * Sets the x coordinate of the point to be the center of the transformed
	 * image.
	 * 
	 * @return DOCUMENT ME!
	 */
	public int[] getCenter() {
		return volumeCenter;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManagerInterfaceListener
	 * #getCenterPt()
	 */
	public Vector3f getCenterPt() {
		return new Vector3f(volumeCenter[0], volumeCenter[1], volumeCenter[2]);
	}

	
	/**
	 * Accessor that returns the reference to imageA.
	 * 
	 * @return image
	 */
	public ModelImage getImageA() {
		return imageA;
		// if (triImage[ViewJFrameTriImage.AXIAL_A] != null) {
		// return triImage[ViewJFrameTriImage.AXIAL_A].getImageA();
		// } else {
		// return null;
		// }
	}

	/**
	 * Accessor that returns the reference to imageB.
	 * 
	 * @return imageB
	 */
	public ModelImage getImageB() {
		return imageB;
		// if (triImage[ViewJFrameTriImage.AXIAL_B] != null) {
		// return triImage[ViewJFrameTriImage.AXIAL_B].getImageB();
		// } else {
		// return null;
		// }
	}

	/**
	 * Gets the linked ViewJFrameTriImage.
	 * 
	 * @return linkedFrame
	 */
	public ViewJFrameTriImage getLinkedTriFrame() {
		return linkTriFrame;
	}

	/**
	 * Returns a reference to the ViewJFrameImage object that is the parent of
	 * this frame.
	 * 
	 * @return a reference to the ViewJFrameImage object that is the parent of
	 *         this frame
	 */
	public ViewJFrameImage getParentFrame() {
		return parentFrame;
	}

		/**
	 * Does setBorderPainted for the appropriate button.
	 * 
	 * @param event
	 *            Event that triggered this function
	 */
	public void itemStateChanged(final ItemEvent event) {}

	/**
	 * keyPressed event method for KeyListener.
	 * 
	 * @param e
	 *            KeyEvent
	 */
	public void keyPressed(final KeyEvent e) {
		// System.err.println("ViewJFrameTriImage keyPressed" );
		final int keyCode = e.getKeyCode();

		if ( keyCode == KeyEvent.VK_ESCAPE ) {
			System.err.println("escape key pressed");
			this.setVisible(false);
			this.dispose();
		} else if ( keyCode == KeyEvent.VK_F1) {
			System.err.println("F1 key pressed");
			captureComponent(getContentPane());
		}
		
		// pass the key bindings to the underlying image (plb)
		String command = null;
		final KeyStroke ks = KeyStroke.getKeyStrokeForEvent(e);

		command = Preferences.getShortcutCommand(ks);

		if (command != null) {
			parentFrame.actionPerformed(new ActionEvent(ks, 0, command));
		}
		

	}

	/**
	 * keyReleased event method for KeyListener.
	 * 
	 * @param e
	 *            KeyEvent
	 */
	public void keyReleased(final KeyEvent e) {

	}

	/**
	 * DOCUMENT ME!
	 * 
	 * @param e
	 *            DOCUMENT ME!
	 */
	public void keyTyped(final KeyEvent e) {
		// System.err.println("ViewJFrameTriImage keyTyped" );
	}

	/**
	 * DOCUMENT ME!
	 * 
	 * @param event
	 *            DOCUMENT ME!
	 */
	public void mouseClicked(final MouseEvent event) {

	    if (event.getButton() == MouseEvent.BUTTON3) {

			if (event.getSource() instanceof AbstractButton) {
				final AbstractButton btnSource = (AbstractButton) event.getSource();

				if (btnSource.getActionCommand().equals("MagImage") || btnSource.getActionCommand().equals("UnMagImage")
						|| btnSource.getActionCommand().equals("IndivMagImage")
						|| btnSource.getActionCommand().equals("IndivMinImage")) {

					handleZoomPopupMenu(btnSource, event);

					return;
				}
			}
		}

	}

	/**
	 * DOCUMENT ME!
	 * 
	 * @param event
	 *            DOCUMENT ME!
	 */
	public void mouseEntered(final MouseEvent event) {
		

	}

	/**
	 * DOCUMENT ME!
	 * 
	 * @param event
	 *            DOCUMENT ME!
	 */
	public void mouseExited(final MouseEvent event) {
  
	}
	
	public void mouseMoved(MouseEvent evt) {
		
	}
	
	public synchronized void mouseDragged(MouseEvent e) {

		synchronized (this) {
			if (e.isAltDown()) {
				int deltaX = origin.x - e.getX();
				int deltaY = origin.y - e.getY();
				JViewport viewPort0 = imageScroll[0].getViewport();
				if (viewPort0 != null) {
					Rectangle view = viewPort0.getViewRect();
					view.x += deltaX;
					view.y += deltaY;

					imageComp[0].scrollRectToVisible(view);
				}

				JViewport viewPort1 = imageScroll[1].getViewport();
				if (viewPort1 != null) {
					Rectangle view = viewPort1.getViewRect();
					view.x += deltaX;
					view.y += deltaY;

					imageComp[1].scrollRectToVisible(view);
				}

				JViewport viewPort2 = imageScroll[2].getViewport();
				if (viewPort2 != null) {
					Rectangle view = viewPort2.getViewRect();
					view.x += deltaX;
					view.y += deltaY;

					imageComp[2].scrollRectToVisible(view);
				}

				JViewport viewPort3 = imageScroll[3].getViewport();
				if (viewPort3 != null) {
					Rectangle view = viewPort3.getViewRect();
					view.x += deltaX;
					view.y += deltaY;

					imageComp[3].scrollRectToVisible(view);
				}
			}
		}
	}
	
	

	/**
	 * DOCUMENT ME!
	 * 
	 * @param event
	 *            DOCUMENT ME!
	 */
	public synchronized void mousePressed(final MouseEvent event) {

		synchronized (this) {
			origin = new Point(event.getPoint());

			// System.err.println("in mouse pressed");
			int currentSlice0 = imageComp[0].getSlice();
			int currentSlice1 = imageComp[1].getSlice();
			int currentSlice2 = imageComp[2].getSlice();
			int currentSlice3 = imageComp[3].getSlice();

			if (event.getButton() == MouseEvent.BUTTON1 && event.isShiftDown()) {

				imageScroll[0].setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
				imageScroll[0].setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
				imageScroll[1].setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
				imageScroll[1].setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
				imageScroll[2].setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
				imageScroll[2].setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
				imageScroll[3].setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
				imageScroll[3].setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

				// quad 0
				float oldZoom0 = imageComp[0].getZoomX();
				float newZoom0 = 1;
				if (imageComp[0].getZoomX() < 1.0f) {
					newZoom0 = 2.0f * imageComp[0].getZoomX();
				} else {
					newZoom0 = imageComp[0].getZoomX() + 1.0f;
				}
				imageComp[0].setZoom(newZoom0, newZoom0);

				Vector2f oldCrosshairPoint0 = new Vector2f(event.getX(), event.getY());

				if (oldCrosshairPoint0 != null) {
					final int newX = MipavMath.round((oldCrosshairPoint0.X * newZoom0) / oldZoom0);
					final int newY = MipavMath.round((oldCrosshairPoint0.Y * newZoom0) / oldZoom0);
					imageComp[0].setCenter(new Vector3f(newX, newY, currentSlice0));
					adjustScrollbars(newX, newY, imageScroll[0]);
				}
				// quad 1
				float oldZoom1 = imageComp[1].getZoomX();
				float newZoom1 = 1;
				if (imageComp[1].getZoomX() < 1.0f) {
					newZoom1 = 2.0f * imageComp[1].getZoomX();
				} else {
					newZoom1 = imageComp[1].getZoomX() + 1.0f;
				}
				imageComp[1].setZoom(newZoom1, newZoom1);

				Vector2f oldCrosshairPoint1 = new Vector2f(event.getX(), event.getY());

				if (oldCrosshairPoint1 != null) {
					int newX = MipavMath.round((oldCrosshairPoint1.X * newZoom1) / oldZoom1);
					int newY = MipavMath.round((oldCrosshairPoint1.Y * newZoom1) / oldZoom1);
					imageComp[1].setCenter(new Vector3f(newX, newY, currentSlice1));
					adjustScrollbars(newX, newY, imageScroll[1]);
				}
				// quad 2
				float oldZoom2 = imageComp[2].getZoomX();
				float newZoom2 = 1;
				if (imageComp[2].getZoomX() < 1.0f) {
					newZoom2 = 2.0f * imageComp[2].getZoomX();
				} else {
					newZoom2 = imageComp[2].getZoomX() + 1.0f;
				}
				imageComp[2].setZoom(newZoom2, newZoom2);

				Vector2f oldCrosshairPoint2 = new Vector2f(event.getX(), event.getY());

				if (oldCrosshairPoint2 != null) {
					int newX = MipavMath.round((oldCrosshairPoint2.X * newZoom2) / oldZoom2);
					int newY = MipavMath.round((oldCrosshairPoint2.Y * newZoom2) / oldZoom2);
					imageComp[2].setCenter(new Vector3f(newX, newY, currentSlice2));
					adjustScrollbars(newX, newY, imageScroll[2]);
				}
				// quad3
				float oldZoom3 = imageComp[3].getZoomX();
				float newZoom3 = 1;
				if (imageComp[3].getZoomX() < 1.0f) {
					newZoom3 = 2.0f * imageComp[3].getZoomX();
				} else {
					newZoom3 = imageComp[3].getZoomX() + 1.0f;
				}
				imageComp[3].setZoom(newZoom3, newZoom3);

				Vector2f oldCrosshairPoint3 = new Vector2f(event.getX(), event.getY());

				if (oldCrosshairPoint3 != null) {
					int newX = MipavMath.round((oldCrosshairPoint3.X * newZoom3) / oldZoom3);
					int newY = MipavMath.round((oldCrosshairPoint3.Y * newZoom3) / oldZoom3);
					imageComp[3].setCenter(new Vector3f(newX, newY, currentSlice3));
					adjustScrollbars(newX, newY, imageScroll[3]);
				}

			} else if (event.getButton() == MouseEvent.BUTTON1 && event.isControlDown()) {
				imageScroll[0].setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
				imageScroll[0].setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
				imageScroll[1].setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
				imageScroll[1].setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
				imageScroll[2].setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
				imageScroll[2].setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
				imageScroll[3].setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
				imageScroll[3].setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

				// quad 0
				float oldZoom0 = imageComp[0].getZoomX();
				float newZoom0 = 1;
				if (imageComp[0].getZoomX() > 1.0f) {
					newZoom0 = imageComp[0].getZoomX() - 1.0f;
				} else {
					newZoom0 = 0.5f * imageComp[0].getZoomX();
				}
				imageComp[0].mouseExited(event);
				imageComp[0].setZoom(newZoom0, newZoom0);
				
				Vector2f oldCrosshairPoint0 = new Vector2f(event.getX(), event.getY());

				if (oldCrosshairPoint0 != null) {
					final int newX = MipavMath.round((oldCrosshairPoint0.X * newZoom0) / oldZoom0);
					final int newY = MipavMath.round((oldCrosshairPoint0.Y * newZoom0) / oldZoom0);
					imageComp[0].setCenter(new Vector3f(newX, newY, currentSlice0));
					adjustScrollbars(newX, newY, imageScroll[0]);
				}
				// quad 1
				float oldZoom1 = imageComp[1].getZoomX();
				float newZoom1 = 1;
				if (imageComp[1].getZoomX() > 1.0f) {
					newZoom1 = imageComp[1].getZoomX() - 1.0f;
				} else {
					newZoom1 = 0.5f * imageComp[1].getZoomX();
				}
				imageComp[1].mouseExited(event);
				imageComp[1].setZoom(newZoom1, newZoom1);

				Vector2f oldCrosshairPoint1 = new Vector2f(event.getX(), event.getY());

				if (oldCrosshairPoint1 != null) {
					int newX = MipavMath.round((oldCrosshairPoint1.X * newZoom1) / oldZoom1);
					int newY = MipavMath.round((oldCrosshairPoint1.Y * newZoom1) / oldZoom1);
					imageComp[1].setCenter(new Vector3f(newX, newY, currentSlice1));
					adjustScrollbars(newX, newY, imageScroll[1]);
				}
				// quad 2
				float oldZoom2 = imageComp[2].getZoomX();
				float newZoom2 = 1;
				if (imageComp[2].getZoomX() > 1.0f) {
					newZoom2 = imageComp[2].getZoomX() - 1.0f;
				} else {
					newZoom2 = 0.5f * imageComp[2].getZoomX();
				}
				imageComp[2].mouseExited(event);
				imageComp[2].setZoom(newZoom2, newZoom2);

				Vector2f oldCrosshairPoint2 = new Vector2f(event.getX(), event.getY());

				if (oldCrosshairPoint2 != null) {
					int newX = MipavMath.round((oldCrosshairPoint2.X * newZoom2) / oldZoom2);
					int newY = MipavMath.round((oldCrosshairPoint2.Y * newZoom2) / oldZoom2);
					imageComp[2].setCenter(new Vector3f(newX, newY, currentSlice2));
					adjustScrollbars(newX, newY, imageScroll[2]);
				}
				// quad3
				float oldZoom3 = imageComp[3].getZoomX();
				float newZoom3 = 1;
				if (imageComp[3].getZoomX() > 1.0f) {
					newZoom3 = imageComp[3].getZoomX() - 1.0f;
				} else {
					newZoom3 = 0.5f * imageComp[3].getZoomX();
				}
				imageComp[3].mouseExited(event);
				imageComp[3].setZoom(newZoom3, newZoom3);

				Vector2f oldCrosshairPoint3 = new Vector2f(event.getX(), event.getY());

				if (oldCrosshairPoint3 != null) {
					int newX = MipavMath.round((oldCrosshairPoint3.X * newZoom3) / oldZoom3);
					int newY = MipavMath.round((oldCrosshairPoint3.Y * newZoom3) / oldZoom3);
					imageComp[3].setCenter(new Vector3f(newX, newY, currentSlice3));
					adjustScrollbars(newX, newY, imageScroll[3]);
				}

			}

		}
	}

	/**
	 * DOCUMENT ME!
	 * 
	 * @param event
	 *            DOCUMENT ME!
	 */
	public void mouseReleased(final MouseEvent event) {
		if ( event.getButton() == MouseEvent.BUTTON1 && event.isShiftDown() || 
				 event.getButton() == MouseEvent.BUTTON1 && event.isControlDown() ||
				 event.getButton() == MouseEvent.BUTTON1 && event.isAltDown() ) {
			imageScroll[0].setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
			imageScroll[0].setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
			imageScroll[1].setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
			imageScroll[1].setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
			imageScroll[2].setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
			imageScroll[2].setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
			imageScroll[3].setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
			imageScroll[3].setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		}
	}

	
	/**
	 * Set the active image for drawing VOIs. VOIs are only drawn in the active
	 * image. In addition, algorithms are executed on the active window.
	 * 
	 * @param active
	 *            ViewJComponentBase.IMAGE_A or ViewJComponentBase.IMAGE_B
	 */
	public void setActiveImage(final int active) {}

	

	/**
	 * sets the crosshair positions and slices for each of the triImages. The
	 * inputs are in FileCoordinates, and are passed to the triImages in
	 * FileCoordinates. Each triImage converts from FileCoordinates to the local
	 * PatientCoordinate space, based on the triImage orientation
	 * (FileInfoBase.AXIAL, FileInfoBase.CORONAL, FileInfoBase.SAGITTAL).
	 * 
	 * @param i
	 *            model space coordinate
	 * @param j
	 *            model space coordinate
	 * @param k
	 *            model space coordinate
	 */
	public void setCenter(final int i, final int j, final int k) {
		setCenter(i, j, k, true);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManagerInterfaceListener
	 * #setCenter(WildMagic.LibFoundation.Mathematics.Vector3f)
	 */
	public void setCenter(final Vector3f kCenter) {
		setCenter((int) kCenter.X, (int) kCenter.Y, (int) kCenter.Z, true);
	}

	/**
	 * sets the crosshair positions and slices for each of the triImages. The
	 * inputs are in FileCoordinates, and are passed to the triImages in
	 * FileCoordinates. Each triImage converts from FileCoordinates to the local
	 * PatientCoordinate space, based on the triImage orientation
	 * (FileInfoBase.AXIAL, FileInfoBase.CORONAL, FileInfoBase.SAGITTAL).
	 * 
	 * @param i
	 *            model space coordinate
	 * @param j
	 *            model space coordinate
	 * @param k
	 *            model space coordinate
	 * @param checkLinkedScroll
	 *            (boolean telling whether to look for linked images to sync
	 *            scroll... necessary to avoid infinite loop)
	 */
	public void setCenter(int i, int j, int k, final boolean checkLinkedScroll) {
		i = (i < 0) ? 0 : ((i >= extents[0]) ? (extents[0] - 1) : i);
		j = (j < 0) ? 0 : ((j >= extents[1]) ? (extents[1] - 1) : j);
		k = (k < 0) ? 0 : ((k >= extents[2]) ? (extents[2] - 1) : k);

		setVolumeCenter(i, j, k);

		for (int image = 0; image < triImage.length; image++) {

			if (triImage[image] != null) {
				triImage[image].setCenter(i, j, k);
			}
		}
		
		fireCoordinateChange(i, j, k);
		setPositionLabels(i, j, k);
		updateImages(false);
	}

	


	/**
	 * Controls whether or not the images/VOIs of the frame can be modified.
	 * 
	 * @param flag
	 *            if true the image/VOIs can be modified; if false image/VOIs
	 *            can NOT be modified
	 */
	public void setEnabled(final boolean flag) {

		for (int i = 0; i < triImage.length; i++) {

			if (triImage[i] != null) {
				triImage[i].setEnabled(flag);
			}
		}
		if (voiManager != null) {
			voiManager.setEnabled(flag);
		}
	}

	/**
	 * Changes imageA to a new model image reference. Swaps the references in
	 * the frame and all the component images.
	 * 
	 * @param image
	 *            the new image to use
	 */
	public void setImageA(final ModelImage image) {
		super.setImageA(image);

	

		// Get all frames
		final Vector<ViewImageUpdateInterface> frameList = image.getImageFrameVector();

		if (frameList == null) {
			return;
		}

		for (int i = 0; i < frameList.size(); i++) {

			if (((ViewJFrameBase) frameList.elementAt(i)) != this) {
				((ViewJFrameBase) frameList.elementAt(i)).setImageA(image);
			}
		}

		image.setImageOrder(ModelImage.IMAGE_A);

		image.addImageDisplayListener(this);

		setActiveImage(ViewJComponentBase.IMAGE_A);
	}

	/**
	 * Accessor that sets the reference to imageB. Includes changing the frame's
	 * reference and the references the components keep.
	 * 
	 * @param _imageB
	 *            image to set the frame to
	 */
	public void setImageB(final ModelImage _imageB) {
	
	}

	/**
	 * Accessor that sets the reference to imageB. Includes changing the frame's
	 * reference and the references the components keep. Unlike
	 * <code>setImageB(ModelImage)</code> this method matches the functionality
	 * of <code>setImageA(ModelImage)</code>
	 * 
	 * @param _imageB
	 *            image to set the frame to
	 */
	public void setImageB(final ModelImage _imageB, boolean disposeOldB) {}

	

	/**
	 * Accessor that sets the LUT.
	 * 
	 * @param LUT
	 *            the LUT
	 */
	public void setLUTa(final ModelLUT LUT) {

	
	}

	/**
	 * Accessor that sets the LUT.
	 * 
	 * @param LUT
	 *            the LUT
	 */
	public void setLUTb(final ModelLUT LUT) {

	
	}

	/**
	 * When switching the active image, take the paintBitmap of the previous
	 * active image as the paintBitmap of the new active image Currenlty unused.
	 * 
	 * @param paintBitmapSwitch
	 *            if true don't do a getMask on the new actve image
	 */
	public void setPaintBitmapSwitch(final boolean paintBitmapSwitch) {
	}

	/**
	 * Sets the color of the paint.
	 * 
	 * @param color
	 *            Color the desired color of the paint
	 */
	public void setPaintColor(final Color color) {
		this.color = color;

		colorPaintButton.setBackground(color);

		for (int i = 0; i < triImage.length; i++) {

			if (triImage[i] != null) {
				triImage[i].getActiveImage().notifyImageDisplayListeners(null, true);
			}
		}
	}

	/**
	 * Sets the labels which show the absolute position within the image volume
	 * and the patient position.
	 * 
	 * @param x
	 *            the x volume coordinate
	 * @param y
	 *            the y volume coordinate
	 * @param z
	 *            the z volume coordinate
	 */
	public void setPositionLabels(final int x, final int y, final int z) {
		setAbsPositionLabels(new Vector3f(x, y, z));
		setScannerPosition(new Vector3f(x, y, z));

		if (showTalairachPosition) {
			setTalairachPositionLabels(x, y, z);
		}

		currentAbsolutePositionLabels = new Point3D(x, y, z);

		if (linkTriFrame != null) {
			linkTriFrame.setSlicesFromFrame(x, y, z);
		}

		// BEN

	}

	/**
	 * Sets the RGB table for ARGB image A.
	 * 
	 * @param RGBT
	 *            the new RGB transfer functions for imageA
	 */
	public void setRGBTA(final ModelRGB RGBT) {

	}

	/**
	 * Sets the RGB table for image B.
	 * 
	 * @param RGBT
	 *            the new RGB transfer functions for imageB
	 */
	public void setRGBTB(final ModelRGB RGBT) {

	}

	/**
	 * Sets whether the linking button should be set for this image, implies
	 * that this image will be linked to another tri-frame when true
	 */

	public void setLinkButtonSelected(final boolean selected) {
		scrollButton.setSelected(selected);
	}

	/**
	 * Does nothing.
	 * 
	 * @param slice
	 *            the slice to show
	 */
	public void setSlice(final int slice) {
	}

	/**
	 * Sets the slice index for each plane in the frame and components. Should
	 * be zero indexed.
	 * 
	 * @param x
	 *            slice index in the patient
	 * @param y
	 *            slice index in the patient
	 * @param z
	 *            slice index in the patient
	 */
	public void setSlices(final int x, final int y, final int z) {
		setCenter(x, y, z);
	}

	/**
	 * Called from the &quot;normal&quot; image component, sets the slices for
	 * the tri planar view to display. Parameters are in terms of the image
	 * volume and so must be converted.
	 * 
	 * @param x
	 *            X Slice of image.
	 * @param y
	 *            Y Slice of image.
	 * @param z
	 *            Z Slice of image.
	 */

	public void setSlicesFromFrame(final int x, final int y, final int z) {
		final Vector3f inPoint = new Vector3f();
		inPoint.X = x;
		inPoint.Y = y;
		inPoint.Z = z;

		// x, y, z passed in from ViewJComponentEditImage.mouseReleased() are
		// already in image volume space
		setCenter(x, y, z);
		setPositionLabels(x, y, z);
	}

	/**
	 * Sets the text to display in the Talairach voxel label. This text has a
	 * format corresponding to the Talairach grid. Example: "AcL3" - an full
	 * explanation of what the text means is beyond the scope of this comment.
	 * 
	 * @param newLabelText
	 *            the text to display in the Talairach voxel label
	 */
	// public void setTalairachVoxelLabelText(String newLabelText) {
	// talairachVoxelLabel.setText(newLabelText);
	// }
	/**
	 * Sets the slice to be displayed and updates title frame.
	 * 
	 * @param slice
	 *            indicates image time-slice (4th dimension) to be displayed
	 */
	public void setTimeSlice(final int slice) {

	}

	/**
	 * Sets the slice to be displayed and updates title frame.
	 * 
	 * @param slice
	 *            indicates image time-slice (4th dimension) to be displayed
	 * @param checkedLinkedScroll
	 *            whether corresponding tri-frames should also be scrolled
	 */
	public void setTimeSlice(final int slice, final boolean checkedLinkedScroll) {}

	/**
	 * Sets the title bar for the tri-image frame. Called for initialization and
	 * updating. Displays time series location for 4D volumes.
	 */
	public void setTitle() {}

	/**
	 * Sets the traverse button to selected.
	 */
	public void setTraverseButton() {}

	/**
	 * Sets the x coordinate of the point to be the center of the transformed
	 * image.
	 * 
	 * @param newVolumeCenter
	 *            The x coordinate of the center.
	 */
	public void setVolumeCenter(final Point3D newVolumeCenter) {
		volumeCenter[0] = newVolumeCenter.x;
		volumeCenter[1] = newVolumeCenter.y;
		volumeCenter[2] = newVolumeCenter.z;
	}

	/**
	 * Sets the x coordinate of the point to be the center of the transformed
	 * image.
	 * 
	 * @param x
	 *            The x coordinate of the center.
	 * @param y
	 *            DOCUMENT ME!
	 * @param z
	 *            DOCUMENT ME!
	 */
	public void setVolumeCenter(final int x, final int y, final int z) {
		volumeCenter[0] = x;
		volumeCenter[1] = y;
		volumeCenter[2] = z;
	}


	/**
	 * Sets values based on knob along time slider.
	 * 
	 * @param e
	 *            Event that triggered this function
	 */
	public void stateChanged(final ChangeEvent e) {}

	/**
	 * Do nothing - required by ViewJFrameBase.
	 * 
	 * @return always false
	 */
	public boolean updateImageExtents() {
		return false;
	}

	/**
	 * This methods calls the componentImage's update method to redraw the
	 * screen.
	 * 
	 * @return boolean confirming successful update
	 */
	public boolean updateImages() {
		return updateImages(null, null, true, -1);
	}

	/**
	 * This methods calls the componentImage's update method to redraw the
	 * screen. Without LUT changes.
	 * 
	 * @param forceShow
	 *            forces show to reimport image and calc. java image
	 * 
	 * @return boolean confirming successful update
	 */
	public boolean updateImages(final boolean forceShow) {
		return updateImages(null, null, forceShow, -1);
	}

	/**
	 * This methods calls the componentImage's update method to redraw the
	 * screen. Without LUT changes.
	 * 
	 * @param forceShow
	 *            forces show to reimport image and calc. java image
	 * @param interpMode
	 * 
	 * @return boolean confirming successful update
	 */
	public boolean updateImages(final boolean forceShow, final int interpMode) {
		return updateImages(null, null, forceShow, interpMode);
	}

	static int v = 0;

	/**
	 * This methods calls the componentImage's update method to redraw the
	 * screen.
	 * 
	 * @param LUTa
	 *            LUT used to update imageA
	 * @param LUTb
	 *            LUT used to update imageB
	 * @param forceShow
	 *            forces show to reimport image and calc. java image
	 * @param interpMode
	 *            image interpolation method (Nearest or Smooth)
	 * 
	 * @return boolean confirming successful update
	 */
	public boolean updateImages(final ModelLUT LUTa, final ModelLUT LUTb, final boolean forceShow,
			final int interpMode) {

		return true;
	}

	/**
	 * DOCUMENT ME!
	 * 
	 * @param triImage
	 *            DOCUMENT ME!
	 */
	public void updateImageSubset(final ViewJComponentTriImage triImage) {}

	/**
	 * Updates the VOI ID for the three component images.
	 * 
	 * @param voiID
	 *            New VOI ID. public void updatevoiID(int voiID) {
	 * 
	 *            for (int i = 0; i < MAX_TRI_IMAGES; i++) {
	 * 
	 *            if (triImage[i] != null) {
	 *            triImage[i].getVOIHandler().setVOI_ID(voiID); } } }
	 */

	/**
	 * Closes window and disposes of frame and component.
	 * 
	 * @param event
	 *            Event that triggered function
	 */
	public void windowClosing(final WindowEvent event) {

		if (volumePositionFrame != null) {
			volumePositionFrame.dispose();
			volumePositionFrame = null;
		}

		close();
		this.disposeLocal();
	}

	public void windowOpened(final WindowEvent event) {
		// setOldLayout(oldLayout);
	}

	/**
	 * Builds the active image panel for choosing which image (A, B, or BOTH) to
	 * perform operations on.
	 */
	protected void buildActiveImagePanel() {}

	/**
	 * DOCUMENT ME!
	 * 
	 * @throws Throwable
	 *             DOCUMENT ME!
	 */
	protected void finalize() throws Throwable {
		disposeLocal();
		super.finalize();
	}


	/**
	 * DOCUMENT ME!
	 * 
	 * @param x
	 *            DOCUMENT ME!
	 * @param y
	 *            DOCUMENT ME!
	 * @param scrollPane
	 *            DOCUMENT ME!
	 */
	private void adjustScrollbars(final int x, final int y, final JScrollPane scrollPane) {

		if (scrollPane == null) {
			return;
		}

		final JViewport viewport = scrollPane.getViewport();

		if (viewport == null) {
			return;
		}

		final Dimension extentSize = viewport.getExtentSize();

		if (extentSize == null) {
			return;
		}

		final int scrollPaneX = extentSize.width / 2;
		final int scrollPaneY = extentSize.height / 2;

		final Runnable adjustScrollbarsAWTEvent = new Runnable() {
			public void run() {
				scrollPane.getHorizontalScrollBar().setValue(x - scrollPaneX);
				scrollPane.getVerticalScrollBar().setValue(y - scrollPaneY);
			}
		};
		SwingUtilities.invokeLater(adjustScrollbarsAWTEvent);
	}


	/**
	 * this method will zoom in a particular frame.
	 * 
	 * @param frame
	 *            frame the frame number
	 */
	protected void zoomInFrame(final int frame) {
		final float oldZoom = triImage[frame].getZoomX();

		float newZoom = 1;

		if ((Preferences.is(Preferences.PREF_ZOOM_LINEAR)) && (triImage[frame] != null)) {
			if (triImage[frame].getZoomX() < 1.0f) {
				newZoom = 2.0f * triImage[frame].getZoomX();
			} else {
				newZoom = triImage[frame].getZoomX() + 1.0f;
			}
		} else if (triImage[frame] != null) // zoomMode ==
											// ViewJComponentEditImage.EXPONENTIAL
		{
			newZoom = 2.0f * triImage[frame].getZoomX();

		}

		if (triImage[frame] != null) {
			triImage[frame].setZoom(newZoom, newZoom);

			final Vector2f oldCrosshairPoint = triImage[frame].getCrosshairPoint();

			if (oldCrosshairPoint != null) {
				final int newX = MipavMath.round((oldCrosshairPoint.X * newZoom) / oldZoom);
				final int newY = MipavMath.round((oldCrosshairPoint.Y * newZoom) / oldZoom);

				triImage[frame].updateCrosshairPosition(newX, newY);

				adjustScrollbars(newX, newY, scrollPane[frame]);
			}
		}

		validate();
		updateImages(true);

	}

	/**
	 * this method will zoom out a particular frame.
	 * 
	 * @param frame
	 *            frame the frame number
	 */
	protected void zoomOutFrame(final int frame) {
		final float oldZoom = triImage[frame].getZoomX();

		float newZoom = 1;

		if ((Preferences.is(Preferences.PREF_ZOOM_LINEAR)) && (triImage[frame].getZoomX() > 1.0f)) {

			// linear zoom is prevented if getZoomX() <= 1.0
			newZoom = triImage[frame].getZoomX() - 1.0f;
		} else {
			newZoom = 0.5f * triImage[frame].getZoomX();
		}

		if (triImage[frame] != null) {
			triImage[frame].setZoom(newZoom, newZoom);

			final Vector2f oldCrosshairPoint = triImage[frame].getCrosshairPoint();

			if (oldCrosshairPoint != null) {
				final int newX = MipavMath.round((oldCrosshairPoint.X * newZoom) / oldZoom);
				final int newY = MipavMath.round((oldCrosshairPoint.Y * newZoom) / oldZoom);

				triImage[frame].updateCrosshairPosition(newX, newY);

				adjustScrollbars(newX, newY, scrollPane[frame]);
			}
		}

		validate();
		updateImages(true);
	}


	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManagerInterfaceListener
	 * #getFrame()
	 */
	public JFrame getFrame() {
		return this;
	}



	/*
	 * (non-Javadoc)
	 * 
	 * @see java.awt.Window#setCursor(java.awt.Cursor)
	 */
	public void setCursor(final Cursor kCursor) {
		for (int i = 0; i < triImage.length; i++) {
			if (triImage[i] != null) {
				triImage[i].setCursor(kCursor);
			}
		}
	}

	

}
