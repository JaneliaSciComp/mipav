package gov.nih.mipav.view;

import gov.nih.mipav.util.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmConvert3Dto4D;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.view.ViewJFrameMultimodalitySingleViewer.MousePopupListener;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.renderer.WildMagic.VOI.*;

import java.awt.*;
import java.awt.event.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
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

	private String[] imageNames = new String[8];
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
	
	private JLabel label5;
	
	private JPopupMenu popup = new JPopupMenu();
	
	private int imageActiveIndex;
	
	// ~ Constructors
	// ---------------------------------------------------------------------------------------------------

	public ViewJFrameMultimodalityViewer(final ModelImage _imageA, ViewJFrameImage frame) {
		super(_imageA, null);
		currentFrame = frame;
		UI = ViewUserInterface.getReference();
		readMultlmodalImages();
		// getFramesInfo();
		initLayout();
		addPopup();
		fitsToScreen();
		setRecordingMode();
		startRecording();
	}


	
	// ~ Methods
	// --------------------------------------------------------------------------------------------------------

	public void setRecordingMode() {
		for ( int i = 0; i < 4; i++ ) {
			int []extents = images[i].getExtents();
			String refPtsLocation = "0, 0," + (extents[0]-1) + ", " + "0" + ", " + "0, " + (extents[1]-1) + ", " + (extents[0]-1) + ", " + (extents[1]-1) + ", ";
			imageComp[i].setEyetrackerRecordMode(ViewJComponentEditImage.MultiFrameEyetrackerMode);
			imageComp[i].setFullScreenModeLocation(refPtsLocation);
		}
	}
	
	public void addPopup() {
		   JMenuItem m = new JMenuItem("Measure");
		    m.addActionListener(this);
		    popup.add(m);
		  
		    MousePopupListener pl = new MousePopupListener();
		    addMouseListener(pl);
		   
	}
	
	class MousePopupListener extends MouseAdapter {
		public void mousePressed(MouseEvent e) {
			checkPopup(e);
		}

		public void mouseClicked(MouseEvent e) {
			checkPopup(e);
		}

		public void mouseReleased(MouseEvent e) {
			checkPopup(e);
		}

		private void checkPopup(MouseEvent e) {
			if (e.isPopupTrigger()) {
				popup.show(getContentPane(), e.getX(), e.getY());
			}
		}
	}
	
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
		cornerImage = MipavUtil.getIcon("WhiteCircle.png");
		ImageIcon blackImage;
		blackImage = MipavUtil.getIcon("BlackCircle.png");

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
		label5 = new JLabel();
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
		
		// GraphicsEnvironment ge = GraphicsEnvironment.getLocalGraphicsEnvironment();
		// GraphicsDevice gs = ge.getDefaultScreenDevice();
		// gs.setFullScreenWindow(this);
		// GraphicsDevice[] gs = ge.getScreenDevices();
		// gs[0].setFullScreenWindow(this);

		setSize(screenWidth, screenHeight);
		setMinimumSize(getSize());
		setVisible(true);
		setResizable(false);
		
		pack();
		this.validate();

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
	       
	    	int currentSlice = imageComp[imageActiveIndex].getSlice();
	    	
			int currentSlice3 = imageComp[3].getSlice();
	    	
			int zDim0 = images[0].getExtents()[2];
			int zDim1 = images[1].getExtents()[2];
			int zDim2 = images[2].getExtents()[2];
			int zDim3 = images[3].getExtents()[2];
			
			int zTimeDim = images[3].getExtents()[3];
			// System.err.println("zTimeDim = " + zTimeDim);
			
			int tCurrentSlice = imageComp[3].getTimeSlice();
			   
			if ( mouseWheelEvent.isAltDown() ) {
				 if (wheelRotation < 0) {
					 
					 if ( imageFrame[3].isActive() ) {
						 tCurrentSlice++;
		         		 // System.err.println("tDim = " + tCurrentSlice);
		         	 } 
				 } else {
				 
					if ( imageFrame[3].isActive() ) {
						tCurrentSlice--;
	         			// System.err.println("tDim = " + tCurrentSlice);
	         		}
				 }
				 
				 if ( tCurrentSlice >= 0 && tCurrentSlice <= (zTimeDim-1) ) {
					currentSlice3 = imageComp[3].getSlice();
      				imageComp[3].setTimeSlice(tCurrentSlice);
      				imageComp[3].show(tCurrentSlice, currentSlice3, true);
      				
      			}
				 
				 
		} else {
            /*
			if (wheelRotation < 0) {
				currentSlice++;
			} else {
				currentSlice--;
			}
            */
			imageComp[0].setSlice(currentSlice);
			imageComp[1].setSlice(currentSlice);
			imageComp[2].setSlice(currentSlice);
			imageComp[3].setSlice(currentSlice);
			
			if (currentSlice >= 0 && currentSlice < zDim0 && currentSlice < zDim1 && currentSlice < zDim2
					&& currentSlice < zDim3) {
				imageComp[0].show(0, currentSlice, true);
				imageComp[1].show(0, currentSlice, true);
				imageComp[2].show(0, currentSlice, true);
				imageComp[3].show(0, currentSlice, true);

			}
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

	
	public void doTraverse(File dir) {
		int i;
		
		imageNames = new String[8];
		imageNamesIndex = 0;
		images = new ModelImage[4];
		
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (i = 0; i < children.length; i++) {
				traverse(new File(dir, children[i]));
			}
		}
	}
	
	private void readImages(File dir) {
		int i;
		FileIO imageIO = null;
		String currentDirectory;
		boolean hasConvertedImages = false;
		boolean hasXMLimages = false;
		if (dir.isDirectory()) {
			
			doTraverse(dir);
			
			currentDirectory = dir.getAbsolutePath() + File.separator;
			
			try {
			
				for (i = 0; i < imageNamesIndex; i++) {
					if ( imageNames[i].contains("t2w_new.xml")) {
						hasConvertedImages = true;
						break;
					} else if ( imageNames[i].contains("adc_new.xml")) {
						hasConvertedImages = true;
						break;
					} else if ( imageNames[i].contains("dwi_new.xml")) {
						hasConvertedImages = true;
						break;
					} else if ( imageNames[i].contains("dce_new.xml")) {
						hasConvertedImages = true;
						break;
					}
				}
				 
				// read target images
				imageIO = new FileIO();
				if ( hasConvertedImages ) {
					readNewXML();
				} else {
					
					for (i = 0; i < imageNamesIndex; i++) {

						if (imageNames[i].contains("t2w.xml")) {
							hasXMLimages = true;
						}

						if (imageNames[i].contains("adc.xml")) {
							hasXMLimages = true;
						}

						if (imageNames[i].contains("dwi.xml")) {
							hasXMLimages = true;
						}

						if (imageNames[i].contains("dce.xml")) {
							hasXMLimages = true;
						}

						System.err.println(imageNames[i]);
					}
					 
					if ( !hasXMLimages ) {
						readDicomImage(currentDirectory);
						doTraverse(dir);
						readXML(currentDirectory);
						doTraverse(dir);
						readNewXML();
					} else {
						readXML(currentDirectory);
						doTraverse(dir);
						readNewXML();
					}
				
				
				} // end if hasConvertedImages
			
			

			} catch (Exception e) {
				e.printStackTrace();
			}

		}
	}

	public void readXML(String currentDirectory) {
		
		FileIO imageIO = new FileIO();
		int i; 
		
		for (i = 0; i < imageNamesIndex; i++) {

			if (imageNames[i].contains("t2w.xml")) {
				images[0] = imageIO.readImage(imageNames[i]);
			}

			if (imageNames[i].contains("adc.xml")) {
				images[1] = imageIO.readImage(imageNames[i]);
			}

			if (imageNames[i].contains("dwi.xml")) {
				images[2] = imageIO.readImage(imageNames[i]);
			}

			if (imageNames[i].contains("dce.xml")) {
				images[3] = imageIO.readImage(imageNames[i]);
			}

			System.err.println(imageNames[i]);
		}
		
		images[0].saveImage(currentDirectory, "t2w_new.xml", FileUtility.XML, false, true, true);

		int[] t2wExtents = images[0].getExtents();
		float[] t2wResols = images[0].getResolutions(0);
		// 1. transform adc.xml
		int[] adcExtents = images[1].getExtents();
		float[] adcResols = images[1].getResolutions(0);
		float[] adcResolsNew = new float[3];
		float constantFOV = 1.0f;
		float factor, fov;

		factor = (t2wExtents[0] - constantFOV) / (adcExtents[0] - constantFOV);
		fov = (adcExtents[0] - constantFOV) * adcResols[0];
		adcResolsNew[0] = fov / (t2wExtents[0] - constantFOV);
		adcResolsNew[1] = fov / (t2wExtents[0] - constantFOV);
		adcResolsNew[2] = adcResols[2];

		int oXdim, oYdim, oZdim, cXdim, cYdim, cZdim;
		float oXres, oYres, oZres, cXres, cYres, cZres;
		int[] units;
		boolean doVOI, doClip, doPad, perserveFOV, doUpdateOrigin, doInvMat;
		boolean doRotateCenter;
		float fillValue = 0.0f;
		boolean isSATransform = false;
		Vector3f center = null;
		TransMatrix xfrm;
		int interp;

		units = new int[3];
		units[0] = units[1] = units[2] = Unit.MILLIMETERS.getLegacyNum();

		factor = 1.0f;
		doVOI = false;
		doClip = true;
		doPad = false;
		doRotateCenter = false;
		center = null;

		fillValue = 0.0f;
		doUpdateOrigin = true;
		isSATransform = false;

		interp = 0;
		xfrm = new TransMatrix(4);
		xfrm.identity();

		oXres = adcResolsNew[0];
		oYres = adcResolsNew[1];
		oZres = adcResolsNew[2];

		oXdim = t2wExtents[0];
		oYdim = t2wExtents[1];
		oZdim = t2wExtents[2];

		AlgorithmTransform algoTrans = new AlgorithmTransform(images[1], xfrm, interp, oXres, oYres, oZres,
				oXdim, oYdim, oZdim, units, doVOI, doClip, doPad, doRotateCenter, center);
		algoTrans.setFillValue(fillValue);
		algoTrans.setUpdateOriginFlag(doUpdateOrigin);
		algoTrans.setUseScannerAnatomical(isSATransform);
		algoTrans.run();

		images[1] = algoTrans.getTransformedImage();
		images[1].calcMinMax();

		images[1].saveImage(currentDirectory, "adc_new.xml", FileUtility.XML, false, true, true);

		algoTrans.disposeLocal();
		algoTrans = null;

		// new ViewJFrameImage(images[1]);

		// 2. transform dwi.xml
		int[] dwiExtents = images[2].getExtents();
		float[] dwiResols = images[2].getResolutions(0);
		float[] dwiResolsNew = new float[3];

		factor = (t2wExtents[0] - constantFOV) / (dwiExtents[0] - constantFOV);
		fov = (dwiExtents[0] - constantFOV) * dwiResols[0];
		dwiResolsNew[0] = fov / (t2wExtents[0] - constantFOV);
		dwiResolsNew[1] = fov / (t2wExtents[0] - constantFOV);
		dwiResolsNew[2] = dwiResols[2];

		units = new int[3];
		units[0] = units[1] = units[2] = Unit.MILLIMETERS.getLegacyNum();

		factor = 1.0f;
		doVOI = false;
		doClip = true;
		doPad = false;
		doRotateCenter = false;
		center = null;

		fillValue = 0.0f;
		doUpdateOrigin = true;
		isSATransform = false;

		interp = 0;
		xfrm = new TransMatrix(4);
		xfrm.identity();

		oXres = dwiResolsNew[0];
		oYres = dwiResolsNew[1];
		oZres = dwiResolsNew[2];

		oXdim = t2wExtents[0];
		oYdim = t2wExtents[1];
		oZdim = t2wExtents[2];

		AlgorithmTransform algoTransDWI = new AlgorithmTransform(images[2], xfrm, interp, oXres, oYres,
				oZres, oXdim, oYdim, oZdim, units, doVOI, doClip, doPad, doRotateCenter, center);
		algoTransDWI.setFillValue(fillValue);
		algoTransDWI.setUpdateOriginFlag(doUpdateOrigin);
		algoTransDWI.setUseScannerAnatomical(isSATransform);
		algoTransDWI.run();

		images[2] = algoTransDWI.getTransformedImage();
		images[2].calcMinMax();

		images[2].saveImage(currentDirectory, "dwi_new.xml", FileUtility.XML, false, true, true);

		algoTransDWI.disposeLocal();
		algoTransDWI = null;

		// new ViewJFrameImage(images[2]);

		// 3. transform dce.xml
		int[] xBounds = new int[2];
		int[] yBounds = new int[2];
		int[] zBounds = new int[2];

		int[] dceExtents = images[3].getExtents();
		float[] dceResols = images[3].getResolutions(0);
		float[] dceResolsNew = new float[3];

		int[] destExtents = new int[3];
		destExtents[0] = 128;
		destExtents[1] = 128;
		destExtents[2] = dceExtents[2];

		xBounds[0] = -64;
		xBounds[1] = -64;
		yBounds[0] = -64;
		yBounds[1] = -64;
		zBounds[0] = 0;
		zBounds[1] = 0;

		ModelImage resultImage = new ModelImage(images[3].getType(), destExtents,
				images[3].getImageName() + "_crop");
		AlgorithmAddMargins cropAlgo = new AlgorithmAddMargins(images[3], resultImage, xBounds, yBounds,
				zBounds);
		cropAlgo.run();

		images[3] = resultImage;

		// new ViewJFrameImage(resultImage);
		// if( true) System.exit(1);
		
		cropAlgo = null;

		dceExtents = images[3].getExtents();
		dceResols = images[3].getResolutions(0);
		dceResolsNew = new float[3];

		factor = (t2wExtents[0] - constantFOV) / (dwiExtents[0] - constantFOV);
		fov = (dceExtents[0] - constantFOV) * dceResols[0];
		dceResolsNew[0] = fov / (t2wExtents[0] - constantFOV);
		dceResolsNew[1] = fov / (t2wExtents[0] - constantFOV);
		dceResolsNew[2] = dceResols[2];

		units = new int[3];
		units[0] = units[1] = units[2] = Unit.MILLIMETERS.getLegacyNum();

		factor = 1.0f;
		doVOI = false;
		doClip = true;
		doPad = false;
		doRotateCenter = false;
		center = null;

		fillValue = 0.0f;
		doUpdateOrigin = true;
		isSATransform = false;

		interp = 0;
		xfrm = new TransMatrix(4);
		xfrm.identity();

		oXres = dceResolsNew[0];
		oYres = dceResolsNew[1];
		oZres = dceResolsNew[2];

		oXdim = t2wExtents[0];
		oYdim = t2wExtents[1];
		oZdim = dceExtents[2];

		AlgorithmTransform algoTransDCE = new AlgorithmTransform(images[3], xfrm, interp, oXres, oYres,
				oZres, oXdim, oYdim, oZdim, units, doVOI, doClip, doPad, doRotateCenter, center);
		algoTransDCE.setFillValue(fillValue);
		algoTransDCE.setUpdateOriginFlag(doUpdateOrigin);
		algoTransDCE.setUseScannerAnatomical(isSATransform);
		algoTransDCE.run();

		images[3] = algoTransDCE.getTransformedImage();
		images[3].calcMinMax();

		images[3].saveImage(currentDirectory, "dce_new.xml", FileUtility.XML, false, true, true);

		algoTransDCE.disposeLocal();
		algoTransDCE = null;
		
		for ( i = 0; i < images.length; i++ ) {
			images[i].disposeLocal();
			images[i] = null;
		}
        System.gc();
	}
	
	public void readNewXML() {
		
		FileIO imageIO = new FileIO();
		int i; 
		
		for (i = 0; i < imageNamesIndex; i++) {

			if (imageNames[i].contains("t2w_new.xml")) {
				images[0] = imageIO.readImage(imageNames[i]);
			}

			if (imageNames[i].contains("adc_new.xml")) {
				images[1] = imageIO.readImage(imageNames[i]);
			}

			if (imageNames[i].contains("dwi_new.xml")) {
				images[2] = imageIO.readImage(imageNames[i]);
			}

			if (imageNames[i].contains("dce_new.xml")) {
				images[3] = imageIO.readImage(imageNames[i]);
			}

			// System.err.println(imageNames[i]);
		}
		
		int volumeLength = 26;
		float res3 = 3.0f;
		float res4 = 1.0f;
		int unit3 = 8;
		int unit4 = 8;
		AlgorithmConvert3Dto4D convert3Dto4DAlgo = new AlgorithmConvert3Dto4D(images[3], volumeLength, res3,
				res4, unit3, unit4);
		convert3Dto4DAlgo.run();
		images[3] = convert3Dto4DAlgo.getResultImage();
	
		convert3Dto4DAlgo = null;
		
		equalScaleImage();
       

	}
	
	public void equalScaleImage() {
		
		int screenWidth = Toolkit.getDefaultToolkit().getScreenSize().width;
	    int screenHeight = Toolkit.getDefaultToolkit().getScreenSize().height;
		
		for ( int i = 0; i < 4; i++ ) {
			
			
			if ( i != 3 ) {
			imageFrame[i] = new ViewJFrameImage(images[i]);
			// imageFrame[i].setVisible(false);
			imageFrame[i].setLocation(screenWidth, screenHeight);
			imageComp[i] = imageFrame[i].getComponentImage();
			} else {
				imageFrame[3] = new ViewJFrameImage(images[3], null, ViewUserInterface.getReference().getNewFrameLocation(images[3].getExtents()[0], images[3].getExtents()[1]));
				imageFrame[3].setLocation(screenWidth, screenHeight);
				imageComp[3] = imageFrame[i].getComponentImage(); 
			}
		
		}
		
		

	}

	
	public void fitsToScreen() {

		float newZoom, oldZoom;
		int currentSlice;
		int i;
		int compW;
		int compH;
		int frameW;
		int frameH;
		int diffW;
		int diffH;
		
		for ( i = 0; i < 4; i++ ) {
			
				compW = imageComp[i].getWidth();
				compH = imageComp[i].getHeight();

				frameW = imageFrame[i].getWidth();
				frameH = imageFrame[i].getHeight();

				diffW = frameW - compW;
				diffH = frameH - compH;
				
				currentSlice = imageComp[i].getSlice();
				
				int w = imageComp[i].getWidth();
				int h = imageComp[i].getHeight();
				
				oldZoom = imageComp[i].getZoomX();
				newZoom = 2.0f * imageComp[i].getZoomX();
				
			    imageComp[i].setZoomExact(newZoom, newZoom);
			    
			    int newX = MipavMath.round((w/2 * newZoom) / oldZoom);
				int newY = MipavMath.round((h/2 * newZoom) / oldZoom);
				imageComp[i].setCenter(new Vector3f(newX, newY, currentSlice));
				adjustScrollbars(newX, newY, imageScroll[i]);
				
				imageComp[i].show(0, currentSlice, true);
				
				imageFrame[i].setSize(compW / 2 + diffW, compH / 2 + diffH);
				imageFrame[i].updateImages();
			 
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
	 
		
		if ( file.isDirectory() ) {
			traverseDeeper(file); 
			// printTable();
			
		}

	}
	
	private void readDicomImage(String currentDirectory) {
		Set<String> keys = namesTable.keySet();
		for ( String hashID : keys ) {
			System.err.println(" readDicom:   hashID = " + hashID);
		
			Vector<String> names = namesTable.get(hashID);
			FileIO fileIO = new FileIO();
			String imageFullName = names.get(0);
			System.err.println(" imageFullName = " + imageFullName);
			int index = imageFullName.lastIndexOf(File.separator);
		
			String fileName = imageFullName.substring(index+1, imageFullName.length());
			String directory = imageFullName.substring(0, index+1);
			System.err.println("fileName = " + fileName);
			System.err.println("directory = " + directory);
			 
			boolean multiFile = true;
			ModelImage image = fileIO.readImage(fileName, directory, multiFile, null);
			// new ViewJFrameImage(image);
			 
		
			int extents[] = image.getExtents();
			
			System.err.println(" zDim = " + extents[2]);
			
			if ( extents[0] == 512 && extents[1] == 512 ) { // t2w 
				images[0] = image;
				System.err.println("t2w image");
				images[0].saveImage(currentDirectory, "t2w.xml", FileUtility.XML, false, true, true);
			} else if ( extents[2] >= 100 ) {  // dce
				images[3] = image;
				System.err.println("dce image");
				images[3].saveImage(currentDirectory, "dce.xml", FileUtility.XML, false, true, true);
			} else if ( extents[2] < 100 && extents[0] == 256  && extents[1] == 256 ) {
			
				System.err.println("adc and dwi");
				FileInfoBase fileInfo = image.getFileInfo()[0];
				FileInfoDicom fileInfoDicom = (FileInfoDicom)fileInfo;
			    String imageType = ((String) fileInfoDicom.getTagTable().getValue("0008,0008")).trim();
				System.err.println("image type: " + imageType);
				
				if ( imageType.toLowerCase().contains("adc")) {
					images[1] = image;   // adc
					System.err.println("read adc");
					images[1].saveImage(currentDirectory, "adc.xml", FileUtility.XML, false, true, true);
				} else {
					images[2] = image;
					System.err.println("read dwi");
					images[2].saveImage(currentDirectory, "dwi.xml", FileUtility.XML, false, true, true);
				}
				
			} 
			
		}
		
		System.gc();
		
	}
	
	private void printTable() {
		Set<String> keys = namesTable.keySet();
		for ( String hashID : keys ) {
			System.err.println("hashID = " + hashID);
			
			Vector<String> names = namesTable.get(hashID);
			for ( int j = 0; j < names.size(); j++ ) {
				System.err.println(names.get(j));
			}
		 
			System.err.println();
		}
	}
	
	Hashtable<String, Vector<String>>  namesTable = new Hashtable<String, Vector<String>>(); 
	
	private void traverseDeeper(File file) {
		String children[] = file.list();
		for ( int i = 0; i < children.length; i++ ) {
			namesTable.put(children[i], new Vector<String>());
			traverseDicom(new File(file.getAbsolutePath() + File.separator + children[i] + File.separator), children[i]);
		}
		
	}
	
	private void traverseDicom(File file, String dirName) {
		String children[] = file.list();
		for ( int i = 0; i < children.length; i++ ) {
			namesTable.get(dirName).add(file + File.separator + children[i]);
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
	public void actionPerformed(final ActionEvent event) {
		Object eventObj = event.getSource();
		// (JMenuItem)event.getSource()).getText();
		
		/********************  Window-Level   threshold values ******************************/ 
		if ( eventObj instanceof JMenuItem ) {
			String cmd = ((JMenuItem)eventObj).getText();
				
				
				if ( cmd.equals("Measure")) {
					invokeMeasure();
				} 
				
				/*
				for ( int i = 0; i < imageNamesIndex; i++ ) {
					if( cmd.equals(images[i].getImageName() )) {
						updateImage(i);
						updateSlider();
						break;
					}
					
				}
				*/ 	
		}
		
	}

	public void invokeMeasure() {
		for ( int i = 0; i < 4; i++ ) {
		  if ( imageActiveIndex == i ) {
			  imageFrame[i].getVOIManager().doVOI(CustomUIBuilder.PARAM_VOI_LINE.getActionCommand());
		  }
		}	
	}

	
	
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
			stopRecording();
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
				
				String location0 = new String("");        
				String location1 = new String("");
				String location2 = new String("");
				String location3 = new String("");
				
				
				int deltaX = origin.x - e.getX();
				int deltaY = origin.y - e.getY();
				JViewport viewPort0 = imageScroll[0].getViewport();
				if (viewPort0 != null) {
					Rectangle view = viewPort0.getViewRect();
					view.x += deltaX;
					view.y += deltaY;

					imageComp[0].scrollRectToVisible(view);
					Point vul0 = viewPort0.getLocationOnScreen();
					Dimension d0 = viewPort0.getSize();
					Point im0 = imageComp[0].getLocationOnScreen();
					int x0l = Math.max(0,imageComp[0].getScaledX(vul0.x - im0.x));
		            int y0u = Math.max(0,imageComp[0].getScaledY(vul0.y - im0.y));
		            int x0r = Math.min(images[0].getExtents()[0]-1,imageComp[0].getScaledX(vul0.x - im0.x + d0.width));
		            int y0l = Math.min(images[0].getExtents()[1]-1,imageComp[0].getScaledY(vul0.y - im0.y + d0.height));
		            // System.out.println("Image 0 upper left image coordinates at x = " + x0l + " y = " + y0u);
		            // System.out.println("Image 0 upper right image coordinates at x = " + x0r + " y = " + y0u);
		            // System.out.println("Image 0 lower left image coordinates at x = " + x0l + " y = " + y0l);
		            // System.out.println("Image 0 lower right image coordinates at x = " + x0r + " y = " + y0l);
		            
		            location0 += x0l + ", " + y0u + ", ";
		            location0 += x0r + ", " + y0u + ", ";
		            location0 += x0l + ", " + y0l + ", ";
		            location0 += x0r + ", " + y0l + ", ";
				}

				JViewport viewPort1 = imageScroll[1].getViewport();
				if (viewPort1 != null) {
					Rectangle view = viewPort1.getViewRect();
					view.x += deltaX;
					view.y += deltaY;

					imageComp[1].scrollRectToVisible(view);
					Point vul1 = viewPort1.getLocationOnScreen();
					Dimension d1 = viewPort1.getSize();
					Point im1 = imageComp[1].getLocationOnScreen();
					int x1l = Math.max(0,imageComp[1].getScaledX(vul1.x - im1.x));
		            int y1u = Math.max(0,imageComp[1].getScaledY(vul1.y - im1.y));
		            int x1r = Math.min(images[1].getExtents()[0]-1,imageComp[1].getScaledX(vul1.x - im1.x + d1.width));
		            int y1l = Math.min(images[1].getExtents()[1]-1,imageComp[1].getScaledY(vul1.y - im1.y + d1.height));
		            // System.out.println("Image 1 upper left image coordinates at x = " + x1l + " y = " + y1u);
		            // System.out.println("Image 1 upper right image coordinates at x = " + x1r + " y = " + y1u);
		            // System.out.println("Image 1 lower left image coordinates at x = " + x1l + " y = " + y1l);
		            // System.out.println("Image 1 lower right image coordinates at x = " + x1r + " y = " + y1l);
		            
		            location1 += x1l + ", " + y1u + ", ";
		            location1 += x1r + ", " + y1u + ", ";
		            location1 += x1l + ", " + y1l + ", ";
		            location1 += x1r + ", " + y1l + ", ";
				}

				JViewport viewPort2 = imageScroll[2].getViewport();
				if (viewPort2 != null) {
					Rectangle view = viewPort2.getViewRect();
					view.x += deltaX;
					view.y += deltaY;

					imageComp[2].scrollRectToVisible(view);
					Point vul2 = viewPort2.getLocationOnScreen();
					Dimension d2 = viewPort2.getSize();
					Point im2 = imageComp[2].getLocationOnScreen();
					int x2l = Math.max(0,imageComp[2].getScaledX(vul2.x - im2.x));
		            int y2u = Math.max(0,imageComp[2].getScaledY(vul2.y - im2.y));
		            int x2r = Math.min(images[2].getExtents()[0]-1,imageComp[2].getScaledX(vul2.x - im2.x + d2.width));
		            int y2l = Math.min(images[2].getExtents()[1]-1,imageComp[2].getScaledY(vul2.y - im2.y + d2.height));
		            // System.out.println("Image 2 upper left image coordinates at x = " + x2l + " y = " + y2u);
		            // System.out.println("Image 2 upper right image coordinates at x = " + x2r + " y = " + y2u);
		            // System.out.println("Image 2 lower left image coordinates at x = " + x2l + " y = " + y2l);
		            // System.out.println("Image 2 lower right image coordinates at x = " + x2r + " y = " + y2l);
		            
		            location2 += x2l + ", " + y2u + ", ";
		            location2 += x2r + ", " + y2u + ", ";
		            location2 += x2l + ", " + y2l + ", ";
		            location2 += x2r + ", " + y2l + ", ";
		            
				}

				JViewport viewPort3 = imageScroll[3].getViewport();
				if (viewPort3 != null) {
					Rectangle view = viewPort3.getViewRect();
					view.x += deltaX;
					view.y += deltaY;

					imageComp[3].scrollRectToVisible(view);
					Point vul3 = viewPort3.getLocationOnScreen();
					Dimension d3 = viewPort3.getSize();
					Point im3 = imageComp[3].getLocationOnScreen();
					int x3l = Math.max(0,imageComp[3].getScaledX(vul3.x - im3.x));
		            int y3u = Math.max(0,imageComp[3].getScaledY(vul3.y - im3.y));
		            int x3r = Math.min(images[3].getExtents()[0]-1,imageComp[3].getScaledX(vul3.x - im3.x + d3.width));
		            int y3l = Math.min(images[3].getExtents()[1]-1,imageComp[3].getScaledY(vul3.y - im3.y + d3.height));
		            // System.out.println("Image 3 upper left image coordinates at x = " + x3l + " y = " + y3u);
		            // System.out.println("Image 3 upper right image coordinates at x = " + x3r + " y = " + y3u);
		            // System.out.println("Image 3 lower left image coordinates at x = " + x3l + " y = " + y3l);
		            // System.out.println("Image 3 lower right image coordinates at x = " + x3r + " y = " + y3l);
		            
		            location3 += x3l + ", " + y3u + ", ";
		            location3 += x3r + ", " + y3u + ", ";
		            location3 += x3l + ", " + y3l + ", ";
		            location3 += x3r + ", " + y3l + ", ";
		            
				}
				
				
				if ( imageActiveIndex == 0 ) {
					imageComp[imageActiveIndex].setFullScreenModeLocation(location0);
				} else if ( imageActiveIndex == 1 ) {
					imageComp[imageActiveIndex].setFullScreenModeLocation(location1);
				} else if ( imageActiveIndex == 2 ) {
					imageComp[imageActiveIndex].setFullScreenModeLocation(location2);
				} else if ( imageActiveIndex == 3 ) {
					imageComp[imageActiveIndex].setFullScreenModeLocation(location3);
				}
				imageComp[imageActiveIndex].recordPanning();
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
			// Get the top left corner of the upper left white circle in the screen's coordinate space
			Point pul = label5.getLocationOnScreen();
			System.out.println("Upper left circle upper left corner x = " + pul.x + " y = "+ pul.y);
			// Get the width and height of the upper left circle
			Dimension dul = label5.getSize();
			System.out.println("Upper left circle dimensions width = " + dul.width + " height = " + dul.height);
			// Get the center of the upper left circle
			double culx = pul.x + dul.width/2.0;
			double culy = pul.y + dul.height/2.0;
			System.out.println("Center of upper left circle x = " + culx + " y = " + culy);
			JViewport viewPort0 = imageScroll[0].getViewport();
			Point vul0 = viewPort0.getLocationOnScreen();
			Dimension d0 = viewPort0.getSize();
			System.out.println("Upper left corner of JViewport of imageScroll[0] x = " + vul0.x + " y = " + vul0.y);
			Point im0 = imageComp[0].getLocationOnScreen();
            // System.out.println("Image upper left corner in screen coordinates at x = " + vul0.x + " y = " + vul0.y);
            // System.out.println("Image upper right corner in screen coordinates at x = " + (vul0.x + d0.width) + " y = " + vul0.y);
            // System.out.println("Image lower left corner in screen coordinates at x = " + vul0.x + " y = " + (vul0.y + d0.height));
            // System.out.println("Image lower right corner in screen coordinates at x = " + (vul0.x + d0.width) + " y = " + (vul0.y + d0.height));
            
            int x0l = Math.max(0,imageComp[0].getScaledX(vul0.x - im0.x));
            int y0u = Math.max(0,imageComp[0].getScaledY(vul0.y - im0.y));
            int x0r = Math.min(images[0].getExtents()[0]-1,imageComp[0].getScaledX(vul0.x - im0.x + d0.width));
            int y0l = Math.min(images[0].getExtents()[1]-1,imageComp[0].getScaledY(vul0.y - im0.y + d0.height));
            // System.out.println("Image 0 upper left image coordinates at x = " + x0l + " y = " + y0u);
            // System.out.println("Image 0 upper right image coordinates at x = " + x0r + " y = " + y0u);
            // System.out.println("Image 0 lower left image coordinates at x = " + x0l + " y = " + y0l);
            // System.out.println("Image 0 lower right image coordinates at x = " + x0r + " y = " + y0l);
            
        
			JViewport viewPort1 = imageScroll[1].getViewport();
			Point vul1 = viewPort1.getLocationOnScreen();
			Dimension d1 = viewPort1.getSize();
			System.out.println("Upper right corner of JViewPort of imageScroll[1] x = " + (vul1.x + d1.width) +
					" y = " + vul1.y);
			Point im1 = imageComp[1].getLocationOnScreen();
			int x1l = Math.max(0,imageComp[1].getScaledX(vul1.x - im1.x));
            int y1u = Math.max(0,imageComp[1].getScaledY(vul1.y - im1.y));
            int x1r = Math.min(images[1].getExtents()[0]-1,imageComp[1].getScaledX(vul1.x - im1.x + d1.width));
            int y1l = Math.min(images[1].getExtents()[1]-1,imageComp[1].getScaledY(vul1.y - im1.y + d1.height));
            // System.out.println("Image 1 upper left image coordinates at x = " + x1l + " y = " + y1u);
            // System.out.println("Image 1 upper right image coordinates at x = " + x1r + " y = " + y1u);
            // System.out.println("Image 1 lower left image coordinates at x = " + x1l + " y = " + y1l);
            // System.out.println("Image 1 lower right image coordinates at x = " + x1r + " y = " + y1l);
            
			JViewport viewPort2 = imageScroll[2].getViewport();
			Point vul2 = viewPort2.getLocationOnScreen();
			Dimension d2 = viewPort2.getSize();
			System.out.println("Lower left corner of JViewport of imageScroll[2] x = " + vul2.x + 
					" y = " + (vul2.y + d2.height));
			System.out.println("Upper right corner of JViewport of imageScroll[2] x = " + (vul2.x + d2.width) + 
					" y = " + vul2.y);
			Point im2 = imageComp[2].getLocationOnScreen();
			int x2l = Math.max(0,imageComp[2].getScaledX(vul2.x - im2.x));
            int y2u = Math.max(0,imageComp[2].getScaledY(vul2.y - im2.y));
            int x2r = Math.min(images[2].getExtents()[0]-1,imageComp[2].getScaledX(vul2.x - im2.x + d2.width));
            int y2l = Math.min(images[2].getExtents()[1]-1,imageComp[2].getScaledY(vul2.y - im2.y + d2.height));
            // System.out.println("Image 2 upper left image coordinates at x = " + x2l + " y = " + y2u);
            // System.out.println("Image 2 upper right image coordinates at x = " + x2r + " y = " + y2u);
            // System.out.println("Image 2 lower left image coordinates at x = " + x2l + " y = " + y2l);
            // System.out.println("Image 2 lower right image coordinates at x = " + x2r + " y = " + y2l);
            
			JViewport viewPort3 = imageScroll[3].getViewport();
			Point vul3 = viewPort3.getLocationOnScreen();
			Dimension d3 = viewPort3.getSize();
			System.out.println("Lower right corner of JViewport of imageScroll[3] x = " + (vul3.x + d3.width) +
					" y = " + (vul3.y + d3.height));
			Point im3 = imageComp[3].getLocationOnScreen();
			int x3l = Math.max(0,imageComp[3].getScaledX(vul3.x - im3.x));
            int y3u = Math.max(0,imageComp[3].getScaledY(vul3.y - im3.y));
            int x3r = Math.min(images[3].getExtents()[0]-1,imageComp[3].getScaledX(vul3.x - im3.x + d3.width));
            int y3l = Math.min(images[3].getExtents()[1]-1,imageComp[3].getScaledY(vul3.y - im3.y + d3.height));
            // System.out.println("Image 3 upper left image coordinates at x = " + x3l + " y = " + y3u);
            // System.out.println("Image 3 upper right image coordinates at x = " + x3r + " y = " + y3u);
            // System.out.println("Image 3 lower left image coordinates at x = " + x3l + " y = " + y3l);
            // System.out.println("Image 3 lower right image coordinates at x = " + x3r + " y = " + y3l);
            
			Point originScreen = event.getLocationOnScreen();
			System.out.println("Event location in screen coordinates x = " + originScreen.x + " y = " +originScreen.y);
			// Distance from center of upper circle
			double dx = originScreen.x - culx;
			double dy = originScreen.y - culy;
			System.out.println("Event distance from upper left circle center dx = " + dx + " dy = " + dy);

			// System.err.println("in mouse pressed");
			int currentSlice0 = imageComp[0].getSlice();
			int currentSlice1 = imageComp[1].getSlice();
			int currentSlice2 = imageComp[2].getSlice();
			int currentSlice3 = imageComp[3].getSlice();

			if (event.getButton() == MouseEvent.BUTTON1 && event.isShiftDown()) {
				
				String location0 = new String("");        
				String location1 = new String("");
				String location2 = new String("");
				String location3 = new String("");
				
			    location0 +=  x0l + ", " + y0u + ", ";
	            location0 +=   x0r + ", " + y0u + ", ";
	            location0 +=  x0l + ", " + y0l + ", ";
	            location0 +=  x0r + ", " + y0l + ", ";
	            
	            location1 +=  x1l + ", " + y1u + ", ";
	            location1 +=  x1r + ", " + y1u + ", ";
	            location1 +=  x1l + ", " + y1l + ", ";
	            location1 +=  x1r + ", " + y1l + ", ";
	            
	            location2 +=  x2l + ", " + y2u + ", ";
	            location2 +=  x2r + ", " + y2u + ", ";
	            location2 +=  x2l + ", " + y2l + ", ";
	            location2 +=  x2r + ", " + y2l + ", ";
	            
	            location3 +=  x3l + ", " + y3u + ", ";
	            location3 +=  x3r + ", " + y3u + ", ";
	            location3 +=  x3l + ", " + y3l + ", ";
	            location3 +=  x3r + ", " + y3l + ", ";
	            
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
					newZoom0 = 1.2f * imageComp[0].getZoomX();
				} else {
					newZoom0 = imageComp[0].getZoomX() + 0.2f;
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
					newZoom1 = 1.2f * imageComp[1].getZoomX();
				} else {
					newZoom1 = imageComp[1].getZoomX() + 0.2f;
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
					newZoom2 = 1.2f * imageComp[2].getZoomX();
				} else {
					newZoom2 = imageComp[2].getZoomX() + 0.2f;
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
					newZoom3 = 1.2f * imageComp[3].getZoomX();
				} else {
					newZoom3 = imageComp[3].getZoomX() + 0.2f;
				}
				imageComp[3].setZoom(newZoom3, newZoom3);

				Vector2f oldCrosshairPoint3 = new Vector2f(event.getX(), event.getY());

				if (oldCrosshairPoint3 != null) {
					int newX = MipavMath.round((oldCrosshairPoint3.X * newZoom3) / oldZoom3);
					int newY = MipavMath.round((oldCrosshairPoint3.Y * newZoom3) / oldZoom3);
					imageComp[3].setCenter(new Vector3f(newX, newY, currentSlice3));
					adjustScrollbars(newX, newY, imageScroll[3]);
				}
				
				if ( imageActiveIndex == 0 ) {
					imageComp[imageActiveIndex].setFullScreenModeLocation(location0);
				} else if ( imageActiveIndex == 1 ) {
					imageComp[imageActiveIndex].setFullScreenModeLocation(location1);
				} else if ( imageActiveIndex == 2 ) {
					imageComp[imageActiveIndex].setFullScreenModeLocation(location2);
				} else if ( imageActiveIndex == 3 ) {
					imageComp[imageActiveIndex].setFullScreenModeLocation(location3);
				}
				imageComp[imageActiveIndex].recordZoom(true);

			} else if (event.getButton() == MouseEvent.BUTTON1 && event.isControlDown()) {
				
				String location0 = new String("");        
				String location1 = new String("");
				String location2 = new String("");
				String location3 = new String("");
				
			    location0 +=  x0l + ", " + y0u + ", ";
	            location0 +=   x0r + ", " + y0u + ", ";
	            location0 +=  x0l + ", " + y0l + ", ";
	            location0 +=  x0r + ", " + y0l + ", ";
	            
	            location1 +=  x1l + ", " + y1u + ", ";
	            location1 +=  x1r + ", " + y1u + ", ";
	            location1 +=  x1l + ", " + y1l + ", ";
	            location1 +=  x1r + ", " + y1l + ", ";
	            
	            location2 +=  x2l + ", " + y2u + ", ";
	            location2 +=  x2r + ", " + y2u + ", ";
	            location2 +=  x2l + ", " + y2l + ", ";
	            location2 +=  x2r + ", " + y2l + ", ";
	            
	            location3 +=  x3l + ", " + y3u + ", ";
	            location3 +=  x3r + ", " + y3u + ", ";
	            location3 +=  x3l + ", " + y3l + ", ";
	            location3 +=  x3r + ", " + y3l + ", ";
				
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
					newZoom0 = imageComp[0].getZoomX() - 0.2f;
				} else {
					newZoom0 = 0.8f * imageComp[0].getZoomX();
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
					newZoom1 = imageComp[1].getZoomX() - 0.2f;
				} else {
					newZoom1 = 0.8f * imageComp[1].getZoomX();
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
					newZoom2 = imageComp[2].getZoomX() - 0.2f;
				} else {
					newZoom2 = 0.8f * imageComp[2].getZoomX();
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
					newZoom3 = imageComp[3].getZoomX() - 0.2f;
				} else {
					newZoom3 = 0.8f * imageComp[3].getZoomX();
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
				
				if ( imageActiveIndex == 0 ) {
					imageComp[imageActiveIndex].setFullScreenModeLocation(location0);
				} else if ( imageActiveIndex == 1 ) {
					imageComp[imageActiveIndex].setFullScreenModeLocation(location1);
				} else if ( imageActiveIndex == 2 ) {
					imageComp[imageActiveIndex].setFullScreenModeLocation(location2);
				} else if ( imageActiveIndex == 3 ) {
					imageComp[imageActiveIndex].setFullScreenModeLocation(location3);
				}
				imageComp[imageActiveIndex].recordZoom(false);

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
	       
	        for ( int i = 0; i < 4; i++ ) {
	        	if ( imageComp[i].isHighlight() ) {
	        		imageActiveIndex = i; 
	        		break;
	        	}
	        }
		
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

	
	public void startRecording() {
		String defaultDirectory = System.getProperties().getProperty("user.home") + File.separator + "mipav" + File.separator;
		String timeStamp = new SimpleDateFormat("yyyy MMM dd HH:mm:ss").format(new Date()).toString();
		timeStamp = timeStamp.replaceAll("\\s","_");
		timeStamp = timeStamp.replaceAll(":","_");
		System.err.println(timeStamp );
		String defaultFileName = defaultDirectory + "MIPAV_eyetracking_" + timeStamp + ".csv";
		MipavUtil.setEyeTrackingEnabled(true, defaultFileName, imageComp[imageActiveIndex]);
	}

	
	public void stopRecording() {
		System.err.println("stop recording now");
		MipavUtil.setEyeTrackingEnabled(false, null);
	}
	
	

}
