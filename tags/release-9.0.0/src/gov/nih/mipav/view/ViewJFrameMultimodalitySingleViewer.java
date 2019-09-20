package gov.nih.mipav.view;

import gov.nih.mipav.util.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.renderer.WildMagic.VOI.*;

import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.text.DecimalFormat;
import java.util.*;

import javax.swing.*;
import javax.swing.border.Border;
import javax.swing.event.*;

import WildMagic.LibFoundation.Mathematics.*;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;
import java.text.SimpleDateFormat;


public class ViewJFrameMultimodalitySingleViewer extends ViewJFrameTriImage
		implements ItemListener, ChangeListener, KeyListener, MouseListener, MouseMotionListener, MouseWheelListener, VOIManagerInterfaceListener {

	
	
	// ~ Instance fields
	// ------------------------------------------------------------------------------------------------
	/** target image variables. */
	private JFileChooser imagesChooser = new JFileChooser();
	
	/** The main user interface. */
	private ViewUserInterface UI;

	private ViewJFrameImage currentFrame;
	private JScrollPane imageScroll;
	private Point origin;

	private JPanel quadImagePanel;
	private int currentSlice;
	
	private int[] zDim;
	
	
	private JComboBox comboBoxImage;
	private float initZoomFactor;
	
	private JPopupMenu popup = new JPopupMenu();
	
	private int sliceNumCache = 0;
	
	protected Font serif12, serif12B;
	
	private JLabel[] sliderMax;
	private JSlider[] zImageSlider;
	private JTextField[] slicesTextField;

	private ViewJFrameImage[] imageFrame;
	private ViewJComponentEditImage[] imageComp;
	private String[] imageNames = new String[10];
	private int imageNamesIndex = 0;
	private ModelImage[] images;
	
	private int imageActiveIndex = 0;

	private JPanel sliderPanel;
	
	
	private ImageIcon cornerImage;
	private ImageIcon blackImage;
	
	private JPanel rightPanel;
	private JPanel leftPanel;
	private JPanel topPanel;
	private JPanel lowerPanel;
	private JLabel label1;
	private JLabel label2;
	private JLabel label3;
	private JLabel label4;
	private JLabel label5;
	private JLabel label6;
	private JLabel label7;
	private JLabel label8;
	
	private boolean pressed = false;
	
	// ~ Constructors
	// ---------------------------------------------------------------------------------------------------

	public ViewJFrameMultimodalitySingleViewer(final ModelImage _imageA, ViewJFrameImage frame) {
		super(_imageA, null);
		currentFrame = frame;
		UI = ViewUserInterface.getReference();
		
		serif12 = MipavUtil.font12;
	    serif12B = MipavUtil.font12B;
		
	    int readFromDirectory = JOptionPane.showConfirmDialog(UI.getMainFrame(), "Read images from directory?", "Init Dialog",  JOptionPane.YES_NO_OPTION);
	    
	    if ( readFromDirectory == JOptionPane.OK_OPTION ) {
	    	// reading from specific directory. 
			readMultlmodalImages();
	    } else {
		    // reading from opened image list
		    comboBoxImage = new JComboBox();
			comboBoxImage.setBackground(Color.white);
			buildComboBoxImage();
			Object selected = comboBoxImage.getSelectedItem();
			if (selected != null) {
				comboBoxImage.setSelectedItem(selected);
				int len = comboBoxImage.getItemCount();
				imageNamesIndex = len;
				images = new ModelImage[len];
				imageComp = new ViewJComponentEditImage[len];
				imageFrame = new ViewJFrameImage[len];
				imageNames = new String[len];
				
				sliderMax = new JLabel[len];
				zImageSlider = new JSlider[len];
				slicesTextField = new JTextField[len];
				zDim = new int[len];			
				for (int i = 0; i < len; i++) {
					imageNames[i] = (String)comboBoxImage.getItemAt(i);
					System.err.println(imageNames[i]);
					images[i] = ViewUserInterface.getReference().getRegisteredImageByName(imageNames[i]);
				}
				equalScaleImage();
			}
	    }
		
		getFramesInfo();
		initLayout();
		addPopup();
		
		JViewport viewPort = imageScroll.getViewport();
		int vh = viewPort.getHeight();
		
		for ( int i = 0; i < imageNamesIndex; i++ ) {
			int dim[] = images[i].getExtents();
			float res[] = images[i].getResolutions(0);
			initZoomFactor = (float)vh / (float)( dim[1]);
			System.err.println("initZoomFactor = " + initZoomFactor);
		    imageComp[i].setZoom(initZoomFactor, initZoomFactor);
		    imageComp[i].setEyetrackerRecordMode(ViewJComponentEditImage.SingleFrameEyetrackerMode);
		}
		
	    startRecording();
	    
	}

	public void addPopup() {
		   JMenuItem m = new JMenuItem("Soft Tissue");
		    m.addActionListener(this);
		    popup.add(m);
		    m = new JMenuItem("Lung");
		    m.addActionListener(this);
		    popup.add(m);
		    m = new JMenuItem("Bone");
		    m.addActionListener(this);
		    popup.add(m);
		    m = new JMenuItem("Measure");
		    m.addActionListener(this);
		    popup.add(m);
		    for ( int i = 0; i < imageNamesIndex; i++ ) {
		    	m = new JMenuItem(images[i].getImageName());
			    m.addActionListener(this);
			    popup.add(m);
		    }
		    MousePopupListener pl = new MousePopupListener();
		    addMouseListener(pl);
		    quadImagePanel.addMouseListener(pl);
		   
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
		System.err.println("stop recording new");
		MipavUtil.setEyeTrackingEnabled(false, null);
	}
	
	// ~ Methods
	// --------------------------------------------------------------------------------------------------------

	private void getFramesInfo() {
		
		for ( int i = 0; i < imageNamesIndex; i++ ) {
			int[] extents0 = images[i].getExtents();
			zDim[i] = extents0[2];
		}
		
		
	}
	
	void captureComponent(Component component) {
	    Rectangle rect = component.getBounds();
	 
	    try {
	        String format = "png";
	        String fileName = component.getName() + "." + format;
	        System.err.println("filename = " + fileName);
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

		cornerImage = MipavUtil.getIcon("WhiteCircle_550.png");
		blackImage = MipavUtil.getIcon("BlackCircle_550.png");

		leftPanel = new JPanel(new BorderLayout());
		leftPanel.setBackground(Color.black);
		label1 = new JLabel();
		label1.setBackground(Color.black);
		label1.setIcon(blackImage);
		label2 = new JLabel();
		label2.setBackground(Color.black);
		label2.setIcon(blackImage);
		leftPanel.add(label1, BorderLayout.NORTH);
		leftPanel.add(label2, BorderLayout.SOUTH);

		rightPanel = new JPanel(new BorderLayout());
		rightPanel.setBackground(Color.black);
		label3 = new JLabel();
		label3.setBackground(Color.black);
		label3.setIcon(blackImage);
		label4 = new JLabel();
		label4.setBackground(Color.black);
		label4.setIcon(blackImage);
		rightPanel.add(label3, BorderLayout.NORTH);
		rightPanel.add(label4, BorderLayout.SOUTH);
		

		topPanel = new JPanel(new BorderLayout());
		topPanel.setBackground(Color.black);
		label5 = new JLabel();
		label5.setBackground(Color.black);
		label5.setIcon(cornerImage);
		label6 = new JLabel();
		label6.setBackground(Color.black);
		label6.setIcon(cornerImage);
		topPanel.add(label5, BorderLayout.WEST);
		topPanel.add(label6, BorderLayout.EAST);

		lowerPanel = new JPanel(new BorderLayout());
		lowerPanel.setBackground(Color.black);
		label7 = new JLabel();
		label7.setBackground(Color.black);
		label7.setIcon(cornerImage);
		label8 = new JLabel();
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

		
		int currentSlice0 = imageComp[imageActiveIndex].getSlice();

		int compW = imageComp[imageActiveIndex].getWidth();
		int compH = imageComp[imageActiveIndex].getHeight();

		float zoomFactorX = panelWidth / compH;
		float zoomFactorY = panelHeight / compH;

		float zoomX0 = imageComp[imageActiveIndex].getZoomX();
		float zoomY0 = imageComp[imageActiveIndex].getZoomY();
		
		float zoomX = zoomX0 * zoomFactorX;
		float zoomY = zoomY0 * zoomFactorY;

		// imageComp[imageActiveIndex].setZoomExact(zoomY, zoomY);
		imageComp[imageActiveIndex].show(0, currentSlice0, true);
		
		

		quadImagePanel = new JPanel();
		quadImagePanel.setLayout(gbLayout);
		quadImagePanel.setBackground(Color.black);
		quadImagePanel.add(imageComp[imageActiveIndex], gbc);
		
		
		imageScroll = new JScrollPane(quadImagePanel,
	                ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		imageScroll.getVerticalScrollBar().setSize(new Dimension(0, 0));
		imageScroll.getHorizontalScrollBar().setSize(new Dimension(0, 0));
		imageScroll.getVerticalScrollBar().setVisible(false);
		imageScroll.getHorizontalScrollBar().setVisible(false);
		imageScroll.setOpaque(false);
		imageScroll.getViewport().setOpaque(false);
		imageScroll.setBorder(null);;
		

		buildSlider(rightPanel);
		
		getContentPane().add(imageScroll, BorderLayout.CENTER);

		getContentPane().add(leftPanel, BorderLayout.WEST);
		getContentPane().add(rightPanel, BorderLayout.EAST);
		getContentPane().add(topPanel, BorderLayout.NORTH);
		getContentPane().add(lowerPanel, BorderLayout.SOUTH);
		getContentPane().setName("SingleMultiViewContentPane");

		
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setExtendedState(JFrame.MAXIMIZED_BOTH);
		setUndecorated(true);
		
		for ( int i = 0; i < imageNamesIndex; i++ ) {
			imageComp[i].addMouseWheelListener(this);
			imageComp[i].addMouseListener(this);
			imageComp[i].addMouseMotionListener(this);
			imageComp[i].addKeyListener(this);
			imageFrame[i].addKeyListener(this);
		}
		addKeyListener(this);
	
		setSize(screenWidth, screenHeight);
		setMinimumSize(getSize());
		setVisible(true);
		setResizable(false);
		
		pack();
		this.validate();
	}
	
	private void buildSlider(final JPanel rightPanel) {
		
		int i;
		final GridBagConstraints gbc = new GridBagConstraints();
		sliderPanel = new JPanel();
		int[] sliceMax = new int[imageNamesIndex];
		
		rightPanel.add(sliderPanel, BorderLayout.CENTER);
		sliderPanel.setLayout(new GridBagLayout());

		sliderPanel.setForeground(Color.white);
		sliderPanel.setBackground(Color.black);
		
		for ( i = 0; i < imageNamesIndex; i++ ) {
			sliceMax[i] = zDim[i] - 1;
			sliderMax[i] = new JLabel(Integer.toString(sliceMax[i]));
			sliderMax[i].setForeground(Color.white);
			sliderMax[i].setFont(serif12);
		}
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.weightx = 1;
		gbc.weighty = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(3, 3, 3, 3);

		final JLabel levelLabel = new JLabel("Slices");
		levelLabel.setForeground(Color.white);
		sliderPanel.add(levelLabel, gbc);

		gbc.gridy = 1;

		sliderPanel.add(sliderMax[imageActiveIndex], gbc);

		for ( i = 0; i < imageNamesIndex; i++ ) {
			zImageSlider[i] = new JSlider(0, sliceMax[i], imageComp[imageActiveIndex].getSlice());
			zImageSlider[i].setFont(serif12);
			zImageSlider[i].setEnabled(true);
			zImageSlider[i].setMajorTickSpacing((int) (1));
			zImageSlider[i].setPaintTicks(true);
			zImageSlider[i].addChangeListener(this);
			zImageSlider[i].setVisible(true);
			zImageSlider[i].setOrientation(SwingConstants.VERTICAL);
			zImageSlider[i].setBackground(Color.black);
			zImageSlider[i].setForeground(Color.white);
		}

		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.gridwidth = 3;
		gbc.gridheight = 7;
		gbc.weightx = 10;
		gbc.weighty = 10;
		gbc.fill = GridBagConstraints.VERTICAL;
		sliderPanel.add(zImageSlider[imageActiveIndex], gbc);

		gbc.weightx = 1;
		gbc.weighty = 1;
		gbc.gridx = 0;
		gbc.gridy = 10;
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;

		JLabel sliderLevMin = new JLabel(Integer.toString(0));
		int levelMinFloat = 0;
		sliderLevMin.setForeground(Color.white);
		sliderLevMin.setFont(serif12);
		sliderPanel.add(sliderLevMin, gbc);

		gbc.gridx = 0;
		gbc.gridy = 11;
		gbc.gridwidth = 0;
		gbc.gridheight = 1;
		gbc.weightx = 20;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		
		for ( i = 0; i < imageNamesIndex; i++ ) {
			slicesTextField[i] = new JTextField();
			slicesTextField[i].setBorder(BorderFactory.createEmptyBorder());
			slicesTextField[i].setText(Integer.toString(imageComp[imageActiveIndex].getSlice()));
			slicesTextField[i].setForeground(color.white);
			slicesTextField[i].setBackground(color.black);
			slicesTextField[i].addKeyListener(this);
		}
		sliderPanel.add(slicesTextField[imageActiveIndex], gbc);
		
	}
	
	private void updateSlider() {
		
		int i;
		final GridBagConstraints gbc = new GridBagConstraints();
		sliderPanel.removeAll();
		rightPanel.removeAll();
		int[] sliceMax = new int[imageNamesIndex];
		
		// cornerImage = MipavUtil.getIcon("WhiteCircle_550.png");
		// blackImage = MipavUtil.getIcon("BlackCircle_550.png");
		// rightPanel = new JPanel(new BorderLayout());
		
		rightPanel.setBackground(Color.black);
		JLabel label3 = new JLabel();
		label3.setBackground(Color.black);
		label3.setIcon(blackImage);
		JLabel label4 = new JLabel();
		label4.setBackground(Color.black);
		label4.setIcon(blackImage);
		rightPanel.add(label3, BorderLayout.NORTH);
		rightPanel.add(label4, BorderLayout.SOUTH);
	 
		sliderPanel = new JPanel();
		sliderPanel.setLayout(new GridBagLayout());
		sliderPanel.setForeground(Color.white);
		sliderPanel.setBackground(Color.black);
		
		sliceMax[imageActiveIndex] = zDim[imageActiveIndex];
		sliderMax[imageActiveIndex] = new JLabel(Integer.toString(sliceMax[imageActiveIndex]-1));
		sliderMax[imageActiveIndex].setForeground(Color.white);
		sliderMax[imageActiveIndex].setFont(serif12);
		
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.weightx = 1;
		gbc.weighty = 1;
		gbc.anchor = GridBagConstraints.WEST;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		gbc.insets = new Insets(3, 3, 3, 3);

		final JLabel levelLabel = new JLabel("Slices");
		levelLabel.setForeground(Color.white);
		sliderPanel.add(levelLabel, gbc);

		gbc.gridy = 1;

		sliderPanel.add(sliderMax[imageActiveIndex], gbc);

		
		zImageSlider[imageActiveIndex] = new JSlider(0, sliceMax[imageActiveIndex], imageComp[imageActiveIndex].getSlice());
		zImageSlider[imageActiveIndex].setFont(serif12);
		zImageSlider[imageActiveIndex].setEnabled(true);
		zImageSlider[imageActiveIndex].setMajorTickSpacing((int) (1));
		zImageSlider[imageActiveIndex].setPaintTicks(true);
		zImageSlider[imageActiveIndex].addChangeListener(this);
		zImageSlider[imageActiveIndex].setVisible(true);
		zImageSlider[imageActiveIndex].setOrientation(SwingConstants.VERTICAL);
		zImageSlider[imageActiveIndex].setBackground(Color.black);
		zImageSlider[imageActiveIndex].setForeground(Color.white);
		

		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.gridwidth = 3;
		gbc.gridheight = 7;
		gbc.weightx = 10;
		gbc.weighty = 10;
		gbc.fill = GridBagConstraints.VERTICAL;
		sliderPanel.add(zImageSlider[imageActiveIndex], gbc);

		gbc.weightx = 1;
		gbc.weighty = 1;
		gbc.gridx = 0;
		gbc.gridy = 10;
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.fill = GridBagConstraints.HORIZONTAL;

		JLabel sliderLevMin = new JLabel(Integer.toString(0));
		int levelMinFloat = 0;
		sliderLevMin.setForeground(Color.white);
		sliderLevMin.setFont(serif12);
		sliderPanel.add(sliderLevMin, gbc);

		gbc.gridx = 0;
		gbc.gridy = 11;
		gbc.gridwidth = 0;
		gbc.gridheight = 1;
		gbc.weightx = 20;
		gbc.fill = GridBagConstraints.HORIZONTAL;
		
		
		slicesTextField[imageActiveIndex] = new JTextField();
		slicesTextField[imageActiveIndex].setBorder(BorderFactory.createEmptyBorder());
		slicesTextField[imageActiveIndex].setText(Integer.toString(imageComp[imageActiveIndex].getSlice()));
		slicesTextField[imageActiveIndex].setForeground(color.white);
		slicesTextField[imageActiveIndex].setBackground(color.black);
		slicesTextField[imageActiveIndex].addKeyListener(this);
		
		sliderPanel.add(slicesTextField[imageActiveIndex], gbc);
		
		// getContentPane().remove(rightPanel);
		rightPanel.add(sliderPanel, BorderLayout.CENTER);
		// getContentPane().add(rightPanel, BorderLayout.EAST);
		
		sliderPanel.repaint();
		rightPanel.repaint();

		pack();
		this.validate();
	}
	
	public void mouseWheelMoved(final MouseWheelEvent mouseWheelEvent) {
		
		    int wheelRotation = mouseWheelEvent.getWheelRotation();
	        
	    	currentSlice = imageComp[imageActiveIndex].getSlice();
		  
	    	 if (((wheelRotation < 0) && (currentSlice < zDim[imageActiveIndex] )) ||  ((wheelRotation > 0) && (currentSlice > 0))) {
	         	if (wheelRotation < 0) {
	         		currentSlice++;
	             }
	         	else {
	         		currentSlice--;
	         	}
	         	imageComp[imageActiveIndex].setSlice(currentSlice);
	         	
	    	 }
	    	 
	    	 if ( imageFrame[imageActiveIndex].isActive() ) {
	    		
	    		 imageComp[imageActiveIndex].show(0, currentSlice, true);
	    	 }
	    	 
	    	 zImageSlider[imageActiveIndex].setValue(currentSlice);
	    	  
	    	
	}
	
	
	/**
	 * Builds a list of images to operate on from the template image.
	 */
	private void buildComboBoxImage() {
		ViewUserInterface UI;

		comboBoxImage.removeAllItems();

		UI = ViewUserInterface.getReference();

		Enumeration<String> names = UI.getRegisteredImageNames();
		while (names.hasMoreElements()) {
			String name = names.nextElement();
			comboBoxImage.addItem(name);
		}
	}

	public void equalScaleImage() {

		float newZoom;
		int i;
		int compW;
		int compH;
		int frameW;
		int frameH;
		int diffW;
		int diffH;
		
		int screenWidth = Toolkit.getDefaultToolkit().getScreenSize().width;
	    int screenHeight = Toolkit.getDefaultToolkit().getScreenSize().height;
	    System.err.println("imageNamesIndex = " + imageNamesIndex);
	    for ( i = 0; i < imageNamesIndex; i++ ) {
	
		    imageFrame[i] = new ViewJFrameImage(images[i]);
			imageFrame[i].setLocation(screenWidth, screenHeight);
			imageComp[i] = imageFrame[i].getComponentImage();
			imageComp[i].setName("MultiViewSingle_" + i);
			
				
			compW = imageComp[i].getWidth();
			compH = imageComp[i].getHeight();
	
			// System.err.println("compW = " + compW + " compH = " + compH);
			
			frameW = imageFrame[i].getWidth();
			frameH = imageFrame[i].getHeight();
	
			diffW = frameW - compW;
			diffH = frameH - compH;
			
			currentSlice = imageComp[i].getSlice();
			// System.err.println(" zoom X = " + imageComp[i].getZoomX());
			newZoom = 1.1f * imageComp[i].getZoomX();
		    imageComp[i].setZoom(newZoom, newZoom);
		    imageComp[i].show(0, currentSlice, true);
	
			imageFrame[i].updateImages();
	    }
		
	}

	
	public void equalScaleImageLight() {

		float newZoom;
		int i;
		int compW;
		int compH;
		int frameW;
		int frameH;
		int diffW;
		int diffH;
		
		int screenWidth = Toolkit.getDefaultToolkit().getScreenSize().width;
	    int screenHeight = Toolkit.getDefaultToolkit().getScreenSize().height;
	    
	    int actualFrameWidth = screenWidth;
		int actualFrameHeight = screenHeight;

		float panelWidth = actualFrameWidth / 2;
		float panelHeight = actualFrameHeight / 2;
	    
	    System.err.println("imageNamesIndex = " + imageNamesIndex);
	    for ( i = 0; i < imageNamesIndex; i++ ) {
	
		    imageFrame[i] = new ViewJFrameImage(images[i]);
			imageFrame[i].setLocation(screenWidth, screenHeight);
			imageComp[i] = imageFrame[i].getComponentImage();
			imageComp[i].setName("MultiViewSingle_" + i);
			
				
			compW = imageComp[i].getWidth();
			compH = imageComp[i].getHeight();
	
			// System.err.println("compW = " + compW + " compH = " + compH);
			
			frameW = imageFrame[i].getWidth();
			frameH = imageFrame[i].getHeight();
	
			diffW = frameW - compW;
			diffH = frameH - compH;
			
			currentSlice = imageComp[i].getSlice();
			
			float zoomFactorX = panelWidth / compW;
			float zoomFactorY = panelHeight / compH;

			float zoomX0 = imageComp[i].getZoomX()* 1.1f;
			float zoomY0 = imageComp[i].getZoomY();
			
			float zoomX = zoomX0 * zoomFactorX;
			float zoomY = zoomY0 * zoomFactorY;
			
			// System.err.println(" zoom X = " + imageComp[i].getZoomX());
			// newZoom = 1.1f * imageComp[i].getZoomX();
		    imageComp[i].setZoom(zoomX, zoomY);
		    imageComp[i].show(0, currentSlice, true);
	
			imageFrame[i].updateImages();
			
			
	    }
	    // updateImage(imageActiveIndex);
		
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
				
				if ( cmd.equals("Soft Tissue")) {
					imageComp[imageActiveIndex].setWindLevel(450, 50);
				} else if ( cmd.equals("Bone")) {
					imageComp[imageActiveIndex].setWindLevel(2500, 450);
				} else if ( cmd.equals("Lung")) {
					imageComp[imageActiveIndex].setWindLevel(2402,176);
				} else if ( cmd.equals("Measure")) {
					invokeMeasure();
				} 
				
				for ( int i = 0; i < imageNamesIndex; i++ ) {
					if( cmd.equals(images[i].getImageName() )) {
						updateImage(i);
						updateSlider();
						break;
					}
					
				}	
		}
	}

	public void updateImage(int index) {
		
		GridBagLayout gbLayout = new GridBagLayout();
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.fill = GridBagConstraints.NONE;
		gbc.anchor = GridBagConstraints.CENTER;
		gbc.weightx = 0;
		gbc.weighty = 0;
		
		imageActiveIndex = index;
				
		int screenWidth = Toolkit.getDefaultToolkit().getScreenSize().width;
		int screenHeight = Toolkit.getDefaultToolkit().getScreenSize().height;
		
		int actualFrameWidth = screenWidth;
		int actualFrameHeight = screenHeight;

		float panelWidth = actualFrameWidth / 2;
		float panelHeight = actualFrameHeight / 2;
		
		int currentSlice0 = imageComp[imageActiveIndex].getSlice();
	    
		UI.setActiveFrame(imageFrame[imageActiveIndex]);
		imageFrame[imageActiveIndex].setActiveImage(images[imageActiveIndex]);
        
		float compW = imageComp[imageActiveIndex].getWidth();
		float compH = imageComp[imageActiveIndex].getHeight();

		
		float zoomFactorX = panelWidth / compW;
		float zoomFactorY = panelHeight / compH;

		float zoomX0 = imageComp[imageActiveIndex].getZoomX()* 1.1f;
		float zoomY0 = imageComp[imageActiveIndex].getZoomY();
		
		float zoomX = zoomX0 * zoomFactorX;
		float zoomY = zoomY0 * zoomFactorY;
		 
		imageComp[imageActiveIndex].setZoom(zoomX, zoomX);
		imageComp[imageActiveIndex].show(0, currentSlice0, true);
		
		
		quadImagePanel.removeAll();
		quadImagePanel.setLayout(gbLayout);
		quadImagePanel.setBackground(Color.black);
		quadImagePanel.add(imageComp[imageActiveIndex],gbc);
		quadImagePanel.repaint();
		
		
		pack();
		this.validate();
		
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
	 * keyPressed event method for KeyListener.
	 * 
	 * @param e
	 *            KeyEvent
	 */
	public void keyPressed(final KeyEvent e) {
		
		final int keyCode = e.getKeyCode();

		if (!pressed) {
			pressed = true;
			if (keyCode == KeyEvent.VK_ESCAPE) {
				System.err.println("escape key pressed");
				stopRecording();
				this.setVisible(false);
				this.dispose();
			} else if (keyCode == KeyEvent.VK_F1) {
				System.err.println("F1 key pressed");
				captureComponent(getContentPane());
			} else if (keyCode == KeyEvent.VK_F2) {
				changeIcon("WhiteCircle_550.png", "BlackCircle_550.png");
			} else if (keyCode == KeyEvent.VK_F3) {
				changeIcon("WhiteCircle_400.png", "BlackCircle_400.png");
			} else if (keyCode == KeyEvent.VK_F4) {
				changeIcon("WhiteCircle.png", "BlackCircle.png");
			} else if (keyCode == KeyEvent.VK_H) {
				currentSlice = sliceNumCache;
				zImageSlider[imageActiveIndex].setValue(currentSlice);
				imageComp[imageActiveIndex].setSlice(sliceNumCache);
				imageFrame[imageActiveIndex].updateImages(true);
			} else if (keyCode == KeyEvent.VK_R) {
				sliceNumCache = imageComp[imageActiveIndex].getSlice();
				System.err.println("sliceNumCache insert = " + sliceNumCache);
			} else if (keyCode == KeyEvent.VK_D) {
				sliceNumCache = 0;
				currentSlice = sliceNumCache;
				zImageSlider[imageActiveIndex].setValue(currentSlice);
				System.err.println("sliceNumCache delete = " + sliceNumCache);
			}

			// pass the key bindings to the underlying image (plb)
			String command = null;
			final KeyStroke ks = KeyStroke.getKeyStrokeForEvent(e);

			command = Preferences.getShortcutCommand(ks);

			if (command != null) {
				parentFrame.actionPerformed(new ActionEvent(ks, 0, command));
			}
		}

	}
	
	/**
	 * keyReleased event method for KeyListener.
	 * 
	 * @param e
	 *            KeyEvent
	 */
	public void keyReleased(final KeyEvent e) {
         pressed = false;
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
			DecimalFormat df = new DecimalFormat();
			df.setMaximumFractionDigits(2);
			
			if (e.isAltDown()) {
				int deltaX = origin.x - e.getX();
				int deltaY = origin.y - e.getY();
				String location = new String();
				JViewport viewPort0 = imageScroll.getViewport();
				if (viewPort0 != null) {
					Rectangle view = viewPort0.getViewRect();
					view.x += deltaX;
					view.y += deltaY;

					imageComp[imageActiveIndex].scrollRectToVisible(view);
					JViewport viewPort = imageScroll.getViewport();
					Rectangle viewRectangle = viewPort.getViewRect();
					Point viewPt = viewPort.getLocationOnScreen();
					Point compPt = imageComp[imageActiveIndex].getLocationOnScreen();
					int upperLeftCompX = compPt.x;
					int upperLeftCompY = compPt.y;
					int upperLeftViewX = viewPt.x;
					int upperLeftViewY = viewPt.y;
					int viewWidth = viewRectangle.width;
					int viewHeight = viewRectangle.height;
					Rectangle compRectangle = imageComp[imageActiveIndex].getBounds();
					
					int compWidth = compRectangle.width;
					int compHeight = compRectangle.height;
					double viewHeightToCompHeight = (double)viewHeight/(double)compHeight;
					double viewWidthToCompWidth = (double)viewWidth/(double)compWidth;
					int diffX = Math.max(0, upperLeftViewX - upperLeftCompX);
					int diffY = Math.max(0, upperLeftViewY - upperLeftCompY);
					if ((viewWidth >= compWidth) && (viewHeight >= compHeight)) {
						// All of image present
						location += ("0, 0" + ", ");
						location += ((images[imageActiveIndex].getExtents()[0] - 1) + ", " + "0" + ",");
						location += ("0," + (images[imageActiveIndex].getExtents()[1] - 1) + ", ");
						location += ((images[imageActiveIndex].getExtents()[0] - 1) + ", " + (images[imageActiveIndex].getExtents()[1] - 1) + ", ");	
					}
					else if ((viewWidth < compWidth) && (viewHeight >= compHeight)) {
						// Right part of image clipped but all of image height present
						location += (df.format(diffX*viewWidthToCompWidth) + ", " + "0" + ", ");
						location += (df.format(Math.min((images[imageActiveIndex].getExtents()[0] - 1), 
										((images[imageActiveIndex].getExtents()[0] - 1 + diffX)*viewWidthToCompWidth))) + ", " + "0" + ", ");
						location += (df.format(diffX*viewWidthToCompWidth) + ", " + (images[imageActiveIndex].getExtents()[1] - 1) + ", ");
						location += (df.format(Math.min((images[imageActiveIndex].getExtents()[0] - 1), 
										((images[imageActiveIndex].getExtents()[0] - 1 + diffX)*viewWidthToCompWidth))) + 
										"," + (images[imageActiveIndex].getExtents()[1] - 1) + ", ");
					}
					else if ((viewWidth >= compWidth) && (viewHeight < compHeight)) {
						// Bottom part of image clipped but all of image width present
						location += ("0" + ", " + df.format(diffY*viewHeightToCompHeight) + ", ");
						location += ((images[imageActiveIndex].getExtents()[0] - 1) + "," + df.format(diffY*viewHeightToCompHeight) + ", ");
						location += ("0" + ", " + df.format(Math.min((images[imageActiveIndex].getExtents()[1] - 1),  ((images[imageActiveIndex].getExtents()[1] - 1 + diffY)*viewHeightToCompHeight))) + ", ");
						location += ((images[imageActiveIndex].getExtents()[0] - 1) + "," + df.format(Math.min((images[imageActiveIndex].getExtents()[1] - 1), 
										((images[imageActiveIndex].getExtents()[1] - 1 + diffY)*viewHeightToCompHeight))) + ", ");
					}
					else if ((viewWidth < compWidth) && (viewHeight < compHeight)) {
						// Bottom and right part of images clipped
						location += ( df.format(diffX*viewWidthToCompWidth) + "," +
								df.format(diffY*viewHeightToCompHeight) + ", ");
						location += ( df.format(Math.min((images[imageActiveIndex].getExtents()[0] - 1), 
										((images[imageActiveIndex].getExtents()[0] - 1 + diffX)*viewWidthToCompWidth))) +
								", " + df.format(diffY*viewHeightToCompHeight) + ", ");
						location += ( df.format(diffX*viewWidthToCompWidth) + ", " + 
								 df.format(Math.min((images[imageActiveIndex].getExtents()[1] - 1), 
											((images[imageActiveIndex].getExtents()[1] - 1 + diffY)*viewHeightToCompHeight))) + ", ");
						location += (df.format(Math.min((images[imageActiveIndex].getExtents()[0] - 1), 
										((images[imageActiveIndex].getExtents()[0] - 1 + diffX)*viewWidthToCompWidth))) + ", " + 
										 df.format(Math.min((images[imageActiveIndex].getExtents()[1] - 1), 
													((images[imageActiveIndex].getExtents()[1] - 1 + diffY)*viewHeightToCompHeight))) + ", ");
					}
					//System.out.println("upperLeftViewX = " + upperLeftViewX + " upperLeftViewY = " + upperLeftViewY);
					//System.out.println("viewWidth = " + viewWidth + " viewHeight = " + viewHeight);
					//System.out.println("upperLeftCompX = " + upperLeftCompX + " upperLeftCompY = " + upperLeftCompY);
					//System.out.println("CompRectangle.x = " + compRectangle.x + " compRectangle.y = " + compRectangle.y);
					//System.out.println("compWidth = " + compWidth + " compHeight = " + compHeight);
					imageComp[imageActiveIndex].setFullScreenModeLocation(location);
					imageComp[imageActiveIndex].recordPanning();
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
			DecimalFormat df = new DecimalFormat();
			df.setMaximumFractionDigits(2);
			
			// System.err.println("single viewer mousePressed");
			origin = new Point(event.getPoint());
			// Get the top left corner of the upper left white circle in the screen's coordinate space
			Point pul = label5.getLocationOnScreen();
			// System.out.println("Upper left circle upper left corner x = " + pul.x + " y = "+ pul.y);
			// Get the width and height of the upper left circle
			Dimension dul = label5.getSize();
			// System.out.println("Upper left circle dimensions width = " + dul.width + " height = " + dul.height);
			// Get the center of the upper left circle
			double culx = pul.x + dul.width/2.0;
			double culy = pul.y + dul.height/2.0;
			// System.out.println("Center of upper left circle x = " + culx + " y = " + culy);
			// Note that the upper left corner of JViewport of imageScroll and the quadImagePanel upper left corner were
			// initially identical at 81,80.
			// However, while the image scroll location always stayed at 81,80 the quad image panel
			// upper left corner location took on values of (81,3), (81,56), and (81,-102), so
			// do not use the quad image panel upper left corner value
			JViewport viewPort0 = imageScroll.getViewport();
			Point vul = viewPort0.getLocationOnScreen();
			// System.out.println("Upper left corner of JViewport of imageScroll x = " + vul.x + " y = " + vul.y);
			// Get the top left corner of the quadImagePanel in the screen's coordinate space
			//Point qul = quadImagePanel.getLocationOnScreen();
			//System.out.println("quad image panel upper left corner x = " + qul.x + " y = " + qul.y);
			Rectangle rect = imageComp[imageActiveIndex].getBounds();
            // System.out.println("Image upper left corner in screen coordinates at x = " + (vul.x + rect.x) + " y = " + (vul.y + rect.y));
            // System.out.println("Image upper right corner in screen coordinates at x = " + (vul.x + rect.x + rect.width) + " y = " + (vul.y + rect.y));
            // System.out.println("Image lower left corner in screen coordinates at x = " + (vul.x + rect.x) + " y = " + (vul.y + rect.y + rect.height));
            // System.out.println("Image lower right corner in screen coordinates at x = " + (vul.x + rect.x + rect.width) + " y = " + 
            // (vul.y + rect.y + rect.height));
			Point originScreen = event.getLocationOnScreen();
			// System.out.println("Event location in screen coordinates x = " + originScreen.x + " y = " +originScreen.y);
			// Distance from center of upper circle
			double dx = originScreen.x - culx;
			double dy = originScreen.y - culy;
			// System.out.println("Event distance from upper left circle center dx = " + dx + " dy = " + dy);
			
			// System.err.println("in mouse pressed");
			int currentSlice0 = imageComp[imageActiveIndex].getSlice();

			if (event.getButton() == MouseEvent.BUTTON1 && event.isShiftDown()) {

				// imageScroll.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
				// imageScroll.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
				 String location = new String("");        
			       
				// quad 0
				float oldZoom0 = imageComp[imageActiveIndex].getZoomX();
				float newZoom0 = 1;
				if (imageComp[imageActiveIndex].getZoomX() < 1.5f) {
					newZoom0 = 2.0f * imageComp[imageActiveIndex].getZoomX();
				} else {
					newZoom0 = imageComp[imageActiveIndex].getZoomX() + 1.0f;
				}
				imageComp[imageActiveIndex].setZoom(newZoom0, newZoom0);

				Vector2f oldCrosshairPoint0 = new Vector2f(event.getX(), event.getY());

				if (oldCrosshairPoint0 != null) {
					final int newX = MipavMath.round((oldCrosshairPoint0.X * newZoom0) / oldZoom0);
					final int newY = MipavMath.round((oldCrosshairPoint0.Y * newZoom0) / oldZoom0);
					imageComp[imageActiveIndex].setCenter(new Vector3f(newX, newY, currentSlice0));
					adjustScrollbars(newX, newY, imageScroll);
				}
			
			
				JViewport viewPort = imageScroll.getViewport();
				
				Rectangle viewRectangle = viewPort.getViewRect();
				Point viewPt = viewPort.getLocationOnScreen();
				Point compPt = imageComp[imageActiveIndex].getLocationOnScreen();
				int upperLeftCompX = compPt.x;
				int upperLeftCompY = compPt.y;
				int upperLeftViewX = viewPt.x;
				int upperLeftViewY = viewPt.y;
				int viewWidth = viewRectangle.width;
				int viewHeight = viewRectangle.height;
				Rectangle compRectangle = imageComp[imageActiveIndex].getBounds();
				
				int compWidth = compRectangle.width;
				int compHeight = compRectangle.height;
				double viewHeightToCompHeight = (double)viewHeight/(double)compHeight;
				double viewWidthToCompWidth = (double)viewWidth/(double)compWidth;
				int diffX = Math.max(0, upperLeftViewX - upperLeftCompX);
				int diffY = Math.max(0, upperLeftViewY - upperLeftCompY);
				if ((viewWidth >= compWidth) && (viewHeight >= compHeight)) {
					// All of image present
					location += ("0, 0" + ", ");
					location += ((images[imageActiveIndex].getExtents()[0] - 1) +", " + "0" + ", ");
					location += ("0" + ", " + (images[imageActiveIndex].getExtents()[1] - 1) + ", ");
					location += ((images[imageActiveIndex].getExtents()[0] - 1) + ", " + (images[imageActiveIndex].getExtents()[1] - 1) + ", ");	
				}
				else if ((viewWidth < compWidth) && (viewHeight >= compHeight)) {
					// Right part of image clipped but all of image height present
					location += (df.format(diffX*viewWidthToCompWidth) + "," + "0" + ", ");
					location += (df.format(Math.min((images[imageActiveIndex].getExtents()[0] - 1), 
									((images[imageActiveIndex].getExtents()[0] - 1 + diffX)*viewWidthToCompWidth))) + "," + "0" + ", ");
					location += (df.format(diffX*viewWidthToCompWidth) +  ", " + (images[imageActiveIndex].getExtents()[1] - 1) + ", ");
					location += (df.format(Math.min((images[imageActiveIndex].getExtents()[0] - 1), 
									((images[imageActiveIndex].getExtents()[0] - 1 + diffX)*viewWidthToCompWidth))) + 
									"," + (images[imageActiveIndex].getExtents()[1] - 1) + ", ");
				}
				else if ((viewWidth >= compWidth) && (viewHeight < compHeight)) {
					// Bottom part of image clipped but all of image width present
					location += ("0" + "," + df.format(diffY*viewHeightToCompHeight) + ", ");
					location += ((images[imageActiveIndex].getExtents()[0] - 1) + ", " + df.format(diffY*viewHeightToCompHeight) + ", ");
					location += ("0" + "," + df.format(Math.min((images[imageActiveIndex].getExtents()[1] - 1), ((images[imageActiveIndex].getExtents()[1] - 1 + diffY)*viewHeightToCompHeight))) + ", ");
					location += ((images[imageActiveIndex].getExtents()[0] - 1) + ", " + df.format(Math.min((images[imageActiveIndex].getExtents()[1] - 1),  
							((images[imageActiveIndex].getExtents()[1] - 1 + diffY)*viewHeightToCompHeight))) + ", ");
				}
				else if ((viewWidth < compWidth) && (viewHeight < compHeight)) {
					// Bottom and right part of images clipped
					location += (df.format(diffX*viewWidthToCompWidth) + ", " + df.format(diffY*viewHeightToCompHeight) + ", ");
					location += (df.format(Math.min((images[imageActiveIndex].getExtents()[0] - 1),  ((images[imageActiveIndex].getExtents()[0] - 1 + diffX)*viewWidthToCompWidth))) +
							", " + df.format(diffY*viewHeightToCompHeight) + ", ");
					location += (df.format(diffX*viewWidthToCompWidth) + ", " +  df.format(Math.min((images[imageActiveIndex].getExtents()[1] - 1), 
										((images[imageActiveIndex].getExtents()[1] - 1 + diffY)*viewHeightToCompHeight))) + ", ");
					location += (df.format(Math.min((images[imageActiveIndex].getExtents()[0] - 1), 
									((images[imageActiveIndex].getExtents()[0] - 1 + diffX)*viewWidthToCompWidth))) + ", " + 
									 df.format(Math.min((images[imageActiveIndex].getExtents()[1] - 1), 
										((images[imageActiveIndex].getExtents()[1] - 1 + diffY)*viewHeightToCompHeight))) + ", ");
				}
				//System.out.println("upperLeftViewX = " + upperLeftViewX + " upperLeftViewY = " + upperLeftViewY);
				//System.out.println("viewWidth = " + viewWidth + " viewHeight = " + viewHeight);
				//System.out.println("upperLeftCompX = " + upperLeftCompX + " upperLeftCompY = " + upperLeftCompY);
				//System.out.println("CompRectangle.x = " + compRectangle.x + " compRectangle.y = " + compRectangle.y);
				//System.out.println("compWidth = " + compWidth + " compHeight = " + compHeight);
				imageComp[imageActiveIndex].setFullScreenModeLocation(location);
				imageComp[imageActiveIndex].recordZoom(true);
			} else if (event.getButton() == MouseEvent.BUTTON1 && event.isControlDown()) {
				// imageScroll.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED);
				// imageScroll.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
				 String location = new String("");        
			       
				// quad 0
				float oldZoom0 = imageComp[imageActiveIndex].getZoomX();
				float newZoom0 = 1;
				if (imageComp[imageActiveIndex].getZoomX() > 1.5f) {
					newZoom0 = imageComp[imageActiveIndex].getZoomX() - 1.0f;
				} else {
					newZoom0 = 0.5f * imageComp[imageActiveIndex].getZoomX();
				}
				// System.err.println("newZoom0 = " + newZoom0);
				imageComp[imageActiveIndex].mouseExited(event);
				imageComp[imageActiveIndex].setZoom(newZoom0, newZoom0);

				Vector2f oldCrosshairPoint0 = new Vector2f(event.getX(), event.getY());

				if (oldCrosshairPoint0 != null) {
					final int newX = MipavMath.round((oldCrosshairPoint0.X * newZoom0) / oldZoom0);
					final int newY = MipavMath.round((oldCrosshairPoint0.Y * newZoom0) / oldZoom0);
					imageComp[imageActiveIndex].setCenter(new Vector3f(newX, newY, currentSlice0));
					adjustScrollbars(newX, newY, imageScroll);
				}
				
				JViewport viewPort = imageScroll.getViewport();
				int vh = viewPort.getHeight();
				int vw = viewPort.getWidth();
				int compW = imageComp[imageActiveIndex].getSize().width;
				int compH = imageComp[imageActiveIndex].getSize().height;
				int dim[] = images[imageActiveIndex].getExtents();
				// System.err.println("newZoom0 = " + newZoom0 + " " + " vh = " + vh + "  vw = " + vw + "  dim[0] = " + dim[0] + " dim[1] = " + dim[1] + "  compW = " + compW + "  compH = " + compH);
				
				Rectangle viewRectangle = viewPort.getViewRect();
				Point viewPt = viewPort.getLocationOnScreen();
				Point compPt = imageComp[imageActiveIndex].getLocationOnScreen();
				int upperLeftCompX = compPt.x;
				int upperLeftCompY = compPt.y;
				int upperLeftViewX = viewPt.x;
				int upperLeftViewY = viewPt.y;
				int viewWidth = viewRectangle.width;
				int viewHeight = viewRectangle.height;
				Rectangle compRectangle = imageComp[imageActiveIndex].getBounds();
				
				int compWidth = compRectangle.width;
				int compHeight = compRectangle.height;
				double viewHeightToCompHeight = (double)viewHeight/(double)compHeight;
				double viewWidthToCompWidth = (double)viewWidth/(double)compWidth;
				int diffX = Math.max(0, upperLeftViewX - upperLeftCompX);
				int diffY = Math.max(0, upperLeftViewY - upperLeftCompY);
				if ((viewWidth >= compWidth) && (viewHeight >= compHeight)) {
					// All of image present
					location += ("0, 0" + ", ");
					location += ((images[imageActiveIndex].getExtents()[0] - 1) +", " + "0" + ", ");
					location += ("0" + ", " + (images[imageActiveIndex].getExtents()[1] - 1) + ", ");
					location += ((images[imageActiveIndex].getExtents()[0] - 1) + ", " + (images[imageActiveIndex].getExtents()[1] - 1) + ", ");	
				}
				else if ((viewWidth < compWidth) && (viewHeight >= compHeight)) {
					// Right part of image clipped but all of image height present
					location += (df.format(diffX*viewWidthToCompWidth) + "," + "0" + ", ");
					location += (df.format(Math.min((images[imageActiveIndex].getExtents()[0] - 1), 
									((images[imageActiveIndex].getExtents()[0] - 1 + diffX)*viewWidthToCompWidth))) + "," + "0" + ", ");
					location += (df.format(diffX*viewWidthToCompWidth) +  ", " + (images[imageActiveIndex].getExtents()[1] - 1) + ", ");
					location += (df.format(Math.min((images[imageActiveIndex].getExtents()[0] - 1), 
									((images[imageActiveIndex].getExtents()[0] - 1 + diffX)*viewWidthToCompWidth))) + 
									"," + (images[imageActiveIndex].getExtents()[1] - 1) + ", ");
				}
				else if ((viewWidth >= compWidth) && (viewHeight < compHeight)) {
					// Bottom part of image clipped but all of image width present
					location += ("0" + "," + df.format(diffY*viewHeightToCompHeight) + ", ");
					location += ((images[imageActiveIndex].getExtents()[0] - 1) + ", " + df.format(diffY*viewHeightToCompHeight) + ", ");
					location += ("0" + "," + df.format(Math.min((images[imageActiveIndex].getExtents()[1] - 1), ((images[imageActiveIndex].getExtents()[1] - 1 + diffY)*viewHeightToCompHeight))) + ", ");
					location += ((images[imageActiveIndex].getExtents()[0] - 1) + ", " + df.format(Math.min((images[imageActiveIndex].getExtents()[1] - 1),  
							((images[imageActiveIndex].getExtents()[1] - 1 + diffY)*viewHeightToCompHeight))) + ", ");
				}
				else if ((viewWidth < compWidth) && (viewHeight < compHeight)) {
					// Bottom and right part of images clipped
					location += (df.format(diffX*viewWidthToCompWidth) + ", " + df.format(diffY*viewHeightToCompHeight) + ", ");
					location += (df.format(Math.min((images[imageActiveIndex].getExtents()[0] - 1),  ((images[imageActiveIndex].getExtents()[0] - 1 + diffX)*viewWidthToCompWidth))) +
							", " + df.format(diffY*viewHeightToCompHeight) + ", ");
					location += (df.format(diffX*viewWidthToCompWidth) + ", " +  df.format(Math.min((images[imageActiveIndex].getExtents()[1] - 1), 
										((images[imageActiveIndex].getExtents()[1] - 1 + diffY)*viewHeightToCompHeight))) + ", ");
					location += (df.format(Math.min((images[imageActiveIndex].getExtents()[0] - 1), 
									((images[imageActiveIndex].getExtents()[0] - 1 + diffX)*viewWidthToCompWidth))) + ", " + 
									 df.format(Math.min((images[imageActiveIndex].getExtents()[1] - 1), 
										((images[imageActiveIndex].getExtents()[1] - 1 + diffY)*viewHeightToCompHeight))) + ", ");
				}
				imageComp[imageActiveIndex].setFullScreenModeLocation(location);
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
		if ( event.getButton() == MouseEvent.BUTTON1 && event.isShiftDown() || 
				 event.getButton() == MouseEvent.BUTTON1 && event.isControlDown() ||
				 event.getButton() == MouseEvent.BUTTON1 && event.isAltDown() ) {
			imageScroll.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER);
			imageScroll.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
			
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
	public void stateChanged(ChangeEvent e) {
		Object source = e.getSource();
		if (source == zImageSlider[imageActiveIndex]) {
			currentSlice = zImageSlider[imageActiveIndex].getValue();
			imageComp[imageActiveIndex].setImageSlice(currentSlice);
			slicesTextField[imageActiveIndex].setText(Integer.toString(currentSlice));
			imageFrame[imageActiveIndex].updateImages(true);
		}
	}

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

	
	public void invokeMeasure() {
		 imageFrame[imageActiveIndex].getVOIManager().doVOI(CustomUIBuilder.PARAM_VOI_LINE.getActionCommand());
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
			
			images = new ModelImage[imageNamesIndex];
			imageComp = new ViewJComponentEditImage[imageNamesIndex];
			imageFrame = new ViewJFrameImage[imageNamesIndex];
			
			sliderMax = new JLabel[imageNamesIndex];
			zImageSlider = new JSlider[imageNamesIndex];
			slicesTextField = new JTextField[imageNamesIndex];
			zDim = new int[imageNamesIndex];
			
			
			try {
				// read target images
				imageIO = new FileIO();

				for (i = 0; i < imageNamesIndex; i++) {
					System.err.println(imageNames[i]);
					images[i] = imageIO.readImage(imageNames[i]);
					// new ViewJFrameImage(images[i]);
				}

				equalScaleImage();


			} catch (Exception e) {
				e.printStackTrace();
			}

		}
	}
	
	private void traverse(File file) {
		String dirName = file.toString();
		// System.err.println(dirName);
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();

		if (dirName.substring(begin, end).startsWith("Axial") && dirName.substring(begin, end).endsWith(".xml")) {
			imageNames[imageNamesIndex] = new String();
			imageNames[imageNamesIndex++] = file.toString();
		}

		if (dirName.substring(begin, end).startsWith("MIP") && dirName.substring(begin, end).endsWith(".xml")) {
			imageNames[imageNamesIndex] = new String();
			imageNames[imageNamesIndex++] = file.toString();
		}

		/*
		if (dirName.substring(begin, end).startsWith("image") && dirName.substring(begin, end).endsWith(".xml")
				&& dirName.contains("Sagittal")) {
			imageNames[imageNamesIndex] = new String();
			imageNames[imageNamesIndex++] = file.toString();
		}
		*/ 

	}
	

	private void changeIcon(String whiteCicleName, String blackCircleName) {
		System.err.println("changeICON");
		equalScaleImageLight();
		
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

		cornerImage = MipavUtil.getIcon(whiteCicleName);
		blackImage = MipavUtil.getIcon(blackCircleName);

		// leftPanel = new JPanel(new BorderLayout());
		leftPanel.removeAll();
		leftPanel.setBackground(Color.black);
		label1.setBackground(Color.black);
		label1.setIcon(blackImage);
		label2.setBackground(Color.black);
		label2.setIcon(blackImage);
		leftPanel.add(label1, BorderLayout.NORTH);
		leftPanel.add(label2, BorderLayout.SOUTH);

		// rightPanel = new JPanel(new BorderLayout());
		rightPanel.removeAll();
		rightPanel.setBackground(Color.black);
		label3.setBackground(Color.black);
		label3.setIcon(blackImage);
		label4.setBackground(Color.black);
		label4.setIcon(blackImage);
		rightPanel.add(label3, BorderLayout.NORTH);
		rightPanel.add(label4, BorderLayout.SOUTH);

		// topPanel = new JPanel(new BorderLayout());
		topPanel.removeAll();
		topPanel.setBackground(Color.black);
		label5.setBackground(Color.black);
		label5.setIcon(cornerImage);
		label6.setBackground(Color.black);
		label6.setIcon(cornerImage);
		topPanel.add(label5, BorderLayout.WEST);
		topPanel.add(label6, BorderLayout.EAST);

		// lowerPanel = new JPanel(new BorderLayout());
		lowerPanel.removeAll();
		lowerPanel.setBackground(Color.black);
		label7.setBackground(Color.black);
		label7.setIcon(cornerImage);
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

		
		int currentSlice0 = imageComp[imageActiveIndex].getSlice();

		int compW = imageComp[imageActiveIndex].getWidth();
		int compH = imageComp[imageActiveIndex].getHeight();

		float zoomFactorX = panelWidth / compH;
		float zoomFactorY = panelHeight / compH;

		float zoomX0 = imageComp[imageActiveIndex].getZoomX();
		float zoomY0 = imageComp[imageActiveIndex].getZoomY();
		
		float zoomX = zoomX0 * zoomFactorX;
		float zoomY = zoomY0 * zoomFactorY;

		imageComp[imageActiveIndex].setZoomExact(zoomY, zoomY);
		imageComp[imageActiveIndex].show(0, currentSlice0, true);
		
		// quadImagePanel = new JPanel();
		quadImagePanel.removeAll();
		quadImagePanel.setLayout(gbLayout);
		quadImagePanel.setBackground(Color.black);
		quadImagePanel.add(imageComp[imageActiveIndex], gbc);
		
		
		imageScroll = new JScrollPane(quadImagePanel,
	                ScrollPaneConstants.VERTICAL_SCROLLBAR_NEVER, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
		imageScroll.getVerticalScrollBar().setSize(new Dimension(0, 0));
		imageScroll.getHorizontalScrollBar().setSize(new Dimension(0, 0));
		imageScroll.getVerticalScrollBar().setVisible(false);
		imageScroll.getHorizontalScrollBar().setVisible(false);
		imageScroll.setOpaque(false);
		imageScroll.getViewport().setOpaque(false);
		imageScroll.setBorder(null);;
		

		buildSlider(rightPanel);
		
		
		getContentPane().removeAll();
		getContentPane().add(imageScroll, BorderLayout.CENTER);
		getContentPane().add(leftPanel, BorderLayout.WEST);
		getContentPane().add(rightPanel, BorderLayout.EAST);
		getContentPane().add(topPanel, BorderLayout.NORTH);
		getContentPane().add(lowerPanel, BorderLayout.SOUTH);
		getContentPane().setName("SingleMultiViewContentPane");

		
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setExtendedState(JFrame.MAXIMIZED_BOTH);
		// setUndecorated(true);
	     
		for ( int i = 0; i < imageNamesIndex; i++ ) {
			imageComp[i].addMouseWheelListener(this);
			imageComp[i].addMouseListener(this);
			imageComp[i].addMouseMotionListener(this);
			imageComp[i].addKeyListener(this);
			imageFrame[i].addKeyListener(this);
		}
		addKeyListener(this);
	    
		setSize(screenWidth, screenHeight);
		setMinimumSize(getSize());
		setVisible(true);
		setResizable(false);
		
		pack();
		this.validate();
		
	}


}
