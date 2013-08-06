import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogWinLevel;

import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.util.Vector;

import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;


public class PlugInDialogApplyTransferFunction extends JDialogStandaloneScriptablePlugin implements AlgorithmInterface, ItemListener, PreviewImageContainer, ChangeListener, KeyListener, MouseListener{

    //~ Static fields/initializers -------------------------------------------------------------------------------------

//	private static final long serialVersionUID = 3516843154999038969L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private PlugInAlgorithmApplyTransferFunction winLev = null;
    
    private String inputDir;
    
    private Vector<File> inputFiles = new Vector<File>();
    
    private JTextField dirChooserText;
    
    private JButton dirChooserButton;

	private ViewTableModel galleryModel;
	
	private JTable galleryTable;

	private JButton loadButton;
	
	private JButton helpButton;

	private ModelImage[] srcImages;

	private JDialogWinLevel[] srcInfo;

	private float minImage, maxImage, winMax;

	private JPanel previewPanel;

	private ViewUserInterface userInterface = ViewUserInterface.getReference();
    
    private static final ViewImageFileFilter niftiFilter = new ViewImageFileFilter(new String[] { ".img", ".nii" });
    
    /** Reference to the image that will be affected by the adjust of the window and level. */
    private ModelImage image;

    /** Average of the min and max extents of the transfer window that describes the window size. */
    private float level;

    private float min, max;

    /** DOCUMENT ME! */
    public JSlider levelSlider, minSlider;

    /** Stores the maximum slider value. */
    private int levelSliderMax;

    /** Reference to the LUT used to display the image. */
    private ModelLUT[] LUT;

    /** DOCUMENT ME! */
    private JPanel windowLevelPanel;

	private JPanel minMaxPanel;

    /** The size of the window. */
    private float window;

    /** DOCUMENT ME! */
    public JSlider windowSlider, maxSlider;

    /** Stores the minimum slider value. */
    private int windowSliderMax;

    /** DOCUMENT ME! */

    /** textfield inputs for window and level * */
    private JTextField winValTextField, levelValTextField, minValTextField, maxValTextField;

    /** the maxes and mins for window and level * */
    private float winMaxFloat, winMinFloat, levelMaxFloat, levelMinFloat;

    /** Three arrays to save the coordinates of the LUT's transfer fucntion. z[] not used. */
    private final float[] x = new float[4];

    /** DOCUMENT ME! */
    private final float[] y = new float[4];

    /** tabbed pane for w/L and min/max * */
    public JTabbedPane tabbedPane = new JTabbedPane();

    /** slope for mapping of min/max sliders * */
    public float minMaxSlope;

    /** b intercept for mapping of min/max sliders * */
    public float minMaxBInt;

    public boolean keyTyped = false;

	private JLabel sliderMinMax, sliderMinMin, sliderMaxMax, sliderMaxMin;
	
	private JLabel sliderWinMin, sliderWinMax, sliderLevMin, sliderLevMax;

	private ViewJFrameImage[] srcFrames;

	private JPanel workPanel;

	private JCheckBox mode;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogApplyTransferFunction() { }

    /**
     * Creates new dialog for distances within a cell from the geometric center using a plugin.
     *
     * @param  modal	Whether the dialog should be made modal.
     */
    public PlugInDialogApplyTransferFunction(boolean modal) {
        super(modal);

        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Event Processing ****************************
    // ************************************************************************

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Load")) {
        	if (setVariables(mode.isSelected())){
        		loadButton.setText("OK");
        		loadButton.setActionCommand("OK");
        	}
        } else if (command.equals("OK")) {
        	dispose();
        } else if (command.equals("BrowseDir")) {
        	final ViewDirectoryChooser chooser = new ViewDirectoryChooser();
        	String initDir = dirChooserText.getText();
        	final File initDirFile = new File(initDir);
        	if (!initDirFile.exists() || !initDirFile.isDirectory()) {
        		initDir = Preferences.getImageDirectory();
        	}
        	final String dir = chooser.chooseDirectory(initDir);
            if (dir != null) {
            	dirChooserText.setText(dir);
            } else {
            	dirChooserText.setText("");
            }
        } else if (command.equalsIgnoreCase("Help")) {
        	MipavUtil.showWebHelp("Changing_Image_Contrast#Adjust_Window_and_Level");
        } else if (command.equalsIgnoreCase("mode")) {
        	if (!mode.isSelected()) {
        		dirChooserButton.setEnabled(true);
        		dirChooserText.setEnabled(true);
        		pack();
        		validate();
        		repaint();
        	} else {
        		dirChooserButton.setEnabled(false);
        		dirChooserText.setEnabled(false);
        		pack();
        		validate();
        		repaint();
        	}
        } else {
            super.actionPerformed(event);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof PlugInAlgorithmApplyTransferFunction) {
            if (algorithm.isCompleted()) {
                insertScriptLine();
            }

            if (algorithm != null) {
                algorithm.finalize();
                algorithm = null;
            }

            //dispose();
            if (isExitRequired()) {
                System.exit(0);
                // ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
            } else {
                return;
            }
        }
    }
    
    public void setInputDir(String dir) {
    	inputDir = dir;
    	
    	File dirFile = new File(inputDir);
        if (!dirFile.exists()) {
        	MipavUtil.displayError("The directory selected does not exist.  Please choose another.");
        } else if (!dirFile.isDirectory()) {
        	MipavUtil.displayError("Please select a directory of images to process.");
        } else {
        	File[] dirListing;
        	dirListing = dirFile.listFiles(niftiFilter);
        	for (File file : dirListing) {
        		if (file.isDirectory()) {
            		System.err.println("Skipping directory:\t" + file.getName());
            	} else if (file.getName().startsWith(".")) {
            		System.err.println("Skipping file that starts with .:\t" + file.getName());
            	} else {
            		inputFiles.add(file);
            	}
        	}
        }
    }

    /**
     * Once all the necessary variables are set, call PlugInAlgorithmApplyTransferFunction
     */
    protected void callAlgorithm() {

        try {
            winLev = new PlugInAlgorithmApplyTransferFunction();

            // This is very important. Adding this object as a listener allows
            // the algorithm to
            // notify this object when it has completed or failed. See algorithm
            // performed event.
            // This is made possible by implementing AlgorithmedPerformed
            // interface
            winLev.addListener(this);
            
            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (winLev.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                winLev.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Unable to allocate enough memory");

            return;
        }
    }

//    /**
//     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
//     */
    private void init() {
    	
    	try {
            setIconImage(MipavUtil.getIconImage("winlevel.gif"));
        } catch (final Exception e) {
            // setIconImage() is not part of the Java 1.5 API - catch any runtime error on those systems
        }
    	
    	this.setLocationRelativeTo(userInterface.getMainFrame());
        setForeground(Color.black);
        setTitle("Apply Transfer Function");
        //int length = image.getExtents()[0] * image.getExtents()[1];

        
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setBorder(buildTitledBorder("Select Directory"));
        mainPanel.setPreferredSize(new Dimension(230, 120));

        mode = new JCheckBox();
        mode.setSelected(false);
        mode.setActionCommand("mode");
        mode.addActionListener(this);
        mode.setText("Use open images");
        mainPanel.add(mode, gbc);
        gbc.gridy++;
        
        dirChooserText = new JTextField(200);
        dirChooserText.setFont(serif12);
        dirChooserText.setText(Preferences.getImageDirectory());
        gbc.gridy++;
        gbc.gridx++;
        mainPanel.add(dirChooserText, gbc);
        gbc.gridy++;
        JPanel buttonPanel = new JPanel();
        dirChooserButton = new JButton("Browse");
        dirChooserButton.setActionCommand("BrowseDir");
        dirChooserButton.addActionListener(this);
        dirChooserButton.setFont(serif12B);
        dirChooserButton.setSize(new Dimension(30,10));
        
        loadButton = new JButton("Load");
        loadButton.setActionCommand("Load");
        loadButton.addActionListener(this);
        loadButton.setFont(serif12B);
        loadButton.setSize(new Dimension(30,10));
        
        helpButton = new JButton("Help");
        helpButton.setActionCommand("Help");
        helpButton.addActionListener(this);
        helpButton.setFont(serif12B);
        helpButton.setSize(new Dimension(30,10));
        
        
        
        gbc.gridy++;
        buttonPanel.add(dirChooserButton);
        buttonPanel.add(loadButton);
        buttonPanel.add(helpButton);
        mainPanel.add(buttonPanel, gbc);
        workPanel = new JPanel(new GridBagLayout());
//        workPanel.setPreferredSize(new Dimension(300, 400));
        
        galleryModel = new ViewTableModel();
        galleryTable = new JTable(galleryModel);
        
        JScrollPane gallery = new JScrollPane(galleryTable, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
        gallery.setBorder(buildTitledBorder("Image previews"));
        
        previewPanel = new JPanel();
        previewPanel.setBorder(buildTitledBorder("Current Image"));
        previewPanel.setPreferredSize(new Dimension(300, 300));

//        previewPanel.add(previewImage);

        getContentPane().add(mainPanel, BorderLayout.NORTH);
        getContentPane().add(workPanel, BorderLayout.CENTER);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();
    }

	/**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables(boolean mode) {
        if (!mode) {
	        inputDir = dirChooserText.getText();
	        File dirFile = new File(inputDir);
	        if (!dirFile.exists()) {
	        	MipavUtil.displayError("The directory selected does not exist.  Please choose another.");
	        	return false;
	        } else if (!dirFile.isDirectory()) {
	        	MipavUtil.displayError("Please select a directory of images to process.");
	        	return false;
	        } else {
	        	File[] dirListing;
	        	dirListing = dirFile.listFiles(niftiFilter);
	        	for (File file : dirListing) {
	        		if (file.isDirectory()) {
	            		System.err.println("Skipping directory:\t" + file.getName());
	            	} else if (file.getName().startsWith(".")) {
	            		System.err.println("Skipping file that starts with .:\t" + file.getName());
	            	} else {
	            		inputFiles.add(file);
	            	}
	        	}
	        	srcFrames = new ViewJFrameImage[inputFiles.size()];
	        	srcImages = new ModelImage[inputFiles.size()];
	        	srcInfo = new JDialogWinLevel[inputFiles.size()];
	        	LUT = new ModelLUT[inputFiles.size()];
	        	for(int count = 0; count < inputFiles.size(); count++){
	//        		srcImages[count] = io.readImage(inputFiles.get(count).getAbsolutePath());
	        		userInterface.openImageFrame(inputFiles.get(count).getAbsolutePath());
	        		srcFrames[count] = userInterface.getActiveImageFrame();
	        		srcImages[count] = userInterface.getActiveImageFrame().getActiveImage();
	        		srcInfo[count] = new JDialogWinLevel(srcImages[count], ViewJFrameBase.initLUT(srcImages[count]), true);
	        		LUT[count] = srcInfo[count].getLUT();
	        	}
	        	
	        	minImage = srcInfo[0].getMin();
	        	maxImage = srcInfo[0].getMax();
	        	winMax = srcInfo[0].getWinMax();
	        	
	        	for(int count = 0; count < srcImages.length; count++){
	        		if (minImage > srcInfo[count].getMin())
	        			minImage = srcInfo[count].getMin();
	        		if (maxImage < srcInfo[count].getMax())
	        			maxImage = srcInfo[count].getMax();
	        		if (winMax < srcInfo[count].getWinMax())
	        			winMax = srcInfo[count].getWinMax();        			
	        	}
	        	
	        	System.out.println(winMax);
	
		        image = srcImages[0];
		        
		        GridBagConstraints gbc = new GridBagConstraints();
		        gbc.gridwidth = 1;
		        gbc.gridheight = 1;
		        gbc.anchor = GridBagConstraints.WEST;
		        gbc.weightx = 1;
		        gbc.insets = new Insets(3, 3, 3, 3);
		        gbc.fill = GridBagConstraints.HORIZONTAL;
		        
		        windowLevelPanel = buildWindowLevelPanel();
		        minMaxPanel = buildMinMaxPanel();
		        buildLevelSlider(windowLevelPanel, gbc);
		        buildWindowSlider(windowLevelPanel, gbc);
		        buildMinSlider(minMaxPanel, gbc);
		        buildMaxSlider(minMaxPanel, gbc);
		        tabbedPane.setPreferredSize(new Dimension(200, 400));
		        tabbedPane.add("Level & Window", windowLevelPanel);
		        tabbedPane.add("Min & Max", minMaxPanel);
		        tabbedPane.addChangeListener(this);
		        
		        changeBounds(minImage, maxImage, winMax);
		        
		        workPanel.setSize(tabbedPane.getMaximumSize());
		        workPanel.add(tabbedPane);
	
		        pack();
		        validate();
		        repaint();
		        return true;
        	}
        } else {
        	Vector<Frame> openFrames = userInterface.getImageFrameVector();
        	
        	for (Frame frame : openFrames){
        		if(!(frame instanceof ViewJFrameImage)){
        			openFrames.remove(frame);
        		} 
        	}
        	
        	srcFrames = new ViewJFrameImage[openFrames.size()];
        	srcImages = new ModelImage[openFrames.size()];
        	srcInfo = new JDialogWinLevel[openFrames.size()];
        	LUT = new ModelLUT[openFrames.size()];
        	for(int count = 0; count < openFrames.size(); count++){
        		srcFrames[count] = (ViewJFrameImage) openFrames.get(count);
        		srcImages[count] = srcFrames[count].getActiveImage();
        		srcInfo[count] = new JDialogWinLevel(srcImages[count], ViewJFrameBase.initLUT(srcImages[count]), true);
        		LUT[count] = srcInfo[count].getLUT();
        	}
        	
        	minImage = srcInfo[0].getMin();
        	maxImage = srcInfo[0].getMax();
        	winMax = srcInfo[0].getWinMax();
        	
        	for(int count = 0; count < srcImages.length; count++){
        		if (minImage > srcInfo[count].getMin())
        			minImage = srcInfo[count].getMin();
        		if (maxImage < srcInfo[count].getMax())
        			maxImage = srcInfo[count].getMax();
        		if (winMax < srcInfo[count].getWinMax())
        			winMax = srcInfo[count].getWinMax();        			
        	}
        	
	        image = srcImages[0];
	        
	        GridBagConstraints gbc = new GridBagConstraints();
	        gbc.gridwidth = 1;
	        gbc.gridheight = 1;
	        gbc.anchor = GridBagConstraints.WEST;
	        gbc.weightx = 1;
	        gbc.insets = new Insets(3, 3, 3, 3);
	        gbc.fill = GridBagConstraints.HORIZONTAL;
	        
	        windowLevelPanel = buildWindowLevelPanel();
	        minMaxPanel = buildMinMaxPanel();
	        buildLevelSlider(windowLevelPanel, gbc);
	        buildWindowSlider(windowLevelPanel, gbc);
	        buildMinSlider(minMaxPanel, gbc);
	        buildMaxSlider(minMaxPanel, gbc);
	        tabbedPane.setPreferredSize(new Dimension(200, 400));
	        tabbedPane.add("Level & Window", windowLevelPanel);
	        tabbedPane.add("Min & Max", minMaxPanel);
	        tabbedPane.addChangeListener(this);
	        
	        changeBounds(minImage, maxImage, winMax);
	        
	        workPanel.setSize(tabbedPane.getMaximumSize());
	        workPanel.add(tabbedPane);

	        pack();
	        validate();
	        repaint();
	        return true;
        }
    }
    
    
    private void changeBounds(float min, float max, float winMax) {   
    	float minTemp = (min - minMaxBInt) / minMaxSlope;
    	for (int count = 0; count < srcImages.length; count++){
    		image = srcImages[count];
    		image.setMin(min);
    		image.setMax(max);
    		minSlider.setMinimum((int) minTemp);
    		maxSlider.setMinimum((int) minTemp);
    		calcMinMaxSlope(image);
    		calcMinMax();
    		
    		float levMinTemp = ( (min - minImage) * levelSliderMax) / (maxImage - minImage);
    		float levMaxTemp = ( (max - minImage) * levelSliderMax) / (maxImage - minImage);
    		levelSlider.setMinimum((int) levMinTemp);
    		levelSlider.setMaximum((int) levMaxTemp);
    		levelMinFloat = min;
    		levelMaxFloat = max;
    		
    		float winTemp = (winMax * windowSliderMax) / (2 * (maxImage - minImage));
    		windowSlider.setMaximum((int) winTemp);
    		winMaxFloat = winMax;
    	}
    }


	/**
     * Setting location of window-level adjustment panel based on the amount of space available near the image window.
     */
    public void locateDialog() {

        if ( (parentFrame.getLocation().x - getSize().width) > 0) {
            setLocation(parentFrame.getLocation().x - getSize().width, parentFrame.getLocation().y);
        } else {
            final int tmp = (parentFrame.getLocation().x + parentFrame.getSize().width);

            setLocation(tmp, parentFrame.getLocation().y + 30);
        }
    }

    /**
     * Overides the super.setVisible(boolean b) (which also locates a panel in the center of the screen), to use the
     * super.setVisibleStandard(boolean b) which merely displays the Swing object onscreen. (as if it
     * super.super.setVisible(boolean b) could be called) <i>when</i> the property in MipavProps "BoundWindowLevel" is
     * <code>false</code> <i>or</i> when there is no property. The window/level dialog is "free." When there is
     * "BoundWindowLevel" and when it is <code>true</code>, the window/level dialog will get relocated to next to the
     * image window and then redrawn.
     * 
     * @see gov.nih.mipav.view.JDialogBase#setVisible(boolean)
     * @see gov.nih.mipav.view.JDialogBase#setVisibleStandard(boolean)
     * @see JDialogWinLevel#locateDialog()
     */
    public void setVisible(final boolean vis) {
        final String bound = Preferences.getProperty("BoundWindowLevel");

        if (bound != null) { // default is to float the panel

            if (bound.equalsIgnoreCase("true")) {
                locateDialog();
            }
        }

        super.setVisibleStandard(vis);
    }

    /**
     * Sets values based on knob along slider.
     * 
     * @param e event that triggered this function
     */
    public void stateChanged(final ChangeEvent e) {
        // System.out.println("dialog chang(ing/ed)");

        // check old values...see if the corners x&y [1] is along the max/min
        // and then adjust sliders to match....
        // else, adjust the histgram to match the sliders... maybe later
        // keep the current histopoints inside the window/level adjustment.

        final Object source = e.getSource();
        if (source == levelSlider || source == windowSlider) {
            calcMinMax();
            

                level = (levelSlider.getValue() * (maxImage - minImage) / levelSliderMax) + minImage;
                window = (windowSlider.getValue() * 2 * (maxImage - minImage) / windowSliderMax);

           for (int count = 0; count < srcImages.length; count++){

	          	image = srcImages[count];
	            if (image.getType() == ModelStorageBase.FLOAT || image.getType() == ModelStorageBase.ARGB_FLOAT) {
	                winValTextField.setText(Float.toString(window));
	                levelValTextField.setText(Float.toString(level));
	            } else {
	                winValTextField.setText(Float.toString(Math.round(window)));
	                levelValTextField.setText(Float.toString(Math.round(level)));
	            }
	
	            calcWinLevTransferFunction(image, window, level, x, y);
	
	            // update the transfer function so the on-screen image
	            // (modelImage/viewJFrameImage) updates for the user
	            LUT[count].getTransferFunction().importArrays(x, y, 4);
	            image.notifyImageDisplayListeners(LUT[count], false);
	
	            // if ((levelSlider.getValueIsAdjusting()) || (windowSlider.getValueIsAdjusting())) {
	            // return;
	            // }
	
	            // if the slider is finally done, update the transfer function
	            // in the histogram.
	            if (image.getHistogramFrame() != null) {
	                updateHistoLUTFrame(LUT[count]);
	            }
            }
        } else if (source == minSlider || source == maxSlider) {
        	// TODO use shared method
        	
            min = minSlider.getValue();
            max = maxSlider.getValue();

            min = (minMaxSlope * min) + minMaxBInt;
            max = (minMaxSlope * max) + minMaxBInt;

            if (keyTyped) {
                min = Float.parseFloat(minValTextField.getText());
                max = Float.parseFloat(maxValTextField.getText());
                keyTyped = false;
            } else {
                if (image.getType() == ModelStorageBase.FLOAT || image.getType() == ModelStorageBase.ARGB_FLOAT) {
                    minValTextField.setText(Float.toString(min));
                    maxValTextField.setText(Float.toString(max));
                } else {
                    min = Math.round(min);
                    max = Math.round(max);
                    minValTextField.setText(Float.toString(min));
                    maxValTextField.setText(Float.toString(max));
                }
            }

            y[1] = 255.0f;
            x[1] = min;

            y[2] = 0;
            x[2] = max;

            for (int count = 0; count < srcImages.length; count++){

	          	image = srcImages[count];
	            LUT[count].getTransferFunction().importArrays(x, y, 4);
	            image.notifyImageDisplayListeners(LUT[count], false);
	            if (image.getHistogramFrame() != null) {
	                updateHistoLUTFrame(LUT[count]);
	            }
            }

        } else if (source == tabbedPane) {
            if (tabbedPane.getSelectedIndex() == 0) {
                calcMinMax();
                
                // TODO use shared method

	                level = (levelSlider.getValue() * (maxImage - minImage) / levelSliderMax) + minImage;
	                window = (windowSlider.getValue() * 2 * (maxImage - minImage) / windowSliderMax);
	                
	            
	            	
	                if (image.getType() == ModelStorageBase.FLOAT || image.getType() == ModelStorageBase.ARGB_FLOAT) {
	                    winValTextField.setText(Float.toString(window));
	                    levelValTextField.setText(Float.toString(level));
	                } else {
	                    winValTextField.setText(Float.toString(Math.round(window)));
	                    levelValTextField.setText(Float.toString(Math.round(level)));
	                }
	                
	            for (int count = 0; count < srcImages.length; count++){

	            	image = srcImages[count];
	                calcWinLevTransferFunction(image, window, level, x, y);
	
	                // update the transfer function so the on-screen image
	                // (modelImage/viewJFrameImage) updates for the user
	                LUT[count].getTransferFunction().importArrays(x, y, 4);
	                image.notifyImageDisplayListeners(LUT[count], false);
	
	                // if ((levelSlider.getValueIsAdjusting()) || (windowSlider.getValueIsAdjusting())) {
	                // return;
	                // }
	
	                // if the slider is finally done, update the transfer function
	                // in the histogram.
	                if (image.getHistogramFrame() != null) {
	                    updateHistoLUTFrame(LUT[count]);
	                }
	                
	            }
            } else if (tabbedPane.getSelectedIndex() == 1) {
            	// TODO use shared method
            	
                /*
                 * min = minSlider.getValue(); max = maxSlider.getValue();
                 * 
                 * min = (minMaxSlope * min) + minMaxBInt; max = (minMaxSlope * max) + minMaxBInt;
                 * 
                 * if(image.getType() == ModelImage.FLOAT || image.getType() == ModelImage.ARGB_FLOAT) {
                 * minValTextField.setText(Float.toString(min)); maxValTextField.setText(Float.toString(max)); }else {
                 * min = Math.round(min); max = Math.round(max); minValTextField.setText(Float.toString(min));
                 * maxValTextField.setText(Float.toString(max)); }
                 */

                min = Float.parseFloat(minValTextField.getText());
                max = Float.parseFloat(maxValTextField.getText());

                y[1] = 255.0f;
                x[1] = min;

                y[2] = 0;
                x[2] = max;
                
                for (int count = 0; count < srcImages.length; count++){
                	
                	image = srcImages[count];
                	LUT[count].getTransferFunction().importArrays(x, y, 4);
	            
	                image.notifyImageDisplayListeners(LUT[count], false);
	                if (image.getHistogramFrame() != null) {
	                    updateHistoLUTFrame(LUT[count]);
	                }
                }
            }
        }
    }

    /**
     * key typed
     */
    public void keyTyped(final KeyEvent event) {
        final Object source = event.getSource();

        if (event.getKeyChar() == KeyEvent.VK_ENTER) {
            if (source == levelValTextField) {
                final String numString = levelValTextField.getText();
                final float num = validateCurrentNum(numString, levelMinFloat, levelMaxFloat);
                if (num != -1) {
                    final float val = ( (num - minImage) * levelSliderMax) / (maxImage - minImage);
                    levelSlider.setValue((int) val);
                } else {
                    level = (levelSlider.getValue() * (maxImage - minImage) / levelSliderMax) + minImage;
                    levelValTextField.setText(Float.toString(Math.round(level)));
                }
            } else if (source == winValTextField) {
                final String numString = winValTextField.getText();
                final float num = validateCurrentNum(numString, winMinFloat, winMaxFloat);
                if (num != -1) {
                    final float val = (num * windowSliderMax) / (2 * (maxImage - minImage));
                    windowSlider.setValue((int) val);
                } else {
                    window = (windowSlider.getValue() * 2 * (maxImage - minImage) / windowSliderMax);
                    winValTextField.setText(Float.toString(Math.round(window)));
                }
            } else if (source == minValTextField) {
                final String numStringMin = minValTextField.getText();
                final String numStringMax = maxValTextField.getText();
                final boolean success = validateMinMaxNums(image, numStringMin, numStringMax);
                if (success == true) {
                    float val = Float.parseFloat(numStringMin);
                    val = (val - minMaxBInt) / minMaxSlope;
                    keyTyped = true;
                    minSlider.setValue((int) val);
                } else {
                    min = minSlider.getValue();

                    min = (minMaxSlope * min) + minMaxBInt;

                    if (image.getType() == ModelStorageBase.FLOAT || image.getType() == ModelStorageBase.ARGB_FLOAT) {
                        minValTextField.setText(Float.toString(min));
                    } else {
                        min = Math.round(min);
                        minValTextField.setText(Float.toString(min));
                    }
                }

            } else if (source == maxValTextField) {
                final String numStringMin = minValTextField.getText();
                final String numStringMax = maxValTextField.getText();
                final boolean success = validateMinMaxNums(image, numStringMin, numStringMax);
                if (success == true) {
                    float val = Float.parseFloat(numStringMax);
                    val = (val - minMaxBInt) / minMaxSlope;
                    keyTyped = true;
                    maxSlider.setValue((int) val);
                } else {

                    max = maxSlider.getValue();

                    max = (minMaxSlope * max) + minMaxBInt;

                    if (image.getType() == ModelStorageBase.FLOAT || image.getType() == ModelStorageBase.ARGB_FLOAT) {

                        maxValTextField.setText(Float.toString(max));
                    } else {

                        max = Math.round(max);

                        maxValTextField.setText(Float.toString(max));
                    }
                }

            }
        }

    }

    /**
     * Update the window, level sliders from CTPreset dialog.
     * 
     * @param min min value
     * @param max max value
     */
    public void updateSliders(final int min, final int max) {
        int windowValue, levelValue;
        float winVal, levelVal;

        windowValue = (max - min);
        levelValue = (max + min) / 2;

        winVal = windowValue * windowSliderMax / (2 * (maxImage - minImage));
        levelVal = (levelValue - minImage) * levelSliderMax / (maxImage - minImage);

        levelSlider.setValue((int) levelVal);
        windowSlider.setValue((int) winVal);
        level = (levelSlider.getValue() * (maxImage - minImage) / levelSliderMax) + minImage;
        window = (windowSlider.getValue() * 2 * (maxImage - minImage) / windowSliderMax);

        winValTextField.setText(Float.toString(Math.round(window)));
        levelValTextField.setText(Float.toString(Math.round(level)));

    }

    /**
     * Builds the level slider and places it in the slider panel.
     * 
     * @param spanel DOCUMENT ME!
     * @param gbc DOCUMENT ME!
     */
    private void buildLevelSlider(final JPanel spanel, final GridBagConstraints gbc) {

        // discovers the slider max and applies it to a
        // label at the top of the slider
        sliderLevMax = new JLabel(Float.toString(maxImage));
        levelMaxFloat = maxImage;
        sliderLevMax.setForeground(Color.black);
        sliderLevMax.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(3, 3, 3, 3);

        final JLabel levelLabel = new JLabel("Level");
        spanel.add(levelLabel, gbc);

        gbc.gridy = 1;

        spanel.add(sliderLevMax, gbc);

        // current setting of the slider (x[1] is the min and x[2] is the max of the image slice.
        level = (x[1] + x[2]) / 2.0f;
        levelSlider = new JSlider(0, levelSliderMax,
                (int) ( (level - minImage) * levelSliderMax / (maxImage - minImage)));
        
//        if (boundsChanged){
//        	levelSlider = new JSlider((int)levelMinFloat, (int)levelMaxFloat, (int)level);
//        } else {
//        	level = (x[1] + x[2]) / 2.0f;
//            levelSlider = new JSlider(0, levelSliderMax,
//                    (int) ( (level - minImage) * levelSliderMax / (maxImage - minImage)));
//            
//        }

        // set slider attributes
        levelSlider.setFont(serif12);
        levelSlider.setEnabled(true);

        if ( (image.getType() == ModelStorageBase.BYTE) || (image.getType() == ModelStorageBase.UBYTE)) {
            levelSlider.setMajorTickSpacing((int) (levelSliderMax * 0.25f));
        } else {
            levelSlider.setMajorTickSpacing((int) (levelSliderMax * 0.25f));
        }

        levelSlider.setPaintTicks(true);
        levelSlider.addChangeListener(this);
        levelSlider.setVisible(true);
        levelSlider.setOrientation(SwingConstants.VERTICAL);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 3;
        gbc.gridheight = 7;
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.fill = GridBagConstraints.VERTICAL;
        spanel.add(levelSlider, gbc);

        // find at apply a label at the bottom
        // of the slider which displays slider minimum
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 0;
        gbc.gridy = 10;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderLevMin = new JLabel(Float.toString(minImage));
        levelMinFloat = minImage;
        sliderLevMin.setForeground(Color.black);
        sliderLevMin.setFont(serif12);
        spanel.add(sliderLevMin, gbc);

        // current value of level label applied to the
        // bottom of the slider
        gbc.gridx = 0;
        gbc.gridy = 11;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        // levelValLabel = new JLabel(Float.toString(level));
        // levelValLabel.setForeground(Color.black);
        // levelValLabel.setFont(serif12B);
        levelValTextField = new JTextField(6);
        levelValTextField.setText(Float.toString(level));
        levelValTextField.addKeyListener(this);
        levelValTextField.addFocusListener(this);
        spanel.add(levelValTextField, gbc);
    }

    /**
     * Builds the min slider and places it in the slider panel.
     * 
     * @param spanel DOCUMENT ME!
     * @param gbc DOCUMENT ME!
     */
    private void buildMinSlider(final JPanel spanel, final GridBagConstraints gbc) {

        // discovers the slider max and applies it to a
        // label at the top of the slider
        sliderMinMax = new JLabel(Float.toString(maxImage));
        // levelMaxFloat = maxImage;
        sliderMinMax.setForeground(Color.black);
        sliderMinMax.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(3, 3, 3, 3);

        final JLabel minLabel = new JLabel("Min");
        spanel.add(minLabel, gbc);

        gbc.gridy = 1;
        spanel.add(sliderMinMax, gbc);

        // current setting of the slider (x[1] is the min and x[2] is the max of the image slice.
        min = .25f * (maxImage - minImage); //TODO: Change this to a useful value based on image statistics
        minSlider = new JSlider(0, 11999, 3000);

        // set slider attributes
        minSlider.setFont(serif12);
        minSlider.setEnabled(true);

        if ( (image.getType() == ModelStorageBase.BYTE) || (image.getType() == ModelStorageBase.UBYTE)) {
            minSlider.setMajorTickSpacing(3000);
        } else {
            minSlider.setMajorTickSpacing(3000);
        }

        minSlider.setPaintTicks(true);
        minSlider.addChangeListener(this);
        minSlider.setVisible(true);
        minSlider.setOrientation(SwingConstants.VERTICAL);
        minSlider.addMouseListener(this);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 3;
        gbc.gridheight = 7;
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.fill = GridBagConstraints.VERTICAL;
        spanel.add(minSlider, gbc);

        // find at apply a label at the bottom
        // of the slider which displays slider minimum
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 0;
        gbc.gridy = 10;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderMinMin = new JLabel(Float.toString(minImage));
        // levelMinFloat = minImage;
        sliderMinMin.setForeground(Color.black);
        sliderMinMin.setFont(serif12);
        spanel.add(sliderMinMin, gbc);

        // current value of level label applied to the
        // bottom of the slider
        gbc.gridx = 0;
        gbc.gridy = 11;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        // levelValLabel = new JLabel(Float.toString(level));
        // levelValLabel.setForeground(Color.black);
        // levelValLabel.setFont(serif12B);
        minValTextField = new JTextField(6);
        minValTextField.setText(Float.toString(min));
        minValTextField.addKeyListener(this);
        minValTextField.addFocusListener(this);
        spanel.add(minValTextField, gbc);

        minMaxBInt = minImage;
        minMaxSlope = calcMinMaxSlope(image);

    }

    private void buildMaxSlider(final JPanel spanel, final GridBagConstraints gbc) {

        // discovers the slider max and applies it to a
        // label at the top of the slider
        sliderMaxMax = new JLabel(Float.toString(maxImage));
        // levelMaxFloat = maxImage;
        sliderMaxMax.setForeground(Color.black);
        sliderMaxMax.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(3, 3, 3, 3);

        final JLabel maxLabel = new JLabel("Max");
        spanel.add(maxLabel, gbc);

        gbc.gridy = 1;

        spanel.add(sliderMaxMax, gbc);

        // current setting of the slider (x[1] is the min and x[2] is the max of the image slice.
        max = .75f * (maxImage - minImage); //TODO: Change this to a useful value based on image statistics
        maxSlider = new JSlider(0, 11999, 9000);

        // set slider attributes
        maxSlider.setFont(serif12);
        maxSlider.setEnabled(true);

        if ( (image.getType() == ModelStorageBase.BYTE) || (image.getType() == ModelStorageBase.UBYTE)) {
            maxSlider.setMajorTickSpacing(3000);
        } else {
            maxSlider.setMajorTickSpacing(3000);
        }

        maxSlider.setPaintTicks(true);
        maxSlider.addChangeListener(this);
        maxSlider.setVisible(true);
        maxSlider.setOrientation(SwingConstants.VERTICAL);
        maxSlider.addMouseListener(this);

        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.gridwidth = 3;
        gbc.gridheight = 7;
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.fill = GridBagConstraints.VERTICAL;
        spanel.add(maxSlider, gbc);

        // find at apply a label at the bottom
        // of the slider which displays slider minimum
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 1;
        gbc.gridy = 10;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderMaxMin = new JLabel(Float.toString(minImage));
        // levelMinFloat = minImage;
        sliderMaxMin.setForeground(Color.black);
        sliderMaxMin.setFont(serif12);
        spanel.add(sliderMaxMin, gbc);

        // current value of level label applied to the
        // bottom of the slider
        gbc.gridx = 1;
        gbc.gridy = 11;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        // levelValLabel = new JLabel(Float.toString(level));
        // levelValLabel.setForeground(Color.black);
        // levelValLabel.setFont(serif12B);
        maxValTextField = new JTextField(6);
        maxValTextField.setText(Float.toString(max));
        maxValTextField.addKeyListener(this);
        maxValTextField.addFocusListener(this);
        spanel.add(maxValTextField, gbc);
    }

    /**
     * Builds the slider Panel.
     * 
     * @param borderTitle the title of the border.
     * 
     * @return DOCUMENT ME!
     */
    private JPanel buildWindowLevelPanel() {
        final JPanel spanel = new JPanel();

        // getContentPane().add(spanel, BorderLayout.CENTER);
        spanel.setLayout(new GridBagLayout());

        spanel.setForeground(Color.black);
        // spanel.setBorder(buildTitledBorder(borderTitle));

        return spanel;
    }

    private JPanel buildMinMaxPanel() {
        final JPanel spanel = new JPanel();

        // getContentPane().add(spanel, BorderLayout.CENTER);
        // spanel.setVisible(false);
        spanel.setLayout(new GridBagLayout());

        spanel.setForeground(Color.black);
        // spanel.setBorder(buildTitledBorder(borderTitle));

        return spanel;
    }

    /**
     * Builds the window slider and places it in the slider panel.
     * 
     * @param spanel DOCUMENT ME!
     * @param gbc DOCUMENT ME!
     */
    private void buildWindowSlider(final JPanel spanel, final GridBagConstraints gbc) {

        // discovers the slider max and applies it to a
        // label at the top of the slider
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderWinMax = new JLabel(Float.toString(2.0f * (maxImage - minImage)));
        winMaxFloat = 2.0f * (maxImage - minImage);
        sliderWinMax.setForeground(Color.black);
        sliderWinMax.setFont(serif12);

        final JLabel windowLabel = new JLabel("Window");
        spanel.add(windowLabel, gbc);

        gbc.gridy = 1;

        spanel.add(sliderWinMax, gbc);

        // current setting of the slider
        window = x[2] - x[1]; // the width of the window x[2] (max) - x[1] (min)
        windowSlider = new JSlider(0, windowSliderMax,
                (int) (window * windowSliderMax / (2.0f * (maxImage - minImage))));

        // set slider attributes
        windowSlider.setFont(serif12);
        windowSlider.setEnabled(true);

        if ( (image.getType() == ModelStorageBase.BYTE) || (image.getType() == ModelStorageBase.UBYTE)) {
            windowSlider.setMajorTickSpacing((int) (windowSliderMax * 0.25f));
        } else {
            windowSlider.setMajorTickSpacing((int) (windowSliderMax * 0.25f));
        }

        windowSlider.setPaintTicks(true);
        windowSlider.addChangeListener(this);
        windowSlider.setVisible(true);
        windowSlider.setOrientation(SwingConstants.VERTICAL);

        // slider placement
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.gridwidth = 3;
        gbc.gridheight = 7;
        gbc.fill = GridBagConstraints.VERTICAL;
        spanel.add(windowSlider, gbc);

        // find at apply a label at the bottom
        // of the slider which displays slider minimum
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridx = 1;
        gbc.gridy = 10;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderWinMin = new JLabel("0.0");
        winMinFloat = 0.0f;
        sliderWinMin.setForeground(Color.black);
        sliderWinMin.setFont(serif12);
        spanel.add(sliderWinMin, gbc);

        // current value of window
        gbc.gridx = 1;
        gbc.gridy = 11;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        // winValLabel = new JLabel(Float.toString(window));
        // winValLabel.setForeground(Color.black);
        // winValLabel.setFont(serif12B);
        winValTextField = new JTextField(6);
        winValTextField.setText(Float.toString(window));
        winValTextField.addKeyListener(this);
        winValTextField.addFocusListener(this);
        spanel.add(winValTextField, gbc);
    }

    /**
     * validate current number
     * 
     * @param numString
     * @param min
     * @param max
     * @return
     */
    public static final float validateCurrentNum(final String numString, final float min, final float max) {
        float num;

        try {
            num = Float.parseFloat(numString);
        } catch (final NumberFormatException e) {
            return -1;
        }
        if (num >= min && num <= max) {
            return num;
        } else {
            return -1;
        }
    }

    public static final boolean validateMinMaxNums(final ModelImage img, final String minString, final String maxString) {
        float numMin;
        float numMax;
        
        float[] bounds = calcMinMax(img);

        try {
            numMin = Float.parseFloat(minString);
            numMax = Float.parseFloat(maxString);
        } catch (final NumberFormatException e) {
            return false;
        }

        if (numMin < bounds[0] || numMin > bounds[1] || numMax < bounds[0] || numMax > bounds[1] || numMin > numMax) {
            return false;
        } else {
            return true;
        }

    }

    /**
     * Displays histoLUT frame for a gray scale image.
     */
    private void updateHistoLUTFrame(ModelLUT lut) {
        image.notifyImageDisplayListeners(lut, false);
        image.getHistogramFrame().redrawFrames();
    }

    public void keyPressed(final KeyEvent arg0) {}

    public void keyReleased(final KeyEvent arg0) {}

    public void mouseClicked(final MouseEvent e) {}

    public void mouseEntered(final MouseEvent e) {}

    public void mouseExited(final MouseEvent e) {}

    public void mousePressed(final MouseEvent e) {}

    public void mouseReleased(final MouseEvent e) {
        final Object source = e.getSource();

        if (source == maxSlider) {
            if (maxSlider.getValue() <= minSlider.getValue()) {
                maxSlider.setValue(minSlider.getValue() + 1);
            }
        } else if (source == minSlider) {
            if (minSlider.getValue() >= maxSlider.getValue()) {
                minSlider.setValue(maxSlider.getValue() - 1);
            }
        }

    }

    public void notifyImageDisplayListeners(ModelLUT lut) {
        image.notifyImageDisplayListeners(lut, false);
    }
    
    /**
     * Calculate the x and y components of the transfer function, given the active image and the desired window and level.
     * @param img The active image.
     * @param win The desired window.
     * @param lev The desired level.
     * @param tfx The array in which the x components of the transfer function will be placed.  Should already be allocated and of size 4.
     * @param tfy The array in which the y components of the transfer function will be placed.  Should already be allocated and of size 4.
     */
    public static final void calcWinLevTransferFunction(ModelImage img, float win, float lev, float[] tfx, float tfy []) {
    	if (tfx == null || tfx.length != 4) {
    		throw new IllegalArgumentException("Transfer function x component not set up correctly.");
    	}
    	if (tfy == null || tfy.length != 4) {
    		throw new IllegalArgumentException("Transfer function y component not set up correctly.");
    	}
    	
    	float[] bounds = calcMinMax(img);
    	
    	// first point always at lower left
        tfx[0] = bounds[0];
        tfy[0] = 255;
        
        if (win == 0) {
    		win = 1;
    	}
    	
    	tfx[2] = lev + (win / 2);

        if (tfx[2] > bounds[1]) {
            tfy[2] = 255.0f * (tfx[2] - bounds[1]) / win;
            tfx[2] = bounds[1];
        } else {
            tfy[2] = 0.0f;
        }

        tfx[1] = lev - (win / 2);

        if (tfx[1] < bounds[0]) {
            tfy[1] = 255.0f - (255.0f * (bounds[0] - tfx[1]) / win);
            tfx[1] = bounds[0];
        } else {
            tfy[1] = 255.0f;
        }
        
        // last point always at upper right of histogram
        tfx[3] = bounds[1];
        tfy[3] = 0;
    }
    
    /**
     * Calculate the image range bounds for transfer function determination.
     * @param img The active image.
     * @return An array containing the minimum value of the range in the 0th index and the maximum in the 1st index.
     */
    public static final float[] calcMinMax(ModelImage img) {
    	float[] bounds = new float[2];
    	
    	if (img.getType() == ModelStorageBase.UBYTE) {
    		bounds[0] = 0;
    		bounds[1] = 255;
        } else if (img.getType() == ModelStorageBase.BYTE) {
        	bounds[0] = -128;
        	bounds[1] = 127;
        } else {
        	bounds[0] = (float) img.getMin();
        	bounds[1] = (float) img.getMax();
        }
    	
    	return bounds;
    }
    
    private static final int calcLevelSliderMax(ModelImage img) {
    	if (img.getType() == ModelStorageBase.UBYTE) {
            return 255;
        } else if (img.getType() == ModelStorageBase.BYTE) {
            return 255;
        } else {
            return 1999;
        }
    }
    
    private static final int calcWindowSliderMax(ModelImage img) {
    	if (img.getType() == ModelStorageBase.UBYTE) {
            return 511;
        } else if (img.getType() == ModelStorageBase.BYTE) {
            return 511;
        } else {
            return 3999;
        }
    }
    
    private static final float calcMinMaxSlope(ModelImage img) {
    	float[] bounds = calcMinMax(img);
    	return (bounds[1] - bounds[0]) / 11999;
    }
    
    /**
     * Calculate the maximum and minimum valuse to setup the window and level sliders.
     */
    private void calcMinMax() {
    	float[] bounds = calcMinMax(image);
    	minImage = bounds[0];
    	maxImage = bounds[1];
    	levelSliderMax = calcLevelSliderMax(image);
    	windowSliderMax = calcWindowSliderMax(image);
    }

    
    
	@Override
	protected void setGUIFromParams() {
		setInputDir(scriptParameters.getParams().getString("input_dir"));
		
	}

	@Override
	protected void storeParamsFromGUI() throws ParserException {
		scriptParameters.getParams().put(ParameterFactory.newParameter("input_dir", inputDir));
		
	}

	@Override
	public Dimension getPanelSize() {
		// TODO Auto-generated method stub
		return new Dimension(previewPanel.getBounds().width,
				previewPanel.getBounds().height);
	}

}
