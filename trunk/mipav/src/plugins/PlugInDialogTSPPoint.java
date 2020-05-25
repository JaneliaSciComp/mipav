import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;
import gov.nih.mipav.util.ThreadUtil;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;
import java.io.File;

import javax.swing.*;


/**
 
 */
public class PlugInDialogTSPPoint extends JDialogStandaloneScriptablePlugin implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------
	
	private PlugInAlgorithmTSPPoint TSPPointAlgo = null;
	
	private ViewUserInterface UI;
	
	private JTextField pwiImageFileDirectoryText;

	private String pwiImageFileDirectory;
	
	private JTextField baseText;
	
	private JButton maskButton;
	
	private String maskFileDir;

    private String maskFileName;

    private ModelImage maskImage = null;
	
	private JTextField maskText;
	
	private JTextField xText;
	
	private int xLoc;
	
	private JTextField yText;
	
	private int yLoc;
	
	private JTextField zText;
	
	private int zLoc;
	
	private JCheckBox spatialSmoothingCheckBox;
	
	private boolean spatialSmoothing = false;
	
	private JLabel sigmaXLabel;
	
	private JTextField sigmaXText;
	
	private float sigmax = 5.0f;
	
    private JLabel sigmaYLabel;
	
	private JTextField sigmaYText;
	
	private float sigmay = 5.0f;
	
	private JTextField TSP_thresholdText;
	
	private double TSP_threshold = 0.8;
	
    private JTextField TSP_iterText;
    
    private int TSP_iter = 4;
	
	private JCheckBox multiThreadingEnabledCheckBox;
	
	private boolean multiThreading;
	
	private JCheckBox correlationCheckBox;
	
	private boolean calculateCorrelation = true;
	
	private String fileNameBase = "IM";
	
	/**
     * Constructor used for instantiation during script execution (required for dynamic loading).
     */
    public PlugInDialogTSPPoint() {}

    /**
     * 
     */
    public PlugInDialogTSPPoint(final boolean modal) {
        super(modal);

        init();
    }
    
 // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Event Processing ****************************
    // ************************************************************************

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     * 
     * @param event Event that triggers function.
     */
    @Override
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {
            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (source == spatialSmoothingCheckBox) {
        	sigmaXLabel.setEnabled(spatialSmoothingCheckBox.isSelected());
        	sigmaXText.setEnabled(spatialSmoothingCheckBox.isSelected());
        	sigmaYLabel.setEnabled(spatialSmoothingCheckBox.isSelected());
        	sigmaYText.setEnabled(spatialSmoothingCheckBox.isSelected());
        } else if (command.equals("Mask")) {

            try {
                JFileChooser chooser = new JFileChooser();

                UI = ViewUserInterface.getReference();

                if (UI.getDefaultDirectory() != null) {
                    File file = new File(UI.getDefaultDirectory());

                    if (file != null) {
                        chooser.setCurrentDirectory(file);
                    } else {
                        chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                    }
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
                }

                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.GEN));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MICROSCOPY));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MISC));

                chooser.setDialogTitle("Open mask file");
                maskFileDir = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;

                int returnValue = chooser.showOpenDialog(UI.getMainFrame());

                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    maskFileName = chooser.getSelectedFile().getName();
                    maskFileDir = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                    UI.setDefaultDirectory(maskFileDir);
                } else {
                    maskFileName = null;
                }

                if (maskFileName != null) {
                    maskText.setText(maskFileName);
                } else {
                    maskText.setText("");
                }
                if (maskFileName == null) {
                    System.err.println("maskFileName is null");

                    return;
                }
                
                if (maskFileDir == null) {
                	System.err.println("maskFileDir is null");
                }

                FileIO fileIO = new FileIO();

                maskImage = fileIO.readImage(maskFileName, maskFileDir, false, null);
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in PlugInDialogTSPPoint.");

                return;
            }
        } else {
            super.actionPerformed(event);
        }
        // System.out.print(this.getSize());
    } // end actionPerformed()
	
	/**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("TSP Single Point Analysis");
        
        final GuiBuilder gui = new GuiBuilder(this);

        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        JPanel inputPanel = new JPanel(new GridBagLayout());
        inputPanel.setForeground(Color.black);
        inputPanel.setBorder(buildTitledBorder("Input parameters"));
        
        pwiImageFileDirectoryText = gui.buildFileField("Directory containing pwi image file: ", "", false, JFileChooser.DIRECTORIES_ONLY);
        inputPanel.add(pwiImageFileDirectoryText.getParent(), gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 1;
        JLabel baseLabel = new JLabel("Base name in selected files");
        baseLabel.setFont(serif12);
        baseLabel.setForeground(Color.black);
        inputPanel.add(baseLabel, gbc);
        
        gbc.gridx = 1;
        baseText = new JTextField(10);
        baseText.setText("IM");
        baseText.setFont(serif12);
        baseText.setForeground(Color.black);
        inputPanel.add(baseText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 2;
        maskButton = new JButton("Choose mask file");
        maskButton.setForeground(Color.black);
        maskButton.setFont(serif12);
        maskButton.addActionListener(this);
        maskButton.setActionCommand("Mask");
        gbc.gridwidth = 1;
        inputPanel.add(maskButton, gbc);

        gbc.gridx = 1;
        maskText = new JTextField();
        maskText.setFont(serif12);
        inputPanel.add(maskText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 3;
        JLabel xLabel = new JLabel("Voxel x location");
        xLabel.setFont(serif12);
        xLabel.setForeground(Color.black);
        inputPanel.add(xLabel, gbc);
        
        gbc.gridx = 1;
        xText = new JTextField(10);
        xText.setText("");
        xText.setFont(serif12);
        xText.setForeground(Color.black);
        inputPanel.add(xText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 4;
        JLabel yLabel = new JLabel("Voxel y location");
        yLabel.setFont(serif12);
        yLabel.setForeground(Color.black);
        inputPanel.add(yLabel, gbc);
        
        gbc.gridx = 1;
        yText = new JTextField(10);
        yText.setText("");
        yText.setFont(serif12);
        yText.setForeground(Color.black);
        inputPanel.add(yText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 5;
        JLabel zLabel = new JLabel("Voxel z location");
        zLabel.setFont(serif12);
        zLabel.setForeground(Color.black);
        inputPanel.add(zLabel, gbc);
        
        gbc.gridx = 1;
        zText = new JTextField(10);
        zText.setText("");
        zText.setFont(serif12);
        zText.setForeground(Color.black);
        inputPanel.add(zText, gbc);

        gbc.gridx = 0;
        gbc.gridy = 6;
        spatialSmoothingCheckBox = new JCheckBox("Perform spatial smoothing");
        spatialSmoothingCheckBox.setSelected(false);
        spatialSmoothingCheckBox.setFont(MipavUtil.font12);
        spatialSmoothingCheckBox.setForeground(Color.black);
        spatialSmoothingCheckBox.addActionListener(this);
        inputPanel.add(spatialSmoothingCheckBox, gbc);
        
        gbc.gridy = 7;
        sigmaXLabel = new JLabel("Gaussian blur sigma X in millimeters");
        sigmaXLabel.setFont(serif12);
        sigmaXLabel.setForeground(Color.black);
        sigmaXLabel.setEnabled(false);
        inputPanel.add(sigmaXLabel, gbc);
        
        gbc.gridx = 1;
        sigmaXText = new JTextField(10);
        sigmaXText.setText("5.0");
        sigmaXText.setFont(serif12);
        sigmaXText.setForeground(Color.black);
        sigmaXText.setEnabled(false);
        inputPanel.add(sigmaXText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 8;
        sigmaYLabel = new JLabel("Gaussian blur sigma Y in millimeters");
        sigmaYLabel.setFont(serif12);
        sigmaYLabel.setForeground(Color.black);
        sigmaYLabel.setEnabled(false);
        inputPanel.add(sigmaYLabel, gbc);
        
        gbc.gridx = 1;
        sigmaYText = new JTextField(10);
        sigmaYText.setText("5.0");
        sigmaYText.setFont(serif12);
        sigmaYText.setForeground(Color.black);
        sigmaYText.setEnabled(false);
        inputPanel.add(sigmaYText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 9;
        JLabel TSP_thresholdLabel = new JLabel("TSP threshold");
        TSP_thresholdLabel.setFont(serif12);
        TSP_thresholdLabel.setForeground(Color.black);
        inputPanel.add(TSP_thresholdLabel, gbc);
        
        gbc.gridx = 1;
        TSP_thresholdText = new JTextField(10);
        TSP_thresholdText.setText("0.8");
        TSP_thresholdText.setFont(serif12);
        TSP_thresholdText.setForeground(Color.black);
        inputPanel.add(TSP_thresholdText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 10;
        JLabel TSP_iterLabel = new JLabel("TSP iterations");
        TSP_iterLabel.setFont(serif12);
        TSP_iterLabel.setForeground(Color.black);
        inputPanel.add(TSP_iterLabel, gbc);
        
        gbc.gridx = 1;
        TSP_iterText = new JTextField(10);
        TSP_iterText.setText("4");
        TSP_iterText.setFont(serif12);
        TSP_iterText.setForeground(Color.black);
        inputPanel.add(TSP_iterText, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 11;
        if (ThreadUtil.getAvailableCores() > 1) {
            multiThreadingEnabledCheckBox = new JCheckBox("Multi-threading enabled (" + ThreadUtil.getAvailableCores() + " cores)");
        }
        else {
        	multiThreadingEnabledCheckBox = new JCheckBox("Multi-threading disabled (" + ThreadUtil.getAvailableCores() + " core)");
        }
        multiThreadingEnabledCheckBox.setFont(MipavUtil.font12);
        multiThreadingEnabledCheckBox.setForeground(Color.black);
        multiThreadingEnabledCheckBox.setSelected(ThreadUtil.getAvailableCores() > 1);
        multiThreadingEnabledCheckBox.setEnabled(ThreadUtil.getAvailableCores() > 1);
        inputPanel.add(multiThreadingEnabledCheckBox, gbc);
        
        gbc.gridy = 13;
        correlationCheckBox = new JCheckBox("Correlation maps calculated", true);
        correlationCheckBox.setFont(MipavUtil.font12);
        correlationCheckBox.setForeground(Color.black);
        inputPanel.add(correlationCheckBox, gbc);
        
        // Build the Panel that holds the OK and CANCEL Buttons
        JPanel OKCancelPanel = new JPanel();

        // size and place the OK button
        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        // size and place the CANCEL button
        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());
        mainPanel.add(inputPanel, BorderLayout.NORTH);
        mainPanel.add(OKCancelPanel, BorderLayout.SOUTH);
        
        JScrollPane scrollPane = new JScrollPane(mainPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        
        getContentPane().add(scrollPane);

        pack();
        setVisible(true);
        setResizable(true);
        System.gc();
    }

	/**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     * 
     * @param algorithm Algorithm that caused the event.
     */
    @Override
    public void algorithmPerformed(final AlgorithmBase algorithm) {
        if (algorithm instanceof PlugInAlgorithmTSPPoint) {
            Preferences.debug("Elapsed: " + algorithm.getElapsedTime());
            //System.out.println("Elapsed: " + algorithm.getElapsedTime());

            if ( (TSPPointAlgo.isCompleted() == true)) {
                insertScriptLine();
            }

            if (TSPPointAlgo != null) {
                TSPPointAlgo.finalize();
                TSPPointAlgo = null;
            }

            // dispose();
            if (super.isExitRequired()) {
                ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
            }
        }
    } // end algorithmPerformed()
    
    /**
     * Once all the necessary variables are set, call the kidney segmentation algorithm
     */
    @Override
    protected void callAlgorithm() {

        try {

            TSPPointAlgo = new PlugInAlgorithmTSPPoint(pwiImageFileDirectory, maskImage, 
            		xLoc, yLoc, zLoc, spatialSmoothing, sigmax, sigmay,
            		TSP_threshold, TSP_iter, multiThreading, calculateCorrelation,
            		fileNameBase);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            TSPPointAlgo.addListener(this);
            createProgressBar("Creating plugin", " ...", TSPPointAlgo);
            progressBar.setVisible(true);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (TSPPointAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                TSPPointAlgo.run();
            }
        } catch (final OutOfMemoryError x) {
            /*if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }*/

            MipavUtil.displayError("PlugInAlgorithmTSPPoint: unable to allocate enough memory");

            return;
        }

    } // end callAlgorithm()
    
    /**
     * Used in turning your plugin into a script
     */
    @Override
    protected void setGUIFromParams() {
    	pwiImageFileDirectory = scriptParameters.getParams().getString("pwiImageFileDirectory");
    	fileNameBase = scriptParameters.getParams().getString("file_name_base");
    	maskFileName = scriptParameters.getParams().getString("mask_file_name");
    	maskFileDir = scriptParameters.getParams().getString("mask_file_dir");
    	if (maskFileName == null) {
            System.err.println("maskFileName is null");

            return;
        }
        
        if (maskFileDir == null) {
        	System.err.println("maskFileDir is null");
        }

        FileIO fileIO = new FileIO();

        maskImage = fileIO.readImage(maskFileName, maskFileDir, false, null);
        xLoc = scriptParameters.getParams().getInt("x_loc");
        yLoc = scriptParameters.getParams().getInt("y_loc");
        zLoc = scriptParameters.getParams().getInt("z_loc");
    	spatialSmoothing = scriptParameters.getParams().getBoolean("spatial_smoothing");
    	sigmax = scriptParameters.getParams().getFloat("x_sigma");
    	sigmay = scriptParameters.getParams().getFloat("y_sigma");
    	TSP_threshold = scriptParameters.getParams().getDouble("TSP_thresh");
    	TSP_iter = scriptParameters.getParams().getInt("iter");
    	multiThreading = scriptParameters.getParams().getBoolean("multi_thread");
    	calculateCorrelation = scriptParameters.getParams().getBoolean("calc_correlation");
    }
    
    /**
     * Used in turning your plugin into a script
     */
    @Override
    protected void storeParamsFromGUI() throws ParserException {
    	scriptParameters.getParams().put(ParameterFactory.newParameter("pwiImageFileDirectory", pwiImageFileDirectory));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("file_name_base", fileNameBase));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("mask_file_name", maskFileName));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("mask_file_dir", maskFileDir));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("x_loc", xLoc));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("y_loc", yLoc));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("z_loc", zLoc));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("x_sigma", sigmax));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("y_sigma", sigmay));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("TSP_thresh", TSP_threshold));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("iter", TSP_iter));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("multi_thread", multiThreading));
    	scriptParameters.getParams().put(ParameterFactory.newParameter("calc_correlation", calculateCorrelation));
    }
    
    private boolean setVariables() {
    	String tmpStr;
    	pwiImageFileDirectory = pwiImageFileDirectoryText.getText();
    	
    	fileNameBase = baseText.getText().trim();
    	if (fileNameBase == null) {
    		MipavUtil.displayError("The file name base is empty");
    		baseText.requestFocus();
    		baseText.selectAll();
    		return false;
    	}
    	
    	if (maskImage == null) {
    		MipavUtil.displayError("No mask Image has been selected");
    		return false;
    	}
    	
    	tmpStr = xText.getText();
    	try {
    		xLoc = Integer.valueOf(tmpStr).intValue();
    	}
    	catch (NumberFormatException e) {
    	    MipavUtil.displayError("Voxel x location does not have a proper integer");
    	    xText.requestFocus();
    	    xText.selectAll();
    	    return false;
    	}
    	if (xLoc < 0) {
    		MipavUtil.displayError("Voxel x location must be at least 0");
    		xText.requestFocus();
    	    xText.selectAll();
    	    return false;
    	}
    	
    	tmpStr = yText.getText();
    	try {
    		yLoc = Integer.valueOf(tmpStr).intValue();
    	}
    	catch (NumberFormatException e) {
    	    MipavUtil.displayError("Voxel y location does not have a proper integer");
    	    yText.requestFocus();
    	    yText.selectAll();
    	    return false;
    	}
    	if (yLoc < 0) {
    		MipavUtil.displayError("Voxel y location must be at least 0");
    		yText.requestFocus();
    	    yText.selectAll();
    	    return false;
    	}
    	
    	tmpStr = zText.getText();
    	try {
    		zLoc = Integer.valueOf(tmpStr).intValue();
    	}
    	catch (NumberFormatException e) {
    	    MipavUtil.displayError("Voxel z location does not have a proper integer");
    	    zText.requestFocus();
    	    zText.selectAll();
    	    return false;
    	}
    	if (zLoc < 0) {
    		MipavUtil.displayError("Voxel z location must be at least 0");
    		zText.requestFocus();
    	    zText.selectAll();
    	    return false;
    	}
    	
    	spatialSmoothing = spatialSmoothingCheckBox.isSelected();
    	if (spatialSmoothing) {
	    	tmpStr = sigmaXText.getText();
	    	try {
	    		sigmax = Float.valueOf(tmpStr).floatValue();
	    	}
	    	catch (NumberFormatException e) {
	    	    MipavUtil.displayError("sigmax text does not have a proper float");
	    	    sigmaXText.requestFocus();
	    	    sigmaXText.selectAll();
	    	    return false;
	    	}
	    	if (sigmax < 0) {
	    		MipavUtil.displayError("sigmax must be at least 0");
	    		sigmaXText.requestFocus();
	    	    sigmaXText.selectAll();
	    	    return false;
	    	}
	    	
	    	tmpStr = sigmaYText.getText();
	    	try {
	    		sigmay = Float.valueOf(tmpStr).floatValue();
	    	}
	    	catch (NumberFormatException e) {
	    	    MipavUtil.displayError("sigmay text does not have a proper float");
	    	    sigmaYText.requestFocus();
	    	    sigmaYText.selectAll();
	    	    return false;
	    	}
	    	if (sigmay < 0) {
	    		MipavUtil.displayError("sigmay must be at least 0");
	    		sigmaYText.requestFocus();
	    	    sigmaYText.selectAll();
	    	    return false;
	    	}

    	} // if (spatialSmoothing)
    	
    	
    	
    	tmpStr = TSP_thresholdText.getText();
    	try {
    		TSP_threshold = Double.valueOf(tmpStr).doubleValue();
    	}
    	catch (NumberFormatException e) {
    	    MipavUtil.displayError("masking_threshold text does not have a proper double number");
    	    TSP_thresholdText.requestFocus();
    	    TSP_thresholdText.selectAll();
    	    return false;
    	}
    	if (TSP_threshold < 0) {
    		MipavUtil.displayError("TSP_threshold must be at least 0");
    		TSP_thresholdText.requestFocus();
    	    TSP_thresholdText.selectAll();
    	    return false;
    	}
    	
    	tmpStr = TSP_iterText.getText();
    	try {
    		TSP_iter = Integer.valueOf(tmpStr).intValue();
    	}
    	catch (NumberFormatException e) {
    	    MipavUtil.displayError("TSP_iter text does not have a proper integer");
    	    TSP_iterText.requestFocus();
    	    TSP_iterText.selectAll();
    	    return false;
    	}
    	if (TSP_iter < 1) {
    		MipavUtil.displayError("TSP_iter must be at least 1");
    		TSP_iterText.requestFocus();
    	    TSP_iterText.selectAll();
    	    return false;
    	}
    	if (TSP_iter > 100) {
    		MipavUtil.displayError("TSP_iter cannot exceed 100");
    		TSP_iterText.requestFocus();
    	    TSP_iterText.selectAll();
    	    return false;
    	}
    	
    	
    	multiThreading = multiThreadingEnabledCheckBox.isSelected();
    	
    	calculateCorrelation = correlationCheckBox.isSelected();
    	
    	return true;
    }

}
