package gov.nih.mipav.view.dialogs;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.IOException;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call algorithmRegLeastSquares. Selects image is match image, the image that gets transformed
 * until it is registered to the base image. Algorithms are executed in their own thread.
 *
 * @version  0.1 Augusut 15, 2020
 * @author   William Gandler
 */
public class JDialogRegistrationSIFT3D extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUIDL;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Base image - register match image to base image. */
    protected ModelImage baseImage;

    /** Combo box with image names for choosing base image. */
    private JComboBox comboBoxImage;
    
    /** Algorithm to run from this dialog. */
    private SIFT3D reg3D = null;

    /** Match image - register match image to base image. */
    protected ModelImage matchImage;

    /** Result image - image returned from registration algorithm. */
    protected ModelImage resultImage = null;
    
    /** Reference to userface. */
    private ViewUserInterface userInterface;
    
    private double SIFT3D_nn_thresh_default = 0.8; // Default matching threshold
	private double SIFT3D_err_thresh_default = 5.0;
	private int SIFT3D_num_iter_default = 500;
	private boolean useOCL = false;
	private double SIFT3D_GAUSS_WIDTH_FCTR = 3.0;
	// Set SIFT3D_MATCH_MAX_DIST <= 0.0 to avoid using in int match_desc()
	private double SIFT3D_MATCH_MAX_DIST = 0.0;
	//private double SIFT3D_MATCH_MAX_DIST = 0.3; // Maximum distance between matching features
	private boolean ICOS_HIST = true;  // Icosahedral gradient histogram
	private boolean SIFT3D_RANSAC_REFINE = true;	// Use least-squares refinement in RANSAC
	
	private JLabel labelMatchingThreshold;
	
	private JTextField textMatchingThreshold;
	
	private JLabel labelErrorThreshold;
	
	private JTextField textErrorThreshold;
	
	private JLabel labelNumberIterations;
	
	private JTextField textNumberIterations;
	
	private JLabel labelGaussianWidth;
	
	private JTextField textGaussianWidth;
	
	private JCheckBox matchCheckBox;
	
	private JLabel labelMatch;
	
	private JTextField textMatch;
	
	private JCheckBox icosahedralCheckBox;
	
	private JCheckBox refineCheckBox;
    
    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogRegistrationSIFT3D() { }

    /**
     * Creates new registration dialog to get base image name.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogRegistrationSIFT3D(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        matchImage = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    /**
     * Creates a new JDialogRegistrationSIFT3D object.
     *
     * @param  theParentFrame  DOCUMENT ME!
     * @param  _mi             DOCUMENT ME!
     * @param  _ri             DOCUMENT ME!
     */
    public JDialogRegistrationSIFT3D(Frame theParentFrame, ModelImage _mi, ModelImage _ri) {
        matchImage = _mi;
        baseImage = _ri;
        userInterface = ViewUserInterface.getReference();
        setSeparateThread(false);
        callAlgorithm();
    }
    
  //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, set variables, and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (source == matchCheckBox) {
            labelMatch.setEnabled(matchCheckBox.isSelected());
            textMatch.setEnabled(matchCheckBox.isSelected());
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
            //MipavUtil.showWebHelp("");
        } else {
            super.actionPerformed(event);
        }
    }
    
    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to complete.
     * 
     * @param algorithm Algorithm that caused the event.
     */
    public void algorithmPerformed(final AlgorithmBase algorithm) {
    	if (algorithm instanceof SIFT3D) {
    		if (reg3D.isCompleted()) {	
    			resultImage = reg3D.getResultImage();
    			if (resultImage != null) {

                    try {
                        new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                    } catch (final OutOfMemoryError error) {
                        MipavUtil.displayError("Out of memory: unable to open new frame");
                    }
                } else {
                    MipavUtil.displayError("Result Image is null");
                }
    		}
    		if (reg3D != null) {
                reg3D = null;
            }

            matchImage = null; // register match image to reference Image
            baseImage = null;

            dispose();
            System.gc();
    	}
    }
    
    /**
     * Sets arrays appropriately and calls registration algorithm, running it in it's own thread.
     */
    protected void callAlgorithm() {
    	reg3D = new SIFT3D(baseImage, matchImage, SIFT3D_nn_thresh_default, SIFT3D_err_thresh_default,
    			SIFT3D_num_iter_default, useOCL, SIFT3D_GAUSS_WIDTH_FCTR,
        		SIFT3D_MATCH_MAX_DIST, ICOS_HIST, SIFT3D_RANSAC_REFINE);
    	
    	reg3D.addListener(this);
    	
        createProgressBar(matchImage.getImageName(), reg3D);

        // Hide dialog
        setVisible(false);

        if (isRunInSeparateThread()) {

            // Start the thread as a low priority because we wish to still have user interface work fast.
            if (reg3D.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            reg3D.run();
        }
    }
    
    /**
     * Initializes the GUI components and displays the dialog.
     */
    private void init() {
    	setForeground(Color.black);
    	setTitle("SIFT Image Registration 3D");
    	
    	final String matchName = matchImage.getImageName();
        final JLabel labelImage = new JLabel("Register [" + matchName + "] to:");
        labelImage.setForeground(Color.black);
        labelImage.setFont(serif12);
        labelImage.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxImage = buildImgComboBox(matchImage);


        labelMatchingThreshold = new JLabel("Matching threshold");
        labelMatchingThreshold.setForeground(Color.black);
        labelMatchingThreshold.setFont(serif12);

        textMatchingThreshold = new JTextField(10);
        textMatchingThreshold.setText("0.8");
        textMatchingThreshold.setFont(serif12);

        labelErrorThreshold = new JLabel("Error threshold");
        labelErrorThreshold.setForeground(Color.black);
        labelErrorThreshold.setFont(serif12);

        textErrorThreshold = new JTextField(10);
        textErrorThreshold.setText("5.0");
        textErrorThreshold.setFont(serif12);
        
        labelNumberIterations = new JLabel("Number of iterations");
        labelNumberIterations.setForeground(Color.black);
        labelNumberIterations.setFont(serif12);

        textNumberIterations = new JTextField(10);
        textNumberIterations.setText("500");
        textNumberIterations.setFont(serif12);
        
        labelGaussianWidth = new JLabel("Gaussian width");
        labelGaussianWidth.setForeground(Color.black);
        labelGaussianWidth.setFont(serif12);

        textGaussianWidth = new JTextField(10);
        textGaussianWidth.setText("3.0");
        textGaussianWidth.setFont(serif12);
        
        matchCheckBox = new JCheckBox("Use match maximum distance");
        matchCheckBox.setFont(serif12);
        matchCheckBox.setSelected(false);
        matchCheckBox.addActionListener(this);
        
        labelMatch = new JLabel("Match maximum distance");
        labelMatch.setForeground(Color.black);
        labelMatch.setFont(serif12);
        labelMatch.setEnabled(false);

        textMatch = new JTextField(10);
        textMatch.setText("0.3");
        textMatch.setFont(serif12);
        textMatch.setEnabled(false);
        
        icosahedralCheckBox = new JCheckBox("Use icosahedral gradient histogram");
        icosahedralCheckBox.setFont(serif12);
        icosahedralCheckBox.setSelected(true);
        
        refineCheckBox = new JCheckBox("Use least squares refinement in RANSAC");
        refineCheckBox.setFont(serif12);
        refineCheckBox.setSelected(true);
        
        JPanel upperPanel = new JPanel(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelImage, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(comboBoxImage, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        upperPanel.add(labelMatchingThreshold, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textMatchingThreshold, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelErrorThreshold, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textErrorThreshold, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelNumberIterations, gbc);
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textNumberIterations, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelGaussianWidth, gbc);
        gbc.gridx = 1;
        gbc.gridy = 4;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textGaussianWidth, gbc);
        gbc.gridwidth = 2;
        gbc.gridx = 0;
        gbc.gridy = 5;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(matchCheckBox, gbc);
        gbc.gridwidth = 1;
        gbc.gridx = 0;
        gbc.gridy = 6;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelMatch, gbc);
        gbc.gridx = 1;
        gbc.gridy = 6;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textMatch, gbc);
        gbc.gridwidth = 2;
        gbc.gridx = 0;
        gbc.gridy = 7;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(icosahedralCheckBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 8;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(refineCheckBox, gbc);
        
        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());
        mainPanel.add(upperPanel, BorderLayout.NORTH);
        mainPanel.add(buildButtons(), BorderLayout.SOUTH);
        
        JScrollPane scrollPane = new JScrollPane(mainPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        
        getContentPane().add(scrollPane);

        pack();
        setVisible(true);
    }
    
    /**
     * Builds a list of images. Returns combobox.
     * 
     * @param image DOCUMENT ME!
     * 
     * @return Newly created combo box.
     */
    private JComboBox buildImgComboBox(final ModelImage image) {
        final JComboBox comboBox = new JComboBox();
        comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);

        final Enumeration<String> names = userInterface.getRegisteredImageNames();

        while (names.hasMoreElements()) {
            final String name = names.nextElement();

            if ( !name.equals(image.getImageName())) {
                final ModelImage img = userInterface.getRegisteredImageByName(name);

                if ( (img.getNDims() >= 3) && (!img.isColorImage()) && (!img.isComplexImage())
                        && (userInterface.getFrameContainingImage(img) != null)) {
                    comboBox.addItem(name);
                }
            }
        }
        comboBox.addItemListener(this);
        return comboBox;
    }
    
    private boolean setVariables() {
    	String tmpStr;
    	baseImage = userInterface.getRegisteredImageByName((String) comboBoxImage.getSelectedItem());
    	
    	tmpStr = textMatchingThreshold.getText();

        if (testParameter(tmpStr, 1.0E-3, 1000.0)) {
        	SIFT3D_nn_thresh_default = Double.valueOf(tmpStr).doubleValue();
        } else {
            textMatchingThreshold.requestFocus();
            textMatchingThreshold.selectAll();
            return false;
        }
        
        tmpStr = textErrorThreshold.getText();

        if (testParameter(tmpStr, 1.0E-3, 1000.0)) {
        	SIFT3D_err_thresh_default = Double.valueOf(tmpStr).doubleValue();
        } else {
            textErrorThreshold.requestFocus();
            textErrorThreshold.selectAll();
            return false;
        }
        
        tmpStr = textNumberIterations.getText();

        if (testParameter(tmpStr, 1, 1000000)) {
        	SIFT3D_num_iter_default = Integer.valueOf(tmpStr).intValue();
        } else {
            textNumberIterations.requestFocus();
            textNumberIterations.selectAll();
            return false;
        }
        
        tmpStr = textGaussianWidth.getText();

        if (testParameter(tmpStr, 1.0E-3, 1000.0)) {
        	SIFT3D_GAUSS_WIDTH_FCTR = Double.valueOf(tmpStr).doubleValue();
        } else {
            textGaussianWidth.requestFocus();
            textGaussianWidth.selectAll();
            return false;
        }
        
        if (!matchCheckBox.isSelected()) {
        	SIFT3D_MATCH_MAX_DIST = 0.0;	
        }
        else {
        	tmpStr = textMatch.getText();

            if (testParameter(tmpStr, 1.0E-3, 1000.0)) {
            	SIFT3D_MATCH_MAX_DIST = Double.valueOf(tmpStr).doubleValue();
            } else {
                textMatch.requestFocus();
                textMatch.selectAll();
                return false;
            }	
        }
        
        ICOS_HIST = icosahedralCheckBox.isSelected();
        
        SIFT3D_RANSAC_REFINE = refineCheckBox.isSelected();
        
    	return true;

    }
    
    protected void setGUIFromParams() {
    	matchImage = scriptParameters.retrieveInputImage();
    	userInterface = ViewUserInterface.getReference();
    	baseImage = scriptParameters.retrieveImage("base_image");
    	SIFT3D_nn_thresh_default = scriptParameters.getParams().getDouble("nn_thresh");
    	SIFT3D_err_thresh_default = scriptParameters.getParams().getDouble("err_thresh");
    	SIFT3D_num_iter_default = scriptParameters.getParams().getInt("num_iter");
    	SIFT3D_GAUSS_WIDTH_FCTR = scriptParameters.getParams().getDouble("Gauss_width");
    	SIFT3D_MATCH_MAX_DIST = scriptParameters.getParams().getDouble("max_dist");
    	ICOS_HIST = scriptParameters.getParams().getBoolean("icos");
    	SIFT3D_RANSAC_REFINE = scriptParameters.getParams().getBoolean("refine");
    }
    
    protected void storeParamsFromGUI() throws ParserException {
    	scriptParameters.storeInputImage(matchImage);
        scriptParameters.storeImage(baseImage, "base_image");	
        scriptParameters.getParams().put(ParameterFactory.newParameter("nn_thresh", SIFT3D_nn_thresh_default));
        scriptParameters.getParams().put(ParameterFactory.newParameter("err_thresh", SIFT3D_err_thresh_default));
        scriptParameters.getParams().put(ParameterFactory.newParameter("num_iter", SIFT3D_num_iter_default));
        scriptParameters.getParams().put(ParameterFactory.newParameter("Gauss_width", SIFT3D_GAUSS_WIDTH_FCTR));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_dist", SIFT3D_MATCH_MAX_DIST));
        scriptParameters.getParams().put(ParameterFactory.newParameter("icos", ICOS_HIST));
        scriptParameters.getParams().put(ParameterFactory.newParameter("refine", SIFT3D_RANSAC_REFINE));
    }

    
}
