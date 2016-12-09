package gov.nih.mipav.view.dialogs;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmEmbeddedConfidenceEdgeDetection;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewImageUpdateInterface;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;

@SuppressWarnings("serial")
public class JDialogEmbeddedConfidenceEdgeDetection extends JDialogScriptableBase implements AlgorithmInterface {
	 // default values for edge detection
    private static final double CONF_NMX = 0.5;
    private static final double RANK_NMX = 0.5;
    private static final double CONF_H = 0.96;
    private static final double RANK_H = 0.93;
    private static final double CONF_L = 0.91;
    private static final double RANK_L = 0.99;
    private static final int NMIN = 5;
    private static final int KERNEL_SIZE = 2;

    private static final int FC_ELLIPSE = 0;
    private static final int FC_VERT_LINE = 1;
    private static final int FC_HORIZ_LINE = 2;
    private static final int FC_LINE= 3;
    private static final int FC_SQUARE_BOX = 4;
    //private static final int FC_CUSTOM = 5; 
	private int displayLoc; // Flag indicating if a new image is to be generated
	
	private ModelImage image; // source image
	
	private ViewUserInterface userInterface;
	
	private AlgorithmEmbeddedConfidenceEdgeDetection edgeDetectionAlgo;
	
    private ModelImage resultImage = null;
	
	private String[] titles;
	
    private JRadioButton newImage;
    
    private JRadioButton replaceImage;
    
    // Window side is 2*kernelSize + 1
    // In the reference:
    // The 512 by 512 basket image was processed with a 7 by 7 gradient operator.
    // The 256 by 256 cameraman image was processed with a 5 by 5 gradient operator.
    // The 512 by 438 grater image was processed with a 7 by 7 gradient operator.
    // The 548 by 509 golf cart image was processed with a 7 by 7 gradient operator.
    private int kernelSize = KERNEL_SIZE;
    // nmxr, nmxc threshold for non-maxima-suppresion rank, confidence
    private double nmxr = RANK_NMX;
    private double nmxc = CONF_NMX;
    // rh, ch, threshold for hyst. high; rank, confidence
    private double rh = RANK_H;
    private double ch = CONF_H;
    // rl, cl, threshold for hyst. low; rank, confidence
    private double rl = RANK_L;
    private double cl = CONF_L;
    // nMin, min number of pixels on an edge
    private int nMin = NMIN;
    // nmxType, hystTypeHigh, hystTypeLow, type of nmx curve, hyst. high curve, hyst low curve
    //  in (FC_ELLIPSE, FC_VERT_LINE, FC_HORIZ_LINE, FC_LINE, FC_SQUARE_BOX, FC_CUSTOM)
    private int nmxType = FC_ELLIPSE;
    private int hystTypeHigh = FC_SQUARE_BOX;
    private int hystTypeLow = FC_ELLIPSE;
    
    private JTextField windowSizeText;
    
    private JTextField suppressionRankText;
    
    private JTextField suppressionConfidenceText;
    
    private JTextField hysteresisHighRankText;
    
    private JTextField hysteresisHighConfidenceText;
    
    private JTextField hysteresisLowRankText;
    
    private JTextField hysteresisLowConfidenceText;
    
    private JTextField nMinText;
    
    private JComboBox comboBoxSuppression;
    
    private JComboBox comboBoxHigh;
    
    private JComboBox comboBoxLow;
    
  //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogEmbeddedConfidenceEdgeDetection() { }

    // or if the source image is to be replaced

    /**
     * Creates new dialog for entering parameters for embedded confidence edge detection.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogEmbeddedConfidenceEdgeDetection(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface =  ViewUserInterface.getReference();
        init();
    }
    
    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
    	
        JPanel mainPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridx = 0;
    	setForeground(Color.black);
        setTitle("Embedded Confidence Edge Detection");
        
        JLabel windowSizeLabel = new JLabel("Window size");
        windowSizeLabel.setForeground(Color.black);
        windowSizeLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(windowSizeLabel, gbc);
        
        windowSizeText = new JTextField("5");
        windowSizeText.setForeground(Color.black);
        windowSizeText.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(windowSizeText, gbc);
        
        JLabel suppressionRankLabel = new JLabel("Threshold for non-maxima-suppresion rank");
        suppressionRankLabel.setForeground(Color.black);
        suppressionRankLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(suppressionRankLabel, gbc);
        
        suppressionRankText = new JTextField("0.5");
        suppressionRankText.setForeground(Color.black);
        suppressionRankText.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(suppressionRankText, gbc);
        
        JLabel suppressionConfidenceLabel = new JLabel("Threshold for non-maxima-suppresion confidence");
        suppressionConfidenceLabel.setForeground(Color.black);
        suppressionConfidenceLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(suppressionConfidenceLabel, gbc);
        
        suppressionConfidenceText = new JTextField("0.5");
        suppressionConfidenceText.setForeground(Color.black);
        suppressionConfidenceText.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(suppressionConfidenceText, gbc);
        
        JLabel hysteresisHighRankLabel = new JLabel("Threshold for hysteresis high rank");
        hysteresisHighRankLabel.setForeground(Color.black);
        hysteresisHighRankLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(hysteresisHighRankLabel, gbc);
        
        hysteresisHighRankText = new JTextField("0.93");
        hysteresisHighRankText.setForeground(Color.black);
        hysteresisHighRankText.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(hysteresisHighRankText, gbc);
        
        JLabel hysteresisHighConfidenceLabel = new JLabel("Threshold for hysteresis high confidence");
        hysteresisHighConfidenceLabel.setForeground(Color.black);
        hysteresisHighConfidenceLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(hysteresisHighConfidenceLabel, gbc);
        
        hysteresisHighConfidenceText = new JTextField("0.96");
        hysteresisHighConfidenceText.setForeground(Color.black);
        hysteresisHighConfidenceText.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(hysteresisHighConfidenceText, gbc);
        
        JLabel hysteresisLowRankLabel = new JLabel("Threshold for hysteresis low rank");
        hysteresisLowRankLabel.setForeground(Color.black);
        hysteresisLowRankLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(hysteresisLowRankLabel, gbc);
        
        hysteresisLowRankText = new JTextField("0.99");
        hysteresisLowRankText.setForeground(Color.black);
        hysteresisLowRankText.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(hysteresisLowRankText, gbc);
        
        JLabel hysteresisLowConfidenceLabel = new JLabel("Threshold for hysteresis low confidence");
        hysteresisLowConfidenceLabel.setForeground(Color.black);
        hysteresisLowConfidenceLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(hysteresisLowConfidenceLabel, gbc);
        
        hysteresisLowConfidenceText = new JTextField("0.91");
        hysteresisLowConfidenceText.setForeground(Color.black);
        hysteresisLowConfidenceText.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(hysteresisLowConfidenceText, gbc);
        
        JLabel nMinLabel = new JLabel("Minimum number of pixels on an edge");
        nMinLabel.setForeground(Color.black);
        nMinLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(nMinLabel, gbc);
        
        nMinText = new JTextField("5");
        nMinText.setForeground(Color.black);
        nMinText.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(nMinText, gbc);
        
        JLabel suppressionLabel = new JLabel("Type of non-maxima suppression curve");
        suppressionLabel.setForeground(Color.black);
        suppressionLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(suppressionLabel, gbc);
        
        comboBoxSuppression = new JComboBox();
        comboBoxSuppression.setFont(serif12);
        comboBoxSuppression.setBackground(Color.white);
        comboBoxSuppression.addItem("Ellipse");
        comboBoxSuppression.addItem("Vertical line");
        comboBoxSuppression.addItem("Horizontal line");
        comboBoxSuppression.addItem("Line");
        comboBoxSuppression.addItem("Square box");
        comboBoxSuppression.setSelectedIndex(0);
        gbc.gridx = 1;
        mainPanel.add(comboBoxSuppression, gbc);
        
        JLabel highLabel = new JLabel("Type of hysteresis high curve");
        highLabel.setForeground(Color.black);
        highLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(highLabel, gbc);
        
        comboBoxHigh = new JComboBox();
        comboBoxHigh.setFont(serif12);
        comboBoxHigh.setBackground(Color.white);
        comboBoxHigh.addItem("Ellipse");
        comboBoxHigh.addItem("Vertical line");
        comboBoxHigh.addItem("Horizontal line");
        comboBoxHigh.addItem("Line");
        comboBoxHigh.addItem("Square box");
        comboBoxHigh.setSelectedIndex(4);
        gbc.gridx = 1;
        mainPanel.add(comboBoxHigh, gbc);
        
        JLabel lowLabel = new JLabel("Type of hysteresis low curve");
        lowLabel.setForeground(Color.black);
        lowLabel.setFont(serif12);
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy++;
        mainPanel.add(lowLabel, gbc);
        
        comboBoxLow = new JComboBox();
        comboBoxLow.setFont(serif12);
        comboBoxLow.setBackground(Color.white);
        comboBoxLow.addItem("Ellipse");
        comboBoxLow.addItem("Vertical line");
        comboBoxLow.addItem("Horizontal line");
        comboBoxLow.addItem("Line");
        comboBoxLow.addItem("Square box");
        comboBoxLow.setSelectedIndex(0);
        gbc.gridx = 1;
        mainPanel.add(comboBoxLow, gbc);
        
        JPanel destinationPanel = new JPanel(new GridLayout(2, 1));
        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        ButtonGroup destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage);
        destinationPanel.add(newImage);

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage);
        destinationPanel.add(replaceImage);

        if ((image.getLockStatus() == ModelStorageBase.UNLOCKED) && (!image.isColorImage())) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }
        gbc.gridy++;
        mainPanel.add(destinationPanel, gbc);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setSize(600,550);
        setVisible(true);
    }
    
    /**
     * actionPerformed - Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else {
            super.actionPerformed(event);
        }
        
    }
    
    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        int windowSize;
        int index;
        
        tmpStr = windowSizeText.getText();
        try {
        	windowSize = Integer.parseInt(tmpStr);
        }
        catch (NumberFormatException e) {
			MipavUtil.displayError("The window entry is not a valid integer");
			windowSizeText.requestFocus();
			windowSizeText.selectAll();
			return false;
		}
        if (windowSize < 3) {
        	MipavUtil.displayError("Window size must be at least 3");
        	windowSizeText.requestFocus();
			windowSizeText.selectAll();
        	return false;
        }
        if ((windowSize % 2) == 0) {
        	MipavUtil.displayError("Window size must be odd");
        	windowSizeText.requestFocus();
			windowSizeText.selectAll();
        	return false;
        }
        kernelSize = (windowSize - 1)/2;
        
        tmpStr = suppressionRankText.getText();
        try {
        	nmxr = Double.parseDouble(tmpStr);
        }
        catch (NumberFormatException e) {
			MipavUtil.displayError("The non-maxima suppression rank is not a valid double");
			suppressionRankText.requestFocus();
			suppressionRankText.selectAll();
			return false;
		}
        if (nmxr <= 0.0) {
        	MipavUtil.displayError("The non-maxima suppression rank must exceed zero");
        	suppressionRankText.requestFocus();
			suppressionRankText.selectAll();
        	return false;
        }
        if (nmxr > 1.0) {
        	MipavUtil.displayError("The non-maxima suppression rank cannot exceed 1");
        	suppressionRankText.requestFocus();
			suppressionRankText.selectAll();
        	return false;	
        }
        
        tmpStr = suppressionConfidenceText.getText();
        try {
        	nmxc = Double.parseDouble(tmpStr);
        }
        catch (NumberFormatException e) {
			MipavUtil.displayError("The non-maxima suppression confidence is not a valid double");
			suppressionConfidenceText.requestFocus();
			suppressionConfidenceText.selectAll();
			return false;
		}
        if (nmxc <= 0.0) {
        	MipavUtil.displayError("The non-maxima suppression confidence must exceed zero");
        	suppressionConfidenceText.requestFocus();
			suppressionConfidenceText.selectAll();
        	return false;
        }
        if (nmxc > 1.0) {
        	MipavUtil.displayError("The non-maxima suppression confidence cannot exceed 1");
        	suppressionConfidenceText.requestFocus();
			suppressionConfidenceText.selectAll();
        	return false;	
        }
        
        tmpStr = hysteresisHighRankText.getText();
        try {
        	rh = Double.parseDouble(tmpStr);
        }
        catch (NumberFormatException e) {
			MipavUtil.displayError("The hysteresis high rank is not a valid double");
			hysteresisHighRankText.requestFocus();
			hysteresisHighRankText.selectAll();
			return false;
		}
        if (rh <= 0.0) {
        	MipavUtil.displayError("The hysteresis high rank must exceed zero");
        	hysteresisHighRankText.requestFocus();
			hysteresisHighRankText.selectAll();
        	return false;
        }
        if (rh > 1.0) {
        	MipavUtil.displayError("The hysteresis high rank cannot exceed 1");
        	hysteresisHighRankText.requestFocus();
			hysteresisHighRankText.selectAll();
        	return false;	
        }
        
        tmpStr = hysteresisHighConfidenceText.getText();
        try {
        	ch = Double.parseDouble(tmpStr);
        }
        catch (NumberFormatException e) {
			MipavUtil.displayError("The hysteresis high confidence is not a valid double");
			hysteresisHighConfidenceText.requestFocus();
			hysteresisHighConfidenceText.selectAll();
			return false;
		}
        if (ch <= 0.0) {
        	MipavUtil.displayError("The hysteresis high confidence must exceed zero");
        	hysteresisHighConfidenceText.requestFocus();
			hysteresisHighConfidenceText.selectAll();
        	return false;
        }
        if (ch > 1.0) {
        	MipavUtil.displayError("The hysteresis high confidence cannot exceed 1");
        	hysteresisHighConfidenceText.requestFocus();
			hysteresisHighConfidenceText.selectAll();
        	return false;	
        }
        
        tmpStr = hysteresisLowRankText.getText();
        try {
        	rl = Double.parseDouble(tmpStr);
        }
        catch (NumberFormatException e) {
			MipavUtil.displayError("The hysteresis low rank is not a valid double");
			hysteresisLowRankText.requestFocus();
			hysteresisLowRankText.selectAll();
			return false;
		}
        if (rl <= 0.0) {
        	MipavUtil.displayError("The hysteresis low rank must exceed zero");
        	hysteresisLowRankText.requestFocus();
			hysteresisLowRankText.selectAll();
        	return false;
        }
        if (rl > 1.0) {
        	MipavUtil.displayError("The hysteresis low rank cannot exceed 1");
        	hysteresisLowRankText.requestFocus();
			hysteresisLowRankText.selectAll();
        	return false;	
        }
        
        tmpStr = hysteresisLowConfidenceText.getText();
        try {
        	cl = Double.parseDouble(tmpStr);
        }
        catch (NumberFormatException e) {
			MipavUtil.displayError("The hysteresis low confidence is not a valid double");
			hysteresisLowConfidenceText.requestFocus();
			hysteresisLowConfidenceText.selectAll();
			return false;
		}
        if (cl <= 0.0) {
        	MipavUtil.displayError("The hysteresis low confidence must exceed zero");
        	hysteresisLowConfidenceText.requestFocus();
			hysteresisLowConfidenceText.selectAll();
        	return false;
        }
        if (cl > 1.0) {
        	MipavUtil.displayError("The hysteresis low confidence cannot exceed 1");
        	hysteresisLowConfidenceText.requestFocus();
			hysteresisLowConfidenceText.selectAll();
        	return false;	
        }
        
        tmpStr = nMinText.getText();
        try {
        	nMin = Integer.parseInt(tmpStr);
        }
        catch (NumberFormatException e) {
			MipavUtil.displayError("The minimum number of edge pixels is not a valid integer");
			nMinText.requestFocus();
			nMinText.selectAll();
			return false;
		}
        if (nMin <= 0) {
        	MipavUtil.displayError("The minimum number of edge pixels must exceed zero");
        	nMinText.requestFocus();
			nMinText.selectAll();
        	return false;
        }
        
        index = comboBoxSuppression.getSelectedIndex();
        if (index == 0) {
        	nmxType = FC_ELLIPSE;
        }
        else if (index == 1) {
        	nmxType = FC_VERT_LINE;
        }
        else if (index == 2) {
        	nmxType = FC_HORIZ_LINE;
        }
        else if (index == 3) {
        	nmxType = FC_LINE;
        }
        else if (index == 4) {
        	nmxType = FC_SQUARE_BOX;
        }
        
        index = comboBoxHigh.getSelectedIndex();
        if (index == 0) {
        	hystTypeHigh = FC_ELLIPSE;
        }
        else if (index == 1) {
        	hystTypeHigh = FC_VERT_LINE;
        }
        else if (index == 2) {
        	hystTypeHigh = FC_HORIZ_LINE;
        }
        else if (index == 3) {
        	hystTypeHigh = FC_LINE;
        }
        else if (index == 4) {
        	hystTypeHigh = FC_SQUARE_BOX;
        }
        
        index = comboBoxLow.getSelectedIndex();
        if (index == 0) {
        	hystTypeLow = FC_ELLIPSE;
        }
        else if (index == 1) {
        	hystTypeLow = FC_VERT_LINE;
        }
        else if (index == 2) {
        	hystTypeLow = FC_HORIZ_LINE;
        }
        else if (index == 3) {
        	hystTypeLow = FC_LINE;
        }
        else if (index == 4) {
        	hystTypeLow = FC_SQUARE_BOX;
        }
        
        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }
        return true;
    }
    
 // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        if (algorithm instanceof AlgorithmEmbeddedConfidenceEdgeDetection) {
            image.clearMask();

            if ((edgeDetectionAlgo.isCompleted() == true) && (resultImage != null)) {
                // The algorithm has completed and produced a new image to be displayed.

                updateFileInfo(image, resultImage);

                try {

                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }


            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                image.notifyImageDisplayListeners(null, true);
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
            
            
        }
        // Update frame
        // ((ViewJFrameBase)parentFrame).updateImages(true);

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        edgeDetectionAlgo.finalize();
        edgeDetectionAlgo = null;
        dispose();
    }
    
    /**
     * Once all the necessary variables are set, call the Mean Shift Segmentation algorithm based on what type of image
     * this is and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_edgeDetection");

        if (displayLoc == NEW) {

            try {
                
                 resultImage = new ModelImage(ModelStorageBase.BYTE, image.getExtents(), name);	

                /*if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM){
                 *  ((FileInfoDicom)(resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                 *}
                 */
                
                // Make algorithm
                edgeDetectionAlgo = new AlgorithmEmbeddedConfidenceEdgeDetection(resultImage, image, kernelSize, nmxr, nmxc,
                		rh, ch, rl, cl, nMin, nmxType, hystTypeHigh, hystTypeLow);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                edgeDetectionAlgo.addListener(this);

                createProgressBar(image.getImageName(), edgeDetectionAlgo);
                
                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (edgeDetectionAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    edgeDetectionAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Embedded Confidence Edge Detection: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }

                return;
            }
        } else {

            try {

                // No need to make new image space because the user has choosen to replace the source image
                // Make the algorithm class
                edgeDetectionAlgo = new AlgorithmEmbeddedConfidenceEdgeDetection(null, image, kernelSize, nmxr, nmxc,
                		rh, ch, rl, cl, nMin, nmxType, hystTypeHigh, hystTypeLow);

                // This is very important. Adding this object as a listener allows the algorithm to
                // notify this object when it has completed of failed. See algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed interface
                edgeDetectionAlgo.addListener(this);

                createProgressBar(image.getImageName(), edgeDetectionAlgo);
                
                // Hide the dialog since the algorithm is about to run.
                setVisible(false);

                // These next lines set the titles in all frames where the source image is displayed to
                // "locked - " image name so as to indicate that the image is now read/write locked!
                // The image frames are disabled and then unregisted from the userinterface until the
                // algorithm has completed.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();
                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                    ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                    userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                }

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface.
                    if (edgeDetectionAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    edgeDetectionAlgo.run();
                }
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog Embedded Confidence Edge Detection: unable to allocate enough memory");

                return;
            }
        }

    }
    
    /**
     * Accessor that sets the display loc variable to new, so that a new image is created once the algorithm completes.
     */
    public void setDisplayLocNew() {
        displayLoc = NEW;
    }

    /**
     * Accessor that sets the display loc variable to replace, so the current image is replaced once the algorithm
     * completes.
     */
    public void setDisplayLocReplace() {
        displayLoc = REPLACE;
    }
    
    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();
        
        if (scriptParameters.doOutputNewImage()) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }
        kernelSize = scriptParameters.getParams().getInt("kernel_size");
        nmxr = scriptParameters.getParams().getDouble("nmxr_");
        nmxc = scriptParameters.getParams().getDouble("nmxc_");
        rh = scriptParameters.getParams().getDouble("rh_");
        ch = scriptParameters.getParams().getDouble("ch_");
        rl = scriptParameters.getParams().getDouble("rl_");
        cl = scriptParameters.getParams().getDouble("cl_");
        nMin = scriptParameters.getParams().getInt("n_min");
        nmxType = scriptParameters.getParams().getInt("nmx_type");
        hystTypeHigh = scriptParameters.getParams().getInt("hyst_type_high");
        hystTypeLow = scriptParameters.getParams().getInt("hyst_type_low");
    }
    
    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, (displayLoc == NEW));
        scriptParameters.getParams().put(ParameterFactory.newParameter("kernel_size", kernelSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("nmxr_", nmxr));
        scriptParameters.getParams().put(ParameterFactory.newParameter("nmxc_", nmxc));
        scriptParameters.getParams().put(ParameterFactory.newParameter("rh_", rh));
        scriptParameters.getParams().put(ParameterFactory.newParameter("ch_", ch));
        scriptParameters.getParams().put(ParameterFactory.newParameter("rl_", rl));
        scriptParameters.getParams().put(ParameterFactory.newParameter("cl_", cl));
        scriptParameters.getParams().put(ParameterFactory.newParameter("n_min", nMin));
        scriptParameters.getParams().put(ParameterFactory.newParameter("nmx_type", nmxType));
        scriptParameters.getParams().put(ParameterFactory.newParameter("hyst_type_high", hystTypeHigh));
        scriptParameters.getParams().put(ParameterFactory.newParameter("hyst_type_low", hystTypeLow));
    }
}