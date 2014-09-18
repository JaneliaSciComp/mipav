package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.JPanelColorChannels;

import java.awt.*;
import java.awt.event.*;
import java.io.File;
import java.util.Vector;

import javax.swing.ButtonGroup;
import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.filechooser.FileFilter;

//import javax.swing.*;


/**
 * DOCUMENT ME!
 */
public class JDialogBRISK extends JDialogScriptableBase implements AlgorithmInterface, ActionDiscovery, ScriptableActionInterface,
                                                                   ListSelectionListener{

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    
    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage destImage[] = null; // match image array

    /** DOCUMENT ME! */
    private AlgorithmBRISK bAlgo;
    
    private int threshold = 60;
    
    private int HammingDistanceThreshold = 90;
    
    private int octaves = 4;
    
    private boolean rotationInvariant = true;
	
	private boolean scaleInvariant = true;
	
	private double patternScale = 1.0;
   	
    private static final int radiusMatch = 1;
	
	private static final int knnMatch = 2;
	
	private int match = radiusMatch;
	
	private Vector<Double>radiusList = null;
	
	private Vector<Integer>numberList = null;
	
	// Short pair maximum distance
	private double dMax = 5.85;
	
	// Long pair maximum distance
	private double dMin = 8.2;
	
	private Vector<Integer>indexChange = new Vector<Integer>();
	
	private JPanel imagePanel;
	
	private DefaultListModel model;
	
	 private JList imageList;
	 
	 private JButton chooserButton;
	 
	 private JButton removeButton;
	 
	 private ButtonGroup matchGroup;
	 
	 private JRadioButton  radiusButton;
	 
	 private JRadioButton  knnButton;
	 
	 private JPanel matchPanel;
	 
	 private JPanel paramPanel;
	 
	 private JPanel imageVOIPanel;
	 
	 private ButtonGroup imageVOIGroup;
	 
	 private JRadioButton wholeImage;
	 
	 private JRadioButton VOIRegions;
	 
	 private boolean changeRemoveIndex = true;
	 
	 private int removeIndex;
	 
	 private ViewUserInterface userInterface;
	 
	 private int destNumber = 0;
	 
	 private ModelImage[] tempImage = null;
	 
	 private JLabel labelThreshold;
	 
	 private JTextField textThreshold;
	 
	 private boolean wholeImageFlag = true;
	 
	 private JLabel labelHammingDistanceThreshold;
	 
	 private JTextField textHammingDistanceThreshold;
	 
	 private JLabel labelOctaves;
	 
	 private JTextField textOctaves;
	 
	 private JCheckBox rotationCheckBox;
	 
	 private JCheckBox scaleCheckBox;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogBRISK() { }

    /**
     * Creates new dialog for entering parameters for BRISK.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogBRISK(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * actionPerformed - Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        int i;
        int j;

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        }
            
        else if (command.equals("Choose")) {
            ModelImage newImage = open();

            if (!checkImage(newImage)) {
                return;
            }

            destNumber++;
            if (destNumber > 1) {
	            tempImage = new ModelImage[destNumber-1];
	
	            for (i = 0; i < (destNumber-1); i++) {
	                tempImage[i] = destImage[i];
	            }
	
	            destImage = null;
	            destImage = new ModelImage[destNumber];
	
	            for (i = 0; i < (destNumber - 1); i++) {
	                destImage[i] = tempImage[i];
	            }
	
	            tempImage = null;
            } // if (destNumber > 1)
            else {
            	destImage = new ModelImage[destNumber];
            }
            destImage[destNumber - 1] = newImage;
            model.addElement(destImage[destNumber - 1].getImageName());

            newImage = null;
            removeButton.setEnabled(true);
        } // if (command.equals("Choose"))
        else if ((command.equals("Remove")) && (removeIndex == 0)) {

            // Cannot remove original image
            MipavUtil.displayError("Cannot remove original loaded image");
        } else if ((command.equals("Remove")) && (removeIndex <= (destNumber - 1))) {

            // changeRemoveIndex = false is needed because the model.removeElement
            // line causes valueChanged to execute.  Without changeRemoveIndex an
            // unselected element causes removeIndex to be changed to -1.
            changeRemoveIndex = false;
            model.removeElement(destImage[removeIndex].getImageName());
            tempImage = new ModelImage[destNumber - 1];

            for (i = 0, j = 0; i < destNumber; i++) {

                if (i != removeIndex) {
                    tempImage[j++] = destImage[i];
                }
            } // for ( i = 0, j=0; i < destNumber; i++)

            destImage[removeIndex].disposeLocal();
            destImage[removeIndex] = null;
            changeRemoveIndex = true;
            destImage = null;
            destImage = new ModelImage[destNumber - 1];

            for (i = 0; i < (destNumber - 1); i++) {
                destImage[i] = tempImage[i];
            }

            tempImage = null;
            destNumber--;

            if (destNumber == 0) {
                removeButton.setEnabled(false);
            }
        } // else if ((command.equals("Remove"))  && (removeIndex <= (destNumber - 1)))
        else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        } else if (command.equals("Cancel")) {
            dispose();
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

        if (algorithm instanceof AlgorithmBRISK) {
            System.err.println("BRISK Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();
        }

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }
        // save the completion status for later
        setComplete(algorithm.isCompleted());

        bAlgo.finalize();
        bAlgo = null;
        //dispose();
    }


    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Method to handle item events.
     *
     * @param  event  event that cause the method to fire
     */
    public void itemStateChanged(ItemEvent event) {
        // Object source = event.getSource();
        // float tempNum;

    }

    
   
    /**
     * Once all the necessary variables are set, call the rule based contrast enhancement algorithm based on what type of image this is and
     * whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        try {

            // Make algorithm
            bAlgo = new AlgorithmBRISK(destImage, image, wholeImageFlag, threshold, HammingDistanceThreshold, octaves, 
            		rotationInvariant, scaleInvariant, patternScale, match, radiusList, numberList,
                    dMax, dMin, indexChange);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed or failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            bAlgo.addListener(this);

            createProgressBar(image.getImageName(), bAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (bAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                bAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog BRISK: unable to allocate enough memory");

            

            return;
        }
           
    }

    

    /**
     * {@inheritDoc}
     */
    protected void doPostAlgorithmActions() {

        
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
    	setForeground(Color.black);

        setTitle("BRISK");

        labelThreshold = new JLabel("Detection threshold");
        labelThreshold.setForeground(Color.black);
        labelThreshold.setFont(serif12);

        textThreshold = new JTextField(5);
        textThreshold.setText("60");
        textThreshold.setFont(serif12);

        labelHammingDistanceThreshold = new JLabel("Maximum possible descriptors distance");
        labelHammingDistanceThreshold.setForeground(Color.black);
        labelHammingDistanceThreshold.setFont(serif12);

        textHammingDistanceThreshold = new JTextField(5);
        textHammingDistanceThreshold.setText("90");
        textHammingDistanceThreshold.setFont(serif12);

        labelOctaves = new JLabel("Octaves for the detection");
        labelOctaves.setForeground(Color.black);
        labelOctaves.setFont(serif12);
        
        textOctaves = new JTextField(5);
        textOctaves.setText("4");
        textOctaves.setFont(serif12);
        
        rotationCheckBox = new JCheckBox("Rotation invariance");
        rotationCheckBox.setFont(serif12);
        rotationCheckBox.setSelected(true);
        
        scaleCheckBox = new JCheckBox("Scale invariance");
        scaleCheckBox.setFont(serif12);
        scaleCheckBox.setSelected(true);

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
        upperPanel.add(labelThreshold, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textThreshold, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelHammingDistanceThreshold, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textHammingDistanceThreshold, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        upperPanel.add(labelOctaves, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        upperPanel.add(textOctaves, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 2;
        upperPanel.add(rotationCheckBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.gridwidth = 2;
        upperPanel.add(scaleCheckBox, gbc);

        imageVOIPanel = new JPanel(new GridBagLayout());
        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("Image region"));

        imageVOIGroup = new ButtonGroup();
        wholeImage = new JRadioButton("Whole image", true);
        wholeImage.setFont(serif12);
        imageVOIGroup.add(wholeImage);

        VOIRegions = new JRadioButton("VOI region(s)", false);
        VOIRegions.setFont(serif12);
        imageVOIGroup.add(VOIRegions);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.gridwidth = 1;
        gbc.insets = new Insets(0, 0, 0, 0);
        imageVOIPanel.add(wholeImage, gbc);
        gbc.gridy = 1;
        imageVOIPanel.add(VOIRegions, gbc);

        matchGroup = new ButtonGroup();
        radiusButton = new JRadioButton("Radius match", true);
        radiusButton.setFont(serif12);
        matchGroup.add(radiusButton);

        knnButton = new JRadioButton("knn match", false);
        knnButton.setFont(serif12);
        matchGroup.add(knnButton);
        
        matchPanel = new JPanel(new GridBagLayout());
        matchPanel.setBorder(buildTitledBorder("Matching method"));

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridwidth = 1;
        matchPanel.add(radiusButton, gbc);
        gbc.gridy = 1;
        matchPanel.add(knnButton, gbc);

        paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        gbc.gridx = 0;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        int ypos = 0;
        gbc.gridy = ypos++;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1;
        paramPanel.add(upperPanel, gbc);
        gbc.gridy = ypos++;
        gbc.gridwidth = 1;
        paramPanel.add(imageVOIPanel, gbc);
        gbc.gridx = 1;
        paramPanel.add(matchPanel, gbc);

        

        imagePanel = new JPanel(new BorderLayout());
        imagePanel.setBorder(buildTitledBorder("Load Image(s)"));

        model = new DefaultListModel();
        imageList = new JList(model);
        imageList.setVisibleRowCount(6);
        imageList.setPreferredSize(new Dimension(300, 120));
        imageList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        imageList.addListSelectionListener(this);
        imagePanel.add(imageList);

        JPanel chooserPanel = new JPanel();
        chooserButton = new JButton("Load");
        chooserButton.setPreferredSize(MipavUtil.defaultButtonSize);
        chooserButton.setFont(serif12B);
        chooserPanel.add(chooserButton);
        chooserButton.addActionListener(this);
        chooserButton.setActionCommand("Choose");

        removeButton = new JButton("Remove");
        removeButton.setPreferredSize(MipavUtil.defaultButtonSize);
        removeButton.setFont(serif12B);
        removeButton.setEnabled(false);
        chooserPanel.add(removeButton);
        removeButton.addActionListener(this);
        removeButton.setActionCommand("Remove");

        imagePanel.add(chooserPanel, BorderLayout.SOUTH);

        gbc.gridx = 0;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.gridy = ypos++;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        paramPanel.add(imagePanel, gbc);

        getContentPane().add(paramPanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }
    
    /**
     * Open an image based on the suffix of the file.
     *
     * @return  The image.
     */
    private ModelImage open() {
        JFileChooser chooser = null;
        FileIO fileIO = null;
        boolean multiFile = false;
        String fileName;
        String directory;

        try {

            chooser = new JFileChooser();

            if (userInterface.getDefaultDirectory() != null) {
                File file = new File(userInterface.getDefaultDirectory());

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

            FileFilter currentFileFilter = getFileFilter(chooser, Preferences.getFileFilter());
            chooser.setFileFilter(currentFileFilter);

            chooser.setDialogTitle("Open Image");

            int returnValue = chooser.showOpenDialog(userInterface.getMainFrame());

            if (returnValue == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                userInterface.setDefaultDirectory(directory);
            } else {
                return null;
            }
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return null;
        }

        try {
            fileIO = new FileIO();

            return fileIO.readImage(fileName, directory, multiFile, null);
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return null;
        }
    }
    
    /**
     * A private helper function to get the current used FileFilter from JFileChooser.
     *
     * @param   chooser  DOCUMENT ME!
     * @param   index    the index of the choosable file filters.
     *
     * @return  the current used file filter.
     */
    private FileFilter getFileFilter(JFileChooser chooser, int index) {
        FileFilter[] filters = chooser.getChoosableFileFilters();
        String[] descriptions = ViewImageFileFilter.getDescriptions();

        for (int i = 0; i < filters.length; i++) {

            if (filters[i].getDescription().equals(descriptions[index])) {
                return filters[i];
            }
        }

        return null;
    }
    
    /**
     * Checks the color and dimensionality of the new image vs. the original source image. All new images should have
     * the same color modality as the source and be of the same dimensions.
     *
     * @param   testImage  DOCUMENT ME!
     *
     * @return  Flag indicating if the image checks out.
     */
    private boolean checkImage(ModelImage testImage) {

        if (testImage == null) {
            return false;
        }

        if (testImage.isColorImage()) {
            MipavUtil.displayError("Cannot load a color (" + testImage.getImageName() +
                                   ")");

            return false;
        }
        
        if (testImage.isComplexImage()) {
        	MipavUtil.displayError("Cannot load a complex (" + testImage.getImageName() +
                    ")");

            return false;	
        }

        if (testImage.getNDims() != 2) {
            MipavUtil.displayError("Error! " + testImage.getImageName() + " is " + testImage.getNDims() + "D" + "instead of 2D");

            return false;
        }

        return true;

    }
    
    /**
     * Sets the remove index based on the selected index in the list.
     *
     * @param  evt  Event that caused this method to fire.
     */
    public void valueChanged(ListSelectionEvent evt) {

        if (changeRemoveIndex) {
            JList source = (JList) evt.getSource();
            removeIndex = source.getSelectedIndex();
        }
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
    	String tmpStr;

        if (wholeImage.isSelected()) {
            wholeImageFlag = true;
        } else if (VOIRegions.isSelected()) {
            wholeImageFlag = false;
        }
        
        tmpStr = textThreshold.getText();

        if (testParameter(tmpStr, 1, 255)) {
            threshold = Integer.valueOf(tmpStr).intValue();
        } else {
            textThreshold.requestFocus();
            textThreshold.selectAll();

            return false;
        }
        
        tmpStr = textHammingDistanceThreshold.getText();

        if (testParameter(tmpStr, 30, 200)) {
            HammingDistanceThreshold = Integer.valueOf(tmpStr).intValue();
        } else {
            textHammingDistanceThreshold.requestFocus();
            textHammingDistanceThreshold.selectAll();

            return false;
        }
        
        tmpStr = textOctaves.getText();

        if (testParameter(tmpStr, 1, 8)) {
            octaves = Integer.valueOf(tmpStr).intValue();
        } else {
            textOctaves.requestFocus();
            textOctaves.selectAll();

            return false;
        }
        
        rotationInvariant = rotationCheckBox.isSelected();
        
        scaleInvariant = scaleCheckBox.isSelected();
        

        return true;

    }

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms BRISK");
            }

            public String getDescription() {
                return new String("Applies Binary Robust Invariant Scalable Keypoints.");
            }

            public String getDescriptionLong() {
                return new String("Applies Binary Robust Invariant Scalable Keypoints.");
            }

            public String getShortLabel() {
                return new String("BRISK");
            }

            public String getLabel() {
                return new String("BRISK");
            }

            public String getName() {
                return new String("BRISK");
            }
        };
    }


    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
    public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();




        
        try {
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }


    /**
     * Returns a table listing the output parameters of this algorithm (usually just labels used to obtain output image
     * names later).
     * 
     * @return A parameter table listing the outputs of this algorithm.
     */
    public ParameterTable createOutputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }


    

    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        return isComplete();
    }
    
    /**
     * Accessor that returns the image.
     *
     * @return  the result image
     */
    public ModelImage getResultImage() {
        return destImage[0];
    }

    

    /**
     * Returns the name of an image output by this algorithm, the image returned depends on the parameter label given
     * (which can be used to retrieve the image object from the image registry).
     * 
     * @param imageParamName The output image parameter label for which to get the image name.
     * @return The image name of the requested output image parameter label.
     */
    public String getOutputImageName(final String imageParamName) {
        if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE)) {
            if (getResultImage() != null) {
                // algo produced a new result image
                return getResultImage().getImageName();
            } else {
                // algo was done in place
                return image.getImageName();
            }
        }

        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);

        return null;
    }



}
