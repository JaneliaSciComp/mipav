package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;
import java.util.LinkedList;

import javax.swing.*;
import javax.swing.event.*;


/**
 * Dialog to get user input, then call the algorithm.
 */
public class JDialogFindRtableObject extends JDialogScriptableBase implements AlgorithmInterface, ListSelectionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 0L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JButton chooserButton;

    /** DOCUMENT ME! */
    private AlgorithmFindRtableObject findAlgo;

    /** DOCUMENT ME! */
    private JList fileList;

    /** DOCUMENT ME! */
    private JPanel filePanel;

    /** DOCUMENT ME! */
    private DefaultListModel model;

    /** DOCUMENT ME! */
    private JPanel paramPanel;

    /** DOCUMENT ME! */
    private JButton removeButton;

    private ModelImage srcImage;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;
    
    /** For each omega angle index, a linked list of R and B values is present
     *  omega is the gradient angle to the curve, with the gradient defined as
     *  going into the object, and R and B give the distance
     *  and angle from the center of the VOI to the tangent point */
    private LinkedList omegaRBetaList[] = null;
    
    /** Number of bins for gradient angle omega going from 0 to 2*PI */
    private int omegaBins;
    
    /** Number of points to take from each side of a point on a curve in determining a tangent */
    private int sidePointsForTangent;
    
    /** Desired maximum pixel bin width for x, y center value */
    private float maxPixelBinWidth;
    
    private JTextField pixelWidthText;
    
    /** The maximum Hough transform size in megabytes - default is currently 256 */
    private int maxBufferSize;
    
    private JTextField maxBufferText;
    
    private JCheckBox rotationCheckBox;
    
    private JLabel degreesWidthLabel;
    
    private JTextField degreesWidthText;
    
    /** If true, allow rotation of R-table object */
    private boolean allowRotation = true;
    
    /** Desired maximum rotation angle. */
    private float maxDegreesBinWidth = 1.0f;
    
    /** If true, allow scaling of R-table object */
    private boolean allowScaling = true;
    
    private JCheckBox scalingCheckBox;
    
    private float minScaleFactor = 0.1f;
    
    private JLabel minScaleLabel;
    
    private JTextField minScaleText;
    
    private float maxScaleFactor = 10.0f;
    
    private JLabel maxScaleLabel;
    
    private JTextField maxScaleText;
    
    /** Desired number of scaling bins */
    private int scaleBins = 20;
    
    private JLabel scalingLabel;
    
    private JTextField scalingText;
    
    /** Number of instances of R-table of to find in the image */
    private int objectsToFind = 1;
    
    /** Minimum pixel distances between instances of R-table objects */
    private JTextField numObjectsText;
    
    /** File containing omegaBins, sidePointsForTangent, and omegaRBetaList, the R-table */
    private String parametersFile = null;
    
    private boolean endianess = true;
    
    private RandomAccessFile raFile;
    
    //private float creationCenterX;
    
    //private float creationCenterY;
    
    //private int creationXDim;
    
    //private int creationNumPoints;
    
    //private int creationIndexArray[] = null;
    
    //private float creationOmegaArray[] = null;
    
    //private float creationCurvatureArray[] = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogFindRtableObject() { }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  im  Source image.
     */
    public JDialogFindRtableObject(ModelImage im) {
        super();
        userInterface = ViewUserInterface.getReference();
        srcImage = im;
        parentFrame = srcImage.getParentFrame();
    }


    /**
     * Creates a new JDialogFindRTableObject object.
     *
     * @param  theParentFrame  parent frame
     * @param  im              source image
     */
    public JDialogFindRtableObject(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        srcImage = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------
    
    /**
     * Closes dialog box when the OK button is pressed, sets the variables, and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        int i, j;
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Choose")) {
            parametersFileMenu();
            if (parametersFile != null) {
                model.addElement(parametersFile);
                removeButton.setEnabled(true);
                chooserButton.setEnabled(false);
            }
        } else if (command.equals("Remove")) {
            model.removeElement(parametersFile);
            parametersFile = null;
            chooserButton.setEnabled(true);
            removeButton.setEnabled(false);
            for (i = 0; i < omegaRBetaList.length; i++) {
                for (j = 0; j < omegaRBetaList[i].size(); j++) {
                    omegaRBetaList[i].clear();
                }
                omegaRBetaList[i] = null;
            }
            omegaRBetaList = null;
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        }
    }
    
    /**
     * Allows the user to select parameters file.
     *
     * @return  fileName
     */
    public void parametersFileMenu() {
        String directory;
        JFileChooser chooser;
        ViewUserInterface UI = ViewUserInterface.getReference();

        // bring up file dialog
        try {
            chooser = new JFileChooser();

            if (UI.getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(UI.getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.RTABLE));

            int returnVal = chooser.showOpenDialog(UI.getMainFrame());

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                parametersFile = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                UI.setDefaultDirectory(directory);
            } else {
                parametersFile =  null;
                return;
            }
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: JDialogFindRtableObject.parametersFileMenu");

            parametersFile = null;
            return;
        }

        readTransformParameters();

        return;
    }
    
    /**
     * Reads parameters from a file.
     *
     * @param  fileName  name of the R-table file.
     */
    public void readTransformParameters() {
        int i, j;
        int itemsAtIndex;
        float floatArray[];

        if (parametersFile == null) {
            MipavUtil.displayError("parametersFile = null");
        }

        try {
            File file = new File(userInterface.getDefaultDirectory() + parametersFile);
            raFile = new RandomAccessFile(file, "r");
            omegaBins = getInt(endianess);
            omegaRBetaList = new LinkedList[omegaBins];
            for (i = 0; i < omegaBins; i++) {
                omegaRBetaList[i] = new LinkedList();
            }
            sidePointsForTangent = getInt(endianess);
            for (i = 0; i < omegaBins; i++) {
                itemsAtIndex = getInt(endianess);
                for (j = 0; j < itemsAtIndex; j++) {
                    floatArray = new float[2];
                    floatArray[0] = getFloat(endianess); // r
                    floatArray[1] = getFloat(endianess); // beta
                    omegaRBetaList[i].add(floatArray);
                }
            }
            /*creationCenterX = getFloat(endianess);
            creationCenterY = getFloat(endianess);
            sidePointsForTangent = getInt(endianess);
            creationXDim = getInt(endianess);
            creationNumPoints = getInt(endianess);
            creationIndexArray = new int[creationNumPoints];
            creationOmegaArray = new float[creationNumPoints];
            creationCurvatureArray = new float[creationNumPoints];
            for (i = 0; i < creationNumPoints; i++) {
                creationIndexArray[i] = getInt(endianess);
                creationOmegaArray[i] = getFloat(endianess);
                creationCurvatureArray[i] = getFloat(endianess);
            }*/

            raFile.close();

        } catch (IOException error) {
            MipavUtil.displayError("Parameters read error");
            parametersFile = null;
        }
    }
    
    /**
     * Reads four unsigned bytes from file.
     *
     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the float read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    private final float getFloat(boolean endianess) throws IOException {
        int b1, b2, b3, b4;
        b1 = raFile.readUnsignedByte();
        b2 = raFile.readUnsignedByte();
        b3 = raFile.readUnsignedByte();
        b4 = raFile.readUnsignedByte();

        int tmpInt;

        if (endianess == true) {
            tmpInt = ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4);

            return (Float.intBitsToFloat(tmpInt));
        } else {
            tmpInt = ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1);

            return (Float.intBitsToFloat(tmpInt));
        }
    }

    /**
     * Reads four unsigned bytes from file.
     *
     * @param      endianess  <code>true</code> indicates big endian byte order, <code>false</code> indicates little
     *                        endian.
     *
     * @return     The value of the integer read from the file.
     *
     * @exception  IOException  if there is an error reading the file
     */
    private final int getInt(boolean endianess) throws IOException {
        int b1, b2, b3, b4;
        b1 = raFile.readUnsignedByte();
        b2 = raFile.readUnsignedByte();
        b3 = raFile.readUnsignedByte();
        b4 = raFile.readUnsignedByte();

        if (endianess == true) {
            return ((b1 << 24) | (b2 << 16) | (b3 << 8) | b4); // Big Endian
        } else {
            return ((b4 << 24) | (b3 << 16) | (b2 << 8) | b1); // Little Endian
        }
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

        if (algorithm instanceof AlgorithmFindRtableObject) {
            

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }

            dispose();
        }
    }

    /**
     * Accessor that sets the source image.
     *
     * @param  image  new source image.
     */
    public void setSourceImage(ModelImage image) {
        srcImage = image;
    }
    
    /**
     * 
     * @param parametersFile file with R-table
     */
    public void setParametersFile(String parametersFile) {
        this.parametersFile = parametersFile;
    }
    
    /**
     * 
     * @param maxPixelBinWidth Desired maximum pixel bin width for x, y center value
     */
    public void setMaxPixelBinWidth(float maxPixelBinWidth) {
        this.maxPixelBinWidth = maxPixelBinWidth;
    }
    
    /**
     * 
     * @param maxBufferSize  The maximum Hough transform size in megabytes - default is currently 256 
     */
    public void setMaxBufferSize(int maxBufferSize) {
        this.maxBufferSize = maxBufferSize;
    }
    
    /**
     * 
     * @param allowRotation If true, rotate tested objects
     */
    public void setAllowRotation(boolean allowRotation) {
        this.allowRotation = allowRotation;
    }
    
    /**
     * 
     * @param maxDegreesBinWidth Desired maximum rotation angle
     */
    public void setMaxDegreesBinWidth(float maxDegreesBinWidth) {
        this.maxDegreesBinWidth = maxDegreesBinWidth;
    }
    
    /**
     * 
     * @param allowScaling If true, scale tested objects
     */
    public void setAllowScaling(boolean allowScaling) {
        this.allowScaling = allowScaling;
    }
    
    /**
     * 
     * @param minScaleFactor
     */
    public void setMinScaleFactor(float minScaleFactor) {
        this.minScaleFactor = minScaleFactor;
    }
    
    /**
     * 
     * @param maxScaleFactor
     */
    public void setMaxScaleFactor(float maxScaleFactor) {
        this.maxScaleFactor = maxScaleFactor;
    }
    
    /**
     * 
     * @param scaleBins
     */
    public void setScalingBins(int scaleBins) {
        this.scaleBins = scaleBins;
    }
    
    /**
     * 
     * @param objectsToFind number of instances of R-table object to find in image
     */
    public void setObjectsToFind(int objectsToFind) {
        this.objectsToFind = objectsToFind;
    }

    /**
     * Sets the remove index based on the selected index in the list.
     *
     * @param  evt  Event that caused this method to fire.
     */
    public void valueChanged(ListSelectionEvent evt) { }

    /**
     * Once all the necessary variables are set, call AlgorithmFindRtableObject.
     */
    protected void callAlgorithm() {
        System.gc();

        try {

            findAlgo = new AlgorithmFindRtableObject(srcImage, omegaBins, sidePointsForTangent,
                           omegaRBetaList, maxPixelBinWidth, maxBufferSize, 
                           allowRotation, maxDegreesBinWidth, allowScaling,
                           minScaleFactor, maxScaleFactor, scaleBins, objectsToFind);
            
            /*findAlgo = new AlgorithmFindRtableObject(srcImage, 
                    creationCenterX, creationCenterY, sidePointsForTangent, creationXDim,
                    creationIndexArray, creationOmegaArray, creationCurvatureArray, maxPixelBinWidth,
                    allowRotation, maxDegreesBinWidth, allowScaling,
                    minScaleFactor, maxScaleFactor, scaleBins, objectsToFind);*/

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            findAlgo.addListener(this);

            createProgressBar(srcImage.getImageName(), findAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (findAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                findAlgo.run();
            }
        } catch (OutOfMemoryError x) {


            System.gc();
            MipavUtil.displayError("Dialog Find R-table Object: unable to allocate enough memory");

            return;
        }

    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        srcImage = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = srcImage.getParentFrame();

        setParametersFile(scriptParameters.getParams().getString("params_file"));
        readTransformParameters();
        setMaxPixelBinWidth(scriptParameters.getParams().getFloat("max_pixel_bin_width"));
        setMaxBufferSize(scriptParameters.getParams().getInt("max_buffer_size"));
        setAllowRotation(scriptParameters.getParams().getBoolean("allow_rotation"));
        setMaxDegreesBinWidth(scriptParameters.getParams().getFloat("max_degrees_bin_width"));
        setAllowScaling(scriptParameters.getParams().getBoolean("allow_scaling"));
        setMinScaleFactor(scriptParameters.getParams().getFloat("min_scale_factor"));
        setMaxScaleFactor(scriptParameters.getParams().getFloat("max_scale_factor"));
        setScalingBins(scriptParameters.getParams().getInt("scaling_bins"));
        setObjectsToFind(scriptParameters.getParams().getInt("objects_to_find"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(srcImage);

        scriptParameters.getParams().put(ParameterFactory.newParameter("params_file", parametersFile));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_pixel_bin_width", maxPixelBinWidth));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_buffer_size", maxBufferSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("allow_rotation", allowRotation));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_degrees_bin_width", maxDegreesBinWidth));
        scriptParameters.getParams().put(ParameterFactory.newParameter("allow_scaling", allowScaling));
        scriptParameters.getParams().put(ParameterFactory.newParameter("min_scale_factor", minScaleFactor));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_scale_factor", maxScaleFactor));
        scriptParameters.getParams().put(ParameterFactory.newParameter("scaling_bins", scaleBins));
        scriptParameters.getParams().put(ParameterFactory.newParameter("objects_to_find", objectsToFind));
    }


    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        int yPos = 0;
        JPanel essentialPanel;
        JPanel scalingPanel;
        JPanel rotationPanel;
        JPanel numObjectsPanel;
        JLabel pixelWidthLabel;
        JLabel maxBufferLabel;
        JLabel numObjectsLabel;
        setForeground(Color.black);

        setTitle("Find R-table object");

        GridBagConstraints gbc4 = new GridBagConstraints();
        gbc4.gridwidth = 1;
        gbc4.gridheight = 1;
        gbc4.anchor = GridBagConstraints.WEST;
        gbc4.weightx = 1;
        gbc4.insets = new Insets(3, 3, 3, 3);
        gbc4.fill = GridBagConstraints.HORIZONTAL;
        gbc4.gridx = 0;
        gbc4.gridy = 0;
        
        essentialPanel = new JPanel(new GridBagLayout());
        essentialPanel.setBorder(buildTitledBorder("Essential parameters"));
        
        pixelWidthLabel = new JLabel("Desired maximum pixel bin width for x, y center value ");
        pixelWidthLabel.setForeground(Color.black);
        pixelWidthLabel.setFont(serif12);
        pixelWidthLabel.setEnabled(true);
        essentialPanel.add(pixelWidthLabel, gbc4);
        
        gbc4.gridx = 1;
        pixelWidthText = new JTextField(15);
        pixelWidthText.setText("2.0");
        pixelWidthText.setFont(serif12);
        pixelWidthText.setEnabled(true);
        essentialPanel.add(pixelWidthText, gbc4);
        
        gbc4.gridx = 0;
        gbc4.gridy = 1;
        maxBufferLabel = new JLabel("Maximum Hough transform in megabytes ");
        maxBufferLabel.setForeground(Color.black);
        maxBufferLabel.setFont(serif12);
        maxBufferLabel.setEnabled(true);
        essentialPanel.add(maxBufferLabel, gbc4);
        
        gbc4.gridx = 1;
        maxBufferText = new JTextField(15);
        maxBufferText.setText("256");
        maxBufferText.setFont(serif12);
        maxBufferText.setEnabled(true);
        essentialPanel.add(maxBufferText, gbc4);

        rotationPanel = new JPanel(new GridBagLayout());
        rotationPanel.setBorder(buildTitledBorder("Rotation"));

        gbc4.gridx = 0;
        gbc4.gridy = 0;
        rotationCheckBox = new JCheckBox("Allow rotation of R-table object");
        rotationCheckBox.setSelected(true);
        rotationCheckBox.setFont(serif12);
        rotationCheckBox.setForeground(Color.black);
        rotationCheckBox.addItemListener(this);
        rotationPanel.add(rotationCheckBox, gbc4);
        
        gbc4.gridy = 1;
        degreesWidthLabel = new JLabel("Desired maximum degrees difference for rotation angle ");
        degreesWidthLabel.setForeground(Color.black);
        degreesWidthLabel.setFont(serif12);
        degreesWidthLabel.setEnabled(true);
        rotationPanel.add(degreesWidthLabel, gbc4);
        
        gbc4.gridx = 1;
        degreesWidthText = new JTextField(15);
        degreesWidthText.setText("1.0");
        degreesWidthText.setFont(serif12);
        degreesWidthText.setEnabled(true);
        rotationPanel.add(degreesWidthText, gbc4);

        scalingPanel = new JPanel(new GridBagLayout());
        scalingPanel.setBorder(buildTitledBorder("Scaling"));

        gbc4.gridx = 0;
        gbc4.gridy = 0;
        scalingCheckBox = new JCheckBox("Allow scaling of R-table object");
        scalingCheckBox.setSelected(true);
        scalingCheckBox.setFont(serif12);
        scalingCheckBox.setForeground(Color.black);
        scalingCheckBox.addItemListener(this);
        scalingPanel.add(scalingCheckBox, gbc4);
        
        gbc4.gridy = 1;
        minScaleLabel = new JLabel("Minimum scaling factor");
        minScaleLabel.setEnabled(true);
        minScaleLabel.setFont(serif12);
        minScaleLabel.setForeground(Color.black);
        scalingPanel.add(minScaleLabel, gbc4);
        
        gbc4.gridx = 1;
        minScaleText = new JTextField(15);
        minScaleText.setEnabled(true);
        minScaleText.setText("0.1");
        minScaleText.setFont(serif12);
        minScaleText.setForeground(Color.black);
        scalingPanel.add(minScaleText, gbc4);
        
        gbc4.gridx = 0;
        gbc4.gridy = 2;
        maxScaleLabel = new JLabel("Maximum scaling factor");
        maxScaleLabel.setEnabled(true);
        maxScaleLabel.setFont(serif12);
        maxScaleLabel.setForeground(Color.black);
        scalingPanel.add(maxScaleLabel, gbc4);
        
        gbc4.gridx = 1;
        maxScaleText = new JTextField(15);
        maxScaleText.setEnabled(true);
        maxScaleText.setText("10.0");
        maxScaleText.setFont(serif12);
        maxScaleText.setForeground(Color.black);
        scalingPanel.add(maxScaleText, gbc4);
        
        gbc4.gridx = 0;
        gbc4.gridy = 3;
        scalingLabel = new JLabel("Desired number of scaling bins");
        scalingLabel.setEnabled(true);
        scalingLabel.setFont(serif12);
        scalingLabel.setForeground(Color.black);
        scalingPanel.add(scalingLabel, gbc4);
        
        gbc4.gridx = 1;
        scalingText = new JTextField(15);
        //scalingText.setText("21");
        scalingText.setText("201");
        scalingText.setEnabled(true);
        scalingText.setFont(serif12);
        scalingText.setForeground(Color.black);
        scalingPanel.add(scalingText, gbc4);
        
        numObjectsPanel = new JPanel(new GridBagLayout());
        numObjectsPanel.setBorder(buildTitledBorder("Object number"));

        gbc4.gridx = 0;
        gbc4.gridy = 0;
        numObjectsLabel = new JLabel("Number of objects to find in image");
        numObjectsLabel.setForeground(Color.black);
        numObjectsLabel.setFont(serif12);
        numObjectsPanel.add(numObjectsLabel, gbc4);
        
        gbc4.gridx = 1;
        numObjectsText = new JTextField(15);
        numObjectsText.setText("1");
        numObjectsText.setForeground(Color.black);
        numObjectsText.setFont(serif12);
        numObjectsPanel.add(numObjectsText, gbc4);

        paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.gridx = 0;
        gbc.gridy = yPos++;
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 1;
        gbc.gridwidth = 1;
        paramPanel.add(essentialPanel, gbc);
        gbc.gridy = yPos++;
        paramPanel.add(rotationPanel, gbc);
        gbc.gridy = yPos++;
        paramPanel.add(scalingPanel, gbc);
        gbc.gridy = yPos++;
        paramPanel.add(numObjectsPanel, gbc);
        
        
        filePanel = new JPanel(new BorderLayout());
        filePanel.setBorder(buildTitledBorder("Load R-table file"));

        model = new DefaultListModel();
        fileList = new JList(model);
        fileList.setVisibleRowCount(2);
        fileList.setPreferredSize(new Dimension(300, 60));
        fileList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        fileList.addListSelectionListener(this);
        filePanel.add(fileList);

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

        filePanel.add(chooserPanel, BorderLayout.SOUTH);

        gbc.gridx = 0;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.gridy = yPos++;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        paramPanel.add(filePanel, gbc);

        getContentPane().add(paramPanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }
    
    /**
     *
     * @param  event  ItemEvent the item change event that occured
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();
        if (source == rotationCheckBox) {
            if (rotationCheckBox.isSelected()) {
                degreesWidthLabel.setEnabled(true);
                degreesWidthText.setEnabled(true);
            }
            else {
                degreesWidthLabel.setEnabled(false);
                degreesWidthText.setEnabled(false);
            }
        }
        else if (source == scalingCheckBox) {
            if (scalingCheckBox.isSelected()) {
                minScaleLabel.setEnabled(true);
                minScaleText.setEnabled(true);
                maxScaleLabel.setEnabled(true);
                maxScaleText.setEnabled(true);
                scalingLabel.setEnabled(true);
                scalingText.setEnabled(true);
            }
            else {
                minScaleLabel.setEnabled(false);
                minScaleText.setEnabled(false);
                maxScaleLabel.setEnabled(false);
                maxScaleText.setEnabled(false);
                scalingLabel.setEnabled(false);
                scalingText.setEnabled(false);    
            }
        }
    }
    

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;
        
        tmpStr = pixelWidthText.getText();
        if (testParameter(tmpStr, 0.1, 20)) {
            maxPixelBinWidth = Float.valueOf(tmpStr).floatValue();
        } else {
            pixelWidthText.requestFocus();
            pixelWidthText.selectAll();
            return false;
        }
        
        if (!testParameter(maxBufferText.getText(), 1, 10000)) {
            maxBufferText.requestFocus();
            maxBufferText.selectAll();

            return false;
        } else {
            maxBufferSize = Integer.valueOf(maxBufferText.getText()).intValue();
        }

        allowRotation = rotationCheckBox.isSelected();
        if (allowRotation) {
            if (!testParameter(degreesWidthText.getText(), 0.1, 20.0)) {
                degreesWidthText.requestFocus();
                degreesWidthText.selectAll();

                return false;
            } else {
                maxDegreesBinWidth = Float.valueOf(degreesWidthText.getText()).floatValue();
            }
        } // if (allowRotation)
        
        allowScaling = scalingCheckBox.isSelected();
        if (allowScaling) {
            tmpStr = minScaleText.getText();
            if (testParameter(tmpStr, 0.01f, 100.0f)) {
                minScaleFactor = Float.valueOf(tmpStr).floatValue();
            }
            else {
                minScaleText.requestFocus();
                minScaleText.selectAll();
                return false;
            }
            
            tmpStr = maxScaleText.getText();
            if (testParameter(tmpStr, minScaleFactor, 100.0f)) {
                maxScaleFactor = Float.valueOf(tmpStr).floatValue();
            }
            else {
                maxScaleText.requestFocus();
                maxScaleText.selectAll();
                return false;
            }
            
            tmpStr = scalingText.getText();
            if (testParameter(tmpStr, 1, 5001)) {
                scaleBins = Integer.valueOf(tmpStr).intValue();
            }
            else {
                scalingText.requestFocus();
                scalingText.selectAll();
                return false;
            }
        } // if (allowScaling)
        
        tmpStr = numObjectsText.getText();
        if (testParameter(tmpStr, 1, 200)) {
            objectsToFind = Integer.valueOf(tmpStr).intValue();
        }
        else {
            numObjectsText.requestFocus();
            numObjectsText.selectAll();
            return false;
        }
        
        if (omegaRBetaList == null) {
            MipavUtil.displayError("R-table list has not been read");
            return false;
        }
        
        /*if (creationIndexArray == null) {
            MipavUtil.displayError("Hough transform creation file has not been read");
            return false;
        }*/
        return true;
    }

}
