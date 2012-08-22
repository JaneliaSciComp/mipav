package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import javax.swing.*;


/**
 * Extracts a surface using Cube Extraction. Triangle decimation can be invoked to reduce triangle count. The decimation
 * algorithm produces a continious level of detail (clod) structure that can be used to optimize the the visualization
 * of the surface. The input to this algorithm is typically a mask image where 0 = background and 100 = object (i.e.
 * interior to a VOI). The mask image is then blurred slightly and the level (50) is extracted. A greyscale image may
 * also be input and a surface is extracted given a level. The steps are:
 *
 * <ol>
 *   <li>Build mask image of VOI (i.e. all point interior to VOI are set to 100. All points exterior are = 0.</li>
 *   <li>Blur mask image if not greyscale</li>
 *   <li>Extract level surface at 50 or user defined level</li>
 *   <li>Save surface ( ".sur")</li>
 *   <li>If decimate then decimate surface and save (".sur")</li>
 * </ol>
 *
 * @version  0.1 June, 2001
 * @author   Matthew J. McAuliffe, Ph.D.
 * @author   David H. Eberly, Ph.D. wrote all the extraction and decimation code found in the SurfaceExtration,
 *           SurfaceDecimation and associated classes.
 * @see      AlgorithmExtractSurface
 * @see      ModelSurfaceExtractor
 * @see      ModelSurfaceDecimator
 */
public class JDialogExtractSurfaceCubes extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7919553320035488503L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JCheckBox blurCheck;

    /** DOCUMENT ME! */
    private boolean blurFlag;

    /** DOCUMENT ME! */
    private JTextField blurTF;

    /** DOCUMENT ME! */
    private float blurValue = 0.5f;

    /** DOCUMENT ME! */
    private boolean decimateFlag;

    /** DOCUMENT ME! */
    private JCheckBox decimateSurfaceCB;

    /** DOCUMENT ME! */
    private AlgorithmExtractSurfaceCubes extractSurAlgo;

    /** DOCUMENT ME! */
    private JButton fileButton;

    /** DOCUMENT ME! */
    private String fileName = null;

    /** DOCUMENT ME! */
    private JTextField fileTF;

    /** surface file format list. */
    private JComboBox fileTypeList;

    /** surface file formats. */
    private String[] fileTypes = { "Text files (*.txt)", "Surface files (*.sur)", "VRML files (*.wrl)", 
    		                       "XML files (*.xml)",  "VTK Legacy files (*.vtk)",  "VTK XML files (*.vtp)", 
    		                       "STereoLithography(STL) ASCII files (*.stl)",
    		                       "Polygon File Format (*.ply)", 
    		                       "Gifti Surface Format (*.gii)"};

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JRadioButton intensityLevelRB;

    /** DOCUMENT ME! */
    private JTextField intensityTF;

    /** DOCUMENT ME! */
    private int level = 0;

    /** DOCUMENT ME! */
    private JRadioButton maskImageRB;

    /** DOCUMENT ME! */
    private int mode;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private JRadioButton VOIRegionsRB;

    /** Flag indicates if this object should dispose the input image. */
    private boolean disposeImage = false;
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (during script execution).
     */
    public JDialogExtractSurfaceCubes() { }
    
    /**
     * Create a dialog to set variables to extract surface.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogExtractSurfaceCubes(JFrame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }
    
    
    /**
     * Creates a subset of the extract surface dialog. The surface is extracted from VOIs, so the option is set as default.
     * @param theParentFrame
     * @param im
     * @param bDisposeImage
     */
    public JDialogExtractSurfaceCubes(JFrame theParentFrame, ModelImage im, boolean bDisposeImage) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        initSubset();
        disposeImage = bDisposeImage;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        String fileLocation;


        if (command.equals("File")) {

            try {
                JFileChooser chooser = new JFileChooser();

                if (userInterface.getDefaultDirectory() != null) {
                    chooser.setCurrentDirectory(new File(userInterface.getDefaultDirectory()));
                } else {
                    chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
                }

                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.FREESURFER));
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.SURFACE));

                int returnVal = chooser.showSaveDialog(this);

                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    fileName = chooser.getSelectedFile().getName();
                    fileTF.setText(fileName);

                    if (userInterface.getDefaultDirectory() != null) {
                        fileLocation = userInterface.getDefaultDirectory();
                    } else {
                        fileLocation = System.getProperties().getProperty("user.dir");
                    }

                    fileLocation += fileName;
                    fileTF.setToolTipText(fileLocation);

                    userInterface.setDefaultDirectory("" + chooser.getCurrentDirectory() + File.separatorChar);
                    chooser.setVisible(false);
                }

                return;
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory!");

                return;
            }
        } else if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            if ( disposeImage )
            {
                image.disposeLocal();
            }
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("19001");
            MipavUtil.showWebHelp("Extract_Surface_(Marching_Cubes)#Applying_the_Extract_Surface_Algorithm");
        }else if (command.equals("chooseType")) {
            int pos = fileTypeList.getSelectedIndex();

            if (pos == 0) {
                fileName = makeImageName(fileTF.getText(), ".txt");
                decimateSurfaceCB.setSelected(false);
                decimateSurfaceCB.setEnabled(false);
            } else if (pos == 1) {
                fileName = makeImageName(fileTF.getText(), ".sur");
                if(!decimateSurfaceCB.isEnabled()) {
                	decimateSurfaceCB.setEnabled(true);
                }
            } else if (pos == 2) {
                fileName = makeImageName(fileTF.getText(), ".wrl");
                decimateSurfaceCB.setSelected(false);
                decimateSurfaceCB.setEnabled(false);
            } else if (pos == 3) {
                fileName = makeImageName(fileTF.getText(), ".xml");
                decimateSurfaceCB.setSelected(false);
                decimateSurfaceCB.setEnabled(false);
            } else if (pos == 4) {
                fileName = makeImageName(fileTF.getText(), ".vtk");
                decimateSurfaceCB.setSelected(false);
                decimateSurfaceCB.setEnabled(false);  
            } else if (pos == 5) {
                fileName = makeImageName(fileTF.getText(), ".vtp");
                decimateSurfaceCB.setSelected(false);
                decimateSurfaceCB.setEnabled(false);  
            } else if (pos == 6) {
                fileName = makeImageName(fileTF.getText(), ".stl");
                decimateSurfaceCB.setSelected(false);
                decimateSurfaceCB.setEnabled(false);  
            } else if (pos == 7) {
                fileName = makeImageName(fileTF.getText(), ".ply");
                decimateSurfaceCB.setSelected(false);
                decimateSurfaceCB.setEnabled(false);  
            } else if (pos == 8) {
                fileName = makeImageName(fileTF.getText(), ".gii");
                decimateSurfaceCB.setSelected(false);
                decimateSurfaceCB.setEnabled(false);  
            }


            fileTF.setText(fileName);

            if (userInterface.getDefaultDirectory() != null) {
                fileLocation = userInterface.getDefaultDirectory();
            } else {
                fileLocation = System.getProperties().getProperty("user.dir");
            }

            fileLocation += fileName;
            fileTF.setToolTipText(fileLocation);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
        if (algorithm instanceof AlgorithmExtractSurface) {
            image.clearMask();
            System.gc();
        }

        if (extractSurAlgo.isCompleted() == true) {
            attachSurface(image);

            insertScriptLine();
        }

        // Fix VOI IDs
        // int     nVOI  = 0;
        // short   oldID = 0;
        // int i;
        // ViewVOIVector VOIs = image.getVOIs();
        // if ( VOIs != null ) nVOI = VOIs.size();

        // for (i = 0; i < nVOI; i++) {
        // if( VOIs.VOIAt(i).isActive() == true &&
        // VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR){
        // VOI IDs start at 0 therefore ensure VOI ID is > 0
        // oldID = VOIs.VOIAt(i).getID();
        // VOIs.VOIAt(i).setID( (short)(oldID - 1));
        // break;
        // }
        // }

        if ( disposeImage )
        {
            image.disposeLocal();
        }
        dispose();
    }
    
    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        
        if (mode == AlgorithmExtractSurface.VOI_MODE) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("extraction_type", "VOI"));
        } else if (mode == AlgorithmExtractSurface.MASK_MODE) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("extraction_type", "MASK"));
        } else {
            scriptParameters.getParams().put(ParameterFactory.newParameter("extraction_type", "LEVEL"));
            scriptParameters.getParams().put(ParameterFactory.newParameter("extraction_level", level));
        }
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_decimate", decimateFlag));
        scriptParameters.getParams().put(ParameterFactory.newParameter("file_name", fileName));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_blur_before_extraction", blurFlag));
        scriptParameters.getParams().put(ParameterFactory.newParameter("blur_std_dev", blurValue));
    }
    
    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();
        
        String extractionModeStr = scriptParameters.getParams().getString("extraction_type");
        if (extractionModeStr.equalsIgnoreCase("VOI")) {
            setMode(AlgorithmExtractSurface.VOI_MODE);
        } else if (extractionModeStr.equalsIgnoreCase("MASK")) {
            setMode(AlgorithmExtractSurface.MASK_MODE);
            // from setVariables()...it may not be needed...
            setLevel(50);
        } else if (extractionModeStr.equalsIgnoreCase("LEVEL")) {
            setMode(AlgorithmExtractSurface.LEVEL_MODE);
            setLevel(scriptParameters.getParams().getInt("extraction_level"));
        } else {
            throw new ParameterException("extraction_type", "Unrecognized extraction type.  Select either VOI, MASK, or LEVEL.");
        }
        
        setDecimationFlag(scriptParameters.getParams().getBoolean("do_decimate"));
        setFileName(scriptParameters.getParams().getString("file_name"));
        setBlurFlag(scriptParameters.getParams().getBoolean("do_blur_before_extraction"));
        setBlurValue(scriptParameters.getParams().getFloat("blur_std_dev"));
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Sets text field enabled or disabled depending on source.
     *
     * @param  event  Event that triggered this method.
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (source == intensityLevelRB) {
            intensityTF.setEnabled(true);
        } else {
            intensityTF.setEnabled(false);
        }

        if (source == blurCheck) {
            blurTF.setEnabled(blurCheck.isSelected());
        }
    }

    /**
     * Accessor that sets the blurring flag (if the surface is generated from a VOI or mask the surface image will need
     * to be blurred by the Extraction algo before the surface mesh is extracted. Typically not required in the level
     * surface of grayscale image is needed.
     *
     * @param  flag  true indicates that the surface image should be blurred
     */
    public void setBlurFlag(boolean flag) {
        blurFlag = flag;
    }

    /**
     * Accessor that sets the blurring amount.
     *
     * @param  sigma  the amount of blurring (std. Dev. [0.5 - 5.0]
     */
    public void setBlurValue(float sigma) {
        blurValue = sigma;
    }

    /**
     * Accessor that sets the decimation flag.
     *
     * @param  flag  true indicates that the surface triangle mesh should be decimated
     */
    public void setDecimationFlag(boolean flag) {
        decimateFlag = flag;
    }

    /**
     * Accessor that sets the file name. Only the file name and should end in ".sur"
     *
     * @param  name  name of the file where the surface is to be saved.
     */
    public void setFileName(String name) {
        fileName = name;
    }

    /**
     * Accessor that sets the image where the surface is to be extracted.
     *
     * @param  im  the image (3D image)
     */
    public void setImage(ModelImage im) {
        image = im;
    }

    /**
     * Accessor that sets the intensity level that defines the surface that is to be extracted.
     *
     * @param  intenLevel  defines the level surface
     */
    public void setLevel(int intenLevel) {
        level = intenLevel;
    }

    /**
     * Accessor that sets the mode (VOI_MODE, MASK_MODE, or LEVEL_MODE).
     *
     * @param  mode  Mode to set to.
     */
    public void setMode(int mode) {
        this.mode = mode;
    }

    /**
     * Attach the generated surface to an image.
     *
     * @param  img  the image to attach the surface to
     */
    private void attachSurface(ModelImage img) {

        if (img.getFileInfo()[0] instanceof FileInfoXML) {

            for (int i = 0; i < img.getExtents()[2]; i++) {

                // attach the generated surface to the current image
                ((FileInfoImageXML) img.getFileInfo()[i]).addSurface(userInterface.getDefaultDirectory() +
                                                                     File.separator + fileName);
                ((FileInfoImageXML) img.getFileInfo()[i]).getSurface(userInterface.getDefaultDirectory() +
                                                                     File.separator + fileName).setDisplay(true);
            }
        }
    }

    /**
     * Once all the necessary variables are set, call the Extract Surface algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        if (image.getNDims() == 3) {

            // Make algorithm
            extractSurAlgo = new AlgorithmExtractSurfaceCubes(image, level, mode,
                                                              decimateFlag, blurFlag, blurValue, fileName);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            extractSurAlgo.addListener(this);

            createProgressBar(image.getImageName(), extractSurAlgo);
            
            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast
                if (extractSurAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                extractSurAlgo.run();
            }
        }
    }

    /**
     * Initializes the GUI by creating the components, placing them in the dialog, and displaying them.
     */
    private void init() {
        String fileLocation;
        setForeground(Color.black);
        setTitle("Extract Surface");

        JPanel imageVOIPanel = new JPanel(new GridBagLayout());

        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("Extract surface of:"));

        ButtonGroup imageVOIGroup = new ButtonGroup();

        maskImageRB = new JRadioButton("Mask image", false);
        maskImageRB.setFont(serif12);
        imageVOIGroup.add(maskImageRB);
        maskImageRB.addItemListener(this);

        VOIRegionsRB = new JRadioButton("VOI region", true);
        VOIRegionsRB.setFont(serif12);
        imageVOIGroup.add(VOIRegionsRB);
        VOIRegionsRB.addItemListener(this);

        intensityLevelRB = new JRadioButton("Intensity level", false);
        intensityLevelRB.setFont(serif12);
        imageVOIGroup.add(intensityLevelRB);
        intensityLevelRB.addItemListener(this);

        intensityTF = new JTextField();
        intensityTF.setText("50");
        intensityTF.setEnabled(false);
        intensityTF.setFont(serif12);

       
        blurCheck = new JCheckBox("Blur by (Std. Dev)", false);
        blurCheck.setForeground(Color.black);
        blurCheck.setFont(serif12);
        blurCheck.addItemListener(this);

        blurTF = new JTextField();
        blurTF.setText("0.5");
        blurTF.setFont(serif12);
        blurTF.setEnabled(false);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 2;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;

        gbc.gridx = 0;
        gbc.gridy = 0;
        imageVOIPanel.add(VOIRegionsRB, gbc);
        gbc.gridy = 1;
        imageVOIPanel.add(maskImageRB, gbc);
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        imageVOIPanel.add(intensityLevelRB, gbc);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        imageVOIPanel.add(intensityTF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        imageVOIPanel.add(blurCheck, gbc);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        imageVOIPanel.add(blurTF, gbc);

        JPanel decimatePanel = new JPanel();

        decimatePanel.setForeground(Color.black);
        decimatePanel.setBorder(buildTitledBorder("Decimate surface"));

        decimateSurfaceCB = new JCheckBox("Decimate surface (Reduce triangle count)", false);
        decimateSurfaceCB.setFont(serif12);
        decimatePanel.add(decimateSurfaceCB);

        gbc = new GridBagConstraints();
        gbc.gridwidth = 2;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;

        JPanel filePanel = new JPanel(new GridBagLayout());

        filePanel.setForeground(Color.black);
        filePanel.setBorder(buildTitledBorder("Save surface"));

        fileButton = new JButton("Choose...");
        fileButton.setPreferredSize(MipavUtil.defaultButtonSize);
        fileButton.setFont(serif12B);
        fileButton.addActionListener(this);
        fileButton.setActionCommand("File");

        fileTF = new JTextField();
        fileTF.setText(makeImageName(image.getImageName()+ "_surface", ".sur"));

        if (userInterface.getDefaultDirectory() != null) {
            fileLocation = userInterface.getDefaultDirectory();
        } else {
            fileLocation = System.getProperties().getProperty("user.dir");
        }

        fileLocation += makeImageName(image.getImageName(), ".sur");

        fileTF.setToolTipText(fileLocation);
        fileTF.setFont(serif12);

        JLabel formatLabel = new JLabel(" ");
        formatLabel.setFont(serif12B);

        fileTypeList = new JComboBox(fileTypes);
        fileTypeList.setSelectedIndex(1);
        fileTypeList.setActionCommand("chooseType");
        fileTypeList.addActionListener(this);
        fileTypeList.setEditable(false);
        fileTypeList.setFont(serif12);
        fileTypeList.setAlignmentX(Component.LEFT_ALIGNMENT);
        fileTypeList.setBackground(Color.white);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        filePanel.add(fileButton, gbc);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        filePanel.add(fileTF, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.fill = GridBagConstraints.LINE_END;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        filePanel.add(formatLabel, gbc);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        filePanel.add(fileTypeList, gbc);

        JPanel mainPanel = new JPanel(new GridBagLayout());

        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add(imageVOIPanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(decimatePanel, gbc);
        gbc.gridy = 2;
        mainPanel.add(filePanel, gbc);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        //JPanel buttonPanel = new JPanel();

        //buildOKButton();
        //buttonPanel.add(OKButton);
        //buildCancelButton();
        //buttonPanel.add(cancelButton);

        getContentPane().add(mainPanel);
        //getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

    
    /**
     * Initializes the GUI by creating the components, placing them in the dialog, and displaying them.
     */
    private void initSubset() {
        String fileLocation;
        setForeground(Color.black);
        setTitle("Extract Surface");

        JPanel imageVOIPanel = new JPanel(new GridBagLayout());

        imageVOIPanel.setForeground(Color.black);
        imageVOIPanel.setBorder(buildTitledBorder("Extract surface of:"));

        ButtonGroup imageVOIGroup = new ButtonGroup();

        maskImageRB = new JRadioButton("Mask image", false);
        maskImageRB.setFont(serif12);
        imageVOIGroup.add(maskImageRB);
        maskImageRB.addItemListener(this);

        VOIRegionsRB = new JRadioButton("VOI region", false);
        VOIRegionsRB.setFont(serif12);
        imageVOIGroup.add(VOIRegionsRB);
        VOIRegionsRB.addItemListener(this);

        intensityLevelRB = new JRadioButton("Intensity level", true);
        intensityLevelRB.setFont(serif12);
        imageVOIGroup.add(intensityLevelRB);
        intensityLevelRB.addItemListener(this);

        intensityTF = new JTextField();
        intensityTF.setText("50");
        intensityTF.setEnabled(false);
        intensityTF.setFont(serif12);

       
        blurCheck = new JCheckBox("Blur by (Std. Dev)", false);
        blurCheck.setForeground(Color.black);
        blurCheck.setFont(serif12);
        blurCheck.addItemListener(this);

        blurTF = new JTextField();
        blurTF.setText("0.5");
        blurTF.setFont(serif12);
        blurTF.setEnabled(false);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 2;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;

        gbc.gridx = 0;
        gbc.gridy = 0;
        imageVOIPanel.add(VOIRegionsRB, gbc);
        gbc.gridy = 1;
        imageVOIPanel.add(maskImageRB, gbc);
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        imageVOIPanel.add(intensityLevelRB, gbc);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        imageVOIPanel.add(intensityTF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        imageVOIPanel.add(blurCheck, gbc);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        imageVOIPanel.add(blurTF, gbc);

        JPanel decimatePanel = new JPanel();

        decimatePanel.setForeground(Color.black);
        decimatePanel.setBorder(buildTitledBorder("Decimate surface"));

        decimateSurfaceCB = new JCheckBox("Decimate surface (Reduce triangle count)", false);
        decimateSurfaceCB.setFont(serif12);
        decimatePanel.add(decimateSurfaceCB);

        gbc = new GridBagConstraints();
        gbc.gridwidth = 2;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;

        JPanel filePanel = new JPanel(new GridBagLayout());

        filePanel.setForeground(Color.black);
        filePanel.setBorder(buildTitledBorder("Save surface"));

        fileButton = new JButton("Choose...");
        fileButton.setPreferredSize(MipavUtil.defaultButtonSize);
        fileButton.setFont(serif12B);
        fileButton.addActionListener(this);
        fileButton.setActionCommand("File");

        fileTF = new JTextField();
        fileTF.setText(makeImageName(image.getImageName()+ "_surface", ".sur"));

        if (userInterface.getDefaultDirectory() != null) {
            fileLocation = userInterface.getDefaultDirectory();
        } else {
            fileLocation = System.getProperties().getProperty("user.dir");
        }

        fileLocation += makeImageName(image.getImageName(), ".sur");

        fileTF.setToolTipText(fileLocation);
        fileTF.setFont(serif12);

        JLabel formatLabel = new JLabel(" ");
        formatLabel.setFont(serif12B);

        fileTypeList = new JComboBox(fileTypes);
        fileTypeList.setSelectedIndex(1);
        fileTypeList.setActionCommand("chooseType");
        fileTypeList.addActionListener(this);
        fileTypeList.setEditable(false);
        fileTypeList.setFont(serif12);
        fileTypeList.setAlignmentX(Component.LEFT_ALIGNMENT);
        fileTypeList.setBackground(Color.white);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        filePanel.add(fileButton, gbc);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        filePanel.add(fileTF, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.fill = GridBagConstraints.LINE_END;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        filePanel.add(formatLabel, gbc);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        filePanel.add(fileTypeList, gbc);

        JPanel mainPanel = new JPanel(new GridBagLayout());

        gbc.gridx = 0;
        gbc.gridy = 0;
        //mainPanel.add(imageVOIPanel, gbc);
        //gbc.gridy = 1;
        mainPanel.add(decimatePanel, gbc);
        gbc.gridy++;
        mainPanel.add(filePanel, gbc);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        //JPanel buttonPanel = new JPanel();

        //buildOKButton();
        //buttonPanel.add(OKButton);
        //buildCancelButton();
        //buttonPanel.add(cancelButton);

        getContentPane().add(mainPanel);
        //getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

    
    
    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        decimateFlag = decimateSurfaceCB.isSelected();
        blurFlag = blurCheck.isSelected();
        fileName = fileTF.getText();

        int idx = fileName.lastIndexOf('.');

        if (idx < 0) {
            int pos = fileTypeList.getSelectedIndex();

            if (pos == 0) {
                fileName = makeImageName(fileTF.getText(), ".txt");
            } else if (pos == 1) {
                fileName = makeImageName(fileTF.getText(), ".sur");
            } else if (pos == 2) {
                fileName = makeImageName(fileTF.getText(), ".wrl");
            } else if (pos == 3) {
                fileName = makeImageName(fileTF.getText(), ".xml");
            } else if (pos == 4) {
                fileName = makeImageName(fileTF.getText(), ".vtk");
            } else if (pos == 5) {
                fileName = makeImageName(fileTF.getText(), ".vtp");
            } else if (pos == 6) {
                fileName = makeImageName(fileTF.getText(), ".stl");
            } else if (pos == 7) {
            	fileName = makeImageName(fileTF.getText(), ".ply");
            } else if (pos == 8) {
            	fileName = makeImageName(fileTF.getText(), ".gii");
            } 
        }

        if (fileName.equals("")) {
            MipavUtil.displayError("Enter a file name.");
            fileTF.requestFocus();

            return false;
        }

        String tmpStr = blurTF.getText();

        if (testParameter(tmpStr, 0.5f, 5.0f)) {
            blurValue = Float.valueOf(tmpStr).floatValue();
        } else {
            intensityTF.requestFocus();
            intensityTF.selectAll();

            return false;
        }

        if (VOIRegionsRB.isSelected()) {
            mode = AlgorithmExtractSurface.VOI_MODE;

            int i;
            ViewVOIVector VOIs = image.getVOIs();
            int nVOI;

            nVOI = VOIs.size();

            if (nVOI == 0) {
                MipavUtil.displayError("Must create a contour VOI");

                return false;
            }

            //short oldID = 0;
            int contourVOI = 0;
            int activeContourVOI = 0;

            for (i = 0; i < nVOI; i++) {

                if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                    contourVOI++;

                    if (VOIs.VOIAt(i).isActive() == true) {
                        activeContourVOI++;
                        // VOI IDs start at 0 therefore ensure VOI ID is > 0
                        // oldID = VOIs.VOIAt(i).getID();
                        // VOIs.VOIAt(i).setID( (short)(oldID + 1));
                    }
                }
            }

            if (contourVOI == 0) {
                MipavUtil.displayError("Must create a contour VOI");

                return false;
            }

            if ((contourVOI > 1) && (activeContourVOI != 1)) {
                MipavUtil.displayError("VOI must be selected");

                return false;
            }

            level = 50;
        } else if (maskImageRB.isSelected()) {
            mode = AlgorithmExtractSurface.MASK_MODE;
            level = 50;
        } else {
            mode = AlgorithmExtractSurface.LEVEL_MODE;

            String levelStr = intensityTF.getText();

            if (testParameter(levelStr, image.getMin(), image.getMax())) {
                level = Integer.valueOf(levelStr).intValue();
            } else {
                intensityTF.requestFocus();
                intensityTF.selectAll();

                return false;
            }
        }

        return true;
    }
}
