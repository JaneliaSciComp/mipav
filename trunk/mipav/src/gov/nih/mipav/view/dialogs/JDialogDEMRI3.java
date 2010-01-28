package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.AlgorithmBilateralFilter;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.JPanelAlgorithmOutputOptions;
import gov.nih.mipav.view.components.JPanelSigmas;

import java.awt.*;
import java.awt.event.*;

import java.io.File;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input for 3 parameter dynamic (contrast) enhanced MRI model or DEMRI model
 */
public class JDialogDEMRI3 extends JDialogScriptableBase implements AlgorithmInterface, ItemListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    private JLabel labelParamsToFit;

    /** DOCUMENT ME! */
    private JLabel labelContrastRelaxivityRate;
    
    private JTextField textContrastRelaxivityRate;
    
    // contrast relaxivity rate
    double r1;
    
    double r1Min = 0.0;
    
    double r1Max = 1000.0;
    
    private JLabel labelBloodIntrinsicRelaxivityRate;
    
    private JLabel labelBloodIntrinsicRelaxivityRate2;
    
    private JTextField textBloodIntrinsicRelaxivityRate;
    
    // blood intrinsic relaxivity rate
    double rib;
    
    double ribMin = 0.001;
    
    double ribMax = 10000.0;
    
    private ButtonGroup tissueGroup;
    
    private boolean useConstantTissue;
    
    private JRadioButton constantTissueRadioButton;
    
    private JRadioButton fileTissueRadioButton;
    
    private JLabel labelTissueIntrinsicRelaxivityRate;
    
    private JLabel labelTissueIntrinsicRelaxivityRate2;
    
    private JTextField textTissueIntrinsicRelaxivityRate;
    
    // tissue intrinsic relaxivity rate
    double rit;
    
    double ritMin = 0.001;
    
    double ritMax = 10000.0;
    
    private JButton buttonLowFlipFile;
    
    private JTextField textLowFlipFile;
    
    private JButton buttonHighFlipFile;
    
    private JTextField textHighFlipFile;
    
    // non-uniform tissue intrinsic relaxivity map
    double r1i[] = null;
    
    private String directoryLowFlip;
    
    private String fileNameLowFlip;
    
    private ModelImage lowFlipImage = null;
    
    private double lowFlip[] = null;
    
    private String directoryHighFlip;
    
    private String fileNameHighFlip;
    
    private ModelImage highFlipImage = null;
    
    private double highFlip[] = null;
    
    private JLabel labelFlipAngle;
    
    private JTextField textFlipAngle;
    
    // flip angle in degrees
    double theta;
    
    double thetaMin = 0.0;
    
    double thetaMax = 90.0;
    
    private JLabel labelTimeBetweenShots;
    
    private JTextField textTimeBetweenShots;
    
    // time between shots in seconds
    double tr;
    
    double trMin = 0.0;
    
    double trMax = 10000.0;
    
    private ButtonGroup rateGroup;
    
    private JRadioButton secondButton;
    
    private JRadioButton minuteButton;
    
    boolean perMin = false;
    
    private JLabel labelMp;
    
    private JButton buttonMpFile;
    
    private JTextField textMpFile;
    
    private String directoryMp;
    
    private String fileNameMp;
    
    private File fileMp;
    
    // Contents of Mp(t) file
    private double mcp[] = null;
    
    private int nx;
    
    private int ny;
    
    private int mp_len;
    
    private JLabel labelNFirst;
    
    private JTextField textNFirst;
    
    private int nFirst;
    
    private int nFirstMin = 0;
    
    private int nFirstMax = 1000;
    
    private ButtonGroup secondParamGroup;
    
    private JRadioButton kepButton;
    
    private JRadioButton veButton;
    
    private boolean useVe = false;

    private ViewVOIVector VOIs;


    /** DOCUMENT ME! */
    private AlgorithmDEMRI3 demri3Algo = null;

    /** DOCUMENT ME! */
    private ModelImage image;
    
    private ModelImage resultImage;
    
    private JLabel labelMinConstr0;
    
    private JTextField textMinConstr0;
    
    private JLabel labelMaxConstr0;
    
    private JTextField textMaxConstr0;
    
    private JLabel labelMinConstr1;
    
    private JTextField textMinConstr1;
    
    private JLabel labelMaxConstr1;
    
    private JTextField textMaxConstr1;
    
    private JLabel labelMinConstr2;
    
    private JTextField textMinConstr2;
    
    private JLabel labelMaxConstr2;
    
    private JTextField textMaxConstr2;
    
    private double min_constr[] = new double[3];
    private double max_constr[] = new double[3];

    
    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogDEMRI3 object.
     *
     * @param  image  DOCUMENT ME!
     */
    public JDialogDEMRI3(ModelImage image) {
        super();
        this.UI = ViewUserInterface.getReference();
        this.image = image;
        parentFrame = image.getParentFrame();
    }

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogDEMRI3(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        UI = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        FileVOI fileVOI;
        VOI[] voi;
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
        } else if (command.equals("LowFlipFile")) {

                try {
                    JFileChooser chooser = new JFileChooser();

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

                    chooser.setDialogTitle("Open low flip angle 3D image file");
                    directoryLowFlip = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;

                    int returnValue = chooser.showOpenDialog(UI.getMainFrame());

                    if (returnValue == JFileChooser.APPROVE_OPTION) {
                        fileNameLowFlip = chooser.getSelectedFile().getName();
                        directoryLowFlip = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                        UI.setDefaultDirectory(directoryLowFlip);
                    } else {
                        fileNameLowFlip = null;

                        return;
                    }

                    if (fileNameLowFlip != null) {
                        textLowFlipFile.setText(fileNameLowFlip);
                    }
                } catch (OutOfMemoryError e) {
                    MipavUtil.displayError("Out of memory in JDialogDEMRI3.");

                    return;
                }
        } else if (command.equals("HighFlipFile")) {

            try {
                JFileChooser chooser = new JFileChooser();

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

                chooser.setDialogTitle("Open high flip angle 4D image file");
                directoryHighFlip = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;

                int returnValue = chooser.showOpenDialog(UI.getMainFrame());

                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    fileNameHighFlip = chooser.getSelectedFile().getName();
                    directoryHighFlip = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                    UI.setDefaultDirectory(directoryHighFlip);
                } else {
                    fileNameHighFlip = null;

                    return;
                }

                if (fileNameHighFlip != null) {
                    textHighFlipFile.setText(fileNameHighFlip);
                }
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in JDialogDEMRI3.");

                return;
            }
        } else if (command.equals("MpFile")) {

            try {
                JFileChooser chooser = new JFileChooser();

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

                chooser.setDialogTitle("Open sagittal sinus VOI file");
                directoryMp = String.valueOf(chooser.getCurrentDirectory()) + File.separator;

                int returnValue = chooser.showOpenDialog(UI.getMainFrame());

                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    fileNameMp = chooser.getSelectedFile().getName();
                    directoryMp = String.valueOf(chooser.getCurrentDirectory()) + File.separator;
                    UI.setDefaultDirectory(directoryMp);
                } else {
                    fileNameMp = null;

                    return;
                }

                if (fileNameMp != null) {

                    try {
                        fileVOI = new FileVOI(fileNameMp, directoryMp, image);
                    }
                    catch (IOException e) {
                        MipavUtil.displayError("IOException on new FileVOI(fileNameMp, directoryMp, image)");
                        return;
                    }

                    try {
                        voi = fileVOI.readVOI(false);
                    }
                    catch (IOException e) {
                        MipavUtil.displayError("IOException on fileVOI.readVOI(false)");
                        return;
                    }
                    
                    if (voi.length > 1) {
                        MipavUtil.displayError("Found " + voi.length + " vois in file instead of 1");
                        return;
                    }
                    
                    image.getParentFrame().getComponentImage().getVOIHandler().deleteVOIs();
                    image.registerVOI(voi[0]);

                    //  when everything's done, notify the image listeners
                    image.notifyImageDisplayListeners();   
                }

                
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in JDialogDEMRI3.");

                return;
            }
    }   else if (command.equals("Cancel")) {
            dispose();
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
    	if (Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && (this.getOwner() != null) && !isScriptRunning()) {
            saveDefaults();
        }

        if (algorithm instanceof AlgorithmDEMRI3) {
            Preferences.debug("DEMRI3 elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((demri3Algo.isCompleted() == true) && (resultImage != null)) {

                resultImage.clearMask();

                try {
                	openNewFrame(resultImage);
                 //   openNewFrame(resultImage);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();

            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        }
    }
    
    /**
     * Saves the default settings into the Preferences file.
     */
    public void saveDefaults() {
        String delim = ",";
        String defaultsString = min_constr[0] + delim;
        defaultsString += max_constr[0] + delim;
        defaultsString += min_constr[1] + delim;
        defaultsString += max_constr[1] + delim;
        defaultsString += min_constr[2] + delim;
        defaultsString += max_constr[2] + delim;
        defaultsString += r1 + delim;
        defaultsString += rib + delim;
        defaultsString += useConstantTissue + delim;
        if (useConstantTissue) {
            defaultsString += rit + delim;
        }
        else {
        	defaultsString += lowFlipImage.getImageFileName();
        	defaultsString += highFlipImage.getImageFileName();
        }
        defaultsString += theta + delim;
        defaultsString += tr + delim;
        defaultsString += perMin + delim;
        defaultsString += nFirst + delim;
        defaultsString += useVe;

        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
    }
    
    /**
     * Set up the dialog GUI based on the parameters before running the algorithm as part of a script.
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        UI = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        
        min_constr[0] = scriptParameters.getParams().getDouble("min_constr0");
        textMinConstr0.setText(String.valueOf(min_constr[0]));
        max_constr[0] = scriptParameters.getParams().getDouble("max_constr0");
        textMaxConstr0.setText(String.valueOf(max_constr[0]));
        min_constr[1] = scriptParameters.getParams().getDouble("min_constr1");
        textMinConstr1.setText(String.valueOf(min_constr[1]));
        max_constr[1] = scriptParameters.getParams().getDouble("max_constr1");
        textMaxConstr1.setText(String.valueOf(max_constr[1]));
        min_constr[2] = scriptParameters.getParams().getDouble("min_constr2");
        textMinConstr2.setText(String.valueOf(min_constr[2]));
        max_constr[2] = scriptParameters.getParams().getDouble("max_constr2");
        textMaxConstr2.setText(String.valueOf(max_constr[2]));
        r1 = scriptParameters.getParams().getDouble("r1_");
        textContrastRelaxivityRate.setText(String.valueOf(r1));
        rib = scriptParameters.getParams().getDouble("rib_");
        textBloodIntrinsicRelaxivityRate.setText(String.valueOf(1.0/rib));
        useConstantTissue = scriptParameters.getParams().getBoolean("use_constant_tissue");
        constantTissueRadioButton.setSelected(useConstantTissue);
        fileTissueRadioButton.setSelected(!useConstantTissue);
        buttonLowFlipFile.setEnabled(fileTissueRadioButton.isSelected());
        textLowFlipFile.setEnabled(fileTissueRadioButton.isSelected());
        buttonHighFlipFile.setEnabled(fileTissueRadioButton.isSelected());
        textHighFlipFile.setEnabled(fileTissueRadioButton.isSelected());
        labelTissueIntrinsicRelaxivityRate.setEnabled(constantTissueRadioButton.isSelected());
        labelTissueIntrinsicRelaxivityRate2.setEnabled(constantTissueRadioButton.isSelected());
        textTissueIntrinsicRelaxivityRate.setEnabled(constantTissueRadioButton.isSelected());
        if (useConstantTissue) {
            rit = scriptParameters.getParams().getDouble("rit_");
            textTissueIntrinsicRelaxivityRate.setText(String.valueOf(1.0/rit));
        }
        else {
        	lowFlipImage = scriptParameters.retrieveImage("low_flip_image");
        	textLowFlipFile.setText(lowFlipImage.getImageFileName());
            highFlipImage = scriptParameters.retrieveImage("high_flip_image");
            textHighFlipFile.setText(highFlipImage.getImageFileName());
        }
        theta = scriptParameters.getParams().getDouble("theta_");
        textFlipAngle.setText(String.valueOf(theta));
        tr = scriptParameters.getParams().getDouble("tr_");
        textTimeBetweenShots.setText(String.valueOf(tr));
        perMin = scriptParameters.getParams().getBoolean("per_min");
        secondButton.setSelected(!perMin);
        minuteButton.setSelected(perMin);
        nFirst = scriptParameters.getParams().getInt("n_first");
        textNFirst.setText(String.valueOf(nFirst));
        useVe = scriptParameters.getParams().getBoolean("use_ve");
        kepButton.setSelected(!useVe);
        veButton.setSelected(useVe);
        if (kepButton.isSelected()) {
    	    labelMinConstr1.setText("k_ep minimum allowed value (0 - 0.99)");
    	    labelMaxConstr1.setText("k_ep maximum allowed value (0 - 0.99)");
    	}
    	else {
    		labelMinConstr1.setText("ve minimum allowed value (1.0E-5 - 0.99)");
    	    labelMaxConstr1.setText("ve maximum allowed value (1.0E-5 - 0.99)");
    	}
    }

    /**
     * Store the parameters from the dialog to record the execution of this algorithm.
     *
     * @throws  ParserException  If there is a problem creating one of the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("min_constr0", min_constr[0]));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_constr0", max_constr[0]));
        scriptParameters.getParams().put(ParameterFactory.newParameter("min_constr1", min_constr[1]));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_constr1", max_constr[1]));
        scriptParameters.getParams().put(ParameterFactory.newParameter("min_constr2", min_constr[2]));
        scriptParameters.getParams().put(ParameterFactory.newParameter("max_constr2", max_constr[2]));
        scriptParameters.getParams().put(ParameterFactory.newParameter("r1_", r1));
        scriptParameters.getParams().put(ParameterFactory.newParameter("rib_", rib));
        scriptParameters.getParams().put(ParameterFactory.newParameter("use_constant_tissue", useConstantTissue));
        if (useConstantTissue) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("rit_", rit));
        }
        else {
        	try {
        	    scriptParameters.storeImage(lowFlipImage, "low_flip_image");
        	}
        	catch (ParserException e) {
        		MipavUtil.displayError("Parser exception on scriptParameters.storeImage(lowFlipImage");
        		return;
        	}
        	try {
                scriptParameters.storeImage(highFlipImage, "high_flip_image");
        	}
        	catch (ParserException e) {
        		MipavUtil.displayError("Parser exception on scriptParameters.storeImage(highFlipImage");
        		return;
        	}
        }
        scriptParameters.getParams().put(ParameterFactory.newParameter("theta_", theta));
        scriptParameters.getParams().put(ParameterFactory.newParameter("tr_", tr));
        scriptParameters.getParams().put(ParameterFactory.newParameter("per_min", perMin));
        scriptParameters.getParams().put(ParameterFactory.newParameter("n_first", nFirst));
        scriptParameters.getParams().put(ParameterFactory.newParameter("use_ve", useVe));
    }

    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * itemStateChanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if ((source == constantTissueRadioButton) || (source == fileTissueRadioButton)) {
            buttonLowFlipFile.setEnabled(fileTissueRadioButton.isSelected());
            textLowFlipFile.setEnabled(fileTissueRadioButton.isSelected());
            buttonHighFlipFile.setEnabled(fileTissueRadioButton.isSelected());
            textHighFlipFile.setEnabled(fileTissueRadioButton.isSelected());
            labelTissueIntrinsicRelaxivityRate.setEnabled(constantTissueRadioButton.isSelected());
            labelTissueIntrinsicRelaxivityRate2.setEnabled(constantTissueRadioButton.isSelected());
            textTissueIntrinsicRelaxivityRate.setEnabled(constantTissueRadioButton.isSelected());
        }
        else if ((source == kepButton) || (source == veButton)) {
        	if (kepButton.isSelected()) {
        	    labelMinConstr1.setText("k_ep minimum allowed value (0 - 0.99)");
        	    textMinConstr1.setText("0.0");
        	    labelMaxConstr1.setText("k_ep maximum allowed value (0 - 0.99)");
        	    textMaxConstr1.setText("0.99");
        	}
        	else {
        		labelMinConstr1.setText("ve minimum allowed value (1.0E-5 - 0.99)");
        	    textMinConstr1.setText("1.0E-5");
        	    labelMaxConstr1.setText("ve maximum allowed value (1.0E-5 - 0.99)");
        	    textMaxConstr1.setText("0.99");	
        	}
        }


    }

    

    /**
     * Disposes of error dialog, then frame. Sets cancelled to <code>true</code>.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowClosing(WindowEvent event) {

        cancelFlag = true;
        dispose();
    }

    /**
     * DOCUMENT ME!
     */
    protected void callAlgorithm() {
        int i;
        try {

            // Make algorithm
            int resultExtents[] = new int[4];
            for (i = 0; i < 3; i++) {
            	resultExtents[i] = image.getExtents()[i];
            }
            resultExtents[3] = 3;
            resultImage = new ModelImage(ModelStorageBase.FLOAT, resultExtents, image.getImageName() + "_params");
            demri3Algo = new AlgorithmDEMRI3(resultImage, image, min_constr, max_constr, r1,
            		                         rib, rit, lowFlipImage, highFlipImage, theta, tr, perMin, nFirst, useVe);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            demri3Algo.addListener(this);

            createProgressBar(image.getImageName(), demri3Algo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (demri3Algo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                demri3Algo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("Dialog DEMRI3: unable to allocate enough memory");

            return;
        }
    }

    /**
     * Builds a list of images. Returns combobox. List must be all color or all black and white.
     *
     * @param   image  DOCUMENT ME!
     *
     * @return  Newly created combo box.
     */
    private JComboBox buildComboBox(ModelImage image) {
        ViewUserInterface UI;
        ModelImage nextImage;
        boolean doAdd;
        int i;

        JComboBox comboBox = new JComboBox();
        comboBox.setFont(serif12);
        comboBox.setBackground(Color.white);

        UI = ViewUserInterface.getReference();

        Enumeration names = UI.getRegisteredImageNames();

        while (names.hasMoreElements()) {
            String name = (String) names.nextElement();

            if (!name.equals(image.getImageName())) {
                nextImage = UI.getRegisteredImageByName(name);

                if (UI.getFrameContainingImage(nextImage) != null) {

                    if ((image.isColorImage() == nextImage.isColorImage()) && (nextImage.getNDims() == 2)) {
                        doAdd = true;

                        for (i = 0; i < image.getNDims(); i++) {

                            if (image.getExtents()[i] != nextImage.getExtents()[i]) {
                                doAdd = false;
                            }
                        }

                        if (doAdd) {
                            comboBox.addItem(name);
                        }
                    }
                }
            }
        }

        return comboBox;
    }


    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        

        setForeground(Color.black);
        setTitle("3 - parameter DEMRI model");

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
        
        labelParamsToFit = new JLabel("3 model parameters to fit for each voxel in 3D:");
        labelParamsToFit.setForeground(Color.black);
        labelParamsToFit.setFont(serif12);
        mainPanel.add(labelParamsToFit, gbc);
        
        labelMinConstr0 = new JLabel("K_trans minimum allowed value (0 - 0.99)");
        labelMinConstr0.setForeground(Color.black);
        labelMinConstr0.setFont(serif12);
        gbc.gridy = 1;
        mainPanel.add(labelMinConstr0, gbc);
        
        textMinConstr0 = new JTextField(10);
        textMinConstr0.setText("0.0");
        textMinConstr0.setForeground(Color.black);
        textMinConstr0.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textMinConstr0, gbc);
        
        labelMaxConstr0 = new JLabel("K_trans maximum allowed value (0 - 0.99)");
        labelMaxConstr0.setForeground(Color.black);
        labelMaxConstr0.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 2;
        mainPanel.add(labelMaxConstr0, gbc);
        
        textMaxConstr0 = new JTextField(10);
        textMaxConstr0.setText("0.99");
        textMaxConstr0.setForeground(Color.black);
        textMaxConstr0.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textMaxConstr0, gbc);
        
        secondParamGroup = new ButtonGroup();
        kepButton = new JRadioButton("Second parameter is back-transfer rate (k_ep)", true);
        kepButton.setFont(serif12);
        kepButton.setForeground(Color.black);
        kepButton.addItemListener(this);
        secondParamGroup.add(kepButton);
        gbc.gridx = 0;
        gbc.gridy = 3;
        mainPanel.add(kepButton, gbc);
        
        veButton = new JRadioButton("Second parameter is external celluar volume fraction (ve)", false);
        veButton.setFont(serif12);
        veButton.setForeground(Color.black);
        veButton.addItemListener(this);
        secondParamGroup.add(veButton);
        gbc.gridx = 0;
        gbc.gridy = 4;
        mainPanel.add(veButton, gbc);
        
        labelMinConstr1 = new JLabel("k_ep minimum allowed value (0 - 0.99)");
        labelMinConstr1.setForeground(Color.black);
        labelMinConstr1.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 5;
        mainPanel.add(labelMinConstr1, gbc);
        
        textMinConstr1 = new JTextField(10);
        textMinConstr1.setText("0.0");
        textMinConstr1.setForeground(Color.black);
        textMinConstr1.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textMinConstr1, gbc);
        
        labelMaxConstr1 = new JLabel("k_ep maximum allowed value (0 - 0.99)");
        labelMaxConstr1.setForeground(Color.black);
        labelMaxConstr1.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 6;
        mainPanel.add(labelMaxConstr1, gbc);
        
        textMaxConstr1 = new JTextField(10);
        textMaxConstr1.setText("0.99");
        textMaxConstr1.setForeground(Color.black);
        textMaxConstr1.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textMaxConstr1, gbc);
        
        labelMinConstr2 = new JLabel("f_vp minimum allowed value (0 - 0.99)");
        labelMinConstr2.setForeground(Color.black);
        labelMinConstr2.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 7;
        mainPanel.add(labelMinConstr2, gbc);
        
        textMinConstr2 = new JTextField(10);
        textMinConstr2.setText("0.0");
        textMinConstr2.setForeground(Color.black);
        textMinConstr2.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textMinConstr2, gbc);
        
        labelMaxConstr2 = new JLabel("f_vp maximum allowed value (0 - 0.99)");
        labelMaxConstr2.setForeground(Color.black);
        labelMaxConstr2.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 8;
        mainPanel.add(labelMaxConstr2, gbc);
        
        textMaxConstr2 = new JTextField(10);
        textMaxConstr2.setText("0.99");
        textMaxConstr2.setForeground(Color.black);
        textMaxConstr2.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textMaxConstr2, gbc);
        
        labelContrastRelaxivityRate = new JLabel("Contrast relaxivity rate in 1/(mMol*sec) (0.0 - 1000.0)");
        labelContrastRelaxivityRate.setForeground(Color.black);
        labelContrastRelaxivityRate.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 9;
        mainPanel.add(labelContrastRelaxivityRate, gbc);
        
        textContrastRelaxivityRate = new JTextField(10);
        textContrastRelaxivityRate.setText("4.8");
        textContrastRelaxivityRate.setForeground(Color.black);
        textContrastRelaxivityRate.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textContrastRelaxivityRate, gbc);
        
        labelBloodIntrinsicRelaxivityRate = new JLabel("Blood intrinsic relaxivity rate in 1/(mMol * sec)");
        labelBloodIntrinsicRelaxivityRate.setForeground(Color.black);
        labelBloodIntrinsicRelaxivityRate.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 10;
        mainPanel.add(labelBloodIntrinsicRelaxivityRate, gbc);
        
        textBloodIntrinsicRelaxivityRate = new JTextField(10);
        textBloodIntrinsicRelaxivityRate.setText("1.5");
        textBloodIntrinsicRelaxivityRate.setForeground(Color.black);
        textBloodIntrinsicRelaxivityRate.setFont(serif12);
        gbc.gridx = 1;
        gbc.gridheight = 2;
        mainPanel.add(textBloodIntrinsicRelaxivityRate, gbc);
        
        labelBloodIntrinsicRelaxivityRate2 = new JLabel("specified as reciprocal, in seconds (0.001 - 10000.0)");
        labelBloodIntrinsicRelaxivityRate2.setForeground(Color.black);
        labelBloodIntrinsicRelaxivityRate2.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 11;
        gbc.gridheight = 1;
        mainPanel.add(labelBloodIntrinsicRelaxivityRate2, gbc);
        
        tissueGroup = new ButtonGroup();
        constantTissueRadioButton = new JRadioButton("Constant tissue intrinsic relaxivity rate", true);
        constantTissueRadioButton.setFont(serif12);
        constantTissueRadioButton.setForeground(Color.black);
        constantTissueRadioButton.addItemListener(this);
        tissueGroup.add(constantTissueRadioButton);
        gbc.gridx = 0;
        gbc.gridy = 12;
        mainPanel.add(constantTissueRadioButton, gbc);
        
        fileTissueRadioButton = new JRadioButton("File specified tissue intrinsic relaxivity rate", false);
        fileTissueRadioButton.setFont(serif12);
        fileTissueRadioButton.setForeground(Color.black);
        fileTissueRadioButton.addItemListener(this);
        tissueGroup.add(fileTissueRadioButton);
        gbc.gridx = 0;
        gbc.gridy = 13;
        mainPanel.add(fileTissueRadioButton, gbc);
        
        labelTissueIntrinsicRelaxivityRate = new JLabel("Tissue intrinsic relaxivity rate in 1/(mMol * sec)");
        labelTissueIntrinsicRelaxivityRate.setForeground(Color.black);
        labelTissueIntrinsicRelaxivityRate.setFont(serif12);
        labelTissueIntrinsicRelaxivityRate.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 14;
        mainPanel.add(labelTissueIntrinsicRelaxivityRate, gbc);
        
        textTissueIntrinsicRelaxivityRate = new JTextField(10);
        textTissueIntrinsicRelaxivityRate.setText("1.1");
        textTissueIntrinsicRelaxivityRate.setForeground(Color.black);
        textTissueIntrinsicRelaxivityRate.setFont(serif12);
        textTissueIntrinsicRelaxivityRate.setEnabled(true);
        gbc.gridx = 1;
        gbc.gridheight = 2;
        mainPanel.add(textTissueIntrinsicRelaxivityRate, gbc);
        
        labelTissueIntrinsicRelaxivityRate2 = new JLabel("specified as reciprocal, in seconds (0.001 - 10000.0)");
        labelTissueIntrinsicRelaxivityRate2.setForeground(Color.black);
        labelTissueIntrinsicRelaxivityRate2.setFont(serif12);
        labelTissueIntrinsicRelaxivityRate2.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 15;
        gbc.gridheight = 1;
        mainPanel.add(labelTissueIntrinsicRelaxivityRate2, gbc);
        
        buttonLowFlipFile = new JButton("Choose low flip-angle 3D image file");
        buttonLowFlipFile.setForeground(Color.black);
        buttonLowFlipFile.setFont(serif12B);
        buttonLowFlipFile.setEnabled(false);
        buttonLowFlipFile.addActionListener(this);
        buttonLowFlipFile.setActionCommand("LowFlipFile");
        buttonLowFlipFile.setPreferredSize(new Dimension(235, 30));
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridy = 16;
        mainPanel.add(buttonLowFlipFile, gbc);

        textLowFlipFile = new JTextField();
        textLowFlipFile.setFont(serif12);
        textLowFlipFile.setEnabled(false);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(textLowFlipFile, gbc);
        
        buttonHighFlipFile = new JButton("Choose high flip-angle 4D image file");
        buttonHighFlipFile.setForeground(Color.black);
        buttonHighFlipFile.setFont(serif12B);
        buttonHighFlipFile.setEnabled(false);
        buttonHighFlipFile.addActionListener(this);
        buttonHighFlipFile.setActionCommand("HighFlipFile");
        buttonHighFlipFile.setPreferredSize(new Dimension(235, 30));
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy = 17;
        mainPanel.add(buttonHighFlipFile, gbc);

        textHighFlipFile = new JTextField();
        textHighFlipFile.setFont(serif12);
        textHighFlipFile.setEnabled(false);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(textHighFlipFile, gbc);
        
        labelFlipAngle = new JLabel("Flip angle in degrees (0.0 - 90.0)");
        labelFlipAngle.setForeground(Color.black);
        labelFlipAngle.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 18;
        mainPanel.add(labelFlipAngle, gbc);
        
        textFlipAngle = new JTextField(10);
        textFlipAngle.setText("30.0");
        textFlipAngle.setForeground(Color.black);
        textFlipAngle.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textFlipAngle, gbc);
        
        labelTimeBetweenShots = new JLabel("Time between shots in seconds (0.0 - 10000.0)");
        labelTimeBetweenShots.setForeground(Color.black);
        labelTimeBetweenShots.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 19;
        mainPanel.add(labelTimeBetweenShots, gbc);
        
        textTimeBetweenShots = new JTextField(10);
        textTimeBetweenShots.setText("0.008");
        textTimeBetweenShots.setForeground(Color.black);
        textTimeBetweenShots.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textTimeBetweenShots, gbc);
        
        rateGroup = new ButtonGroup();
        secondButton = new JRadioButton("K_trans and k_ep are per second", true);
        secondButton.setFont(serif12);
        secondButton.setForeground(Color.black);
        rateGroup.add(secondButton);
        gbc.gridx = 0;
        gbc.gridy = 20;
        mainPanel.add(secondButton, gbc);
        
        minuteButton = new JRadioButton("K_trans and k_ep are per minute", false);
        minuteButton.setFont(serif12);
        minuteButton.setForeground(Color.black);
        rateGroup.add(minuteButton);
        gbc.gridx = 0;
        gbc.gridy = 21;
        mainPanel.add(minuteButton, gbc);
        
        labelMp = new JLabel("Draw a sagittal sinus VOI or open a VOI file");
        labelMp.setForeground(Color.black);
        labelMp.setFont(serif12);
        gbc.gridy = 22;
        mainPanel.add(labelMp, gbc);
        
        buttonMpFile = new JButton("Open a sagittal sinus VOI file");
        buttonMpFile.setForeground(Color.black);
        buttonMpFile.setFont(serif12B);
        buttonMpFile.addActionListener(this);
        buttonMpFile.setActionCommand("MpFile");
        buttonMpFile.setPreferredSize(new Dimension(205, 30));
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridy = 23;
        mainPanel.add(buttonMpFile, gbc);
        
        labelNFirst = new JLabel("nfirst injection TR index of input dataset (0 - 1000)");
        labelNFirst.setForeground(Color.black);
        labelNFirst.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 24;
        mainPanel.add(labelNFirst, gbc);
        
        textNFirst = new JTextField(10);
        textNFirst.setText("5");
        textNFirst.setForeground(Color.black);
        textNFirst.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textNFirst, gbc);

        getContentPane().add(mainPanel, BorderLayout.CENTER);
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
        String tmpStr;
        BufferedReader br = null;
        
        tmpStr = textMinConstr0.getText();
        min_constr[0] = Double.parseDouble(tmpStr);
        
        if (min_constr[0] < 0.0) {
        	MipavUtil.displayError("Minimum K_trans must be at least 0.0");
        	textMinConstr0.requestFocus();
        	textMinConstr0.selectAll();
        	return false;
        }
        
        tmpStr = textMaxConstr0.getText();
        max_constr[0] = Double.parseDouble(tmpStr);
        
        if (max_constr[0] < min_constr[0]) {
        	MipavUtil.displayError("Maximum K_trans must be at least " + min_constr[0]);
        	textMaxConstr0.requestFocus();
        	textMaxConstr0.selectAll();
        	return false;
        }
        else if (max_constr[0] > 0.99) {
        	MipavUtil.displayError("Maximum K_trans must not exceed 0.99");
        	textMaxConstr0.requestFocus();
        	textMaxConstr0.selectAll();
        	return false;	
        }
        
        if (kepButton.isSelected()) {
            useVe = false;
        }
        else {
            useVe = true;
        }
        
        tmpStr = textMinConstr1.getText();
        min_constr[1] = Double.parseDouble(tmpStr);
        
        if (useVe) {
        	if (min_constr[1] < 1.0E-5) {
	        	MipavUtil.displayError("Minimum ve must be at least 1.0E-5");
	        	textMinConstr1.requestFocus();
	        	textMinConstr1.selectAll();
	        	return false;
	        }
	        
	        tmpStr = textMaxConstr1.getText();
	        max_constr[1] = Double.parseDouble(tmpStr);
	        
	        if (max_constr[1] < min_constr[1]) {
	        	MipavUtil.displayError("Maximum ve must be at least " + min_constr[1]);
	        	textMaxConstr1.requestFocus();
	        	textMaxConstr1.selectAll();
	        	return false;
	        }
	        else if (max_constr[1] > 0.99) {
	        	MipavUtil.displayError("Maximum ve must not exceed 0.99");
	        	textMaxConstr1.requestFocus();
	        	textMaxConstr1.selectAll();
	        	return false;	
	        }	
        }
        else { // use k_ep
	        if (min_constr[1] < 0.0) {
	        	MipavUtil.displayError("Minimum k_ep must be at least 0.0");
	        	textMinConstr1.requestFocus();
	        	textMinConstr1.selectAll();
	        	return false;
	        }
	        
	        tmpStr = textMaxConstr1.getText();
	        max_constr[1] = Double.parseDouble(tmpStr);
	        
	        if (max_constr[1] < min_constr[1]) {
	        	MipavUtil.displayError("Maximum k_ep must be at least " + min_constr[1]);
	        	textMaxConstr1.requestFocus();
	        	textMaxConstr1.selectAll();
	        	return false;
	        }
	        else if (max_constr[1] > 0.99) {
	        	MipavUtil.displayError("Maximum k_ep must not exceed 0.99");
	        	textMaxConstr1.requestFocus();
	        	textMaxConstr1.selectAll();
	        	return false;	
	        }
        }
        
        tmpStr = textMinConstr2.getText();
        min_constr[2] = Double.parseDouble(tmpStr);
        
        if (min_constr[2] < 0.0) {
        	MipavUtil.displayError("Minimum f_vp must be at least 0.0");
        	textMinConstr2.requestFocus();
        	textMinConstr2.selectAll();
        	return false;
        }
        
        tmpStr = textMaxConstr2.getText();
        max_constr[2] = Double.parseDouble(tmpStr);
        
        if (max_constr[2] < min_constr[2]) {
        	MipavUtil.displayError("Maximum f_vp must be at least " + min_constr[2]);
        	textMaxConstr2.requestFocus();
        	textMaxConstr2.selectAll();
        	return false;
        }
        else if (max_constr[2] > 0.99) {
        	MipavUtil.displayError("Maximum f_vp must not exceed 0.99");
        	textMaxConstr2.requestFocus();
        	textMaxConstr2.selectAll();
        	return false;	
        }
        
        tmpStr = textContrastRelaxivityRate.getText();
        r1 = Double.parseDouble(tmpStr);
        
        if (r1 < r1Min) {
            MipavUtil.displayError("Contrast relaxivity rate must be at least " + Double.toString(r1Min));
            textContrastRelaxivityRate.requestFocus();
            textContrastRelaxivityRate.selectAll();
            return false;
        }
        
        if (r1 > r1Max) {
            MipavUtil.displayError("Contrast relaxivity rate must not exceed " + Double.toString(r1Max));
            textContrastRelaxivityRate.requestFocus();
            textContrastRelaxivityRate.selectAll();
            return false;
        }
        
        tmpStr = textBloodIntrinsicRelaxivityRate.getText();
        rib = Double.parseDouble(tmpStr);
        
        if (rib < ribMin) {
            MipavUtil.displayError("Blood intrinsic relaxivity rate must be at least " + Double.toString(ribMin));
            textBloodIntrinsicRelaxivityRate.requestFocus();
            textBloodIntrinsicRelaxivityRate.selectAll();
            return false;
        }
        
        if (rib > ribMax) {
            MipavUtil.displayError("Blood intrinsic relaxivity rate must not exceed " + Double.toString(ribMax));
            textBloodIntrinsicRelaxivityRate.requestFocus();
            textBloodIntrinsicRelaxivityRate.selectAll(); 
            return false;
        }
        rib = 1.0/rib;
        
        useConstantTissue = constantTissueRadioButton.isSelected();
        
        if (constantTissueRadioButton.isSelected()) {
            tmpStr = textTissueIntrinsicRelaxivityRate.getText();
            rit = Double.parseDouble(tmpStr);
            
            if (rit < ritMin) {
                MipavUtil.displayError("Tissue intrinsic relaxivity rate must be at least " + Double.toString(ritMin));
                textTissueIntrinsicRelaxivityRate.requestFocus();
                textTissueIntrinsicRelaxivityRate.selectAll();
                return false;
            }
            
            if (rit > ritMax) {
                MipavUtil.displayError("Tissue intrinsic relaxivity rate must not exceed " + Double.toString(ritMax));
                textTissueIntrinsicRelaxivityRate.requestFocus();
                textTissueIntrinsicRelaxivityRate.selectAll(); 
                return false;
            }
            rit = 1.0/rit;    
        } // (constantTissueRadioButton.isSelected())
        
        if (fileTissueRadioButton.isSelected()) {
            fileNameLowFlip = textLowFlipFile.getText();  
            try {
                FileIO fileIO = new FileIO();
                lowFlipImage = fileIO.readImage(fileNameLowFlip, directoryLowFlip, false, null);

                if (lowFlipImage == null) {
                    MipavUtil.displayError("Low flip image is not valid.");

                    return false;
                } else if (lowFlipImage.getNDims() != 3) {
                    MipavUtil.displayError("Low flip image must be 3D");

                    return false;
                }

                for (int i = 0; i < 3; i++) {

                    if (image.getExtents()[i] != lowFlipImage.getExtents()[i]) {
                        MipavUtil.displayError("First 3 dimensions of source image must match the low flip image.");

                        return false;
                    }
                }
                /*int length = lowFlipImage.getExtents()[0];
                for (int i = 1; i < lowFlipImage.getNDims(); i++) {
                    length *= lowFlipImage.getExtents()[i];
                }
                lowFlip = new double[length];
                try {
                    lowFlipImage.exportData(0, length, lowFlip);
                }
                catch(IOException e) {
                    MipavUtil.displayError("IOException on lowFlipImage.exportData(0, length, lowFlip)");
                    return false;
                }
                lowFlipImage.disposeLocal();
                lowFlipImage = null;*/

            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in JDialogDEMRI3");

                return false;
            }
            
            fileNameHighFlip = textHighFlipFile.getText();  
            try {
                FileIO fileIO = new FileIO();
                highFlipImage = fileIO.readImage(fileNameHighFlip, directoryHighFlip, false, null);

                if (highFlipImage == null) {
                    MipavUtil.displayError("High flip image is not valid.");

                    return false;
                } else if (highFlipImage.getNDims() != 4) {
                    MipavUtil.displayError("High flip image must be 4D");

                    return false;
                }

                for (int i = 0; i < 4; i++) {

                    if (image.getExtents()[i] != highFlipImage.getExtents()[i]) {
                        MipavUtil.displayError("Dimensions of source image must match the high flip image.");

                        return false;
                    }
                }
                /*int length = highFlipImage.getExtents()[0];
                for (int i = 1; i < highFlipImage.getNDims(); i++) {
                    length *= highFlipImage.getExtents()[i];
                }
                highFlip = new double[length];
                try {
                    highFlipImage.exportData(0, length, highFlip);
                }
                catch(IOException e) {
                    MipavUtil.displayError("IOException on highFlipImage.exportData(0, length, highFlip)");
                    return false;
                }
                highFlipImage.disposeLocal();
                highFlipImage = null;*/

            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in JDialogDEMRI3");

                return false;
            }
        } // if (fileTissueRadioButton.isSelected())

        tmpStr = textFlipAngle.getText();
        theta = Double.parseDouble(tmpStr);
        
        if (theta < thetaMin) {
            MipavUtil.displayError("Flip angle must be at least " + Double.toString(thetaMin));
            textFlipAngle.requestFocus();
            textFlipAngle.selectAll();
            return false;
        }
        
        if (theta > thetaMax) {
            MipavUtil.displayError("Flip angle must not exceed " + Double.toString(thetaMax));
            textFlipAngle.requestFocus();
            textFlipAngle.selectAll();  
            return false;
        }
        
        tmpStr = textTimeBetweenShots.getText();
        tr = Double.parseDouble(tmpStr);
        
        if (tr < trMin) {
            MipavUtil.displayError("Time between shots must be at least " + Double.toString(trMin));
            textTimeBetweenShots.requestFocus();
            textTimeBetweenShots.selectAll();
            return false;
        }
        
        if (tr > trMax) {
            MipavUtil.displayError("Time between shots must not exceed " + Double.toString(trMax));
            textTimeBetweenShots.requestFocus();
            textTimeBetweenShots.selectAll(); 
            return false;
        }
        
        if (secondButton.isSelected()) {
            perMin = false;
        }
        else {
            perMin = true;
        }
        
        VOIs = image.getVOIs();
        int nVOIs = VOIs.size();
        int nBoundingVOIs = 0;

        for (int i = 0; i < nVOIs; i++) {

            if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                nBoundingVOIs++;
            }
        }
        
        if (nBoundingVOIs == 0) {
            MipavUtil.displayError("No bounding vois around sagittal sinus");
            return false;
        }
        
        if (nBoundingVOIs > 1) {
            MipavUtil.displayError(nBoundingVOIs + " bounding vois around sagittal sinus instead of the expected 1");
            return false;
        }
        
        /*fileNameMp = textMpFile.getText();
        fileMp = new File(directoryMp + fileNameMp);
        
        try {
            br = new BufferedReader(new InputStreamReader(new FileInputStream(fileMp)));
        }
        catch (FileNotFoundException e) {
            MipavUtil.displayError((directoryMp + fileNameMp) + " was not found");
            return false;
        }
        
        // Port of mri_read_1D_stdin
        // Read lines until first character is not blank and not #
        int ii = 0;
        String line = null;
        do {
            try {
                // Contains the contents of the line not including line termination characters
                line = br.readLine();  
            }
            catch(IOException e) {
                MipavUtil.displayError("IOException on br.readLine");
                return false;
            }
            // have reached end of stream
            if (line == null) {
                MipavUtil.displayError("Have reached end of stream on br.readLine");
                return false;
            }
            for (ii = 0; ((ii < line.length()) && (Character.isSpaceChar(line.charAt(ii)))); ii++);
        } while ((ii == line.length()) || (line.charAt(ii) == '#'));

        int start = 0;
        int end = 0;
        double val[] = new double[10000];
        int nval = 0;
        while (true) {
            for (; (Character.isSpaceChar(line.charAt(start))); start++);
            end = start;
            for (; ((Character.isDigit(line.charAt(end))) || (line.charAt(end) == '.') || 
                           (line.charAt(end) == 'e') || (line.charAt(end) == 'E') ||
                           (line.charAt(end) == '+') || (line.charAt(end) == '-')); end++);
            if (start == end) {
                break;
            }
            val[nval++] = Double.valueOf(line.substring(start, end)).doubleValue();
            if (nval == 10000) {
                break;
            }
            start = end;
            if (line.charAt(start) == ',') {
                start++;
            }
            if (start == line.length()) {
                break;
            }
        } // while (true)
        if (nval < 1) {
            MipavUtil.displayError("No double values found in " + fileNameMp);
            return false;
        }
        
        nx = nval;
        ny = 1;
        double far[] = new double[nx];
        for (ii = 0; ii < nx; ii++) {
            far[ii] = val[ii];
        }
        double far2[] = null;
        
        while (true) {
            try {
                // Contains the contents of the line not including line termination characters
                line = br.readLine();  
            }
            catch(IOException e) {
                MipavUtil.displayError("IOException on br.readLine");
                return false;
            }
            // have reached end of stream
            if (line == null) {
                // done
                break;
            }
            for (ii = 0; ((ii < line.length()) && (Character.isSpaceChar(line.charAt(ii)))); ii++); 
            if ((ii == line.length()) || (line.charAt(ii) == '#')) {
                continue;
            }
            
            // Set input buffer to zero
            for (ii = 0; ii < nx; ii++) {
                val[ii] = 0.0;
            }
            nval = 0;
            start = 0;
            while (true) {
                for (; (Character.isSpaceChar(line.charAt(start))); start++);
                end = start;
                for (; ((Character.isDigit(line.charAt(end))) || (line.charAt(end) == '.') || 
                               (line.charAt(end) == 'e') || (line.charAt(end) == 'E') ||
                               (line.charAt(end) == '+') || (line.charAt(end) == '-')); end++);
                if (start == end) {
                    break;
                }
                val[nval++] = Double.valueOf(line.substring(start, end)).doubleValue();
                if (nval == nx) {
                    break;
                }
                start = end;
                if (line.charAt(start) == ',') {
                    start++;
                }
                if (start == line.length()) {
                    break;
                }
            } // while (true)
            far2 = new double[far.length];
            for (ii = 0; ii < far.length; ii++) {
                far2[ii] = far[ii];
            }
            far = new double[(ny+1)*nx];
            for (ii = 0; ii < far2.length; ii++) {
                far[ii] = far2[ii];
            }
            for (ii = 0; ii < nx; ii++) {
                far[ny*nx + ii] = val[ii];    
            }
            ny++;
        } // while (true)
        
        int jj;
        mcp = new double[far.length];
        int temp;
        if (ny > 1) {
            // more than one row ==> transpose (the usual case)
            for (jj = 0; jj < ny; jj++) {
                for (ii = 0; ii < nx; ii++) {
                    mcp[jj + ii * ny] = far[ii + jj * nx];
                }
            }
            temp = nx;
            nx = ny;
            ny = temp;
        } // if (ny > 1)
        else {
            for (ii = 0; ii < far.length; ii++) {
                mcp[ii] = far[ii];
            }
        }
        
        mp_len = nx;
        Preferences.debug("mcp array is : \n");
        for (ii = 0; ii < mp_len; ii++) {
            Preferences.debug("mcp[" + ii + "] = " + mcp[ii] + "\n");
        }
        Preferences.debug("\n");*/
        
        tmpStr = textNFirst.getText();
        nFirst = Integer.parseInt(tmpStr);
        
        if (nFirst < nFirstMin) {
            MipavUtil.displayError("nfirst must be >= " + nFirstMin);
            textNFirst.requestFocus();
            textNFirst.selectAll();
            return false;
        }
        if (nFirst > nFirstMax) {
            MipavUtil.displayError("nfirst must not exceed " + nFirstMax);
            textNFirst.requestFocus();
            textNFirst.selectAll();
            return false;
        }
        
        return true;
    }

}
