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
 * Dialog to get user input for 3 parameter dynamic (contrast) enhanced MRI model or SM2 model
 */
public class JDialogSM2 extends JDialogScriptableBase implements AlgorithmInterface, WindowListener {

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
    
    private JButton buttonTissueFile;
    
    private JTextField textTissueFile;
    
    // non-uniform tissue intrinsic relaxivity map
    double r1i[] = null;
    
    private String directoryTissue;
    
    private String fileNameTissue;
    
    private ModelImage tissueImage = null;
    
    private JLabel labelVOI;
    
    private JButton buttonVOIFile;
    
    private JTextField textVOIFile;
    
    private String directoryVOI;
    
    private String fileNameVOI;
    
    private File fileVOI;
    
    // Contents of Mp(t) file
    private double mcp[] = null;
    
    private JButton buttonTimesFile;
    
    private JTextField textTimesFile;
    
    private String directoryTimes;
    
    private String fileNameTimes;
    
    private File fileTimes;
    
    private int numTimes;
    
    private double timeVals[];
    
    private int nx;
    
    private int ny;
    
    private int mp_len;
    
    private ViewVOIVector VOIs;


    /** DOCUMENT ME! */
    private AlgorithmSM2 sm2Algo = null;

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
     * Creates a new JDialogSM2 object.
     *
     * @param  image  DOCUMENT ME!
     */
    public JDialogSM2(ModelImage image) {
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
    public JDialogSM2(Frame theParentFrame, ModelImage im) {
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
        BufferedReader br;
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
        } else if (command.equals("TissueFile")) {

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

                    chooser.setDialogTitle("Open 3D tissue instrinsic realxivity rate image file");
                    directoryTissue = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;

                    int returnValue = chooser.showOpenDialog(UI.getMainFrame());

                    if (returnValue == JFileChooser.APPROVE_OPTION) {
                        fileNameTissue = chooser.getSelectedFile().getName();
                        directoryTissue = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                        UI.setDefaultDirectory(directoryTissue);
                    } else {
                        fileNameTissue = null;

                        return;
                    }

                    if (fileNameTissue != null) {
                        textTissueFile.setText(fileNameTissue);
                    }
                } catch (OutOfMemoryError e) {
                    MipavUtil.displayError("Out of memory in JDialogSM2.");

                    return;
                }
        } else if (command.equals("VOIFile")) {

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
                directoryVOI = String.valueOf(chooser.getCurrentDirectory()) + File.separator;

                int returnValue = chooser.showOpenDialog(UI.getMainFrame());

                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    fileNameVOI = chooser.getSelectedFile().getName();
                    directoryVOI = String.valueOf(chooser.getCurrentDirectory()) + File.separator;
                    UI.setDefaultDirectory(directoryVOI);
                } else {
                    fileNameVOI = null;

                    return;
                }

                if (fileNameVOI != null) {

                    try {
                        fileVOI = new FileVOI(fileNameVOI, directoryVOI, image);
                    }
                    catch (IOException e) {
                        MipavUtil.displayError("IOException on new FileVOI(fileNameVOI, directoryVOI, image)");
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
                    
                    textVOIFile.setText(fileNameVOI);
                    
                    image.getParentFrame().getComponentImage().getVOIHandler().deleteVOIs();
                    image.registerVOI(voi[0]);

                    //  when everything's done, notify the image listeners
                    image.notifyImageDisplayListeners();   
                }

                
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in JDialogSM2.");

                return;
            }
        } else if (command.equals("TimesFile")) {

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

                chooser.setDialogTitle("Open file of volume center times");
                directoryTimes = String.valueOf(chooser.getCurrentDirectory()) + File.separator;

                int returnValue = chooser.showOpenDialog(UI.getMainFrame());

                if (returnValue == JFileChooser.APPROVE_OPTION) {
                    fileNameTimes = chooser.getSelectedFile().getName();
                    directoryTimes = String.valueOf(chooser.getCurrentDirectory()) + File.separator;
                    UI.setDefaultDirectory(directoryTimes);
                } else {
                    fileNameTimes = null;

                    return;
                }

                if (fileNameTimes != null) {
                	fileTimes = new File(directoryTimes + fileNameTimes);
                    
                    try {
                        br = new BufferedReader(new InputStreamReader(new FileInputStream(fileTimes)));
                    }
                    catch (FileNotFoundException e) {
                        MipavUtil.displayError((directoryTimes + fileNameTimes) + " was not found");
                        return;
                    }
                    
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
                            br.close();
                            return;
                        }
                        // have reached end of stream
                        if (line == null) {
                            MipavUtil.displayError("Have reached end of stream on br.readLine");
                            br.close();
                            return;
                        }
                        for (ii = 0; ((ii < line.length()) && (Character.isSpaceChar(line.charAt(ii)))); ii++);
                    } while ((ii == line.length()) || (line.charAt(ii) == '#'));
                    
                    int start = 0;
                    int end = 0;
                    for (; ((start < line.length()) && (Character.isSpaceChar(line.charAt(start)))); start++);
                    end = start;
                    for (; ((end < line.length()) && (Character.isDigit(line.charAt(end)))); end++);
                    if (start == end) {
                        MipavUtil.displayError("No digit starts line which should contain number of times");
                        br.close();
                        return;
                    }
                    numTimes = Integer.valueOf(line.substring(start, end)).intValue();
                    if (numTimes != image.getExtents()[3]) {
                    	MipavUtil.displayError("Number of times in file = " + numTimes + " != tDim of source image = " + image.getExtents()[3]);
                    	br.close();
                    	return;
                    }
                    timeVals = new double[numTimes];
                    int nval = 0;
                    while (true) {
                    	try {
                            // Contains the contents of the line not including line termination characters
                            line = br.readLine();  
                        }
                        catch(IOException e) {
                            MipavUtil.displayError("IOException on br.readLine");
                            br.close();
                            return;
                        }
                        // have reached end of stream
                        if (line == null) {
                            MipavUtil.displayError("Have reached end of stream on br.readLine");
                            break;
                        }
                    	start = 0;
                    	end = 0;
                    	for (; ((start < line.length()) && (Character.isSpaceChar(line.charAt(start)))); start++);
                        end = start;
                        for (; ((end < line.length()) && ((Character.isDigit(line.charAt(end))) || (line.charAt(end) == '.') || 
                                       (line.charAt(end) == 'e') || (line.charAt(end) == 'E') ||
                                       (line.charAt(end) == '+') || (line.charAt(end) == '-'))); end++);
                        if (start == end) {
                            continue;
                        }
                        timeVals[nval++] = Double.valueOf(line.substring(start, end)).doubleValue();
                        if (nval ==  numTimes) {
                            break;
                        }
                    } // while (true)
                    br.close();
                    if (nval < 1) {
                        MipavUtil.displayError("No double values found in " + fileNameTimes);
                        return;
                    }
                    if (nval < numTimes) {
                    	MipavUtil.displayError("Only " + nval + " of " + numTimes + " required times found");
                    	return;
                    }

                	textTimesFile.setText(fileNameTimes);
                }
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory in JDialogSM2.");

                return;
            } catch (IOException e) {
            	MipavUtil.displayError("IOException on BufferedReader");
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

        if (algorithm instanceof AlgorithmSM2) {
            Preferences.debug("SM2 elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((sm2Algo.isCompleted() == true) && (resultImage != null)) {

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
        defaultsString += tissueImage.getImageFileName();

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
        tissueImage = scriptParameters.retrieveImage("tissue_image");
        textTissueFile.setText(tissueImage.getImageFileName());
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
    	try {
    	    scriptParameters.storeImage(tissueImage, "tissue_image");
    	}
    	catch (ParserException e) {
    		MipavUtil.displayError("Parser exception on scriptParameters.storeImage(tissueImage");
    		return;
    	}
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
            sm2Algo = new AlgorithmSM2(resultImage, image, min_constr, max_constr, r1, tissueImage, timeVals);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            sm2Algo.addListener(this);

            createProgressBar(image.getImageName(), sm2Algo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (sm2Algo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                sm2Algo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("Dialog SM2: unable to allocate enough memory");

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
        setTitle("3 - parameter SM2 model");

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
        
        labelMinConstr0 = new JLabel("K_trans minimum allowed value (1.0E-5 - 0.99)");
        labelMinConstr0.setForeground(Color.black);
        labelMinConstr0.setFont(serif12);
        gbc.gridy = 1;
        mainPanel.add(labelMinConstr0, gbc);
        
        textMinConstr0 = new JTextField(10);
        textMinConstr0.setText("1.0E-5");
        textMinConstr0.setForeground(Color.black);
        textMinConstr0.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textMinConstr0, gbc);
        
        labelMaxConstr0 = new JLabel("K_trans maximum allowed value (1.0E-5 - 0.99)");
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
        
        labelMinConstr1 = new JLabel("ve minimum allowed value (1.0E-5 - 0.99)");
        labelMinConstr1.setForeground(Color.black);
        labelMinConstr1.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 3;
        mainPanel.add(labelMinConstr1, gbc);
        
        textMinConstr1 = new JTextField(10);
        textMinConstr1.setText("1.0E-5");
        textMinConstr1.setForeground(Color.black);
        textMinConstr1.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textMinConstr1, gbc);
        
        labelMaxConstr1 = new JLabel("ve maximum allowed value (1.0E-5 - 0.99)");
        labelMaxConstr1.setForeground(Color.black);
        labelMaxConstr1.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 4;
        mainPanel.add(labelMaxConstr1, gbc);
        
        textMaxConstr1 = new JTextField(10);
        textMaxConstr1.setText("0.99");
        textMaxConstr1.setForeground(Color.black);
        textMaxConstr1.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textMaxConstr1, gbc);
        
        labelMinConstr2 = new JLabel("vp minimum allowed value (0 - 0.99)");
        labelMinConstr2.setForeground(Color.black);
        labelMinConstr2.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 5;
        mainPanel.add(labelMinConstr2, gbc);
        
        textMinConstr2 = new JTextField(10);
        textMinConstr2.setText("0.0");
        textMinConstr2.setForeground(Color.black);
        textMinConstr2.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textMinConstr2, gbc);
        
        labelMaxConstr2 = new JLabel("vp maximum allowed value (0 - 0.99)");
        labelMaxConstr2.setForeground(Color.black);
        labelMaxConstr2.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 6;
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
        gbc.gridy = 7;
        mainPanel.add(labelContrastRelaxivityRate, gbc);
        
        textContrastRelaxivityRate = new JTextField(10);
        textContrastRelaxivityRate.setText("4.79");
        textContrastRelaxivityRate.setForeground(Color.black);
        textContrastRelaxivityRate.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(textContrastRelaxivityRate, gbc);
        
        buttonTissueFile = new JButton("Choose 3D tissue R1 map");
        buttonTissueFile.setForeground(Color.black);
        buttonTissueFile.setFont(serif12B);
        buttonTissueFile.addActionListener(this);
        buttonTissueFile.setActionCommand("TissueFile");
        buttonTissueFile.setPreferredSize(new Dimension(235, 30));
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy = 8;
        mainPanel.add(buttonTissueFile, gbc);

        textTissueFile = new JTextField();
        textTissueFile.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(textTissueFile, gbc);
        
        labelVOI = new JLabel("Draw a sagittal sinus VOI or open a VOI file");
        labelVOI.setForeground(Color.black);
        labelVOI.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 9;
        mainPanel.add(labelVOI, gbc);
        
        buttonVOIFile = new JButton("Open a sagittal sinus VOI file");
        buttonVOIFile.setForeground(Color.black);
        buttonVOIFile.setFont(serif12B);
        buttonVOIFile.addActionListener(this);
        buttonVOIFile.setActionCommand("VOIFile");
        buttonVOIFile.setPreferredSize(new Dimension(205, 30));
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridy = 10;
        mainPanel.add(buttonVOIFile, gbc);
        
        textVOIFile = new JTextField();
        textVOIFile.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(textVOIFile, gbc);
        
        buttonTimesFile = new JButton("Open a file of volume center times");
        buttonTimesFile.setForeground(Color.black);
        buttonTimesFile.setFont(serif12B);
        buttonTimesFile.addActionListener(this);
        buttonTimesFile.setActionCommand("TimesFile");
        buttonTimesFile.setPreferredSize(new Dimension(225, 30));
        gbc.fill = GridBagConstraints.NONE;
        gbc.gridx = 0;
        gbc.gridy = 11;
        mainPanel.add(buttonTimesFile, gbc);
        
        textTimesFile = new JTextField();
        textTimesFile.setFont(serif12);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 1;
        mainPanel.add(textTimesFile, gbc);
        
        
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
        
        if (min_constr[0] < 1.0E-5) {
        	MipavUtil.displayError("Minimum K_trans must be at least 1.0E-5");
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
        
        tmpStr = textMinConstr1.getText();
        min_constr[1] = Double.parseDouble(tmpStr);
        
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
        
        tmpStr = textMinConstr2.getText();
        min_constr[2] = Double.parseDouble(tmpStr);
        
        if (min_constr[2] < 0.0) {
        	MipavUtil.displayError("Minimum vp must be at least 0.0");
        	textMinConstr2.requestFocus();
        	textMinConstr2.selectAll();
        	return false;
        }
        
        tmpStr = textMaxConstr2.getText();
        max_constr[2] = Double.parseDouble(tmpStr);
        
        if (max_constr[2] < min_constr[2]) {
        	MipavUtil.displayError("Maximum vp must be at least " + min_constr[2]);
        	textMaxConstr2.requestFocus();
        	textMaxConstr2.selectAll();
        	return false;
        }
        else if (max_constr[2] > 0.99) {
        	MipavUtil.displayError("Maximum vp must not exceed 0.99");
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
        
        
        fileNameTissue = textTissueFile.getText();  
        try {
            FileIO fileIO = new FileIO();
            tissueImage = fileIO.readImage(fileNameTissue, directoryTissue, false, null);

            if (tissueImage == null) {
                MipavUtil.displayError("Tissue image is not valid.");

                return false;
            } else if (tissueImage.getNDims() != 3) {
                MipavUtil.displayError("Tissue image must be 3D");

                return false;
            }

            for (int i = 0; i < 3; i++) {

                if (image.getExtents()[i] != tissueImage.getExtents()[i]) {
                    MipavUtil.displayError("First 3 dimensions of source image must match the tissue image.");

                    return false;
                }
            }

        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory in JDialogSM2");

            return false;
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
        
        if (timeVals == null) {
        	MipavUtil.displayError("Array of time values was not created");
        	return false;
        }
        
        if (timeVals.length < image.getExtents()[3]) {
            MipavUtil.displayError("Array of time values only has " + timeVals.length + " of " + image.getExtents()[3] + " required values");
            return false;
        }
        
        return true;
    }

}
