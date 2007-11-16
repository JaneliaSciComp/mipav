package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import javax.swing.*;


/**
 
 */
public class JDialogCreateRtable extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Number of bins for gradient direction, normal to the tangent angle, going from 0 to 2*PI radians.
     *  The gradient direction is defined as going into the object.
     */
    private int binNumber = 90;
    
    private JTextField binText;

    /** DOCUMENT ME! */
    private AlgorithmCreateRtable createRtableAlgo;

    /** DOCUMENT ME! */
    private JButton fileButton;

    /** DOCUMENT ME! */
    private String fileName = null;

    /** DOCUMENT ME! */
    private JTextField fileTF;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;
    
    private JTextField sideText;
    
    /** Number of points to take from each side of a point on a curve in determining a tangent */
    private int sidePointsForTangent;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (during script execution).
     */
    public JDialogCreateRtable() { }
    
    /**
     * Create a dialog to set variables to extract surface.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogCreateRtable(JFrame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
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
                
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.RTABLE));

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
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("");
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
        if (algorithm instanceof AlgorithmCreateRtable) {
            image.clearMask();
            System.gc();
        }

        if (createRtableAlgo.isCompleted() == true) {

            insertScriptLine();
        }

        dispose();
    }
    
    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("bin_number", binNumber));
        scriptParameters.getParams().put(ParameterFactory.newParameter("side_points_for_tangent", sidePointsForTangent));
        scriptParameters.getParams().put(ParameterFactory.newParameter("file_name", fileName));
    }
    
    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();
        
        setBinNumber(scriptParameters.getParams().getInt("bin_number"));
        setSidePointsForTangent(scriptParameters.getParams().getInt("side_points_for_tangent"));
        setFileName(scriptParameters.getParams().getString("file_name"));
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     *
     * @param  event  Event that triggered this method.
     */
    public void itemStateChanged(ItemEvent event) {

        
    }


    /**
     * Accessor that sets the file name. Only the file name and should end in ".r"
     *
     * @param  name  name of the file where the R-table is to be saved.
     */
    public void setFileName(String name) {
        fileName = name;
    }

    /**
     * Accessor that sets the image that has the contour VOI used for R-table creation.
     *
     * @param  im  the image
     */
    public void setImage(ModelImage im) {
        image = im;
    }

    /**
     * Accessor that sets the number of gradient angle bins to be used.
     *
     * @param  binNumber
     */
    public void setBinNumber(int binNumber) {
        this.binNumber = binNumber;
    }

    /**
     * Accessor that set the number of points to take from each side of a point on a curve in determining a tangent
     * @param sidePointsForTangent
     */
    public void setSidePointsForTangent(int sidePointsForTangent) {
        this.sidePointsForTangent = sidePointsForTangent;
    }
 
    /**
     * Once all the necessary variables are set, call the createRtable algorithm
     */
    protected void callAlgorithm() {


        // Make algorithm
        createRtableAlgo = new AlgorithmCreateRtable(image, binNumber, sidePointsForTangent, fileName);

        // This is very important. Adding this object as a listener allows the algorithm to
        // notify this object when it has completed of failed. See algorithm performed event.
        // This is made possible by implementing AlgorithmedPerformed interface
        createRtableAlgo.addListener(this);

        createProgressBar(image.getImageName(), createRtableAlgo);
        
        // Hide dialog
        setVisible(false);

        if (isRunInSeparateThread()) {

            // Start the thread as a low priority because we wish to still have user interface work fast
            if (createRtableAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            createRtableAlgo.run();
        }
    }

    /**
     * Initializes the GUI by creating the components, placing them in the dialog, and displaying them.
     */
    private void init() {
        String fileLocation;
        setForeground(Color.black);
        setTitle("Create R-table from VOI");

        JPanel informationPanel = new JPanel(new GridBagLayout());

        informationPanel.setForeground(Color.black);
        informationPanel.setBorder(buildTitledBorder("Information"));

        JLabel VOILabel = new JLabel("R-table is created with 1 contour VOI");
        VOILabel.setForeground(Color.black);
        VOILabel.setFont(serif12);
        VOILabel.setEnabled(true);
        
        JLabel coverLabel = new JLabel("Gradient angle bins cover 0 to 360 degrees");
        coverLabel.setForeground(Color.black);
        coverLabel.setFont(serif12);
        coverLabel.setEnabled(true);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 2;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;

        gbc.gridx = 0;
        gbc.gridy = 0;
        informationPanel.add(VOILabel, gbc);
        gbc.gridy = 1;
        informationPanel.add(coverLabel, gbc);

        JPanel paramPanel = new JPanel(new GridBagLayout());

        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("User parameters"));
        
        JLabel binLabel = new JLabel("Number of gradient angle bins  ");
        binLabel.setForeground(Color.black);
        binLabel.setFont(serif12);
        binLabel.setEnabled(true);
        
        binText = new JTextField(10);
        binText.setText("90");
        binText.setForeground(Color.black);
        binText.setFont(serif12);
        binText.setEnabled(true);
        
        JLabel sideLabel = new JLabel("Maximum curve points on each side for tangent ");
        sideLabel.setForeground(Color.black);
        sideLabel.setFont(serif12);
        sideLabel.setEnabled(true);

        sideText = new JTextField(10);
        sideText.setText("3");
        sideText.setFont(serif12);
        sideText.setEnabled(true);
        
        gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        paramPanel.add(binLabel, gbc);
        gbc.gridx = 1;
        paramPanel.add(binText, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        paramPanel.add(sideLabel, gbc);
        gbc.gridx = 1;
        paramPanel.add(sideText, gbc);

        gbc = new GridBagConstraints();
        gbc.gridwidth = 2;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;

        JPanel filePanel = new JPanel(new GridBagLayout());

        filePanel.setForeground(Color.black);
        filePanel.setBorder(buildTitledBorder("Save R-table"));

        fileButton = new JButton("Choose...");
        fileButton.setPreferredSize(MipavUtil.defaultButtonSize);
        fileButton.setFont(serif12B);
        fileButton.addActionListener(this);
        fileButton.setActionCommand("File");

        fileTF = new JTextField();
        fileTF.setText(makeImageName(image.getImageName()+ "_contour", ".rtb"));

        if (userInterface.getDefaultDirectory() != null) {
            fileLocation = userInterface.getDefaultDirectory();
        } else {
            fileLocation = System.getProperties().getProperty("user.dir");
        }

        fileLocation += makeImageName(image.getImageName(), ".rtb");

        fileTF.setToolTipText(fileLocation);
        fileTF.setFont(serif12);

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

        JPanel mainPanel = new JPanel(new GridBagLayout());

        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add(informationPanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(paramPanel, gbc);
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
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        
        int i;
        ViewVOIVector VOIs = image.getVOIs();
        int nVOI;

        nVOI = VOIs.size();

        if (nVOI == 0) {
            MipavUtil.displayError("Must create a contour VOI");

            return false;
        }

        int contourVOI = 0;
        int activeContourVOI = 0;

        for (i = 0; i < nVOI; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                contourVOI++;

                if (VOIs.VOIAt(i).isActive() == true) {
                    activeContourVOI++;
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
        
        fileName = fileTF.getText();

        int idx = fileName.lastIndexOf('.');

        if (idx < 0) {
            fileName = makeImageName(fileTF.getText(), ".rtb");
        }

        if (fileName.equals("")) {
            MipavUtil.displayError("Enter a file name.");
            fileTF.requestFocus();

            return false;
        }

        String tmpStr = binText.getText();

        if (testParameter(tmpStr, 3, 1000)) {
            binNumber = Integer.valueOf(tmpStr).intValue();
        } else {
            binText.requestFocus();
            binText.selectAll();

            return false;
        }
        
        if (!testParameter(sideText.getText(), 1, 10)) {
            sideText.requestFocus();
            sideText.selectAll();

            return false;
        } else {
            sidePointsForTangent = Integer.valueOf(sideText.getText()).intValue();
        }

        return true;
    }
}
