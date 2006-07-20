package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Creates the dialog to remove separate slices in an image. Dialog asks which slices the user wishes to remove; it
 * provides buttons to mark all slices for removal and to de-select any slices from image removal; it gives options to
 * remove or to cancel. Allows 3D or 4D images; 2D images would not make sense with this operation.**(as of 25 Oct, does
 * not yet rename removed slice image when saving)**(as of 1 November, does not yet process the more complicated DICOM
 * images completely.
 *
 * @author   David Parsons (parsonsd@cbel.cit.nih.gov) (with vast help from M.McAuliffe)
 * @version  v0.12 1 Nov 1999 (processes most images)
 */
public class JDialogExtractSlicesVolumes extends JDialogScriptableBase implements AlgorithmInterface{

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2217089541745539442L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JCheckBox[] checkboxList;

    /** DOCUMENT ME! */
    private JPanel checkboxPanel;

    /** DOCUMENT ME! */
    private JButton checkButton; // dialog button to set all checks to TRUE (checked-TRUE means 'extract this slice')

    /** DOCUMENT ME! */
    private JButton checkEvenButton; // dialog button to set all checks to FALSE

    /** DOCUMENT ME! */
    private boolean[] checkListExtract;

    /** DOCUMENT ME! */
    private JButton checkOddButton; // dialog button to set all checks to TRUE (checked-TRUE means 'extract this slice')

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    // or if the source image is to be replaced

    /** DOCUMENT ME! */
    private boolean dontOpenFrame = false;

    /** DOCUMENT ME! */
    private JLabel exampleLabel;

    /** DOCUMENT ME! */
    private JLabel exampleLabel2;

    /** DOCUMENT ME! */
    private AlgorithmExtractSlicesVolumes extractSlicesAlgo;

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private int nSlices; // number of slices in image

    /** DOCUMENT ME! */
    private int numChecked;

    /** DOCUMENT ME! */
    private boolean pressedCheckEven = false;

    /** DOCUMENT ME! */
    private boolean pressedCheckOdd = false;

    /** DOCUMENT ME! */
    private JTextField rangeField;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JScrollPane scrollPane;

    /** DOCUMENT ME! */
    private ModelImage srcImage; // source image

    /** DOCUMENT ME! */
    private boolean successful = false; // indicates status of algorithm

    /** DOCUMENT ME! */
    private String[] titles; // title of the frame shown when image is NULL

    /** DOCUMENT ME! */
    private JButton unCheckButton; // dialog button to set all checks to FALSE

    /** DOCUMENT ME! */
    private JCheckBox useRange;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogExtractSlicesVolumes() { }

    /**
     * Creates new dialog for removing slices.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogExtractSlicesVolumes(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        srcImage = im; // set the image from the arguments to an image in this class
        userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------
     
    /**
     * Record the parameters just used to run this algorithm in a script.
     * 
     * @throws  ParserException  If there is a problem creating/recording the new parameters.
     */
   
    protected void storeParamsFromGUI() throws ParserException{
        scriptParameters.storeInputImage(srcImage);
        scriptParameters.getParams().put(ParameterFactory.newParameter("checkListExtract",checkListExtract));
    }
    
    /**
     * Set the dialog GUI using the script parameters while running this algorithm as part of a script.
     */
   protected void setGUIFromParams(){
        checkListExtract = scriptParameters.getParams().getList("checkListExtract").getAsBooleanArray(); 
    }
    
    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        int i;

        if (command.equals("Extract")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("10077");
        } else if (command.equals("Check")) {

            for (i = 0; i < nSlices; i++) {
                (checkboxList[i]).setSelected(true);
            }
        } else if (command.equals("UnCheck")) {

            for (i = 0; i < nSlices; i++) {
                (checkboxList[i]).setSelected(false);
            }
        } else if (command.equals("CheckEven")) {
            pressedCheckEven = true;

            for (i = 1; i < nSlices; i += 2) {
                (checkboxList[i]).setSelected(true);
            }
        } else if (command.equals("CheckOdd")) {
            pressedCheckOdd = true;

            for (i = 0; i < nSlices; i += 2) {
                (checkboxList[i]).setSelected(true);
            }
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

        if (algorithm instanceof AlgorithmExtractSlicesVolumes) {

            insertScriptLine();

            if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM)) {
                int numExtracted = 0;
                Preferences.debug("\nHave extracted slices:\n");

                for (int i = 0; i < checkListExtract.length; i++) {

                    if (checkListExtract[i]) {
                        Preferences.debug("\t" + (i + 1));

                        if (((i % 5) == 4) || (numExtracted == (numChecked - 1))) {
                            Preferences.debug("\n");
                        }

                        numExtracted++;
                    }
                }

                if (srcImage.getNDims() == 3) {
                    Preferences.debug("from " + srcImage.getFileInfo(0).getExtents()[2] + " slice 3D " +
                                      srcImage.getImageName() + "\n");
                } else {
                    Preferences.debug("from " + srcImage.getFileInfo(0).getExtents()[2] + " slice " +
                                      srcImage.getFileInfo(0).getExtents()[3] + " volume 4D " +
                                      srcImage.getImageName() + "\n");
                }

                Preferences.debug("to create:\n");

                if (srcImage.getNDims() == 3) {

                    if (numExtracted > 1) {
                        Preferences.debug(numExtracted + " 2D images\n");
                    } else {
                        Preferences.debug(numExtracted + " 2D image\n");
                    }
                } else {

                    if (numExtracted > 1) {
                        Preferences.debug(numExtracted + " " + srcImage.getFileInfo(0).getExtents()[3] +
                                          " slice 3D images\n");
                    } else {
                        Preferences.debug(numExtracted + " " + srcImage.getFileInfo(0).getExtents()[3] +
                                          " slice 3D image\n");
                    }
                }
            } // if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM))
        } // if ( algorithm instanceof AlgorithmExtractSlicesVolumes )

        extractSlicesAlgo.finalize();
        extractSlicesAlgo = null;
        dispose();
    }

    /**
     * Accessor that returns the result image.
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

 

    /**
     * Returns <code>true</code> if only the even image slices have been selected.
     *
     * @return  <code>true</code> if only even slices selected to be removed, <code>false</code> otherwise
     */
    public boolean isEvenSelected() {

        for (int i = 0; i < nSlices; i++) {

            if ((((i % 2) == 0) && checkListExtract[i]) || (((i % 2) != 0) && !checkListExtract[i])) {
                return false;
            }
        }

        return true;
    }

    /**
     * Returns <code>true</code> if only the odd image slices have been selected.
     *
     * @return  <code>true</code> if only odd slices selected to be removed, <code>false</code> otherwise
     */
    public boolean isOddSelected() {

        for (int i = 0; i < nSlices; i++) {

            if ((((i % 2) == 0) && !checkListExtract[i]) || (((i % 2) != 0) && checkListExtract[i])) {
                return false;
            }
        }

        return true;
    }

    /**
     * Accessor that returns the whether or not the algorithm completed successfully.
     *
     * @return  DOCUMENT ME!
     */
    public boolean isSuccessful() {
        return successful;
    }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) {

        if (event.getSource() == useRange) {

            if (useRange.isSelected()) {
                rangeField.setEnabled(true);
                exampleLabel.setEnabled(true);
                exampleLabel2.setEnabled(true);
                checkButton.setEnabled(false);
                unCheckButton.setEnabled(false);
                checkOddButton.setEnabled(false);
                checkEvenButton.setEnabled(false);

                for (int i = 0; i < checkboxList.length; i++) {
                    checkboxList[i].setEnabled(false);
                }
            } else {
                rangeField.setEnabled(false);
                exampleLabel.setEnabled(false);
                exampleLabel2.setEnabled(false);
                checkButton.setEnabled(true);
                unCheckButton.setEnabled(true);
                checkOddButton.setEnabled(true);
                checkEvenButton.setEnabled(true);

                for (int i = 0; i < checkboxList.length; i++) {
                    checkboxList[i].setEnabled(true);
                }
            }
        }
    }



    /**
     * Accessor that sets the which slices to remove according to the boolean array paramater.
     *
     * @param  cl  for every element that is true, the slice corresponding to that element index will be removed
     */
    public void setCheckListExtract(boolean[] cl) {
        checkListExtract = cl;
    }

    /**
     * Accessor that sets the which slices to remove according to the vector of strings.
     *
     * @param  slices  - the list of slices to be removed.
     */
    public void setCheckListExtract(Vector slices) {

        nSlices = srcImage.getExtents()[2];
        checkListExtract = new boolean[nSlices];

        // set to true slices to be removed, others to false
        for (int i = 0; i < nSlices; i++) {

            if ((slices != null) && slices.contains(Integer.toString(i))) {
                checkListExtract[i] = true;
            } else {
                checkListExtract[i] = false;
            }
        }

    } // end setCheckListExtract()

    /**
     * Sets up so that only even slices will be removed.
     */
    public void setCheckListExtractEven() {
        nSlices = srcImage.getExtents()[2];
        checkListExtract = new boolean[nSlices];

        for (int i = 0; i < nSlices; i++) {

            if ((i % 2) == 0) {
                checkListExtract[i] = false;
            } else {
                checkListExtract[i] = true;
            }
        }
    }

    /**
     * Sets up so that only odd slices will be removed.
     */
    public void setCheckListExtractOdd() {
        nSlices = srcImage.getExtents()[2];
        checkListExtract = new boolean[nSlices];

        for (int i = 0; i < nSlices; i++) {

            if ((i % 2) == 0) {
                checkListExtract[i] = true;
            } else {
                checkListExtract[i] = false;
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
     * DOCUMENT ME!
     *
     * @param  dontOpenFrame  DOCUMENT ME!
     */
    public void setDontOpenFrame(boolean dontOpenFrame) {
        this.dontOpenFrame = dontOpenFrame;
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {

        // make sure that this is a 3D image first
        // make sure this image, im, is not 2D, for removing an image's only slice makes no sense...
        if ((srcImage.getNDims() == 2) || (srcImage.getExtents()[2] == 1)) {
            MipavUtil.displayError("Extract Individual Slices does not make sense for single-slice (2-D)\n" +
                                   "images.  No operation may be performed.");

            return; // the wrong kind of image gets sent back before wasting anymore time.
        }

        nSlices = srcImage.getExtents()[2];

        JPanel mainPanel = new JPanel(new BorderLayout()); // everything gets placed on this panel

        setTitle("Extract slices / volumes");
        setForeground(Color.black);

        checkboxPanel = new JPanel(); // place a check-box list in here
        checkboxPanel.setLayout(new GridLayout(nSlices, 1));
        checkboxPanel.setForeground(Color.white);
        checkboxPanel.setBackground(Color.white);
        checkboxList = new JCheckBox[nSlices]; // selector for the user to choose which slices to remove.  TRUE means
                                               // remove.

        for (int i = 0; i < nSlices; i++) { // place nSlices of check options for user and give them a name
            checkboxList[i] = new JCheckBox("Image slice " + (String.valueOf(i + 1)));

            // checkboxList[i].setFont(serif12B);
            checkboxList[i].setBackground(Color.white);
            checkboxPanel.add(checkboxList[i]);
        }

        // make the list scroll if there are enough checkboxes
        scrollPane = new JScrollPane(checkboxPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                     JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        mainPanel.add(scrollPane);
        mainPanel.setBorder(buildTitledBorder("Check the slices to extract"));
        mainPanel.setPreferredSize(new Dimension(210, 390));

        JPanel checkPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;

        // make check & uncheck buttons for the panel--place inside the above border
        checkButton = new JButton("Select all");
        checkButton.setPreferredSize(new Dimension(95, 30));
        checkButton.setMinimumSize(new Dimension(95, 30));
        checkButton.setFont(serif12B);
        checkPanel.add(checkButton, gbc);
        checkButton.addActionListener(this);
        checkButton.setActionCommand("Check");

        gbc.gridx = 1;
        unCheckButton = new JButton("Clear");
        unCheckButton.setPreferredSize(new Dimension(95, 30));
        unCheckButton.setMinimumSize(new Dimension(95, 30));
        unCheckButton.setFont(serif12B);
        unCheckButton.addActionListener(this);
        unCheckButton.setActionCommand("UnCheck");
        checkPanel.add(unCheckButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        checkEvenButton = new JButton("Check even");
        checkEvenButton.setPreferredSize(new Dimension(95, 30));
        checkEvenButton.setMinimumSize(new Dimension(95, 30));
        checkEvenButton.setFont(serif12B);
        checkPanel.add(checkEvenButton, gbc);
        checkEvenButton.addActionListener(this);
        checkEvenButton.setActionCommand("CheckEven");

        gbc.gridx = 1;
        checkOddButton = new JButton("Check odd");
        checkOddButton.setPreferredSize(new Dimension(95, 30));
        checkOddButton.setMinimumSize(new Dimension(95, 30));
        checkOddButton.setFont(serif12B);
        checkPanel.add(checkOddButton, gbc);
        checkOddButton.addActionListener(this);
        checkOddButton.setActionCommand("CheckOdd");

        JPanel rangePanel = new JPanel();

        rangePanel.setLayout(new BoxLayout(rangePanel, BoxLayout.Y_AXIS));
        rangePanel.setBorder(buildTitledBorder("Range of slices"));
        useRange = new JCheckBox("Specify range of slices", false);
        useRange.addItemListener(this);
        useRange.setFont(serif12B);
        exampleLabel = new JLabel("Enter slice numbers and/or slice ranges.");
        exampleLabel2 = new JLabel("For example, 1,3,5-12");
        exampleLabel.setFont(serif12);
        exampleLabel2.setFont(serif12);
        exampleLabel.setEnabled(false);
        exampleLabel2.setEnabled(false);
        rangeField = new JTextField(10);
        rangeField.setEnabled(false);
        rangePanel.add(useRange);
        rangePanel.add(exampleLabel);
        rangePanel.add(exampleLabel2);
        rangePanel.add(rangeField);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridwidth = 2;
        checkPanel.add(rangePanel, gbc);

        mainPanel.add(checkPanel, BorderLayout.SOUTH);

        JPanel buttonPanel = new JPanel(new FlowLayout());

        buttonPanel.add(buildButtons());
        OKButton.setText("Extract");

        mainDialogPanel.setLayout(new BorderLayout());
        mainDialogPanel.add(mainPanel); // put the main panel into the center of the dialog
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);
        mainDialogPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        getContentPane().add(mainDialogPanel);
        pack();
        setSize(350, 474);
        setVisible(true); // let someone see the dialog.
    }

    /**
     * Once all the necessary variables are set, call the Remove Slices algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        System.gc();

        nSlices = srcImage.getExtents()[2];
        numChecked = 0;

        for (int i = 0; i < nSlices; i++) {

            if (checkListExtract[i]) {
                numChecked++;
            }
        }

        if (numChecked != 0) {
            extractSlicesAlgo = new AlgorithmExtractSlicesVolumes(srcImage, checkListExtract);
            extractSlicesAlgo.addListener(this);
            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (extractSlicesAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                if (!userInterface.isAppFrameVisible()) {
                    extractSlicesAlgo.setProgressBarVisible(false);
                }

                extractSlicesAlgo.run();
            }

        } else if (numChecked == 0) {
            MipavUtil.displayError("No slices were selected!  Select some slices.");
        } else {
            MipavUtil.displayError("All slices are selected!  Unselect some slices.");
        }
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        int i; // counting variable

        // copy the selection of whether or not to remove from the list of boxes:
        checkListExtract = new boolean[nSlices];

        if (!useRange.isSelected()) {

            for (i = 0; i < nSlices; i++) {

                if (checkboxList[i].isSelected()) {
                    checkListExtract[i] = true;
                } else {
                    checkListExtract[i] = false;
                }
            }
        } else {

            for (i = 0; i < nSlices; i++) {
                checkListExtract[i] = false;
            }

            // must parse the range field
            String rangeString = rangeField.getText();
            StringTokenizer tokens = new StringTokenizer(rangeString, ",");
            boolean hasTokens = false;

            while (tokens.hasMoreTokens()) {
                hasTokens = true;

                try {
                    String temp = tokens.nextToken();
                    StringTokenizer tokens2 = new StringTokenizer(temp, "-");
                    String startString = tokens2.nextToken();

                    while (startString.startsWith(" ")) {
                        startString = startString.substring(1, startString.length());
                    }

                    int start = Integer.parseInt(startString);

                    if (!tokens2.hasMoreTokens()) {

                        if (start > checkboxList.length) {
                            MipavUtil.displayError("Must specify valid range.  Ex: 10-20, 25, 30-50");

                            return false;
                        } else {
                            checkListExtract[start - 1] = true;
                        }
                    } else {
                        String endString = tokens2.nextToken();
                        int end = Integer.parseInt(endString);

                        if ((start > end) || (end > checkboxList.length)) {
                            MipavUtil.displayError("Must specify valid range.  Ex: 10-20, 25, 30-50");

                            return false;
                        } else {

                            for (i = start; i < (end + 1); i++) {
                                checkListExtract[i - 1] = true;
                            }
                        }
                    }
                } catch (Exception ex) {
                    MipavUtil.displayError("Must specify valid range.  Ex: 10-20, 25, 30-50");

                    return false;
                }
            }

            if (!hasTokens) {
                return false;
            }
        }

        return true;
    }

}
