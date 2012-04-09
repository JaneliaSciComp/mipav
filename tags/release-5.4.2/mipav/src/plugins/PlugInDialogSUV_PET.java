import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * @version  May 9, 2005
 * @author   William Gandler
 * @see      JDialogBase
 * @see      AlgorithmInterface
 */
public class PlugInDialogSUV_PET extends JDialogBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1059724818991849179L;

    /** 3 types of decay correction. */
    private static final int UNKNOWN = 0;

    /** DOCUMENT ME! */
    private static final int NONE = 1;

    /** DOCUMENT ME! */
    private static final int START = 2;

    /** DOCUMENT ME! */
    private static final int ADMIN = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float acqTime = 0.0f;

    /** DOCUMENT ME! */
    private String acqTimeStr = null;

    /** DOCUMENT ME! */
    private JRadioButton adminButton;

    /** DOCUMENT ME! */
    private int decayCorrection = UNKNOWN;

    /** DOCUMENT ME! */
    private long dose;

    /** DOCUMENT ME! */
    private String doseStr;

    /** DOCUMENT ME! */
    private JRadioButton femaleButton;

    /** DOCUMENT ME! */
    private long halfLife = 0L;

    /** DOCUMENT ME! */
    private String halfLifeStr;

    /** DOCUMENT ME! */
    private boolean haveAcqTime = false;

    /** DOCUMENT ME! */
    private boolean haveDose = false;

    /** DOCUMENT ME! */
    private boolean haveHalfLife = false;

    /** DOCUMENT ME! */
    private boolean haveHeight = false;

    /** DOCUMENT ME! */
    private boolean haveRadStartTime = false;

    /** DOCUMENT ME! */
    private boolean haveSex = false;

    /** DOCUMENT ME! */
    private boolean haveWeight = false;

    /** DOCUMENT ME! */
    private float height;

    /** DOCUMENT ME! */
    private String heightStr;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private boolean male;

    /** DOCUMENT ME! */
    private JRadioButton maleButton;

    /** DOCUMENT ME! */
    private JRadioButton noneButton;

    /** DOCUMENT ME! */
    private String radStartStr = null;

    /** DOCUMENT ME! */
    private float radStartTime = 0.0f;

    /** DOCUMENT ME! */
    private JRadioButton startButton;

    /** DOCUMENT ME! */
    private PlugInAlgorithmSUV_PET suvAlgo = null;

    /** DOCUMENT ME! */
    private JTextField textAcqStartTime;

    /** DOCUMENT ME! */
    private JTextField textDose;

    /** DOCUMENT ME! */
    private JTextField textHalfLife;

    /** DOCUMENT ME! */
    private JTextField textHeight;

    /** DOCUMENT ME! */
    private JTextField textRadStartTime;

    /** DOCUMENT ME! */
    private JTextField textWeight;

    /** DOCUMENT ME! */
    private JRadioButton unknownDecayButton;

    /** DOCUMENT ME! */
    private JRadioButton unknownSexButton;

    /** DOCUMENT ME! */
    private float weight;

    /** DOCUMENT ME! */
    private String weightStr;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for standardized uptake values within VOIs in a 3D PET image.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public PlugInDialogSUV_PET(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        findValues();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // ************************** Event Processing ****************************
    // ************************************************************************

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
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

        if (algorithm instanceof PlugInAlgorithmSUV_PET) {
            image.clearMask();

            dispose();
        }

    } // end AlgorithmPerformed()

    /**
     * Once all the necessary variables are set, call the SUV_PET algorithm.
     */
    protected void callAlgorithm() {

        try {

            suvAlgo = new PlugInAlgorithmSUV_PET(image, male, height, weight, dose, decayCorrection, radStartStr,
                                                 radStartTime, acqTimeStr, acqTime, halfLife);

            // This is very important. Adding this object as a listener allows
            // the algorithm to
            // notify this object when it has completed or failed. See algorithm
            // performed event.
            // This is made possible by implementing AlgorithmedPerformed
            // interface
            suvAlgo.addListener(this);

            createProgressBar(image.getImageName(), " ...", suvAlgo);

            setVisible(false); // Hide dialog

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still
                // have user interface work fast.
                if (suvAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                suvAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("SUV_PET: unable to allocate enough memory");

            return;
        }

    } // end callAlgorithm()

    /**
     * DOCUMENT ME!
     */
    private void findValues() {
        FileInfoBase[] fileInfo;
        fileInfo = image.getFileInfo();

        String sex;
        FileDicomSQ sq;
        Vector<String> display;
        Enumeration<String> f;
        StringTokenizer st;
        String name;
        boolean found;
        String decayString;
        int firstColIndex;
        int secondColIndex;
        int i;
        String hourStr;
        String minuteStr;
        String secondStr;
        boolean haveHour;
        boolean haveMinute;
        boolean haveSecond;
        int hour = 0;
        int minute = 0;
        float second = 0.0f;

        // From Dicom file format tries to read in 4 fields:
        // 1.) Patient's sex at 0010,0040
        // 2.) Patient's height in meters at 0010,1020
        // 3.) Patient's weight in kilograms at 0010,1030
        // 4.) Total Radionuclide dose in becquerels at 0018,1074.
        // Note that 0018,1074 is part of the
        // Radiopharmaceutical Information Sequence at 0054,0016.
        // DICOM standard PS 3.3-2004 Information Object Definitions
        // provides conflicting definitions of Radionuclide Total Dose:
        // C.8.4.10.1.7 Radionuclide Total Dose
        // Radionuclide Total Dose (0018,1074) is the radiopharmaceutical
        // dose administered to the patient measured in MegaBecquerels
        // (Mbq) at the Radiopharmaceutical Start Time.
        // C.8.9.2 PET Isotope Mode
        // Table C.8.-61 PET Isotope Module Attribute
        // Radionuclide Total Dose (0018,1074) The radiopharmaceutical dose
        // administered to the patient measured in Becquerels (Bq) at the
        // Radiopharmaceutical Start Time (0018,1072).
        // David Clunie wrote:
        /*
         * I was as surprised as you probably were when I first encountered this inconsistency and asked the same
         * question.
         *
         * This discrepancy is well known, has been discussed by the various vendor participants, and irritating though it
         * may be, it has been agreed that there will be no change to the standard.
         *
         * In short, in NM instances, the units are MBA, and in PET instances they are in Bq, and that is how the vendors
         * have implemented it and should implement it.
         *
         * This creates a well defined but unambiguous inconsistency between the two IODs.
         *
         * To change this now would invalidate most existing implementations and most images stored in archives, i.e. the
         * cure would be worse than the disease.
         *
         * That is not to say that from time to time vendors do not make an error, say when sending a PET image to a
         * station that was primarily intended for NM, and inadvertently re-encode the values in the wrong units. Folks
         * affected have generally addressed this concern by doing a sanity check on the magnitude of the value.
         *
         * But it is true to say that any vendor making such an error is in violation of the standard, and hopefully will
         * act expeditiously to correct any such error.
         *
         * Sorry if this is not the response that you wanted to hear.
         *
         * David -- Dr. David A. Clunie mailto:dclunie@radpharm.com Chief Technology Officer (RadPharm)
         * http://www.radpharm.com/ Princeton Radiology Pharmaceutical Research Phone 609-936-2600 103 Carnegie Center
         * Drive #300A Fax 609-936-2601 Princeton NJ 08540 http://www.dclunie.com/
         */
        // Jeff Polhammer wrote:
        /*
         * Hi Howard, WG3 decided not to change this, as it would break existing implementations of either the PET or NM
         * IOD. Therefore, a note will be added to warn implementors of the difference in units. -Jeff
         */

        if (fileInfo[0] instanceof FileInfoDicom) {
            FileDicomTagTable firstSliceTagTable = ((FileInfoDicom) fileInfo[0]).getTagTable();

            try {

                if (firstSliceTagTable.getValue("0010,0040") != null) {
                    sex = (String) firstSliceTagTable.getValue("0010,0040");

                    if (sex.length() != 0) {

                        if (sex.substring(0, 1).equalsIgnoreCase("M")) {
                            male = true;
                            haveSex = true;
                        } else if (sex.substring(0, 1).equalsIgnoreCase("F")) {
                            male = false;
                            haveSex = true;
                        } else {
                            Preferences.debug("Sex not M or F.\n");
                        }
                    } else {
                        Preferences.debug("Sex string is zero length\n");
                    }
                } else {
                    Preferences.debug("Sex string is null\n");
                }
            } // try
            catch (NullPointerException noTag) {
                Preferences.debug("Tag (0010,0040) was not found.\n");
            }

            try {

                if (firstSliceTagTable.getValue("0010,1020") != null) {
                    heightStr = (String) firstSliceTagTable.getValue("0010,1020");

                    try {
                        height = Float.valueOf(heightStr).floatValue();

                        if (height <= 0.0f) {
                            Preferences.debug("height <= 0\n");
                        } else {
                            haveHeight = true;
                        }
                    } catch (NumberFormatException err) {
                        Preferences.debug("Height string gives NumberFormatException");
                    }
                } else {
                    Preferences.debug(" Height string is null\n");
                }
            } // try
            catch (NullPointerException noTag) {
                Preferences.debug("Tag (0010,1020) was not found.\n");
            }

            try {

                if (firstSliceTagTable.getValue("0010,1030") != null) {
                    weightStr = (String) firstSliceTagTable.getValue("0010,1030");

                    try {
                        weight = Float.valueOf(weightStr).floatValue();

                        if (weight <= 0.0f) {
                            Preferences.debug("weight <= 0.0\n");
                        } else {
                            haveWeight = true;
                        }
                    } catch (NumberFormatException err) {
                        Preferences.debug("Number format exception on weight string\n");
                    }
                } else {
                    Preferences.debug("Weight string is null\n");
                }
            } // try
            catch (NullPointerException noTag) {
                Preferences.debug("Tag (0010,1030) was not found.\n");
            }

            try {

                if (firstSliceTagTable.get("0054,0016") != null) {

                    if (firstSliceTagTable.getValue("0054,0016") != null) {
                        sq = (FileDicomSQ) firstSliceTagTable.getValue("0054,0016", false);
                        display = sq.getSequenceDisplay();
                        doseStr = "";
                        found = false;

                        for (f = display.elements(); f.hasMoreElements() && !found;) {
                            st = new StringTokenizer((String) f.nextElement(), ";;;");
                            name = (String) st.nextToken();

                            if (name.substring(0, 11).equals("(0018,1074)")) {
                                found = true;
                            }

                            if (st.hasMoreTokens()) {
                                doseStr = (String) st.nextToken();
                            }
                        } // for (f = display.elements();
                          // f.hasMoreElements();)

                        if (!found) {
                            Preferences.debug(" 0018,1074 not found\n");
                        } // if (!found)
                        else { // found
                            doseStr = doseStr.trim();

                            try {
                                dose = Long.valueOf(doseStr).longValue();

                                if (dose <= 0L) {
                                    Preferences.debug("dose <= 0\n");
                                } else {
                                    haveDose = true;
                                }
                            } catch (NumberFormatException err) {
                                Preferences.debug("Number format exception on dose string\n");
                            }
                        } // else found
                    } else {
                        Preferences.debug("Tag (0054,0016) has null value\n");
                    }
                } else {
                    Preferences.debug("Tag (0054,0016) was null.\n");
                }
            } // try
            catch (NullPointerException noTag) {
                Preferences.debug("Tag (0054,0016) was not found.\n");
            }

            try {

                if (firstSliceTagTable.getValue("0054,1102") != null) {
                    decayString = (String) firstSliceTagTable.getValue("0054,1102");
                    decayString = decayString.trim();

                    if (decayString.equalsIgnoreCase("NONE")) {
                        decayCorrection = NONE;
                        Preferences.debug("No decay correction\n");
                    } else if (decayString.equalsIgnoreCase("START")) {
                        decayCorrection = START;
                        Preferences.debug("Decay corrected to acquisition start time\n");
                    } else if (decayString.equalsIgnoreCase("ADMIN")) {
                        decayCorrection = ADMIN;
                        Preferences.debug("Decay corrected to radiopharmaceutical start time\n");
                    } else {
                        Preferences.debug("Unrecognized decay correction string = " + decayString + "\n");
                    }
                } else {
                    Preferences.debug("Decay correction string is null\n");
                }
            } catch (NullPointerException noTag) {
                ViewUserInterface.getReference().setDataText("Decay Correction = \n");
                Preferences.debug("Tag (0054,1102) was not found.\n");
            }

            try {

                if (firstSliceTagTable.get("0054,0016") != null) {

                    if (firstSliceTagTable.getValue("0054,0016") != null) {
                        sq = (FileDicomSQ) firstSliceTagTable.getValue("0054,0016", false);
                        display = sq.getSequenceDisplay();
                        radStartStr = "";
                        found = false;

                        for (f = display.elements(); f.hasMoreElements() && !found;) {
                            st = new StringTokenizer((String) f.nextElement(), ";;;");
                            name = (String) st.nextToken();

                            if (name.substring(0, 11).equals("(0018,1072)")) {
                                found = true;
                            }

                            if (st.hasMoreTokens()) {
                                radStartStr = (String) st.nextToken();
                            }
                        } // for (f = display.elements();
                          // f.hasMoreElements();)

                        if (!found) {
                            Preferences.debug("0018,1072 not found\n");
                        } // if (!found)
                        else { // found
                            radStartStr = radStartStr.trim();
                            Preferences.debug("Radiopharmaceutical Start Time = " + radStartStr + "\n");
                            firstColIndex = -1;
                            secondColIndex = -1;

                            for (i = 0; i < radStartStr.length(); i++) {

                                if (radStartStr.charAt(i) == ':') {

                                    if (firstColIndex == -1) {
                                        firstColIndex = i;
                                    } else {
                                        secondColIndex = i;
                                    }
                                } // if (radStartStr.charAt(i) == ':')
                            } // for (i = 0; i < radStartStr.length(); i++)

                            if ((firstColIndex != -1) && (secondColIndex != -1)) {
                                hourStr = radStartStr.substring(0, firstColIndex);
                                minuteStr = radStartStr.substring(firstColIndex + 1, secondColIndex);
                                secondStr = radStartStr.substring(secondColIndex + 1);
                            } else { // if ((firstColIndex != -1) &&
                                     // (secondIndex != -1))
                                hourStr = radStartStr.substring(0, 2);
                                minuteStr = radStartStr.substring(2, 4);
                                secondStr = radStartStr.substring(4);
                            }

                            haveHour = false;
                            haveMinute = false;
                            haveSecond = false;

                            try {
                                hour = Integer.valueOf(hourStr).intValue();

                                if (hour < 0) {
                                    Preferences.debug("Radiopharmaceutical Start Time Hour < 0\n");
                                } else if (hour > 24) {
                                    Preferences.debug("Radiopharmaceutical Start Time Hour > 24\n");
                                } else {
                                    haveHour = true;
                                }
                            } catch (NumberFormatException err) { // try
                                Preferences.debug("Number format exception on Radiopharmaceutical Start Time hour string\n");
                            }

                            try {
                                minute = Integer.valueOf(minuteStr).intValue();

                                if (minute < 0) {
                                    Preferences.debug("Radiopharmaceutical Start Time Minute < 0\n");
                                } else if (minute > 59) {
                                    Preferences.debug("Radiopharmaceutical Start Time Minute > 59\n");
                                } else {
                                    haveMinute = true;
                                }
                            } catch (NumberFormatException err) { // try
                                Preferences.debug("Number format exception on Radiopharmaceutical Start Time minute string\n");
                            }

                            try {
                                second = Float.valueOf(secondStr).floatValue();

                                if (second < 0.0f) {
                                    Preferences.debug("Radiopharmaceutical Start Time Second < 0.0\n");
                                } else if (second >= 60.0f) {
                                    Preferences.debug("Radiopharmaceutical Start Time Second >= 60.0\n");
                                } else {
                                    haveSecond = true;
                                }
                            } catch (NumberFormatException err) { // try
                                Preferences.debug("Number format exception on Radiopharmaceutical Start Time second string\n");
                            }

                            if (haveHour && haveMinute && haveSecond) {
                                haveRadStartTime = true;
                                radStartTime = (3600 * hour) + (60 * minute) + second;
                            } // if (haveHour && haveMinute && haveSecond)
                        } // else found
                    } else {
                        Preferences.debug("Tag (0054,0016) has null value\n");
                    }
                } else {
                    Preferences.debug("Tag (0054,0016) was null.\n");
                }
            } // try
            catch (NullPointerException noTag) {
                Preferences.debug("Tag (0054,0016) was not found.\n");
            }

            try {

                if (firstSliceTagTable.getValue("0008,0032") != null) {
                    acqTimeStr = (String) firstSliceTagTable.getValue("0008,0032");
                    acqTimeStr = acqTimeStr.trim();
                    Preferences.debug("Acqusition Start Time = " + acqTimeStr + "\n");
                    firstColIndex = -1;
                    secondColIndex = -1;

                    for (i = 0; i < acqTimeStr.length(); i++) {

                        if (acqTimeStr.charAt(i) == ':') {

                            if (firstColIndex == -1) {
                                firstColIndex = i;
                            } else {
                                secondColIndex = i;
                            }
                        } // if (acqTimeStr.charAt(i) == ':')
                    } // for (i = 0; i < acqTimeStr.length(); i++)

                    if ((firstColIndex != -1) && (secondColIndex != -1)) {
                        hourStr = acqTimeStr.substring(0, firstColIndex);
                        minuteStr = acqTimeStr.substring(firstColIndex + 1, secondColIndex);
                        secondStr = acqTimeStr.substring(secondColIndex + 1);
                    } // if ((firstColIndex != -1) && (secondIndex != -1))
                    else {
                        hourStr = acqTimeStr.substring(0, 2);
                        minuteStr = acqTimeStr.substring(2, 4);
                        secondStr = acqTimeStr.substring(4);
                    }

                    haveHour = false;
                    haveMinute = false;
                    haveSecond = false;

                    try {
                        hour = Integer.valueOf(hourStr).intValue();

                        if (hour < 0) {
                            Preferences.debug("Acquisition Time Hour < 0\n");
                        } else if (hour > 24) {
                            Preferences.debug("Acquisition Time Hour > 24\n");
                        } else {
                            haveHour = true;
                        }
                    } // try
                    catch (NumberFormatException err) {
                        Preferences.debug("Number format exception on Acquisition time hour string\n");
                    }

                    try {
                        minute = Integer.valueOf(minuteStr).intValue();

                        if (minute < 0) {
                            Preferences.debug("Acquisition Time Minute < 0\n");
                        } else if (minute > 59) {
                            Preferences.debug("Acquisition Time Minute > 59\n");
                        } else {
                            haveMinute = true;
                        }
                    } // try
                    catch (NumberFormatException err) {
                        Preferences.debug("Number format exception on Acquisition time minute string\n");
                    }

                    try {
                        second = Float.valueOf(secondStr).floatValue();

                        if (second < 0.0f) {
                            Preferences.debug("Acquisition Time Second < 0.0\n");
                        } else if (second >= 60.0f) {
                            Preferences.debug("Acquisition Time Second >= 60.0\n");
                        } else {
                            haveSecond = true;
                        }
                    } catch (NumberFormatException err) { // try
                        Preferences.debug("Number format exception on Acquisition time second string\n");
                    }

                    if (haveHour && haveMinute && haveSecond) {
                        haveAcqTime = true;
                        acqTime = (3600 * hour) + (60 * minute) + second;
                    } // if (haveHour && haveMinute && haveSecond)
                } else {
                    Preferences.debug("Tag (0008,0032) for acquistion Start Time is null\n");
                }
            } catch (NullPointerException noTag) {
                ViewUserInterface.getReference().setDataText("Acquisition Time = \n");
                Preferences.debug("Tag (0008,0032) was not found.\n");
            }

            try {

                if (firstSliceTagTable.get("0054,0016") != null) {

                    if (firstSliceTagTable.getValue("0054,0016") != null) {
                        sq = (FileDicomSQ) firstSliceTagTable.getValue("0054,0016", false);
                        display = sq.getSequenceDisplay();
                        halfLifeStr = "";
                        found = false;

                        for (f = display.elements(); f.hasMoreElements() && !found;) {
                            st = new StringTokenizer((String) f.nextElement(), ";;;");
                            name = (String) st.nextToken();

                            if (name.substring(0, 11).equals("(0018,1075)")) {
                                found = true;
                            }

                            if (st.hasMoreTokens()) {
                                halfLifeStr = (String) st.nextToken();
                            }
                        } // for (f = display.elements();
                          // f.hasMoreElements();)

                        if (!found) {
                            Preferences.debug("0018,1075 not found\n");
                        } // if (!found)
                        else { // found
                            halfLifeStr = halfLifeStr.trim();
                            Preferences.debug("Radionuclide half life = " + halfLifeStr + " seconds\n");

                            try {
                                halfLife = Long.valueOf(halfLifeStr).longValue();

                                if (halfLife <= 0L) {
                                    Preferences.debug("Half Life <= 0\n");
                                } else {
                                    haveHalfLife = true;
                                }
                            } catch (NumberFormatException err) {
                                Preferences.debug("Number format exception on half life string\n");
                            }
                        } // else found
                    } else {
                        Preferences.debug("Tag (0054,0016) has null value\n");
                    }
                } else {
                    Preferences.debug("Tag (0054,0016) was null.\n");
                }
            } // try
            catch (NullPointerException noTag) {
                Preferences.debug("Tag (0054,0016) was not found.\n");
            }

        } // if ( fileInfo[0] instanceof FileInfoDicom )
        else {
            Preferences.debug("Not dicom file format");
        }

    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Standardized Uptake Values");

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;

        int yPos = 0;
        gbc.gridx = 0;
        gbc.gridy = yPos++;

        JPanel mainPanel = new JPanel(new GridBagLayout());
        mainPanel.setForeground(Color.black);
        mainPanel.setBorder(buildTitledBorder(image.getImageName()));

        JLabel labelVOI = new JLabel("Outline regions with VOIs");
        labelVOI.setForeground(Color.black);
        labelVOI.setFont(serif12);
        mainPanel.add(labelVOI, gbc);

        ButtonGroup sexGroup = new ButtonGroup();

        if (haveSex && male) {
            maleButton = new JRadioButton("Male", true);
        } else {
            maleButton = new JRadioButton("Male", false);
        }

        maleButton.setFont(serif12);
        maleButton.setForeground(Color.black);
        sexGroup.add(maleButton);
        gbc.gridy = yPos;
        mainPanel.add(maleButton, gbc);

        if (haveSex && !male) {
            femaleButton = new JRadioButton("Female", true);
        } else {
            femaleButton = new JRadioButton("Female", false);
        }

        femaleButton.setFont(serif12);
        femaleButton.setForeground(Color.black);
        sexGroup.add(femaleButton);
        gbc.gridx = 1;
        gbc.gridy = yPos;
        mainPanel.add(femaleButton, gbc);

        if (!haveSex) {
            unknownSexButton = new JRadioButton("Unknown", true);
        } else {
            unknownSexButton = new JRadioButton("Unknown", false);
        }

        unknownSexButton.setFont(serif12);
        unknownSexButton.setForeground(Color.black);
        sexGroup.add(unknownSexButton);
        gbc.gridx = 2;
        gbc.gridy = yPos++;
        mainPanel.add(unknownSexButton, gbc);

        JLabel labelHeight = new JLabel("Height in meters");
        labelHeight.setFont(serif12);
        labelHeight.setForeground(Color.black);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(labelHeight, gbc);

        textHeight = new JTextField(15);
        textHeight.setFont(serif12);
        textHeight.setForeground(Color.black);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(textHeight, gbc);

        if (haveHeight) {
            textHeight.setText(heightStr);
        }

        JLabel labelWeight = new JLabel("Weight in kilograms");
        labelWeight.setFont(serif12);
        labelWeight.setForeground(Color.black);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(labelWeight, gbc);

        textWeight = new JTextField(15);
        textWeight.setFont(serif12);
        textWeight.setForeground(Color.black);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(textWeight, gbc);

        if (haveWeight) {
            textWeight.setText(weightStr);
        }

        JLabel labelDose = new JLabel("Dose in becquerels");
        labelDose.setFont(serif12);
        labelDose.setForeground(Color.black);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(labelDose, gbc);

        textDose = new JTextField(15);
        textDose.setFont(serif12);
        textDose.setForeground(Color.black);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(textDose, gbc);

        if (haveDose) {
            textDose.setText(doseStr);
        }

        ButtonGroup decayGroup = new ButtonGroup();

        if (decayCorrection == NONE) {
            noneButton = new JRadioButton("No decay correction", true);
        } else {
            noneButton = new JRadioButton("No decay correction", false);
        }

        noneButton.setFont(serif12);
        noneButton.setForeground(Color.black);
        decayGroup.add(noneButton);
        gbc.gridx = 0;
        gbc.gridy = yPos++;
        mainPanel.add(noneButton, gbc);

        if (decayCorrection == START) {
            startButton = new JRadioButton("Decay corrected to acquisition start time", true);
        } else {
            startButton = new JRadioButton("Decay corrected to acquisition start time", false);
        }

        startButton.setFont(serif12);
        startButton.setForeground(Color.black);
        decayGroup.add(startButton);
        gbc.gridx = 0;
        gbc.gridy = yPos++;
        mainPanel.add(startButton, gbc);

        if (decayCorrection == ADMIN) {
            adminButton = new JRadioButton("Decay corrected to radiopharmaceutical start time", true);
        } else {
            adminButton = new JRadioButton("Decay corrected to radiopharmaceutical start time", false);
        }

        adminButton.setFont(serif12);
        adminButton.setForeground(Color.black);
        decayGroup.add(adminButton);
        gbc.gridx = 0;
        gbc.gridy = yPos++;
        mainPanel.add(adminButton, gbc);

        if (decayCorrection == UNKNOWN) {
            unknownDecayButton = new JRadioButton("Unknown decay correction status", true);
        } else {
            unknownDecayButton = new JRadioButton("Unknown decay correction status", false);
        }

        unknownDecayButton.setFont(serif12);
        unknownDecayButton.setForeground(Color.black);
        decayGroup.add(unknownDecayButton);
        gbc.gridx = 0;
        gbc.gridy = yPos++;
        mainPanel.add(unknownDecayButton, gbc);

        JLabel labelRadStartTime;

        if (haveRadStartTime) {
            labelRadStartTime = new JLabel("Radiopharmaceutical start time");
        } else {
            labelRadStartTime = new JLabel("Radiopharmaceutical start time hh:mm:ss.ss");
        }

        labelRadStartTime.setFont(serif12);
        labelRadStartTime.setForeground(Color.black);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(labelRadStartTime, gbc);

        textRadStartTime = new JTextField(15);
        textRadStartTime.setFont(serif12);
        textRadStartTime.setForeground(Color.black);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(textRadStartTime, gbc);

        if (haveRadStartTime) {
            textRadStartTime.setText(radStartStr);
        }

        JLabel labelAcqStartTime;

        if (haveAcqTime) {
            labelAcqStartTime = new JLabel("Acquisition start time");
        } else {
            labelAcqStartTime = new JLabel("Acquisition start time hh:mm:ss.ss");
        }

        labelAcqStartTime.setFont(serif12);
        labelAcqStartTime.setForeground(Color.black);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(labelAcqStartTime, gbc);

        textAcqStartTime = new JTextField(15);
        textAcqStartTime.setFont(serif12);
        textAcqStartTime.setForeground(Color.black);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(textAcqStartTime, gbc);

        if (haveAcqTime) {
            textAcqStartTime.setText(acqTimeStr);
        }

        JLabel labelHalfLife = new JLabel("Radionuclide half life in seconds");
        labelHalfLife.setFont(serif12);
        labelHalfLife.setForeground(Color.black);
        gbc.gridx = 0;
        gbc.gridy = yPos;
        mainPanel.add(labelHalfLife, gbc);

        textHalfLife = new JTextField(15);
        textHalfLife.setFont(serif12);
        textHalfLife.setForeground(Color.black);
        gbc.gridx = 1;
        gbc.gridy = yPos++;
        mainPanel.add(textHalfLife, gbc);

        if (haveHalfLife) {
            textHalfLife.setText(halfLifeStr);
        }

        getContentPane().add(mainPanel, BorderLayout.CENTER);

        // Build the Panel that holds the OK and CANCEL Buttons
        JPanel OKCancelPanel = new JPanel();

        // size and place the OK button
        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        // size and place the CANCEL button
        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();

    } // end init()

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        ViewVOIVector VOIs;
        int nVOIs;
        int nBoundingVOIs = 0;
        int i;
        String tmpStr;
        int firstColIndex;
        int secondColIndex;
        String hourStr;
        String minuteStr;
        String secondStr;
        boolean haveHour;
        boolean haveMinute;
        boolean haveSecond;
        int hour = 0;
        int minute = 0;
        float second = 0.0f;

        VOIs = image.getVOIs();
        nVOIs = VOIs.size();

        if (nVOIs == 0) {
            MipavUtil.displayError("No VOIs were present");

            return false;
        }

        for (i = 0; i < nVOIs; i++) {

            if (VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) {
                nBoundingVOIs++;
            }
        }

        if (nBoundingVOIs == 0) {
            MipavUtil.displayError("No countour VOIs were present");

            return false;
        }

        if (unknownSexButton.isSelected()) {
            unknownSexButton.requestFocus();
            MipavUtil.displayError("Male or female sex must be specified");

            return false;

        }

        male = maleButton.isSelected();

        tmpStr = textHeight.getText();

        if (testParameter(tmpStr, Float.MIN_VALUE, Float.MAX_VALUE)) {
            height = Float.valueOf(tmpStr).floatValue();
        } else {
            textHeight.requestFocus();
            textHeight.selectAll();

            return false;
        }

        tmpStr = textWeight.getText();

        if (testParameter(tmpStr, Float.MIN_VALUE, Float.MAX_VALUE)) {
            weight = Float.valueOf(tmpStr).floatValue();
        } else {
            textWeight.requestFocus();
            textWeight.selectAll();

            return false;
        }

        tmpStr = textDose.getText();

        if (testParameter(tmpStr, Float.MIN_VALUE, Float.MAX_VALUE)) {
            dose = Long.valueOf(tmpStr).longValue();
        } else {
            textDose.requestFocus();
            textDose.selectAll();

            return false;
        }

        if (unknownDecayButton.isSelected()) {
            unknownDecayButton.requestFocus();
            MipavUtil.displayError("Decay correction status must be specified");

            return false;
        }

        if (noneButton.isSelected()) {
            decayCorrection = NONE;
        } else if (startButton.isSelected()) {
            decayCorrection = START;
        } else {
            decayCorrection = ADMIN;
        }

        if (decayCorrection == START) {
            radStartStr = textRadStartTime.getText();
            radStartStr = radStartStr.trim();
            Preferences.debug("Radiopharmaceutical Start Time = " + radStartStr + "\n");
            firstColIndex = -1;
            secondColIndex = -1;

            for (i = 0; i < radStartStr.length(); i++) {

                if (radStartStr.charAt(i) == ':') {

                    if (firstColIndex == -1) {
                        firstColIndex = i;
                    } else {
                        secondColIndex = i;
                    }
                } // if (radStartStr.charAt(i) == ':')
            } // for (i = 0; i < radStartStr.length(); i++)

            if ((firstColIndex != -1) && (secondColIndex != -1)) {
                hourStr = radStartStr.substring(0, firstColIndex);
                minuteStr = radStartStr.substring(firstColIndex + 1, secondColIndex);
                secondStr = radStartStr.substring(secondColIndex + 1);
            } else { // if ((firstColIndex != -1) && (secondIndex != -1))
                hourStr = radStartStr.substring(0, 2);
                minuteStr = radStartStr.substring(2, 4);
                secondStr = radStartStr.substring(4);
            }

            haveHour = false;
            haveMinute = false;
            haveSecond = false;

            try {
                hour = Integer.valueOf(hourStr).intValue();

                if (hour < 0) {
                    MipavUtil.displayError("Radiopharmaceutical Start Time Hour must be >= 0\n");
                } else if (hour > 24) {
                    MipavUtil.displayError("Radiopharmaceutical Start Time Hour must be <= 24\n");
                } else {
                    haveHour = true;
                }
            } catch (NumberFormatException err) { // try
                MipavUtil.displayError("Number format exception on Radiopharmaceutical Start Time hour string\n");
            }

            try {
                minute = Integer.valueOf(minuteStr).intValue();

                if (minute < 0) {
                    MipavUtil.displayError("Radiopharmaceutical Start Time Minute must be >= 0\n");
                } else if (minute > 59) {
                    MipavUtil.displayError("Radiopharmaceutical Start Time Minute must be <= 59\n");
                } else {
                    haveMinute = true;
                }
            } catch (NumberFormatException err) { // try
                MipavUtil.displayError("Number format exception on Radiopharmaceutical Start Time minute string\n");
            }

            try {
                second = Float.valueOf(secondStr).floatValue();

                if (second < 0.0f) {
                    MipavUtil.displayError("Radiopharmaceutical Start Time Second must be >= 0.0\n");
                } else if (second >= 60.0f) {
                    MipavUtil.displayError("Radiopharmaceutical Start Time Second must be < 60.0\n");
                } else {
                    haveSecond = true;
                }
            } catch (NumberFormatException err) { // try
                MipavUtil.displayError("Number format exception on Radiopharmaceutical Start Time second string\n");
            }

            if (haveHour && haveMinute && haveSecond) {
                radStartTime = (3600 * hour) + (60 * minute) + second;
            } // if (haveHour && haveMinute && haveSecond)
            else {
                textRadStartTime.requestFocus();
                textRadStartTime.selectAll();

                return false;
            }

            acqTimeStr = acqTimeStr.trim();
            Preferences.debug("Acqusition Start Time = " + acqTimeStr + "\n");
            firstColIndex = -1;
            secondColIndex = -1;

            for (i = 0; i < acqTimeStr.length(); i++) {

                if (acqTimeStr.charAt(i) == ':') {

                    if (firstColIndex == -1) {
                        firstColIndex = i;
                    } else {
                        secondColIndex = i;
                    }
                } // if (acqTimeStr.charAt(i) == ':')
            } // for (i = 0; i < acqTimeStr.length(); i++)

            if ((firstColIndex != -1) && (secondColIndex != -1)) {
                hourStr = acqTimeStr.substring(0, firstColIndex);
                minuteStr = acqTimeStr.substring(firstColIndex + 1, secondColIndex);
                secondStr = acqTimeStr.substring(secondColIndex + 1);
            } // if ((firstColIndex != -1) && (secondIndex != -1))
            else {
                hourStr = acqTimeStr.substring(0, 2);
                minuteStr = acqTimeStr.substring(2, 4);
                secondStr = acqTimeStr.substring(4);
            }

            haveHour = false;
            haveMinute = false;
            haveSecond = false;

            try {
                hour = Integer.valueOf(hourStr).intValue();

                if (hour < 0) {
                    MipavUtil.displayError("Acquisition Time Hour must be >= 0\n");
                } else if (hour > 24) {
                    MipavUtil.displayError("Acquisition Time Hour must be <= 24\n");
                } else {
                    haveHour = true;
                }
            } // try
            catch (NumberFormatException err) {
                MipavUtil.displayError("Number format exception on Acquisition time hour string\n");
            }

            try {
                minute = Integer.valueOf(minuteStr).intValue();

                if (minute < 0) {
                    MipavUtil.displayError("Acquisition Time Minute must be >= 0\n");
                } else if (minute > 59) {
                    MipavUtil.displayError("Acquisition Time Minute must be <= 59\n");
                } else {
                    haveMinute = true;
                }
            } // try
            catch (NumberFormatException err) {
                MipavUtil.displayError("Number format exception on Acquisition time minute string\n");
            }

            try {
                second = Float.valueOf(secondStr).floatValue();

                if (second < 0.0f) {
                    MipavUtil.displayError("Acquisition Time Second must be >= 0.0\n");
                } else if (second >= 60.0f) {
                    MipavUtil.displayError("Acquisition Time Second must be < 60.0\n");
                } else {
                    haveSecond = true;
                }
            } catch (NumberFormatException err) { // try
                MipavUtil.displayError("Number format exception on Acquisition time second string\n");
            }

            if (haveHour && haveMinute && haveSecond) {
                haveAcqTime = true;
                acqTime = (3600 * hour) + (60 * minute) + second;
            } // if (haveHour && haveMinute && haveSecond)
            else {
                textAcqStartTime.requestFocus();
                textAcqStartTime.selectAll();

                return false;
            }

            halfLifeStr = textHalfLife.getText();
            halfLifeStr = halfLifeStr.trim();
            Preferences.debug("Radionuclide half life = " + halfLifeStr + " seconds\n");

            try {
                halfLife = Long.valueOf(halfLifeStr).longValue();

                if (halfLife <= 0L) {
                    MipavUtil.displayError("Half life must be greater than zero");
                    textHalfLife.requestFocus();
                    textHalfLife.selectAll();

                    return false;
                }
            } catch (NumberFormatException err) {
                MipavUtil.displayError("Number format exception on half life string\n");
                textHalfLife.requestFocus();
                textHalfLife.selectAll();

                return false;
            }

        } // if (decayCorrection == START)

        return true;
    } // setVariables()

}
