package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.file.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.*;
import java.util.*;

import javax.swing.*;


/**
 * Simple dialog to tell the program important information necessary for saving the DICOM file.
 * 
 * @version 1.0 Aug 1, 2000
 * @author Neva Cherniavsky
 * @see FileDicom
 */
public class JDialogSaveDicom extends JDialogBase {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7303665224054251504L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JButton autofillButton;

    /** DOCUMENT ME! */
    private final FileInfoDicom dicomFileInfo;

    /** DOCUMENT ME! */
    private final FileInfoBase fileInfo;

    /** DOCUMENT ME! */
    private JButton fillButton;

    /** DOCUMENT ME! */
    private final GridBagConstraints gbc;

    /** DOCUMENT ME! */
    private final String[] lateralities = {"Unknown", "Left", "Right"};

    /** DOCUMENT ME! */
    private JComboBox laterality;

    /** DOCUMENT ME! */
    private final GridBagLayout layout;

    /** DOCUMENT ME! */
    private final String[] modalities = {"Biomagnetic Imaging", "Color Flow Doppler", "Computed Radiography",
            "Computed Tomography", "Duplex Doppler", "Diaphanography", "Digital Radiography", "Endoscopy",
            "General Microscopy", "Hardcopy", "Intraoral Radiography", "Laser Surface Scan", "MR Angiography",
            "Mammography", "Magnetic Resonance", "MR Spectroscopy", "Nuclear Medicine", "Other", "PET",
            "Panoramic XRay", "Radio Fluoroscopy", "Radiographic Imaging", "Radiotherapy Dose", "Radiotherapy Image",
            "Radiotherapy Plan", "Radiotherapy Record", "Radiotherapy Structure", "Slide Microscopy", "SPECT",
            "Thermography", "Ultrasound", "XRay Angiography", "External Photography"};

    /** DOCUMENT ME! */
    private final String[] parts = {"Unknown", "Skull", "CSpine", "TSpine", "LSpine", "SSpine", "Coccyx", "Chest",
            "Clavicle", "Breast", "Abdomen", "Pelvis", "Hip", "Shoulder", "Elbow", "Knee", "Ankle", "Hand", "Foot",
            "Extremity", "Head", "Heart", "Neck", "Leg", "Arm", "Jaw"};

    /** DOCUMENT ME! */
    private JTextField patientBirthDate;

    /** DOCUMENT ME! */
    private JTextField patientBirthTime;

    /** DOCUMENT ME! */
    private JTextField patientComments;

    /** DOCUMENT ME! */
    private JTextField patientEthnicGroup;

    /** DOCUMENT ME! */
    private JTextField patientID;

    /** DOCUMENT ME! */
    private JTextField patientName;

    /** DOCUMENT ME! */
    private JTextField patientOrientation;

    /** DOCUMENT ME! */
    private JTextField patientOtherIDs;

    /** DOCUMENT ME! */
    private JTextField patientOtherNames;

    /** DOCUMENT ME! */
    private JPanel patientPanel;

    /** DOCUMENT ME! */
    private JComboBox patientSex;

    /** DOCUMENT ME! */
    private final String[] positions = {"Unknown", "Head-First Prone", "Head-First Supine", "Feet First-Prone",
            "Feet First-Supine", "HF-Decubitus Right", "HF-Decubitus Left", "FF-Decubitus Right", "FF-Decubitus Left"};

    /** DOCUMENT ME! */
    private JPanel requiredPanel;

    /** DOCUMENT ME! */
    private JComboBox seriesBody;

    /** DOCUMENT ME! */
    private JTextField seriesDate;

    /** DOCUMENT ME! */
    private JTextField seriesDescrip;

    /** DOCUMENT ME! */
    private JTextField seriesLarge;

    /** DOCUMENT ME! */
    private JComboBox seriesMod;

    /** DOCUMENT ME! */
    private JTextField seriesNo;

    /** DOCUMENT ME! */
    private JTextField seriesOp;

    /** DOCUMENT ME! */
    private JPanel seriesPanel;

    /** DOCUMENT ME! */
    private JTextField seriesPerfPhy;

    /** DOCUMENT ME! */
    private JComboBox seriesPos;

    /** DOCUMENT ME! */
    private JTextField seriesProtocol;

    /** DOCUMENT ME! */
    private JTextField seriesSmall;

    /** DOCUMENT ME! */
    private JTextField seriesStepDate;

    /** DOCUMENT ME! */
    private JTextField seriesStepDescrip;

    /** DOCUMENT ME! */
    private JTextField seriesStepID;

    /** DOCUMENT ME! */
    private JTextField seriesStepTime;

    /** DOCUMENT ME! */
    private JTextField seriesTime;

    /** DOCUMENT ME! */
    private JTextField seriesUID;

    /** DOCUMENT ME! */
    private final String[] sexes = {"Unknown", "Male", "Female", "Other"};

    /** DOCUMENT ME! */
    private JTextField studyAccNumber;

    /** DOCUMENT ME! */
    private JTextField studyAge;

    /** DOCUMENT ME! */
    private JTextField studyDate;

    /** DOCUMENT ME! */
    private JTextField studyDescrip;

    /** DOCUMENT ME! */
    private JTextField studyDiag;

    /** DOCUMENT ME! */
    private JTextField studyHist;

    /** DOCUMENT ME! */
    private JTextField studyID;

    /** DOCUMENT ME! */
    private JTextField studyOcc;

    /** DOCUMENT ME! */
    private JPanel studyPanel;

    /** DOCUMENT ME! */
    private JTextField studyPhyRead;

    /** DOCUMENT ME! */
    private JTextField studyPhyRec;

    /** DOCUMENT ME! */
    private JTextField studyRefPhy;

    /** DOCUMENT ME! */
    private JTextField studySize;

    /** DOCUMENT ME! */
    private JTextField studyTime;

    /** DOCUMENT ME! */
    private JTextField studyUID;

    /** DOCUMENT ME! */
    private JTextField studyWeight;

    /** DOCUMENT ME! */
    private final JTabbedPane tabPane;

    /**
     * DICOM tags extracted from the image we want to save. Example: dicom_0xNNNN el_0xNNNN tags stored in MINC headers.
     */
    private Hashtable<String, String> tagsImportedFromNonDicomImage;

    /** The additional tags list is a list of tags the DTI group has requested. */
    private Hashtable<String,JComponent> tagsList;
    private Hashtable<String,String> additionalTagsList;

    /** DOCUMENT ME! */
    private final ViewUserInterface UI;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Creates dialog for setting tag information for DICOM image.
     * 
     * @param theParentFrame Parent frame.
     * @param _fileInfo File info object to get initialization info from.
     * @param dicomInfo Dicom file info object.
     * @param isScriptRunning Whether this dialog is being instantiated as part of the running of a script.
     */
    public JDialogSaveDicom(final Frame theParentFrame, final FileInfoBase _fileInfo, final FileInfoDicom dicomInfo,
            final boolean isScriptRunning) {
        super(theParentFrame, true);

        UI = ViewUserInterface.getReference();
        fileInfo = _fileInfo;
        dicomFileInfo = dicomInfo;

        setTitle("Attributes to save");
        setResizable(true);
        cancelFlag = false;

        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.insets = new Insets(1, 1, 1, 1);
        layout = new GridBagLayout();

        createRequiredPanel();
        createSeriesPanel();
        createPatientPanel();
        createStudyPanel();

        createHashtable();

        tabPane = new JTabbedPane();

        // tabPane.setLayout(new BorderLayout());
        tabPane.addTab("Required", requiredPanel);
        tabPane.addTab("Patient", patientPanel);
        tabPane.addTab("Study", studyPanel);
        tabPane.addTab("Series", seriesPanel);

        final JPanel buttonPanel = new JPanel();
        buildOKButton();
        buildCancelButton();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);
        buttonPanel.add(createButton("Fill tags from file", fillButton));
        buttonPanel.add(createButton("Autofill required tags", autofillButton));

        getContentPane().setLayout(new BorderLayout());
        getContentPane().add(tabPane, BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        validate();

        setModalityChooser(fileInfo.getModality());

        // try to extract out dicom-converted tags which may be buried in the minc header
        if (fileInfo.getFileFormat() == FileUtility.MINC) {
            tagsImportedFromNonDicomImage = ((FileInfoMinc) fileInfo).convertTagsToTable();
            fillDataFromTable(tagsImportedFromNonDicomImage);

            // remove tags which were used to fill the GUI in fillDataFromTable(). the rest will be blindly imported
            // into the dicom fileinfo later..
            final Enumeration<String> keys = tagsImportedFromNonDicomImage.keys();
            String tag;

            while (keys.hasMoreElements()) {
                tag = keys.nextElement();

                // handle chooser fields differently
                if (tag.equals("(0008,0060)") || tag.equals("(0010,0040)") || tag.equals("(0018,0015)")
                        || tag.equals("(0018,5100)")) {

                    // don't blindly import tags which are in the GUI
                    tagsImportedFromNonDicomImage.remove(tag);
                } else if (tagsList.get(tag) != null) {

                    // don't blindly import tags which are in the GUI
                    tagsImportedFromNonDicomImage.remove(tag);
                }
            }
        } else if (fileInfo.getFileFormat() == FileUtility.MINC_HDF) {
            tagsImportedFromNonDicomImage = ((FileInfoMincHDF) fileInfo).getDicomTable();
            fillDataFromTable(tagsImportedFromNonDicomImage);

            // remove tags which were used to fill the GUI in fillDataFromTable(). the rest will be blindly imported
            // into the dicom fileinfo later..
            final Enumeration<String> keys = tagsImportedFromNonDicomImage.keys();
            String tag;

            while (keys.hasMoreElements()) {
                tag = keys.nextElement();

                // handle chooser fields differently
                if (tag.equals("(0008,0060)") || tag.equals("(0010,0040)") || tag.equals("(0018,0015)")
                        || tag.equals("(0018,5100)")) {

                    // don't blindly import tags which are in the GUI
                    tagsImportedFromNonDicomImage.remove(tag);
                } else if (tagsList.get(tag) != null) {

                    // don't blindly import tags which are in the GUI
                    tagsImportedFromNonDicomImage.remove(tag);
                }
            }
        }

        // if we are running a script, auto-fill the required tags and skip the dialog
        if (isScriptRunning) {
            autofillRequiredFields();
            setVisible(false);
            actionPerformed(new ActionEvent(this, 0, "OK"));
        } else {
            setVisible(true);
        }
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and sets the information, doing error checking for every tag.
     * 
     * @param event Event that triggers this function.
     */
    public void actionPerformed(final ActionEvent event) {

        if (event.getActionCommand().equals("OK")) {

            if (checkTag(studyUID.getText(), "UI") && checkTag(seriesUID.getText(), "UI")
                    && checkTag(patientName.getText(), "PN") && checkTag(patientID.getText(), "LO")
                    && checkTag(patientBirthDate.getText(), "DA") && checkTag(patientBirthDate.getText(), "TM")
                    && checkTag(patientOtherIDs.getText(), "LO") && checkTag(patientOtherNames.getText(), "PN")
                    && checkTag(patientEthnicGroup.getText(), "SH") && checkTag(patientComments.getText(), "LT")
                    && checkTag(patientOrientation.getText(), "Orient") && checkTag(studyDate.getText(), "DA")
                    && checkTag(studyTime.getText(), "TM") && checkTag(studyRefPhy.getText(), "PN")
                    && checkTag(studyID.getText(), "SH") && checkTag(studyAccNumber.getText(), "SH")
                    && checkTag(studyDescrip.getText(), "LO") && checkTag(studyPhyRec.getText(), "PN")
                    && checkTag(studyPhyRead.getText(), "PN") && checkTag(studyDiag.getText(), "LO")
                    && checkTag(studyAge.getText(), "AS") && checkTag(studyWeight.getText(), "DS")
                    && checkTag(studySize.getText(), "DS") && checkTag(studyOcc.getText(), "SH")
                    && checkTag(studyHist.getText(), "LT") && checkTag(seriesNo.getText(), "IS")
                    && checkTag(seriesDate.getText(), "DA") && checkTag(seriesTime.getText(), "TM")
                    && checkTag(seriesPerfPhy.getText(), "PN") && checkTag(seriesProtocol.getText(), "LO")
                    && checkTag(seriesDescrip.getText(), "LO") && checkTag(seriesOp.getText(), "PN")
                    && checkTag(seriesSmall.getText(), "USorSS") && checkTag(seriesLarge.getText(), "USorSS")
                    && checkTag(seriesStepID.getText(), "SH") && checkTag(seriesStepDate.getText(), "DA")
                    && checkTag(seriesStepTime.getText(), "TM") && checkTag(seriesStepDescrip.getText(), "LO")) {

                // blindly import any tags which were exported from the image we want to save (shouldn't be any gui
                // tags, though)
                if (tagsImportedFromNonDicomImage != null) {
                    final Enumeration<String> keys = tagsImportedFromNonDicomImage.keys();
                    String tag;
                    String value;
                    Object[] valueArray;

                    while (keys.hasMoreElements()) {
                        tag = keys.nextElement();
                        value = tagsImportedFromNonDicomImage.get(tag);
                        tag = tag.replaceAll("[()]", "");
                        tag = tag.toUpperCase();

                        // special case for a US tag with VM 4 (0018,1310) which the minc people convert to a strange
                        // format "0x80, 0, 0, 0, 0, 0, 0x80, 0" -> "128, 0, 0, 128"
                        // also handles spliting the tag when the values are delimited by backslashes (as normal in
                        // dicom)
                        if (tag.equals("0018,1310")) {
                            final String[] vals = value.split(", ");
                            valueArray = new Short[vals.length / 2];

                            try {
                                valueArray[0] = Short.decode(vals[0]);
                            } catch (final NumberFormatException nfe) {
                                MipavUtil
                                        .displayError("Error parsing short values from minc-stored dicom tag (0018,1310): "
                                                + vals[0]);
                            }

                            for (int i = 1; i < vals.length; i++) {

                                if ( (i % 2) == 0) {

                                    try {
                                        valueArray[i / 2] = Short.decode(vals[i]);
                                    } catch (final NumberFormatException nfe) {
                                        MipavUtil
                                                .displayError("Error parsing short values from minc-stored dicom tag (0018,1310): "
                                                        + vals[i]);

                                        break;
                                    }
                                }
                            }

                            try {
                                dicomFileInfo.getTagTable().setValue(tag, valueArray);
                            } catch (final Exception e) {
                                dicomFileInfo.getTagTable().removeTag(tag);

                                Preferences.debug("Error tranferring tag from non-dicom image to dicom: \n",
                                        Preferences.DEBUG_FILEIO);
                                Preferences.debug("\t" + tag + " = " + value + "\n", Preferences.DEBUG_FILEIO);
                                Preferences.debug("\terror: " + e.getMessage() + "\n", Preferences.DEBUG_FILEIO);
                            }
                        } else {

                            try {
                                dicomFileInfo.getTagTable().setValue(tag, value);
                            } catch (final Exception e) {
                                dicomFileInfo.getTagTable().removeTag(tag);

                                Preferences.debug("Error tranferring tag from non-dicom image to dicom: \n",
                                        Preferences.DEBUG_FILEIO);
                                Preferences.debug("\t" + tag + " = " + value + "\n", Preferences.DEBUG_FILEIO);
                                Preferences.debug("\terror: " + e.getMessage() + "\n", Preferences.DEBUG_FILEIO);
                            }
                        }
                    }
                }

                // dicomFileInfo.setVRType(dicomFileInfo.EXPLICIT);
                dicomFileInfo.getTagTable().setValue("0002,0000", new Integer(152), 4);

                final Byte[] version = new Byte[2];
                version[0] = new Byte((byte) 1);
                version[1] = new Byte((byte) 0);
                dicomFileInfo.getTagTable().setValue("0002,0001", version, 2);
                dicomFileInfo.getTagTable().setValue("0002,0002", "1.2.840.10008.5.1.4.1.1.7 ", 26); // Secondary
                // Capture SOP
                // UID
                dicomFileInfo.getTagTable().setValue("0002,0003",
                        FileInfoDicom.generateNewTagValue("0002,0003", "1.2.840.999999999999999999"), 26); // bogus
                // SOP
                // Instance UID
                dicomFileInfo.getTagTable().setValue("0002,0010", "1.2.840.10008.1.2 ", 18); // Little Endian
                // transfer
                // syntax
                dicomFileInfo.getTagTable().setValue("0002,0012", "1.2.840.34379.17", 16); // bogus Implementation UID
                // made up by Matt
                dicomFileInfo.getTagTable().setValue("0002,0013", "MIPAV--NIH", 10); //

                dicomFileInfo.getTagTable().setValue("0008,0016", "1.2.840.10008.5.1.4.1.1.7 ", 26); // Secondary
                // Capture UID
                dicomFileInfo.getTagTable().setValue("0008,0018",
                        FileInfoDicom.generateNewTagValue("0008,0018", "1.2.840.999999999999999999"), 26); // bogus
                // SOP
                // Instance UID

                // all secondary capture info is installed by FileDicom.writeImage(), under the assumption that all
                // saves must have been modified (and need that stuff)
                dicomFileInfo.getTagTable()
                        .setValue("0010,0010", patientName.getText(), patientName.getText().length());
                dicomFileInfo.getTagTable().setValue("0010,0020", patientID.getText(), patientID.getText().length());
                dicomFileInfo.getTagTable().setValue("0010,0030", patientBirthDate.getText(),
                        patientBirthDate.getText().length());
                dicomFileInfo.getTagTable().setValue("0010,0032", patientBirthTime.getText(),
                        patientBirthTime.getText().length());
                dicomFileInfo.getTagTable().setValue("0010,1000", patientOtherIDs.getText(),
                        patientOtherIDs.getText().length());
                dicomFileInfo.getTagTable().setValue("0010,1001", patientOtherNames.getText(),
                        patientOtherNames.getText().length());
                dicomFileInfo.getTagTable().setValue("0010,2160", patientEthnicGroup.getText(),
                        patientEthnicGroup.getText().length());
                dicomFileInfo.getTagTable().setValue("0010,4000", patientComments.getText(),
                        patientComments.getText().length());
                dicomFileInfo.getTagTable().setValue("0020,0020", patientOrientation.getText(),
                        patientOrientation.getText().length());
                dicomFileInfo.getTagTable().setValue("0020,000D", studyUID.getText(), studyUID.getText().length());
                dicomFileInfo.getTagTable().setValue("0020,0010", studyID.getText(), studyID.getText().length());
                dicomFileInfo.getTagTable().setValue("0008,0020", studyDate.getText(), studyDate.getText().length());
                dicomFileInfo.getTagTable().setValue("0008,0030", studyTime.getText(), studyTime.getText().length());
                dicomFileInfo.getTagTable().setValue("0008,0050", studyAccNumber.getText(),
                        studyAccNumber.getText().length());
                dicomFileInfo.getTagTable().setValue("0008,1030", studyDescrip.getText(),
                        studyDescrip.getText().length());
                dicomFileInfo.getTagTable()
                        .setValue("0008,0090", studyRefPhy.getText(), studyRefPhy.getText().length());
                dicomFileInfo.getTagTable()
                        .setValue("0008,1048", studyPhyRec.getText(), studyPhyRec.getText().length());
                dicomFileInfo.getTagTable().setValue("0008,1060", studyPhyRead.getText(),
                        studyPhyRead.getText().length());
                dicomFileInfo.getTagTable().setValue("0008,1080", studyDiag.getText(), studyDiag.getText().length());
                dicomFileInfo.getTagTable().setValue("0010,1010", studyAge.getText(), studyAge.getText().length());
                dicomFileInfo.getTagTable()
                        .setValue("0010,1030", studyWeight.getText(), studyWeight.getText().length());
                dicomFileInfo.getTagTable().setValue("0010,1020", studySize.getText(), studySize.getText().length());
                dicomFileInfo.getTagTable().setValue("0010,2180", studyOcc.getText(), studyOcc.getText().length());
                dicomFileInfo.getTagTable().setValue("0010,21B0", studyHist.getText(), studyHist.getText().length());
                dicomFileInfo.getTagTable().setValue("0020,000E", seriesUID.getText(), seriesUID.getText().length());
                dicomFileInfo.getTagTable().setValue("0020,0011", seriesNo.getText(), seriesNo.getText().length());
                dicomFileInfo.getTagTable().setValue("0008,0021", seriesDate.getText(), seriesDate.getText().length());
                dicomFileInfo.getTagTable().setValue("0008,0031", seriesTime.getText(), seriesTime.getText().length());
                dicomFileInfo.getTagTable().setValue("0008,1050", seriesPerfPhy.getText(),
                        seriesPerfPhy.getText().length());
                dicomFileInfo.getTagTable().setValue("0018,1030", seriesProtocol.getText(),
                        seriesProtocol.getText().length());
                dicomFileInfo.getTagTable().setValue("0008,103E", seriesDescrip.getText(),
                        seriesDescrip.getText().length());
                dicomFileInfo.getTagTable().setValue("0008,1070", seriesOp.getText(), seriesOp.getText().length());

                // additional tags that DTI folk requested...importing from another DICOM
                if (additionalTagsList != null) {

                    if (additionalTagsList.get("(0008,0008)") != null) {
                        final String value = (String) additionalTagsList.get("(0008,0008)");
                        dicomFileInfo.getTagTable().setValue("0008,0008", value, value.length());
                    }

                    if (additionalTagsList.get("(0008,0032)") != null) {
                        final String value = (String) additionalTagsList.get("(0008,0032)");
                        dicomFileInfo.getTagTable().setValue("0008,0032", value, value.length());
                    }

                    if (additionalTagsList.get("(0008,0070)") != null) {
                        final String value = (String) additionalTagsList.get("(0008,0070)");
                        dicomFileInfo.getTagTable().setValue("0008,0070", value, value.length());
                    }

                    if (additionalTagsList.get("(0008,1090)") != null) {
                        final String value = (String) additionalTagsList.get("(0008,1090)");
                        dicomFileInfo.getTagTable().setValue("0008,1090", value, value.length());
                    }

                    if (additionalTagsList.get("(0018,0080)") != null) {
                        final String value = (String) additionalTagsList.get("(0018,0080)");
                        dicomFileInfo.getTagTable().setValue("0018,0080", value, value.length());
                    }

                    if (additionalTagsList.get("(0018,0081)") != null) {
                        final String value = (String) additionalTagsList.get("(0018,0081)");
                        dicomFileInfo.getTagTable().setValue("0018,0081", value, value.length());
                    }

                    if (additionalTagsList.get("(0018,0082)") != null) {
                        final String value = (String) additionalTagsList.get("(0018,0082)");
                        dicomFileInfo.getTagTable().setValue("0018,0082", value, value.length());
                    }

                    if (additionalTagsList.get("(0018,0087)") != null) {
                        final String value = (String) additionalTagsList.get("(0018,0087)");
                        dicomFileInfo.getTagTable().setValue("0018,0087", value, value.length());
                    }

                    if (additionalTagsList.get("(0018,1100)") != null) {
                        final String value = (String) additionalTagsList.get("(0018,1100)");
                        dicomFileInfo.getTagTable().setValue("0018,1100", value, value.length());
                    }

                    if (additionalTagsList.get("(0018,1310)") != null) {
                        boolean test = true;
                        final String value = (String) additionalTagsList.get("(0018,1310)");
                        Object[] valueArray;
                        final String[] vals = value.split(", ");
                        valueArray = new Short[vals.length];

                        for (int i = 0; i < vals.length; i++) {

                            try {
                                valueArray[i] = Short.decode(vals[i]);
                            } catch (final NumberFormatException nfe) {
                                test = false;
                                MipavUtil.displayError("Error parsing short values from dicom tag (0018,1310): "
                                        + vals[i]);

                                break;
                            }

                        }

                        if (test) {
                            dicomFileInfo.getTagTable().setValue("0018,1310", valueArray);
                        }
                    }

                    if (additionalTagsList.get("(0018,1312)") != null) {
                        final String value = (String) additionalTagsList.get("(0018,1312)");
                        dicomFileInfo.getTagTable().setValue("0018,1312", value, value.length());
                    }

                    if (additionalTagsList.get("(0018,1314)") != null) {
                        final String value = (String) additionalTagsList.get("(0018,1314)");
                        dicomFileInfo.getTagTable().setValue("0018,1314", value, value.length());
                    }
                }

                if (seriesSmall.getText().length() > 0) {
                    dicomFileInfo.getTagTable().setValue("0028,0108", Short.valueOf(seriesSmall.getText()), 2);
                }

                if (seriesLarge.getText().length() > 0) {
                    dicomFileInfo.getTagTable().setValue("0028,0109", Short.valueOf(seriesLarge.getText()), 2);
                }

                dicomFileInfo.getTagTable().setValue("0040,0253", seriesStepID.getText(),
                        seriesStepID.getText().length());
                dicomFileInfo.getTagTable().setValue("0040,0244", seriesStepDate.getText(),
                        seriesStepDate.getText().length());
                dicomFileInfo.getTagTable().setValue("0040,0245", seriesStepTime.getText(),
                        seriesStepTime.getText().length());
                dicomFileInfo.getTagTable().setValue("0040,0254", seriesStepDescrip.getText(),
                        seriesStepDescrip.getText().length());

                switch (patientSex.getSelectedIndex()) {

                    case 0:
                        dicomFileInfo.getTagTable().setValue("0010,0040", "", 0);
                        break;

                    case 1:
                        dicomFileInfo.getTagTable().setValue("0010,0040", "M ", 2);
                        break;

                    case 2:
                        dicomFileInfo.getTagTable().setValue("0010,0040", "F ", 2);
                        break;

                    case 3:
                        dicomFileInfo.getTagTable().setValue("0010,0040", "O ", 2);
                        break;
                }

                switch (seriesMod.getSelectedIndex()) {

                    case 0:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "BI", 2);
                        break;

                    case 1:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "CD", 2);
                        break;

                    case 2:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "CR", 2);
                        break;

                    case 3:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "CT", 2);
                        break;

                    case 4:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "DD", 2);
                        break;

                    case 5:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "DG", 2);
                        break;

                    case 6:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "DX", 2);
                        break;

                    case 7:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "ES", 2);
                        break;

                    case 8:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "GM", 2);
                        break;

                    case 9:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "HC", 2);
                        break;

                    case 10:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "IO", 2);
                        break;

                    case 11:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "LS", 2);
                        break;

                    case 12:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "MA", 2);
                        break;

                    case 13:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "MG", 2);
                        break;

                    case 14:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "MR", 2);
                        break;

                    case 15:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "MS", 2);
                        break;

                    case 16:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "NM", 2);
                        break;

                    case 17:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "OT", 2);
                        break;

                    case 18:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "PT", 2);
                        break;

                    case 19:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "PX", 2);
                        break;

                    case 20:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "RF", 2);
                        break;

                    case 21:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "RG", 2);
                        break;

                    case 22:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "RTDOSE", 6);
                        break;

                    case 23:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "RTIMAGE ", 8);
                        break;

                    case 24:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "RTPLAN", 6);
                        break;

                    case 25:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "RTRECORD", 8);
                        break;

                    case 26:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "RTSTRUCT", 8);
                        break;

                    case 27:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "SM", 2);
                        break;

                    case 28:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "ST", 2);
                        break;

                    case 29:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "TG", 2);
                        break;

                    case 30:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "US", 2);
                        break;

                    case 31:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "XA", 2);
                        break;

                    case 32:
                        dicomFileInfo.getTagTable().setValue("0008,0060", "XC", 2);
                        break;
                }

                switch (laterality.getSelectedIndex()) {

                    case 1:
                        dicomFileInfo.getTagTable().setValue("0020,0060", "L ", 2);
                        break;

                    case 2:
                        dicomFileInfo.getTagTable().setValue("0020,0060", "R ", 2);
                        break;
                }

                switch (seriesBody.getSelectedIndex()) {

                    case 1:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "SKULL ", 6);
                        break;

                    case 2:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "CSPINE", 6);
                        break;

                    case 3:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "TSPINE", 6);
                        break;

                    case 4:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "LSPINE", 6);
                        break;

                    case 5:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "SSPINE", 6);
                        break;

                    case 6:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "COCCYX", 6);
                        break;

                    case 7:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "CHEST ", 6);
                        break;

                    case 8:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "CLAVICLE", 8);
                        break;

                    case 9:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "BREAST", 6);
                        break;

                    case 10:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "ABDOMEN ", 8);
                        break;

                    case 11:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "PELVIS", 6);
                        break;

                    case 12:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "HIP ", 4);
                        break;

                    case 13:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "SHOULDER", 8);
                        break;

                    case 14:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "ELBOW ", 6);
                        break;

                    case 15:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "KNEE", 4);
                        break;

                    case 16:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "ANKLE ", 6);
                        break;

                    case 17:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "HAND", 4);
                        break;

                    case 18:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "FOOT", 4);
                        break;

                    case 19:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "EXTREMITY ", 10);
                        break;

                    case 20:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "HEAD", 4);
                        break;

                    case 21:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "HEART ", 6);
                        break;

                    case 22:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "NECK", 4);
                        break;

                    case 23:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "LEG ", 4);
                        break;

                    case 24:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "ARM ", 4);
                        break;

                    case 25:
                        dicomFileInfo.getTagTable().setValue("0018,0015", "JAW ", 4);
                        break;
                }

                switch (seriesPos.getSelectedIndex()) {

                    case 1:
                        dicomFileInfo.getTagTable().setValue("0018,5100", "HFP ", 4);
                        break;

                    case 2:
                        dicomFileInfo.getTagTable().setValue("0018,5100", "HFS ", 4);
                        break;

                    case 3:
                        dicomFileInfo.getTagTable().setValue("0018,5100", "FFP ", 4);
                        break;

                    case 4:
                        dicomFileInfo.getTagTable().setValue("0018,5100", "FFS ", 4);
                        break;

                    case 5:
                        dicomFileInfo.getTagTable().setValue("0018,5100", "HFDR", 4);
                        break;

                    case 6:
                        dicomFileInfo.getTagTable().setValue("0018,5100", "HFDL", 4);
                        break;

                    case 7:
                        dicomFileInfo.getTagTable().setValue("0018,5100", "FFDR", 4);
                        break;

                    case 8:
                        dicomFileInfo.getTagTable().setValue("0018,5100", "FFDL", 4);
                        break;
                }

                // Some of these tags seem to be hard coded - why ??
                if (dicomFileInfo.getTagTable().getValue("0020,0013") == null) {
                    dicomFileInfo.getTagTable().setValue("0020,0013", "1 ", 2); // Instance number, reset in FileIO
                }

                final Short samples = new Short((short) 1);

                // These next two tags may change with RGB DICOM images !!!!
                dicomFileInfo.getTagTable().setValue("0028,0002", samples, 2);
                dicomFileInfo.getTagTable().setValue("0028,0004", "MONOCHROME2 ", 12);

                // Column and row
                dicomFileInfo.getTagTable().setValue("0028,0011", new Short((short) fileInfo.getExtents()[0]), 2);
                dicomFileInfo.getTagTable().setValue("0028,0010", new Short((short) fileInfo.getExtents()[1]), 2);

                // Pixel spacing
                String s = String.valueOf(fileInfo.getResolutions()[1]) + "\\"
                        + String.valueOf(fileInfo.getResolutions()[0]);
                dicomFileInfo.getTagTable().setValue("0028,0030", s, s.length());

                // Slice thickness and spacing
                if (fileInfo.getResolutions().length >= 3) {
                    s = String.valueOf(fileInfo.getResolutions()[2]);
                    dicomFileInfo.getTagTable().setValue("0018,0088", s, s.length()); // spacing between slices

                    if (fileInfo.getSliceThickness() != 0) {
                        s = String.valueOf(fileInfo.getSliceThickness());
                    }

                    dicomFileInfo.getTagTable().setValue("0018,0050", s, s.length()); // slice thickness
                }

                cancelFlag = false;
                dispose();
            }
        } else if (event.getActionCommand().equals("Cancel")) {
            cancelFlag = true;
            dispose();
        } else if (event.getActionCommand().equals("Fill tags from file")) {
            fillDataFromFile();
        } else if (event.getActionCommand().equals("Autofill required tags")) {
            autofillRequiredFields();
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * Autofill the dicom required tags to loosely fullfill the dicom standard.
     */
    private void autofillRequiredFields() {
        final String studyPostfix = "0";
        final String seriesPostfix = "1";

        String version = MipavUtil.getVersion();
        version = version.replaceAll("[\\.-]", "");

        final long datetime = System.currentTimeMillis();
        final String uidPrefix = "1.2.840.99999.9." + version + "." + datetime;

        // (0020,000D) study instance uid
        if ( ((JTextField) (tagsList.get("(0020,000D)"))).getText().equals("")) {
            ((JTextField) (tagsList.get("(0020,000D)"))).setText(uidPrefix + "." + studyPostfix);
            resetSize((JTextField) (tagsList.get("(0020,000D)")));
        }

        // (0008,0060) modality
        if (seriesMod.getSelectedIndex() == 0) {
            setChooserFromTag("(0008,0060)", "secondary_capture");
        }

        // (0020,000E) series instance uid
        if ( ((JTextField) (tagsList.get("(0020,000E)"))).getText().equals("")) {
            ((JTextField) (tagsList.get("(0020,000E)"))).setText(uidPrefix + "." + seriesPostfix);
            resetSize((JTextField) (tagsList.get("(0020,000E)")));
        }
    }

    /**
     * Checks the text field data for the proper format. For example, UIDs must be strings of numbers and periods,
     * person's names must be in the format Doe^John, etc.
     * 
     * @param value The text we are checking.
     * @param type Types we can expect - UI, PN, LO, DA, TM, etc.
     * 
     * @return Boolean confirming if the tag checked through or not.
     */
    private boolean checkTag(String value, final String type) {
        StringTokenizer tok;

        if (value != null) {
            value = value.trim();
        }

        if (type.equals("UI")) { // Must be in the form 9.999.99999999.99.9999 etc.

            if (value.length() == 0) { // Also it is a required tag so it must not be empty.
                MipavUtil.displayError("The required field UID must be filled in.");

                return false;
            } else {

                for (int i = 0; i < value.length(); i++) {

                    if ( !Character.isDigit(value.charAt(i)) && (value.charAt(i) != '.')) {
                        MipavUtil.displayError("UIDs must only contain numbers and periods:\n" + value);

                        return false;
                    }
                }

                return true;
            }
        } else if (type.equals("PN")) {

            if (value.length() > 0) {

                if (value.length() > 64) {
                    MipavUtil.displayError("Person's name must be under 64 characters.");

                    return false;
                }

                tok = new StringTokenizer(value, "^");

                while (tok.hasMoreTokens()) {

                    if ( !isDefaultCharacter(tok.nextToken(), false)) {
                        MipavUtil.displayError("Person's name must be of the form 'Doe^John'.");

                        return false;
                    }
                }

                return true;
            } else {
                return true;
            }
        } else if (type.equals("LO")) {

            if (value.length() > 0) {

                if (value.length() > 64) {
                    MipavUtil.displayError(value + "\nis too long.  It must be under 64 characters.");

                    return false;
                }

                if ( !isDefaultCharacter(value, false)) {
                    MipavUtil.displayError(value + "contains invalid characters.");

                    return false;
                } else {
                    return true;
                }
            } else {
                return true;
            }
        } else if (type.equals("DA")) {

            if ( (value.length() != 0) && (value.length() != 8)) {
                MipavUtil.displayError("Date must be of the form yyyymmdd where yyyy contains the year,\n"
                        + "mm contains the month and dd contains the day.\n" + "All characters should be numbers.");

                return false;
            }

            for (int i = 0; i < value.length(); i++) {

                if ( !Character.isDigit(value.charAt(i))) {
                    MipavUtil.displayError("Date must be of the form yyyymmdd where yyyy contains the year,\n"
                            + "mm contains the month and dd contains the day.\n" + "All characters should be numbers.");

                    return false;
                }
            }

            if (value.length() != 0) {

                if ( (Integer.valueOf(value.substring(0, 4)).intValue() < 0)
                        || (Integer.valueOf(value.substring(0, 4)).intValue() > 2500)) {
                    MipavUtil.displayError("Date must be of the form yyyymmdd where yyyy contains the year,\n"
                            + "mm contains the month and dd contains the day.\n" + "Year " + value.substring(0, 4)
                            + " is invalid.");

                    return false;
                }

                if ( (Integer.valueOf(value.substring(4, 6)).intValue() < 1)
                        || (Integer.valueOf(value.substring(4, 6)).intValue() > 12)) {
                    MipavUtil.displayError("Date must be of the form yyyymmdd where yyyy contains the year,\n"
                            + "mm contains the month and dd contains the day.\n" + "Month " + value.substring(4, 6)
                            + " is invalid.");

                    return false;
                }

                if ( (Integer.valueOf(value.substring(6, 8)).intValue() < 1)
                        || (Integer.valueOf(value.substring(6, 8)).intValue() > 31)) {
                    MipavUtil.displayError("Date must be of the form yyyymmdd where yyyy contains the year,\n"
                            + "mm contains the month and dd contains the day.\n" + "Day " + value.substring(6, 8)
                            + " is invalid.");

                    return false;
                }
            }

            return true;
        } else if (type.equals("TM")) {
            final char[] array = value.toCharArray();

            if ( (array.length != 0) && (array.length > 16)) {
                MipavUtil.displayError("Time cannot be more than 16 characters long.");

                return false;
            }

            for (int i = 0; i < array.length; i++) {

                if ( !Character.isDigit(array[i]) && (array[i] != '.')) {
                    MipavUtil.displayError("Time must be of form hhmmss.frac where hh is hours, mm is minutes,\n"
                            + "ss is seconds, and frac is a fractional part of a second.");

                    return false;
                }
            }

            return true;
        } else if (type.equals("SH")) {

            if ( (value.length() > 0) && (value.length() > 16)) {
                MipavUtil.displayError(value + " is too long.  It must be under 16 characters.");

                return false;
            }

            if ( !isDefaultCharacter(value, false)) {
                MipavUtil.displayError(value + "contains invalid characters.");

                return false;
            } else {
                return true;
            }
        } else if (type.equals("LT")) {

            if ( (value.length() > 0) && (value.length() > 10240)) {
                MipavUtil.displayError(value + " is too long.  It must be under 10240 characters.");

                return false;
            }

            if ( !isDefaultCharacter(value, true)) {
                MipavUtil.displayError(value + "contains invalid characters.");

                return false;
            } else {
                return true;
            }
        } else if (type.equals("AS")) {

            if (value.length() > 0) {

                if (value.length() != 4) {

                    // try to detect when leading zeros have been stripped from the value
                    if ( (value.length() < 4)
                            && ( (value.charAt(value.length() - 1) == 'D') || (value.charAt(value.length() - 1) == 'W')
                                    || (value.charAt(value.length() - 1) == 'M') || (value.charAt(value.length() - 1) == 'Y'))) {

                        for (int i = value.length() - 2; i >= 0; i--) {

                            if ( !Character.isDigit(value.charAt(i))) {
                                MipavUtil.displayError("Dicom tag value: " + value + " does not start with a digit.\n"
                                        + "Age must be of the form nnnD, nnnW, nnnM, or nnnY, where nnn contains\n"
                                        + "the number of days for D, the number of weeks for W, the number\n"
                                        + "of months for M, or the number of years for Y (4 characters).");
                            }
                        }

                        String zeroPadding = new String();

                        for (int i = 0; i < (4 - value.length()); i++) {
                            zeroPadding += "0";
                        }

                        studyAge.setText(zeroPadding + value);

                        return true;
                    }

                    MipavUtil.displayError("Dicom tag value: " + value + " is not four characters long.\n"
                            + "Age must be of the form nnnD, nnnW, nnnM, or nnnY, where nnn contains\n"
                            + "the number of days for D, the number of weeks for W, the number\n"
                            + "of months for M, or the number of years for Y (4 characters).");

                    return false;
                }

                if ( (value.charAt(3) != 'D') && (value.charAt(3) != 'W') && (value.charAt(3) != 'M')
                        && (value.charAt(3) != 'Y')) {
                    MipavUtil.displayError("Dicom tag value: " + value
                            + " does not have a time indicator (D, W, M or Y).\n"
                            + "Age must be of the form nnnD, nnnW, nnnM, or nnnY, where nnn contains\n"
                            + "the number of days for D, the number of weeks for W, the number\n"
                            + "of months for M, or the number of years for Y (4 characters).");

                    return false;
                }

                if ( !Character.isDigit(value.charAt(0)) || !Character.isDigit(value.charAt(1))
                        || !Character.isDigit(value.charAt(2))) {
                    MipavUtil.displayError("Dicom tag value: " + value + " does not start with a digit.\n"
                            + "Age must be of the form nnnD, nnnW, nnnM, or nnnY, where nnn contains\n"
                            + "the number of days for D, the number of weeks for W, the number\n"
                            + "of months for M, or the number of years for Y (4 characters).");

                    return false;
                }

                return true;
            } else {
                return true;
            }
        } else if (type.equals("DS")) {

            if (value.length() > 16) {
                MipavUtil.displayError(value + " is too long.  It must be less than 16 characters.");

                return false;
            }

            if (value.length() > 0) {
                final char[] array = value.toCharArray();

                for (int i = 0; i < array.length; i++) {

                    if ( !Character.isDigit(array[i]) && (array[i] != '.') && (array[i] != '+') && (array[i] != '-')
                            && (array[i] != 'E') && (array[i] != 'e')) {
                        MipavUtil.displayError(value + " contains invalid characters.  Decimal strings may\n"
                                + "only contain digits, '.', '+', '-', 'e', and 'E'.");

                        return false;
                    }
                }

                return true;
            } else {
                return true;
            }
        } else if (type.equals("LT")) {

            if (value.length() > 0) {

                if (value.length() > 10240) {
                    MipavUtil.displayError(value + "\n is too long.  It must be less than 10240 characters.");

                    return false;
                }

                if ( !isDefaultCharacter(value, true)) {
                    MipavUtil.displayError(value + " contains invalid characters.");

                    return false;
                } else {
                    return true;
                }
            } else {
                return true;
            }
        } else if (type.equals("IS")) {

            if (value.length() > 0) {

                if (value.length() > 12) {
                    MipavUtil.displayError(value + "\n is too long.  It must be less than 12 characters.");

                    return false;
                }

                final char[] array = value.toCharArray();

                for (int i = 0; i < array.length; i++) {

                    if ( !Character.isDigit(array[i]) && (array[i] != '+') && (array[i] != '-')) {
                        MipavUtil.displayError(value + " contains invalid characters.  Integer strings may\n"
                                + "only contain digits, '+', and '-'.");

                        return false;
                    }
                }

                return true;
            } else {
                return true;
            }
        } else if (type.equals("SS")) {

            if (value.length() > 0) {

                try {
                	@SuppressWarnings("unused")
                    final short s = Short.parseShort(value);

                    return true;
                } catch (final NumberFormatException error) {
                    MipavUtil.displayError(value + " is not a signed short.  It must be in the range "
                            + Short.MIN_VALUE + " to " + Short.MAX_VALUE + ".");

                    return false;
                }
            } else {
                return true;
            }
        } else if (type.equals("US")) {

            if (value.length() > 0) {

                try {
                    final short s = Short.parseShort(value);

                    if (s < 0) {
                        MipavUtil.displayError(s + " must be greater than 0.");

                        return false;
                    }

                    return true;
                } catch (final NumberFormatException error) {

                    try {
                        final int s = Integer.parseInt(value);

                        if (s < 65536) {
                            return true;
                        }

                        MipavUtil.displayError(value + " must be in the range 0 to 65536.");

                        return false;
                    } catch (final NumberFormatException error2) {
                        MipavUtil.displayError(value
                                + " is not an unsigned short.  It must be in the range 0 to 65536.");

                        return false;
                    }
                }
            } else {
                return true;
            }
        } else if (type.equals("USorSS")) {

            if (value.length() > 0) {

                try {
                	@SuppressWarnings("unused")
                    final short s = Short.parseShort(value);

                    return true;
                } catch (final NumberFormatException error) {

                    try {
                        final int s = Integer.parseInt(value);

                        if (s < 65536) {
                            return true;
                        }

                        MipavUtil.displayError(value + " must either be in the range 0 to 65536\n" + "or in the range "
                                + Short.MIN_VALUE + " to " + Short.MAX_VALUE + ".");

                        return false;
                    } catch (final NumberFormatException error2) {
                        MipavUtil.displayError(value
                                + " is not an unsigned or signed short.  It must either be in the range 0 to 65536\n"
                                + "or in the range " + Short.MIN_VALUE + " to " + Short.MAX_VALUE + ".");

                        return false;
                    }
                }
            } else {
                return true;
            }
        } else if (type.equals("Orient")) {

            if (value.length() > 0) {

                try {
                    final String s1 = value.substring(0, value.indexOf("\\"));
                    final char[] array1 = s1.toCharArray();

                    for (final char element : array1) {

                        if ( (element != 'A') && (element != 'P') && (element != 'R') && (element != 'L')
                                && (element != 'H') && (element != 'F')) {
                            MipavUtil
                                    .displayError("The Patient Orientation (0020,0020) relative to the image plane shall be specified by two values that\n"
                                            + "designate the anatomical direction of the positive row axis (left to right) and the positive column axis\n"
                                            + "(top to bottom). The first entry is the direction of the rows, given by the direction of the last pixel in\n"
                                            + "the first row from the first pixel in that row. The second entry is the direction of the columns, given\n"
                                            + "by the direction of the last pixel in the first column from the first pixel in that column.\n\n"
                                            + "Anatomical direction shall be designated by the capital letters: A (anterior), P (posterior), R (right),\n"
                                            + "L (left), H (head), F (foot). Each value of the orientation attribute shall contain at least one of these\n"
                                            + "characters. If refinements in the orientation descriptions are to be specified, then they shall be\n"
                                            + "designated by one or two additional letters in each value. Within each value, the letters shall be\n"
                                            + "ordered with the principal orientation designated in the first character.");

                            return false;
                        }
                    }

                    return true;
                } catch (final StringIndexOutOfBoundsException error) {
                    MipavUtil.displayError("There must be two values for Patient Orientation, separated by a '\\'.");

                    return false;
                }
            } else {
                return true;
            }
        }

        // this should not happen!
        Preferences.debug("returning false on " + value + " " + type + "\n");

        return false;
    }

    /**
     * Makes a button.
     * 
     * @param title The value of the button.
     * @param button Button to create.
     * 
     * @return The button.
     */
    private JButton createButton(final String title, JButton button) {
        button = new JButton(title);
        button.setActionCommand(title);
        button.addActionListener(this);
        button.setFont(serif12B);
        // button.setPreferredSize(MipavUtil.defaultButtonSize);

        return button;
    }

    /**
     * Creates a combo box and adds it to the panel.
     * 
     * @param panel the panel to add the combo box to
     * @param choices the layout to add the constraints to
     * @param x the constraints for this combo box
     * @param y DOCUMENT ME!
     * @param w DOCUMENT ME!
     * @param h DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */
    private JComboBox createComboBox(final Container panel, final String[] choices, final int x, final int y,
            final int w, final int h) {
        final JComboBox combo = new JComboBox(choices);
        combo.setFont(serif12);
        combo.setForeground(Color.black);
        combo.setPreferredSize(new Dimension(200, 25));
        setGBC(x, y, w, h);
        layout.setConstraints(combo, gbc);
        panel.add(combo);

        return combo;
    }

    /**
     * Creates static hashtable to use to fill the tags in the dialog.
     */
    private void createHashtable() {

        tagsList = new Hashtable<String,JComponent>();

        tagsList.put("(0010,0010)", patientName);
        tagsList.put("(0010,0020)", patientID);
        tagsList.put("(0010,0030)", patientBirthDate);
        tagsList.put("(0010,0032)", patientBirthTime);
        tagsList.put("(0010,0040)", patientSex);
        tagsList.put("(0010,1000)", patientOtherIDs);
        tagsList.put("(0010,1001)", patientOtherNames);
        tagsList.put("(0010,2160)", patientEthnicGroup);
        tagsList.put("(0010,4000)", patientComments);
        tagsList.put("(0020,0020)", patientOrientation);
        tagsList.put("(0020,000D)", studyUID);
        tagsList.put("(0020,0010)", studyID);
        tagsList.put("(0008,0020)", studyDate);
        tagsList.put("(0008,0030)", studyTime);
        tagsList.put("(0008,0050)", studyAccNumber);
        tagsList.put("(0008,1030)", studyDescrip);
        tagsList.put("(0008,0090)", studyRefPhy);
        tagsList.put("(0008,1048)", studyPhyRec);
        tagsList.put("(0008,1060)", studyPhyRead);
        tagsList.put("(0008,1080)", studyDiag);
        tagsList.put("(0010,1010)", studyAge);
        tagsList.put("(0010,1030)", studyWeight);
        tagsList.put("(0010,1020)", studySize);
        tagsList.put("(0010,2180)", studyOcc);
        tagsList.put("(0010,21B0)", studyHist);
        tagsList.put("(0008,0060)", seriesMod);
        tagsList.put("(0020,000E)", seriesUID);
        tagsList.put("(0020,0011)", seriesNo);
        tagsList.put("(0020,0080)", laterality);
        tagsList.put("(0008,0021)", seriesDate);
        tagsList.put("(0008,0031)", seriesTime);
        tagsList.put("(0008,1050)", seriesPerfPhy);
        tagsList.put("(0018,1030)", seriesProtocol);
        tagsList.put("(0008,103E)", seriesDescrip);
        tagsList.put("(0008,1070)", seriesOp);
        tagsList.put("(0018,0015)", seriesBody);
        tagsList.put("(0018,5100)", seriesPos);
        tagsList.put("(0028,0108)", seriesSmall);
        tagsList.put("(0028,0109)", seriesLarge);
        tagsList.put("(0040,0253)", seriesStepID);
        tagsList.put("(0040,0244)", seriesStepDate);
        tagsList.put("(0040,0245)", seriesStepTime);
        tagsList.put("(0040,0254)", seriesDescrip);

    }

    /**
     * Makes a label and adds it to the panel.
     * 
     * @param title the value of the label
     * @param panel the panel to add the label to
     * @param x the layout to add the constraints to
     * @param y the constraints for this label
     * @param w DOCUMENT ME!
     * @param h DOCUMENT ME!
     */
    private void createLabel(final String title, final Container panel, final int x, final int y, final int w,
            final int h) {
        final JLabel label = new JLabel(title);
        label.setFont(serif12);
        label.setForeground(Color.black);
        setGBC(x, y, w, h);
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;
        layout.setConstraints(label, gbc);
        panel.add(label);
    }

    /**
     * Makes a panel with a grid layout with the parameters specified.
     * 
     * @return the panel created
     */
    private JPanel createPanel() {
        final JPanel panel = new JPanel();
        panel.setLayout(layout);
        panel.setForeground(Color.black);

        return panel;
    }

    /**
     * Creates the panel for the patient tab.
     */
    private void createPatientPanel() {
        patientPanel = createPanel();
        createLabel("Patient's Name (0010,0010):", patientPanel, 0, 0, 1, 1);
        patientName = setTextField("", patientPanel, 1, 0, 1, 1);
        patientName.setPreferredSize(seriesUID.getPreferredSize());
        createLabel("Patient ID (0010,0020):", patientPanel, 2, 0, 1, 1);
        patientID = setTextField("", patientPanel, 3, 0, 1, 1);
        createLabel("Patient's Birth Date (0010,0030):", patientPanel, 0, 1, 1, 1);
        patientBirthDate = setTextField("", patientPanel, 1, 1, 1, 1);
        createLabel("Patient's Sex (0010,0040):", patientPanel, 2, 1, 1, 1);
        patientSex = createComboBox(patientPanel, sexes, 3, 1, 1, 1);
        patientSex.setSelectedIndex(0);
        createLabel("Patient's Birth Time (0010,0032):", patientPanel, 0, 2, 1, 1);
        patientBirthTime = setTextField("", patientPanel, 1, 2, 1, 1);
        createLabel("Other Patient IDs (0010,1000):", patientPanel, 2, 2, 1, 1);
        patientOtherIDs = setTextField("", patientPanel, 3, 2, 1, 1);
        createLabel("Other Patient Names (0010,1001):", patientPanel, 0, 3, 1, 1);
        patientOtherNames = setTextField("", patientPanel, 1, 3, 1, 1);
        createLabel("Ethnic Group (0010,2160):", patientPanel, 2, 3, 1, 1);
        patientEthnicGroup = setTextField("", patientPanel, 3, 3, 1, 1);
        createLabel("Patient Comments (0010,4000):", patientPanel, 0, 4, 1, 1);
        patientComments = setTextField("", patientPanel, 1, 4, 1, 1);
        createLabel("Patient Orientation (0020,0020):", patientPanel, 2, 4, 1, 1);
        patientOrientation = setTextField("", patientPanel, 3, 4, 1, 1);
    }

    /**
     * Creates the panel of required tags.
     */
    private void createRequiredPanel() {
        requiredPanel = createPanel();
        createLabel("Study Instance UID (0020,000D):", requiredPanel, 0, 0, 1, 1);
        studyUID = setTextField("", requiredPanel, 1, 0, 1, 1);
        createLabel("Modality (0008,0060):", requiredPanel, 0, 1, 1, 1);
        seriesMod = createComboBox(requiredPanel, modalities, 1, 1, 1, 1);
        seriesMod.setSelectedIndex(0);
        seriesMod.setMaximumRowCount(5);
        createLabel("Series Instance UID (0020,000E):", requiredPanel, 0, 2, 1, 1);
        seriesUID = setTextField("", requiredPanel, 1, 2, 1, 1);
    }

    /**
     * Creates the panel for the series tab.
     */
    private void createSeriesPanel() {
        seriesPanel = createPanel();
        createLabel("Series Number (0020,0011):", seriesPanel, 0, 0, 1, 1);
        seriesNo = setTextField("", seriesPanel, 1, 0, 1, 1);
        createLabel("Laterality (0020,0080):", seriesPanel, 2, 0, 1, 1);
        laterality = createComboBox(seriesPanel, lateralities, 3, 0, 1, 1);
        laterality.setSelectedIndex(0);
        createLabel("Series Date (0008,0021):", seriesPanel, 0, 1, 1, 1);
        seriesDate = setTextField("", seriesPanel, 1, 1, 1, 1);
        createLabel("Series Time (0008,0031):", seriesPanel, 2, 1, 1, 1);
        seriesTime = setTextField("", seriesPanel, 3, 1, 1, 1);
        createLabel("Performing Physicians' Name (0008,1050):", seriesPanel, 0, 2, 1, 1);
        seriesPerfPhy = setTextField("", seriesPanel, 1, 2, 1, 1);
        createLabel("Protocol Name (0018,1030):", seriesPanel, 2, 2, 1, 1);
        seriesProtocol = setTextField("", seriesPanel, 3, 2, 1, 1);
        createLabel("Series Description (0008,103E):", seriesPanel, 0, 3, 1, 1);
        seriesDescrip = setTextField("", seriesPanel, 1, 3, 1, 1);
        createLabel("Operators' Name (0008,1070):", seriesPanel, 2, 3, 1, 1);
        seriesOp = setTextField("", seriesPanel, 3, 3, 1, 1);
        createLabel("Body Part Examined (0018,0015):", seriesPanel, 0, 4, 1, 1);
        seriesBody = createComboBox(seriesPanel, parts, 1, 4, 1, 1);
        seriesBody.setSelectedIndex(0);
        seriesBody.setMaximumRowCount(5);
        createLabel("Patient Position (0018,5100):", seriesPanel, 2, 4, 1, 1);
        seriesPos = createComboBox(seriesPanel, positions, 3, 4, 1, 1);
        seriesPos.setSelectedIndex(0);
        seriesPos.setMaximumRowCount(25);
        createLabel("Smallest Pixel Value (0028,0108):", seriesPanel, 0, 5, 1, 1);
        seriesSmall = setTextField("", seriesPanel, 1, 5, 1, 1);
        createLabel("Largest Pixel Value (0028,0109):", seriesPanel, 2, 5, 1, 1);
        seriesLarge = setTextField("", seriesPanel, 3, 5, 1, 1);
        createLabel("Procedure Step ID (0040,0253):", seriesPanel, 0, 6, 1, 1);
        seriesStepID = setTextField("", seriesPanel, 1, 6, 1, 1);
        createLabel("Procedure Step Start Date (0040,0244):", seriesPanel, 2, 6, 1, 1);
        seriesStepDate = setTextField("", seriesPanel, 3, 6, 1, 1);
        createLabel("Procedure Step Start Time (0040,0245):", seriesPanel, 0, 7, 1, 1);
        seriesStepTime = setTextField("", seriesPanel, 1, 7, 1, 1);
        createLabel("Procedure Step Description (0040,0254):", seriesPanel, 2, 7, 1, 1);
        seriesStepDescrip = setTextField("", seriesPanel, 3, 7, 1, 1);
    }

    /**
     * Creates the panel for the study tab.
     */
    private void createStudyPanel() {
        studyPanel = createPanel();
        createLabel("Study ID (0020,0010):", studyPanel, 0, 0, 1, 1);
        studyID = setTextField("", studyPanel, 1, 0, 1, 1);
        createLabel("Study Date (0008,0020):", studyPanel, 2, 0, 1, 1);
        studyDate = setTextField("", studyPanel, 3, 0, 1, 1);
        createLabel("Study Time (0008,0030):", studyPanel, 0, 1, 1, 1);
        studyTime = setTextField("", studyPanel, 1, 1, 1, 1);
        createLabel("Accession Number (0008,0050):", studyPanel, 2, 1, 1, 1);
        studyAccNumber = setTextField("", studyPanel, 3, 1, 1, 1);
        createLabel("Study Description (0008,1030):", studyPanel, 0, 2, 1, 1);
        studyDescrip = setTextField("", studyPanel, 1, 2, 1, 1);
        createLabel("Referring Physician's Name (0008,0090):", studyPanel, 2, 2, 1, 1);
        studyRefPhy = setTextField("", studyPanel, 3, 2, 1, 1);
        createLabel("Physician(s) of Record (0008,1048):", studyPanel, 0, 3, 1, 1);
        studyPhyRec = setTextField("", studyPanel, 1, 3, 1, 1);
        createLabel("Physician(s) Reading Study (0008,1060):", studyPanel, 2, 3, 1, 1);
        studyPhyRead = setTextField("", studyPanel, 3, 3, 1, 1);
        createLabel("Admitting Diagnoses Description (0008,1080):", studyPanel, 0, 4, 1, 1);
        studyDiag = setTextField("", studyPanel, 1, 4, 1, 1);
        createLabel("Patient's Age (0010,1010):", studyPanel, 2, 4, 1, 1);
        studyAge = setTextField("", studyPanel, 3, 4, 1, 1);
        createLabel("Patient's Size (0010,1020):", studyPanel, 0, 5, 1, 1);
        studySize = setTextField("", studyPanel, 1, 5, 1, 1);
        createLabel("Patient's Weight (0010,1030):", studyPanel, 2, 5, 1, 1);
        studyWeight = setTextField("", studyPanel, 3, 5, 1, 1);
        createLabel("Occupation (0010,2180):", studyPanel, 0, 6, 1, 1);
        studyOcc = setTextField("", studyPanel, 1, 6, 1, 1);
        createLabel("Additional Patient's History (0010,21B0):", studyPanel, 2, 6, 1, 1);
        studyHist = setTextField("", studyPanel, 3, 6, 1, 1);

    }

    /**
     * Asks for a text file of tags, then loads them into the text fields.
     */
    private void fillDataFromFile() {

        String fileName, directory;
        String tag = null;
        String value = null;
        File file;
        RandomAccessFile raFile;
        JFileChooser chooser;
        additionalTagsList = new Hashtable<String,String>();

        // Open file dialog.
        chooser = new JFileChooser();

        if (UI.getDefaultDirectory() != null) {
            file = new File(UI.getDefaultDirectory());

            if (file != null) {
                chooser.setCurrentDirectory(file);
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }
        } else {
            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }

        chooser.setDialogTitle("Open tags file.");

        final int returnValue = chooser.showSaveDialog(UI.getMainFrame());

        if (returnValue == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            UI.setDefaultDirectory(directory);
        } else {
            return;
        }

        file = new File(directory + fileName);

        try {
            raFile = new RandomAccessFile(file, "rw");
        } catch (final FileNotFoundException error) {
            Preferences.debug("SaveDicom: File not found.");
            MipavUtil.displayError("SaveDicom: File not found.");

            return;
        }

        String tempString;

        try {
            tempString = raFile.readLine();

            while (tempString != null) {
                final StringTokenizer strTok = new StringTokenizer(tempString, "\t");

                // This file should be tab delimited and look like
                // (0002,0000) tab Meta Element Group Length tab tab 190
                if (strTok == null) {
                    return;
                }

                while (strTok.hasMoreTokens()) {
                    tag = strTok.nextToken();
                    tempString = strTok.nextToken(); // tag description - waste it

                    if (strTok.hasMoreTokens()) {
                        value = strTok.nextToken();
                    } else {
                        value = "";
                    }
                }

                if (value != null) {
                    value = value.trim();
                    // put data into dialogs
                }

                if (tag.equals("(0008,0060)") || tag.equals("(0010,0040)") || tag.equals("(0018,0015)")
                        || tag.equals("(0018,5100)")) {
                    setChooserFromTag(tag, value);
                }
                // Times, special format
                else if (tag.equals("(0010,0032)") || tag.equals("(0008,0030)") || tag.equals("(0008,0031)")
                        || tag.equals("(0040,0245)")) {

                    if (value.length() > 0) {
                        String s = "";
                        final StringTokenizer timeToken = new StringTokenizer(value, ":");

                        while (timeToken.hasMoreTokens()) {
                            s += timeToken.nextToken();
                        }

                        ((JTextField) (tagsList.get(tag))).setText(s);
                        resetSize((JTextField) (tagsList.get(tag)));
                    }
                }
                // Dates, special format
                else if (tag.equals("(0008,0020)") || tag.equals("(0040,0024)") || tag.equals("(0008,0021)")
                        || tag.equals("(0010,0030)")) {

                    if (value.length() == 10) {
                        final String month = value.substring(0, 2);
                        final String day = value.substring(3, 5);
                        final String year = value.substring(6);
                        ((JTextField) (tagsList.get(tag))).setText(year + month + day);
                        resetSize((JTextField) (tagsList.get(tag)));
                    }
                }
                // Ages, special format
                else if (tag.equals("(0010,1010)")) {

                    if (value.length() > 0) {
                        String s = "";
                        boolean done = false;
                        int i = 0;

                        while ( !done && (i < value.length())) {

                            if (value.charAt(i) != ' ') {
                                s += value.charAt(i);
                            } else {

                                while (s.length() < 3) {
                                    s = "0" + s;
                                }

                                s += value.charAt(i + 1);
                                done = true;
                            }

                            i++;
                        }

                        ((JTextField) (tagsList.get(tag))).setText(s);
                        resetSize((JTextField) (tagsList.get(tag)));
                    }
                }
                // Patient orientation, special format
                else if (tag.equals("(0020,0020)")) {

                    if (value.length() > 0) {
                        final StringTokenizer orientToken = new StringTokenizer(value);
                        String s = "";

                        while (orientToken.hasMoreTokens()) {
                            s += orientToken.nextToken().charAt(0) + "\\";
                        }

                        s = s.substring(0, s.length() - 1);
                        ((JTextField) (tagsList.get(tag))).setText(s);
                        resetSize((JTextField) (tagsList.get(tag)));
                    }
                }
                // dti tags
                else if (tag.equals("(0008,0008)")) {
                    additionalTagsList.put("(0008,0008)", value);
                } else if (tag.equals("(0008,0032)")) {
                    additionalTagsList.put("(0008,0032)", value);
                } else if (tag.equals("(0008,0070)")) {
                    additionalTagsList.put("(0008,0070)", value);
                } else if (tag.equals("(0008,1090)")) {
                    additionalTagsList.put("(0008,1090)", value);
                } else if (tag.equals("(0018,0080)")) {
                    additionalTagsList.put("(0018,0080)", value);
                } else if (tag.equals("(0018,0081)")) {
                    additionalTagsList.put("(0018,0081)", value);
                } else if (tag.equals("(0018,0082)")) {
                    additionalTagsList.put("(0018,0082)", value);
                } else if (tag.equals("(0018,0087)")) {
                    additionalTagsList.put("(0018,0087)", value);
                } else if (tag.equals("(0018,1100)")) {
                    additionalTagsList.put("(0018,1100)", value);
                } else if (tag.equals("(0018,1310)")) {
                    additionalTagsList.put("(0018,1310)", value);
                } else if (tag.equals("(0018,1312)")) {
                    additionalTagsList.put("(0018,1312)", value);
                } else if (tag.equals("(0018,1314)")) {
                    additionalTagsList.put("(0018,1314)", value);
                } else if (tagsList.get(tag) != null) {
                    ((JTextField) (tagsList.get(tag))).setText(value);
                    resetSize((JTextField) (tagsList.get(tag)));
                }

                tempString = raFile.readLine();
            }

            raFile.close();
        } catch (final IOException error) {
            MipavUtil.displayError("SaveDicom: Error reading file.");

            return;
        }

        pack();
    }

    /**
     * Fill the values in the dialog from tag-value pairs in a hashtable.
     * 
     * @param table the table of tag-value pairs
     */
    private void fillDataFromTable(final Hashtable<String, String> table) {
        final Enumeration<String> keys = table.keys();

        String tag;
        String tagNoParens;
        String value;

        while (keys.hasMoreElements()) {
            tagNoParens = keys.nextElement();
            tag = "(" + tagNoParens + ")";
            value = table.get(tagNoParens);

            // handle chooser fields differently
            if (tag.equals("(0008,0060)") || tag.equals("(0010,0040)") || tag.equals("(0018,0015)")
                    || tag.equals("(0018,5100)")) {
                setChooserFromTag(tag, value);
            } else if (tagsList.get(tag) != null) {
                ((JTextField) (tagsList.get(tag))).setText(value);
                resetSize((JTextField) (tagsList.get(tag)));
            }
        }
    }

    /**
     * Checks that all the characters of the string are in the default character repertoire.
     * 
     * @param word Word to check.
     * @param control Indicates if control characters should be included or not.
     * 
     * @return <code>true</code> if the string is in the default character repertoire.
     */
    private boolean isDefaultCharacter(final String word, final boolean control) {
        final char[] array = word.toCharArray();

        for (final char element : array) {

            if (control) {

                if ( ( ( (element) < 32) && ( (element) != 10) && ( (element) != 12) && ( (element) != 13) && ( (element) != 27))
                        || ( (element) > 126)) {
                    return false;
                }
            } else {

                if ( ( ( (element) < 32) && ( (element) != 27)) || ( (element) > 126)) {
                    return false;
                }
            }
        }

        return true;
    }

    /**
     * Resizes the text field so that all characters are displayed.
     * 
     * @param field Text field to change.
     */
    private void resetSize(final JTextField field) {
        field.setPreferredSize(new Dimension(field.getHorizontalVisibility().getMaximum() + 5,
                field.getPreferredSize().height));
    }

    /**
     * Sets the value for one of the choosers based on a dicom tag and value.
     * 
     * @param tag the tag (which determines which chooser to set)
     * @param value the value (which determines which index in the chooser is selected)
     */
    private void setChooserFromTag(final String tag, final String value) {

        if (tag.equals("(0008,0060)")) {

            // set modality combo box
            if (value.toLowerCase().equals("biomagnetic_imaging") || value.trim().equals("BI")) {
                seriesMod.setSelectedIndex(0);
            } else if (value.toLowerCase().equals("color_flow_doppler") || value.trim().equals("CD")) {
                seriesMod.setSelectedIndex(1);
            } else if (value.toLowerCase().equals("computed_radiography") || value.trim().equals("CR")) {
                seriesMod.setSelectedIndex(2);
            } else if (value.toLowerCase().equals("computed_tomography") || value.trim().equals("CT")) {
                seriesMod.setSelectedIndex(3);
            } else if (value.toLowerCase().equals("duplex_doppler") || value.trim().equals("DD")) {
                seriesMod.setSelectedIndex(4);
            } else if (value.toLowerCase().equals("diaphanography") || value.trim().equals("DG")) {
                seriesMod.setSelectedIndex(5);
            } else if (value.toLowerCase().equals("digital_radiography") || value.trim().equals("DX")) {
                seriesMod.setSelectedIndex(6);
            } else if (value.toLowerCase().equals("endoscopy") || value.trim().equals("ES")) {
                seriesMod.setSelectedIndex(7);
            } else if (value.toLowerCase().equals("general_microscopy") || value.trim().equals("GM")) {
                seriesMod.setSelectedIndex(8);
            } else if (value.toLowerCase().equals("hardcopy") || value.trim().equals("HC")) {
                seriesMod.setSelectedIndex(9);
            } else if (value.toLowerCase().equals("intraoral_radiography") || value.trim().equals("IO")) {
                seriesMod.setSelectedIndex(10);
            } else if (value.toLowerCase().equals("laser_surface_scan") || value.trim().equals("LS")) {
                seriesMod.setSelectedIndex(11);
            } else if (value.toLowerCase().equals("mr_angiography") || value.trim().equals("MA")) {
                seriesMod.setSelectedIndex(12);
            } else if (value.toLowerCase().equals("mammography") || value.trim().equals("MG")) {
                seriesMod.setSelectedIndex(13);
            } else if (value.toLowerCase().equals("magnetic_resonance") || value.trim().equals("MR")) {
                seriesMod.setSelectedIndex(14);
            } else if (value.toLowerCase().equals("mr_spectroscopy") || value.trim().equals("NS")) {
                seriesMod.setSelectedIndex(15);
            } else if (value.toLowerCase().equals("nuclear_medicine") || value.trim().equals("NM")) {
                seriesMod.setSelectedIndex(16);
            } else if (value.toLowerCase().equals("other") || value.trim().equals("OT")) {
                seriesMod.setSelectedIndex(17);
            } else if (value.toLowerCase().equals("pet") || value.trim().equals("PT")) {
                seriesMod.setSelectedIndex(18);
            } else if (value.toLowerCase().equals("panoramic_xray") || value.trim().equals("PX")) {
                seriesMod.setSelectedIndex(19);
            } else if (value.toLowerCase().equals("radio_fluoroscopy") || value.trim().equals("RF")) {
                seriesMod.setSelectedIndex(20);
            } else if (value.toLowerCase().equals("radiographic_imaging") || value.trim().equals("RG")) {
                seriesMod.setSelectedIndex(21);
            } else if (value.toLowerCase().equals("radiotherapy_dose") || value.trim().equals("RTDOSE")) {
                seriesMod.setSelectedIndex(22);
            } else if (value.toLowerCase().equals("radiotherapy_image") || value.trim().equals("RTIMAGE")) {
                seriesMod.setSelectedIndex(23);
            } else if (value.toLowerCase().equals("radiotherapy_plan") || value.trim().equals("RTPLAN")) {
                seriesMod.setSelectedIndex(24);
            } else if (value.toLowerCase().equals("radiotherapy_record") || value.trim().equals("RTSTRUCT")) {
                seriesMod.setSelectedIndex(25);
            } else if (value.toLowerCase().equals("radiotherapy_structure") || value.trim().equals("RTRECORD")) {
                seriesMod.setSelectedIndex(26);
            } else if (value.toLowerCase().equals("slide_microscopy") || value.trim().equals("SM")) {
                seriesMod.setSelectedIndex(27);
            } else if (value.toLowerCase().equals("spect") || value.trim().equals("ST")) {
                seriesMod.setSelectedIndex(28);
            } else if (value.toLowerCase().equals("thermography") || value.trim().equals("TG")) {
                seriesMod.setSelectedIndex(29);
            } else if (value.toLowerCase().equals("ultrasound") || value.trim().equals("US")) {
                seriesMod.setSelectedIndex(30);
            } else if (value.toLowerCase().equals("xray_angiography") || value.trim().equals("XA")) {
                seriesMod.setSelectedIndex(31);
            } else if (value.toLowerCase().equals("external_photography") || value.trim().equals("XC")) {
                seriesMod.setSelectedIndex(32);
            } else {
                seriesMod.setSelectedIndex(17);
            }
        } else if (tag.equals("(0010,0040)")) {

            // set patient sex combo box
            if (value.equals("Other") || value.trim().equals("O")) {
                patientSex.setSelectedIndex(3);
            } else if (value.equals("Male") || value.trim().equals("M")) {
                patientSex.setSelectedIndex(1);
            } else if (value.equals("Female") || value.trim().equals("F")) {
                patientSex.setSelectedIndex(2);
            } else {
                patientSex.setSelectedIndex(0); // Unknown
            }
        } else if (tag.equals("(0018,0015)")) {

            // set area examined combo box
            if (value.equals("Skull") || value.trim().equals("SKULL")) {
                seriesBody.setSelectedIndex(1);
            } else if (value.equals("CSpine") || value.trim().equals("CSPINE")) {
                seriesBody.setSelectedIndex(2);
            } else if (value.equals("TSpine") || value.trim().equals("TSPINE")) {
                seriesBody.setSelectedIndex(3);
            } else if (value.equals("LSpine") || value.trim().equals("LSPINE")) {
                seriesBody.setSelectedIndex(4);
            } else if (value.equals("SSpine") || value.trim().equals("SSPINE")) {
                seriesBody.setSelectedIndex(5);
            } else if (value.equals("Coccyx") || value.trim().equals("COCCYX")) {
                seriesBody.setSelectedIndex(6);
            } else if (value.equals("Chest") || value.trim().equals("CHEST")) {
                seriesBody.setSelectedIndex(7);
            } else if (value.equals("Clavicle") || value.trim().equals("CLAVICLE")) {
                seriesBody.setSelectedIndex(8);
            } else if (value.equals("Breast") || value.trim().equals("BREAST")) {
                seriesBody.setSelectedIndex(9);
            } else if (value.equals("Abdomen") || value.trim().equals("ABDOMEN")) {
                seriesBody.setSelectedIndex(10);
            } else if (value.equals("Pelvis") || value.trim().equals("PELVIS")) {
                seriesBody.setSelectedIndex(11);
            } else if (value.equals("Hip") || value.trim().equals("HIP")) {
                seriesBody.setSelectedIndex(12);
            } else if (value.equals("Shoulder") || value.trim().equals("SHOULDER")) {
                seriesBody.setSelectedIndex(13);
            } else if (value.equals("Elbow") || value.trim().equals("ELBOW")) {
                seriesBody.setSelectedIndex(14);
            } else if (value.equals("Knee") || value.trim().equals("KNEE")) {
                seriesBody.setSelectedIndex(15);
            } else if (value.equals("Ankle") || value.trim().equals("ANKLE")) {
                seriesBody.setSelectedIndex(16);
            } else if (value.equals("Hand") || value.trim().equals("HAND")) {
                seriesBody.setSelectedIndex(17);
            } else if (value.equals("Foot") || value.trim().trim().equals("FOOT")) {
                seriesBody.setSelectedIndex(18);
            } else if (value.equals("Extremity") || value.trim().equals("EXTREMITY")) {
                seriesBody.setSelectedIndex(19);
            } else if (value.equals("Head") || value.trim().equals("HEAD")) {
                seriesBody.setSelectedIndex(20);
            } else if (value.equals("Heart") || value.trim().equals("HEART")) {
                seriesBody.setSelectedIndex(21);
            } else if (value.equals("Neck") || value.trim().equals("NECK")) {
                seriesBody.setSelectedIndex(22);
            } else if (value.equals("Leg") || value.trim().equals("LEG")) {
                seriesBody.setSelectedIndex(23);
            } else if (value.equals("Arm") || value.trim().equals("ARM")) {
                seriesBody.setSelectedIndex(24);
            } else if (value.equals("Jaw") || value.trim().equals("JAW")) {
                seriesBody.setSelectedIndex(25);
            } else {
                seriesBody.setSelectedIndex(0);
            }
        } else if (tag.equals("(0018,5100)")) {

            // set patient position combo box
            if (value.equals("Head First-Prone") || value.equals("HFP")) {
                seriesPos.setSelectedIndex(1);
            } else if (value.equals("Head First-Supine") || value.trim().equals("HFS")) {
                seriesPos.setSelectedIndex(2);
            } else if (value.equals("Feet First-Prone") || value.trim().equals("FFP")) {
                seriesPos.setSelectedIndex(3);
            } else if (value.equals("Feet First-Supine") || value.trim().equals("FFS")) {
                seriesPos.setSelectedIndex(4);
            } else if (value.equals("HF-Decubitus Right") || value.trim().equals("HFDR")) {
                seriesPos.setSelectedIndex(5);
            } else if (value.equals("HF-Decubitus Left") || value.trim().equals("HFDL")) {
                seriesPos.setSelectedIndex(6);
            } else if (value.equals("FF-Decubitus Right") || value.trim().equals("FFDR")) {
                seriesPos.setSelectedIndex(7);
            } else if (value.equals("FF-Decubitus Left") || value.trim().equals("FFDL")) {
                seriesPos.setSelectedIndex(8);
            } else {
                seriesPos.setSelectedIndex(0); // Unknown
            }
        }
    }

    /**
     * A helper method for adding a component using GridBagLayout, so we don't have to set up the x, y, width, and
     * height over and over again.
     * 
     * @param x GridBagConstraints.gridx
     * @param y GridBagConstraints.gridy
     * @param w GridBagContraints.gridwidth
     * @param h GridBagConstraints.gridheight
     * 
     * @see GridBagConstraints
     */
    private void setGBC(final int x, final int y, final int w, final int h) {
        gbc.gridx = x;
        gbc.gridy = y;
        gbc.gridwidth = w;
        gbc.gridheight = h;
    }

    /**
     * Sets the modality chooser box based on the file info modality (the modality indexes should match up with the
     * ordering of the chooser).
     * 
     * @param modality The image modality.
     */
    private void setModalityChooser(final int modality) {

        // avoid unknown modalities and modalities not supported by the chooser
        if ( (modality > 0) && ( (modality - 1) < seriesMod.getItemCount())) {
            seriesMod.setSelectedIndex(modality - 1);
        }
    }

    /**
     * Makes a text field and adds it to the panel.
     * 
     * @param initial the initial string in the text field
     * @param panel the panel to add the text field to
     * @param x the layout to add the constraints to
     * @param y the constraints for this text field
     * @param w DOCUMENT ME!
     * @param h DOCUMENT ME!
     * 
     * @return the text field created
     */
    private JTextField setTextField(final String initial, final Container panel, final int x, final int y, final int w,
            final int h) {
        final JTextField field = new JTextField();

        // JTextField field = new JTextField(initial, 10);
        field.setPreferredSize(new Dimension(100, 25));
        field.setMinimumSize(new Dimension(100, 25));
        setGBC(x, y, w, h);
        gbc.fill = GridBagConstraints.HORIZONTAL;

        // gbc.weightx = 1;
        layout.setConstraints(field, gbc);
        panel.add(field);

        return field;
    }
}
