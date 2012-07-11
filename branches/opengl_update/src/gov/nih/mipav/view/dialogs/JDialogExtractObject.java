package gov.nih.mipav.view.dialogs;

import WildMagic.LibGraphics.Detail.*;
import WildMagic.LibGraphics.SceneGraph.*;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.WildMagic.Interface.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * DOCUMENT ME!
 *
 * @version  1.0 July 17, 2000
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class JDialogExtractObject extends JDialogBase implements AlgorithmInterface, ListSelectionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -4587834627195392398L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JButton chooserButton;

    /** DOCUMENT ME! */
    private JButton chooserButton2;

    /** DOCUMENT ME! */
    private AlgorithmObjectExtractor extractObjectAlgo;

    /** DOCUMENT ME! */
    private int groupNum;

    /** DOCUMENT ME! */
    private ModelImage[] gvfImage = null;

    /** DOCUMENT ME! */
    private DefaultListModel gvfModel;

    /** DOCUMENT ME! */
    private String gvfName;

    /** DOCUMENT ME! */
    private ModelImage image = null; // source image

    /** DOCUMENT ME! */
    private boolean justInit;

    /** DOCUMENT ME! */
    private JCheckBox justInitCheckbox;

    /** DOCUMENT ME! */
    private DefaultListModel model;

    /** DOCUMENT ME! */
    private JButton removeButton;

    /** DOCUMENT ME! */
    private JButton removeButton2;

    /** DOCUMENT ME! */
    private boolean saveGVF;

    /** DOCUMENT ME! */
    private JCheckBox saveGVFCheckBox;

    /** DOCUMENT ME! */
    private VOI srcVOI = null;

    /** DOCUMENT ME! */
    private String surName;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private TriMesh triMesh = null;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Sets the appropriate variables. Does not actually create a dialog that is visible because no user input is
     * necessary at present.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogExtractObject(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        setForeground(Color.black);
        image = im;
        userInterface = ViewUserInterface.getReference();

        ViewVOIVector VOIs = null;

        VOIs = im.getVOIs();

        int nVOI;

        nVOI = VOIs.size();

        if (nVOI == 0) {
            MipavUtil.displayWarning("No contour VOI is present, so a .SUR file must be opened");
        }

        if (nVOI > 0) {

            for (groupNum = 0; groupNum < nVOI; groupNum++) {

                if ((VOIs.VOIAt(groupNum).isActive() == true) && (VOIs.VOIAt(groupNum).getCurveType() == VOI.CONTOUR)) {
                    break;
                }
            }

            if (groupNum == nVOI) {
                MipavUtil.displayError("VOI must be selected");
                dispose();

                return;
            }

            srcVOI = VOIs.VOIAt(groupNum);
        } // if (nVOI > 0)

        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Presently only the script function calls this method. When the script sends this dialog the action command, this
     * method calls run.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Choose")) {
            open();

            if (triMesh != null) {
                model.addElement(surName);
                removeButton.setEnabled(true);
                chooserButton.setEnabled(false);
            }
        } // if (command.equals("Choose"))
        else if (command.equals("Remove")) {
            model.removeElement(surName);
            triMesh = null;
            removeButton.setEnabled(false);
            chooserButton.setEnabled(true);
        } // else if ((command.equals("Remove"))

        if (command.equals("Choose2")) {
            gvfImage = openGVF();

            if (!checkImage(gvfImage[0])) {
                return;
            }

            if (!checkImage(gvfImage[1])) {
                return;
            }

            if (!checkImage(gvfImage[2])) {
                return;
            }

            gvfName = gvfImage[0].getImageName();
            gvfModel.addElement(gvfName);
            removeButton2.setEnabled(true);
            chooserButton2.setEnabled(false);
        } // if (command.equals("Choose2"))
        else if (command.equals("Remove2")) {
            gvfModel.removeElement(gvfName);
            gvfImage[0].disposeLocal();
            gvfImage[1].disposeLocal();
            gvfImage[2].disposeLocal();
            gvfImage[0] = null;
            gvfImage[1] = null;
            gvfImage[2] = null;
            gvfImage = null;
            removeButton2.setEnabled(false);
            chooserButton2.setEnabled(true);
        } // else if ((command.equals("Remove2"))

        if (command.equals("OK")) {

            if ((srcVOI == null) && (triMesh == null)) {
                MipavUtil.displayError("Must select a VOI or open a surface mesh file");

                return;
            }

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
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmObjectExtractor) {

            // These next lines set the titles in all frames where the source image is displayed to
            // image name so as to indicate that the image is now unlocked!
            // The image frames are enabled and then registed to the userinterface.
            Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

            for (int i = 0; i < imageFrames.size(); i++) {
                ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                if ((((Frame) (imageFrames.elementAt(i))) != parentFrame) && (parentFrame != null)) {
                    userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                }
            }

            if (parentFrame != null) {
                userInterface.registerFrame(parentFrame);
            }

            image.calcMinMax();
            image.notifyImageDisplayListeners(null, true);
        }
    }

    /**
     * Calls the algorithm.
     */
    protected void callAlgorithm() {
        float[] uvf = null;
        float[] vvf = null;
        float[] wvf = null;
        int length;

        try {

            if (gvfImage != null) {
                length = image.getSliceSize() * image.getExtents()[2];

                if (gvfImage[0] != null) {
                    uvf = new float[length];

                    try {
                        gvfImage[0].exportData(0, length, uvf);
                    } catch (IOException error) {
                        MipavUtil.displayError("Error on gvfImage[0].expportData");

                        return;
                    }

                    gvfImage[0].disposeLocal();
                    gvfImage[0] = null;
                }

                if (gvfImage[1] != null) {
                    vvf = new float[length];

                    try {
                        gvfImage[1].exportData(0, length, vvf);
                    } catch (IOException error) {
                        MipavUtil.displayError("Error on gvfImage[1].expportData");

                        return;
                    }

                    gvfImage[1].disposeLocal();
                    gvfImage[1] = null;
                }

                if (gvfImage[2] != null) {
                    wvf = new float[length];

                    try {
                        gvfImage[2].exportData(0, length, wvf);
                    } catch (IOException error) {
                        MipavUtil.displayError("Error on gvfImage[2].exportData");

                        return;
                    }

                    gvfImage[2].disposeLocal();
                    gvfImage[2] = null;
                }

                gvfImage = null;
            } // if (gvfImage != null)

            System.gc();

            // Make algorithm
            extractObjectAlgo = new AlgorithmObjectExtractor(image, srcVOI, justInit, saveGVF, triMesh, uvf, vvf, wvf);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            extractObjectAlgo.addListener(this);

            createProgressBar(image.getImageName(), extractObjectAlgo);
            
            // Hide dialog
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

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (extractObjectAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                extractObjectAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog Extract Brain : unable to allocate enough memory");

            return;
        }
    }

    /**
     * Sets the remove index based on the selected index in the list.
     *
     * @param  evt  Event that caused this method to fire.
     */
    public void valueChanged(ListSelectionEvent evt) { }

    /**
     * Checks the dimensionality of the new image vs. the original source image. All new images should be of the same
     * dimensions.
     *
     * @param   testImage  DOCUMENT ME!
     *
     * @return  Flag indicating if the image checks out.
     */
    private boolean checkImage(ModelImage testImage) {

        if (testImage == null) {
            return false;
        }

        if (image.getNDims() != testImage.getNDims()) {
            MipavUtil.displayError("Error! " + image.getImageName() + " is " + image.getNDims() + "D, while " +
                                   testImage.getImageName() + " is " + testImage.getNDims() + "D");

            return false;
        }

        for (int i = 0; i < image.getNDims(); i++) {

            if ((testImage != null) && (image.getExtents()[i] != testImage.getExtents()[i])) {
                MipavUtil.displayError("Error! For dimension = " + i + " " + image.getImageName() + " has length = " +
                                       image.getExtents()[i] + " while " + testImage.getImageName() + " has length = " +
                                       testImage.getExtents()[i]);

                return false;
            }
        }

        return true;

    }

    /**
     * Makes the GUI elements of the dialog.
     */
    private void init() {
        setTitle("Extract Object");
        getContentPane().setLayout(new BorderLayout());

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        justInitCheckbox = new JCheckBox("Just 1 iteration");
        justInitCheckbox.setFont(serif12);
        justInitCheckbox.setSelected(false);
        justInitCheckbox.addItemListener(this);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        paramPanel.add(justInitCheckbox, gbc);

        saveGVFCheckBox = new JCheckBox("Save uvf, vvf, and wvf files");
        saveGVFCheckBox.setFont(serif12);
        saveGVFCheckBox.setSelected(false);
        saveGVFCheckBox.addItemListener(this);
        gbc.gridx = 0;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.gridy = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        paramPanel.add(saveGVFCheckBox, gbc);

        JPanel surPanel = new JPanel(new BorderLayout());
        surPanel.setBorder(buildTitledBorder("Open SUR file"));

        model = new DefaultListModel();

        JList surList = new JList(model);
        surList.setVisibleRowCount(1);
        surList.setPreferredSize(new Dimension(300, 30));
        surList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        surList.addListSelectionListener(this);
        surPanel.add(surList);

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

        surPanel.add(chooserPanel, BorderLayout.SOUTH);

        gbc.gridx = 0;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.gridy = 2;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        paramPanel.add(surPanel, gbc);

        JPanel gvfPanel = new JPanel(new BorderLayout());
        gvfPanel.setBorder(buildTitledBorder("Open UVF file"));

        gvfModel = new DefaultListModel();

        JList gvfList = new JList(gvfModel);
        gvfList.setVisibleRowCount(1);
        gvfList.setPreferredSize(new Dimension(300, 30));
        gvfList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
        gvfList.addListSelectionListener(this);
        gvfPanel.add(gvfList);

        JPanel chooserPanel2 = new JPanel();
        chooserButton2 = new JButton("Load");
        chooserButton2.setPreferredSize(MipavUtil.defaultButtonSize);
        chooserButton2.setFont(serif12B);
        chooserPanel2.add(chooserButton2);
        chooserButton2.addActionListener(this);
        chooserButton2.setActionCommand("Choose2");

        removeButton2 = new JButton("Remove");
        removeButton2.setPreferredSize(MipavUtil.defaultButtonSize);
        removeButton2.setFont(serif12B);
        removeButton2.setEnabled(false);
        chooserPanel2.add(removeButton2);
        removeButton2.addActionListener(this);
        removeButton2.setActionCommand("Remove2");

        gvfPanel.add(chooserPanel2, BorderLayout.SOUTH);

        gbc.gridx = 0;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.gridy = 3;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        paramPanel.add(gvfPanel, gbc);

        getContentPane().add(paramPanel);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setResizable(true);
        setVisible(true);
    }

    /**
     * Open an image based on the suffix of the file.
     */
    private void open() {
        JFileChooser chooser = null;
        File surFile;
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

            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.SURFACE));

            chooser.setDialogTitle("Open Image");

            int returnValue = chooser.showOpenDialog(userInterface.getMainFrame());

            if (returnValue == JFileChooser.APPROVE_OPTION) {
                surFile = chooser.getSelectedFile();
                surName = surFile.getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                userInterface.setDefaultDirectory(directory);
            } else {
                return;
            }
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return;
        }

        // open the file containing one or more meshes
        RandomAccessFile in;
        int iType, iQuantity;
        boolean isSur = true;

        if (surName.endsWith("sur")) {

            try {
                in = new RandomAccessFile(surFile, "r");
                iType = in.readInt();
                iQuantity = in.readInt();
                isSur = true;
            } catch (IOException e) {
                return;
            }
        } else {

            try {
                in = new RandomAccessFile(surFile, "r");
                iType = 0;
                iQuantity = FileSurface_WM.parseVRMLMesh(in);
                in.seek(0);
                isSur = false;
            } catch (NoSuchElementException e) {
                MipavUtil.displayError("Only load VRML file specifically written by MIPAV!");

                return;
            } catch (IOException e) {
                return;
            }
        }

        if (iQuantity > 1) {
            MipavUtil.displayWarning(iQuantity + " meshes are present.  Only reading first");
        }

        ViewJProgressBar progress = new ViewJProgressBar("Loading surface", "Loading surface", 0, 100, false, null,
                                                         null);

        if (iType == 0) {

            // meshes are type TriangleMesh
            if (isSur == true) {
                triMesh = FileSurface_WM.loadTMesh(in, progress, 0, 1, true, null, null, null, null);
            } else {
                triMesh = FileSurface_WM.loadVRMLMesh(in, progress, 0, 1, true, null, null, null);
            }

            if (triMesh == null) {
                MipavUtil.displayError("Error while reading in triangle mesh.");

                return;
            }
        } else {

            // meshes are type ClodMesh
            ClodMesh kClod = FileSurface_WM.loadCMesh(in, null, 0, 1);
            kClod.TargetRecord(kClod.GetMaximumLOD());
            kClod.SelectLevelOfDetail();
            triMesh = kClod;
        }
        progress.dispose();
    }

    /**
     * Open an image based on the suffix of the file.
     *
     * @return  The image.
     */
    private ModelImage[] openGVF() {
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

            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));

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

            int i = fileName.lastIndexOf('.');

            // Files are _uvf.xml, _vvf.xml, _wvf.xml
            String fileNameBase = fileName.substring(0, i - 3);
            String fileUVF = fileNameBase + "uvf.xml";
            String fileVVF = fileNameBase + "vvf.xml";
            String fileWVF = fileNameBase + "wvf.xml";
            gvfImage = new ModelImage[3];
            gvfImage[0] = fileIO.readImage(fileUVF, directory, multiFile, null);
            gvfImage[1] = fileIO.readImage(fileVVF, directory, multiFile, null);
            gvfImage[2] = fileIO.readImage(fileWVF, directory, multiFile, null);
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return null;
        }

        return gvfImage;
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        if (justInitCheckbox.isSelected()) {
            justInit = true;
        } else {
            justInit = false;
        }

        if (saveGVFCheckBox.isSelected()) {
            saveGVF = true;
        } else {
            saveGVF = false;
        }

        return true;
    }

}
