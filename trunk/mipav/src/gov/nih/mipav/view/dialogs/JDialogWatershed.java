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

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user is able to control the degree of blurring in all
 * dimensions and indicate if a correction factor be applied to the z-dimension to account for differing resolutions
 * between the xy resolutions (intra-plane) and the z resolution (inter-plane). The algorithms are executed in their own
 * thread.
 *
 * @version  0.1 Nov 17, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      AlgorithmWatershed
 */
public class JDialogWatershed extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3432667720289138422L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JButton buttonEnergyInput;

    /** DOCUMENT ME! */
    private JCheckBox choiceCheckBox;

    /** DOCUMENT ME! */
    private String gmDirectory = null;

    /** DOCUMENT ME! */
    private String gmFileName = null;

    /** DOCUMENT ME! */
    private ModelImage gmImage = null;

    /** DOCUMENT ME! */
    private JTextField gmTextFile;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JLabel labelCorrected;

    /** DOCUMENT ME! */
    private JLabel labelGaussX;

    /** DOCUMENT ME! */
    private JLabel labelGaussY;

    /** DOCUMENT ME! */
    private JLabel labelGaussZ;

    /** DOCUMENT ME! */
    private float normFactor = 1; // normalization factor to adjust for resolution

    /** difference between x,y resolutions (in plane) and z resolution (between planes). */
    private JCheckBox resolutionCheckbox;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private float scaleX;

    /** DOCUMENT ME! */
    private float scaleY;

    /** DOCUMENT ME! */
    private float scaleZ;

    /** DOCUMENT ME! */
    private JTextField textGaussX;

    /** DOCUMENT ME! */
    private JTextField textGaussY;

    /** DOCUMENT ME! */
    private JTextField textGaussZ;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** DOCUMENT ME! */
    private AlgorithmWatershed watershedAlgo;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor for scripting.
     */
    public JDialogWatershed() { }

    /**
     * Creates new watershed dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogWatershed(JFrame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();


        if (source == buttonEnergyInput) {

            try {
                JFileChooser chooser = new JFileChooser(image.getFileInfo(0).getFileDirectory());
                int returnVal = chooser.showOpenDialog(this);

                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    gmFileName = chooser.getSelectedFile().getName();
                    gmDirectory = chooser.getCurrentDirectory().getPath() + File.separatorChar;
                    gmTextFile.setText(gmFileName);
                }

                return;
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory!");

                return;
            }
        } else if (source == OKButton) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (source == cancelButton) {
            dispose();
        }
         else if (source == helpButton) {
            MipavUtil.showHelp("WShed10");
            
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************
    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmWatershed) {
            System.err.println("finished algorithm");
            image.clearMask();

            if ((watershedAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                try {
                    resultImage.setImageName("Watershed");
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }

                insertScriptLine();
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                        userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                    }
                }

                if (parentFrame != null) {
                    userInterface.registerFrame(parentFrame);
                }

                image.notifyImageDisplayListeners(null, true);
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        }

        dispose();
    }

    /**
     * DOCUMENT ME!
     */
    public void callAlgorithm() {

        if (image.getNDims() == 2) { // source image is 2D

            int[] destExtents = new int[2];
            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            float[] sigmas = new float[2];
            sigmas[0] = scaleX; // set standard deviations (sigma) in X and Y
            sigmas[1] = scaleY;

            try {

                // Make result image of unsigned type
                resultImage = new ModelImage(ModelImage.USHORT, destExtents, " Watershed");

                // Make algorithm
                watershedAlgo = new AlgorithmWatershed(resultImage, image, gmImage, sigmas, null);

            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog watershed: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result image
                    resultImage = null;
                }

                return;
            }
        } else if (image.getNDims() == 3) {
            int[] destExtents = new int[3];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];

            float[] sigmas = new float[3];
            sigmas[0] = scaleX;
            sigmas[1] = scaleY;
            sigmas[2] = scaleZ; // normalized - scaleZ * resolutionX/resolutionZ; !!!!!!!

            try {

                // Make result image of unsigned type
                resultImage = new ModelImage(ModelImage.USHORT, destExtents, " Watershed");

                // Make algorithm
                watershedAlgo = new AlgorithmWatershed(resultImage, image, gmImage, sigmas, null);
            } catch (OutOfMemoryError x) {
                MipavUtil.displayError("Dialog watershed: unable to allocate enough memory");

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up image memory
                    resultImage = null;
                }

                return;
            }

        }

        //       This is very important. Adding this object as a listener allows the algorithm to
        // notify this object when it has completed of failed. See algorithm performed event.
        // This is made possible by implementing AlgorithmedPerformed interface
        watershedAlgo.addListener(this);

        //      Hide dialog
        setVisible(false);
        createProgressBar(image.getImageName(), watershedAlgo);

        //       Start the thread as a low priority because we wish to still have user interface work fast
        if (isRunInSeparateThread()) {

            if (watershedAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            watershedAlgo.run();
        }
    }

    /**
     * When the user clicks the mouse out of a text field, resets the neccessary variables.
     *
     * @param  event  Event that triggers this function
     */
    public void focusLost(FocusEvent event) {
        Object source = event.getSource();
        JTextField field;
        String text;
        float tempNum;

        if (source == textGaussZ) {
            field = (JTextField) source;
            text = field.getText();

            if (resolutionCheckbox.isSelected()) {
                tempNum = normFactor * Float.valueOf(text).floatValue();
                labelCorrected.setText("      Corrected scale = " + makeString(tempNum, 3));
            } else {
                labelCorrected.setText(" ");
            }
        }
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************
    /**
     * Changes value of corrected scale based on resolution checkbox; sets buttons to enabled or disabled based on
     * choice checkbox.
     *
     * @param  event  Event that cause the method to fire
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();
        float tempNum;

        if (source == resolutionCheckbox) {

            if (resolutionCheckbox.isSelected()) {
                tempNum = normFactor * Float.valueOf(textGaussZ.getText()).floatValue();
                labelCorrected.setText("      Corrected scale = " + makeString(tempNum, 3));
            } else {
                labelCorrected.setText(" ");
            }
        } else if (source == choiceCheckBox) {

            if (choiceCheckBox.isSelected()) {
                textGaussX.setEnabled(false);
                textGaussY.setEnabled(false);

                if (image.getNDims() > 2) {
                    textGaussZ.setEnabled(false);
                    resolutionCheckbox.setEnabled(false);
                    labelCorrected.setEnabled(false);
                }

                buttonEnergyInput.setEnabled(true);
            } else {
                textGaussX.setEnabled(true);
                textGaussY.setEnabled(true);

                if (image.getNDims() > 2) {
                    textGaussZ.setEnabled(true);
                    resolutionCheckbox.setEnabled(true);
                    labelCorrected.setEnabled(true);
                }

                buttonEnergyInput.setEnabled(false);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(resultImage);
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();

        try {
            gmImage = scriptParameters.retrieveImage("gmImage");
        } catch (Exception e) { // nothing, this image is not necessary
        }

        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        scaleX = scriptParameters.getParams().getFloat("scaleX");
        scaleY = scriptParameters.getParams().getFloat("scaleY");

        if (image.getNDims() == 3) {
            scaleZ = scriptParameters.getParams().getFloat("scaleZ");
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        if (gmImage != null) {
            scriptParameters.storeImage(gmImage, "gmImage");
        }

        scriptParameters.storeOutputImageParams(resultImage, true);

        scriptParameters.getParams().put(ParameterFactory.newParameter("scaleX", scaleX));
        scriptParameters.getParams().put(ParameterFactory.newParameter("scaleY", scaleY));

        if (image.getNDims() == 3) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("scaleZ", scaleZ));
        }
    }

    /**
     * Compares the dimensionality of two images.
     *
     * @param   im1  First image
     * @param   im2  Second image
     *
     * @return  <code>true</code> if images have the same dimensionality
     */
    private boolean compareDimensions(ModelImage im1, ModelImage im2) {
        int nDims = im1.getNDims();

        if (im1.getNDims() != im2.getNDims()) {
            return false;
        }

        for (int i = 0; i < nDims; i++) {

            if (im1.getExtents()[i] != im2.getExtents()[i]) {
                return false;
            }
        }

        return true;
    }

    /**
     * Tries to find the energy file that should have the name - sourceImageName_gm.xml and puts it into the text box
     * and sets the use precalculated checkbox to true.
     */
    private void findEnergyFile() {
        int i;
        File imageDir;
        String[] fileList;
        imageDir = new File(image.getFileInfo(0).getFileDirectory());

        // Read gmDirectory and find no. of images
        fileList = imageDir.list();

        String subName = image.getImageName() + "_gm"; // subName = name without indexing numbers at end

        // gm image should always be saved as xml by AlgorithmWatershed
        String suffix = ".xml";

        // check to see that they end in suffix.
        int index;
        String curFile, curFileNoSuffix;

        for (i = 0; i < fileList.length; i++) {
            curFile = fileList[i].trim();

            if (curFile.toLowerCase().endsWith(suffix)) { // note: not case sensitive!
                index = curFile.indexOf(".");
                curFileNoSuffix = curFile.substring(0, index);

                if (curFileNoSuffix.equals(subName)) {
                    choiceCheckBox.setSelected(true);
                    gmTextFile.setText(curFile);
                    gmFileName = curFile;
                    gmDirectory = image.getFileInfo(0).getFileDirectory();
                }
            }
        }
    }

    /**
     * Sets up GUI components and displays dialog.
     */
    private void init() {
        GridBagConstraints gbc = new GridBagConstraints();
        setForeground(Color.black);
        setTitle("Watershed");

        JPanel scalePanel = new JPanel(new GridBagLayout());
        scalePanel.setBorder(buildTitledBorder("Scale of the Gaussian"));
        textGaussX = new JTextField();
        textGaussX.setText("1.0");
        textGaussX.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 5, 0, 5);
        scalePanel.add(textGaussX, gbc);
        labelGaussX = new JLabel("X Dimension (0.5 - 5.0)");
        labelGaussX.setForeground(Color.black);
        labelGaussX.setFont(serif12);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.REMAINDER;
        gbc.weightx = 1;
        scalePanel.add(labelGaussX, gbc);
        textGaussY = new JTextField();
        textGaussY.setText("1.0");
        textGaussY.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 5, 0, 5);
        scalePanel.add(textGaussY, gbc);
        labelGaussY = new JLabel("Y Dimension (0.5 - 5.0)");
        labelGaussY.setForeground(Color.black);
        labelGaussY.setFont(serif12);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.REMAINDER;
        gbc.weightx = 1;
        scalePanel.add(labelGaussY, gbc);
        textGaussZ = new JTextField();
        textGaussZ.setText("1.0");
        textGaussZ.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 5, 0, 5);
        scalePanel.add(textGaussZ, gbc);
        labelGaussZ = new JLabel("Z Dimension (0.5 - 5.0)");
        labelGaussZ.setForeground(Color.black);
        labelGaussZ.setFont(serif12);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.REMAINDER;
        gbc.weightx = 1;
        scalePanel.add(labelGaussZ, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.fill = GridBagConstraints.REMAINDER;
        gbc.gridwidth = 2;
        resolutionCheckbox = new JCheckBox("Use image resolutions to normalize Z scale.");
        resolutionCheckbox.setFont(serif12);
        resolutionCheckbox.setSelected(true);
        scalePanel.add(resolutionCheckbox, gbc);

        if (image.getNDims() == 3) { // if the source image is 3D then allow
            resolutionCheckbox.setEnabled(true); // the user to indicate if it wishes to
            resolutionCheckbox.addItemListener(this); // use the correction factor
            textGaussZ.addFocusListener(this);
            textGaussZ.setEnabled(true);

            int index = image.getExtents()[2] / 2;
            float xRes = image.getFileInfo(index).getResolutions()[0];
            float zRes = image.getFileInfo(index).getResolutions()[2];
            normFactor = xRes / zRes; // Calculate correction factor
            labelCorrected = new JLabel("      Corrected scale = " +
                                        String.valueOf(normFactor * Float.valueOf(textGaussZ.getText()).floatValue()));
            labelCorrected.setForeground(Color.black);
            labelCorrected.setFont(serif12);
            gbc.gridx = 0;
            gbc.gridy = 4;
            gbc.fill = GridBagConstraints.REMAINDER;
            gbc.gridwidth = 2;
            scalePanel.add(labelCorrected, gbc);
        } else {
            resolutionCheckbox.setEnabled(false); // Image is only 2D, thus this checkbox
            labelGaussZ.setEnabled(false); // is not relevent
            textGaussZ.setEnabled(false);
        }

        JPanel energyPanel = new JPanel(new GridBagLayout());
        energyPanel.setBorder(buildTitledBorder("Energy file"));
        choiceCheckBox = new JCheckBox("Use pre-calculated energy file.");
        choiceCheckBox.setFont(serif12);
        choiceCheckBox.setForeground(Color.black);
        choiceCheckBox.setSelected(false);
        choiceCheckBox.addItemListener(this);
        buttonEnergyInput = new JButton("Choose...");
        buttonEnergyInput.setForeground(Color.black);
        buttonEnergyInput.setFont(serif12B);
        buttonEnergyInput.setEnabled(false);
        buttonEnergyInput.addActionListener(this);
        buttonEnergyInput.setActionCommand("Ref");
        buttonEnergyInput.setPreferredSize(MipavUtil.defaultButtonSize);
        gmTextFile = new JTextField("", 15);
        gmTextFile.setFont(serif12);
        gmTextFile.setEnabled(false);
        findEnergyFile();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        energyPanel.add(choiceCheckBox, gbc);
        gbc.gridy = 1;
        energyPanel.add(buttonEnergyInput, gbc);
        gbc.gridx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        energyPanel.add(gmTextFile, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buildCancelButton();
        buildHelpButton();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);
        buttonPanel.add(helpButton);

        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        scalePanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        energyPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        mainPanel.add(energyPanel);
        mainPanel.add(scalePanel);
        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private boolean setVariables() {
        String tmpStr;

        if (choiceCheckBox.isSelected() == true) {

            try {
                FileIO fileIO = new FileIO();
                gmImage = fileIO.readImage(gmFileName, gmDirectory, false, null);

                if (compareDimensions(gmImage, image) == false) {
                    MipavUtil.displayError("Images of different dimensions");

                    return false;
                }

                if (gmImage == null) {
                    return false;
                }
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory!");

                return false;
            }
        } else {
            gmImage = null; // This will force the watershed algorithm to

            // calculate the gradient magnitude of the image.
        }

        tmpStr = textGaussX.getText();

        if (testParameter(tmpStr, 0.5, 5.0)) {
            scaleX = Float.valueOf(tmpStr).floatValue();
        } else {
            textGaussX.requestFocus();
            textGaussX.selectAll();

            return false;
        }

        tmpStr = textGaussY.getText();

        if (testParameter(tmpStr, 0.5, 5.0)) {
            scaleY = Float.valueOf(tmpStr).floatValue();
        } else {
            textGaussY.requestFocus();
            textGaussY.selectAll();

            return false;
        }

        tmpStr = textGaussZ.getText();

        if (testParameter(tmpStr, 0.0, 5.0)) {
            scaleZ = Float.valueOf(tmpStr).floatValue();
        } else {
            textGaussZ.requestFocus();
            textGaussZ.selectAll();

            return false;
        }

        // Apply normalization if requested!
        if (resolutionCheckbox.isSelected()) {
            scaleZ = scaleZ * normFactor;
        }


        return true;
    }
}
