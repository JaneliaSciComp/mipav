package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;
import java.util.Enumeration;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. It should be noted, that the algorithms are executed in their own
 * thread.
 *
 * @version  0.1 Jan 17, 2001
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      JDialogGaussianBlur
 */
public class JDialogQuantify extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 412669652447327059L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmQuantify algoQuantify;

    /** DOCUMENT ME! */
    private String directory = null;

    /** DOCUMENT ME! */
    private String fileName = null;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private ModelImage maskImage = null; // result image

    /** DOCUMENT ME! */
    private JTextField textFile;
    
    private JButton buttonFile;
    
    private JComboBox comboBoxImage;
    
    private JCheckBox loadCheckBox;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogQuantify() { }

    /**
     * Creates a new JDialogQuantify object.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogQuantify(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            } else {
                dispose();
            }
        } else if (command.equals("Choose")) {

            try {
                JFileChooser chooser = new JFileChooser(image.getFileInfo(0).getFileDirectory());
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));

                int returnVal = chooser.showOpenDialog(this);

                if (returnVal == JFileChooser.APPROVE_OPTION) {
                    fileName = chooser.getSelectedFile().getName();
                    directory = chooser.getCurrentDirectory().getPath().concat(File.separator);
                    textFile.setText(fileName);
                }
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory!");

                return;
            }
        } else if (source.equals(loadCheckBox)) {
            if (loadCheckBox.isSelected()) {
                buttonFile.setEnabled(false);
                textFile.setEnabled(false);
                comboBoxImage.setEnabled(true);
            }
            else {
                buttonFile.setEnabled(true);
                textFile.setEnabled(true);
                comboBoxImage.setEnabled(false);    
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
        	//MipavUtil.showHelp("U4043");
        	MipavUtil.showWebHelp("Quantify_Mask");
        } else {
            super.actionPerformed(event);
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

        // ViewJFrameImage imageFrame = null;

        if (algorithm instanceof AlgorithmQuantify) {
            image.clearMask();

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        }
        
        if ((!loadCheckBox.isSelected()) && (maskImage != null)) {
            maskImage.disposeLocal();
            maskImage = null;
        }

        System.gc();
        algoQuantify.finalize();
        algoQuantify = null;
        dispose();
    }

    /**
     * Accessor to set the directory.
     *
     * @param  image  New directory.
     */
    public void setMaskImage(ModelImage image) {
        maskImage = image;
    }

    /**
     * Once all the necessary variables are set, call the quantify mask algorithm using the previously set file name.
     */
    protected void callAlgorithm() {

        try {

            // Make algorithm
            algoQuantify = new AlgorithmQuantify(image, maskImage);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            algoQuantify.addListener(this);

            createProgressBar(image.getImageName(), algoQuantify);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (algoQuantify.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                algoQuantify.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog Quantify: unable to allocate enough memory");

            return;
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        parentFrame = image.getParentFrame();

        setMaskImage(scriptParameters.retrieveImage("mask_image"));
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeImage(maskImage, "mask_image");
    }

    /**
     * Compares the dimensionality of two images.
     *
     * @param   im1  first image
     * @param   im2  second image
     *
     * @return  true if images have the same dimensionality else false
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
     * Builds a list of images to operate on from the template image.
     */
    private void buildComboBoxImage() {
        ViewUserInterface UI;
        boolean sameDims = true;

        comboBoxImage = new JComboBox();
        comboBoxImage.setFont(serif12);
        comboBoxImage.setBackground(Color.white);

        UI = ViewUserInterface.getReference();

        Enumeration<String> names = UI.getRegisteredImageNames();
        // Add images from user interface that have the same exact dimensionality
        while (names.hasMoreElements()) {
            String name = names.nextElement();
            sameDims = true;

            if (!image.getImageName().equals(name)) {
                ModelImage img = UI.getRegisteredImageByName(name);

                if (UI.getFrameContainingImage(img) != null) {

                    if (image.getNDims() == img.getNDims()) {
                        sameDims = compareDimensions(image, img);

                        if (sameDims) {
                            if ((img.getType() == ModelStorageBase.BOOLEAN) || (img.getType() == ModelStorageBase.UBYTE) ||
                                (img.getType() == ModelStorageBase.SHORT)) {
                                comboBoxImage.addItem(name);
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Quantify");

        JPanel filePanel = new JPanel(new GridBagLayout());
        filePanel.setForeground(Color.black);
        filePanel.setBorder(buildTitledBorder("Identify mask image file"));
        
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;

        buttonFile = new JButton("Choose...");
        buttonFile.setPreferredSize(MipavUtil.defaultButtonSize);
        buttonFile.setMinimumSize(MipavUtil.defaultButtonSize);
        buttonFile.setFont(serif12B);
        buttonFile.addActionListener(this);
        buttonFile.setActionCommand("Choose");
        filePanel.add(buttonFile, gbc);

        gbc.gridx = 1;
        textFile = new JTextField(15);
        textFile.setEnabled(false);
        textFile.setFont(serif12);
        filePanel.add(textFile, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 1;
        loadCheckBox = new JCheckBox("Load image from frame");
        loadCheckBox.setFont(serif12);
        loadCheckBox.setForeground(Color.black);
        loadCheckBox.setSelected(false);
        loadCheckBox.addActionListener(this);
        filePanel.add(loadCheckBox, gbc);

        gbc.gridx = 1;
        buildComboBoxImage();
        comboBoxImage.setEnabled(false);
        filePanel.add(comboBoxImage, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buttonPanel.add(buildHelpButton());
        
        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(filePanel);
        mainPanel.add(buttonPanel, BorderLayout.SOUTH);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        getContentPane().add(mainPanel);
        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        try {
            if (loadCheckBox.isSelected()) {
                String selectedName = (String) comboBoxImage.getSelectedItem();
                maskImage = ViewUserInterface.getReference().getRegisteredImageByName(selectedName);
            }
            else {
                FileIO fileIO = new FileIO();
    
                if ((fileName != null) && (directory != null)) {
                    maskImage = fileIO.readImage(fileName, directory, false, null);
    
                    if (maskImage != null) {
    
                        if (compareDimensions(maskImage, image) == false) {
                            MipavUtil.displayError("Images of different dimensions");
    
                            return false;
                        }
                    } else {
                        MipavUtil.displayError("Unable to open image.");
    
                        return false;
                    }
                } else {
                    MipavUtil.displayError("Unable to open image.");
    
                    return false;
                }
            }
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return false;
        }

        return true;
    }
}
