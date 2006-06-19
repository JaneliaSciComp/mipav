package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. It should be noted, that the algorithms are executed in their own
 * thread.
 *
 * @version  0.1 Jan 17, 2001
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      AlgorithmGaussianBlur
 */
public class JDialogQuantify extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

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

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

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
        super(theParentFrame, true);
        image = im;
        userInterface = ((ViewJFrameBase) (parentFrame)).getUserInterface();
        init();
    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  UI  The user interface, needed to create the image frame.
     * @param  im  Source image.
     */
    public JDialogQuantify(ViewUserInterface UI, ModelImage im) {
        super();
        userInterface = UI;
        image = im;
        parentFrame = image.getParentFrame();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

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
                    directory = chooser.getCurrentDirectory().getPath().concat("\\");
                    textFile.setText(fileName);
                }
            } catch (OutOfMemoryError e) {
                MipavUtil.displayError("Out of memory!");

                return;
            }
        } else if (command.equals("Cancel")) {
            dispose();
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
            insertScriptLine(algorithm);
        }

        System.gc();
        algoQuantify.finalize();
        algoQuantify = null;
        dispose();
    }

    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     *
     * @param  algo  the algorithm to make an entry for
     */
    public void insertScriptLine(AlgorithmBase algo) {

        if (algo.isCompleted()) {

            if (userInterface.isScriptRecording()) {

                // check to see if the match image is already in the ImgTable
                if (userInterface.getScriptDialog().getImgTableVar(image.getImageName()) == null) {

                    if (userInterface.getScriptDialog().getActiveImgTableVar(image.getImageName()) == null) {
                        userInterface.getScriptDialog().putActiveVar(image.getImageName());
                    }
                }

                userInterface.getScriptDialog().append("Quantify " +
                                                       userInterface.getScriptDialog().getVar(image.getImageName()) +
                                                       " " +
                                                       userInterface.getScriptDialog().getVar(maskImage.getImageName()) +
                                                       "\n");
            }
        }
    }

    /**
     * Run this algorithm from a script.
     *
     * @param   parser  the script parser we get the state from
     *
     * @throws  IllegalArgumentException  if there is something wrong with the arguments in the script
     */
    public void scriptRun(AlgorithmScriptParser parser) throws IllegalArgumentException {
        String srcImageKey = null;
        String maskImageKey = null;

        try {
            srcImageKey = parser.getNextString();
            maskImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        ModelImage im = parser.getImage(srcImageKey);

        image = im;
        userInterface = image.getUserInterface();
        parentFrame = image.getParentFrame();

        setMaskImage(parser.getImage(maskImageKey));

        setSeparateThread(false);
        callAlgorithm();
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
    private void callAlgorithm() {

        try {

            // Make algorithm
            algoQuantify = new AlgorithmQuantify(image, maskImage);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            algoQuantify.addListener(this);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (algoQuantify.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                if (!userInterface.isAppFrameVisible()) {
                    algoQuantify.setProgressBarVisible(false);
                }

                algoQuantify.run();
            }
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog Quantify: unable to allocate enough memory");

            return;
        }
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
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Quantify");

        JPanel filePanel = new JPanel();
        filePanel.setForeground(Color.black);
        filePanel.setBorder(buildTitledBorder("Identify mask image file"));

        JButton buttonFile = new JButton("Choose...");
        buttonFile.setPreferredSize(MipavUtil.defaultButtonSize);
        buttonFile.setMinimumSize(MipavUtil.defaultButtonSize);
        buttonFile.setFont(serif12B);
        buttonFile.addActionListener(this);
        buttonFile.setActionCommand("Choose");
        filePanel.add(buttonFile);

        textFile = new JTextField(15);
        textFile.setEnabled(false);
        textFile.setFont(serif12);
        filePanel.add(textFile);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

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
        } catch (OutOfMemoryError e) {
            MipavUtil.displayError("Out of memory!");

            return false;
        }

        return true;
    }
}
