package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm.
 */
public class JDialogSkeletonize extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3658025439350217121L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean do25D = false; // do a 2.5D skeletonize on a 3D image

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JCheckBox image25D;

    /** DOCUMENT ME! */
    private boolean pruning = false;

    /** DOCUMENT ME! */
    private JCheckBox pruningCheckbox;

    /** DOCUMENT ME! */
    private JTextField pruningField;

    /** DOCUMENT ME! */
    private JLabel pruningLabel;

    /** DOCUMENT ME! */
    private JPanel pruningPanel;

    /** DOCUMENT ME! */
    private int pruningPixels;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private AlgorithmMorphology25D skeletonizeAlgo25D = null;

    /** DOCUMENT ME! */
    private AlgorithmMorphology2D skeletonizeAlgo2D = null;

    /** DOCUMENT ME! */
    private AlgorithmMorphology3D skeletonizeAlgo3D = null;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;
    
    private JPanelAlgorithmOutputOptions outputPanel;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogSkeletonize() { }

    /**
     * Constructor.
     *
     * @param  theParentFrame  parent frame
     * @param  im              source image
     */
    public JDialogSkeletonize(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, true);

        if ((im.getType() != ModelImage.BOOLEAN) && (im.getType() != ModelImage.UBYTE) &&
                (im.getType() != ModelImage.USHORT)) {
            MipavUtil.displayError("Source Image must be BOOLEAN, UNSIGNED BYTE or UNSIGNED SHORT");
            dispose();

            return;
        }

        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * actionPerformed - Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("PruneToggle")) {
            pruningField.setEnabled(pruningCheckbox.isSelected());
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
        String name = makeImageName(image.getImageName(), "_skel");

        if (algorithm instanceof AlgorithmMorphology2D) {
            image.clearMask();

            if ((skeletonizeAlgo2D.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {
                    resultImage.setImageName(name);
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector imageFrames = image.getImageFrameVector();

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
        } else if (algorithm instanceof AlgorithmMorphology25D) {
            image.clearMask();

            if ((skeletonizeAlgo25D.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName("Skeletonized image");
                    new ViewJFrameImage(resultImage);
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector imageFrames = image.getImageFrameVector();

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
        } // else if ( algorithm instanceof AlgorithmMorphology25D )
        else if (algorithm instanceof AlgorithmMorphology3D) {
            image.clearMask();

            if ((skeletonizeAlgo3D.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {
                    resultImage.setImageName(name);
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector imageFrames = image.getImageFrameVector();

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

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        if (skeletonizeAlgo2D != null) {
            skeletonizeAlgo2D.finalize();
            skeletonizeAlgo2D = null;
        }

        if (skeletonizeAlgo25D != null) {
            skeletonizeAlgo25D.finalize();
            skeletonizeAlgo25D = null;
        }

        if (skeletonizeAlgo3D != null) {
            skeletonizeAlgo3D.finalize();
            skeletonizeAlgo3D = null;
        }

        dispose();
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), outputPanel.isOutputNewImageSet());
        
        scriptParameters.storeProcessingOptions(outputPanel.isProcessWholeImageSet(), do25D);
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_pruning", pruning));
        scriptParameters.getParams().put(ParameterFactory.newParameter("num_pruning_pixels", pruningPixels));
    }
    
    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();
        
        if ((image.getType() != ModelImage.BOOLEAN) && (image.getType() != ModelImage.UBYTE) &&
                (image.getType() != ModelImage.USHORT)) {
            MipavUtil.displayError("Source Image must be Boolean or UByte or UShort");
            dispose();

            return;
        }
        
        outputPanel = new JPanelAlgorithmOutputOptions(image);
        scriptParameters.setOutputOptionsGUI(outputPanel);
        
        setImage25D(scriptParameters.getParams().getBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D));
        setPruningEnabled(scriptParameters.getParams().getBoolean("do_pruning"));
        setNumPruningPixels(scriptParameters.getParams().getInt("num_pruning_pixels"));
    }
    
    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        if (outputPanel.isOutputNewImageSet()) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    /**
     * Process the image in 2.5D.
     *
     * @param  b  whether to do 2.5D morphology
     */
    public void setImage25D(boolean b) {
        do25D = b;
    }

    /**
     * Accessor that sets the number of pixels to prune.
     *
     * @param  n  the desired number of pixels to prune
     */
    public void setNumPruningPixels(int n) {
        pruningPixels = n;
    }

    /**
     * Accessor that sets whether or not to prune.
     *
     * @param  flag  <code>true</code> indicates the image should also be pruned, <code>false</code> indicates the image
     *               should not be pruned.
     */
    public void setPruningEnabled(boolean flag) {
        pruning = flag;
    }
    
    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        int kernel = 0;

        if (image.getNDims() == 2) { // source image is 2D

            int[] destExtents = new int[2];
            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            if (outputPanel.isOutputNewImageSet()) {

                try {

                    // Make result image of float type
                    resultImage = (ModelImage) image.clone();

                    // Make algorithm
                    if (pruning) {
                        skeletonizeAlgo2D = new AlgorithmMorphology2D(resultImage, kernel, 0,
                                                                      AlgorithmMorphology2D.SKELETONIZE, 0, 0,
                                                                      pruningPixels, 0, outputPanel.isProcessWholeImageSet());
                    } else {
                        skeletonizeAlgo2D = new AlgorithmMorphology2D(resultImage, kernel, 0,
                                                                      AlgorithmMorphology2D.SKELETONIZE, 0, 0, 0, 0,
                                                                      outputPanel.isProcessWholeImageSet());
                    }

                    if (outputPanel.isProcessWholeImageSet() == false) {
                        skeletonizeAlgo2D.setMask(image.generateVOIMask());
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    skeletonizeAlgo2D.addListener(this);

                    createProgressBar(image.getImageName(), skeletonizeAlgo2D);
                    
                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (skeletonizeAlgo2D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        if (!userInterface.isAppFrameVisible()) {
                            skeletonizeAlgo2D.setProgressBarVisible(false);
                        }

                        skeletonizeAlgo2D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Skeletonize: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up memory of result image
                        resultImage = null;
                    }

                    return;
                }
            } else {

                try {

                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    if (pruning) {
                        skeletonizeAlgo2D = new AlgorithmMorphology2D(image, kernel, 0,
                                                                      AlgorithmMorphology2D.SKELETONIZE, 0, 0,
                                                                      pruningPixels, 0, outputPanel.isProcessWholeImageSet());
                    } else {
                        skeletonizeAlgo2D = new AlgorithmMorphology2D(image, kernel, 0,
                                                                      AlgorithmMorphology2D.SKELETONIZE, 0, 0, 0, 0,
                                                                      outputPanel.isProcessWholeImageSet());
                    }

                    if (outputPanel.isProcessWholeImageSet() == false) {
                        skeletonizeAlgo2D.setMask(image.generateVOIMask());
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    skeletonizeAlgo2D.addListener(this);

                    createProgressBar(image.getImageName(), skeletonizeAlgo2D);
                    
                    // Hide the dialog since the algorithm is about to run.
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector imageFrames = image.getImageFrameVector();
                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface.
                        if (skeletonizeAlgo2D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        if (!userInterface.isAppFrameVisible()) {
                            skeletonizeAlgo2D.setProgressBarVisible(false);
                        }

                        skeletonizeAlgo2D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Skeletonize: unable to allocate enough memory");

                    return;
                }
            }
        } else if ((image.getNDims() == 3) && !do25D) {
            int[] destExtents = new int[3];
            destExtents[0] = image.getExtents()[0];
            destExtents[1] = image.getExtents()[1];
            destExtents[2] = image.getExtents()[2];

            if (outputPanel.isOutputNewImageSet()) {

                try {

                    // Make result image of float type
                    resultImage = (ModelImage) image.clone();

                    // Make algorithm
                    if (pruning) {
                        skeletonizeAlgo3D = new AlgorithmMorphology3D(resultImage, kernel, 0,
                                                                      AlgorithmMorphology3D.SKELETONIZE, 0, 0,
                                                                      pruningPixels, 0, outputPanel.isProcessWholeImageSet());
                    } else {
                        skeletonizeAlgo3D = new AlgorithmMorphology3D(resultImage, kernel, 0,
                                                                      AlgorithmMorphology3D.SKELETONIZE, 0, 0, 0, 0,
                                                                      outputPanel.isProcessWholeImageSet());
                    }

                    if (outputPanel.isProcessWholeImageSet() == false) {
                        skeletonizeAlgo3D.setMask(image.generateVOIMask());
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    skeletonizeAlgo3D.addListener(this);

                    
                    createProgressBar(image.getImageName(), skeletonizeAlgo3D);
                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (skeletonizeAlgo3D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        if (!userInterface.isAppFrameVisible()) {
                            skeletonizeAlgo3D.setProgressBarVisible(false);
                        }

                        skeletonizeAlgo3D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Skeletonize objects: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    return;
                }
            } else {

                try {

                    // Make algorithm
                    if (pruning) {
                        skeletonizeAlgo3D = new AlgorithmMorphology3D(image, kernel, 0,
                                                                      AlgorithmMorphology3D.SKELETONIZE, 0, 0,
                                                                      pruningPixels, 0, outputPanel.isProcessWholeImageSet());
                    } else {
                        skeletonizeAlgo3D = new AlgorithmMorphology3D(image, kernel, 0,
                                                                      AlgorithmMorphology3D.SKELETONIZE, 0, 0, 0, 0,
                                                                      outputPanel.isProcessWholeImageSet());
                    }

                    if (outputPanel.isProcessWholeImageSet() == false) {
                        skeletonizeAlgo3D.setMask(image.generateVOIMask());
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    skeletonizeAlgo3D.addListener(this);

                    createProgressBar(image.getImageName(), skeletonizeAlgo3D);
                    
                    // Hide dialog
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector imageFrames = image.getImageFrameVector();
                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (skeletonizeAlgo3D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        if (!userInterface.isAppFrameVisible()) {
                            skeletonizeAlgo3D.setProgressBarVisible(false);
                        }

                        skeletonizeAlgo3D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog Skeletonize objects: unable to allocate enough memory");

                    return;
                }
            }
        } // else if (image.getNDims() == 3  && !do25D)
        else if (do25D) {

            if (outputPanel.isOutputNewImageSet()) {

                try {
                    resultImage = (ModelImage) image.clone();

                    // Make algorithm
                    if (pruning) {
                        skeletonizeAlgo25D = new AlgorithmMorphology25D(resultImage, kernel, 0,
                                                                        AlgorithmMorphology25D.SKELETONIZE, 0, 0,
                                                                        pruningPixels, 0, outputPanel.isProcessWholeImageSet());
                    } else {
                        skeletonizeAlgo25D = new AlgorithmMorphology25D(resultImage, kernel, 0,
                                                                        AlgorithmMorphology25D.SKELETONIZE, 0, 0, 0, 0,
                                                                        outputPanel.isProcessWholeImageSet());
                    }

                    if (outputPanel.isProcessWholeImageSet() == false) {
                        skeletonizeAlgo25D.setMask(image.generateVOIMask());
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    skeletonizeAlgo25D.addListener(this);

                    createProgressBar(image.getImageName(), skeletonizeAlgo25D);
                    
                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (skeletonizeAlgo25D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        if (!userInterface.isAppFrameVisible()) {
                            skeletonizeAlgo25D.setProgressBarVisible(false);
                        }

                        skeletonizeAlgo25D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog skeletonize: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up memory of result image
                        resultImage = null;
                    }

                    return;
                }
            } else {

                try {

                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    if (pruning) {
                        skeletonizeAlgo25D = new AlgorithmMorphology25D(image, kernel, 0,
                                                                        AlgorithmMorphology25D.SKELETONIZE, 0, 0,
                                                                        pruningPixels, 0, outputPanel.isProcessWholeImageSet());
                    } else {
                        skeletonizeAlgo25D = new AlgorithmMorphology25D(image, kernel, 0,
                                                                        AlgorithmMorphology25D.SKELETONIZE, 0, 0, 0, 0,
                                                                        outputPanel.isProcessWholeImageSet());
                    }

                    if (outputPanel.isProcessWholeImageSet() == false) {
                        skeletonizeAlgo25D.setMask(image.generateVOIMask());
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    skeletonizeAlgo25D.addListener(this);

                    createProgressBar(image.getImageName(), skeletonizeAlgo25D);
                    
                    // Hide the dialog since the algorithm is about to run.
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector imageFrames = image.getImageFrameVector();

                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface.
                        if (skeletonizeAlgo25D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        if (!userInterface.isAppFrameVisible()) {
                            skeletonizeAlgo25D.setProgressBarVisible(false);
                        }

                        skeletonizeAlgo25D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog skeletonize: unable to allocate enough memory");

                    return;
                }
            }
        } // else if (do25D)
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Skeletonize");

        pruningCheckbox = new JCheckBox("Prune Image after Skeletonization", false);
        pruningCheckbox.addActionListener(this);
        pruningCheckbox.setActionCommand("PruneToggle");
        pruningCheckbox.setFont(serif12);

        pruningField = new JTextField("3");
        pruningField.setFont(serif12);
        pruningField.setEnabled(false);

        pruningLabel = new JLabel("Number of pixels to remove.");
        pruningLabel.setFont(serif12);
        pruningLabel.setForeground(Color.black);

        pruningPanel = new JPanel(new GridBagLayout());
        pruningPanel.setForeground(Color.black);
        pruningPanel.setBorder(buildTitledBorder("Pruning"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 2;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);
        gbc.weightx = 1;

        gbc.gridx = 0;
        gbc.gridy = 0;
        pruningPanel.add(pruningCheckbox, gbc);
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.NONE;
        pruningPanel.add(pruningLabel, gbc);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        pruningPanel.add(pruningField, gbc);

        outputPanel = new JPanelAlgorithmOutputOptions(image);

        if (image.getNDims() == 3) {
            image25D = new JCheckBox("Process image in 2.5D", true);
        } else {
            image25D = new JCheckBox("Process image in 2.5D", false);
        }

        image25D.setFont(serif12);

        if (image.getNDims() == 3) {
            image25D.setEnabled(false);
        } else {
            image25D.setEnabled(false);
        }
        
        PanelManager optionsManager = new PanelManager("Options");
        optionsManager.add(image25D);

        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(pruningPanel, gbc);
        gbc.gridy = 1;
        gbc.fill = GridBagConstraints.BOTH;
        mainPanel.add(outputPanel, gbc);
        gbc.gridy = 2;
        mainPanel.add(optionsManager.getPanel(), gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        System.gc();

        pruningPixels = Integer.parseInt(pruningField.getText());

        do25D = image25D.isSelected();

        pruning = pruningCheckbox.isSelected();

        return true;
    }

}
