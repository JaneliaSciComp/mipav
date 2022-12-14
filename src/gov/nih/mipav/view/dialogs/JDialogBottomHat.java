package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
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
 * Dialog to get user input, then call the algorithm. The user has the option to generate a new image or replace the
 * source image. In addition the user selects if the algorithm is applied to whole image or to the VOI regions. It
 * should be noted that the algorithms are executed in their own threads.
 *
 * @version  1.1 March 20, 2009
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      AlgorithmMorphology2D
 */
public class JDialogBottomHat extends JDialogScriptableBase 
	implements AlgorithmInterface, ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    
    private AlgorithmGrayScaleMorphology2D gsBottomHatAlgo2D = null;
    
    private AlgorithmGrayScaleMorphology25D gsBottomHatAlgo25D = null;
    
    private AlgorithmGrayScaleMorphology3D gsBottomHatAlgo3D = null;
    
    /** DOCUMENT ME! */
    private JComboBox comboBoxKernel;

    /** DOCUMENT ME! */
    private boolean do25D = false;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JCheckBox image25D;

    /** or if the source image is to be replaced. */
    private int itersD;

    /** DOCUMENT ME! */
    private int itersE;

    /** DOCUMENT ME! */
    private int kernel = 0;

    /** DOCUMENT ME! */
    private float kernelSize;

    /** DOCUMENT ME! */
    private JLabel labelKernel;

    /** DOCUMENT ME! */
    private JLabel labelKernelSize;

    /** DOCUMENT ME! */
    private JLabel labelNIterD;

    /** DOCUMENT ME! */
    private JLabel labelNIterE;

    /** DOCUMENT ME! */
    private JPanel maskPanel;

    /** DOCUMENT ME! */
    private JPanelAlgorithmOutputOptions outputPanel;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JTextField textKernelSize;

    /** DOCUMENT ME! */
    private JTextField textNIterD;

    /** DOCUMENT ME! */
    private JTextField textNIterE;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogBottomHat() { }

    /**
     * Constructor.
     *
     * @param  theParentFrame  parent frame
     * @param  im              source image
     */
    public JDialogBottomHat(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        
        if (im.getType() == ModelImage.BOOLEAN) {
            MipavUtil.displayError("Source Image cannot be a booolean image");
            dispose();
            
            return;
        }
        
        if (im.isColorImage()) {
            MipavUtil.displayError("Source Image cannot be a color image");
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
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
        	//MipavUtil.showHelp("");
        } else {
            super.actionPerformed(event);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * algorithmPerformed - this method is required if the AlgorithmPerformed interface is implemented. It is called by
     * the algorithms when it has completed or failed to to complete, so that the dialog can display the result image
     * and/or clean up.
     *
     * @param  algorithm  algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        
        if (algorithm instanceof AlgorithmGrayScaleMorphology2D) {
            image.clearMask();

            if ((gsBottomHatAlgo2D.isCompleted() == true) && (resultImage != null)) {

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName("Bottom hat image");
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
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
        } else if (algorithm instanceof AlgorithmGrayScaleMorphology25D) {
            image.clearMask();

            if ((gsBottomHatAlgo25D.isCompleted() == true) && (resultImage != null)) {

                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName("Bottom hat image");
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
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
        } else if (algorithm instanceof AlgorithmGrayScaleMorphology3D) {
            image.clearMask();

            if ((gsBottomHatAlgo3D.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName("Bottom hat image");
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
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

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        // Update frame
        // ((ViewJFrameBase)parentFrame).updateImages(true);
        
        if (gsBottomHatAlgo2D != null) {
            gsBottomHatAlgo2D.finalize();
            gsBottomHatAlgo2D = null;
        }

        if (gsBottomHatAlgo25D != null) {
            gsBottomHatAlgo25D.finalize();
            gsBottomHatAlgo25D = null;
        }

        if (gsBottomHatAlgo3D != null) {
            gsBottomHatAlgo3D.finalize();
            gsBottomHatAlgo3D = null;
        }

        dispose();
    }

    /**
     * Accessor that returns the image.
     *
     * @return  the result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * itemStateChanged - unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (source == comboBoxKernel) {

            if (comboBoxKernel.getSelectedIndex() == 3) {
                textKernelSize.setEnabled(true);
                labelKernelSize.setEnabled(true);
            } else {
                textKernelSize.setEnabled(false);
                labelKernelSize.setEnabled(false);
            }
        }
    }

    /**
     * Process the image in 2.5D.
     *
     * @param  image25D  whether to do 2.5D morphology
     */
    public void setImage25D(boolean image25D) {
        do25D = image25D;
    }

    /**
     * Accessor that sets the kernel size.
     *
     * @param  size  the desired kernel size
     */
    public void setKernelSize(float size) {
        kernelSize = size;
    }

    /**
     * Accessor that sets the kernel type to use.
     *
     * @param  krnl  the kernel type to use (either AlgorithmMorphology2D.CONNECTED4,
     *               AlgorithmMorphology2D.CONNECTED8, AlgorithmMorphology2D.CONNECTED12,
     *               AlgorithmMorphology2D.SIZED_CIRCLE, AlgorithmMorphology3D.CONNECTED6,
     *               AlgorithmMorphology3D.CONNECTED26,
     *               AlgorithmMorphology3D.CONNECTED24, AlgorithmMorphology3D.SIZED_SPHERE (or the ones in
     *               AlgorithmMorphology25D))
     */
    public void setKernelType(int krnl) {
        kernel = krnl;
    }

    /**
     * Accessor that sets the number of dilations to perform.
     *
     * @param  iters  The number of erosions to do.
     */
    public void setNumDilations(int iters) {
        itersD = iters;
    }

    /**
     * Accessor that sets the number of erosions to perform.
     *
     * @param  iters  The number of erosions to do.
     */
    public void setNumErosions(int iters) {
        itersE = iters;
    }

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_bottom_hat");

        
            if (image.getNDims() == 2) { // source image is 2D
                
                int[] destExtents = new int[2];
                destExtents[0] = image.getExtents()[0]; // X dim
                destExtents[1] = image.getExtents()[1]; // Y dim
    
                if (outputPanel.isOutputNewImageSet()) {
    
                    try {
                        resultImage = (ModelImage) image.clone();
                        resultImage.setImageName(name);
    
                        // Make algorithm
                        gsBottomHatAlgo2D = new AlgorithmGrayScaleMorphology2D(resultImage, kernel, kernelSize,
                                                                AlgorithmGrayScaleMorphology2D.BOTTOM_HAT, itersD, itersE, 0, 0,
                                                                outputPanel.isProcessWholeImageSet());
    
                        if (outputPanel.isProcessWholeImageSet() == false) {
                            gsBottomHatAlgo2D.setMask(image.generateVOIMask());
                        }
    
                        // gsBottomHatAlgo2D.setIterations(itersD, itersE);
                        // This is very important. Adding this object as a listener allows the algorithm to
                        // notify this object when it has completed or failed. See algorithm performed event.
                        // This is made possible by implementing AlgorithmedPerformed interface
                        gsBottomHatAlgo2D.addListener(this);
    
                        createProgressBar(image.getImageName(), gsBottomHatAlgo2D);
    
                        // Hide dialog
                        setVisible(false);
    
                        if (isRunInSeparateThread()) {
    
                            // Start the thread as a low priority because we wish to still have user interface work fast.
                            if (gsBottomHatAlgo2D.startMethod(Thread.MIN_PRIORITY) == false) {
                                MipavUtil.displayError("A thread is already running on this object");
                            }
                        } else {
    
                            gsBottomHatAlgo2D.run();
                        }
                    } catch (OutOfMemoryError x) {
                        MipavUtil.displayError("Dialog bottom hat: unable to allocate enough memory");
    
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
                        gsBottomHatAlgo2D = new AlgorithmGrayScaleMorphology2D(image, kernel, kernelSize, AlgorithmGrayScaleMorphology2D.BOTTOM_HAT,
                                                                itersD, itersE, 0, 0, outputPanel.isProcessWholeImageSet());
    
                        if (outputPanel.isProcessWholeImageSet() == false) {
                            gsBottomHatAlgo2D.setMask(image.generateVOIMask());
                            // gsBottomHatAlgo2D.setIterations(itersD, itersE);
                        }
    
                        // This is very important. Adding this object as a listener allows the algorithm to
                        // notify this object when it has completed or failed. See algorithm performed event.
                        // This is made possible by implementing AlgorithmedPerformed interface
                        gsBottomHatAlgo2D.addListener(this);
    
                        createProgressBar(image.getImageName(), gsBottomHatAlgo2D);
    
                        // Hide the dialog since the algorithm is about to run.
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
    
                            // Start the thread as a low priority because we wish to still have user interface.
                            if (gsBottomHatAlgo2D.startMethod(Thread.MIN_PRIORITY) == false) {
                                MipavUtil.displayError("A thread is already running on this object");
                            }
                        } else {
    
                            gsBottomHatAlgo2D.run();
                        }
                    } catch (OutOfMemoryError x) {
                        MipavUtil.displayError("Dialog bottom hat: unable to allocate enough memory");
    
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
                        resultImage = (ModelImage) image.clone();
                        resultImage.setImageName(name);
    
                        // Make algorithm
                        gsBottomHatAlgo3D = new AlgorithmGrayScaleMorphology3D(resultImage, kernel, kernelSize,
                                                                AlgorithmGrayScaleMorphology3D.BOTTOM_HAT, itersD, itersE, 0, 0,
                                                                outputPanel.isProcessWholeImageSet());
    
                        if (outputPanel.isProcessWholeImageSet() == false) {
                            gsBottomHatAlgo3D.setMask(image.generateVOIMask());
                        }
    
                        // This is very important. Adding this object as a listener allows the algorithm to
                        // notify this object when it has completed or failed. See algorithm performed event.
                        // This is made possible by implementing AlgorithmedPerformed interface
                        gsBottomHatAlgo3D.addListener(this);
    
                        createProgressBar(image.getImageName(), gsBottomHatAlgo3D);
    
                        // Hide dialog
                        setVisible(false);
    
                        if (isRunInSeparateThread()) {
    
                            // Start the thread as a low priority because we wish to still have user interface work fast
                            if (gsBottomHatAlgo3D.startMethod(Thread.MIN_PRIORITY) == false) {
                                MipavUtil.displayError("A thread is already running on this object");
                            }
                        } else {
    
                            gsBottomHatAlgo3D.run();
                        }
                    } catch (OutOfMemoryError x) {
                        MipavUtil.displayError("Dialog bottom hat: unable to allocate enough memory");
    
                        if (resultImage != null) {
                            resultImage.disposeLocal(); // Clean up image memory
                            resultImage = null;
                        }
    
                        return;
                    }
                } else {
    
                    try {
    
                        // Make algorithm
                        gsBottomHatAlgo3D = new AlgorithmGrayScaleMorphology3D(image, kernel, kernelSize, AlgorithmGrayScaleMorphology3D.BOTTOM_HAT,
                                                                itersD, itersE, 0, 0, outputPanel.isProcessWholeImageSet());
    
                        if (outputPanel.isProcessWholeImageSet() == false) {
                            gsBottomHatAlgo3D.setMask(image.generateVOIMask());
                        }
    
                        // This is very important. Adding this object as a listener allows the algorithm to
                        // notify this object when it has completed or failed. See algorithm performed event.
                        // This is made possible by implementing AlgorithmedPerformed interface
                        gsBottomHatAlgo3D.addListener(this);
    
                        createProgressBar(image.getImageName(), gsBottomHatAlgo3D);
    
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
    
                            // Start the thread as a low priority because we wish to still have user interface work fast
                            if (gsBottomHatAlgo3D.startMethod(Thread.MIN_PRIORITY) == false) {
                                MipavUtil.displayError("A thread is already running on this object");
                            }
                        } else {
    
                            gsBottomHatAlgo3D.run();
                        }
                    } catch (OutOfMemoryError x) {
                        MipavUtil.displayError("Dialog bottom hat: unable to allocate enough memory");
    
                        return;
                    }
                }
            } else if (do25D) {
                int[] destExtents = new int[3];
                destExtents[0] = image.getExtents()[0]; // X dim
                destExtents[1] = image.getExtents()[1]; // Y dim
                destExtents[2] = image.getExtents()[2]; // Z dim
    
                // convert to 2.5d kernel type
                if (kernel == AlgorithmMorphology3D.CONNECTED6) {
                    kernel = AlgorithmMorphology25D.CONNECTED4;
                } else if (kernel == AlgorithmMorphology3D.CONNECTED26) {
                    kernel = AlgorithmMorphology25D.CONNECTED8;
                } else if (kernel == AlgorithmMorphology3D.CONNECTED24) {
                    kernel = AlgorithmMorphology25D.CONNECTED12;
                } else if (kernel == AlgorithmMorphology3D.SIZED_SPHERE) {
                    kernel = AlgorithmMorphology25D.SIZED_CIRCLE;
                }
    
                if (outputPanel.isOutputNewImageSet()) {
    
                try {
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    // Make algorithm
                    gsBottomHatAlgo25D = new AlgorithmGrayScaleMorphology25D(resultImage, kernel, kernelSize,
                                                              AlgorithmGrayScaleMorphology2D.BOTTOM_HAT, itersD, itersE, 0, 0,
                                                              outputPanel.isProcessWholeImageSet());

                    if (outputPanel.isProcessWholeImageSet() == false) {
                        gsBottomHatAlgo25D.setMask(image.generateVOIMask());
                    }

                    // gsBottomHatAlgo25D.setIterations(itersD, itersE);
                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    gsBottomHatAlgo25D.addListener(this);

                    createProgressBar(image.getImageName(), gsBottomHatAlgo25D);

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (gsBottomHatAlgo25D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {


                        gsBottomHatAlgo25D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog bottom hat: unable to allocate enough memory");

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
                    gsBottomHatAlgo25D = new AlgorithmGrayScaleMorphology25D(image, kernel, kernelSize, AlgorithmGrayScaleMorphology25D.BOTTOM_HAT,
                                                              itersD, itersE, 0, 0,
                                                              outputPanel.isProcessWholeImageSet());

                    if (outputPanel.isProcessWholeImageSet() == false) {
                        gsBottomHatAlgo25D.setMask(image.generateVOIMask());
                        // gsBottomHatAlgo25D.setIterations(itersD, itersE);
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    gsBottomHatAlgo25D.addListener(this);

                    createProgressBar(image.getImageName(), gsBottomHatAlgo25D);

                    // Hide the dialog since the algorithm is about to run.
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

                        // Start the thread as a low priority because we wish to still have user interface.
                        if (gsBottomHatAlgo25D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        gsBottomHatAlgo25D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog bottom hat: unable to allocate enough memory");

                    return;
                }
            }
        }    
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
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        if (image.getType() == ModelImage.BOOLEAN) {
            throw new ParameterException(AlgorithmParameters.getInputImageLabel(1), "Source Image cannot be boolean");        
        }
        
        if (image.isColorImage()) {
            throw new ParameterException(AlgorithmParameters.getInputImageLabel(1), "Source Image cannot be color");
        }

        outputPanel = new JPanelAlgorithmOutputOptions(image);
        scriptParameters.setOutputOptionsGUI(outputPanel);

        do25D = scriptParameters.doProcess3DAs25D();
        itersD = scriptParameters.getParams().getInt("dilation_iterations");
        itersE = scriptParameters.getParams().getInt("erosion_iterations");
        kernel = scriptParameters.getParams().getInt("kernel_type");
        kernelSize = scriptParameters.getParams().getFloat("kernel_size");
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, outputPanel.isOutputNewImageSet());

        scriptParameters.storeProcessingOptions(outputPanel.isProcessWholeImageSet(), do25D);

        scriptParameters.getParams().put(ParameterFactory.newParameter("dilation_iterations", itersD));
        scriptParameters.getParams().put(ParameterFactory.newParameter("erosion_iterations", itersE));
        scriptParameters.getParams().put(ParameterFactory.newParameter("kernel_type", kernel));
        scriptParameters.getParams().put(ParameterFactory.newParameter("kernel_size", kernelSize));
    }

    /**
     * buildComboBox.
     */
    private void buildComboBox() {

        comboBoxKernel = new JComboBox();
        comboBoxKernel.setFont(serif12);
        comboBoxKernel.setBackground(Color.white);

        if (image.getNDims() == 2) {
            comboBoxKernel.addItem("3x3 -  4 connected");
            comboBoxKernel.addItem("3x3 -  8 connected");
            comboBoxKernel.addItem("5x5 - 12 connected");
            comboBoxKernel.addItem("User sized circle.");
        } else if (image.getNDims() == 3) {
            comboBoxKernel.addItem("3x3x3 -  6 connected (2.5D: 4)");
            comboBoxKernel.addItem("3x3x3 - 26 connected (2.5D: 8)");
            comboBoxKernel.addItem("5x5x5 - 24 connected (2.5D: 12)");
            comboBoxKernel.addItem("User sized sphere.");
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Bottom hat");

        labelNIterD = new JLabel("Number of dilations (1-20)");
        labelNIterD.setForeground(Color.black);
        labelNIterD.setFont(serif12);

        textNIterD = new JTextField(5);
        textNIterD.setText("1");
        textNIterD.setFont(serif12);

        labelNIterE = new JLabel("Number of erosions (1-20)");
        labelNIterE.setForeground(Color.black);
        labelNIterE.setFont(serif12);

        textNIterE = new JTextField(5);
        textNIterE.setText("1");
        textNIterE.setFont(serif12);

        labelKernel = new JLabel("Kernel selection");
        labelKernel.setForeground(Color.black);
        labelKernel.setFont(serif12);

        buildComboBox();
        comboBoxKernel.addItemListener(this);

        String unitString = null;
        unitString = Unit.getUnitFromLegacyNum(image.getFileInfo()[0].getUnitsOfMeasure(0)).toString();

        if (image.getNDims() == 2) {
            labelKernelSize = new JLabel("Circle diameter - " + unitString);
        } else {
            labelKernelSize = new JLabel("Sphere diameter - " + unitString);
        }

        labelKernelSize.setForeground(Color.black);
        labelKernelSize.setBounds(75, 120, 155, 25);
        labelKernelSize.setFont(serif12);
        labelKernelSize.setEnabled(false);

        textKernelSize = new JTextField(5);
        textKernelSize.setText("1");
        textKernelSize.setFont(serif12);
        textKernelSize.setEnabled(false);
        
        maskPanel = new JPanel(new GridBagLayout());
        maskPanel.setForeground(Color.black);
        maskPanel.setBorder(buildTitledBorder("Parameters"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        maskPanel.add(labelNIterD, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        maskPanel.add(textNIterD, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        maskPanel.add(labelNIterE, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        maskPanel.add(textNIterE, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        maskPanel.add(labelKernel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        maskPanel.add(comboBoxKernel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        maskPanel.add(labelKernelSize, gbc);
        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        maskPanel.add(textKernelSize, gbc);

        outputPanel = new JPanelAlgorithmOutputOptions(image);

        image25D = new JCheckBox("Process image in 2.5D", false);
        image25D.setFont(serif12);

        PanelManager optionsManager = new PanelManager("Options");
        optionsManager.add(image25D);

        if (image.getNDims() == 3) {
            image25D.setEnabled(true);
        } else {
            image25D.setEnabled(false);
        }

        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(maskPanel, gbc);
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.BOTH;
        mainPanel.add(outputPanel, gbc);
        gbc.gridx = 1;
        mainPanel.add(optionsManager.getPanel(), gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

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

        String tmpStr;

        do25D = image25D.isSelected();

        tmpStr = textNIterD.getText();

        if (testParameter(tmpStr, 1, 20)) {
            itersD = Integer.valueOf(tmpStr).intValue();
        } else {
            textNIterD.requestFocus();
            textNIterD.selectAll();

            return false;
        }

        tmpStr = textNIterE.getText();

        if (testParameter(tmpStr, 1, 20)) {
            itersE = Integer.valueOf(tmpStr).intValue();
        } else {
            textNIterE.requestFocus();
            textNIterE.selectAll();

            return false;
        }

        tmpStr = textKernelSize.getText();

        float max = (float) (image.getExtents()[0] * image.getFileInfo()[0].getResolutions()[0] / 5);

        if (textKernelSize.isEnabled() == true) {

            if (testParameter(tmpStr, 0, max)) {
                kernelSize = Float.valueOf(tmpStr).floatValue();
            } else {
                textKernelSize.requestFocus();
                textKernelSize.selectAll();

                return false;
            }
        }

        if (image.getNDims() == 2) {

            if (comboBoxKernel.getSelectedIndex() == 0) {
                kernel = AlgorithmMorphology2D.CONNECTED4;
            } else if (comboBoxKernel.getSelectedIndex() == 1) {
                kernel = AlgorithmMorphology2D.CONNECTED8;
            } else if (comboBoxKernel.getSelectedIndex() == 2) {
                kernel = AlgorithmMorphology2D.CONNECTED12;
            } else if (comboBoxKernel.getSelectedIndex() == 3) {
                kernel = AlgorithmMorphology2D.SIZED_CIRCLE;
            }
        } else if (image.getNDims() == 3) {

            if (comboBoxKernel.getSelectedIndex() == 0) {
                kernel = AlgorithmMorphology3D.CONNECTED6;
            } else if (comboBoxKernel.getSelectedIndex() == 1) {
                kernel = AlgorithmMorphology3D.CONNECTED26;
            } else if (comboBoxKernel.getSelectedIndex() == 2) {
                kernel = AlgorithmMorphology3D.CONNECTED24;
            } else if (comboBoxKernel.getSelectedIndex() == 3) {
                kernel = AlgorithmMorphology3D.SIZED_SPHERE;
            }
        }

        return true;
    }

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms.Morphological");
            }

            public String getDescription() {
                return new String("Subtracts the image from the Close of the image");
            }

            public String getDescriptionLong() {
                return new String("Subtracts the image from the Close of the image");
            }

            public String getShortLabel() {
                return new String("BottomHat");
            }

            public String getLabel() {
                return new String("Bottom Hat");
            }

            public String getName() {
                return new String("Bottom Hat");
            }
            
            //NO WAY TO SAY NOT BOOLEAN?
            public Set<ImageRequirements> getInputImageRequirements() {
                return EnumSet.of(ImageRequirements.GRAYSCALE);
            }
        };
    }

    /**
     * Returns a table listing the input parameters of this algorithm (which should match up with the scripting
     * parameters used in {@link #setGUIFromParams()}).
     * 
     * @return A parameter table listing the inputs of this algorithm.
     */
   public ParameterTable createInputParameters() {
        final ParameterTable table = new ParameterTable();
        
        try {
          	
            table.put(new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(1)));
            table.put(new ParameterInt("dilation_iterations", 1));
            table.put(new ParameterInt("erosion_iterations", 1));
            
            //See buildComboBox method for information on kernel_type
            table.put(new ParameterInt("kernel_type", 1));
            table.put(new ParameterFloat("kernel_size", 1.0f));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D, false));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }

    /**
     * Returns a table listing the output parameters of this algorithm (usually just labels used to obtain output image
     * names later).
     * 
     * @return A parameter table listing the outputs of this algorithm.
     */
    public ParameterTable createOutputParameters() {
        final ParameterTable table = new ParameterTable();

        try {
            table.put(new ParameterImage(AlgorithmParameters.RESULT_IMAGE));
        } catch (final ParserException e) {
            // this shouldn't really happen since there isn't any real parsing going on...
            e.printStackTrace();
        }

        return table;
    }

    /**
     * Returns the name of an image output by this algorithm, the image returned depends on the parameter label given
     * (which can be used to retrieve the image object from the image registry).
     * 
     * @param imageParamName The output image parameter label for which to get the image name.
     * @return The image name of the requested output image parameter label.
     */
    public String getOutputImageName(final String imageParamName) {
        if (imageParamName.equals(AlgorithmParameters.RESULT_IMAGE)) {
            if (getResultImage() != null) {
                // algo produced a new result image
                return getResultImage().getImageName();
            } else {
                // algo was done in place
                return image.getImageName();
            }
        }

        Preferences.debug("Unrecognized output image parameter: " + imageParamName + "\n", Preferences.DEBUG_SCRIPTING);

        return null;
    }

    /**
     * Returns whether the action has successfully completed its execution.
     * 
     * @return True, if the action is complete. False, if the action failed or is still running.
     */
    public boolean isActionComplete() {
        return isComplete();
    }
}
