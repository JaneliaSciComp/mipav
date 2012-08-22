package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterBoolean;
import gov.nih.mipav.model.scripting.parameters.ParameterException;
import gov.nih.mipav.model.scripting.parameters.ParameterExternalImage;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.scripting.parameters.ParameterFloat;
import gov.nih.mipav.model.scripting.parameters.ParameterImage;
import gov.nih.mipav.model.scripting.parameters.ParameterInt;
import gov.nih.mipav.model.scripting.parameters.ParameterTable;
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
 * @version  1.1 March 18, 2009
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      AlgorithmMorphology2D
 */
public class JDialogMorphologicalGradient extends JDialogScriptableBase 
	implements AlgorithmInterface, ActionDiscovery {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JComboBox comboBoxKernel;

    /** DOCUMENT ME! */
    private boolean do25D = false; // do a 2.5D MORPHOLOGICAL_GRADIENT on a 3D image

    /** DOCUMENT ME! */
    private AlgorithmMorphology25D morphGradAlgo25D = null;

    /** DOCUMENT ME! */
    private AlgorithmMorphology2D morphGradAlgo2D = null;

    /** DOCUMENT ME! */
    private AlgorithmMorphology3D morphGradAlgo3D = null;
    
    private AlgorithmGrayScaleMorphology2D gsMorphGradAlgo2D = null;
    
    private AlgorithmGrayScaleMorphology25D gsMorphGradAlgo25D = null;
    
    private AlgorithmGrayScaleMorphology3D gsMorphGradAlgo3D = null;
    
    private ButtonGroup morphologyGroup;
    
    private JRadioButton binaryButton;
    
    private JRadioButton grayScaleButton;
    
    private boolean binaryMorphology = true;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JCheckBox image25D;

    /** DOCUMENT ME! */
    private int kernel = 0;

    /** DOCUMENT ME! */
    private float kernelSize;

    /** DOCUMENT ME! */
    private JLabel labelKernel;

    /** DOCUMENT ME! */
    private JLabel labelKernelSize;

    /** DOCUMENT ME! */
    private JPanel maskPanel;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JTextField textKernelSize;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;
    
    private JPanelAlgorithmOutputOptions outputPanel;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogMorphologicalGradient() { }

    /**
     * Creates new Morphological gradient dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogMorphologicalGradient(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

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
     * Record the parameters just used to run this algorithm in a script.
     * 
     * @throws ParserException If there is a problem creating/recording the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, outputPanel.isOutputNewImageSet());
        
        scriptParameters.storeProcessingOptions(outputPanel.isProcessWholeImageSet(), do25D);
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("kernel_type", kernel));
        scriptParameters.getParams().put(ParameterFactory.newParameter("kernel_size", kernelSize));
        scriptParameters.getParams().put(ParameterFactory.newParameter("binary_morphology", binaryMorphology));
    }

    /**
     * Set the dialog GUI using the script parameters while running this algorithm as part of a script.
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();
        
        if (image.isColorImage()) {
            throw new ParameterException(AlgorithmParameters.getInputImageLabel(1), "Source Image cannot be color");
        }
        
        outputPanel = new JPanelAlgorithmOutputOptions(image);
        scriptParameters.setOutputOptionsGUI(outputPanel);
        
        setImage25D(scriptParameters.doProcess3DAs25D());
        
        kernel = scriptParameters.getParams().getInt("kernel_type");
        kernelSize = scriptParameters.getParams().getFloat("kernel_size");
        binaryMorphology = scriptParameters.getParams().getBoolean("binary_morphology");
    }

    /**
     * Used to perform actions after the execution of the algorithm is completed (e.g., put the result image in the
     * image table). Defaults to no action, override to actually have it do something.
     */
    protected void doPostAlgorithmActions() {
        if (outputPanel.isOutputNewImageSet()) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    } 
    
    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     * 
     * @param event event that triggers function
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
        	//MipavUtil.showHelp("Mor003Er1");
            MipavUtil.showWebHelp("Morphology#Applying_the_erosion");
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
        if (algorithm instanceof AlgorithmMorphology2D) {
            image.clearMask();

            if ((morphGradAlgo2D.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName(MORPHOLOGICAL_GRADIENT image");
                    openNewFrame(resultImage);
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

        } else if (algorithm instanceof AlgorithmMorphology25D) {
            image.clearMask();

            if ((morphGradAlgo25D.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName(MORPHOLOGICAL_GRADIENT image");
                    openNewFrame(resultImage);
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
        } else if (algorithm instanceof AlgorithmMorphology3D) {
            image.clearMask();

            if ((morphGradAlgo3D.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName(MORPHOLOGICAL_GRADIENT image");
                    openNewFrame(resultImage);
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
        else if (algorithm instanceof AlgorithmGrayScaleMorphology2D) {
            image.clearMask();

            if ((gsMorphGradAlgo2D.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName(MORPHOLOGICAL_GRADIENT image");
                    openNewFrame(resultImage);
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

            if ((gsMorphGradAlgo25D.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName(MORPHOLOGICAL_GRADIENT image");
                    openNewFrame(resultImage);
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

            if ((gsMorphGradAlgo3D.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName(MORPHOLOGICAL_GRADIENT image");
                    openNewFrame(resultImage);
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

        if (morphGradAlgo2D != null) {
            morphGradAlgo2D.finalize();
            morphGradAlgo2D = null;
        }

        if (morphGradAlgo25D != null) {
            morphGradAlgo25D.finalize();
            morphGradAlgo25D = null;
        }

        if (morphGradAlgo3D != null) {
            morphGradAlgo3D.finalize();
            morphGradAlgo3D = null;
        }
        
        if (gsMorphGradAlgo2D != null) {
            gsMorphGradAlgo2D.finalize();
            gsMorphGradAlgo2D = null;
        }

        if (gsMorphGradAlgo25D != null) {
            gsMorphGradAlgo25D.finalize();
            gsMorphGradAlgo25D = null;
        }

        if (gsMorphGradAlgo3D != null) {
            gsMorphGradAlgo3D.finalize();
            gsMorphGradAlgo3D = null;
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
     * Sets text fields enabled depending upon which item in the combo box is selected.
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
     * @param  b  whether to do 2.5D morphology
     */
    public void setImage25D(boolean b) {
        do25D = b;
    }

    /**
     * Accessor that sets the kernel size.
     *
     * @param  s  the desired kernel size
     */
    public void setKernelSize(float s) {
        kernelSize = s;
    }

    /**
     * Accessor that sets the kernel type to use.
     *
     * @param  krnl  the kernel type to use (either AlgorithmMorphology2D.CONNECTED4, 
     *               AlgorithmMorphology2D.CONNECTED8, AlgorithmMorphology2D.CONNECTED12,
     *               AlgorithmMorphology2D.SIZED_CIRCLE, AlgorithmMorphology3D.CONNECTED6,
     *               AlgorithmMorphology3D.CONNECTED26,
     *               AlgorithmMorphology3D.CONNECTED24, AlgorithmMorphology3D.SIZED_SPHERE (or the
     *               AlgorithmMorphology25D ones))
     */
    public void setKernelType(int krnl) {
        kernel = krnl;
    }

    /**
     * Builds kernel combo box.
     */
    private void buildComboBox() {

        comboBoxKernel = new JComboBox();
        comboBoxKernel.setFont(serif12);
        comboBoxKernel.setBackground(Color.white);

        if (image.getNDims() == 2) {
            comboBoxKernel.addItem("3x3 -  4 connected");
            comboBoxKernel.addItem("3x3 - 8 connected");
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
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_morphological_gradient");

        if (binaryMorphology) {
            if (image.getNDims() == 2) { // source image is 2D
    
                if (outputPanel.isOutputNewImageSet()) {
    
                    try {
                        resultImage = (ModelImage) image.clone();
                        resultImage.setImageName(name);
    
                        // Make algorithm
                        morphGradAlgo2D = new AlgorithmMorphology2D(resultImage, kernel, kernelSize,
                                                                AlgorithmMorphology2D.MORPHOLOGICAL_GRADIENT, 0, 1, 0, 0, outputPanel.isProcessWholeImageSet());
    
                        if (outputPanel.isProcessWholeImageSet() == false) {
                            morphGradAlgo2D.setMask(image.generateVOIMask());
                        }
    
                        // This is very important. Adding this object as a listener allows the algorithm to
                        // notify this object when it has completed or failed. See algorithm performed event.
                        // This is made possible by implementing AlgorithmedPerformed interface
                        morphGradAlgo2D.addListener(this);
    
                        createProgressBar(image.getImageName(), morphGradAlgo2D);
                        
                        // Hide dialog
                        setVisible(false);
    
                        if (isRunInSeparateThread()) {
    
                            // Start the thread as a low priority because we wish to still have user interface work fast.
                            if (morphGradAlgo2D.startMethod(Thread.MIN_PRIORITY) == false) {
                                MipavUtil.displayError("A thread is already running on this object");
                            }
                        } else {
                            morphGradAlgo2D.run();
                        }
                    } catch (OutOfMemoryError x) {
                        MipavUtil.displayError("Dialog MORPHOLOGICAL_GRADIENT: unable to allocate enough memory");
    
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
                        morphGradAlgo2D = new AlgorithmMorphology2D(image, kernel, kernelSize,
                                                                AlgorithmMorphology2D.MORPHOLOGICAL_GRADIENT, 0, 1, 0, 0, outputPanel.isProcessWholeImageSet());
    
                        if (outputPanel.isProcessWholeImageSet() == false) {
                            morphGradAlgo2D.setMask(image.generateVOIMask());
                        }
    
                        // This is very important. Adding this object as a listener allows the algorithm to
                        // notify this object when it has completed or failed. See algorithm performed event.
                        // This is made possible by implementing AlgorithmedPerformed interface
                        morphGradAlgo2D.addListener(this);
    
                        createProgressBar(image.getImageName(), morphGradAlgo2D);
                        
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
                            if (morphGradAlgo2D.startMethod(Thread.MIN_PRIORITY) == false) {
                                MipavUtil.displayError("A thread is already running on this object");
                            }
                        } else {
                            morphGradAlgo2D.run();
                        }
                    } catch (OutOfMemoryError x) {
                        MipavUtil.displayError("Dialog MORPHOLOGICAL_GRADIENT: unable to allocate enough memory");
    
                        return;
                    }
                }
            } else if ((image.getNDims() == 3) && !do25D) {
    
                if (outputPanel.isOutputNewImageSet()) {
    
                    try {
                        resultImage = (ModelImage) image.clone();
                        resultImage.setImageName(name);
    
                        // Make algorithm
                        morphGradAlgo3D = new AlgorithmMorphology3D(resultImage, kernel, kernelSize,
                                                                AlgorithmMorphology3D.MORPHOLOGICAL_GRADIENT, 0, 1, 0, 0, outputPanel.isProcessWholeImageSet());
    
                        if (outputPanel.isProcessWholeImageSet() == false) {
                            morphGradAlgo3D.setMask(image.generateVOIMask());
                        }
    
                        // This is very important. Adding this object as a listener allows the algorithm to
                        // notify this object when it has completed or failed. See algorithm performed event.
                        // This is made possible by implementing AlgorithmedPerformed interface
                        morphGradAlgo3D.addListener(this);
    
                        createProgressBar(image.getImageName(), morphGradAlgo3D);
                        
                        // Hide dialog
                        setVisible(false);
    
                        if (isRunInSeparateThread()) {
    
                            // Start the thread as a low priority because we wish to still have user interface work fast
                            if (morphGradAlgo3D.startMethod(Thread.MIN_PRIORITY) == false) {
                                MipavUtil.displayError("A thread is already running on this object");
                            }
                        } else {
                            morphGradAlgo3D.run();
                        }
                    } catch (OutOfMemoryError x) {
                        MipavUtil.displayError("Dialog MORPHOLOGICAL_GRADIENT: unable to allocate enough memory");
    
                        if (resultImage != null) {
                            resultImage.disposeLocal(); // Clean up image memory
                            resultImage = null;
                        }
    
                        return;
                    }
                } else {
    
                    try {
    
                        // Make algorithm
                        morphGradAlgo3D = new AlgorithmMorphology3D(image, kernel, kernelSize,
                                                                AlgorithmMorphology3D.MORPHOLOGICAL_GRADIENT, 0, 1, 0, 0, outputPanel.isProcessWholeImageSet());
    
                        if (outputPanel.isProcessWholeImageSet() == false) {
                            morphGradAlgo3D.setMask(image.generateVOIMask());
                        }
    
                        // This is very important. Adding this object as a listener allows the algorithm to
                        // notify this object when it has completed or failed. See algorithm performed event.
                        // This is made possible by implementing AlgorithmedPerformed interface
                        morphGradAlgo3D.addListener(this);
    
                        createProgressBar(image.getImageName(), morphGradAlgo3D);
                        
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
                            if (morphGradAlgo3D.startMethod(Thread.MIN_PRIORITY) == false) {
                                MipavUtil.displayError("A thread is already running on this object");
                            }
                        } else {
                            morphGradAlgo3D.run();
                        }
                    } catch (OutOfMemoryError x) {
                        MipavUtil.displayError("Dialog MORPHOLOGICAL_GRADIENT: unable to allocate enough memory");
    
                        return;
                    }
                }
            } else if (do25D) {
    
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
                        morphGradAlgo25D = new AlgorithmMorphology25D(resultImage, kernel, kernelSize,
                                                                  AlgorithmMorphology25D.MORPHOLOGICAL_GRADIENT, 0, 1, 0, 0, outputPanel.isProcessWholeImageSet());
    
                        if (outputPanel.isProcessWholeImageSet() == false) {
                            morphGradAlgo25D.setMask(image.generateVOIMask());
                        }
    
                        // This is very important. Adding this object as a listener allows the algorithm to
                        // notify this object when it has completed or failed. See algorithm performed event.
                        // This is made possible by implementing AlgorithmedPerformed interface
                        morphGradAlgo25D.addListener(this);
    
                        createProgressBar(image.getImageName(), morphGradAlgo25D);
                        
                        // Hide dialog
                        setVisible(false);
    
                        if (isRunInSeparateThread()) {
    
                            // Start the thread as a low priority because we wish to still have user interface work fast.
                            if (morphGradAlgo25D.startMethod(Thread.MIN_PRIORITY) == false) {
                                MipavUtil.displayError("A thread is already running on this object");
                            }
                        } else {
                            morphGradAlgo25D.run();
                        }
                    } catch (OutOfMemoryError x) {
                        MipavUtil.displayError("Dialog MORPHOLOGICAL_GRADIENT: unable to allocate enough memory");
    
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
                        morphGradAlgo25D = new AlgorithmMorphology25D(image, kernel, kernelSize,
                                                                  AlgorithmMorphology25D.MORPHOLOGICAL_GRADIENT, 0, 1, 0, 0, outputPanel.isProcessWholeImageSet());
    
                        if (outputPanel.isProcessWholeImageSet() == false) {
                            morphGradAlgo25D.setMask(image.generateVOIMask());
                        }
    
                        // This is very important. Adding this object as a listener allows the algorithm to
                        // notify this object when it has completed or failed. See algorithm performed event.
                        // This is made possible by implementing AlgorithmedPerformed interface
                        morphGradAlgo25D.addListener(this);
    
                        createProgressBar(image.getImageName(), morphGradAlgo25D);
                        
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
                            if (morphGradAlgo25D.startMethod(Thread.MIN_PRIORITY) == false) {
                                MipavUtil.displayError("A thread is already running on this object");
                            }
                        } else {
                            morphGradAlgo25D.run();
                        }
                    } catch (OutOfMemoryError x) {
                        MipavUtil.displayError("Dialog MORPHOLOGICAL_GRADIENT: unable to allocate enough memory");
    
                        return;
                    }
                }
            }
        } // if (binaryMorphology)
        else { // grayScaleMorphology
            if (image.getNDims() == 2) { // source image is 2D
                
                if (outputPanel.isOutputNewImageSet()) {
    
                    try {
                        resultImage = (ModelImage) image.clone();
                        resultImage.setImageName(name);
    
                        // Make algorithm
                        gsMorphGradAlgo2D = new AlgorithmGrayScaleMorphology2D(resultImage, kernel, kernelSize,
                                                                AlgorithmGrayScaleMorphology2D.MORPHOLOGICAL_GRADIENT, 0, 1, 0, 0, outputPanel.isProcessWholeImageSet());
    
                        if (outputPanel.isProcessWholeImageSet() == false) {
                            gsMorphGradAlgo2D.setMask(image.generateVOIMask());
                        }
    
                        // This is very important. Adding this object as a listener allows the algorithm to
                        // notify this object when it has completed or failed. See algorithm performed event.
                        // This is made possible by implementing AlgorithmedPerformed interface
                        gsMorphGradAlgo2D.addListener(this);
    
                        createProgressBar(image.getImageName(), gsMorphGradAlgo2D);
                        
                        // Hide dialog
                        setVisible(false);
    
                        if (isRunInSeparateThread()) {
    
                            // Start the thread as a low priority because we wish to still have user interface work fast.
                            if (gsMorphGradAlgo2D.startMethod(Thread.MIN_PRIORITY) == false) {
                                MipavUtil.displayError("A thread is already running on this object");
                            }
                        } else {
                            gsMorphGradAlgo2D.run();
                        }
                    } catch (OutOfMemoryError x) {
                        MipavUtil.displayError("Dialog MORPHOLOGICAL_GRADIENT: unable to allocate enough memory");
    
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
                        gsMorphGradAlgo2D = new AlgorithmGrayScaleMorphology2D(image, kernel, kernelSize,
                                                                AlgorithmGrayScaleMorphology2D.MORPHOLOGICAL_GRADIENT, 0, 1, 0, 0, outputPanel.isProcessWholeImageSet());
    
                        if (outputPanel.isProcessWholeImageSet() == false) {
                            gsMorphGradAlgo2D.setMask(image.generateVOIMask());
                        }
    
                        // This is very important. Adding this object as a listener allows the algorithm to
                        // notify this object when it has completed or failed. See algorithm performed event.
                        // This is made possible by implementing AlgorithmedPerformed interface
                        gsMorphGradAlgo2D.addListener(this);
    
                        createProgressBar(image.getImageName(), gsMorphGradAlgo2D);
                        
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
                            if (gsMorphGradAlgo2D.startMethod(Thread.MIN_PRIORITY) == false) {
                                MipavUtil.displayError("A thread is already running on this object");
                            }
                        } else {
                            gsMorphGradAlgo2D.run();
                        }
                    } catch (OutOfMemoryError x) {
                        MipavUtil.displayError("Dialog MORPHOLOGICAL_GRADIENT: unable to allocate enough memory");
    
                        return;
                    }
                }
            } else if ((image.getNDims() == 3) && !do25D) {
    
                if (outputPanel.isOutputNewImageSet()) {
    
                    try {
                        resultImage = (ModelImage) image.clone();
                        resultImage.setImageName(name);
    
                        // Make algorithm
                        gsMorphGradAlgo3D = new AlgorithmGrayScaleMorphology3D(resultImage, kernel, kernelSize,
                                                                AlgorithmGrayScaleMorphology3D.MORPHOLOGICAL_GRADIENT, 0, 1, 0, 0, outputPanel.isProcessWholeImageSet());
    
                        if (outputPanel.isProcessWholeImageSet() == false) {
                            gsMorphGradAlgo3D.setMask(image.generateVOIMask());
                        }
    
                        // This is very important. Adding this object as a listener allows the algorithm to
                        // notify this object when it has completed or failed. See algorithm performed event.
                        // This is made possible by implementing AlgorithmedPerformed interface
                        gsMorphGradAlgo3D.addListener(this);
    
                        createProgressBar(image.getImageName(), gsMorphGradAlgo3D);
                        
                        // Hide dialog
                        setVisible(false);
    
                        if (isRunInSeparateThread()) {
    
                            // Start the thread as a low priority because we wish to still have user interface work fast
                            if (gsMorphGradAlgo3D.startMethod(Thread.MIN_PRIORITY) == false) {
                                MipavUtil.displayError("A thread is already running on this object");
                            }
                        } else {
                            gsMorphGradAlgo3D.run();
                        }
                    } catch (OutOfMemoryError x) {
                        MipavUtil.displayError("Dialog MORPHOLOGICAL_GRADIENT: unable to allocate enough memory");
    
                        if (resultImage != null) {
                            resultImage.disposeLocal(); // Clean up image memory
                            resultImage = null;
                        }
    
                        return;
                    }
                } else {
    
                    try {
    
                        // Make algorithm
                        gsMorphGradAlgo3D = new AlgorithmGrayScaleMorphology3D(image, kernel, kernelSize,
                                                                AlgorithmGrayScaleMorphology3D.MORPHOLOGICAL_GRADIENT, 0, 1, 0, 0, outputPanel.isProcessWholeImageSet());
    
                        if (outputPanel.isProcessWholeImageSet() == false) {
                            gsMorphGradAlgo3D.setMask(image.generateVOIMask());
                        }
    
                        // This is very important. Adding this object as a listener allows the algorithm to
                        // notify this object when it has completed or failed. See algorithm performed event.
                        // This is made possible by implementing AlgorithmedPerformed interface
                        gsMorphGradAlgo3D.addListener(this);
    
                        createProgressBar(image.getImageName(), gsMorphGradAlgo3D);
                        
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
                            if (gsMorphGradAlgo3D.startMethod(Thread.MIN_PRIORITY) == false) {
                                MipavUtil.displayError("A thread is already running on this object");
                            }
                        } else {
                            gsMorphGradAlgo3D.run();
                        }
                    } catch (OutOfMemoryError x) {
                        MipavUtil.displayError("Dialog MORPHOLOGICAL_GRADIENT: unable to allocate enough memory");
    
                        return;
                    }
                }
            } else if (do25D) {
    
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
                        gsMorphGradAlgo25D = new AlgorithmGrayScaleMorphology25D(resultImage, kernel, kernelSize,
                                                                  AlgorithmGrayScaleMorphology25D.MORPHOLOGICAL_GRADIENT, 0, 1, 0, 0, outputPanel.isProcessWholeImageSet());
    
                        if (outputPanel.isProcessWholeImageSet() == false) {
                            gsMorphGradAlgo25D.setMask(image.generateVOIMask());
                        }
    
                        // This is very important. Adding this object as a listener allows the algorithm to
                        // notify this object when it has completed or failed. See algorithm performed event.
                        // This is made possible by implementing AlgorithmedPerformed interface
                        gsMorphGradAlgo25D.addListener(this);
    
                        createProgressBar(image.getImageName(), gsMorphGradAlgo25D);
                        
                        // Hide dialog
                        setVisible(false);
    
                        if (isRunInSeparateThread()) {
    
                            // Start the thread as a low priority because we wish to still have user interface work fast.
                            if (gsMorphGradAlgo25D.startMethod(Thread.MIN_PRIORITY) == false) {
                                MipavUtil.displayError("A thread is already running on this object");
                            }
                        } else {
                            gsMorphGradAlgo25D.run();
                        }
                    } catch (OutOfMemoryError x) {
                        MipavUtil.displayError("Dialog MORPHOLOGICAL_GRADIENT: unable to allocate enough memory");
    
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
                        gsMorphGradAlgo25D = new AlgorithmGrayScaleMorphology25D(image, kernel, kernelSize,
                                                                  AlgorithmGrayScaleMorphology25D.MORPHOLOGICAL_GRADIENT, 0, 1, 0, 0, outputPanel.isProcessWholeImageSet());
    
                        if (outputPanel.isProcessWholeImageSet() == false) {
                            gsMorphGradAlgo25D.setMask(image.generateVOIMask());
                        }
    
                        // This is very important. Adding this object as a listener allows the algorithm to
                        // notify this object when it has completed or failed. See algorithm performed event.
                        // This is made possible by implementing AlgorithmedPerformed interface
                        gsMorphGradAlgo25D.addListener(this);
    
                        createProgressBar(image.getImageName(), gsMorphGradAlgo25D);
                        
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
                            if (gsMorphGradAlgo25D.startMethod(Thread.MIN_PRIORITY) == false) {
                                MipavUtil.displayError("A thread is already running on this object");
                            }
                        } else {
                            gsMorphGradAlgo25D.run();
                        }
                    } catch (OutOfMemoryError x) {
                        MipavUtil.displayError("Dialog MORPHOLOGICAL_GRADIENT: unable to allocate enough memory");
    
                        return;
                    }
                }
            }    
        } // else grayScaleMorphology
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Morphological gradient");

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
        
        morphologyGroup = new ButtonGroup();
        binaryButton = new JRadioButton("Binary morphology");
        if ((image.getType() == ModelImage.BOOLEAN) || (image.getType() == ModelImage.UBYTE) ||
            (image.getType() == ModelImage.USHORT)) {
            binaryButton.setSelected(true);
        }
        else {
            binaryButton.setSelected(false);
        }
        if ((image.getType() == ModelImage.UBYTE) || (image.getType() == ModelImage.USHORT)) {
            binaryButton.setEnabled(true);
        }
        else {
            binaryButton.setEnabled(false);
        }
        binaryButton.setFont(serif12);
        binaryButton.setForeground(Color.black);
        morphologyGroup.add(binaryButton);
        
        grayScaleButton = new JRadioButton("Gray scale morphology");
        if ((image.getType() != ModelImage.BOOLEAN) && (image.getType() != ModelImage.UBYTE) &&
            (image.getType() != ModelImage.USHORT)) {
            grayScaleButton.setSelected(true);
        }
        else {
            grayScaleButton.setSelected(false);
        }
        if ((image.getType() == ModelImage.UBYTE) || (image.getType() == ModelImage.USHORT)) {
            grayScaleButton.setEnabled(true);
        }
        else {
           grayScaleButton.setEnabled(false);
        }
        grayScaleButton.setFont(serif12);
        grayScaleButton.setForeground(Color.black);
        morphologyGroup.add(grayScaleButton);

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
        maskPanel.add(labelKernel, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        maskPanel.add(comboBoxKernel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        maskPanel.add(labelKernelSize, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        maskPanel.add(textKernelSize, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        maskPanel.add(binaryButton, gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        maskPanel.add(grayScaleButton, gbc);

        outputPanel = new JPanelAlgorithmOutputOptions(image);
        
        PanelManager optionsPanelManager = new PanelManager("Options");
        image25D = WidgetFactory.buildCheckBox("Process image in 2.5D", false);
        if (image.getNDims() == 3) {
            image25D.setEnabled(true);
        } else {
            image25D.setEnabled(false);
        }
        optionsPanelManager.add(image25D);

        PanelManager mainPanelManager = new PanelManager();
        mainPanelManager.add(maskPanel);
        mainPanelManager.addOnNextLine(outputPanel);
        mainPanelManager.addOnNextLine(optionsPanelManager.getPanel());

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);
        buildHelpButton();
        buttonPanel.add(helpButton);

        getContentPane().add(mainPanelManager.getPanel());
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
        String tmpStr;

        System.gc();

        do25D = image25D.isSelected();

        tmpStr = textKernelSize.getText();

        float max = (float) (image.getExtents()[0] * image.getFileInfo()[0].getResolutions()[0] / 5);

        // if ( max < 10 ) max = 10;
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
        
        binaryMorphology = binaryButton.isSelected();
        
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
                return new String("Erodes and dilates an image.");
            }

            public String getDescriptionLong() {
                return new String("Erodes and dilates an image using " +
                		"the indicated kernel and the indicated number of executions.");
            }

            public String getShortLabel() {
                return new String("MorphologicalGradient");
            }

            public String getLabel() {
                return new String("Morphological Gradient");
            }

            public String getName() {
                return new String("Morphological Gradient");
            }
            
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
            
            //See buildComboBox method for information on kernel_type
            table.put(new ParameterInt("kernel_type", 1));
            table.put(new ParameterFloat("kernel_size", 1.0f));
            table.put(new ParameterBoolean("binary_morphology", false));
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
