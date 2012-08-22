package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.JPanelAlgorithmOutputOptions;
import gov.nih.mipav.view.components.PanelManager;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user has the option to generate a new image or replace the
 * source image. In addition the user selects if the algorithm is applied to whole image or to the VOI regions. It
 * should be noted that the algorithms are executed in their own threads.
 *
 * @version  1.0 Aug 24, 1999
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      AlgorithmMorphology2D
 */
public class JDialogParticleAnalysisNew extends JDialogScriptableBase 
	implements AlgorithmInterface, ActionDiscovery, ItemListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7849492525308767787L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JComboBox comboBoxKernelClose;

    /** DOCUMENT ME! */
    private JComboBox comboBoxKernelOpen;
    
    /** DOCUMENT ME! */
    private boolean do25D = false;

    /** Erode panel. */
    private JPanel erodePanel;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    private int itersClose, itersOpen, itersErode;

    /** DOCUMENT ME! */
    private int kernelClose = 0;

    /** DOCUMENT ME! */
    private int kernelOpen = 0;

    /** DOCUMENT ME! */
    private float kernelSizeClose;

    /** DOCUMENT ME! */
    private float kernelSizeOpen;

    /** DOCUMENT ME! */
    private JLabel labelKernelClose;

    /** DOCUMENT ME! */
    private JLabel labelKernelOpen;

    /** DOCUMENT ME! */
    private JLabel labelKernelSizeClose;

    /** DOCUMENT ME! */
    private JLabel labelKernelSizeOpen;

    /** DOCUMENT ME! */
    private JLabel labelNIterClose;

    /** DOCUMENT ME! */
    private JLabel labelNIterErode;

    /** DOCUMENT ME! */
    private JLabel labelNIterOpen;

    /** Close declearation. */
    private JPanel maskPanelClose;

    /** DOCUMENT ME! */
    private JPanel maskPanelOpen;

    /** DOCUMENT ME! */
    private AlgorithmMorphology25D particleAlgo25D = null;

    /** DOCUMENT ME! */
    private AlgorithmMorphology2D particleAlgo2D = null;

    /** DOCUMENT ME! */
    private AlgorithmMorphology3D particleAlgo3D = null;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private boolean showFrame = false;

    /** DOCUMENT ME! */
    private JCheckBox showResultCB;

    /** DOCUMENT ME! */
    private JTextField textErode;

    /** DOCUMENT ME! */
    private JTextField textKernelSizeClose;

    /** DOCUMENT ME! */
    private JTextField textKernelSizeOpen;

    /** DOCUMENT ME! */
    private JTextField textNIterClose;

    /** DOCUMENT ME! */
    private JTextField textNIterOpen;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;
    
    private JPanelAlgorithmOutputOptions outputPanel;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogParticleAnalysisNew() { }

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogParticleAnalysisNew(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        if ((im.getType() != ModelImage.BOOLEAN) && (im.getType() != ModelImage.UBYTE) &&
                (im.getType() != ModelImage.USHORT)) {
            MipavUtil.displayError("Source Image must be Boolean or UByte or UShort");
            dispose();

            return;
        }

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
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
        	//MipavUtil.showHelp("Mor012PA");
        	MipavUtil.showWebHelp("Morphology#Applying_particle_analysis");
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

            if ((particleAlgo2D.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName("Particle analysis image");
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
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(true);

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

            if ((particleAlgo25D.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName("Particle analysis image");
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
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(true);

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

            if ((particleAlgo3D.isCompleted() == true) && (resultImage != null)) {
                updateFileInfo(image, resultImage);
                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName("Particle analysis image");
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
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(true);

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
        if (particleAlgo2D != null) {
            particleAlgo2D.finalize();
            particleAlgo2D = null;
        }

        if (particleAlgo25D != null) {
            particleAlgo25D.finalize();
            particleAlgo25D = null;
        }

        if (particleAlgo3D != null) {
            particleAlgo3D.finalize();
            particleAlgo3D = null;
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
        scriptParameters.getParams().put(ParameterFactory.newParameter("open_iterations", itersOpen));
        scriptParameters.getParams().put(ParameterFactory.newParameter("erosion_iterations", itersErode));
        scriptParameters.getParams().put(ParameterFactory.newParameter("kernel_type", kernelOpen));
        scriptParameters.getParams().put(ParameterFactory.newParameter("kernel_size", kernelSizeOpen));
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
        
        setNumOpens(scriptParameters.getParams().getInt("open_iterations"));
        setNumErode(scriptParameters.getParams().getInt("erosion_iterations"));
        setKernelType(scriptParameters.getParams().getInt("kernel_type"));
        setKernelSize(scriptParameters.getParams().getFloat("kernel_size"));
    }
    
    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        if (outputPanel.isOutputNewImageSet()) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Enables text boxes depending on selection in combo box.
     *
     * @param  event  Event that triggered this function.
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (source == comboBoxKernelOpen) {

            if (comboBoxKernelOpen.getSelectedIndex() == 2) {
                textKernelSizeOpen.setEnabled(true);
                labelKernelSizeOpen.setEnabled(true);
            } else {
                textKernelSizeOpen.setEnabled(false);
                labelKernelSizeOpen.setEnabled(false);
            }
        } else if (source == comboBoxKernelClose) {

            if (comboBoxKernelClose.getSelectedIndex() == 2) {
                textKernelSizeClose.setEnabled(true);
                labelKernelSizeClose.setEnabled(true);
            } else {
                textKernelSizeClose.setEnabled(false);
                labelKernelSizeClose.setEnabled(false);
            }
        }

        if (source == showResultCB) {

            if (showResultCB.isSelected()) {
                showFrame = true;
            } else {
                showFrame = false;
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
        kernelSizeOpen = s;
    }

    /**
     * Accessor that sets the kernel type to use.
     *
     * @param  krnl  the kernel type to use (either AlgorithmMorphology2D.CONNECTED4, AlgorithmMorphology2D.CONNECTED12,
     *               AlgorithmMorphology2D.SIZED_CIRCLE, AlgorithmMorphology3D.CONNECTED6,
     *               AlgorithmMorphology3D.CONNECTED24, AlgorithmMorphology3D.SIZED_SPHERE (or the ones in
     *               AlgorithmMorphology25D))
     */
    public void setKernelType(int krnl) {
        kernelOpen = krnl;
    }

    /**
     * Accessor that sets the number of dilations to perform.
     *
     * @param  n  The number of erosions to do.
     */
    public void setNumErode(int n) {
        itersErode = n;
    }

    /**
     * Accessor that sets the number of dilations to perform.
     *
     * @param  n  The number of erosions to do.
     */
    public void setNumOpens(int n) {
        itersOpen = n;
    }

    /**
     * Builds kernel combo box.
     */
    private void buildComboBox() {

        comboBoxKernelOpen = new JComboBox();
        comboBoxKernelOpen.setFont(serif12);
        comboBoxKernelOpen.setBackground(Color.white);

        if (image.getNDims() == 2) {
            comboBoxKernelOpen.addItem("3x3 -  4 connected");
            comboBoxKernelOpen.addItem("5x5 - 12 connected");
            comboBoxKernelOpen.addItem("User sized circle.");
            comboBoxKernelOpen.setSelectedIndex(2);
        } else if (image.getNDims() == 3) {
            comboBoxKernelOpen.addItem("3x3x3 -  6 connected (2.5D: 4)");
            comboBoxKernelOpen.addItem("5x5x5 - 24 connected (2.5D: 12)");
            comboBoxKernelOpen.addItem("User sized sphere.");
        }
    }

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_particle");

        if (image.getNDims() == 2) { // source image is 2D

            int[] destExtents = new int[2];

            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            if (outputPanel.isOutputNewImageSet()) {

                try {
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    // Make algorithm
                    particleAlgo2D = new AlgorithmMorphology2D(resultImage, kernelOpen, kernelSizeOpen, kernelClose,
                                                               kernelSizeClose,
                                                               AlgorithmMorphology2D.PARTICLE_ANALYSIS_NEW, itersOpen,
                                                               itersErode, 0, 0, outputPanel.isProcessWholeImageSet(), showFrame);

                    if (outputPanel.isProcessWholeImageSet() == false) {
                        particleAlgo2D.setMask(image.generateVOIMask());
                    }

                    // particleAlgo2D = new AlgorithmMorphology2D(resultImage, kernel, 5,
                    // AlgorithmMorphology2D.PARTICLE_ANALYSIS_NEW, iters, regionFlag);
                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    particleAlgo2D.addListener(this);

                    createProgressBar(image.getImageName(), particleAlgo2D);
                    
                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (particleAlgo2D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        particleAlgo2D.run();

                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog particle analysis new: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up memory of result image
                        resultImage = null;
                    }

                    return;
                }

            } else {

                try {

                    // No need to make new image space because the user has choosen to replace the source image Make the
                    // algorithm class particleAlgo2D = new AlgorithmMorphology2D( image, kernel, kernelSize,
                    // AlgorithmMorphology2D.PARTICLE_ANALYSIS_NEW,       itersD, 0, 0, 0, regionFlag );
                    particleAlgo2D = new AlgorithmMorphology2D(image, kernelOpen, kernelSizeOpen, kernelClose, kernelSizeClose,
                                                               AlgorithmMorphology2D.PARTICLE_ANALYSIS_NEW, itersOpen,
                                                               itersErode, 0, 0, outputPanel.isProcessWholeImageSet(), showFrame);

                    if (outputPanel.isProcessWholeImageSet() == false) {
                        particleAlgo2D.setMask(image.generateVOIMask());
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    particleAlgo2D.addListener(this);

                    createProgressBar(image.getImageName(), particleAlgo2D);
                    
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
                        if (particleAlgo2D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        particleAlgo2D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog particle analysis new: unable to allocate enough memory");

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
                    particleAlgo3D = new AlgorithmMorphology3D(resultImage, kernelOpen, kernelSizeOpen,
                                                               AlgorithmMorphology3D.PARTICLE_ANALYSIS, itersClose,
                                                               itersOpen, 0, 0, outputPanel.isProcessWholeImageSet());

                    if (outputPanel.isProcessWholeImageSet() == false) {
                        particleAlgo3D.setMask(image.generateVOIMask());
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    particleAlgo3D.addListener(this);

                    createProgressBar(image.getImageName(), particleAlgo3D);
                    
                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (particleAlgo3D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        particleAlgo3D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog particle analysis new: unable to allocate enough memory");

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    return;
                }
            } else {

                try {

                    // Make algorithm
                    particleAlgo3D = new AlgorithmMorphology3D(image, kernelOpen, kernelSizeOpen,
                                                               AlgorithmMorphology3D.PARTICLE_ANALYSIS, itersClose,
                                                               itersOpen, 0, 0, outputPanel.isProcessWholeImageSet());

                    if (outputPanel.isProcessWholeImageSet() == false) {
                        particleAlgo3D.setMask(image.generateVOIMask());
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    particleAlgo3D.addListener(this);

                    createProgressBar(image.getImageName(), particleAlgo3D);
                    
                    // Hide dialog
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((ViewJFrameBase) (imageFrames.elementAt(i))).getTitle();
                        ((ViewJFrameBase) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((ViewJFrameBase) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (particleAlgo3D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        particleAlgo3D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog particle analysis new: unable to allocate enough memory");

                    return;
                }
            }
        } else if (do25D) {
            int[] destExtents = new int[3];

            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim
            destExtents[2] = image.getExtents()[2]; // Z dim

            // convert to 2.5d kernel type
            if (kernelOpen == AlgorithmMorphology3D.CONNECTED6) {
                kernelOpen = AlgorithmMorphology25D.CONNECTED4;
            } else if (kernelOpen == AlgorithmMorphology3D.CONNECTED24) {
                kernelOpen = AlgorithmMorphology25D.CONNECTED12;
            } else if (kernelOpen == AlgorithmMorphology3D.SIZED_SPHERE) {
                kernelOpen = AlgorithmMorphology25D.SIZED_CIRCLE;
            }

            if (outputPanel.isOutputNewImageSet()) {

                try {
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    // Make algorithm
                    particleAlgo25D = new AlgorithmMorphology25D(resultImage, kernelOpen, kernelSizeOpen,
                                                                 AlgorithmMorphology25D.PARTICLE_ANALYSIS, itersClose,
                                                                 0, 0, 0, outputPanel.isProcessWholeImageSet());

                    if (outputPanel.isProcessWholeImageSet() == false) {
                        particleAlgo25D.setMask(image.generateVOIMask());
                    }

                    // particleAlgo25D = new AlgorithmMorphology25D(resultImage, kernel, 5,
                    // AlgorithmMorphology25D.PARTICLE_ANALYSIS, iters, regionFlag);
                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    particleAlgo25D.addListener(this);

                    createProgressBar(image.getImageName(), particleAlgo25D);
                    
                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (particleAlgo25D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        particleAlgo25D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog particle analysis new: unable to allocate enough memory");

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
                    particleAlgo25D = new AlgorithmMorphology25D(image, kernelOpen, kernelSizeOpen,
                                                                 AlgorithmMorphology25D.PARTICLE_ANALYSIS, itersClose,
                                                                 0, 0, 0, outputPanel.isProcessWholeImageSet());

                    if (outputPanel.isProcessWholeImageSet() == false) {
                        particleAlgo25D.setMask(image.generateVOIMask());
                    }

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed or failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    particleAlgo25D.addListener(this);

                    createProgressBar(image.getImageName(), particleAlgo25D);
                    
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
                        if (particleAlgo25D.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        particleAlgo25D.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog particle analysis new: unable to allocate enough memory");

                    return;
                }
            }
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Particle Analysis New");

        labelNIterOpen = new JLabel("Number of open (1-20)");
        labelNIterOpen.setForeground(Color.black);
        labelNIterOpen.setFont(serif12);

        textNIterOpen = new JTextField(5);
        textNIterOpen.setText("1");
        textNIterOpen.setFont(serif12);

        labelKernelOpen = new JLabel("Kernel selection");
        labelKernelOpen.setForeground(Color.black);
        labelKernelOpen.setFont(serif12);

        buildComboBox();
        comboBoxKernelOpen.addItemListener(this);

        String unitString = null;

        unitString = Unit.getUnitFromLegacyNum(image.getFileInfo()[0].getUnitsOfMeasure(0)).toString();

        if (image.getNDims() == 2) {
            labelKernelSizeOpen = new JLabel("Circle diameter - " + unitString);
        } else {
            labelKernelSizeOpen = new JLabel("Sphere diameter - " + unitString);
        }

        labelKernelSizeOpen.setForeground(Color.black);
        labelKernelSizeOpen.setBounds(75, 120, 155, 25);
        labelKernelSizeOpen.setFont(serif12);
        labelKernelSizeOpen.setEnabled(false);

        textKernelSizeOpen = new JTextField(5);
        textKernelSizeOpen.setText("1");
        textKernelSizeOpen.setFont(serif12);
        textKernelSizeOpen.setEnabled(false);

        maskPanelOpen = new JPanel(new GridBagLayout());
        maskPanelOpen.setForeground(Color.black);
        maskPanelOpen.setBorder(buildTitledBorder("Open Parameters"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        maskPanelOpen.add(labelNIterOpen, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        maskPanelOpen.add(textNIterOpen, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        maskPanelOpen.add(labelKernelOpen, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        maskPanelOpen.add(comboBoxKernelOpen, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        maskPanelOpen.add(labelKernelSizeOpen, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        maskPanelOpen.add(textKernelSizeOpen, gbc);

        labelNIterClose = new JLabel("Number of close (1-20)");
        labelNIterClose.setForeground(Color.black);
        labelNIterClose.setFont(serif12);

        textNIterClose = new JTextField(5);
        textNIterClose.setText("1");
        textNIterClose.setFont(serif12);

        labelKernelClose = new JLabel("Kernel selection");
        labelKernelClose.setForeground(Color.black);
        labelKernelClose.setFont(serif12);

        comboBoxKernelClose = new JComboBox();
        comboBoxKernelClose.setFont(serif12);
        comboBoxKernelClose.setBackground(Color.white);

        if (image.getNDims() == 2) {
            comboBoxKernelClose.addItem("3x3 -  4 connected");
            comboBoxKernelClose.addItem("5x5 - 12 connected");
            comboBoxKernelClose.addItem("User sized circle.");
            comboBoxKernelClose.setSelectedIndex(2);
        } else if (image.getNDims() == 3) {
            comboBoxKernelClose.addItem("3x3x3 -  6 connected (2.5D: 4)");
            comboBoxKernelClose.addItem("5x5x5 - 24 connected (2.5D: 12)");
            comboBoxKernelClose.addItem("User sized sphere.");
        }

        comboBoxKernelClose.addItemListener(this);

        unitString = null;

        unitString = Unit.getUnitFromLegacyNum(image.getFileInfo()[0].getUnitsOfMeasure(0)).toString();

        if (image.getNDims() == 2) {
            labelKernelSizeClose = new JLabel("Circle diameter - " + unitString);
        } else {
            labelKernelSizeClose = new JLabel("Sphere diameter - " + unitString);
        }

        labelKernelSizeClose.setForeground(Color.black);
        labelKernelSizeClose.setBounds(75, 120, 155, 25);
        labelKernelSizeClose.setFont(serif12);
        labelKernelSizeClose.setEnabled(false);

        textKernelSizeClose = new JTextField(5);
        textKernelSizeClose.setText("1");
        textKernelSizeClose.setFont(serif12);
        textKernelSizeClose.setEnabled(false);

        maskPanelClose = new JPanel(new GridBagLayout());
        maskPanelClose.setForeground(Color.black);
        maskPanelClose.setBorder(buildTitledBorder("Close Parameters"));

        gbc = new GridBagConstraints();

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        maskPanelClose.add(labelNIterClose, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        maskPanelClose.add(textNIterClose, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        maskPanelClose.add(labelKernelClose, gbc);
        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        maskPanelClose.add(comboBoxKernelClose, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        maskPanelClose.add(labelKernelSizeClose, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        maskPanelClose.add(textKernelSizeClose, gbc);


        // build erode panel
        labelNIterErode = new JLabel("Number of erode (1-20)");
        labelNIterErode.setForeground(Color.black);
        labelNIterErode.setFont(serif12);

        textErode = new JTextField(5);
        textErode.setText("1");
        textErode.setFont(serif12);

        erodePanel = new JPanel(new GridBagLayout());
        erodePanel.setForeground(Color.black);
        erodePanel.setBorder(buildTitledBorder("Erode Parameters"));

        gbc = new GridBagConstraints();

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);

        /*
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        erodePanel.add(labelNIterErode, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        erodePanel.add(textErode, gbc); */

        /*image25D = new JCheckBox("Process image in 2.5D", false);
        image25D.setFont(serif12);
        if (image.getNDims() == 3) {
            image25D.setEnabled(true);
        } else {
            image25D.setEnabled(false);
        }*/
        
        showResultCB = new JCheckBox("Show intermediate result frames");
        showResultCB.addItemListener(this);
        showResultCB.setSelected(false);
        showResultCB.setFont(serif12);

        PanelManager optionsManager = new PanelManager("Options");
        //optionsManager.add(image25D);
        optionsManager.addOnNextLine(showResultCB);
        
        outputPanel = new JPanelAlgorithmOutputOptions(image);

        JPanel mainPanel = new JPanel(new GridBagLayout());

        gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(maskPanelOpen, gbc);

        
         gbc.gridx = 0;
         gbc.gridy = 1;
         gbc.gridwidth = 2;
         gbc.weightx = 1;
         gbc.fill = GridBagConstraints.HORIZONTAL; mainPanel.add(
         maskPanelClose, gbc );
         

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 2;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        mainPanel.add(erodePanel, gbc);

        JPanel rowPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.BOTH;
        rowPanel.add(outputPanel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        mainPanel.add(rowPanel, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 1;
        gbc.fill = GridBagConstraints.BOTH;
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

        textKernelSizeOpen.setEnabled(true);
        labelKernelSizeOpen.setEnabled(true);
        textKernelSizeClose.setEnabled(true);
        labelKernelSizeClose.setEnabled(true);

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

        //do25D = image25D.isSelected();

        tmpStr = textNIterOpen.getText();

        if (testParameter(tmpStr, 1, 20)) {
            itersOpen = Integer.valueOf(tmpStr).intValue();
        } else {
            textNIterOpen.requestFocus();
            textNIterOpen.selectAll();

            return false;
        }

        tmpStr = textKernelSizeOpen.getText();

        float max = (float) (image.getExtents()[0] * image.getFileInfo()[0].getResolutions()[0] / 5);

        // if ( max < 10 ) max = 10;
        if (textKernelSizeOpen.isEnabled() == true) {

            if (testParameter(tmpStr, 0, max)) {
                kernelSizeOpen = Float.valueOf(tmpStr).floatValue();
            } else {
                textKernelSizeOpen.requestFocus();
                textKernelSizeOpen.selectAll();

                return false;
            }
        }

        if (image.getNDims() == 2) {

            if (comboBoxKernelOpen.getSelectedIndex() == 0) {
                kernelOpen = AlgorithmMorphology2D.CONNECTED4;
            } else if (comboBoxKernelOpen.getSelectedIndex() == 1) {
                kernelOpen = AlgorithmMorphology2D.CONNECTED12;
            } else if (comboBoxKernelOpen.getSelectedIndex() == 2) {
                kernelOpen = AlgorithmMorphology2D.SIZED_CIRCLE;
            }
        } else if (image.getNDims() == 3) {

            if (comboBoxKernelOpen.getSelectedIndex() == 0) {
                kernelOpen = AlgorithmMorphology3D.CONNECTED6;
            } else if (comboBoxKernelOpen.getSelectedIndex() == 1) {
                kernelOpen = AlgorithmMorphology3D.CONNECTED24;
            } else if (comboBoxKernelOpen.getSelectedIndex() == 2) {
                kernelOpen = AlgorithmMorphology3D.SIZED_SPHERE;
            }
        }

        tmpStr = textNIterClose.getText();

        if (testParameter(tmpStr, 1, 20)) {
            itersClose = Integer.valueOf(tmpStr).intValue();
        } else {
            textNIterClose.requestFocus();
            textNIterClose.selectAll();

            return false;
        }

        tmpStr = textErode.getText();

        if (testParameter(tmpStr, 1, 20)) {
            itersErode = Integer.valueOf(tmpStr).intValue();
        } else {
            textErode.requestFocus();
            textErode.selectAll();

            return false;
        }

        tmpStr = textKernelSizeClose.getText();
        max = (float) (image.getExtents()[0] * image.getFileInfo()[0].getResolutions()[0] / 5);

        // if ( max < 10 ) max = 10;
        if (textKernelSizeClose.isEnabled() == true) {

            if (testParameter(tmpStr, 0, max)) {
                kernelSizeClose = Float.valueOf(tmpStr).floatValue();
            } else {
                textKernelSizeClose.requestFocus();
                textKernelSizeClose.selectAll();

                return false;
            }
        }

        if (image.getNDims() == 2) {

            if (comboBoxKernelClose.getSelectedIndex() == 0) {
                kernelClose = AlgorithmMorphology2D.CONNECTED4;
            } else if (comboBoxKernelClose.getSelectedIndex() == 1) {
                kernelClose = AlgorithmMorphology2D.CONNECTED12;
            } else if (comboBoxKernelClose.getSelectedIndex() == 2) {
                kernelClose = AlgorithmMorphology2D.SIZED_CIRCLE;
            }
        } else if (image.getNDims() == 3) {

            if (comboBoxKernelClose.getSelectedIndex() == 0) {
                kernelClose = AlgorithmMorphology3D.CONNECTED6;
            } else if (comboBoxKernelClose.getSelectedIndex() == 1) {
                kernelClose = AlgorithmMorphology3D.CONNECTED24;
            } else if (comboBoxKernelClose.getSelectedIndex() == 2) {
                kernelClose = AlgorithmMorphology3D.SIZED_SPHERE;
            }
        }

        return true;
    }
    
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms.Morphological");
            }

            public String getDescription() {
                return new String("Prepares the image for, then performs " +
                		"particleAnalysis = Ult erode & (bg dist map AND orig image) => watershed(ultErodePts, ANDED Bg Dist) => * IDobjects.");
            }

            public String getDescriptionLong() {
                return new String("Prepares the image for, then performs " +
                		"particleAnalysis = Ult erode & (bg dist map AND orig image) => watershed(ultErodePts, ANDED Bg Dist) => * IDobjects.");
            }

            public String getShortLabel() {
                return new String("ParticleAnalysis");
            }

            public String getLabel() {
                return new String("Particle Analysis");
            }

            public String getName() {
                return new String("Particle Analysis");
            }
            
            //NO WAY TO SAY BOOLEAN UBYTE OR USHORT?
            /*public Set<ImageRequirements> getInputImageRequirements() {
                return EnumSet.of(ImageRequirements.GRAYSCALE);
            }*/
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
            table.put(new ParameterInt("open_iterations", 1));
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
