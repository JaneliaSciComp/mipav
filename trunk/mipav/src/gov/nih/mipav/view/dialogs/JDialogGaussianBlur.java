package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmFFT;
import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmGaussianBlur;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.components.*;

import java.awt.*;
import java.awt.event.*;
import java.io.IOException;
import java.util.*;

import javax.swing.JCheckBox;


/**
 * Dialog to get user input, then call the algorithm. The user is able to control the degree of blurring in all
 * dimensions and indicate if a correction factor be applied to the z-dimension to account for differing resolutions
 * between the xy resolutions (intra-plane) and the z resolution (inter-plane). The user has the option to generate a
 * new image or replace the source image. In addition the user can indicate if he/she wishes to have the algorithm
 * applied to whole image or to the VOI regions. It should be noted that the algorithms are executed in their own
 * thread.
 * 
 * @version 0.1 Nov 17, 1998
 * @author Matthew J. McAuliffe, Ph.D.
 * @see AlgorithmGaussianBlur
 */
public class JDialogGaussianBlur extends JDialogScriptableBase implements AlgorithmInterface, 
        ActionDiscovery {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5074546334694615886L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JPanelColorChannels colorChannelPanel;

    /** DOCUMENT ME! */
    private AlgorithmGaussianBlur gaussianBlurAlgo;

    /** DOCUMENT ME! */
    private AlgorithmGaussianBlurSep gaussianBlurSepAlgo;

    /** Source image. */
    private ModelImage image;

    /** Flag indicating if slices should be blurred independently. */
    private boolean image25D = false;

    /** DOCUMENT ME! */
    private JCheckBox image25DCheckbox;
    
    /** Indicates whether user wants openCL for processing. */
    private JCheckBox useOCLCheckbox;

    /** DOCUMENT ME! */
    private JPanelAlgorithmOutputOptions outputOptionsPanel;

    /** Result image. */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private boolean separable = true;

    /** DOCUMENT ME! */
    private JCheckBox sepCheckbox;

    /** DOCUMENT ME! */
    private JPanelSigmas sigmaPanel;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /**
     * Empty constructor needed for dynamic instantiation.
     */
    public JDialogGaussianBlur() {}

    /**
     * Construct the gaussian blur dialog.
     * 
     * @param theParentFrame Parent frame.
     * @param im Source image.
     */
    public JDialogGaussianBlur(final Frame theParentFrame, final ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
        loadDefaults();
        setVisible(true);
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     * 
     * @param event Event that triggers function.
     */
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            // TODO: testing wiki help
            //MipavUtil.showHelp("10009");
            MipavUtil.showWebHelp("Filters_(Spatial):_Gaussian_Blur#Applying_the_Gaussian_Blur_algorithm");
        } else {
            super.actionPerformed(event);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     * 
     * @param algorithm Algorithm that caused the event.
     */
    public void algorithmPerformed(final AlgorithmBase algorithm) {

        if (Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && (this.getOwner() != null) && !isScriptRunning()) {
            saveDefaults();
        }
        final String name = JDialogBase.makeImageName(image.getImageName(), "_gblur");

        if ( algorithm instanceof OpenCLAlgorithmGaussianBlur )
        {
        	if ( algorithm.isCompleted() )
        	{
        		if ( displayInNewFrame )
        		{
        			new ViewJFrameImage( algorithm.getDestImage() );
        		}
        		else
        		{
        			// These next lines set the titles in all frames where the source image is displayed to
        			// image name so as to indicate that the image is now unlocked!
        			// The image frames are enabled and then registered to the userinterface.
        			final Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

        			for (int i = 0; i < imageFrames.size(); i++) {
        				((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
        				((Frame) (imageFrames.elementAt(i))).setEnabled(true);

        				if ( ((Frame) (imageFrames.elementAt(i))) != parentFrame) {
        					userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
        				}
        			}

        			if (parentFrame != null) {
        				userInterface.registerFrame(parentFrame);
        			}

        			image.notifyImageDisplayListeners(null, true);
        		}
        	}
        }
        
        if (algorithm instanceof AlgorithmGaussianBlur) {
            Preferences.debug("Gaussian Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if ( (gaussianBlurAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                if (resultImage.isColorImage()) {
                    JDialogBase.updateFileInfo(image, resultImage);
                }

                resultImage.clearMask();

                try {
                    openNewFrame(resultImage);
                    // openNewFrame(resultImage);
                } catch (final OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registered to the userinterface.
                final Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                for (int i = 0; i < imageFrames.size(); i++) {
                    ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                    if ( ((Frame) (imageFrames.elementAt(i))) != parentFrame) {
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
                System.gc();

            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        } // if (algorithm instanceof AlgorithmGaussianBlur)

        if (algorithm instanceof AlgorithmGaussianBlurSep) {
            Preferences.debug("GaussianSep Elapsed: " + algorithm.getElapsedTime());
            image.clearMask();

            if (gaussianBlurSepAlgo.isCompleted()) {
                if (displayInNewFrame) {
                    // Make result image
                    if (image.getType() == ModelStorageBase.ARGB) {
                        resultImage = new ModelImage(ModelStorageBase.ARGB, image.getExtents(), name);
                    } else if (image.getType() == ModelStorageBase.ARGB_USHORT) {
                        resultImage = new ModelImage(ModelStorageBase.ARGB_USHORT, image.getExtents(), name);
                    } else if (image.getType() == ModelStorageBase.ARGB_FLOAT) {
                        resultImage = new ModelImage(ModelStorageBase.ARGB_FLOAT, image.getExtents(), name);
                    } else {

                        // resultImage = new ModelImage(ModelImage.FLOAT,
                        // destExtents, name, userInterface);
                        resultImage = (ModelImage) image.clone();
                        resultImage.setImageName(name);

                        if ( (resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {

                            // For 2D Dicom set secondary capture tags only for fileinfo(0)
                            if (resultImage.getExtents().length == 2) {
                                ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                            } else {
                                for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                                    ((FileInfoDicom) (resultImage.getFileInfo(i))).setSecondaryCaptureTags();
                                }
                            }

                        }
                    }
                    try {
                        resultImage.importData(0, gaussianBlurSepAlgo.getResultBuffer(), true);
                    } catch (final IOException e) {
                        resultImage.disposeLocal();
                        MipavUtil.displayError("Algorithm Gausssian Blur importData: Image(s) Locked.");
                        return;
                    }

                    // The algorithm has completed and produced a new image to
                    // be displayed.
                    if (resultImage.isColorImage()) {
                        JDialogBase.updateFileInfo(image, resultImage);
                    }

                    resultImage.clearMask();

                    try {
                        openNewFrame(resultImage);
                    } catch (final OutOfMemoryError error) {
                        System.gc();
                        MipavUtil.displayError("Out of memory: unable to open new frame");
                    }
                } else {

                    // These next lines set the titles in all frames where the
                    // source image is displayed to
                    // image name so as to indicate that the image is now
                    // unlocked!
                    // The image frames are enabled and then registered to the
                    // userinterface.
                    try {
                        image.importData(0, gaussianBlurSepAlgo.getResultBuffer(), true);
                    } catch (final IOException e) {

                    }
                    final Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                    for (int i = 0; i < imageFrames.size(); i++) {
                        ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                        if ( ((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                            userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                        }
                    }

                    if (parentFrame != null) {
                        userInterface.registerFrame(parentFrame);
                    }
                    image.notifyImageDisplayListeners(null, true);
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();

            }

            if (algorithm.isCompleted()) {
                insertScriptLine();
            }
        } // if (algorithm instanceof AlgorithmGaussianBlurSep)

        // save the completion status for later
        setComplete(algorithm.isCompleted());

        if (gaussianBlurAlgo != null) {
            gaussianBlurAlgo.finalize();
            gaussianBlurAlgo = null;
        }

        if (gaussianBlurSepAlgo != null) {
            gaussianBlurSepAlgo.finalize();
            gaussianBlurSepAlgo = null;
        }

        dispose();
    }

    /**
     * Accessor that returns the image.
     * 
     * @return The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Changes labels based on whether or not check box is checked.
     * 
     * @param event event that cause the method to fire
     */
    public void itemStateChanged(final ItemEvent event) {
        final Object source = event.getSource();

        if (source == image25DCheckbox) {
            sigmaPanel.enable3DComponents( !image25DCheckbox.isSelected());
        }
    }

    /**
     * Accessor that sets the slicing flag.
     * 
     * @param flag <code>true</code> indicates slices should be blurred independently.
     */
    public void setImage25D(final boolean flag) {
        image25D = flag;
    }

    /**
     * Accessor that sets whether or not the separable convolution kernel is used.
     * 
     * @param separable Whether or not the separable convolution kernel is used.
     */
    public void setSeparable(final boolean separable) {
        this.separable = separable;
    }

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        final String name = JDialogBase.makeImageName(image.getImageName(), "_gblur");
        displayInNewFrame = outputOptionsPanel.isOutputNewImageSet();
        
        // Check if the algorithm should use OpenCL, calculate and return:
    	if ( useOCLCheckbox.isSelected() )
    	{
    		float[] sigmas = sigmaPanel.getNormalizedSigmas();
    		OpenCLAlgorithmGaussianBlur blurAlgo;
    		if ( displayInNewFrame )
    		{
    			resultImage = new ModelImage( image.getType(), image.getExtents(), name );
    			JDialogBase.updateFileInfo( image, resultImage );
    			blurAlgo = new OpenCLAlgorithmGaussianBlur(resultImage, image, 
    					sigmas, outputOptionsPanel.isProcessWholeImageSet(), separable, image25D);    			
    		}
    		else
    		{
                final Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                    ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                    userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                }
    			blurAlgo = new OpenCLAlgorithmGaussianBlur(image, sigmas,
    					outputOptionsPanel.isProcessWholeImageSet(), separable, image25D);
    		}
    		blurAlgo.setRed(colorChannelPanel.isRedProcessingRequested());
    		blurAlgo.setGreen(colorChannelPanel.isGreenProcessingRequested());
    		blurAlgo.setBlue(colorChannelPanel.isBlueProcessingRequested());
    		blurAlgo.addListener(this);

    		// Hide the dialog since the algorithm is about to run.
    		setVisible(false);
    		blurAlgo.run();
    		return;
        }
        if ( (image.getNDims() == 2) && separable) { // source image is 2D and
            // kernel is separable

            final float[] sigmas = sigmaPanel.getNormalizedSigmas();
            if ( !displayInNewFrame) {
                final Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                    ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                    userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                }

            }
            try {

                // Make algorithm
                gaussianBlurSepAlgo = new AlgorithmGaussianBlurSep(image, sigmas, outputOptionsPanel
                        .isProcessWholeImageSet(), false);

                // This is very important. Adding this object as a listener
                // allows the algorithm to
                // notify this object when it has completed of failed. See
                // algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed
                // interface
                gaussianBlurSepAlgo.addListener(this);

                createProgressBar(image.getImageName(), gaussianBlurSepAlgo);

                gaussianBlurSepAlgo.setRed(colorChannelPanel.isRedProcessingRequested());
                gaussianBlurSepAlgo.setGreen(colorChannelPanel.isGreenProcessingRequested());
                gaussianBlurSepAlgo.setBlue(colorChannelPanel.isBlueProcessingRequested());

                if ( !outputOptionsPanel.isProcessWholeImageSet()) {
                    gaussianBlurSepAlgo.setMask(image.generateVOIMask());
                }

                // Hide dialog
                setVisible(false);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to
                    // still have user interface work fast.
                    if (gaussianBlurSepAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {

                    gaussianBlurSepAlgo.run();
                }

                // Make result image
                if (image.getType() == ModelStorageBase.ARGB) {
                    resultImage = new ModelImage(ModelStorageBase.ARGB, image.getExtents(), name);
                } else if (image.getType() == ModelStorageBase.ARGB_USHORT) {
                    resultImage = new ModelImage(ModelStorageBase.ARGB_USHORT, image.getExtents(), name);
                } else if (image.getType() == ModelStorageBase.ARGB_FLOAT) {
                    resultImage = new ModelImage(ModelStorageBase.ARGB_FLOAT, image.getExtents(), name);
                } else {

                    // resultImage = new ModelImage(ModelImage.FLOAT,
                    // destExtents, name, userInterface);
                    resultImage = (ModelImage) image.clone();
                    resultImage.setImageName(name);

                    if ( (resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                        ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                    }
                }
            } catch (final OutOfMemoryError x) {

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up memory of result
                    // image
                    resultImage = null;
                }

                System.gc();
                MipavUtil.displayError("Dialog Gaussian blur: unable to allocate enough memory");

                return;
            }
        } else if ( (image.getNDims() >= 3) && separable) { // kernel is
            // separable

            final float[] sigmas = sigmaPanel.getNormalizedSigmas();
            if ( !displayInNewFrame) {
                final Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                titles = new String[imageFrames.size()];

                for (int i = 0; i < imageFrames.size(); i++) {
                    titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                    ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                    ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                    userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                }

            }
            try {

                // Make algorithm
                gaussianBlurSepAlgo = new AlgorithmGaussianBlurSep(image, sigmas, outputOptionsPanel
                        .isProcessWholeImageSet(), image25D);

                // This is very important. Adding this object as a listener
                // allows the algorithm to
                // notify this object when it has completed of failed. See
                // algorithm performed event.
                // This is made possible by implementing AlgorithmedPerformed
                // interface
                gaussianBlurSepAlgo.addListener(this);
                gaussianBlurSepAlgo.setRed(colorChannelPanel.isRedProcessingRequested());
                gaussianBlurSepAlgo.setGreen(colorChannelPanel.isGreenProcessingRequested());
                gaussianBlurSepAlgo.setBlue(colorChannelPanel.isBlueProcessingRequested());

                if ( !outputOptionsPanel.isProcessWholeImageSet()) {
                    gaussianBlurSepAlgo.setMask(image.generateVOIMask());
                }

                // Hide dialog
                setVisible(false);

                createProgressBar(image.getImageName(), gaussianBlurSepAlgo);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to
                    // still have user interface work fast.
                    if (gaussianBlurSepAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {
                    gaussianBlurSepAlgo.run();
                }
            } catch (final OutOfMemoryError x) {

                if (resultImage != null) {
                    resultImage.disposeLocal(); // Clean up image memory
                    resultImage = null;
                }

                System.gc();
                MipavUtil.displayError("Dialog Gaussian blur: unable to allocate enough memory");

                return;
            }
        } else if (image.getNDims() == 2) { // source image is 2D and kernel not
            // separable

            final float[] sigmas = sigmaPanel.getNormalizedSigmas();

            if (outputOptionsPanel.isOutputNewImageSet()) {

                try {

                    // Make result image
                    if (image.getType() == ModelStorageBase.ARGB) {
                        resultImage = new ModelImage(ModelStorageBase.ARGB, image.getExtents(), name);
                    } else if (image.getType() == ModelStorageBase.ARGB_USHORT) {
                        resultImage = new ModelImage(ModelStorageBase.ARGB_USHORT, image.getExtents(), name);
                    } else if (image.getType() == ModelStorageBase.ARGB_FLOAT) {
                        resultImage = new ModelImage(ModelStorageBase.ARGB_FLOAT, image.getExtents(), name);
                    } else {

                        // resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name, userInterface);
                        resultImage = (ModelImage) image.clone();
                        resultImage.setImageName(name);

                        if ( (resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                            ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                        }
                    }

                    // Make algorithm
                    gaussianBlurAlgo = new AlgorithmGaussianBlur(resultImage, image, sigmas, outputOptionsPanel
                            .isProcessWholeImageSet(), false);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    gaussianBlurAlgo.addListener(this);

                    createProgressBar(image.getImageName(), gaussianBlurAlgo);

                    gaussianBlurAlgo.setRed(colorChannelPanel.isRedProcessingRequested());
                    gaussianBlurAlgo.setGreen(colorChannelPanel.isGreenProcessingRequested());
                    gaussianBlurAlgo.setBlue(colorChannelPanel.isBlueProcessingRequested());

                    if ( !outputOptionsPanel.isProcessWholeImageSet()) {
                        gaussianBlurAlgo.setMask(image.generateVOIMask());
                    }

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (gaussianBlurAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        gaussianBlurAlgo.run();
                    }
                } catch (final OutOfMemoryError x) {

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up memory of result image
                        resultImage = null;
                    }

                    System.gc();
                    MipavUtil.displayError("Dialog Gaussian blur: unable to allocate enough memory");

                    return;
                }
            } else {

                try {

                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    gaussianBlurAlgo = new AlgorithmGaussianBlur(image, sigmas, outputOptionsPanel
                            .isProcessWholeImageSet(), false);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    gaussianBlurAlgo.addListener(this);

                    createProgressBar(image.getImageName(), gaussianBlurAlgo);

                    gaussianBlurAlgo.setRed(colorChannelPanel.isRedProcessingRequested());
                    gaussianBlurAlgo.setGreen(colorChannelPanel.isGreenProcessingRequested());
                    gaussianBlurAlgo.setBlue(colorChannelPanel.isBlueProcessingRequested());

                    if ( !outputOptionsPanel.isProcessWholeImageSet()) {
                        gaussianBlurAlgo.setMask(image.generateVOIMask());
                    }

                    // Hide the dialog since the algorithm is about to run.
                    setVisible(false);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    final Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (gaussianBlurAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        gaussianBlurAlgo.run();
                    }
                } catch (final OutOfMemoryError x) {
                    System.gc();
                    MipavUtil.displayError("Dialog Gaussian blur: unable to allocate enough memory");

                    return;
                }
            }
        } else if (image.getNDims() >= 3) { // kerenl not separable

            final float[] sigmas = sigmaPanel.getNormalizedSigmas();

            if (outputOptionsPanel.isOutputNewImageSet()) {

                try {

                    // Make result image
                    if (image.getType() == ModelStorageBase.ARGB) {
                        resultImage = new ModelImage(ModelStorageBase.ARGB, image.getExtents(), name);
                    } else if (image.getType() == ModelStorageBase.ARGB_USHORT) {
                        resultImage = new ModelImage(ModelStorageBase.ARGB_USHORT, image.getExtents(), name);
                    } else if (image.getType() == ModelStorageBase.ARGB_FLOAT) {
                        resultImage = new ModelImage(ModelStorageBase.ARGB_FLOAT, image.getExtents(), name);
                    } else {

                        // resultImage = new ModelImage(ModelImage.FLOAT, destExtents, name, userInterface);
                        resultImage = (ModelImage) image.clone();
                        resultImage.setImageName(name);

                        if ( (resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {

                            for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                                ((FileInfoDicom) (resultImage.getFileInfo(i))).setSecondaryCaptureTags();
                            }
                        }
                    }

                    // Make algorithm
                    gaussianBlurAlgo = new AlgorithmGaussianBlur(resultImage, image, sigmas, outputOptionsPanel
                            .isProcessWholeImageSet(), image25D);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    gaussianBlurAlgo.addListener(this);

                    createProgressBar(image.getImageName(), gaussianBlurAlgo);

                    gaussianBlurAlgo.setRed(colorChannelPanel.isRedProcessingRequested());
                    gaussianBlurAlgo.setGreen(colorChannelPanel.isGreenProcessingRequested());
                    gaussianBlurAlgo.setBlue(colorChannelPanel.isBlueProcessingRequested());

                    if ( !outputOptionsPanel.isProcessWholeImageSet()) {
                        gaussianBlurAlgo.setMask(image.generateVOIMask());
                    }

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (gaussianBlurAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        gaussianBlurAlgo.run();

                    }
                } catch (final OutOfMemoryError x) {

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    System.gc();
                    MipavUtil.displayError("Dialog Gaussian blur: unable to allocate enough memory");

                    return;
                }
            } else {

                try {

                    // Make algorithm
                    gaussianBlurAlgo = new AlgorithmGaussianBlur(image, sigmas, outputOptionsPanel
                            .isProcessWholeImageSet(), image25D);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    gaussianBlurAlgo.addListener(this);
                    gaussianBlurAlgo.setRed(colorChannelPanel.isRedProcessingRequested());
                    gaussianBlurAlgo.setGreen(colorChannelPanel.isGreenProcessingRequested());
                    gaussianBlurAlgo.setBlue(colorChannelPanel.isBlueProcessingRequested());

                    if ( !outputOptionsPanel.isProcessWholeImageSet()) {
                        gaussianBlurAlgo.setMask(image.generateVOIMask());
                    }

                    // Hide dialog
                    setVisible(false);

                    createProgressBar(image.getImageName(), gaussianBlurAlgo);

                    // These next lines set the titles in all frames where the source image is displayed to
                    // "locked - " image name so as to indicate that the image is now read/write locked!
                    // The image frames are disabled and then unregisted from the userinterface until the
                    // algorithm has completed.
                    final Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

                    titles = new String[imageFrames.size()];

                    for (int i = 0; i < imageFrames.size(); i++) {
                        titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                        ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                        ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                        userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
                    }

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (gaussianBlurAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        gaussianBlurAlgo.run();
                    }
                } catch (final OutOfMemoryError x) {
                    System.gc();
                    MipavUtil.displayError("Dialog Gaussian blur: unable to allocate enough memory");

                    return;
                }
            }
        }
    }

    /**
     * Perform any actions required after the running of the algorithm is complete.
     */
    protected void doPostAlgorithmActions() {

        if (outputOptionsPanel.isOutputNewImageSet()) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    /**
     * Set up the dialog GUI based on the parameters before running the algorithm as part of a script.
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        outputOptionsPanel = new JPanelAlgorithmOutputOptions(image);
        sigmaPanel = new JPanelSigmas(image);
        colorChannelPanel = new JPanelColorChannels(image);

        scriptParameters.setOutputOptionsGUI(outputOptionsPanel);
        setSeparable(scriptParameters.doProcessSeparable());
        setImage25D(scriptParameters.doProcess3DAs25D());
        scriptParameters.setSigmasGUI(sigmaPanel);
        scriptParameters.setColorOptionsGUI(colorChannelPanel);
    }

    /**
     * Store the parameters from the dialog to record the execution of this algorithm.
     * 
     * @throws ParserException If there is a problem creating one of the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(resultImage, outputOptionsPanel.isOutputNewImageSet());

        scriptParameters.storeProcessingOptions(outputOptionsPanel.isProcessWholeImageSet(), image25D);
        scriptParameters.storeProcessSeparable(separable);
        scriptParameters.storeSigmas(sigmaPanel);
        scriptParameters.storeColorOptions(colorChannelPanel);
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);

        setTitle("Gaussian Blur");
        getContentPane().setLayout(new BorderLayout());

        sigmaPanel = new JPanelSigmas(image);

        sepCheckbox = WidgetFactory.buildCheckBox("Use separable convolution kernels", true, this);
        image25DCheckbox = WidgetFactory.buildCheckBox("Process each slice independently (2.5D)", false, this);
        useOCLCheckbox = WidgetFactory.buildCheckBox("Use OpenCL", false, this);
        useOCLCheckbox.setFont(serif12);
        useOCLCheckbox.setForeground(Color.black);
    	useOCLCheckbox.setEnabled(Preferences.isGpuCompEnabled() && OpenCLAlgorithmFFT.isOCLAvailable());
    	if ( !useOCLCheckbox.isEnabled() && OpenCLAlgorithmFFT.isOCLAvailable() )
    	{
    		useOCLCheckbox.setToolTipText( "see Help->Mipav Options->Other to enable GPU computing");
    	}

        if (image.getNDims() != 3) {
            image25DCheckbox.setEnabled(false);
        } else {
            image25DCheckbox.setSelected(image.getFileInfo()[0].getIs2_5D());
        }

        final PanelManager kernelOptionsPanelManager = new PanelManager("Options");
        kernelOptionsPanelManager.add(sepCheckbox);
        kernelOptionsPanelManager.addOnNextLine(image25DCheckbox);
        kernelOptionsPanelManager.addOnNextLine(useOCLCheckbox);

        colorChannelPanel = new JPanelColorChannels(image);
        outputOptionsPanel = new JPanelAlgorithmOutputOptions(image);

        final PanelManager paramPanelManager = new PanelManager();
        paramPanelManager.add(sigmaPanel);
        paramPanelManager.addOnNextLine(kernelOptionsPanelManager.getPanel());
        paramPanelManager.addOnNextLine(colorChannelPanel);
        paramPanelManager.addOnNextLine(outputOptionsPanel);

        getContentPane().add(paramPanelManager.getPanel(), BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        setResizable(true);

        System.gc();
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     * 
     * @return <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        if (image25DCheckbox.isSelected()) {
            image25D = true;
        } else {
            image25D = false;
        }

        if ( !sigmaPanel.testSigmaValues()) {
            return false;
        }

        separable = sepCheckbox.isSelected();

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
                return new String("Algorithms.Filters (spatial)");
            }

            public String getDescription() {
                return new String("Applies a simple gaussian blur filter.");
            }

            public String getDescriptionLong() {
                return new String("Applies a simple gaussian blur filter.");
            }

            public String getShortLabel() {
                return new String("GaussianBlur");
            }

            public String getLabel() {
                return new String("Gaussian blur");
            }

            public String getName() {
                return new String("Gaussian blur");
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
            table.put(new ParameterBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_WHOLE_IMAGE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_SEPARABLE, true));
            table.put(new ParameterBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D, false));
            table.put(new ParameterList(AlgorithmParameters.SIGMAS, Parameter.PARAM_FLOAT, "1.0,1.0,1.0"));
            table.put(new ParameterBoolean(AlgorithmParameters.SIGMA_DO_Z_RES_CORRECTION, true));
            table.put(new ParameterList(AlgorithmParameters.DO_PROCESS_RGB, Parameter.PARAM_BOOLEAN, "true,true,true"));
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
