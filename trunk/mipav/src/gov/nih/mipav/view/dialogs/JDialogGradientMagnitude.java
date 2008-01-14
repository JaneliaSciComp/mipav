package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.filters.AlgorithmGradientMagnitude;
import gov.nih.mipav.model.algorithms.filters.AlgorithmGradientMagnitudeSep;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.DialogDefaultsInterface;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.JPanelAlgorithmOutputOptions;
import gov.nih.mipav.view.components.JPanelColorChannels;
import gov.nih.mipav.view.components.JPanelSigmas;
import gov.nih.mipav.view.components.PanelManager;
import gov.nih.mipav.view.components.WidgetFactory;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.io.IOException;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.JCheckBox;


/**
 * Dialog to get user input, then call the algorithm. The user is able to control the degree of blurring in all
 * dimensions and indicate if a correction factor be applied to the z-dimension to account for differing resolutions
 * between the xy resolutions (intra-plane) and the z resolution (inter-plane). The user has the option to generate a
 * new image or replace the source image. In addition the user can indicate if you wishes to have the algorithm applied
 * to whole image or to the VOI regions. In should be noted, that the algorithms are executed in their own thread.
 *
 * @version  0.1 Nov 17, 1998
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      AlgorithmGradientMagnitude
 */
public class JDialogGradientMagnitude extends JDialogScriptableBase
        implements AlgorithmInterface, DialogDefaultsInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2890654165880748219L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JPanelColorChannels colorChannelPanel;

    /** DOCUMENT ME! */
    private AlgorithmGradientMagnitude gradientMagAlgo;

    /** DOCUMENT ME! */
    private AlgorithmGradientMagnitudeSep gradientMagSepAlgo;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private boolean image25D = false;

    /** DOCUMENT ME! */
    private JCheckBox image25DCheckbox;

    /** DOCUMENT ME! */
    private JPanelAlgorithmOutputOptions outputOptionsPanel;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

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

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogGradientMagnitude() { }

    /**
     * Construct the gradient magnitude dialog.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogGradientMagnitude(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();
        loadDefaults();
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, sets variables and calls algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {

        // Object source = event.getSource();
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {

            MipavUtil.showHelp("10011");
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

        if (Preferences.is(Preferences.PREF_SAVE_DEFAULTS) && (this.getOwner() != null) && !isScriptRunning()) {
            saveDefaults();
        }
        String name = makeImageName(image.getImageName(), "_gmag");

        if (algorithm instanceof AlgorithmGradientMagnitude) {
            image.clearMask();

            if ((gradientMagAlgo.isCompleted() == true) && (resultImage != null)) {

                if (resultImage.isColorImage()) {
                    updateFileInfo(image, resultImage);
                }

                resultImage.clearMask();

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName("Gradient magnitude");
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage == null) {

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registered to the userinterface.
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
        } // if (algorithm instanceof AlgorithmGradientMagnitude)

        if (algorithm instanceof AlgorithmGradientMagnitudeSep) {
            image.clearMask();

            if (gradientMagSepAlgo.isCompleted()) {
				if (displayInNewFrame) {

					// Make result image
					if (image.getType() == ModelImage.ARGB) {
						resultImage = new ModelImage(ModelImage.ARGB, image
								.getExtents(), name);
					} else if (image.getType() == ModelImage.ARGB_USHORT) {
						resultImage = new ModelImage(ModelImage.ARGB_USHORT, image
								.getExtents(), name);
					} else if (image.getType() == ModelImage.ARGB_FLOAT) {
						resultImage = new ModelImage(ModelImage.ARGB_FLOAT, image.getExtents(), name);
					} else {

						// resultImage = new ModelImage(ModelImage.FLOAT,
						// destExtents, name, userInterface);
						resultImage = (ModelImage) image.clone();
						resultImage.setImageName(name);

						if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
							((FileInfoDicom) (resultImage.getFileInfo(0)))
									.setSecondaryCaptureTags();
						}
					}
					if (resultImage.isColorImage()) {
						updateFileInfo(image, resultImage);
					}

					resultImage.clearMask();
					try{
						resultImage.importData(0, gradientMagSepAlgo.getResultBuffer(), true);
					}catch(IOException e){
						resultImage.disposeLocal();
						MipavUtil.displayError("Algorithm Gradient Magnitude importData: Image(s) lockced.");
						return;
					}

					// The algorithm has completed and produced a new image to
					// be
					// displayed.
					try {

						// resultImage.setImageName("Gradient magnitude");
						new ViewJFrameImage(resultImage, null, new Dimension(
								610, 200));
					} catch (OutOfMemoryError error) {
						MipavUtil
								.displayError("Out of memory: unable to open new frame");
					}

				} else {

					// These next lines set the titles in all frames where the
					// source image is displayed to
					// image name so as to indicate that the image is now
					// unlocked!
					// The image frames are enabled and then registered to the
					// userinterface.
					try{
						image.importData(0, gradientMagSepAlgo.getResultBuffer(), true);
					}catch(IOException e){
						
					}
					Vector imageFrames = image.getImageFrameVector();

					for (int i = 0; i < imageFrames.size(); i++) {
						((Frame) (imageFrames.elementAt(i)))
								.setTitle(titles[i]);
						((Frame) (imageFrames.elementAt(i))).setEnabled(true);

						if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
							userInterface.registerFrame((Frame) (imageFrames
									.elementAt(i)));
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
            }
        } // if (algorithm instanceof AlgorithmGradientMagnitudeSep)

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        if (gradientMagAlgo != null) {
            Preferences.debug("Algorithm took: " + gradientMagAlgo.getElapsedTime());
            gradientMagAlgo.finalize();
            gradientMagAlgo = null;
        }

        if (gradientMagSepAlgo != null) {
            Preferences.debug("Algorithm took: " + gradientMagSepAlgo.getElapsedTime());
            gradientMagSepAlgo.finalize();
            gradientMagSepAlgo = null;
        }

        dispose();
    }

    /**
     * Construct a delimited string that contains the parameters to this algorithm.
     *
     * @param   delim  the parameter delimiter (defaults to " " if empty)
     *
     * @return  the parameter string
     */
    public String getParameterString(String delim) {

        if (delim.equals("")) {
            delim = " ";
        }

        String str = new String();
        str += outputOptionsPanel.isProcessWholeImageSet() + delim;
        str += separable + delim;
        str += image25D + delim;
        str += sigmaPanel.getUnnormalized3DSigmas()[0] + delim;
        str += sigmaPanel.getUnnormalized3DSigmas()[1] + delim;
        str += sigmaPanel.getUnnormalized3DSigmas()[2] + delim;
        str += sigmaPanel.isResolutionCorrectionEnabled() + delim;
        str += colorChannelPanel.isRedProcessingRequested() + delim;
        str += colorChannelPanel.isGreenProcessingRequested() + delim;
        str += colorChannelPanel.isBlueProcessingRequested();

        return str;
    }

    /**
     * Accessor that returns the image.
     *
     * @return  The result image.
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    // *******************************************************************
    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * Resets labels if checkboxes are checked or unchecked.
     *
     * @param  event  Event that cause the method to fire.
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();

        if (source == image25DCheckbox) {
            sigmaPanel.enable3DComponents(!image25DCheckbox.isSelected());
        }
    }

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void loadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if ((defaultsString != null) && (outputOptionsPanel != null)) {

            try {
                StringTokenizer st = new StringTokenizer(defaultsString, ",");

                outputOptionsPanel.setProcessWholeImage(MipavUtil.getBoolean(st));
                sepCheckbox.setSelected(MipavUtil.getBoolean(st));
                image25DCheckbox.setSelected(MipavUtil.getBoolean(st));
                sigmaPanel.setSigmaX(MipavUtil.getFloat(st));
                sigmaPanel.setSigmaY(MipavUtil.getFloat(st));
                sigmaPanel.setSigmaZ(MipavUtil.getFloat(st));
                sigmaPanel.enableResolutionCorrection(MipavUtil.getBoolean(st));
                colorChannelPanel.setRedProcessingRequested(MipavUtil.getBoolean(st));
                colorChannelPanel.setGreenProcessingRequested(MipavUtil.getBoolean(st));
                colorChannelPanel.setBlueProcessingRequested(MipavUtil.getBoolean(st));
                outputOptionsPanel.setOutputNewImage(MipavUtil.getBoolean(st));
            } catch (Exception ex) {
                ex.printStackTrace();
            }
        }

    }

    /**
     * Saves the default settings into the Preferences file.
     */
    public void saveDefaults() {
        String defaultsString = new String(getParameterString(",") + "," + outputOptionsPanel.isOutputNewImageSet());
        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
    }

    /**
     * Accessor that sets the slicing flag.
     *
     * @param  flag  <code>true</code> indicates slices should be blurred independently.
     */
    public void setImage25D(boolean flag) {
        image25D = flag;
    }

    /**
     * Accessor that sets whether or not the separable convolution kernel is used.
     *
     * @param  separable  DOCUMENT ME!
     */
    public void setSeparable(boolean separable) {
        this.separable = separable;
    }

    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        String name = makeImageName(image.getImageName(), "_gmag");
        displayInNewFrame = outputOptionsPanel.isOutputNewImageSet();
        if(!displayInNewFrame){
        	
        }
        if (separable) { // source image is 2D and kernel is separable
            float[] sigmas = sigmaPanel.getNormalizedSigmas();

            if (!displayInNewFrame) {
				// These next lines set the titles in all frames where the
				// source image is displayed to
				// "locked - " image name so as to indicate that the image is
				// now read/write locked!
				// The image frames are disabled and then unregisted from the
				// userinterface until the
				// algorithm has completed.
				Vector imageFrames = image.getImageFrameVector();
				titles = new String[imageFrames.size()];

				for (int i = 0; i < imageFrames.size(); i++) {
					titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
					((Frame) (imageFrames.elementAt(i))).setTitle("Locked: "
							+ titles[i]);
					((Frame) (imageFrames.elementAt(i))).setEnabled(false);
					userInterface.unregisterFrame((Frame) (imageFrames
							.elementAt(i)));
				}
			}

			try {
				// Make algorithm
				gradientMagSepAlgo = new AlgorithmGradientMagnitudeSep(image,
						sigmas, outputOptionsPanel.isProcessWholeImageSet(),
						false);

				// This is very important. Adding this object as a listener
				// allows the algorithm to
				// notify this object when it has completed of failed. See
				// algorithm performed event.
				// This is made possible by implementing AlgorithmedPerformed
				// interface
				gradientMagSepAlgo.addListener(this);

				createProgressBar(image.getImageName(), gradientMagSepAlgo);

				gradientMagSepAlgo.setRed(colorChannelPanel
						.isRedProcessingRequested());
				gradientMagSepAlgo.setGreen(colorChannelPanel
						.isGreenProcessingRequested());
				gradientMagSepAlgo.setBlue(colorChannelPanel
						.isBlueProcessingRequested());
				gradientMagSepAlgo.setMinProgressValue(0);
				gradientMagSepAlgo.setMaxProgressValue(100);
				// Hide dialog
				setVisible(false);

				if (isRunInSeparateThread()) {

					// Start the thread as a low priority because we wish to
					// still have user interface work fast
					if (gradientMagSepAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
						MipavUtil
								.displayError("A thread is already running on this object");
					}
				} else {
					gradientMagSepAlgo.run();
				}
			} catch (OutOfMemoryError x) {

				if (resultImage != null) {
					resultImage.disposeLocal(); // Clean up memory of result
					// image
					resultImage = null;
				}

				MipavUtil
						.displayError("Dialog Gradient magnitude: unable to allocate enough memory");

				return;
			}

        } else if (image.getNDims() == 2) { // source image is 2D and kernel is not separable

            int[] destExtents = new int[2];
            destExtents[0] = image.getExtents()[0]; // X dim
            destExtents[1] = image.getExtents()[1]; // Y dim

            float[] sigmas = sigmaPanel.getNormalizedSigmas();

            if (outputOptionsPanel.isOutputNewImageSet()) {

                try {

                    // Make result image
                    if (image.getType() == ModelImage.ARGB) {
                        resultImage = new ModelImage(ModelImage.ARGB, image.getExtents(), name);
                    } else if (image.getType() == ModelImage.ARGB_USHORT) {
                        resultImage = new ModelImage(ModelImage.ARGB_USHORT, image.getExtents(), name);
                    } else if (image.getType() == ModelImage.ARGB_FLOAT) {
                        resultImage = new ModelImage(ModelImage.ARGB_FLOAT, image.getExtents(), name);
                    } else {

                        // resultImage     = new ModelImage(ModelImage.FLOAT, destExtents, name, userInterface);
                        resultImage = (ModelImage) image.clone();
                        resultImage.setImageName(name);

                        if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {
                            ((FileInfoDicom) (resultImage.getFileInfo(0))).setSecondaryCaptureTags();
                        }
                    }

                    // Make algorithm
                    gradientMagAlgo = new AlgorithmGradientMagnitude(resultImage, image, sigmas,
                                                                     outputOptionsPanel.isProcessWholeImageSet(),
                                                                     false);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    gradientMagAlgo.addListener(this);

                    createProgressBar(image.getImageName(), gradientMagAlgo);

                    gradientMagAlgo.setRed(colorChannelPanel.isRedProcessingRequested());
                    gradientMagAlgo.setGreen(colorChannelPanel.isGreenProcessingRequested());
                    gradientMagAlgo.setBlue(colorChannelPanel.isBlueProcessingRequested());

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (gradientMagAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        gradientMagAlgo.run();
                    }
                } catch (OutOfMemoryError x) {

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up memory of result image
                        resultImage = null;
                    }

                    MipavUtil.displayError("Dialog Gradient magnitude: unable to allocate enough memory");

                    return;
                }
            } else {

                try {

                    // No need to make new image space because the user has choosen to replace the source image
                    // Make the algorithm class
                    gradientMagAlgo = new AlgorithmGradientMagnitude(image, sigmas,
                                                                     outputOptionsPanel.isProcessWholeImageSet(),
                                                                     false);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    gradientMagAlgo.addListener(this);

                    createProgressBar(image.getImageName(), gradientMagAlgo);

                    gradientMagAlgo.setRed(colorChannelPanel.isRedProcessingRequested());
                    gradientMagAlgo.setGreen(colorChannelPanel.isGreenProcessingRequested());
                    gradientMagAlgo.setBlue(colorChannelPanel.isBlueProcessingRequested());

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

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (gradientMagAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        gradientMagAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    System.gc();
                    MipavUtil.displayError("Dialog Gradient Magnitude: unable to allocate enough memory");

                    return;
                }
            }
        } else if (image.getNDims() >= 3) { // kernel is not separable

            float[] sigmas = sigmaPanel.getNormalizedSigmas();

            if (outputOptionsPanel.isOutputNewImageSet()) {

                try {

                    if (image.getType() == ModelImage.ARGB) {
                        resultImage = new ModelImage(ModelImage.ARGB, image.getExtents(), name);
                    } else if (image.getType() == ModelImage.ARGB_USHORT) {
                        resultImage = new ModelImage(ModelImage.ARGB_USHORT, image.getExtents(), name);
                    } else if (image.getType() == ModelImage.ARGB_FLOAT) {
                        resultImage = new ModelImage(ModelImage.ARGB_FLOAT, image.getExtents(), name);
                    } else {

                        // resultImage     = new ModelImage(ModelImage.FLOAT, destExtents, name, userInterface);
                        resultImage = (ModelImage) image.clone();
                        resultImage.setImageName(name);

                        if ((resultImage.getFileInfo()[0]).getFileFormat() == FileUtility.DICOM) {

                            for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                                ((FileInfoDicom) (resultImage.getFileInfo(i))).setSecondaryCaptureTags();
                            }
                        }
                    }

                    // Make algorithm
                    gradientMagAlgo = new AlgorithmGradientMagnitude(resultImage, image, sigmas,
                                                                     outputOptionsPanel.isProcessWholeImageSet(),
                                                                     image25D);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    gradientMagAlgo.addListener(this);

                    createProgressBar(image.getImageName(), gradientMagAlgo);

                    gradientMagAlgo.setRed(colorChannelPanel.isRedProcessingRequested());
                    gradientMagAlgo.setGreen(colorChannelPanel.isGreenProcessingRequested());
                    gradientMagAlgo.setBlue(colorChannelPanel.isBlueProcessingRequested());

                    // Hide dialog
                    setVisible(false);

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast
                        if (gradientMagAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {

                        gradientMagAlgo.run();
                    }
                } catch (OutOfMemoryError x) {

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    MipavUtil.displayError("Dialog Gradient Magnitude: unable to allocate enough memory");

                    return;
                }
            } else {

                try {

                    // Make algorithm
                    gradientMagAlgo = new AlgorithmGradientMagnitude(image, sigmas,
                                                                     outputOptionsPanel.isProcessWholeImageSet(),
                                                                     image25D);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    gradientMagAlgo.addListener(this);

                    createProgressBar(image.getImageName(), gradientMagAlgo);

                    gradientMagAlgo.setRed(colorChannelPanel.isRedProcessingRequested());
                    gradientMagAlgo.setGreen(colorChannelPanel.isGreenProcessingRequested());
                    gradientMagAlgo.setBlue(colorChannelPanel.isBlueProcessingRequested());

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
                        if (gradientMagAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        gradientMagAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    System.gc();
                    MipavUtil.displayError("Dialog Gradient magnitude: unable to allocate enough memory");

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
     * @throws  ParserException  If there is a problem creating one of the new parameters.
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
     * Initializes the GUI by creating the components, placing them in the dialog, and displaying them.
     */
    private void init() {
        setForeground(Color.black);

        getContentPane().setLayout(new BorderLayout());
        setTitle("Gradient Magnitude");

        sigmaPanel = new JPanelSigmas(image);

        sepCheckbox = WidgetFactory.buildCheckBox("Use separable convolution kernels", true);
        image25DCheckbox = WidgetFactory.buildCheckBox("Process each slice independently (2.5D)", false, this);

        if (image.getNDims() != 3) {
            image25DCheckbox.setEnabled(false);
        } else {
            image25DCheckbox.setSelected(image.getFileInfo()[0].getIs2_5D());
        }

        PanelManager kernelOptionsPanelManager = new PanelManager("Options");
        kernelOptionsPanelManager.add(sepCheckbox);
        kernelOptionsPanelManager.addOnNextLine(image25DCheckbox);

        colorChannelPanel = new JPanelColorChannels(image);
        outputOptionsPanel = new JPanelAlgorithmOutputOptions(image);

        PanelManager paramPanelManager = new PanelManager();
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
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        if (image25DCheckbox.isSelected() && (image.getNDims() > 2)) {
            image25D = true;
        } else {
            image25D = false;
        }

        if (!sigmaPanel.testSigmaValues()) {
            return false;
        }

        separable = sepCheckbox.isSelected();

        return true;
    }
}
