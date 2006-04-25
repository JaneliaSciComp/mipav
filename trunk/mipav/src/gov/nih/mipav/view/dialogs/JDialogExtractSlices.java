package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;


/**
 * Dialog to call the extractSlices. This dialog will not be visible because it does not require user input at this
 * time. It was made a dialog object to be consistent with the dialog/algorithm paradigm.
 *
 * @version  0.1 Nov 17, 1998
 * @author   Lynne M. Pusanik
 */
public class JDialogExtractSlices extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5258936423490848172L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean convert4Dto3D = false;

    /** DOCUMENT ME! */
    private Vector extractList = null; // the list of slices to extract

    /** DOCUMENT ME! */
    private AlgorithmExtractSlices extractSlicesAlgo;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private ModelImage srcImage = null; // source image

    /** these are stored as Strings. */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogExtractSlices() { }

    /**
     * Constructor.
     *
     * @param  theParentFrame  parent frame
     * @param  image           source image
     * @param  eList           DOCUMENT ME!
     */
    public JDialogExtractSlices(Frame theParentFrame, ModelImage image, Vector eList) {
        super(theParentFrame, true);

        srcImage = image;
        extractList = eList;

        userInterface = ((ViewJFrameBase) (parentFrame)).getUserInterface();
    }

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  UI     The user interface, needed to create the image frame.
     * @param  image  Source image.
     * @param  eList  DOCUMENT ME!
     */
    public JDialogExtractSlices(ViewUserInterface UI, ModelImage image, Vector eList) {
        super(false);
        userInterface = UI;
        srcImage = image;
        extractList = eList;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Script")) {
            run();
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
        ViewJFrameImage imageFrame = null;

        if (algorithm instanceof AlgorithmExtractSlices) {

            if ((extractSlicesAlgo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                try {

                    // resultImage.setImageName("Compressed image");
                    imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("ExtractSlices: Out of memory: unable to open new frame");
                }

                if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM)) {
                    Preferences.debug("\nHave extracted slices:\n");

                    for (int i = 0; i < extractList.size(); i++) {
                        String str = (String) (extractList.elementAt(i));
                        int index = str.indexOf('.');

                        if (index == -1) {
                            Preferences.debug("\t" + (Integer.valueOf(str).intValue() + 1));

                            if (((i % 5) == 4) || (i == (extractList.size() - 1))) {
                                Preferences.debug("\n");
                            }
                        } else {
                            String sliceStr = str.substring(0, index);
                            String volStr = str.substring(index + 1);
                            Preferences.debug("slice " + (Integer.valueOf(sliceStr).intValue() + 1) + " volume " +
                                              (Integer.valueOf(volStr).intValue() + 1) + "\n");
                        }
                    }

                    if (srcImage.getNDims() == 3) {
                        Preferences.debug("from " + srcImage.getFileInfo(0).getExtents()[2] + " slice 3D " +
                                          srcImage.getImageName() + "\n");
                    } else {
                        Preferences.debug("from " + srcImage.getFileInfo(0).getExtents()[2] + " slice " +
                                          srcImage.getFileInfo(0).getExtents()[3] + " volume 4D " +
                                          srcImage.getImageName() + "\n");

                    }

                    Preferences.debug("to create:\n");

                    if (resultImage.getNDims() == 2) {
                        Preferences.debug("2D " + resultImage.getImageName() + "\n");
                    } else if (resultImage.getNDims() == 3) {
                        Preferences.debug(resultImage.getFileInfo(0).getExtents()[2] + " slice 3D " +
                                          resultImage.getImageName() + "\n");
                    } else {
                        Preferences.debug(resultImage.getFileInfo(0).getExtents()[2] + " slice " +
                                          resultImage.getFileInfo(0).getExtents()[3] + " volume 4D " +
                                          resultImage.getImageName() + "\n");

                    }
                } // if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM))
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();
            }

            // Update frame
            if (parentFrame != null) {
                ((ViewJFrameBase) parentFrame).updateImages(true);
            }

            insertScriptLine(algorithm);
        }

    } // end algorithmPerformed()

    /**
     * Accessor that returns the result image.
     *
     * @return  DOCUMENT ME!
     */
    public ModelImage getResultImage() {
        return resultImage;
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
                if (userInterface.getScriptDialog().getImgTableVar(srcImage.getImageName()) == null) {

                    if (userInterface.getScriptDialog().getActiveImgTableVar(srcImage.getImageName()) == null) {
                        userInterface.getScriptDialog().putActiveVar(srcImage.getImageName());
                    }
                }

                userInterface.getScriptDialog().append("ExtractSlices " +
                                                       userInterface.getScriptDialog().getVar(srcImage.getImageName()) +
                                                       " ");

                userInterface.getScriptDialog().append(userInterface.getScriptDialog().getVar(resultImage.getImageName()) +
                                                       " ");

                for (int i = 0; i < extractList.size(); i++) {
                    userInterface.getScriptDialog().append(" " + extractList.elementAt(i));
                }

                userInterface.getScriptDialog().append("\n");
            }
        }

    }

    /**
     * Accessor that returns the whether or not the algorithm completed successfully.
     *
     * @return  DOCUMENT ME!
     */
    public boolean isSuccessful() {

        if (extractSlicesAlgo.isCompleted() && (resultImage != null)) {
            return true;
        } else {
            return false;
        }
    }

    /**
     * run.
     */
    public void run() {

        try {
            System.gc();

            int numDestSlices = extractList.size();
            int[] destExtents = null;

            // determine the extents of the destination image
            // if the number checked is not as large as the number of slices available
            // (if user checked them all) or at least ONE is checked ...
            if (numDestSlices == srcImage.getExtents()[2]) {
                MipavUtil.displayError("Extract Slices: Cannot extract all slices from image.");

                return;
            } else if (numDestSlices == 0) {
                MipavUtil.displayError("Extract Slices: Must select slices to extract from image.");

                return;
            }

            if (srcImage.getNDims() == 3) {

                // destination image extents (length in a particular direction)
                // if user only extracts 1 slice, make dest a 2D image:
                if (numDestSlices == 1) {
                    destExtents = new int[2];
                    destExtents[0] = srcImage.getExtents()[0];
                    destExtents[1] = srcImage.getExtents()[1];
                }
                // else dest will have volume, so make it a 3D image:
                else if (numDestSlices > 1) {
                    destExtents = new int[3];
                    destExtents[0] = srcImage.getExtents()[0];
                    destExtents[1] = srcImage.getExtents()[1];
                    destExtents[2] = numDestSlices;
                }
            } // end if (image.getNDims() == 3)
            else { // 4D

                // destination image extents (length in a particular direction)
                // if user extracts only 1 slice, make dest a 3D image:

                if (convert4Dto3D) {

                    if (numDestSlices == 1) {
                        destExtents = new int[2];
                        destExtents[0] = srcImage.getExtents()[0];
                        destExtents[1] = srcImage.getExtents()[1];
                    } else {
                        destExtents = new int[3];
                        destExtents[0] = srcImage.getExtents()[0];
                        destExtents[1] = srcImage.getExtents()[1];
                        destExtents[2] = numDestSlices;
                    }
                } else {

                    if (numDestSlices == 1) {
                        destExtents = new int[3];
                        destExtents[0] = srcImage.getExtents()[0];
                        destExtents[1] = srcImage.getExtents()[1];
                        destExtents[2] = srcImage.getExtents()[3];
                    }
                    // else dest will have 4D, so make it a 4D image:
                    else if (numDestSlices > 1) {
                        destExtents = new int[4];
                        destExtents[0] = srcImage.getExtents()[0];
                        destExtents[1] = srcImage.getExtents()[1];
                        destExtents[2] = numDestSlices;
                        destExtents[3] = srcImage.getExtents()[3];
                    }
                }
            } // 4D

            // Make result image of same image-type (eg., BOOLEAN, FLOAT, INT)
            resultImage = new ModelImage(srcImage.getType(), destExtents, srcImage.getImageName() + "Extract",
                                         srcImage.getUserInterface());

            // algorithm needs an array of Strings -- so convert Vector
            String[] selected = new String[extractList.size()];

            for (int i = 0; i < extractList.size(); i++) {
                selected[i] = (String) extractList.elementAt(i);
            }

            // Make algorithm:
            extractSlicesAlgo = new AlgorithmExtractSlices(srcImage, resultImage, selected);

            extractSlicesAlgo.setConvert4Dto3D(convert4Dto3D);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            extractSlicesAlgo.addListener(this);

            // Hide dialog
            setVisible(false);

            if (runInSeparateThread) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (extractSlicesAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                extractSlicesAlgo.setActiveImage(isActiveImage);

                if (!userInterface.isAppFrameVisible()) {
                    extractSlicesAlgo.setProgressBarVisible(false);
                }

                extractSlicesAlgo.run();
            }

        } catch (OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            System.gc();
            MipavUtil.displayError("Dialog Extract Slices: unable to allocate enough memory");

            return;
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
        String destImageKey = null;

        try {
            srcImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        ModelImage im = parser.getImage(srcImageKey);

        setModal(false);
        srcImage = im;
        userInterface = srcImage.getUserInterface();
        parentFrame = srcImage.getParentFrame();

        // the result image
        try {
            destImageKey = parser.getNextString();
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        try {
            Vector extractList = new Vector();

            for (;;) {

                try {
                    extractList.addElement(parser.getNextString());
                } catch (NoSuchElementException e) {
                    break;
                }
            }
        } catch (Exception e) {
            throw new IllegalArgumentException();
        }

        setActiveImage(parser.isActiveImage());
        setSeparateThread(false);
        run();
        parser.putVariable(destImageKey, getResultImage().getImageName());
    }

    /**
     * DOCUMENT ME!
     *
     * @param  doConvert  DOCUMENT ME!
     */
    public void setConvert4Dto3D(boolean doConvert) {
        this.convert4Dto3D = doConvert;
    }

} // end class JDialogExtractSlices
