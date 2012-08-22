package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Creates the dialog to remove separate slices in an image. Dialog asks which slices the user wishes to remove; it
 * provides buttons to mark all slices for removal and to de-select any slices from image removal; it gives options to
 * remove or to cancel. Allows 3D or 4D images; 2D images would not make sense with this operation.**(as of 25 Oct, does
 * not yet rename removed slice image when saving)**(as of 1 November, does not yet process the more complicated DICOM
 * images completely.
 *
 * @author   David Parsons (parsonsd@cbel.cit.nih.gov) (with vast help from M.McAuliffe)
 * @version  v0.12 1 Nov 1999 (processes most images)
 */
public class JDialogRemoveSlices extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7932215505116955663L;

    /** DOCUMENT ME! */
    public static final int CHECKED_EVEN = 0;

    /** DOCUMENT ME! */
    public static final int CHECKED_ODD = 1;

    /** DOCUMENT ME! */
    public static final int USER_DEFINED = 2;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JCheckBox[] checkboxList;

    /** DOCUMENT ME! */
    private JPanel checkboxPanel;

    /** DOCUMENT ME! */
    private JButton checkButton; // dialog button to set all checks to TRUE (checked-TRUE means 'remove this slice')

    /** DOCUMENT ME! */
    private JButton checkEvenButton; // dialog button to set all checks to FALSE

    /** DOCUMENT ME! */
    private boolean[] checkListRemove;

    /** DOCUMENT ME! */
    private JButton checkOddButton; // dialog button to set all checks to TRUE (checked-TRUE means 'remove this slice')

    /** DOCUMENT ME! */
    private ButtonGroup destinationGroup;

    /** DOCUMENT ME! */
    private int displayLoc; // Flag indicating if a new image is to be generated

    // or if the source image is to be replaced

    /** DOCUMENT ME! */
    private boolean dontOpenFrame = false;

    /** DOCUMENT ME! */
    private boolean doReplace = false;

    /** DOCUMENT ME! */
    private JLabel exampleLabel;

    /** DOCUMENT ME! */
    private JLabel exampleLabel2;

    /** DOCUMENT ME! */
    private ModelImage image; // source image

    /** DOCUMENT ME! */
    private JRadioButton newImage;

    /** DOCUMENT ME! */
    private int nSlices; // number of slices in image

    /** DOCUMENT ME! */
    private int numChecked;

    /** DOCUMENT ME! */
    private int[] origExtents = null;

    /** DOCUMENT ME! */
    private int origNDims;

    /** DOCUMENT ME! */
    private boolean pressedCheckEven = false;

    /** DOCUMENT ME! */
    private boolean pressedCheckOdd = false;

    /** DOCUMENT ME! */
    private JTextField rangeField;

    /** DOCUMENT ME! */
    private AlgorithmRemoveSlices removeSlicesAlgo;

    /** DOCUMENT ME! */
    private JCheckBox replaceBox;

    /** DOCUMENT ME! */
    private JRadioButton replaceImage;

    /** DOCUMENT ME! */
    private AlgorithmReplaceRemovedSlices replaceSlicesAlgo;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JScrollPane scrollPane;

    /** DOCUMENT ME! */
    private boolean successful = false; // indicates status of algorithm

    /** DOCUMENT ME! */
    private String[] titles; // title of the frame shown when image is NULL

    /** DOCUMENT ME! */
    private JButton unCheckButton; // dialog button to set all checks to FALSE

    /** DOCUMENT ME! */
    private JCheckBox useRange;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;
    
    /** If true, insert a blank rather than a weighted average */
    private boolean insertBlank = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogRemoveSlices() { }

    /**
     * Creates new dialog for removing slices.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogRemoveSlices(Frame theParentFrame, ModelImage im) {
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
        String command = event.getActionCommand();
        int i;

        if (command.equals("Remove")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("U4059");
            MipavUtil.showWebHelp("Inserting_slices_into_image_datasets#Removing_images_.28slices.29_from_datasets");
        } else if (command.equals("Check")) {

            for (i = 0; i < nSlices; i++) {
                (checkboxList[i]).setSelected(true);
            }
        } else if (command.equals("UnCheck")) {

            for (i = 0; i < nSlices; i++) {
                (checkboxList[i]).setSelected(false);
            }
        } else if (command.equals("CheckEven")) {
            pressedCheckEven = true;

            for (i = 0; i < nSlices; i += 2) {
                (checkboxList[i]).setSelected(true);
            }
        } else if (command.equals("CheckOdd")) {
            pressedCheckOdd = true;

            for (i = 1; i < nSlices; i += 2) {
                (checkboxList[i]).setSelected(true);
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
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

        if (algorithm instanceof AlgorithmRemoveSlices) {

            if (displayLoc == NEW) {

                if ((removeSlicesAlgo.isCompleted() == true) && (resultImage != null)) {

                    // The algorithm has completed and produced a new image to be displayed.
                    try {

                        // put the new image into a new frame
                        if (!dontOpenFrame && !doReplace) {
                            new ViewJFrameImage(resultImage, null, new Dimension(25, 32));
                        }

                        successful = true;
                    } catch (OutOfMemoryError error) {
                        MipavUtil.displayError("Remove Slices reports: out of memory; " + "unable to open a new frame");
                        successful = false;
                    }

                    if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM)) {
                        int currentNum = 0;
                        Preferences.debug("\nHave removed slices:\n");

                        for (int i = 0; i < checkListRemove.length; i++) {

                            if (checkListRemove[i]) {
                                Preferences.debug("\t" + (i));

                                if (((currentNum % 5) == 4) || (currentNum == (numChecked - 1))) {
                                    Preferences.debug("\n");
                                }

                                currentNum++;
                            } // if (checkListRemove[i])
                        } // for (int i = 0; i < checkListRemove.length; i++)

                        if (image.getNDims() == 3) {
                            Preferences.debug("from " + image.getFileInfo(0).getExtents()[2] + " slice 3D " +
                                              image.getImageName() + "\n");
                        } else {
                            Preferences.debug("from " + image.getFileInfo(0).getExtents()[2] + " slice " +
                                              image.getFileInfo(0).getExtents()[3] + " volume 4D " +
                                              image.getImageName() + "\n");
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
                } else if ((removeSlicesAlgo.isCompleted() == false) && (resultImage != null)) {

                    // algorithm failed but result image still has garbage
                    resultImage.disposeLocal(); // clean up memory
                    resultImage = null;
                    System.gc();
                    successful = false;
                }
                // last case is that algorithm failed, but no image was produced.
                // since there is no image, don't need to clean up anything!


            }

            if (displayLoc == REPLACE) {
                // need to clean up locks that were set during replace.

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

                // this won't really work until the notifyImageExtentsListeners has been
                // fully implemented.
                successful = true;
                image.notifyImageExtentsListeners();

                if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM)) {
                    int currentNum = 0;
                    Preferences.debug("\nHave removed slices:\n");

                    for (int i = 0; i < checkListRemove.length; i++) {

                        if (checkListRemove[i]) {
                            Preferences.debug("\t" + (i));

                            if (((currentNum % 5) == 4) || (currentNum == (numChecked - 1))) {
                                Preferences.debug("\n");
                            }

                            currentNum++;
                        } // if (checkListRemove[i])
                    } // for (int i = 0; i < checkListRemove.length; i++)

                    if (origNDims == 3) {
                        Preferences.debug("from " + origExtents[2] + " slice 3D " + image.getImageName() + "\n");
                    } else {
                        Preferences.debug("from " + origExtents[2] + " slice " + origExtents[3] + " volume 4D " +
                                          image.getImageName() + "\n");
                    }

                    Preferences.debug("to create:\n");

                    if (image.getNDims() == 2) {
                        Preferences.debug("2D " + image.getImageName() + "\n");
                    } else if (image.getNDims() == 3) {
                        Preferences.debug(image.getFileInfo(0).getExtents()[2] + " slice 3D " + image.getImageName() +
                                          "\n");
                    } else {
                        Preferences.debug(image.getFileInfo(0).getExtents()[2] + " slice " +
                                          image.getFileInfo(0).getExtents()[3] + " volume 4D " + image.getImageName() +
                                          "\n");
                    }

                    image.getParentFrame().updateImages(true);
                } // if (Preferences.debugLevel(Preferences.DEBUG_ALGORITHM))
            }

            // will now replace slices with averages if requested
            if (doReplace && successful) {

                if (displayLoc == REPLACE) {
                    replaceSlicesAlgo = new AlgorithmReplaceRemovedSlices(image, checkListRemove, false, false,
                                                                          insertBlank);
                } else {
                    replaceSlicesAlgo = new AlgorithmReplaceRemovedSlices(resultImage, checkListRemove, false, false,
                                                                          insertBlank);
                }

                replaceSlicesAlgo.addListener(this);

                if (isRunInSeparateThread()) {

                    // Start the thread as a low priority because we wish to still have user interface work fast.
                    if (replaceSlicesAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                        MipavUtil.displayError("A thread is already running on this object");
                    }
                } else {

                    // replaceSlicesAlgo.setSeparateThread(false);
                    replaceSlicesAlgo.setRunningInSeparateThread(false);
                    replaceSlicesAlgo.run();
                }
            }


        } else if (algorithm instanceof AlgorithmReplaceRemovedSlices) {

            if (displayLoc == NEW) {

                if ((replaceSlicesAlgo.isCompleted() == true) && (resultImage != null)) {

                    try {

                        // put the new image into a new frame
                        if (!dontOpenFrame) {
                            new ViewJFrameImage(resultImage, null, new Dimension(25, 32));
                        }

                        successful = true;
                    } catch (OutOfMemoryError error) {
                        MipavUtil.displayError("Remove Slices reports: out of memory; " + "unable to open a new frame");
                        successful = false;
                    }

                }
            } else {

                if (replaceSlicesAlgo.isCompleted() == true) {
                    image.notifyImageExtentsListeners();
                    successful = true;

                }
            }
        }

        if (!doReplace && algorithm.isCompleted() && (algorithm instanceof AlgorithmRemoveSlices)) {
            insertScriptLine();
        } else if (doReplace && algorithm.isCompleted() && (algorithm instanceof AlgorithmReplaceRemovedSlices)) {
            insertScriptLine();
        }

        algorithm.finalize();
        algorithm = null;
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
    
    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeOutputImageParams(getResultImage(), (displayLoc == NEW));
        
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_replace_with_averages", doReplace));
        
        if (pressedCheckEven && isEvenSelected()) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("selection_type", CHECKED_EVEN));
        } else if (pressedCheckOdd && isOddSelected()) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("selection_type", CHECKED_ODD));
        } else {
            scriptParameters.getParams().put(ParameterFactory.newParameter("selection_type", USER_DEFINED));
            
            int numSelectedSlices = 0;
            for (int i = 0; i < nSlices; i++) {
                if (checkListRemove[i]) {
                    numSelectedSlices++;
                }
            }
            
            int[] selectedSlices = new int[numSelectedSlices];
            for (int i = 0, j = 0; i < nSlices; i++) {
                if (checkListRemove[i]) {
                    selectedSlices[j++] = i;
                }
            }
            
            scriptParameters.getParams().put(ParameterFactory.newParameter("user_selected_slices", selectedSlices));
        }
    }
    
    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();
        
        if (scriptParameters.getParams().getBoolean(AlgorithmParameters.DO_OUTPUT_NEW_IMAGE)) {
            setDisplayLocNew();
        } else {
            setDisplayLocReplace();
        }
        
        doReplace = scriptParameters.getParams().getBoolean("do_replace_with_averages");

        int removeMode = scriptParameters.getParams().getInt("selection_type");

        if (removeMode == JDialogRemoveSlices.CHECKED_EVEN) {
            setCheckListRemoveEven();
        } else if (removeMode == JDialogRemoveSlices.CHECKED_ODD) {
            setCheckListRemoveOdd();
        } else if (removeMode == JDialogRemoveSlices.USER_DEFINED) {
            int numSlices = image.getExtents()[2];
            
            boolean[] checkListRemoved = new boolean[numSlices];
            for (int i = 0; i < numSlices; i++) {
                checkListRemoved[i] = false;
            }
            
            int[] selectedSlices = scriptParameters.getParams().getList("user_selected_slices").getAsIntArray();
            for (int i = 0; i < selectedSlices.length; i++) {
                checkListRemoved[selectedSlices[i]] = true;
            }
            
            setCheckListRemove(checkListRemoved);
        }
    }
    
    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        if (displayLoc == NEW) {
            AlgorithmParameters.storeImageInRunner(getResultImage());
        }
    }

    /**
     * Returns <code>true</code> if only the even image slices have been selected.
     *
     * @return  <code>true</code> if only even slices selected to be removed, <code>false</code> otherwise
     */
    public boolean isEvenSelected() {

        for (int i = 0; i < nSlices; i++) {

            if ((((i % 2) == 0) && checkListRemove[i]) || (((i % 2) != 0) && !checkListRemove[i])) {
                return false;
            }
        }

        return true;
    }

    /**
     * Returns <code>true</code> if only the odd image slices have been selected.
     *
     * @return  <code>true</code> if only odd slices selected to be removed, <code>false</code> otherwise
     */
    public boolean isOddSelected() {

        for (int i = 0; i < nSlices; i++) {

            if ((((i % 2) == 0) && !checkListRemove[i]) || (((i % 2) != 0) && checkListRemove[i])) {
                return false;
            }
        }

        return true;
    }

    /**
     * Accessor that returns the whether or not the algorithm completed successfully.
     *
     * @return  DOCUMENT ME!
     */
    public boolean isSuccessful() {
        return successful;
    }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) {

        if (event.getSource() == useRange) {

            if (useRange.isSelected()) {
                rangeField.setEnabled(true);
                exampleLabel.setEnabled(true);
                exampleLabel2.setEnabled(true);
                checkButton.setEnabled(false);
                unCheckButton.setEnabled(false);
                checkOddButton.setEnabled(false);
                checkEvenButton.setEnabled(false);

                for (int i = 0; i < checkboxList.length; i++) {
                    checkboxList[i].setEnabled(false);
                }
            } else {
                rangeField.setEnabled(false);
                exampleLabel.setEnabled(false);
                exampleLabel2.setEnabled(false);
                checkButton.setEnabled(true);
                unCheckButton.setEnabled(true);
                checkOddButton.setEnabled(true);
                checkEvenButton.setEnabled(true);

                for (int i = 0; i < checkboxList.length; i++) {
                    checkboxList[i].setEnabled(true);
                }
            }
        }
    }

    /**
     * Accessor that sets the which slices to remove according to the boolean array paramater.
     *
     * @param  cl  for every element that is true, the slice corresponding to that element index will be removed
     */
    public void setCheckListRemove(boolean[] cl) {
        checkListRemove = cl;
    }

    /**
     * Accessor that sets the which slices to remove according to the vector of strings.
     *
     * @param  slices  - the list of slices to be removed.
     */
    public void setCheckListRemove(Vector<String> slices) {

        nSlices = image.getExtents()[2];
        checkListRemove = new boolean[nSlices];

        // set to true slices to be removed, others to false
        for (int i = 0; i < nSlices; i++) {

            if ((slices != null) && slices.contains(Integer.toString(i))) {
                checkListRemove[i] = true;
            } else {
                checkListRemove[i] = false;
            }
        }

    } // end setCheckListRemove()

    /**
     * Sets up so that only even slices will be removed.
     */
    public void setCheckListRemoveEven() {
        nSlices = image.getExtents()[2];
        checkListRemove = new boolean[nSlices];

        for (int i = 0; i < nSlices; i++) {

            if ((i % 2) == 0) {
                checkListRemove[i] = false;
            } else {
                checkListRemove[i] = true;
            }
        }
    }

    /**
     * Sets up so that only odd slices will be removed.
     */
    public void setCheckListRemoveOdd() {
        nSlices = image.getExtents()[2];
        checkListRemove = new boolean[nSlices];

        for (int i = 0; i < nSlices; i++) {

            if ((i % 2) == 0) {
                checkListRemove[i] = true;
            } else {
                checkListRemove[i] = false;
            }
        }
    }

    /**
     * Accessor that sets the display loc variable to new, so that a new image is created once the algorithm completes.
     */
    public void setDisplayLocNew() {
        displayLoc = NEW;
    }

    /**
     * Accessor that sets the display loc variable to replace, so the current image is replaced once the algorithm
     * completes.
     */
    public void setDisplayLocReplace() {
        displayLoc = REPLACE;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  dontOpenFrame  DOCUMENT ME!
     */
    public void setDontOpenFrame(boolean dontOpenFrame) {
        this.dontOpenFrame = dontOpenFrame;
    }

    /**
     * Once all the necessary variables are set, call the Remove Slices algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {
        System.gc();

        int[] destExtents = null;
        nSlices = image.getExtents()[2];
        origExtents = image.getExtents();
        origNDims = image.getNDims();
        numChecked = 0;

        for (int i = 0; i < nSlices; i++) {

            if (checkListRemove[i]) {
                numChecked++;
            }
        }

        if ((numChecked != image.getExtents()[2]) && (numChecked != 0)) {

            if (displayLoc == NEW) {

                // if the number checked is not as large as the number of slices available (if user checked them all)
                // or at least ONE is checked ...
                try {

                    if (image.getNDims() == 3) {

                        // destination image extents (length in a particular direction)
                        // if user cuts all but 1 slice, make dest a 2D image:
                        if ((image.getExtents()[2] - numChecked) == 1) {
                            destExtents = new int[2];
                            destExtents[0] = image.getExtents()[0];
                            destExtents[1] = image.getExtents()[1];
                        } // else dest will have volume, so make it a 3D image:
                        else if ((image.getExtents()[2] - numChecked) > 1) {
                            destExtents = new int[3];
                            destExtents[0] = image.getExtents()[0];
                            destExtents[1] = image.getExtents()[1];
                            destExtents[2] = image.getExtents()[2] - numChecked;
                        }
                    } // if (image.getNDims() == 3)
                    else { // 4D

                        // destination image extents (length in a particular direction)
                        // if user cuts all but 1 slice, make dest a 3D image:
                        if ((image.getExtents()[2] - numChecked) == 1) {
                            destExtents = new int[3];
                            destExtents[0] = image.getExtents()[0];
                            destExtents[1] = image.getExtents()[1];
                            destExtents[2] = image.getExtents()[3];
                        } // else dest will have 4D, so make it a 4D image:
                        else if ((image.getExtents()[2] - numChecked) > 1) {
                            destExtents = new int[4];
                            destExtents[0] = image.getExtents()[0];
                            destExtents[1] = image.getExtents()[1];
                            destExtents[2] = image.getExtents()[2] - numChecked;
                            destExtents[3] = image.getExtents()[3];
                        }
                    } // 4D

                    // Make result image of same image-type (eg., BOOLEAN, FLOAT, INT)
                    resultImage = new ModelImage(image.getType(), destExtents, image.getImageName());

                    // if dimensions have not been changed, copy the fileinfo core information into the appropriate
                    // result image fileinfos
                    if (destExtents.length == image.getExtents().length) {
                    	
                    	if (image.getMatrix().getTransformID() != TransMatrix.TRANSFORM_COMPOSITE) {
                            resultImage.getMatrixHolder().addMatrix((TransMatrix) image.getMatrix().clone());
                        }

                        FileInfoBase.copyCoreInfo(image.getFileInfo(), resultImage.getFileInfo(), checkListRemove);
                    }

                    // Make algorithm:
                    removeSlicesAlgo = new AlgorithmRemoveSlices(image, resultImage, checkListRemove);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    removeSlicesAlgo.addListener(this);
                    
                    createProgressBar(image.getImageName(), removeSlicesAlgo);
                    
                    setVisible(false); // Hide dialog

                    if (isRunInSeparateThread()) {

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (removeSlicesAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        removeSlicesAlgo.run();
                    }
                } catch (OutOfMemoryError x) {

                    if (resultImage != null) {
                        resultImage.disposeLocal(); // Clean up image memory
                        resultImage = null;
                    }

                    MipavUtil.displayError("Remove Slices reports: unable to allocate enough memory");

                    return;
                }

            } else if (displayLoc == REPLACE) {

                try {
                    // No need to make new image space because the user has
                    // choosen to replace the source image

                    // Make algorithm:
                    removeSlicesAlgo = new AlgorithmRemoveSlices(image, (ModelImage) null, checkListRemove);

                    // This is very important. Adding this object as a listener allows the algorithm to
                    // notify this object when it has completed of failed. See algorithm performed event.
                    // This is made possible by implementing AlgorithmedPerformed interface
                    removeSlicesAlgo.addListener(this);
                    
                    createProgressBar(image.getImageName(), removeSlicesAlgo);
                    
                    setVisible(false); // Hide dialog

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

                        // Start the thread as a low priority because we wish to still have user interface work fast.
                        if (removeSlicesAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                            MipavUtil.displayError("A thread is already running on this object");
                        }
                    } else {
                        removeSlicesAlgo.run();
                    }
                } catch (OutOfMemoryError x) {
                    MipavUtil.displayError("Dialog RemoveSlices: unable to allocate enough memory");

                    return;
                }

            } // end if display is LOC or REPLACE

        } else if (numChecked == 0) {
            MipavUtil.displayError("No slices were selected!  Select some slices.");
        } else {
            MipavUtil.displayError("All slices are selected!  Unselect some slices.");
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {

        // make sure that this is a 3D image first
        // make sure this image, im, is not 2D, for removing an image's only slice makes no sense...
        if ((image.getNDims() == 2) || (image.getExtents()[2] == 1)) {
            MipavUtil.displayError("Remove Slices does not make sense for single-slice (2-D)\n" +
                                   "images.  No operation may be performed.");

            return; // the wrong kind of image gets sent back before wasting anymore time.
        }

        nSlices = image.getExtents()[2];

        JPanel mainPanel = new JPanel(new BorderLayout()); // everything gets placed on this panel

        setTitle("Remove slices");
        setForeground(Color.black);

        checkboxPanel = new JPanel(); // place a check-box list in here
        checkboxPanel.setLayout(new GridLayout(nSlices, 1));
        checkboxPanel.setForeground(Color.white);
        checkboxPanel.setBackground(Color.white);
        checkboxList = new JCheckBox[nSlices]; // selector for the user to choose which slices to remove.  TRUE means
                                               // remove.

        for (int i = 0; i < nSlices; i++) { // place nSlices of check options for user and give them a name
            checkboxList[i] = new JCheckBox("Image slice index " + (String.valueOf(i)));

            // checkboxList[i].setFont(serif12B);
            checkboxList[i].setBackground(Color.white);
            checkboxPanel.add(checkboxList[i]);
        }

        // make the list scroll if there are enough checkboxes
        scrollPane = new JScrollPane(checkboxPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                     JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        mainPanel.add(scrollPane);
        mainPanel.setBorder(buildTitledBorder("Check the slice indices to remove"));
        mainPanel.setPreferredSize(new Dimension(210, 390));

        JPanel checkPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;

        // make check & uncheck buttons for the panel--place inside the above border
        checkButton = new JButton("Select all");
        checkButton.setPreferredSize(new Dimension(95, 30));
        checkButton.setMinimumSize(new Dimension(95, 30));
        checkButton.setFont(serif12B);
        checkPanel.add(checkButton, gbc);
        checkButton.addActionListener(this);
        checkButton.setActionCommand("Check");

        gbc.gridx = 1;
        unCheckButton = new JButton("Clear");
        unCheckButton.setPreferredSize(new Dimension(95, 30));
        unCheckButton.setMinimumSize(new Dimension(95, 30));
        unCheckButton.setFont(serif12B);
        unCheckButton.addActionListener(this);
        unCheckButton.setActionCommand("UnCheck");
        checkPanel.add(unCheckButton, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        checkEvenButton = new JButton("Check even");
        checkEvenButton.setPreferredSize(new Dimension(95, 30));
        checkEvenButton.setMinimumSize(new Dimension(95, 30));
        checkEvenButton.setFont(serif12B);
        checkPanel.add(checkEvenButton, gbc);
        checkEvenButton.addActionListener(this);
        checkEvenButton.setActionCommand("CheckEven");

        gbc.gridx = 1;
        checkOddButton = new JButton("Check odd");
        checkOddButton.setPreferredSize(new Dimension(95, 30));
        checkOddButton.setMinimumSize(new Dimension(95, 30));
        checkOddButton.setFont(serif12B);
        checkPanel.add(checkOddButton, gbc);
        checkOddButton.addActionListener(this);
        checkOddButton.setActionCommand("CheckOdd");

        JPanel rangePanel = new JPanel();
        rangePanel.setLayout(new BoxLayout(rangePanel, BoxLayout.Y_AXIS));
        rangePanel.setBorder(buildTitledBorder("Range of slice indices"));
        useRange = new JCheckBox("Specify range of slice indices", false);
        useRange.addItemListener(this);
        useRange.setFont(serif12B);
        exampleLabel = new JLabel("Enter slice number indices and/or slice range indices.");
        exampleLabel2 = new JLabel("For example, 0,3,5-12");
        exampleLabel.setFont(serif12);
        exampleLabel2.setFont(serif12);
        exampleLabel.setEnabled(false);
        exampleLabel2.setEnabled(false);
        rangeField = new JTextField(10);
        rangeField.setEnabled(false);
        rangePanel.add(useRange);
        rangePanel.add(exampleLabel);
        rangePanel.add(exampleLabel2);
        rangePanel.add(rangeField);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridwidth = 2;
        checkPanel.add(rangePanel, gbc);

        JPanel optionsPanel = new JPanel();
        optionsPanel.setLayout(new BoxLayout(optionsPanel, BoxLayout.Y_AXIS));

        optionsPanel.setForeground(Color.black);
        optionsPanel.setBorder(buildTitledBorder("Options"));

        replaceBox = new JCheckBox("Replace slices (using averages)", false);
        replaceBox.setFont(MipavUtil.font12);
        optionsPanel.add(replaceBox);
        if (image.getNDims() >= 4) {
            replaceBox.setEnabled(false);
        }

        gbc.gridy++;
        checkPanel.add(optionsPanel, gbc);


        // destination goes in the left of the lower box
        JPanel destinationPanel = new JPanel();
        destinationPanel.setLayout(new BoxLayout(destinationPanel, BoxLayout.Y_AXIS));

        destinationPanel.setForeground(Color.black);
        destinationPanel.setBorder(buildTitledBorder("Destination"));

        destinationGroup = new ButtonGroup();
        newImage = new JRadioButton("New image", true);
        newImage.setFont(serif12);
        destinationGroup.add(newImage); // add the button to the grouping
        destinationPanel.add(newImage); // add the button to the component

        replaceImage = new JRadioButton("Replace image", false);
        replaceImage.setFont(serif12);
        destinationGroup.add(replaceImage); // add the button to the grouping
        destinationPanel.add(replaceImage); // add the button to the component

        gbc.gridy++;
        checkPanel.add(destinationPanel, gbc);

        // Only if the image is unlocked can it be replaced.
        if (image.getLockStatus() == ModelStorageBase.UNLOCKED) {
            replaceImage.setEnabled(true);
        } else {
            replaceImage.setEnabled(false);
        }

        mainPanel.add(checkPanel, BorderLayout.SOUTH);

        JPanel buttonPanel = new JPanel(new FlowLayout());
        // Make & set the OK (remove) and Cancel buttons--place outside the border


        buttonPanel.add(buildButtons());
        OKButton.setText("Remove");

        mainDialogPanel.setLayout(new BorderLayout());
        mainDialogPanel.add(mainPanel); // put the main panel into the center of the dialog
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);
        mainDialogPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        getContentPane().add(mainDialogPanel);
        pack();
        setSize(350, 474);
        setVisible(true); // let someone see the dialog.
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        int i; // counting variable

        if (replaceImage.isSelected()) {
            displayLoc = REPLACE;
        } else if (newImage.isSelected()) {
            displayLoc = NEW;
        }

        doReplace = replaceBox.isSelected();

        // copy the selection of whether or not to remove from the list of boxes:
        checkListRemove = new boolean[nSlices];

        if (!useRange.isSelected()) {

            for (i = 0; i < nSlices; i++) {

                if (checkboxList[i].isSelected()) {
                    checkListRemove[i] = true;
                } else {
                    checkListRemove[i] = false;
                }
            }
        } else {

            for (i = 0; i < nSlices; i++) {
                checkListRemove[i] = false;
            }

            // must parse the range field
            String rangeString = rangeField.getText();
            StringTokenizer tokens = new StringTokenizer(rangeString, ",");
            boolean hasTokens = false;

            while (tokens.hasMoreTokens()) {
                hasTokens = true;

                try {
                    String temp = tokens.nextToken();
                    StringTokenizer tokens2 = new StringTokenizer(temp, "-");
                    String startString = tokens2.nextToken();

                    while (startString.startsWith(" ")) {
                        startString = startString.substring(1, startString.length());
                    }

                    int start = Integer.parseInt(startString);

                    if (!tokens2.hasMoreTokens()) {

                        if (start > checkboxList.length) {
                            MipavUtil.displayError("Must specify valid range.  Ex: 10-20, 25, 30-50");

                            return false;
                        } else {
                            checkListRemove[start] = true;
                        }
                    } else {
                        String endString = tokens2.nextToken();
                        int end = Integer.parseInt(endString);

                        if ((start > end) || (end > checkboxList.length)) {
                            MipavUtil.displayError("Must specify valid range.  Ex: 10-20, 25, 30-50");

                            return false;
                        } else {

                            for (i = start; i <= (end); i++) {
                                checkListRemove[i] = true;
                            }
                        }
                    }
                } catch (Exception ex) {
                    MipavUtil.displayError("Must specify valid range.  Ex: 10-20, 25, 30-50");

                    return false;
                }
            }

            if (!hasTokens) {
                return false;
            }
        }

        return true;
    }

}
