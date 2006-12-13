package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call algorithmChamfer. Selects image is match image, the image that gets transformed
 * until it is registered to the base image. Algorithms are executed in their own thread.
 *
 * @version  0.1 May 19, 1999
 * @author   Delia McGarry
 */
public class JDialogRegistrationLeastSquares extends JDialogScriptableBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2171665599919057675L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Base image - register match image to base image. */
    private ModelImage baseImage;

    /** Combo box with image names for choosing base image. */
    private JComboBox comboBoxImage;

    /** Number of dimensions in match image. */
    private int DIM;

    /** DOCUMENT ME! */
    private boolean fromOAR3D = false;

    /** DOCUMENT ME! */
    private boolean lsCompleted = false;

    /** Algorithm to run from this dialog. */
    private AlgorithmRegLeastSquares LSMatch = null;

    /** Match image - register match image to base image. */
    private ModelImage matchImage;

    /** Result image - image returned from registration algorithm. */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private TransMatrix resultMatrix = null;

    /** Used to lock and unlock images. */
    private String[] titles;

    /** Reference to userface. */
    private ViewUserInterface userInterface;

    /** Dimensions of match image and base image. */
    private int xdimA, ydimA, zdimA;

    /** Resolutions of match image and base image. */
    private float xresA, yresA, zresA, xresB, yresB, zresB;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogRegistrationLeastSquares() { }

    /**
     * Creates new registration dialog to get base image name.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogRegistrationLeastSquares(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        matchImage = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    /**
     * Creates a new JDialogRegistrationLeastSquares object.
     *
     * @param  theParentFrame  DOCUMENT ME!
     * @param  _mi             DOCUMENT ME!
     * @param  _ri             DOCUMENT ME!
     */
    public JDialogRegistrationLeastSquares(Frame theParentFrame, ModelImage _mi, ModelImage _ri) {
        matchImage = _mi;
        baseImage = _ri;
        DIM = 3;
        userInterface = ViewUserInterface.getReference();
        setSeparateThread(false);
        fromOAR3D = true;
        callAlgorithm();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, set variables, and calls the algorithm.
     *
     * @param  event  Event that triggers function.
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
            MipavUtil.showHelp("10040");
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmRegLeastSquares) {

            if (LSMatch.isCompleted() == true) {
                lsCompleted = true;
                matchImage.setMatrix(LSMatch.getTransformBtoA());
                Preferences.debug(matchImage.getMatrix().toString());
                LSMatch.getTransformBtoA().saveMatrix(userInterface.getDefaultDirectory() + matchImage.getImageName() +
                                                      "_To_" + baseImage.getImageName() + ".mtx");
                LSMatch.calculateResiduals();
                xdimA = baseImage.getExtents()[0];
                ydimA = baseImage.getExtents()[1];

                String name = makeImageName(matchImage.getImageName(), "_register");

                if (DIM == 2) {
                    int[] extents = new int[] { xdimA, ydimA };
                    float[] resolutions = new float[] { xresA, yresA };
                    resultImage = new ModelImage(matchImage.getType(), extents, name, matchImage.getUserInterface());
                    resultImage.getFileInfo(0).setResolutions(resolutions);

                    if (matchImage.isColorImage() == false) {
                        AlgorithmTransform.transformBilinear(matchImage, resultImage, LSMatch.getTransformBtoA(), null);
                    } else {
                        AlgorithmTransform.transformBilinearC(matchImage, resultImage, LSMatch.getTransformBtoA(),
                                                              xdimA, ydimA, xresA, yresA);
                    }

                } else if (DIM == 3) {

                    if (fromOAR3D) {
                        resultMatrix = LSMatch.getTransformBtoA();
                    }

                    zdimA = baseImage.getExtents()[2];

                    int[] extents = new int[] { xdimA, ydimA, zdimA };
                    float[] resolutions = new float[] { xresA, yresA, zresA };
                    resultImage = new ModelImage(matchImage.getType(), extents, name, matchImage.getUserInterface());

                    for (int i = 0; i < zdimA; i++) {
                        resultImage.getFileInfo(i).setResolutions(resolutions);
                    }

                    if (matchImage.isColorImage() == false) {
                        AlgorithmTransform.transformTrilinear(matchImage, resultImage, LSMatch.getTransformBtoA(),
                                                              null);
                    } else {
                        AlgorithmTransform.transformTrilinearC(matchImage, resultImage, LSMatch.getTransformBtoA(),
                                                               xdimA, ydimA, zdimA, xresA, yresA, zresA);
                    }
                }

                resultImage.calcMinMax();

                // These next lines set the titles in all frames where the source image is displayed to
                // image name so as to indicate that the image is now unlocked!
                // The image frames are enabled and then registed to the userinterface.
                Vector imageFrames = matchImage.getImageFrameVector();

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

                matchImage.notifyImageDisplayListeners(null, true);

                if (resultImage != null) {

                    try {
                        resultImage.setImageName("LS Transformed image");
                        updateFileInfo(baseImage, resultImage);

                        new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                    } catch (OutOfMemoryError error) {
                        MipavUtil.displayError("Out of memory: unable to open new frame");
                    }

                } else {
                    MipavUtil.displayError("Result Image is null");
                }

                insertScriptLine();
            }
        }

        if (!fromOAR3D) {
            dispose();
        }
    }

    /**
     * Accessor that returns whether or not the algorithm successfully completed.
     *
     * @return  boolean
     */
    public boolean getLSCompleted() {
        return lsCompleted;
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
     * Gets the result matrix (only used by OAR3D).
     *
     * @return  TransMatrix
     */
    public TransMatrix getResultMatrix() {
        return resultMatrix;
    }

    /**
     * Sets arrays appropriately and calls registration algorithm, running it in it's own thread.
     */
    protected void callAlgorithm() {
        int nPtsA = 0; // = baseImage.getVOIs().size();
        int nPtsB = 0; // = matchImage.getVOIs().size()
        Point3Df[] tmpptA = null;
        Point3Df[] tmpptB = null;
        Point3Df[] ptA = null; // new Point3Df[nPtsA];
        Point3Df[] ptB = null; // new Point3Df[nPtsB];
        int i, s, ptNum;
        Vector[] curves;

        try {

            if (baseImage.getVOIs().size() == 0) {
                MipavUtil.displayError("Select coordinates before clicking OK");

                return;
            } else {

                if ((baseImage.getNDims() == 3) && (matchImage.getNDims() == 3)) {
                    curves = baseImage.getVOIs().VOIAt(0).getCurves(); // curves[s] holds all VOIs in slice s

                    for (s = 0; s < baseImage.getExtents()[2]; s++) {
                        nPtsA += curves[s].size();
                    }

                    Preferences.debug("nPtsA = " + nPtsA + "\n");
                    ptA = new Point3Df[nPtsA];

                    for (s = 0; s < baseImage.getExtents()[2]; s++) {
                        tmpptA = baseImage.getVOIs().VOIAt(0).exportPoints(s);

                        for (i = 0; i < tmpptA.length; i++) {
                            ptNum = (int) (Short.valueOf(((VOIPoint) curves[s].elementAt(i)).getLabel()).shortValue()) -
                                    1;
                            ptA[ptNum] = tmpptA[i];
                        }
                    }

                    curves = matchImage.getVOIs().VOIAt(0).getCurves();

                    for (s = 0; s < matchImage.getExtents()[2]; s++) {
                        nPtsB += curves[s].size();
                    }

                    if (nPtsA != nPtsB) {
                        MipavUtil.displayError("Both images must have the same number of points");

                        return;
                    }

                    Preferences.debug("nPtsB = " + nPtsB + "\n");
                    ptB = new Point3Df[nPtsB];

                    for (s = 0; s < matchImage.getExtents()[2]; s++) {
                        tmpptB = matchImage.getVOIs().VOIAt(0).exportPoints(s);

                        for (i = 0; i < tmpptB.length; i++) {
                            ptNum = (int) (Short.valueOf(((VOIPoint) curves[s].elementAt(i)).getLabel()).shortValue()) -
                                    1;

                            // ptNum = (int)(Short.valueOf(((VOIPoint)tmpptB[i]).getLabel()).shortValue());
                            ptB[ptNum] = tmpptB[i];
                        }
                    }

                    if ((nPtsA < 4) || (nPtsB < 4)) {
                        MipavUtil.displayError("Must select at least " + (DIM + 1) + " points.");

                        return;
                    }
                } else if ((baseImage.getNDims() == 2) && (matchImage.getNDims() == 2)) {
                    curves = baseImage.getVOIs().VOIAt(0).getCurves(); // curves[s] holds all VOIs in slice s
                    nPtsA = curves[0].size();
                    Preferences.debug("nPtsA = " + nPtsA + "\n");
                    ptA = new Point3Df[nPtsA];
                    tmpptA = baseImage.getVOIs().VOIAt(0).exportPoints(0);

                    for (i = 0; i < tmpptA.length; i++) {
                        ptNum = (int) (Short.valueOf(((VOIPoint) curves[0].elementAt(i)).getLabel()).shortValue()) - 1;
                        ptA[ptNum] = tmpptA[i];
                    }

                    curves = matchImage.getVOIs().VOIAt(0).getCurves();
                    nPtsB += curves[0].size();

                    if (nPtsA != nPtsB) {
                        MipavUtil.displayError("Both images must have the same number of points");

                        return;
                    }

                    Preferences.debug("nPtsB = " + nPtsB + "\n");
                    ptB = new Point3Df[nPtsB];
                    tmpptB = matchImage.getVOIs().VOIAt(0).exportPoints(0);

                    for (i = 0; i < tmpptB.length; i++) {
                        ptNum = (int) (Short.valueOf(((VOIPoint) curves[0].elementAt(i)).getLabel()).shortValue()) - 1;
                        ptB[ptNum] = tmpptB[i];
                    }

                    if ((nPtsA < 3) || (nPtsB < 3)) {
                        MipavUtil.displayError("Must select at least " + (DIM + 1) + " points.");

                        return;
                    }
                }
            }

            Point3Dd[] ptAmm = new Point3Dd[nPtsA];
            Point3Dd[] ptBmm = new Point3Dd[nPtsB];
            zresA = 1;
            xresA = baseImage.getFileInfo(0).getResolutions()[0];
            yresA = baseImage.getFileInfo(0).getResolutions()[1];

            if (baseImage.getNDims() == 3) {
                zresA = baseImage.getFileInfo(0).getResolutions()[2];
            }

            for (i = 0; i < nPtsA; i++) {
                ptAmm[i] = new Point3Dd((double) (ptA[i].x * xresA), (double) (ptA[i].y * yresA),
                                        (double) (ptA[i].z * zresA));
                Preferences.debug(ptAmm[i].x + ", " + ptAmm[i].y + ", " + ptAmm[i].z + "\n");
            }

            xresB = matchImage.getFileInfo(0).getResolutions()[0];
            yresB = matchImage.getFileInfo(0).getResolutions()[1];

            if (matchImage.getNDims() == 3) {
                zresB = matchImage.getFileInfo(0).getResolutions()[2];
            }

            for (i = 0; i < nPtsB; i++) {
                ptBmm[i] = new Point3Dd((double) (ptB[i].x * xresB), (double) (ptB[i].y * yresB),
                                        (double) (ptB[i].z * zresB));
                Preferences.debug(ptBmm[i].x + ", " + ptBmm[i].y + ", " + ptBmm[i].z + "\n");
            }

            LSMatch = new AlgorithmRegLeastSquares(ptAmm, ptBmm, DIM);

        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog Register Least Squares: unable to allocate enough memory");

            if (resultImage != null) {
                resultImage.disposeLocal(); // Clean up memory of result image
                resultImage = null;
            }

            return;
        }

        // This is very important. Adding this object as a listener allows
        // the algorithm to notify this object when it has completed of failed.
        // See algorithm performed event. This is made possible by implementing
        LSMatch.addListener(this);

        createProgressBar(baseImage.getImageName(), LSMatch);

        // These next lines set the titles in all frames where the source image
        // is displayed to "locked - " image name so as to indicate that the image
        // is now read/write locked!  The image frames are disabled and then
        // unregisted from the userinterface until the algorithm has completed.
        Vector imageFrames = matchImage.getImageFrameVector();
        titles = new String[imageFrames.size()];

        for (i = 0; i < imageFrames.size(); i++) {
            titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
            ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
            ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
            userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
        }

        // Start the thread as a low priority because we wish to still have
        // user interface work fast
        // if (erodeAlgo3D.startMethod(Thread.MIN_PRIORITY) == false){
        // MipavUtil.displayError("A thread is already running on this object", "Error");

        if (isRunInSeparateThread()) {

            if (LSMatch.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            LSMatch.run();
        }


    }

    /**
     * Store the result image in the script runner's image table now that the action execution is finished.
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(getResultImage());
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        matchImage = scriptParameters.retrieveInputImage();
        baseImage = scriptParameters.retrieveImage("reference_image");

        if (matchImage.getNDims() == 2) {
            DIM = 2;
        } else if (matchImage.getNDims() == 3) {
            DIM = 3;
        }

        userInterface = matchImage.getUserInterface();
        parentFrame = matchImage.getParentFrame();
    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(matchImage);
        scriptParameters.storeImage(baseImage, "reference_image");

        AlgorithmParameters.storeImageInRecorder(getResultImage());
    }

    /**
     * Constructs a string of the construction parameters and outputs the string to the messsage frame if the logging
     * procedure is turned on.
     */
    private void constructLog() {
        String logString = new String("Register " + matchImage.getImageName() + " to " + baseImage.getImageName() +
                                      "\n");
        // Preferences.log(matchImage.getUserInterface(), logString);
    }


    /**
     * Initializes GuserInterface components and displays dialog.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Least Squares Registration");

        String matchName = matchImage.getImageName();

        JLabel labelImage = new JLabel("Register [" + matchName + "] to:");
        labelImage.setForeground(Color.black);
        labelImage.setFont(serif12);
        comboBoxImage = buildImageComboBox(matchImage);

        JPanel imagePanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
        imagePanel.add(labelImage);
        imagePanel.add(comboBoxImage);

        getContentPane().add(imagePanel);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    /**
     * Sets the variables needed for calling the algorithm.
     *
     * @return  <code>true</code> if successful in setting variables.
     */
    private boolean setVariables() {

        // assign baseImage to image selected in comboBox
        String selectedName = (String) comboBoxImage.getSelectedItem();

        baseImage = ViewUserInterface.getReference().getRegisteredImageByName(selectedName);

        if (matchImage.getNDims() == 2) {
            DIM = 2;
        } else if (matchImage.getNDims() == 3) {
            DIM = 3;
            constructLog();
        }

        return true;
    }

}
