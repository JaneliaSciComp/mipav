package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.model.algorithms.*;

import java.awt.event.*;
import java.awt.*;
import java.util.*;
import javax.swing.*;

/**
 *   Dialog to get user input
 *   Selected image is match image, the image that gets transformed until it is
 *   registered to the base image.  Thin plate spline algorithm is used for matching.
 *
 *   The mouse is used to put a set of point VOIs on the base image.  Then, the
 *   mouse is used to put the same number of point VOIs at the corresponding
 *   positions on the match image.  For 2D images 3 or more points are required
 *   and the algorithm may fail if the points all nearly fall on the same line.
 *   For 3D images 4 or more points are required and the algorithm may fail if
 *   the points nearly all fall on the same plane.  The base image is selected
 *   from a combo box containing the names of images other than the selected
 *   match image.
 *
 *   The dimensions or image type of the match image need not be the same as
 *   the dimensions or image type of the base image.  The registered resultImage
 *   will have the same image type as the match image and the same extents as
 *   the base image.
 *
 *   The spline matching points sets are used to obtain spline interpolation
 *   coefficients.  These spline interpolation coefficents are used to
 *   transform all the xorg,yorg grid positions in base image space to xnew,
 *   ynew grid positions in match space.  Then, at every xorg,yorg grid
 *   position in the base space, the program checks to see if the corresponding
 *   xnew,ynew grid position in the match space is within the image bounds of
 *   the match image.  If xnew, ynew is within the match space bounds, then
 *   the data value at the xnew,ynew grid position in the match space is assigned
 *   to be the registered value at the xorg,yorg position in the base space.
 *   Since xnew, ynew is a floating point number and the data in the match image
 *   is only contained at integer grid points, interpolation must be used.  For
 *   a 2D image the data value at xnew,ynew in the match space is obtained by
 *   bilinear interpolation from its 4 nearest neighbors.  For a 3D image the
 *   data value at xnew,ynew in the match space is obtained by trilinear
 *   interpolation from its 8 nearest neighbors.  If the xnew, ynew is outside
 *   the match space bounds, then a zero is assigned to the xorg, yorg position
 *   in the base space.
 *
 *   This software does not yet provide a general coplanar solution for 3D images.
 *   However, special handling does exist for the case where the z values of
 *   the corresponding point landmarks are identical.
 */
public class JDialogRegistrationTPSpline
    extends JDialogBase implements AlgorithmInterface, ScriptableInterface {

    private int DIM;
    private ModelImage matchImage; //register match image to baseImage
    private ModelImage baseImage;
    private ModelImage resultImage = null; // result image
    private String titles[];
    private JComboBox comboBoxImage;
    private ViewUserInterface UI;
    private double xSource[];
    private double ySource[];
    private double zSource[];
    private double xTar[];
    private double yTar[];
    private double zTar[];
    private AlgorithmTPSpline spline = null;
    private boolean coplanar = false;

    /**
     *  Creates new dialog.
     *  @param theParentFrame    Parent frame
     *  @param im                Source image
     */
    public JDialogRegistrationTPSpline(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        UI = ( (ViewJFrameBase) (parentFrame)).getUserInterface();
        matchImage = im;
        init();
    }

    /**
     * Empty constructor for scripts
     */
    public JDialogRegistrationTPSpline() { }

    /**
     *	Initializes GUI components and displays dialog.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Thin Plate Spline Registration");
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
     * Run this algorithm from a script.
     * @param parser the script parser we get the state from
     * @throws IllegalArgumentException if there is something wrong with the arguments in the script
     */
    public void scriptRun(AlgorithmScriptParser parser) throws IllegalArgumentException {
        String srcImageKey = null;
        String image2Key = null;
        String destImageKey = null;

        try {
            srcImageKey = parser.getNextString();
            image2Key = parser.getNextString();
            destImageKey = parser.getNextString();
        }
        catch (Exception e) {
            throw new IllegalArgumentException();
        }
        matchImage = parser.getImage(srcImageKey);

        baseImage = parser.getImage(image2Key);

        UI = matchImage.getUserInterface();
        parentFrame = matchImage.getParentFrame();
        setActiveImage(parser.isActiveImage());
        setSeparateThread(false);
        callAlgorithm();

        parser.putVariable(destImageKey, getResultImage().getImageName());
    }

    /**
     * If a script is being recorded and the algorithm is done, add an entry for this algorithm.
     * @param algo the algorithm to make an entry for
     */
    public void insertScriptLine(AlgorithmBase algo) {
        if (algo.isCompleted()) {
            if (UI.isScriptRecording()) {
                //check to see if the match image is already in the ImgTable
                if (UI.getScriptDialog().getImgTableVar(matchImage.getImageName()) == null) {
                    if (UI.getScriptDialog().getActiveImgTableVar(matchImage.getImageName()) == null) {
                        UI.getScriptDialog().putActiveVar(matchImage.getImageName());
                    }
                }

                //do the same for the reference image
                if (UI.getScriptDialog().getImgTableVar(baseImage.getImageName()) == null) {
                    if (UI.getScriptDialog().getActiveImgTableVar(baseImage.getImageName()) == null) {
                        UI.getScriptDialog().putActiveVar(baseImage.getImageName());
                    }
                }

                UI.getScriptDialog().putVar(resultImage.getImageName());

                UI.getScriptDialog().append("RegistrationTPSpline ");
                UI.getScriptDialog().append(UI.getScriptDialog().getVar(matchImage.getImageName()) + " ");
                UI.getScriptDialog().append(UI.getScriptDialog().getVar(baseImage.getImageName()) + " ");
                UI.getScriptDialog().append(UI.getScriptDialog().getVar(resultImage.getImageName()) + "\n");

            }
        }
    }

    /**
     *    Closes dialog box when the OK button is pressed and calls the algorithm
     *    @param event       Event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

        if (source == OKButton) {
            if (setVariables()) {
                callAlgorithm();
            }

        }
        else if (source == cancelButton) {
            dispose();
        }
        else if (source == helpButton) {
            MipavUtil.showHelp("10033");
        }
    }

    private boolean setVariables() {
        UI = matchImage.getUserInterface();
        String selectedName = (String) comboBoxImage.getSelectedItem();
        baseImage = UI.getRegisteredImageByName(selectedName);

        if (baseImage.getVOIs().size() == 0) {
            MipavUtil.displayError("Error! No VOIs were present in the base image");
            return false;
        }
        else if (matchImage.getVOIs().size() == 0) {
            MipavUtil.displayError("Error! No VOIs were present in the match image");
            return false;
        }

        return true;
    }

    /**
     * Runs the algorithm
     */
    private void callAlgorithm() {

        if (matchImage.getNDims() == 2) {
            DIM = 2;
        }
        else if (matchImage.getNDims() == 3) {
            DIM = 3;
            constructLog();
        }

        // Hide dialog
        setVisible(false);
        int nPtsA = 0; //= baseImage.getVOIs().size();
        int nPtsB = 0; //= matchImage.getVOIs().size()
        Point3Df[] tmpptA = null;
        Point3Df[] tmpptB = null;
        Point3Df[] ptA = null; //new Point3Df[nPtsA];
        Point3Df[] ptB = null; //new Point3Df[nPtsB];
        int i, s;
        int ptNum = 0;
        Vector curvesB[];
        Vector curvesM[];

        if ( (baseImage.getNDims() == 3) && (matchImage.getNDims() == 3)) {
            curvesB = baseImage.getVOIs().VOIAt(0).getCurves(); //curves[s] holds all VOIs in slice s
            for (s = 0; s < baseImage.getExtents()[2]; s++) {
                nPtsA += curvesB[s].size();
            }
            Preferences.debug("nPtsA = " + nPtsA);

            curvesM = matchImage.getVOIs().VOIAt(0).getCurves(); //curves[s] holds all VOIs in slice s
            for (s = 0; s < matchImage.getExtents()[2]; s++) {
                nPtsB += curvesM[s].size();
            }
            Preferences.debug("nPtsB = " + nPtsB);

            try {
                ptA = new Point3Df[nPtsA];
                ptB = new Point3Df[nPtsB];
            }
            catch (OutOfMemoryError error) {
                ptA = null;
                ptB = null;
                System.gc();
                MipavUtil.displayError("JDialogRegistrationTPSpline: Out of memory on ptA");
                return;
            }

            for (s = 0; s < baseImage.getExtents()[2]; s++) {
                tmpptA = baseImage.getVOIs().VOIAt(0).exportPoints(s);
                for (i = 0; i < tmpptA.length; i++) {
                    ptNum = (int) (Short.valueOf( ( (VOIPoint) curvesB[s].elementAt(i)).getLabel()).shortValue()) - 1;
                    ptA[ptNum] = tmpptA[i];
                }
            }

            for (s = 0; s < matchImage.getExtents()[2]; s++) {
                tmpptB = matchImage.getVOIs().VOIAt(0).exportPoints(s);
                for (i = 0; i < tmpptB.length; i++) {
                    ptNum = (int) (Short.valueOf( ( (VOIPoint) curvesM[s].elementAt(i)).getLabel()).shortValue()) - 1;

                    //ptNum = (int)(Short.valueOf(((VOIPoint)tmpptB[i]).getLabel()).shortValue());
                    ptB[ptNum] = tmpptB[i];
                }
            }

            coplanar = true;
            for (i = 0; (i < ptA.length) && coplanar; i++) {
                if (ptA[i].z != ptB[i].z) {
                    coplanar = false;
                }
            }

        }
        else if ( (baseImage.getNDims() == 2) && (matchImage.getNDims() == 2)) {
            curvesB = baseImage.getVOIs().VOIAt(0).getCurves(); //curves[s] holds all VOIs in slice s
            nPtsA = curvesB[0].size();
            Preferences.debug("nPtsA = " + nPtsA);

            curvesM = matchImage.getVOIs().VOIAt(0).getCurves(); //curves[s] holds all VOIs in slice s
            nPtsB = curvesM[0].size();
            Preferences.debug("nPtsB = " + nPtsB);
            try {
                ptA = new Point3Df[nPtsA];
                ptB = new Point3Df[nPtsB];
            }
            catch (OutOfMemoryError error) {
                ptA = null;
                ptB = null;
                System.gc();
                MipavUtil.displayError("JDialogRegistrationTPSpline: Out of memory.");
                return;
            }

            tmpptA = baseImage.getVOIs().VOIAt(0).exportPoints(0);
            for (i = 0; i < tmpptA.length; i++) {
                ptNum = (int) (Short.valueOf( ( (VOIPoint) curvesB[0].elementAt(i)).getLabel()).shortValue()) - 1;
                ptA[ptNum] = tmpptA[i];
            }

            tmpptB = matchImage.getVOIs().VOIAt(0).exportPoints(0);
            for (i = 0; i < tmpptB.length; i++) {
                ptNum = (int) (Short.valueOf( ( (VOIPoint) curvesM[0].elementAt(i)).getLabel()).shortValue()) - 1;

                ptB[ptNum] = tmpptB[i];
            }
        }

        if ( (DIM == 2) || (coplanar)) {
            // Calculate the reverse direction to find the values of the grid positions in x',y' space in
            // terms of x, y values in the original space
            try {
                xSource = new double[nPtsA];
                ySource = new double[nPtsA];
                xTar = new double[nPtsB];
                yTar = new double[nPtsB];
            }
            catch (OutOfMemoryError error) {
                xSource = null;
                ySource = null;
                xTar = null;
                yTar = null;
                System.gc();
                MipavUtil.displayError("JDialogRegistrationTPSpline: Out of memory on xSource");
                return;
            }

            for (i = 0; i < nPtsA; i++) {
                xSource[i] = ptA[i].x;
                ySource[i] = ptA[i].y;
            }

            for (i = 0; i < nPtsB; i++) {
                xTar[i] = ptB[i].x;
                yTar[i] = ptB[i].y;
            }

            // 0.0f for no smoothing, with smoothing interpolation is not exact
            try {
                spline = new AlgorithmTPSpline(xSource, ySource, xTar, yTar, 0.0f,
                                               baseImage, matchImage);
            }
            catch (OutOfMemoryError error) {
                spline = null;
                System.gc();
                MipavUtil.displayError("JDialogRegistrationTPSpline: Out of memory on spline");
                return;
            }

        } // if (DIM == 2)
        else { // DIM == 3
            // Calculate the reverse direction to find the values of the grid positions in x',y',z' space in
            // terms of x, y, z values in the original space
            try {
                xSource = new double[nPtsA];
                ySource = new double[nPtsA];
                zSource = new double[nPtsA];

                xTar = new double[nPtsB];
                yTar = new double[nPtsB];
                zTar = new double[nPtsB];
            }
            catch (OutOfMemoryError error) {
                xSource = null;
                ySource = null;
                zSource = null;

                xTar = null;
                yTar = null;
                zTar = null;

                System.gc();
                MipavUtil.displayError("JDialogRegistrationTPSpline: Out of memory.");
                return;
            }

            for (i = 0; i < nPtsA; i++) {
                xSource[i] = ptA[i].x;
                ySource[i] = ptA[i].y;
                zSource[i] = ptA[i].z;
            }

            for (i = 0; i < nPtsB; i++) {
                xTar[i] = ptB[i].x;
                yTar[i] = ptB[i].y;
                zTar[i] = ptB[i].z;
            }

            // 0.0f for no smoothing, with smoothing interpolation is not exact
            try {
                spline = new AlgorithmTPSpline(xSource, ySource, zSource, xTar, yTar, zTar,
                                               0.0f, baseImage, matchImage);
            }
            catch (OutOfMemoryError error) {
                spline = null;
                System.gc();
                MipavUtil.displayError("JDialogRegistrationTPSpline: Out of memory on spline");
                return;
            }

        } // else DIM == 3

        // This is very important. Adding this object as a listener allows
        // the algorithm to notify this object when it has completed of failed.
        // See algorithm performed event. This is made possible by implementing
        spline.addListener(this);

        // These next lines set the titles in all frames where the source image
        //is displayed to "locked - " image name so as to indicate that the image
        //is now read/write locked!  The image frames are disabled and then
        //unregisted from the userinterface until the algorithm has completed.
        Vector imageFrames = matchImage.getImageFrameVector();
        titles = new String[imageFrames.size()];
        for (i = 0; i < imageFrames.size(); i++) {
            titles[i] = ( (Frame) (imageFrames.elementAt(i))).getTitle();
            ( (Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
            ( (Frame) (imageFrames.elementAt(i))).setEnabled(false);
            UI.unregisterFrame( (Frame) (imageFrames.elementAt(i)));
        }

        if (runInSeparateThread) {
            if (spline.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        }
        else {
            spline.setActiveImage(isActiveImage);
            if (!UI.isAppFrameVisible()) {
                spline.setProgressBarVisible(false);
            }
            spline.run();
        }

    }

    /**
     * Gets the result image
     * @return ModelImage result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    //************************************************************************
     //************************** Algorithm Events ****************************
      //************************************************************************

       /**
        *	This method is required if the AlgorithmPerformed interface is implemented. It is called by the
        *   algorithms when it has completed or failed to to complete, so that the dialog can be display
        *   the result image and/or clean up.
        *   @param algorithm   Algorithm that caused the event.
        */
       public void algorithmPerformed(AlgorithmBase algorithm) {
           int i;
           ViewJFrameImage imageFrame = null;
           if (algorithm instanceof AlgorithmTPSpline) {
               if (spline.isCompleted() == true) {

                   // These next lines set the titles in all frames where the source image is displayed to
                   // image name so as to indicate that the image is now unlocked!
                   // The image frames are enabled and then registed to the userinterface.
                   Vector imageFrames = matchImage.getImageFrameVector();
                   for (i = 0; i < imageFrames.size(); i++) {
                       ( (Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                       ( (Frame) (imageFrames.elementAt(i))).setEnabled(true);
                       if ( ( (Frame) (imageFrames.elementAt(i))) != parentFrame) {
                           UI.registerFrame( (Frame) (imageFrames.elementAt(i)));
                       }
                       UI.registerFrame(parentFrame);
                       matchImage.notifyImageDisplayListeners(null, true);
                   }

                   resultImage = spline.getResultImage();

                   if (resultImage != null) {
                       try {
                           //resultImage.setImageName("Transformed image");
                           imageFrame = new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                       }
                       catch (OutOfMemoryError error) {
                           MipavUtil.displayError("Out of memory: unable to open new frame");
                       }
                   }
                   else
                       MipavUtil.displayError("result Image is null");
                   closingLog();

                   Preferences.debug("Done.");
                   insertScriptLine(algorithm);
               }

           }
           dispose();
       }

    /**
     * constructLog - constructs a string of the construction parameters and
     *                outputs the string to the messsage frame if the logging
     *                procedure is turned on.
     */
    private void constructLog() {

        String logString = new String("Register " + matchImage.getImageName() +
                                      " to " + baseImage.getImageName() + "\n");
//        Preferences.log(matchImage.getUserInterface(), logString);
    }

    /**
     *   closingLog  - constructs a string indicating if the whether or not
     *                 the algorithm completed sucessfully.
     *   overrides AlgorithmBase's nonfunctional closingLog function
     */
    protected void closingLog() {
        String logString;
        if (spline.isCompleted() == true) {
            logString = new String("Register " + matchImage.getImageName() +
                                   " to " + baseImage.getImageName() +
                                   " Completed successfully!" + "\n");
        }
        else {
            logString = new String("Register " + matchImage.getImageName() +
                                   " to " + baseImage.getImageName() +
                                   " Algorithm failed!" + "\n");
        }
//        Preferences.log(matchImage.getUserInterface(), logString);
    }
}
