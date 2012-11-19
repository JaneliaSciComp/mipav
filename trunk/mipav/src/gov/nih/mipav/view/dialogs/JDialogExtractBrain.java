package gov.nih.mipav.view.dialogs;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.*;
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
 * DOCUMENT ME!
 *
 * @version  1.0 July 17, 2000
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class JDialogExtractBrain extends JDialogScriptableBase implements AlgorithmInterface, LegacyDialogDefaultsInterface, ActionDiscovery, ScriptableActionInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 7723410343739118755L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** The factor above which second stage edge erosion occurs. */
    private float aboveMedian;

    /** The volume's center of mass computed from <code>computeCenter(ModelImage, int, boolean)</code> */
    private Vector3f centerOfMass;

    /** The maximum depth within the brain's surface to sample image intensities, by default set to 5. */
    private int depth = 5;

    /** Field for entering the maximum depth.  The dialog box suggests a value between 3 - 19 and requires a value from 3 - 33. */
    private JTextField depthTF;

    /** Label for the percentage entered for second stage edge erosion. */
    private JLabel erosionLabel;

    /** Defines the allowable percentage for edge intensity values ub second stage edge erosion, default is 50 percent. */
    private JTextField erosionTF;

    /** Algorithm used for brain extraction. */
    private AlgorithmBrainExtractor extractBrainAlgo;

    /** When true, mask of extracted brain is painted over the source image. */
    private boolean extractToPaint;

    /** When checked, mask of extracted brain is painted over the source image. */
    private JCheckBox extractToPaintCheckBox;

    /** Source image for extracting brain, default image is <code>null</code>. */
    private ModelImage image = null; 

    /** Image influence ratio for controlling the sampling depth used to calculate intensity extrema, by default set to .1. */
    private float imageRatio = 0.1f;

    /** Field for entering the image influence ratio.  The dialog box requires that a value from .01 - .51 be entered. */
    private JTextField imageRatioTF;

    /** Either the volume's center of mass or a user indicated point depending on the value of <code>useCenterOfMass</code>. */
    private Vector3f initCenterPoint;

    /** Used to denote the brain's center of mass y-coordinate.  By default set to zero. */
    private float initCenterX = 0;

    /** Label for the x-coordinate of the user-defined center. */
    private JLabel initCenterXLabel;

    /** An optional field for denoting a desired initial x-corrdinate for the mesh.
     *  Editable if and only if <code>useCenterOfMass</code> is set to <code>false</code>
     **/
    private JTextField initCenterXTF;

    /** Used to denote the brain's center of mass y-coordinate.  By default set to zero. */
    private float initCenterY = 0;

    /** Label for the y-coordinate of the user-defined center. */
    private JLabel initCenterYLabel;

    /** An optional field for denoting a desired initial y-corrdinate for the mesh.
     *  Editable if and only if <code>useCenterOfMass</code> is set to <code>false</code>
     **/
    private JTextField initCenterYTF;

    /** Used to denote the brain's center of mass y-coordinate.  By default set to zero. */
    private float initCenterZ = 0;

    /** Label for the z-coordinate of the user-defined center. */
    private JLabel initCenterZLabel;

    /** An optional field for denoting a desired initial z-corrdinate for the mesh.
     *  Editable if and only if <code>useCenterOfMass</code> is set to <code>false</code>
     **/
    private JTextField initCenterZTF;

    /** When true, image mask is set to the initial ellipsoid approximation of the brain calculated from the volume's center of mass. */
    private boolean justEllipse;

    /** When checked, <code>justEllipse</code> is set to true. */
    private JCheckBox justInitEllipsoidCheckbox;

    /** Number of surface evolutions to perform, by default set to 500. */
    private int nIterations = 500;

    /** Field for inputting the number of surface evolutions to perform.  This requests, but does not require, a value from 100 - 2000 */
    private JTextField nIterationsTF;

    /** Orientation value corresponding to not the value of <code>AlgorithmBrainExtractor.SAT_COR</code> */
    private int orientation;

    /** When checked, denotes that the images orientation does not correspond to <code>AlgorithmBrainExtractor.SAT_COR</code> */
    private JCheckBox orientCheckbox;

    /** Initially unchecked, determined whether this algorithm performs <code>secondStageErosion</code> */
    private JCheckBox secondStageCheckBox;

    /** When true, sets edge values which are greater than the median intensity by a user-defined factor of <code>aboveMedian</code> to zero. */
    private boolean secondStageErosion;

    /** Allows for variable motion of the mesh in the surface normal direction, default value of .15 */
    private float stiffness = 0.15f;

    /** Field for specifing stiffness of the mesh.  The dialog box suggests, but does not require that a value between .01 and .5 entered. */
    private JTextField stiffnessTF;

    /** DOCUMENT ME! */
    private String[] titles;

    /** By default true, parameter indicates that the center position of the brain should be calculated 
     *  using <code>JDialogExtractBrain.computeCenter(ModelImage, int, boolean)</code> 
     */
    private boolean useCenterOfMass = true;

    /** By default checked, yields a <code>true</code> value for <code>useCenterOfMass</code> */
    private JCheckBox useCenterOfMassCheckBox;

    /** The user interface for this dialog box. */
    private ViewUserInterface userInterface;

    /** When true estimates that boundary of the VOI by a sphere. */
    private boolean useSphere = true;

    /** Initially not checked, estimates the boundary by a sphere rather than an ellipse. */
    private JCheckBox useSphereCheckbox;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogExtractBrain() { }  

    /**
     * Sets the appropriate variables. Does not actually create a dialog that is visible because no user input is
     * necessary at present.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogExtractBrain(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);

        setForeground(Color.black);
        image = im;
        userInterface = ViewUserInterface.getReference();
        init();

        centerOfMass = computeCenter(image, orientation, useSphere);
        setVariables();

        // use the center of mass if there was a problem with the defaults or the user wants to use it explicitly
        if (useCenterOfMass || (initCenterX == -1) || (initCenterY == -1) || (initCenterZ == -1)) {
            initCenterPoint = centerOfMass;
            initCenterX = initCenterPoint.X;
            initCenterY = initCenterPoint.Y;
            initCenterZ = initCenterPoint.Z;
            initCenterXTF.setText("" + initCenterX);
            initCenterYTF.setText("" + initCenterY);
            initCenterZTF.setText("" + initCenterZ);
        }
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Calculate the center of the sphere / ellipsoid.
     *
     * @param   img          the image to get the center of
     * @param   orientation  the orientation of the image
     * @param   sphereFlag   whether to find the center for a sphere or an ellipsoid
     *
     * @return  the starting center point
     */
    public static final Vector3f computeCenter(ModelImage img, int orientation, boolean sphereFlag) {
        Vector3f centerPt = new Vector3f();

        int xDim = img.getExtents()[0];
        int yDim = img.getExtents()[1];
        int zDim = img.getExtents()[2];
        int sliceSize = xDim * yDim;
        int imgSize = sliceSize * zDim;

        int[] imgBuffer = new int[imgSize];
        float fMin = (float) img.getMin();
        float fMax = (float) img.getMax();

        // Re-map image data to 0 - 1023
        float fMult = 1023.0f / (fMax - fMin);

        for (int i = 0; i < imgSize; i++) {
            imgBuffer[i] = (int) (fMult * (img.getFloat(i) - fMin));
        }

        int backgroundThreshold = histogramAnalysis(imgBuffer);

        Preferences.debug("Orient = " + orientation);

        if (sphereFlag) {
            Preferences.debug("Brain extractor: estimateSphere: backgroundThreshold = " + backgroundThreshold + "\n");

            int count = 0;

            for (int iZ = 0, iIndex = 0; iZ < zDim; iZ++) {

                for (int iY = 0; iY < yDim; iY++) {

                    for (int iX = 0; iX < xDim; iX++) {

                        if (imgBuffer[iIndex++] >= backgroundThreshold) {
                            count++;
                            centerPt.X += iIndex % xDim;
                            centerPt.Y += (iIndex % sliceSize) / xDim;
                            centerPt.Z += iIndex / sliceSize;
                        }
                    }
                }
            }

            centerPt.X = (centerPt.X / count++);
            centerPt.Y = (centerPt.Y / count++);
            centerPt.Z = (centerPt.Z / count++);
        } else {

            // Make the estimation numerically robust by tracking voxel positions
            // that are uniformly scaled into [-1,1]^3.
            Preferences.debug("Brain extractor: extimateEllisoid: brightnessThreshold = " + backgroundThreshold + "\n");

            float fBMax = (float) xDim;

            if ((float) yDim > fBMax) {
                fBMax = (float) yDim;
            }

            if ((float) zDim > fBMax) {
                fBMax = (float) zDim;
            }

            float fInvBMax = 1.0f / fBMax;

            // The arrays "less" and "greater" store positions of bright voxels
            // that occur less or greater than YBound/2, respectively.  The
            // array with the smaller number of voxels represents the scalp
            // voxels in the upper-half of the head.  The comparison of counts
            // is based on empirical studies.
            Vector<Vector3f> kLess = new Vector<Vector3f>();
            Vector<Vector3f> kGreater = new Vector<Vector3f>();
            int iHalfYBound = yDim / 2;
            int iHalfZBound = zDim / 2;

            for (int iZ = 0, iIndex = 0; iZ < zDim; iZ++) {

                for (int iY = 0; iY < yDim; iY++) {

                    for (int iX = 0; iX < xDim; iX++) {

                        if (imgBuffer[iIndex++] >= backgroundThreshold) {
                            Vector3f kVoxel = new Vector3f(fInvBMax * iX, fInvBMax * iY, fInvBMax * iZ);

                            if (orientation == AlgorithmBrainExtractor.SAT_COR) {

                                if (iY < iHalfYBound) {
                                    kLess.add(kVoxel);
                                } else {
                                    kGreater.add(kVoxel);
                                }
                            } else { // Axial image

                                if (iZ < iHalfZBound) {
                                    kLess.add(kVoxel);
                                } else {

                                    // kGreater.add(kVoxel);
                                    kLess.add(kVoxel);
                                }
                            }
                        }
                    }
                }
            }

            // Fit points with an ellipsoid.  The algorithm uses a least-squares
            // estimation of the coefficients for a quadratic equation that
            // represents the ellipsoid.
            AlgorithmQuadraticFit kQFit;
            kQFit = new AlgorithmQuadraticFit(kLess);

            // rescale from [-1,1]^3 to voxel coordinates
            centerPt.copy(kQFit.getCenter()).scale(fBMax);

            if (centerPt.X >= img.getExtents()[0]) {
                centerPt.X = img.getExtents()[0] / 2;
            }

            if (centerPt.Y >= img.getExtents()[1]) {
                centerPt.Y = img.getExtents()[1] / 2;
            }

            if (centerPt.Z >= img.getExtents()[2]) {
                centerPt.Z = img.getExtents()[2] / 2;
            }

            centerPt.Y *= 0.90f; // move it up alittle on the y axis

            // m_kCenter.Y *= 1.2f; // move it up alittle on the y axis
        }

        Preferences.debug("center of mass = " + centerPt + "\n");

        return centerPt;
    }

    /**
     * Analyze the histogram of the 10-bit binned 3D MRI. The function computes a minimum threshold, a maximum
     * threshold, and a background threshold that are used in the image term of the surface evolution. A brightness
     * threshold is also computed that is used for determining the initial ellipsoid that approximates the brain
     * surface.
     *
     * @param   imgBuffer  image data buffer normalized to 0 - 1023
     *
     * @return  the current background threshold, which may have been changed during the histogram analysis
     */
    public static final int histogramAnalysis(int[] imgBuffer) {

        // compute histogram
        int[] aiHistogram = new int[1024];
        Arrays.fill(aiHistogram, 0);

        int i;
        int j;

        for (i = 0; i < imgBuffer.length; i++) {
            aiHistogram[imgBuffer[i]]++;
        }

        // Eliminate a large chunk of background.  The four parameters below
        // were selected based on empirical studies.
        double dMinFactor = 0.03;
        double dMaxFactor = 0.98;
        double dBrightFactor = 0.95;

        int iMax = 64;
        int iMinCutoff = (int) (dMinFactor * imgBuffer.length);
        int iMaxCutoff = (int) (dMaxFactor * imgBuffer.length);

        // Find background - i.e. the value with the most counts
        float maxCount = -1;
        int maxCountIndex = 0;

        for (j = 0; j < iMax; j++) {

            if (aiHistogram[j] > maxCount) {
                maxCount = aiHistogram[j];
                maxCountIndex = j;
            }
        }

        int backgroundThreshold;

        // maxCountIndex = iMax;
        int iAccum = 0;
        int maxThreshold = 0;
        int minThreshold = 0;

        if (maxCountIndex == iMax) {

            // unable to find background from above - use cumulative histogram method
            for (i = 0; i < iMax; i++) {
                iAccum += aiHistogram[i];

                if (iAccum <= iMaxCutoff) {
                    maxThreshold = i;
                }

                if (iAccum <= iMinCutoff) {
                    minThreshold = i;
                }
            }

            backgroundThreshold = Math.round((0.9f * minThreshold) + (0.1f * maxThreshold));
        } else {
            backgroundThreshold = maxCountIndex + 1;
        }

        int iReducedQuantity = imgBuffer.length;

        for (j = 0; j <= maxCountIndex; j++) {
            iReducedQuantity -= aiHistogram[j];
        }

        Preferences.debug("Brain extractor: histogramAnalysis: m_iQuantity = " + imgBuffer.length + "\n");
        Preferences.debug("Brain extractor: histogramAnalysis: iReducedQuantity = " + iReducedQuantity + "\n");

        // compute brightness thresholds
        iAccum = 0;

        int iBrightCutoff = (int) (dBrightFactor * iReducedQuantity);

        // m_iMaxThreshold     = m_iBackThreshold;
        for (i = backgroundThreshold; i < 1024; i++) {
            iAccum += aiHistogram[i];

            if (iBrightCutoff >= iAccum) {
                backgroundThreshold = i;
            } // Used to estimate ellipsoid !
        }

        if (backgroundThreshold == 0) {
            backgroundThreshold = 1;
            minThreshold = 0;
        } else if (backgroundThreshold == 1) {
            minThreshold = 0;
        } else {
            minThreshold = (int) Math.floor(0.5f * backgroundThreshold);
        }

        if (minThreshold == backgroundThreshold) {
            backgroundThreshold++;
        }

        // m_iBrightThreshold *= 1.25f;
        Preferences.debug("Brain extractor: histogramAnalysis: MinThreshold = " + minThreshold + "\n");
        Preferences.debug("Brain extractor: histogramAnalysis: BackThreshold = " + backgroundThreshold + "\n");
        Preferences.debug("Brain extractor: histogramAnalysis: Brightness Threshold = " + backgroundThreshold + "\n");

        return backgroundThreshold;
    }

    /**
     * Presently only the script function calls this method. When the script sends this dialog the action command, this
     * method calls run.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("10045");
            MipavUtil.showWebHelp("Extract_Brain:_Extract_Brain_Surface_(BET)");
        } else if (source == secondStageCheckBox) {

            if (secondStageCheckBox.isSelected()) {
                erosionLabel.setEnabled(true);
                erosionTF.setEnabled(true);
            } else {
                erosionLabel.setEnabled(false);
                erosionTF.setEnabled(false);
            }
        } else {
            super.actionPerformed(event);
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmBrainExtractor) {

            // These next lines set the titles in all frames where the source image is displayed to
            // image name so as to indicate that the image is now unlocked!
            // The image frames are enabled and then registed to the userinterface.
            Vector<ViewImageUpdateInterface> imageFrames = image.getImageFrameVector();

            for (int i = 0; i < imageFrames.size(); i++) {
                ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                if ((((Frame) (imageFrames.elementAt(i))) != parentFrame) && (parentFrame != null)) {
                    userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                }
            }

            userInterface.setGlobalDataText(image.getImageName() + " volume = \t" +
                                            (extractBrainAlgo.getBrainVolume() / 1000.0f) + " cc\n");

            insertScriptLine();

            if (parentFrame != null) {
                userInterface.registerFrame(parentFrame);
            }

            image.calcMinMax();

            extractBrainAlgo.finalize();
            extractBrainAlgo = null;
        }

        // necessary for paint to appear when using 'extract to paint' option
        if (parentFrame != null) {
            ((ViewJFrameImage) parentFrame).getComponentImage().setPaintMask(image.getMask());
        }

        image.notifyImageDisplayListeners(null, true);
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
        str += orientation + delim;
        str += justEllipse + delim;
        str += nIterations + delim;
        str += depth + delim;
        str += imageRatio + delim;
        str += stiffness + delim;
        str += useSphere + delim;
        str += secondStageErosion + delim;
        str += aboveMedian + delim;
        str += useCenterOfMass + delim;
        str += extractToPaint + delim;
        str += initCenterX + delim;
        str += initCenterY + delim;
        str += initCenterZ;

        return str;
    }

    /**
     * Initial center position checkbox listener.
     *
     * @param  event  checkbox event
     */
    public void itemStateChanged(ItemEvent event) {

        if (event.getSource() == useCenterOfMassCheckBox) {
            useCenterOfMass = useCenterOfMassCheckBox.isSelected();
            initCenterXLabel.setEnabled(!useCenterOfMass);
            initCenterXTF.setEnabled(!useCenterOfMass);
            initCenterYLabel.setEnabled(!useCenterOfMass);
            initCenterYTF.setEnabled(!useCenterOfMass);
            initCenterZLabel.setEnabled(!useCenterOfMass);
            initCenterZTF.setEnabled(!useCenterOfMass);

            if (useCenterOfMass) {
                initCenterXTF.setText("" + centerOfMass.X);
                initCenterYTF.setText("" + centerOfMass.Y);
                initCenterZTF.setText("" + centerOfMass.Z);
            }
        } else if (event.getSource() == useSphereCheckbox) {
            boolean flag = useSphereCheckbox.isSelected();

            // orientation affects ellisoid center calculation
            int orient = AlgorithmBrainExtractor.SAT_COR;

            if (orientCheckbox.isSelected()) {
                orient = FileInfoBase.AXIAL;
            } else {
                orient = AlgorithmBrainExtractor.SAT_COR;
            }

            Vector3f oldPoint = new Vector3f(Float.parseFloat(initCenterXTF.getText()),
                                           Float.parseFloat(initCenterYTF.getText()),
                                           Float.parseFloat(initCenterZTF.getText()));
            Vector3f point = computeCenter(image, orient, flag);

            if (centerOfMass != null) {
            	if (oldPoint.equals(centerOfMass) || useCenterOfMass) {

                    // user hasn't changed the initial point from the center of mass (or has chosen to just use the center
                    // of mass), then we can change the values
                    initCenterXTF.setText("" + point.X);
                    initCenterYTF.setText("" + point.Y);
                    initCenterZTF.setText("" + point.Z);
                }

            }
            
            centerOfMass = point;
        } else if ((event.getSource() == orientCheckbox) && !useSphereCheckbox.isSelected()) {

            // orientation affects ellisoid center calculation
            int orient = AlgorithmBrainExtractor.SAT_COR;

            if (orientCheckbox.isSelected()) {
                orient = FileInfoBase.AXIAL;
            } else {
                orient = AlgorithmBrainExtractor.SAT_COR;
            }

            Vector3f oldPoint = new Vector3f(Float.parseFloat(initCenterXTF.getText()),
                                           Float.parseFloat(initCenterYTF.getText()),
                                           Float.parseFloat(initCenterZTF.getText()));
            Vector3f point = computeCenter(image, orient, useSphereCheckbox.isSelected());

            if (oldPoint.equals(centerOfMass) || useCenterOfMass) {

                // user hasn't changed the initial point from the center of mass (or has chosen to just use the center
                // of mass), then we can change the values
                initCenterXTF.setText("" + point.X);
                initCenterYTF.setText("" + point.Y);
                initCenterZTF.setText("" + point.Z);
            }

            centerOfMass = point;
        }
    }

    /**
     * Loads the default settings from Preferences to set up the dialog.
     */
    public void legacyLoadDefaults() {
        String defaultsString = Preferences.getDialogDefaults(getDialogName());

        if (defaultsString != null) {

            try {
                StringTokenizer st = new StringTokenizer(defaultsString, ",");
                
                
                orientCheckbox.setSelected(MipavUtil.getInt(st) == FileInfoBase.AXIAL);
                justInitEllipsoidCheckbox.setSelected(MipavUtil.getBoolean(st));
                nIterationsTF.setText("" + MipavUtil.getInt(st));
                depthTF.setText("" + MipavUtil.getInt(st));
                imageRatioTF.setText("" + MipavUtil.getFloat(st));
                stiffnessTF.setText("" + MipavUtil.getFloat(st));
                useSphereCheckbox.setSelected(MipavUtil.getBoolean(st));
                secondStageCheckBox.setSelected(MipavUtil.getBoolean(st));
                erosionTF.setText("" + MipavUtil.getFloat(st));
                useCenterOfMassCheckBox.setSelected(MipavUtil.getBoolean(st));
                extractToPaintCheckBox.setSelected(MipavUtil.getBoolean(st));
                
                float val = MipavUtil.getFloat(st);

                if (val >= image.getExtents()[0]) {
                    val = image.getExtents()[0] / 2;
                }

                initCenterXTF.setText("" + val);

                val = MipavUtil.getFloat(st);

                if (val >= image.getExtents()[1]) {
                    val = image.getExtents()[1] / 2;
                }

                initCenterYTF.setText("" + val);

                val = MipavUtil.getFloat(st);

                if (val >= image.getExtents()[2]) {
                    val = image.getExtents()[2] / 2;
                }

                initCenterZTF.setText("" + val);
            } catch (Exception ex) {

                // since there was a problem parsing the defaults string, start over with the original defaults
                Preferences.debug("Resetting defaults for dialog: " + getDialogName());
                Preferences.removeProperty(getDialogName());
                ex.printStackTrace();
            }
        }
    }

    /**
     * Saves the default settings into the Preferences file.
     */
    public void legacySaveDefaults() {
        String defaultsString = new String(getParameterString(","));
        Preferences.saveDialogDefaults(getDialogName(), defaultsString);
    }


    /**
     * Set the factor above the median at which second stage erosion occurs.
     *
     * @param  aboveMedian  DOCUMENT ME!
     */
    public void setAboveMedian(float aboveMedian) {
        this.aboveMedian = aboveMedian;
    }

    /**
     * Sets whether or not to stop segmentation at paint.
     *
     * @param  extractToPaint  .
     */
    public void setExtractToPaint(boolean extractToPaint) {
        this.extractToPaint = extractToPaint;
    }

    /**
     * Sets the image influence ratio.
     *
     * @param  ratio  the image influence ratio
     */
    public void setImageRatio(float ratio) {
        imageRatio = ratio;
    }

    /**
     * Sets the number of iterations.
     *
     * @param  nIter  the number of iterations to do
     */
    public void setIterations(int nIter) {
        nIterations = nIter;
    }

    /**
     * Accessor that sets whether of not initial ellipsoid result is displayed.
     *
     * @param  justEllipse  DOCUMENT ME!
     */
    public void setJustEllipse(boolean justEllipse) {
        this.justEllipse = justEllipse;
    }

    /**
     * Set the maximum depth that is part of the image term in the surface evolution.
     *
     * @param  iMaxDepth  the new maximum depth
     */
    public void setMaxDepth(int iMaxDepth) {
        depth = iMaxDepth;
    }

    /**
     * Accessor that sets the orientation flag.
     *
     * @param  orient  DOCUMENT ME!
     */
    public void setOrientation(int orient) {
        orientation = orient;
    }

    /**
     * Sets whether or not the second stage performing edge erosion occurs.
     *
     * @param  secondStageErosion  if true erode boundary to clean up some.
     */
    public void setSecondStageErosion(boolean secondStageErosion) {
        this.secondStageErosion = secondStageErosion;
    }

    /**
     * Set the stiffness of the mesh that is part of the surface normal term in the surface evolution.
     *
     * @param  fStiffness  the new stiffness
     */
    public void setStiffness(float fStiffness) {
        stiffness = fStiffness;
    }


    /**
     * Sets whether or not to use only an estimate of sphere to nitialize the boundary evolution.
     *
     * @param  useSphere  if true estimate boundary using sphere model. If false the initial boundary is estimated using
     *                    an ellipsoid, which is more difficult and error prone.
     */
    public void setUseSphere(boolean useSphere) {
        this.useSphere = useSphere;
    }

    /**
     * Calls the algorithm.
     */
    protected void callAlgorithm() {

        try {
            System.gc();

            // Make algorithm
            extractBrainAlgo = new AlgorithmBrainExtractor(image, orientation, justEllipse, useSphere, initCenterPoint);
            extractBrainAlgo.setIterations(nIterations);
            extractBrainAlgo.setMaxDepth(depth);
            extractBrainAlgo.setImageRatio(imageRatio);
            extractBrainAlgo.setStiffness(stiffness);
            extractBrainAlgo.setSecondStageErosion(secondStageErosion);
            extractBrainAlgo.setAboveMedian(aboveMedian);
            extractBrainAlgo.setExtractPaint(extractToPaint);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            extractBrainAlgo.addListener(this);

            createProgressBar(image.getImageName(), extractBrainAlgo);

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

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (extractBrainAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                extractBrainAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog Extract Brain : unable to allocate enough memory");

            return;
        }
    }

    /**
     * Set the dialog GUI using the script parameters while running this algorithm as part of a script.
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        orientation = scriptParameters.getParams().getInt("orientation_type");
        useSphere = scriptParameters.getParams().getBoolean("do_use_sphere_estimation");
        justEllipse = scriptParameters.getParams().getBoolean("do_show_just_init_ellipse");
        nIterations = scriptParameters.getNumIterations();
        depth = scriptParameters.getParams().getInt("depth");
        imageRatio = scriptParameters.getParams().getFloat("image_ratio");
        stiffness = scriptParameters.getParams().getFloat("stiffness");
        secondStageErosion = scriptParameters.getParams().getBoolean("do_second_stage_erosion");
        aboveMedian = scriptParameters.getParams().getFloat("factor_above_median_to_erode");
        extractToPaint = scriptParameters.getParams().getBoolean("do_extract_paint");

        centerOfMass = computeCenter(image, orientation, useSphere);

        useCenterOfMass = scriptParameters.getParams().getBoolean("do_init_with_center_of_mass");

        float[] centerPoint = scriptParameters.getParams().getList("init_center_point").getAsFloatArray();
        initCenterX = centerPoint[0];
        initCenterY = centerPoint[1];
        initCenterZ = centerPoint[2];

        if (useCenterOfMass) {
            initCenterPoint = centerOfMass;
        } else {
            initCenterPoint = new Vector3f(initCenterX, initCenterY, initCenterZ);
        }
    }

    /**
     * Record the parameters just used to run this algorithm in a script.
     *
     * @throws  ParserException  If there is a problem creating/recording the new parameters.
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);

        scriptParameters.getParams().put(ParameterFactory.newParameter("orientation_type", orientation));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_use_sphere_estimation", useSphere));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_show_just_init_ellipse", justEllipse));
        scriptParameters.storeNumIterations(nIterations);
        scriptParameters.getParams().put(ParameterFactory.newParameter("depth", depth));
        scriptParameters.getParams().put(ParameterFactory.newParameter("image_ratio", imageRatio));
        scriptParameters.getParams().put(ParameterFactory.newParameter("stiffness", stiffness));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_second_stage_erosion", secondStageErosion));
        scriptParameters.getParams().put(ParameterFactory.newParameter("factor_above_median_to_erode", aboveMedian));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_extract_paint", extractToPaint));

        scriptParameters.getParams().put(ParameterFactory.newParameter("do_init_with_center_of_mass", useCenterOfMass));
        scriptParameters.getParams().put(ParameterFactory.newParameter("init_center_point",
                                                                       new float[] {
                                                                           initCenterPoint.X, initCenterPoint.Y,
                                                                           initCenterPoint.Z
                                                                       }));
    }

    /**
     * Makes the GUI elements of the dialog. Not called at present because it is not necessary.
     */
    private void init() {
        setTitle("Extract Brain");
        getContentPane().setLayout(new BorderLayout());

        JPanel optionsPanel = new JPanel(new GridLayout(3, 2));

        optionsPanel.setForeground(Color.black);
        optionsPanel.setBorder(buildTitledBorder("Options"));

        orientCheckbox = new JCheckBox("Axial image orientation.");
        orientCheckbox.setFont(serif12);
        if (image.getImageOrientation() == FileInfoBase.AXIAL) {
            orientCheckbox.setSelected(true);
        } else {
            orientCheckbox.setSelected(false);
        }
        orientCheckbox.addItemListener(this);
        optionsPanel.add(orientCheckbox);

        justInitEllipsoidCheckbox = new JCheckBox("Display initial ellipsoid result.");
        justInitEllipsoidCheckbox.setFont(serif12);
        justInitEllipsoidCheckbox.setSelected(false);
        justInitEllipsoidCheckbox.addItemListener(this);
        optionsPanel.add(justInitEllipsoidCheckbox);

        useSphereCheckbox = new JCheckBox("Estimate initial boundary using a sphere.");
        useSphereCheckbox.setFont(serif12);
        useSphereCheckbox.setSelected(false);
        useSphereCheckbox.addItemListener(this);
        optionsPanel.add(useSphereCheckbox);

        secondStageCheckBox = new JCheckBox("Second stage edge erosion");
        secondStageCheckBox.setSelected(false);
        secondStageCheckBox.addActionListener(this);
        secondStageCheckBox.setFont(serif12);
        optionsPanel.add(secondStageCheckBox);

        extractToPaintCheckBox = new JCheckBox("Extract brain to paint");
        extractToPaintCheckBox.setSelected(false);
        extractToPaintCheckBox.setFont(serif12);
        optionsPanel.add(extractToPaintCheckBox);

        JPanel paramPanel = new JPanel(new GridBagLayout());

        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        JLabel nIterLabel = new JLabel("Iterations (100 - 2000) ");

        nIterLabel.setFont(serif12);
        paramPanel.add(nIterLabel, gbc);

        gbc.gridx = 1;
        nIterationsTF = new JTextField();
        nIterationsTF.setText("500");
        nIterationsTF.setFont(serif12);
        paramPanel.add(nIterationsTF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;

        JLabel depthLabel = new JLabel("Depth (3 - 19) ");

        depthLabel.setFont(serif12);
        paramPanel.add(depthLabel, gbc);

        gbc.gridx = 1;
        depthTF = new JTextField();
        depthTF.setText("5");
        depthTF.setFont(serif12);
        paramPanel.add(depthTF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;

        JLabel imageRatioLabel = new JLabel("Image influence (0.01 - 0.5) ");

        imageRatioLabel.setFont(serif12);
        paramPanel.add(imageRatioLabel, gbc);

        gbc.gridx = 1;
        imageRatioTF = new JTextField();
        imageRatioTF.setText("0.1");
        imageRatioTF.setFont(serif12);
        paramPanel.add(imageRatioTF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;

        JLabel stiffnessLabel = new JLabel("Stiffness (0.01 - 0.5) ");

        stiffnessLabel.setFont(serif12);
        paramPanel.add(stiffnessLabel, gbc);

        gbc.gridx = 1;
        stiffnessTF = new JTextField();
        stiffnessTF.setText("0.15");
        stiffnessTF.setFont(serif12);
        paramPanel.add(stiffnessTF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        erosionLabel = new JLabel("Erode at percent above median: ");
        erosionLabel.setEnabled(false);
        erosionLabel.setFont(serif12);
        paramPanel.add(erosionLabel, gbc);

        gbc.gridx = 1;
        erosionTF = new JTextField("50.0");
        erosionTF.setEnabled(false);
        erosionTF.setFont(serif12);
        paramPanel.add(erosionTF, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 5;
        String orientText = "<html>Image origin is in the upper left hand corner (first slice)." + "<P>" +
        "Righthand coordinate system.</html>";
        JLabel orientIconLabel = new JLabel(orientText, MipavUtil.getIcon("orient.gif"), JLabel.LEFT);
        orientIconLabel.setFont(serif12);
        orientIconLabel.setForeground(Color.black);
        paramPanel.add(orientIconLabel, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 6;
        gbc.gridwidth = 2;
        useCenterOfMassCheckBox = new JCheckBox("Use the volume center of mass", useCenterOfMass);
        useCenterOfMassCheckBox.setFont(serif12);
        useCenterOfMassCheckBox.addItemListener(this);
        paramPanel.add(useCenterOfMassCheckBox, gbc);

        gbc.gridx = 0;
        gbc.gridy = 7;
        gbc.gridwidth = 1;
        initCenterXLabel = new JLabel("Initial mesh X position");
        initCenterXLabel.setEnabled(!useCenterOfMass);
        initCenterXLabel.setFont(serif12);
        paramPanel.add(initCenterXLabel, gbc);

        gbc.gridx = 1;
        initCenterXTF = new JTextField("" + initCenterX);
        initCenterXTF.setEnabled(!useCenterOfMass);
        initCenterXTF.setFont(serif12);
        paramPanel.add(initCenterXTF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 8;
        initCenterYLabel = new JLabel("Initial mesh Y position");
        initCenterYLabel.setEnabled(!useCenterOfMass);
        initCenterYLabel.setFont(serif12);
        paramPanel.add(initCenterYLabel, gbc);

        gbc.gridx = 1;
        initCenterYTF = new JTextField("" + initCenterY);
        initCenterYTF.setEnabled(!useCenterOfMass);
        initCenterYTF.setFont(serif12);
        paramPanel.add(initCenterYTF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 9;
        initCenterZLabel = new JLabel("Initial mesh Z position");
        initCenterZLabel.setEnabled(!useCenterOfMass);
        initCenterZLabel.setFont(serif12);
        paramPanel.add(initCenterZLabel, gbc);

        gbc.gridx = 1;
        initCenterZTF = new JTextField("" + initCenterZ);
        initCenterZTF.setEnabled(!useCenterOfMass);
        initCenterZTF.setFont(serif12);
        paramPanel.add(initCenterZTF, gbc);

        getContentPane().add(paramPanel, BorderLayout.CENTER);
        getContentPane().add(optionsPanel, BorderLayout.NORTH);

        // JPanel buttonPanel = new JPanel();
        // buildOKButton();
        // buttonPanel.add(OKButton);
        // buildCancelButton();
        // buttonPanel.add(cancelButton);
        // getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);
        pack();
        //setResizable(false);
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        if (orientCheckbox.isSelected()) {
            orientation = FileInfoBase.AXIAL;
        } else {
            orientation = AlgorithmBrainExtractor.SAT_COR;
        }

        if (justInitEllipsoidCheckbox.isSelected()) {
            justEllipse = true;
        } else {
            justEllipse = false;
        }

        if (useSphereCheckbox.isSelected()) {
            useSphere = true;
        } else {
            useSphere = false;
        }

        tmpStr = nIterationsTF.getText();

        if (testParameter(tmpStr, 1, 5000)) {
            nIterations = Integer.valueOf(tmpStr).intValue();
        } else {
            nIterationsTF.requestFocus();
            nIterationsTF.selectAll();

            return false;
        }

        tmpStr = depthTF.getText();

        if (testParameter(tmpStr, 3, 33)) {
            depth = Integer.valueOf(tmpStr).intValue();
        } else {
            depthTF.requestFocus();
            depthTF.selectAll();

            return false;
        }

        tmpStr = imageRatioTF.getText();

        if (testParameter(tmpStr, 0.01f, 0.51f)) {
            imageRatio = Float.valueOf(tmpStr).floatValue();
        } else {
            imageRatioTF.requestFocus();
            imageRatioTF.selectAll();

            return false;
        }

        tmpStr = stiffnessTF.getText();

        if (testParameter(tmpStr, 0.01f, 0.51f)) {
            stiffness = Float.valueOf(tmpStr).floatValue();
        } else {
            stiffnessTF.requestFocus();
            stiffnessTF.selectAll();

            return false;
        }

        secondStageErosion = secondStageCheckBox.isSelected();

        if (secondStageErosion) {
            tmpStr = erosionTF.getText();

            if (testParameter(tmpStr, 0.00f, 200.0f)) {
                aboveMedian = (Float.valueOf(tmpStr).floatValue() / 100.0f) + 1.0f;
            } else {
                erosionTF.requestFocus();
                erosionTF.selectAll();

                return false;
            }
        }

        extractToPaint = extractToPaintCheckBox.isSelected();
        useCenterOfMass = useCenterOfMassCheckBox.isSelected();

        if (!useCenterOfMass) {
            tmpStr = initCenterXTF.getText();

            if (testParameter(tmpStr, 0, image.getExtents()[0])) {
                initCenterX = Float.valueOf(tmpStr).floatValue();
            } else {
                initCenterXTF.requestFocus();
                initCenterXTF.selectAll();

                return false;
            }

            tmpStr = initCenterYTF.getText();

            if (testParameter(tmpStr, 0, image.getExtents()[1])) {
                initCenterY = Float.valueOf(tmpStr).floatValue();
            } else {
                initCenterYTF.requestFocus();
                initCenterYTF.selectAll();

                return false;
            }

            tmpStr = initCenterZTF.getText();

            if (testParameter(tmpStr, 0, image.getExtents()[2])) {
                initCenterZ = Float.valueOf(tmpStr).floatValue();
                //System.out.println("\n initCenterZ py = " + initCenterZ);
            } else {
                initCenterZTF.requestFocus();
                initCenterZTF.selectAll();

                return false;
            }
        }

        if (useCenterOfMass) {
            initCenterPoint = new Vector3f( centerOfMass );
        } else {
            initCenterPoint = new Vector3f(initCenterX, initCenterY, initCenterZ);
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
                return new String("Algorithms.Brain tools");
            }

            public String getDescription() {
                return new String("Segmentation of the brain from a 3D MRI.");
            }

            public String getDescriptionLong() {
                return new String("Segmentation of the brain from a 3D MRI.");
            }

            public String getShortLabel() {
                return new String("BET");
            }

            public String getLabel() {
                return new String("Extract Brain Surface (BET)");
            }

            public String getName() {
                return new String("Extract Brain Surface (BET)");
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
            table.put(new ParameterInt("orientation_type", 1));
            table.put(new ParameterBoolean("do_use_sphere_estimation", false));
            table.put(new ParameterBoolean("do_show_just_init_ellipse", false));
            table.put(new ParameterInt(AlgorithmParameters.NUM_ITERATIONS, 500));
            table.put(new ParameterInt("depth", 5));
            table.put(new ParameterFloat("image_ratio", .1f));
            table.put(new ParameterFloat("stiffness", .15f));
            table.put(new ParameterBoolean("do_second_stage_erosion", false));
            table.put(new ParameterFloat("factor_above_median_to_erode", 0f));
            table.put(new ParameterBoolean("do_extract_paint", false));
            table.put(new ParameterBoolean("do_init_with_center_of_mass", true));
            table.put(new ParameterList("init_center_point", Parameter.PARAM_FLOAT,"130.88019,108.925064,20.55177" ));

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
               return image.getImageName();
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
