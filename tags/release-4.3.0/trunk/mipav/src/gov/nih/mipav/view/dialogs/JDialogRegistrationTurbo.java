package gov.nih.mipav.view.dialogs;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * This code is derived from the Turbo Registration code of Philippe Thevenaz, speicifcally code from the module
 * TurboReg_.java. The work is based on the paper: P. Thevenaz, U.E. Ruttimann, M. Unser A Pyramid Approach to Subpixel
 * Registration Based on Intensity IEEE Transactions on Image Processing vol. 7, no. 1, pp. 27-41, January, 1998. This
 * paper is available at: http://bigwww.epfl.ch/publications/thevenaz9801.html Dialog to get user input The user selects
 * the target image. The source image is transformed until it is registered to the target image. Algorithms are executed
 * in their own thread.
 *
 * <p>In automatic mode, the landmark points of the source image are automatically refined to minimize the mean-square
 * difference between the target and the warped source image. For automatic mode if apply to only VOI region is not
 * selected, then every pixel within the image is considered relevant and should participate in the mean-square
 * computation. If apply to only VOI region is selected, then only those data inside the VOI region should participate
 * in the mean-square computation.</p>
 *
 * <p>Two output images are returned. The first is the warped source image. The second is a boolean mask image formed
 * from the warped source VOI.</p>
 */
public class JDialogRegistrationTurbo extends JDialogBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5462614304976325159L;

    /** A translation is described by a single point. It keeps area, angle, and orientation. */
    private static final int TRANSLATION = 1;

    /**
     * In 2D a pair of points determines the combination of a translation, of a rotation, and of an isotropic scaling.
     * Angles are conserved.
     */
    private static final int SCALED_ROTATION = 2;

    /**
     * In 2D three points generate an affine transformation, which is any combination of translation, rotation,
     * isotropic scaling, anisotropic scaling, shearing, and skewing. An affine transformation maps parallel lines onto
     * parallel lines.
     */
    private static final int AFFINE = 3;

    /**
     * In 2D four points describe a bilinear transformation, where a point of coordinates(x,y) is mapped on a point of
     * coordinates (u,v) such that u = p0 + p1*x + p2*y + p3*x*y and v = q0 + q1*x + q2*y + q3*x*y. Thus, both u and v
     * are both linear in x, and in y as well.
     */
    private static final int BILINEAR_DISTORTION = 4;

    /** DOCUMENT ME! */
    private static final int TRILINEAR_DISTORTION = 5;

    /** NEAREST_NEIGHBOR interpolation cannot be used in automatic mode. */
    private static final int NEAREST_NEIGHBOR = 1;

    /** Cubic spline interpolation. */
    private static final int CUBIC_SPLINE = 2;

    /**
     * Create an output image that is distorted in such a way that the landmarks of the source are made to coincide with
     * those of the target.
     */
    private static final int MANUAL = 1;

    /**
     * Refine the landmarks of the source image in such a way that the least-squares error between the source image and
     * target is minimized.
     */
    private static final int AUTOMATIC = 2;

    /**
     * Minimal size of an image in the multiresolution pyramid. To use automatic mode all all dimensions of both the
     * source and target images must be >= 2*MIN_SIZE. If any dimension of either image is less than 2*MIN_SIZE,
     * processing will be NEAREST_NEIGHBOR interpolation in MANUAL mode.
     */
    private static final int MIN_SIZE = 8;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private int automan; // MANUAL or AUTOMATIC

    /** DOCUMENT ME! */
    private JCheckBox automaticCheckBox;

    /** DOCUMENT ME! */
    private JComboBox comboBoxDistort;

    /** DOCUMENT ME! */
    private JComboBox comboBoxImage;

    /** DOCUMENT ME! */
    private JComboBox comboBoxInterp;

    /** DOCUMENT ME! */
    private int DIM;

    /** DOCUMENT ME! */
    private boolean entireSource = true;

    /** DOCUMENT ME! */
    private boolean entireTarget = true;

    /** or TRILINEAR_DISTORTION. */
    private int interpolation; // NEAREST_NEIGHBOR or CUBIC_SPLINE

    /** DOCUMENT ME! */
    private ModelImage[] resultImage = null; // result image

    /** DOCUMENT ME! */
    private ModelImage sourceImage; // register source image to targetImage

    /** DOCUMENT ME! */
    private JCheckBox sourceVOICheckBox;

    /** DOCUMENT ME! */
    private ModelImage targetImage;

    /** DOCUMENT ME! */
    private JCheckBox targetVOICheckBox;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private int transformation; // TRANSLATION, SCALED_ROTATION, AFFINE, BILINEAR_DISTORTION,

    /** DOCUMENT ME! */
    private AlgorithmRegTurbo turbo = null;

    /** DOCUMENT ME! */
    private ViewUserInterface UI;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for registering two images using Turbo.
     *
     * @param  theParentFrame  parent frame
     * @param  im              source image
     */
    public JDialogRegistrationTurbo(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        UI = ((ViewJFrameBase) theParentFrame).getUserInterface();
        sourceImage = im;
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
        } else if (command.equals("Auto")) {

            if (automaticCheckBox.isSelected()) {
                sourceVOICheckBox.setEnabled(true);
                targetVOICheckBox.setEnabled(true);
                comboBoxInterp.setSelectedIndex(0);
            } else {
                sourceVOICheckBox.setEnabled(false);
                sourceVOICheckBox.setSelected(false);
                targetVOICheckBox.setEnabled(false);
                targetVOICheckBox.setSelected(false);
            }
        } else if (command.equals("Interp")) {

            if (automaticCheckBox.isSelected()) {

                if (comboBoxInterp.getSelectedIndex() == 1) {
                    MipavUtil.displayWarning("Nearest neightbor interpolation cannot be used in automatic mode.");
                    comboBoxInterp.setSelectedIndex(0);
                }
            }
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        int i;
        ViewJFrameImage[] imageFrame = new ViewJFrameImage[2];

        if (algorithm instanceof AlgorithmRegTurbo) {
            sourceImage.clearMask();

            if ((turbo.isCompleted() == true) && (resultImage != null)) {

                // The algorithm has completed and produced a new image to be displayed.
                for (i = 0; i < 2; i++) {
                    updateFileInfo(sourceImage, resultImage[i]);
                    resultImage[i].clearMask();

                    try {
                        imageFrame[i] = new ViewJFrameImage(resultImage[i], null, new Dimension(610, 200 + (i * 20)));
                    } catch (OutOfMemoryError error) {
                        System.gc();
                        JOptionPane.showMessageDialog(null, "Out of memory: unable to open new frame", "Error",
                                                      JOptionPane.ERROR_MESSAGE);
                    }
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                for (i = 0; i < 2; i++) {

                    if (resultImage[i] != null) {
                        resultImage[i].disposeLocal(); // Clean up memory of result image
                        resultImage[i] = null;
                    }
                }

                resultImage = null;
                System.gc();
            }
        }

        // These next lines set the titles in all frames where the source image is displayed to
        // image name so as to indicate that the image is now unlocked!
        // The image frames are enabled and then registed to the userinterface.
        Vector imageFrames = sourceImage.getImageFrameVector();

        for (i = 0; i < imageFrames.size(); i++) {
            ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
            ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

            if (((Frame) (imageFrames.elementAt(i))) != parentFrame) {
                UI.registerFrame((Frame) (imageFrames.elementAt(i)));
            }

            if (parentFrame != null) {
                UI.registerFrame(parentFrame);
                // image.notifyImageDisplayListeners(null, true);
            }
        }

        // Update frame
        // ((ViewJFrameBase)parentFrame).updateImages(true);
        sourceImage.notifyImageDisplayListeners(null, true);
        dispose();
    }

    /**
     * Once all the variables have been set, calls the algorithm.
     */
    protected void callAlgorithm() {
        System.gc();

        try {

            if (sourceImage.getNDims() == 2) {
                DIM = 2;
            } else if (sourceImage.getNDims() == 3) {
                DIM = 3;
            }

            // Hide dialog
            setVisible(false);

            int nPtsA = 0; // = targetImage.getVOIs().size();
            int nPtsB = 0; // = sourceImage.getVOIs().size()
            Vector3f[] tmpptA = null;
            Vector3f[] tmpptB = null;
            double[][] targetPt = null; // new double[nPtsA][2] or new double[nPtsA][3]
            double[][] sourcePt = null; // new double[nPtsB][2] or new double[nPtsB][3]
            int i, j, s, ptNum, nVOIs;
            Vector[] curves;
            ViewVOIVector VOIs;

            if (targetImage.getVOIs().size() == 0) {
                MipavUtil.displayError("Select points before clicking OK");

                return;
            } else {

                if ((targetImage.getNDims() == 3) && (sourceImage.getNDims() == 3)) {

                    if (((automan == AUTOMATIC) || (interpolation != NEAREST_NEIGHBOR)) &&
                            ((targetImage.getExtents()[0] < (2 * MIN_SIZE)) ||
                                 (targetImage.getExtents()[1] < (2 * MIN_SIZE)) ||
                                 (targetImage.getExtents()[2] < (2 * MIN_SIZE)) ||
                                 (sourceImage.getExtents()[0] < (2 * MIN_SIZE)) ||
                                 (sourceImage.getExtents()[1] < (2 * MIN_SIZE)) ||
                                 (sourceImage.getExtents()[2] < (2 * MIN_SIZE)))) {
                        automan = MANUAL;
                        interpolation = NEAREST_NEIGHBOR;
                        entireSource = true;
                        entireTarget = true;
                        i = 2 * MIN_SIZE;
                        MipavUtil.displayWarning("Because of dimension < " + i +
                                                 "changing to AUTOMATIC and NEAREST_NEIGHBOR");
                    }

                    VOIs = targetImage.getVOIs();
                    nVOIs = VOIs.size();

                    for (i = 0; i < nVOIs; i++) {

                        if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) {
                            curves = targetImage.getVOIs().VOIAt(i).getCurves();

                            for (s = 0; s < targetImage.getExtents()[2]; s++) {
                                nPtsA += curves[s].size();
                            }

                            Preferences.debug("nPtsA = " + nPtsA + "\n");
                            targetPt = new double[nPtsA][3];

                            for (s = 0; s < targetImage.getExtents()[2]; s++) {
                                tmpptA = targetImage.getVOIs().VOIAt(i).exportPoints(s);

                                for (j = 0; j < tmpptA.length; j++) {
                                    ptNum = (int)
                                                (Short.valueOf(((VOIPoint) curves[s].elementAt(j)).getLabel()).shortValue()) -
                                            1;
                                    targetPt[ptNum][0] = tmpptA[j].X;
                                    targetPt[ptNum][1] = tmpptA[j].Y;
                                    targetPt[ptNum][2] = tmpptA[j].Z;
                                }
                            }
                        } // if (VOIs.VOIAt(i).getCurveType() == VOI.POINT)
                    } // for (i = 0; i < nVOIs; i++)

                    VOIs = sourceImage.getVOIs();
                    nVOIs = VOIs.size();

                    for (i = 0; i < nVOIs; i++) {

                        if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) {
                            curves = sourceImage.getVOIs().VOIAt(i).getCurves();

                            for (s = 0; s < sourceImage.getExtents()[2]; s++) {
                                nPtsB += curves[s].size();
                            }

                            Preferences.debug("nPtsB = " + nPtsB + "\n");
                            sourcePt = new double[nPtsB][3];

                            for (s = 0; s < sourceImage.getExtents()[2]; s++) {
                                tmpptB = sourceImage.getVOIs().VOIAt(i).exportPoints(s);

                                for (j = 0; j < tmpptB.length; j++) {
                                    ptNum = (int)
                                                (Short.valueOf(((VOIPoint) curves[s].elementAt(j)).getLabel()).shortValue()) -
                                            1;
                                    sourcePt[ptNum][0] = tmpptB[j].X;
                                    sourcePt[ptNum][1] = tmpptB[j].Y;
                                    sourcePt[ptNum][2] = tmpptB[j].Z;
                                }
                            }
                        } // if (VOIs.VOIAt(i).getCurveType() == VOI.POINT)
                    } // for (i = 0; i < nVOIs; i++)

                } // if ((targetImage.getNDims() == 3 ) && (sourceImage.getNDims() == 3 ))
                else if ((targetImage.getNDims() == 2) && (sourceImage.getNDims() == 2)) {

                    if (((automan == AUTOMATIC) || (interpolation != NEAREST_NEIGHBOR)) &&
                            ((targetImage.getExtents()[0] < (2 * MIN_SIZE)) ||
                                 (targetImage.getExtents()[1] < (2 * MIN_SIZE)) ||
                                 (sourceImage.getExtents()[0] < (2 * MIN_SIZE)) ||
                                 (sourceImage.getExtents()[1] < (2 * MIN_SIZE)))) {
                        automan = MANUAL;
                        interpolation = NEAREST_NEIGHBOR;
                        entireSource = true;
                        entireTarget = true;
                        i = 2 * MIN_SIZE;
                        MipavUtil.displayWarning("Because of dimension < " + i +
                                                 "changing to AUTOMATIC and NEAREST_NEIGHBOR");
                    }

                    VOIs = targetImage.getVOIs();
                    nVOIs = VOIs.size();

                    for (i = 0; i < nVOIs; i++) {

                        if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) {
                            curves = targetImage.getVOIs().VOIAt(i).getCurves();
                            nPtsA = curves[0].size();
                            Preferences.debug("nPtsA = " + nPtsA + "\n");
                            targetPt = new double[nPtsA][2];
                            tmpptA = targetImage.getVOIs().VOIAt(i).exportPoints(0);

                            for (j = 0; j < tmpptA.length; j++) {
                                ptNum = (int)
                                            (Short.valueOf(((VOIPoint) curves[0].elementAt(j)).getLabel()).shortValue()) -
                                        1;
                                targetPt[ptNum][0] = tmpptA[j].X;
                                targetPt[ptNum][1] = tmpptA[j].Y;
                            }
                        } // if (VOIs.VOIAt(i).getCurveType() == VOI.POINT)
                    } // for (i = 0; i < nVOIs; i++)

                    VOIs = sourceImage.getVOIs();
                    nVOIs = VOIs.size();

                    for (i = 0; i < nVOIs; i++) {

                        if (VOIs.VOIAt(i).getCurveType() == VOI.POINT) {
                            curves = sourceImage.getVOIs().VOIAt(i).getCurves();
                            nPtsB += curves[0].size();
                            Preferences.debug("nPtsB = " + nPtsB + "\n");
                            sourcePt = new double[nPtsB][2];
                            tmpptB = sourceImage.getVOIs().VOIAt(i).exportPoints(0);

                            for (j = 0; j < tmpptB.length; j++) {
                                ptNum = (int)
                                            (Short.valueOf(((VOIPoint) curves[0].elementAt(j)).getLabel()).shortValue()) -
                                        1;
                                sourcePt[ptNum][0] = tmpptB[j].X;
                                sourcePt[ptNum][1] = tmpptB[j].Y;
                            }
                        } // if (VOIs.VOIAt(i).getCurveType() == VOI.POINT)
                    } // for (i = 0; i < nVOIs; i++)

                } // else if ((targetImage.getNDims() == 2 ) && (sourceImage.getNDims() == 2 ))
            }

            resultImage = new ModelImage[2];
            resultImage[0] = new ModelImage(ModelStorageBase.FLOAT, targetImage.getExtents(),
                                            sourceImage.getImageName() + " registered");

            resultImage[1] = new ModelImage(ModelStorageBase.BOOLEAN, targetImage.getExtents(),
                                            sourceImage.getImageName() + " output mask");

            turbo = new AlgorithmRegTurbo(resultImage, targetImage, entireTarget, sourceImage, entireSource, targetPt,
                                          sourcePt, transformation, interpolation, automan);
        } catch (OutOfMemoryError x) {
            MipavUtil.displayError("Dialog RegistrationTurbo: unable to allocate enough memory");

            if (resultImage != null) {

                for (int i = 0; i < 2; i++) {

                    if (resultImage[i] != null) {
                        resultImage[i].disposeLocal(); // Clean up memory of result image
                        resultImage[i] = null;
                    }
                }

                resultImage = null;
            }

            return;
        }

        // This is very important. Adding this object as a listener allows
        // the algorithm to notify this object when it has completed of failed.
        // See algorithm performed event. This is made possible by implementing
        turbo.addListener(this);

        createProgressBar(sourceImage.getImageName(), turbo);

        // These next lines set the titles in all frames where the source image
        // is displayed to "locked - " image name so as to indicate that the image
        // is now read/write locked!  The image frames are disabled and then
        // unregisted from the userinterface until the algorithm has completed.
        Vector imageFrames = sourceImage.getImageFrameVector();
        titles = new String[imageFrames.size()];

        for (int i = 0; i < imageFrames.size(); i++) {
            titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
            ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
            ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
            UI.unregisterFrame((Frame) (imageFrames.elementAt(i)));
        }

        if (isRunInSeparateThread()) {

            // Start the thread as a low priority because we wish to still have
            // user interface work fast
            if (turbo.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            turbo.run();
        }
    }

    /**
     * Initializes GUI components and adds them to the dialog.
     */
    private void init() {
        setTitle("Turbo Registration");

        JPanel optPanel = new JPanel(new GridLayout(3, 2));
        optPanel.setBorder(buildTitledBorder("Options"));

        String matchName = sourceImage.getImageName();
        JLabel labelImage = new JLabel("Register [" + matchName + "] to:");
        labelImage.setForeground(Color.black);
        labelImage.setFont(serif12);

        comboBoxImage = buildImageComboBox(sourceImage);

        JLabel distortLabel = new JLabel("Distortion methods");
        distortLabel.setForeground(Color.black);
        distortLabel.setFont(serif12);

        comboBoxDistort = new JComboBox();
        comboBoxDistort.setBackground(Color.white);
        comboBoxDistort.setFont(serif12);

        comboBoxDistort.addItem("Translation");
        comboBoxDistort.addItem("Scaled Rotation");
        comboBoxDistort.addItem("Affine");

        if (sourceImage.getNDims() == 2) {
            comboBoxDistort.addItem("Bilinear");
        } else {
            comboBoxDistort.addItem("Trilinear");
        }

        JLabel interpLabel = new JLabel("Interpolation");
        interpLabel.setFont(serif12);
        interpLabel.setForeground(Color.black);

        comboBoxInterp = new JComboBox();
        comboBoxInterp.setBackground(Color.white);
        comboBoxInterp.setFont(serif12);

        comboBoxInterp.addItem("BSpline 3rd order");
        comboBoxInterp.addItem("Nearest Neighbor");

        optPanel.add(labelImage);
        optPanel.add(comboBoxImage);
        optPanel.add(distortLabel);
        optPanel.add(comboBoxDistort);
        optPanel.add(interpLabel);
        optPanel.add(comboBoxInterp);

        JPanel automaticPanel = new JPanel(new GridBagLayout());
        automaticPanel.setBorder(buildTitledBorder("Source automatic alignment"));

        automaticCheckBox = new JCheckBox("Automatically nudge initial source landmarks");
        automaticCheckBox.setFont(serif12);
        automaticCheckBox.setEnabled(true);
        automaticCheckBox.setSelected(true);
        automaticCheckBox.addActionListener(this);
        automaticCheckBox.setActionCommand("Auto");

        sourceVOICheckBox = new JCheckBox("Apply only to VOI region in source");
        sourceVOICheckBox.setFont(serif12);
        sourceVOICheckBox.setEnabled(true);
        sourceVOICheckBox.setSelected(false);
        sourceVOICheckBox.addActionListener(this);

        targetVOICheckBox = new JCheckBox("Apply only to VOI region in destination");
        targetVOICheckBox.setBounds(10, 75, 280, 25);
        targetVOICheckBox.setFont(serif12);
        targetVOICheckBox.setEnabled(true);
        targetVOICheckBox.setSelected(false);
        targetVOICheckBox.addActionListener(this);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        automaticPanel.add(automaticCheckBox, gbc);
        gbc.gridy = 1;
        automaticPanel.add(sourceVOICheckBox, gbc);
        gbc.gridy = 2;
        automaticPanel.add(targetVOICheckBox, gbc);

        comboBoxInterp.addActionListener(this);
        comboBoxInterp.setActionCommand("Interp");

        JPanel mainPanel = new JPanel(new GridBagLayout());
        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add(optPanel, gbc);
        gbc.gridy = 1;
        mainPanel.add(automaticPanel, gbc);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

    /**
     * Sets up the variables needed from the GUI components.
     *
     * @return  Flag indicating successful set.
     */
    private boolean setVariables() {
        int index = comboBoxDistort.getSelectedIndex();

        if (index == 0) {
            transformation = TRANSLATION;
        } else if (index == 1) {
            transformation = SCALED_ROTATION;
        } else if (index == 2) {
            transformation = AFFINE;
        } else {

            if (sourceImage.getNDims() == 2) {
                transformation = BILINEAR_DISTORTION;
            } else {
                transformation = TRILINEAR_DISTORTION;
            }
        }

        index = comboBoxInterp.getSelectedIndex();

        if (index == 0) {
            interpolation = CUBIC_SPLINE;
        } else {
            interpolation = NEAREST_NEIGHBOR;
        }

        if (automaticCheckBox.isSelected()) {
            automan = AUTOMATIC;
        } else {
            automan = MANUAL;
        }

        if (sourceVOICheckBox.isSelected()) {
            entireSource = false;
        } else {
            entireSource = true;
        }

        if (targetVOICheckBox.isSelected()) {
            entireTarget = false;
        } else {
            entireTarget = true;
        }
        // assign targetImage to image selected in comboBox

        String selectedName = (String) comboBoxImage.getSelectedItem();
        targetImage = UI.getRegisteredImageByName(selectedName);

        return true;
    }
}
