package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;
import javax.swing.event.*;

import Jama.Matrix;
import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * Dialog to get user input, then call algorithmTransform. User may select resample or transform. User may input matrix
 * or use image's associated transformation matrix. User may input desired resolutions and dims. User may select
 * interpolation method. Creates new volume.
 * 
 * You can choose either of 2 goals in bilinear or trilinear interpolation, but you cannot choose both. You can choose
 * to:
 * 
 * 1.) Match the start row, column, and slice in the original image with the start row, column, and slice in the
 * transformed image with no duplication of start values and match the end row, column, and slice in the original image
 * with the end row column, and slice in the transformed image with no duplication of end values with a smooth
 * interpolation occurring between the beginning and end. For a smooth bilinear or trilinear interpolation you must map
 * from 0 to n1t - 1 in the transformed image to 0 to n1 - 1 in the original image. Mapping from n1t - 1 to n1t in the
 * transformed image to n1 - 1 to n1 in the source image would lead to multiple identical transformed copies for source
 * image values between n1 - 1 and n1 - 0.5 and identical or out of bounds transformed values for source values from n1 -
 * 0.5 to n1. This necessitates using equations of the form (dim - 1) * res = (transformedDim - 1) * transformedRes.
 * 
 * Since the field of view = dim * res, this does not preserve field of view.
 * 
 * If a user wishes to reslice an image and have the beginning and end slices match without duplication, then this would
 * be the method to select
 * 
 * 2.) If a user simply wishes to magnify the field of view and is not worried about duplicate beginning and end values,
 * then preserve the field of view = dim * res = transformedDim * transformedRes.
 * 
 * If the interpolation is not bilinear or trilinear, then the purpose of interpolation is always to preserve the FOV.
 * 
 * For either interpolation purpose, note that if the user selects the new dimension, then the floating point resolution
 * can be perfectly adjusted, but if the user selects the new resolution, since dimensions are integers, the new
 * dimension value may not be perfectly adjusted.
 * 
 * @version 0.1 Nov. 19, 1999
 * @author Delia McGarry
 * @author Neva Cherniavsky
 * @author Zohara Cohen
 */
public class JDialogScriptableTransform extends JDialogScriptableBase implements AlgorithmInterface, ChangeListener,
        ActionDiscovery {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7786904359172693422L;

    /** DOCUMENT ME! */
    private static final int ORIG_TO_ACPC = 0;

    /** DOCUMENT ME! */
    private static final int ORIG_TO_TLRC = 1;

    /** DOCUMENT ME! */
    private static final int ACPC_TO_TLRC = 2;

    /** DOCUMENT ME! */
    private static final int TLRC_TO_ACPC = 3;

    /** DOCUMENT ME! */
    private static final int TLRC_TO_ORIG = 4;

    /** DOCUMENT ME! */
    private static final int ACPC_TO_ORIG = 5;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmTalairachTransform algoTal = null;

    /** or if the source image is to be replaced. */
    private AlgorithmTransform algoTrans = null;

    /** DOCUMENT ME! */
    private JCheckBox clipCheckbox, voiCheckbox, image25DCheckbox, updateOriginCheckbox, invertCheckbox;

    /** DOCUMENT ME! */
    private JComboBox comboBoxImage;

    /** DOCUMENT ME! */
    private JComboBox comboBoxInterp;

    /** DOCUMENT ME! */
    private JComboBox comboBoxTalTransform;

    /** DOCUMENT ME! */
    private JRadioButton computeTImage;

    /** DOCUMENT ME! */
    private boolean do25D = false;

    /** DOCUMENT ME! */
    private boolean doRotateCenter;

    /** DOCUMENT ME! */
    private boolean doTalairach = false;

    /** DOCUMENT ME! */
    private boolean doVOI, doClip, doPad, preserveFOV, doUpdateOrigin, doInvMat;

    /**
     * Stores the matrix read in from a file it then can be converted to the corrected axis orientation (i.e. world
     * coordinate and/or left-hand coordinate systems).
     */
    private TransMatrix fileTransMatrix;

    /** source image */
    private ModelImage image; 

    /** Interpolation method */
    private int interp = AlgorithmTransform.TRILINEAR;

    private float fillValue = 0.0f;

    private JLabel labelOrigin;

    /** DOCUMENT ME! */
    private JLabel labelResX, labelResY, labelResZ, labelDimX, labelDimY, labelDimZ;

    /** DOCUMENT ME! */
    private JLabel labelTx, labelTy, labelTz, labelRx, labelRy, labelRz, labelSx, labelSy, labelSz, labelSKx, labelSKy,
            labelSKz;

    /** If true change matrix to the left-hand coordinate system. */
    private boolean leftHandSystem = false;

    /** DOCUMENT ME! */
    private JSlider magSlider;

    /** DOCUMENT ME! */
    private ButtonGroup matrixDeterminationGroup, rotationAxisGroup, cropOrPad;

    /** DOCUMENT ME! */
    private String matrixFile;

    /** DOCUMENT ME! */
    private JTextField matrixFName;

    /** DOCUMENT ME! */
    private int max, min;

    /** DOCUMENT ME! */
    private JLabel maximum, minimum, current;

    /** DOCUMENT ME! */
    private int oXdim, oYdim, oZdim, cXdim, cYdim, cZdim;

    /** DOCUMENT ME! */
    private float oXres, oYres, oZres, cXres, cYres, cZres;

    private int[] units;

    /** DOCUMENT ME! */
    private ButtonGroup resampleGroup;

    /** DOCUMENT ME! */
    private ModelImage resampleImage;

    /** DOCUMENT ME! */
    private JRadioButton resampletoUser, resampletoImage, resampleSlider;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null; // result image

    /** DOCUMENT ME! */
    private JRadioButton rotCenter, rotOrigin, cropRadio, padRadio;

    /** DOCUMENT ME! */
    private JRadioButton storedMatrix, noTransform, userDefinedMatrix, fileMatrix;

    private JComboBox storedMatrixBox;

    /** DOCUMENT ME! */
    private JTextField textResX, textResY, textResZ, textDimX, textDimY, textDimZ;

    /** DOCUMENT ME! */
    private JTextField textTx, textTy, textTz, textRx, textRy, textRz, textSx, textSy, textSz, textSKx, textSKy,
            textSKz;

    /** DOCUMENT ME! */
    private TalairachTransformInfo tInfo = null;

    /** DOCUMENT ME! */
    private int transformType;

    /** DOCUMENT ME! */
    private String[] tVal;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    /** If true change matrix to the world coordinate system. */
    private boolean wcSystem = false;

    /** DOCUMENT ME! */
    private TransMatrix xfrm;

    /** DOCUMENT ME! */
    private JCheckBox xyAspectRatio, xyzAspectRatio;
    
    private JRadioButton preserveFOVButton;
    
    private JRadioButton preserveOutputDimsButton;
    
    private ButtonGroup outputDimsGroup;
    
    private JRadioButton preserveFOV2Button;
    
    private JRadioButton separateDimResButton;
    
    private ButtonGroup resDimGroup;

    /** checkbox telling the algorithm to use the scanner coordinate center rather than the image center */
    private JCheckBox useSACenterBox;

    private boolean useSACenter = false;

    /** Tabbed pane */
    private JTabbedPane tabbedPane = null;

    /** is this a scanner anatomical transform (->AXIAL) */
    private boolean isSATransform = false;

    private boolean enableSATransform = false;

    /** We use the constant to preserve the FOV. Either 1 or 0; */
    private int constantFOV = 1;

    /** Button group for interpolation type, contant FOV or start, end matching. */
    private ButtonGroup interpFOVgroup;

    /** Radio button for constant FOV interpolation. */
    private JRadioButton constantFOVradio;

    /** Radio button for slice start & end matching interpolation. */
    private JRadioButton endMatchFOVradio;

    private float[] dims;

    private float[] resols;

    private JLabel outOfBoundsLabel;

    private JComboBox outOfBoundsComboBox;

    private JLabel valueLabel;

    private JTextField valueText;

    private double imageMin;

    private double imageMax;

    private int dataType;
    
    private JPanel resamplePanel;

    /**
     * Tells how to select fill value for out of bounds data 0 for image minimum 1 for NaN for float, zero otherwise. 2
     * for user defined 3 for image maximum
     */
    private int outOfBoundsIndex = 0;

    private AlgorithmTPSpline spline = null;

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogScriptableTransform() {}

    /**
     * Constructs new transform dialog and sets up GUI components.
     * 
     * @param theParentFrame Parent frame.
     * @param im Source image.
     */
    public JDialogScriptableTransform(final Frame theParentFrame, final ModelImage im) {
        super(theParentFrame, false);

        image = im;
        image.makeUnitsOfMeasureIdentical();
        resampleImage = im;

        userInterface = ViewUserInterface.getReference();
        cZres = 1.f;
        cZdim = 1;
        dims = new float[3];
        resols = new float[3];
        dims[0] = image.getFileInfo()[0].getExtents()[0];
        dims[1] = image.getFileInfo()[0].getExtents()[1];
        if (image.getNDims() > 2) {
            dims[2] = image.getFileInfo()[0].getExtents()[2];
        }

        resols[0] = image.getFileInfo()[0].getResolutions()[0];
        resols[1] = image.getFileInfo()[0].getResolutions()[1];
        if (image.getNDims() > 2) {
            resols[2] = image.getFileInfo()[0].getResolutions()[2];
        }
        init();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed, sets the variables, and calls the algorithm.
     * 
     * @param event Event that triggers function.
     */
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {
            //MipavUtil.showHelp("Transform010");
            MipavUtil.showWebHelp("Transform");
        }
    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     * 
     * @param algorithm Algorithm that caused the event.
     */
    public void algorithmPerformed(final AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmTransform) {
            resultImage = algoTrans.getTransformedImage();

            if ( (algoTrans.isCompleted() == true) && (resultImage != null)) {
                resultImage.calcMinMax();

                // The algorithm has completed and produced a new image to be displayed.
                try {
                    if(!isQuietRunning()) {
                        new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                    }
                } catch (final OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }
        } // if (algorithm instanceof AlgorithmTransform)
        else if (algorithm instanceof AlgorithmTalairachTransform) {

            if ( (algoTal.isCompleted() == true) && (resultImage != null)) {
                resultImage.calcMinMax();

                // The algorithm has completed and produced a new image to be displayed.
                try {
                    if(!isQuietRunning()) {
                        new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                    }
                } catch (final OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } // if ((algoTal.isCompleted == true) && (resultImage != null))
            else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
            }

        } // else if (algorithm instanceof AlgorithmTalairachTransform)

        System.gc();

        // Update frames
        image.notifyImageDisplayListeners(null, true);

        // save the completion status for later
        setComplete(algorithm.isCompleted());

        if (algorithm.isCompleted()) {
            insertScriptLine();
        }

        if (algoTrans != null) {
            algoTrans.disposeLocal();
            algoTrans = null;
        }

        if (algoTal != null) {
            algoTal.disposeLocal();
            algoTal = null;
        }

        dispose();
    }

    /**
     * When the user clicks the mouse out of a text field, resets the necessary variables.
     * 
     * @param event event that triggers this function
     */
    public void focusLost(final FocusEvent event) {
        final Object source = event.getSource();
        JTextField tempTextField;
        String userText;
        float userValue;
        float factor, fov;

        factor = 1.f;

        tempTextField = (JTextField) source;
        userText = tempTextField.getText();
        userValue = Float.valueOf(userText).floatValue();

        if (source == textDimX) {
            factor = (userValue - constantFOV) / (cXdim - constantFOV);
            dims[0] = userValue;

            if (preserveFOV2Button.isSelected()) { // update resolution (user set dimensions and FOV is selected)
                fov = (cXdim - constantFOV) * cXres;
                resols[0] = fov / (dims[0] - constantFOV);
            }
        } else if (source == textDimY) {
            factor = (userValue - constantFOV) / (cYdim - constantFOV);
            dims[1] = userValue;

            if (preserveFOV2Button.isSelected()) { // update resolution (user set dimensions and FOV is selected)
                fov = (cYdim - constantFOV) * cYres;
                resols[1] = fov / (dims[1] - constantFOV);
            }
        } else if (source == textDimZ) {
            factor = (userValue - constantFOV) / (cZdim - constantFOV);
            dims[2] = userValue;

            if (preserveFOV2Button.isSelected()) { // update resolution in z
                fov = (cZdim - constantFOV) * cZres;
                resols[2] = fov / (dims[2] - constantFOV);
            }

        } else if (source == textResX) {
            factor = cXres / userValue;
            resols[0] = userValue;

            if (preserveFOV2Button.isSelected()) { // update resolution (user set dimensions and FOV is selected)
                fov = (cXdim - constantFOV) * cXres;
                dims[0] = fov / resols[0] + constantFOV;
            }
        } else if (source == textResY) {
            factor = cYres / userValue;
            resols[1] = userValue;

            if (preserveFOV2Button.isSelected()) { // update resolution (user set dimensions and FOV is selected)
                fov = (cYdim - constantFOV) * cYres;
                dims[1] = fov / resols[1] + constantFOV;
            }
        } else if (source == textResZ) {
            factor = cZres / userValue;
            resols[2] = userValue;

            if (preserveFOV2Button.isSelected()) { // update resolution (user set dimensions and FOV is selected)
                fov = (cZdim - constantFOV) * cZres;
                dims[2] = fov / resols[2] + constantFOV;
                // System.err.println(" cZres = " + cZres + " cZdim = " + cZdim);
            }
        }

        if ( (source == textResX) || (source == textDimX)) {

            if (xyAspectRatio.isSelected() || xyzAspectRatio.isSelected()) { // update y values
                if ( (source == textDimX) || (preserveFOV2Button.isSelected())) {
                    dims[1] = (dims[1] - constantFOV) * factor + constantFOV;
                }

                if (preserveFOV2Button.isSelected()) {
                    resols[1] = resols[1] / factor;
                }
            }

            if (xyzAspectRatio.isSelected()) { // update z values
                if ( (source == textDimX) || (preserveFOV2Button.isSelected())) {
                    dims[2] = (dims[2] - constantFOV) * factor + constantFOV;
                }

                if (preserveFOV2Button.isSelected()) {
                    resols[2] = resols[2] / factor;
                }
            }
        } else if ( (source == textResY) || (source == textDimY)) {

            if (xyAspectRatio.isSelected() || xyzAspectRatio.isSelected()) { // update x
                if ( (source == textDimY) || (preserveFOV2Button.isSelected())) {
                    dims[0] = (dims[0] - constantFOV) * factor + constantFOV;
                }

                if (preserveFOV2Button.isSelected()) {
                    resols[0] = resols[0] / factor;
                }
            }

            if (xyzAspectRatio.isSelected()) { // update z
                if ( (source == textDimY) || (preserveFOV2Button.isSelected())) {
                    dims[2] = (dims[2] - constantFOV) * factor + constantFOV;
                }

                if (preserveFOV2Button.isSelected()) {
                    resols[2] = resols[2] / factor;
                }
            }
        } else if ( (source == textResZ) || (source == textDimZ)) {

            if (xyAspectRatio.isSelected()) {} // do nothing, x and y not affected by z

            if (xyzAspectRatio.isSelected()) { // update x and y accordingly
                if ( (source == textDimZ) || (preserveFOV2Button.isSelected())) {
                    dims[0] = (dims[0] - constantFOV) * factor + constantFOV;
                    dims[1] = (dims[1] - constantFOV) * factor + constantFOV;
                }

                if (preserveFOV2Button.isSelected()) {
                    resols[0] = resols[0] / factor;
                    resols[1] = resols[1] / factor;
                }
            }
        }

        setDimAndResXYZ();
    }

    /**
     * Accessor that returns the image.
     * 
     * @return The result image
     */
    public ModelImage getResultImage() {
        return resultImage;
    }

    /**
     * Method to handle item events.
     * 
     * @param event event that cause the method to fire
     */
    public void itemStateChanged(final ItemEvent event) {
        final Object source = event.getSource();

        if (source == image25DCheckbox) {

            if (image25DCheckbox.isSelected()) {
                do25D = true;
                comboBoxInterp.removeItemAt(1);
                comboBoxInterp.insertItemAt("Bilinear", 1);
                comboBoxInterp.setSelectedIndex(1); // bilinear

                if (userDefinedMatrix.isSelected()) {
                    labelTx.setEnabled(true);
                    labelTy.setEnabled(true);
                    labelRz.setEnabled(true);
                    textTx.setEnabled(true);
                    textTy.setEnabled(true);
                    textRz.setEnabled(true);
                    labelSx.setEnabled(true);
                    labelSy.setEnabled(true);
                    labelSKx.setEnabled(true);
                    textSx.setEnabled(true);
                    textSy.setEnabled(true);
                    textSKy.setEnabled(true);
                }

                labelTz.setEnabled(false);
                labelRx.setEnabled(false);
                labelRy.setEnabled(false);
                labelSz.setEnabled(false);
                labelSKz.setEnabled(false);
                textTz.setEnabled(false);
                textRx.setEnabled(false);
                textRy.setEnabled(false);
                textSz.setEnabled(false);
                textSKz.setEnabled(false);

                textResZ.setText(String.valueOf(image.getFileInfo()[0].getResolutions()[2]));
                textDimZ.setText(String.valueOf(image.getExtents()[2]));
                labelResZ.setEnabled(false);
                labelDimZ.setEnabled(false);
                textResZ.setEnabled(false);
                textDimZ.setEnabled(false);
            } else if (image.getNDims() >= 3) { // && image25D is not selected
                do25D = false;
                comboBoxInterp.removeItemAt(1);
                comboBoxInterp.insertItemAt("Trilinear", 1);
                comboBoxInterp.setSelectedIndex(1); // /trilinear

                if (userDefinedMatrix.isSelected()) {
                    labelTz.setEnabled(true);
                    labelRx.setEnabled(true);
                    labelRy.setEnabled(true);
                    textTz.setEnabled(true);
                    textRx.setEnabled(true);
                    textRy.setEnabled(true);
                    labelSz.setEnabled(true);
                    labelSKz.setEnabled(true);
                    textSz.setEnabled(true);
                    textSKz.setEnabled(true);
                }

                if (resampletoUser.isSelected()) {
                    labelResZ.setEnabled(true);
                    labelDimZ.setEnabled(true);
                    textResZ.setEnabled(true);
                    textDimZ.setEnabled(true);
                }
            } else if (image.getNDims() == 2) {
                comboBoxInterp.setSelectedIndex(1); // bilinear
            }

        }

        if (source == comboBoxInterp) {

            if (comboBoxInterp.getSelectedIndex() < 4) {
                clipCheckbox.setSelected(true);
                clipCheckbox.setEnabled(false);
            } else {
                clipCheckbox.setEnabled(true);
            }

            if (comboBoxInterp.getSelectedIndex() == 1) {
                // bilinear or trilinear interpolation
                constantFOVradio.setEnabled(true);
                endMatchFOVradio.setEnabled(true);
            } else {
                constantFOVradio.setEnabled(false);
                endMatchFOVradio.setEnabled(false);
                constantFOVradio.setSelected(true);
                endMatchFOVradio.setSelected(false);
                preserveFOVButton.setText("Set output dim to preserve input dim*resolution");
                preserveFOV2Button.setText("Set both dim and resolution at the same time to preserve dim*resolution");
            }

            if (userDefinedMatrix.isSelected()) {
                labelSx.setEnabled(true);
                labelSy.setEnabled(true);
                labelSKx.setEnabled(true);
                labelSKy.setEnabled(true);
                textSx.setEnabled(true);
                textSy.setEnabled(true);
                textSKx.setEnabled(true);
                textSKy.setEnabled(true);

                if ( (image.getNDims() >= 3) && (image25DCheckbox.isSelected() == false)) {
                    labelSz.setEnabled(true);
                    labelSKz.setEnabled(true);
                    textSz.setEnabled(true);
                    textSKz.setEnabled(true);
                }
            } // if (userDefinedMatrix.isSelected())

            resampletoUser.setEnabled(true);
            resampleSlider.setEnabled(true);
            comboBoxImage.setEnabled(true);
        }

        if (source == computeTImage) {

            if (computeTImage.isSelected()) {
                comboBoxTalTransform.setEnabled(true);
                labelTx.setEnabled(false);
                labelTy.setEnabled(false);
                labelTz.setEnabled(false);
                labelRx.setEnabled(false);
                labelRy.setEnabled(false);
                labelRz.setEnabled(false);
                labelSx.setEnabled(false);
                labelSy.setEnabled(false);
                labelSz.setEnabled(false);
                labelSKx.setEnabled(false);
                labelSKy.setEnabled(false);
                labelSKz.setEnabled(false);

                textTx.setEnabled(false);
                textTy.setEnabled(false);
                textTz.setEnabled(false);
                textRx.setEnabled(false);
                textRy.setEnabled(false);
                textRz.setEnabled(false);
                textSx.setEnabled(false);
                textSy.setEnabled(false);
                textSz.setEnabled(false);
                textSKx.setEnabled(false);
                textSKy.setEnabled(false);
                textSKz.setEnabled(false);

                rotCenter.setEnabled(false);
                rotOrigin.setEnabled(false);
                useSACenterBox.setEnabled(false);

                constantFOVradio.setEnabled(false);
                endMatchFOVradio.setEnabled(false);

                xyAspectRatio.setEnabled(false);
                xyzAspectRatio.setEnabled(false);
                preserveFOV2Button.setEnabled(false);
                separateDimResButton.setEnabled(false);
                enableDims(false);
                enableResols(false);
                maximum.setEnabled(false);
                minimum.setEnabled(false);
                current.setEnabled(false);
                magSlider.setEnabled(false);
                comboBoxImage.setEnabled(false);
                preserveFOVButton.setEnabled(false);
                preserveOutputDimsButton.setEnabled(false);

                cropRadio.setEnabled(false);
                padRadio.setEnabled(false);
                image25DCheckbox.setEnabled(false);
                updateOriginCheckbox.setEnabled(false);
                resampletoImage.setEnabled(false);
                resampletoUser.setEnabled(false);
                resampleSlider.setEnabled(false);
            } // if (computeTImage.isSelected())
            else { // computeTImage not selected
                comboBoxTalTransform.setEnabled(false);
                cropRadio.setEnabled(true);
                padRadio.setEnabled(true);
                image25DCheckbox.setEnabled(true);
                updateOriginCheckbox.setEnabled(true);
                resampletoImage.setEnabled(true);
                resampletoUser.setEnabled(true);
                resampleSlider.setEnabled(true);
            } // else computeTImage not selected
        } // if (source == computeTImage)

        if (source == userDefinedMatrix) {
            matrixFName.setText(" ");
            storedMatrixBox.setEnabled(false);
            if (userDefinedMatrix.isSelected()) {
                invertCheckbox.setSelected(false);
                invertCheckbox.setEnabled(false);
                labelTx.setEnabled(true);
                labelTy.setEnabled(true);
                labelRz.setEnabled(true);
                textTx.setEnabled(true);
                textTy.setEnabled(true);
                textRz.setEnabled(true);
                labelSx.setEnabled(true);
                labelSy.setEnabled(true);
                labelSKx.setEnabled(true);
                labelSKy.setEnabled(true);
                textSx.setEnabled(true);
                textSy.setEnabled(true);
                textSKx.setEnabled(true);
                textSKy.setEnabled(true);

                if ( (image.getNDims() >= 3) && (image25DCheckbox.isSelected() == false)) {
                    labelTz.setEnabled(true);
                    labelRx.setEnabled(true);
                    labelRy.setEnabled(true);
                    textTz.setEnabled(true);
                    textRx.setEnabled(true);
                    textRy.setEnabled(true);
                    labelSz.setEnabled(true);
                    labelSKz.setEnabled(true);
                    textSz.setEnabled(true);
                    textSKz.setEnabled(true);
                }
            } else {
                labelTx.setEnabled(false);
                labelTy.setEnabled(false);
                labelTz.setEnabled(false);
                labelRx.setEnabled(false);
                labelRy.setEnabled(false);
                labelRz.setEnabled(false);
                labelSx.setEnabled(false);
                labelSy.setEnabled(false);
                labelSz.setEnabled(false);
                labelSKx.setEnabled(false);
                labelSKy.setEnabled(false);
                labelSKz.setEnabled(false);

                textTx.setEnabled(false);
                textTy.setEnabled(false);
                textTz.setEnabled(false);
                textRx.setEnabled(false);
                textRy.setEnabled(false);
                textRz.setEnabled(false);
                textSx.setEnabled(false);
                textSy.setEnabled(false);
                textSz.setEnabled(false);
                textSKx.setEnabled(false);
                textSKy.setEnabled(false);
                textSKz.setEnabled(false);
            }
        } else if (source == fileMatrix) {

            if (fileMatrix.isSelected()) {
                invertCheckbox.setEnabled(true);
                matrixFile = matrixFileMenu();

                if ( (matrixFile == null) && (storedMatrixBox.getItemCount() > 0)) {
                    storedMatrix.setSelected(true);
                    storedMatrixBox.setEnabled(true);
                } else if (matrixFile == null) {
                    noTransform.setSelected(true);
                } else {
                    rotOrigin.setSelected(true);
                    rotCenter.setEnabled(true);
                    rotOrigin.setEnabled(true);
                }
            } else {
                rotCenter.setEnabled(true);
                rotOrigin.setEnabled(true);
            }
        } else if (source == storedMatrix) {
            matrixFName.setText(" ");

            if (storedMatrix.isSelected()) {
                invertCheckbox.setEnabled(true);
                storedMatrixBox.setEnabled(true);
            }
        } else if (source == noTransform) {

            matrixFName.setText(" ");
            storedMatrixBox.setEnabled(false);
            // tabbedPane.setEnabledAt(1, noTransform.isSelected());
            if (noTransform.isSelected()) {
                invertCheckbox.setSelected(false);
                invertCheckbox.setEnabled(false);
                rotCenter.setEnabled(false);
                rotOrigin.setEnabled(false);
                useSACenterBox.setEnabled(false);
                padRadio.setEnabled(false);
                cropRadio.setEnabled(false);
                constantFOVradio.setEnabled(false);
                endMatchFOVradio.setEnabled(false);
            } else {
                rotCenter.setEnabled(true);
                rotOrigin.setEnabled(true);
                useSACenterBox.setEnabled(true && enableSATransform);
                padRadio.setEnabled(true);
                cropRadio.setEnabled(true);
                final int boxIndex = comboBoxInterp.getSelectedIndex();
                if (boxIndex == 1) {
                    constantFOVradio.setEnabled(true);
                    endMatchFOVradio.setEnabled(true);
                } else {
                    constantFOVradio.setEnabled(false);
                    endMatchFOVradio.setEnabled(false);
                }
            }
        } else if ( (source == constantFOVradio) || (source == endMatchFOVradio)) {

            if (constantFOVradio.isSelected()) {
                constantFOV = 0;
                if (preserveFOVButton != null) {
                    preserveFOVButton.setText("Set output dim to preserve input dim*resolution");
                }
                if (preserveFOV2Button != null) {
                    preserveFOV2Button.setText("Set both dim and resolution at the same time to preserve dim*resolution");
                }
            } else {
                constantFOV = 1;
                if (preserveFOVButton != null) {
                    preserveFOVButton.setText("Set output dim to preserve input (dim-1)*resolution");
                }
                if (preserveFOV2Button != null) {
                    preserveFOV2Button.setText("Set both dim and resolution at the same time to preserve (dim-1)*resolution");
                }
            }
        } else if (source == resampletoUser) {

            if (resampletoUser.isSelected()) {
                xyAspectRatio.setEnabled(true);
                if ( (image.getNDims() > 2) && ( !image25DCheckbox.isSelected())) {
                    xyzAspectRatio.setEnabled(true);
                }
                preserveFOV2Button.setEnabled(true);
                separateDimResButton.setEnabled(true);
                enableDims(true);
                enableResols(true);
                preserveFOVButton.setEnabled(false);
                preserveOutputDimsButton.setEnabled(false);
                magSlider.setEnabled(false);
                maximum.setEnabled(false);
                minimum.setEnabled(false);
                current.setEnabled(false);
                setDefaultResampleToUser();
            } else {
                xyAspectRatio.setEnabled(false);
                xyzAspectRatio.setEnabled(false);
                preserveFOV2Button.setEnabled(false);
                separateDimResButton.setEnabled(false);
                enableDims(false);
                enableResols(false);
            }
        } else if (source == xyAspectRatio) {

            if (xyAspectRatio.isSelected()) {
                enableYSettings(false);
                xyzAspectRatio.setSelected(false);
            } else if ( !xyzAspectRatio.isSelected()) {
                enableYSettings(true);
            }
        } else if (source == xyzAspectRatio) {

            if (xyzAspectRatio.isSelected()) {
                enableYSettings(false);
                xyAspectRatio.setSelected(false);
            } else if ( !xyAspectRatio.isSelected()) {
                enableYSettings(true);
            }
        } else if (source == resampletoImage) {

            if (resampletoImage.isSelected()) {
                xyAspectRatio.setEnabled(false);
                xyzAspectRatio.setEnabled(false);
                preserveFOV2Button.setEnabled(false);
                separateDimResButton.setEnabled(false);
                maximum.setEnabled(false);
                minimum.setEnabled(false);
                current.setEnabled(false);
                magSlider.setEnabled(false);
                comboBoxImage.setEnabled(true);
                preserveFOVButton.setEnabled(true);
                preserveOutputDimsButton.setEnabled(true);
            } else {
                comboBoxImage.setEnabled(false);
            }
        } else if ((source == preserveFOVButton) || (source == preserveOutputDimsButton)){

            if (preserveFOVButton.isSelected()) {
                preserveFOV = true;
            } else {
                preserveFOV = false;
            }
        } else if (source == resampleSlider) {

            if (resampleSlider.isSelected()) {
                magSlider.setEnabled(true);
                maximum.setEnabled(true);
                minimum.setEnabled(true);
                current.setEnabled(true);
                preserveFOVButton.setEnabled(false);
                preserveOutputDimsButton.setEnabled(false);
            } else {
                magSlider.setEnabled(false);
                maximum.setEnabled(false);
                minimum.setEnabled(false);
                current.setEnabled(false);
            }
        } else if (source == comboBoxImage) {

            final String selectName = (String) (comboBoxImage.getSelectedItem());
            resampleImage = ViewUserInterface.getReference().getRegisteredImageByName(selectName);

            if (textResX != null) {
                textResX.setText(String.valueOf(resampleImage.getFileInfo(0).getResolutions()[0]));
                textResY.setText(String.valueOf(resampleImage.getFileInfo(0).getResolutions()[1]));
                textDimX.setText(String.valueOf(Math.round(resampleImage.getExtents()[0])));
                textDimY.setText(String.valueOf(Math.round(resampleImage.getExtents()[1])));

                if ( (image.getNDims() > 2) && ( !image25DCheckbox.isSelected()) && (resampleImage.getNDims() > 2)) {
                    textResZ.setText(String.valueOf(resampleImage.getFileInfo(0).getResolutions()[2]));
                    textDimZ.setText(String.valueOf(Math.round(resampleImage.getExtents()[2])));
                }
            }
        } else if (source == rotCenter) {
            useSACenterBox.setEnabled(rotCenter.isSelected() && enableSATransform);
        } else if (source == rotOrigin) {
            useSACenterBox.setEnabled(rotCenter.isSelected() && enableSATransform);
        } else if (source == outOfBoundsComboBox) {
            switch (outOfBoundsComboBox.getSelectedIndex()) {
                case 0: // image minimum
                    valueText.setText(String.valueOf(imageMin));
                    valueText.setEnabled(false);
                    break;
                case 1: // If float NaN, else 0
                    if ( (dataType == ModelStorageBase.FLOAT) || (dataType == ModelStorageBase.DOUBLE)
                            || (dataType == ModelStorageBase.ARGB_FLOAT)) {
                        valueText.setText(String.valueOf(Float.NaN));
                    } else {
                        valueText.setText(String.valueOf(0));
                    }
                    valueText.setEnabled(false);
                    break;
                case 2: // User defined;
                    valueText.setEnabled(true);
                    break;
                case 3: // Image maximum
                    valueText.setText(String.valueOf(imageMax));
                    valueText.setEnabled(false);
                    break;
            } // switch (outOfBoundsComboBox.getSelectedIndex())
        } // else if (event.getSource() == outOfBoundsComboBox)
    }

    /**
     * Allows the user to select matrix file.
     * 
     * @return fileName
     */
    public String matrixFileMenu() {
        String fileName, directory;
        JFileChooser chooser;
        fileName = null;

        // bring up file dialog
        try {
            chooser = new JFileChooser();

            if (ViewUserInterface.getReference().getDefaultDirectory() != null) {
                chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
            } else {
                chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MATRIX));

            final int returnVal = chooser.showOpenDialog(ViewUserInterface.getReference().getMainFrame());

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                ViewUserInterface.getReference().setDefaultDirectory(directory);
                matrixFName.setText(fileName);
            } else {
                return null;
            }
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: JDialogScriptableTransform.displayMatrixFileMenu");

            return null;
        }

        readTransformMatrixFile(fileName);

        return fileName;
    }

    /**
     * Reads a matrix from a file.
     * 
     * @param fileName name of the matrix file.
     */
    public TransMatrix readTransformMatrixFile(final String fileName) {
    	
        final TransMatrix matrix = new TransMatrix(image.getNDims() + 1);
        matrix.MakeIdentity();

        if (fileName == null) {
            MipavUtil.displayError("filename = null");
        }

        try {
            // search for file name relative to image first, then relative to MIPAV default, then absolute path
            File file;
            file = new File(image.getImageDirectory() + fileName);
            if ( !file.exists()) {
                file = new File(ViewUserInterface.getReference().getDefaultDirectory() + fileName);
            }
            if ( !file.exists()) {
                file = new File(fileName);
            }

            final RandomAccessFile raFile = new RandomAccessFile(file, "r");
            
            final String extension = FileUtility.getExtension(file.getAbsolutePath()).toLowerCase();
            if (extension.equals(".tps")) {
                spline = new AlgorithmTPSpline(image);
                spline.readMatrix(raFile);
                raFile.close();
            } else {
                spline = null;
                matrix.readMatrix(raFile, false);
                raFile.close();
                fileTransMatrix = matrix;
            }

            // We don't know the coordinate system that the transformation represents. Therefore
            // bring up a dialog where the user can ID the coordinate system changes (i.e.
            // world coordinate and/or the "left-hand" coordinate system!
            // new JDialogOrientMatrix(parentFrame, (JDialogBase) this);
        } catch (final IOException error) {
            MipavUtil.displayError("Matrix read error");
            fileTransMatrix.MakeIdentity();
        }
        
        return fileTransMatrix;
    }

    /**
     * Accessor that sets value for the preserveFOV boolean
     * 
     * @param flag <code>true</code> indicates that output dim should be set from input dim*res or
     * input (dim-1)*res.  flag <code>false</code> indicates that the output dim should be set
     * from the selected image output dim.
     */
    public void setPreserveFOV(final boolean flag) {
        preserveFOV = flag;
    }

    /**
     * Accessor that sets the clip flag.
     * 
     * @param flag <code>true</code> indicates clip image, <code>false</code> otherwise.
     */
    public void setClipFlag(final boolean flag) {
        doClip = flag;
    }

    /**
     * Accessor that sets the boolean for invert matrix.
     * 
     * @param flag <code>true</code> indicates invert matrix, <code>false</code> otherwise.
     */
    public void setDoInvMat(final boolean flag) {
        doInvMat = flag;
    }

    /**
     * Resets the dimension and resolution fields for resampling panel. Called by focusLost.
     * 
     */
    public void setDimAndResXYZ() {
        int[] iDims;
        int ndim;

        if (image.getNDims() >= 3) {
            ndim = 3;
        } else {
            ndim = 2;
        }

        iDims = new int[ndim];

        for (int i = 0; i < ndim; i++) {
            iDims[i] = Math.round(dims[i]);
        }

        textDimX.setText(String.valueOf(iDims[0]));
        textResX.setText(String.valueOf(resols[0]));
        textDimY.setText(String.valueOf(iDims[1]));
        textResY.setText(String.valueOf(resols[1]));

        if (image.getNDims() >= 3) {
            textDimZ.setText(String.valueOf(iDims[2]));
            textResZ.setText(String.valueOf(resols[2]));
        }

        cXdim = Math.round(dims[0]);
        cXres = resols[0];
        cYdim = Math.round(dims[1]);
        cYres = resols[1];
        cZdim = Math.round(dims[2]);
        cZres = resols[2];
        
        
    }

    /**
     * Accessor that sets the boolean for doing a Talairach type transformation.
     * 
     * @param doTalairach boolean
     */
    public void setDoTalairach(final boolean doTalairach) {
        this.doTalairach = doTalairach;
    }

    /**
     * Accessor that sets the slicing flag.
     * 
     * @param flag <code>true</code> indicates slices should be blurred independently.
     */
    public void setImage25D(final boolean flag) {
        do25D = flag;
    }

    /**
     * Accessor that sets the interpolation method.
     * 
     * @param interp DOCUMENT ME!
     */
    public void setInterp(final int interp) {
        this.interp = interp;
    }

    /**
     * Sets the left-hand coordinate flag. If true, change matrix to the left-hand coordinate system.
     * 
     * @param leftHandSys DOCUMENT ME!
     */
    public void setLeftHandSystem(final boolean leftHandSys) {
        leftHandSystem = leftHandSys;
    }

    /**
     * Accessor that sets the transformation matrix.
     * 
     * @param matrix The transformation matrix.
     */
    public void setMatrix(final TransMatrix matrix) {
        xfrm = matrix;
    }

    /**
     * Accessor to set the output image's dimensions.
     * 
     * @param outDim Array of the dimensions.
     */
    public void setOutDimensions(final int[] outDim) {
        oXdim = outDim[0];
        oYdim = outDim[1];

        if ( (image.getNDims() >= 3) && !do25D) {
            oZdim = outDim[2];
        }
    }

    /**
     * Accessor to set the output image's resolutions.
     * 
     * @param outRes Array of the resolutions.
     */
    public void setOutResolutions(final float[] outRes) {
        oXres = outRes[0];
        oYres = outRes[1];

        if ( (image.getNDims() >= 3) && !do25D) {
            oZres = outRes[2];
        }
    }

    /**
     * Accessor that sets the padding flag.
     * 
     * @param flag <code>true</code> indicates slices should be blurred independently.
     */
    public void setPadFlag(final boolean flag) {
        doPad = flag;
    }

    /**
     * Accessor that sets the fillValue.
     * 
     * @param fillValue DOCUMENT ME!
     */
    public void setFillValue(final float fillValue) {
        this.fillValue = fillValue;
    }

    /**
     * Accessor that sets the type of Talairach transformation.
     * 
     * @param transformType int
     */
    public void setTransformType(final int transformType) {
        this.transformType = transformType;
    }
    
    /**
     * Accessor that sets the units.
     * 
     * @param units of image
     */
    public void setUnits(final int[] units) {
        this.units = new int[units.length];
        for(int i=0; i<units.length; i++) {
            this.units[i] = units[i];
        }
    }

    /**
     * Accessor that sets the update origin flag.
     * 
     * @param flag <code>true</code> indicates to update the image origin using the transformation matrix.
     */
    public void setUpdateOrigin(final boolean flag) {
        doUpdateOrigin = flag;
    }

    /**
     * Accessor that sets the voi flag.
     * 
     * @param flag <code>true</code> indicates transform VOI, <code>false</code> otherwise.
     */
    public void setVOIFlag(final boolean flag) {
        doVOI = flag;
    }

    /**
     * Sets the world coordinate flag. If true, change matrix to the world coordinate system.
     * 
     * @param wcSys DOCUMENT ME!
     */
    public void setWCSystem(final boolean wcSys) {
        wcSystem = wcSys;
    }

    /**
     * Sets values based on knob along slider.
     * 
     * @param event ChangeEvent event that triggered this function
     */
    public void stateChanged(final ChangeEvent event) {
        final Object source = event.getSource();
        float factor;

        if (source == magSlider) {
            factor = magSlider.getValue() / (float) 100;
            current.setText(String.valueOf(factor));
            textDimX.setText(String.valueOf(Math.round(factor * resampleImage.getExtents()[0])));
            textResY.setText(String.valueOf(resampleImage.getFileInfo(0).getResolutions()[1] / factor));
            textDimY.setText(String.valueOf(Math.round(factor * resampleImage.getExtents()[1])));
            textResX.setText(String.valueOf(resampleImage.getFileInfo(0).getResolutions()[0] / factor));

            if ( (image.getNDims() >= 3) && ( !image25DCheckbox.isSelected())) {
                textResZ.setText(String.valueOf(resampleImage.getFileInfo(0).getResolutions()[2] / factor));
                textDimZ.setText(String.valueOf(Math.round(factor * resampleImage.getExtents()[2])));
            }
        }
    }

    /**
     * Calls the algorithm with the set variables.
     */
    protected void callAlgorithm() {
        Vector3f center = null;

        if (doInvMat) {
            xfrm.Inverse();
        }
        // if ((invertCheckbox != null) && (invertCheckbox.isSelected())) {
        // xfrm.invert();
        // }

        // System.err.println("matrix: " + xfrm);
        // System.err.println("matrix inverse: " + xfrm.inverse());

        // Hide dialog
        setVisible(false);
        if (doTalairach) {
            callTalAlgorithm();

            return;
        }

        if (spline != null) {
            spline.run();
            resultImage = spline.getResultImage();

            if (resultImage != null) {

                try {

                    // resultImage.setImageName("Transformed image");
                    new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
                } catch (final OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            }
            return;
        }

        if (doRotateCenter) {
            Preferences.debug("useSACenter = " + useSACenter + "\n", Preferences.DEBUG_SCRIPTING);
            center = resampleImage.getImageCentermm(useSACenter);
        }

        if ( (image.getNDims() == 2) || (do25D)) {
            Preferences.debug("oXres, oYres = " + oXres + ", " + oYres, Preferences.DEBUG_SCRIPTING);
            Preferences.debug(" oXdim, oYdim = " + oXdim + ", " + oYdim + "\n", Preferences.DEBUG_SCRIPTING);
            Preferences.debug("xfrm = " + xfrm, Preferences.DEBUG_SCRIPTING);
            algoTrans = new AlgorithmTransform(image, xfrm, interp, oXres, oYres, oXdim, oYdim, units, doVOI, doClip,
                    doPad, doRotateCenter, center);
            algoTrans.setFillValue(fillValue);
            algoTrans.setUpdateOriginFlag(doUpdateOrigin);
        } else { // ((image.getNDims() >= 3) && (!do25D))
            algoTrans = new AlgorithmTransform(image, xfrm, interp, oXres, oYres, oZres, oXdim, oYdim, oZdim, units,
                    doVOI, doClip, doPad, doRotateCenter, center);
            algoTrans.setFillValue(fillValue);
            algoTrans.setUpdateOriginFlag(doUpdateOrigin);
            algoTrans.setUseScannerAnatomical(isSATransform);
        }

        // This is very important. Adding this object as a listener allows
        // the algorithm to notify this object when it has completed of failed.
        // See algorithm performed event. This is made possible by implementing
        algoTrans.addListener(this);

        createProgressBar(image.getImageName(), algoTrans);

        // Start the thread as a low priority because we wish to still have
        // user interface work fast

        if (isRunInSeparateThread()) {

            if (algoTrans.startMethod(Thread.MIN_PRIORITY) == false) {
                MipavUtil.displayError("A thread is already running on this object");
            }
        } else {
            algoTrans.run();
        }
    }

    /**
     * {@inheritDoc}
     */
    protected void doPostAlgorithmActions() {
        AlgorithmParameters.storeImageInRunner(resultImage);
    }

    /**
     * {@inheritDoc}
     */
    protected void setGUIFromParams() {
        image = scriptParameters.retrieveInputImage();
        image.calcMinMax();
        imageMin = image.getMin();
        imageMax = image.getMax();
        dataType = image.getFileInfo()[0].getDataType();
        image.makeUnitsOfMeasureIdentical();
        resampleImage = image;
        userInterface = ViewUserInterface.getReference();
        parentFrame = image.getParentFrame();

        interp = scriptParameters.getParams().getInt("interpolation_type");
        constantFOV = scriptParameters.getParams().getInt("constant_fov");
        doVOI = scriptParameters.getParams().getBoolean("do_transform_VOIs");
        doClip = scriptParameters.getParams().getBoolean("do_clip_output");
        doRotateCenter = scriptParameters.getParams().getBoolean("do_rotate_about_center");
        doTalairach = scriptParameters.getParams().getBoolean("do_talairach_transform");
        useSACenter = scriptParameters.getParams().getBoolean("use_scanner_center");
        doInvMat = scriptParameters.getParams().getBoolean("do_invert_matrix");

        if (doTalairach) {
            tInfo = new TalairachTransformInfo();
            tInfo.isAcpc(true);

            transformType = scriptParameters.getParams().getInt("transform_type");

            float[] tempArray = scriptParameters.getParams().getList("acpc_PC").getAsFloatArray();
            final Vector3f acpcPC = new Vector3f(tempArray[0], tempArray[1], tempArray[2]);
            tInfo.setAcpcPC(acpcPC);

            tInfo.setAcpcRes(scriptParameters.getParams().getFloat("acpc_res"));

            tempArray = scriptParameters.getParams().getList("orig_AC").getAsFloatArray();

            final Vector3f origAC = new Vector3f(tempArray[0], tempArray[1], tempArray[2]);
            tInfo.setOrigAC(origAC);

            tempArray = scriptParameters.getParams().getList("orig_PC").getAsFloatArray();

            final Vector3f origPC = new Vector3f(tempArray[0], tempArray[1], tempArray[2]);
            tInfo.setOrigPC(origPC);

            tInfo.setOrigRes(scriptParameters.getParams().getList("orig_res").getAsFloatArray());
            tInfo.setOrigDim(scriptParameters.getParams().getList("orig_dim").getAsIntArray());
            tInfo.setOrigOrigin(scriptParameters.getParams().getList("orig_origin").getAsFloatArray());

            final float[][] origOrient = new float[3][3];

            for (int i = 0; i < 3; i++) {
                origOrient[i] = scriptParameters.getParams().getList("orig_orient_" + i).getAsFloatArray();
            }

            tInfo.setOrigOrient(origOrient);

            if ( (transformType == JDialogScriptableTransform.ORIG_TO_TLRC)
                    || (transformType == JDialogScriptableTransform.ACPC_TO_TLRC)
                    || (transformType == JDialogScriptableTransform.TLRC_TO_ORIG)
                    || (transformType == JDialogScriptableTransform.TLRC_TO_ACPC)) {
                tempArray = scriptParameters.getParams().getList("acpc_min").getAsFloatArray();

                final Vector3f acpcMin = new Vector3f(tempArray[0], tempArray[1], tempArray[2]);
                tInfo.setAcpcMin(acpcMin);

                tempArray = scriptParameters.getParams().getList("acpc_max").getAsFloatArray();

                final Vector3f acpcMax = new Vector3f(tempArray[0], tempArray[1], tempArray[2]);
                tInfo.setAcpcMax(acpcMax);

                tInfo.setTlrcRes(scriptParameters.getParams().getList("tlrc_res").getAsFloatArray());
            } // do Talairach Transformation
        } // if doTalairach
        else { // not Talairach

            TransMatrix transMat = null;

            do25D = scriptParameters.doProcess3DAs25D();
            doUpdateOrigin = scriptParameters.getParams().getBoolean("do_update_origin");
            doPad = scriptParameters.getParams().getBoolean("do_pad");
            outOfBoundsIndex = scriptParameters.getParams().getInt("out_of_bounds_index");
            switch (outOfBoundsIndex) {
                case 0:
                    fillValue = (float) imageMin;
                    break;
                case 1:
                    if ( (dataType == ModelStorageBase.FLOAT) || (dataType == ModelStorageBase.DOUBLE)
                            || (dataType == ModelStorageBase.ARGB_FLOAT)) {
                        fillValue = Float.NaN;
                    } else {
                        fillValue = 0.0f;
                    }
                    break;
                case 2:
                    fillValue = scriptParameters.getParams().getFloat("fill_value");
                    break;
                case 3:
                    fillValue = (float) imageMax;
                    break;
            }

            // boolean useImageMatrix = scriptParameters.getParams().getBoolean("use_image_matrix");
            boolean useImageMatrix = false;

            if ( (image.getNDims() == 2) || do25D) {
                transMat = new TransMatrix(3);

                final float[] outputRes = scriptParameters.getParams().getList("output_res").getAsFloatArray();
                oXres = outputRes[0];
                oYres = outputRes[1];

                final int[] outputDim = scriptParameters.getParams().getList("output_dim").getAsIntArray();
                oXdim = outputDim[0];
                oYdim = outputDim[1];

                if ( !useImageMatrix) {
                    final double[][] xMat = new double[3][3];

                    for (int i = 0; i < 3; i++) {
                        xMat[i] = scriptParameters.getParams().getList("x_mat" + i).getAsDoubleArray();
                    }

                    transMat.copyMatrix(xMat);
                    xfrm = transMat;
                }
            } else {
                transMat = new TransMatrix(4);

                // get the resampling information
                final String resampleType = scriptParameters.getParams().getString("resample_type");
                if (resampleType.equals("none")) {
                    oXres = image.getFileInfo()[0].getResolutions()[0];
                    oYres = image.getFileInfo()[0].getResolutions()[1];

                    oXdim = image.getExtents()[0];
                    oYdim = image.getExtents()[1];

                    if (image.getNDims() > 2) {
                        oZres = image.getFileInfo()[0].getResolutions()[2];
                        oZdim = image.getExtents()[2];
                    }
                } else if (resampleType.equals("to_image")) {
                    final ModelImage selectedImg = scriptParameters.retrieveInputImage(2);

                    // assign output resolutions and dims to those of image selected in comboBox
                    oXres = selectedImg.getFileInfo(0).getResolutions()[0];
                    oYres = selectedImg.getFileInfo(0).getResolutions()[1];

                    /*
                     * //get the units now units[0] = selectedImg.getUnitsOfMeasure(0); units[1] =
                     * selectedImg.getUnitsOfMeasure(1);
                     * 
                     * System.err.println("units of original image: " + image.getUnitsOfMeasure(0));
                     * System.err.println("setting units: " + units[0]);
                     */

                    preserveFOV = scriptParameters.getParams().getBoolean("preserve_fov");

                    final float iXres = image.getFileInfo(0).getResolutions()[0];
                    final float iYres = image.getFileInfo(0).getResolutions()[1];
                    final int iXdim = image.getExtents()[0];
                    final int iYdim = image.getExtents()[1];

                    int iZdim = 1;
                    float iZres = 1.f;
                    if (image.getNDims() >= 3) {
                        iZdim = image.getExtents()[2];
                        iZres = image.getFileInfo(0).getResolutions()[2];
                    }

                    if (preserveFOV) {
                        final float fovX = iXres * (iXdim - constantFOV);
                        final float fovY = iYres * (iYdim - constantFOV);
                        oXdim = Math.round(fovX / oXres) + constantFOV;
                        oYdim = Math.round(fovY / oYres) + constantFOV;
                    } else {
                        oXdim = selectedImg.getExtents()[0];
                        oYdim = selectedImg.getExtents()[1];
                    }

                    if ( (image.getNDims() >= 3) && ( !do25D)) {
                        oZres = selectedImg.getFileInfo(0).getResolutions()[2];
                        // units[2] = selectedImg.getUnitsOfMeasure(2);
                        if (preserveFOV) {
                            final float fovZ = iZres * (iZdim - constantFOV);
                            oZdim = Math.round(fovZ / oZres) + constantFOV;

                        } else {
                            oZdim = selectedImg.getExtents()[2];
                        }
                    } else if ( (image.getNDims() >= 3) && (do25D)) { // cannot change third dimension
                        oZres = image.getFileInfo(0).getResolutions()[2];
                        oZdim = image.getExtents()[2];
                    }
                } else if (resampleType.equals("to_user")) {
                    final float[] outputRes = scriptParameters.getParams().getList("output_res").getAsFloatArray();
                    oXres = outputRes[0];
                    oYres = outputRes[1];
                    oZres = outputRes[2];

                    final int[] outputDim = scriptParameters.getParams().getList("output_dim").getAsIntArray();
                    oXdim = outputDim[0];
                    oYdim = outputDim[1];
                    oZdim = outputDim[2];
                } else if (resampleType.equals("to_factor")) {
                    final float factor = scriptParameters.getParams().getFloat("resample_factor");
                    oXdim = Math.round(image.getExtents()[0] * factor);
                    oYdim = Math.round(image.getExtents()[1] * factor);
                    oXres = image.getFileInfo(0).getResolutions()[0] * (image.getExtents()[0] - 1) / (oXdim - 1);
                    oYres = image.getFileInfo(0).getResolutions()[1] * (image.getExtents()[1] - 1) / (oYdim - 1);

                    if ( (image.getNDims() >= 3) && ( !do25D)) {
                        oZdim = Math.round(image.getExtents()[2] * factor);
                        oZres = image.getFileInfo(0).getResolutions()[2] * (image.getExtents()[2] - 1) / (oZdim - 1);
                    } else if ( (image.getNDims() >= 3) && (do25D)) { // cannot change third dimension
                        oZdim = image.getExtents()[2];
                        oZres = image.getFileInfo(0).getResolutions()[2];
                    }
                }
                // set the transform information
                final String transformSource = scriptParameters.getParams().getString("transform_source");
                if (transformSource.equals("file")) {
                    final String matrixFile = scriptParameters.getParams().getString("transform_file");
                    readTransformMatrixFile(matrixFile);
                    transMat.MakeIdentity();
                    transMat.Mult(fileTransMatrix);
                    xfrm = transMat;
                } else if (transformSource.equals("user")) {
                    final double[][] xMat = new double[4][4];

                    for (int i = 0; i < 4; i++) {
                        xMat[i] = scriptParameters.getParams().getList("x_mat" + i).getAsDoubleArray();
                    }
                    transMat.copyMatrix(xMat);
                    xfrm = transMat;
                } else if (transformSource.equals("self")) {
                    transMat.MakeIdentity();
                    xfrm = transMat;
                    if (image.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL)) {
                        isSATransform = true;
                    }

                    final TransMatrix imageMatrix = image.getMatrix();

                    xfrm.Mult(imageMatrix);
                } else if (transformSource.equals("none")) {
                    transMat.MakeIdentity();
                    xfrm = transMat;
                }
            }
        } // else not Talairach

    }

    /**
     * {@inheritDoc}
     */
    protected void storeParamsFromGUI() throws ParserException {
        scriptParameters.storeInputImage(image);
        scriptParameters.storeImageInRecorder(resultImage);

        scriptParameters.getParams().put(ParameterFactory.newParameter("interpolation_type", interp));
        scriptParameters.getParams().put(ParameterFactory.newParameter("constant_fov", constantFOV));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_transform_VOIs", doVOI));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_clip_output", doClip));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_rotate_about_center", doRotateCenter));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_talairach_transform", doTalairach));
        scriptParameters.getParams().put(ParameterFactory.newParameter("use_scanner_center", this.useSACenter));
        scriptParameters.getParams().put(ParameterFactory.newParameter("do_invert_matrix", doInvMat));

        if (doTalairach) {
            scriptParameters.getParams().put(ParameterFactory.newParameter("transform_type", transformType));

            final Vector3f acpcPC = tInfo.getAcpcPC();
            scriptParameters.getParams().put(
                    ParameterFactory.newParameter("acpc_PC", new float[] {acpcPC.X, acpcPC.Y, acpcPC.Z}));
            scriptParameters.getParams().put(ParameterFactory.newParameter("acpc_res", tInfo.getAcpcRes()));

            final Vector3f origAC = tInfo.getOrigAC();
            scriptParameters.getParams().put(
                    ParameterFactory.newParameter("orig_AC", new float[] {origAC.X, origAC.Y, origAC.Z}));

            final Vector3f origPC = tInfo.getOrigPC();
            scriptParameters.getParams().put(
                    ParameterFactory.newParameter("orig_PC", new float[] {origPC.X, origPC.Y, origPC.Z}));

            scriptParameters.getParams().put(ParameterFactory.newParameter("orig_res", tInfo.getOrigRes()));
            scriptParameters.getParams().put(ParameterFactory.newParameter("orig_dim", tInfo.getOrigDim()));
            scriptParameters.getParams().put(ParameterFactory.newParameter("orig_origin", tInfo.getOrigOrigin()));

            final float[][] origOrient = tInfo.getOrigOrient();

            for (int i = 0; i < 3; i++) {
                scriptParameters.getParams().put(ParameterFactory.newParameter("orig_orient_" + i, origOrient[i]));
            }

            if ( (transformType == JDialogScriptableTransform.ORIG_TO_TLRC)
                    || (transformType == JDialogScriptableTransform.ACPC_TO_TLRC)
                    || (transformType == JDialogScriptableTransform.TLRC_TO_ORIG)
                    || (transformType == JDialogScriptableTransform.TLRC_TO_ACPC)) {
                final Vector3f acpcMin = tInfo.getAcpcMin();

                scriptParameters.getParams().put(
                        ParameterFactory.newParameter("acpc_min", new float[] {acpcMin.X, acpcMin.Y, acpcMin.Z}));

                final Vector3f acpcMax = tInfo.getAcpcMax();
                scriptParameters.getParams().put(
                        ParameterFactory.newParameter("acpc_max", new float[] {acpcMax.X, acpcMax.Y, acpcMax.Z}));

                scriptParameters.getParams().put(ParameterFactory.newParameter("tlrc_res", tInfo.getTlrcRes()));

            } // do Talairach Transformation
        } // if doTalairach
        else { // not Talairach

            scriptParameters.storeProcess3DAs25D(do25D);
            scriptParameters.getParams().put(ParameterFactory.newParameter("do_update_origin", doUpdateOrigin));
            scriptParameters.getParams().put(ParameterFactory.newParameter("do_pad", doPad));
            scriptParameters.getParams().put(ParameterFactory.newParameter("out_of_bounds_index", outOfBoundsIndex));
            scriptParameters.getParams().put(ParameterFactory.newParameter("fill_value", fillValue));

            if ( (image.getNDims() == 2) || do25D) {
                scriptParameters.getParams().put(
                        ParameterFactory.newParameter("output_res", new float[] {oXres, oYres}));
                scriptParameters.getParams().put(ParameterFactory.newParameter("output_dim", new int[] {oXdim, oYdim}));

                final double[] xCol = new double[3];

                for (int i = 0; i < 3; i++) {
                    xfrm.getColumn(i, xCol);
                    scriptParameters.getParams().put(ParameterFactory.newParameter("x_mat" + i, xCol));
                }

            } else {
                /*
                 * scriptParameters.getParams().put(ParameterFactory.newParameter("output_res", new float[] { oXres,
                 * oYres, oZres })); scriptParameters.getParams().put(ParameterFactory.newParameter("output_dim", new
                 * int[] { oXdim, oYdim, oZdim }));
                 * 
                 * //if this is set to use image's associated matrices, then do not store matrix
                 * 
                 * scriptParameters.getParams().put(ParameterFactory.newParameter("use_image_matrix",
                 * storedMatrix.isSelected()));
                 * 
                 * if (!storedMatrix.isSelected()) { for (int i = 0; i < 4; i++) {
                 * scriptParameters.getParams().put(ParameterFactory.newParameter("x_mat" + i, xMat[i])); } }
                 */
                // different possibilities for the transform source
                String transformSource;
                if (fileMatrix.isSelected()) {
                    transformSource = "file";
                    scriptParameters.getParams()
                            .put(ParameterFactory.newParameter("transform_source", transformSource));
                    scriptParameters.getParams().put(ParameterFactory.newParameter("transform_file", matrixFile));
                } else if (userDefinedMatrix.isSelected()) {
                    transformSource = "user";
                    scriptParameters.getParams()
                            .put(ParameterFactory.newParameter("transform_source", transformSource));
                    final double[] xCol = new double[4];
                    for (int i = 0; i < 4; i++) {
                        xfrm.getColumn(i, xCol);
                        scriptParameters.getParams().put(ParameterFactory.newParameter("x_mat" + i, xCol));
                    }
                } else if (storedMatrix.isSelected()) {
                    transformSource = "self";
                    scriptParameters.getParams()
                            .put(ParameterFactory.newParameter("transform_source", transformSource));
                } else {
                    transformSource = "none";
                    scriptParameters.getParams()
                            .put(ParameterFactory.newParameter("transform_source", transformSource));
                }

                // different possibilities for the resampling parameters
                String resampleType;
                if (resampletoImage.isSelected()) {
                    resampleType = "to_image";
                    scriptParameters.getParams().put(ParameterFactory.newParameter("resample_type", resampleType));

                    final String selectName = (String) (comboBoxImage.getSelectedItem());
                    final ModelImage selectedImg = userInterface.getRegisteredImageByName(selectName);

                    scriptParameters.storeInputImage(selectedImg);

                    scriptParameters.getParams().put(ParameterFactory.newParameter("preserve_fov", preserveFOV));

                } else if (resampletoUser.isSelected()) {
                    resampleType = "to_user";
                    scriptParameters.getParams().put(ParameterFactory.newParameter("resample_type", resampleType));

                    scriptParameters.getParams().put(
                            ParameterFactory.newParameter("output_res", new float[] {oXres, oYres, oZres}));
                    scriptParameters.getParams().put(
                            ParameterFactory.newParameter("output_dim", new int[] {oXdim, oYdim, oZdim}));
                } else if (resampleSlider.isSelected()) {
                    resampleType = "to_factor";
                    scriptParameters.getParams().put(ParameterFactory.newParameter("resample_type", resampleType));

                    final float factor = magSlider.getValue() / (float) 100;
                    scriptParameters.getParams().put(ParameterFactory.newParameter("resample_factor", factor));
                } else {
                    // default: keep image dimensions,resolutions
                    resampleType = "none";
                    scriptParameters.getParams().put(ParameterFactory.newParameter("resample_type", resampleType));
                }

            }
        } // else not Talairach
    }

    /**
     * Builds a list of images to register to the template image.
     */
    private void buildComboBox() {
        ModelImage img;
        comboBoxImage = new JComboBox();
        comboBoxImage.setFont(serif12);
        comboBoxImage.setBackground(Color.white);
        comboBoxImage.addItemListener(this);

        if ( (image.getNDims() == 2) || (image.getNDims() >= 3)) {
            comboBoxImage.addItem(image.getImageName()); // add its own name first.

            final Enumeration<String> names = ViewUserInterface.getReference().getRegisteredImageNames();

            while (names.hasMoreElements()) {
                final String name = names.nextElement();
                img = ViewUserInterface.getReference().getRegisteredImageByName(name);

                if (ViewUserInterface.getReference().getFrameContainingImage(img) != null) {

                    if ( !image.getImageName().equals(name)) {
                        comboBoxImage.addItem(name);
                    }
                }
            }
        }
    }

    /**
     * Builds the matrixPanel.
     * 
     * @return The matrix panel.
     */
    private JPanel buildMatrixPanel() {

        final JPanel matrixPanel = new JPanel();
        matrixPanel.setBorder(buildTitledBorder("Transform"));
        matrixPanel.setLayout(new BoxLayout(matrixPanel, BoxLayout.Y_AXIS));
        matrixPanel.setForeground(Color.black);

        matrixDeterminationGroup = new ButtonGroup();
        noTransform = new JRadioButton("No Transformation", true);
        noTransform.setFont(serif12);
        noTransform.setEnabled(true);
        matrixDeterminationGroup.add(noTransform);
        noTransform.setAlignmentX(Component.LEFT_ALIGNMENT);
        matrixPanel.add(noTransform);
        noTransform.addItemListener(this);

        tInfo = image.getTalairachTransformInfo();

        if (tInfo != null) {
            final JPanel talPanel = new JPanel(new GridBagLayout());
            final GridBagConstraints gbc = new GridBagConstraints();
            gbc.gridwidth = 1;
            gbc.gridheight = 1;
            gbc.anchor = GridBagConstraints.WEST;
            gbc.weightx = 1;
            gbc.insets = new Insets(0, 0, 0, 0);
            gbc.fill = GridBagConstraints.HORIZONTAL;
            gbc.gridx = 0;
            gbc.gridy = 0;

            computeTImage = new JRadioButton("Talairach space computation:", false);
            computeTImage.setFont(serif12);
            computeTImage.setEnabled(true);
            matrixDeterminationGroup.add(computeTImage);
            talPanel.add(computeTImage, gbc);
            computeTImage.addItemListener(this);

            tVal = new String[6];
            tVal[0] = "orig to acpc";
            tVal[1] = "orig to Tlrc";
            tVal[2] = "acpc to Tlrc";
            tVal[3] = "Tlrc to acpc";
            tVal[4] = "Tlrc to orig";
            tVal[5] = "acpc to orig";
            comboBoxTalTransform = new JComboBox(tVal);
            comboBoxTalTransform.setFont(serif12);
            comboBoxTalTransform.setEnabled(false);
            gbc.gridx = 1;
            gbc.gridy = 0;
            gbc.weightx = 10;
            talPanel.add(comboBoxTalTransform, gbc);

            matrixPanel.add(talPanel);
            talPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
        } // if (tInfo != null)

        final JPanel imageMatrixPanel = new JPanel(new BorderLayout());
        imageMatrixPanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        storedMatrix = new JRadioButton("Use image's associated matrix", false);
        storedMatrix.setFont(serif12);
        storedMatrix.setEnabled(true);
        matrixDeterminationGroup.add(storedMatrix);
        storedMatrix.setAlignmentX(Component.LEFT_ALIGNMENT);
        imageMatrixPanel.add(storedMatrix, BorderLayout.WEST);
        storedMatrix.addItemListener(this);

        storedMatrixBox = new JComboBox();
        storedMatrixBox.setFont(MipavUtil.font12);
        storedMatrixBox.setAlignmentX(Component.LEFT_ALIGNMENT);
        storedMatrixBox.setEnabled(false);
        final MatrixHolder mHolder = image.getMatrixHolder();
        final Set<String> matrixKeys = mHolder.getMatrixMap().keySet();
        final Iterator<String> iter = matrixKeys.iterator();

        // storedMatrixBox.addItem("Composite");

        enableSATransform = image.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL);

        while (iter.hasNext()) {
            storedMatrixBox.addItem(iter.next());
        }

        if (storedMatrixBox.getItemCount() > 1) {
            storedMatrixBox.insertItemAt("Composite", 0);
        }
        if (storedMatrixBox.getItemCount() == 0) {
            storedMatrix.setEnabled(false);
        }

        imageMatrixPanel.add(storedMatrixBox);
        matrixPanel.add(imageMatrixPanel);

        final JPanel filePanel = new JPanel(new BorderLayout());
        filePanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        fileMatrix = new JRadioButton("Read matrix from file", false);
        fileMatrix.setFont(serif12);
        fileMatrix.setEnabled(true);
        fileMatrix.setAlignmentX(Component.LEFT_ALIGNMENT);
        matrixDeterminationGroup.add(fileMatrix);
        filePanel.add(fileMatrix, BorderLayout.WEST);
        fileMatrix.addItemListener(this);

        matrixFName = new JTextField(10);
        matrixFName.setFont(serif12);
        matrixFName.setAlignmentX(Component.LEFT_ALIGNMENT);
        matrixFName.setEnabled(false);
        filePanel.add(matrixFName);

        matrixPanel.add(filePanel);

        final String orientText = "<html>Image origin is in the upper left hand corner (first slice)." + "<P>"
                + "Righthand coordinate system.</html>";
        final JLabel orientIconLabel = new JLabel(orientText, MipavUtil.getIcon("orient.gif"), SwingConstants.LEFT);
        orientIconLabel.setFont(serif12);
        orientIconLabel.setForeground(Color.black);
        matrixPanel.add(orientIconLabel);

        userDefinedMatrix = new JRadioButton("User defined transformation matrix", false);
        userDefinedMatrix.setBounds(10, 95, 200, 25);
        userDefinedMatrix.setFont(serif12);
        userDefinedMatrix.setEnabled(true);
        matrixDeterminationGroup.add(userDefinedMatrix);
        userDefinedMatrix.setAlignmentX(Component.LEFT_ALIGNMENT);
        matrixPanel.add(userDefinedMatrix);
        userDefinedMatrix.addItemListener(this);

        final JPanel translationPanel = new JPanel();
        translationPanel.setLayout(new GridBagLayout());
        translationPanel.setAlignmentX(Component.LEFT_ALIGNMENT);

        // translation
        labelTx = new JLabel("Tx (mm)");
        labelTx.setForeground(Color.black);
        labelTx.setFont(serif12);

        textTx = new JTextField();
        textTx.setPreferredSize(new Dimension(45, 20));
        textTx.setMinimumSize(new Dimension(25, 20));
        textTx.setText("0");
        textTx.setFont(serif12);
        textTx.addFocusListener(this);

        labelTy = new JLabel("Ty");
        labelTy.setForeground(Color.black);
        labelTy.setBounds(40, 155, 50, 25);
        labelTy.setFont(serif12);

        textTy = new JTextField();
        textTy.setPreferredSize(new Dimension(45, 20));
        textTy.setMinimumSize(new Dimension(25, 20));
        textTy.setText("0");
        textTy.setFont(serif12);
        textTy.addFocusListener(this);

        labelTz = new JLabel("Tz");
        labelTz.setForeground(Color.black);
        labelTz.setFont(serif12);

        textTz = new JTextField();
        textTz.setPreferredSize(new Dimension(45, 20));
        textTz.setMinimumSize(new Dimension(25, 20));
        textTz.setText("0");
        textTz.setFont(serif12);
        textTz.addFocusListener(this);

        // rotation
        labelRx = new JLabel("Rx (degrees)");
        labelRx.setForeground(Color.black);
        labelRx.setFont(serif12);

        textRx = new JTextField();
        textRx.setPreferredSize(new Dimension(45, 20));
        textRx.setMinimumSize(new Dimension(25, 20));
        textRx.setText("0");
        textRx.setFont(serif12);
        textRx.addFocusListener(this);

        labelRy = new JLabel("Ry");
        labelRy.setForeground(Color.black);
        labelRy.setFont(serif12);

        textRy = new JTextField();
        textRy.setPreferredSize(new Dimension(45, 20));
        textRy.setMinimumSize(new Dimension(25, 20));
        textRy.setText("0");
        textRy.setFont(serif12);
        textRy.addFocusListener(this);

        labelRz = new JLabel("Rz");
        labelRz.setForeground(Color.black);
        labelRz.setFont(serif12);

        textRz = new JTextField();
        textRz.setPreferredSize(new Dimension(45, 20));
        textRz.setMinimumSize(new Dimension(25, 20));
        textRz.setText("0");
        textRz.setFont(serif12);
        textRz.addFocusListener(this);

        // scaling
        labelSx = new JLabel("Sx");
        labelSx.setForeground(Color.black);
        labelSx.setFont(serif12);

        textSx = new JTextField();
        textSx.setPreferredSize(new Dimension(45, 20));
        textSx.setMinimumSize(new Dimension(25, 20));
        textSx.setText("1");
        textSx.setFont(serif12);
        textSx.addFocusListener(this);

        labelSy = new JLabel("Sy");
        labelSy.setForeground(Color.black);
        labelSy.setFont(serif12);

        textSy = new JTextField();
        textSy.setPreferredSize(new Dimension(45, 20));
        textSy.setMinimumSize(new Dimension(25, 20));
        textSy.setText("1");
        textSy.setFont(serif12);
        textSy.addFocusListener(this);

        labelSz = new JLabel("Sz");
        labelSz.setForeground(Color.black);
        labelSz.setFont(serif12);

        textSz = new JTextField();
        textSz.setPreferredSize(new Dimension(45, 20));
        textSz.setMinimumSize(new Dimension(25, 20));
        textSz.setText("1");
        textSz.setFont(serif12);
        textSz.addFocusListener(this);

        // skewing
        labelSKx = new JLabel("SKx");
        labelSKx.setForeground(Color.black);
        labelSKx.setFont(serif12);

        textSKx = new JTextField();
        textSKx.setPreferredSize(new Dimension(45, 20));
        textSKx.setMinimumSize(new Dimension(25, 20));
        textSKx.setText("0");
        textSKx.setFont(serif12);
        textSKx.addFocusListener(this);

        labelSKy = new JLabel("SKy");
        labelSKy.setForeground(Color.black);
        labelSKy.setFont(serif12);

        textSKy = new JTextField();
        textSKy.setPreferredSize(new Dimension(45, 20));
        textSKy.setMinimumSize(new Dimension(25, 20));
        textSKy.setText("0");
        textSKy.setFont(serif12);
        textSKy.addFocusListener(this);

        labelSKz = new JLabel("SKz");
        labelSKz.setForeground(Color.black);
        labelSKz.setFont(serif12);

        textSKz = new JTextField();
        textSKz.setPreferredSize(new Dimension(45, 20));
        textSKz.setMinimumSize(new Dimension(25, 20));
        textSKz.setText("0");
        textSKz.setFont(serif12);
        textSKz.addFocusListener(this);

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridheight = 1;
        gbc.gridwidth = 1;
        gbc.insets = new Insets(2, 2, 2, 2);

        gbc.gridx = 0;
        gbc.gridy = 0;
        translationPanel.add(Box.createHorizontalStrut(15), gbc);
        gbc.gridx = 1;
        translationPanel.add(labelTx, gbc);
        gbc.gridx = 2;
        translationPanel.add(textTx, gbc);
        gbc.gridx = 3;
        translationPanel.add(Box.createHorizontalStrut(5), gbc);
        gbc.gridx = 4;
        translationPanel.add(labelRx, gbc);
        gbc.gridx = 5;
        translationPanel.add(textRx, gbc);
        gbc.gridx = 6;
        translationPanel.add(Box.createHorizontalStrut(5), gbc);
        gbc.gridx = 7;
        translationPanel.add(labelSx, gbc);
        gbc.gridx = 8;
        translationPanel.add(textSx, gbc);
        gbc.gridx = 9;
        translationPanel.add(Box.createHorizontalStrut(5), gbc);
        gbc.gridx = 10;
        translationPanel.add(labelSKx, gbc);
        gbc.gridx = 11;
        translationPanel.add(textSKx, gbc);

        gbc.gridx = 1;
        gbc.gridy = 1;
        translationPanel.add(labelTy, gbc);
        gbc.gridx = 2;
        translationPanel.add(textTy, gbc);
        gbc.gridx = 4;
        translationPanel.add(labelRy, gbc);
        gbc.gridx = 5;
        translationPanel.add(textRy, gbc);
        gbc.gridx = 7;
        translationPanel.add(labelSy, gbc);
        gbc.gridx = 8;
        translationPanel.add(textSy, gbc);
        gbc.gridx = 10;
        translationPanel.add(labelSKy, gbc);
        gbc.gridx = 11;
        translationPanel.add(textSKy, gbc);

        gbc.gridx = 1;
        gbc.gridy = 2;
        translationPanel.add(labelTz, gbc);
        gbc.gridx = 2;
        translationPanel.add(textTz, gbc);
        gbc.gridx = 4;
        translationPanel.add(labelRz, gbc);
        gbc.gridx = 5;
        translationPanel.add(textRz, gbc);
        gbc.gridx = 7;
        translationPanel.add(labelSz, gbc);
        gbc.gridx = 8;
        translationPanel.add(textSz, gbc);
        gbc.gridx = 10;
        translationPanel.add(labelSKz, gbc);
        gbc.gridx = 11;
        translationPanel.add(textSKz, gbc);

        matrixPanel.add(translationPanel);

        labelTx.setEnabled(false);
        labelTy.setEnabled(false);
        labelTz.setEnabled(false);
        labelRx.setEnabled(false);
        labelRy.setEnabled(false);
        labelRz.setEnabled(false);
        labelSx.setEnabled(false);
        labelSy.setEnabled(false);
        labelSz.setEnabled(false);
        labelSKx.setEnabled(false);
        labelSKy.setEnabled(false);
        labelSKz.setEnabled(false);

        textTx.setEnabled(false);
        textTy.setEnabled(false);
        textTz.setEnabled(false);
        textRx.setEnabled(false);
        textRy.setEnabled(false);
        textRz.setEnabled(false);
        textSx.setEnabled(false);
        textSy.setEnabled(false);
        textSz.setEnabled(false);
        textSKx.setEnabled(false);
        textSKy.setEnabled(false);
        textSKz.setEnabled(false);

        return matrixPanel;
    }

    /**
     * Builds the OptionPanel.
     * 
     * @return DOCUMENT ME!
     */
    private JPanel buildOptionPanel() {

        final JPanel optionPanel = new JPanel();
        optionPanel.setForeground(Color.black);
        optionPanel.setBorder(buildTitledBorder("Options"));

        // *******INTERPOLATION****************
        final JLabel labelInterp = new JLabel("Interpolation:");
        labelInterp.setForeground(Color.black);
        labelInterp.setFont(serif12);
        labelInterp.setAlignmentX(Component.LEFT_ALIGNMENT);
        comboBoxInterp = new JComboBox();
        comboBoxInterp.setFont(serif12);
        comboBoxInterp.setBackground(Color.white);
        comboBoxInterp.setAlignmentX(Component.LEFT_ALIGNMENT);

        comboBoxInterp.addItem("Nearest Neighbor");

        if (image.getNDims() == 2) {
            comboBoxInterp.addItem("Bilinear");
        } else {
            comboBoxInterp.addItem("Trilinear");
        }

        comboBoxInterp.addItem("Bspline 3rd order");
        comboBoxInterp.addItem("Bspline 4th order");
        comboBoxInterp.addItem("Cubic Lagrangian");
        comboBoxInterp.addItem("Quintic Lagrangian");
        comboBoxInterp.addItem("Heptic Lagrangian");
        comboBoxInterp.addItem("Windowed sinc");
        comboBoxInterp.addItemListener(this);

        final JLabel labelInterpFor = new JLabel("Interpolate For :");
        labelInterpFor.setForeground(Color.black);
        labelInterpFor.setFont(serif12);
        labelInterpFor.setAlignmentX(Component.LEFT_ALIGNMENT);

        interpFOVgroup = new ButtonGroup();
        constantFOVradio = new JRadioButton("Constant FOV", true);
        constantFOVradio.setFont(serif12);
        constantFOVradio.setEnabled(true);
        interpFOVgroup.add(constantFOVradio);
        constantFOVradio.setSelected(false);
        constantFOVradio.addItemListener(this);
        constantFOVradio.setAlignmentX(Component.LEFT_ALIGNMENT);

        endMatchFOVradio = new JRadioButton("Unrepeated Begin & End Matching", true);
        endMatchFOVradio.setFont(serif12);
        endMatchFOVradio.setEnabled(true);
        interpFOVgroup.add(endMatchFOVradio);
        endMatchFOVradio.setSelected(true);
        endMatchFOVradio.addItemListener(this);
        endMatchFOVradio.setAlignmentX(Component.LEFT_ALIGNMENT);

        cropOrPad = new ButtonGroup();
        cropRadio = new JRadioButton("Retain original image size", true);
        cropRadio.setFont(serif12);
        cropRadio.setEnabled(false);
        cropOrPad.add(cropRadio);
        optionPanel.add(cropRadio);
        cropRadio.addItemListener(this);
        cropRadio.setAlignmentX(Component.LEFT_ALIGNMENT);

        padRadio = new JRadioButton("Pad image to include entire original image", false);
        padRadio.setFont(serif12);
        padRadio.setEnabled(false);
        cropOrPad.add(padRadio);
        padRadio.addItemListener(this);
        padRadio.setAlignmentX(Component.LEFT_ALIGNMENT);

        outOfBoundsLabel = new JLabel("Out of bounds data:");
        outOfBoundsLabel.setForeground(Color.black);
        outOfBoundsLabel.setFont(serif12);
        outOfBoundsLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

        outOfBoundsComboBox = new JComboBox();
        outOfBoundsComboBox.setFont(serif12);
        outOfBoundsComboBox.setBackground(Color.white);
        outOfBoundsComboBox.setAlignmentX(Component.LEFT_ALIGNMENT);

        outOfBoundsComboBox.addItem("Image minimum");
        outOfBoundsComboBox.addItem("If float NaN, else 0");
        outOfBoundsComboBox.addItem("User defined");
        outOfBoundsComboBox.addItem("Image maximum");
        outOfBoundsComboBox.setSelectedIndex(0);
        outOfBoundsComboBox.addItemListener(this);

        valueLabel = new JLabel("Out of bounds intensity value:");
        valueLabel.setForeground(Color.black);
        valueLabel.setFont(serif12);
        valueLabel.setAlignmentX(Component.LEFT_ALIGNMENT);

        valueText = new JTextField(String.valueOf(imageMin));
        valueText.setFont(serif12);
        valueText.setEnabled(false);

        clipCheckbox = new JCheckBox("Clip output values to input range");
        clipCheckbox.setFont(serif12);
        optionPanel.add(clipCheckbox);
        clipCheckbox.setSelected(true);
        clipCheckbox.setEnabled(false);
        clipCheckbox.addItemListener(this);
        clipCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);

        image25DCheckbox = new JCheckBox("Process each slice independently (2.5D).");
        image25DCheckbox.setFont(serif12);
        optionPanel.add(image25DCheckbox);
        image25DCheckbox.setSelected(false);
        image25DCheckbox.addItemListener(this);

        if (image.getNDims() < 3) {
            image25DCheckbox.setEnabled(false);
        }

        image25DCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);

        updateOriginCheckbox = new JCheckBox("Update origin.");
        updateOriginCheckbox.setFont(serif12);
        optionPanel.add(updateOriginCheckbox);
        updateOriginCheckbox.setSelected(true);
        updateOriginCheckbox.addItemListener(this);
        updateOriginCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);

        invertCheckbox = new JCheckBox("Invert matrix");
        invertCheckbox.setFont(serif12);
        invertCheckbox.setSelected(false);
        invertCheckbox.setEnabled(false);
        invertCheckbox.addItemListener(this);
        invertCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);

        voiCheckbox = new JCheckBox("Transform VOIs");
        voiCheckbox.setFont(serif12);
        optionPanel.add(voiCheckbox);
        voiCheckbox.setSelected(false);
        voiCheckbox.addItemListener(this);
        voiCheckbox.setAlignmentX(Component.LEFT_ALIGNMENT);

        // Should be transform VOI also
        if ( (image.getVOIs() == null) || (image.getVOIs().isEmpty() == true)) {
            voiCheckbox.setEnabled(false);
        }

        labelOrigin = new JLabel("Rotate About:");
        labelOrigin.setForeground(Color.black);
        labelOrigin.setFont(serif12);
        optionPanel.add(labelOrigin);
        // labelOrigin.setAlignmentX(Component.LEFT_ALIGNMENT);

        final JPanel rotateOptionPanel = new JPanel();

        rotationAxisGroup = new ButtonGroup();
        rotOrigin = new JRadioButton("Origin", false);
        rotOrigin.setFont(serif12);
        rotOrigin.setEnabled(false);
        rotationAxisGroup.add(rotOrigin);

        rotateOptionPanel.add(rotOrigin);
        // optionPanel.add(rotOrigin);
        rotOrigin.addItemListener(this);
        // rotOrigin.setAlignmentX(Component.LEFT_ALIGNMENT);

        rotCenter = new JRadioButton("Center", true);
        rotCenter.setFont(serif12);
        rotCenter.setEnabled(false);
        rotationAxisGroup.add(rotCenter);
        rotCenter.addItemListener(this);
        // rotCenter.setAlignmentX(Component.LEFT_ALIGNMENT);
        rotateOptionPanel.add(rotCenter);

        useSACenterBox = new JCheckBox("Use scanner center", false);
        useSACenterBox.setFont(serif12);
        useSACenterBox.setSelected(false);
        useSACenterBox.setEnabled(false);

        optionPanel.setLayout(new GridBagLayout());

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(2, 0, 2, 0);

        gbc.gridx = 0;
        gbc.gridy = 0;
        optionPanel.add(labelInterp, gbc);
        gbc.gridx = 1;
        optionPanel.add(Box.createHorizontalStrut(10), gbc);
        gbc.gridx = 2;
        gbc.gridwidth = 2;
        optionPanel.add(comboBoxInterp, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        optionPanel.add(labelInterpFor, gbc);
        gbc.gridx = 2;
        optionPanel.add(constantFOVradio, gbc);
        gbc.gridx = 3;
        optionPanel.add(endMatchFOVradio, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        optionPanel.add(labelOrigin, gbc);
        gbc.gridx = 2;
        optionPanel.add(rotateOptionPanel, gbc);
        gbc.gridx = 3;
        optionPanel.add(useSACenterBox, gbc);
        // optionPanel.add(rotCenter, gbc);

        gbc.gridx = 2;
        gbc.gridy = 3;
        optionPanel.add(cropRadio, gbc);
        gbc.gridx = 3;
        optionPanel.add(padRadio, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        optionPanel.add(outOfBoundsLabel, gbc);
        gbc.gridx = 2;
        gbc.gridy = 4;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optionPanel.add(outOfBoundsComboBox, gbc);
        gbc.gridx = 0;
        gbc.gridy = 5;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        optionPanel.add(valueLabel, gbc);
        gbc.gridx = 2;
        gbc.gridy = 5;
        gbc.weightx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        optionPanel.add(valueText, gbc);
        gbc.gridx = 0;
        gbc.gridy = 6;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.weightx = 1;
        optionPanel.add(clipCheckbox, gbc);
        gbc.gridy = 7;
        optionPanel.add(image25DCheckbox, gbc);
        gbc.gridy = 8;
        optionPanel.add(updateOriginCheckbox, gbc);
        gbc.gridy = 9;
        optionPanel.add(voiCheckbox, gbc);
        gbc.gridy = 10;
        optionPanel.add(invertCheckbox, gbc);

        return optionPanel;
    }

    /**
     * Builds the resample panel.
     * 
     * @return The resample panel.
     */
    private JPanel buildResamplePanel() {

        resamplePanel = new JPanel(new GridBagLayout());
        resamplePanel.setBorder(buildTitledBorder("Resample"));

        resampleGroup = new ButtonGroup();

        resampletoImage = new JRadioButton("Resample to size of:", true);
        resampletoImage.setFont(serif12);
        resampletoImage.setEnabled(true);
        resampleGroup.add(resampletoImage);
        resampletoImage.addItemListener(this);

        buildComboBox();

        resampletoUser = new JRadioButton("User defined size:", false);
        resampletoUser.setFont(serif12);
        resampletoUser.setEnabled(true);
        resampleGroup.add(resampletoUser);
        resampletoUser.addItemListener(this);

        xyAspectRatio = new JCheckBox("Lock XY aspect ratio.", false);
        xyAspectRatio.setFont(serif12);
        xyAspectRatio.addItemListener(this);
        xyAspectRatio.setEnabled(false);

        xyzAspectRatio = new JCheckBox("Lock XYZ aspect ratio.", false);
        xyzAspectRatio.setFont(serif12);
        xyzAspectRatio.addItemListener(this);
        xyzAspectRatio.setEnabled(false);

        resDimGroup = new ButtonGroup();
        separateDimResButton = new JRadioButton("Set dim and resolution independently", true);
        separateDimResButton.setFont(serif12);
        separateDimResButton.setEnabled(false);
        resDimGroup.add(separateDimResButton);
        separateDimResButton.addItemListener(this);
        separateDimResButton.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        preserveFOV2Button = new JRadioButton("Set both dim and resolution at the same time to preserve (dim-1)*resolution", false);
        preserveFOV2Button.setFont(serif12);
        preserveFOV2Button.setEnabled(false);
        resDimGroup.add(preserveFOV2Button);
        preserveFOV2Button.addItemListener(this);
        preserveFOV2Button.setAlignmentX(Component.LEFT_ALIGNMENT);

        outputDimsGroup = new ButtonGroup();
        preserveOutputDimsButton = new JRadioButton("Set output dim from selected image dim", true);
        preserveOutputDimsButton.setFont(serif12);
        preserveOutputDimsButton.setEnabled(true);
        outputDimsGroup.add(preserveOutputDimsButton);
        preserveOutputDimsButton.addItemListener(this);
        preserveOutputDimsButton.setAlignmentX(Component.LEFT_ALIGNMENT);
        
        preserveFOVButton = new JRadioButton("Set output dim to preserve input (dim-1)*resolution",false);
        preserveFOVButton.setFont(serif12);
        preserveFOVButton.setEnabled(true);
        outputDimsGroup.add(preserveFOVButton);
        preserveFOVButton.addItemListener(this);
        preserveFOVButton.setAlignmentX(Component.LEFT_ALIGNMENT);

        labelResX = new JLabel("ResX");
        labelResX.setForeground(Color.black);
        labelResX.setFont(serif12);

        textResX = new JTextField();
        textResX.setPreferredSize(new Dimension(72, 21));
        textResX.setMinimumSize(new Dimension(30, 21));
        cXres = image.getFileInfo()[0].getResolutions()[0];
        textResX.setText(String.valueOf(cXres));
        textResX.setFont(serif12);
        textResX.addFocusListener(this);

        labelResY = new JLabel("ResY");
        labelResY.setForeground(Color.black);
        labelResY.setFont(serif12);

        textResY = new JTextField();
        textResY.setPreferredSize(new Dimension(72, 21));
        textResY.setMinimumSize(new Dimension(30, 21));
        cYres = image.getFileInfo()[0].getResolutions()[1];
        textResY.setText(String.valueOf(cYres));
        textResY.setFont(serif12);
        textResY.addFocusListener(this);

        labelDimX = new JLabel("DimX");
        labelDimX.setForeground(Color.black);
        labelDimX.setFont(serif12);

        textDimX = new JTextField();
        textDimX.setPreferredSize(new Dimension(55, 20));
        textDimX.setMinimumSize(new Dimension(30, 20));
        cXdim = image.getExtents()[0];
        textDimX.setText(String.valueOf(cXdim));
        textDimX.setFont(serif12);
        textDimX.addFocusListener(this);

        labelDimY = new JLabel("DimY");
        labelDimY.setForeground(Color.black);
        labelDimY.setFont(serif12);

        textDimY = new JTextField();
        textDimY.setPreferredSize(new Dimension(55, 20));
        textDimY.setMinimumSize(new Dimension(30, 20));
        cYdim = image.getExtents()[1];
        textDimY.setText(String.valueOf(cYdim));
        textDimY.setFont(serif12);
        textDimY.addFocusListener(this);

        labelResZ = new JLabel("ResZ");
        labelResZ.setForeground(Color.black);
        labelResZ.setFont(serif12);

        textResZ = new JTextField();
        textResZ.setPreferredSize(new Dimension(72, 21));
        textResZ.setMinimumSize(new Dimension(30, 21));

        if (image.getNDims() >= 3) {
            cZres = image.getFileInfo()[0].getResolutions()[2];
            textResZ.setText(String.valueOf(cZres));
        }

        textResZ.setFont(serif12);
        textResZ.addFocusListener(this);

        labelDimZ = new JLabel("DimZ");
        labelDimZ.setForeground(Color.black);
        labelDimZ.setFont(serif12);

        textDimZ = new JTextField();
        textDimZ.setPreferredSize(new Dimension(55, 20));
        textDimZ.setMinimumSize(new Dimension(30, 20));

        if (image.getNDims() >= 3) {
            cZdim = image.getExtents()[2];
            textDimZ.setText(String.valueOf(cZdim));
        }

        textDimZ.setFont(serif12);
        textDimZ.addFocusListener(this);

        resampleSlider = new JRadioButton("Resample by factor:", false);
        resampleSlider.setFont(serif12);
        resampleSlider.setEnabled(true);
        resampleGroup.add(resampleSlider);
        resampleSlider.addItemListener(this);

        max = 400;
        min = 25;

        final int initialVal = 100;
        magSlider = new JSlider(SwingConstants.HORIZONTAL, min, max, initialVal);

        magSlider.setMajorTickSpacing( (max - min) / 5);
        magSlider.setPaintTicks(true);
        magSlider.setEnabled(false);

        magSlider.addChangeListener(this);

        maximum = new JLabel(String.valueOf(magSlider.getMaximum() / 100.0f));
        maximum.setForeground(Color.black);
        maximum.setFont(MipavUtil.font12);

        current = new JLabel(String.valueOf(magSlider.getValue() / 100.0f));
        current.setForeground(Color.black);
        current.setFont(MipavUtil.font12B);

        minimum = new JLabel(String.valueOf(magSlider.getMinimum() / 100.0f));
        minimum.setForeground(Color.black);
        minimum.setFont(MipavUtil.font12);

        labelResX.setEnabled(false);
        labelResY.setEnabled(false);
        labelResZ.setEnabled(false);
        labelDimX.setEnabled(false);
        labelDimY.setEnabled(false);
        labelDimZ.setEnabled(false);
        textResX.setEnabled(false);
        textResY.setEnabled(false);
        textResZ.setEnabled(false);
        textDimX.setEnabled(false);
        textDimY.setEnabled(false);
        textDimZ.setEnabled(false);
        maximum.setEnabled(false);
        minimum.setEnabled(false);
        current.setEnabled(false);

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(2, 2, 2, 2);

        /* First radio button: "Resample to size of: " */
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 2;
        resamplePanel.add(resampletoImage, gbc);

        gbc.gridx += 2;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.CENTER;
        resamplePanel.add(comboBoxImage, gbc);

        gbc.gridx = 1;
        gbc.gridy++;
        gbc.gridwidth = 2;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        resamplePanel.add(preserveOutputDimsButton, gbc);
        gbc.gridx = 1;
        gbc.gridy++;
        resamplePanel.add(preserveFOVButton, gbc);

        gbc.gridy++;
        resamplePanel.add(Box.createVerticalStrut(5), gbc);

        /* Second radio button: "User defined size:" */
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.anchor = GridBagConstraints.WEST;
        resamplePanel.add(resampletoUser, gbc);

        /* Before x values, in center: option to constrain field-of-view ratio */
        gbc.gridx = 1;
        gbc.gridy++;
        gbc.anchor = GridBagConstraints.WEST;
        resamplePanel.add(separateDimResButton, gbc);
        gbc.gridy++;
        resamplePanel.add(preserveFOV2Button, gbc);

        /* First row: x dimensions (first row of resolution info corresponds to gridy=4) */
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = 1;
        resamplePanel.add(Box.createHorizontalStrut(7), gbc);
        gbc.gridx++;
        resamplePanel.add(labelResX, gbc);
        gbc.gridx++;
        resamplePanel.add(textResX, gbc);
        gbc.gridx++;
        resamplePanel.add(labelDimX, gbc);
        gbc.gridx++;
        resamplePanel.add(textDimX, gbc);

        /* Second row: y dimensions */
        gbc.gridx = 0;
        gbc.gridy++;
        resamplePanel.add(Box.createHorizontalStrut(7), gbc);
        gbc.gridx++;
        resamplePanel.add(labelResY, gbc);
        gbc.gridx++;
        resamplePanel.add(textResY, gbc);
        gbc.gridx++;
        resamplePanel.add(labelDimY, gbc);
        gbc.gridx++;
        resamplePanel.add(textDimY, gbc);

        /* Between first and second rows, on right: option to constrain x-y aspect ratio */
        gbc.gridx++;
        resamplePanel.add(Box.createHorizontalStrut(3), gbc);
        gbc.gridx++;
        resamplePanel.add(xyAspectRatio, gbc);

        /* Third row: z dimensions */
        gbc.gridx = 0;
        gbc.gridy++;
        resamplePanel.add(Box.createHorizontalStrut(7), gbc);
        gbc.gridx++;
        resamplePanel.add(labelResZ, gbc);
        gbc.gridx++;
        resamplePanel.add(textResZ, gbc);
        gbc.gridx++;
        resamplePanel.add(labelDimZ, gbc);
        gbc.gridx++;
        resamplePanel.add(textDimZ, gbc);

        /* At far right of second row: option to constrain x-y-z aspect ratio */
        gbc.gridx++;
        resamplePanel.add(Box.createHorizontalStrut(3), gbc);
        gbc.gridx++;
        resamplePanel.add(xyzAspectRatio, gbc);

        gbc.gridy++;
        resamplePanel.add(Box.createVerticalStrut(5), gbc);

        /* Third radio button: "Resample by factor:" */
        gbc.gridx = 0;
        gbc.gridy++;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridwidth = 2;
        resamplePanel.add(resampleSlider, gbc);

        final JPanel sliderPanel = new JPanel(new GridBagLayout());
        final GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.gridwidth = 3;
        gbc2.weightx = 1;
        gbc2.gridheight = 1;
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        sliderPanel.add(magSlider, gbc2);
        gbc2.gridx = 0;
        gbc2.gridy = 1;
        gbc2.gridwidth = 1;
        gbc2.weightx = 0;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.fill = GridBagConstraints.NONE;
        sliderPanel.add(minimum, gbc2);
        gbc2.gridx = 1;
        gbc2.anchor = GridBagConstraints.CENTER;
        gbc2.weightx = .5;
        sliderPanel.add(current, gbc2);
        gbc2.gridx = 2;
        gbc2.anchor = GridBagConstraints.EAST;
        gbc2.weightx = 0;
        sliderPanel.add(maximum, gbc2);

        gbc.gridx = 0;
        gbc.gridy++;
        gbc.gridwidth = GridBagConstraints.REMAINDER;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.anchor = GridBagConstraints.NORTH;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        resamplePanel.add(sliderPanel, gbc);

        return resamplePanel;
    }

    /**
     * DOCUMENT ME!
     */
    private void callTalAlgorithm() {
        int[] dims = null;
        FileInfoBase[] fileInfo;
        int i;
        final Vector3f currentOrigin = new Vector3f(image.getOrigin());
        final Vector3f newOrigin = new Vector3f();
        int[] newOrientations = new int[3];

        if (transformType == JDialogScriptableTransform.ACPC_TO_ORIG) {
            dims = tInfo.getOrigDim();
            tInfo.acpcToOrig(currentOrigin, newOrigin);
            newOrientations = tInfo.getOrigOrientLabelsInverse();
        } else if (transformType == JDialogScriptableTransform.TLRC_TO_ORIG) {
            dims = tInfo.getOrigDim();
            tInfo.tlrcToOrig(currentOrigin, newOrigin);
            newOrientations = tInfo.getOrigOrientLabelsInverse();
        } else if (transformType == JDialogScriptableTransform.ORIG_TO_ACPC) {
            dims = tInfo.getAcpcDim();
            tInfo.origToAcpc(currentOrigin, newOrigin);
        } else if (transformType == JDialogScriptableTransform.TLRC_TO_ACPC) {
            dims = tInfo.getAcpcDim();
            tInfo.tlrcToAcpc(currentOrigin, newOrigin);
        } else if (transformType == JDialogScriptableTransform.ORIG_TO_TLRC) {
            dims = tInfo.getTlrcDim();
            tInfo.origToTlrc(currentOrigin, newOrigin);
        } else if (transformType == JDialogScriptableTransform.ACPC_TO_TLRC) {
            dims = tInfo.getTlrcDim();
            tInfo.acpcToTlrc(currentOrigin, newOrigin);
        }

        try {
            resultImage = new ModelImage(ModelStorageBase.FLOAT, dims, JDialogBase.makeImageName(image.getImageName(),
                    "_TT"));
            fileInfo = resultImage.getFileInfo();

            // Dont think the following code is needed...seems redundant to the stuff below it
            // Won't erase for now

            /*
             * if ( (transformType == JDialogScriptableTransform.ACPC_TO_ORIG) || (transformType ==
             * JDialogScriptableTransform.TLRC_TO_ORIG)) { axisOrientation = getAxisOrientation(tInfo.getOrigOrient());
             * zAxisOrientation = axisOrientation[2];
             * 
             * if ( (zAxisOrientation == FileInfoBase.ORI_R2L_TYPE) || (zAxisOrientation == FileInfoBase.ORI_L2R_TYPE)) {
             * imageOrientation = FileInfoBase.SAGITTAL; } else if ( (zAxisOrientation == FileInfoBase.ORI_A2P_TYPE) ||
             * (zAxisOrientation == FileInfoBase.ORI_P2A_TYPE)) { imageOrientation = FileInfoBase.CORONAL; } else if (
             * (zAxisOrientation == FileInfoBase.ORI_I2S_TYPE) || (zAxisOrientation == FileInfoBase.ORI_S2I_TYPE)) {
             * imageOrientation = FileInfoBase.AXIAL; } else { imageOrientation = FileInfoBase.ORI_UNKNOWN_TYPE; }
             * 
             * resultImage.setImageOrientation(imageOrientation);
             * 
             * for (i = 0; i < fileInfo.length; i++) { final int[] units = new int[3]; units[0] = units[1] = units[2] =
             * Unit.MILLIMETERS.getLegacyNum(); fileInfo[i].setUnitsOfMeasure(units);
             * fileInfo[i].setResolutions(tInfo.getOrigRes()); fileInfo[i].setExtents(dims);
             * fileInfo[i].setAxisOrientation(axisOrientation); fileInfo[i].setImageOrientation(imageOrientation); } //
             * for (i = 0; i < fileInfo.length; i++) } // if ((transformType == ACPC_TO_ORIG) || (transformType ==
             * TLRC_TO_ORIG)) else { // not transformed to ORIG resultImage.setImageOrientation(FileInfoBase.AXIAL);
             * 
             * final int[] units = new int[3]; units[0] = units[1] = units[2] = Unit.MILLIMETERS.getLegacyNum();
             * 
             * final float[] resol = new float[3]; resol[0] = resol[1] = resol[2] = tInfo.getAcpcRes(); axisOrientation =
             * new int[3]; axisOrientation[0] = FileInfoBase.ORI_R2L_TYPE; axisOrientation[1] =
             * FileInfoBase.ORI_A2P_TYPE; axisOrientation[2] = FileInfoBase.ORI_I2S_TYPE;
             * 
             * for (i = 0; i < fileInfo.length; i++) { fileInfo[i].setUnitsOfMeasure(units);
             * fileInfo[i].setResolutions(resol); fileInfo[i].setExtents(dims);
             * fileInfo[i].setAxisOrientation(axisOrientation); fileInfo[i].setImageOrientation(FileInfoBase.AXIAL); } //
             * for (i = 0; i < fileInfo.length; i++) } // else not transformed to ORIG
             */
            if ( (transformType == AlgorithmTalairachTransform.ACPC_TO_ORIG)
                    || (transformType == AlgorithmTalairachTransform.TLRC_TO_ORIG)) {

                resultImage.setImageOrientation(tInfo.getOrigImageOrientLabel());

                // new image in original space (only difference: units forced to millimeters)
                final int[] units = new int[3];
                units[0] = units[1] = units[2] = Unit.MILLIMETERS.getLegacyNum();

                for (i = 0; i < fileInfo.length; i++) {
                    fileInfo[i].setUnitsOfMeasure(units);
                    fileInfo[i].setResolutions(tInfo.getOrigRes());
                    fileInfo[i].setExtents(tInfo.getOrigDim());
                    fileInfo[i].setOrigin(tInfo.getOrigOrigin());
                    fileInfo[i].setAxisOrientation(newOrientations);
                    fileInfo[i].setImageOrientation(tInfo.getOrigImageOrientLabel());

                    // BEN
                    // fileInfo[i].setTransformID(FileInfoBase.TRANSFORM_TALAIRACH_TOURNOUX);
                }

                resultImage.setFileInfo(fileInfo);
                resultImage.setTalairachTransformInfo(tInfo);

            } else if ( (transformType == AlgorithmTalairachTransform.ORIG_TO_ACPC)
                    || (transformType == AlgorithmTalairachTransform.TLRC_TO_ACPC)) {

                // new image in AC-PC space: everything is standard
                resultImage.setImageOrientation(FileInfoBase.AXIAL);

                final int[] units = new int[3];
                units[0] = units[1] = units[2] = Unit.MILLIMETERS.getLegacyNum();

                final float[] resol = new float[3];
                resol[0] = resol[1] = resol[2] = tInfo.getAcpcRes();

                final int[] orient = new int[3];
                orient[0] = FileInfoBase.ORI_R2L_TYPE;
                orient[1] = FileInfoBase.ORI_A2P_TYPE;
                orient[2] = FileInfoBase.ORI_I2S_TYPE;

                for (i = 0; i < fileInfo.length; i++) {
                    fileInfo[i].setUnitsOfMeasure(units);
                    fileInfo[i].setResolutions(resol);
                    fileInfo[i].setExtents(tInfo.getAcpcDim());
                    fileInfo[i].setAxisOrientation(orient);
                    fileInfo[i].setImageOrientation(FileInfoBase.AXIAL);

                    // BEN
                    // fileInfo[i].setTransformID(FileInfoBase.TRANSFORM_TALAIRACH_TOURNOUX);
                }

                resultImage.setFileInfo(fileInfo);
                resultImage.setTalairachTransformInfo(tInfo);

            } else {

                // new image in Talairach space: also standard, only the extents are changed
                resultImage.setImageOrientation(FileInfoBase.AXIAL);

                final int[] units = new int[3];
                units[0] = units[1] = units[2] = Unit.MILLIMETERS.getLegacyNum();

                final float[] resol = new float[3];
                resol[0] = resol[1] = resol[2] = tInfo.getAcpcRes();

                final int[] orient = new int[3];
                orient[0] = FileInfoBase.ORI_R2L_TYPE;
                orient[1] = FileInfoBase.ORI_A2P_TYPE;
                orient[2] = FileInfoBase.ORI_I2S_TYPE;

                for (i = 0; i < fileInfo.length; i++) {
                    fileInfo[i].setUnitsOfMeasure(units);
                    fileInfo[i].setResolutions(resol);
                    fileInfo[i].setExtents(tInfo.getTlrcDim());
                    fileInfo[i].setAxisOrientation(orient);
                    fileInfo[i].setImageOrientation(FileInfoBase.AXIAL);

                    // BEN
                    // fileInfo[i].setTransformID(FileInfoBase.TRANSFORM_TALAIRACH_TOURNOUX);
                }

                resultImage.setFileInfo(fileInfo);
                resultImage.setTalairachTransformInfo(tInfo);
            }

            resultImage.setFileInfo(fileInfo);

            algoTal = new AlgorithmTalairachTransform(resultImage, image, tInfo, transformType, interp, doClip, doVOI);

            // This is very important. Adding this object as a listener allows
            // the algorithm to notify this object when it has completed of failed.
            // See algorithm performed event. This is made possible by implementing
            algoTal.addListener(this);
            // Start the thread as a low priority because we wish to still have
            // user interface work fast

            if (isRunInSeparateThread()) {

                if (algoTal.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                algoTal.run();
            }
        } // try
        catch (final OutOfMemoryError x) {

            if (resultImage != null) {
                resultImage.disposeLocal();
                resultImage = null;
            }

            System.gc();
            MipavUtil.displayError("JDialogScriptableTransform unable to allocate enough memory");

            return;
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @param state DOCUMENT ME!
     */
    private void enableDims(final boolean state) {
        labelDimX.setEnabled(state);
        labelDimY.setEnabled(state);
        textDimX.setEnabled(state);
        textDimY.setEnabled(state);

        if ( (image.getNDims() >= 3) && (image25DCheckbox.isSelected() == false)) {
            labelDimZ.setEnabled(state);
            textDimZ.setEnabled(state);
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @param state DOCUMENT ME!
     */
    private void enableResols(final boolean state) {
        labelResX.setEnabled(state);
        labelResY.setEnabled(state);
        textResX.setEnabled(state);
        textResY.setEnabled(state);

        if ( (image.getNDims() >= 3) && (image25DCheckbox.isSelected() == false)) {
            labelResZ.setEnabled(state);
            textResZ.setEnabled(state);
        }
    }

    /**
     * DOCUMENT ME!
     * 
     * @param state DOCUMENT ME!
     */
    private void enableYSettings(final boolean state) {
        labelDimY.setEnabled(state);
        textDimY.setEnabled(state);
        labelResY.setEnabled(state);
        textResY.setEnabled(state);
    }

    /**
     * Return the 3 axis orientation codes that correspond to the closest standard anatomical orientation of the (i,j,k)
     * axes.
     * 
     * @param array 4x4 matrix that transforms (i,j,k) indexes to x,y,z coordinates where +x =Left, +y = Posterior, +z =
     *            Superior Only the upper-left 3x3 corner of the matrix is used This routine finds the permutation of
     *            (x,y,z) which has the smallest angle to the (i,j,k) axes directions, which are columns of the input
     *            matrix
     * 
     * @return DOCUMENT ME!
     */
    @SuppressWarnings("unused")
    private int[] getAxisOrientation(final float[][] array) {
        final int[] axisOrientation = new int[3];
        double xi, xj, xk, yi, yj, yk, zi, zj, zk, val;
        Matrix Q;
        double detQ;
        double vbest;
        int ibest, jbest, kbest, pbest, qbest, rbest;
        int i, j, k, p, q, r;
        Matrix P;
        double detP;
        Matrix M;

        xi = array[0][0];
        xj = array[0][1];
        xk = array[0][2];
        yi = array[1][0];
        yj = array[1][1];
        yk = array[1][2];
        zi = array[2][0];
        zj = array[2][1];
        zk = array[2][2];
        // Normalize column vectors to get unit vectors along each ijk-axis

        // Normalize i axis
        val = Math.sqrt( (xi * xi) + (yi * yi) + (zi * zi));

        if (val == 0.0) {
            MipavUtil.displayError("xi = yi = zi = 0 in getAxisOrientation");

            return null;
        }

        xi /= val;
        yi /= val;
        zi /= val;

        // Normalize j axis
        val = Math.sqrt( (xj * xj) + (yj * yj) + (zj * zj));

        if (val == 0.0) {
            MipavUtil.displayError("xj = yj = zj = 0 in getAxisOrientation");

            return null;
        }

        xj /= val;
        yj /= val;
        zj /= val;

        // Orthogonalize j axis to i axis, if needed
        val = (xi * xj) + (yi * yj) + (zi * zj); // dot product between i and j

        if (Math.abs(val) > 1.0e-4) {
            xj -= val * xi;
            yj -= val * yi;
            zj -= val * zi;
            val = Math.sqrt( (xj * xj) + (yj * yj) + (zj * zj)); // Must renormalize

            if (val == 0.0) {
                MipavUtil.displayError("j was parallel to i in getAxisOrientation");

                return null;
            }

            xj /= val;
            yj /= val;
            zj /= val;
        }

        // Normalize k axis; if it is zero, make it the cross product i x j
        val = Math.sqrt( (xk * xk) + (yk * yk) + (zk * zk));

        if (val == 0.0) {
            xk = (yi * zj) - (zi * yj);
            yk = (zi * xj) - (zj * xi);
            zk = (xi * yj) - (yi * xj);
        } else {
            xk /= val;
            yk /= val;
            zk /= val;
        }

        // Orthogonalize k to i
        val = (xi * xk) + (yi * yk) + (zi * zk); // dot product between i and k

        if (Math.abs(val) > 1.0e-4) {
            xk -= val * xi;
            yk -= val * yi;
            zk -= val * zi;
            val = Math.sqrt( (xk * xk) + (yk * yk) + (zk * zk));

            if (val == 0.0) {
                MipavUtil.displayError("val == 0 when orthogonalizing k to i");

                return null;
            }

            xk /= val;
            yk /= val;
            zk /= val;
        }

        // Orthogonalize k to j
        val = (xj * xk) + (yj * yk) + (zj * zk); // dot product between j and k

        if (Math.abs(val) > 1.0e-4) {
            xk -= val * xj;
            yk -= val * yj;
            zk -= val * zj;
            val = Math.sqrt( (xk * xk) + (yk * yk) + (zk * zk));

            if (val == 0.0) {
                MipavUtil.displayError("val == 0 when orthogonalizing k to j");

                return null;
            }

            xk /= val;
            yk /= val;
            zk /= val;
        }

        final double[][] darray = new double[3][3];
        darray[0][0] = xi;
        darray[0][1] = xj;
        darray[0][2] = xk;
        darray[1][0] = yi;
        darray[1][1] = yj;
        darray[1][2] = yk;
        darray[2][0] = zi;
        darray[2][1] = zj;
        darray[2][2] = zk;

        // At this point, Q is the rotation matrix from the (i,j,k) to the (x,y,z) axes
        Q = new Matrix(darray);
        detQ = Q.det();

        if (detQ == 0.0) {
            MipavUtil.displayError("detQ == 0.0 in getAxisOrientation");

            return null;
        }

        // Build and test all possible +1/-1 coordinate permutation matrices P;
        // then find the P such that the rotation matrix M=PQ is closest to the
        // identity, in the sense of M having the smallest total rotation angle

        // Despite the formidable looking 6 nested loops, there are
        // only 3*3*3*2*2*2 = 216 passes, which will run very quickly
        vbest = -Double.MAX_VALUE;
        pbest = 1;
        qbest = 1;
        rbest = 1;
        ibest = 1;
        jbest = 2;
        kbest = 3;

        for (i = 1; i <= 3; i++) { // i = column number to use for row #1

            for (j = 1; j <= 3; j++) { // j = column number to use for row #2

                if (i == j) {
                    continue;
                }

                for (k = 1; k <= 3; k++) { // k = column number to use for row #3

                    if ( (i == k) || (j == k)) {
                        continue;
                    }

                    darray[0][0] = 0.0;
                    darray[0][1] = 0.0;
                    darray[0][2] = 0.0;
                    darray[1][0] = 0.0;
                    darray[1][1] = 0.0;
                    darray[1][2] = 0.0;
                    darray[2][0] = 0.0;
                    darray[2][1] = 0.0;
                    darray[2][2] = 0.0;
                    P = new Matrix(darray);

                    for (p = -1; p <= 1; p += 2) { // p,q,r are -1 or +1 and go into rows #1,2,3

                        for (q = -1; q <= 1; q += 2) {

                            for (r = -1; r <= 1; r += 2) {
                                P.set(0, i - 1, p);
                                P.set(1, j - 1, q);
                                P.set(2, k - 1, r);
                                detP = P.det();

                                // sign of permutation doesn't match sign of Q
                                if ( (detP * detQ) <= 0.0) {
                                    continue;
                                }

                                M = P.times(Q);

                                // angle of M rotation = 2.0*acos(0.5*sqrt(1.0+trace(M)))
                                // we want largest trace(M) == smallest angle == M nearest to I
                                val = M.get(0, 0) + M.get(1, 1) + M.get(2, 2); // trace

                                if (val > vbest) {
                                    vbest = val;
                                    ibest = i;
                                    jbest = j;
                                    kbest = k;
                                    pbest = p;
                                    qbest = q;
                                    rbest = r;
                                }
                            }
                        }
                    }
                }
            }
        }

        // At this point ibest is 1 or 2 or 3; pbest is -1 or +1; etc.

        // The matrix P that corresponds is the best permutation approximation
        // to Q-inverse; that is, P (approximately) takes (x,y,z) coordinates
        // to the (i,j,k) axes

        // For example, the first row of P (which contains pbest in column ibest)
        // determines the way the i axis points relative to the anatomical
        // (x,y,z) axes. If ibest is 2, then the i axis is along the yaxis,
        // which is direction P2A (if pbest < 0) or A2P (if pbest > 0).

        // So, using ibest and pbest, we can assign the output code for
        // the i axis. The same also applies for the j and k axes.

        switch (ibest * pbest) {

            case -1:
                axisOrientation[0] = FileInfoBase.ORI_L2R_TYPE;
                break;

            case 1:
                axisOrientation[0] = FileInfoBase.ORI_R2L_TYPE;
                break;

            case -2:
                axisOrientation[0] = FileInfoBase.ORI_P2A_TYPE;
                break;

            case 2:
                axisOrientation[0] = FileInfoBase.ORI_A2P_TYPE;
                break;

            case -3:
                axisOrientation[0] = FileInfoBase.ORI_S2I_TYPE;
                break;

            case 3:
                axisOrientation[0] = FileInfoBase.ORI_I2S_TYPE;
                break;
        }

        switch (jbest * qbest) {

            case -1:
                axisOrientation[1] = FileInfoBase.ORI_L2R_TYPE;
                break;

            case 1:
                axisOrientation[1] = FileInfoBase.ORI_R2L_TYPE;
                break;

            case -2:
                axisOrientation[1] = FileInfoBase.ORI_P2A_TYPE;
                break;

            case 2:
                axisOrientation[1] = FileInfoBase.ORI_A2P_TYPE;
                break;

            case -3:
                axisOrientation[1] = FileInfoBase.ORI_S2I_TYPE;
                break;

            case 3:
                axisOrientation[1] = FileInfoBase.ORI_I2S_TYPE;
                break;
        }

        switch (kbest * rbest) {

            case -1:
                axisOrientation[2] = FileInfoBase.ORI_L2R_TYPE;
                break;

            case 1:
                axisOrientation[2] = FileInfoBase.ORI_R2L_TYPE;
                break;

            case -2:
                axisOrientation[2] = FileInfoBase.ORI_P2A_TYPE;
                break;

            case 2:
                axisOrientation[2] = FileInfoBase.ORI_A2P_TYPE;
                break;

            case -3:
                axisOrientation[2] = FileInfoBase.ORI_S2I_TYPE;
                break;

            case 3:
                axisOrientation[2] = FileInfoBase.ORI_I2S_TYPE;
                break;
        }

        return axisOrientation;
    }

    /**
     * Initializes the dialog box to a certain size and adds the components.
     */
    private void init() {
        image.calcMinMax();
        imageMin = image.getMin();
        imageMax = image.getMax();
        dataType = image.getFileInfo()[0].getDataType();
        setForeground(Color.black);
        setTitle("Transform/Resample Image");

        final JPanel matrixPanel = buildMatrixPanel();
        final JPanel optionPanel = buildOptionPanel();
        final JPanel resamplePanel = buildResamplePanel();

        final JPanel leftPanel = new JPanel();
        leftPanel.setLayout(new BorderLayout());
        leftPanel.add(matrixPanel);
        leftPanel.add(optionPanel, BorderLayout.SOUTH);

        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.font12B);
        tabbedPane.addTab("Transform", leftPanel);

        tabbedPane.addTab("Resample", resamplePanel);

        tabbedPane.addChangeListener(this);

        final GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 1.0;
        gbc.weighty = 1.0;

        final JPanel buttonPanel = new JPanel();
        buttonPanel.add(buildButtons());

        getContentPane().add(tabbedPane);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        comboBoxInterp.setSelectedIndex(1);
        pack();
        setVisible(true);
        setResizable(false);

    }

    /**
     * Re-orient the matrix to world and left-hand coordinate systems if required.
     * 
     * @see reorientCoordSystem
     * @param rkMatrix the matrix to be converted
     * @return result
     */
    private TransMatrix reorientCoordSystem(final TransMatrix rkMatrix) {
        return JDialogScriptableTransform.reorientCoordSystem(rkMatrix, image, resampleImage, wcSystem, leftHandSystem);

    }

    /**
     * Re-orient the matrix to world and left-hand coordinate systems if required. Note at the moment the voxel
     * resolutions are handled in the transformation algorithm. At some future point we should adjust for voxel
     * resolutions in the transformation matrix - its faster.
     * 
     * @param rkMatrix the matrix to be converted
     * @param rkImage main image
     * @param rkResampleImage resampled image
     * @param bWcSystem true if world coord system
     * @param bLeftHandSystem true if left-handed system.
     * @return result
     */
    public static TransMatrix reorientCoordSystem(final TransMatrix rkMatrix, final ModelImage rkImage,
            final ModelImage rkResampleImage, final boolean bWcSystem, final boolean bLeftHandSystem) {

        try {
            final TransMatrix mat = new TransMatrix(4);
            final TransMatrix wcMatrix = new TransMatrix(4);
            final TransMatrix rh_lhMatrix = new TransMatrix(4);
            // double[][] dMat;

            // right handed to left handed or left handed to right handed
            // coordinate systems
            rh_lhMatrix.Set(2, 2, -1);

            // p.223 Foley, Van Dam ...
            // Flipping only z axis
            // 1 0 0 0
            // 0 1 0 0
            // 0 0 -1 0
            // 0 0 0 1

            wcMatrix.Set(1, 1, -1);
            wcMatrix.Set(2, 2, -1);

            // Flipping y and z axes
            // Left handed cooordinates will remain left handed and right handed will remain right handed
            // 1 0 0 0
            // 0 -1 0 0
            // 0 0 -1 0
            // 0 0 0 1

            final Vector3f cPt = rkImage.getImageCentermm(false);
            final Vector3f cPtRS = rkResampleImage.getImageCentermm(false);

            if ( (bWcSystem == true) && (bLeftHandSystem == true)) {

                // change from both the "left-hand" and world coordinate system.
                mat.setTranslate(cPtRS.X, cPtRS.Y, cPtRS.Z);
                mat.Mult(rh_lhMatrix);
                mat.Mult(wcMatrix);
                mat.setTranslate( -cPtRS.X, -cPtRS.Y, -cPtRS.Z);

                mat.Mult(rkMatrix);

                mat.setTranslate(cPt.X, cPt.Y, cPt.Z);
                mat.Mult(wcMatrix);
                mat.Mult(rh_lhMatrix);
                mat.setTranslate( -cPt.X, -cPt.Y, -cPt.Z);

                // mat.print();
                return mat;
            } else if (bWcSystem == true) { // Change just from the world coordinate system

                mat.setTranslate(cPtRS.X, cPtRS.Y, cPtRS.Z);
                mat.Mult(wcMatrix);
                mat.setTranslate( -cPtRS.X, -cPtRS.Y, -cPtRS.Z);

                mat.Mult(rkMatrix);

                mat.setTranslate(cPt.X, cPt.Y, cPt.Z);
                mat.Mult(wcMatrix);
                mat.setTranslate( -cPt.X, -cPt.Y, -cPt.Z);

                return mat;
            } else if (bLeftHandSystem == true) { // Change just from the "left-hand" system

                mat.setTranslate(cPtRS.X, cPtRS.Y, cPtRS.Z);
                mat.Mult(rh_lhMatrix);
                mat.setTranslate( -cPtRS.X, -cPtRS.Y, -cPtRS.Z);

                mat.Mult(rkMatrix);

                mat.setTranslate(cPt.X, cPt.Y, cPt.Z);
                mat.Mult(rh_lhMatrix);
                mat.setTranslate( -cPt.X, -cPt.Y, -cPt.Z);

                return mat;
            } else {
                return rkMatrix;
            }
        } catch (final OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: Orient matrix.");

            return rkMatrix;
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void setDefaultResampleToUser() {
        xyAspectRatio.setSelected(true);
        enableYSettings(false);
        preserveFOV2Button.setSelected(true);
        separateDimResButton.setSelected(false);
    }

    /**
     * Sets the variables needed to run the algorithm.
     * 
     * @return Flag indicating successful set of the variables.
     */
    private boolean setVariables() {
        String tmpStr;
        float factor;
        int iXdim, iYdim, iZdim = 0;
        float iXres, iYres, iZres = 1.f;
        double Tx = 0, Ty = 0, Tz = 0, Rx, Ry, Rz, Sx, Sy, Sz, SKx, SKy, SKz;
        float fovX = 0.f, fovY = 0.f, fovZ = 0.f;

        final int boxIndex = comboBoxInterp.getSelectedIndex();

        if (boxIndex != 1) {
            constantFOVradio.setEnabled(false);
            endMatchFOVradio.setEnabled(false);
            constantFOVradio.setSelected(true);
            endMatchFOVradio.setSelected(false);
            constantFOV = 0;
        }

        if (boxIndex == 0) {
            interp = AlgorithmTransform.NEAREST_NEIGHBOR;
        } else if (boxIndex == 1) {

            if (do25D || (image.getNDims() == 2)) {
                interp = AlgorithmTransform.BILINEAR;
            } else {
                interp = AlgorithmTransform.TRILINEAR;
            }
            constantFOVradio.setEnabled(true);
            endMatchFOVradio.setEnabled(true);
            if (constantFOVradio.isSelected()) {
                constantFOV = 0;
            } else {
                constantFOV = 1;
            }
        } // else if (boxIndex == 1)
        else if (boxIndex == 2) {
            interp = AlgorithmTransform.BSPLINE3;
        } else if (boxIndex == 3) {
            interp = AlgorithmTransform.BSPLINE4;
        } else if (boxIndex == 4) {
            interp = AlgorithmTransform.CUBIC_LAGRANGIAN;
        } else if (boxIndex == 5) {
            interp = AlgorithmTransform.QUINTIC_LAGRANGIAN;
        } else if (boxIndex == 6) {
            interp = AlgorithmTransform.HEPTIC_LAGRANGIAN;
        } else if (boxIndex == 7) {
            interp = AlgorithmTransform.WSINC;
        }

        doVOI = voiCheckbox.isSelected();
        doClip = clipCheckbox.isSelected();
        doInvMat = invertCheckbox.isSelected();

        // set the units. will be reset if image is being resampled to another image's size
        units = new int[image.getUnitsOfMeasure().length];
        for (int i = 0; i < units.length; i++) {
            units[i] = image.getUnitsOfMeasure(i);
        }

        if ( (computeTImage != null) && computeTImage.isSelected()) {
            transformType = comboBoxTalTransform.getSelectedIndex();

            if ( !tInfo.isAcpc()) {
                MipavUtil.displayError("The ACPC transformation is not set");

                return false;
            } // if (!tInfo.isAcpc())

            if ( (transformType == JDialogScriptableTransform.ORIG_TO_TLRC)
                    || (transformType == JDialogScriptableTransform.ACPC_TO_TLRC)
                    || (transformType == JDialogScriptableTransform.TLRC_TO_ORIG)
                    || (transformType == JDialogScriptableTransform.TLRC_TO_ACPC)) {

                if ( !tInfo.isTlrc()) {
                    MipavUtil.displayError("The Talairach transformation is not set");

                    return false;
                }
            }

            doTalairach = true;

            return true;
        } // if (computeTImage.isSelected())

        doPad = padRadio.isSelected();
        doUpdateOrigin = updateOriginCheckbox.isSelected();

        if ( (do25D) || (image.getNDims() == 2)) {
            xfrm = new TransMatrix(3);
        } else { // (image.getNDims() >= 3) && (!do25D)
            xfrm = new TransMatrix(4);
        }

        xfrm.MakeIdentity();

        iXres = image.getFileInfo(0).getResolutions()[0];
        iYres = image.getFileInfo(0).getResolutions()[1];
        iXdim = image.getExtents()[0];
        iYdim = image.getExtents()[1];

        if (image.getNDims() >= 3) {
            iZdim = image.getExtents()[2];
            iZres = image.getFileInfo(0).getResolutions()[2];
        } else {
            iZdim = 1;
            iZres = 1.f;
        }

        oXres = oYres = oZres = oXdim = oYdim = oZdim = 1; // initialize

        // if a transform matrix is to be used (NOT resampling)
        // must adjust the result extents and resolutions
        if ( !noTransform.isSelected()) {

            oXres = image.getFileInfo()[0].getResolutions()[0];
            oYres = image.getFileInfo()[0].getResolutions()[1];

            oXdim = image.getExtents()[0];
            oYdim = image.getExtents()[1];

            if (image.getNDims() > 2) {
                oZres = image.getFileInfo()[0].getResolutions()[2];
                oZdim = image.getExtents()[2];
            }
        }

        // if (noTransform.isSelected()) {

        // RESAMPLE INFO
        if (resampletoImage.isSelected()) {
            final String selectName = (String) (comboBoxImage.getSelectedItem());

            // get the selected image
            final ModelImage selectedImg = userInterface.getRegisteredImageByName(selectName);

            // assign output resolutions and dims to those of image selected in comboBox
            oXres = selectedImg.getFileInfo(0).getResolutions()[0];
            oYres = selectedImg.getFileInfo(0).getResolutions()[1];

            // get the units now
            units[0] = selectedImg.getUnitsOfMeasure(0);
            units[1] = selectedImg.getUnitsOfMeasure(1);

            System.err.println("units of original image: " + image.getUnitsOfMeasure(0));
            System.err.println("setting units: " + units[0]);

            if (preserveFOV) {
                fovX = iXres * (iXdim - constantFOV);
                fovY = iYres * (iYdim - constantFOV);
                oXdim = Math.round(fovX / oXres) + constantFOV;
                oYdim = Math.round(fovY / oYres) + constantFOV;
            } else {
                oXdim = selectedImg.getExtents()[0];
                oYdim = selectedImg.getExtents()[1];
            }

            if ( (image.getNDims() >= 3) && ( !do25D) && (selectedImg.getNDims() >= 3)) {
                oZres = selectedImg.getFileInfo(0).getResolutions()[2];
                units[2] = selectedImg.getUnitsOfMeasure(2);
                if (preserveFOV) {
                    fovZ = iZres * (iZdim - constantFOV);
                    oZdim = Math.round(fovZ / oZres) + constantFOV;
                } else {
                    oZdim = selectedImg.getExtents()[2];
                }
            } else if (image.getNDims() >= 3) { // cannot change third dimension
                oZres = image.getFileInfo(0).getResolutions()[2];
                oZdim = image.getExtents()[2];
            }
        } else if (resampletoUser.isSelected()) {
            // Read X, Y and Z resolutions
            tmpStr = textResX.getText();

            if (JDialogBase.testParameter(tmpStr, 0, 1000000000)) {
                oXres = Float.valueOf(tmpStr).floatValue();
            } else {
                textResX.requestFocus();
                textResX.selectAll();

                return false;
            }

            tmpStr = textResY.getText();

            if (JDialogBase.testParameter(tmpStr, 0, 1000000000)) {
                oYres = Float.valueOf(tmpStr).floatValue();
            } else {
                textResY.requestFocus();
                textResY.selectAll();

                return false;
            }

            if (image.getNDims() >= 3) {
                tmpStr = textResZ.getText();

                if (JDialogBase.testParameter(tmpStr, 0, 1000000000)) {
                    oZres = Float.valueOf(tmpStr).floatValue();
                } else {
                    textResZ.requestFocus();
                    textResZ.selectAll();

                    return false;
                }
            }

            // Read X, Y and Z dimensions
            tmpStr = textDimX.getText();

            if (JDialogBase.testParameter(tmpStr, 0, 2048)) {
                oXdim = Integer.valueOf(tmpStr).intValue();
            } else {
                textDimX.requestFocus();
                textDimX.selectAll();

                return false;
            }

            tmpStr = textDimY.getText();

            if (JDialogBase.testParameter(tmpStr, 0, 2048)) {
                oYdim = Integer.valueOf(tmpStr).intValue();
            } else {
                textDimY.requestFocus();
                textDimY.selectAll();

                return false;
            }

            if (image.getNDims() >= 3) {
                tmpStr = textDimZ.getText();

                if (JDialogBase.testParameter(tmpStr, 0, 2048)) {
                    oZdim = Integer.valueOf(tmpStr).intValue();
                } else {
                    textDimZ.requestFocus();
                    textDimZ.selectAll();

                    return false;
                }
            }
        } else if (resampleSlider.isSelected()) { // resampleSlider
            factor = magSlider.getValue() / (float) 100;
            oXdim = Math.round(image.getExtents()[0] * factor);
            oYdim = Math.round(image.getExtents()[1] * factor);
            oXres = image.getFileInfo(0).getResolutions()[0] * (image.getExtents()[0] - constantFOV)
                    / (oXdim - constantFOV);
            oYres = image.getFileInfo(0).getResolutions()[1] * (image.getExtents()[1] - constantFOV)
                    / (oYdim - constantFOV);

            if ( (image.getNDims() >= 3) && ( !do25D)) {
                oZdim = Math.round(image.getExtents()[2] * factor);
                oZres = image.getFileInfo(0).getResolutions()[2] * (image.getExtents()[2] - constantFOV)
                        / (oZdim - constantFOV);
            } else if ( (image.getNDims() >= 3) && (do25D)) { // cannot change third dimension
                oZdim = image.getExtents()[2];
                oZres = image.getFileInfo(0).getResolutions()[2];
            }
        }
        // } //end if (noTransform.isSelected())

        // TRANSFORMATION MATRIX INFO

        if (fileMatrix.isSelected() && (spline == null)) { // read matrix from file

            readTransformMatrixFile(matrixFile);
            ((ViewJFrameImage) parentFrame).getUserInterface().setGlobalDataText(
                    "Matrix loaded from Transform dialog:\n");
            ((ViewJFrameImage) parentFrame).getUserInterface().setGlobalDataText(
                    reorientCoordSystem(fileTransMatrix).toString());
            xfrm.Mult(reorientCoordSystem(fileTransMatrix));
        } else if (userDefinedMatrix.isSelected()) { // user matrix
            spline = null;
            // user stuff
            tmpStr = textTx.getText();

            if (JDialogBase.testParameter(tmpStr, -2048, 2048)) {
                Tx = Double.valueOf(tmpStr).doubleValue();
            } else {
                textTx.requestFocus();
                textTx.selectAll();

                return false;
            }

            tmpStr = textTy.getText();

            if (JDialogBase.testParameter(tmpStr, -2048, 2048)) {
                Ty = Double.valueOf(tmpStr).doubleValue();
            } else {
                textTy.requestFocus();
                textTy.selectAll();

                return false;
            }

            tmpStr = textRz.getText();

            if (JDialogBase.testParameter(tmpStr, -360, 360)) {
                Rz = Double.valueOf(tmpStr).doubleValue();
            } else {
                textRz.requestFocus();
                textRz.selectAll();

                return false;
            }

            tmpStr = textSx.getText();

            if (JDialogBase.testParameter(tmpStr, 0, 300)) {
                Sx = Double.valueOf(tmpStr).doubleValue();
            } else {
                textSx.requestFocus();
                textSx.selectAll();

                return false;
            }

            tmpStr = textSy.getText();

            if (JDialogBase.testParameter(tmpStr, 0, 300)) {
                Sy = Double.valueOf(tmpStr).doubleValue();
            } else {
                textSy.requestFocus();
                textSy.selectAll();

                return false;
            }

            tmpStr = textSKx.getText();

            if (JDialogBase.testParameter(tmpStr, -2048, 2048)) {
                SKx = Double.valueOf(tmpStr).doubleValue();
            } else {
                textSKx.requestFocus();
                textSKx.selectAll();

                return false;
            }

            tmpStr = textSKy.getText();

            if (JDialogBase.testParameter(tmpStr, -2048, 2048)) {
                SKy = Double.valueOf(tmpStr).doubleValue();
            } else {
                textSKy.requestFocus();
                textSKy.selectAll();

                return false;
            }

            if ( (image.getNDims() >= 3) && ( !do25D)) {
                tmpStr = textTz.getText();

                if (JDialogBase.testParameter(tmpStr, -2048, 2048)) {
                    Tz = Double.valueOf(tmpStr).doubleValue();
                } else {
                    textTz.requestFocus();
                    textTz.selectAll();

                    return false;
                }

                tmpStr = textRx.getText();

                if (JDialogBase.testParameter(tmpStr, -360, 360)) {
                    Rx = Double.valueOf(tmpStr).doubleValue();
                } else {
                    textRx.requestFocus();
                    textRx.selectAll();

                    return false;
                }

                tmpStr = textRy.getText();

                if (JDialogBase.testParameter(tmpStr, -360, 360)) {
                    Ry = Double.valueOf(tmpStr).doubleValue();
                } else {
                    textRy.requestFocus();
                    textRy.selectAll();

                    return false;
                }

                tmpStr = textSz.getText();

                if (JDialogBase.testParameter(tmpStr, -2048, 2048)) {
                    Sz = Double.valueOf(tmpStr).doubleValue();
                } else {
                    textSz.requestFocus();
                    textSz.selectAll();

                    return false;
                }

                tmpStr = textSKz.getText();

                if (JDialogBase.testParameter(tmpStr, -2048, 2048)) {
                    SKz = Double.valueOf(tmpStr).doubleValue();
                } else {
                    textSKz.requestFocus();
                    textSKz.selectAll();

                    return false;
                }

                xfrm.setTranslate(Tx, Ty, Tz);
                xfrm.setRotate(Rx, Ry, Rz, TransMatrix.DEGREES);
                xfrm.setSkew(SKx, SKy, SKz);
                xfrm.setZoom(Sx, Sy, Sz);
            } else { // (image.getNDims() == 2) || (do25D)
                xfrm.setTranslate(Tx, Ty);
                xfrm.setRotate(Rz);
                xfrm.setSkew(SKx, SKy);
                xfrm.setZoom(Sx, Sy);
            }
        } // if (userDefinedMatrix.isSelected())
        else if (storedMatrix.isSelected()) { // use image's stored matrix
            spline = null;
            Preferences.debug("Image's stored matrix = \n" + image.getMatrix(), Preferences.DEBUG_SCRIPTING);

            if (image.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL)) {
                isSATransform = true;
            }

            TransMatrix imageMatrix = null;

            if (storedMatrixBox.getSelectedItem().equals("Composite")) {
                imageMatrix = image.getMatrix();
            } else {
                imageMatrix = image.getMatrixHolder().getMatrixMap().get(storedMatrixBox.getSelectedItem());
            }

            if ( !do25D) {
                xfrm.Mult(imageMatrix);
            } else {
                final TransMatrix xfrm25 = new TransMatrix(3);
                xfrm25.Set(0, 0, imageMatrix.Get(0, 0));
                xfrm25.Set(0, 1, imageMatrix.Get(0, 1));
                xfrm25.Set(0, 2, imageMatrix.Get(0, 3));
                xfrm25.Set(1, 0, imageMatrix.Get(1, 0));
                xfrm25.Set(1, 1, imageMatrix.Get(1, 1));
                xfrm25.Set(1, 2, imageMatrix.Get(1, 3));
                xfrm25.Set(2, 0, 0.0f);
                xfrm25.Set(2, 1, 0.0f);
                xfrm25.Set(2, 2, 1.0f);

                xfrm.Mult(xfrm25);
            }
        } else if (noTransform.isSelected()) { // no transform
            spline = null;
            xfrm.MakeIdentity();
        }

        // if ((no transformation) OR (user input transformation))
        // AND (total image size !=), then scale
        if (noTransform.isSelected() || userDefinedMatrix.isSelected()) {

            if ( (image.getNDims() >= 3) && ( !do25D)) {
                if ( ( (iXres * (iXdim - constantFOV)) != (oXres * (oXdim - constantFOV)))
                        || ( (iYres * (iYdim - constantFOV)) != (oYres * (oYdim - constantFOV)))
                        || ( (iZres * (iZdim - constantFOV)) != ( (oZdim - constantFOV) * oZres))) {
                    Sx = ( (oXdim - constantFOV) * oXres) / ( (iXdim - constantFOV) * iXres);
                    Sy = ( (oYdim - constantFOV) * oYres) / ( (iYdim - constantFOV) * iYres);
                    Sz = ( (oZdim - constantFOV) * oZres) / ( (iZdim - constantFOV) * iZres);
                    xfrm.setZoom(Sx, Sy, Sz);
                }
            } else { // ((image.getNDims() == 2) || (do25D))
                if ( ( (iXres * (iXdim - constantFOV)) != (oXres * (oXdim - constantFOV)))
                        || ( (iYres * (iYdim - constantFOV)) != (oYres * (oYdim - constantFOV)))) {
                    Sx = ( (oXdim - constantFOV) * oXres) / ( (iXdim - constantFOV) * iXres);
                    Sy = ( (oYdim - constantFOV) * oYres) / ( (iYdim - constantFOV) * iYres);
                    xfrm.setZoom(Sx, Sy);
                }

            }
        }

        if (image.getNDims() == 3) {
            Preferences.debug("oDim = " + oXdim + ", " + oYdim + ", " + oZdim, Preferences.DEBUG_SCRIPTING);
            Preferences.debug("oRes = " + oXres + ", " + oYres + ", " + oZres, Preferences.DEBUG_SCRIPTING);
        }

        Preferences.debug(xfrm + "\n", Preferences.DEBUG_SCRIPTING);

         if (tabbedPane.getSelectedComponent() == resamplePanel) {
             doRotateCenter = false;
         }
         else {
             doRotateCenter = rotCenter.isSelected();
         }
        if (doRotateCenter) {
            useSACenter = useSACenterBox.isSelected();
        }

        fillValue = Float.valueOf(valueText.getText()).floatValue();
        outOfBoundsIndex = outOfBoundsComboBox.getSelectedIndex();
        if (outOfBoundsIndex == 2) {
            // user defined value
            boolean success = testType(dataType, fillValue);
            if ( !success) {
                MipavUtil.displayError("User defined value is out of the data type range");
                valueText.requestFocus();
                valueText.selectAll();
                return false;
            }
        }

        return true;
    }

    /**
     * Determine if the value is in the image type range and within the float range since AlgorithmTransform does not
     * use double buffers.
     * 
     * @param type image type
     * @param value value tested
     * 
     * @return true if value is within acceptable range
     */
    private boolean testType(final int type, final float value) {

        if (type == ModelStorageBase.BOOLEAN) {

            if ( (value < 0) || (value > 1)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.BYTE) {

            if ( (value < -128) || (value > 127)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.UBYTE) {

            if ( (value < 0) || (value > 255)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.SHORT) {

            if ( (value < -32768) || (value > 32767)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.USHORT) {

            if ( (value < 0) || (value > 65535)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.INTEGER) {

            if ( (value < Integer.MIN_VALUE) || (value > Integer.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.UINTEGER) {

            if ( (value < 0) || (value > 4294967295L)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.LONG) {

            if ( (value < Long.MIN_VALUE) || (value > Long.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.FLOAT) {

            if ( (value < -Float.MAX_VALUE) || (value > Float.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.DOUBLE) {
            // Float buffers are used in the AlgorithmTransform routines
            if ( (value < -Float.MAX_VALUE) || (value > Float.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.ARGB) {

            if ( (value < 0) || (value > 255)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.ARGB_USHORT) {

            if ( (value < 0) || (value > 65535)) {
                return false;
            } else {
                return true;
            }
        } else if (type == ModelStorageBase.ARGB_FLOAT) {

            if ( (value < -Float.MAX_VALUE) || (value > Float.MAX_VALUE)) {
                return false;
            } else {
                return true;
            }
        } else {
            return false;
        }
    }

    /**
     * Return meta-information about this discoverable action for categorization and labeling purposes.
     * 
     * @return Metadata for this action.
     */
    public ActionMetadata getActionMetadata() {
        return new MipavActionMetadata() {
            public String getCategory() {
                return new String("Algorithms.Transformation tools");
            }

            public String getDescription() {
                return new String("Applies a linear transformation and/or rescaling to an image.");
            }

            public String getDescriptionLong() {
                return new String("Applies a linear transformation and/or rescaling to an image.");
            }

            public String getShortLabel() {
                return new String("Transform");
            }

            public String getLabel() {
                return new String("Transform");
            }

            public String getName() {
                return new String("Transform");
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

            table.put(new ParameterInt("interpolation_type", AlgorithmTransform.TRILINEAR));
            table.put(new ParameterInt("constant_fov", 1));
            table.put(new ParameterBoolean("do_transform_VOIs", false));
            table.put(new ParameterBoolean("do_clip_output", true));
            table.put(new ParameterBoolean("do_rotate_about_center", true));
            table.put(new ParameterBoolean("use_scanner_center", false));
            table.put(new ParameterBoolean("do_invert_matrix", false));

            final Parameter talParam = new ParameterBoolean("do_talairach_transform", false);
            table.put(talParam);

            // --- talairach-dependent section ---
            Parameter tempParam = new ParameterInt("transform_type");
            tempParam.setParentCondition(talParam, "true");
            table.put(tempParam);
            tempParam = new ParameterList("acpc_PC", Parameter.PARAM_FLOAT);
            tempParam.setParentCondition(talParam, "true");
            table.put(tempParam);
            tempParam = new ParameterFloat("acpc_res");
            tempParam.setParentCondition(talParam, "true");
            table.put(tempParam);
            tempParam = new ParameterList("orig_AC", Parameter.PARAM_FLOAT);
            tempParam.setParentCondition(talParam, "true");
            table.put(tempParam);
            tempParam = new ParameterList("orig_PC", Parameter.PARAM_FLOAT);
            tempParam.setParentCondition(talParam, "true");
            table.put(tempParam);
            tempParam = new ParameterList("orig_res", Parameter.PARAM_FLOAT);
            tempParam.setParentCondition(talParam, "true");
            table.put(tempParam);
            tempParam = new ParameterList("orig_dim", Parameter.PARAM_INT);
            tempParam.setParentCondition(talParam, "true");
            table.put(tempParam);
            for (int i = 0; i < 3; i++) {
                tempParam = new ParameterList("orig_orient_" + i, Parameter.PARAM_FLOAT);
                tempParam.setParentCondition(talParam, "true");
                table.put(tempParam);
            }
            // TODO: these params are also dependent on the transform_type param: ORIG_TO_TLRC, ACPC_TO_TLRC,
            // TLRC_TO_ORIG, TLRC_TO_ACPC
            tempParam = new ParameterList("acpc_min", Parameter.PARAM_FLOAT);
            tempParam.setParentCondition(talParam, "true");
            table.put(tempParam);
            tempParam = new ParameterList("acpc_max", Parameter.PARAM_FLOAT);
            tempParam.setParentCondition(talParam, "true");
            table.put(tempParam);
            tempParam = new ParameterList("tlrc_res", Parameter.PARAM_FLOAT);
            tempParam.setParentCondition(talParam, "true");
            table.put(tempParam);

            // --- non-talairach-dependent section ---
            tempParam = new ParameterBoolean(AlgorithmParameters.DO_PROCESS_3D_AS_25D, false);
            tempParam.setParentCondition(talParam, "false");
            table.put(tempParam);
            tempParam = new ParameterBoolean("do_update_origin", true);
            tempParam.setParentCondition(talParam, "false");
            table.put(tempParam);
            tempParam = new ParameterBoolean("do_pad", false);
            tempParam.setParentCondition(talParam, "false");
            table.put(tempParam);
            tempParam = new ParameterInt("out_of_bounds_index", 0);
            tempParam.setParentCondition(talParam, "false");
            table.put(tempParam);
            tempParam = new ParameterFloat("fill_value", 0.0f);
            tempParam.setParentCondition(table.getParameter("out_of_bounds_index"), "2");
            table.put(tempParam);

            // used for 2D/2.5D transforms and 3D+ resampling to user res/dim
            tempParam = new ParameterList("output_res", Parameter.PARAM_FLOAT);
            tempParam.setParentCondition(talParam, "false");
            table.put(tempParam);
            tempParam = new ParameterList("output_dim", Parameter.PARAM_INT);
            tempParam.setParentCondition(talParam, "false");
            table.put(tempParam);
            // used for 2D/2.5D transforms and 3D+ transform to user matrix
            for (int i = 0; i < 4; i++) {
                tempParam = new ParameterList("x_mat" + i, Parameter.PARAM_DOUBLE);
                tempParam.setParentCondition(talParam, "false");
                table.put(tempParam);
            }

            Parameter sourceParam = new ParameterString("transform_source", "none");
            sourceParam.setParentCondition(talParam, "false");
            table.put(sourceParam);

            // file transform
            // TODO: switch to ParameterFile
            tempParam = new ParameterString("transform_file");
            tempParam.setParentCondition(sourceParam, "file");
            table.put(tempParam);

            // user transform - no other params needed, uses x_mat

            // self transform - no other params needed

            // none transform - no other params needed

            // type of resampling
            sourceParam = new ParameterString("resample_type", "none");
            sourceParam.setParentCondition(talParam, "false");
            table.put(sourceParam);

            // reample to_image
            tempParam = new ParameterExternalImage(AlgorithmParameters.getInputImageLabel(2));
            tempParam.setParentCondition(sourceParam, "to_image");
            table.put(tempParam);
            tempParam = new ParameterBoolean("set_pixels", false);
            tempParam.setParentCondition(sourceParam, "to_image");
            table.put(tempParam);

            // resample to_user - also uses output_res, output_dim from above

            // resample to_factor
            tempParam = new ParameterFloat("resample_factor", 1.0f);
            tempParam.setParentCondition(sourceParam, "to_factor");
            table.put(tempParam);

            // resample none - no other params needed
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
                return getResultImage().getImageName();
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
