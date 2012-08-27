package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.*;
import gov.nih.mipav.view.renderer.WildMagic.Interface.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * Dialog to get the upper and lower bounds for the region grow used by the paint functions.
 *
 * @version  1.0 Nov 9, 1999
 * @author   Matthew J. McAuliffe, Ph.D.
 * @see      PaintGrowListener
 */
public class JDialogPaintGrow extends JDialogBase implements RegionGrowDialog, ChangeListener, KeyListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -165750503127683051L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    protected JCheckBox constrainBoundsCheckbox;

    /** DOCUMENT ME! */
    protected JLabel currentFuzzy;

    /** DOCUMENT ME! */
    protected boolean displayFuzzy;

    /** DOCUMENT ME! */
    protected JCheckBox distanceCheckbox;

    /** DOCUMENT ME! */
    protected boolean enableVariableCheckbox;

    /** DOCUMENT ME! */
    protected Frame frame;

    /** DOCUMENT ME! */
    protected JCheckBox fuzzyCheckbox;

    /** DOCUMENT ME! */
    protected JCheckBox fuzzyImageCheckbox;

    /** DOCUMENT ME! */
    protected JSlider fuzzySlider;

    /** DOCUMENT ME! */
    protected float fuzzyThreshold;

    /** DOCUMENT ME! */
    protected boolean haveBlue;

    /** DOCUMENT ME! */
    protected boolean haveColor;

    /** DOCUMENT ME! */
    protected boolean haveGreen;

    /** DOCUMENT ME! */
    protected boolean haveRed;

    /** DOCUMENT ME! */
    protected int initialDelta;

    /** DOCUMENT ME! */
    protected int initialDeltaB;

    /** DOCUMENT ME! */
    protected int initialDeltaG;

    /** DOCUMENT ME! */
    protected int initialDeltaR;

    /** DOCUMENT ME! */
    protected String leadString = null;

    /** DOCUMENT ME! */
    protected float less, more;

    /** DOCUMENT ME! */
    protected JLabel lowerMid;

    /** DOCUMENT ME! */
    protected JLabel lowerMidB;

    /** DOCUMENT ME! */
    protected JLabel lowerMidG;

    /** DOCUMENT ME! */
    protected JLabel lowerMidR;

    /** DOCUMENT ME! */
    protected JSlider lowSlider = null;

    /** DOCUMENT ME! */
    protected JSlider lowSliderB = null;

    /** DOCUMENT ME! */
    protected JTextField lowSliderField;

    /** DOCUMENT ME! */
    protected JTextField lowSliderFieldB;

    /** DOCUMENT ME! */
    protected JTextField lowSliderFieldG;

    /** DOCUMENT ME! */
    protected JTextField lowSliderFieldR;

    /** DOCUMENT ME! */
    protected JSlider lowSliderG = null;

    /** DOCUMENT ME! */
    protected JSlider lowSliderR = null;

    /** DOCUMENT ME! */
    protected float max;

    /** DOCUMENT ME! */
    protected float maxB;

    /** DOCUMENT ME! */
    protected JLabel maxDistLabel;

    /** DOCUMENT ME! */
    protected JTextField maxDistTextF;

    /** DOCUMENT ME! */
    protected float maxG;

    /** DOCUMENT ME! */
    protected JLabel maximumFuzzy;

    /** DOCUMENT ME! */
    protected JLabel maximumL;

    /** DOCUMENT ME! */
    protected JLabel maximumLB;

    /** DOCUMENT ME! */
    protected JLabel maximumLG;

    /** DOCUMENT ME! */
    protected JLabel maximumLR;

    /** DOCUMENT ME! */
    protected JLabel maximumU;

    /** DOCUMENT ME! */
    protected JLabel maximumUB;

    /** DOCUMENT ME! */
    protected JLabel maximumUG;

    /** DOCUMENT ME! */
    protected JLabel maximumUR;

    /** DOCUMENT ME! */
    protected double maximumValue;

    /** DOCUMENT ME! */
    protected JButton maximumValueButton;

    /** DOCUMENT ME! */
    protected JTextField maximumValueField;

    /** DOCUMENT ME! */
    protected float maxR;

    /** DOCUMENT ME! */
    protected JLabel maxSizeLabel;

    /** DOCUMENT ME! */
    protected JTextField maxSizeTextF;

    /** DOCUMENT ME! */
    protected double maxValue, minValue; // minimum and maximum values in the image

    /** DOCUMENT ME! */
    protected double maxValueB, minValueB;

    /** DOCUMENT ME! */
    protected double maxValueG, minValueG;

    /** DOCUMENT ME! */
    protected double maxValueR, minValueR;

    /** DOCUMENT ME! */
    protected float min;

    /** DOCUMENT ME! */
    protected float minB;

    /** DOCUMENT ME! */
    protected float minG;

    /** DOCUMENT ME! */
    protected JLabel minimumFuzzy;

    /** DOCUMENT ME! */
    protected JLabel minimumL;

    /** DOCUMENT ME! */
    protected JLabel minimumLB;

    /** DOCUMENT ME! */
    protected JLabel minimumLG;

    /** DOCUMENT ME! */
    protected JLabel minimumLR;

    /** DOCUMENT ME! */
    protected JLabel minimumU;

    /** DOCUMENT ME! */
    protected JLabel minimumUB;

    /** DOCUMENT ME! */
    protected JLabel minimumUG;

    /** DOCUMENT ME! */
    protected JLabel minimumUR;

    /** DOCUMENT ME! */
    protected float minR;

    /** DOCUMENT ME! */
    protected Vector<PaintGrowListener> paintGrowListeners;

    /** DOCUMENT ME! */
    protected JTextField posTextF;

    /** DOCUMENT ME! */
    protected double range; // maxValue - minValue

    /** DOCUMENT ME! */
    protected double rangeB;

    /** DOCUMENT ME! */
    protected double rangeG;

    /** DOCUMENT ME! */
    protected double rangeR;

    /** DOCUMENT ME! */
    protected AlgorithmRegionGrow regionGrowAlgo = null;

    /** DOCUMENT ME! */
    protected boolean setRadioBoth = false;

    /** DOCUMENT ME! */
    protected JCheckBox sizeCheckbox;

    /** DOCUMENT ME! */
    protected JLabel upperMid;

    /** DOCUMENT ME! */
    protected JLabel upperMidB;

    /** DOCUMENT ME! */
    protected JLabel upperMidG;

    /** DOCUMENT ME! */
    protected JLabel upperMidR;

    /** DOCUMENT ME! */
    protected JSlider upSlider = null;

    /** DOCUMENT ME! */
    protected JSlider upSliderB = null;

    /** DOCUMENT ME! */
    protected JTextField upSliderField;

    /** DOCUMENT ME! */
    protected JTextField upSliderFieldB;

    /** DOCUMENT ME! */
    protected JTextField upSliderFieldG;

    /** DOCUMENT ME! */
    protected JTextField upSliderFieldR;

    /** DOCUMENT ME! */
    protected JSlider upSliderG = null;

    /** DOCUMENT ME! */
    protected JSlider upSliderR = null;

    /** DOCUMENT ME! */
    protected boolean useVOI;

    /** DOCUMENT ME! */
    protected JCheckBox variableDeltasCheckbox;

    /** DOCUMENT ME! */
    protected boolean variableThresholds = false;

    /** DOCUMENT ME! */
    protected JCheckBox voiCheckbox;

    /** DOCUMENT ME! */
    private boolean disableSliders = false;

    /** DOCUMENT ME! */
    private boolean disableSlidersB = false;

    /** DOCUMENT ME! */
    private boolean disableSlidersG = false;

    /** DOCUMENT ME! */
    private boolean disableSlidersR = false;

    /** DOCUMENT ME! */
    private Font font12B = MipavUtil.font12B;

    /** DOCUMENT ME! */
    private int imageType = -1;

    /** DOCUMENT ME! */
    private boolean lowSetFromField = false;

    /** DOCUMENT ME! */
    private boolean lowSetFromFieldB = false;

    /** DOCUMENT ME! */
    private boolean lowSetFromFieldG = false;

    /** DOCUMENT ME! */
    private boolean lowSetFromFieldR = false;

    /** DOCUMENT ME! */
    private JTabbedPane tabbedPane;

    /** DOCUMENT ME! */
    private boolean upSetFromField = false;

    /** DOCUMENT ME! */
    private boolean upSetFromFieldB = false;

    /** DOCUMENT ME! */
    private boolean upSetFromFieldG = false;

    /** DOCUMENT ME! */
    private boolean upSetFromFieldR = false;

    /** SurfacePaint reference */
    private SurfacePaint surfacePaint = null;
    /** SurfacePaint reference */
    private SurfacePaint_WM surfacePaint_WM = null;


    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog and sets up GUI components.
     *
     * @param  theParentFrame  Parent frame.
     * @param  paintListeners  DOCUMENT ME!
     */
    public JDialogPaintGrow(Frame theParentFrame, Vector<PaintGrowListener> paintListeners) {
        super(theParentFrame, false);
        paintGrowListeners = paintListeners;
        frame = theParentFrame;
        init();
    }

    /**
     * Creates new dialog and sets up GUI components.
     *
     * @param  theParentFrame  Parent frame.
     * @param  paintListeners  DOCUMENT ME!
     * @param  isVisible       Dialog visible or not.
     */
    public JDialogPaintGrow(Frame theParentFrame, Vector<PaintGrowListener> paintListeners, boolean isVisible) {
        super(theParentFrame, false);
        paintGrowListeners = paintListeners;
        frame = theParentFrame;
    }

    /**
     * Creates new dialog and sets up GUI components.
     *
     * @param  theParentFrame  Parent frame (the frame with the component image this dialog should perform region grows
     *                         on).
     * @param  attachedFrame   the frame this dialog should be attached to
     * @param  paintListeners  DOCUMENT ME!
     */
    public JDialogPaintGrow(Frame theParentFrame, Frame attachedFrame, Vector<PaintGrowListener> paintListeners) {
        super(attachedFrame, false);
        paintGrowListeners = paintListeners;
        frame = theParentFrame;
        init();
    }

    /**
     * Creates new dialog and sets up GUI components.
     *
     * @param  theParentFrame  Parent frame.
     * @param  paintListeners  DOCUMENT ME!
     * @param  setRadioBoth    enabled the both button option on frame
     * @param  leadString      DOCUMENT ME!
     */
    public JDialogPaintGrow(Frame theParentFrame, Vector<PaintGrowListener> paintListeners, boolean setRadioBoth, String leadString) {
        super(theParentFrame, false);
        paintGrowListeners = paintListeners;
        frame = theParentFrame;
        this.setRadioBoth = setRadioBoth;
        this.leadString = leadString;
        init();
    }

    /**
     * Creates new dialog and sets up GUI components.
     *
     * @param  theParentFrame  Parent frame.
     * @param  surfacePaint for Painting on a surface
     * @param  paintListeners  DOCUMENT ME!
     */
    public JDialogPaintGrow(Frame theParentFrame, SurfacePaint surfacePaint, Vector<PaintGrowListener> paintListeners)
    {
        super(theParentFrame, false);
        paintGrowListeners = paintListeners;
        frame = theParentFrame;
        this.surfacePaint = surfacePaint;
        init();
    }
    /**
     * Creates new dialog and sets up GUI components.
     *
     * @param  theParentFrame  Parent frame.
     * @param  surfacePaint for Painting on a surface
     * @param  paintListeners  DOCUMENT ME!
     */
    public JDialogPaintGrow(Frame theParentFrame, SurfacePaint_WM surfacePaint, Vector<PaintGrowListener> paintListeners)
    {
        super(theParentFrame, false);
        paintGrowListeners = paintListeners;
        frame = theParentFrame;
        this.surfacePaint_WM = surfacePaint;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Sets parameters in PaintGrowListener when Apply is pressed. Closes dialog box in response to both Apply and
     * Cancel buttons.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String tmpStr;
        int maxS;

        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("Help")) {
            //MipavUtil.showHelp("PaintGrow001");
            MipavUtil.showWebHelp("Creating_a_mask_using_the_Paint_Grow_Segmentation_method");
        }
        if (command.equals("Close") || command.equals("Apply")) {

            if (sizeCheckbox.isSelected()) { // unrestricted size
                maxS = -1;
            } else {
                tmpStr = maxSizeTextF.getText();

                if (testParameter(tmpStr, 1, 10000000)) {
                    maxS = Integer.valueOf(tmpStr).intValue();
                } else {
                    maxSizeTextF.requestFocus();
                    maxSizeTextF.selectAll();
                    maxS = -2;
                }
            }

            if (frame instanceof ViewJFrameImage) {
                ((ViewJFrameImage) frame).getComponentImage().setSizeLimit(maxS);
            } else if (frame instanceof ViewJFramePaintVasculature) {
                ((ViewJFramePaintVasculature) frame).getComponentImage().setSizeLimit(maxS);
            } else if (frame instanceof ViewJFrameTriImage) {
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setSizeLimit(maxS);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setSizeLimit(maxS);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setSizeLimit(maxS);
            }

            if (distanceCheckbox.isSelected()) { // unrestricted distance
                maxS = -1;
            } else {
                tmpStr = maxDistTextF.getText();

                if (testParameter(tmpStr, 1, 10000000)) {
                    maxS = Integer.valueOf(tmpStr).intValue();
                } else {
                    maxDistTextF.requestFocus();
                    maxDistTextF.selectAll();
                    maxS = -2;
                }
            }

            variableThresholds = variableDeltasCheckbox.isSelected();

            if (frame instanceof ViewJFrameImage) {
                ((ViewJFrameImage) frame).getComponentImage().setMaxDistance(maxS);

                if (!haveColor) {
                    ((ViewJFrameImage) frame).getComponentImage().setLess(lowSlider.getValue() / (float) 100);

                    ((ViewJFrameImage) frame).getComponentImage().setMore(upSlider.getValue() / (float) 100);
                }

                if (haveRed) {
                    ((ViewJFrameImage) frame).getComponentImage().setLessR(lowSliderR.getValue() / (float) 100);

                    ((ViewJFrameImage) frame).getComponentImage().setMoreR(upSliderR.getValue() / (float) 100);
                }

                if (haveGreen) {
                    ((ViewJFrameImage) frame).getComponentImage().setLessG(lowSliderG.getValue() / (float) 100);

                    ((ViewJFrameImage) frame).getComponentImage().setMoreG(upSliderG.getValue() / (float) 100);
                }

                if (haveBlue) {
                    ((ViewJFrameImage) frame).getComponentImage().setLessB(lowSliderB.getValue() / (float) 100);

                    ((ViewJFrameImage) frame).getComponentImage().setMoreB(upSliderB.getValue() / (float) 100);
                }

                ((ViewJFrameImage) frame).getComponentImage().setVariableThresholds(variableThresholds);
            } else if (frame instanceof ViewJFramePaintVasculature) {
                ((ViewJFramePaintVasculature) frame).getComponentImage().setMaxDistance(maxS);

                if (!haveColor) {
                    ((ViewJFramePaintVasculature) frame).getComponentImage().setLess(lowSlider.getValue() /
                                                                                         (float) 100);

                    ((ViewJFramePaintVasculature) frame).getComponentImage().setMore(upSlider.getValue() / (float) 100);
                }

                if (haveRed) {
                    ((ViewJFramePaintVasculature) frame).getComponentImage().setLessR(lowSliderR.getValue() /
                                                                                          (float) 100);

                    ((ViewJFramePaintVasculature) frame).getComponentImage().setMoreR(upSliderR.getValue() /
                                                                                          (float) 100);
                }

                if (haveGreen) {
                    ((ViewJFramePaintVasculature) frame).getComponentImage().setLessG(lowSliderG.getValue() /
                                                                                          (float) 100);

                    ((ViewJFramePaintVasculature) frame).getComponentImage().setMoreG(upSliderG.getValue() /
                                                                                          (float) 100);
                }

                if (haveBlue) {
                    ((ViewJFramePaintVasculature) frame).getComponentImage().setLessB(lowSliderB.getValue() /
                                                                                          (float) 100);

                    ((ViewJFramePaintVasculature) frame).getComponentImage().setMoreB(upSliderB.getValue() /
                                                                                          (float) 100);
                }

                ((ViewJFramePaintVasculature) frame).getComponentImage().setVariableThresholds(variableThresholds);
            } else if (frame instanceof ViewJFrameTriImage) {
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setMaxDistance(maxS);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setMaxDistance(maxS);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setMaxDistance(maxS);

                if (!haveColor) {
                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setLess(lowSlider.getValue() /
                                                                                                     (float) 100);
                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setLess(lowSlider.getValue() /
                                                                                                       (float) 100);
                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setLess(lowSlider.getValue() /
                                                                                                        (float) 100);

                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setMore(upSlider.getValue() /
                                                                                                     (float) 100);
                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setMore(upSlider.getValue() /
                                                                                                       (float) 100);
                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setMore(upSlider.getValue() /
                                                                                                        (float) 100);
                }

                if (haveRed) {
                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setLessR(lowSliderR.getValue() /
                                                                                                      (float) 100);
                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setLessR(lowSliderR.getValue() /
                                                                                                        (float) 100);
                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setLessR(lowSliderR.getValue() /
                                                                                                         (float) 100);

                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setMoreR(upSliderR.getValue() /
                                                                                                      (float) 100);
                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setMoreR(upSliderR.getValue() /
                                                                                                        (float) 100);
                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setMoreR(upSliderR.getValue() /
                                                                                                         (float) 100);
                }

                if (haveGreen) {
                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setLessG(lowSliderG.getValue() /
                                                                                                      (float) 100);
                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setLessG(lowSliderG.getValue() /
                                                                                                        (float) 100);
                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setLessG(lowSliderG.getValue() /
                                                                                                         (float) 100);

                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setMoreG(upSliderG.getValue() /
                                                                                                      (float) 100);
                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setMoreG(upSliderG.getValue() /
                                                                                                        (float) 100);
                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setMoreG(upSliderG.getValue() /
                                                                                                         (float) 100);
                }

                if (haveBlue) {
                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setLessB(lowSliderB.getValue() /
                                                                                                      (float) 100);
                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setLessB(lowSliderB.getValue() /
                                                                                                        (float) 100);
                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setLessB(lowSliderB.getValue() /
                                                                                                         (float) 100);

                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setMoreB(upSliderB.getValue() /
                                                                                                      (float) 100);
                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setMoreB(upSliderB.getValue() /
                                                                                                        (float) 100);
                    ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setMoreB(upSliderB.getValue() /
                                                                                                         (float) 100);
                }

                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setVariableThresholds(variableThresholds);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setVariableThresholds(variableThresholds);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setVariableThresholds(variableThresholds);
            } // else if (frame instanceof ViewJFrameTriImage)

            if (fuzzyCheckbox.isSelected()) {
                fuzzyThreshold = fuzzySlider.getValue() / 1000.0f;
            } else {
                fuzzyThreshold = -1;
            }

            if (frame instanceof ViewJFrameImage) {
                ((ViewJFrameImage) frame).getComponentImage().setFuzzyThreshold(fuzzyThreshold);
            } else if (frame instanceof ViewJFramePaintVasculature) {
                ((ViewJFramePaintVasculature) frame).getComponentImage().setFuzzyThreshold(fuzzyThreshold);
            } else if (frame instanceof ViewJFrameTriImage) {
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setFuzzyThreshold(fuzzyThreshold);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setFuzzyThreshold(fuzzyThreshold);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setFuzzyThreshold(fuzzyThreshold);
            }

            if (voiCheckbox.isSelected()) {
                useVOI = true;
            } else {
                useVOI = false;
            }

            if (frame instanceof ViewJFrameImage) {
                ((ViewJFrameImage) frame).getComponentImage().setUseVOI(useVOI);
            } else if (frame instanceof ViewJFramePaintVasculature) {
                ((ViewJFramePaintVasculature) frame).getComponentImage().setUseVOI(useVOI);
            } else if (frame instanceof ViewJFrameTriImage) {
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setUseVOI(useVOI);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setUseVOI(useVOI);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setUseVOI(useVOI);
            }

            if (fuzzyImageCheckbox.isSelected()) {
                displayFuzzy = true;
            } else {
                displayFuzzy = false;
            }

            if (frame instanceof ViewJFrameImage) {
                ((ViewJFrameImage) frame).getComponentImage().setDisplayFuzzy(displayFuzzy);
                ((ViewJFrameImage) frame).getControls().getTools().setPointerSelected();
                ((ViewJFrameImage) frame).getComponentImage().setCursorMode(ViewJComponentEditImage.DEFAULT);
                // ((ViewJFrameImage)frame).getComponentImage().growDialog = null;
                resetDialogs();
            } else if (frame instanceof ViewJFramePaintVasculature) {
                ((ViewJFramePaintVasculature) frame).getComponentImage().setDisplayFuzzy(displayFuzzy);
                ((ViewJFramePaintVasculature) frame).getControls().getTools().setPointerSelected();
                ((ViewJFramePaintVasculature) frame).getComponentImage().setCursorMode(ViewJComponentEditImage.DEFAULT);
                // ((ViewJFramePaintVasculature)frame).getComponentImage().growDialog = null;
                resetDialogs();
            } else if (frame instanceof ViewJFrameTriImage) {
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setDisplayFuzzy(displayFuzzy);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setDisplayFuzzy(displayFuzzy);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setDisplayFuzzy(displayFuzzy);
                ((ViewJFrameTriImage) frame).setTraverseButton();
                resetDialogs();
            }

            dispose();
        } else if (source == sizeCheckbox) {

            if (sizeCheckbox.isSelected()) {
                maxSizeLabel.setEnabled(false);
                maxSizeTextF.setEnabled(false);
                maxSizeTextF.setText("  ");
            } else {
                maxSizeLabel.setEnabled(true);
                maxSizeTextF.setEnabled(true);
                maxSizeTextF.setText(String.valueOf(1));
            }
        } else if (source == distanceCheckbox) {

            if (distanceCheckbox.isSelected()) {
                maxDistLabel.setEnabled(false);
                maxDistTextF.setEnabled(false);
                maxDistTextF.setText("  ");
            } else {
                maxDistLabel.setEnabled(true);
                maxDistTextF.setEnabled(true);
                maxDistTextF.setText(String.valueOf(1));
            }
        } else if (source == fuzzyCheckbox) {

            if (fuzzyCheckbox.isSelected()) {
                voiCheckbox.setEnabled(true);
                fuzzySlider.setEnabled(true);
                minimumFuzzy.setEnabled(true);
                currentFuzzy.setEnabled(true);
                maximumFuzzy.setEnabled(true);
                fuzzyImageCheckbox.setEnabled(true);

                if (!haveColor) {
                    upSlider.setEnabled(false);
                    upperMid.setEnabled(false);
                    maximumU.setEnabled(false);
                    minimumU.setEnabled(false);
                    lowSlider.setEnabled(false);
                    lowerMid.setEnabled(false);
                    maximumL.setEnabled(false);
                    minimumL.setEnabled(false);
                    maximumValueButton.setEnabled(false);
                    maximumValueField.setEnabled(false);
                } // if (!haveColor)

                if (haveRed) {
                    upSliderR.setEnabled(false);
                    upperMidR.setEnabled(false);
                    maximumUR.setEnabled(false);
                    minimumUR.setEnabled(false);
                    lowSliderR.setEnabled(false);
                    lowerMidR.setEnabled(false);
                    maximumLR.setEnabled(false);
                    minimumLR.setEnabled(false);
                } // if (haveRed)

                if (haveGreen) {
                    upSliderG.setEnabled(false);
                    upperMidG.setEnabled(false);
                    maximumUG.setEnabled(false);
                    minimumUG.setEnabled(false);
                    lowSliderG.setEnabled(false);
                    lowerMidG.setEnabled(false);
                    maximumLG.setEnabled(false);
                    minimumLG.setEnabled(false);
                } // if (haveGreen)

                if (haveBlue) {
                    upSliderB.setEnabled(false);
                    upperMidB.setEnabled(false);
                    maximumUB.setEnabled(false);
                    minimumUB.setEnabled(false);
                    lowSliderB.setEnabled(false);
                    lowerMidB.setEnabled(false);
                    maximumLB.setEnabled(false);
                    minimumLB.setEnabled(false);
                } // if (haveBlue)

                sizeCheckbox.setEnabled(false);
                maxSizeLabel.setEnabled(false);
                maxSizeTextF.setEnabled(false);
                distanceCheckbox.setEnabled(false);
                maxDistLabel.setEnabled(false);
                maxDistTextF.setEnabled(false);
                constrainBoundsCheckbox.setEnabled(false);
                variableDeltasCheckbox.setEnabled(false);
            } else {
                voiCheckbox.setEnabled(false);
                fuzzySlider.setEnabled(false);
                minimumFuzzy.setEnabled(false);
                currentFuzzy.setEnabled(false);
                maximumFuzzy.setEnabled(false);
                fuzzyImageCheckbox.setEnabled(false);

                if (!haveColor) {
                    upSlider.setEnabled(true);
                    upperMid.setEnabled(true);
                    maximumU.setEnabled(true);
                    minimumU.setEnabled(true);
                    lowSlider.setEnabled(true);
                    lowerMid.setEnabled(true);
                    maximumL.setEnabled(true);
                    minimumL.setEnabled(true);
                    maximumValueButton.setEnabled(true);
                    maximumValueField.setEnabled(true);
                } // if (!haveColor)

                if (haveRed) {
                    upSliderR.setEnabled(true);
                    upperMidR.setEnabled(true);
                    maximumUR.setEnabled(true);
                    minimumUR.setEnabled(true);
                    lowSliderR.setEnabled(true);
                    lowerMidR.setEnabled(true);
                    maximumLR.setEnabled(true);
                    minimumLR.setEnabled(true);
                } // if (haveRed)

                if (haveGreen) {
                    upSliderG.setEnabled(true);
                    upperMidG.setEnabled(true);
                    maximumUG.setEnabled(true);
                    minimumUG.setEnabled(true);
                    lowSliderG.setEnabled(true);
                    lowerMidG.setEnabled(true);
                    maximumLG.setEnabled(true);
                    minimumLG.setEnabled(true);
                } // if (haveGreen)

                if (haveBlue) {
                    upSliderB.setEnabled(true);
                    upperMidB.setEnabled(true);
                    maximumUB.setEnabled(true);
                    minimumUB.setEnabled(true);
                    lowSliderB.setEnabled(true);
                    lowerMidB.setEnabled(true);
                    maximumLB.setEnabled(true);
                    minimumLB.setEnabled(true);
                } // if (haveBlue)

                if (frame instanceof ViewJFrameTriImage) {
                    constrainBoundsCheckbox.setEnabled(true);
                }

                sizeCheckbox.setEnabled(true);

                if (sizeCheckbox.isSelected()) {
                    maxSizeLabel.setEnabled(false);
                    maxSizeTextF.setEnabled(false);
                    maxSizeTextF.setText("  ");
                } else {
                    maxSizeLabel.setEnabled(true);
                    maxSizeTextF.setEnabled(true);
                    maxSizeTextF.setText(String.valueOf(1));
                }

                distanceCheckbox.setEnabled(true);

                if (distanceCheckbox.isSelected()) {
                    maxDistLabel.setEnabled(false);
                    maxDistTextF.setEnabled(false);
                    maxDistTextF.setText("  ");
                } else {
                    maxDistLabel.setEnabled(true);
                    maxDistTextF.setEnabled(true);
                    maxDistTextF.setText(String.valueOf(1));
                }

                variableDeltasCheckbox.setEnabled(enableVariableCheckbox);
            }
        } // else if (source == fuzzyCheckbox)
        else if ((maximumValueButton != null) && (source == maximumValueButton)) {
            tmpStr = maximumValueField.getText();

            // The (int)max passed to upSlider.setMaximum must not exceed the
            // maximum integer value of +2,147,483,647, so the maximumValue
            // expressed by the slider scale must not exceed 21,474,836
            if (testParameter(tmpStr, 1, Math.min(range, Integer.MAX_VALUE / 100.0))) {
                maximumValue = Double.valueOf(tmpStr).doubleValue();
                max = (float) (100.0 * maximumValue);
                upSlider.setMaximum((int) max);
                upSlider.setMajorTickSpacing((int) (max - min) / 10);
                maximumU.setText(String.valueOf((upSlider.getMaximum()) / 100.0f));

                float mid = upSlider.getMaximum() / 200.0f;

                if (mid >= 100) {
                    upperMid.setText("       " + mid);
                } else {
                    upperMid.setText("     " + mid);
                }

                lowSlider.setMaximum((int) max);
                lowSlider.setMajorTickSpacing((int) (max - min) / 10);
                maximumL.setText(String.valueOf((lowSlider.getMaximum()) / 100.0f));
                mid = lowSlider.getMaximum() / 200.0f;

                if (mid >= 100) {
                    lowerMid.setText("       " + mid);
                } else {
                    lowerMid.setText("     " + mid);
                }
            }
        } else { // else if ((maximumValueButton != null) && (source == maximumValueButton))
            super.actionPerformed(event);
        }
        
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    public boolean boundsConstrained() {
        return constrainBoundsCheckbox.isSelected() && constrainBoundsCheckbox.isEnabled();
    }

    /**
     * Accessor that returns displayFuzzy.
     *
     * @return  displayFuzzy
     */
    public boolean getDisplayFuzzy() {

        if (fuzzyImageCheckbox.isSelected()) {
            displayFuzzy = true;
        } else {
            displayFuzzy = false;
        }

        return displayFuzzy;
    }

    /**
     * Accessor that returns the fuzzy threshold.
     *
     * @return  fuzzy threshold
     */
    public float getFuzzyThreshold() {

        if (fuzzyCheckbox.isSelected()) {
            fuzzyThreshold = fuzzySlider.getValue() / 1000.0f;
        } else {
            fuzzyThreshold = -1;
        }

        return fuzzyThreshold;
    }

    /**
     * Accessor to the lower bound slider.
     *
     * @return  The value to be subtracted to the seed intensity the defines the lower intensity value that the region
     *          can grow into.
     */
    public float getLowerBound() {
        return lowSlider.getValue() / (float) 100;
    }

    /**
     * Accessor to the lower bound blue slider.
     *
     * @return  The value to be subtracted to the seed blue intensity the defines the lower blue intensity value that
     *          the region can grow into.
     */
    public float getLowerBoundB() {

        if (lowSliderB != null) {
            return lowSliderB.getValue() / (float) 100;
        } else {
            return 0.0f;
        }
    }

    /**
     * Accessor to the lower bound green slider.
     *
     * @return  The value to be subtracted to the seed green intensity the defines the lower green intensity value that
     *          the region can grow into.
     */
    public float getLowerBoundG() {

        if (lowSliderG != null) {
            return lowSliderG.getValue() / (float) 100;
        } else {
            return 0.0f;
        }
    }

    /**
     * Accessor to the lower bound red slider.
     *
     * @return  The value to be subtracted to the seed red intensity the defines the lower red intensity value that the
     *          region can grow into.
     */
    public float getLowerBoundR() {

        if (lowSliderR != null) {
            return lowSliderR.getValue() / (float) 100;
        } else {
            return 0.0f;
        }
    }

    /**
     * Accessor that returns the maximum distance from the seed point to a point in the object.
     *
     * @return  The maximum distance from the seed point to the object in units of the image.
     */
    public int getMaxDistance() {

        String tmpStr;
        int maxS;

        if (distanceCheckbox.isSelected()) { // unrestricted distance
            maxS = -1;
        } else {
            tmpStr = maxDistTextF.getText();

            if (testParameter(tmpStr, 1, 10000000)) {
                maxS = Integer.valueOf(tmpStr).intValue();
            } else {
                maxDistTextF.requestFocus();
                maxDistTextF.selectAll();
                maxS = -2;
            }
        }

        return maxS;
    }

    /**
     * Accessor that returns the maximum size of the object.
     *
     * @return  The maximum size of the object in units of the image.
     */
    public int getMaxSize() {

        String tmpStr;
        int maxS;

        if (sizeCheckbox.isSelected()) { // unrestricted size
            maxS = -1;
        } else {
            tmpStr = maxSizeTextF.getText();

            if (testParameter(tmpStr, 1, 10000000)) {
                maxS = Integer.valueOf(tmpStr).intValue();
            } else {
                maxSizeTextF.requestFocus();
                maxSizeTextF.selectAll();
                maxS = -2;
            }
        }

        return maxS;
    }

    /**
     * Accessor to the upper bound slider.
     *
     * @return  The value to be added to the seed intensity the defines the upper intensity value that the region can
     *          grow into.
     */
    public float getUpperBound() {
        return upSlider.getValue() / (float) 100;
    }

    /**
     * Accessor to the upper bound blue slider.
     *
     * @return  The value to be added to the seed blue intensity the defines the upper blue intensity value that the
     *          region can grow into.
     */
    public float getUpperBoundB() {

        if (upSliderB != null) {
            return upSliderB.getValue() / (float) 100;
        } else {
            return 0.0f;
        }
    }

    /**
     * Accessor to the upper bound green slider.
     *
     * @return  The value to be added to the seed green intensity the defines the upper green intensity value that the
     *          region can grow into.
     */
    public float getUpperBoundG() {

        if (upSliderG != null) {
            return upSliderG.getValue() / (float) 100;
        } else {
            return 0.0f;
        }
    }

    /**
     * Accessor to the upper bound red slider.
     *
     * @return  The value to be added to the seed red intensity the defines the upper red intensity value that the
     *          region can grow into.
     */
    public float getUpperBoundR() {

        if (upSliderR != null) {
            return upSliderR.getValue() / (float) 100;
        } else {
            return 0.0f;
        }
    }

    /**
     * Accessor that returns useVOI.
     *
     * @return  useVOI
     */
    public boolean getUseVOI() {

        if (voiCheckbox.isSelected()) {
            useVOI = true;
        } else {
            useVOI = false;
        }

        return useVOI;
    }

    /**
     * Accessor that tells if the thresholds vary with region growth.
     *
     * @return  variableThresholds
     */
    public boolean getVariableThresholds() {
        return variableDeltasCheckbox.isSelected();
    }

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void keyPressed(KeyEvent e) {
        int keyCode = e.getKeyCode();

        // int modifiers = e.getModifiers();
        if ((upSlider != null) && (e.getSource() == upSlider)) {

            if (keyCode == KeyEvent.VK_RIGHT) {

                // System.err.println("Source is upslider, keypress right");
                if (upSlider.getMaximum() >= 1000) {
                    upSlider.setValue(upSlider.getValue() + 100);
                } else {
                    upSlider.setValue(upSlider.getValue() + 1);
                }
            } else if (keyCode == KeyEvent.VK_LEFT) {

                if (upSlider.getMaximum() >= 1000) {
                    upSlider.setValue(upSlider.getValue() - 100);
                } else {
                    upSlider.setValue(upSlider.getValue() - 1);
                }
            }
        } else if ((upSliderR != null) && (e.getSource() == upSliderR)) {

            if (keyCode == KeyEvent.VK_RIGHT) {

                // System.err.println("Source is upslider, keypress right");
                if (upSliderR.getMaximum() >= 1000) {
                    upSliderR.setValue(upSliderR.getValue() + 100);
                } else {
                    upSliderR.setValue(upSliderR.getValue() + 1);
                }
            } else if (keyCode == KeyEvent.VK_LEFT) {

                if (upSliderR.getMaximum() >= 1000) {
                    upSliderR.setValue(upSliderR.getValue() - 100);
                } else {
                    upSliderR.setValue(upSliderR.getValue() - 1);
                }
            }
        } else if ((upSliderG != null) && (e.getSource() == upSliderG)) {

            if (keyCode == KeyEvent.VK_RIGHT) {

                // System.err.println("Source is upslider, keypress right");
                if (upSliderG.getMaximum() >= 1000) {
                    upSliderG.setValue(upSliderG.getValue() + 100);
                } else {
                    upSliderG.setValue(upSliderG.getValue() + 1);
                }
            } else if (keyCode == KeyEvent.VK_LEFT) {

                if (upSliderG.getMaximum() >= 1000) {
                    upSliderG.setValue(upSliderG.getValue() - 100);
                } else {
                    upSliderG.setValue(upSliderG.getValue() - 1);
                }
            }
        } else if ((upSliderB != null) && (e.getSource() == upSliderB)) {

            if (keyCode == KeyEvent.VK_RIGHT) {

                // System.err.println("Source is upslider, keypress right");
                if (upSliderB.getMaximum() >= 1000) {
                    upSliderB.setValue(upSliderB.getValue() + 100);
                } else {
                    upSliderB.setValue(upSliderB.getValue() + 1);
                }
            } else if (keyCode == KeyEvent.VK_LEFT) {

                if (upSliderB.getMaximum() >= 1000) {
                    upSliderB.setValue(upSliderB.getValue() - 100);
                } else {
                    upSliderB.setValue(upSliderB.getValue() - 1);
                }
            }
        } else if ((lowSlider != null) && (e.getSource() == lowSlider)) {

            if (keyCode == KeyEvent.VK_RIGHT) {

                // System.err.println("Source is upslider, keypress right");
                if (lowSlider.getMaximum() >= 1000) {
                    lowSlider.setValue(lowSlider.getValue() + 100);
                } else {
                    lowSlider.setValue(lowSlider.getValue() + 1);
                }
            } else if (keyCode == KeyEvent.VK_LEFT) {

                if (lowSlider.getMaximum() >= 1000) {
                    lowSlider.setValue(lowSlider.getValue() - 100);
                } else {
                    lowSlider.setValue(lowSlider.getValue() - 1);
                }
            }
        } else if ((lowSliderR != null) && (e.getSource() == lowSliderR)) {

            if (keyCode == KeyEvent.VK_RIGHT) {

                // System.err.println("Source is upslider, keypress right");
                if (lowSliderR.getMaximum() >= 1000) {
                    lowSliderR.setValue(lowSliderR.getValue() + 100);
                } else {
                    lowSliderR.setValue(lowSliderR.getValue() + 1);
                }
            } else if (keyCode == KeyEvent.VK_LEFT) {

                if (lowSliderR.getMaximum() >= 1000) {
                    lowSliderR.setValue(lowSliderR.getValue() - 100);
                } else {
                    lowSliderR.setValue(lowSliderR.getValue() - 1);
                }
            }
        } else if ((lowSliderG != null) && (e.getSource() == lowSliderG)) {

            if (keyCode == KeyEvent.VK_RIGHT) {

                // System.err.println("Source is upslider, keypress right");
                if (lowSliderG.getMaximum() >= 1000) {
                    lowSliderG.setValue(lowSliderG.getValue() + 100);
                } else {
                    lowSliderG.setValue(lowSliderG.getValue() + 1);
                }
            } else if (keyCode == KeyEvent.VK_LEFT) {

                if (lowSliderG.getMaximum() >= 1000) {
                    lowSliderG.setValue(lowSliderG.getValue() - 100);
                } else {
                    lowSliderG.setValue(lowSliderG.getValue() - 1);
                }
            }
        } else if ((lowSliderB != null) && (e.getSource() == lowSliderB)) {

            if (keyCode == KeyEvent.VK_RIGHT) {

                // System.err.println("Source is upslider, keypress right");
                if (lowSliderB.getMaximum() >= 1000) {
                    lowSliderB.setValue(lowSliderB.getValue() + 100);
                } else {
                    lowSliderB.setValue(lowSliderB.getValue() + 1);
                }
            } else if (keyCode == KeyEvent.VK_LEFT) {

                if (lowSliderB.getMaximum() >= 1000) {
                    lowSliderB.setValue(lowSliderB.getValue() - 100);
                } else {
                    lowSliderB.setValue(lowSliderB.getValue() - 1);
                }
            }
        } else if ((upSliderField != null) && (e.getSource() == upSliderField)) {

            if (keyCode == KeyEvent.VK_ENTER) {

                try {
                    int newVal = (int) Float.parseFloat(upSliderField.getText()) * 100;

                    if ((newVal > upSlider.getMaximum()) || (newVal < upSlider.getMinimum())) {
                        upSliderField.setText(String.valueOf(upSlider.getValue() / 100.0f));
                    } else {
                        upSetFromField = true;
                        upSlider.setValue(newVal);
                    }
                } catch (Exception ex) {
                    upSliderField.setText(String.valueOf(upSlider.getValue() / 100.0f));
                }
            }
        } else if ((upSliderFieldR != null) && (e.getSource() == upSliderFieldR)) {

            if (keyCode == KeyEvent.VK_ENTER) {

                try {
                    int newVal = (int) Float.parseFloat(upSliderFieldR.getText()) * 100;

                    if ((newVal > upSliderR.getMaximum()) || (newVal < upSliderR.getMinimum())) {
                        upSliderFieldR.setText(String.valueOf(upSliderR.getValue() / 100.0f));
                    } else {
                        upSetFromFieldR = true;
                        upSliderR.setValue(newVal);
                    }
                } catch (Exception ex) {
                    upSliderFieldR.setText(String.valueOf(upSliderR.getValue() / 100.0f));
                }
            }
        } else if ((upSliderFieldG != null) && (e.getSource() == upSliderFieldG)) {

            if (keyCode == KeyEvent.VK_ENTER) {

                try {
                    int newVal = (int) Float.parseFloat(upSliderFieldG.getText()) * 100;

                    if ((newVal > upSliderG.getMaximum()) || (newVal < upSliderG.getMinimum())) {
                        upSliderFieldG.setText(String.valueOf(upSliderG.getValue() / 100.0f));
                    } else {
                        upSetFromFieldG = true;
                        upSliderG.setValue(newVal);
                    }
                } catch (Exception ex) {
                    upSliderFieldG.setText(String.valueOf(upSliderG.getValue() / 100.0f));
                }
            }
        } else if ((upSliderFieldB != null) && (e.getSource() == upSliderFieldB)) {

            if (keyCode == KeyEvent.VK_ENTER) {

                try {
                    int newVal = (int) Float.parseFloat(upSliderFieldB.getText()) * 100;

                    if ((newVal > upSliderB.getMaximum()) || (newVal < upSliderB.getMinimum())) {
                        upSliderFieldB.setText(String.valueOf(upSliderB.getValue() / 100.0f));
                    } else {
                        upSetFromFieldB = true;
                        upSliderB.setValue(newVal);
                    }
                } catch (Exception ex) {
                    upSliderFieldB.setText(String.valueOf(upSliderB.getValue() / 100.0f));
                }
            }
        } else if ((lowSliderField != null) && (e.getSource() == lowSliderField)) {

            if (keyCode == KeyEvent.VK_ENTER) {

                try {
                    int newVal = (int) Float.parseFloat(lowSliderField.getText()) * 100;

                    if ((newVal > lowSlider.getMaximum()) || (newVal < lowSlider.getMinimum())) {
                        lowSliderField.setText(String.valueOf(lowSlider.getValue() / 100.0f));
                    } else {
                        lowSetFromField = true;
                        lowSlider.setValue(newVal);
                    }
                } catch (Exception ex) {
                    lowSliderField.setText(String.valueOf(lowSlider.getValue() / 100.0f));
                }
            }
        } else if ((lowSliderFieldR != null) && (e.getSource() == lowSliderFieldR)) {

            if (keyCode == KeyEvent.VK_ENTER) {

                try {
                    int newVal = (int) Float.parseFloat(lowSliderFieldR.getText()) * 100;

                    if ((newVal > lowSliderR.getMaximum()) || (newVal < lowSliderR.getMinimum())) {
                        lowSliderFieldR.setText(String.valueOf(lowSliderR.getValue() / 100.0f));
                    } else {
                        lowSetFromFieldR = true;
                        lowSliderR.setValue(newVal);
                    }
                } catch (Exception ex) {
                    lowSliderFieldR.setText(String.valueOf(lowSliderR.getValue() / 100.0f));
                }
            }
        } else if ((lowSliderFieldG != null) && (e.getSource() == lowSliderFieldG)) {

            if (keyCode == KeyEvent.VK_ENTER) {

                try {
                    int newVal = (int) Float.parseFloat(lowSliderFieldG.getText()) * 100;

                    if ((newVal > lowSliderG.getMaximum()) || (newVal < lowSliderG.getMinimum())) {
                        lowSliderFieldG.setText(String.valueOf(lowSliderG.getValue() / 100.0f));
                    } else {
                        lowSetFromFieldG = true;
                        lowSliderG.setValue(newVal);
                    }
                } catch (Exception ex) {
                    lowSliderFieldG.setText(String.valueOf(lowSliderG.getValue() / 100.0f));
                }
            }
        } else if ((lowSliderFieldB != null) && (e.getSource() == lowSliderFieldB)) {

            if (keyCode == KeyEvent.VK_ENTER) {

                try {
                    int newVal = (int) Float.parseFloat(lowSliderFieldB.getText()) * 100;

                    if ((newVal > lowSliderB.getMaximum()) || (newVal < lowSliderB.getMinimum())) {
                        lowSliderFieldB.setText(String.valueOf(lowSliderB.getValue() / 100.0f));
                    } else {
                        lowSetFromFieldB = true;
                        lowSliderB.setValue(newVal);
                    }
                } catch (Exception ex) {
                    lowSliderFieldB.setText(String.valueOf(lowSliderB.getValue() / 100.0f));
                }
            }
        }
    }

    /**
     * Placeholder required by KeyListener. Does nothing.
     *
     * @param  e  KeyEvent
     */
    public void keyReleased(KeyEvent e) { }

    /**
     * Placeholder required by KeyListener. Does nothing.
     *
     * @param  e  KeyEvent
     */
    public void keyTyped(KeyEvent e) { }

    /**
     * Tells images which are watching the paint region grow to update themselves.
     *
     * @param  backup  whether to backup the paint region before updating it (to allow for undos)
     */
    public void notifyPaintListeners(boolean backup) { }

    /**
     * {@inheritDoc}
     */
    public void notifyPaintListeners(boolean isRegionGrow, boolean backup, BitSet paintMask) {
        BitSet paintRegion = (BitSet)paintMask.clone();
        

        // System.err.println("in notify paint listeners() of JDialogPaintGrow()");

        // System.err.println("paintGrowListeners.size() = " + paintGrowListeners.size());
        PaintGrowListener temp, temp2;

        for (int i = 0; i < paintGrowListeners.size(); i++) {
            temp = (paintGrowListeners.elementAt(i));

            if (frame instanceof ViewJFrameImage) {

                // System.err.println("in notifyPaintListeners() and frame instanceof ViewJFrameImage");
                temp2 = (PaintGrowListener) ((ViewJFrameImage) frame).getComponentImage();

                if ((temp == temp2) && isRegionGrow) {

                    // System.err.println("temp is temp2 so calling updatePaint(paintRegion, backup, true)");
                    temp.updatePaint(paintRegion, backup, true);
                } else {

                    // System.err.println("temp is NOT temp2 so calling updatePaint(paintRegion, backup, false)");
                    temp.updatePaint(paintRegion, backup, false);
                }
            } else if (frame instanceof ViewJFrameTriImage) {

                // System.err.println("in notifyPaintListeners() and frame instanceof ViewJFrameTriImage");
                temp2 = (PaintGrowListener) ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A);

                if ((temp == temp2) && isRegionGrow) {
                    temp.updatePaint(paintRegion, backup, true);
                }
            } else if (frame instanceof ViewJFramePaintVasculature) {

                // System.err.println("in notifyPaintListeners() and frame instanceof ViewJFramePaintVasculature");
                temp2 = (PaintGrowListener) ((ViewJFramePaintVasculature) frame).getComponentImage();

                if ((temp == temp2) && isRegionGrow) {
                    temp.updatePaint(paintRegion, backup, true);
                } else {
                    temp.updatePaint(paintRegion, backup, false);
                }
            }
        }
    }

    /**
     * Sets the reference to this dialog for all of the images listening to it to null.
     */
    public void resetDialogs() {

        for (int i = 0; i < paintGrowListeners.size(); i++) {
            (paintGrowListeners.elementAt(i)).setGrowDialog(null);
        }
    }

    /**
     * Sets text in positionPanel.
     *
     * @param  posString  String to put in text field.
     */
    public void setPositionText(String posString) {
        posTextF.setText(posString);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  regionGrowAlgo  DOCUMENT ME!
     */
    public void setRegionGrowAlgo(AlgorithmRegionGrow regionGrowAlgo) {
        this.regionGrowAlgo = regionGrowAlgo;
    }

    /**
     * Sets values based on knob along slider.
     *
     * @param  e  Event that triggered this function
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        if ((upSlider != null) && (source == upSlider)) {

            if (!upSetFromField) {
                upSliderField.setText(String.valueOf(upSlider.getValue() / (float) 100));
            } else {
                upSetFromField = false;
            }
        } else if ((upSliderR != null) && (source == upSliderR)) {

            if (!upSetFromFieldR) {
                upSliderFieldR.setText(String.valueOf(upSliderR.getValue() / (float) 100));
            } else {
                upSetFromFieldR = false;
            }
        } else if ((upSliderG != null) && (source == upSliderG)) {

            if (!upSetFromFieldG) {
                upSliderFieldG.setText(String.valueOf(upSliderG.getValue() / (float) 100));
            } else {
                upSetFromFieldG = false;
            }
        } else if ((upSliderB != null) && (source == upSliderB)) {

            if (!upSetFromFieldB) {
                upSliderFieldB.setText(String.valueOf(upSliderB.getValue() / (float) 100));
            } else {
                upSetFromFieldB = false;
            }
        } else if ((lowSlider != null) && (source == lowSlider)) {

            if (!lowSetFromField) {
                lowSliderField.setText(String.valueOf(lowSlider.getValue() / (float) 100));
            } else {
                lowSetFromField = false;
            }
        } else if ((lowSliderR != null) && (source == lowSliderR)) {

            if (!lowSetFromFieldR) {
                lowSliderFieldR.setText(String.valueOf(lowSliderR.getValue() / (float) 100));
            } else {
                lowSetFromFieldR = false;
            }
        } else if ((lowSliderG != null) && (source == lowSliderG)) {

            if (!lowSetFromFieldG) {
                lowSliderFieldG.setText(String.valueOf(lowSliderG.getValue() / (float) 100));
            } else {
                lowSetFromFieldG = false;
            }
        } else if ((lowSliderB != null) && (source == lowSliderB)) {

            if (!lowSetFromFieldB) {
                lowSliderFieldB.setText(String.valueOf(lowSliderB.getValue() / (float) 100));
            } else {
                lowSetFromFieldB = false;
            }
        }

        if (((upSlider != null) && (lowSlider != null)) && ((source == upSlider) || (source == lowSlider)) &&
                (!upSlider.getValueIsAdjusting() && !lowSlider.getValueIsAdjusting())) {

            if (frame instanceof ViewJFrameImage) {
                ((ViewJFrameImage) frame).getComponentImage().regionGrow(leadString);
            } else if (frame instanceof ViewJFrameTriImage) {
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).regionGrow(leadString);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).regionGrow(leadString);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).regionGrow(leadString);
            } else if (frame instanceof ViewJFramePaintVasculature) {
                ((ViewJFramePaintVasculature) frame).getComponentImage().regionGrow(leadString);
            }

        } else if (((upSliderR != null) && (lowSliderR != null)) && ((source == upSliderR) || (source == lowSliderR)) &&
                       (!upSliderR.getValueIsAdjusting() && !lowSliderR.getValueIsAdjusting())) {

            if (frame instanceof ViewJFrameImage) {
                ((ViewJFrameImage) frame).getComponentImage().regionGrowColor(leadString);
            } else if (frame instanceof ViewJFrameTriImage) {
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).regionGrowColor(leadString);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).regionGrowColor(leadString);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).regionGrowColor(leadString);
            } else if (frame instanceof ViewJFramePaintVasculature) {
                ((ViewJFramePaintVasculature) frame).getComponentImage().regionGrowColor(leadString);
            }

        } else if (((upSliderG != null) && (lowSliderG != null)) && ((source == upSliderG) || (source == lowSliderG)) &&
                       (!upSliderG.getValueIsAdjusting() && !lowSliderG.getValueIsAdjusting())) {

            if (frame instanceof ViewJFrameImage) {
                ((ViewJFrameImage) frame).getComponentImage().regionGrowColor(leadString);
            } else if (frame instanceof ViewJFrameTriImage) {
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).regionGrowColor(leadString);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).regionGrowColor(leadString);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).regionGrowColor(leadString);
            } else if (frame instanceof ViewJFramePaintVasculature) {
                ((ViewJFramePaintVasculature) frame).getComponentImage().regionGrowColor(leadString);
            }

        } else if (((upSliderB != null) && (lowSliderB != null)) && ((source == upSliderB) || (source == lowSliderB)) &&
                       (!upSliderB.getValueIsAdjusting() && !lowSliderB.getValueIsAdjusting())) {

            if (frame instanceof ViewJFrameImage) {
                ((ViewJFrameImage) frame).getComponentImage().regionGrowColor(leadString);
            } else if (frame instanceof ViewJFrameTriImage) {
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).regionGrowColor(leadString);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).regionGrowColor(leadString);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).regionGrowColor(leadString);
            } else if (frame instanceof ViewJFramePaintVasculature) {
                ((ViewJFramePaintVasculature) frame).getComponentImage().regionGrowColor(leadString);
            }

        } else if (source == fuzzySlider) {
            currentFuzzy.setText(String.valueOf(fuzzySlider.getValue() / 1000.0f));
            fuzzyThreshold = fuzzySlider.getValue() / 1000.0f;

            if (regionGrowAlgo != null) {
                regionGrowAlgo.setNewThreshold(fuzzyThreshold, ((ViewJFrameImage) frame).getComponentImage(),
                                               displayFuzzy);
            }
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  event  Event that triggered this method.
     */
    public void windowClosing(WindowEvent event) {

        String tmpStr;
        int maxS;

        if (sizeCheckbox.isSelected()) { // unrestricted size
            maxS = -1;
        } else {
            tmpStr = maxSizeTextF.getText();

            if (testParameter(tmpStr, 1, 10000000)) {
                maxS = Integer.valueOf(tmpStr).intValue();
            } else {
                maxSizeTextF.requestFocus();
                maxSizeTextF.selectAll();
                maxS = -2;
            }
        }

        if (frame instanceof ViewJFrameImage) {
            ((ViewJFrameImage) frame).getComponentImage().setSizeLimit(maxS);
        } else if (frame instanceof ViewJFramePaintVasculature) {
            ((ViewJFramePaintVasculature) frame).getComponentImage().setSizeLimit(maxS);
        } else if (frame instanceof ViewJFrameTriImage) {
            ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setSizeLimit(maxS);
            ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setSizeLimit(maxS);
            ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setSizeLimit(maxS);
        }

        if (distanceCheckbox.isSelected()) { // unrestricted distance
            maxS = -1;
        } else {
            tmpStr = maxDistTextF.getText();

            if (testParameter(tmpStr, 1, 10000000)) {
                maxS = Integer.valueOf(tmpStr).intValue();
            } else {
                maxDistTextF.requestFocus();
                maxDistTextF.selectAll();
                maxS = -2;
            }
        }

        variableThresholds = variableDeltasCheckbox.isSelected();

        if (frame instanceof ViewJFrameImage) {
            ((ViewJFrameImage) frame).getComponentImage().setMaxDistance(maxS);

            if (!haveColor) {
                ((ViewJFrameImage) frame).getComponentImage().setLess(lowSlider.getValue() / (float) 100);

                ((ViewJFrameImage) frame).getComponentImage().setMore(upSlider.getValue() / (float) 100);
            }

            if (haveRed) {
                ((ViewJFrameImage) frame).getComponentImage().setLessR(lowSliderR.getValue() / (float) 100);

                ((ViewJFrameImage) frame).getComponentImage().setMoreR(upSliderR.getValue() / (float) 100);
            }

            if (haveGreen) {
                ((ViewJFrameImage) frame).getComponentImage().setLessG(lowSliderG.getValue() / (float) 100);

                ((ViewJFrameImage) frame).getComponentImage().setMoreG(upSliderG.getValue() / (float) 100);
            }

            if (haveBlue) {
                ((ViewJFrameImage) frame).getComponentImage().setLessB(lowSliderB.getValue() / (float) 100);

                ((ViewJFrameImage) frame).getComponentImage().setMoreB(upSliderB.getValue() / (float) 100);
            }

            ((ViewJFrameImage) frame).getComponentImage().setVariableThresholds(variableThresholds);
        } else if (frame instanceof ViewJFramePaintVasculature) {
            ((ViewJFramePaintVasculature) frame).getComponentImage().setMaxDistance(maxS);

            if (!haveColor) {
                ((ViewJFramePaintVasculature) frame).getComponentImage().setLess(lowSlider.getValue() / (float) 100);

                ((ViewJFramePaintVasculature) frame).getComponentImage().setMore(upSlider.getValue() / (float) 100);
            }

            if (haveRed) {
                ((ViewJFramePaintVasculature) frame).getComponentImage().setLessR(lowSliderR.getValue() / (float) 100);

                ((ViewJFramePaintVasculature) frame).getComponentImage().setMoreR(upSliderR.getValue() / (float) 100);
            }

            if (haveGreen) {
                ((ViewJFramePaintVasculature) frame).getComponentImage().setLessG(lowSliderG.getValue() / (float) 100);

                ((ViewJFramePaintVasculature) frame).getComponentImage().setMoreG(upSliderG.getValue() / (float) 100);
            }

            if (haveBlue) {
                ((ViewJFramePaintVasculature) frame).getComponentImage().setLessB(lowSliderB.getValue() / (float) 100);

                ((ViewJFramePaintVasculature) frame).getComponentImage().setMoreB(upSliderB.getValue() / (float) 100);
            }

            ((ViewJFramePaintVasculature) frame).getComponentImage().setVariableThresholds(variableThresholds);
        } else if (frame instanceof ViewJFrameTriImage) {
            ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setMaxDistance(maxS);
            ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setMaxDistance(maxS);
            ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setMaxDistance(maxS);

            if (!haveColor) {
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setLess(lowSlider.getValue() /
                                                                                                 (float) 100);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setLess(lowSlider.getValue() /
                                                                                                   (float) 100);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setLess(lowSlider.getValue() /
                                                                                                    (float) 100);

                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setMore(upSlider.getValue() /
                                                                                                 (float) 100);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setMore(upSlider.getValue() /
                                                                                                   (float) 100);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setMore(upSlider.getValue() /
                                                                                                    (float) 100);
            }

            if (haveRed) {
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setLessR(lowSliderR.getValue() /
                                                                                                  (float) 100);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setLessR(lowSliderR.getValue() /
                                                                                                    (float) 100);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setLessR(lowSliderR.getValue() /
                                                                                                     (float) 100);

                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setMoreR(upSliderR.getValue() /
                                                                                                  (float) 100);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setMoreR(upSliderR.getValue() /
                                                                                                    (float) 100);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setMoreR(upSliderR.getValue() /
                                                                                                     (float) 100);
            }

            if (haveGreen) {
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setLessG(lowSliderG.getValue() /
                                                                                                  (float) 100);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setLessG(lowSliderG.getValue() /
                                                                                                    (float) 100);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setLessG(lowSliderG.getValue() /
                                                                                                     (float) 100);

                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setMoreG(upSliderG.getValue() /
                                                                                                  (float) 100);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setMoreG(upSliderG.getValue() /
                                                                                                    (float) 100);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setMoreG(upSliderG.getValue() /
                                                                                                     (float) 100);
            }

            if (haveBlue) {
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setLessB(lowSliderB.getValue() /
                                                                                                  (float) 100);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setLessB(lowSliderB.getValue() /
                                                                                                    (float) 100);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setLessB(lowSliderB.getValue() /
                                                                                                     (float) 100);

                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setMoreB(upSliderB.getValue() /
                                                                                                  (float) 100);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setMoreB(upSliderB.getValue() /
                                                                                                    (float) 100);
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setMoreB(upSliderB.getValue() /
                                                                                                     (float) 100);
            }

            ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setVariableThresholds(variableThresholds);
            ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setVariableThresholds(variableThresholds);
            ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setVariableThresholds(variableThresholds);
        } // else if (frame instanceof ViewJFrameTriImage)

        if (fuzzyCheckbox.isSelected()) {
            fuzzyThreshold = fuzzySlider.getValue() / 1000.0f;
        } else {
            fuzzyThreshold = -1;
        }

        if (frame instanceof ViewJFrameImage) {
            ((ViewJFrameImage) frame).getComponentImage().setFuzzyThreshold(fuzzyThreshold);
        } else if (frame instanceof ViewJFramePaintVasculature) {
            ((ViewJFramePaintVasculature) frame).getComponentImage().setFuzzyThreshold(fuzzyThreshold);
        } else if (frame instanceof ViewJFrameTriImage) {
            ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setFuzzyThreshold(fuzzyThreshold);
            ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setFuzzyThreshold(fuzzyThreshold);
            ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setFuzzyThreshold(fuzzyThreshold);
        }

        if (voiCheckbox.isSelected()) {
            useVOI = true;
        } else {
            useVOI = false;
        }

        if (frame instanceof ViewJFrameImage) {
            ((ViewJFrameImage) frame).getComponentImage().setUseVOI(useVOI);
        } else if (frame instanceof ViewJFramePaintVasculature) {
            ((ViewJFramePaintVasculature) frame).getComponentImage().setUseVOI(useVOI);
        } else if (frame instanceof ViewJFrameTriImage) {
            ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setUseVOI(useVOI);
            ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setUseVOI(useVOI);
            ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setUseVOI(useVOI);
        }

        if (fuzzyImageCheckbox.isSelected()) {
            displayFuzzy = true;
        } else {
            displayFuzzy = false;
        }

        if (frame instanceof ViewJFrameImage) {
            ((ViewJFrameImage) frame).getComponentImage().setDisplayFuzzy(displayFuzzy);
            ((ViewJFrameImage) frame).getControls().getTools().setPointerSelected();
            ((ViewJFrameImage) frame).getComponentImage().setCursorMode(ViewJComponentEditImage.DEFAULT);
            resetDialogs();
        } else if (frame instanceof ViewJFramePaintVasculature) {
            ((ViewJFramePaintVasculature) frame).getComponentImage().setDisplayFuzzy(displayFuzzy);
            ((ViewJFramePaintVasculature) frame).getControls().getTools().setPointerSelected();
            ((ViewJFramePaintVasculature) frame).getComponentImage().setCursorMode(ViewJComponentEditImage.DEFAULT);
            resetDialogs();
        } else if (frame instanceof ViewJFrameTriImage) {
            ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).setDisplayFuzzy(displayFuzzy);
            ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.CORONAL_A).setDisplayFuzzy(displayFuzzy);
            ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.SAGITTAL_A).setDisplayFuzzy(displayFuzzy);
            ((ViewJFrameTriImage) frame).setTraverseButton();
            resetDialogs();
        }

        
        dispose();
    }

    /**
     * Initializes GUI components.
     */
    protected void init() {
        setTitle("Paint Grow");
        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(font12B);

        tabbedPane.addTab("Static threshold", null, buildStaticPanel());
        tabbedPane.addTab("Fuzzy connectedness", null, buildFuzzyPanel());

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buildHelpButton();
        OKButton.setText("Close");
        helpButton.setText("Help");
        buttonPanel.add(OKButton);
        buttonPanel.add(helpButton);
        mainDialogPanel.add(tabbedPane);
        mainDialogPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainDialogPanel);

        pack();
        setVisible(true);
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private JPanel buildFuzzyPanel() {
        JPanel fuzzyPanel = new JPanel(new GridBagLayout());
        fuzzyPanel.setBorder(buildTitledBorder("Fuzzy connectedness"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.WEST;

        fuzzyCheckbox = new JCheckBox("Fuzzy connectedness");
        fuzzyCheckbox.setFont(serif12);
        fuzzyCheckbox.setEnabled(true);
        fuzzyCheckbox.setSelected(false);
        fuzzyCheckbox.addActionListener(this);

        voiCheckbox = new JCheckBox("Initial variance from selected VOI");
        voiCheckbox.setFont(serif12);
        voiCheckbox.setSelected(false);
        voiCheckbox.setEnabled(false);
        voiCheckbox.addActionListener(this);

        fuzzySlider = new JSlider(JSlider.HORIZONTAL, 0, 1000, 600);
        fuzzySlider.setMajorTickSpacing(100);
        fuzzySlider.setPaintTicks(true);
        fuzzySlider.setEnabled(false);
        fuzzySlider.addChangeListener(this);

        maximumFuzzy = new JLabel("1.0");
        maximumFuzzy.setForeground(Color.black);
        maximumFuzzy.setFont(serif12);
        maximumFuzzy.setEnabled(false);

        currentFuzzy = new JLabel(String.valueOf(fuzzySlider.getValue() / 1000.0f));
        currentFuzzy.setForeground(Color.black);
        currentFuzzy.setFont(serif12B);
        currentFuzzy.setEnabled(false);

        minimumFuzzy = new JLabel("0.0");
        minimumFuzzy.setForeground(Color.black);
        minimumFuzzy.setFont(serif12);
        minimumFuzzy.setEnabled(false);

        fuzzyImageCheckbox = new JCheckBox("Display fuzzy image");
        fuzzyImageCheckbox.setFont(serif12);
        fuzzyImageCheckbox.setSelected(false);
        fuzzyImageCheckbox.setEnabled(false);
        fuzzyImageCheckbox.addActionListener(this);

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        fuzzyPanel.add(fuzzyCheckbox, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        fuzzyPanel.add(voiCheckbox, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        fuzzyPanel.add(fuzzyImageCheckbox, gbc);

        JPanel thresholdPanel = new JPanel(new GridBagLayout());
        thresholdPanel.setBorder(buildTitledBorder("Fuzzy threshold"));

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        thresholdPanel.add(fuzzySlider, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;
        thresholdPanel.add(minimumFuzzy, gbc);

        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = .5;
        thresholdPanel.add(currentFuzzy, gbc);

        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;
        thresholdPanel.add(maximumFuzzy, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 1;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.NORTH;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        fuzzyPanel.add(thresholdPanel, gbc);

        return fuzzyPanel;
    }

    /**
     * DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private JPanel buildStaticPanel() {
        JPanel upperPanel = null;
        JPanel upperPanelR = null;
        JPanel upperPanelG = null;
        JPanel upperPanelB = null;
        JPanel lowerPanel = null;
        JPanel lowerPanelR = null;
        JPanel lowerPanelG = null;
        JPanel lowerPanelB = null;
        JPanel maximumPanel = null;

        JPanel staticPanel = new JPanel(new GridBagLayout());
        staticPanel.setBorder(buildTitledBorder(""));

        if (frame instanceof ViewJFrameImage) {

            if (((ViewJFrameImage) frame).getComponentImage().getActiveImage().isColorImage()) {
                haveColor = true;
            } else {
                haveColor = false;
            }
        } else if (frame instanceof ViewJFramePaintVasculature) {

            if (((ViewJFramePaintVasculature) frame).getComponentImage().getActiveImage().isColorImage()) {
                haveColor = true;
            } else {
                haveColor = false;
            }
        } else if (frame instanceof ViewJFrameTriImage) {

            if (((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).getActiveImage().isColorImage()) {
                haveColor = true;
            } else {
                haveColor = false;
            }
        } else if ( (frame == null) && (surfacePaint != null) ) {
            if ( surfacePaint.getPaintImage().isColorImage() ) {
                haveColor = true;
            } else {
                haveColor = false;
            }
        } else if ( (frame == null) && (surfacePaint_WM != null) ) {
            if ( surfacePaint_WM.getPaintImage().isColorImage() ) {
                haveColor = true;
            } else {
                haveColor = false;
            }
        }

        JPanel positionPanel = new JPanel(new GridBagLayout());
        positionPanel.setBorder(buildTitledBorder("Cursor position and voxel intensity"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.WEST;

        posTextF = new JTextField(10);
        posTextF.setText("  ");
        posTextF.setFont(serif12);
        //posTextF.setEnabled(false);
        positionPanel.add(posTextF, gbc);

        if (!haveColor) {

            if (frame instanceof ViewJFrameImage) {
                imageType = ((ViewJFrameImage) frame).getComponentImage().getActiveImage().getType();
                ((ViewJFrameImage) frame).getComponentImage().getActiveImage().calcMinMax();
                maxValue = ((ViewJFrameImage) frame).getComponentImage().getActiveImage().getMax();
                minValue = ((ViewJFrameImage) frame).getComponentImage().getActiveImage().getMin();
            } else if (frame instanceof ViewJFrameTriImage) {
                imageType = ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).getActiveImage().getType();
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).getActiveImage().calcMinMax();
                maxValue = ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).getActiveImage().getMax();
                minValue = ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).getActiveImage().getMin();
            } else if (frame instanceof ViewJFramePaintVasculature) {
                imageType = ((ViewJFramePaintVasculature) frame).getComponentImage().getActiveImage().getType();
                ((ViewJFramePaintVasculature) frame).getComponentImage().getActiveImage().calcMinMax();
                maxValue = ((ViewJFramePaintVasculature) frame).getComponentImage().getActiveImage().getMax();
                minValue = ((ViewJFramePaintVasculature) frame).getComponentImage().getActiveImage().getMin();
            } else if ( (frame == null) && (surfacePaint != null) ) {
                imageType = surfacePaint.getPaintImage().getType();
                surfacePaint.getPaintImage().calcMinMax();
                maxValue = surfacePaint.getPaintImage().getMax();
                minValue = surfacePaint.getPaintImage().getMin();
            } else if ( (frame == null) && (surfacePaint_WM != null) ) {
                imageType = surfacePaint_WM.getPaintImage().getType();
                surfacePaint_WM.getPaintImage().calcMinMax();
                maxValue = surfacePaint_WM.getPaintImage().getMax();
                minValue = surfacePaint_WM.getPaintImage().getMin();
            }

            range = maxValue - minValue;

            if ((imageType != ModelStorageBase.FLOAT) && (imageType != ModelStorageBase.DOUBLE) && (range == 1)) {
                max = 100.0f;
                initialDelta = 0;
                min = 0;
                disableSliders = true;
            } else if ((imageType != ModelStorageBase.FLOAT) && (imageType != ModelStorageBase.DOUBLE) &&
                           (range >= 2) && (range <= 255)) {
                max = (float) Math.min(100.0 * range, 100000);

                // max = 100000;
                initialDelta = 100;
                min = 0;
            } else {
                max = (float) Math.min(100.0 * range, 100000);

                // max = 100000;
                initialDelta = (int) Math.min(1000, 100.0 * range);
                min = 0;
            }

            upSlider = new JSlider(JSlider.HORIZONTAL, (int) min, (int) max, initialDelta);
            upSlider.setMajorTickSpacing((int) (max - min) / 10);
            upSlider.setPaintTicks(true);

            if (disableSliders) {
                upSlider.setEnabled(false);
            } else {
                upSlider.setEnabled(true);
            }

            upSlider.addChangeListener(this);
            upSlider.setInputMap(JSlider.WHEN_FOCUSED, new InputMap());
            upSlider.addKeyListener(this);

            upSliderField = new JTextField(4);
            upSliderField.setText(String.valueOf((float) (upSlider.getValue() / 100.0f)));
            upSliderField.addKeyListener(this);
            upSliderField.addFocusListener(this);

            if (disableSliders) {
                upSliderField.setEnabled(false);
                upSliderField.setEditable(false);
            } else {
                upSliderField.setEnabled(true);
                upSliderField.setEditable(true);
            }

            maximumU = new JLabel(String.valueOf((upSlider.getMaximum()) / 100.0f));
            maximumU.setForeground(Color.black);
            maximumU.setFont(serif12);

            if (disableSliders) {
                maximumU.setEnabled(false);
            } else {
                maximumU.setEnabled(true);
            }

            float mid = upSlider.getMaximum() / 200.0f;
            upperMid = new JLabel();

            if (mid >= 100) {
                upperMid.setText("       " + mid);
            } else {
                upperMid.setText("     " + mid);
            }

            upperMid.setForeground(Color.black);
            upperMid.setFont(serif12);

            if (disableSliders) {
                upperMid.setEnabled(false);
            } else {
                upperMid.setEnabled(true);
            }

            minimumU = new JLabel(String.valueOf(upSlider.getMinimum() / 100.0f));
            minimumU.setForeground(Color.black);
            minimumU.setFont(serif12);

            if (disableSliders) {
                minimumU.setEnabled(false);
            } else {
                minimumU.setEnabled(true);
            }

            upperPanel = new JPanel(new GridBagLayout());
            upperPanel.setBorder(buildTitledBorder("Delta above selected voxel intensity"));
            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.gridwidth = 3;
            gbc.weightx = 1;
            gbc.gridheight = 1;
            gbc.fill = GridBagConstraints.HORIZONTAL;

            upperPanel.add(upSlider, gbc);

            gbc.gridwidth = 1;
            gbc.gridx = 3;
            upperPanel.add(upSliderField, gbc);

            gbc.gridx = 0;
            gbc.gridy = 1;
            gbc.gridwidth = 1;
            gbc.weightx = 0;
            gbc.anchor = GridBagConstraints.WEST;
            gbc.fill = GridBagConstraints.NONE;

            upperPanel.add(minimumU, gbc);

            gbc.gridx = 1;
            gbc.anchor = GridBagConstraints.CENTER;
            gbc.weightx = .5;

            upperPanel.add(upperMid, gbc);

            gbc.gridx = 2;
            gbc.anchor = GridBagConstraints.EAST;
            gbc.weightx = 0;

            upperPanel.add(maximumU, gbc);

            lowSlider = new JSlider(JSlider.HORIZONTAL, (int) min, (int) max, initialDelta);
            lowSlider.setMajorTickSpacing((int) (max - min) / 10);
            lowSlider.setPaintTicks(true);
            lowSlider.setInputMap(JSlider.WHEN_FOCUSED, new InputMap());
            lowSlider.addKeyListener(this);

            if (disableSliders) {
                lowSlider.setEnabled(false);
            } else {
                lowSlider.setEnabled(true);
            }

            lowSlider.addChangeListener(this);

            lowSliderField = new JTextField(4);
            lowSliderField.setText(String.valueOf((float) (lowSlider.getValue() / 100.0f)));
            lowSliderField.addKeyListener(this);
            lowSliderField.addFocusListener(this);

            if (disableSliders) {
                lowSliderField.setEnabled(false);
                lowSliderField.setEditable(false);
            } else {
                lowSliderField.setEnabled(true);
                lowSliderField.setEditable(true);
            }

            maximumL = new JLabel(String.valueOf((lowSlider.getMaximum()) / 100.0f));
            maximumL.setForeground(Color.black);
            maximumL.setFont(serif12);

            if (disableSliders) {
                maximumL.setEnabled(false);
            } else {
                maximumL.setEnabled(true);
            }

            lowerMid = new JLabel();
            mid = lowSlider.getMaximum() / 200.0f;

            if (mid >= 100) {
                lowerMid.setText("       " + mid);
            } else {
                lowerMid.setText("     " + mid);
            }

            lowerMid.setForeground(Color.black);
            lowerMid.setFont(serif12);

            if (disableSliders) {
                lowerMid.setEnabled(false);
            } else {
                lowerMid.setEnabled(true);
            }

            minimumL = new JLabel(String.valueOf(lowSlider.getMinimum() / 100.0f));
            minimumL.setForeground(Color.black);
            minimumL.setFont(serif12);

            if (disableSliders) {
                minimumL.setEnabled(false);
            } else {
                minimumL.setEnabled(true);
            }

            lowerPanel = new JPanel(new GridBagLayout());
            lowerPanel.setBorder(buildTitledBorder("Delta below selected voxel intensity"));

            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.gridwidth = 3;
            gbc.weightx = 1;
            gbc.gridheight = 1;
            gbc.fill = GridBagConstraints.HORIZONTAL;

            lowerPanel.add(lowSlider, gbc);

            gbc.gridwidth = 1;
            gbc.gridx = 3;
            lowerPanel.add(lowSliderField, gbc);

            gbc.gridx = 0;
            gbc.gridy = 1;
            gbc.gridwidth = 1;
            gbc.weightx = 0;
            gbc.anchor = GridBagConstraints.WEST;
            gbc.fill = GridBagConstraints.NONE;

            lowerPanel.add(minimumL, gbc);

            gbc.gridx = 1;
            gbc.anchor = GridBagConstraints.CENTER;
            gbc.weightx = .5;

            lowerPanel.add(lowerMid, gbc);

            gbc.gridx = 2;
            gbc.anchor = GridBagConstraints.EAST;
            gbc.weightx = 0;

            lowerPanel.add(maximumL, gbc);

            maximumValueButton = new JButton("Set");
            maximumValueButton.addActionListener(this);
            maximumValueButton.setToolTipText("Change slider maximum value.");
            maximumValueButton.setFont(MipavUtil.font12B);
            maximumValueButton.setMinimumSize(new Dimension(20, 20));
            maximumValueButton.setMargin(new Insets(2, 7, 2, 7));
            maximumValueButton.setEnabled(!haveColor);

            maximumValueField = new JTextField(10);
            maximumValueField.setText(String.valueOf((float) (upSlider.getMaximum() / 100.0f)));
            maximumValueField.setFont(serif12);
            maximumValueField.setEnabled(!haveColor);

            maximumPanel = new JPanel(new GridBagLayout());
            maximumPanel.setBorder(buildTitledBorder("Set maximum slider values"));

            gbc.gridx = 0;
            gbc.gridy = 0;
            gbc.gridwidth = 1;
            gbc.weightx = 1;
            gbc.gridheight = 1;
            gbc.fill = GridBagConstraints.NONE;

            maximumPanel.add(maximumValueButton, gbc);

            gbc.gridx = 2;
            maximumPanel.add(maximumValueField, gbc);
        } else { // haveColor

            if (frame instanceof ViewJFrameImage) {
                imageType = ((ViewJFrameImage) frame).getComponentImage().getActiveImage().getType();
                ((ViewJFrameImage) frame).getComponentImage().getActiveImage().calcMinMax();
                maxValueR = ((ViewJFrameImage) frame).getComponentImage().getActiveImage().getMaxR();
                minValueR = ((ViewJFrameImage) frame).getComponentImage().getActiveImage().getMinR();
                maxValueG = ((ViewJFrameImage) frame).getComponentImage().getActiveImage().getMaxG();
                minValueG = ((ViewJFrameImage) frame).getComponentImage().getActiveImage().getMinG();
                maxValueB = ((ViewJFrameImage) frame).getComponentImage().getActiveImage().getMaxB();
                minValueB = ((ViewJFrameImage) frame).getComponentImage().getActiveImage().getMinB();
            } else if (frame instanceof ViewJFrameTriImage) {
                imageType = ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).getActiveImage().getType();
                ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).getActiveImage().calcMinMax();
                maxValueR = ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).getActiveImage().getMaxR();
                minValueR = ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).getActiveImage().getMinR();
                maxValueG = ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).getActiveImage().getMaxG();
                minValueG = ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).getActiveImage().getMinG();
                maxValueB = ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).getActiveImage().getMaxB();
                minValueB = ((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).getActiveImage().getMinB();
            } else if (frame instanceof ViewJFramePaintVasculature) {
                imageType = ((ViewJFramePaintVasculature) frame).getComponentImage().getActiveImage().getType();
                ((ViewJFramePaintVasculature) frame).getComponentImage().getActiveImage().calcMinMax();
                maxValueR = ((ViewJFramePaintVasculature) frame).getComponentImage().getActiveImage().getMaxR();
                minValueR = ((ViewJFramePaintVasculature) frame).getComponentImage().getActiveImage().getMinR();
                maxValueG = ((ViewJFramePaintVasculature) frame).getComponentImage().getActiveImage().getMaxG();
                minValueG = ((ViewJFramePaintVasculature) frame).getComponentImage().getActiveImage().getMinG();
                maxValueB = ((ViewJFramePaintVasculature) frame).getComponentImage().getActiveImage().getMaxB();
                minValueB = ((ViewJFramePaintVasculature) frame).getComponentImage().getActiveImage().getMinB();
            } else if ( (frame == null) && (surfacePaint != null) ) {
                imageType = surfacePaint.getPaintImage().getType();
                surfacePaint.getPaintImage().calcMinMax();
                maxValueR = surfacePaint.getPaintImage().getMaxR();
                minValueR = surfacePaint.getPaintImage().getMinR();
                maxValueG = surfacePaint.getPaintImage().getMaxG();
                minValueG = surfacePaint.getPaintImage().getMinG();
                maxValueB = surfacePaint.getPaintImage().getMaxB();
                minValueB = surfacePaint.getPaintImage().getMinB();
            } else if ( (frame == null) && (surfacePaint_WM != null) ) {
                imageType = surfacePaint_WM.getPaintImage().getType();
                surfacePaint_WM.getPaintImage().calcMinMax();
                maxValueR = surfacePaint_WM.getPaintImage().getMaxR();
                minValueR = surfacePaint_WM.getPaintImage().getMinR();
                maxValueG = surfacePaint_WM.getPaintImage().getMaxG();
                minValueG = surfacePaint_WM.getPaintImage().getMinG();
                maxValueB = surfacePaint_WM.getPaintImage().getMaxB();
                minValueB = surfacePaint_WM.getPaintImage().getMinB();
            }

            rangeR = maxValueR - minValueR;

            if (rangeR > 0) {
                haveRed = true;
            } else {
                haveRed = false;
            }

            rangeG = maxValueG - minValueG;

            if (rangeG > 0) {
                haveGreen = true;
            } else {
                haveGreen = false;
            }

            rangeB = maxValueB - minValueB;

            if (rangeB > 0) {
                haveBlue = true;
            } else {
                haveBlue = false;
            }

            if (haveRed) {

                if ((imageType != ModelStorageBase.ARGB_FLOAT) && (rangeR == 1)) {
                    maxR = 100.0f;
                    initialDeltaR = 0;
                    minR = 0;
                    disableSlidersR = true;
                } else if ((imageType != ModelStorageBase.ARGB_FLOAT) && (rangeR >= 2) && (rangeR <= 255)) {
                    maxR = (float) Math.min(100.0 * rangeR, 100000);
                    initialDeltaR = 100;
                    minR = 0;
                } else {
                    maxR = (float) Math.min(100.0 * rangeR, 100000);
                    initialDeltaR = (int) Math.min(1000, 100.0 * rangeR);
                    minR = 0;
                }

                upSliderR = new JSlider(JSlider.HORIZONTAL, (int) minR, (int) maxR, initialDeltaR);
                upSliderR.setMajorTickSpacing((int) (maxR - minR) / 10);
                upSliderR.setPaintTicks(true);

                if (disableSlidersR) {
                    upSliderR.setEnabled(false);
                } else {
                    upSliderR.setEnabled(true);
                }

                upSliderR.addChangeListener(this);
                upSliderR.setInputMap(JSlider.WHEN_FOCUSED, new InputMap());
                upSliderR.addKeyListener(this);

                upSliderFieldR = new JTextField(4);
                upSliderFieldR.setText(String.valueOf((float) (upSliderR.getValue() / 100.0f)));
                upSliderFieldR.addKeyListener(this);

                if (disableSlidersR) {
                    upSliderFieldR.setEnabled(false);
                    upSliderFieldR.setEditable(false);
                } else {
                    upSliderFieldR.setEnabled(true);
                    upSliderFieldR.setEditable(true);
                }

                maximumUR = new JLabel(String.valueOf((upSliderR.getMaximum()) / 100.0f));
                maximumUR.setForeground(Color.black);
                maximumUR.setFont(serif12);

                if (disableSlidersR) {
                    maximumUR.setEnabled(false);
                } else {
                    maximumUR.setEnabled(true);
                }

                float midR = upSliderR.getMaximum() / 200.0f;
                upperMidR = new JLabel();

                if (midR >= 100) {
                    upperMidR.setText("       " + midR);
                } else {
                    upperMidR.setText("     " + midR);
                }

                upperMidR.setForeground(Color.black);
                upperMidR.setFont(serif12);

                if (disableSlidersR) {
                    upperMidR.setEnabled(false);
                } else {
                    upperMidR.setEnabled(true);
                }

                minimumUR = new JLabel(String.valueOf(upSliderR.getMinimum() / 100.0f));
                minimumUR.setForeground(Color.black);
                minimumUR.setFont(serif12);

                if (disableSlidersR) {
                    minimumUR.setEnabled(false);
                } else {
                    minimumUR.setEnabled(true);
                }

                upperPanelR = new JPanel(new GridBagLayout());
                upperPanelR.setBorder(buildTitledBorder("Delta above selected red voxel intensity"));
                gbc.gridx = 0;
                gbc.gridy = 0;
                gbc.gridwidth = 3;
                gbc.weightx = 1;
                gbc.gridheight = 1;
                gbc.fill = GridBagConstraints.HORIZONTAL;

                upperPanelR.add(upSliderR, gbc);

                gbc.gridwidth = 1;
                gbc.gridx = 3;
                upperPanelR.add(upSliderFieldR, gbc);

                gbc.gridx = 0;
                gbc.gridy = 1;
                gbc.gridwidth = 1;
                gbc.weightx = 0;
                gbc.anchor = GridBagConstraints.WEST;
                gbc.fill = GridBagConstraints.NONE;

                upperPanelR.add(minimumUR, gbc);

                gbc.gridx = 1;
                gbc.anchor = GridBagConstraints.CENTER;
                gbc.weightx = .5;

                upperPanelR.add(upperMidR, gbc);

                gbc.gridx = 2;
                gbc.anchor = GridBagConstraints.EAST;
                gbc.weightx = 0;

                upperPanelR.add(maximumUR, gbc);

                lowSliderR = new JSlider(JSlider.HORIZONTAL, (int) minR, (int) maxR, initialDeltaR);
                lowSliderR.setMajorTickSpacing((int) (maxR - minR) / 10);
                lowSliderR.setPaintTicks(true);
                lowSliderR.setInputMap(JSlider.WHEN_FOCUSED, new InputMap());
                lowSliderR.addKeyListener(this);

                if (disableSlidersR) {
                    lowSliderR.setEnabled(false);
                } else {
                    lowSliderR.setEnabled(true);
                }

                lowSliderR.addChangeListener(this);

                lowSliderFieldR = new JTextField(4);
                lowSliderFieldR.setText(String.valueOf((float) (lowSliderR.getValue() / 100.0f)));
                lowSliderFieldR.addKeyListener(this);

                if (disableSlidersR) {
                    lowSliderFieldR.setEnabled(false);
                    lowSliderFieldR.setEditable(false);
                } else {
                    lowSliderFieldR.setEnabled(true);
                    lowSliderFieldR.setEditable(true);
                }

                maximumLR = new JLabel(String.valueOf((lowSliderR.getMaximum()) / 100.0f));
                maximumLR.setForeground(Color.black);
                maximumLR.setFont(serif12);

                if (disableSlidersR) {
                    maximumLR.setEnabled(false);
                } else {
                    maximumLR.setEnabled(true);
                }

                lowerMidR = new JLabel();
                midR = lowSliderR.getMaximum() / 200.0f;

                if (midR >= 100) {
                    lowerMidR.setText("       " + midR);
                } else {
                    lowerMidR.setText("     " + midR);
                }

                lowerMidR.setForeground(Color.black);
                lowerMidR.setFont(serif12);

                if (disableSlidersR) {
                    lowerMidR.setEnabled(false);
                } else {
                    lowerMidR.setEnabled(true);
                }

                minimumLR = new JLabel(String.valueOf(lowSliderR.getMinimum() / 100.0f));
                minimumLR.setForeground(Color.black);
                minimumLR.setFont(serif12);

                if (disableSlidersR) {
                    minimumLR.setEnabled(false);
                } else {
                    minimumLR.setEnabled(true);
                }

                lowerPanelR = new JPanel(new GridBagLayout());
                lowerPanelR.setBorder(buildTitledBorder("Delta below selected red voxel intensity"));

                gbc.gridx = 0;
                gbc.gridy = 0;
                gbc.gridwidth = 3;
                gbc.weightx = 1;
                gbc.gridheight = 1;
                gbc.fill = GridBagConstraints.HORIZONTAL;

                lowerPanelR.add(lowSliderR, gbc);

                gbc.gridwidth = 1;
                gbc.gridx = 3;
                lowerPanelR.add(lowSliderFieldR, gbc);

                gbc.gridx = 0;
                gbc.gridy = 1;
                gbc.gridwidth = 1;
                gbc.weightx = 0;
                gbc.anchor = GridBagConstraints.WEST;
                gbc.fill = GridBagConstraints.NONE;

                lowerPanelR.add(minimumLR, gbc);

                gbc.gridx = 1;
                gbc.anchor = GridBagConstraints.CENTER;
                gbc.weightx = .5;

                lowerPanelR.add(lowerMidR, gbc);

                gbc.gridx = 2;
                gbc.anchor = GridBagConstraints.EAST;
                gbc.weightx = 0;

                lowerPanelR.add(maximumLR, gbc);
            } // if (haveRed)

            if (haveGreen) {

                if ((imageType != ModelStorageBase.ARGB_FLOAT) && (rangeG == 1)) {
                    maxG = 100.0f;
                    initialDeltaG = 0;
                    minG = 0;
                    disableSlidersG = true;
                } else if ((imageType != ModelStorageBase.ARGB_FLOAT) && (rangeG >= 2) && (rangeG <= 255)) {
                    maxG = (float) Math.min(100.0 * rangeG, 100000);
                    initialDeltaG = 100;
                    minG = 0;
                } else {
                    maxG = (float) Math.min(100.0 * rangeG, 100000);
                    initialDeltaG = (int) Math.min(1000, 100.0 * rangeG);
                    minG = 0;
                }

                upSliderG = new JSlider(JSlider.HORIZONTAL, (int) minG, (int) maxG, initialDeltaG);
                upSliderG.setMajorTickSpacing((int) (maxG - minG) / 10);
                upSliderG.setPaintTicks(true);

                if (disableSlidersG) {
                    upSliderG.setEnabled(false);
                } else {
                    upSliderG.setEnabled(true);
                }

                upSliderG.addChangeListener(this);
                upSliderG.setInputMap(JSlider.WHEN_FOCUSED, new InputMap());
                upSliderG.addKeyListener(this);

                upSliderFieldG = new JTextField(4);
                upSliderFieldG.setText(String.valueOf((float) (upSliderG.getValue() / 100.0f)));
                upSliderFieldG.addKeyListener(this);

                if (disableSlidersG) {
                    upSliderFieldG.setEnabled(false);
                    upSliderFieldG.setEditable(false);
                } else {
                    upSliderFieldG.setEnabled(true);
                    upSliderFieldG.setEditable(true);
                }

                maximumUG = new JLabel(String.valueOf((upSliderG.getMaximum()) / 100.0f));
                maximumUG.setForeground(Color.black);
                maximumUG.setFont(serif12);

                if (disableSlidersG) {
                    maximumUG.setEnabled(false);
                } else {
                    maximumUG.setEnabled(true);
                }

                float midG = upSliderG.getMaximum() / 200.0f;
                upperMidG = new JLabel();

                if (midG >= 100) {
                    upperMidG.setText("       " + midG);
                } else {
                    upperMidG.setText("     " + midG);
                }

                upperMidG.setForeground(Color.black);
                upperMidG.setFont(serif12);

                if (disableSlidersG) {
                    upperMidG.setEnabled(false);
                } else {
                    upperMidG.setEnabled(true);
                }

                minimumUG = new JLabel(String.valueOf(upSliderG.getMinimum() / 100.0f));
                minimumUG.setForeground(Color.black);
                minimumUG.setFont(serif12);

                if (disableSlidersG) {
                    minimumUG.setEnabled(false);
                } else {
                    minimumUG.setEnabled(true);
                }

                upperPanelG = new JPanel(new GridBagLayout());
                upperPanelG.setBorder(buildTitledBorder("Delta above selected green voxel intensity"));
                gbc.gridx = 0;
                gbc.gridy = 0;
                gbc.gridwidth = 3;
                gbc.weightx = 1;
                gbc.gridheight = 1;
                gbc.fill = GridBagConstraints.HORIZONTAL;

                upperPanelG.add(upSliderG, gbc);

                gbc.gridwidth = 1;
                gbc.gridx = 3;
                upperPanelG.add(upSliderFieldG, gbc);

                gbc.gridx = 0;
                gbc.gridy = 1;
                gbc.gridwidth = 1;
                gbc.weightx = 0;
                gbc.anchor = GridBagConstraints.WEST;
                gbc.fill = GridBagConstraints.NONE;

                upperPanelG.add(minimumUG, gbc);

                gbc.gridx = 1;
                gbc.anchor = GridBagConstraints.CENTER;
                gbc.weightx = .5;

                upperPanelG.add(upperMidG, gbc);

                gbc.gridx = 2;
                gbc.anchor = GridBagConstraints.EAST;
                gbc.weightx = 0;

                upperPanelG.add(maximumUG, gbc);

                lowSliderG = new JSlider(JSlider.HORIZONTAL, (int) minG, (int) maxG, initialDeltaG);
                lowSliderG.setMajorTickSpacing((int) (maxG - minG) / 10);
                lowSliderG.setPaintTicks(true);
                lowSliderG.setInputMap(JSlider.WHEN_FOCUSED, new InputMap());
                lowSliderG.addKeyListener(this);

                if (disableSlidersG) {
                    lowSliderG.setEnabled(false);
                } else {
                    lowSliderG.setEnabled(true);
                }

                lowSliderG.addChangeListener(this);

                lowSliderFieldG = new JTextField(4);
                lowSliderFieldG.setText(String.valueOf((float) (lowSliderG.getValue() / 100.0f)));
                lowSliderFieldG.addKeyListener(this);

                if (disableSlidersG) {
                    lowSliderFieldG.setEnabled(false);
                    lowSliderFieldG.setEditable(false);
                } else {
                    lowSliderFieldG.setEnabled(true);
                    lowSliderFieldG.setEditable(true);
                }

                maximumLG = new JLabel(String.valueOf((lowSliderG.getMaximum()) / 100.0f));
                maximumLG.setForeground(Color.black);
                maximumLG.setFont(serif12);

                if (disableSlidersG) {
                    maximumLG.setEnabled(false);
                } else {
                    maximumLG.setEnabled(true);
                }

                lowerMidG = new JLabel();
                midG = lowSliderG.getMaximum() / 200.0f;

                if (midG >= 100) {
                    lowerMidG.setText("       " + midG);
                } else {
                    lowerMidG.setText("     " + midG);
                }

                lowerMidG.setForeground(Color.black);
                lowerMidG.setFont(serif12);

                if (disableSlidersG) {
                    lowerMidG.setEnabled(false);
                } else {
                    lowerMidG.setEnabled(true);
                }

                minimumLG = new JLabel(String.valueOf(lowSliderG.getMinimum() / 100.0f));
                minimumLG.setForeground(Color.black);
                minimumLG.setFont(serif12);

                if (disableSlidersG) {
                    minimumLG.setEnabled(false);
                } else {
                    minimumLG.setEnabled(true);
                }

                lowerPanelG = new JPanel(new GridBagLayout());
                lowerPanelG.setBorder(buildTitledBorder("Delta below selected green voxel intensity"));

                gbc.gridx = 0;
                gbc.gridy = 0;
                gbc.gridwidth = 3;
                gbc.weightx = 1;
                gbc.gridheight = 1;
                gbc.fill = GridBagConstraints.HORIZONTAL;

                lowerPanelG.add(lowSliderG, gbc);

                gbc.gridwidth = 1;
                gbc.gridx = 3;
                lowerPanelG.add(lowSliderFieldG, gbc);

                gbc.gridx = 0;
                gbc.gridy = 1;
                gbc.gridwidth = 1;
                gbc.weightx = 0;
                gbc.anchor = GridBagConstraints.WEST;
                gbc.fill = GridBagConstraints.NONE;

                lowerPanelG.add(minimumLG, gbc);

                gbc.gridx = 1;
                gbc.anchor = GridBagConstraints.CENTER;
                gbc.weightx = .5;

                lowerPanelG.add(lowerMidG, gbc);

                gbc.gridx = 2;
                gbc.anchor = GridBagConstraints.EAST;
                gbc.weightx = 0;

                lowerPanelG.add(maximumLG, gbc);
            } // if (haveGreen)

            if (haveBlue) {

                if ((imageType != ModelStorageBase.ARGB_FLOAT) && (rangeB == 1)) {
                    maxB = 100.0f;
                    initialDeltaB = 0;
                    minB = 0;
                    disableSlidersB = true;
                } else if ((imageType != ModelStorageBase.ARGB_FLOAT) && (rangeB >= 2) && (rangeB <= 255)) {
                    maxB = (float) Math.min(100.0 * rangeB, 100000);
                    initialDeltaB = 100;
                    minB = 0;
                } else {
                    maxB = (float) Math.min(100.0 * rangeB, 100000);
                    initialDeltaB = (int) Math.min(1000, 100.0 * rangeB);
                    minB = 0;
                }

                upSliderB = new JSlider(JSlider.HORIZONTAL, (int) minB, (int) maxB, initialDeltaB);
                upSliderB.setMajorTickSpacing((int) (maxB - minB) / 10);
                upSliderB.setPaintTicks(true);

                if (disableSlidersB) {
                    upSliderB.setEnabled(false);
                } else {
                    upSliderB.setEnabled(true);
                }

                upSliderB.addChangeListener(this);
                upSliderB.setInputMap(JSlider.WHEN_FOCUSED, new InputMap());
                upSliderB.addKeyListener(this);

                upSliderFieldB = new JTextField(4);
                upSliderFieldB.setText(String.valueOf((float) (upSliderB.getValue() / 100.0f)));
                upSliderFieldB.addKeyListener(this);

                if (disableSlidersB) {
                    upSliderFieldB.setEnabled(false);
                    upSliderFieldB.setEditable(false);
                } else {
                    upSliderFieldB.setEnabled(true);
                    upSliderFieldB.setEditable(true);
                }

                maximumUB = new JLabel(String.valueOf((upSliderB.getMaximum()) / 100.0f));
                maximumUB.setForeground(Color.black);
                maximumUB.setFont(serif12);

                if (disableSlidersB) {
                    maximumUB.setEnabled(false);
                } else {
                    maximumUB.setEnabled(true);
                }

                float midB = upSliderB.getMaximum() / 200.0f;
                upperMidB = new JLabel();

                if (midB >= 100) {
                    upperMidB.setText("       " + midB);
                } else {
                    upperMidB.setText("     " + midB);
                }

                upperMidB.setForeground(Color.black);
                upperMidB.setFont(serif12);

                if (disableSlidersB) {
                    upperMidB.setEnabled(false);
                } else {
                    upperMidB.setEnabled(true);
                }

                minimumUB = new JLabel(String.valueOf(upSliderB.getMinimum() / 100.0f));
                minimumUB.setForeground(Color.black);
                minimumUB.setFont(serif12);

                if (disableSlidersB) {
                    minimumUB.setEnabled(false);
                } else {
                    minimumUB.setEnabled(true);
                }

                upperPanelB = new JPanel(new GridBagLayout());
                upperPanelB.setBorder(buildTitledBorder("Delta above selected blue voxel intensity"));
                gbc.gridx = 0;
                gbc.gridy = 0;
                gbc.gridwidth = 3;
                gbc.weightx = 1;
                gbc.gridheight = 1;
                gbc.fill = GridBagConstraints.HORIZONTAL;

                upperPanelB.add(upSliderB, gbc);

                gbc.gridwidth = 1;
                gbc.gridx = 3;
                upperPanelB.add(upSliderFieldB, gbc);

                gbc.gridx = 0;
                gbc.gridy = 1;
                gbc.gridwidth = 1;
                gbc.weightx = 0;
                gbc.anchor = GridBagConstraints.WEST;
                gbc.fill = GridBagConstraints.NONE;

                upperPanelB.add(minimumUB, gbc);

                gbc.gridx = 1;
                gbc.anchor = GridBagConstraints.CENTER;
                gbc.weightx = .5;

                upperPanelB.add(upperMidB, gbc);

                gbc.gridx = 2;
                gbc.anchor = GridBagConstraints.EAST;
                gbc.weightx = 0;

                upperPanelB.add(maximumUB, gbc);

                lowSliderB = new JSlider(JSlider.HORIZONTAL, (int) minB, (int) maxB, initialDeltaB);
                lowSliderB.setMajorTickSpacing((int) (maxB - minB) / 10);
                lowSliderB.setPaintTicks(true);
                lowSliderB.setInputMap(JSlider.WHEN_FOCUSED, new InputMap());
                lowSliderB.addKeyListener(this);

                if (disableSlidersB) {
                    lowSliderB.setEnabled(false);
                } else {
                    lowSliderB.setEnabled(true);
                }

                lowSliderB.addChangeListener(this);

                lowSliderFieldB = new JTextField(4);
                lowSliderFieldB.setText(String.valueOf((float) (lowSliderB.getValue() / 100.0f)));
                lowSliderFieldB.addKeyListener(this);

                if (disableSlidersB) {
                    lowSliderFieldB.setEnabled(false);
                    lowSliderFieldB.setEditable(false);
                } else {
                    lowSliderFieldB.setEnabled(true);
                    lowSliderFieldB.setEditable(true);
                }

                maximumLB = new JLabel(String.valueOf((lowSliderB.getMaximum()) / 100.0f));
                maximumLB.setForeground(Color.black);
                maximumLB.setFont(serif12);

                if (disableSlidersB) {
                    maximumLB.setEnabled(false);
                } else {
                    maximumLB.setEnabled(true);
                }

                lowerMidB = new JLabel();
                midB = lowSliderB.getMaximum() / 200.0f;

                if (midB >= 100) {
                    lowerMidB.setText("       " + midB);
                } else {
                    lowerMidB.setText("     " + midB);
                }

                lowerMidB.setForeground(Color.black);
                lowerMidB.setFont(serif12);

                if (disableSlidersB) {
                    lowerMidB.setEnabled(false);
                } else {
                    lowerMidB.setEnabled(true);
                }

                minimumLB = new JLabel(String.valueOf(lowSliderB.getMinimum() / 100.0f));
                minimumLB.setForeground(Color.black);
                minimumLB.setFont(serif12);

                if (disableSlidersB) {
                    minimumLB.setEnabled(false);
                } else {
                    minimumLB.setEnabled(true);
                }

                lowerPanelB = new JPanel(new GridBagLayout());
                lowerPanelB.setBorder(buildTitledBorder("Delta below selected blue voxel intensity"));

                gbc.gridx = 0;
                gbc.gridy = 0;
                gbc.gridwidth = 3;
                gbc.weightx = 1;
                gbc.gridheight = 1;
                gbc.fill = GridBagConstraints.HORIZONTAL;

                lowerPanelB.add(lowSliderB, gbc);

                gbc.gridwidth = 1;
                gbc.gridx = 3;
                lowerPanelB.add(lowSliderFieldB, gbc);

                gbc.gridx = 0;
                gbc.gridy = 1;
                gbc.gridwidth = 1;
                gbc.weightx = 0;
                gbc.anchor = GridBagConstraints.WEST;
                gbc.fill = GridBagConstraints.NONE;

                lowerPanelB.add(minimumLB, gbc);

                gbc.gridx = 1;
                gbc.anchor = GridBagConstraints.CENTER;
                gbc.weightx = .5;

                lowerPanelB.add(lowerMidB, gbc);

                gbc.gridx = 2;
                gbc.anchor = GridBagConstraints.EAST;
                gbc.weightx = 0;

                lowerPanelB.add(maximumLB, gbc);
            } // if (haveBlue)
        } // else haveColor

        sizeCheckbox = new JCheckBox("Unrestricted size");
        sizeCheckbox.setFont(serif12);
        sizeCheckbox.setSelected(true);
        sizeCheckbox.setEnabled(true);
        sizeCheckbox.addActionListener(this);

        String unitString = null;
        String sizeString = null;

        if (frame instanceof ViewJFrameImage) {
            unitString = Unit.getUnitFromLegacyNum(((ViewJFrameImage)frame).getComponentImage().getActiveImage().getFileInfo()[0].getUnitsOfMeasure(0)).toString();

            if (((ViewJFrameImage) frame).getComponentImage().getActiveImage().getNDims() == 2) {
                sizeString = unitString + "^2";
            } else if (((ViewJFrameImage) frame).getComponentImage().getActiveImage().getNDims() == 3) {
                sizeString = unitString + "^3";
            }
        } else if (frame instanceof ViewJFramePaintVasculature) {
            unitString = Unit.getUnitFromLegacyNum(((ViewJFramePaintVasculature)frame).getComponentImage().getActiveImage().getFileInfo()[0].getUnitsOfMeasure(0)).toString();

            if (((ViewJFramePaintVasculature) frame).getComponentImage().getActiveImage().getNDims() == 2) {
                sizeString = unitString + "^2";
            } else if (((ViewJFramePaintVasculature) frame).getComponentImage().getActiveImage().getNDims() == 3) {
                sizeString = unitString + "^3";
            }
        } else if (frame instanceof ViewJFrameTriImage) {
            unitString = Unit.getUnitFromLegacyNum(((ViewJFrameTriImage)frame).getTriImage(ViewJFrameTriImage.AXIAL_A).getActiveImage().getFileInfo()[0].getUnitsOfMeasure(0)).toString();
            sizeString = unitString + "^3";
        } else if ( (frame == null) && (surfacePaint != null) ) {
            unitString = Unit.getUnitFromLegacyNum(surfacePaint.getPaintImage().getFileInfo()[0].getUnitsOfMeasure(0)).toString();
            if (surfacePaint.getPaintImage().getNDims() == 3) {
                sizeString = unitString + "^3";
            }
        } else if ( (frame == null) && (surfacePaint_WM != null) ) {
            unitString = Unit.getUnitFromLegacyNum(surfacePaint_WM.getPaintImage().getFileInfo()[0].getUnitsOfMeasure(0)).toString();
            if (surfacePaint_WM.getPaintImage().getNDims() == 3) {
                sizeString = unitString + "^3";
            }
        }

        maxSizeLabel = new JLabel("Maximum size ( " + sizeString + " )");
        maxSizeLabel.setForeground(Color.black);
        maxSizeLabel.setFont(serif12);
        maxSizeLabel.setEnabled(false);

        maxSizeTextF = new JTextField();
        maxSizeTextF.setText("  ");
        maxSizeTextF.setFont(serif12);
        maxSizeTextF.setEnabled(false);

        distanceCheckbox = new JCheckBox("Unrestricted distance");
        distanceCheckbox.setFont(serif12);
        distanceCheckbox.setSelected(true);
        distanceCheckbox.setEnabled(true);
        distanceCheckbox.addActionListener(this);

        maxDistLabel = new JLabel("Maximum distance ( " + unitString + " )");
        maxDistLabel.setForeground(Color.black);
        maxDistLabel.setFont(serif12);
        maxDistLabel.setEnabled(false);

        maxDistTextF = new JTextField();
        maxDistTextF.setText("  ");
        maxDistTextF.setFont(serif12);
        maxDistTextF.setEnabled(false);

        constrainBoundsCheckbox = new JCheckBox("Constrain region grow to cropping volume");
        constrainBoundsCheckbox.setFont(serif12);
        constrainBoundsCheckbox.setSelected(false);
        constrainBoundsCheckbox.addActionListener(this);

        if (!(frame instanceof ViewJFrameTriImage)) {
            constrainBoundsCheckbox.setEnabled(false);
        }

        if (frame instanceof ViewJFrameImage) {

            if (((ViewJFrameImage) frame).getComponentImage().getActiveImage().getType() == ModelStorageBase.BOOLEAN) {
                enableVariableCheckbox = false;
            } else {
                enableVariableCheckbox = true;
            }
        } else if (frame instanceof ViewJFramePaintVasculature) {

            if (((ViewJFramePaintVasculature) frame).getComponentImage().getActiveImage().getType() ==
                    ModelStorageBase.BOOLEAN) {
                enableVariableCheckbox = false;
            } else {
                enableVariableCheckbox = true;
            }
        } else if (frame instanceof ViewJFrameTriImage) {

            if (((ViewJFrameTriImage) frame).getTriImage(ViewJFrameTriImage.AXIAL_A).getActiveImage().getType() ==
                    ModelStorageBase.BOOLEAN) {
                enableVariableCheckbox = false;
            } else {
                enableVariableCheckbox = true;
            }
        } else if ( (frame == null) && (surfacePaint != null) ) {
            if (surfacePaint.getPaintImage().getType() ==
                    ModelStorageBase.BOOLEAN) {
                enableVariableCheckbox = false;
            } else {
                enableVariableCheckbox = true;
            }
        } else if ( (frame == null) && (surfacePaint_WM != null) ) {
            if (surfacePaint_WM.getPaintImage().getType() ==
                ModelStorageBase.BOOLEAN) {
            enableVariableCheckbox = false;
        } else {
            enableVariableCheckbox = true;
        }
    }

        if (haveColor) {
            enableVariableCheckbox = false;
        }

        variableDeltasCheckbox = new JCheckBox("Vary deltas with region growth");
        variableDeltasCheckbox.setFont(serif12);
        variableDeltasCheckbox.setSelected(false);
        variableDeltasCheckbox.setEnabled(enableVariableCheckbox);

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        paramPanel.add(sizeCheckbox, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        paramPanel.add(maxSizeLabel, gbc);

        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        paramPanel.add(maxSizeTextF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        paramPanel.add(distanceCheckbox, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        paramPanel.add(maxDistLabel, gbc);

        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        paramPanel.add(maxDistTextF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.weightx = 2;
        gbc.gridwidth = 2;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        paramPanel.add(constrainBoundsCheckbox, gbc);

        gbc.gridy = 5;
        paramPanel.add(variableDeltasCheckbox, gbc);

        int yPos = 0;
        gbc.gridx = 0;
        gbc.gridy = yPos++;
        gbc.gridwidth = 1;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        staticPanel.add(positionPanel, gbc);
        gbc.gridy = yPos++;

        if (!haveColor) {
            staticPanel.add(upperPanel, gbc);
            gbc.gridy = yPos++;
            staticPanel.add(lowerPanel, gbc);
            gbc.gridy = yPos++;
            staticPanel.add(maximumPanel, gbc);
            gbc.gridy = yPos++;
        } // if (!haveColor)

        if (haveRed) {
            staticPanel.add(upperPanelR, gbc);
            gbc.gridy = yPos++;
            staticPanel.add(lowerPanelR, gbc);
            gbc.gridy = yPos++;
        } // if (haveRed)

        if (haveGreen) {
            staticPanel.add(upperPanelG, gbc);
            gbc.gridy = yPos++;
            staticPanel.add(lowerPanelG, gbc);
            gbc.gridy = yPos++;
        } // if (haveGreen)

        if (haveBlue) {
            staticPanel.add(upperPanelB, gbc);
            gbc.gridy = yPos++;
            staticPanel.add(lowerPanelB, gbc);
            gbc.gridy = yPos++;
        } // if (haveBlue)

        staticPanel.add(paramPanel, gbc);

        return staticPanel;
    }
    
    /* (non-Javadoc)
     * @see gov.nih.mipav.view.dialogs.JDialogBase#focusLost(java.awt.event.FocusEvent)
     */
    public void focusLost(FocusEvent e)
    {
        super.focusLost(e);

        if ((upSliderField != null) && (e.getSource() == upSliderField)) {

            try {
                int newVal = (int) Float.parseFloat(upSliderField.getText()) * 100;

                if ((newVal > upSlider.getMaximum()) || (newVal < upSlider.getMinimum())) {
                    upSliderField.setText(String.valueOf(upSlider.getValue() / 100.0f));
                } else {
                    upSetFromField = true;
                    upSlider.setValue(newVal);
                }
            } catch (Exception ex) {
                upSliderField.setText(String.valueOf(upSlider.getValue() / 100.0f));
            }
        } 
        if ((lowSliderField != null) && (e.getSource() == lowSliderField)) {

            try {
                int newVal = (int) Float.parseFloat(lowSliderField.getText()) * 100;

                if ((newVal > lowSlider.getMaximum()) || (newVal < lowSlider.getMinimum())) {
                    lowSliderField.setText(String.valueOf(lowSlider.getValue() / 100.0f));
                } else {
                    lowSetFromField = true;
                    lowSlider.setValue(newVal);
                }
            } catch (Exception ex) {
                lowSliderField.setText(String.valueOf(lowSlider.getValue() / 100.0f));
            }
        } 
    }
    
    
 
}
