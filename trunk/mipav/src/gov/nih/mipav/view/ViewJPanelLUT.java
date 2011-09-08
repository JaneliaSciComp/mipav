package gov.nih.mipav.view;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.net.URL;
import java.util.*;

import javax.swing.*;
import javax.swing.border.EtchedBorder;
import javax.swing.event.*;

import WildMagic.LibFoundation.Mathematics.Vector2f;


/**
 * Panel which contains the LUT / HistoLUT components and related GUI components.
 * 
 * @author Evan McCreedy
 * @version 1.0
 */
public class ViewJPanelLUT extends JPanel implements ItemListener, ActionListener, ChangeListener, KeyListener,
        HistoLUTParent {

    // ~ Static fields/initializers
    // -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -1769974472965867218L;

    // ~ Instance fields
    // ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JDialogCT ctDialogA, ctDialogB;

    /** Active mouse cursor index of the imageA, B and GM image A, B. */
    private int cursorIndex, cursorIndexB;

    /** DOCUMENT ME! */
    private boolean entireFlag = true;

    /** DOCUMENT ME! */
    private ModelHistogram histogramA = null;

    /** DOCUMENT ME! */
    private ModelHistogram histogramB = null;

    /** DOCUMENT ME! */
    private ViewJPanelHistoLUT histoPanelA;

    /** DOCUMENT ME! */
    private ViewJPanelHistoLUT histoPanelB;

    /** DOCUMENT ME! */
    private JTextField indexColorATextF, indexColorBTextF;

    /** DOCUMENT ME! */
    private JCheckBox interpCheckBoxA, interpCheckBoxB;

    /** Histogram dialog slider labels of the imageA, B and GM imageA, B. */
    private Hashtable<Integer, JLabel> labelsTable, labelsTableB;

    /** DOCUMENT ME! */
    private JCheckBox logCheckBoxA, logCheckBoxB;

    /** The opacity slider label. */
    private JLabel mouseLabel, mouseLabelB;

    /** Opacity X scale sliders. */
    private JSlider mouseSlider, mouseSliderB;

    /** The labels below the opacity slider. */
    private JLabel[] mouseSliderLabels, mouseSliderLabelsB;

    /** DOCUMENT ME! */
    private JTextField nColorsATextF, nColorsBTextF;

    /** DOCUMENT ME! */
    // private JCheckBox oneBasedLUTCheckBoxImageA;
    /** DOCUMENT ME! */
    private JCheckBox oneBasedLUTCheckBoxImageB;

    /** DOCUMENT ME! */
    private JComboBox outputBox;

    /** DOCUMENT ME! */
    private JComboBox outputBoxB;

    /** DOCUMENT ME! */
    private JPanel panelA;

    /** DOCUMENT ME! */
    private JPanel panelB = null;

    /** DOCUMENT ME! */
    private ViewJFrameHistoLUT panelParent;

    /** X range text field in the imageA, B histogram dialog. */
    private JTextField rangeText, rangeTextB;

    /** X range value of the imageA, B and GM imageA, B. */
    private float rangeX, rangeXB;

    /** Scale range value according to the image min and max. */
    private int scaleRangeA, scaleRangeB;

    /** DOCUMENT ME! */
    private JTabbedPane tabbedPane;

    /** DOCUMENT ME! */
    private JTextField threshLowerBF, threshUpperBF, threshFillBF;

    /** threshold related. */
    private JTextField threshLowerF, threshUpperF, threshFillF;

    /** DOCUMENT ME! */
    private JToolBar toolBarBottom;

    /** center toolbar */
    private JToolBar toolBarCenter;

    /** DOCUMENT ME! */
    private ViewToolBarBuilder toolBarObj;

    /** DOCUMENT ME! */
    private JToolBar toolBarThreshold;

    /** DOCUMENT ME! */
    private JToolBar toolBarTop;

    /** DOCUMENT ME! */
    private JCheckBox updateCheckBoxA = null, updateCheckBoxB = null;

    /** Used to display the volume or area of the voxels/pixels between the upper and lower bounds. */
    private JLabel voxelVolumeLabel;

    /**
     * X range text field in the imageA, B and GM image A, B histogram dialog. Following text fields are used by the
     * tri-planar volume view.
     */
    private JTextField xRangeTextA, xRangeTextB;

    /**
     * Y range text field in the imageA, B and GM image A, B histogram dialog. Following text fields are used by the
     * tri-planar volume view.
     */
    private JTextField yRangeTextA, yRangeTextB;

    /**
     * The location of custom LUT definitions, mostly used for volume rendering purposes. The original set were taken
     * from the Osirix imaging application.
     */
    // public static final String customLUTsLocation = "gov" + File.separator + "nih" + File.separator + "mipav" +
    // File.separator + "view" + File.separator + "WildMagic" + File.separator + "Shaders" + File.separator + "LUTs";
    public static final String customLUTsLocation = "WildMagic/Shaders/LUTs";

    // ~ Constructors
    // ---------------------------------------------------------------------------------------------------

    /**
     * Set up the LUT panel.
     * 
     * @param parent DOCUMENT ME!
     */
    public ViewJPanelLUT(ViewJFrameHistoLUT parent) {
        panelParent = parent;
        entireFlag = parent.getWholeImageFlag();

        initGUI();
    }

    // ~ Methods
    // --------------------------------------------------------------------------------------------------------

    /**
     * end KeyListener.
     * 
     * @param event DOCUMENT ME!
     */

    /**
     * ActionListener.
     * 
     * @param event DOCUMENT ME!
     */

    /**
     * Calls various methods depending on the action.
     * 
     * @param event event that triggered function
     */
    public void actionPerformed(ActionEvent event) {

        String text;
        int nColors = 256;
        String command;

        command = event.getActionCommand();

        if (isImageASelected()) {
            text = nColorsATextF.getText();

            if (MipavUtil.testParameter(text, 2, 256)) {
                nColors = Integer.valueOf(text).intValue();
            }
        } else {
            text = nColorsBTextF.getText();

            if (MipavUtil.testParameter(text, 2, 256)) {
                nColors = Integer.valueOf(text).intValue();
            }
        }

        if (event.getActionCommand().equals("GrayLUT")) {

            if (isImageASelected()) {
                panelParent.getLUTa().makeGrayTransferFunctions();
                panelParent.getLUTa().makeLUT(nColors);
                panelParent.setLUTA(panelParent.getLUTa());
                // oneBasedLUTCheckBoxImageA.setSelected(false);
            } else {
                panelParent.getLUTb().makeGrayTransferFunctions();
                panelParent.getLUTb().makeLUT(nColors);
                panelParent.setLUTB(panelParent.getLUTb());
                oneBasedLUTCheckBoxImageB.setSelected(false);
            }
        } else if (event.getActionCommand().equals("redLUT")) {

            if (isImageASelected()) {
                panelParent.getLUTa().makeRedTransferFunctions();
                panelParent.getLUTa().makeLUT(nColors);
                panelParent.setLUTA(panelParent.getLUTa());
                // oneBasedLUTCheckBoxImageA.setSelected(false);
            } else {
                panelParent.getLUTb().makeRedTransferFunctions();
                panelParent.getLUTb().makeLUT(nColors);
                panelParent.setLUTB(panelParent.getLUTb());
                oneBasedLUTCheckBoxImageB.setSelected(false);
            }
        } else if (event.getActionCommand().equals("greenLUT")) {

            if (isImageASelected()) {
                panelParent.getLUTa().makeGreenTransferFunctions();
                panelParent.getLUTa().makeLUT(nColors);
                panelParent.setLUTA(panelParent.getLUTa());
                // oneBasedLUTCheckBoxImageA.setSelected(false);
            } else {
                panelParent.getLUTb().makeGreenTransferFunctions();
                panelParent.getLUTb().makeLUT(nColors);
                panelParent.setLUTB(panelParent.getLUTb());
                oneBasedLUTCheckBoxImageB.setSelected(false);
            }
        } else if (event.getActionCommand().equals("blueLUT")) {

            if (isImageASelected()) {
                panelParent.getLUTa().makeBlueTransferFunctions();
                panelParent.getLUTa().makeLUT(nColors);
                panelParent.setLUTA(panelParent.getLUTa());
                // oneBasedLUTCheckBoxImageA.setSelected(false);
            } else {
                panelParent.getLUTb().makeBlueTransferFunctions();
                panelParent.getLUTb().makeLUT(nColors);
                panelParent.setLUTB(panelParent.getLUTb());
                oneBasedLUTCheckBoxImageB.setSelected(false);
            }
        } else if (event.getActionCommand().equals("graybrLUT")) {

            if (isImageASelected()) {
                panelParent.getLUTa().makeGrayBRTransferFunctions();
                panelParent.getLUTa().makeLUT(nColors);
                panelParent.setLUTA(panelParent.getLUTa());
                // oneBasedLUTCheckBoxImageA.setSelected(false);
            } else {
                panelParent.getLUTb().makeGrayBRTransferFunctions();
                panelParent.getLUTb().makeLUT(nColors);
                panelParent.setLUTB(panelParent.getLUTb());
                oneBasedLUTCheckBoxImageB.setSelected(false);
            }
        } else if (event.getActionCommand().equals("HotMetalLUT")) {

            if (isImageASelected()) {
                panelParent.getLUTa().makeHotMetalTransferFunctions();
                panelParent.getLUTa().makeLUT(nColors);
                panelParent.setLUTA(panelParent.getLUTa());
                // oneBasedLUTCheckBoxImageA.setSelected(false);
            } else {
                panelParent.getLUTb().makeHotMetalTransferFunctions();
                panelParent.getLUTb().makeLUT(nColors);
                panelParent.setLUTB(panelParent.getLUTb());
                oneBasedLUTCheckBoxImageB.setSelected(false);
            }
        } else if (event.getActionCommand().equals("spectrumLUT")) {

            if (isImageASelected()) {
                panelParent.getLUTa().makeSpectrumTransferFunctions();
                panelParent.getLUTa().makeLUT(nColors);
                panelParent.setLUTA(panelParent.getLUTa());
                // oneBasedLUTCheckBoxImageA.setSelected(false);
            } else {
                panelParent.getLUTb().makeSpectrumTransferFunctions();
                panelParent.getLUTb().makeLUT(nColors);
                panelParent.setLUTB(panelParent.getLUTb());
                oneBasedLUTCheckBoxImageB.setSelected(false);
            }
        } else if (event.getActionCommand().equals("coolHotLUT")) {

            if (isImageASelected()) {
                panelParent.getLUTa().makeCoolHotTransferFunctions();
                panelParent.getLUTa().makeLUT(nColors);
                panelParent.setLUTA(panelParent.getLUTa());
                // oneBasedLUTCheckBoxImageA.setSelected(false);
            } else {
                panelParent.getLUTb().makeCoolHotTransferFunctions();
                panelParent.getLUTb().makeLUT(nColors);
                panelParent.setLUTB(panelParent.getLUTb());
                oneBasedLUTCheckBoxImageB.setSelected(false);
            }
        } else if (event.getActionCommand().equals("skinLUT")) {

            if (isImageASelected()) {
                panelParent.getLUTa().makeSkinTransferFunctions();
                panelParent.getLUTa().makeLUT(nColors);
                panelParent.setLUTA(panelParent.getLUTa());
                // oneBasedLUTCheckBoxImageA.setSelected(false);
            } else {
                panelParent.getLUTb().makeSkinTransferFunctions();
                panelParent.getLUTb().makeLUT(nColors);
                panelParent.setLUTB(panelParent.getLUTb());
                oneBasedLUTCheckBoxImageB.setSelected(false);
            }
        } else if (event.getActionCommand().equals("boneLUT")) {

            if (isImageASelected()) {
                panelParent.getLUTa().makeBoneTransferFunctions();
                panelParent.getLUTa().makeLUT(nColors);
                panelParent.setLUTA(panelParent.getLUTa());
                // oneBasedLUTCheckBoxImageA.setSelected(false);
            } else {
                panelParent.getLUTb().makeBoneTransferFunctions();
                panelParent.getLUTb().makeLUT(nColors);
                panelParent.setLUTB(panelParent.getLUTb());
                oneBasedLUTCheckBoxImageB.setSelected(false);
            }
        } else if (event.getActionCommand().equals("stripedLUT")) {

            if (isImageASelected()) {
                panelParent.getLUTa().makeStripedLUT();
                panelParent.setLUTA(panelParent.getLUTa());
                // oneBasedLUTCheckBoxImageA.setSelected(false);
            } else {
                panelParent.getLUTb().makeStripedLUT();
                panelParent.setLUTB(panelParent.getLUTb());
                oneBasedLUTCheckBoxImageB.setSelected(false);
            }
        } else if (event.getActionCommand().equals("invertLUT")) {

            if (isImageASelected()) {
                panelParent.getLUTa().invertLUT();
                panelParent.setLUTA(panelParent.getLUTa());
            } else {
                panelParent.getLUTb().invertLUT();
                panelParent.setLUTB(panelParent.getLUTb());
            }
        } else if (command.equals("ctPresetsLUT")) {
            if (isImageASelected()) {

                if (getHistoLUTComponentA() != null) {
                    ctDialogA = new JDialogCT(panelParent);
                    ctDialogA.setVisible(true);
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    ctDialogB = new JDialogCT(panelParent);
                    ctDialogB.setVisible(true);
                }
            }
        } else if (event.getActionCommand().equals("linearLUT")) {
            toolBarBottom.getComponentAtIndex(1).setEnabled(true);
            threshUpperF.setEnabled(false);
            threshLowerF.setEnabled(false);
            threshFillF.setEnabled(false);

            toolBarThreshold.getComponentAtIndex(0).setEnabled(false);
            toolBarThreshold.getComponentAtIndex(1).setEnabled(false);
            toolBarThreshold.getComponentAtIndex(3).setEnabled(false);
            toolBarThreshold.getComponentAtIndex(4).setEnabled(false);

            if (isImageASelected()) {

                if (getHistoLUTComponentA() != null) {
                    outputBox.setEnabled(false);
                    getHistoLUTComponentA();
					getHistoLUTComponentA().setMode(ViewJComponentHLUTBase.LINEAR);
                    histoPanelA.updateLUTRecorder();
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    outputBoxB.setEnabled(false);
                    getHistoLUTComponentB();
					getHistoLUTComponentB().setMode(ViewJComponentHLUTBase.LINEAR);
                    histoPanelB.updateLUTRecorder();
                }
            }
        } else if (event.getActionCommand().equals("resetLinearLUT")) {

            if (isImageASelected()) {

                if (getHistoLUTComponentA() != null) {
                    getHistoLUTComponentA().linearMode();
                    histoPanelA.updateLUTRecorder();
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    getHistoLUTComponentB().linearMode();
                    histoPanelB.updateLUTRecorder();
                }
            }
        } else if (event.getActionCommand().equals("evendistriLUT")) {

            if (isImageASelected()) {

                if (getHistoLUTComponentA() != null) {
                    getHistoLUTComponentA().evenDistribution();
                    histoPanelA.updateLUTRecorder();
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    getHistoLUTComponentB().evenDistribution();
                    histoPanelB.updateLUTRecorder();
                }
            }
        } else if (event.getActionCommand().equals("thresholdLUT")) {

            // turn on the run threshold button
            toolBarBottom.getComponentAtIndex(1).setEnabled(false);

            if (isImageASelected()) {

                if (getHistoLUTComponentA() != null) {
                    outputBox.setEnabled(true);
                    panelParent.enableThresholdingItems(true);
                    toolBarThreshold.getComponentAtIndex(0).setEnabled(true);
                    toolBarThreshold.getComponentAtIndex(1).setEnabled(true);
                    toolBarThreshold.getComponentAtIndex(3).setEnabled(true);
                    toolBarThreshold.getComponentAtIndex(4).setEnabled(false);
                    panelParent.getLUTa().makeGrayTransferFunctions();
                    panelParent.getLUTa().makeLUT(nColors);
                    panelParent.getLUTa().setColor(255, new Color(200, 0, 0));
                    panelParent.setLUTA(panelParent.getLUTa());
                    // oneBasedLUTCheckBoxImageA.setSelected(false);
                    getHistoLUTComponentA().dualThresholdMode(ViewJComponentHLUTBase.DUAL_THRESHOLD);
                    threshLowerF.setEnabled(true);
                    threshUpperF.setEnabled(true);
                    threshFillF.setEnabled(true);

                    float upper = ((Vector2f) (getLUTa().getTransferFunction().getPoint(4))).X;
                    float lower = ((Vector2f) (getLUTa().getTransferFunction().getPoint(1))).X;
                    threshLowerF.setText(Float.toString(lower));
                    threshUpperF.setText(Float.toString(upper));
                    threshFillF.setText(Double.toString(panelParent.getImageA().getMin()));

                    if (panelParent.doCalcThresholdVolume()) {
                        calculateThreshold(lower, upper);
                    }

                    histoPanelA.updateLUTRecorder();
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    outputBox.setEnabled(true);
                    panelParent.enableThresholdingItems(true);
                    toolBarThreshold.getComponentAtIndex(0).setEnabled(true);
                    toolBarThreshold.getComponentAtIndex(1).setEnabled(true);
                    toolBarThreshold.getComponentAtIndex(3).setEnabled(true);
                    toolBarThreshold.getComponentAtIndex(4).setEnabled(false);
                    panelParent.getLUTb().makeGrayTransferFunctions();
                    panelParent.getLUTb().makeLUT(nColors);
                    panelParent.getLUTb().setColor(255, new Color(200, 0, 0));
                    panelParent.setLUTB(panelParent.getLUTb());
                    oneBasedLUTCheckBoxImageB.setSelected(false);
                    getHistoLUTComponentB().dualThresholdMode(ViewJComponentHLUTBase.DUAL_THRESHOLD);
                    threshLowerBF.setEnabled(true);
                    threshUpperBF.setEnabled(true);
                    threshFillBF.setEnabled(true);

                    float upper = ((Vector2f) (getLUTb().getTransferFunction().getPoint(4))).X;
                    float lower = ((Vector2f) (getLUTb().getTransferFunction().getPoint(1))).X;
                    threshLowerBF.setText(Float.toString(lower));
                    threshUpperBF.setText(Float.toString(upper));

                    if (panelParent.doCalcThresholdVolume()) {
                        calculateThreshold(lower, upper);
                    }

                    threshFillBF.setText(Double.toString(panelParent.getImageB().getMin()));
                    histoPanelA.updateLUTRecorder();
                }
            }
        } else if (event.getActionCommand().equals("inverseThresholdLUT")) {

            // turn on the run threshold button
            // toolBarBottom.getComponentAtIndex(13).setEnabled(true);
            toolBarBottom.getComponentAtIndex(1).setEnabled(false);

            if (isImageASelected()) {

                if (getHistoLUTComponentA() != null) {
                    outputBox.setEnabled(true);
                    panelParent.enableThresholdingItems(true);
                    toolBarThreshold.getComponentAtIndex(0).setEnabled(true);
                    toolBarThreshold.getComponentAtIndex(1).setEnabled(true);
                    toolBarThreshold.getComponentAtIndex(3).setEnabled(false);
                    toolBarThreshold.getComponentAtIndex(4).setEnabled(true);
                    panelParent.getLUT().setColor(255, new Color(200, 0, 0));
                    panelParent.setLUTA(panelParent.getLUTa());
                    // oneBasedLUTCheckBoxImageA.setSelected(false);
                    getHistoLUTComponentA().dualThresholdMode(ViewJComponentHLUTBase.DUAL_THRESHOLD_INV);
                    threshLowerF.setEnabled(true);
                    threshUpperF.setEnabled(true);
                    threshFillF.setEnabled(true);

                    float upper = ((Vector2f) (getLUTa().getTransferFunction().getPoint(3))).X;
                    float lower = ((Vector2f) (getLUTa().getTransferFunction().getPoint(1))).X;
                    threshLowerF.setText(Float.toString(lower));
                    threshUpperF.setText(Float.toString(upper));

                    if (panelParent.doCalcThresholdVolume()) {
                        calculateThreshold(lower, upper);
                    }

                    threshFillF.setText(Double.toString(panelParent.getImageA().getMin()));
                    histoPanelA.updateLUTRecorder();
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    outputBoxB.setEnabled(true);
                    panelParent.enableThresholdingItems(true);
                    toolBarThreshold.getComponentAtIndex(0).setEnabled(true);
                    toolBarThreshold.getComponentAtIndex(1).setEnabled(true);
                    toolBarThreshold.getComponentAtIndex(3).setEnabled(false);
                    toolBarThreshold.getComponentAtIndex(4).setEnabled(true);
                    panelParent.getLUTb().setColor(255, new Color(200, 0, 0));
                    panelParent.setLUTB(panelParent.getLUTb());
                    oneBasedLUTCheckBoxImageB.setSelected(false);
                    getHistoLUTComponentB().dualThresholdMode(ViewJComponentHLUTBase.DUAL_THRESHOLD_INV);
                    threshLowerBF.setEnabled(true);
                    threshUpperBF.setEnabled(true);
                    threshFillBF.setEnabled(true);

                    float upper = ((Vector2f) (getLUTa().getTransferFunction().getPoint(3))).X;
                    float lower = ((Vector2f) (getLUTa().getTransferFunction().getPoint(1))).X;
                    threshLowerBF.setText(Float.toString(lower));
                    threshUpperBF.setText(Float.toString(upper));

                    if (panelParent.doCalcThresholdVolume()) {
                        calculateThreshold(lower, upper);
                    }

                    threshFillBF.setText(Double.toString(panelParent.getImageB().getMin()));
                    histoPanelA.updateLUTRecorder();
                }
            }
        } else if (event.getActionCommand().equals("runInverseThreshold")
                || event.getActionCommand().equals("runThreshold")) {
            boolean isInverse = true;

            if (event.getActionCommand().equals("runThreshold")) {
                isInverse = false;
            }

            if (isImageASelected()) {

                if (MipavUtil.testParameter(threshLowerF.getText(), panelParent.getImageA().getMin(),
                        ((Vector2f) (getLUTa().getTransferFunction().getPoint(3))).X)) {

                    if (MipavUtil.testParameter(threshUpperF.getText(), ((Vector2f) (getLUTa().getTransferFunction()
                            .getPoint(2))).X, panelParent.getImageA().getMax())) {

                        int outputType = outputBox.getSelectedIndex();

                        if (outputType == 0) {
                            outputType = AlgorithmThresholdDual.ORIGINAL_TYPE;
                        } else if (outputType == 1) {
                            outputType = AlgorithmThresholdDual.BINARY_TYPE;
                        } else { // outputType == 2
                            outputType = AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE;
                        }

                        // run threshold algorithm
                        JDialogThreshold threshD = new JDialogThreshold();

                        threshD.runFromLUTFrame(panelParent.getImageA(),
                                new Float(threshLowerF.getText()).floatValue(), new Float(threshUpperF.getText())
                                        .floatValue(), new Float(threshFillF.getText()).floatValue(), outputType,
                                isInverse);

                    } else {
                        threshUpperF.requestFocus();
                        threshUpperF.selectAll();
                    }

                } else {
                    threshLowerF.requestFocus();
                    threshLowerF.selectAll();
                }
            } else {

                if (MipavUtil.testParameter(threshLowerBF.getText(), panelParent.getImageB().getMin(),
                        ((Vector2f) (getLUTb().getTransferFunction().getPoint(3))).X)) {

                    if (MipavUtil.testParameter(threshUpperBF.getText(), ((Vector2f) (getLUTb().getTransferFunction()
                            .getPoint(2))).X, panelParent.getImageB().getMax())) {

                        int outputType = outputBoxB.getSelectedIndex();

                        if (outputType == 0) {
                            outputType = AlgorithmThresholdDual.ORIGINAL_TYPE;
                        } else if (outputType == 1) {
                            outputType = AlgorithmThresholdDual.BINARY_TYPE;
                        } else { // outputType == 2
                            outputType = AlgorithmThresholdDual.UNSIGNED_BYTE_TYPE;
                        }

                        // run threshold algorithm
                        JDialogThreshold threshD = new JDialogThreshold();

                        threshD.runFromLUTFrame(panelParent.getImageB(), new Float(threshLowerBF.getText())
                                .floatValue(), new Float(threshUpperBF.getText()).floatValue(), new Float(threshFillBF
                                .getText()).floatValue(), outputType, isInverse);

                    } else {
                        threshUpperBF.requestFocus();
                        threshUpperBF.selectAll();
                    }

                } else {
                    threshLowerBF.requestFocus();
                    threshLowerBF.selectAll();
                }

            }
        } else if (event.getActionCommand().equals("otsuThreshold")) {

            if (isImageASelected()) {
                int maxBin = histogramA.getOtsuThreshold();

                double dif = panelParent.getImageA().getMax() - panelParent.getImageA().getMin();

                double factor = dif / histogramA.getExtents()[0];

                double otsu = ( (maxBin * factor) + panelParent.getImageA().getMin());

                if ( (otsu > panelParent.getImageA().getMin()) && (otsu < panelParent.getImageA().getMax())) {

                    getHistoLUTComponentA();
					if (getHistoLUTComponentA().getMode() == ViewJComponentHLUTBase.DUAL_THRESHOLD_INV) {
                        threshLowerF.setText(Double.toString(otsu));
                        threshUpperF.setText(Double.toString(panelParent.getImageA().getMax()));
                        getHistoLUTComponentA().updateDualThreshold((float) otsu,
                                (float) panelParent.getImageA().getMax());
                        calculateThreshold((float) otsu, (float) panelParent.getImageA().getMax());
                    } else {
                        threshUpperF.setText(Double.toString(otsu));
                        threshLowerF.setText(Double.toString(panelParent.getImageA().getMin()));
                        getHistoLUTComponentA().updateDualThreshold((float) panelParent.getImageA().getMin(),
                                (float) otsu);
                        calculateThreshold((float) panelParent.getImageA().getMin(), (float) otsu);
                    }
                }
            } else {
                int maxBin = histogramB.getOtsuThreshold();

                double dif = panelParent.getImageB().getMax() - panelParent.getImageB().getMin();

                double factor = dif / histogramB.getExtents()[0];

                double otsu = ( (maxBin * factor) + panelParent.getImageB().getMin());

                if ( (otsu > panelParent.getImageB().getMin()) && (otsu < panelParent.getImageB().getMax())) {

                    getHistoLUTComponentB();
					if (getHistoLUTComponentB().getMode() == ViewJComponentHLUTBase.DUAL_THRESHOLD_INV) {
                        threshLowerBF.setText(Double.toString(otsu));
                        threshUpperBF.setText(Double.toString(panelParent.getImageB().getMax()));
                        getHistoLUTComponentB().updateDualThreshold((float) otsu,
                                (float) panelParent.getImageB().getMax());

                        if (panelParent.doCalcThresholdVolume()) {
                            calculateThreshold((float) otsu, (float) panelParent.getImageA().getMax());
                        }
                    } else {
                        threshUpperBF.setText(Double.toString(otsu));
                        threshLowerBF.setText(Double.toString(panelParent.getImageB().getMin()));
                        getHistoLUTComponentB().updateDualThreshold((float) panelParent.getImageB().getMin(),
                                (float) otsu);

                        if (panelParent.doCalcThresholdVolume()) {
                            calculateThresholdVolume((float) panelParent.getImageA().getMin(), (float) otsu);
                        }
                    }
                }

            }
        } else if (event.getActionCommand().equals("maxEntThreshold")) {

            if (isImageASelected()) {

                int maxBin = histogramA.getMaxEntropyThreshold();

                double dif = panelParent.getImageA().getMax() - panelParent.getImageA().getMin();

                double factor = dif / histogramA.getExtents()[0];

                double ent = ( (maxBin * factor) + panelParent.getImageA().getMin());

                if ( (ent > panelParent.getImageA().getMin()) && (ent < panelParent.getImageA().getMax())) {

                    getHistoLUTComponentA();
					if (getHistoLUTComponentA().getMode() == ViewJComponentHLUTBase.DUAL_THRESHOLD_INV) {
                        threshLowerF.setText(Double.toString(ent));
                        threshUpperF.setText(Double.toString(panelParent.getImageA().getMax()));
                        getHistoLUTComponentA().updateDualThreshold((float) ent,
                                (float) panelParent.getImageA().getMax());

                        if (panelParent.doCalcThresholdVolume()) {
                            calculateThreshold((float) ent, (float) panelParent.getImageA().getMax());
                        }
                    } else {
                        threshUpperF.setText(Double.toString(ent));
                        threshLowerF.setText(Double.toString(panelParent.getImageA().getMin()));
                        getHistoLUTComponentA().updateDualThreshold((float) panelParent.getImageA().getMin(),
                                (float) ent);

                        if (panelParent.doCalcThresholdVolume()) {
                            calculateThreshold((float) panelParent.getImageA().getMin(), (float) ent);
                        }
                    }

                }
            } else {
                int maxBin = histogramB.getMaxEntropyThreshold();

                double dif = panelParent.getImageB().getMax() - panelParent.getImageB().getMin();

                double factor = dif / histogramB.getExtents()[0];

                double ent = ( (maxBin * factor) + panelParent.getImageB().getMin());

                if ( (ent > panelParent.getImageB().getMin()) && (ent < panelParent.getImageB().getMax())) {

                    getHistoLUTComponentB();
					if (getHistoLUTComponentB().getMode() == ViewJComponentHLUTBase.DUAL_THRESHOLD_INV) {
                        threshLowerBF.setText(Double.toString(ent));
                        threshUpperBF.setText(Double.toString(panelParent.getImageB().getMax()));
                        getHistoLUTComponentB().updateDualThreshold((float) ent,
                                (float) panelParent.getImageB().getMax());
                    } else {
                        threshUpperBF.setText(Double.toString(ent));
                        threshLowerBF.setText(Double.toString(panelParent.getImageB().getMin()));
                        getHistoLUTComponentB().updateDualThreshold((float) panelParent.getImageB().getMin(),
                                (float) ent);
                    }
                }
            }
        } else if (event.getActionCommand().equals("alpha")) {

            if (isImageASelected()) {

                if (getHistoLUTComponentA() != null) {
                    getHistoLUTComponentA();
					getHistoLUTComponentA().setMode(ViewJComponentHLUTBase.ALPHA);
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    getHistoLUTComponentB();
					getHistoLUTComponentB().setMode(ViewJComponentHLUTBase.ALPHA);
                }
            }
        } else if (event.getActionCommand().equals("red")) {

            if (isImageASelected()) {

                if (getHistoLUTComponentA() != null) {
                    getHistoLUTComponentA();
					getHistoLUTComponentA().setMode(ViewJComponentHLUTBase.RED);
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    getHistoLUTComponentB();
					getHistoLUTComponentB().setMode(ViewJComponentHLUTBase.RED);
                }
            }
        } else if (event.getActionCommand().equals("green")) {

            if (isImageASelected()) {

                if (getHistoLUTComponentA() != null) {
                    getHistoLUTComponentA();
					getHistoLUTComponentA().setMode(ViewJComponentHLUTBase.GREEN);
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    getHistoLUTComponentB();
					getHistoLUTComponentB().setMode(ViewJComponentHLUTBase.GREEN);
                }
            }
        } else if (event.getActionCommand().equals("blue")) {

            if (isImageASelected()) {

                if (getHistoLUTComponentA() != null) {
                    getHistoLUTComponentA();
					getHistoLUTComponentA().setMode(ViewJComponentHLUTBase.BLUE);
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    getHistoLUTComponentB();
					getHistoLUTComponentB().setMode(ViewJComponentHLUTBase.BLUE);
                }
            }
        } else if (event.getActionCommand().equals("Threshold")) {

            if (panelParent.doCalcThresholdVolume()) {
                calcThreshold();
            }

            if (panelParent.getImageA().getType() == ModelStorageBase.BOOLEAN) {
                setVisible(false);
                panelParent.getImageA().removeImageDisplayListener(panelParent);

                if (panelParent.getImageB() != null) {
                    panelParent.getImageB().removeImageDisplayListener(panelParent);
                }

                dispose();
            }

            updateFrames(false);
        } else if (event.getActionCommand().equals("OpenUDLUT") || event.getActionCommand().equals("SaveUDLUT")) {
            panelParent.actionPerformed(event);
        } else if (event.getActionCommand().equals("GenerateLUT")) {
            histoPanelA.showLUTRecorder();
        } else if (event.getSource() instanceof JComboBox) {
            JComboBox cb = (JComboBox) event.getSource();
            String lutName = (String) cb.getSelectedItem();
            if (isImageASelected()) {
                panelParent.getLUTa().makeCustomizedLUT(lutName);
                panelParent.setLUTA(panelParent.getLUTa());
                updateFrames(false);
            } else {
                panelParent.getLUTb().makeCustomizedLUT(lutName);
                panelParent.setLUTB(panelParent.getLUTb());
                updateFrames(false);
            }
        }

        // Setup threshold buttons to be enabled or disabled.
        if (isImageASelected()) {

            if ( (getHistoLUTComponentA() != null)
                    && (getHistoLUTComponentA().getMode() != ViewJComponentHistoLUT.DUAL_THRESHOLD)) {
                panelParent.enableThresholdingItems(false);
            }
        } else {

            if ( (getHistoLUTComponentB() != null)
                    && (getHistoLUTComponentB().getMode() != ViewJComponentHistoLUT.DUAL_THRESHOLD)) {
                panelParent.enableThresholdingItems(false);
            }
        }

    }

    /**
     * DOCUMENT ME!
     */
    public void clearVoxelLabel() {

        if (panelParent.getImageA().getNDims() == 3) {
            voxelVolumeLabel.setText("Threshold volume(red):");
        } else {
            voxelVolumeLabel.setText("Threshold area(red):");
        }
    }

    /**
     * Disposes of components and frame.
     */
    public void dispose() {
        getHistoLUTComponentA().dispose();
        getLUTComponentA().dispose(true);
        histogramA.disposeLocal();

        if (histogramB != null) {
            getHistoLUTComponentB().dispose();
            getLUTComponentB().dispose(true);
            histogramB.disposeLocal();
        }

        if (histoPanelA != null) {
            histoPanelA.finalize();
            histoPanelA = null;
        }

        if (histoPanelB != null) {
            histoPanelB.finalize();
            histoPanelB = null;
        }
    }

    /**
     * Placeholder.
     * 
     * @param mouseEvent drag event
     */
    public void dragPoint(MouseEvent mouseEvent) {}

    /**
     * Get the histogram component for imageA.
     * 
     * @return the imageA histogram component
     */
    public final ViewJComponentHLUTBase getHistoLUTComponentA() {
        return histoPanelA.getHistoLUTComponent();
    }

    /**
     * Get the histogram component for imageB.
     * 
     * @return the imageB histogram component
     */
    public final ViewJComponentHLUTBase getHistoLUTComponentB() {
        return histoPanelB.getHistoLUTComponent();
    }

    /**
     * Returns the lower threshold value.
     * 
     * @return float lower thresh
     */
    public float getLowerThreshold() {

        if (isImageASelected()) {
            return new Float(threshLowerF.getText()).floatValue();
        } else {
            return new Float(threshLowerBF.getText()).floatValue();
        }
    }

    /**
     * Get the imageA histo component lut.
     * 
     * @return ModelLUT
     */
    public final ModelLUT getLUTa() {
        return ((ViewJComponentHistoLUT) getHistoLUTComponentA()).getLUT();
    }

    /**
     * Get the imageB histo component lut.
     * 
     * @return ModelLUT
     */
    public final ModelLUT getLUTb() {
        return ((ViewJComponentHistoLUT) getHistoLUTComponentB()).getLUT();
    }

    /**
     * Get the imageA LUT.
     * 
     * @return the imageA LUT component
     */
    public final ViewJComponentLUT getLUTComponentA() {
        return histoPanelA.getLUTComponent();
    }

    /**
     * Get the imageB LUT.
     * 
     * @return the imageB LUT component
     */
    public final ViewJComponentLUT getLUTComponentB() {
        return histoPanelB.getLUTComponent();
    }

    /**
     * Get the LUT recorder reference.
     * 
     * @return JDialogRecordLUT reference to LUT recorder.
     */
    public JDialogRecordLUT getLUTRecorder() {
        return histoPanelA.getLUTRecorder();
    }

    /**
     * Returns the upper threshold value.
     * 
     * @return float upper thresh
     */
    public float getUpperThreshold() {

        if (isImageASelected()) {
            return new Float(threshUpperF.getText()).floatValue();
        } else {
            return new Float(threshUpperBF.getText()).floatValue();
        }
    }

    /**
     * Set up the panel components.
     */
    public void initGUI() {
        setBackground(new Color(160, 160, 160));

        toolBarObj = new ViewToolBarBuilder(this);

        toolBarTop = toolBarObj.buildLUTToolBarTop();
        toolBarCenter = buildLUTSelectionList(this);
        toolBarBottom = toolBarObj.buildLUTToolBarBottom();
        toolBarThreshold = toolBarObj.buildLUTThresholdToolBar();

        if (panelParent.getImageA().getNDims() == 3) {
            voxelVolumeLabel = new JLabel("Threshold volume(red):");
        } else {
            voxelVolumeLabel = new JLabel("Threshold area(red):");
        }

        voxelVolumeLabel.setFont(MipavUtil.font12);

        toolBarThreshold.add(voxelVolumeLabel);

        JPanel topPanel = new JPanel(new BorderLayout());

        topPanel.add(toolBarTop, BorderLayout.NORTH);
        topPanel.add(toolBarCenter, BorderLayout.CENTER);
        topPanel.add(toolBarBottom, BorderLayout.SOUTH);

        JPanel fullPanel = new JPanel(new BorderLayout());

        fullPanel.add(topPanel, BorderLayout.NORTH);
        fullPanel.add(toolBarThreshold, BorderLayout.SOUTH);

        this.setLayout(new BorderLayout());
        this.add(fullPanel, BorderLayout.NORTH);

        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.font12B);
        buildPanelA(panelParent.getImageA(), panelParent.getLUTa(), entireFlag);

        if (panelParent.getImageB() != null) {
            buildPanelB(panelParent.getImageB(), panelParent.getLUTb(), entireFlag);
        }
        if(panelParent.getUserInterface().getActiveImageFrame().getActiveImage() == panelParent.getImageA()) {
        	tabbedPane.setSelectedIndex(0);
        }else {
        	tabbedPane.setSelectedIndex(1);
        }
        
        tabbedPane.addChangeListener(this);
        this.add(tabbedPane, BorderLayout.SOUTH);
        validate();
    }

    /**
     * Build the center part of the LUT toolbar.
     * 
     * @param listener The listener to attach to the created LUT selection combo box.
     * 
     * @return the top part of the LUT toolbar
     */
    public static final JToolBar buildLUTSelectionList(ActionListener listener) {

        JToolBar LUTToolBar = new JToolBar();
        LUTToolBar.setBorderPainted(true);
        LUTToolBar.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        LUTToolBar.setFloatable(false);

        Vector<String> lutStrings = getCustomLUTList();

        JComboBox lutList = new JComboBox(lutStrings);
        lutList.setBackground(Color.white);
        lutList.setSelectedIndex(0);
        lutList.addActionListener(listener);

        LUTToolBar.add(lutList);

        return LUTToolBar;
    }

    private static final Vector<String> getCustomLUTList() {
        String listingFilename = ViewJPanelLUT.customLUTsLocation + "/LUT_listing";

        // use this long call instead of ClassLoader.getSystemResource() to work properly from a jnlp launch
        URL listingFileURL = Thread.currentThread().getContextClassLoader().getResource(listingFilename);

        if (listingFileURL == null) {
            Preferences.debug("Unable to open " + listingFilename + ".\n", Preferences.DEBUG_MINOR);
        }

        // use buffering this implementation reads one line at a time
        Vector<String> lutStrings = new Vector<String>();
        try {
            // reading from a buffered reader pointed to a directory should return the files contained within it
            BufferedReader br = new BufferedReader(new InputStreamReader(listingFileURL.openStream()));
            String line = null;
            while ( (line = br.readLine()) != null) {
                lutStrings.add(line);
            }
        } catch (IOException e) {
            Preferences.debug("Unable to create custom LUT list: " + e.getMessage() + ".\n", Preferences.DEBUG_MINOR);
            e.printStackTrace();
        }

        return lutStrings;
    }

    /**
     * Returns whether the imageA LUT panel is the one being worked on.
     * 
     * @return whether the imageA LUT panel is the one being worked on
     */
    public boolean isImageASelected() {
        return tabbedPane.getSelectedComponent() == panelA;
    }

    /**
     * Returns whether the imageB LUT panel is the one being worked on.
     * 
     * @return whether the imageB LUT panel is the one being worked on
     */
    public boolean isImageBSelected() {
        return tabbedPane.getSelectedComponent() == panelB;
    }

    /**
     * {@inheritDoc}
     */
    public boolean isImageUpdate() {
        return updateCheckBoxA.isSelected();
    }

    /**
     * end ActionListener.
     * 
     * @param event DOCUMENT ME!
     */

    /**
     * ItemListener.
     * 
     * @param event DOCUMENT ME!
     */

    /**
     * Sets the flags for the checkboxes.
     * 
     * @param event event that triggered this function
     */
    public synchronized void itemStateChanged(ItemEvent event) {

        Object source = event.getSource();

        if (source == logCheckBoxA) {

            if (logCheckBoxA.isSelected() == true) {
                getHistoLUTComponentA().setLogFlag(true);
            } else {
                getHistoLUTComponentA().setLogFlag(false);
            }

            getHistoLUTComponentA().showHistogram();
        } else if (source == logCheckBoxB) {

            if (logCheckBoxB.isSelected() == true) {
                getHistoLUTComponentB().setLogFlag(true);
            } else {
                getHistoLUTComponentB().setLogFlag(false);
            }

            getHistoLUTComponentB().showHistogram();
        } else if (source == interpCheckBoxA) {
            // the following code could look a little screwey to some people, like maybe there is a simpler way of doing
            // it. basically, the interpolation of images used to be controlled by one variable, and both images were
            // interpolated according to this variable. however, now we want to be able to interpolate the images
            // independently. so without changing the code structure, i added some constants that represent whether or
            // not to interpolate each image. note: INTERPOLATE_A means interpolate image A only. likewise for image B.
            // NEAREST_BOTH means no interpolation for either image

            int interpMode = 0;

            if (interpCheckBoxA.isSelected() == true) {

                if (interpCheckBoxB != null) {

                    if (interpCheckBoxB.isSelected() == true) {
                        interpMode = ViewJComponentBase.INTERPOLATE_BOTH;
                    } else {
                        interpMode = ViewJComponentBase.INTERPOLATE_A;
                    }
                } else {
                    interpMode = ViewJComponentBase.INTERPOLATE_A;
                }
            } else {

                if (interpCheckBoxB != null) {

                    if (interpCheckBoxB.isSelected() == true) {
                        interpMode = ViewJComponentBase.INTERPOLATE_B;
                    } else {
                        interpMode = ViewJComponentBase.NEAREST_BOTH;
                    }
                } else {
                    interpMode = ViewJComponentBase.NEAREST_BOTH;
                }
            }

            // the -50 means do not change the opacity of the image (actually, any negative value
            // will serve not to change the opacity levels. previously, this was hard-coded to "50"
            // but the problem was that then the opacity would change every time the images were
            // interpolated or de-interpolated
            panelParent.getImageA().notifyImageDisplayListeners(panelParent.getLUTa(), true, -50, interpMode);
        } else if (source == interpCheckBoxB) {
            int interpMode = 0;

            if (interpCheckBoxB.isSelected() == true) {

                if (interpCheckBoxA.isSelected() == true) {
                    interpMode = ViewJComponentBase.INTERPOLATE_BOTH;
                } else {
                    interpMode = ViewJComponentBase.INTERPOLATE_B;
                }
            } else {

                if (interpCheckBoxA.isSelected() == true) {
                    interpMode = ViewJComponentBase.INTERPOLATE_A;
                } else {
                    interpMode = ViewJComponentBase.NEAREST_BOTH;
                }
            }

            panelParent.getImageB().notifyImageDisplayListeners(panelParent.getLUTb(), true, -50, interpMode);
        } else if (source == updateCheckBoxA) {

            if (updateCheckBoxB != null) {
                updateCheckBoxB.removeItemListener(this);
            }

            if ( (updateCheckBoxA.isSelected() == true) && (updateCheckBoxB != null)) {
                updateCheckBoxB.setSelected(true);
            } else if ( (updateCheckBoxA.isSelected() == false) && (updateCheckBoxB != null)) {
                updateCheckBoxB.setSelected(false);
            }

            if (updateCheckBoxB != null) {
                updateCheckBoxB.addItemListener(this);
            }
        } else if (source == updateCheckBoxB) {
            updateCheckBoxB.removeItemListener(this);

            if (updateCheckBoxB.isSelected() == true) {
                updateCheckBoxA.setSelected(true);
            } else {
                updateCheckBoxA.setSelected(false);
            }

            updateCheckBoxA.addItemListener(this);
        } else if (source == outputBox) {
            threshFillF.setEnabled(outputBox.getSelectedIndex() == AlgorithmThresholdDual.ORIGINAL_TYPE);
        } else if (source == outputBoxB) {
            threshFillBF.setEnabled(outputBoxB.getSelectedIndex() == AlgorithmThresholdDual.ORIGINAL_TYPE);
        } /*
         * else if (source == oneBasedLUTCheckBoxImageA) { // get the color of the LUT index 0 Color zeroIndexColor =
         * getLUTa().getColor(0); // test to see if the color is R == 0, G == 0, B == 0 boolean zeroIndexColorIs000 =
         * ((zeroIndexColor.getRed() == 0) && (zeroIndexColor.getGreen() == 0) && (zeroIndexColor.getBlue() == 0));
         * boolean zeroIndexColorIs111 = ((zeroIndexColor.getRed() == 1) && (zeroIndexColor.getGreen() == 1) &&
         * (zeroIndexColor.getBlue() == 1)); // if the user wants a 1-based LUT if
         * (oneBasedLUTCheckBoxImageA.isSelected() == true) { // only change index 0 to 1's if it is currently R ==
         * 0, G == 0, B == 0. if (zeroIndexColorIs000 == true) { getLUTa().setColor(0, new Color(1, 1, 1)); } } else { //
         * only change index 1 to 0's if it is currently R == 1, G == 1, B == 1. if (zeroIndexColorIs111 == true) {
         * getLUTa().setColor(0, new Color(0, 0, 0)); } }
         * 
         * updateFrames(false); }
         */else if (source == oneBasedLUTCheckBoxImageB) {

            // get the color of the LUT index 0
            Color zeroIndexColor = getLUTb().getColor(0);

            // test to see if the color is R == 0, G == 0, B == 0
            boolean zeroIndexColorIs000 = ( (zeroIndexColor.getRed() == 0) && (zeroIndexColor.getGreen() == 0) && (zeroIndexColor
                    .getBlue() == 0));
            boolean zeroIndexColorIs111 = ( (zeroIndexColor.getRed() == 1) && (zeroIndexColor.getGreen() == 1) && (zeroIndexColor
                    .getBlue() == 1));

            // if the user wants a 1-based LUT
            if (oneBasedLUTCheckBoxImageB.isSelected() == true) {

                // only change index 0 to 1's if it is currently R == 0, G == 0, B == 0.
                if (zeroIndexColorIs000 == true) {
                    getLUTb().setColor(0, new Color(1, 1, 1));
                }
            } else {

                // only change index 1 to 0's if it is currently R == 1, G == 1, B == 1.
                if (zeroIndexColorIs111 == true) {
                    getLUTb().setColor(0, new Color(0, 0, 0));
                }
            }

            updateFrames(false);
        }

    }

    /**
     * KeyListener.
     * 
     * @param e DOCUMENT ME!
     */

    /**
     * Unchanged.
     * 
     * @param e DOCUMENT ME!
     */
    public void keyPressed(KeyEvent e) {}

    /**
     * Unchanged.
     * 
     * @param e DOCUMENT ME!
     */
    public void keyReleased(KeyEvent e) {}

    /**
     * If the ENTER key is hit while in threshold boxes, update the LUT's threshold (for dual threshold).
     * 
     * @param e KeyEvent
     */
    public void keyTyped(KeyEvent e) {

        if (e.getKeyChar() == KeyEvent.VK_ENTER) {

            if (e.getSource().equals(threshLowerF)) {

                if (MipavUtil.testParameter(threshLowerF.getText(), panelParent.getImageA().getMin(),
                        ((Vector2f) (getLUTa().getTransferFunction().getPoint(3))).X)) {

                    getHistoLUTComponentA().updateDualThreshold(new Float(threshLowerF.getText()).floatValue(),
                            new Float(threshUpperF.getText()).floatValue());
                } else {
                    threshLowerF.requestFocus();
                    threshLowerF.selectAll();
                }
            } else if (e.getSource().equals(threshUpperF)) {

                if (MipavUtil.testParameter(threshUpperF.getText(), ((Vector2f) (getLUTa().getTransferFunction()
                        .getPoint(2))).X, panelParent.getImageA().getMax())) {
                    getHistoLUTComponentA().updateDualThreshold(new Float(threshLowerF.getText()).floatValue(),
                            new Float(threshUpperF.getText()).floatValue());
                } else {
                    threshUpperF.requestFocus();
                    threshUpperF.selectAll();
                }
            } else if (e.getSource().equals(threshFillF)) {

                // System.err.println("MIN: " + imageA.getMin() + " MAX: " + imageA.getMax());
                if (MipavUtil.testParameter(threshFillF.getText(), panelParent.getImageA().getMin(), panelParent
                        .getImageA().getMax())) { //
                    // componentHistogramA.updateDualThreshold(new
                    //
                    // Float(threshFillF.getText()).floatValue(),
                    // -1);
                } else {
                    threshFillF.requestFocus();
                    threshFillF.selectAll();
                }
            } else if (e.getSource().equals(threshLowerBF)) {

                if (MipavUtil.testParameter(threshLowerBF.getText(), panelParent.getImageB().getMin(),
                        ((Vector2f) (getLUTb().getTransferFunction().getPoint(3))).X)) {

                    getHistoLUTComponentB().updateDualThreshold(new Float(threshLowerBF.getText()).floatValue(),
                            new Float(threshUpperBF.getText()).floatValue());
                } else {
                    threshLowerBF.requestFocus();
                    threshLowerBF.selectAll();
                }
            } else if (e.getSource().equals(threshUpperBF)) {

                if (MipavUtil.testParameter(threshUpperBF.getText(), ((Vector2f) (getLUTb().getTransferFunction()
                        .getPoint(2))).X, panelParent.getImageB().getMax())) {
                    getHistoLUTComponentB().updateDualThreshold(new Float(threshLowerBF.getText()).floatValue(),
                            new Float(threshUpperBF.getText()).floatValue());
                } else {
                    threshUpperBF.requestFocus();
                    threshUpperBF.selectAll();
                }
            } else if (e.getSource().equals(threshFillBF)) {

                // System.err.println("MIN: " + imageA.getMin() + " MAX: " + imageA.getMax());
                if (MipavUtil.testParameter(threshFillBF.getText(), panelParent.getImageB().getMin(), panelParent
                        .getImageB().getMax())) { //
                    // componentHistogramA.updateDualThreshold(new
                    //
                    // Float(threshFillF.getText()).floatValue(),
                    // -1);
                } else {
                    threshFillBF.requestFocus();
                    threshFillBF.selectAll();
                }
            }
        }
    }

    /**
     * Removes the tabbed pane for the histogram of image B.
     */
    public void removeHistoLUTb() {

        if (tabbedPane.getTabCount() == 2) {
            tabbedPane.removeTabAt(1);
            panelParent.setImageB(null);
            panelB = null;
            tabbedPane.validate();
            validate();
        }
    }

    /**
     * Placeholder.
     */
    public void setAllOff() {}

    /**
     * end ChangeListener.
     * 
     * @param newLUT DOCUMENT ME!
     */

    /**
     * HistoLUTParent.
     * 
     * @param newLUT DOCUMENT ME!
     */

    /**
     * {@inheritDoc}
     */
    public void setLUT(ModelLUT newLUT) {

        if (isImageASelected()) {
            panelParent.setLUTA(newLUT);
            // oneBasedLUTCheckBoxImageA.setSelected(false);
        } else {
            panelParent.setLUTB(newLUT);
            oneBasedLUTCheckBoxImageB.setSelected(false);
        }
    }

    /**
     * Change the histogram component LUT.
     * 
     * @param lut the new lut
     */
    public final void setLUTa(ModelLUT lut) {
        ((ViewJComponentHistoLUT) getHistoLUTComponentA()).setLUT(lut);
        // oneBasedLUTCheckBoxImageA.setSelected(false);
    }

    /**
     * Change the histogram component LUT.
     * 
     * @param lut the new lut
     */
    public final void setLUTb(ModelLUT lut) {
        ((ViewJComponentHistoLUT) getHistoLUTComponentB()).setLUT(lut);
        oneBasedLUTCheckBoxImageB.setSelected(false);
    }

    /**
     * Change the text field showing the number of colors.
     * 
     * @param value the number of colors
     */
    public void setNColors(int value) {
        nColorsATextF.setText(String.valueOf(value));
    }

    /**
     * {@inheritDoc}
     */
    public void setRangeText(float x, float y, int _index) {

        if (panelParent.doCalcThresholdVolume()) {
            calculateThreshold();
        }

        String start, mid, end;

        if (isImageASelected()) {
            String str = String.valueOf(x);

            cursorIndex = _index;
            rangeX = x;

            int index = str.indexOf(".");
            int length = str.length();
            int indexE = str.indexOf("E");

            if ( ( (index + 2) < length) && (indexE == -1)) {
                str = str.substring(0, index + 2 + 1);
            } else if (indexE != -1) {
                str = str.substring(0, index + 2 + 1) + str.substring(indexE);
            }

            if (labelsTable == null) {
                rangeText.setText(str);
                mouseSlider.setValue(255 - (int) y);
            } else {
                xRangeTextA.setText(str);
                yRangeTextA.setText(String.valueOf(255 - (int) y));

                // Change slider's labels
                start = MipavUtil.makeFloatString(x - scaleRangeA, 2);
                mid = MipavUtil.makeFloatString(x, 2);
                end = MipavUtil.makeFloatString(x + scaleRangeA, 2);
                mouseSliderLabels[0] = ViewJFrameHistoLUT.createSliderLabel(start);
                mouseSliderLabels[1] = ViewJFrameHistoLUT.createSliderLabel(mid);
                mouseSliderLabels[2] = ViewJFrameHistoLUT.createSliderLabel(end);
                labelsTable = new Hashtable<Integer, JLabel>();
                labelsTable.put(0 + (start.length() / 2), mouseSliderLabels[0]);
                labelsTable.put(50 + (mid.length() / 2), mouseSliderLabels[1]);
                labelsTable.put(100 - (mid.length() / 2), mouseSliderLabels[2]);
                mouseSlider.setLabelTable(labelsTable);
                mouseSlider.repaint();
                mouseSlider.setValue(50);
            }
        } else if (isImageBSelected()) {
            String str = String.valueOf(x);

            cursorIndexB = _index;
            rangeXB = x;

            int index = str.indexOf(".");
            int length = str.length();
            int indexE = str.indexOf("E");

            if ( ( (index + 2) < length) && (indexE == -1)) {
                str = str.substring(0, index + 2 + 1);
            } else if (indexE != -1) {
                str = str.substring(0, index + 2 + 1) + str.substring(indexE);
            }

            if (labelsTableB == null) {
                rangeTextB.setText(str);
                mouseSliderB.setValue(255 - (int) y);
            } else {
                xRangeTextB.setText(str);
                yRangeTextB.setText(String.valueOf(255 - (int) y));

                // Change slider's labels
                start = MipavUtil.makeFloatString(x - scaleRangeB, 2);
                mid = MipavUtil.makeFloatString(x, 2);
                end = MipavUtil.makeFloatString(x + scaleRangeB, 2);
                mouseSliderLabelsB[0] = ViewJFrameHistoLUT.createSliderLabel(start);
                mouseSliderLabelsB[1] = ViewJFrameHistoLUT.createSliderLabel(mid);
                mouseSliderLabelsB[2] = ViewJFrameHistoLUT.createSliderLabel(end);
                labelsTableB = new Hashtable<Integer, JLabel>();
                labelsTableB.put(0 + (start.length() / 2), mouseSliderLabelsB[0]);
                labelsTableB.put(50 + (mid.length() / 2), mouseSliderLabelsB[1]);
                labelsTableB.put(100 - (mid.length() / 2), mouseSliderLabelsB[2]);
                mouseSliderB.setLabelTable(labelsTableB);
                mouseSliderB.repaint();
                mouseSliderB.setValue(50);
            }
        }
    }

    /**
     * end ItemListener.
     * 
     * @param e DOCUMENT ME!
     */

    /**
     * ChangeListener.
     * 
     * @param e DOCUMENT ME!
     */

    /**
     * Sets values based on knob along slider.
     * 
     * @param e event that triggered this function
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();
        float value, sliderValue;

        // Slider has changed lets update.
        if ( (source == tabbedPane) && isImageASelected()) {
            panelParent.setDisplayMode(ViewJFrameBase.IMAGE_A);
            panelParent.setLUTA(panelParent.getLUTa());
            panelParent.setTitle("Lookup Table: " + panelParent.getImageA().getImageName());

            if (panelParent.doCalcThresholdVolume()) {
                calculateThreshold();
            }
        } else if ( (source == tabbedPane) && isImageBSelected() && (panelParent.getImageB() != null)) {
            panelParent.setDisplayMode(ViewJFrameBase.IMAGE_B);
            panelParent.setLUTB(panelParent.getLUTb());
            panelParent.setTitle("Lookup Table: " + panelParent.getImageB().getImageName());

            if (panelParent.doCalcThresholdVolume()) {
                calculateThreshold();
            }
        }

        if (source == mouseSlider) {

            // componentOpacityA.updateCursor(rangeX, 100-mouseSlider.getValue(), cursorIndex);
            if (mouseSlider.getValueIsAdjusting() == true) {
                return;
            }

            if (xRangeTextA != null) {
                sliderValue = mouseSlider.getValue();

                if (mouseSliderLabels[0].getText().equals("0")) {
                    value = rangeX + ( (sliderValue) / 100.0f * scaleRangeA * 2.0f);
                } else {

                    if (sliderValue > 50) {
                        value = rangeX + ( (sliderValue - 50) / 100.0f * scaleRangeA * 2.0f);
                    } else {
                        value = rangeX - ( (50 - sliderValue) / 100.0f * scaleRangeA * 2.0f);
                    }
                }

                // value = (mouseSlider.getValue() / 100.0f * 32.0f) + rangeX;
                xRangeTextA.setText(MipavUtil.makeFloatString(value, 2));
                ((ViewJComponentHistoLUT) getHistoLUTComponentA()).updateCursorXPos(value,
                        100 - mouseSlider.getValue(), cursorIndex);
            } else {
                ((ViewJComponentHistoLUT) getHistoLUTComponentA()).updateCursor(rangeX, 100 - mouseSlider.getValue(),
                        cursorIndex);
            }
        } else if (source == mouseSliderB) {

            if (mouseSliderB.getValueIsAdjusting() == true) {
                return;
            }

            if (xRangeTextB != null) {
                sliderValue = mouseSliderB.getValue();

                if (mouseSliderLabelsB[0].getText().equals("0")) {
                    value = rangeXB + (sliderValue / 100.0f * scaleRangeB * 2.0f);
                } else {

                    if (sliderValue > 50) {
                        value = rangeXB + ( (sliderValue - 50) / 100.0f * scaleRangeB * 2.0f);
                    } else {
                        value = rangeXB - ( (50 - sliderValue) / 100.0f * scaleRangeB * 2.0f);
                    }
                }

                // value = (mouseSliderB.getValue() / 100.0f * 32.0f) + rangeXB;
                xRangeTextB.setText(MipavUtil.makeFloatString(value, 2));
                ((ViewJComponentHistoLUT) getHistoLUTComponentB()).updateCursorXPos(value, 100 - mouseSliderB
                        .getValue(), cursorIndexB);
            } else {
                ((ViewJComponentHistoLUT) getHistoLUTComponentB()).updateCursor(rangeXB, 100 - mouseSliderB.getValue(),
                        cursorIndexB);
            }
        }
    }

    /**
     * Placeholder.
     */
    public void updateComponentLUT() {}

    /**
     * {@inheritDoc}
     */
    public void updateFrames(boolean flag) {
        panelParent.updateFrames(flag);
    }

    /**
     * This method is called to update the histogram(s) displayed in each tabbed pane of the frame.
     * 
     * @param _imageA image A
     * @param _LUTa lookup table for image A
     * @param _imageB image B
     * @param _LUTb lookup table for image B
     * @param progressFlag passed to calculateHistogram algorithm. If false progress bar is not displayed
     */
    public void updateHistoLUT(ModelImage _imageA, ModelLUT _LUTa, ModelImage _imageB, ModelLUT _LUTb,
            boolean progressFlag) {

        if (_imageA != null) {
            panelParent.setImageA(_imageA);

            if (_LUTa != null) {
                panelParent.setLUTa(_LUTa);

                if (isImageASelected()) {
                    getLUTComponentA().show(panelParent.getLUTa());
                }
            }

            calcHistogram(ViewJFrameBase.IMAGE_A, entireFlag, progressFlag);
            setLUTa(panelParent.getLUTa());
            getHistoLUTComponentA().setHistogramInfo(panelParent.getImageA(), histogramA);

            if (isImageASelected()) {
                getHistoLUTComponentA().linearMode();
                getHistoLUTComponentA().showHistogram(_LUTa);
            }
        }

        if ( (_imageB != null) && (panelParent.getImageB() != null)) {

            panelParent.setImageB(_imageB);

            if (_LUTb != null) {
                panelParent.setLUTb(_LUTb);
            }

            if (panelB == null) {
                buildPanelB(panelParent.getImageB(), panelParent.getLUTb(), true);
            }

            getLUTComponentB().show(panelParent.getLUTb());

            calcHistogram(ViewJFrameBase.IMAGE_B, entireFlag, progressFlag);
            setLUTb(panelParent.getLUTb());
            getHistoLUTComponentB().setHistogramInfo(panelParent.getImageB(), histogramB);

            getHistoLUTComponentB().linearMode();
            getHistoLUTComponentB().showHistogram(panelParent.getLUTb());
        } else if ( (_imageB != null) && (panelParent.getImageB() == null)) {

            if (_LUTb != null) {
                panelParent.setLUTb(_LUTb);
            }

            panelParent.setImageB(_imageB);
            buildPanelB(panelParent.getImageB(), panelParent.getLUTb(), true);
        }

        tabbedPane.validate();
        validate();
    }

    /**
     * Placeholder.
     * 
     * @param str string
     */
    public void updateLUTPositionString(String str) {

        if (isImageASelected()) {
            indexColorATextF.setText(str);
        } else {
            indexColorBTextF.setText(str);
        }
    }

    /**
     * {@inheritDoc}
     */
    public void updateThresholdFields(float lower, float upper) {

        if (isImageASelected()) {
            threshLowerF.setText(Float.toString(lower));
            threshUpperF.setText(Float.toString(upper));

            if (panelParent.isThresholding()) {

                if (panelParent.doCalcThresholdVolume()) {
                    calculateThreshold(lower, upper);
                }
            }
        } else {
            threshLowerBF.setText(Float.toString(lower));
            threshUpperBF.setText(Float.toString(upper));

            if (panelParent.isThresholding()) {

                if (panelParent.doCalcThresholdVolume()) {
                    calculateThreshold(lower, upper);
                }
            }

        }
    }

    /**
     * Method that displays the histogram and LUT and other controls to manipulate the LUT. Panel for image A.
     * 
     * @param image Model of image
     * @param LUT Model of LUT
     * @param entireFlag Flag indicating if histogram should be made of entire image.
     */
    private void buildPanelA(ModelImage image, ModelLUT LUT, boolean entireFlag) {
        calcHistogram(ViewJFrameBase.IMAGE_A, entireFlag, true);

        JPanel controlPanel = new JPanel(new GridBagLayout());

        controlPanel.setBorder(new EtchedBorder());

        updateCheckBoxA = new JCheckBox("Update (real-time)", true);
        updateCheckBoxA.setFont(MipavUtil.font12);
        updateCheckBoxA.addItemListener(this);

        logCheckBoxA = new JCheckBox("Log scale (Histogram)", true);
        logCheckBoxA.setFont(MipavUtil.font12);
        logCheckBoxA.addItemListener(this);

        interpCheckBoxA = new JCheckBox("Interpolate image display", Preferences.isInterpolateDisplay());
        interpCheckBoxA.setFont(MipavUtil.font12);
        interpCheckBoxA.addItemListener(this);

        int interpMode = 0;

        if (Preferences.isInterpolateDisplay()) {

            if (panelParent.getImageB() != null) {

                interpMode = ViewJComponentBase.INTERPOLATE_BOTH;
                
            } else {
                interpMode = ViewJComponentBase.INTERPOLATE_A;
            }
        } else {
            
                interpMode = ViewJComponentBase.NEAREST_BOTH;
        }

        panelParent.getImageA().notifyImageDisplayListeners(panelParent.getLUTa(), true, -50, interpMode);

        String[] outputChoices = new String[] {panelParent.getImageA().getTypeString(), "Binary", "Short mask"};

        outputBox = new JComboBox(outputChoices);
        outputBox.setFont(MipavUtil.font12);
        outputBox.addItemListener(this);
        outputBox.setEnabled(false);

        JLabel nColorsLabel = new JLabel("Number of colors: ");

        nColorsLabel.setFont(MipavUtil.font12);
        nColorsLabel.setForeground(Color.black);

        nColorsATextF = new JTextField("256");
        nColorsATextF.setFont(MipavUtil.font12);
        nColorsATextF.setEditable(false);

        JLabel indexColorLabel = new JLabel("LUT:");

        indexColorLabel.setFont(MipavUtil.font12);
        indexColorLabel.setForeground(Color.black);

        indexColorATextF = new JTextField(13);
        indexColorATextF.setFont(MipavUtil.font12);
        indexColorATextF.setEditable(false);

        threshLowerF = new JTextField(5);
        MipavUtil.makeNumericsOnly(threshLowerF, true);
        threshLowerF.setFont(MipavUtil.font12);
        threshLowerF.setEditable(false);
        threshLowerF.addKeyListener(this);

        threshUpperF = new JTextField(5);
        MipavUtil.makeNumericsOnly(threshUpperF, true);
        threshUpperF.setFont(MipavUtil.font12);
        threshUpperF.setEditable(false);
        threshUpperF.addKeyListener(this);

        threshFillF = new JTextField(5);
        MipavUtil.makeNumericsOnly(threshFillF, true);
        threshFillF.setFont(MipavUtil.font12);
        threshFillF.setEditable(false);
        threshFillF.addKeyListener(this);

        JLabel threshLabel = new JLabel("Lower threshold:");

        threshLabel.setFont(MipavUtil.font12);
        threshLabel.setForeground(Color.black);

        JLabel threshLabel2 = new JLabel("Upper threshold:");

        threshLabel2.setFont(MipavUtil.font12);
        threshLabel2.setForeground(Color.black);

        JLabel threshFillLabel = new JLabel("Fill value (non-red):");

        threshFillLabel.setFont(MipavUtil.font12);
        threshFillLabel.setForeground(Color.black);

        // oneBasedLUTCheckBoxImageA = new JCheckBox("0 to 1 LUT adjustment", false);
        // oneBasedLUTCheckBoxImageA.setFont(MipavUtil.font12);
        // oneBasedLUTCheckBoxImageA.setToolTipText("Only relevant when the LUT's first index is either the color (0, 0,
        // 0) or (1, 1, 1)");
        // oneBasedLUTCheckBoxImageA.setSelected(isLUT1Based(LUT));
        // oneBasedLUTCheckBoxImageA.addItemListener(this);

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.weightx = 1;
        gbc.weighty = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(0, 5, 0, -5);
        controlPanel.add(updateCheckBoxA, gbc);

        gbc.gridx = 1;
        gbc.ipadx = -5;
        gbc.insets = new Insets(0, 10, 0, -20);
        controlPanel.add(nColorsLabel, gbc);
        gbc.gridx = 2;
        gbc.insets = new Insets(0, 0, 0, 5);
        controlPanel.add(nColorsATextF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.insets = new Insets(0, 5, 0, -5);
        controlPanel.add(logCheckBoxA, gbc);

        gbc.gridx = 1;
        gbc.insets = new Insets(0, 10, 0, -20);
        controlPanel.add(indexColorLabel, gbc);
        gbc.gridx = 2;
        gbc.insets = new Insets(0, 0, 0, 5);
        controlPanel.add(indexColorATextF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.insets = new Insets(0, 5, 0, -5);
        controlPanel.add(interpCheckBoxA, gbc);

        // new textfields for thresholding
        gbc.gridx = 1;
        gbc.insets = new Insets(0, 10, 0, -20);
        controlPanel.add(threshLabel2, gbc);

        // gbc.gridx = 0;
        // gbc.gridy = 3;
        // gbc.insets = new Insets(0, 5, 0, -5);
        // controlPanel.add(oneBasedLUTCheckBoxImageA, gbc);

        gbc.gridx = 2;
        gbc.gridy = 2;
        gbc.insets = new Insets(0, 0, 0, 5);
        controlPanel.add(threshUpperF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.insets = new Insets(0, 5, 0, -5);
        controlPanel.add(outputBox, gbc);

        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.insets = new Insets(0, 10, 0, -20);

        controlPanel.add(threshLabel, gbc);

        gbc.gridx = 2;
        gbc.insets = new Insets(0, 0, 0, 5);
        controlPanel.add(threshLowerF, gbc);

        gbc.gridy = 4;
        gbc.gridx = 1;
        gbc.insets = new Insets(0, 10, 0, -20);

        controlPanel.add(threshFillLabel, gbc);

        gbc.gridx = 2;
        gbc.insets = new Insets(0, 0, 0, 5);
        controlPanel.add(threshFillF, gbc);

        gbc.gridy = 5;

        histoPanelA = new ViewJPanelHistoLUT(this, image, LUT, histogramA);

        panelA = new JPanel(new BorderLayout());
        panelA.add(controlPanel, BorderLayout.NORTH);
        panelA.add(histoPanelA, BorderLayout.CENTER);
        tabbedPane.addTab("ImageA", null, panelA);
        tabbedPane.setFont(MipavUtil.font12B);

        mouseLabel = new JLabel("    X Scale");
        mouseLabel.setFont(MipavUtil.font12B);
        mouseLabel.setForeground(Color.black);

        scaleRangeA = ((int) Math.round(image.getMax() - image.getMin()) + 1) / 256;

        mouseSliderLabels = new JLabel[3];
        mouseSliderLabels[0] = ViewJFrameHistoLUT.createSliderLabel(String.valueOf(0));
        mouseSliderLabels[1] = ViewJFrameHistoLUT.createSliderLabel(String.valueOf(scaleRangeA));
        mouseSliderLabels[2] = ViewJFrameHistoLUT.createSliderLabel(String.valueOf(scaleRangeA * 2));

        labelsTable = new Hashtable<Integer, JLabel>();

        labelsTable.put(3, mouseSliderLabels[0]);
        labelsTable.put(50, mouseSliderLabels[1]);
        labelsTable.put(100, mouseSliderLabels[2]);

        mouseSlider = new JSlider(0, 100, 50);
        mouseSlider.setFont(MipavUtil.font12);
        mouseSlider.setMinorTickSpacing(10);
        mouseSlider.setPaintTicks(true);
        mouseSlider.addChangeListener(this);
        mouseSlider.setLabelTable(labelsTable);
        mouseSlider.setPaintLabels(true);
        mouseSlider.setAlignmentX(Component.LEFT_ALIGNMENT);
        mouseLabel.setAlignmentX(Component.LEFT_ALIGNMENT);
        mouseSlider.setEnabled(true);
        mouseLabel.setEnabled(true);
        mouseSliderLabels[0].setEnabled(true);
        mouseSliderLabels[1].setEnabled(true);
        mouseSliderLabels[2].setEnabled(true);

        JLabel textXRange = new JLabel("X Range");

        textXRange.setFont(MipavUtil.font12B);

        xRangeTextA = new JTextField(String.valueOf(0), 5);
        xRangeTextA.setFont(MipavUtil.font12);
        xRangeTextA.setEnabled(false);

        JLabel textYRange = new JLabel("Y Range");

        textYRange.setFont(MipavUtil.font12B);

        yRangeTextA = new JTextField(String.valueOf(0), 5);
        yRangeTextA.setFont(MipavUtil.font12);
        yRangeTextA.setEnabled(false);

        GridBagLayout cpGBL = new GridBagLayout();

        gbc = new GridBagConstraints();

        gbc.fill = GridBagConstraints.NONE;
        gbc.weightx = 35;
        gbc.weighty = 35;

        JPanel panelMouse = new JPanel();

        panelMouse.setLayout(cpGBL);
        gbc.fill = GridBagConstraints.WEST;
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        panelMouse.add(textXRange, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        panelMouse.add(xRangeTextA, gbc);

        gbc.gridx = 2;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        panelMouse.add(textYRange, gbc);
        gbc.gridx = 3;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        panelMouse.add(yRangeTextA, gbc);

        gbc.weightx = 70;
        gbc.weighty = 70;

        gbc.fill = GridBagConstraints.BOTH;
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        panelMouse.add(mouseLabel, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.gridwidth = 4;
        gbc.gridheight = 1;
        panelMouse.add(mouseSlider, gbc);
        panelA.add(panelMouse, BorderLayout.SOUTH);
    }

    /**
     * Method that displays the histogram and LUT and other controls to manipulate the LUT. Panel for image B.
     * 
     * @param image Model of image
     * @param LUT Model of LUT
     * @param entireFlag Flag indicating if histogram should be made of entire image.
     */
    private void buildPanelB(ModelImage image, ModelLUT LUT, boolean entireFlag) {

        // go calc histo
        calcHistogram(ViewJFrameBase.IMAGE_B, entireFlag, true);

        JPanel controlPanelB = new JPanel(new GridBagLayout());

        controlPanelB.setBorder(new EtchedBorder());

        updateCheckBoxB = new JCheckBox("Update (real-time)", true);
        updateCheckBoxB.setFont(MipavUtil.font12);
        updateCheckBoxB.addItemListener(this);

        logCheckBoxB = new JCheckBox("Log scale (Histogram)", true);
        logCheckBoxB.setFont(MipavUtil.font12);
        logCheckBoxB.addItemListener(this);

        interpCheckBoxB = new JCheckBox("Interpolate image", Preferences.isInterpolateDisplay());
        interpCheckBoxB.setFont(MipavUtil.font12);
        interpCheckBoxB.addItemListener(this);
        
        int interpMode = 0;

        if (Preferences.isInterpolateDisplay()) {

            if (panelParent.getImageA() != null) {
                interpMode = ViewJComponentBase.INTERPOLATE_BOTH;
            } else {
                interpMode = ViewJComponentBase.INTERPOLATE_B;
            }
        } else {
                interpMode = ViewJComponentBase.NEAREST_BOTH;
        }

        panelParent.getImageB().notifyImageDisplayListeners(panelParent.getLUTb(), true, -50, interpMode);

        String[] outputChoices = new String[] {panelParent.getImageA().getTypeString(), "Binary", "Short mask"};
        outputBoxB = new JComboBox(outputChoices);
        outputBoxB.setFont(MipavUtil.font12);
        outputBoxB.addItemListener(this);
        outputBoxB.setEnabled(false);

        oneBasedLUTCheckBoxImageB = new JCheckBox("0 to 1 LUT adjustment", false);
        oneBasedLUTCheckBoxImageB.setFont(MipavUtil.font12);
        oneBasedLUTCheckBoxImageB
                .setToolTipText("Only relevant when the LUT's first index is either the color (0, 0, 0) or (1, 1, 1)");
        oneBasedLUTCheckBoxImageB.setSelected(isLUT1Based(LUT));
        oneBasedLUTCheckBoxImageB.addItemListener(this);

        JLabel nColorsLabel = new JLabel("Number of colors: ");

        nColorsLabel.setFont(MipavUtil.font12);
        nColorsLabel.setForeground(Color.black);

        nColorsBTextF = new JTextField("256");
        nColorsBTextF.setFont(MipavUtil.font12);
        nColorsBTextF.setEditable(false);

        JLabel indexColorLabel = new JLabel("LUT:");

        indexColorLabel.setFont(MipavUtil.font12);
        indexColorLabel.setForeground(Color.black);

        indexColorBTextF = new JTextField(13);
        indexColorBTextF.setFont(MipavUtil.font12);
        indexColorBTextF.setEditable(false);

        threshLowerBF = new JTextField(5);
        MipavUtil.makeNumericsOnly(threshLowerBF, true);
        threshLowerBF.setFont(MipavUtil.font12);
        threshLowerBF.setEditable(false);
        threshLowerBF.addKeyListener(this);

        threshUpperBF = new JTextField(5);
        MipavUtil.makeNumericsOnly(threshUpperBF, true);
        threshUpperBF.setFont(MipavUtil.font12);
        threshUpperBF.setEditable(false);
        threshUpperBF.addKeyListener(this);

        threshFillBF = new JTextField(5);
        MipavUtil.makeNumericsOnly(threshFillBF, true);
        threshFillBF.setFont(MipavUtil.font12);
        threshFillBF.setEditable(false);
        threshFillBF.addKeyListener(this);

        JLabel threshLabel = new JLabel("Lower threshold:");

        threshLabel.setFont(MipavUtil.font12);
        threshLabel.setForeground(Color.black);

        JLabel threshLabel2 = new JLabel("Upper threshold:");

        threshLabel2.setFont(MipavUtil.font12);
        threshLabel2.setForeground(Color.black);

        JLabel threshFillLabel = new JLabel("Outer value (non-red):");

        threshFillLabel.setFont(MipavUtil.font12);
        threshFillLabel.setForeground(Color.black);

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.weightx = 1;
        gbc.weighty = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        gbc.insets = new Insets(0, 5, 0, -5);
        controlPanelB.add(updateCheckBoxB, gbc);

        gbc.gridx = 1;
        gbc.ipadx = -5;
        gbc.insets = new Insets(0, 10, 0, -20);
        controlPanelB.add(nColorsLabel, gbc);
        gbc.gridx = 2;
        gbc.insets = new Insets(0, 0, 0, 5);
        controlPanelB.add(nColorsBTextF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.insets = new Insets(0, 5, 0, -5);
        controlPanelB.add(logCheckBoxB, gbc);

        gbc.gridx = 1;
        gbc.insets = new Insets(0, 10, 0, -20);
        controlPanelB.add(indexColorLabel, gbc);
        gbc.gridx = 2;
        gbc.insets = new Insets(0, 0, 0, 5);
        controlPanelB.add(indexColorBTextF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.insets = new Insets(0, 5, 0, -5);
        controlPanelB.add(interpCheckBoxB, gbc);

        // new textfields for thresholding
        gbc.gridx = 1;
        gbc.insets = new Insets(0, 10, 0, -20);
        controlPanelB.add(threshLabel2, gbc);

        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.insets = new Insets(0, 5, 0, -5);
        controlPanelB.add(oneBasedLUTCheckBoxImageB, gbc);

        gbc.gridx = 2;
        gbc.gridy = 2;
        gbc.insets = new Insets(0, 0, 0, 5);
        controlPanelB.add(threshUpperBF, gbc);

        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.insets = new Insets(0, 5, 0, -5);
        controlPanelB.add(outputBoxB, gbc);

        gbc.gridx = 1;
        gbc.gridy = 3;
        gbc.insets = new Insets(0, 10, 0, -20);
        controlPanelB.add(threshLabel, gbc);

        gbc.gridx = 2;
        gbc.insets = new Insets(0, 0, 0, 5);
        controlPanelB.add(threshLowerBF, gbc);

        gbc.gridy = 4;
        gbc.gridx = 1;
        gbc.insets = new Insets(0, 10, 0, -20);

        controlPanelB.add(threshFillLabel, gbc);

        gbc.gridx = 2;
        gbc.insets = new Insets(0, 0, 0, 5);
        controlPanelB.add(threshFillBF, gbc);

        gbc.gridy = 5;

        histoPanelB = new ViewJPanelHistoLUT(this, image, LUT, histogramB);

        panelB = new JPanel(new BorderLayout());
        panelB.add(controlPanelB, BorderLayout.NORTH);
        panelB.add(histoPanelB, BorderLayout.CENTER);
        tabbedPane.addTab("ImageB", null, panelB);
        tabbedPane.setFont(MipavUtil.font12B);

        mouseLabelB = new JLabel("    X Scale");
        mouseLabelB.setFont(MipavUtil.font12B);
        mouseLabelB.setForeground(Color.black);

        scaleRangeB = ((int) Math.round(image.getMax() - image.getMin()) + 1) / 256;

        mouseSliderLabelsB = new JLabel[3];
        mouseSliderLabelsB[0] = ViewJFrameHistoLUT.createSliderLabel(String.valueOf(0));
        mouseSliderLabelsB[1] = ViewJFrameHistoLUT.createSliderLabel(String.valueOf(scaleRangeB));
        mouseSliderLabelsB[2] = ViewJFrameHistoLUT.createSliderLabel(String.valueOf(scaleRangeB * 2));

        labelsTableB = new Hashtable<Integer, JLabel>();

        labelsTableB.put(0, mouseSliderLabelsB[0]);
        labelsTableB.put(50, mouseSliderLabelsB[1]);
        labelsTableB.put(100, mouseSliderLabelsB[2]);

        mouseSliderB = new JSlider(0, 100, 50);
        mouseSliderB.setFont(MipavUtil.font12);
        mouseSliderB.setMinorTickSpacing(10);
        mouseSliderB.setPaintTicks(true);
        mouseSliderB.addChangeListener(this);
        mouseSliderB.setLabelTable(labelsTableB);
        mouseSliderB.setPaintLabels(true);
        mouseSliderB.setAlignmentX(Component.LEFT_ALIGNMENT);
        mouseLabelB.setAlignmentX(Component.LEFT_ALIGNMENT);
        mouseSliderB.setEnabled(true);
        mouseLabelB.setEnabled(true);
        mouseSliderLabelsB[0].setEnabled(true);
        mouseSliderLabelsB[1].setEnabled(true);
        mouseSliderLabelsB[2].setEnabled(true);

        JLabel textXRange = new JLabel("X Range");

        textXRange.setFont(MipavUtil.font12B);

        xRangeTextB = new JTextField(String.valueOf(0), 5);
        xRangeTextB.setFont(MipavUtil.font12);
        xRangeTextB.setEnabled(false);

        JLabel textYRange = new JLabel("Y Range");

        textYRange.setFont(MipavUtil.font12B);

        yRangeTextB = new JTextField(String.valueOf(0), 5);
        yRangeTextB.setFont(MipavUtil.font12);
        yRangeTextB.setEnabled(false);

        GridBagLayout cpGBL = new GridBagLayout();

        gbc = new GridBagConstraints();

        gbc.fill = GridBagConstraints.NONE;

        gbc.weightx = 35;
        gbc.weighty = 35;

        JPanel panelMouse = new JPanel();

        panelMouse.setLayout(cpGBL);
        gbc.fill = GridBagConstraints.CENTER;
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        panelMouse.add(textXRange, gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        panelMouse.add(xRangeTextB, gbc);

        gbc.gridx = 2;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;

        panelMouse.add(textYRange, gbc);
        gbc.gridx = 3;
        gbc.gridy = 2;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        panelMouse.add(yRangeTextB, gbc);

        gbc.weightx = 70;
        gbc.weighty = 70;

        gbc.fill = GridBagConstraints.BOTH;
        gbc.gridx = 0;
        gbc.gridy = 3;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        panelMouse.add(mouseLabelB, gbc);
        gbc.gridx = 0;
        gbc.gridy = 4;
        gbc.gridwidth = 4;
        gbc.gridheight = 1;
        panelMouse.add(mouseSliderB, gbc);
        panelB.add(panelMouse, BorderLayout.SOUTH);
    }

    /**
     * Calculates histogram for the image(s).
     * 
     * @param imageAorB flag to indicate if histogram is to be calculated for imageA or imageB.
     * @param entireFlag if true calculate histogram for the entire image. if false uses areas defined by VOI regions.
     * @param progressFlag passed to calculateHistogram algorithm. If false progress bar is not displayed
     */
    private void calcHistogram(int imageAorB, boolean entireFlag, boolean progressFlag) {

        int[] dimExtentsA = new int[1];
        int[] dimExtentsB = new int[1];

        if ( (panelParent.getImageA() != null) && (imageAorB == ViewJFrameBase.IMAGE_A)) {

            dimExtentsA[0] = 256;
            histogramA = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsA);

            AlgorithmHistogram histoAlgoA = new AlgorithmHistogram(histogramA, panelParent.getImageA(), entireFlag);

            // histoAlgoA.setProgressBarVisible(progressFlag);
            histoAlgoA.setRunningInSeparateThread(false);
            histoAlgoA.run();
        }

        if ( (panelParent.getImageB() != null) && (imageAorB == ViewJFrameBase.IMAGE_B)) {
            dimExtentsB[0] = 256;
            histogramB = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsB);

            AlgorithmHistogram histoAlgoB = new AlgorithmHistogram(histogramB, panelParent.getImageB(), entireFlag);

            // histoAlgoB.setProgressBarVisible(progressFlag);
            histoAlgoB.setRunningInSeparateThread(false);
            histoAlgoB.run();
        }
    }

    /**
     * Calculates the thresholded image based on the parameters of the threshold transfer function. Image A is
     * thresholded if the selected panel is for imageA and likewise for image B.
     */
    private void calcThreshold() {
        float[] thresholds = new float[2];

        if (isImageASelected()) {
            thresholds[0] = ((Vector2f) (getLUTa().getTransferFunction().getPoint(1))).X;
            thresholds[1] = ((Vector2f) (getLUTa().getTransferFunction().getPoint(4))).X;

            JDialogThresholdLUT dialogLUT = new JDialogThresholdLUT(panelParent, panelParent.getImageA(),
                    thresholds[0], thresholds[1]);

            if ( (dialogLUT.cancelFlag == false) && (panelParent.getImageA().getType() != ModelStorageBase.BOOLEAN)) {
                updateHistoLUT(panelParent.getImageA(), panelParent.getLUTa(), null, null, false);
                getHistoLUTComponentA().dualThresholdMode(ViewJComponentHLUTBase.DUAL_THRESHOLD);
            } else if (panelParent.getImageA().getType() == ModelStorageBase.BOOLEAN) {
                panelParent.getLUTa().makeGrayTransferFunctions();
                panelParent.getLUTa().makeLUT(256);
                panelParent.setLUTA(panelParent.getLUTa());
                // oneBasedLUTCheckBoxImageA.setSelected(false);
            }
        } else {
            thresholds[0] = ((Vector2f) (getLUTb().getTransferFunction().getPoint(1))).X;
            thresholds[1] = ((Vector2f) (getLUTb().getTransferFunction().getPoint(4))).X;

            JDialogThresholdLUT dialogLUT = new JDialogThresholdLUT(panelParent, panelParent.getImageB(),
                    thresholds[0], thresholds[1]);

            if ( (dialogLUT.cancelFlag == false) && (panelParent.getImageB().getType() != ModelStorageBase.BOOLEAN)) {
                updateHistoLUT(null, null, panelParent.getImageB(), panelParent.getLUTb(), false);
                getHistoLUTComponentB().dualThresholdMode(ViewJComponentHLUTBase.DUAL_THRESHOLD);
            } else if (panelParent.getImageB().getType() == ModelStorageBase.BOOLEAN) {
                panelParent.getLUTb().makeGrayTransferFunctions();
                panelParent.getLUTb().makeLUT(256);
                panelParent.setLUTB(panelParent.getLUTb());
                oneBasedLUTCheckBoxImageB.setSelected(false);
            }
        }
    }

    /**
     * Calculates the volume (for 3D images) or area (for 2D images) of the image between the two values from the upper
     * and lower bounds text areas.
     */
    private void calculateThreshold() {
        float upper, lower;

        if (panelParent.isThresholding()) {

            try {

                if (isImageASelected()) {
                    lower = Float.parseFloat(threshLowerF.getText());
                    upper = Float.parseFloat(threshUpperF.getText());
                } else // image B is selected
                {
                    lower = Float.parseFloat(threshLowerBF.getText());
                    upper = Float.parseFloat(threshUpperBF.getText());
                }
            } catch (Exception e) {
                return;
            }

            if (isImageASelected()) {

                if (panelParent.getImageA().getNDims() == 3) {
                    calculateThresholdVolume(lower, upper);
                } else {
                    calculateThresholdArea(lower, upper);
                }
            } else // image B is selected
            {

                if (panelParent.getImageB().getNDims() == 3) {
                    calculateThresholdVolume(lower, upper);
                } else {
                    calculateThresholdArea(lower, upper);
                }
            }
        }
    }

    /**
     * Calculates the volume or area of the image between the two values from the upper and lower bounds (inclusive).
     * 
     * @param lower Lower bound of the threshold (inclusive).
     * @param upper Upper bound of the threshold (inclusive).
     */
    private void calculateThreshold(float lower, float upper) {

        if (panelParent.getImageA().getNDims() == 3) {
            calculateThresholdVolume(lower, upper);
        } else {
            calculateThresholdArea(lower, upper);
        }
    }

    /**
     * Calculates the area of the image between the two values from the upper and lower bounds (inclusive).
     * 
     * @param lower Lower bound of the threshold (inclusive).
     * @param upper Upper bound of the threshold (inclusive).
     */
    private void calculateThresholdArea(float lower, float upper) {
        ModelImage image;

        if (isImageASelected()) {
            image = panelParent.getImageA();
        } else {
            image = panelParent.getImageB();
        }

        int[] imageBuffer = new int[image.getExtents()[0] * image.getExtents()[1]];
        int numPixels = 0;

        try {
            image.exportData(0, imageBuffer.length, imageBuffer);

            for (int j = 0; j < imageBuffer.length; j++) {

                if ( (imageBuffer[j] >= lower) && (imageBuffer[j] <= upper)) {
                    numPixels++;
                }
            }
        } catch (IOException ioe) {
            return;
        }

        float[] res = new float[2];
        res[0] = Math.abs(image.getFileInfo(0).getResolutions()[0]);
        res[1] = Math.abs(image.getFileInfo(0).getResolutions()[1]);

        String units = image.getFileInfo(0).getAreaUnitsOfMeasureStr();

        voxelVolumeLabel.setText("Threshold area(red): " + String.valueOf(numPixels * res[0] * res[1]) + units);
    }

    /**
     * Calculates the volume of the image between the two values from the upper and lower bounds (inclusive).
     * 
     * @param lower Lower bound of the threshold (inclusive).
     * @param upper Upper bound of the threshold (inclusive).
     */
    private void calculateThresholdVolume(float lower, float upper) {
        ModelImage image;

        boolean isInverse = false;

        if (isImageASelected()) {
            image = panelParent.getImageA();

            if (getHistoLUTComponentA().getMode() == ViewJComponentHistoLUT.DUAL_THRESHOLD_INV) {
                isInverse = true;
            }
        } else {
            image = panelParent.getImageB();

            if (getHistoLUTComponentB().getMode() == ViewJComponentHistoLUT.DUAL_THRESHOLD_INV) {
                isInverse = true;
            }

        }

        int[] imageBuffer = new int[image.getExtents()[0] * image.getExtents()[1]];
        int numVoxels = 0;

        for (int i = 0; i < image.getExtents()[2]; i++) {

            try {
                image.exportData(i * image.getExtents()[0] * image.getExtents()[1], imageBuffer.length, imageBuffer);

                for (int j = 0; j < imageBuffer.length; j++) {

                    if ( !isInverse) {

                        if ( (imageBuffer[j] >= lower) && (imageBuffer[j] <= upper)) {
                            numVoxels++;
                        }
                    } else {

                        if ( (imageBuffer[j] <= lower) || (imageBuffer[j] >= upper)) {
                            numVoxels++;
                        }
                    }
                }
            } catch (IOException ioe) {
                return;
            }
        }

        float[] res = new float[3];
        res[0] = Math.abs(image.getFileInfo(0).getResolutions()[0]);
        res[1] = Math.abs(image.getFileInfo(0).getResolutions()[1]);
        res[2] = Math.abs(image.getFileInfo(0).getResolutions()[2]);

        String units = image.getFileInfo(0).getVolumeUnitsOfMeasureStr();

        voxelVolumeLabel.setText("Threshold volume(red): " + String.valueOf(numVoxels * res[0] * res[1] * res[2])
                + units);
    }

    /**
     * end HistoLUTParent.
     * 
     * @param LUT DOCUMENT ME!
     * 
     * @return DOCUMENT ME!
     */

    private boolean isLUT1Based(ModelLUT LUT) {
        Color color = LUT.getColor(0);

        if ( (color.getRed() == 1) && (color.getGreen() == 1) && (color.getGreen() == 1)) {
            return true;
        }

        return false;
    }

    /**
     * Opens and returns a buffered reader for a given custom LUT name.
     * 
     * @param lutName The name of the LUT file (without the extension).
     * 
     * @return A LUT file buffered reader.
     */
    public static final BufferedReader openLUTFile(String lutName) throws IOException {
        String filename = customLUTsLocation + "/" + lutName + ".txt";

        // use this long call instead of ClassLoader.getSystemResource() to work properly from a jnlp launch
        URL fileURL = Thread.currentThread().getContextClassLoader().getResource(filename);

        if (fileURL == null) {
            Preferences.debug("Unable to open " + filename + ".\n", Preferences.DEBUG_MINOR);
            return null;
        }

        // use buffering this implementation reads one line at a time
        return new BufferedReader(new InputStreamReader(fileURL.openStream()));
    }
}
