package gov.nih.mipav.view.renderer;

import WildMagic.LibFoundation.Mathematics.Vector2f;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.Preferences.DefaultDisplay;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;

import java.util.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;


/**
 * This class produces a frame where the user can specify a specific LUT, the number of colors of the LUT or dynamically
 * edit the LUT. The frame with histogram of the image data is displayed using the color map. All frames using the color
 * map are dynamically updated with the new color map. This control panel is the control LUT of the gray scale images.
 *
 * @version  1.0
 * @deprecated
 * @see JFrameHistogram
 */
public class JPanelHistoLUT
        implements ItemListener, ActionListener, ChangeListener, ViewImageUpdateInterface, FocusListener, KeyListener,
                   HistoLUTParent {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Display mode image A. */
    public static final int IMAGE_A = 0;

    /** Display mode image B. */
    public static final int IMAGE_B = 1;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Indicates which image is to be acted upon when two images are displayed. */
    protected int displayMode = IMAGE_A;

    /** Active mouse cursor index of the imageA, B and GM image A, B. */
    private int cursorIndex, cursorIndexB;

    /** Flag indicating if histogram should be done on all of image or not. */
    private boolean entireFlag = true;

    /** Model histogram A. */
    private ModelHistogram histogramA = null;

    /** Model histogram B. */
    private ModelHistogram histogramB = null;

    /** Panel containing the histogram and lut components for image A. */
    private ViewJPanelHistoLUT histoPanelA;

    /** Panel containing the histogram and lut components for image B. */
    private ViewJPanelHistoLUT histoPanelB;

    /** image A reference. */
    private ModelImage imageA;

    /** image B reference. */
    private ModelImage imageB;

    /** Color LUT A, B textfield. */
    private JTextField indexColorATextF, indexColorBTextF;

    /** Histogram dialog slider labels of the imageA, B and GM imageA, B. */
    private Hashtable<Integer, JLabel> labelsTable, labelsTableB;

    /** The histogram log view Check box. */
    private JCheckBox logCheckBoxA, logCheckBoxB;

    /** Model LUT A. */
    private ModelLUT LUTa;

    /** LutA table adjust check box. */
    private JCheckBox lutAdjustCheckboxA;

    /** LutB table adjust check box. */
    private JCheckBox lutAdjustCheckboxB;

    /** Model LUT B. */
    private ModelLUT LUTb;

    /** The main GUI control panel. */
    private JPanel mainPanel;

    /** Not used now. */
    private ViewMenuBuilder menuObj;

    /** The opacity slider label. */
    private JLabel mouseLabel, mouseLabelB;

    /** Opacity X scale sliders. */
    private JSlider mouseSlider, mouseSliderB;

    /** The labels below the opacity slider. */
    private JLabel[] mouseSliderLabels, mouseSliderLabelsB;

    /** Update check box. Updating the histogram and volume during mouse drag. */
    // private JCheckBox updateCheckBoxA = null, updateCheckBoxB = null;
    private JTextField nColorsATextF, nColorsBTextF;

    /** Tabbed panel A. */
    private JPanel panelA;

    /** Tabbed panel B. */
    private JPanel panelB = null;

    /** X range text field in the imageA, B histogram dialog. */
    private JTextField rangeText, rangeTextB;

    /** X range value of the imageA, B and GM imageA, B. */
    private float rangeX, rangeXB;

    /** Scale range value according to the image min and max. */
    private int scaleRangeA, scaleRangeB;

    /** Scroll pane. */
    private JScrollPane scroller;

    /** Scroll panel that holding the all the control components. */
    private DrawingPanel scrollPanel;

    /** The main tabbed control panel. */
    private JTabbedPane tabbedPane;

    /** threshold related. */
    private JTextField threshLowerF, threshUpperF;

    /** Bottom toolbar. */
    private JToolBar toolBarBottom;

    /** Toolbar that hold the linear, horizontal mode control buttons. */
    private ViewToolBarBuilder toolBarObj;

    /** Top toolbar */
    private JToolBar toolBarTop;
    
    /** center toolbar */
    private JToolBar toolBarCenter;

    /** Toolbar panel. */
    private JPanel topPanel;

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
     * Reference to CT dialog.   The CT dialog only works with the componentHistogram. 
     */
    private JDialogCTHistoLUT ctDialogA, ctDialogB;

    /** Update the LUT in real-time: */
    private boolean bImageUpdate = false;
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Makes a frame of the histogram and LUT.
     *
     * @param  _imageA      Model of imageA
     * @param  _imageB      Model of imageB
     * @param  _LUTa        Model of LUT for image A
     * @param  _LUTb        Model of LUT for image B
     * @param  _entireFlag  Flag indicating if histogram should be done on all of image.
     * @deprecated
     */
    public JPanelHistoLUT(ModelImage _imageA, ModelImage _imageB, ModelLUT _LUTa, ModelLUT _LUTb, boolean _entireFlag) {

        imageA = _imageA;
        imageB = _imageB;
        setLUTs(_LUTa, _LUTb);

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        scrollPanel = new DrawingPanel();
        scrollPanel.setLayout(new BorderLayout());

        // Put the drawing area in a scroll pane.
        scroller = new JScrollPane(scrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());

        entireFlag = _entireFlag;

        toolBarObj = new ViewToolBarBuilder(this);

        toolBarTop = toolBarObj.buildLUTToolBarTop();
        toolBarCenter = ViewJPanelLUT.buildLUTSelectionList(this);
        toolBarBottom = toolBarObj.buildLUTToolBarBottom();
        toolBarBottom.getComponentAtIndex(4).setEnabled(false);

        topPanel = new JPanel(new BorderLayout());
        topPanel.add(toolBarTop, BorderLayout.NORTH);
        topPanel.add(toolBarCenter, BorderLayout.CENTER);
        topPanel.add(toolBarBottom, BorderLayout.SOUTH);

        mainPanel.add(topPanel, BorderLayout.NORTH);

        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.font12B);
        buildPanelA(imageA, _LUTa, entireFlag);

        if (imageB != null) {
            buildPanelB(imageB, _LUTb, entireFlag);
        }

        tabbedPane.setSelectedIndex(0);
        tabbedPane.addChangeListener(this);
        scrollPanel.add(tabbedPane, BorderLayout.NORTH);

        mainPanel.add(scroller, BorderLayout.CENTER);
    }

    /**
     * Makes a frame of the histogram and LUT.
     *
     * @param  _imageA      Model of imageA
     * @param  _imageB      Model of imageB
     * @param  _LUTa        Model of LUT for image A
     * @param  _LUTb        Model of LUT for image B
     * @param  _entireFlag  Flag indicating if histogram should be done on all of image.
     * @deprecated
     */
    public JPanelHistoLUT(ModelImage _imageA, ModelImage _imageB, 
            ModelLUT _LUTa, ModelLUT _LUTb, boolean _entireFlag, boolean bUpdateImage) {

        this(_imageA, _imageB, _LUTa, _LUTb, _entireFlag);
        this.bImageUpdate = bUpdateImage;
    }
    
    // ************************************************************************
    // **************************** Action Events *****************************
    // ************************************************************************

    /**
     * Calls various methods depending on the action.
     *
     * @param  event  event that triggered function
     */
    public void actionPerformed(ActionEvent event) {

        String text;
        int nColors = 256;
        String command;

        command = event.getActionCommand();

        
        if (tabbedPane.getSelectedComponent() == panelA) {
            text = nColorsATextF.getText();

            if (testParameter(text, 2, 256)) {
                nColors = Integer.valueOf(text).intValue();
            }
        } else {
            text = nColorsBTextF.getText();

            if (testParameter(text, 2, 256)) {
                nColors = Integer.valueOf(text).intValue();
            }
        }

        if (event.getActionCommand().equals("GrayLUT")) {

            if (tabbedPane.getSelectedComponent() == panelA) {
                LUTa.makeGrayTransferFunctions();
                LUTa.makeLUT(nColors);
                //lutAdjustCheckboxA.setSelected(false);
                setLUTA(LUTa);
                updateFrames(false);
            } else {
                LUTb.makeGrayTransferFunctions();
                LUTb.makeLUT(nColors);
                lutAdjustCheckboxB.setSelected(false);
                setLUTB(LUTb);
                updateFrames(false);
            }
        } else if (event.getActionCommand().equals("redLUT")) {

            if (tabbedPane.getSelectedComponent() == panelA) {
                LUTa.makeRedTransferFunctions();
                LUTa.makeLUT(nColors);
                //lutAdjustCheckboxA.setSelected(false);
                setLUTA(LUTa);
                updateFrames(false);
            } else {
                LUTb.makeRedTransferFunctions();
                LUTb.makeLUT(nColors);
                lutAdjustCheckboxB.setSelected(false);
                setLUTB(LUTb);
                updateFrames(false);
            }
        } else if (event.getActionCommand().equals("greenLUT")) {

            if (tabbedPane.getSelectedComponent() == panelA) {
                LUTa.makeGreenTransferFunctions();
                LUTa.makeLUT(nColors);
                //lutAdjustCheckboxA.setSelected(false);
                setLUTA(LUTa);
                updateFrames(false);
            } else {
                LUTb.makeGreenTransferFunctions();
                LUTb.makeLUT(nColors);
                lutAdjustCheckboxB.setSelected(false);
                setLUTB(LUTb);
                updateFrames(false);
            }
        } else if (event.getActionCommand().equals("blueLUT")) {

            if (tabbedPane.getSelectedComponent() == panelA) {
                LUTa.makeBlueTransferFunctions();
                LUTa.makeLUT(nColors);
                //lutAdjustCheckboxA.setSelected(false);
                setLUTA(LUTa);
                updateFrames(false);
            } else {
                LUTb.makeBlueTransferFunctions();
                LUTb.makeLUT(nColors);
                lutAdjustCheckboxB.setSelected(false);
                setLUTB(LUTb);
                updateFrames(false);
            }
        } else if (event.getActionCommand().equals("graybrLUT")) {

            if (tabbedPane.getSelectedComponent() == panelA) {
                LUTa.makeGrayBRTransferFunctions();
                LUTa.makeLUT(nColors);
                //lutAdjustCheckboxA.setSelected(false);
                setLUTA(LUTa);
                updateFrames(false);
            } else {
                LUTb.makeGrayBRTransferFunctions();
                LUTb.makeLUT(nColors);
                lutAdjustCheckboxB.setSelected(false);
                setLUTB(LUTb);
                updateFrames(false);
            }
        } else if (event.getActionCommand().equals("HotMetalLUT")) {

            if (tabbedPane.getSelectedComponent() == panelA) {
                LUTa.makeHotMetalTransferFunctions();
                LUTa.makeLUT(nColors);
                //lutAdjustCheckboxA.setSelected(false);
                setLUTA(LUTa);
                updateFrames(false);
            } else {
                LUTb.makeHotMetalTransferFunctions();
                LUTb.makeLUT(nColors);
                lutAdjustCheckboxB.setSelected(false);
                setLUTB(LUTb);
                updateFrames(false);
            }
        } else if (event.getActionCommand().equals("spectrumLUT")) {

            if (tabbedPane.getSelectedComponent() == panelA) {
                LUTa.makeSpectrumTransferFunctions();
                LUTa.makeLUT(nColors);
                //lutAdjustCheckboxA.setSelected(false);
                setLUTA(LUTa);
                updateFrames(false);
            } else {
                LUTb.makeSpectrumTransferFunctions();
                LUTb.makeLUT(nColors);
                lutAdjustCheckboxB.setSelected(false);
                setLUTB(LUTb);
                updateFrames(false);
            }
        } else if (event.getActionCommand().equals("coolHotLUT")) {

            if (tabbedPane.getSelectedComponent() == panelA) {
                LUTa.makeCoolHotTransferFunctions();
                LUTa.makeLUT(nColors);
                //lutAdjustCheckboxA.setSelected(false);
                setLUTA(LUTa);
                updateFrames(false);
            } else {
                LUTb.makeCoolHotTransferFunctions();
                LUTb.makeLUT(nColors);
                lutAdjustCheckboxB.setSelected(false);
                setLUTB(LUTb);
                updateFrames(false);
            }
        } else if (event.getActionCommand().equals("skinLUT")) {

            if (tabbedPane.getSelectedComponent() == panelA) {
                LUTa.makeSkinTransferFunctions();
                LUTa.makeLUT(nColors);
                //lutAdjustCheckboxA.setSelected(false);
                setLUTA(LUTa);
                updateFrames(false);
            } else {
                LUTb.makeSkinTransferFunctions();
                LUTb.makeLUT(nColors);
                lutAdjustCheckboxB.setSelected(false);
                setLUTB(LUTb);
                updateFrames(false);
            }
        } else if (event.getActionCommand().equals("boneLUT")) {

            if (tabbedPane.getSelectedComponent() == panelA) {
                LUTa.makeBoneTransferFunctions();
                LUTa.makeLUT(nColors);
                //lutAdjustCheckboxA.setSelected(false);
                setLUTA(LUTa);
                updateFrames(false);
            } else {
                LUTb.makeBoneTransferFunctions();
                LUTb.makeLUT(nColors);
                lutAdjustCheckboxB.setSelected(false);
                setLUTB(LUTb);
                updateFrames(false);
            }
        } else if (event.getActionCommand().equals("stripedLUT")) {

            if (tabbedPane.getSelectedComponent() == panelA) {
                LUTa.makeStripedLUT();
                //lutAdjustCheckboxA.setSelected(false);
                setLUTA(LUTa);
                updateFrames(false);
            } else {
                LUTb.makeStripedLUT();
                lutAdjustCheckboxB.setSelected(false);
                setLUTB(LUTb);
                updateFrames(false);
            }
        } else if (event.getActionCommand().equals("invertLUT")) {

            if (tabbedPane.getSelectedComponent() == panelA) {
                LUTa.invertLUT();
                setLUTA(LUTa);
                updateFrames(false);
            } else {
                LUTb.invertLUT();
                setLUTB(LUTb);
                updateFrames(false);
            }
        } else if (event.getActionCommand().equals("linearLUT")) {

            if (tabbedPane.getSelectedComponent() == panelA) {

                if (getHistoLUTComponentA() != null) {
                    getHistoLUTComponentA().setMode(getHistoLUTComponentA().LINEAR);
                    histoPanelA.updateLUTRecorder();
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    getHistoLUTComponentB().setMode(getHistoLUTComponentB().LINEAR);
                    histoPanelB.updateLUTRecorder();
                }
            }
            updateFrames(false);
        } else if (event.getActionCommand().equals("resetLinearLUT")) {

            if (tabbedPane.getSelectedComponent() == panelA) {

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
            updateFrames(false);
        } else if (event.getActionCommand().equals("evendistriLUT")) {

            if (tabbedPane.getSelectedComponent() == panelA) {

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
            updateFrames(false);
        } else if (event.getActionCommand().equals("thresholdLUT")) {

            if (tabbedPane.getSelectedComponent() == panelA) {

                if (getHistoLUTComponentA() != null) {
                    LUTa.setColor(255, new Color(200, 0, 0));
                    setLUTA(LUTa);
                    getHistoLUTComponentA().dualThresholdMode(ViewJComponentHLUTBase.DUAL_THRESHOLD);
                    threshLowerF.setEnabled(true);
                    threshUpperF.setEnabled(true);
                    threshLowerF.setText(Float.toString(((Vector2f) (getLUTa().getTransferFunction().getPoint(1))).X));
                    threshUpperF.setText(Float.toString(((Vector2f) (getLUTa().getTransferFunction().getPoint(4))).X));
                    histoPanelA.updateLUTRecorder();
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                    LUTb.setColor(255, new Color(200, 0, 0));
                    setLUTB(LUTb);
                    getHistoLUTComponentB().dualThresholdMode(ViewJComponentHLUTBase.DUAL_THRESHOLD);
                    histoPanelB.updateLUTRecorder();
                }
            }
        } else if (event.getActionCommand().equals("inverseThresholdLUT")) {

            // turn on the run threshold button
            // toolBarBottom.getComponentAtIndex(13).setEnabled(true);
            toolBarBottom.getComponentAtIndex(1).setEnabled(false);

            if (tabbedPane.getSelectedComponent() == panelA) {

                if (getHistoLUTComponentA() != null) {

                    // binaryThreshBox.setEnabled( true );
                    // menuObj.setMenuItemEnabled( "Threshold image", true );
                    // toolBarThreshold.getComponentAtIndex( 0 ).setEnabled( true );
                    // toolBarThreshold.getComponentAtIndex( 1 ).setEnabled( true );
                    // toolBarThreshold.getComponentAtIndex( 3 ).setEnabled( false );
                    // toolBarThreshold.getComponentAtIndex( 4 ).setEnabled( true );
                    LUTa.setColor(255, new Color(200, 0, 0));
                    setLUTA(LUTa);
                    getHistoLUTComponentA().dualThresholdMode(ViewJComponentHLUTBase.DUAL_THRESHOLD_INV);
                    threshLowerF.setEnabled(true);
                    threshUpperF.setEnabled(true);

                    // threshFillF.setEnabled( true );
                    threshLowerF.setText(Float.toString(((Vector2f) (getLUTa().getTransferFunction().getPoint(1))).X));
                    threshUpperF.setText(Float.toString(((Vector2f) (getLUTa().getTransferFunction().getPoint(3))).X));
                    histoPanelA.updateLUTRecorder();
                    // threshFillF.setText( Double.toString( imageA.getMin() ) );
                }
            } else {

                if (getHistoLUTComponentB() != null) {

                    // binaryThreshBoxB.setEnabled( true );
                    // menuObj.setMenuItemEnabled( "Threshold image", true );
                    // toolBarThreshold.getComponentAtIndex( 0 ).setEnabled( true );
                    // toolBarThreshold.getComponentAtIndex( 1 ).setEnabled( true );
                    // toolBarThreshold.getComponentAtIndex( 3 ).setEnabled( false );
                    // toolBarThreshold.getComponentAtIndex( 4 ).setEnabled( true );
                    LUTb.setColor(255, new Color(200, 0, 0));
                    setLUTB(LUTb);
                    getHistoLUTComponentB().dualThresholdMode(ViewJComponentHLUTBase.DUAL_THRESHOLD_INV);
                    histoPanelB.updateLUTRecorder();
                    // threshLowerBF.setEnabled( true ); threshUpperBF.setEnabled( true ); threshFillBF.setEnabled( true
                    // ); threshLowerBF.setText( Float.toString( ( (Vector3f) (
                    // getLUTb().getTransferFunction().elementAt( 1 ) ) ).x ) ); threshUpperBF.setText( Float.toString(
                    // ( (Vector3f) ( getLUTb().getTransferFunction().elementAt( 3 ) ) ).x ) ); threshFillBF.setText(
                    // Double.toString( imageB.getMin() ) );
                }
            }
        } else if (event.getActionCommand().equals("ctPresetsLUT")) { 
        	if (tabbedPane.getSelectedComponent() == panelA) {

                if (getHistoLUTComponentA() != null) {
                	ctDialogA = new JDialogCTHistoLUT((ViewJComponentHistoLUT)getHistoLUTComponentA());
                    ctDialogA.setVisible(true);
                }
            } else {

                if (getHistoLUTComponentB() != null) {
                	ctDialogB = new JDialogCTHistoLUT((ViewJComponentHistoLUT)getHistoLUTComponentB());
                    ctDialogB.setVisible(true);
                }
            }

        } else if (event.getActionCommand().equals("alpha")) {

            if (tabbedPane.getSelectedComponent() == panelA) {

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

            if (tabbedPane.getSelectedComponent() == panelA) {

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

            if (tabbedPane.getSelectedComponent() == panelA) {

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

            if (tabbedPane.getSelectedComponent() == panelA) {

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
        } else if (command.equals("SaveLUT")) {

            // save both the LUT and the transfer functions
            saveLUTAs(true, null, null);

            if (displayMode == IMAGE_A) {
                setLUTA(LUTa);
            } else {
                setLUTB(LUTb);
            }
        } else if (command.equals("SaveUDLUT")) {
        	
        	saveLUTandTransferFunction(true, "userdefine.lut", Preferences.getPreferencesDir());

            if (displayMode == IMAGE_A) {
                setLUTA(LUTa);
            } else {
                setLUTB(LUTb);
            }
        } else if (command.equals("OpenLUT")) {
            loadLUTFrom(true, null, null, false);

            if (displayMode == IMAGE_A) {
                setLUTA(LUTa);
            } else {
                setLUTB(LUTb);
            }
        } else if (command.equals("OpenUDLUT")) {
        	
        	loadLUTandTransferFunctionFrom(true, "userdefine.lut", Preferences.getPreferencesDir(), false);
            
            if (displayMode == IMAGE_A) {
                setLUTA(LUTa);
            } else {
                setLUTB(LUTb);
            }
            
            
        } else if (event.getActionCommand().equals("GenerateLUT")) {
            histoPanelA.showLUTRecorder();
        } else if (command.equals("SaveFuncts")) {

            // save only the transfer functions
            saveLUTAs(false, null, null);

            if (displayMode == IMAGE_A) {
                setLUTA(LUTa);
            } else {
                setLUTB(LUTb);
            }
        } else if (command.equals("OpenFuncts")) {

            // open only the transfer functions
            loadLUTFrom(false, null, null, false);

            if (displayMode == IMAGE_A) {
                setLUTA(LUTa);
            } else {
                setLUTB(LUTb);
            }
        } else if (event.getActionCommand().equals("Threshold")) { /*
                                                                    * calcThreshold();
                                                                    *
                                                                    * if ( imageA.getType() == ModelStorageBase.BOOLEAN )
                                                                    * { setVisible( false );
                                                                    * imageA.removeImageDisplayListener( this ); if (
                                                                    * imageB != null ) {
                                                                    * imageB.removeImageDisplayListener( this ); }
                                                                    * dispose(); }
                                                                    *
                                                                    * updateFrames( false );
                                                                    */
        } else if (event.getActionCommand().equals("ChangeNColors")) { /*
                                                                        * JDialogNColors dialog = new JDialogNColors(
                                                                        * this );
                                                                        *
                                                                        * if ( !dialog.isCancelled() ) { if (
                                                                        * tabbedPane.getSelectedComponent() == panelA )
                                                                        * { nColorsATextF.setText( String.valueOf(
                                                                        * dialog.getNColors() ) ); LUTa.makeLUT(
                                                                        * dialog.getNColors() ); setLUTA( LUTa ); } else
                                                                        * if ( tabbedPane.getSelectedComponent() ==
                                                                        * panelB ) { nColorsBTextF.setText(
                                                                        * String.valueOf( dialog.getNColors() ) );
                                                                        * LUTb.makeLUT( dialog.getNColors() ); setLUTB(
                                                                        * LUTb ); } updateFrames( false ); }
                                                                        */
        } else if (event.getActionCommand().equals("Close")) { /*
                                                                * setVisible( false );
                                                                * imageA.removeImageDisplayListener( this ); if ( imageB
                                                                * != null ) { imageB.removeImageDisplayListener( this );
                                                                * } dispose();
                                                                */
        } else if (event.getActionCommand().equals("UpdateA")) {
            updateHistoLUT(imageA, LUTa, null, null, false);
            // menuObj.setMenuItemEnabled( "Reset histogram & LUT A", false );
        } else if (event.getActionCommand().equals("UpdateB")) {
            updateHistoLUT(null, null, imageB, LUTb, false);
            // menuObj.setMenuItemEnabled( "Reset histogram & LUT B", false );
        } /*else if (event.getSource() == lutAdjustCheckboxA) {

            if (LUTa != null) {

                if (lutAdjustCheckboxA.isSelected()) {
                    LUTa.zeroToOneLUTAdjust();
                } else {
                    LUTa.oneToZeroLUTAdjust();
                }R

                updateFrames(false);
            }
        }*/ else if (event.getSource() == lutAdjustCheckboxB) {
            if (LUTb != null) {

                if (lutAdjustCheckboxB.isSelected()) {
                    LUTb.zeroToOneLUTAdjust();
                } else {
                    LUTb.oneToZeroLUTAdjust();
                }

                updateFrames(false);
            }
        } else if (event.getSource() instanceof JComboBox ) {
          JComboBox cb = (JComboBox)event.getSource();
          String lutName = (String)cb.getSelectedItem();
          if (tabbedPane.getSelectedComponent() == panelA) {
              LUTa.makeCustomizedLUT(lutName);
              setLUTA(LUTa);
              updateFrames(false);
          } else {
        	  LUTb.makeCustomizedLUT(lutName); 
              lutAdjustCheckboxB.setSelected(false);
              setLUTB(LUTb);
              updateFrames(false);
          }
        }
        // Setup threshold buttons to be enabled or disabled.
        /*
         * if ( tabbedPane.getSelectedComponent() == panelA ) { if ( getHistoLUTComponentA() != null &&
         * getHistoLUTComponentA().getMode() != ViewJComponentColorHistoLUT.DUAL_THRESHOLD ) {
         * menuObj.setMenuItemEnabled( "Threshold image", false ); } } else { if ( getHistoLUTComponentB() != null &&
         * getHistoLUTComponentB().getMode() != ViewJComponentColorHistoLUT.DUAL_THRESHOLD ) {
         * menuObj.setMenuItemEnabled( "Threshold image", false ); } }
         */
    }

    /**
     * Still needs more work Matt.
     */
    public void disposeLocal() {

        Preferences.debug("JPanelColorHistoLUT disposeLocal");
        imageA = null;
        imageB = null;
        LUTa = null;
        LUTb = null;

        if (histogramA != null) {
            histogramA.disposeLocal();
            histogramA = null;
        }

        if (histogramB != null) {
            histogramB.disposeLocal();
            histogramB = null;
        }

        if (histoPanelA != null) {
            histoPanelA.finalize();
            histoPanelA = null;
        }

        if (histoPanelB != null) {
            histoPanelB.finalize();
            histoPanelB = null;
        }
        
        if ( ctDialogA != null ) ctDialogA = null;
        if ( ctDialogB != null ) ctDialogB = null;
    }

    /**
     * Placeholder.
     *
     * @param  mouseEvent  drag event
     */
    public void dragPoint(MouseEvent mouseEvent) { }

    /**
     * Focus Events:
     *
     * @param  e  event handler.
     */
    public void focusGained(FocusEvent e) { // do nothing
    }

    /**
     * If focus is lost from either threshold field (lower/upper), update the new value on the LUT.
     *
     * @param  e  FocusEvent
     */
    public void focusLost(FocusEvent e) {

        if (e.getSource().equals(threshLowerF)) {

            if (this.testParameter(threshLowerF.getText(), ((Vector2f) (getLUTa().getTransferFunction().getPoint(0))).X,
                                       ((Vector2f) (getLUTa().getTransferFunction().getPoint(3))).X)) {

                getHistoLUTComponentA().updateDualThreshold(new Float(threshLowerF.getText()).floatValue(),
                                                            new Float(threshUpperF.getText()).floatValue());

            }
        } else if (e.getSource().equals(threshUpperF)) {

            if (this.testParameter(threshUpperF.getText(), ((Vector2f) (getLUTa().getTransferFunction().getPoint(2))).X,
                                       ((Vector2f) (getLUTa().getTransferFunction().getPoint(5))).X)) {

                // System.err.println("GOT HERE: UPPER");
                getHistoLUTComponentA().updateDualThreshold(new Float(threshLowerF.getText()).floatValue(),
                                                            new Float(threshUpperF.getText()).floatValue());
            }
        }
    }

    /**
     * Get the LUT panel current display mode.
     *
     * @return  int displayMode value, which is either IMAGE_A or IMAGE_B
     */
    public int getDisplayMode() {
        return displayMode;
    }

    /**
     * Get the histogram component for imageA.
     *
     * @return  the imageA histogram component
     */
    public ViewJComponentHLUTBase getHistoLUTComponentA() {
        return histoPanelA.getHistoLUTComponent();
    }

    /**
     * Get the histogram component for imageB.
     *
     * @return  the imageB histogram component
     */
    public ViewJComponentHLUTBase getHistoLUTComponentB() {

        if (histoPanelB == null) {
            return null;
        }

        return histoPanelB.getHistoLUTComponent();
    }

    /**
     * Accessor that returns the imageA.
     *
     * @return  image
     */
    public ModelImage getImageA() {
        return imageA;
    }

    /**
     * Accessor that returns the imageB.
     *
     * @return  imageB
     */
    public ModelImage getImageB() {
        return imageB;
    }

    /**
     * This method returns the current LUT.
     *
     * @return  LUT for Image A if selected, otherwise for Image B
     */
    public ModelLUT getLUT() {

        if (tabbedPane.getSelectedComponent() == panelA) {
            return LUTa;
        } else {
            return LUTb;
        }
    }

    /**
     * Get the imageA histo component lut.
     *
     * @return  ModelLUT
     */
    public final ModelLUT getLUTa() {
        return ((ViewJComponentHistoLUT) getHistoLUTComponentA()).getLUT();
    }

    /**
     * Get the imageB histo component lut.
     *
     * @return  ModelLUT
     */
    public final ModelLUT getLUTb() {

        if (getHistoLUTComponentB() == null) {
            return null;
        }

        return ((ViewJComponentHistoLUT) getHistoLUTComponentB()).getLUT();
    }

    /**
     * Get the imageA LUT.
     *
     * @return  the imageA LUT component
     */
    public ViewJComponentLUT getLUTComponentA() {
        return histoPanelA.getLUTComponent();
    }

    /**
     * Get the imageB LUT.
     *
     * @return  the imageB LUT component
     */
    public ViewJComponentLUT getLUTComponentB() {
        return histoPanelB.getLUTComponent();
    }

    /**
     * Return the main control panel.
     *
     * @return  mainPanel the main control panel
     */
    public JPanel getMainPanel() {
        return mainPanel;
    }

    /**
     * Returns which LUT is currently visible.
     *
     * @return  return the currently selcted LUT table index.
     */
    public int getSelectedLUT() {

        if (tabbedPane.getSelectedComponent() == panelA) {
            return 0;
        }

        return 1;
    }

    /**
     * This method is used to obtain the VOIBase Single Transfer Line.
     *
     * @return  LUTb.getTransferFunction() - VOIBase of Single Transfer Line for Image B
     */
    public TransferFunction getTransferLine() {

        if (tabbedPane.getSelectedComponent() == panelA) {
            return LUTa.getTransferFunction();
        } else {
            return LUTb.getTransferFunction();
        }
    }

    /**
     * {@inheritDoc}
     */
    public boolean isImageUpdate() {

        // return updateCheckBoxA.isSelected();
        return bImageUpdate;
        //return false;
    }

    // ********************************************************************
    // ************************** Item Events *****************************
    // ********************************************************************

    /**
     * Sets the flags for the checkboxes.
     *
     * @param  event  event that triggered this function
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

        } /*else if ( source == updateCheckBoxA ) {
           * if ( updateCheckBoxB != null ) {   updateCheckBoxB.removeItemListener( this ); } if (
           * updateCheckBoxA.isSelected() == true && updateCheckBoxB != null ) {   updateCheckBoxB.setSelected( true );
           * } else if ( updateCheckBoxA.isSelected() == false && updateCheckBoxB != null ) {
           * updateCheckBoxB.setSelected( false ); } if ( updateCheckBoxB != null ) {   updateCheckBoxB.addItemListener(
           * this ); } } else if ( source == updateCheckBoxB ) { updateCheckBoxB.removeItemListener( this ); if (
           * updateCheckBoxB.isSelected() == true ) {   updateCheckBoxA.setSelected( true ); } else {
           * updateCheckBoxA.setSelected( false ); } updateCheckBoxA.addItemListener( this );}*/

    }

    /**
     * Unchanged.
     *
     * @param  e  key press event handler.
     */
    public void keyPressed(KeyEvent e) { }

    /**
     * Unchanged.
     *
     * @param  e  kay release event handler.
     */
    public void keyReleased(KeyEvent e) { }

    /**
     * If the ENTER key is hit while in threshold boxes, update the LUT's threshold (for dual threshold).
     *
     * @param  e  KeyEvent
     */
    public void keyTyped(KeyEvent e) {

        if (e.getKeyChar() == KeyEvent.VK_ENTER) {

            if (e.getSource().equals(threshLowerF)) {

                if (this.testParameter(threshLowerF.getText(),
                                           ((Vector2f) (getLUTa().getTransferFunction().getPoint(0))).X,
                                           ((Vector2f) (getLUTa().getTransferFunction().getPoint(3))).X)) {

                    getHistoLUTComponentA().updateDualThreshold(new Float(threshLowerF.getText()).floatValue(),
                                                                new Float(threshUpperF.getText()).floatValue());
                }
            } else if (e.getSource().equals(threshUpperF)) {

                if (this.testParameter(threshUpperF.getText(),
                                           ((Vector2f) (getLUTa().getTransferFunction().getPoint(2))).X,
                                           ((Vector2f) (getLUTa().getTransferFunction().getPoint(5))).X)) {

                    // System.err.println("GOT HERE: UPPER");
                    getHistoLUTComponentA().updateDualThreshold(new Float(threshLowerF.getText()).floatValue(),
                                                                new Float(threshUpperF.getText()).floatValue());
                }
            }
        }

    }

    /**
     * This method loads the LUT for the active image. If the image is not a color image then both the functions and the
     * LUT data are loaded. If this is a color image, then only the functions are loaded.
     *
     * @param  loadAll    loadAll boolean indicating that both lut and transfer functions should be loaded. If false,
     *                    then only transfer functions are loaded.
     * @param  filename   filename filename to save LUT as
     * @param  dirName    dirName directory to save LUT to
     * @param  quietMode  quietMode if true indicates that warnings should not be displayed.
     */
    public void loadLUTFrom(boolean loadAll, String filename, String dirName, boolean quietMode) {

        ModelRGB rgb;
        ModelLUT lut;
        ModelImage img;
        FileHistoLUT fileHistoLUT;
        boolean useLUT = false;

        if (displayMode == IMAGE_A) {
            img = this.getImageA();
            useLUT = true;
            rgb = null;
            lut = LUTa;
        } else {
            img = this.getImageB();
            useLUT = true;
            rgb = null;
            lut = LUTb;

        }

        // if not using a lut (i.e. rgb only), then you
        // can't loadAll.... there are only functions, so
        // reset the loadAll variable
        if (!useLUT) {
            loadAll = false;
        }

        if ((filename == null) || (dirName == null)) {
            dirName = img.getFileInfo(0).getFileDirectory();

            if (dirName == null) {
                dirName = System.getProperties().getProperty("user.dir");
            }

            JFileChooser chooser = new JFileChooser();

            chooser.setCurrentDirectory(new File(dirName));

            if (loadAll) {
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.LUT));
            } else {
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.FUNCT));
            }

            int returnVal = chooser.showOpenDialog(ViewUserInterface.getReference().getActiveImageFrame());

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                filename = chooser.getSelectedFile().getName();
                dirName = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            } else if (returnVal == JFileChooser.CANCEL_OPTION) {
                chooser.setVisible(false);

                return;
            }
        }

        try {

            if (useLUT) {
                fileHistoLUT = new FileHistoLUT(filename, dirName, lut);

                if (loadAll) {
                    fileHistoLUT.readLUTandTransferFunction(quietMode);
                } else {
                    fileHistoLUT.readFunctions();
                }

                if (displayMode == IMAGE_A) {
                    this.setLUTa(lut);
                } else {
                    this.setLUTb(lut);
                }
            } else {
                fileHistoLUT = new FileHistoLUT(filename, dirName, rgb);

                if (loadAll) {
                    fileHistoLUT.readLUTandTransferFunction(quietMode);
                } else {
                    fileHistoLUT.readFunctions();
                }
            }

            img.notifyImageDisplayListeners(lut, true);

        } catch (IOException error) {

            if (!quietMode) {
                MipavUtil.displayError("Error reading LUT: \n" + error.getMessage());
            }
        }
    } // end loadLUTFrom()

    /**
     * This method loads the LUT for the active image. If the image is not a color image then both the functions and the
     * LUT data are loaded. If this is a color image, then only the functions are loaded.
     *
     * @param  loadAll    loadAll boolean indicating that both lut and transfer functions should be loaded. If false,
     *                    then only transfer functions are loaded.
     * @param  filename   filename filename to save LUT as
     * @param  dirName    dirName directory to save LUT to
     * @param  quietMode  quietMode if true indicates that warnings should not be displayed.
     */
    public void loadLUTandTransferFunctionFrom(boolean loadAll, String filename, String dirName, boolean quietMode) {

        ModelRGB rgb;
        ModelLUT lut;
        ModelImage img;
        FileHistoLUT fileHistoLUT;
        boolean useLUT = false;

        if (displayMode == IMAGE_A) {
            img = this.getImageA();
            useLUT = true;
            rgb = null;
            lut = this.LUTa;
        } else {
            img = this.getImageB();
            useLUT = true;
            rgb = null;
            lut = LUTb;
        }

        // if not using a lut (i.e. rgb only), then you
        // can't loadAll.... there are only functions, so
        // reset the loadAll variable
        if (!useLUT) {
            loadAll = false;
        }

        try {

            if (useLUT) {
                fileHistoLUT = new FileHistoLUT(filename, dirName, lut);

                if (loadAll) {
                    fileHistoLUT.readLUTandTransferFunction(quietMode);
                } else {
                    fileHistoLUT.readFunctions();
                }

                if (displayMode == IMAGE_A) {
                    this.setLUTa(lut);
                } else {
                    this.setLUTb(lut);
                }
            } else {
                fileHistoLUT = new FileHistoLUT(filename, dirName, rgb);

                if (loadAll) {
                    fileHistoLUT.readLUTandTransferFunction(quietMode);
                } else {
                    fileHistoLUT.readFunctions();
                }
            }

            img.notifyImageDisplayListeners(lut, true);

        } catch (IOException error) {

            if (!quietMode) {
                MipavUtil.displayError("Error reading LUT: \n" + error.getMessage());
            }
        }
    } // end loadLUTFrom()

    /**
     * Enable button to indicate image has changed and the histogram should be recalculated.
     *
     * @param  LUT        new Lookup table
     * @param  imageAorB  indicates which histogram needs to be recalculated
     */
    public void notifyOfUpdate(ModelLUT LUT, int imageAorB) {

        if (imageAorB == ModelImage.IMAGE_A) {
            // menuObj.setMenuItemEnabled( "Reset histogram & LUT A", true );
        } else {
            // menuObj.setMenuItemEnabled( "Reset histogram & LUT B", true );
        }
    }

    /**
     * Removes the tabbed pane for the histogram of image B.
     */
    public void removeHistoLUTb() {

        if (tabbedPane.getTabCount() == 2) {
            tabbedPane.removeTabAt(1);

            // menuObj.setMenuItemEnabled( "Reset histogram & LUT B", false );
            imageB = null;
            tabbedPane.validate();
        }
    }

    /**
     * Resizig the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   panel width.
     * @param  frameHeight  parent frame height.
     */
    public void resizePanel(int panelWidth, int frameHeight) {
        scroller.setPreferredSize(new Dimension(panelWidth, frameHeight - (topPanel.getHeight() * 2)));
        scroller.setSize(new Dimension(panelWidth, frameHeight - (topPanel.getHeight() * 2)));
        scroller.revalidate();
    }

    /**
     * This method saves the LUT for the active image. If the image is not a color image then both the functions and the
     * LUT data are saved. If this is a color image, then only the functions are saved.
     *
     * @param  saveAll   boolean indicating that both lut and transfer functions should be saved. If false, then only
     *                   transfer functions are saved.
     *
     *                   <p>If either filename or directory is null, then the user will be prompted for a filename.</p>
     * @param  filename  filename to save LUT as
     * @param  dirName   directory to save LUT to
     */
    public void saveLUTAs(boolean saveAll, String filename, String dirName) {

        ModelRGB rgb;
        ModelLUT lut;
        ModelImage img;
        FileHistoLUT fileHistoLUT;
        boolean useLUT = false;

        if (displayMode == IMAGE_A) {
            img = this.getImageA();

            useLUT = true;
            rgb = null;
            lut = LUTa;

        } else {
            img = this.getImageB();
            useLUT = true;
            rgb = null;
            lut = LUTb;
        }

        // if not using a lut (i.e. rgb only), then you
        // can't saveAll.... there are only functions, so
        // reset the saveAll variable
        if (!useLUT) {
            saveAll = false;
        }

        // if filename and/or dirName is null, then get it from user
        if ((filename == null) || (dirName == null)) {
            dirName = img.getFileInfo(0).getFileDirectory();

            if (dirName == null) {
                dirName = System.getProperties().getProperty("user.dir");
            }

            JFileChooser chooser = new JFileChooser();

            chooser.setCurrentDirectory(new File(dirName));

            if (saveAll) {
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.LUT));
            } else {
                chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.FUNCT));
            }

            int returnVal = chooser.showSaveDialog(ViewUserInterface.getReference().getActiveImageFrame());

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                filename = chooser.getSelectedFile().getName();
                dirName = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            } else if (returnVal == JFileChooser.CANCEL_OPTION) {
                chooser.setVisible(false);

                return;
            }

        }

        try {

            if (useLUT) {
                fileHistoLUT = new FileHistoLUT(filename, dirName, lut);
            } else {
                fileHistoLUT = new FileHistoLUT(filename, dirName, rgb);
            }

            if (saveAll) {
                fileHistoLUT.writeAll();
            } else {
                fileHistoLUT.writeFunctions();
            }

        } catch (IOException error) {
            MipavUtil.displayError("Error writing LUT: \n" + error.getMessage());
        }

    } // end saveLUTAs()

    /**
     * This method saves the LUT for the active image. If the image is not a color image then both the functions and the
     * LUT data are saved. If this is a color image, then only the functions are saved.
     *
     * @param  saveAll   boolean indicating that both lut and transfer functions should be saved. If false, then only
     *                   transfer functions are saved.
     *
     *                   <p>If either filename or directory is null, then the user will be prompted for a filename.</p>
     * @param  filename  filename to save LUT as
     * @param  dirName   directory to save LUT to
     */
    public void saveLUTandTransferFunction(boolean saveAll, String filename, String dirName) {

        ModelRGB rgb;
        ModelLUT lut;
        ModelImage img;
        FileHistoLUT fileHistoLUT;
        boolean useLUT = false;

        if (displayMode == IMAGE_A) {
            img = this.getImageA();

            useLUT = true;
            rgb = null;
            lut = LUTa;
        } else {
            img = this.getImageB();
            useLUT = true;
            rgb = null;
            lut = LUTb;
        }

        try {

            if (useLUT) {
                fileHistoLUT = new FileHistoLUT(filename, dirName, lut);
            } else {
                fileHistoLUT = new FileHistoLUT(filename, dirName, rgb);
            }

            fileHistoLUT.writeLUTandTransferFunction();
            Preferences.setDefaultDisplay(DefaultDisplay.LUT);

        } catch (IOException error) {
            MipavUtil.displayError("Error writing LUT: \n" + error.getMessage());
        }

    } // end saveLUTAs()

    /**
     * Forces the panel to display the LUTB panel.
     */
    public void selectLUTa() {

        if (tabbedPane.getSelectedComponent() == panelA) {
            return;
        } else {
            tabbedPane.setSelectedComponent(panelA);
        }
    }

    /**
     * Forces the panel to display the LUTB panel.
     */
    public void selectLUTb() {

        if (tabbedPane.getSelectedComponent() == panelB) {
            return;
        } else {
            tabbedPane.setSelectedComponent(panelB);
        }
    }

    /**
     * Placeholder.
     */
    public void setAllOff() { }

    /**
     * This method is used to set Computed Tomography (CT) presets for CT images.
     *
     * @param  st   starting preset of the window
     * @param  end  ending preset of the window
     *
     * @see    JDialogCT
     */
    public void setCTMode(int st, int end) {

        if (tabbedPane.getSelectedComponent() == panelA) {
            ((ViewJComponentHistoLUT) getHistoLUTComponentA()).ctMode(st, end);
        } else {
            ((ViewJComponentHistoLUT) getHistoLUTComponentB()).ctMode(st, end);
        }
    }

    /**
     * Change the histogram component LUT.
     *
     * @param  lut  the new lut
     */
    public final void setHistoLUTa(ModelLUT lut) {
        ((ViewJComponentHistoLUT) getHistoLUTComponentA()).setLUT(lut);
    }

    /**
     * Change the histogram component LUT.
     *
     * @param  lut  the new lut
     */
    public final void setHistoLUTb(ModelLUT lut) {
        ((ViewJComponentHistoLUT) getHistoLUTComponentB()).setLUT(lut);
    }

    /**
     * Accessor that sets the imageA.
     *
     * @param  image  image to set frame to
     */
    public void setImageA(ModelImage image) {
        imageA = image;
    }

    /**
     * Accessor that sets the imageB.
     *
     * @param  image  image to set frame to
     */
    public void setImageB(ModelImage image) {
        imageB = image;
    }

    /**
     * This method sets the histogram to Linear Transfer Function Mode.
     */
    public void setLinearLUT() {

        if (tabbedPane.getSelectedComponent() == panelA) {

            if (getHistoLUTComponentA() != null) {
                getHistoLUTComponentA().setMode(getHistoLUTComponentA().LINEAR);
            }
        } else {

            if (getHistoLUTComponentB() != null) {
                getHistoLUTComponentB().setMode(getHistoLUTComponentB().LINEAR);
            }
        }
    }

    /**
     * {@inheritDoc}
     */
    public void setLUT(ModelLUT LUT) {

        if (tabbedPane.getSelectedComponent() == panelA) {
            setLUTA(LUT);
        } else {
            setLUTB(LUT);
        }
    }

    /**
     * Replaces the LUT A component.
     *
     * @param  LUT  new LUT
     */
    public void setLUTA(ModelLUT LUT) {
        setLUTa(LUT);

        if (tabbedPane.getSelectedComponent() == panelA) {
            getLUTComponentA().show(LUT);
            getHistoLUTComponentA().showHistogram(LUT);
        }
    }

    /**
     * Set imageA lookup table.
     *
     * @param  _LUTa  ModelLUTa
     */
    public void setLUTa(ModelLUT _LUTa) {
        LUTa = _LUTa;
    }

    /**
     * Replaces the LUT B component.
     *
     * @param  LUT  new LUT
     */
    public void setLUTB(ModelLUT LUT) {
        setLUTb(LUT);

        if (tabbedPane.getSelectedComponent() == panelB) {
            getLUTComponentB().show(LUT);
            getHistoLUTComponentB().showHistogram(LUT);
        }
    }

    /**
     * Set imageB lookup table.
     *
     * @param  _LUTb  ModelLUTb
     */
    public void setLUTb(ModelLUT _LUTb) {
        LUTb = _LUTb;
    }

    /**
     * {@inheritDoc}
     */
    public void setRangeText(float x, float y, int _index) {
        String start, mid, end;

        if (tabbedPane.getSelectedComponent() == panelA) {
            String str = String.valueOf(x);

            cursorIndex = _index;
            rangeX = x;

            int index = str.indexOf(".");
            int length = str.length();
            int indexE = str.indexOf("E");

            if (((index + 2) < length) && (indexE == -1)) {
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
                start = makeString(x - scaleRangeA, 2);
                mid = makeString(x, 2);
                end = makeString(x + scaleRangeA, 2);
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
        } else if (tabbedPane.getSelectedComponent() == panelB) {
            String str = String.valueOf(x);

            cursorIndexB = _index;
            rangeXB = x;

            int index = str.indexOf(".");
            int length = str.length();
            int indexE = str.indexOf("E");

            if (((index + 2) < length) && (indexE == -1)) {
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
                start = makeString(x - scaleRangeB, 2);
                mid = makeString(x, 2);
                end = makeString(x + scaleRangeB, 2);
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
     * Does nothing in this.
     *
     * @param  slice  slice value.
     */
    public void setSlice(int slice) { }

    /**
     * Does nothing in this class.
     *
     * @param  tSlice  time slice value.
     */
    public void setTimeSlice(int tSlice) { }

    /**
     * Sets values based on knob along slider.
     *
     * @param  e  event that triggered this function
     */
    public void stateChanged(ChangeEvent e) {
        float value, sliderValue;
        Object source = e.getSource();

        // Slider has changed lets update.
        if ((source == tabbedPane) && (tabbedPane.getSelectedComponent() == panelA)) {
            displayMode = IMAGE_A;
            setLUTA(LUTa);
            // setTitle( "Lookup Table: " + imageA.getImageName() );
        } else if ((source == tabbedPane) && (tabbedPane.getSelectedComponent() == panelB) && (imageB != null)) {
            displayMode = IMAGE_B;
            setLUTB(LUTb);
            // setTitle( "Lookup Table: " + imageB.getImageName() );
        }

        if (source == mouseSlider) {

            // componentOpacityA.updateCursor(rangeX, 100-mouseSlider.getValue(), cursorIndex);
            if (mouseSlider.getValueIsAdjusting() == true) {
                return;
            }

            if (xRangeTextA != null) {
                sliderValue = mouseSlider.getValue();

                if (mouseSliderLabels[0].getText().equals("0")) {
                    value = rangeX + ((sliderValue) / 100.0f * scaleRangeA * 2.0f);
                } else {

                    if (sliderValue > 50) {
                        value = rangeX + ((sliderValue - 50) / 100.0f * scaleRangeA * 2.0f);
                    } else {
                        value = rangeX - ((50 - sliderValue) / 100.0f * scaleRangeA * 2.0f);
                    }
                }

                // value = (mouseSlider.getValue() / 100.0f * 32.0f) + rangeX;
                xRangeTextA.setText(makeString(value, 2));
                ((ViewJComponentHistoLUT) getHistoLUTComponentA()).updateCursorXPos(value, 100 - mouseSlider.getValue(),
                                                                                    cursorIndex);
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
                        value = rangeXB + ((sliderValue - 50) / 100.0f * scaleRangeB * 2.0f);
                    } else {
                        value = rangeXB - ((50 - sliderValue) / 100.0f * scaleRangeB * 2.0f);
                    }
                }

                // value = (mouseSliderB.getValue() / 100.0f * 32.0f) + rangeXB;
                xRangeTextB.setText(makeString(value, 2));
                ((ViewJComponentHistoLUT) getHistoLUTComponentB()).updateCursorXPos(value,
                                                                                    100 - mouseSliderB.getValue(),
                                                                                    cursorIndexB);
            } else {
                ((ViewJComponentHistoLUT) getHistoLUTComponentB()).updateCursor(rangeXB, 100 - mouseSliderB.getValue(),
                                                                                cursorIndexB);
            }
        }
    }

    /**
     * update the LUT and images.   
     */
    public void updateComponentLUT() { 
    	updateHistoLUT(imageA, LUTa, null, null, false);
    	updateFrames(false);
    }

    /**
     * {@inheritDoc}
     */
    public void updateFrames(boolean flag) {

        if ((imageA != null) && (tabbedPane.getSelectedComponent() == panelA)) {
            imageA.notifyImageDisplayListeners(LUTa, flag);
        }

        if ((imageB != null) && (tabbedPane.getSelectedComponent() == panelB)) {
            imageB.notifyImageDisplayListeners(LUTb, flag);
        }
    }

    /**
     * This method is called to update the histogram(s) displayed in each tabbed pane of the frame.
     *
     * @param  _imageA       image A
     * @param  _LUTa         lookup table for image A
     * @param  _imageB       image B
     * @param  _LUTb         lookup table for image B
     * @param  progressFlag  passed to calculateHistogram algorithm. If false progress bar is not displayed
     */
    public void updateHistoLUT(ModelImage _imageA, ModelLUT _LUTa, ModelImage _imageB, ModelLUT _LUTb,
                               boolean progressFlag) {

        if (_imageA != null) {
            setImageA(_imageA);

            if (_LUTa != null) {
                setLUTa(_LUTa);

                if (tabbedPane.getSelectedComponent() == panelA) {
                    getLUTComponentA().show(LUTa);
                }
            }

            calcHistogram(IMAGE_A, entireFlag);
            setHistoLUTa(LUTa);
            getHistoLUTComponentA().setHistogramInfo(imageA, histogramA);

            if (tabbedPane.getSelectedComponent() == panelA) {
                getHistoLUTComponentA().linearMode();
                getHistoLUTComponentA().showHistogram(_LUTa);
            }
        }

        if ((_imageB != null) && (imageB != null)) {
            setImageB(_imageB);

            if (_LUTb != null) {
                setLUTb(_LUTb);
            }

            if (panelB == null) {
                buildPanelB(imageB, LUTb, true);
            }

            if (tabbedPane.getSelectedComponent() == panelB) {
                getLUTComponentB().show(LUTb);
            }

            calcHistogram(IMAGE_B, entireFlag);
            setHistoLUTb(LUTb);
            getHistoLUTComponentB().setHistogramInfo(imageB, histogramB);

            if (tabbedPane.getSelectedComponent() == panelB) {
                getHistoLUTComponentB().linearMode();
                getHistoLUTComponentB().showHistogram(LUTb);
            }
        } else if ((_imageB != null) && (imageB == null)) {

            if (_LUTb != null) {
                setLUTb(_LUTb);
            }

            setImageB(_imageB);

            // menuObj.setMenuItemEnabled( "Reset histogram & LUT B", true );
            buildPanelB(imageB, LUTb, true);
        }

        tabbedPane.validate();
    }

    /**
     * This methods calls the componentImage's REPAINT method to redraw the screen. The extents on this image have
     * changed, so the extents need to be read in again and menus, panes and slide bars adjusted accordingly.
     *
     * @return  return false. 
     */
    public boolean updateImageExtents() {
        return false;
    }

    // **** Methods here only because ViewImageUpdateInterface is implemented. They do
    // nothing here.

    /**
     * This methods calls the componentImage's REPAINT method to redraw the screen. Without LUT changes or image changes
     *
     * @return  return false.
     */
    public boolean updateImages() {
        return false;
    }

    /**
     * This methods calls the componentImage's update method to redraw the screen. Without LUT changes. Does nothing in
     * this class.
     *
     * @param   flag  forces show to re import image and calc. java image
     *
     * @return  boolean confirming successful update
     */
    public boolean updateImages(boolean flag) {
        return false;
    }

    /**
     * This methods calls the componentImage's update method to redraw the screen. Does nothing in this class.
     *
     * @param   LUTa        LUT used to update imageA
     * @param   LUTb        LUT used to update imageB
     * @param   flag        forces show to re import image and calc. java image
     * @param   interpMode  image interpolation method (Nearest or Smooth)
     *
     * @return  boolean confirming a successful update
     */
    public boolean updateImages(ModelLUT LUTa, ModelLUT LUTb, boolean flag, int interpMode) {
        return false;
    }

    /**
     * {@inheritDoc}
     */
    public void updateLUTPositionString(String str) {

        if (tabbedPane.getSelectedComponent() == panelA) {
            indexColorATextF.setText(str);
        } else {
            indexColorBTextF.setText(str);
        }
    }

    /**
     * {@inheritDoc}
     */
    public void updateThresholdFields(float lower, float upper) {
        threshLowerF.setText(Float.toString(lower));
        threshUpperF.setText(Float.toString(upper));
    }

    /**
     * Calls dispose.
     *
     * @throws  Throwable dispose memory.
     */
    protected void finalize() throws Throwable {
        this.disposeLocal();
        super.finalize();
    }

    /**
     * Makes a string of a float with a specific number of decimal points.
     *
     * @param   number  number to be converted to a string
     * @param   decPts  the number of decimal points
     *
     * @return  string representation of the number
     */
    protected String makeString(float number, int decPts) {
        String subStr = null;
        String tmpStr = null;
        String str = null;

        try {
            subStr = new String();
            tmpStr = new String();
            str = new String(tmpStr.valueOf(number));
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("FrameBase.makeString: out of memory");

            return null;
        }

        int index = str.indexOf(".");
        int length = str.length();

        if ((index + decPts) < length) {
            subStr = str.substring(0, index + decPts + 1);
        } else {
            subStr = str;
        }

        return subStr;
    }

    /**
     * Tests that the entered parameter is in range.
     *
     * @param   str       the value entered by the user
     * @param   minValue  the minimum value this variable may be set to
     * @param   maxValue  the maximum value this variable may be set to
     *
     * @return  boolean result of test
     */
    protected boolean testParameter(String str, double minValue, double maxValue) {

        double tmp;

        try {
            tmp = Double.valueOf(str).doubleValue();

            if ((tmp > maxValue) || (tmp < minValue)) {
                MipavUtil.displayError("Value is out of range.");

                return false;
            } else {
                return true;
            }
        } catch (NumberFormatException error) {
            MipavUtil.displayError("Must enter numeric value");

            return false;
        }
    }

    /**
     * Method that displays the histogram and LUT and other controls to manipulate the LUT. Panel for image A.
     *
     * @param  image       Model of image
     * @param  LUT         Model of LUT
     * @param  entireFlag  Flag indicating if histogram should be made of entire image.
     */
    private void buildPanelA(ModelImage image, ModelLUT LUT, boolean entireFlag) {
        calcHistogram(IMAGE_A, entireFlag);

        JPanel controlPanel = new JPanel(new GridBagLayout());

        controlPanel.setBorder(new EtchedBorder());

        /*updateCheckBoxA = new JCheckBox( "Update image (real-time).", Preferences.is(Preferences.PREF_HISTOGRAM_DISPLAY) );
         * updateCheckBoxA.setFont( MipavUtil.font12 ); updateCheckBoxA.addItemListener( this );
         * updateCheckBoxA.setSelected( false );
         */
        logCheckBoxA = new JCheckBox("Log scale (Histogram).", true);
        logCheckBoxA.setFont(MipavUtil.font12);
        logCheckBoxA.addItemListener(this);

        //lutAdjustCheckboxA = new JCheckBox("0 to 1 LUT adjustment");
        //lutAdjustCheckboxA.setFont(MipavUtil.font12);
        //lutAdjustCheckboxA.addActionListener(this);

        JLabel nColorsLabel = new JLabel("Number of colors: ");

        nColorsLabel.setFont(MipavUtil.font12);
        nColorsLabel.setForeground(Color.black);

        nColorsATextF = new JTextField("256");
        nColorsATextF.setFont(MipavUtil.font12);
        nColorsATextF.setEnabled(false);

        JLabel indexColorLabel = new JLabel("LUT:");

        indexColorLabel.setFont(MipavUtil.font12);
        indexColorLabel.setForeground(Color.black);

        indexColorATextF = new JTextField(13);
        indexColorATextF.setFont(MipavUtil.font12);
        indexColorATextF.setEnabled(false);

        threshLowerF = new JTextField(5);
        threshLowerF.setFont(MipavUtil.font12);
        threshLowerF.setEnabled(false);
        threshLowerF.addFocusListener(this);
        threshLowerF.addKeyListener(this);

        threshUpperF = new JTextField(5);
        threshUpperF.setFont(MipavUtil.font12);
        threshUpperF.setEnabled(false);
        threshUpperF.addFocusListener(this);
        threshUpperF.addKeyListener(this);

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.weightx = 1;
        gbc.weighty = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(0, 5, 0, -5);
        // controlPanel.add( updateCheckBoxA, gbc );

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.insets = new Insets(0, 5, 0, -5);
        controlPanel.add(logCheckBoxA, gbc);

        //gbc.gridx = 1;
        //gbc.gridy = 0;
        //gbc.insets = new Insets(0, 0, 0, 0);
        //controlPanel.add(lutAdjustCheckboxA, gbc);

        gbc.gridy = 2;
        gbc.gridx = 0;
        gbc.ipadx = -5;
        gbc.insets = new Insets(0, 10, 0, -20);
        controlPanel.add(nColorsLabel, gbc);
        gbc.gridx = 1;
        gbc.insets = new Insets(0, 0, 0, 5);
        controlPanel.add(nColorsATextF, gbc);

        gbc.gridy = 3;
        gbc.gridx = 0;
        gbc.insets = new Insets(0, 10, 0, -20);
        controlPanel.add(indexColorLabel, gbc);
        gbc.gridx = 1;
        gbc.insets = new Insets(0, 0, 0, 5);
        controlPanel.add(indexColorATextF, gbc);

        histoPanelA = new ViewJPanelHistoLUT(this, image, LUT, histogramA);

        panelA = new JPanel();
        panelA.setLayout(new BoxLayout(panelA, BoxLayout.Y_AXIS));
        panelA.add(controlPanel);
        panelA.add(histoPanelA);
        tabbedPane.addTab("ImageA", null, panelA);
        tabbedPane.setFont(MipavUtil.font12B);

        mouseLabel = new JLabel("    X Scale");
        mouseLabel.setFont(MipavUtil.font12B);
        mouseLabel.setForeground(Color.black);

        scaleRangeA = ((int) Math.round(imageA.getMax() - imageA.getMin()) + 1) / 256;

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
        panelA.add(panelMouse);

    }

    /**
     * Method that displays the histogram and LUT and other controls to manipulate the LUT. Panel for image B.
     *
     * @param  image       Model of image
     * @param  LUT         Model of LUT
     * @param  entireFlag  Flag indicating if histogram should be made of entire image.
     */
    private void buildPanelB(ModelImage image, ModelLUT LUT, boolean entireFlag) {

        // go calc histo
        calcHistogram(IMAGE_B, entireFlag);

        JPanel controlPanelB = new JPanel(new GridBagLayout());

        controlPanelB.setBorder(new EtchedBorder());

        /*updateCheckBoxB = new JCheckBox( "Update (real-time)", Preferences.is(Preferences.PREF_HISTOGRAM_DISPLAY) );
         * updateCheckBoxB.setFont( MipavUtil.font12 ); updateCheckBoxB.addItemListener( this
         * );updateCheckBoxB.setSelected( false );*/

        logCheckBoxB = new JCheckBox("Log scale (Histogram)", true);
        logCheckBoxB.setFont(MipavUtil.font12);
        logCheckBoxB.addItemListener(this);

        lutAdjustCheckboxB = new JCheckBox("0 to 1 LUT adjustment");
        lutAdjustCheckboxB.setFont(MipavUtil.font12);
        lutAdjustCheckboxB.addActionListener(this);

        JLabel nColorsLabel = new JLabel("Number of colors:  ");

        nColorsLabel.setFont(MipavUtil.font12);
        nColorsLabel.setForeground(Color.black);

        nColorsBTextF = new JTextField("256");
        nColorsBTextF.setFont(MipavUtil.font12);
        nColorsBTextF.setEnabled(false);

        JLabel indexColorLabel = new JLabel("LUT:");

        indexColorLabel.setFont(MipavUtil.font12);
        indexColorLabel.setForeground(Color.black);

        indexColorBTextF = new JTextField(15);
        indexColorBTextF.setFont(MipavUtil.font12);
        indexColorBTextF.setEnabled(false);

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 1;
        gbc.weightx = 1;
        gbc.weighty = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.insets = new Insets(0, 5, 0, -5);
        // controlPanelB.add( updateCheckBoxB, gbc );

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.insets = new Insets(0, 5, 0, -5);
        controlPanelB.add(logCheckBoxB, gbc);

        gbc.gridx = 1;
        gbc.gridy = 0;
        gbc.insets = new Insets(0, 0, 0, 0);
        controlPanelB.add(lutAdjustCheckboxB, gbc);

        gbc.gridy = 2;
        gbc.gridx = 0;
        gbc.ipadx = -5;
        gbc.insets = new Insets(0, 10, 0, -20);
        controlPanelB.add(nColorsLabel, gbc);
        gbc.gridx = 1;
        gbc.insets = new Insets(0, 0, 0, 5);
        controlPanelB.add(nColorsBTextF, gbc);

        gbc.gridy = 3;
        gbc.gridx = 0;
        gbc.insets = new Insets(0, 10, 0, -20);
        controlPanelB.add(indexColorLabel, gbc);
        gbc.gridx = 1;
        gbc.insets = new Insets(0, 0, 0, 5);
        controlPanelB.add(indexColorBTextF, gbc);

        histoPanelB = new ViewJPanelHistoLUT(this, image, LUT, histogramB);

        panelB = new JPanel();
        panelB.setLayout(new BoxLayout(panelB, BoxLayout.Y_AXIS));
        panelB.add(controlPanelB);
        panelB.add(histoPanelB);
        tabbedPane.addTab("ImageB", null, panelB);
        tabbedPane.setFont(MipavUtil.font12B);

        mouseLabelB = new JLabel("    X Scale");
        mouseLabelB.setFont(MipavUtil.font12B);
        mouseLabelB.setForeground(Color.black);

        scaleRangeB = ((int) Math.round(imageA.getMax() - imageA.getMin()) + 1) / 256;

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
        panelB.add(panelMouse);

    }

    /**
     * Calculates histogram for the image(s).
     *
     * @param  imageAorB   flag to indicate if histogram is to be calculated for imageA or imageB.
     * @param  entireFlag  if true calculate histogram for the entire image. if false uses areas defined by VOI regions.
     */
    private void calcHistogram(int imageAorB, boolean entireFlag) {

        int[] dimExtentsA = new int[1];
        int[] dimExtentsB = new int[1];

        if ((imageA != null) && (imageAorB == IMAGE_A)) {
            dimExtentsA[0] = 256;
            histogramA = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsA);

            AlgorithmHistogram histoAlgoA = new AlgorithmHistogram(histogramA, imageA, entireFlag);

            histoAlgoA.setRunningInSeparateThread(false);
            histoAlgoA.run();
        }

        if ((imageB != null) && (imageAorB == IMAGE_B)) {

            dimExtentsB[0] = 256;
            histogramB = new ModelHistogram(ModelStorageBase.INTEGER, dimExtentsB);

            AlgorithmHistogram histoAlgoB = new AlgorithmHistogram(histogramB, imageB, entireFlag);

            histoAlgoB.setRunningInSeparateThread(false);
            histoAlgoB.run();
        }
    }

    /**
     * Set the gray scale image LUT tables.
     *
     * @param  _LUTa  ModelLUTA
     * @param  _LUTb  ModelLUTB
     */
    private void setLUTs(ModelLUT _LUTa, ModelLUT _LUTb) {
        LUTa = _LUTa;
        LUTb = _LUTb;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Wrapper in order to hold the control panel layout in the JScrollPane.
     */
    class DrawingPanel extends JPanel {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -7791172507789867643L;

        /**
         * Wrapper to repaint the panel.
         *
         * @param  g  graphics reference.
         */
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);

        }
    }

    /********* end HistoLUTParent *********/

}
