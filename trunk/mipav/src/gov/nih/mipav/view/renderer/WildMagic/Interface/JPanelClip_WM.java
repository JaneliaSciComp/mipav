package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJColorChooser;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.util.Hashtable;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import WildMagic.LibFoundation.Mathematics.ColorRGB;


/**
 */
public class JPanelClip_WM extends JInterfaceBase
        implements ChangeListener {

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -3342110506544352170L;

    /** Which arbitray clipping slice is currently displayed. */
    private int aSlice;

    /** Arbitrary and static clipping plane check box. */
    private JCheckBox boundingCheckA, boundingCheckStatic, boundingCheckStaticInv;

    /** Check box to turn the clipping plane frame on and off. */
    private JCheckBox boundingCheckX, boundingCheckY, boundingCheckZ;

    /** X,Y,Z inverse clipping plane check box. */
    private JCheckBox boundingCheckXInv, boundingCheckYInv, boundingCheckZInv;

    /** Static and static inverse, arbitrary clipping plane check box. */
    private JCheckBox boxStatic, boxStaticInv, boxA;

    /** Check boxes that turn the image plane and the sliders on and off. */
    private JCheckBox boxX, boxY, boxZ, boxXInv, boxYInv, boxZInv;

    /** Static, static inverse and arbitrary clipping plane boundary box. */
    private JSlider clipSliderStatic, clipSliderStaticInv, sliderA;

    /** Sliders for the image planes. */
    private JSlider clipSliderX, clipSliderY, clipSliderZ, sliderXInv, sliderYInv, sliderZInv;

    /** Color button for the arbitrary clipping plane frame. */
    private JButton colorButtonA;

    /** Color button for the static clipping plane frame. */
    private JButton colorButtonStatic, colorButtonStaticInv;

    /** Color button for X clipping plane frame. */
    private JButton colorButtonX, colorButtonXInv;

    /** Color button for Y clipping plane frame. */
    private JButton colorButtonY, colorButtonYInv;

    /** Color button for Z clipping plane frame. */
    private JButton colorButtonZ, colorButtonZInv;

    /** Extract arbitrary clipping plane button. */
    private JButton extractButtonA;

    /** Extract static eye clipping plane button. */
    private JButton extractButtonS;

    /** Arbitrary clipping slider labels. */
    private JLabel labelAStart, labelAMid, labelAEnd;

    /** Static clipping slider labels. */
    private JLabel labelStatic, labelStaticInv, labelA;

    /** Static inverse clipping plane labels. */
    private JLabel labelStaticInvStart, labelStaticInvMid, labelStaticInvEnd;

    /** Static clipping plane labels. */
    private JLabel labelStaticStart, labelStaticMid, labelStaticEnd;

    /** Sliders labels. */
    private JLabel labelX, labelY, labelZ, labelXInv, labelYInv, labelZInv;

    /** Slider tick labels. */
    private JLabel labelXStart, labelXMid, labelXEnd;

    /** X clipping slider labels. */
    private JLabel labelXStartInv, labelXMidInv, labelXEndInv;

    /** Y clipping slider labels. */
    private JLabel labelYStart, labelYMid, labelYEnd;

    /** Y inverse clipping slider labels. */
    private JLabel labelYStartInv, labelYMidInv, labelYEndInv;

    /** Z clipping slider labels. */
    private JLabel labelZStart, labelZMid, labelZEnd;

    /** Z clipping slider labels. */
    private JLabel labelZStartInv, labelZMidInv, labelZEndInv;

    /** Arbitrary and static control panels. */
    private JPanel panelA, panelS, panelSInv;

    /** Declare each clipping plane control panel. */
    private JPanel panelX, panelXInv, panelY, panelYInv, panelZ, panelZInv;

    /** Arbitrary clipping plane expanding ratio. */
    private float radicalRatio = (float) Math.sqrt(2);

    /** The scroll pane holding the panel content. Used when the screen is small. */
    private JScrollPane scroller;

    /** Scroll panel that holding all the control components. */
    private JPanel scrollPanel;

    /** Record the current static clipping slice number. */
    private int sSlice, sSliceInv;

    /** Tabbed Panel that hold the each clipping planes control box. */
    private JTabbedPane tabbedPane;

    /** Static and arbitrary text field. */
    private JTextField textStatic, textStaticInv, textA;

    /** Text fields that display the slice number when slider moves. */
    private JTextField textX, textY, textZ;

    /** X, Y, Z inverse text field. */
    private JTextField textXInv, textYInv, textZInv;

    /** Image X, Y, Z dimension. */
    private int xDim, yDim, zDim;

    /** Which slice is currently displayed in the ZY plane. */
    private int xSlice, xSliceInv;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /** Which slice is currently displayed in the XZ plane. */
    private int ySlice, ySliceInv;

    //~ Methods --------------------------------------------------------------------------------------------------------

    /** Which slice is currently displayed in the XY plane. */
    private int zSlice, zSliceInv;

    /**
     * 3D clipping dialog control.
     * @param kVolumeViewer parent frame.
     */
    public JPanelClip_WM(VolumeTriPlanarInterface kVolumeViewer) {
        super(kVolumeViewer);
        xDim = kVolumeViewer.getImageA().getExtents()[0];
        yDim = kVolumeViewer.getImageA().getExtents()[1];
        zDim = kVolumeViewer.getImageA().getExtents()[2];
        xSlice = (xDim - 1) / 2;
        ySlice = (yDim - 1) / 2;
        zSlice = (zDim - 1) / 2;

        // Build dialog.
        init();
    }

    /**
     * Changes color of slices box frame and button if color button was pressed; turns bounding box on and off if
     * checkbox was pressed.
     *
     * @param  event  Event that triggered function.
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String command = event.getActionCommand();

        if ((source == extractButtonA) || (source == extractButtonS)) {
            // extract arbitrary clipping plane.
/*
            Vector3f[] pts;

            pts = getAClipPlanePts();

            ModelImage img = renderBase.getImageA();
            float[] plane = img.getPlane(pts[0], pts[1], pts[2], pts[3]);

            int length = (int)
                             Math.round(Math.sqrt(((pts[2].x - pts[0].x) * (pts[2].x - pts[0].x)) +
                                                  ((pts[2].y - pts[0].y) * (pts[2].y - pts[0].y)) +
                                                  ((pts[2].z - pts[0].z) * (pts[2].z - pts[0].z))));
            int width = (int)
                            Math.round(Math.sqrt(((pts[1].x - pts[0].x) * (pts[1].x - pts[0].x)) +
                                                 ((pts[1].y - pts[0].y) * (pts[1].y - pts[0].y)) +
                                                 ((pts[1].z - pts[0].z) * (pts[1].z - pts[0].z))));

            int[] ext = new int[2];

            ext[0] = width;
            ext[1] = length;

            ModelImage resultImage = new ModelImage(renderBase.getImageA().getType(), ext, "Image plane");

            try {
                resultImage.importData(0, plane, true);
            } catch (IOException er) {
                return;
            }

            new ViewJFrameImage(resultImage, null, new Dimension(610, 200));
            plane = null;
            */
        } else if (source instanceof JButton) {
            colorChooser = new ViewJColorChooser(new Frame(), "Pick color", new OkColorListener((JButton) source),
                                                 new CancelListener());
        } else if (source == boundingCheckX) {

            if (boundingCheckX.isSelected()) {
                colorButtonX.setEnabled(true);
                if ( rayBasedRenderWM != null )
                {
                    rayBasedRenderWM.displayClipPlane( 1, true, 
                            new ColorRGB( colorButtonX.getBackground().getRed(),
                                          colorButtonX.getBackground().getGreen(),
                                          colorButtonX.getBackground().getBlue() ) );
                }
            } else {
                colorButtonX.setEnabled(false);
                if ( rayBasedRenderWM != null )
                {
                    rayBasedRenderWM.displayClipPlane( 1, false );
                }
            }
        } else if (source == boundingCheckXInv) {

            if (boundingCheckXInv.isSelected()) {
                colorButtonXInv.setEnabled(true);
                if ( rayBasedRenderWM != null )
                {
                    rayBasedRenderWM.displayClipPlane( 0, true, 
                            new ColorRGB( colorButtonXInv.getBackground().getRed(),
                                          colorButtonXInv.getBackground().getGreen(),
                                          colorButtonXInv.getBackground().getBlue() ) );
                }

            } else {
                colorButtonXInv.setEnabled(false);
                if ( rayBasedRenderWM != null )
                {
                    rayBasedRenderWM.displayClipPlane( 0, false );
                }
            }
        } else if (source == boundingCheckY) {

            if (boundingCheckY.isSelected()) {
                colorButtonY.setEnabled(true);
                if ( rayBasedRenderWM != null )
                {
                    rayBasedRenderWM.displayClipPlane( 3, true, 
                            new ColorRGB( colorButtonY.getBackground().getRed(),
                                          colorButtonY.getBackground().getGreen(),
                                          colorButtonY.getBackground().getBlue() ) );
                }
            } else {
                colorButtonY.setEnabled(false);
                if ( rayBasedRenderWM != null )
                {
                    rayBasedRenderWM.displayClipPlane( 3, false );
                }
            }
        } else if (source == boundingCheckYInv) {

            if (boundingCheckYInv.isSelected()) {
                colorButtonYInv.setEnabled(true);
                if ( rayBasedRenderWM != null )
                {
                    rayBasedRenderWM.displayClipPlane( 2, true, 
                            new ColorRGB( colorButtonYInv.getBackground().getRed(),
                                          colorButtonYInv.getBackground().getGreen(),
                                          colorButtonYInv.getBackground().getBlue() ) );
                }
            } else {
                colorButtonYInv.setEnabled(false);
                if ( rayBasedRenderWM != null )
                {
                    rayBasedRenderWM.displayClipPlane( 2, false );
                }
            }
        } else if (source == boundingCheckZ) {

            if (boundingCheckZ.isSelected()) {
                colorButtonZ.setEnabled(true);
                if ( rayBasedRenderWM != null )
                {
                    rayBasedRenderWM.displayClipPlane( 5, true, 
                            new ColorRGB( colorButtonZ.getBackground().getRed(),
                                          colorButtonZ.getBackground().getGreen(),
                                          colorButtonZ.getBackground().getBlue() ) );
                }
            } else {
                colorButtonZ.setEnabled(false);
                if ( rayBasedRenderWM != null )
                {
                    rayBasedRenderWM.displayClipPlane( 5, false );
                }
            }
        } else if (source == boundingCheckZInv) {

            if (boundingCheckZInv.isSelected()) {
                colorButtonZInv.setEnabled(true);
                if ( rayBasedRenderWM != null )
                {
                    rayBasedRenderWM.displayClipPlane( 4, true, 
                            new ColorRGB( colorButtonZInv.getBackground().getRed(),
                                          colorButtonZInv.getBackground().getGreen(),
                                          colorButtonZInv.getBackground().getBlue() ) );
                }
            } else {
                colorButtonZInv.setEnabled(false);
                if ( rayBasedRenderWM != null )
                {
                    rayBasedRenderWM.displayClipPlane( 4, false );
                }
            }
        } else if (source == boundingCheckA) {

            if (boundingCheckA.isSelected()) {
                colorButtonA.setEnabled(true);
            } else {
                colorButtonA.setEnabled(false);
            }
            if ( rayBasedRenderWM != null )
            {
                aSlice = sliderA.getValue() - 1;
                rayBasedRenderWM.setArbitraryClipPlane(aSlice, boxA.isSelected());
                rayBasedRenderWM.enableArbitraryClipPlane( boxA.isSelected(), boundingCheckA.isSelected(),
                                                           new ColorRGB( colorButtonA.getBackground().getRed(),
                                                                         colorButtonA.getBackground().getGreen(),
                                                                         colorButtonA.getBackground().getBlue() ) );
            }
       } else if (source == boundingCheckStatic) {

            if (boundingCheckStatic.isSelected()) {
                colorButtonStatic.setEnabled(true);
            } else {
                colorButtonStatic.setEnabled(false);
            }
            if ( rayBasedRenderWM != null )
            {
                sSlice = clipSliderStatic.getValue() - 1;
                rayBasedRenderWM.setEyeClipPlane(sSlice, boundingCheckStatic.isSelected(), boxStatic.isSelected());
                rayBasedRenderWM.enableEyeClipPlane( boxStatic.isSelected(), boundingCheckStatic.isSelected(),
                            new ColorRGB( colorButtonStatic.getBackground().getRed(),
                                          colorButtonStatic.getBackground().getGreen(),
                                          colorButtonStatic.getBackground().getBlue() ) );
            }
       } else if (source == boundingCheckStaticInv) {

            if (boundingCheckStaticInv.isSelected()) {
                colorButtonStaticInv.setEnabled(true);
            } else {
                colorButtonStaticInv.setEnabled(false);
            }
            if ( rayBasedRenderWM != null )
            {
                sSliceInv = clipSliderStaticInv.getValue() - 1;
                rayBasedRenderWM.setEyeInvClipPlane(sSliceInv, boundingCheckStaticInv.isSelected(), boxStaticInv.isSelected());
                rayBasedRenderWM.enableEyeInvClipPlane( boxStaticInv.isSelected(), boundingCheckStaticInv.isSelected(),
                            new ColorRGB( colorButtonStaticInv.getBackground().getRed(),
                                          colorButtonStaticInv.getBackground().getGreen(),
                                          colorButtonStaticInv.getBackground().getBlue() ) );
            }
        } else if (command.equals("+X")) {

            if (!boxX.isSelected()) {
                setXSliderEnabled(false);
                boundingCheckX.setSelected(false);
                colorButtonX.setEnabled(false);
            } else {
                setXSliderEnabled(true);
                disableClipPlanesArbi();
            }
            if ( rayBasedRenderWM != null )
            {
                rayBasedRenderWM.enableClipPlane( 1, boxX.isSelected(), boundingCheckX.isSelected() );
            }
        } else if (command.equals("+Y")) {
            if (!boxY.isSelected()) {
                setYSliderEnabled(false);
                colorButtonY.setEnabled(false);
                boundingCheckY.setSelected(false);
            } else {
                setYSliderEnabled(true);
                disableClipPlanesArbi();
            }
            if ( rayBasedRenderWM != null )
            {
                rayBasedRenderWM.enableClipPlane( 3, boxY.isSelected(), boundingCheckY.isSelected() );
            }
        } else if (command.equals("+Z")) {
            if (!boxZ.isSelected()) {
                setZSliderEnabled(false);
                colorButtonZ.setEnabled(false);
                boundingCheckZ.setSelected(false);
            } else {
                setZSliderEnabled(true);
                disableClipPlanesArbi();
            }
            if ( rayBasedRenderWM != null )
            {
                rayBasedRenderWM.enableClipPlane( 5, boxZ.isSelected(), boundingCheckZ.isSelected() );
            }
        } else if (command.equals("-X")) {
            if (!boxXInv.isSelected()) {
                setXSliderInvEnabled(false);
                colorButtonXInv.setEnabled(false);
                boundingCheckXInv.setSelected(false);
            } else {
                setXSliderInvEnabled(true);
                disableClipPlanesArbi();
            }
            if ( rayBasedRenderWM != null )
            {
                rayBasedRenderWM.enableClipPlane( 0, boxXInv.isSelected(), boundingCheckXInv.isSelected() );
            }
        } else if (command.equals("-Y")) {
            if (!boxYInv.isSelected()) {
                setYSliderInvEnabled(false);
                colorButtonYInv.setEnabled(false);
                boundingCheckYInv.setSelected(false);
            } else {
                setYSliderInvEnabled(true);
                disableClipPlanesArbi();
            }
            if ( rayBasedRenderWM != null )
            {
                rayBasedRenderWM.enableClipPlane( 2, boxYInv.isSelected(), boundingCheckYInv.isSelected() );
            }
        } else if (command.equals("-Z")) {
            if (!boxZInv.isSelected()) {
                setZSliderInvEnabled(false);
                colorButtonZInv.setEnabled(false);
                boundingCheckZInv.setSelected(false);
            } else {
                setZSliderInvEnabled(true);
                disableClipPlanesArbi();
            }
            if ( rayBasedRenderWM != null )
            {
                rayBasedRenderWM.enableClipPlane( 4, boxZInv.isSelected(), boundingCheckZInv.isSelected() );
            }
        } else if (command.equals("A")) {

            if (!boxA.isSelected()) {
                setASliderEnabled(false);
                colorButtonA.setEnabled(false);
                boundingCheckA.setSelected(false);
                disableClipPlanesArbi();
            } else {
                setASliderEnabled(true);
                disableClipPlanes();
                colorButtonA.setEnabled(true);
            }
            if ( rayBasedRenderWM != null )
            {
                aSlice = sliderA.getValue() - 1;
                rayBasedRenderWM.setArbitraryClipPlane(aSlice, boxA.isSelected());
                rayBasedRenderWM.enableArbitraryClipPlane( boxA.isSelected(), boundingCheckA.isSelected(),
                                                           new ColorRGB( colorButtonA.getBackground().getRed(),
                                                                         colorButtonA.getBackground().getGreen(),
                                                                         colorButtonA.getBackground().getBlue() ) );
            }
       } else if (command.equals("EYE")) {

            if (!boxStatic.isSelected()) {
                setStaticSliderEnabled(false);
            } else {
                disableClipPlanes();
                disableClipPlanesArbi();
                setStaticSliderEnabled(true);
            }
            if ( rayBasedRenderWM != null )
            {
                sSlice = clipSliderStatic.getValue() - 1;
                rayBasedRenderWM.setEyeClipPlane(sSlice, boundingCheckStatic.isSelected(), boxStatic.isSelected());
                rayBasedRenderWM.enableEyeClipPlane( boxStatic.isSelected(), boundingCheckStatic.isSelected(),
                            new ColorRGB( colorButtonStatic.getBackground().getRed(),
                                          colorButtonStatic.getBackground().getGreen(),
                                          colorButtonStatic.getBackground().getBlue() ) );
            }
       } else if (command.equals("-EYE")) {

            if (!boxStaticInv.isSelected()) {
                setStaticInvSliderEnabled(false);
            } else {
                disableClipPlanes();
                disableClipPlanesArbi();
                setStaticInvSliderEnabled(true);
            }
            if ( rayBasedRenderWM != null )
            {
                sSliceInv = clipSliderStaticInv.getValue() - 1;
                rayBasedRenderWM.setEyeInvClipPlane(sSliceInv, boundingCheckStaticInv.isSelected(), boxStaticInv.isSelected());
                rayBasedRenderWM.enableEyeInvClipPlane( boxStaticInv.isSelected(), boundingCheckStaticInv.isSelected(),
                            new ColorRGB( colorButtonStaticInv.getBackground().getRed(),
                                          colorButtonStaticInv.getBackground().getGreen(),
                                          colorButtonStaticInv.getBackground().getBlue() ) );
            }
        }

    }

    /**
     * Build the arbitrary clipping slider control panel.
     */
    public void buildPanelA() {
        GridBagLayout cpGBL = new GridBagLayout();
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.NONE;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        panelA = new JPanel();
        panelA.setBounds(10, 100, 100, 120);
        panelA.setLayout(cpGBL);

        boxA = new JCheckBox();
        boxA.setSelected(false);
        boxA.addActionListener(this);
        boxA.setActionCommand("A");
        boxA.setText("A Slider");
        boxA.setFont(MipavUtil.font12);
        addControlPanel(panelA, boxA, cpGBC, 0, 0, 1, 1);

        cpGBC.fill = GridBagConstraints.BOTH;
        labelA = new JLabel(" A (" + String.valueOf(-xDim) + " - " + String.valueOf((int) (xDim * radicalRatio)) + ")");
        labelA.setForeground(Color.black);
        labelA.setFont(MipavUtil.font12);
        labelA.setEnabled(false);
        addControlPanel(panelA, labelA, cpGBC, 0, 1, 2, 1);

        sliderA = new JSlider(-xDim, (int) (xDim * radicalRatio), (int) (xDim * radicalRatio));
        sliderA.setFont(MipavUtil.font12);
        sliderA.setEnabled(false);
        sliderA.setMinorTickSpacing(xDim / 10);
        sliderA.setPaintTicks(true);
        sliderA.addChangeListener(this);
        aSlice = sliderA.getValue();
        sliderA.setVisible(true);

        labelAStart = new JLabel(String.valueOf(-xDim));
        labelAStart.setForeground(Color.black);
        labelAStart.setFont(MipavUtil.font12);
        labelAStart.setEnabled(false);
        labelAMid = new JLabel(String.valueOf((int) ((((xDim * radicalRatio) + xDim) / radicalRatio) - xDim)));
        labelAMid.setForeground(Color.black);
        labelAMid.setFont(MipavUtil.font12);
        labelAMid.setEnabled(false);
        labelAEnd = new JLabel(String.valueOf((int) (xDim * radicalRatio)));
        labelAEnd.setForeground(Color.black);
        labelAEnd.setFont(MipavUtil.font12);
        labelAEnd.setEnabled(false);

        Hashtable<Integer,JLabel> labelTableA = new Hashtable<Integer,JLabel>();

        labelTableA.put(new Integer(-xDim), labelAStart);
        labelTableA.put(new Integer((int) ((((xDim * radicalRatio) + xDim) / radicalRatio) - xDim)), labelAMid);
        labelTableA.put(new Integer((int) (xDim * radicalRatio)), labelAEnd);
        sliderA.setLabelTable(labelTableA);
        sliderA.setPaintLabels(true);
        addControlPanel(panelA, sliderA, cpGBC, 2, 1, 8, 1);

        textA = new JTextField(String.valueOf((int) (xDim * radicalRatio)), 4);
        textA.setFont(MipavUtil.font12);
        textA.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(panelA, textA, cpGBC, 10, 1, 1, 1);

        // add bounding box
        boundingCheckA = new JCheckBox("A Frame");
        boundingCheckA.setFont(MipavUtil.font12);
        boundingCheckA.addActionListener(this);
        addControlPanel(panelA, boundingCheckA, cpGBC, 0, 2, 1, 1);

        JPanel buttonPanel = new JPanel();

        colorButtonA = new JButton();
        colorButtonA.setPreferredSize(new Dimension(25, 25));
        colorButtonA.setToolTipText("Change arbitrary clipping plane frame color");
        colorButtonA.addActionListener(this);
        colorButtonA.setBackground(new Color(0.73f, 0.70f, 0.86f)); // light blue
        colorButtonA.setEnabled(false);

        extractButtonA = new JButton(MipavUtil.getIcon("extract.gif"));
        extractButtonA.addActionListener(this);
        extractButtonA.setToolTipText("Extract Slice");
        extractButtonA.setActionCommand("extract");
        extractButtonA.setBorderPainted(false);
        extractButtonA.setRolloverEnabled(true);
        extractButtonA.setRolloverIcon(MipavUtil.getIcon("extractroll.gif"));
        extractButtonA.setFocusPainted(false);
        extractButtonA.setEnabled(true);

        buttonPanel.add(colorButtonA);
        buttonPanel.add(extractButtonA);

        addControlPanel(panelA, buttonPanel, cpGBC, 1, 2, 2, 1);
        tabbedPane.addTab("A", null, panelA);
    }

    /**
     * Build static clipping slider control panel.
     */
    public void buildPanelS() {
        GridBagLayout cpGBL = new GridBagLayout();
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.NONE;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        panelS = new JPanel();
        panelS.setBounds(10, 100, 100, 120);
        panelS.setLayout(cpGBL);

        // Build static clipping plane
        boxStatic = new JCheckBox();
        boxStatic.setSelected(false);
        boxStatic.addActionListener(this);
        boxStatic.setActionCommand("EYE");
        boxStatic.setText("Eye Slider");
        boxStatic.setFont(MipavUtil.font12);
        addControlPanel(panelS, boxStatic, cpGBC, 0, 0, 1, 1);

        cpGBC.fill = GridBagConstraints.BOTH;
        labelStatic = new JLabel(" EYE (1 - " + String.valueOf(zDim) + ")");
        labelStatic.setForeground(Color.black);
        labelStatic.setFont(MipavUtil.font12);
        labelStatic.setEnabled(false);
        addControlPanel(panelS, labelStatic, cpGBC, 0, 1, 2, 1);

        clipSliderStatic = new JSlider(1, zDim, 1);
        clipSliderStatic.setFont(MipavUtil.font12);
        clipSliderStatic.setEnabled(false);
        clipSliderStatic.setMinorTickSpacing(zDim / 10);
        clipSliderStatic.setPaintTicks(true);
        clipSliderStatic.addChangeListener(this);
        sSlice = clipSliderStatic.getValue();
        clipSliderStatic.setVisible(true);
        clipSliderStatic.setEnabled(false);

        labelStaticStart = new JLabel("1");
        labelStaticStart.setForeground(Color.black);
        labelStaticStart.setFont(MipavUtil.font12);
        labelStaticStart.setEnabled(false);
        labelStaticMid = new JLabel(String.valueOf((zDim + 1) / 2));
        labelStaticMid.setForeground(Color.black);
        labelStaticMid.setFont(MipavUtil.font12);
        labelStaticMid.setEnabled(false);
        labelStaticEnd = new JLabel(String.valueOf(zDim));
        labelStaticEnd.setForeground(Color.black);
        labelStaticEnd.setFont(MipavUtil.font12);
        labelStaticEnd.setEnabled(false);

        Hashtable<Integer,JLabel> labelTableStatic = new Hashtable<Integer,JLabel>();

        labelTableStatic.put(new Integer(1), labelStaticStart);
        labelTableStatic.put(new Integer((zDim + 1) / 2), labelStaticMid);
        labelTableStatic.put(new Integer(zDim), labelStaticEnd);
        clipSliderStatic.setLabelTable(labelTableStatic);
        clipSliderStatic.setPaintLabels(true);
        addControlPanel(panelS, clipSliderStatic, cpGBC, 2, 1, 8, 1);

        textStatic = new JTextField(String.valueOf(1), 4);
        textStatic.setFont(MipavUtil.font12);
        textStatic.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(panelS, textStatic, cpGBC, 10, 1, 1, 1);

        // add bounding box
        boundingCheckStatic = new JCheckBox("Eye Frame");
        boundingCheckStatic.setFont(MipavUtil.font12);
        boundingCheckStatic.addActionListener(this);
        addControlPanel(panelS, boundingCheckStatic, cpGBC, 0, 2, 1, 1);

        JPanel buttonPanel = new JPanel();

        colorButtonStatic = new JButton();
        colorButtonStatic.setPreferredSize(new Dimension(25, 25));
        colorButtonStatic.setToolTipText("Change eye clipping plane frame color");
        colorButtonStatic.addActionListener(this);
        colorButtonStatic.setBackground(Color.orange);
        colorButtonStatic.setEnabled(false);

        extractButtonS = new JButton(MipavUtil.getIcon("extract.gif"));
        extractButtonS.addActionListener(this);
        extractButtonS.setToolTipText("Extract Slice");
        extractButtonS.setActionCommand("extract");
        extractButtonS.setBorderPainted(false);
        extractButtonS.setRolloverEnabled(true);
        extractButtonS.setRolloverIcon(MipavUtil.getIcon("extractroll.gif"));
        extractButtonS.setFocusPainted(false);
        extractButtonS.setEnabled(true);

        buttonPanel.add(colorButtonStatic);
        buttonPanel.add(extractButtonS);

        addControlPanel(panelS, buttonPanel, cpGBC, 1, 2, 2, 1);

        tabbedPane.addTab("EYE", null, panelS);
    }

    /**
     * Build static inverse clipping slider control panel.
     */
    public void buildPanelSInv() {
        GridBagLayout cpGBL = new GridBagLayout();
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.NONE;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        panelSInv = new JPanel();
        panelSInv.setBounds(10, 100, 100, 120);
        panelSInv.setLayout(cpGBL);

        // Build static clipping plane
        boxStaticInv = new JCheckBox();
        boxStaticInv.setSelected(false);
        boxStaticInv.addActionListener(this);
        boxStaticInv.setActionCommand("-EYE");
        boxStaticInv.setText("Eye Slider");
        boxStaticInv.setFont(MipavUtil.font12);
        addControlPanel(panelSInv, boxStaticInv, cpGBC, 0, 0, 1, 1);

        cpGBC.fill = GridBagConstraints.BOTH;
        labelStaticInv = new JLabel(" -EYE (1 - " + String.valueOf(zDim) + ")");
        labelStaticInv.setForeground(Color.black);
        labelStaticInv.setFont(MipavUtil.font12);
        labelStaticInv.setEnabled(false);
        addControlPanel(panelSInv, labelStaticInv, cpGBC, 0, 1, 2, 1);

        clipSliderStaticInv = new JSlider(1, zDim, zDim);
        clipSliderStaticInv.setFont(MipavUtil.font12);
        clipSliderStaticInv.setEnabled(false);
        clipSliderStaticInv.setMinorTickSpacing(zDim / 10);
        clipSliderStaticInv.setPaintTicks(true);
        clipSliderStaticInv.addChangeListener(this);
        sSliceInv = clipSliderStaticInv.getValue();
        clipSliderStaticInv.setVisible(true);
        clipSliderStaticInv.setEnabled(false);

        labelStaticInvStart = new JLabel("1");
        labelStaticInvStart.setForeground(Color.black);
        labelStaticInvStart.setFont(MipavUtil.font12);
        labelStaticInvStart.setEnabled(false);
        labelStaticInvMid = new JLabel(String.valueOf((zDim + 1) / 2));
        labelStaticInvMid.setForeground(Color.black);
        labelStaticInvMid.setFont(MipavUtil.font12);
        labelStaticInvMid.setEnabled(false);
        labelStaticInvEnd = new JLabel(String.valueOf(zDim));
        labelStaticInvEnd.setForeground(Color.black);
        labelStaticInvEnd.setFont(MipavUtil.font12);
        labelStaticInvEnd.setEnabled(false);

        Hashtable<Integer,JLabel> labelTableStatic = new Hashtable<Integer,JLabel>();

        labelTableStatic.put(new Integer(1), labelStaticStart);
        labelTableStatic.put(new Integer((zDim + 1) / 2), labelStaticMid);
        labelTableStatic.put(new Integer(zDim), labelStaticEnd);
        clipSliderStaticInv.setLabelTable(labelTableStatic);
        clipSliderStaticInv.setPaintLabels(true);
        addControlPanel(panelSInv, clipSliderStaticInv, cpGBC, 2, 1, 8, 1);

        textStaticInv = new JTextField(String.valueOf(1), 4);
        textStaticInv.setFont(MipavUtil.font12);
        textStaticInv.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(panelSInv, textStaticInv, cpGBC, 10, 1, 1, 1);

        // add bounding box
        boundingCheckStaticInv = new JCheckBox("Eye Frame");
        boundingCheckStaticInv.setFont(MipavUtil.font12);
        boundingCheckStaticInv.addActionListener(this);
        addControlPanel(panelSInv, boundingCheckStaticInv, cpGBC, 0, 2, 1, 1);

        JPanel buttonPanel = new JPanel();

        colorButtonStaticInv = new JButton();
        colorButtonStaticInv.setPreferredSize(new Dimension(25, 25));
        colorButtonStaticInv.setToolTipText("Change eye clipping plane frame color");
        colorButtonStaticInv.addActionListener(this);
        colorButtonStaticInv.setBackground(new Color(1.0f, 0.64f, 1.0f));
        colorButtonStaticInv.setEnabled(false);

        buttonPanel.add(colorButtonStaticInv);

        addControlPanel(panelSInv, buttonPanel, cpGBC, 1, 2, 2, 1);

        tabbedPane.addTab("-EYE", null, panelSInv);
    }

    /**
     * Build x slider control panel.
     */
    public void buildPanelX() {
        GridBagLayout cpGBL = new GridBagLayout();
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.NONE;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        panelX = new JPanel();
        panelX.setBounds(10, 100, 100, 120);
        panelX.setLayout(cpGBL);

        boxX = new JCheckBox();
        boxX.addActionListener(this);
        boxX.setActionCommand("+X");
        boxX.setText("X Slider");
        boxX.setFont(MipavUtil.font12);
        addControlPanel(panelX, boxX, cpGBC, 0, 0, 1, 1);
        boxX.setSelected(false);

        cpGBC.fill = GridBagConstraints.BOTH;
        labelX = new JLabel(" +X (1 - " + String.valueOf(xDim) + ")");
        labelX.setForeground(Color.black);
        labelX.setFont(MipavUtil.font12);
        addControlPanel(panelX, labelX, cpGBC, 0, 1, 2, 1);
        labelX.setEnabled(false);

        clipSliderX = new JSlider(1, xDim, xDim);
        clipSliderX.setFont(MipavUtil.font12);
        clipSliderX.setMinorTickSpacing(xDim / 10);
        clipSliderX.setPaintTicks(true);
        clipSliderX.addChangeListener(this);
        xSlice = clipSliderX.getValue() - 1;
        clipSliderX.setVisible(true);
        clipSliderX.setEnabled(false);

        labelXStart = new JLabel("1");
        labelXStart.setForeground(Color.black);
        labelXStart.setFont(MipavUtil.font12);
        labelXStart.setEnabled(false);
        labelXMid = new JLabel(String.valueOf((xDim + 1) / 2));
        labelXMid.setForeground(Color.black);
        labelXMid.setFont(MipavUtil.font12);
        labelXMid.setEnabled(false);
        labelXEnd = new JLabel(String.valueOf(xDim));
        labelXEnd.setForeground(Color.black);
        labelXEnd.setFont(MipavUtil.font12);
        labelXEnd.setEnabled(false);

        Hashtable<Integer, JLabel> labelTableX = new Hashtable<Integer, JLabel>();

        labelTableX.put(new Integer(1), labelXStart);
        labelTableX.put(new Integer((xDim + 1) / 2), labelXMid);
        labelTableX.put(new Integer(xDim), labelXEnd);
        clipSliderX.setLabelTable(labelTableX);
        clipSliderX.setPaintLabels(true);
        addControlPanel(panelX, clipSliderX, cpGBC, 2, 1, 8, 1);

        textX = new JTextField(String.valueOf(xDim), 4);
        textX.setFont(MipavUtil.font12);
        textX.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(panelX, textX, cpGBC, 10, 1, 1, 1);

        // add bounding box
        boundingCheckX = new JCheckBox("X Frame");
        boundingCheckX.setFont(MipavUtil.font12);
        boundingCheckX.addActionListener(this);
        addControlPanel(panelX, boundingCheckX, cpGBC, 0, 2, 1, 1);

        colorButtonX = new JButton();
        colorButtonX.setPreferredSize(new Dimension(25, 25));
        colorButtonX.setToolTipText("Change +x clip frame color");
        colorButtonX.addActionListener(this);
        colorButtonX.setBackground(Color.yellow);
        colorButtonX.setEnabled(false);
        addControlPanel(panelX, colorButtonX, cpGBC, 1, 2, 2, 1);
        tabbedPane.addTab("X", null, panelX);
    }

    /**
     * Build x negative clipping slider control panel.
     */
    public void buildPanelXInv() {
        GridBagLayout cpGBL = new GridBagLayout();
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.NONE;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        panelXInv = new JPanel();
        panelXInv.setBounds(10, 100, 100, 120);
        panelXInv.setLayout(cpGBL);

        boxXInv = new JCheckBox();
        boxXInv.setSelected(false);
        boxXInv.addActionListener(this);
        boxXInv.setActionCommand("-X");
        boxXInv.setText("-X Slider");
        boxXInv.setFont(MipavUtil.font12);
        addControlPanel(panelXInv, boxXInv, cpGBC, 0, 0, 1, 1);

        cpGBC.fill = GridBagConstraints.BOTH;
        labelXInv = new JLabel(" -X (1 - " + String.valueOf(xDim) + ")");
        labelXInv.setForeground(Color.black);
        labelXInv.setFont(MipavUtil.font12);
        labelXInv.setEnabled(false);
        addControlPanel(panelXInv, labelXInv, cpGBC, 0, 1, 2, 1);

        sliderXInv = new JSlider(1, xDim, 1);
        sliderXInv.setFont(MipavUtil.font12);
        sliderXInv.setEnabled(false);
        sliderXInv.setMinorTickSpacing(xDim / 10);
        sliderXInv.setPaintTicks(true);
        sliderXInv.addChangeListener(this);
        xSliceInv = sliderXInv.getValue() - 1;
        sliderXInv.setVisible(true);
        sliderXInv.setEnabled(false);

        labelXStartInv = new JLabel("1");
        labelXStartInv.setForeground(Color.black);
        labelXStartInv.setFont(MipavUtil.font12);
        labelXStartInv.setEnabled(false);
        labelXMidInv = new JLabel(String.valueOf((xDim + 1) / 2));
        labelXMidInv.setForeground(Color.black);
        labelXMidInv.setFont(MipavUtil.font12);
        labelXMidInv.setEnabled(false);
        labelXEndInv = new JLabel(String.valueOf(xDim));
        labelXEndInv.setForeground(Color.black);
        labelXEndInv.setFont(MipavUtil.font12);
        labelXEndInv.setEnabled(false);

        Hashtable<Integer,JLabel> labelTableXInv = new Hashtable<Integer,JLabel>();

        labelTableXInv.put(new Integer(1), labelXStartInv);
        labelTableXInv.put(new Integer((xDim + 1) / 2), labelXMidInv);
        labelTableXInv.put(new Integer(xDim), labelXEndInv);
        sliderXInv.setLabelTable(labelTableXInv);
        sliderXInv.setPaintLabels(true);
        addControlPanel(panelXInv, sliderXInv, cpGBC, 1, 1, 8, 1);

        textXInv = new JTextField(String.valueOf(1), 4);
        textXInv.setFont(MipavUtil.font12);
        textXInv.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(panelXInv, textXInv, cpGBC, 10, 1, 1, 1);

        // add bounding box
        boundingCheckXInv = new JCheckBox("-X Frame");
        boundingCheckXInv.setFont(MipavUtil.font12);
        boundingCheckXInv.addActionListener(this);
        addControlPanel(panelXInv, boundingCheckXInv, cpGBC, 0, 2, 1, 1);

        colorButtonXInv = new JButton();
        colorButtonXInv.setPreferredSize(new Dimension(25, 25));
        colorButtonXInv.setToolTipText("Change -x clip frame color");
        colorButtonXInv.addActionListener(this);
        colorButtonXInv.setBackground(Color.yellow);
        colorButtonXInv.setEnabled(false);
        addControlPanel(panelXInv, colorButtonXInv, cpGBC, 1, 2, 2, 1);
        tabbedPane.addTab("-X", null, panelXInv);
    }

    /**
     * Build the y clipping slider control panel.
     */
    public void buildPanelY() {
        GridBagLayout cpGBL = new GridBagLayout();
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.NONE;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        panelY = new JPanel();
        panelY.setBounds(10, 100, 100, 120);
        panelY.setLayout(cpGBL);

        boxY = new JCheckBox();
        boxY.setSelected(false);
        boxY.addActionListener(this);
        boxY.setActionCommand("+Y");
        boxY.setText("Y Slider");
        boxY.setFont(MipavUtil.font12);
        addControlPanel(panelY, boxY, cpGBC, 0, 0, 1, 1);

        cpGBC.fill = GridBagConstraints.BOTH;
        labelY = new JLabel(" +Y (1 - " + String.valueOf(yDim) + ")");
        labelY.setForeground(Color.black);
        labelY.setFont(MipavUtil.font12);
        labelY.setEnabled(false);
        addControlPanel(panelY, labelY, cpGBC, 0, 1, 2, 1);

        clipSliderY = new JSlider(1, yDim, yDim);
        clipSliderY.setFont(MipavUtil.font12);
        clipSliderY.setEnabled(false);
        clipSliderY.setMinorTickSpacing(yDim / 10);
        clipSliderY.setPaintTicks(true);
        clipSliderY.addChangeListener(this);
        ySlice = clipSliderY.getValue() - 1;
        clipSliderY.setVisible(true);
        clipSliderY.setEnabled(false);

        labelYStart = new JLabel("1");
        labelYStart.setForeground(Color.black);
        labelYStart.setFont(MipavUtil.font12);
        labelYStart.setEnabled(false);
        labelYMid = new JLabel(String.valueOf((yDim + 1) / 2));
        labelYMid.setForeground(Color.black);
        labelYMid.setFont(MipavUtil.font12);
        labelYMid.setEnabled(false);
        labelYEnd = new JLabel(String.valueOf(yDim));
        labelYEnd.setForeground(Color.black);
        labelYEnd.setFont(MipavUtil.font12);
        labelYEnd.setEnabled(false);

        Hashtable<Integer,JLabel> labelTableY = new Hashtable<Integer,JLabel>();

        labelTableY.put(new Integer(1), labelYStart);
        labelTableY.put(new Integer((yDim + 1) / 2), labelYMid);
        labelTableY.put(new Integer(yDim), labelYEnd);
        clipSliderY.setLabelTable(labelTableY);
        clipSliderY.setPaintLabels(true);
        addControlPanel(panelY, clipSliderY, cpGBC, 2, 1, 8, 1);

        textY = new JTextField(String.valueOf(yDim), 4);
        textY.setFont(MipavUtil.font12);
        textY.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(panelY, textY, cpGBC, 10, 1, 1, 1);

        // add bounding box
        boundingCheckY = new JCheckBox("Y Frame");
        boundingCheckY.setFont(MipavUtil.font12);
        boundingCheckY.addActionListener(this);
        addControlPanel(panelY, boundingCheckY, cpGBC, 0, 2, 1, 1);

        colorButtonY = new JButton();
        colorButtonY.setPreferredSize(new Dimension(25, 25));
        colorButtonY.setToolTipText("Change +y clipping plane frame color");
        colorButtonY.addActionListener(this);
        colorButtonY.setBackground(Color.green);
        colorButtonY.setEnabled(false);
        addControlPanel(panelY, colorButtonY, cpGBC, 1, 2, 2, 1);
        tabbedPane.addTab("Y", null, panelY);
    }

    /**
     * Build the y negative clipping slider control panel.
     */
    public void buildPanelYInv() {
        GridBagLayout cpGBL = new GridBagLayout();
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.NONE;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        panelYInv = new JPanel();
        panelYInv.setBounds(10, 100, 100, 120);
        panelYInv.setLayout(cpGBL);

        boxYInv = new JCheckBox();
        boxYInv.setSelected(false);
        boxYInv.addActionListener(this);
        boxYInv.setActionCommand("-Y");
        boxYInv.setText("-Y Slider");
        boxYInv.setFont(MipavUtil.font12);
        addControlPanel(panelYInv, boxYInv, cpGBC, 0, 0, 1, 1);

        cpGBC.fill = GridBagConstraints.BOTH;
        labelYInv = new JLabel(" -Y (1 - " + String.valueOf(yDim) + ")");
        labelYInv.setForeground(Color.black);
        labelYInv.setFont(MipavUtil.font12);
        labelYInv.setEnabled(false);
        addControlPanel(panelYInv, labelYInv, cpGBC, 0, 1, 2, 1);

        sliderYInv = new JSlider(1, yDim, 1);
        sliderYInv.setFont(MipavUtil.font12);
        sliderYInv.setEnabled(false);
        sliderYInv.setMinorTickSpacing(yDim / 10);
        sliderYInv.setPaintTicks(true);
        sliderYInv.addChangeListener(this);
        ySliceInv = sliderYInv.getValue() - 1;
        sliderYInv.setVisible(true);
        sliderYInv.setEnabled(false);

        labelYStartInv = new JLabel("1");
        labelYStartInv.setForeground(Color.black);
        labelYStartInv.setFont(MipavUtil.font12);
        labelYStartInv.setEnabled(false);
        labelYMidInv = new JLabel(String.valueOf((yDim + 1) / 2));
        labelYMidInv.setForeground(Color.black);
        labelYMidInv.setFont(MipavUtil.font12);
        labelYMidInv.setEnabled(false);
        labelYEndInv = new JLabel(String.valueOf(yDim));
        labelYEndInv.setForeground(Color.black);
        labelYEndInv.setFont(MipavUtil.font12);
        labelYEndInv.setEnabled(false);

        Hashtable<Integer,JLabel> labelTableYInv = new Hashtable<Integer,JLabel>();

        labelTableYInv.put(new Integer(1), labelYStartInv);
        labelTableYInv.put(new Integer((yDim + 1) / 2), labelYMidInv);
        labelTableYInv.put(new Integer(yDim), labelYEndInv);
        sliderYInv.setLabelTable(labelTableYInv);
        sliderYInv.setPaintLabels(true);
        addControlPanel(panelYInv, sliderYInv, cpGBC, 2, 1, 8, 1);

        textYInv = new JTextField(String.valueOf(1), 4);
        textYInv.setFont(MipavUtil.font12);
        textYInv.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(panelYInv, textYInv, cpGBC, 10, 1, 1, 1);

        // add bounnding box
        boundingCheckYInv = new JCheckBox("-Y Frame");
        boundingCheckYInv.setFont(MipavUtil.font12);
        boundingCheckYInv.addActionListener(this);
        addControlPanel(panelYInv, boundingCheckYInv, cpGBC, 0, 2, 1, 1);

        colorButtonYInv = new JButton();
        colorButtonYInv.setPreferredSize(new Dimension(25, 25));
        colorButtonYInv.setToolTipText("Change -y clipping plane frame color");
        colorButtonYInv.addActionListener(this);
        colorButtonYInv.setBackground(Color.green);
        colorButtonYInv.setEnabled(false);
        addControlPanel(panelYInv, colorButtonYInv, cpGBC, 1, 2, 2, 1);
        tabbedPane.addTab("-Y", null, panelYInv);
    }

    /**
     * Build the z clipping slider control panel.
     */
    public void buildPanelZ() {
        GridBagLayout cpGBL = new GridBagLayout();
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.NONE;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        panelZ = new JPanel();
        panelZ.setBounds(10, 100, 100, 120);
        panelZ.setLayout(cpGBL);

        boxZ = new JCheckBox();
        boxZ.setSelected(false);
        boxZ.addActionListener(this);
        boxZ.setActionCommand("+Z");
        boxZ.setText("Z Slider");
        boxZ.setFont(MipavUtil.font12);
        addControlPanel(panelZ, boxZ, cpGBC, 0, 0, 1, 1);

        cpGBC.fill = GridBagConstraints.BOTH;
        labelZ = new JLabel(" +Z (1 - " + String.valueOf(zDim) + ")");
        labelZ.setForeground(Color.black);
        labelZ.setFont(MipavUtil.font12);
        labelZ.setEnabled(false);
        addControlPanel(panelZ, labelZ, cpGBC, 0, 1, 2, 1);

        clipSliderZ = new JSlider(1, zDim, zDim);
        clipSliderZ.setFont(MipavUtil.font12);
        clipSliderZ.setEnabled(false);
        clipSliderZ.setMinorTickSpacing(zDim / 10);
        clipSliderZ.setPaintTicks(true);
        clipSliderZ.addChangeListener(this);
        zSlice = clipSliderZ.getValue() - 1;
        clipSliderZ.setVisible(true);
        clipSliderZ.setEnabled(false);

        labelZStart = new JLabel("1");
        labelZStart.setForeground(Color.black);
        labelZStart.setFont(MipavUtil.font12);
        labelZStart.setEnabled(false);
        labelZMid = new JLabel(String.valueOf((zDim + 1) / 2));
        labelZMid.setForeground(Color.black);
        labelZMid.setFont(MipavUtil.font12);
        labelZMid.setEnabled(false);
        labelZEnd = new JLabel(String.valueOf(zDim));
        labelZEnd.setForeground(Color.black);
        labelZEnd.setFont(MipavUtil.font12);
        labelZEnd.setEnabled(false);

        Hashtable<Integer,JLabel> labelTableZ = new Hashtable<Integer,JLabel>();

        labelTableZ.put(new Integer(1), labelZStart);
        labelTableZ.put(new Integer((zDim + 1) / 2), labelZMid);
        labelTableZ.put(new Integer(zDim), labelZEnd);
        clipSliderZ.setLabelTable(labelTableZ);
        clipSliderZ.setPaintLabels(true);
        addControlPanel(panelZ, clipSliderZ, cpGBC, 2, 1, 8, 1);

        textZ = new JTextField(String.valueOf(zDim), 4);
        textZ.setFont(MipavUtil.font12);
        textZ.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(panelZ, textZ, cpGBC, 10, 1, 1, 1);

        // add bounding box
        boundingCheckZ = new JCheckBox("Z Frame");
        boundingCheckZ.setFont(MipavUtil.font12);
        boundingCheckZ.addActionListener(this);
        addControlPanel(panelZ, boundingCheckZ, cpGBC, 0, 2, 1, 1);

        colorButtonZ = new JButton();
        colorButtonZ.setPreferredSize(new Dimension(25, 25));
        colorButtonZ.setToolTipText("Change +z clipping plane frame color");
        colorButtonZ.addActionListener(this);
        colorButtonZ.setBackground(Color.red);
        colorButtonZ.setEnabled(false);
        addControlPanel(panelZ, colorButtonZ, cpGBC, 1, 2, 2, 1);
        tabbedPane.addTab("Z", null, panelZ);
    }

    /**
     * Build the z negative clipping slider control panel.
     */
    public void buildPanelZInv() {
        GridBagLayout cpGBL = new GridBagLayout();
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.NONE;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        panelZInv = new JPanel();
        panelZInv.setBounds(10, 100, 100, 120);
        panelZInv.setLayout(cpGBL);

        boxZInv = new JCheckBox();
        boxZInv.setSelected(false);
        boxZInv.addActionListener(this);
        boxZInv.setActionCommand("-Z");
        boxZInv.setText("-Z Slider");
        boxZInv.setFont(MipavUtil.font12);
        addControlPanel(panelZInv, boxZInv, cpGBC, 0, 0, 1, 1);

        cpGBC.fill = GridBagConstraints.BOTH;
        labelZInv = new JLabel(" -Z (1 - " + String.valueOf(zDim) + ")");
        labelZInv.setForeground(Color.black);
        labelZInv.setFont(MipavUtil.font12);
        labelZInv.setEnabled(false);
        addControlPanel(panelZInv, labelZInv, cpGBC, 0, 1, 2, 1);

        sliderZInv = new JSlider(1, zDim, 1);
        sliderZInv.setFont(MipavUtil.font12);
        sliderZInv.setEnabled(false);
        sliderZInv.setMinorTickSpacing(zDim / 10);
        sliderZInv.setPaintTicks(true);
        sliderZInv.addChangeListener(this);
        zSliceInv = sliderZInv.getValue() -1;
        sliderZInv.setVisible(true);
        sliderZInv.setEnabled(false);

        labelZStartInv = new JLabel("1");
        labelZStartInv.setForeground(Color.black);
        labelZStartInv.setFont(MipavUtil.font12);
        labelZStartInv.setEnabled(false);
        labelZMidInv = new JLabel(String.valueOf((zDim + 1) / 2));
        labelZMidInv.setForeground(Color.black);
        labelZMidInv.setFont(MipavUtil.font12);
        labelZMidInv.setEnabled(false);
        labelZEndInv = new JLabel(String.valueOf(zDim));
        labelZEndInv.setForeground(Color.black);
        labelZEndInv.setFont(MipavUtil.font12);
        labelZEndInv.setEnabled(false);

        Hashtable<Integer,JLabel> labelTableZInv = new Hashtable<Integer,JLabel>();

        labelTableZInv.put(new Integer(1), labelZStartInv);
        labelTableZInv.put(new Integer((zDim + 1) / 2), labelZMidInv);
        labelTableZInv.put(new Integer(zDim), labelZEndInv);
        sliderZInv.setLabelTable(labelTableZInv);
        sliderZInv.setPaintLabels(true);
        addControlPanel(panelZInv, sliderZInv, cpGBC, 2, 1, 8, 1);

        textZInv = new JTextField(String.valueOf(1), 4);
        textZInv.setFont(MipavUtil.font12);
        textZInv.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(panelZInv, textZInv, cpGBC, 10, 1, 1, 1);

        // add buonding box
        boundingCheckZInv = new JCheckBox("-Z Frame");
        boundingCheckZInv.setFont(MipavUtil.font12);
        boundingCheckZInv.addActionListener(this);
        addControlPanel(panelZInv, boundingCheckZInv, cpGBC, 0, 2, 1, 1);

        colorButtonZInv = new JButton();
        colorButtonZInv.setPreferredSize(new Dimension(25, 25));
        colorButtonZInv.setToolTipText("Change -z clipping plane frame color");
        colorButtonZInv.addActionListener(this);
        colorButtonZInv.setBackground(Color.red);
        colorButtonZInv.setEnabled(false);
        addControlPanel(panelZInv, colorButtonZInv, cpGBC, 1, 2, 2, 1);
        tabbedPane.addTab("-Z", null, panelZInv);
    }

    /**
     * Disable the 6 clipping planes.
     */
    public void disable6Planes() {
        setXSliderEnabled(false);
        boxX.setSelected(false);
        boundingCheckX.setSelected(false);
        colorButtonX.setEnabled(false);

        setYSliderEnabled(false);
        boxY.setSelected(false);
        colorButtonY.setEnabled(false);
        boundingCheckY.setSelected(false);

        setZSliderEnabled(false);
        boxZ.setSelected(false);
        colorButtonZ.setEnabled(false);
        boundingCheckZ.setSelected(false);

        setXSliderInvEnabled(false);
        boxXInv.setSelected(false);
        colorButtonXInv.setEnabled(false);
        boundingCheckXInv.setSelected(false);

        setYSliderInvEnabled(false);
        boxYInv.setSelected(false);
        colorButtonYInv.setEnabled(false);
        boundingCheckYInv.setSelected(false);

        setZSliderInvEnabled(false);
        boxZInv.setSelected(false);
        colorButtonZInv.setEnabled(false);
        boundingCheckZInv.setSelected(false);

        if ( rayBasedRenderWM != null )
        {
            rayBasedRenderWM.enableClipPlane( 0, boxXInv.isSelected(), boundingCheckXInv.isSelected() );
            rayBasedRenderWM.enableClipPlane( 1, boxX.isSelected(), boundingCheckX.isSelected() );
            rayBasedRenderWM.enableClipPlane( 2, boxYInv.isSelected(), boundingCheckYInv.isSelected() );
            rayBasedRenderWM.enableClipPlane( 3, boxY.isSelected(), boundingCheckY.isSelected() );
            rayBasedRenderWM.enableClipPlane( 4, boxZInv.isSelected(), boundingCheckZInv.isSelected() );
            rayBasedRenderWM.enableClipPlane( 5, boxZ.isSelected(), boundingCheckZ.isSelected() );
        }
    }

    /**
     * Disable the arbitrary clipping.
     */
    public void disableClipA() {
        setASliderEnabled(false);
        boxA.setSelected(false);
        boundingCheckA.setSelected(false);
        colorButtonA.setEnabled(false);
    }

    /**
     * Disable clipping planes when dialog window closed.
     */
    public void disableClipPlanes() {

        if (boxX.isSelected() || boundingCheckX.isSelected()) {
            setXSliderEnabled(false);
            boxX.setSelected(false);
            boundingCheckX.setSelected(false);
            colorButtonX.setEnabled(false);
        }

        if (boxY.isSelected() || boundingCheckY.isSelected()) {
            setYSliderEnabled(false);
            boxY.setSelected(false);
            colorButtonY.setEnabled(false);
            boundingCheckY.setSelected(false);
        }

        if (boxZ.isSelected() || boundingCheckZ.isSelected()) {
            setZSliderEnabled(false);
            boxZ.setSelected(false);
            colorButtonZ.setEnabled(false);
            boundingCheckZ.setSelected(false);
        }

        if (boxXInv.isSelected() || boundingCheckXInv.isSelected()) {
            setXSliderInvEnabled(false);
            boxXInv.setSelected(false);
            colorButtonXInv.setEnabled(false);
            boundingCheckXInv.setSelected(false);
        }

        if (boxYInv.isSelected() || boundingCheckYInv.isSelected()) {
            setYSliderInvEnabled(false);
            boxYInv.setSelected(false);
            colorButtonYInv.setEnabled(false);
            boundingCheckYInv.setSelected(false);
        }

        if (boxZInv.isSelected() || boundingCheckZInv.isSelected()) {
            setZSliderInvEnabled(false);
            boxZInv.setSelected(false);
            colorButtonZInv.setEnabled(false);
            boundingCheckZInv.setSelected(false);
        }
    }

    /**
     * Disable arbitrary clipping planes.
     */
    public void disableClipPlanesArbi() {

        if (boxA.isSelected() || boundingCheckA.isSelected()) {
            setASliderEnabled(false);
            boxA.setSelected(false);
            boundingCheckA.setSelected(false);
            colorButtonA.setEnabled(false);
            if ( rayBasedRenderWM != null )
            {
                rayBasedRenderWM.enableArbitraryClipPlane( boxA.isSelected(), boundingCheckA.isSelected(),
                        new ColorRGB( colorButtonA.getBackground().getRed(),
                                colorButtonA.getBackground().getGreen(),
                                colorButtonA.getBackground().getBlue() ) );
                rayBasedRenderWM.enableEyeClipPlane( boxStatic.isSelected(), boundingCheckStatic.isSelected(),
                        new ColorRGB( colorButtonStatic.getBackground().getRed(),
                                colorButtonStatic.getBackground().getGreen(),
                                colorButtonStatic.getBackground().getBlue() ) );
            }
        }
    }

    /**
     * Dispose memory.
     */
    public void dispose() {
        clipSliderX = null;
        clipSliderY = null;
        clipSliderZ = null;
        clipSliderStatic = null;
        clipSliderStaticInv = null;
        sliderXInv = null;
        sliderYInv = null;
        sliderZInv = null;
        sliderA = null;
        boxX = null;
        boxY = null;
        boxZ = null;
        boxStatic = null;
        boxStaticInv = null;
        boxXInv = null;
        boxYInv = null;
        boxZInv = null;
        boxA = null;
        labelX = null;
        labelY = null;
        labelZ = null;
        labelStatic = null;
        labelStaticInv = null;
        labelXInv = null;
        labelYInv = null;
        labelZInv = null;
        labelA = null;
        labelXStart = null;
        labelXMid = null;
        labelXEnd = null;
        labelYStart = null;
        labelYMid = null;
        labelYEnd = null;
        labelZStart = null;
        labelZMid = null;
        labelZEnd = null;
        labelXStartInv = null;
        labelXMidInv = null;
        labelXEndInv = null;
        labelYStartInv = null;
        labelYMidInv = null;
        labelYEndInv = null;
        labelZStartInv = null;
        labelZMidInv = null;
        labelZEndInv = null;
        labelAStart = null;
        labelAMid = null;
        labelAEnd = null;
        labelStaticStart = null;
        labelStaticMid = null;
        labelStaticEnd = null;
        labelStaticInvStart = null;
        labelStaticInvMid = null;
        labelStaticInvEnd = null;
        textX = null;
        textY = null;
        textZ = null;
        textStatic = null;
        textStaticInv = null;
        textXInv = null;
        textYInv = null;
        textZInv = null;
        textA = null;
        colorButtonX = null;
        colorButtonXInv = null;
        colorButtonY = null;
        colorButtonYInv = null;
        colorButtonZ = null;
        colorButtonZInv = null;
        colorButtonStatic = null;
        colorButtonStaticInv = null;
        colorButtonA = null;
        colorChooser = null;
        boundingCheckX = null;
        boundingCheckY = null;
        boundingCheckZ = null;
        boundingCheckXInv = null;
        boundingCheckYInv = null;
        boundingCheckZInv = null;
        boundingCheckA = null;
        boundingCheckStatic = null;
        boundingCheckStaticInv = null;
        
        tabbedPane = null;
        panelX = null;
        panelXInv = null;
        panelY = null;
        panelYInv = null;
        panelZ = null;
        panelZInv = null;
        panelA = null;
        panelS = null;
        panelSInv = null;
    }
    /**
     * Return check box arbitrary selection value.
     * @return  boxA.isSelected() box arbitrary selection value.
     */
    public boolean getAVisible() {
        return boxA.isSelected();
    }

    /**
     * Initializes GUI components.
     */
    public void init() {

        // setTitle("Clipping Planes");
        Box contentBox = new Box(BoxLayout.Y_AXIS);

        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.font12B);

        buildPanelX();
        buildPanelXInv();
        buildPanelY();
        buildPanelYInv();
        buildPanelZ();
        buildPanelZInv();
        buildPanelA();
        buildPanelS();
        buildPanelSInv();

        tabbedPane.setSelectedIndex(0);
        tabbedPane.addChangeListener(this);;
        contentBox.add(tabbedPane);

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        scrollPanel = new JPanel();
        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(contentBox, BorderLayout.NORTH);

        scroller = new JScrollPane(scrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel = new JPanel();
        mainPanel.add(scroller);
    }

     /**
     * Invokes all the 6 clipping when 6 clipping checkbox is checked.
     */
    public void invokeClippingPlanes() {
        disable6Planes();
        boundingCheckX.setSelected(true);
        boundingCheckY.setSelected(true);
        boundingCheckZ.setSelected(true);
        boundingCheckXInv.setSelected(true);
        boundingCheckYInv.setSelected(true);
        boundingCheckZInv.setSelected(true);
        boxX.setSelected(true);
        boxY.setSelected(true);
        boxZ.setSelected(true);
        boxXInv.setSelected(true);
        boxYInv.setSelected(true);
        boxZInv.setSelected(true);
        colorButtonX.setEnabled(true);
        colorButtonXInv.setEnabled(true);
        colorButtonY.setEnabled(true);
        colorButtonYInv.setEnabled(true);
        colorButtonZ.setEnabled(true);
        colorButtonZInv.setEnabled(true);
        setXSliderEnabled(true);

        setYSliderEnabled(true);

        setZSliderEnabled(true);

        setXSliderInvEnabled(true);

        setYSliderInvEnabled(true);

        setZSliderInvEnabled(true);

        disableClipPlanesArbi();


        if ( rayBasedRenderWM != null )
        {
            rayBasedRenderWM.displayClipPlane( 0, true, 
                                               new ColorRGB( colorButtonXInv.getBackground().getRed(),
                                                             colorButtonXInv.getBackground().getGreen(),
                                                             colorButtonXInv.getBackground().getBlue() ) );
            rayBasedRenderWM.setClipPlane( 0, xSliceInv, boxXInv.isSelected() );

            rayBasedRenderWM.displayClipPlane( 1, true, 
                                               new ColorRGB( colorButtonX.getBackground().getRed(),
                                                             colorButtonX.getBackground().getGreen(),
                                                             colorButtonX.getBackground().getBlue() ) );
            rayBasedRenderWM.setClipPlane( 1, xSlice, boxX.isSelected() );
            rayBasedRenderWM.displayClipPlane( 2, true, 
                                               new ColorRGB( colorButtonYInv.getBackground().getRed(),
                                                             colorButtonYInv.getBackground().getGreen(),
                                                             colorButtonYInv.getBackground().getBlue() ) );
            rayBasedRenderWM.setClipPlane( 2, ySliceInv, boxYInv.isSelected() );

            rayBasedRenderWM.displayClipPlane( 3, true, 
                                               new ColorRGB( colorButtonY.getBackground().getRed(),
                                                             colorButtonY.getBackground().getGreen(),
                                                             colorButtonY.getBackground().getBlue() ) );
            rayBasedRenderWM.setClipPlane( 3, ySlice, boxY.isSelected() );

            rayBasedRenderWM.displayClipPlane( 4, true, 
                                               new ColorRGB( colorButtonZInv.getBackground().getRed(),
                                                             colorButtonZInv.getBackground().getGreen(),
                                                             colorButtonZInv.getBackground().getBlue() ) );
            rayBasedRenderWM.setClipPlane( 4, zSliceInv, boxZInv.isSelected() );

            rayBasedRenderWM.displayClipPlane( 5, true, 
                                               new ColorRGB( colorButtonZ.getBackground().getRed(),
                                                             colorButtonZ.getBackground().getGreen(),
                                                             colorButtonZ.getBackground().getBlue() ) );
            rayBasedRenderWM.setClipPlane( 5, zSlice, boxZ.isSelected() );


            rayBasedRenderWM.enableArbitraryClipPlane( boxA.isSelected(), boundingCheckA.isSelected(),
                                                       new ColorRGB( colorButtonA.getBackground().getRed(),
                                                                     colorButtonA.getBackground().getGreen(),
                                                                     colorButtonA.getBackground().getBlue() ) );
            rayBasedRenderWM.enableEyeClipPlane( boxStatic.isSelected(), boundingCheckStatic.isSelected(),
                                                 new ColorRGB( colorButtonStatic.getBackground().getRed(),
                                                               colorButtonStatic.getBackground().getGreen(),
                                                               colorButtonStatic.getBackground().getBlue() ) );
        }
    }

    /**
     * Resizig the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   panel width
     * @param  frameHeight  parent frame height.
     */
    public void resizePanel(int panelWidth, int frameHeight) {
        scroller.setPreferredSize(new Dimension(panelWidth, frameHeight));
        scroller.setSize(new Dimension(panelWidth, frameHeight));
        scroller.revalidate();
    }
    
    /**
     * Sets the arbitrary clip slider and the labels. state given by <code>flag</code>.
     * @param  flag  if <code>true</code> enable, otherwise disable.
     */
    public void setASliderEnabled(boolean flag) {
        sliderA.setEnabled(flag);
        labelA.setEnabled(flag);
        labelAStart.setEnabled(flag);
        labelAMid.setEnabled(flag);
        labelAEnd.setEnabled(flag);
    }

    /**
     * Set the color of the Arbitrary clip plane.
     * @param color new color.
     */
    public void setClipSliceAColor(Color color) {
        if ( rayBasedRenderWM != null )
        {
            rayBasedRenderWM.setArbColor( new ColorRGB( color.getRed(),
                    color.getGreen(),
                    color.getBlue() ) );
        }
   }

    /**
     * Sets the color of the static clipping plane slice frame.
     * @param  color  Color to set to.
     */
    public void setClipSliceSColor(Color color) {
        if ( rayBasedRenderWM != null )
        {
            rayBasedRenderWM.setEyeColor( new ColorRGB( color.getRed(),
                    color.getGreen(),
                    color.getBlue() ) );
        }
   }

    /**
     * Sets the color of the static inverse clipping plane slice frame.
     * @param  color  Color to set to.
     */
    public void setClipSliceSInvColor(Color color) {
        if ( rayBasedRenderWM != null )
        {
            rayBasedRenderWM.setEyeInvColor(  new ColorRGB( color.getRed(),
                    color.getGreen(),
                    color.getBlue() ) );
        }
    }

    /**
     * Sets the color of the x clipping plane slice frame.
     * @param  color  Color to set to.
     */
    public void setClipSliceXColor(Color color) {
        if ( rayBasedRenderWM != null )
        {
            rayBasedRenderWM.setClipPlaneColor( 1,
                                                new ColorRGB( color.getRed(),
                                                        color.getGreen(),
                                                        color.getBlue() ) );
        }
    }

    /**
     * Sets the color of the -x clipping plane slice frame.
     * @param  color  Color to set to.
     */
    public void setClipSliceXInvColor(Color color) {
        if ( rayBasedRenderWM != null )
        {
            rayBasedRenderWM.setClipPlaneColor( 0,
                                                new ColorRGB( color.getRed(),
                                                        color.getGreen(),
                                                        color.getBlue() ) );
        }
    }

    /**
     * Sets the color of the y clipping plane slice frame.
     * @param  color  Color to set to.
     */
    public void setClipSliceYColor(Color color) {
        if ( rayBasedRenderWM != null )
        {
            rayBasedRenderWM.setClipPlaneColor( 3,
                                                new ColorRGB( color.getRed(),
                                                        color.getGreen(),
                                                        color.getBlue() ) );
        }
    }

    /**
     * Sets the color of the -y clipping plane slice frame.
     * @param  color  Color to set to.
     */
    public void setClipSliceYInvColor(Color color) {
        if ( rayBasedRenderWM != null )
        {
            rayBasedRenderWM.setClipPlaneColor( 2,
                                                new ColorRGB( color.getRed(),
                                                        color.getGreen(),
                                                        color.getBlue() ) );
        }
    }

    /**
     * Sets the color of the z clipping plane slice frame.
     * @param  color  Color to set to.
     */
    public void setClipSliceZColor(Color color) {
        if ( rayBasedRenderWM != null )
        {
            rayBasedRenderWM.setClipPlaneColor( 5,
                                                new ColorRGB( color.getRed(),
                                                        color.getGreen(),
                                                        color.getBlue() ) );
        }
    }

    /**
     * Sets the color of the -z clipping plane slice frame.
     * @param  color  Color to set to.
     */
    public void setClipSliceZInvColor(Color color) {
        if ( rayBasedRenderWM != null )
        {
            rayBasedRenderWM.setClipPlaneColor( 4,
                                                new ColorRGB( color.getRed(),
                                                        color.getGreen(),
                                                        color.getBlue() ) );
        }
    }

    /**
     * Sets the static inverse slider and the labels beside and beneath it to the state given by <code>flag</code>.
     * @param  flag  if <code>true</code> enable, otherwise disable.
     */
    public void setStaticInvSliderEnabled(boolean flag) {
        clipSliderStaticInv.setEnabled(flag);
        labelStaticInv.setEnabled(flag);
        labelStaticInvStart.setEnabled(flag);
        labelStaticInvMid.setEnabled(flag);
        labelStaticInvEnd.setEnabled(flag);
    }

    /**
     * Sets the static slider and the labels beside and beneath it to the state given by <code>flag</code>.
     * @param  flag  if <code>true</code> enable, otherwise disable.
     */
    public void setStaticSliderEnabled(boolean flag) {
        clipSliderStatic.setEnabled(flag);
        labelStatic.setEnabled(flag);
        labelStaticStart.setEnabled(flag);
        labelStaticMid.setEnabled(flag);
        labelStaticEnd.setEnabled(flag);
    }

    /**
     * Sets the x slider and the labels beside and beneath it to the state given by <code>flag</code>.
     * @param  flag  if <code>true</code> enable, otherwise disable.
     */
    public void setXSliderEnabled(boolean flag) {
        clipSliderX.setEnabled(flag);
        labelX.setEnabled(flag);
        labelXStart.setEnabled(flag);
        labelXMid.setEnabled(flag);
        labelXEnd.setEnabled(flag);
    }

    /**
     * Sets the x slider and the labels beside and beneath it to the state given by <code>flag</code>.
     * @param  flag  if <code>true</code> enable, otherwise disable.
     */
    public void setXSliderInvEnabled(boolean flag) {
        sliderXInv.setEnabled(flag);
        labelXInv.setEnabled(flag);
        labelXStartInv.setEnabled(flag);
        labelXMidInv.setEnabled(flag);
        labelXEndInv.setEnabled(flag);
    }

    /**
     * Sets the y slider and the labels beside and beneath it to the state given by <code>flag</code>.
     * @param  flag  if <code>true</code> enable, otherwise disable.
     */
    public void setYSliderEnabled(boolean flag) {
        clipSliderY.setEnabled(flag);
        labelY.setEnabled(flag);
        labelYStart.setEnabled(flag);
        labelYMid.setEnabled(flag);
        labelYEnd.setEnabled(flag);
    }

    /**
     * Sets the y slider and the labels beside and beneath it to the state given by <code>flag</code>.
     * @param  flag  if <code>true</code> enable, otherwise disable.
     */
    public void setYSliderInvEnabled(boolean flag) {
        sliderYInv.setEnabled(flag);
        labelYInv.setEnabled(flag);
        labelYStartInv.setEnabled(flag);
        labelYMidInv.setEnabled(flag);
        labelYEndInv.setEnabled(flag);
    }

    /**
     * Sets the z slider and the labels beside and beneath it to the state given by <code>flag</code>.
     * @param  flag  if <code>true</code> enable, otherwise disable.
     */
    public void setZSliderEnabled(boolean flag) {
        clipSliderZ.setEnabled(flag);
        labelZ.setEnabled(flag);
        labelZStart.setEnabled(flag);
        labelZMid.setEnabled(flag);
        labelZEnd.setEnabled(flag);
    }

    /**
     * Sets the z slider and the labels beside and beneath it to the state given by <code>flag</code>.
     * @param  flag  if <code>true</code> enable, otherwise disable.
     */
    public void setZSliderInvEnabled(boolean flag) {
        sliderZInv.setEnabled(flag);
        labelZInv.setEnabled(flag);
        labelZStartInv.setEnabled(flag);
        labelZMidInv.setEnabled(flag);
        labelZEndInv.setEnabled(flag);
    }

    /**
     * Sets how the image plane should be displayed depending on value of slider.
     * @param  e  Event that triggered this function.
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        if (source == clipSliderX) {
            xSlice = clipSliderX.getValue() - 1;

            if (xSlice < xSliceInv) {
                xSlice = xSliceInv;
                clipSliderX.setValue(xSlice);
            }
            textX.setText(String.valueOf(xSlice + 1));

            if ( rayBasedRenderWM != null )
            {
                rayBasedRenderWM.setClipPlane( 1, xSlice, boxX.isSelected() );
            }
        } else if (source == clipSliderY) {
            ySlice = clipSliderY.getValue() - 1;

            if (ySlice < ySliceInv) {
                ySlice = ySliceInv;
                clipSliderY.setValue(ySlice);
            }

            textY.setText(String.valueOf(ySlice + 1));

            if ( rayBasedRenderWM != null )
            {
                rayBasedRenderWM.setClipPlane( 3, ySlice, boxY.isSelected() );
            }
        } else if (source == clipSliderZ) {
            zSlice = clipSliderZ.getValue() - 1;

            if (zSlice < zSliceInv) {
                zSlice = zSliceInv;
                clipSliderZ.setValue(zSlice);
            }

            textZ.setText(String.valueOf(zSlice + 1));

            if ( rayBasedRenderWM != null )
            {
                rayBasedRenderWM.setClipPlane( 5, zSlice, boxZ.isSelected() );
            }
        } else if (source == sliderXInv) {
            xSliceInv = sliderXInv.getValue() - 1;

            if (xSliceInv > xSlice) {
                xSliceInv = xSlice;
                sliderXInv.setValue(xSliceInv);
            }
            textXInv.setText(String.valueOf(xSliceInv + 1));

            if ( rayBasedRenderWM != null )
            {
                rayBasedRenderWM.setClipPlane( 0, xSliceInv, boxXInv.isSelected() );
            }
        } else if (source == sliderYInv) {
            ySliceInv = sliderYInv.getValue() - 1;

            if (ySliceInv > ySlice) {
                ySliceInv = ySlice;
                sliderYInv.setValue(ySliceInv);
            }
            textYInv.setText(String.valueOf(ySliceInv + 1));

            if ( rayBasedRenderWM != null )
            {
                rayBasedRenderWM.setClipPlane( 2, ySliceInv, boxYInv.isSelected() );
            }
        } else if (source == sliderZInv) {
            zSliceInv = sliderZInv.getValue() - 1;

            if (zSliceInv > zSlice) {
                zSliceInv = zSlice;
                sliderZInv.setValue(zSliceInv);
            }

            textZInv.setText(String.valueOf(zSliceInv + 1));

            if ( rayBasedRenderWM != null )
            {
                rayBasedRenderWM.setClipPlane( 4, zSliceInv, boxZInv.isSelected() );
            }
        } else if (source == sliderA) {
            aSlice = sliderA.getValue() - 1;

            textA.setText(String.valueOf(aSlice + 1));
            if ( rayBasedRenderWM != null )
            {
                rayBasedRenderWM.setArbitraryClipPlane(aSlice, boxA.isSelected());
            }
        } else if (source == clipSliderStatic) {
            sSlice = clipSliderStatic.getValue() - 1;

            if (sSlice > sSliceInv) {
                sSlice = sSliceInv;
                clipSliderStatic.setValue(sSlice);
            }

            textStatic.setText(String.valueOf(sSlice + 1));
            if ( rayBasedRenderWM != null )
            {
                rayBasedRenderWM.setEyeClipPlane(sSlice, boundingCheckStatic.isSelected(), boxStatic.isSelected());

            }

        } else if (source == clipSliderStaticInv) {
            sSliceInv = clipSliderStaticInv.getValue() - 1;

            if (sSliceInv < sSlice) {
                sSliceInv = sSlice;
                clipSliderStaticInv.setValue(sSlice);
            }

            textStaticInv.setText(String.valueOf(sSliceInv + 1));
            if ( rayBasedRenderWM != null )
            {
                rayBasedRenderWM.setEyeInvClipPlane(sSliceInv, boundingCheckStatic.isSelected(), boxStaticInv.isSelected());
            }
        }
    }

    /**
     * Closing the mouse recorder window frame.
     * @param  e  Window event.
     */
    public void windowClosing(@SuppressWarnings("unused")
    WindowEvent e) {
        disableClipPlanes();
        disableClipPlanesArbi();
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase#setButtonColor(javax.swing.JButton, java.awt.Color)
     */
    public void setButtonColor(JButton _button, Color _color) {

        super.setButtonColor(_button, _color);
        if (_button == colorButtonX) {
            setClipSliceXColor(_color);
        } else if (_button == colorButtonY) {
            setClipSliceYColor(_color);
        } else if (_button == colorButtonZ) {
            setClipSliceZColor(_color);
        } else if (_button == colorButtonXInv) {
            setClipSliceXInvColor(_color);
        } else if (_button == colorButtonYInv) {
            setClipSliceYInvColor(_color);
        } else if (_button == colorButtonZInv) {
            setClipSliceZInvColor(_color);
        } else if (_button == colorButtonA) {
            setClipSliceAColor(_color);
        } else if (_button == colorButtonStatic) {
            setClipSliceSColor(_color);
        } else if (_button == colorButtonStaticInv) {
            setClipSliceSInvColor(_color);
        }
    }


    /**
     * Helper method that adds components to the control panel for the grid bag layout.
     *
     * @param  panel  control panel.
     * @param  c      Component added to the control panel.
     * @param  gbc    GridBagConstraints of added component.
     * @param  x      Gridx location
     * @param  y      Gridy location
     * @param  w      Gridwidth
     * @param  h      Gridheight
     */
    private void addControlPanel(JPanel panel, Component c, GridBagConstraints gbc, int x, int y, int w, int h) {
        gbc.gridx = x;
        gbc.gridy = y;
        gbc.gridwidth = w;
        gbc.gridheight = h;
        panel.add(c, gbc);
    }

}
