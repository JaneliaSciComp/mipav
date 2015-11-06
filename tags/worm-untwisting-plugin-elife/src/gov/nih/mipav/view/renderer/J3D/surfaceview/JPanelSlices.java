package gov.nih.mipav.view.renderer.J3D.surfaceview;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.*;
import gov.nih.mipav.view.renderer.J3D.*;
import java.awt.*;
import java.awt.event.*;

import java.util.*;


import javax.swing.*;
import javax.swing.event.*;


/**
 * Dialog to turn slices bounding box of surface renderer on and off, and to
 * change the color of the frame. This dialog also control the X, Y, Z slices
 * movements.
 */
public class JPanelSlices extends JPanelRendererJ3D
    implements ChangeListener,  // for slider changes
               MouseListener    // (LOD change)
{
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -8359831093707979536L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Check boxes that turn the image plane on and off. */
    public JCheckBox boxX, boxY, boxZ;

    /** Sliders for the image planes. */
    public JSlider sliderX, sliderY, sliderZ, sliderT;

    /** Which time slice is currently displayed. */
    public int tSlice;

    private Vector3f slicePosition = new Vector3f();
    /** Which slice is currently displayed in the ZY plane. */
    public int xSlice;

    /** Flags indicating if the image slices are on or off. */
    public boolean[] sliceVisible = { true, true, true };

    /** Which slice is currently displayed in the XZ plane. */
    public int ySlice;

    /** Which slice is currently displayed in the XY plane. */
    public int zSlice;

    /** Bounding box control panel. */
    private JPanel boundingBoxPanel;

    /** Check box for turning bounding boxes on and off. */
    private JCheckBox[] boundingCheck = new JCheckBox[3];

    /** Color button for changing bounding box color. */
    private JButton[] colorButton = new JButton[3];

    /** Color chooser dialog. */
    private ViewJColorChooser colorChooser;

    /** Main panel for sliders. */
    private JPanel controlPanel;

    /** Current event vector index. */
    private int current;

    /** Labels next to sliders. */
    private JLabel labelX, labelY, labelZ, labelT;

    /** Labels beneath sliders. */
    private JLabel labelX1, labelXMid, labelXEnd, labelY1, labelYMid, labelYEnd, labelZ1, labelZMid, labelZEnd;

    /** Opacity control panel. */
    private JPanel opacityControlPanel;

    /** The opacity slider label. */
    private JLabel opacityLabelX, opacityLabelY, opacityLabelZ;

    /** The labels below the opacity slider. */
    private JLabel[] opacitySliderLabelsX, opacitySliderLabelsY, opacitySliderLabelsZ;

    /** Opacity slider, not enabled yet. */
    private JSlider opacitySliderX, opacitySliderY, opacitySliderZ;

    /** The scroll pane holding the panel content. Useful when the screen is small. */
    private JScrollPane scroller;

    /** Scroll panel that holding the all the control components. */
    private DrawingPanel scrollPanel;

    /** Flag to indicate the first time slider name changes. */
    private boolean setSliderFlag;

    /** Text fields that display the slice number next to the sliders. */
    private JTextField textX, textY, textZ, textT;

    /** x, y, z and time dimension values. */
    private int xDim, yDim, zDim, tDim;

    /** x, y, z opacity slider values. */
    private int xOpacitySlice, yOpacitySlice, zOpacitySlice;

    /** Probe x, y, z position. */
    private int xProbe, yProbe, zProbe;
    
    
    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog for turning slices bounding box frame on and off.
     *
     * @param  parent  Should be of type ViewJFrameSurfaceRenderer
     */
    public JPanelSlices(SurfaceRender parent) {
        super(parent);

        int[] fileExtents = renderBase.getImageA().getExtents( );
        Vector3f modelExtents = new Vector3f();
        MipavCoordinateSystems.fileToModel( new Vector3f( fileExtents[0], fileExtents[1], fileExtents[2] ),
                                            modelExtents, renderBase.getImageA() );

        xDim = (int)modelExtents.X;
        yDim = (int)modelExtents.Y;
        zDim = (int)modelExtents.Z;

        xSlice = (xDim - 1) / 2;
        ySlice = (yDim - 1) / 2;
        zSlice = (zDim - 1) / 2;

        xProbe = xSlice;
        yProbe = ySlice;
        zProbe = zSlice;



        if (renderBase.getImageA().getNDims() == 4) {
            tDim = renderBase.getImageA().getExtents()[3];

            // tSlice  = 0;
            tSlice = (tDim - 1) / 2;
        }

        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Changes color of slices box frame and button if color button was
     * pressed; turns bounding box on and off if checkbox was pressed; and
     * closes dialog if "Close" button was pressed.
     *
     * @param  event  Event that triggered function.
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String command = event.getActionCommand();

        if (source instanceof JButton) {
            colorChooser = new ViewJColorChooser(new Frame(), "Pick color", new OkColorListener((JButton) source),
                                                 new CancelListener());
        }

        for ( int i = 0; i < 3; i++ )
        {
            if (source == boundingCheck[i]) {
                if (boundingCheck[i].isSelected()) {
                    if ( renderBase != null )
                    {
                        ((SurfaceRender) renderBase).updateBoxSlicePos( true );
                        ((SurfaceRender) renderBase).showBoxSlice(i);
                    }
                    colorButton[i].setEnabled(true);
                } else {
                    if ( renderBase != null )
                    {
                        ((SurfaceRender) renderBase).removeBoxSlice(i);
                    }
                    colorButton[i].setEnabled(false);
                }
                break;
            }
        }

        if (command.equals("X")) {
            if (!boxX.isSelected()) {
                if ( renderBase != null )
                {
                    ((SurfaceRender) renderBase).getObjPlane_BG(0).detach();
                }
                setXSliderEnabled(false);
                setOpacitySliderXEnabled(false);
                sliceVisible[1] = false;
            } else {
                if ( renderBase != null )
                {
                    renderBase.getTriPlanarViewBG().addChild(((SurfaceRender) renderBase).getObjPlane_BG(0));
                }
                setXSliderEnabled(true);
                setOpacitySliderXEnabled(true);
                sliceVisible[1] = true;
            }
        } else if (command.equals("Y")) {
            if (!boxY.isSelected()) {
                if ( renderBase != null )
                {
                    ((SurfaceRender) renderBase).getObjPlane_BG(1).detach();
                }
                setYSliderEnabled(false);
                setOpacitySliderYEnabled(false);
                sliceVisible[2] = false;
            } else {
                if ( renderBase != null )
                {
                    renderBase.getTriPlanarViewBG().addChild(((SurfaceRender) renderBase).getObjPlane_BG(1));
                }
                setYSliderEnabled(true);
                setOpacitySliderYEnabled(true);
                sliceVisible[2] = true;
            }
        } else if (command.equals("Z")) {
            if (!boxZ.isSelected()) {
                if ( renderBase != null )
                {
                    ((SurfaceRender) renderBase).getObjPlane_BG(2).detach();
                }
                setZSliderEnabled(false);
                setOpacitySliderZEnabled(false);
                sliceVisible[0] = false;
            } else {
                if ( renderBase != null )
                {
                    renderBase.getTriPlanarViewBG().addChild(((SurfaceRender) renderBase).getObjPlane_BG(2));
                }
                setZSliderEnabled(true);
                setOpacitySliderZEnabled(true);
                sliceVisible[0] = true;
            }
        }

    }

    /**
     * Build the slice pickable panel 
     */
    public void buildSlicePickPanel() {
    	GridBagLayout cpGBL = new GridBagLayout();
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.NONE;
        cpGBC.weightx = 50;
        cpGBC.weighty = 50;
        
    }
    
    /**
     * Builds panel that has 3 sliders for the 3 planes shown, 3 checkboxes
     * for showing the planes, 3 text boxes for the current values of the
     * sliders, and a fourth slider and text box for the time dimension, if
     * necessary.
     */
    public void buildControlPanel() {
        int levelX = 0, levelY = 1, levelZ = 2;

        GridBagLayout cpGBL = new GridBagLayout();
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.NONE;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        controlPanel = new JPanel();
        controlPanel.setBounds(10, 100, 500, 120);
        controlPanel.setBorder(buildTitledBorder("Slices control box"));
        controlPanel.setLayout(cpGBL);

        cpGBC.fill = GridBagConstraints.BOTH;

        labelX = new JLabel(" Axial: (1 - " + String.valueOf(xDim) + ")");
        levelX = 0;

        boxX = new JCheckBox();
        boxX.setSelected(true);
        if ( renderBase != null )
        {
            boxX.addActionListener(((SurfaceRender) renderBase).getMouseDialog());
        }
        boxX.addActionListener(this);
        boxX.setActionCommand("X");
        addControlPanel(boxX, cpGBC, 0, levelX, 1, 1);

        labelX.setForeground(Color.black);
        labelX.setFont(MipavUtil.font12);
        labelX.setEnabled(true);
        addControlPanel(labelX, cpGBC, 1, levelX, 2, 1);

        sliderX = new JSlider(0, xDim - 1, xSlice);
        sliderX.setFont(MipavUtil.font12);
        sliderX.setEnabled(true);
        sliderX.setMinorTickSpacing(xDim / 10);
        sliderX.setPaintTicks(true);
        if ( renderBase != null )
        {
            sliderX.addChangeListener(((SurfaceRender) renderBase).getMouseDialog());
        }
        sliderX.addChangeListener(this);
        sliderX.addMouseListener(this);
        sliderX.setVisible(true);

        labelX1 = new JLabel("1");
        labelX1.setForeground(Color.black);
        labelX1.setFont(MipavUtil.font12);
        labelX1.setEnabled(true);
        labelXMid = new JLabel(String.valueOf(xSlice + 1));
        labelXMid.setForeground(Color.black);
        labelXMid.setFont(MipavUtil.font12);
        labelXMid.setEnabled(true);
        labelXEnd = new JLabel(String.valueOf(xDim));
        labelXEnd.setForeground(Color.black);
        labelXEnd.setFont(MipavUtil.font12);
        labelXEnd.setEnabled(true);

        Hashtable labelTableX = new Hashtable();

        labelTableX.put(new Integer(0), labelX1);
        labelTableX.put(new Integer(xSlice), labelXMid);
        labelTableX.put(new Integer(xDim - 1), labelXEnd);
        sliderX.setLabelTable(labelTableX);
        sliderX.setPaintLabels(true);
        addControlPanel(sliderX, cpGBC, 4, levelX, 8, 1);

        textX = new JTextField(String.valueOf(xSlice + 1), 4);
        textX.setFont(MipavUtil.font12);
        textX.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(textX, cpGBC, 14, levelX, 1, 1);

        cpGBC.fill = GridBagConstraints.BOTH;

        labelY = new JLabel(" Coronal: (1 - " + String.valueOf(yDim) + ")");
        levelY = 1;

        boxY = new JCheckBox();
        boxY.setSelected(true);
        if ( renderBase != null )
        {
            boxY.addActionListener(((SurfaceRender) renderBase).getMouseDialog());
        }
        boxY.addActionListener(this);
        boxY.setActionCommand("Y");
        addControlPanel(boxY, cpGBC, 0, levelY, 1, 1);

        labelY.setForeground(Color.black);
        labelY.setFont(MipavUtil.font12);
        labelY.setEnabled(true);
        addControlPanel(labelY, cpGBC, 1, levelY, 2, 1);

        sliderY = new JSlider(0, yDim - 1, ySlice);
        sliderY.setFont(MipavUtil.font12);
        sliderY.setEnabled(true);
        sliderY.setMinorTickSpacing(yDim / 10);
        sliderY.setPaintTicks(true);
        if ( renderBase != null )
        {
            sliderY.addChangeListener(((SurfaceRender) renderBase).getMouseDialog());
        }
        sliderY.addChangeListener(this);
        sliderY.addMouseListener(this);
        sliderY.setVisible(true);

        labelY1 = new JLabel("1");
        labelY1.setForeground(Color.black);
        labelY1.setFont(MipavUtil.font12);
        labelY1.setEnabled(true);
        labelYMid = new JLabel(String.valueOf(ySlice + 1));
        labelYMid.setForeground(Color.black);
        labelYMid.setFont(MipavUtil.font12);
        labelYMid.setEnabled(true);
        labelYEnd = new JLabel(String.valueOf(yDim));
        labelYEnd.setForeground(Color.black);
        labelYEnd.setFont(MipavUtil.font12);
        labelYEnd.setEnabled(true);

        Hashtable labelTableY = new Hashtable();

        labelTableY.put(new Integer(0), labelY1);
        labelTableY.put(new Integer(ySlice), labelYMid);
        labelTableY.put(new Integer(yDim - 1), labelYEnd);
        sliderY.setLabelTable(labelTableY);
        sliderY.setPaintLabels(true);
        addControlPanel(sliderY, cpGBC, 4, levelY, 8, 1);

        textY = new JTextField(String.valueOf(ySlice + 1), 4);
        textY.setFont(MipavUtil.font12);
        textY.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(textY, cpGBC, 14, levelY, 1, 1);

        cpGBC.fill = GridBagConstraints.BOTH;

        labelZ = new JLabel(" Sagittal (1 - " + String.valueOf(zDim) + ")");
        levelZ = 2;

        boxZ = new JCheckBox();
        boxZ.setSelected(true);
        if ( renderBase != null )
        {
            boxZ.addActionListener(((SurfaceRender) renderBase).getMouseDialog());
        }
        boxZ.addActionListener(this);
        boxZ.setActionCommand("Z");
        addControlPanel(boxZ, cpGBC, 0, levelZ, 1, 1);

        labelZ.setForeground(Color.black);
        labelZ.setFont(MipavUtil.font12);
        labelZ.setEnabled(true);
        addControlPanel(labelZ, cpGBC, 1, levelZ, 2, 1);

        sliderZ = new JSlider(0, zDim - 1, zSlice);
        sliderZ.setFont(MipavUtil.font12);
        sliderZ.setEnabled(true);
        sliderZ.setMinorTickSpacing(zDim / 10);
        sliderZ.setPaintTicks(true);
        if ( renderBase != null )
        {
            sliderZ.addChangeListener(((SurfaceRender) renderBase).getMouseDialog());
        }
        sliderZ.addChangeListener(this);
        sliderZ.addMouseListener(this);
        sliderZ.setVisible(true);

        labelZ1 = new JLabel("1");
        labelZ1.setForeground(Color.black);
        labelZ1.setFont(MipavUtil.font12);
        labelZ1.setEnabled(true);
        labelZMid = new JLabel(String.valueOf(zSlice + 1));
        labelZMid.setForeground(Color.black);
        labelZMid.setFont(MipavUtil.font12);
        labelZMid.setEnabled(true);
        labelZEnd = new JLabel(String.valueOf(zDim));
        labelZEnd.setForeground(Color.black);
        labelZEnd.setFont(MipavUtil.font12);
        labelZEnd.setEnabled(true);

        Hashtable labelTableZ = new Hashtable();

        labelTableZ.put(new Integer(0), labelZ1);
        labelTableZ.put(new Integer(zSlice), labelZMid);
        labelTableZ.put(new Integer(zDim - 1), labelZEnd);
        sliderZ.setLabelTable(labelTableZ);
        sliderZ.setPaintLabels(true);
        addControlPanel(sliderZ, cpGBC, 4, levelZ, 8, 1);

        textZ = new JTextField(String.valueOf(zSlice + 1), 4);
        textZ.setFont(MipavUtil.font12);
        textZ.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;

        addControlPanel(textZ, cpGBC, 14, levelZ, 1, 1);
        
        if ( renderBase != null )
        {
            if (renderBase.getImageA().getNDims() == 4) {
                buildTimeSlider();
            }
        }
    }

    /**
     * Builds panel for opacity control change on the triplanar X, Y, Z.
     */
    public void buildOpacityPanel() {

        // Slider for changing opacity; not currently enabled.
        int levelX = 0, levelY = 2, levelZ = 4;
        GridBagLayout cpGBL = new GridBagLayout();
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.NONE;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        opacityControlPanel = new JPanel();
        opacityControlPanel.setBounds(10, 100, 500, 120);
        opacityControlPanel.setBorder(buildTitledBorder("Opacity control box"));
        opacityControlPanel.setLayout(cpGBL);

        cpGBC.fill = GridBagConstraints.BOTH;

        opacityLabelX = new JLabel("X Opacity");
        levelX = 0;

        opacityLabelX.setFont(serif12B);
        opacityLabelX.setForeground(Color.black);
        addOpacityControlPanel(opacityLabelX, cpGBC, 0, levelX, 2, 1);

        opacitySliderLabelsX = new JLabel[3];
        opacitySliderLabelsX[0] = createLabel("0");
        opacitySliderLabelsX[1] = createLabel("50");
        opacitySliderLabelsX[2] = createLabel("100");

        Hashtable labelsX = new Hashtable();

        labelsX.put(new Integer(0), opacitySliderLabelsX[0]);
        labelsX.put(new Integer(50), opacitySliderLabelsX[1]);
        labelsX.put(new Integer(100), opacitySliderLabelsX[2]);

        opacitySliderX = new JSlider(0, 100, 100);
        opacitySliderX.setFont(serif12);
        opacitySliderX.setMinorTickSpacing(10);
        opacitySliderX.setPaintTicks(true);
        if ( renderBase != null )
        {
            opacitySliderX.addChangeListener(((SurfaceRender) renderBase).getMouseDialog());
        }
        opacitySliderX.addChangeListener(this);
        opacitySliderX.addMouseListener(this);
        opacitySliderX.setLabelTable(labelsX);
        opacitySliderX.setPaintLabels(true);
        opacitySliderX.setAlignmentX(Component.LEFT_ALIGNMENT);
        opacityLabelX.setAlignmentX(Component.LEFT_ALIGNMENT);
        opacitySliderX.setEnabled(true);
        opacityLabelX.setEnabled(true);
        xOpacitySlice = opacitySliderX.getValue();
        opacitySliderLabelsX[0].setEnabled(true);
        opacitySliderLabelsX[1].setEnabled(true);
        opacitySliderLabelsX[2].setEnabled(true);
        addOpacityControlPanel(opacitySliderX, cpGBC, 0, levelX + 1, 8, 1);

        opacityLabelY = new JLabel("Y Opacity");
        levelY = 2;

        opacityLabelY.setFont(serif12B);
        opacityLabelY.setForeground(Color.black);
        addOpacityControlPanel(opacityLabelY, cpGBC, 0, levelY, 2, 1);

        opacitySliderLabelsY = new JLabel[3];
        opacitySliderLabelsY[0] = createLabel("0");
        opacitySliderLabelsY[1] = createLabel("50");
        opacitySliderLabelsY[2] = createLabel("100");

        Hashtable labelsY = new Hashtable();

        labelsY.put(new Integer(0), opacitySliderLabelsY[0]);
        labelsY.put(new Integer(50), opacitySliderLabelsY[1]);
        labelsY.put(new Integer(100), opacitySliderLabelsY[2]);

        opacitySliderY = new JSlider(0, 100, 100);
        opacitySliderY.setFont(serif12);
        opacitySliderY.setMinorTickSpacing(10);
        opacitySliderY.setPaintTicks(true);
        if ( renderBase != null )
        {
            opacitySliderY.addChangeListener(((SurfaceRender) renderBase).getMouseDialog());
        }
        opacitySliderY.addChangeListener(this);
        opacitySliderY.addMouseListener(this);
        opacitySliderY.setLabelTable(labelsY);
        opacitySliderY.setPaintLabels(true);
        opacitySliderY.setAlignmentX(Component.LEFT_ALIGNMENT);
        opacityLabelY.setAlignmentX(Component.LEFT_ALIGNMENT);
        opacitySliderY.setEnabled(true);
        opacityLabelY.setEnabled(true);
        yOpacitySlice = opacitySliderY.getValue();
        opacitySliderLabelsY[0].setEnabled(true);
        opacitySliderLabelsY[1].setEnabled(true);
        opacitySliderLabelsY[2].setEnabled(true);
        addOpacityControlPanel(opacitySliderY, cpGBC, 0, levelY + 1, 8, 1);

        opacityLabelZ = new JLabel("Z Opacity");
        levelZ = 4;

        opacityLabelZ.setFont(serif12B);
        opacityLabelZ.setForeground(Color.black);
        addOpacityControlPanel(opacityLabelZ, cpGBC, 0, levelZ, 2, 1);

        opacitySliderLabelsZ = new JLabel[3];
        opacitySliderLabelsZ[0] = createLabel("0");
        opacitySliderLabelsZ[1] = createLabel("50");
        opacitySliderLabelsZ[2] = createLabel("100");

        Hashtable labelsZ = new Hashtable();

        labelsZ.put(new Integer(0), opacitySliderLabelsZ[0]);
        labelsZ.put(new Integer(50), opacitySliderLabelsZ[1]);
        labelsZ.put(new Integer(100), opacitySliderLabelsZ[2]);

        opacitySliderZ = new JSlider(0, 100, 100);
        opacitySliderZ.setFont(serif12);
        opacitySliderZ.setMinorTickSpacing(10);
        opacitySliderZ.setPaintTicks(true);
        if ( renderBase != null )
        {
            opacitySliderZ.addChangeListener(((SurfaceRender) renderBase).getMouseDialog());
        }
        opacitySliderZ.addChangeListener(this);
        opacitySliderZ.addMouseListener(this);
        opacitySliderZ.setLabelTable(labelsZ);
        opacitySliderZ.setPaintLabels(true);
        opacitySliderZ.setAlignmentX(Component.LEFT_ALIGNMENT);
        opacityLabelZ.setAlignmentX(Component.LEFT_ALIGNMENT);
        opacitySliderZ.setEnabled(true);
        opacityLabelZ.setEnabled(true);
        zOpacitySlice = opacitySliderZ.getValue();
        opacitySliderLabelsZ[0].setEnabled(true);
        opacitySliderLabelsZ[1].setEnabled(true);
        opacitySliderLabelsZ[2].setEnabled(true);

        addOpacityControlPanel(opacitySliderZ, cpGBC, 0, levelZ + 1, 8, 1);
    }

    /**
     * When 3D texture volume render is invoked, disable all the slices and
     * bounding frame boxes.
     */
    public void disableSlices() {
        if ( renderBase == null )
        {
            return;
        }

        for ( int i = 0; i < 3; i++ )
        {
            if (((SurfaceRender) renderBase).getObjPlane_BG(i).isLive())
            {
                ((SurfaceRender) renderBase).getObjPlane_BG(i).detach();
            }
            if (((SurfaceRender) renderBase).getObjBoxSlice_BG(i).isLive())
            {
                ((SurfaceRender) renderBase).getObjBoxSlice_BG(i).detach();
            }
        }
    }

    /**
     * Dispose memory.
     */
    public void dispose() {
        boundingBoxPanel = null;
        opacityLabelX = null;
        opacityLabelY = null;
        opacityLabelZ = null;
        opacitySliderLabelsX = null;
        opacitySliderLabelsY = null;
        opacitySliderLabelsZ = null;
        sliderX = null;
        sliderY = null;
        sliderZ = null;
        sliderT = null;
        opacitySliderX = null;
        opacitySliderY = null;
        opacitySliderZ = null;
        boxX = null;
        boxY = null;
        boxZ = null;
        labelX = null;
        labelY = null;
        labelZ = null;
        labelT = null;
        labelX1 = null;
        labelXMid = null;
        labelXEnd = null;
        labelY1 = null;
        labelYMid = null;
        labelYEnd = null;
        labelZ1 = null;
        labelZMid = null;
        labelZEnd = null;
        textX = null;
        textY = null;
        textZ = null;
        textT = null;
        opacityControlPanel = null;
        controlPanel = null;
        colorChooser = null;
        //sliderEvents = null;
    }

    /**
     * Enable slider X, Y, Z and triplanar.
     */
    public void enableSlices() {
        if ( renderBase == null )
        {
            return;
        }

        if ( sliceVisible[2] && !((SurfaceRender) renderBase).getObjPlane_BG(2).isLive()) {
            renderBase.getTriPlanarViewBG().addChild(((SurfaceRender) renderBase).getObjPlane_BG(2));
            setYSliderEnabled(true);
            setOpacitySliderYEnabled(true);
            boxY.setSelected(true);
            sliceVisible[2] = true;
        }

        if ( sliceVisible[0] && !((SurfaceRender) renderBase).getObjPlane_BG(0).isLive()) {
            renderBase.getTriPlanarViewBG().addChild(((SurfaceRender) renderBase).getObjPlane_BG(0));
            setZSliderEnabled(true);
            setOpacitySliderZEnabled(true);
            boxZ.setSelected(true);
            sliceVisible[0] = true;
        }

        if ( sliceVisible[0] && !((SurfaceRender) renderBase).getObjPlane_BG(0).isLive()) {
            renderBase.getTriPlanarViewBG().addChild(((SurfaceRender) renderBase).getObjPlane_BG(0));
            setXSliderEnabled(true);
            setOpacitySliderXEnabled(true);
            boxX.setSelected(true);
            sliceVisible[0] = true;
        }

        for ( int i = 0; i < 3; i++ )
        {
            if (boundingCheck[i].isSelected() && !((SurfaceRender) renderBase).getObjBoxSlice_BG(i).isLive()) {
            renderBase.getTriPlanarViewBG().addChild(((SurfaceRender) renderBase).getObjBoxSlice_BG(i));
            }
        }

    }

    /**
     * Return the checkbox of X slider.
     *
     * @return  boxX Get X slider check box.
     */
    public JCheckBox getBoxX() {
        return boxX;
    }

    /**
     * Return the checkbox of Y slider.
     *
     * @return  boxY Get Y slider check box.
     */
    public JCheckBox getBoxY() {
        return boxY;
    }

    /**
     * Return the checkbox of Z slider.
     *
     * @return  boxZ Get Z slider check box.
     */
    public JCheckBox getBoxZ() {
        return boxZ;
    }

    /**
     * Return the control panel.
     *
     * @return  JPanel slices move control panel
     */
    public JPanel getMainPanel() {
        return mainPanel;
    }

    /**
     * Return the X opacity slider.
     *
     * @return  opcitySliderX Opacity slider X.
     */
    public JSlider getOpacitySliderX() {
        return opacitySliderX;
    }

    /**
     * Return the Y opacity slider.
     *
     * @return  opcitySliderY Opacity slider Y.
     */
    public JSlider getOpacitySliderY() {
        return opacitySliderY;
    }

    /**
     * Return the Z opacity slider.
     *
     * @return  opcitySliderZ Opacity slider Z.
     */
    public JSlider getOpacitySliderZ() {
        return opacitySliderZ;
    }

    /**
     * Return the X slider.
     *
     * @return  sliderX X slider.
     */
    public JSlider getSliderX() {
        return sliderX;
    }

    /**
     * Return the Y slider.
     *
     * @return  sliderY Y slider.
     */
    public JSlider getSliderY() {
        return sliderY;
    }

    /**
     * Return the Z slider.
     *
     * @return  sliderZ Z slider.
     */
    public JSlider getSliderZ() {
        return sliderZ;
    }

    /**
     * Get the x opacity slider value.
     *
     * @return  xOpacityslice X opacity slider value.
     */
    public int getXOpacitySlice() {
        return xOpacitySlice;
    }

    /**
     * Get the probe x coordinate.
     *
     * @return  xProbe probe x position
     */
    public int getXProbePos() {
        return xProbe;
    }

    /**
     * Get the current slider value for the slider that matches the
     * orientation input parameter.
     * @param orientation either FileInfoBase.AXIAL, FileInfoBase.CORONAL,
     * FileInfoBase.SAGITTAL, or FileInfoBase.UNKNOWN_ORIENT.
     * @return The slider position for the given orientation is returned.  *
     */
    public int getSlice( int orientation )
    {
        if ( orientation == FileInfoBase.AXIAL )
        {
            return xSlice;
        }
        else if ( orientation == FileInfoBase.CORONAL )
        {
            return ySlice;
        }
        return zSlice;
    }

    /**
     * Sets the three slider positions, based on the position of the
     * three orthogonal planes: AXIAL, CORONAL, and SAGITTAL.
     * @param x center x-position in FileCoordinates
     * @param y center y-position in FileCoordinates
     * @param z center z-position in FileCoordinates
     */
    public void setCenter( int x, int y, int z )
    {
        Vector3f center = new Vector3f();
        if ( renderBase != null )
        {
            MipavCoordinateSystems.fileToModel( new Vector3f( x, y, z ), center,
                                                renderBase.getImageA() );
        }
        setXSlicePos( (int)center.X );
        setYSlicePos( (int)center.Y );
        setZSlicePos( (int)center.Z );
    }

    /**
     * Gets the three slider positions, representing the center point of the
     * three orthogonal planes. The center point is translated from local
     * ModelCoordinates into FileCoordinates.
     * @return, the center of the three orthogonal planes (the three slider
     * positions) in FileCoordinates.
     */
    public Vector3f getCenter( )
    {
        Vector3f center = new Vector3f();
        if ( renderBase != null )
        {
            MipavCoordinateSystems.modelToFile( new Vector3f( xSlice, ySlice, zSlice ), center,
                                                renderBase.getImageA() );
        }
        return center;
    }

    /**
     * Get the X slider value.
     *
     * @return  xSlice X slider value.
     */
    public int getXSlice() {
        return xSlice;
    }

    /**
     * slice slider visible or not.
     *
     * @return sliceVisible if <code>true</code> visible, otherwise invisible.
     */
    public boolean getVisible( int orientation )
    {
        return sliceVisible[orientation];
    }

    /**
     * Get the y opacity slider value.
     *
     * @return  yOpacityslice Y opacity slider value.
     */
    public int getYOpacitySlice() {
        return yOpacitySlice;
    }

    /**
     * Get the probe y coordinate.
     *
     * @return  yProbe probe y position
     */
    public int getYProbePos() {
        return yProbe;
    }

    /**
     * Get the y slider value.
     *
     * @return  ySlice Y slider value.
     */
    public int getYSlice() {
        return ySlice;
    }

    /**
     * Get the z opacity slider value.
     *
     * @return  zOpacityslice Z opacity slider value.
     */
    public int getZOpacitySlice() {
        return zOpacitySlice;
    }

    /**
     * Get the probe z coordinate.
     *
     * @return  zProbe probe z position
     */
    public int getZProbePos() {
        return zProbe;
    }

    /**
     * Get the z slider value.
     *
     * @return  zSlice Z slider value.
     */
    public int getZSlice() {
        return zSlice;
    }


    /**
     * Initializes GUI components.
     */
    public void init() {
        buildBoundingBox();
        buildControlPanel();
        buildOpacityPanel();
        buildSlicePickPanel();

        Box contentBox = new Box(BoxLayout.Y_AXIS);

        contentBox.add(opacityControlPanel);
        contentBox.add(boundingBoxPanel);
        contentBox.add(controlPanel);

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        scrollPanel = new DrawingPanel();
        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(contentBox, BorderLayout.NORTH);

        scroller = new JScrollPane(scrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel = new JPanel();
        mainPanel.add(scroller);
    }

    /**
     * Unchanged.
     *
     * @param  event  Original mouse event.
     */
    public void mouseClicked(MouseEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  Original mouse event.
     */
    public void mouseEntered(MouseEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  Original mouse event.
     */
    public void mouseExited(MouseEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  Original mouse event.
     */
    public void mousePressed(MouseEvent event) {
        JPanelMouse myMouseDialog = null;
        if ( renderBase != null )
        {
            myMouseDialog = ((SurfaceRender) renderBase).getMouseDialog();
        }
        if ((myMouseDialog != null) && myMouseDialog.isRecording()) {
            
            renderBase.recordMouse( "Slider" + myMouseDialog.sliderCount, myMouseDialog,
                                                ((SurfaceRender) renderBase).getMouseMode());
            setSliderFlag = true;
            myMouseDialog.events.add(renderBase.getSliderEvents());
            current = myMouseDialog.events.indexOf(renderBase.getSliderEvents());
        }

    }

    /**
     * Used in MouseRecorder to stop one series of slide moves.
     *
     * @param  event  Original mouse event.
     */
    public void mouseReleased(MouseEvent event) {
        JPanelMouse myMouseDialog = null;
        if ( renderBase != null )
        {
            myMouseDialog = ((SurfaceRender) renderBase).getMouseDialog();
        }
        if ((myMouseDialog != null) && myMouseDialog.isRecording()) {
            myMouseDialog.sliderCount++;
        }
    }

    /**
     * Resizig the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   DOCUMENT ME!
     * @param  frameHeight  DOCUMENT ME!
     */
    public void resizePanel(int panelWidth, int frameHeight) {
        scroller.setPreferredSize(new Dimension(panelWidth, frameHeight - 40));
        scroller.setSize(new Dimension(panelWidth, frameHeight - 40));
        scroller.revalidate();
    }

    /**
     * Set the opacity slider X with given boolean value.
     *
     * @param  flag  indicate opacity slider is set or not
     */
    public void setOpacitySliderXEnabled(boolean flag) {
        opacityLabelX.setEnabled(flag);
        opacitySliderLabelsX[0].setEnabled(flag);
        opacitySliderLabelsX[1].setEnabled(flag);
        opacitySliderLabelsX[2].setEnabled(flag);
        opacitySliderX.setEnabled(flag);
    }

    /**
     * Set the opacity slider Y with given boolean value.
     *
     * @param  flag  indicate opacity slider is set or not
     */
    public void setOpacitySliderYEnabled(boolean flag) {
        opacityLabelY.setEnabled(flag);
        opacitySliderLabelsY[0].setEnabled(flag);
        opacitySliderLabelsY[1].setEnabled(flag);
        opacitySliderLabelsY[2].setEnabled(flag);
        opacitySliderY.setEnabled(flag);
    }

    /**
     * Set the opacity slider Z with given boolean value.
     *
     * @param  flag  indicate opacity slider is set or not
     */
    public void setOpacitySliderZEnabled(boolean flag) {
        opacityLabelZ.setEnabled(flag);
        opacitySliderLabelsZ[0].setEnabled(flag);
        opacitySliderLabelsZ[1].setEnabled(flag);
        opacitySliderLabelsZ[2].setEnabled(flag);
        opacitySliderZ.setEnabled(flag);
    }

    /**
     * Sets the scene state appropriately, so that the slices that are
     * supposed to be visible are showing, the ones that aren't are hidden,
     * and the sliders are starting at the appropriate value.
     *
     * @param  scene  The state of the scene.
     */
    public void setSceneState(Object scene) {
        xSlice = ((SceneState) scene).x;
        ySlice = ((SceneState) scene).y;
        zSlice = ((SceneState) scene).z;

        if (!((SceneState) scene).xVisible) {
            ((SurfaceRender) renderBase).getObjPlane_BG(0).detach();
            setXSliderEnabled(false);
            sliceVisible[0] = false;
            boxX.setSelected(false);
        } else if (((SceneState) scene).xVisible) {

            if (!((SurfaceRender) renderBase).getObjPlane_BG(0).isLive()) {
            	if ( renderBase.getTriPlanarViewBG().isLive()) {
                  (renderBase.getTriPlanarViewBG()).addChild(((SurfaceRender) renderBase).getObjPlane_BG(0));
                }
            }

            setXSliderEnabled(true);
            sliceVisible[0] = true;
            boxX.setSelected(true);
        }

        if (!((SceneState) scene).yVisible) {
            ((SurfaceRender) renderBase).getObjPlane_BG(1).detach();
            setYSliderEnabled(false);
            sliceVisible[1] = false;
            boxY.setSelected(false);
        } else if (((SceneState) scene).yVisible) {

            if (!((SurfaceRender) renderBase).getObjPlane_BG(1).isLive()) {
            	if ( renderBase.getTriPlanarViewBG().isLive()) {
                    (renderBase.getTriPlanarViewBG()).addChild(((SurfaceRender) renderBase).getObjPlane_BG(1));
                }
            }

            setYSliderEnabled(true);
            sliceVisible[1] = true;
            boxY.setSelected(true);
        }

        if (!((SceneState) scene).zVisible) {
            ((SurfaceRender) renderBase).getObjPlane_BG(2).detach();
            setZSliderEnabled(false);
            sliceVisible[2] = false;
            boxZ.setSelected(false);
        } else if (((SceneState) scene).zVisible) {

            if (!((SurfaceRender) renderBase).getObjPlane_BG(2).isLive()) {
            	if ( renderBase.getTriPlanarViewBG().isLive()) {
                  (renderBase.getTriPlanarViewBG()).addChild(((SurfaceRender) renderBase).getObjPlane_BG(2));
                }
           }

            setZSliderEnabled(true);
            sliceVisible[2] = true;
            boxZ.setSelected(true);
        }

        sliceVisible[0] = ((SceneState) scene).xVisible;
        sliceVisible[1] = ((SceneState) scene).yVisible;
        sliceVisible[2] = ((SceneState) scene).zVisible;
    }

    /**
     * Set probe x coordinate.
     *
     * @param  _xProbe  probe x position
     */
    public void setXProbePos(int _xProbe) {
        xProbe = _xProbe;
    }

    /**
     * Set the current x slider move position.
     *
     * @param  _xSlice  x slider position
     */
    private void setXSlicePos(int _xSlice)
    {
        xSlice = _xSlice;
        sliderX.setValue(xSlice);
        textX.setText(String.valueOf(xSlice + 1));
    }

    /**
     * Sets the x slider and the labels beside and beneath it to the state
     * given by <code>flag</code>.
     *
     * @param  flag  if <code>true</code> enable, otherwise disable.
     */
    public void setXSliderEnabled(boolean flag) {
        sliderX.setEnabled(flag);
        labelX.setEnabled(flag);
        labelX1.setEnabled(flag);
        labelXMid.setEnabled(flag);
        labelXEnd.setEnabled(flag);
    }

    /**
     * Set probe y coordinate.
     *
     * @param  _yProbe  probe y position
     */
    public void setYProbePos(int _yProbe) {
        yProbe = _yProbe;
    }

    /**
     * Set the current y slider move position.
     *
     * @param  _ySlice  y slider position
     */
    private void setYSlicePos(int _ySlice) {
        ySlice = _ySlice;
        sliderY.setValue(ySlice);
        textY.setText(String.valueOf(ySlice + 1));
    }

    /**
     * Sets the y slider and the labels beside and beneath it to the state
     * given by <code>flag</code>.
     *
     * @param  flag  if <code>true</code> enable, otherwise disable.
     */
    public void setYSliderEnabled(boolean flag) {
        sliderY.setEnabled(flag);
        labelY.setEnabled(flag);
        labelY1.setEnabled(flag);
        labelYMid.setEnabled(flag);
        labelYEnd.setEnabled(flag);
    }

    /**
     * Set probe z coordinate.
     *
     * @param  _zProbe  probe z position
     */
    public void setZProbePos(int _zProbe) {
        zProbe = _zProbe;
    }

    /**
     * Set the current z slider move position.
     *
     * @param  _zSlice  z slider position
     */
    private void setZSlicePos(int _zSlice) {
        zSlice = _zSlice;
        sliderZ.setValue(zSlice);
        textZ.setText(String.valueOf(zSlice + 1));
    }

    /**
     * Sets the z slider and the labels beside and beneath it to the state
     * given by <code>flag</code>.
     *
     * @param  flag  if <code>true</code> enable, otherwise disable.
     */
    public void setZSliderEnabled(boolean flag) {
        sliderZ.setEnabled(flag);
        labelZ.setEnabled(flag);
        labelZ1.setEnabled(flag);
        labelZMid.setEnabled(flag);
        labelZEnd.setEnabled(flag);

    }

    /**
     * Sets how the image plane should be displayed depending on value of
     * slider.
     *
     * @param  e  Event that triggered this function.
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();
        JPanelMouse myMouseDialog = null;
        if ( renderBase != null )
        {
            myMouseDialog = ((SurfaceRender) renderBase).getMouseDialog();
        }
        if (source == sliderX) {
            if ( xSlice == sliderX.getValue() )
            {
                return;
            }
            // Change the currently displayed x slice
            xSlice = sliderX.getValue();
            textX.setText(String.valueOf(xSlice + 1));
            if ( renderBase != null )
            {
                ((SurfaceRender) renderBase).updateBoxSlicePos( true );
                ((SurfaceRender) renderBase).update3DTriplanar(null, null, false);
            }
            if ( (myMouseDialog != null) && myMouseDialog.isRecording() && setSliderFlag) {

                renderBase.getSliderEvents().setName("xSlider" + current);
                myMouseDialog.listModel.addElement("xSlider" + current);

                setSliderFlag = false;
            }
        } else if (source == sliderY) {
            if ( ySlice == sliderY.getValue() )
            {
                return;
            }
            // Change the currently displayed y slice
            ySlice = sliderY.getValue();
            textY.setText(String.valueOf(ySlice + 1));
            if ( renderBase != null )
            {
                ((SurfaceRender) renderBase).updateBoxSlicePos( true );
                ((SurfaceRender) renderBase).update3DTriplanar(null, null, false);
            }
            if ((myMouseDialog != null) && myMouseDialog.isRecording() && setSliderFlag) {

                renderBase.getSliderEvents().setName("ySlider" + current);
                myMouseDialog.listModel.addElement("ySlider" + current);

                setSliderFlag = false;
            }
        } else if (source == sliderZ) {
            if ( zSlice == sliderZ.getValue() )
            {
                return;
            }

            // Change the currently displayed z slice
            zSlice = sliderZ.getValue();
            textZ.setText(String.valueOf(zSlice + 1));
            if ( renderBase != null )
            {
                ((SurfaceRender) renderBase).updateBoxSlicePos( true );
                ((SurfaceRender) renderBase).update3DTriplanar(null, null, false);
            }
            if ((myMouseDialog != null) && myMouseDialog.isRecording() && setSliderFlag) {

                renderBase.getSliderEvents().setName("zSlider" + current);
                myMouseDialog.listModel.addElement("zSlider" + current);

                setSliderFlag = false;
            }
        } else if (source == sliderT) {

            // Change the currently displayed t slice
            tSlice = sliderT.getValue() - 1;
            textT.setText(String.valueOf(tSlice + 1));
            if ( renderBase != null )
            {
                renderBase.updateImages(true);
            }
        } else if (source == opacitySliderX) {
            xOpacitySlice = opacitySliderX.getValue();
            yOpacitySlice = opacitySliderY.getValue();
            zOpacitySlice = opacitySliderZ.getValue();
            if ( renderBase != null )
            {
                ((SurfaceRender) renderBase).updateOpacityOfOthrogPlanes(xOpacitySlice, -1, -1);
            }
            if ((myMouseDialog != null) && myMouseDialog.isRecording() && setSliderFlag) {

                renderBase.getSliderEvents().setName("xSliderOpacity" + current);
                myMouseDialog.listModel.addElement("xSliderOpacity" + current);

                setSliderFlag = false;
            }
        } else if (source == opacitySliderY) {
            xOpacitySlice = opacitySliderX.getValue();
            yOpacitySlice = opacitySliderY.getValue();
            zOpacitySlice = opacitySliderZ.getValue();
            if ( renderBase != null )
            {
                ((SurfaceRender) renderBase).updateOpacityOfOthrogPlanes(-1, yOpacitySlice, -1);
            }
            if ((myMouseDialog != null) && myMouseDialog.isRecording() && setSliderFlag) {

                renderBase.getSliderEvents().setName("ySliderOpacity" + current);
                myMouseDialog.listModel.addElement("ySliderOpacity" + current);

                setSliderFlag = false;
            }
        } else if (source == opacitySliderZ) {
            xOpacitySlice = opacitySliderX.getValue();
            yOpacitySlice = opacitySliderY.getValue();
            zOpacitySlice = opacitySliderZ.getValue();
            if ( renderBase != null )
            {
                ((SurfaceRender) renderBase).updateOpacityOfOthrogPlanes(-1, -1, zOpacitySlice);
            }
            if ((myMouseDialog != null) && myMouseDialog.isRecording() && setSliderFlag) {

                renderBase.getSliderEvents().setName("zSliderOpacity" + current);
                myMouseDialog.listModel.addElement("zSliderOpacity" + current);

                setSliderFlag = false;
            }
        }

        if ((myMouseDialog != null) &&  myMouseDialog.isRecording()) {
            renderBase.getSliderEvents().add(e, renderBase.getSceneState());
        }
    }

    /**
     * Calls the appropriate method in the parent frame.
     *
     * @param  button  DOCUMENT ME!
     * @param  color   Color to set box frame to.
     */
    protected void setBoxColor(JButton button, Color color) {
        if ( renderBase == null )
        {
            return;
        }

        if ( renderBase != null )
        {
            if (button == colorButton[0]) {
                ((SurfaceRender) renderBase).setSliceColor(color, 0);
            } else if (button == colorButton[1]) {
                ((SurfaceRender) renderBase).setSliceColor(color, 1);
            } else if (button == colorButton[2]) {
                ((SurfaceRender) renderBase).setSliceColor(color, 2);
            }
        }
    }
    
    /**
     * Helper method that adds components to the control panel for the grid
     * bag layout.
     *
     * @param  c    Component added to the control panel.
     * @param  gbc  GridBagConstraints of added component.
     * @param  x    Gridx location
     * @param  y    Gridy location
     * @param  w    Gridwidth
     * @param  h    Gridheight
     */
    private void addControlPanel(Component c, GridBagConstraints gbc, int x, int y, int w, int h) {
        gbc.gridx = x;
        gbc.gridy = y;
        gbc.gridwidth = w;
        gbc.gridheight = h;
        controlPanel.add(c, gbc);
    }

    /**
     * Helper method that adds components to the control panel for the grid
     * bag layout.
     *
     * @param  c    Component added to the control panel.
     * @param  gbc  GridBagConstraints of added component.
     * @param  x    Gridx location
     * @param  y    Gridy location
     * @param  w    Gridwidth
     * @param  h    Gridheight
     */
    private void addOpacityControlPanel(Component c, GridBagConstraints gbc, int x, int y, int w, int h) {
        gbc.gridx = x;
        gbc.gridy = y;
        gbc.gridwidth = w;
        gbc.gridheight = h;
        opacityControlPanel.add(c, gbc);
    }

    /**
     * Build the boudning box for X, Y, Z slices.
     */
    private void buildBoundingBox() {

        boundingCheck[0] = new JCheckBox("Show axial slice frame");
        boundingCheck[1] = new JCheckBox("Show coronal slice frame");
        boundingCheck[2] = new JCheckBox("Show sagittal slice frame");
        for ( int i = 0; i < 3; i++ )
        {
            boundingCheck[i].setFont(serif12);
            boundingCheck[i].addActionListener(this);
            boundingCheck[i].setSelected(true);

            colorButton[i] = new JButton();
            colorButton[i].setPreferredSize(new Dimension(25, 25));
            colorButton[i].addActionListener(this);
            colorButton[i].setEnabled(true);
        }

        colorButton[0].setToolTipText("Change axial frame color");
        colorButton[0].setBackground(Color.red);

        colorButton[1].setToolTipText("Change coronal frame color");
        colorButton[1].setBackground(Color.green);

        colorButton[2].setToolTipText("Change sagittal frame color");
        colorButton[2].setBackground(Color.yellow);


        boundingBoxPanel = new JPanel(new GridBagLayout());

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(5, 5, 5, 5);

        gbc.gridx = 0;
        gbc.gridy = 1;
        for ( int i = 0; i < 3; i++ )
        {
            boundingBoxPanel.add(colorButton[i], gbc);
            gbc.gridx = 1;
            boundingBoxPanel.add(boundingCheck[i], gbc);
            gbc.gridx = 0;
            gbc.gridy++;
        }
        boundingBoxPanel.setBorder(buildTitledBorder("Slices bounding box"));
    }

    /**
     * Builds the time (4D) slider. No check box, because there is no plane to
     * turn off.
     */
    private void buildTimeSlider() {
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.BOTH;
        labelT = new JLabel(" T (1 - " + String.valueOf(tDim) + ")");
        labelT.setForeground(Color.black);
        labelT.setFont(MipavUtil.font12);
        labelT.setEnabled(true);
        addControlPanel(labelT, cpGBC, 1, 3, 2, 1);

        sliderT = new JSlider(1, tDim, tSlice + 1);
        sliderT.setFont(MipavUtil.font12);
        sliderT.setEnabled(true);
        sliderT.setMinorTickSpacing(tDim / 10);
        sliderT.setPaintTicks(true);
        sliderT.addChangeListener(this);
        sliderT.setVisible(true);

        JLabel labelT1 = new JLabel("1");

        labelT1.setForeground(Color.black);
        labelT1.setFont(MipavUtil.font12);
        labelT1.setEnabled(true);

        JLabel labelTMid = new JLabel(String.valueOf(tSlice + 1));

        labelTMid.setForeground(Color.black);
        labelTMid.setFont(MipavUtil.font12);
        labelTMid.setEnabled(true);

        JLabel labelTEnd = new JLabel(String.valueOf(tDim));

        labelTEnd.setForeground(Color.black);
        labelTEnd.setFont(MipavUtil.font12);
        labelTEnd.setEnabled(true);

        Hashtable labelTableT = new Hashtable();

        labelTableT.put(new Integer(1), labelT1);
        labelTableT.put(new Integer(tSlice + 1), labelTMid);
        labelTableT.put(new Integer(tDim), labelTEnd);
        sliderT.setLabelTable(labelTableT);
        sliderT.setPaintLabels(true);
        addControlPanel(sliderT, cpGBC, 4, 3, 8, 1);

        textT = new JTextField(String.valueOf(tSlice + 1), 4);
        textT.setFont(MipavUtil.font12);
        textT.setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(textT, cpGBC, 14, 3, 1, 1);

    }

    /**
     * Creates a label in the proper font and color.
     *
     * @param   title  The title of the label.
     *
     * @return  The new label.
     */
    private JLabel createLabel(String title) {
        JLabel label = new JLabel(title);

        label.setFont(serif12);
        label.setForeground(Color.black);

        return label;
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Does nothing.
     */
    class CancelListener implements ActionListener {

        /**
         * Does nothing.
         *
         * @param  e  DOCUMENT ME!
         */
        public void actionPerformed(ActionEvent e) { }
    }

    /**
     * Wrapper in order to hold the control panel layout in the JScrollPane.
     */
    class DrawingPanel extends JPanel {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -6456589720445279985L;

        /**
         * DOCUMENT ME!
         *
         * @param  g  DOCUMENT ME!
         */
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);
        }
    }

    /**
     * Pick up the selected color and call method to change the VOI color.
     */
    class OkColorListener implements ActionListener {

        /** DOCUMENT ME! */
        JButton button;

        /**
         * Creates a new OkColorListener object.
         *
         * @param  _button  DOCUMENT ME!
         */
        OkColorListener(JButton _button) {
            super();
            button = _button;
        }

        /**
         * Get color from chooser and set button and VOI color.
         *
         * @param  e  Event that triggered function.
         */
        public void actionPerformed(ActionEvent e) {
            Color color = colorChooser.getColor();

            button.setBackground(color);
            setBoxColor(button, color);
        }
    }
}
