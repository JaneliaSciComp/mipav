package gov.nih.mipav.view.renderer.WildMagic.Interface;

import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.model.file.FileInfoBase;
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
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.util.Hashtable;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JTextField;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * Dialog to turn slices bounding box of surface renderer on and off, and to
 * change the color of the frame. This dialog also control the X, Y, Z slices
 * movements.
 */
public class JPanelSlices_WM extends JInterfaceBase
    implements ChangeListener 
{

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -8359831093707979536L;

    /** Check boxes that turn the image plane on and off. */
    public JCheckBox boxX, boxY, boxZ;

    /** Sliders for the image planes. */
    public JSlider sliderX, sliderY, sliderZ, sliderT;

    /** Which time slice is currently displayed. */
    public int tSlice;

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

    /** Main panel for sliders. */
    private JPanel controlPanel;

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

    /** Text fields that display the slice number next to the sliders. */
    private JTextField textX, textY, textZ, textT;

    /** x, y, z and time dimension values. */
    private int xDim, yDim, zDim, tDim;

    /** x, y, z opacity slider values. */
    private int xOpacitySlice, yOpacitySlice, zOpacitySlice;
    
    /** Probe x, y, z position. */
    private int xProbe, yProbe, zProbe;

    /**
     * Constructor.
     * @param kVolumeViewer parent frame.
     */
    public JPanelSlices_WM(VolumeTriPlanarInterface kVolumeViewer) {
        super(kVolumeViewer);

        int[] fileExtents = kVolumeViewer.getImageA().getExtents( );
        Vector3f modelExtents = new Vector3f();
        MipavCoordinateSystems.fileToModel( new Vector3f( fileExtents[0], fileExtents[1], fileExtents[2] ),
                                            modelExtents, kVolumeViewer.getImageA() );

        xDim = (int)modelExtents.X;
        yDim = (int)modelExtents.Y;
        zDim = (int)modelExtents.Z;

        xSlice = (xDim - 1) / 2;
        ySlice = (yDim - 1) / 2;
        zSlice = (zDim - 1) / 2;

        xProbe = xSlice;
        yProbe = ySlice;
        zProbe = zSlice;



        if (kVolumeViewer.getImageA().getNDims() == 4) {
            tDim = kVolumeViewer.getImageA().getExtents()[3];

            // tSlice  = 0;
            tSlice = (tDim - 1) / 2;
        }

        init();
    }

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

                if ( m_kVolumeViewer != null )
                {
                    m_kVolumeViewer.setSliceFromSurface( getCenter() );
                    m_kVolumeViewer.showBoundingBox( i, boundingCheck[i].isSelected() );
                }

                if (boundingCheck[i].isSelected()) {
                    colorButton[i].setEnabled(true);
                } else {
                    colorButton[i].setEnabled(false);
                }
                break;
            }
        }

        if (command.equals("X")) {
            if ( m_kVolumeViewer != null )
            {
                m_kVolumeViewer.setSliceFromSurface( getCenter() );
                m_kVolumeViewer.showSlice( 0, boxX.isSelected() );
            }
            if (!boxX.isSelected()) {
                setXSliderEnabled(false);
                setOpacitySliderXEnabled(false);
                sliceVisible[1] = false;
            } else {
                setXSliderEnabled(true);
                setOpacitySliderXEnabled(true);
                sliceVisible[1] = true;
            }
        } else if (command.equals("Y")) {
            if ( m_kVolumeViewer != null )
            {
                m_kVolumeViewer.setSliceFromSurface( getCenter() );
                m_kVolumeViewer.showSlice( 1, boxY.isSelected() );
            }

            if (!boxY.isSelected()) {
                setYSliderEnabled(false);
                setOpacitySliderYEnabled(false);
                sliceVisible[2] = false;
            } else {
                setYSliderEnabled(true);
                setOpacitySliderYEnabled(true);
                sliceVisible[2] = true;
            }
        } else if (command.equals("Z")) {
            if ( m_kVolumeViewer != null )
            {
                m_kVolumeViewer.setSliceFromSurface( getCenter() );
                m_kVolumeViewer.showSlice( 2, boxZ.isSelected() );
            }

            if (!boxZ.isSelected()) {
                setZSliderEnabled(false);
                setOpacitySliderZEnabled(false);
                sliceVisible[0] = false;
            } else {
                setZSliderEnabled(true);
                setOpacitySliderZEnabled(true);
                sliceVisible[0] = true;
            }
        }

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
        sliderX.addChangeListener(this);
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

        Hashtable<Integer,JLabel> labelTableX = new Hashtable<Integer,JLabel>();

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
        sliderY.addChangeListener(this);
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

        Hashtable<Integer,JLabel> labelTableY = new Hashtable<Integer,JLabel>();

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
        sliderZ.addChangeListener(this);
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

        Hashtable<Integer,JLabel> labelTableZ = new Hashtable<Integer,JLabel>();

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
        
        if ( m_kVolumeViewer != null )
        {
            if (m_kVolumeViewer.getImageA().getNDims() == 4) {
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

        opacityLabelX.setFont(MipavUtil.font12B);
        opacityLabelX.setForeground(Color.black);
        addOpacityControlPanel(opacityLabelX, cpGBC, 0, levelX, 2, 1);

        opacitySliderLabelsX = new JLabel[3];
        opacitySliderLabelsX[0] = createLabel("0");
        opacitySliderLabelsX[1] = createLabel("50");
        opacitySliderLabelsX[2] = createLabel("100");

        Hashtable<Integer,JLabel> labelsX = new Hashtable<Integer,JLabel>();

        labelsX.put(new Integer(0), opacitySliderLabelsX[0]);
        labelsX.put(new Integer(50), opacitySliderLabelsX[1]);
        labelsX.put(new Integer(100), opacitySliderLabelsX[2]);

        opacitySliderX = new JSlider(0, 100, 100);
        opacitySliderX.setFont(MipavUtil.font12);
        opacitySliderX.setMinorTickSpacing(10);
        opacitySliderX.setPaintTicks(true);
        opacitySliderX.addChangeListener(this);
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

        opacityLabelY.setFont(MipavUtil.font12B);
        opacityLabelY.setForeground(Color.black);
        addOpacityControlPanel(opacityLabelY, cpGBC, 0, levelY, 2, 1);

        opacitySliderLabelsY = new JLabel[3];
        opacitySliderLabelsY[0] = createLabel("0");
        opacitySliderLabelsY[1] = createLabel("50");
        opacitySliderLabelsY[2] = createLabel("100");

        Hashtable<Integer,JLabel> labelsY = new Hashtable<Integer,JLabel>();

        labelsY.put(new Integer(0), opacitySliderLabelsY[0]);
        labelsY.put(new Integer(50), opacitySliderLabelsY[1]);
        labelsY.put(new Integer(100), opacitySliderLabelsY[2]);

        opacitySliderY = new JSlider(0, 100, 100);
        opacitySliderY.setFont(MipavUtil.font12);
        opacitySliderY.setMinorTickSpacing(10);
        opacitySliderY.setPaintTicks(true);
        opacitySliderY.addChangeListener(this);
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

        opacityLabelZ.setFont(MipavUtil.font12B);
        opacityLabelZ.setForeground(Color.black);
        addOpacityControlPanel(opacityLabelZ, cpGBC, 0, levelZ, 2, 1);

        opacitySliderLabelsZ = new JLabel[3];
        opacitySliderLabelsZ[0] = createLabel("0");
        opacitySliderLabelsZ[1] = createLabel("50");
        opacitySliderLabelsZ[2] = createLabel("100");

        Hashtable<Integer,JLabel> labelsZ = new Hashtable<Integer,JLabel>();

        labelsZ.put(new Integer(0), opacitySliderLabelsZ[0]);
        labelsZ.put(new Integer(50), opacitySliderLabelsZ[1]);
        labelsZ.put(new Integer(100), opacitySliderLabelsZ[2]);

        opacitySliderZ = new JSlider(0, 100, 100);
        opacitySliderZ.setFont(MipavUtil.font12);
        opacitySliderZ.setMinorTickSpacing(10);
        opacitySliderZ.setPaintTicks(true);
        opacitySliderZ.addChangeListener(this);
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
     * Dispose memory.
     */
    public void disposeLocal() {
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
     * Gets the three slider positions, representing the center point of the
     * three orthogonal planes. The center point is translated from local
     * ModelCoordinates into FileCoordinates.
     * @return, the center of the three orthogonal planes (the three slider
     * positions) in FileCoordinates.
     */
    public Vector3f getCenter( )
    {
        Vector3f center = new Vector3f();
        if ( m_kVolumeViewer != null )
        {
            MipavCoordinateSystems.modelToFile( new Vector3f( xSlice, ySlice, zSlice ), center,
                                                m_kVolumeViewer.getImageA() );
        }        
        return center;
    }

    public int getOpacity( int i )
    {
        if ( i == 0 )
        {
            return opacitySliderX.getValue();
        }
        if ( i == 1 )
        {
            return opacitySliderY.getValue();
        }
        return opacitySliderZ.getValue();
    }

    public void setOpacity( int i, int value )
    {
        if ( i == 0 )
        {
            xOpacitySlice = value;
            opacitySliderX.setValue(value);
            m_kVolumeViewer.setSliceOpacity( 0, xOpacitySlice/100.0f );
        }
        else if ( i == 1 )
        {
            yOpacitySlice = value;
            opacitySliderY.setValue(value);
            m_kVolumeViewer.setSliceOpacity( 1, yOpacitySlice/100.0f );
        }
        else
        {
            zOpacitySlice = value;
            opacitySliderZ.setValue(value);
            m_kVolumeViewer.setSliceOpacity( 2, zOpacitySlice/100.0f );
        }
    }

    public Color getColor( int i )
    {
        return colorButton[i].getBackground();
    }
    public void setColor( int i, Color value )
    {
        colorButton[i].setBackground(value);
        setButtonColor(colorButton[i], value);
    }
    
    public boolean getShowSlice( int i )
    {
        if ( i == 0 )
        {
            return boxX.isSelected();
        }
        if ( i == 1 )
        {
            return boxY.isSelected();
        }
        return boxZ.isSelected();
    }
    
    public void setShowSlice( int i, boolean value )
    {
        if ( i == 0 )
        {
            boxX.setSelected(value);
            m_kVolumeViewer.showSlice( 0, boxX.isSelected() );
            if (!boxX.isSelected()) {
                setXSliderEnabled(false);
                setOpacitySliderXEnabled(false);
                sliceVisible[0] = false;
            } else {
                setXSliderEnabled(true);
                setOpacitySliderXEnabled(true);
                sliceVisible[0] = true;
            }
        }
        else if ( i == 1 )
        {
            boxY.setSelected(value);
            m_kVolumeViewer.showSlice( 1, boxY.isSelected() );
            if (!boxY.isSelected()) {
                setYSliderEnabled(false);
                setOpacitySliderYEnabled(false);
                sliceVisible[1] = false;
            } else {
                setYSliderEnabled(true);
                setOpacitySliderYEnabled(true);
                sliceVisible[1] = true;
            }
        }
        else
        {
            boxZ.setSelected(value);
            m_kVolumeViewer.showSlice( 2, boxZ.isSelected() );
            if (!boxZ.isSelected()) {
                setZSliderEnabled(false);
                setOpacitySliderZEnabled(false);
                sliceVisible[2] = false;
            } else {
                setZSliderEnabled(true);
                setOpacitySliderZEnabled(true);
                sliceVisible[2] = true;
            }
        }
    }
    
    public boolean getShowBound( int i )
    {
        return boundingCheck[i].isSelected();
    }
    
    public void setShowBound( int i, boolean value )
    {
        boundingCheck[i].setSelected(value);
        m_kVolumeViewer.showBoundingBox( i, boundingCheck[i].isSelected() );
        if (boundingCheck[i].isSelected()) {
            colorButton[i].setEnabled(true);
        } else {
            colorButton[i].setEnabled(false);
        }
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
     * slice slider visible or not.
     *
     * @return sliceVisible if <code>true</code> visible, otherwise invisible.
     */
    public boolean getVisible( int orientation )
    {
        return sliceVisible[orientation];
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
     * Get the X slider value.
     *
     * @return  xSlice X slider value.
     */
    public int getXSlice() {
        return xSlice;
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
     * Resizing the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   DOCUMENT ME!
     * @param  frameHeight  DOCUMENT ME!
     */
    public void resizePanel(int panelWidth, int frameHeight) {
        scroller.setPreferredSize(new Dimension(panelWidth, frameHeight - 40));
        scroller.setSize(new Dimension(panelWidth, frameHeight - 40));
        //scroller.revalidate();
    }

    /* (non-Javadoc)
     * @see gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase#setButtonColor(javax.swing.JButton, java.awt.Color)
     */
    public void setButtonColor(JButton _button, Color _color) {

        super.setButtonColor(_button, _color);
        if ( m_kVolumeViewer == null )
        {
            return;
        }
        if ( m_kVolumeViewer != null )
        {
            if (_button == colorButton[0]) {
                m_kVolumeViewer.setSliceHairColor( 0, _color );
            } else if (_button == colorButton[1]) {
                m_kVolumeViewer.setSliceHairColor( 1, _color );
            } else if (_button == colorButton[2]) {
                m_kVolumeViewer.setSliceHairColor( 2, _color );
            }
        }
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
        if ( m_kVolumeViewer != null )
        {
            MipavCoordinateSystems.fileToModel( new Vector3f( x, y, z ), center,
                                                m_kVolumeViewer.getImageA() );
        }

        setXSlicePos( (int)center.X );
        setYSlicePos( (int)center.Y );
        setZSlicePos( (int)center.Z );
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
     * Set probe x coordinate.
     *
     * @param  _xProbe  probe x position
     */
    public void setXProbePos(int _xProbe) {
        xProbe = _xProbe;
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
        if (source == sliderX) {
            if ( xSlice == sliderX.getValue() )
            {
                return;
            }
            // Change the currently displayed x slice
            xSlice = sliderX.getValue();
            textX.setText(String.valueOf(xSlice + 1));
            if ( m_kVolumeViewer != null )
            {
                m_kVolumeViewer.setSliceFromSurface( getCenter() );
            }
        } else if (source == sliderY) {
            if ( ySlice == sliderY.getValue() )
            {
                return;
            }
            // Change the currently displayed y slice
            ySlice = sliderY.getValue();
            textY.setText(String.valueOf(ySlice + 1));
            if ( m_kVolumeViewer != null )
            {
                m_kVolumeViewer.setSliceFromSurface( getCenter() );
            }
        } else if (source == sliderZ) {
            if ( zSlice == sliderZ.getValue() )
            {
                return;
            }

            // Change the currently displayed z slice
            zSlice = sliderZ.getValue();
            textZ.setText(String.valueOf(zSlice + 1));
            if ( m_kVolumeViewer != null )
            {
                m_kVolumeViewer.setSliceFromSurface( getCenter() );
            }
        } else if (source == sliderT) {

            // Change the currently displayed t slice
            tSlice = sliderT.getValue() - 1;
            textT.setText(String.valueOf(tSlice + 1));
        } else if (source == opacitySliderX) {
            xOpacitySlice = opacitySliderX.getValue();
            yOpacitySlice = opacitySliderY.getValue();
            zOpacitySlice = opacitySliderZ.getValue();
            if ( m_kVolumeViewer != null )
            {
                m_kVolumeViewer.setSliceOpacity( 0, xOpacitySlice/100.0f );
            }
        } else if (source == opacitySliderY) {
            xOpacitySlice = opacitySliderX.getValue();
            yOpacitySlice = opacitySliderY.getValue();
            zOpacitySlice = opacitySliderZ.getValue();
            if ( m_kVolumeViewer != null )
            {
                m_kVolumeViewer.setSliceOpacity( 1, yOpacitySlice/100.0f );
            }
        } else if (source == opacitySliderZ) {
            xOpacitySlice = opacitySliderX.getValue();
            yOpacitySlice = opacitySliderY.getValue();
            zOpacitySlice = opacitySliderZ.getValue();
            if ( m_kVolumeViewer != null )
            {
                m_kVolumeViewer.setSliceOpacity( 2, zOpacitySlice/100.0f );
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
            boundingCheck[i].setFont(MipavUtil.font12);
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

        Hashtable<Integer,JLabel> labelTableT = new Hashtable<Integer,JLabel>();

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

        label.setFont(MipavUtil.font12);
        label.setForeground(Color.black);

        return label;
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
     * Set the current z slider move position.
     *
     * @param  _zSlice  z slider position
     */
    private void setZSlicePos(int _zSlice) {
        zSlice = _zSlice;
        sliderZ.setValue(zSlice);
        textZ.setText(String.valueOf(zSlice + 1));
    }
}
