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

    public static final int CLIP_X = 0;
    public static final int CLIP_X_INV = 1;
    public static final int CLIP_Y = 2;
    public static final int CLIP_Y_INV = 3;
    public static final int CLIP_Z = 4;
    public static final int CLIP_Z_INV = 5;
    public static final int CLIP_EYE = 6;
    public static final int CLIP_EYE_INV = 7;
    public static final int CLIP_A = 8;
    public static final int MAX_CLIP_PLANES = 9;

    private int[] clipValue = new int[MAX_CLIP_PLANES];

    private JCheckBox[] enableClip = new JCheckBox[MAX_CLIP_PLANES];
    private JCheckBox[] displayClip = new JCheckBox[MAX_CLIP_PLANES];

    private JSlider[] clipSlider = new JSlider[MAX_CLIP_PLANES];

    private JButton[] clipColor = new JButton[MAX_CLIP_PLANES];

    private JButton[] extract = new JButton[MAX_CLIP_PLANES];

    private JPanel[] clipPanel = new JPanel[MAX_CLIP_PLANES];

    private JTextField[] clipText = new JTextField[MAX_CLIP_PLANES];

    private JLabel[] clipLabel = new JLabel[MAX_CLIP_PLANES];
    private JLabel[] labelStart = new JLabel[MAX_CLIP_PLANES];
    private JLabel[] labelMid = new JLabel[MAX_CLIP_PLANES];
    private JLabel[] labelEnd = new JLabel[MAX_CLIP_PLANES];

    /** The scroll pane holding the panel content. Used when the screen is small. */
    private JScrollPane scroller;

    /** Scroll panel that holding all the control components. */
    private JPanel scrollPanel;

    /** Tabbed Panel that hold the each clipping planes control box. */
    private JTabbedPane tabbedPane;


    /**
     * 3D clipping dialog control.
     * @param kVolumeViewer parent frame.
     */
    public JPanelClip_WM(VolumeTriPlanarInterface kVolumeViewer) {
        super(kVolumeViewer);
        clipValue[CLIP_X_INV] = kVolumeViewer.getImageA().getExtents()[0];
        clipValue[CLIP_X] = 0;
        clipValue[CLIP_Y_INV] = kVolumeViewer.getImageA().getExtents()[1];
        clipValue[CLIP_Y] = 0;
        clipValue[CLIP_Z_INV] = kVolumeViewer.getImageA().getExtents()[2];
        clipValue[CLIP_Z] = 0;
        clipValue[CLIP_EYE_INV] = kVolumeViewer.getImageA().getExtents()[2];
        clipValue[CLIP_EYE] = 0;
        clipValue[CLIP_A] = clipValue[CLIP_X_INV];//(int)(Math.max( clipValue[CLIP_X_INV], Math.max( clipValue[CLIP_Y_INV], clipValue[CLIP_Z_INV]) ) );

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

//        if ((source == extractButtonA) || (source == extractButtonS)) {
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

        if (source instanceof JButton) {
            colorChooser = new ViewJColorChooser(new Frame(), "Pick color", new OkColorListener((JButton) source),
                                                 new CancelListener());
        }
        for ( int i = 0; i < MAX_CLIP_PLANES; i++ )
        {
            if (source == displayClip[i])
            {
                displayClip(i);
            }
            else if ( source == enableClip[i] )
            {
                enableClip(i);
            }
        }
    }


    /**
     * Build x slider control panel.
     */
    public void buildPanel( int i, String kName, int iMax, Color kColor )
    {
        GridBagLayout cpGBL = new GridBagLayout();
        GridBagConstraints cpGBC = new GridBagConstraints();

        cpGBC.fill = GridBagConstraints.NONE;
        cpGBC.weightx = 100;
        cpGBC.weighty = 100;
        clipPanel[i] = new JPanel();
        clipPanel[i].setBounds(10, 100, 100, 120);
        clipPanel[i].setLayout(cpGBL);

        enableClip[i] = new JCheckBox();
        enableClip[i].addActionListener(this);
        enableClip[i].setActionCommand(kName);
        enableClip[i].setText(kName + " Slider");
        enableClip[i].setFont(MipavUtil.font12);
        addControlPanel(clipPanel[i], enableClip[i], cpGBC, 0, 0, 1, 1);
        enableClip[i].setSelected(false);

        cpGBC.fill = GridBagConstraints.BOTH;
        clipLabel[i] = new JLabel(kName + " (1 - " + String.valueOf(iMax) + ")");
        clipLabel[i].setForeground(Color.black);
        clipLabel[i].setFont(MipavUtil.font12);
        addControlPanel(clipPanel[i], clipLabel[i], cpGBC, 0, 1, 2, 1);
        clipLabel[i].setEnabled(false);

        clipSlider[i] = new JSlider(1, iMax, iMax);
        clipSlider[i].setFont(MipavUtil.font12);
        clipSlider[i].setMinorTickSpacing(iMax / 10);
        clipSlider[i].setPaintTicks(true);
        //xSlice = clipSlider[i].getValue() - 1;
        clipSlider[i].setVisible(true);
        clipSlider[i].setEnabled(false);
        if ( !kName.contains("-") )
        {
            clipSlider[i].setValue(0);
        }
        clipSlider[i].addChangeListener(this);

        labelStart[i] = new JLabel("1");
        labelStart[i].setForeground(Color.black);
        labelStart[i].setFont(MipavUtil.font12);
        labelStart[i].setEnabled(false);
        labelMid[i] = new JLabel(String.valueOf((iMax + 1) / 2));
        labelMid[i].setForeground(Color.black);
        labelMid[i].setFont(MipavUtil.font12);
        labelMid[i].setEnabled(false);
        labelEnd[i] = new JLabel(String.valueOf(iMax));
        labelEnd[i].setForeground(Color.black);
        labelEnd[i].setFont(MipavUtil.font12);
        labelEnd[i].setEnabled(false);

        Hashtable<Integer, JLabel> labelTable = new Hashtable<Integer, JLabel>();

        labelTable.put(new Integer(1), labelStart[i]);
        labelTable.put(new Integer((iMax + 1) / 2), labelMid[i]);
        labelTable.put(new Integer(iMax), labelEnd[i]);
        clipSlider[i].setLabelTable(labelTable);
        clipSlider[i].setPaintLabels(true);
        addControlPanel(clipPanel[i], clipSlider[i], cpGBC, 2, 1, 8, 1);

        clipText[i] = new JTextField(String.valueOf(iMax), 4);
        clipText[i].setFont(MipavUtil.font12);
        clipText[i].setEnabled(false);
        cpGBC.fill = GridBagConstraints.NONE;
        addControlPanel(clipPanel[i], clipText[i], cpGBC, 10, 1, 1, 1);

        // add bounding box
        displayClip[i] = new JCheckBox(kName + " Frame");
        displayClip[i].setFont(MipavUtil.font12);
        displayClip[i].addActionListener(this);
        addControlPanel(clipPanel[i], displayClip[i], cpGBC, 0, 2, 1, 1);

        clipColor[i] = new JButton();
        clipColor[i].setPreferredSize(new Dimension(25, 25));
        clipColor[i].setToolTipText("Change "+ kName + " clip frame color");
        clipColor[i].addActionListener(this);
        clipColor[i].setBackground(kColor);
        clipColor[i].setEnabled(false);
        addControlPanel(clipPanel[i], clipColor[i], cpGBC, 1, 2, 2, 1);
        tabbedPane.addTab(kName, null, clipPanel[i]);
    }

    /**
     * Disable the 6 clipping planes.
     */
    public void disable6Planes() {
        for ( int i = 0; i < 6; i++ )
        {
            setSliderEnabled(i, false);
            enableClip[i].setSelected(false);
            displayClip[i].setSelected(false);
            clipColor[i].setEnabled(false);
        }
        if ( rayBasedRenderWM != null )
        {
            for ( int i = 0; i < 6; i++ )
            {
                rayBasedRenderWM.enableClipPlane( i, enableClip[i].isSelected(), displayClip[i].isSelected() );
            }
        }
    }

    /**
     * Disable the arbitrary clipping.
     */
    public void disableClipA() {
        setSliderEnabled(CLIP_A, false);
        enableClip[CLIP_A].setSelected(false);
        displayClip[CLIP_A].setSelected(false);
        clipColor[CLIP_A].setEnabled(false);
    }

    /**
     * Disable clipping planes when dialog window closed.
     */
    public void disableClipPlanes() {
        for ( int i = 0; i < 6; i++ )
        {
            if (enableClip[i].isSelected() || displayClip[i].isSelected()) {
                setSliderEnabled(i, false);
                enableClip[i].setSelected(false);
                displayClip[i].setSelected(false);
                clipColor[i].setEnabled(false);
            }
        }
    }

    /**
     * Disable arbitrary clipping planes.
     */
    public void disableClipPlanesArbi() {

        if ( enableClip[CLIP_A].isSelected() || displayClip[CLIP_A].isSelected()) {
            setSliderEnabled(CLIP_A, false);
            enableClip[CLIP_A].setSelected(false);
            displayClip[CLIP_A].setSelected(false);
            clipColor[CLIP_A].setEnabled(false);
            if ( rayBasedRenderWM != null )
            {
                rayBasedRenderWM.enableArbitraryClipPlane( enableClip[CLIP_A].isSelected(),
                                                           displayClip[CLIP_A].isSelected(),
                                                           new ColorRGB( clipColor[CLIP_A].getBackground().getRed(),
                                                                         clipColor[CLIP_A].getBackground().getGreen(),
                                                                         clipColor[CLIP_A].getBackground().getBlue() ) );
//                 rayBasedRenderWM.enableEyeClipPlane( boxStatic.isSelected(), boundingCheckStatic.isSelected(),
//                         new ColorRGB( colorButtonStatic.getBackground().getRed(),
//                                 colorButtonStatic.getBackground().getGreen(),
//                                 colorButtonStatic.getBackground().getBlue() ) );
            }
        }
    }

    private void displayClip( int i )
    {
        if ( i == CLIP_A )
        {
            clipValue[CLIP_A] = clipSlider[CLIP_A].getValue() - 1;
            if ( rayBasedRenderWM != null )
            {
                rayBasedRenderWM.setArbitraryClipPlane(clipValue[CLIP_A], enableClip[CLIP_A].isSelected());
                rayBasedRenderWM.enableArbitraryClipPlane( enableClip[CLIP_A].isSelected(), displayClip[CLIP_A].isSelected(),
                                                           new ColorRGB( clipColor[CLIP_A].getBackground().getRed(),
                                                                         clipColor[CLIP_A].getBackground().getGreen(),
                                                                         clipColor[CLIP_A].getBackground().getBlue() ) );
            }
        }
        else if ( i == CLIP_EYE )
        {
            clipValue[CLIP_EYE] = clipSlider[CLIP_EYE].getValue() - 1;
            rayBasedRenderWM.setEyeClipPlane( clipValue[CLIP_EYE], displayClip[CLIP_EYE].isSelected(),
                                              enableClip[CLIP_EYE].isSelected());
            rayBasedRenderWM.enableEyeClipPlane( enableClip[CLIP_EYE].isSelected(), displayClip[CLIP_EYE].isSelected(),
                                                 new ColorRGB( clipColor[CLIP_EYE].getBackground().getRed(),
                                                               clipColor[CLIP_EYE].getBackground().getGreen(),
                                                               clipColor[CLIP_EYE].getBackground().getBlue() ) );
        }
        else if ( i == CLIP_EYE_INV )
        {
            clipValue[CLIP_EYE_INV] = clipSlider[CLIP_EYE_INV].getValue() - 1;
            rayBasedRenderWM.setEyeInvClipPlane( clipValue[CLIP_EYE_INV],
                                                 displayClip[CLIP_EYE_INV].isSelected(),
                                                 enableClip[CLIP_EYE_INV].isSelected());
            rayBasedRenderWM.enableEyeInvClipPlane( enableClip[CLIP_EYE_INV].isSelected(), 
                                                    displayClip[CLIP_EYE_INV].isSelected(),
                                                    new ColorRGB( clipColor[CLIP_EYE_INV].getBackground().getRed(),
                                                                  clipColor[CLIP_EYE_INV].getBackground().getGreen(),
                                                                  clipColor[CLIP_EYE_INV].getBackground().getBlue() ) );
        }
        else
        {
            if (displayClip[i].isSelected())
            {
                clipColor[i].setEnabled(true);
                if ( rayBasedRenderWM != null )
                {
                    rayBasedRenderWM.displayClipPlane( i, true, 
                            new ColorRGB( clipColor[i].getBackground().getRed(),
                                    clipColor[i].getBackground().getGreen(),
                                    clipColor[i].getBackground().getBlue() ) );
                }
            }
            else
            {
                clipColor[i].setEnabled(false);
                if ( rayBasedRenderWM != null )
                {
                    rayBasedRenderWM.displayClipPlane( i, false );
                }
            }
        }
    }
    
    private void enableClip( int i )
    {
        if ( i == CLIP_A )
        {
            if (!enableClip[CLIP_A].isSelected()) {
                setSliderEnabled(CLIP_A, false);
                clipColor[CLIP_A].setEnabled(false);
                displayClip[CLIP_A].setSelected(false);
                disableClipPlanesArbi();
            } else {
                setSliderEnabled(CLIP_A, true);
                disableClipPlanes();
                clipColor[CLIP_A].setEnabled(true);
            }
            if ( rayBasedRenderWM != null )
            {
                clipValue[CLIP_A] = clipSlider[CLIP_A].getValue() - 1;
                rayBasedRenderWM.setArbitraryClipPlane(clipValue[CLIP_A], enableClip[CLIP_A].isSelected());
                rayBasedRenderWM.enableArbitraryClipPlane( enableClip[CLIP_A].isSelected(), displayClip[CLIP_A].isSelected(),
                                                           new ColorRGB( clipColor[CLIP_A].getBackground().getRed(),
                                                                         clipColor[CLIP_A].getBackground().getGreen(),
                                                                         clipColor[CLIP_A].getBackground().getBlue() ) );
            }
        }
        else if ( i == CLIP_EYE )
        {
            if (!enableClip[CLIP_EYE].isSelected()) {
                setSliderEnabled(CLIP_EYE, false);
            } else {
                disableClipPlanes();
                disableClipPlanesArbi();
                setSliderEnabled(CLIP_EYE, true);
            }
            if ( rayBasedRenderWM != null )
            {
                clipValue[CLIP_EYE] = clipSlider[CLIP_EYE].getValue() - 1;
                rayBasedRenderWM.setEyeClipPlane(clipValue[CLIP_EYE], displayClip[CLIP_EYE].isSelected(),
                                                 enableClip[CLIP_EYE].isSelected());
                rayBasedRenderWM.enableEyeClipPlane( enableClip[CLIP_EYE].isSelected(), displayClip[CLIP_EYE].isSelected(),
                            new ColorRGB( clipColor[CLIP_EYE].getBackground().getRed(),
                                          clipColor[CLIP_EYE].getBackground().getGreen(),
                                          clipColor[CLIP_EYE].getBackground().getBlue() ) );
            }
        }
        else if ( i == CLIP_EYE_INV )
        {
            if (!enableClip[CLIP_EYE_INV].isSelected()) {
                setSliderEnabled(CLIP_EYE_INV, false);
            } else {
                disableClipPlanes();
                disableClipPlanesArbi();
                setSliderEnabled(CLIP_EYE_INV, true);
            }
            if ( rayBasedRenderWM != null )
            {
                clipValue[CLIP_EYE_INV] = clipSlider[CLIP_EYE_INV].getValue() - 1;
                rayBasedRenderWM.setEyeInvClipPlane(clipValue[CLIP_EYE_INV], displayClip[CLIP_EYE_INV].isSelected(),
                                                 enableClip[CLIP_EYE_INV].isSelected());
                rayBasedRenderWM.enableEyeInvClipPlane( enableClip[CLIP_EYE_INV].isSelected(),
                                                     displayClip[CLIP_EYE_INV].isSelected(),
                            new ColorRGB( clipColor[CLIP_EYE_INV].getBackground().getRed(),
                                          clipColor[CLIP_EYE_INV].getBackground().getGreen(),
                                          clipColor[CLIP_EYE_INV].getBackground().getBlue() ) );
            }
        }
        else
        {
            if (!enableClip[i].isSelected())
            {
                setSliderEnabled(i, false);
                displayClip[i].setSelected(false);
                clipColor[i].setEnabled(false);
            } else {
                setSliderEnabled(i, true);
                disableClipPlanesArbi();
            }
            if ( rayBasedRenderWM != null )
            {
                rayBasedRenderWM.enableClipPlane( i, enableClip[i].isSelected(), displayClip[i].isSelected() );
            }
        }
    }
    
    /**
     * Dispose memory.
     */
    public void disposeLocal() {
        for ( int i = 0; i < MAX_CLIP_PLANES; i++ )
        {
            clipSlider[i] = null;
            enableClip[i] = null;
            displayClip[i] = null;
            clipLabel[i] = null;
            labelStart[i] = null;
            labelMid[i] = null;
            labelEnd[i] = null;
            clipText[i] = null;
            clipColor[i] = null;
            clipPanel[i] = null;
        }
        colorChooser = null;
        tabbedPane = null;
    }

    public boolean[] getClipEnabled()
    {
        boolean[] abEnabled = new boolean[9];
        for ( int i = 0; i < MAX_CLIP_PLANES; i++ )
        {
            abEnabled[i] = enableClip[i].isSelected();
        }
        return abEnabled;
    }
    
    public void setClipEnabled(boolean[] abEnabled)
    {
        for ( int i = 0; i < MAX_CLIP_PLANES; i++ )
        {
            enableClip[i].setSelected(abEnabled[i]);
            enableClip(i);
        }
    }


    public boolean[] getClipDisplayed()
    {
        boolean[] abDisplayed = new boolean[9];
        for ( int i = 0; i < MAX_CLIP_PLANES; i++ )
        {
            abDisplayed[i] = displayClip[i].isSelected();
        }
        return abDisplayed;
    }

    public void setClipDisplayed(boolean[] abDisplayed)
    {
        for ( int i = 0; i < MAX_CLIP_PLANES; i++ )
        {
            displayClip[i].setSelected(abDisplayed[i]);
            displayClip(i);
        }
    }
    
    public int[] getClipValues()
    {       
        int[] aiValues = new int[9];
        for ( int i = 0; i < MAX_CLIP_PLANES; i++ )
        {
            aiValues[i] = clipValue[i];
        }
        return aiValues;
    }

    public void setClipValues(int[] newValues )
    {
        for ( int i = 0; i < MAX_CLIP_PLANES; i++ )
        {
            clipValue[i] = newValues[i];
            clipSlider[i].setValue(clipValue[i] + 1);
            clipText[i].setText(String.valueOf(clipValue[i] + 1));

            if ( rayBasedRenderWM != null )
            {
                if ( i == CLIP_EYE )
                {
                    rayBasedRenderWM.setEyeClipPlane(clipValue[CLIP_EYE], displayClip[CLIP_EYE].isSelected(),
                            enableClip[CLIP_EYE].isSelected());
                }
                else if ( i == CLIP_EYE_INV )
                {
                    rayBasedRenderWM.setEyeInvClipPlane(clipValue[CLIP_EYE_INV], displayClip[CLIP_EYE_INV].isSelected(),
                            enableClip[CLIP_EYE_INV].isSelected());
                }
                else if ( i == CLIP_A )
                {
                    rayBasedRenderWM.setArbitraryClipPlane(clipValue[CLIP_A], enableClip[CLIP_A].isSelected());
                }
                else
                {
                    rayBasedRenderWM.setClipPlane( i, clipValue[i], enableClip[i].isSelected() );
                }
            }
        }
    }
    
    public Color[] getClipColors()
    {
        Color[] akColors = new Color[9];
        for ( int i = 0; i < MAX_CLIP_PLANES; i++ )
        {
            akColors[i] = clipColor[i].getBackground();
        }
        return akColors;
    }
    
    public void setClipColors(Color[] akColors)
    {
        for ( int i = 0; i < MAX_CLIP_PLANES; i++ )
        {
            clipColor[i].setBackground(akColors[i]);
            setClipColor( i, akColors[i] );
        }
    }
    
    
    /**
     * Return check box arbitrary selection value.
     * @return  boxA.isSelected() box arbitrary selection value.
     */
    public boolean getAVisible() {
        return enableClip[CLIP_A].isSelected();
    }

    /**
     * Initializes GUI components.
     */
    public void init() {

        // setTitle("Clipping Planes");
        Box contentBox = new Box(BoxLayout.Y_AXIS);

        tabbedPane = new JTabbedPane();
        tabbedPane.setFont(MipavUtil.font12B);

        buildPanel(CLIP_X, "+X", clipValue[CLIP_X_INV], Color.yellow );
        buildPanel(CLIP_X_INV, "-X", clipValue[CLIP_X_INV], Color.yellow );
        buildPanel(CLIP_Y, "+Y", clipValue[CLIP_Y_INV], Color.green );
        buildPanel(CLIP_Y_INV, "-Y", clipValue[CLIP_Y_INV], Color.green );
        buildPanel(CLIP_Z, "+Z", clipValue[CLIP_Z_INV], Color.red );
        buildPanel(CLIP_Z_INV, "-Z", clipValue[CLIP_Z_INV], Color.red );

        buildPanel( CLIP_A, "-A", clipValue[CLIP_A], new Color(0.73f, 0.70f, 0.86f) );
        buildPanel(CLIP_EYE, "+EYE", clipValue[CLIP_EYE_INV], Color.orange);
        buildPanel(CLIP_EYE_INV, "-EYE", clipValue[CLIP_EYE_INV], new Color(1.0f, 0.64f, 1.0f) );

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

        for ( int i = 0; i < 6; i++ )
        {
            setSliderEnabled(i, true);
        }
        disableClipPlanesArbi();


        if ( rayBasedRenderWM != null )
        {
            for ( int i = 0; i < 6; i++ )
            {
                displayClip[i].setSelected(true);
                enableClip[i].setSelected(true);
                clipColor[i].setEnabled(true);
                clipSlider[i].setEnabled(true);

                rayBasedRenderWM.displayClipPlane( i, true, 
                                                   new ColorRGB( clipColor[i].getBackground().getRed(),
                                                                 clipColor[i].getBackground().getGreen(),
                                                                 clipColor[i].getBackground().getBlue() ) );
                rayBasedRenderWM.setClipPlane( i, clipValue[i], enableClip[i].isSelected() );
            }
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
     * @param i index.
     * @param color new color.
     */
    public void setClipColor( int i, Color color)
    {
        if ( rayBasedRenderWM != null )
        {
            rayBasedRenderWM.setClipPlaneColor( i, new ColorRGB( color.getRed(),
                    color.getGreen(),
                    color.getBlue() ) );
        }
    }


    /**
     * Sets the x slider and the labels beside and beneath it to the state given by <code>flag</code>.
     * @param  flag  if <code>true</code> enable, otherwise disable.
     */
    public void setSliderEnabled(int i, boolean flag) {
        clipSlider[i].setEnabled(flag);
        clipLabel[i].setEnabled(flag);
        labelStart[i].setEnabled(flag);
        labelMid[i].setEnabled(flag);
        labelEnd[i].setEnabled(flag);
    }


    /**
     * Sets how the image plane should be displayed depending on value of slider.
     * @param  e  Event that triggered this function.
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        for ( int i = 0; i < 6; i++ )
        {
            if (source == clipSlider[i]) {
                clipValue[i] = clipSlider[i].getValue() - 1;
                clipText[i].setText(String.valueOf(clipValue[i] + 1));

                if ( rayBasedRenderWM != null )
                {
                    rayBasedRenderWM.setClipPlane( i, clipValue[i], enableClip[i].isSelected() );
                }
            }
        }
        if ( source == clipSlider[CLIP_EYE] )
        {
            clipValue[CLIP_EYE] = clipSlider[CLIP_EYE].getValue() - 1;
            rayBasedRenderWM.setEyeClipPlane(clipValue[CLIP_EYE], displayClip[CLIP_EYE].isSelected(),
                    enableClip[CLIP_EYE].isSelected());
        }
        else if ( source == clipSlider[CLIP_EYE_INV] )
        {
            clipValue[CLIP_EYE_INV] = clipSlider[CLIP_EYE_INV].getValue() - 1;
            rayBasedRenderWM.setEyeInvClipPlane(clipValue[CLIP_EYE_INV], displayClip[CLIP_EYE_INV].isSelected(),
                    enableClip[CLIP_EYE_INV].isSelected());
        }
        else if ( source == clipSlider[CLIP_A] )
        {
            clipValue[CLIP_A] = clipSlider[CLIP_A].getValue() - 1;
            rayBasedRenderWM.setArbitraryClipPlane(clipValue[CLIP_A], enableClip[CLIP_A].isSelected());
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
        for ( int i = 0;  i < MAX_CLIP_PLANES; i++ )
        {
            if (_button == clipColor[i] )
            {
                setClipColor( i, _color );
                break;
            }
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
