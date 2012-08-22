package gov.nih.mipav.view.renderer.J3D;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.*;
import gov.nih.mipav.view.renderer.WildMagic.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;
import javax.swing.event.*;

import javax.vecmath.*;


/**
 * Dialog to turn slices bounding box of surface renderer on and off, and to change the color of the frame. This dialog
 * also control the X, Y, Z slices movements.
 */
public class JPanelLights extends JPanelRendererJ3D implements ChangeListener, MouseListener, ListSelectionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 8549491288085975699L;

    /** Static light index. */
    public static final int LIGHT_INDEX_STATIC = 0;

    /** Ambient light index. */
    public static final int LIGHT_INDEX_AMBIENT = 1;

    /** light for corner X0Y0Z0. */
    public static final int LIGHT_INDEX_MODEL_X0Y0Z0 = 2;

    /** light for corner X1Y0Z0. */
    public static final int LIGHT_INDEX_MODEL_X1Y0Z0 = 3;

    /** light for corner X0Y1Z0. */
    public static final int LIGHT_INDEX_MODEL_X0Y1Z0 = 4;

    /** light for corner X1Y1Z0. */
    public static final int LIGHT_INDEX_MODEL_X1Y1Z0 = 5;

    /** light for corner X0Y0Z1. */
    public static final int LIGHT_INDEX_MODEL_X0Y0Z1 = 6;

    /** light for corner X1Y0Z1. */
    public static final int LIGHT_INDEX_MODEL_X1Y0Z1 = 7;

    /** light for corner X0Y1Z1. */
    public static final int LIGHT_INDEX_MODEL_X0Y1Z1 = 8;

    /** light for corner X1Y1Z1. */
    public static final int LIGHT_INDEX_MODEL_X1Y1Z1 = 9;

    /** Max number of light. */
    public static final int LIGHT_INDEX_MAX = 10;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Radio button for different light type. */
    private JRadioButton ambientRadio;

    /**
     * Flag is set if one of the controls changed a value and the renderering needs to be redrawn when the mouse is
     * released.
     */
    private boolean bNeedsRedraw = false;

    /** Check box to show the light bulb or not. */
    private JCheckBox checkBoxShowLight;

    /** Color button, checkBox On/Off. */
    private JButton colorButton;

    /** Color choose dialog reference. */
    private ViewJColorChooser colorChooser;

    /** Color label. */
    private JLabel colorLabel;

    /** Control panel. */
    private JPanel controlPanel;

    /** Control panel box that hold the control panel. */
    private Box controlPanelBox;

    /** Directional light radio botton. */
    private JRadioButton directionalRadio;

    /** Scroll panel reference. */
    private JPanel drawPanel;

    /** Intensity slider. */
    private JSlider intensitySlider;

    /** index of currently selected one. */
    private int iSelect = 0;

    /** Light intensity label. */
    private JLabel labelIntensity;

    /** Light intensity label begin. */
    private JLabel labelIntensityBegin;

    /** Light intensity label end. */
    private JLabel labelIntensityEnd;

    /** Light intensity label middle. */
    private JLabel labelIntensityMid;

    /** Light shinness label. */
    private JLabel labelShininess;

    /** Shinness label begin. */
    private JLabel labelShininessBegin;

    /** Shinness label end. */
    private JLabel labelShininessEnd;

    /** Shinness lable middle. */
    private JLabel labelShininessMid;

    /** List of lights. */
    private JList list;

    /** Light scale factor array. */
    private int[] m_aiLightScale;

    /** The structure for the light bulbs. */
    private GeneralLight[] m_akLights;

    /** General light array. */
    private GeneralLight[] m_akModelLights;

    /** Soft light array. */
    private SoftwareLight[] m_akSoftwareLightsModel = new SoftwareLight[0];

    /** Lighting objects needed for software rendering. */
    private SoftwareLight[] m_akSoftwareLightsWorld = new SoftwareLight[0];

    /** World general light array. */
    private GeneralLight[] m_akWorldLights;

    /** Label for slider X position. */
    private JLabel m_kLabelPosX;

    /** Label for slider Y position. */
    private JLabel m_kLabelPosY;

    /** Label for slider Z position. */
    private JLabel m_kLabelPosZ;

    /** Label for X light target position. */
    private JLabel m_kLabelTrgX;

    /** Label for Y light target position. */
    private JLabel m_kLabelTrgY;

    /** Label for Z light target position.*/
    private JLabel m_kLabelTrgZ;

    /** surface panel reference. */
    private JPanelSurface m_kPanelSurface;

    /** Parent render frame reference. */
    private SurfaceRender m_kRenderSurface;

    /** Light x position slider. */
    private JSlider m_kSliderPosX;

    /** Light y position slider. */
    private JSlider m_kSliderPosY;

    /**  Light z position slider. */
    private JSlider m_kSliderPosZ;

    /**  Light x target position slider. */
    private JSlider m_kSliderTrgX;

    /** Light y target position slider.  */
    private JSlider m_kSliderTrgY;

    /** Light z target position slider. */
    private JSlider m_kSliderTrgZ;

    /** X position text field. */
    private JTextField m_kTextPosX;

    /** Y position text field. */
    private JTextField m_kTextPosY;

    /** Z position text field. */
    private JTextField m_kTextPosZ;

    /** X target position text field. */
    private JTextField m_kTextTrgX;

    /** Y target position text field. */
    private JTextField m_kTextTrgY;

    /** Z target position text field. */
    private JTextField m_kTextTrgZ;

    /** Light turn on/off check box. */
    private JCheckBox onOffCheckBox;

    /** Light turn on/off label. */
    private JLabel onOffLabel;

    /** Point light radio button. */
    private JRadioButton pointRadio;

    /** Scroll pane. */
    private JScrollPane scroller;

    /** Scroll the control panel when the frame changes size. */
    private JPanel scrollPanel;

    /** Shininess panel. */
    private JPanel shininessPanel;

    /** Shininess slider. */
    private JSlider shininessSlider;

    /** Spot light radio button. */
    private JRadioButton spotRadio;

    /** Light intensity textfield. */
    private JTextField textIntensity;

    /** Light shinness textfield. */
    private JTextField textShininess;

    /** x, y, z box size. */
    private float xBox, yBox, zBox, maxBox;

    private VolumeTriPlanarRender m_kGPUVolumeRender = null;


    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor.
     *
     * @param  kPanelSurface   JPanelSurface Reference to the panel which contains the surface view. This may be a null
     *                         reference to indicate that there is no corresponding surface view panel.
     * @param  kRenderSurface  SurfaceRender Reference to view which contains the surface rendered view.
     */
    public JPanelLights(JPanelSurface kPanelSurface, SurfaceRender kRenderSurface) {

        super(kRenderSurface);
        m_kPanelSurface = kPanelSurface;
        m_kRenderSurface = kRenderSurface;

        ModelImage image = renderBase.getImageA();
        int xDim = image.getExtents()[0];
        int yDim = image.getExtents()[1];
        int zDim = image.getExtents()[2];
        float xScale = Math.abs(image.getResolutions(0)[0]);
        float yScale = Math.abs(image.getResolutions(0)[1]);
        float zScale = Math.abs(image.getResolutions(0)[2]);

        if ((xScale == 0.0f) || (yScale == 0.0f) || (zScale == 0.0f)) {
            xScale = 1.0f;
            yScale = 1.0f;
            zScale = 1.0f;
        }

        xBox = (xDim - 1) * xScale;
        yBox = (yDim - 1) * yScale;
        zBox = (zDim - 1) * zScale;
        maxBox = xBox;

        if (yBox > maxBox) {
            maxBox = yBox;
        }

        if (zBox > maxBox) {
            maxBox = zBox;
        }

        // Setup for the lights.
        m_aiLightScale = new int[LIGHT_INDEX_MAX];
        m_akLights = new GeneralLight[LIGHT_INDEX_MAX];

        GeneralLight kLight;

        // Ambient light for model.
        kLight = new GeneralLight(GeneralLight.MASK_AMBIENT, xBox / 2.0f, yBox / 2.0f, zBox / 2.0f);
        kLight.setDescription("Ambient Light");
        kLight.setIntensity(0.6f);
        kLight.setTarget(0.0f, 0.0f, 0.0f);
        kLight.setTypeAmbient();
        kLight.enable(true);
        m_akLights[LIGHT_INDEX_AMBIENT] = kLight;
        m_aiLightScale[LIGHT_INDEX_AMBIENT] = 1;

        // Model lights at corners of the volume.
        for (int i = 0; i < 8; i++) {
            float fX = ((0 != (i & 1)) ? +1.0f : -1.0f);
            float fY = ((0 != (i & 2)) ? +1.0f : -1.0f);
            float fZ = ((0 != (i & 4)) ? +1.0f : -1.0f);
            int iTypeMask = GeneralLight.MASK_ALL ^ GeneralLight.MASK_AMBIENT;
            kLight = new GeneralLight(iTypeMask, xBox / 2.0f, yBox / 2.0f, zBox / 2.0f);
            kLight.setDescription(new String("Object Light " + i));
            kLight.setTypeDirectional();
            kLight.setPosition(fX, fY, fZ);
            kLight.setTarget(0.0f, 0.0f, 0.0f);
            m_akLights[LIGHT_INDEX_MODEL_X0Y0Z0 + i] = kLight;
            m_aiLightScale[LIGHT_INDEX_MODEL_X0Y0Z0 + i] = 1;
        }

        // Directional light for world.
        kLight = new GeneralLight(GeneralLight.MASK_ALL ^ GeneralLight.MASK_AMBIENT, maxBox / 2.0f, maxBox / 2.0f,
                                  maxBox / 2.0f);
        kLight.setDescription("Static Light");
        kLight.setTypeDirectional();
        kLight.enable(true);
        kLight.setPosition(0.00f, 0.00f, 3.0f);
        kLight.setTarget(0.0f, 0.0f, 0.0f);
        m_akLights[LIGHT_INDEX_STATIC] = kLight;
        m_aiLightScale[LIGHT_INDEX_STATIC] = 3;


        // Create world/model lights.
        m_akWorldLights = new GeneralLight[] { m_akLights[LIGHT_INDEX_STATIC], };
        m_akModelLights = new GeneralLight[] {
                              m_akLights[LIGHT_INDEX_AMBIENT], m_akLights[LIGHT_INDEX_MODEL_X0Y0Z0],
                              m_akLights[LIGHT_INDEX_MODEL_X1Y0Z0], m_akLights[LIGHT_INDEX_MODEL_X0Y1Z0],
                              m_akLights[LIGHT_INDEX_MODEL_X1Y1Z0], m_akLights[LIGHT_INDEX_MODEL_X0Y0Z1],
                              m_akLights[LIGHT_INDEX_MODEL_X1Y0Z1], m_akLights[LIGHT_INDEX_MODEL_X0Y1Z1],
                              m_akLights[LIGHT_INDEX_MODEL_X1Y1Z1],
                          };

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        drawPanel = new JPanel(new BorderLayout());

        // Put the drawing area in a scroll pane.
        scroller = new JScrollPane(drawPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());

        // initialize GUI components
        Box contentBox = new Box(BoxLayout.Y_AXIS);
        Box centerBox = new Box(BoxLayout.Y_AXIS);
        controlPanelBox = new Box(BoxLayout.Y_AXIS);

        Box leftBox = new Box(BoxLayout.Y_AXIS);

        // build list panel
        buildListPanel();
        leftBox.add(scrollPanel);
        centerBox.add(leftBox);

        // build control panel
        buildControlPanel();
        controlPanelBox.add(controlPanel);

        // build shininess panel
        buildShininessPanel();
        controlPanelBox.add(shininessPanel);

        centerBox.add(controlPanelBox);
        contentBox.add(centerBox);
        drawPanel.add(contentBox, BorderLayout.NORTH);
        mainPanel.add(scroller, BorderLayout.CENTER);

        setSelectedIndex(0);
        updateSoftwareLights();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Changes color of slices box frame and button if color button was pressed; turns bounding box on and off if
     * checkbox was pressed; and closes dialog if "Close" button was pressed.
     *
     * @param  event  Event that triggered function.
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String command = event.getActionCommand();

        if (source == checkBoxShowLight) {

            if (null != m_kPanelSurface) {
                m_kPanelSurface.setLightBulbsVisible(checkBoxShowLight.isSelected());
            }
        } else if (source == onOffCheckBox) {
            m_akLights[iSelect].enable(onOffCheckBox.isSelected());
            refreshControlPanel();
            refreshLighting();
        } else if (source == ambientRadio) {

            if (ambientRadio.isSelected()) {
                m_akLights[iSelect].setTypeAmbient();
                refreshControlPanel();
                refreshLighting();
            }
        } else if (source == spotRadio) {

            if (spotRadio.isSelected()) {
                m_akLights[iSelect].setTypeSpot();
                refreshControlPanel();
                refreshLighting();
            }
        } else if (source == directionalRadio) {

            if (directionalRadio.isSelected()) {
                m_akLights[iSelect].setTypeDirectional();
                refreshControlPanel();
                refreshLighting();
            }
        } else if (source == pointRadio) {

            if (pointRadio.isSelected()) {
                m_akLights[iSelect].setTypePoint();
                refreshControlPanel();
                refreshLighting();
            }
        } else if (source == colorButton) {
            colorChooser = new ViewJColorChooser(new Frame(), "Pick light color", new OkColorListener(),
                                                 new CancelListener());
        } else if (command.equals("Close")) {
            disposeLocal();
            setVisible(false);
        }
    }

    /**
     * Clear memory and garbage collection.
     *
     * @param  flag  calling super dispose or not.
     */
    /**
     * Clear memory and garbage collection.
     *
     * @param  flag  call super dispose or not
     */
    public void disposeLocal(boolean flag) {
        m_kPanelSurface = null;
        m_kRenderSurface = null;

        controlPanelBox = null;
        scrollPanel = null;
        colorChooser = null;

        // Intensity slider
        intensitySlider = null;
        labelIntensity = null;
        labelIntensityBegin = null;
        labelIntensityMid = null;
        labelIntensityEnd = null;
        textIntensity = null;

        // Shininess slider
        shininessSlider = null;
        labelShininess = null;
        labelShininessBegin = null;
        labelShininessMid = null;
        labelShininessEnd = null;
        textShininess = null;

        // Light type radio buttons
        ambientRadio = null;
        spotRadio = null;
        directionalRadio = null;
        pointRadio = null;

        m_kSliderPosX = null;
        m_kSliderPosY = null;
        m_kSliderPosZ = null;
        m_kLabelPosX = null;
        m_kLabelPosY = null;
        m_kLabelPosZ = null;
        m_kTextPosX = null;
        m_kTextPosY = null;
        m_kTextPosZ = null;

        m_kSliderTrgX = null;
        m_kSliderTrgY = null;
        m_kSliderTrgZ = null;
        m_kLabelTrgX = null;
        m_kLabelTrgY = null;
        m_kLabelTrgZ = null;
        m_kTextTrgX = null;
        m_kTextTrgY = null;
        m_kTextTrgZ = null;

        m_akSoftwareLightsModel = null;
        m_akSoftwareLightsWorld = null;

        if (m_akModelLights != null) {

            for (int i = 0; i < m_akModelLights.length; i++) {
                m_akModelLights[i] = null;
            }

            m_akModelLights = null;
        }

        if (m_akWorldLights != null) {

            for (int i = 0; i < m_akWorldLights.length; i++) {
                m_akWorldLights[i] = null;
            }

            m_akWorldLights = null;
        }

        m_akLights = null;

        checkBoxShowLight = null;
        list = null;

        if (flag == true) {
            super.disposeLocal();
        }
    }

    /**
     * The the general light with the given index.
     *
     * @param   iIndex  light index
     *
     * @return  GeneralLight general light model.
     */
    public GeneralLight getGeneralLight(int iIndex) {
        return m_akLights[iIndex];
    }

    /**
     * Returns all general lights.
     *
     * @return  GeneralLight[] general light model.
     */
    public GeneralLight[] getGeneralLights() {
        return m_akLights;
    }

    /**
     * Get the main control Panel.
     *
     * @return  mainPanel main control panel
     */
    public JPanel getMainPanel() {
        return mainPanel;
    }

    /**
     * Get the number of lights.
     *
     * @return  int max light number
     */
    public int getNumLights() {
        return LIGHT_INDEX_MAX;
    }

    /**
     * Get the software light model.
     *
     * @return  SoftwareLight[] array of the software light.
     */
    public SoftwareLight[] getSoftwareLightsModel() {
        return m_akSoftwareLightsModel;
    }

    /**
     * Get the software light world.
     *
     * @return  SoftwareLight[] array of the software light world.
     */
    public SoftwareLight[] getSoftwareLightsWorld() {
        return m_akSoftwareLightsWorld;
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
    public void mousePressed(MouseEvent event) { }

    /**
     * Used in MouseRecorder to stop one series of slide moves.
     *
     * @param  event  Original mouse event.
     */
    public void mouseReleased(MouseEvent event) {

        if (bNeedsRedraw) {
            bNeedsRedraw = false;
            refreshLighting();
        }
    }

    /**
     * Repaints the parent frame with the correct lighting.
     */
    public void refreshLighting() {

        updateSoftwareLights();

        if (null != m_kPanelSurface) {
            m_kPanelSurface.setLightAttributes(iSelect);
        }

        if (null != m_kRenderSurface) {
            m_kRenderSurface.updateLighting();
        }

        if (rayBasedRender != null) {
            rayBasedRender.updateLighting();
        }
    }

    /**
     * Resizig the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   width
     * @param  frameHeight  height
     */
    public void resizePanel(int panelWidth, int frameHeight) {
        scroller.setPreferredSize(new Dimension(panelWidth, frameHeight - 40));
        scroller.setSize(new Dimension(panelWidth, frameHeight - 40));
        scroller.revalidate();
    }

    /**
     * Sets the light to selected.
     *
     * @param  index  Index of light
     */
    public void setSelectedIndex(int index) {
        iSelect = index;
        list.setSelectedIndex(index);
        refreshControlPanel();
    }

    /**
     * Slider move event handler.
     *
     * @param  e  Slider move events
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        if (source == intensitySlider) {
            float value = intensitySlider.getValue() / 100.0f;
            textIntensity.setText(String.valueOf(value));
            m_akLights[iSelect].setIntensity(value);
            bNeedsRedraw = true;
        } else if (source == shininessSlider) {
            float value = shininessSlider.getValue();
            textShininess.setText(String.valueOf(value));

            if (null != m_kRenderSurface) {
                m_kRenderSurface.updateShininess(value);
            }

            bNeedsRedraw = true;
        } else if (source == m_kSliderPosX) {
            Point3f kPosition = new Point3f(m_akLights[iSelect].getPosition());
            kPosition.x = (float) (m_kSliderPosX.getValue() / 100.0);
            m_kTextPosX.setText(String.valueOf((m_kSliderPosX.getValue() / 100.0d)));
            m_akLights[iSelect].setPosition(kPosition);
            bNeedsRedraw = true;
        } else if (source == m_kSliderPosY) {
            Point3f kPosition = new Point3f(m_akLights[iSelect].getPosition());
            kPosition.y = (float) (m_kSliderPosY.getValue() / 100.0);
            m_kTextPosY.setText(String.valueOf((m_kSliderPosY.getValue() / 100.0d)));
            m_akLights[iSelect].setPosition(kPosition);
            bNeedsRedraw = true;
        } else if (source == m_kSliderPosZ) {
            Point3f kPosition = new Point3f(m_akLights[iSelect].getPosition());
            kPosition.z = (float) (m_kSliderPosZ.getValue() / 100.0);
            m_kTextPosZ.setText(String.valueOf((m_kSliderPosZ.getValue() / 100.0d)));
            m_akLights[iSelect].setPosition(kPosition);
            bNeedsRedraw = true;
        } else if (source == m_kSliderTrgX) {
            Point3f kTarget = new Point3f(m_akLights[iSelect].getTarget());
            kTarget.x = (float) (m_kSliderTrgX.getValue() / 100.0);
            m_kTextTrgX.setText(String.valueOf((m_kSliderTrgX.getValue() / 100.0d)));
            m_akLights[iSelect].setTarget(kTarget);
            bNeedsRedraw = true;
        } else if (source == m_kSliderTrgY) {
            Point3f kTarget = new Point3f(m_akLights[iSelect].getTarget());
            kTarget.y = (float) (m_kSliderTrgY.getValue() / 100.0);
            m_kTextTrgY.setText(String.valueOf((m_kSliderTrgY.getValue() / 100.0d)));
            m_akLights[iSelect].setTarget(kTarget);
            bNeedsRedraw = true;
        } else if (source == m_kSliderTrgZ) {
            Point3f kTarget = new Point3f(m_akLights[iSelect].getTarget());
            kTarget.z = (float) (m_kSliderTrgZ.getValue() / 100.0);
            m_kTextTrgZ.setText(String.valueOf((m_kSliderTrgZ.getValue() / 100.0d)));
            m_akLights[iSelect].setTarget(kTarget);
            bNeedsRedraw = true;
        }

    }

    /**
     * Get the current SoftwareLight instances from each of the GeneralLight instances -- for the world- and model-based
     * lights.
     */
    public void updateSoftwareLights() {

        m_akSoftwareLightsModel = new SoftwareLight[m_akModelLights.length];

        for (int i = 0; i < m_akSoftwareLightsModel.length; i++) {
            m_akSoftwareLightsModel[i] = m_akModelLights[i].createSoftwareLightModel();
        }

        m_akSoftwareLightsWorld = new SoftwareLight[m_akWorldLights.length];

        for (int i = 0; i < m_akSoftwareLightsWorld.length; i++) {
            m_akSoftwareLightsWorld[i] = m_akWorldLights[i].createSoftwareLightWorld();
        }

    }

    /**
     * Sets values of sliders and intensities appropriately based on which light was chosen in the list.
     *
     * @param  kEvent  Event that triggered this function.
     */
    public void valueChanged(ListSelectionEvent kEvent) {
        JList kList = (JList) kEvent.getSource();
        Object[] akValues = kList.getSelectedValues();

        if (akValues.length > 0) {
            iSelect = kList.getMinSelectionIndex();
            setSelectedIndex(iSelect);
        }
    }

    /**
     * Calls dispose.
     *
     * @throws  Throwable  throw exception.
     */
    protected void finalize() throws Throwable {
        disposeLocal(false);
        super.finalize();
    }

    /**
     * Helper method that adds components to the control panel for the grid bag layout.
     *
     * @param  panelControl  control panel reference.
     * @param  c             Component added to the control panel.
     * @param  gbc           GridBagConstraints of added component.
     * @param  x             Gridx location
     * @param  y             Gridy location
     * @param  w             Gridwidth
     * @param  h             Gridheight
     */
    private void addControlPanel(JPanel panelControl, Component c, GridBagConstraints gbc, int x, int y, int w, int h) {
        gbc.gridx = x;
        gbc.gridy = y;
        gbc.gridwidth = w;
        gbc.gridheight = h;
        panelControl.add(c, gbc);
    }

    /**
     * Build the light control panel.
     */
    private void buildControlPanel() {
        controlPanel = new JPanel(new GridBagLayout());
        controlPanel.setBorder(buildTitledBorder("Selected Light Properties"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.BOTH;
        gbc.weightx = 100;
        gbc.weighty = 100;

        onOffLabel = new JLabel("On");
        onOffLabel.setFont(MipavUtil.font12);
        onOffLabel.setForeground(Color.black);

        onOffCheckBox = new JCheckBox();
        onOffCheckBox.addActionListener(this);
        onOffCheckBox.setSelected(false);

        JPanel onOffPanel = new JPanel();
        onOffPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        onOffPanel.add(onOffLabel);
        onOffPanel.add(onOffCheckBox);

        colorButton = new JButton("   ");
        colorButton.setToolTipText("Change light color");
        colorButton.addActionListener(this);
        colorButton.setActionCommand("ChangeColor");
        colorButton.setBackground(Color.white);

        colorLabel = new JLabel("Light color");
        colorLabel.setFont(MipavUtil.font12);
        colorLabel.setForeground(Color.black);

        JPanel colorPanel = new JPanel();
        colorPanel.setLayout(new FlowLayout(FlowLayout.LEFT));
        colorPanel.add(colorButton);
        colorPanel.add(colorLabel);

        // build intensity slider
        labelIntensity = new JLabel("Intensity");
        labelIntensity.setForeground(Color.black);
        labelIntensity.setFont(MipavUtil.font12);

        intensitySlider = new JSlider(0, 100, 50);
        intensitySlider.setFont(MipavUtil.font12);
        intensitySlider.setMinorTickSpacing(100 / 10);
        intensitySlider.setPaintTicks(true);
        intensitySlider.addMouseListener(this);
        intensitySlider.setVisible(true);

        labelIntensityBegin = new JLabel("0.0");
        labelIntensityBegin.setForeground(Color.black);
        labelIntensityBegin.setFont(MipavUtil.font12);
        labelIntensityMid = new JLabel("0.5");
        labelIntensityMid.setForeground(Color.black);
        labelIntensityMid.setFont(MipavUtil.font12);
        labelIntensityEnd = new JLabel("1.0");
        labelIntensityEnd.setForeground(Color.black);
        labelIntensityEnd.setFont(MipavUtil.font12);

        Hashtable labelTableIntensity = new Hashtable();
        labelTableIntensity.put(new Integer(0), labelIntensityBegin);
        labelTableIntensity.put(new Integer(50), labelIntensityMid);
        labelTableIntensity.put(new Integer(100), labelIntensityEnd);
        intensitySlider.setLabelTable(labelTableIntensity);
        intensitySlider.setPaintLabels(true);

        textIntensity = new JTextField(String.valueOf(1.0d), 4);
        textIntensity.setFont(MipavUtil.font12);

        ambientRadio = new JRadioButton("Ambient");
        ambientRadio.setActionCommand("Ambient");
        ambientRadio.addActionListener(this);
        spotRadio = new JRadioButton("Spot");
        spotRadio.setActionCommand("Spot");
        spotRadio.addActionListener(this);
        directionalRadio = new JRadioButton("Directional");
        directionalRadio.setActionCommand("Directional");
        directionalRadio.addActionListener(this);
        pointRadio = new JRadioButton("Point");
        pointRadio.setActionCommand("Point");
        pointRadio.addActionListener(this);

        ButtonGroup lightTypeButtonGroup = new ButtonGroup();
        lightTypeButtonGroup.add(ambientRadio);
        lightTypeButtonGroup.add(spotRadio);
        lightTypeButtonGroup.add(directionalRadio);
        lightTypeButtonGroup.add(pointRadio);

        JPanel lightTypePanel = new JPanel(new GridBagLayout());
        lightTypePanel.setBorder(buildTitledBorder("Type"));
        lightTypePanel.add(ambientRadio);
        lightTypePanel.add(spotRadio);
        lightTypePanel.add(directionalRadio);
        lightTypePanel.add(pointRadio);

        // Slider for changing position.  Range is [-1,1] with initial
        // value based on light.
        m_kLabelPosX = new JLabel("Source X");
        m_kLabelPosX.setFont(MipavUtil.font12);
        m_kLabelPosX.setForeground(Color.black);

        m_kSliderPosX = new JSlider();
        m_kSliderPosX.setFont(serif12);
        m_kSliderPosX.setMinorTickSpacing(10);
        m_kSliderPosX.setPaintTicks(true);
        m_kSliderPosX.addMouseListener(this);
        m_kSliderPosX.setPaintLabels(true);
        m_kSliderPosX.setAlignmentX(Component.LEFT_ALIGNMENT);
        m_kLabelPosX.setAlignmentX(Component.LEFT_ALIGNMENT);
        m_kTextPosX = new JTextField(String.valueOf(1.0d), 4);
        m_kTextPosX.setFont(MipavUtil.font12);
        m_kTextPosX.setEnabled(false);
        m_kLabelPosX.setEnabled(false);
        m_kSliderPosX.setEnabled(false);

        // Slider for changing position.  Range is [-1,1] with initial
        // value based on light.
        m_kLabelPosY = new JLabel("Source Y");
        m_kLabelPosY.setFont(MipavUtil.font12);
        m_kLabelPosY.setForeground(Color.black);

        m_kSliderPosY = new JSlider();
        m_kSliderPosY.setFont(serif12);
        m_kSliderPosY.setMinorTickSpacing(10);
        m_kSliderPosY.setPaintTicks(true);
        m_kSliderPosY.addMouseListener(this);
        m_kSliderPosY.setPaintLabels(true);
        m_kSliderPosY.setAlignmentX(Component.LEFT_ALIGNMENT);
        m_kLabelPosY.setAlignmentX(Component.LEFT_ALIGNMENT);
        m_kTextPosY = new JTextField(String.valueOf(1.0d), 4);
        m_kTextPosY.setFont(MipavUtil.font12);
        m_kTextPosY.setEnabled(false);
        m_kLabelPosY.setEnabled(false);
        m_kSliderPosY.setEnabled(false);

        // Slider for changing position.  Range is [-1,1] with initial
        // value based on light.
        m_kLabelPosZ = new JLabel("Source Z");
        m_kLabelPosZ.setFont(MipavUtil.font12);
        m_kLabelPosZ.setForeground(Color.black);

        m_kSliderPosZ = new JSlider();
        m_kSliderPosZ.setFont(serif12);
        m_kSliderPosZ.setMinorTickSpacing(10);
        m_kSliderPosZ.setPaintTicks(true);
        m_kSliderPosZ.addMouseListener(this);
        m_kSliderPosZ.setPaintLabels(true);
        m_kSliderPosZ.setAlignmentX(Component.LEFT_ALIGNMENT);
        m_kLabelPosZ.setAlignmentX(Component.LEFT_ALIGNMENT);
        m_kTextPosZ = new JTextField(String.valueOf(1.0d), 4);
        m_kTextPosZ.setFont(MipavUtil.font12);
        m_kTextPosZ.setEnabled(false);
        m_kLabelPosZ.setEnabled(false);
        m_kSliderPosZ.setEnabled(false);

        // Slider for changing position.  Range is [-1,1] with initial
        // value based on light.
        m_kLabelTrgX = new JLabel("Target X");
        m_kLabelTrgX.setFont(MipavUtil.font12);
        m_kLabelTrgX.setForeground(Color.black);

        m_kSliderTrgX = new JSlider();
        m_kSliderTrgX.setFont(serif12);
        m_kSliderTrgX.setMinorTickSpacing(10);
        m_kSliderTrgX.setPaintTicks(true);
        m_kSliderTrgX.addMouseListener(this);
        m_kSliderTrgX.setPaintLabels(true);
        m_kSliderTrgX.setAlignmentX(Component.LEFT_ALIGNMENT);
        m_kLabelTrgX.setAlignmentX(Component.LEFT_ALIGNMENT);
        m_kTextTrgX = new JTextField(String.valueOf(1.0d), 4);
        m_kTextTrgX.setFont(MipavUtil.font12);
        m_kTextTrgX.setEnabled(false);
        m_kLabelTrgX.setEnabled(false);
        m_kSliderTrgX.setEnabled(false);

        // Slider for changing position.  Range is [-1,1] with initial
        // value based on light.
        m_kLabelTrgY = new JLabel("Target Y");
        m_kLabelTrgY.setFont(MipavUtil.font12);
        m_kLabelTrgY.setForeground(Color.black);

        m_kSliderTrgY = new JSlider();
        m_kSliderTrgY.setFont(serif12);
        m_kSliderTrgY.setMinorTickSpacing(10);
        m_kSliderTrgY.setPaintTicks(true);
        m_kSliderTrgY.addMouseListener(this);
        m_kSliderTrgY.setPaintLabels(true);
        m_kSliderTrgY.setAlignmentX(Component.LEFT_ALIGNMENT);
        m_kLabelTrgY.setAlignmentX(Component.LEFT_ALIGNMENT);
        m_kTextTrgY = new JTextField(String.valueOf(1.0d), 4);
        m_kTextTrgY.setFont(MipavUtil.font12);
        m_kTextTrgY.setEnabled(false);
        m_kLabelTrgY.setEnabled(false);
        m_kSliderTrgY.setEnabled(false);

        // Slider for changing position.  Range is [-1,1] with initial
        // value based on light.
        m_kLabelTrgZ = new JLabel("Target Z");
        m_kLabelTrgZ.setFont(MipavUtil.font12);
        m_kLabelTrgZ.setForeground(Color.black);

        m_kSliderTrgZ = new JSlider();
        m_kSliderTrgZ.setFont(serif12);
        m_kSliderTrgZ.setMinorTickSpacing(10);
        m_kSliderTrgZ.setPaintTicks(true);
        m_kSliderTrgZ.addMouseListener(this);
        m_kSliderTrgZ.setPaintLabels(true);
        m_kSliderTrgZ.setAlignmentX(Component.LEFT_ALIGNMENT);
        m_kLabelTrgZ.setAlignmentX(Component.LEFT_ALIGNMENT);
        m_kTextTrgZ = new JTextField(String.valueOf(1.0d), 4);
        m_kTextTrgZ.setFont(MipavUtil.font12);
        m_kTextTrgZ.setEnabled(false);
        m_kLabelTrgZ.setEnabled(false);
        m_kSliderTrgZ.setEnabled(false);

        JPanel boxPanel = new JPanel(new GridBagLayout());

        // check box indicating if light bulbs are on.
        checkBoxShowLight = new JCheckBox("Show light bulbs", false);
        checkBoxShowLight.setFont(serif12);
        checkBoxShowLight.addActionListener(this);
        gbc.fill = GridBagConstraints.NONE;
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.gridy = 0;
        gbc.gridx = 0;
        boxPanel.add(checkBoxShowLight, gbc);

        gbc.fill = GridBagConstraints.NONE;
        addControlPanel(controlPanel, onOffPanel, gbc, 1, 0, 2, 1);
        addControlPanel(controlPanel, colorPanel, gbc, 6, 0, 2, 1);
        addControlPanel(controlPanel, boxPanel, gbc, 9, 0, 2, 1);
        addControlPanel(controlPanel, labelIntensity, gbc, 1, 1, 2, 1);
        addControlPanel(controlPanel, intensitySlider, gbc, 3, 1, 8, 1);
        addControlPanel(controlPanel, textIntensity, gbc, 14, 1, 1, 1);
        addControlPanel(controlPanel, lightTypePanel, gbc, 1, 4, 20, 1);
        addControlPanel(controlPanel, m_kLabelPosX, gbc, 1, 6, 2, 1);
        addControlPanel(controlPanel, m_kSliderPosX, gbc, 3, 6, 8, 1);
        addControlPanel(controlPanel, m_kTextPosX, gbc, 14, 6, 8, 1);
        addControlPanel(controlPanel, m_kLabelPosY, gbc, 1, 7, 2, 1);
        addControlPanel(controlPanel, m_kSliderPosY, gbc, 3, 7, 8, 1);
        addControlPanel(controlPanel, m_kTextPosY, gbc, 14, 7, 8, 1);
        addControlPanel(controlPanel, m_kLabelPosZ, gbc, 1, 8, 2, 1);
        addControlPanel(controlPanel, m_kSliderPosZ, gbc, 3, 8, 8, 1);
        addControlPanel(controlPanel, m_kTextPosZ, gbc, 14, 8, 8, 1);
        addControlPanel(controlPanel, m_kLabelTrgX, gbc, 1, 9, 2, 1);
        addControlPanel(controlPanel, m_kSliderTrgX, gbc, 3, 9, 8, 1);
        addControlPanel(controlPanel, m_kTextTrgX, gbc, 14, 9, 8, 1);
        addControlPanel(controlPanel, m_kLabelTrgY, gbc, 1, 10, 2, 1);
        addControlPanel(controlPanel, m_kSliderTrgY, gbc, 3, 10, 8, 1);
        addControlPanel(controlPanel, m_kTextTrgY, gbc, 14, 10, 8, 1);
        addControlPanel(controlPanel, m_kLabelTrgZ, gbc, 1, 11, 2, 1);
        addControlPanel(controlPanel, m_kSliderTrgZ, gbc, 3, 11, 8, 1);
        addControlPanel(controlPanel, m_kTextTrgZ, gbc, 14, 11, 8, 1);
    }

    /**
     * Build the light list panel.
     */
    private void buildListPanel() {

        // list panel for light names
        DefaultListModel listModel = new DefaultListModel();

        for (int i = 0; i < m_akLights.length; i++) {
            listModel.addElement(m_akLights[i].getDescription());
        }

        list = new JList(listModel);
        list.addListSelectionListener(this);
        list.addMouseListener(this);
        list.setVisibleRowCount(-1);
        list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);

        JScrollPane kScrollPane = new JScrollPane(list);
        scrollPanel = new JPanel();
        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(kScrollPane, BorderLayout.CENTER);
        scrollPanel.setPreferredSize(new Dimension(300, 190));
    }

    /**
     * Build the shinness control panel.
     */
    private void buildShininessPanel() {
        shininessPanel = new JPanel(new GridBagLayout());
        shininessPanel.setBorder(buildTitledBorder("Material shininess"));

        // build shinness slider
        labelShininess = new JLabel("Shininess");
        labelShininess.setForeground(Color.black);
        labelShininess.setFont(MipavUtil.font12);

        shininessSlider = new JSlider(0, 100, 5);
        shininessSlider.setFont(MipavUtil.font12);
        shininessSlider.setMinorTickSpacing(100 / 10);
        shininessSlider.setPaintTicks(true);
        shininessSlider.addChangeListener(this);
        shininessSlider.addMouseListener(this);
        shininessSlider.setVisible(true);

        labelShininessBegin = new JLabel("0.0");
        labelShininessBegin.setForeground(Color.black);
        labelShininessBegin.setFont(MipavUtil.font12);
        labelShininessMid = new JLabel("50");
        labelShininessMid.setForeground(Color.black);
        labelShininessMid.setFont(MipavUtil.font12);
        labelShininessEnd = new JLabel("100");
        labelShininessEnd.setForeground(Color.black);
        labelShininessEnd.setFont(MipavUtil.font12);

        Hashtable labelTableShininess = new Hashtable();
        labelTableShininess.put(new Integer(0), labelShininessBegin);
        labelTableShininess.put(new Integer(50), labelShininessMid);
        labelTableShininess.put(new Integer(100), labelShininessEnd);
        shininessSlider.setLabelTable(labelTableShininess);
        shininessSlider.setPaintLabels(true);

        textShininess = new JTextField(String.valueOf(5), 4);
        textShininess.setFont(MipavUtil.font12);

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.NONE;
        gbc.weightx = 100;
        gbc.weighty = 100;
        gbc.gridy = 0;
        gbc.gridx = 0;

        addControlPanel(shininessPanel, labelShininess, gbc, 1, 0, 2, 1);
        addControlPanel(shininessPanel, shininessSlider, gbc, 3, 0, 8, 1);
        addControlPanel(shininessPanel, textShininess, gbc, 14, 0, 1, 1);

    }

    /**
     * Creates a label in the proper font and color.
     *
     * @param   title    The title of the label.
     * @param   bEnable  True if label is to be enabled.
     *
     * @return  The new label.
     */
    private JLabel createLabel(String title, boolean bEnable) {
        JLabel label = new JLabel(title);

        label.setFont(serif12);
        label.setForeground(Color.black);
        label.setEnabled(bEnable);

        return label;
    }

    /**
     * Refresh the light control panel.
     */
    public void refreshControlPanel() {

        DefaultListModel listModel = (DefaultListModel) list.getModel();

        for (int i = 0; i < m_akLights.length; i++) {
            listModel.set(i,
                          m_akLights[i].getDescription() + " - " + m_akLights[i].getTypeString() + " - " +
                          (m_akLights[i].isEnabled() ? "ON" : "off"));
        }

        list.repaint();
        list.setSelectedIndex(iSelect);

        int iScale = m_aiLightScale[iSelect];
        GeneralLight light = m_akLights[iSelect];
        boolean bEnable = light.isEnabled();
        boolean bEnablePosition = bEnable && !light.isTypeAmbient();
        boolean bEnableDirectional = bEnable && (light.isTypeDirectional() || light.isTypeSpot());

        Point3f kPosition = light.getPosition();
        Point3f kTarget = light.getTarget();
        Color kColor = light.getColor();

        onOffCheckBox.setSelected(bEnable);

        float intensityValue = light.getIntensity();
        intensitySlider.setValue((int) (intensityValue * 100));
        textIntensity.setText(String.valueOf(intensityValue));
        intensitySlider.setEnabled(bEnable);
        textIntensity.setEnabled(bEnable);

        colorButton.setBackground(kColor);
        colorButton.setEnabled(bEnable);
        colorLabel.setEnabled(bEnable);

        // update light intensity

        if (bEnable) {
            intensitySlider.addChangeListener(this);
            // colorButton.addActionListener( this );
        } else {
            intensitySlider.removeChangeListener(this);
            // colorButton.removeActionListener( this );
        }


        // Labels for the x, y, and z sliders
        JLabel[] sliderLabelsPos = new JLabel[3];
        sliderLabelsPos[0] = createLabel(String.valueOf(-1.0f * iScale), bEnablePosition);
        sliderLabelsPos[1] = createLabel(String.valueOf(0.0f * iScale), bEnablePosition);
        sliderLabelsPos[2] = createLabel(String.valueOf(+1.0f * iScale), bEnablePosition);

        // Labels for the x, y, and z sliders
        JLabel[] sliderLabelsTrg = new JLabel[3];
        sliderLabelsTrg[0] = createLabel(String.valueOf(-1.0f * iScale), bEnableDirectional);
        sliderLabelsTrg[1] = createLabel(String.valueOf(0.0f * iScale), bEnableDirectional);
        sliderLabelsTrg[2] = createLabel(String.valueOf(+1.0f * iScale), bEnableDirectional);

        // Labels for the x, y, and z sliders
        Hashtable labelsPos = new Hashtable();
        Hashtable labelsTrg = new Hashtable();

        labelsPos.put(new Integer(-100 * iScale), sliderLabelsPos[0]);
        labelsPos.put(new Integer(0 * iScale), sliderLabelsPos[1]);
        labelsPos.put(new Integer(100 * iScale), sliderLabelsPos[2]);
        labelsTrg.put(new Integer(-100 * iScale), sliderLabelsTrg[0]);
        labelsTrg.put(new Integer(0 * iScale), sliderLabelsTrg[1]);
        labelsTrg.put(new Integer(100 * iScale), sliderLabelsTrg[2]);

        // update light position
        m_kSliderPosX.removeChangeListener(this);
        m_kSliderPosX.setMinimum(-100 * iScale);
        m_kSliderPosX.setMaximum(+100 * iScale);
        m_kSliderPosX.setValue(Math.round(kPosition.x * 100));
        m_kSliderPosX.setLabelTable(labelsPos);
        m_kSliderPosX.addChangeListener(this);
        m_kTextPosX.setText(makeString(kPosition.x, 2));

        m_kSliderPosY.removeChangeListener(this);
        m_kSliderPosY.setMinimum(-100 * iScale);
        m_kSliderPosY.setMaximum(+100 * iScale);
        m_kSliderPosY.setValue(Math.round(kPosition.y * 100));
        m_kSliderPosY.setLabelTable(labelsPos);
        m_kSliderPosY.addChangeListener(this);
        m_kTextPosY.setText(makeString(kPosition.y, 2));

        m_kSliderPosZ.removeChangeListener(this);
        m_kSliderPosZ.setMinimum(-100 * iScale);
        m_kSliderPosZ.setMaximum(+100 * iScale);
        m_kSliderPosZ.setValue(Math.round(kPosition.z * 100));
        m_kSliderPosZ.setLabelTable(labelsPos);
        m_kSliderPosZ.addChangeListener(this);
        m_kTextPosZ.setText(makeString(kPosition.z, 2));

        // update light target
        m_kSliderTrgX.removeChangeListener(this);
        m_kSliderTrgX.setMinimum(-100 * iScale);
        m_kSliderTrgX.setMaximum(+100 * iScale);
        m_kSliderTrgX.setValue(Math.round(kTarget.x * 100));
        m_kSliderTrgX.setLabelTable(labelsTrg);
        m_kSliderTrgX.addChangeListener(this);
        m_kTextTrgX.setText(makeString(kTarget.x, 2));

        m_kSliderTrgY.removeChangeListener(this);
        m_kSliderTrgY.setMinimum(-100 * iScale);
        m_kSliderTrgY.setMaximum(+100 * iScale);
        m_kSliderTrgY.setValue(Math.round(kTarget.y * 100));
        m_kSliderTrgY.setLabelTable(labelsTrg);
        m_kSliderTrgY.addChangeListener(this);
        m_kTextTrgY.setText(makeString(kTarget.y, 2));

        m_kSliderTrgZ.removeChangeListener(this);
        m_kSliderTrgZ.setMinimum(-100 * iScale);
        m_kSliderTrgZ.setMaximum(+100 * iScale);
        m_kSliderTrgZ.setValue(Math.round(kTarget.z * 100));
        m_kSliderTrgZ.setLabelTable(labelsTrg);
        m_kSliderTrgZ.addChangeListener(this);
        m_kTextTrgZ.setText(makeString(kTarget.z, 2));

        // Enable slider and value controls
        m_kSliderPosX.setEnabled(bEnablePosition);
        m_kSliderPosY.setEnabled(bEnablePosition);
        m_kSliderPosZ.setEnabled(bEnablePosition);
        m_kLabelPosX.setEnabled(bEnablePosition);
        m_kLabelPosY.setEnabled(bEnablePosition);
        m_kLabelPosZ.setEnabled(bEnablePosition);
        m_kTextPosX.setEnabled(bEnablePosition);
        m_kTextPosY.setEnabled(bEnablePosition);
        m_kTextPosZ.setEnabled(bEnablePosition);
        m_kSliderTrgX.setEnabled(bEnableDirectional);
        m_kSliderTrgY.setEnabled(bEnableDirectional);
        m_kSliderTrgZ.setEnabled(bEnableDirectional);
        m_kLabelTrgX.setEnabled(bEnableDirectional);
        m_kLabelTrgY.setEnabled(bEnableDirectional);
        m_kLabelTrgZ.setEnabled(bEnableDirectional);
        m_kTextTrgX.setEnabled(bEnableDirectional);
        m_kTextTrgY.setEnabled(bEnableDirectional);
        m_kTextTrgZ.setEnabled(bEnableDirectional);

        // Enable radio buttons only for types allowed for this light.
        ambientRadio.setEnabled(bEnable && light.canSetTypeAmbient());
        spotRadio.setEnabled(bEnable && light.canSetTypeSpot());
        directionalRadio.setEnabled(bEnable && light.canSetTypeDirectional());
        ;
        pointRadio.setEnabled(bEnable && light.canSetTypePoint());

        // Set the radio button corresponding to the current light type.
        if (light.isTypeAmbient()) {
            ambientRadio.setSelected(true);
        } else if (light.isTypeSpot()) {
            spotRadio.setSelected(true);
        } else if (light.isTypeDirectional()) {
            directionalRadio.setSelected(true);
        } else if (light.isTypePoint()) {
            pointRadio.setSelected(true);
        }

        // This flag can be set by the change listener as a result
        // of updating the slider values.
        bNeedsRedraw = false;
    }

    public void enableLight( int iSelect, boolean bOn )
    {
        onOffCheckBox.setSelected(bOn);
        m_akLights[iSelect].enable(bOn);
        refreshControlPanel();
        refreshLighting();
    }
    
    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Cancel the color dialog, change nothing.
     */
    class CancelListener implements ActionListener {

        /**
         * Unchanged.
         *
         * @param  e  action event handler.
         */
        public void actionPerformed(ActionEvent e) { }
    }

    /**
     * Pick up the selected color and call method to change the surface color.
     */
    class OkColorListener implements ActionListener {

        /**
         * Sets the button color to the chosen color and changes the color of the surface.
         *
         * @param  e  Event that triggered this method.
         */
        public void actionPerformed(ActionEvent e) {
            Object source = e.getSource();

            if (colorChooser != source) {
                Color colorA = colorChooser.getColor();

                m_akLights[iSelect].setColor(colorA);
                refreshControlPanel();
                refreshLighting();
            }
        }
    }
}
