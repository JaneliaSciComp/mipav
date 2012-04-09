package gov.nih.mipav.view.renderer.J3D.surfaceview;


import gov.nih.mipav.view.renderer.J3D.*;

import com.sun.j3d.utils.geometry.*;
import com.sun.j3d.utils.universe.*;

import java.awt.*;
import java.awt.event.*;

import javax.media.j3d.*;
import javax.swing.*;
import javax.swing.event.*;

import javax.vecmath.*;


/**
 * Window for setting the surface material properties. The window displays two sample spheres: a "before" and "after"
 * sphere. Both are initialized to the current material properties and transparency of the surface. The user can change
 * the ambient, diffuse, specular, and emmissive colors with a color control dialog. The user can also change the
 * specular coefficient, or "shininess" through a slider interface.
 *
 * <p>Pressing the "apply" button the user can see the changes applied to sthe surface. Pressing the "reset" button
 * resets the surface and material properties interface to the original surface parameters. Pressing "OK" applies the
 * changes to the surface and closes the window, Pressing "cancel" resets the changes and closes the window.</p>
 *
 * @author  Alexandra Bokinsky
 */
public class JFrameSurfaceMaterialProperties extends JFrame
        implements WindowListener, ActionListener, ChangeListener /* Slider changes */ {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3077827124095959620L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Before/After index values for the two displayed spheres, canvases, and display panels:. */
    private int AFTER = 1;

    /** Before/After index values for the two displayed spheres, canvases, and display panels:. */
    private int BEFORE = 0;

    /** Pre-defined materials */
    private Material[] m_akMaterialPreset = null;

    /** specular coefficient */
    private float m_fShininess;

    /** Preset Material's examples:. */
    private int m_iNumPreset = 6;

    /** the index of the surface that is being changed in the JPanelSurface */
    private int m_iSurfaceIndex;

    /** Which button is pressed to activate the JColorChooser:. */
    private int m_iWhichButton;

    /** Current color values:. */
    private Color3f m_kAmbient;

    /** Material Properties color buttons:. */
    private JButton m_kAmbientColorButton;

    /** Color Chooser dialog:. */
    private JColorChooser m_kColorChooser;

    /** diffuse color */
    private Color3f m_kDiffuse;

    /** diffuse color button */
    private JButton m_kDiffuseColorButton;

    /** emissive color */
    private Color3f m_kEmissive;

    /** emissive color button */
    private JButton m_kEmissiveColorButton;

    /** Split pane for displaying the canvases side-by-side:. */
    private JSplitPane m_kImagePane;

    /** Canvas display panels:. */
    private JPanel[] m_kImagePanel;

    /** Split pane for displaying the rendered spheres above the interface:. */
    private JSplitPane m_kMainPane;

    /** Materials for new vales and backup:. */
    private Material m_kMaterialNew;

    /** Window display panels: Material properties buttons/slider panel:. */
    private JPanel m_kMaterialPanel;

    /** Saved Material for backup */
    private Material m_kMaterialSave;

    /** Parent class:. */
    private JPanelSurface m_kParent = null;

    /** Shininess slider. */
    private JSlider m_kShininessSlider;

    /** Specular color component */
    private Color3f m_kSpecular;

    /** Specular color button */
    private JButton m_kSpecularColorButton;

    /** keeps track of whether or not color has been applied to surface (for
     * backup on cancel) */
    private boolean m_bColorApplied = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates the Surface Material Editor window. The two example spheres are displayed with the same lights, opacity,
     * and material properties as the surface.
     *
     * @param  kParent          the array of lights that are used to light the surface
     * @param  iSurface         the opacity of the surface
     * @param  akGeneralLights  DOCUMENT ME!
     * @param  fOpacity         DOCUMENT ME!
     * @param  kMaterial        the original material properties.
     */
    public JFrameSurfaceMaterialProperties(JPanelSurface kParent, int iSurface, GeneralLight[] akGeneralLights,
                                           float fOpacity, Material kMaterial) {

        /* Create the window: */
        super("Advanced Material Properties");

        m_kParent = kParent;
        m_iSurfaceIndex = iSurface;

        /* Setup the material properties of the two spheres, backing up the
         * original data: */
        setupMaterials(kMaterial);

        /* Create the rendered scene, two spheres, a "Before" and "After"
         * sphere: */
        setupScene(fOpacity, akGeneralLights);

        /* Setup the user-interface: */
        setupInterface();

        /* Listen for the close window event: */
        addWindowListener(this);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Called when a button is pressed:
     *
     * @param  kEvent  action event
     */
    public void actionPerformed(ActionEvent kEvent) {
        String kCommand = kEvent.getActionCommand();

        /* When one of the color buttons is pressed, Ambient, Diffuse,
         * Specular, or Emissive, a JColorChooser is created with the modal value set to true. This ensures that only
         * one color is set through the JColorChooser dialog at a time.  The data member m_iWhichButton keeps track of
         * which button is pressed at the time the JColorChooser is created, so that when it is closed with the "OK"
         * command the proper actions can be taken: */
        if (kCommand.equals("Ambient")) {
            m_kColorChooser = new JColorChooser(m_kAmbientColorButton.getBackground());

            JDialog kDialog = JColorChooser.createDialog(new Frame(), "Set Ambient Color", true, m_kColorChooser,
                                                           this, this);
            m_iWhichButton = Material.AMBIENT;
            kDialog.setVisible(true);
        } else if (kCommand.equals("Diffuse")) {
            m_kColorChooser = new JColorChooser(m_kDiffuseColorButton.getBackground());

            JDialog kDialog = JColorChooser.createDialog(new Frame(), "Set Diffuse color", true, m_kColorChooser,
                                                           this, this);
            m_iWhichButton = Material.DIFFUSE;
            kDialog.setVisible(true);
        } else if (kCommand.equals("Specular")) {
            m_kColorChooser = new JColorChooser(m_kSpecularColorButton.getBackground());

            JDialog kDialog = JColorChooser.createDialog(new Frame(), "Set Specular color", true, m_kColorChooser,
                                                           this, this);
            m_iWhichButton = Material.SPECULAR;
            kDialog.setVisible(true);
        } else if (kCommand.equals("Emissive")) {
            m_kColorChooser = new JColorChooser(m_kEmissiveColorButton.getBackground());

            JDialog kDialog = JColorChooser.createDialog(new Frame(), "Set Emissive color", true, m_kColorChooser,
                                                           this, this);
            m_iWhichButton = Material.EMISSIVE;
            kDialog.setVisible(true);
        }

        /* The JColorChooser has been closed with the OK button. The
         * m_iWhichButton is used to determine which color parameter is being set, and the new color value is read from
         * the m_kColorChooser
         * dialog. The color of the button is the set with the new value: */
        else if (kCommand.equals("OK")) {

            if (m_iWhichButton == Material.AMBIENT) {
                Color kColor = m_kColorChooser.getColor();
                m_kAmbientColorButton.setBackground(kColor);
                m_kMaterialNew.setAmbientColor(new Color3f(kColor));
                m_kAmbient.set(kColor);
            } else if (m_iWhichButton == Material.DIFFUSE) {
                Color kColor = m_kColorChooser.getColor();
                m_kDiffuseColorButton.setBackground(kColor);
                m_kMaterialNew.setDiffuseColor(new Color3f(kColor));
                m_kDiffuse.set(kColor);
            } else if (m_iWhichButton == Material.SPECULAR) {
                Color kColor = m_kColorChooser.getColor();
                m_kSpecularColorButton.setBackground(kColor);
                m_kMaterialNew.setSpecularColor(new Color3f(kColor));
                m_kSpecular.set(kColor);
            } else if (m_iWhichButton == Material.EMISSIVE) {
                Color kColor = m_kColorChooser.getColor();
                m_kEmissiveColorButton.setBackground(kColor);
                m_kMaterialNew.setEmissiveColor(new Color3f(kColor));
                m_kEmissive.set(kColor);
            }

            m_kColorChooser = null;
        }
        /* One of the presest material buttons is pressed, get which button
         * and use the defined material: */
        else if (kCommand.startsWith("Preset")) {
            int iPreset = Integer.valueOf(kCommand.substring("Preset".length())).intValue();
            m_akMaterialPreset[iPreset].getAmbientColor(m_kAmbient);
            m_kAmbientColorButton.setBackground(m_kAmbient.get());
            m_kMaterialNew.setAmbientColor(m_kAmbient);

            m_akMaterialPreset[iPreset].getDiffuseColor(m_kDiffuse);
            m_kDiffuseColorButton.setBackground(m_kDiffuse.get());
            m_kMaterialNew.setDiffuseColor(m_kDiffuse);

            m_akMaterialPreset[iPreset].getSpecularColor(m_kSpecular);
            m_kSpecularColorButton.setBackground(m_kSpecular.get());
            m_kMaterialNew.setSpecularColor(m_kSpecular);

            m_akMaterialPreset[iPreset].getEmissiveColor(m_kEmissive);
            m_kEmissiveColorButton.setBackground(m_kEmissive.get());
            m_kMaterialNew.setEmissiveColor(m_kEmissive);

            m_fShininess = m_akMaterialPreset[iPreset].getShininess();
            m_kMaterialNew.setShininess(m_fShininess);
            m_kShininessSlider.setValue((int) m_fShininess);
        }

        /* The Apply button is pressed, call applyColorChange to apply the new
         * material properties to the original surface: */
        else if (kCommand.equals("Apply")) {
            applyColorChange();
        }
        /* The Reset button is pressed, call resetColorChange to reset the
         * surface material properties to the original values: */
        else if (kCommand.equals("Reset")) {
            resetColorChange();
        }
        /* OK Button in the JPanelSurfaceMaterialProperties window is pressed,
         * apply color changes and close this window: */
        else if (kCommand.equals("OK_CLOSE")) {
            applyColorChange();
            setVisible(false);
            this.dispose();
        }
        /* Cancel Button in the JPanelSurfaceMaterialProperties window is
         * pressed, reset the color changes and close this window: */
        else if (kCommand.equals("CANCEL_CLOSE")) {
            resetColorChange();
            setVisible(false);
            this.dispose();
        }
    }

    /**
     * Deletes all local data members.
     */
    public void dispose() {
    	if ( m_kMaterialPanel != null ) {
           m_kMaterialPanel.removeAll();
           m_kMaterialPanel = null;
    	}
        m_kImagePanel[BEFORE].removeAll();
        m_kImagePanel[BEFORE] = null;
        m_kImagePanel[AFTER].removeAll();
        m_kImagePanel[AFTER] = null;

        m_kImagePane.removeAll();
        m_kImagePane = null;
        m_kMainPane.removeAll();
        m_kMainPane = null;

        m_kMaterialNew = null;
        m_kMaterialSave = null;

        m_kAmbient = null;
        m_kEmissive = null;
        m_kDiffuse = null;
        m_kSpecular = null;

        m_kColorChooser = null;
        m_kAmbientColorButton = null;
        m_kDiffuseColorButton = null;
        m_kSpecularColorButton = null;
        m_kEmissiveColorButton = null;

        super.dispose();

        System.gc();
    }

    /**
     * Called when the slider values have changed. Reads the new shininess values and sets the Material Properties.
     *
     * @param  event  ChangeEvent state change event.
     */
    public void stateChanged(ChangeEvent event) {

        if (event.getSource() == m_kShininessSlider) {
            m_fShininess = (float) (m_kShininessSlider.getValue());
            m_kMaterialNew.setShininess(m_fShininess);
        }
    }

    /**
     * windowActivated.
     *
     * @param  e  WindowEvent
     */
    public void windowActivated(WindowEvent e) { }

    /**
     * windowClosed.
     *
     * @param  e  WindowEvent
     */
    public void windowClosed(WindowEvent e) { }

    /**
     * windowClosing Catch the windowClosing event so that the local data memebrs can be destroyed.
     *
     * @param  e  WindowEvent
     */
    public void windowClosing(WindowEvent e) {
        this.dispose();
    }

    /**
     * windowDeactivated.
     *
     * @param  e  WindowEvent
     */
    public void windowDeactivated(WindowEvent e) { }

    /**
     * windowDeiconified.
     *
     * @param  e  WindowEvent
     */
    public void windowDeiconified(WindowEvent e) { }

    /**
     * windowIconified.
     *
     * @param  e  WindowEvent
     */
    public void windowIconified(WindowEvent e) { }

    /**
     * windowOpened.
     *
     * @param  e  WindowEvent
     */
    public void windowOpened(WindowEvent e) { }

    /**
     * Update the color changes to the Before sphere and the original surface.
     */
    private void applyColorChange() {
        m_kMaterialNew.getAmbientColor(m_kAmbient);
        m_kMaterialNew.getEmissiveColor(m_kEmissive);
        m_kMaterialNew.getDiffuseColor(m_kDiffuse);
        m_kMaterialNew.getSpecularColor(m_kSpecular);
        m_fShininess = m_kMaterialNew.getShininess();

        Material kMaterial = new Material( m_kAmbient, m_kEmissive, m_kDiffuse, m_kSpecular, m_fShininess );
        m_kParent.setMaterial( kMaterial, m_iSurfaceIndex );

        m_bColorApplied = true;
    }

    /**
     * Builds the preset panel to display the preset images and buttons.
     *
     * @return  the preset panel to display the preset images and buttons.
     */
    private JPanel buildPresetPanel() {
        JPanel kPresetPanel = new JPanel(new GridLayout(2, m_iNumPreset));

        for (int i = 0; i < m_iNumPreset; i++) {
            kPresetPanel.add(m_kImagePanel[2 + i]);
        }

        for (int i = 0; i < m_iNumPreset; i++) {
            JButton kButton = new JButton("Preset " + i);
            kButton.addActionListener(this);
            kButton.setActionCommand("Preset" + i);
            kPresetPanel.add(kButton);
        }

        return kPresetPanel;
    }

    /**
     * Creates the canvas.
     *
     * @return  Canvas3D
     */
    private Canvas3D createCanvas3D() {
        GraphicsConfiguration config = SimpleUniverse.getPreferredConfiguration();
        Canvas3D canvas3D = new Canvas3D(config);

        return canvas3D;
    }

    /**
     * Creates the rendered spheres, setting the apprearance, material properties, and transparency.
     *
     * @param   fOpacity   default opacity
     * @param   kMaterial  initial Material
     *
     * @return  BranchGroup
     */
    private BranchGroup createSceneGraph(float fOpacity, Material kMaterial) {

        /* Sphere to mark the point being drawn: */
        Shape3D kSphere = new Sphere(.5f, Primitive.GENERATE_NORMALS | Primitive.ENABLE_APPEARANCE_MODIFY, 50).getShape();
        Appearance kAppearance = kSphere.getAppearance();
        kAppearance.setMaterial(kMaterial);
        kSphere.getAppearance().getMaterial().setLightingEnable(true);

        TransparencyAttributes tap = new TransparencyAttributes();
        tap.setCapability(TransparencyAttributes.ALLOW_VALUE_WRITE);

        if (fOpacity == 0) {
            tap.setTransparencyMode(TransparencyAttributes.NONE);
        } else {
            tap.setTransparencyMode(TransparencyAttributes.BLENDED);
        }

        tap.setSrcBlendFunction(TransparencyAttributes.BLEND_SRC_ALPHA);
        tap.setDstBlendFunction(TransparencyAttributes.BLEND_ONE_MINUS_SRC_ALPHA);
        tap.setTransparency( 1.0f - fOpacity);
        kAppearance.setTransparencyAttributes(tap);


        /* Transforms and BranchGroup to contain the sphere shape: */
        Transform3D kTransform = new Transform3D();

        TransformGroup kTransformGroup = new TransformGroup();
        kTransformGroup.setTransform(kTransform);
        kTransformGroup.addChild(kSphere.cloneTree());

        BranchGroup kBranchGroup = new BranchGroup();
        kBranchGroup.setCapability(BranchGroup.ALLOW_CHILDREN_WRITE);
        kBranchGroup.setCapability(BranchGroup.ALLOW_CHILDREN_EXTEND);
        kBranchGroup.addChild(kTransformGroup);
        kBranchGroup.compile();

        return kBranchGroup;
    }

    /**
     * Up to 6 preset materials can be defined.
     */
    private void initMaterialPreset() {
        m_akMaterialPreset = new Material[6];
        m_akMaterialPreset[0] = /* Diffuse Red */ new Material(new Color3f(0f, 0f, 0f), /* ambient */
                                                               new Color3f(0f, 0f, 0f), /* emissive */
                                                               new Color3f(1f, 0f, 0f), /* diffuse */
                                                               new Color3f(0f, 0f, 0f), /* specular */
                                                               0f); /* shininess */
        m_akMaterialPreset[0].setCapability(Material.ALLOW_COMPONENT_READ);
        m_akMaterialPreset[0].setCapability(Material.ALLOW_COMPONENT_WRITE);

        m_akMaterialPreset[1] = /* Blue plastic (white highlights) */ new Material(new Color3f(0f, 0f, 0f), /* ambient */
                                                                                   new Color3f(0f, 0f, 0f), /* emissive */
                                                                                   new Color3f(0f, 0f, 1f), /* diffuse */
                                                                                   new Color3f(1f, 1f, 1f), /* specular */
                                                                                   128f); /* shininess */
        m_akMaterialPreset[1].setCapability(Material.ALLOW_COMPONENT_READ);
        m_akMaterialPreset[1].setCapability(Material.ALLOW_COMPONENT_WRITE);

        m_akMaterialPreset[2] = /* Green metallic (color highlights) */ new Material(new Color3f(0f, 0f, 0f), /* ambient */
                                                                                     new Color3f(0f, 0f, 0f), /* emissive */
                                                                                     new Color3f(0f, 0.5f, 0f), /* diffuse */
                                                                                     new Color3f(0.5f, 0.5f, 0f), /* specular */
                                                                                     33f); /* shininess */
        m_akMaterialPreset[2].setCapability(Material.ALLOW_COMPONENT_READ);
        m_akMaterialPreset[2].setCapability(Material.ALLOW_COMPONENT_WRITE);

        m_akMaterialPreset[3] = /* Red/orange metallic (color highlights) */ new Material(new Color3f(0f, 0f, 0f), /* ambient */
                                                                                          new Color3f(0f, 0f, 0f), /* emissive */
                                                                                          new Color3f(1f, 0f, 0f), /* diffuse */
                                                                                          new Color3f(1f, 0.5f, 0f), /* specular */
                                                                                          25f); /* shininess */
        m_akMaterialPreset[3].setCapability(Material.ALLOW_COMPONENT_READ);
        m_akMaterialPreset[3].setCapability(Material.ALLOW_COMPONENT_WRITE);

        m_akMaterialPreset[4] = new Material(); /* Default */
        m_akMaterialPreset[4].setCapability(Material.ALLOW_COMPONENT_READ);
        m_akMaterialPreset[4].setCapability(Material.ALLOW_COMPONENT_WRITE);

        m_akMaterialPreset[5] = new Material(); /* Default */
        m_akMaterialPreset[5].setCapability(Material.ALLOW_COMPONENT_READ);
        m_akMaterialPreset[5].setCapability(Material.ALLOW_COMPONENT_WRITE);
    }

    /**
     * Reset the color changes for the Before sphere and the original surface,
     * based on the original surface material properties, also update the
     * user-interface buttons to the original values.
     */
    private void resetColorChange() {
        // if no color has been applied, do not need to restore.
        if ( !m_bColorApplied )
        {
            return;
        }

        m_kMaterialSave.getAmbientColor(m_kAmbient);
        m_kMaterialSave.getEmissiveColor(m_kEmissive);
        m_kMaterialSave.getDiffuseColor(m_kDiffuse);
        m_kMaterialSave.getSpecularColor(m_kSpecular);
        m_fShininess = m_kMaterialSave.getShininess();

        // restore the per-vertex color and the old Material, then restore the material
        Material kMaterial = new Material( m_kAmbient, m_kEmissive, m_kDiffuse, m_kSpecular, m_fShininess );
        m_kParent.restorePerVertexColor( kMaterial, m_iSurfaceIndex );

        m_kMaterialNew.setAmbientColor(m_kAmbient);
        m_kMaterialNew.setEmissiveColor(m_kEmissive);
        m_kMaterialNew.setDiffuseColor(m_kDiffuse);
        m_kMaterialNew.setSpecularColor(m_kSpecular);
        m_kMaterialNew.setShininess(m_fShininess);

        m_kAmbientColorButton.setBackground(m_kAmbient.get());
        m_kDiffuseColorButton.setBackground(m_kDiffuse.get());
        m_kSpecularColorButton.setBackground(m_kSpecular.get());
        m_kEmissiveColorButton.setBackground(m_kEmissive.get());
        m_kShininessSlider.setValue((int) m_fShininess);
    }

    /**
     * Sets up the user interface. Button and slider.
     */
    private void setupInterface() {

        /* Color buttons and labels, both are JButtons and can be pressed to
         * change the Ambient, Diffuse, Specular, or Emissive colors: */
        m_kAmbientColorButton = new JButton("       ");
        m_kAmbientColorButton.setBackground(m_kAmbient.get());
        m_kAmbientColorButton.setToolTipText("Change surface ambient color");
        m_kAmbientColorButton.addActionListener(this);
        m_kAmbientColorButton.setActionCommand("Ambient");

        JButton kAmbientColorButtonLabel = new JButton("Ambient Color");
        kAmbientColorButtonLabel.addActionListener(this);
        kAmbientColorButtonLabel.setActionCommand("Ambient");

        m_kDiffuseColorButton = new JButton("       ");
        m_kDiffuseColorButton.setBackground(m_kDiffuse.get());
        m_kDiffuseColorButton.setToolTipText("Change surface diffuse color");
        m_kDiffuseColorButton.addActionListener(this);
        m_kDiffuseColorButton.setActionCommand("Diffuse");

        JButton kDiffuseColorButtonLabel = new JButton("Diffuse Color");
        kDiffuseColorButtonLabel.addActionListener(this);
        kDiffuseColorButtonLabel.setActionCommand("Diffuse");

        m_kSpecularColorButton = new JButton("       ");
        m_kSpecularColorButton.setBackground(m_kSpecular.get());
        m_kSpecularColorButton.setToolTipText("Change surface specular color");
        m_kSpecularColorButton.addActionListener(this);
        m_kSpecularColorButton.setActionCommand("Specular");

        JButton kSpecularColorButtonLabel = new JButton("Specular Color");
        kSpecularColorButtonLabel.addActionListener(this);
        kSpecularColorButtonLabel.setActionCommand("Specular");

        m_kEmissiveColorButton = new JButton("       ");
        m_kEmissiveColorButton.setBackground(m_kEmissive.get());
        m_kEmissiveColorButton.setToolTipText("Change surface emmissive color");
        m_kEmissiveColorButton.addActionListener(this);
        m_kEmissiveColorButton.setActionCommand("Emissive");

        JButton kEmissiveColorButtonLabel = new JButton("Emissive Color");
        kEmissiveColorButtonLabel.addActionListener(this);
        kEmissiveColorButtonLabel.setActionCommand("Emissive");


        /* Shininess Slider: */
        m_kShininessSlider = new JSlider(1, 128, (int) m_fShininess);
        m_kShininessSlider.addChangeListener(this);
        m_kShininessSlider.setMajorTickSpacing(16);
        m_kShininessSlider.setPaintTicks(true);
        m_kShininessSlider.setPaintLabels(true);

        /* User-action buttons, apply the color changes to the original
         * surface:*/
        JButton kApply = new JButton("Apply");
        kApply.addActionListener(this);
        kApply.setActionCommand("Apply");

        /* User-action buttons, reset the original color values for the
         * surface:*/
        JButton kReset = new JButton("Reset");
        kReset.addActionListener(this);
        kReset.setActionCommand("Reset");

        /* Apply color changes and close the window: */
        JButton kOK = new JButton("OK");
        kOK.addActionListener(this);
        kOK.setActionCommand("OK_CLOSE");

        /* Reset color changes and close the window: */
        JButton kCancel = new JButton("Cancel");
        kCancel.addActionListener(this);
        kCancel.setActionCommand("CANCEL_CLOSE");


        /* Before/After label panel: */
        JPanel kBeforeAfterPanel = new JPanel();
        GridBagLayout kGBL_Labels = new GridBagLayout();
        kBeforeAfterPanel.setLayout(kGBL_Labels);

        GridBagConstraints kGBC_Labels = new GridBagConstraints();
        kGBL_Labels.setConstraints(kBeforeAfterPanel, kGBC_Labels);
        kGBC_Labels.anchor = GridBagConstraints.CENTER;
        kGBC_Labels.fill = GridBagConstraints.HORIZONTAL;
        kGBC_Labels.gridx = 0;
        kGBC_Labels.gridy = 0;
        kBeforeAfterPanel.add(new JLabel("Before", JLabel.CENTER), kGBC_Labels);
        kGBC_Labels.gridx = 1;
        kBeforeAfterPanel.add(new JLabel("After", JLabel.CENTER), kGBC_Labels);
        kGBL_Labels.layoutContainer(kBeforeAfterPanel);
        kGBL_Labels.columnWeights = new double[] { 1, 1 };
        kGBL_Labels.rowWeights = new double[] { 0 };

        /* Ambient/Diffuse color panel: */
        JPanel kColorPanel1 = new JPanel(new GridLayout(1, 4));
        kColorPanel1.add(kAmbientColorButtonLabel);
        kColorPanel1.add(m_kAmbientColorButton);
        kColorPanel1.add(kDiffuseColorButtonLabel);
        kColorPanel1.add(m_kDiffuseColorButton);

        /* Specular/Emissive color panel: */
        JPanel kColorPanel2 = new JPanel(new GridLayout(1, 4));
        kColorPanel2.add(kSpecularColorButtonLabel);
        kColorPanel2.add(m_kSpecularColorButton);
        kColorPanel2.add(kEmissiveColorButtonLabel);
        kColorPanel2.add(m_kEmissiveColorButton);

        /* Shininess slider panel: */
        JPanel kShininessPanel = new JPanel(new GridBagLayout());
        kShininessPanel.add(new JLabel("Shininess"));
        kShininessPanel.add(m_kShininessSlider);

        /* User-action button, Apply, Reset, OK, Cancel: panel: */
        JPanel kClosePanel = new JPanel(new GridLayout(1, 4));
        kClosePanel.add(kApply);
        kClosePanel.add(kReset);
        kClosePanel.add(kOK);
        kClosePanel.add(kCancel);

        /* Material panel holds all the above button/slider panels:*/
        m_kMaterialPanel = new JPanel();

        GridBagLayout kGBLMat = new GridBagLayout();
        m_kMaterialPanel.setLayout(kGBLMat);

        GridBagConstraints kgbcMat = new GridBagConstraints();
        kGBLMat.setConstraints(kBeforeAfterPanel, kgbcMat);
        kgbcMat.anchor = GridBagConstraints.CENTER;
        kgbcMat.fill = GridBagConstraints.HORIZONTAL;
        kgbcMat.gridx = 0;
        kgbcMat.gridy = 0;
        m_kMaterialPanel.add(kBeforeAfterPanel, kgbcMat);
        kgbcMat.gridy++;

        if (m_iNumPreset > 0) {
            m_kMaterialPanel.add(buildPresetPanel(), kgbcMat);
            kgbcMat.gridy++;
        }

        m_kMaterialPanel.add(kColorPanel1, kgbcMat);
        kgbcMat.gridy++;
        m_kMaterialPanel.add(kColorPanel2, kgbcMat);
        kgbcMat.gridy++;
        m_kMaterialPanel.add(kShininessPanel, kgbcMat);
        kgbcMat.gridy++;
        m_kMaterialPanel.add(kClosePanel, kgbcMat);
        kGBLMat.layoutContainer(m_kMaterialPanel);
        kGBLMat.columnWeights = new double[] { 1 };
        kGBLMat.rowWeights = new double[] { 0, 1, 1, 1, 0 };


        /* ImagePanes, Before canvas is on the left, After canvas is on the
         * right: */
        m_kImagePane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT, true, m_kImagePanel[BEFORE], m_kImagePanel[AFTER]);
        m_kImagePane.setOneTouchExpandable(true);
        m_kImagePane.setDividerSize(6);
        m_kImagePane.setMinimumSize(new Dimension(606, 300));
        m_kImagePane.setDividerLocation(0.5);

        /* The rendered spheres are displayed at the top of the window, and
         * the buttons at the bottom: */
        m_kMainPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, true, m_kImagePane, m_kMaterialPanel);
        m_kMainPane.setOneTouchExpandable(true);
        m_kMainPane.setDividerSize(6);
        m_kMainPane.setMinimumSize(new Dimension(606, 606));
        m_kMainPane.setDividerLocation(0.5);

        getContentPane().setLayout(new BorderLayout());
        getContentPane().add(m_kMainPane, BorderLayout.CENTER);
        setSize(new Dimension(606, 606));
        setVisible(true);

        m_kImagePane.setDividerLocation(0.5);
        m_kMainPane.setDividerLocation(0.5);

    }

    /**
     * Reads the current color values from the Material and sets the values for the two spheres displayed in the
     * interface. Saves the original values in case the Reset button is pressed.
     *
     * @param  kMaterial  material reference.
     */
    private void setupMaterials(Material kMaterial) {
        m_kAmbient = new Color3f();
        m_kEmissive = new Color3f();
        m_kDiffuse = new Color3f();
        m_kSpecular = new Color3f();
        kMaterial.getAmbientColor(m_kAmbient);
        kMaterial.getEmissiveColor(m_kEmissive);
        kMaterial.getDiffuseColor(m_kDiffuse);
        kMaterial.getSpecularColor(m_kSpecular);
        m_fShininess = kMaterial.getShininess();

        /* The new color values: */
        m_kMaterialNew = new Material(m_kAmbient, m_kEmissive, m_kDiffuse, m_kSpecular, m_fShininess);
        m_kMaterialNew.setCapability(Material.ALLOW_COMPONENT_READ);
        m_kMaterialNew.setCapability(Material.ALLOW_COMPONENT_WRITE);

        /* Save the original: */
        m_kMaterialSave = new Material(m_kAmbient, m_kEmissive, m_kDiffuse, m_kSpecular, m_fShininess);
        m_kMaterialSave.setCapability(Material.ALLOW_COMPONENT_READ);

        if (m_iNumPreset < 0) {
            m_iNumPreset = 0;
        }

        if (m_iNumPreset > 0) {

            if (m_iNumPreset > 6) {
                m_iNumPreset = 6;
            }

            initMaterialPreset();
        }
    }

    /**
     * Sets up the rendered scenes. Two spheres, each displayed in a separate canvas window with the same lighting,
     * material, and transparency parameters as the original surface. The "Before" sphere only changes when the "Apply"
     * button is presesd, the "After" sphere changes each time one of the color buttons or the shininess slider is
     * changed
     *
     * @param  fOpacity         float opacity value
     * @param  akGeneralLights  GeneralLight[] light array
     */
    private void setupScene(float fOpacity, GeneralLight[] akGeneralLights) {

        /* Create the Before/After canvases and scene graphs: */
        Canvas3D[] akCanvas3D = new Canvas3D[2 + m_iNumPreset];

        for (int iCanvas = 0; iCanvas < (2 + m_iNumPreset); iCanvas++) {
            akCanvas3D[iCanvas] = createCanvas3D();
        }

        BranchGroup[] akScene = new BranchGroup[2 + m_iNumPreset];
        akScene[BEFORE] = createSceneGraph(fOpacity, m_kMaterialSave);
        akScene[AFTER] = createSceneGraph(fOpacity, m_kMaterialNew);

        for (int iScene = 2; iScene < (2 + m_iNumPreset); iScene++) {
            akScene[iScene] = createSceneGraph(fOpacity, m_akMaterialPreset[iScene - 2]);
        }

        /* Setup the scene lighting: */
        for (int iLights = 0; iLights < akGeneralLights.length; iLights++) {

            for (int iScene = 0; iScene < (2 + m_iNumPreset); iScene++) {
                BranchGroup kLightArray = new BranchGroup();
                kLightArray.setCapability(BranchGroup.ALLOW_DETACH);

                Light kLight = akGeneralLights[iLights].createJava3dLight();
                kLight.setInfluencingBounds(new BoundingSphere(new Point3d(0.0f, 0.0f, 0.0f), 100.0f));
                kLightArray.addChild(kLight);
                akScene[iScene].addChild(kLightArray);
            }
        }

        /* Create the Before/After universes: */
        SimpleUniverse[] akUniverse = new SimpleUniverse[2 + m_iNumPreset];
        m_kImagePanel = new JPanel[2 + m_iNumPreset];

        for (int iScene = 0; iScene < (2 + m_iNumPreset); iScene++) {
            akUniverse[iScene] = new SimpleUniverse(akCanvas3D[iScene]);
            akUniverse[iScene].getViewingPlatform().setNominalViewingTransform();
            akUniverse[iScene].addBranchGraph(akScene[iScene]);

            m_kImagePanel[iScene] = new JPanel(new BorderLayout());
            m_kImagePanel[iScene].add(akCanvas3D[iScene], BorderLayout.CENTER);

            if (iScene < 2) {
                m_kImagePanel[iScene].setMinimumSize(new Dimension(300, 300));
            } else {
                m_kImagePanel[iScene].setMinimumSize(new Dimension(600 / m_iNumPreset, 300));
            }
        }
    }
}
