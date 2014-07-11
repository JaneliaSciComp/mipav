package gov.nih.mipav.view.renderer.WildMagic.Interface;

import java.awt.Frame;
import java.awt.event.WindowAdapter;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.media.opengl.*;
import javax.media.opengl.GLAutoDrawable;
import javax.media.opengl.GLEventListener;
import javax.media.opengl.awt.GLCanvas;
import com.jogamp.opengl.util.Animator;

/*
import com.jogamp.newt.Window;
import com.jogamp.newt.event.KeyEvent;
import com.jogamp.newt.event.KeyListener;
import com.jogamp.newt.event.MouseAdapter;
import com.jogamp.newt.event.MouseEvent;
import com.jogamp.newt.event.MouseListener;
import com.jogamp.newt.event.awt.AWTMouseAdapter;
import com.jogamp.newt.opengl.GLWindow;
*/
import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;

import gov.nih.mipav.view.renderer.WildMagic.Render.*;
import WildMagic.LibFoundation.Mathematics.*;
import WildMagic.LibGraphics.Rendering.*;

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
public class JFrameSurfaceMaterialProperties_WM extends JFrame
        implements WindowListener, ActionListener, ChangeListener /* Slider changes */ {

    //~ Static fields/initializers -------------------------------------------------------------------------------------
    public static int AMBIENT = 0;
    public static int DIFFUSE = 1;
    public static int EMISSIVE = 2;
    public static int SPECULAR = 3;


    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 3077827124095959620L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Before/After index values for the two displayed spheres, canvases, and display panels:. */
    private int AFTER = 1;

    /** Before/After index values for the two displayed spheres, canvases, and display panels:. */
    private int BEFORE = 0;

    /** Pre-defined materials */
    private MaterialState[] m_akMaterialPreset = null;

    /** specular coefficient */
    private float m_fShininess;

    /** Preset Material's examples:. */
    private int m_iNumPreset = 6;

    /** the index of the surface that is being changed in the JPanelSurface */
    private int m_iSurfaceIndex;

    /** Which button is pressed to activate the JColorChooser:. */
    private int m_iWhichButton;

    /** Current color values:. */
    private ColorRGB m_kAmbient;

    /** Material Properties color buttons:. */
    private JButton m_kAmbientColorButton;

    /** Color Chooser dialog:. */
    private JColorChooser m_kColorChooser;

    /** diffuse color */
    private ColorRGB m_kDiffuse;

    /** diffuse color button */
    private JButton m_kDiffuseColorButton;

    /** emissive color */
    private ColorRGB m_kEmissive;

    /** emissive color button */
    private JButton m_kEmissiveColorButton;

    /** Split pane for displaying the canvases side-by-side:. */
    private JSplitPane m_kImagePane;

    /** Canvas display panels:. */
    private JPanel[] m_kImagePanel;

    /** Split pane for displaying the rendered spheres above the interface:. */
    private JSplitPane m_kMainPane;

    /** Materials for new vales and backup:. */
    private MaterialState m_kMaterialNew;

    /** Window display panels: Material properties buttons/slider panel:. */
    private JPanel m_kMaterialPanel;

    /** Saved Material for backup */
    private MaterialState m_kMaterialSave;

    /** Parent class:. */
    private JPanelSurface_WM m_kParent = null;

    /** Shininess slider. */
    private JSlider m_kShininessSlider;

    /** Specular color component */
    private ColorRGB m_kSpecular;

    /** Specular color button */
    private JButton m_kSpecularColorButton;

    /** keeps track of whether or not color has been applied to surface (for
     * backup on cancel) */
    private boolean m_bColorApplied = false;
    
    private SurfaceMaterialDisplay[] m_akSurfaceMaterial;
    private Animator m_kAnimator;

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
    public JFrameSurfaceMaterialProperties_WM(JPanelSurface_WM kParent, int iSurface,
                                              Light[] akGeneralLights,
                                              /*float fOpacity,*/ MaterialState kMaterial )
    {

        /* Create the window: */
        super("Advanced Material Properties");

        m_kParent = kParent;
        m_iSurfaceIndex = iSurface;
        m_kAnimator = new Animator();
        
        /* Setup the material properties of the two spheres, backing up the
         * original data: */
        setupMaterials(kMaterial);

        /* Create the rendered scene, two spheres, a "Before" and "After"
         * sphere: */
        setupScene(akGeneralLights);

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
            m_iWhichButton = AMBIENT;
            kDialog.setVisible(true);
        } else if (kCommand.equals("Diffuse")) {
            m_kColorChooser = new JColorChooser(m_kDiffuseColorButton.getBackground());

            JDialog kDialog = JColorChooser.createDialog(new Frame(), "Set Diffuse color", true, m_kColorChooser,
                                                           this, this);
            m_iWhichButton = DIFFUSE;
            kDialog.setVisible(true);
        } else if (kCommand.equals("Specular")) {
            m_kColorChooser = new JColorChooser(m_kSpecularColorButton.getBackground());

            JDialog kDialog = JColorChooser.createDialog(new Frame(), "Set Specular color", true, m_kColorChooser,
                                                           this, this);
            m_iWhichButton = SPECULAR;
            kDialog.setVisible(true);
        } else if (kCommand.equals("Emissive")) {
            m_kColorChooser = new JColorChooser(m_kEmissiveColorButton.getBackground());

            JDialog kDialog = JColorChooser.createDialog(new Frame(), "Set Emissive color", true, m_kColorChooser,
                                                           this, this);
            m_iWhichButton = EMISSIVE;
            kDialog.setVisible(true);
        }

        /* The JColorChooser has been closed with the OK button. The
         * m_iWhichButton is used to determine which color parameter is being set, and the new color value is read from
         * the m_kColorChooser
         * dialog. The color of the button is the set with the new value: */
        else if (kCommand.equals("OK")) {

            if (m_iWhichButton == AMBIENT) {
                Color kColor = m_kColorChooser.getColor();
                m_kAmbientColorButton.setBackground(kColor);
                m_kMaterialNew.Ambient.Set(kColor.getRed()/255.0f, kColor.getGreen()/255.0f, kColor.getBlue()/255.0f);
            } else if (m_iWhichButton == DIFFUSE) {
                Color kColor = m_kColorChooser.getColor();
                m_kDiffuseColorButton.setBackground(kColor);
                m_kMaterialNew.Diffuse.Set(kColor.getRed()/255.0f, kColor.getGreen()/255.0f, kColor.getBlue()/255.0f);
            } else if (m_iWhichButton == SPECULAR) {
                Color kColor = m_kColorChooser.getColor();
                m_kSpecularColorButton.setBackground(kColor);
                m_kMaterialNew.Specular.Set(kColor.getRed()/255.0f, kColor.getGreen()/255.0f, kColor.getBlue()/255.0f);
            } else if (m_iWhichButton == EMISSIVE) {
                Color kColor = m_kColorChooser.getColor();
                m_kEmissiveColorButton.setBackground(kColor);
                m_kMaterialNew.Emissive.Set(kColor.getRed()/255.0f, kColor.getGreen()/255.0f, kColor.getBlue()/255.0f);
            }

            m_kColorChooser = null;
        }
        /* One of the preset material buttons is pressed, get which button
         * and use the defined material: */
        else if (kCommand.startsWith("Preset")) {
            int iPreset = Integer.valueOf(kCommand.substring("Preset".length())).intValue();
            m_kAmbient.Copy( m_akMaterialPreset[iPreset].Ambient );
            m_kAmbientColorButton.setBackground( new Color(m_kAmbient.R, m_kAmbient.G, m_kAmbient.B) );
            m_kMaterialNew.Ambient.Set(m_kAmbient.R, m_kAmbient.G, m_kAmbient.B );

            m_kDiffuse.Copy( m_akMaterialPreset[iPreset].Diffuse );
            m_kDiffuseColorButton.setBackground( new Color(m_kDiffuse.R, m_kDiffuse.G, m_kDiffuse.B) );
            m_kMaterialNew.Diffuse.Set(m_kDiffuse.R, m_kDiffuse.G, m_kDiffuse.B );

            m_kSpecular.Copy( m_akMaterialPreset[iPreset].Specular );
            m_kSpecularColorButton.setBackground( new Color(m_kSpecular.R, m_kSpecular.G, m_kSpecular.B) );
            m_kMaterialNew.Specular.Set(m_kSpecular.R, m_kSpecular.G, m_kSpecular.B );

            m_kEmissive.Copy( m_akMaterialPreset[iPreset].Emissive );
            m_kEmissiveColorButton.setBackground( new Color(m_kEmissive.R, m_kEmissive.G, m_kEmissive.B) );
            m_kMaterialNew.Emissive.Set(m_kEmissive.R, m_kEmissive.G, m_kEmissive.B );

            m_fShininess = m_akMaterialPreset[iPreset].Shininess;
            //m_kMaterialNew.Shininess = m_fShininess;
            m_kShininessSlider.setValue((int) m_fShininess);
            
            //m_akSurfaceMaterial[1].setMaterial( m_kMaterialNew );
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
            m_fShininess = m_kShininessSlider.getValue();
            m_kMaterialNew.Shininess = m_fShininess;
        }
    }

    /* (non-Javadoc)
     * @see java.awt.event.WindowListener#windowActivated(java.awt.event.WindowEvent)
     */
    public void windowActivated(WindowEvent e) { }

    /* (non-Javadoc)
     * @see java.awt.event.WindowListener#windowClosed(java.awt.event.WindowEvent)
     */
    public void windowClosed(WindowEvent e) { }

    /**
     * windowClosing Catch the windowClosing event so that the local data memebrs can be destroyed.
     *
     * @param  e  WindowEvent
     */
    public void windowClosing(WindowEvent e) {
        m_kAnimator.stop();
        for (int i = 0; i < (2 + m_iNumPreset); i++) {
            m_kAnimator.remove(m_akSurfaceMaterial[i].GetCanvas());
        }
        this.dispose();
    }

    /* (non-Javadoc)
     * @see java.awt.event.WindowListener#windowDeactivated(java.awt.event.WindowEvent)
     */
    public void windowDeactivated(WindowEvent e) { }

    /* (non-Javadoc)
     * @see java.awt.event.WindowListener#windowDeiconified(java.awt.event.WindowEvent)
     */
    public void windowDeiconified(WindowEvent e) { }

    /* (non-Javadoc)
     * @see java.awt.event.WindowListener#windowIconified(java.awt.event.WindowEvent)
     */
    public void windowIconified(WindowEvent e) { }

    /**
     * Starts Animator on windowOpened.
     *
     * @param  e  WindowEvent
     */
    public void windowOpened(WindowEvent e) 
    {
        m_kAnimator.start();
    }

    /**
     * Update the color changes to the Before sphere and the original surface.
     */
    private void applyColorChange() {
        m_kAmbient.Copy( m_kMaterialNew.Ambient );
        m_kEmissive.Copy( m_kMaterialNew.Emissive );
        m_kDiffuse.Copy( m_kMaterialNew.Diffuse );
        m_kSpecular.Copy( m_kMaterialNew.Specular );
        m_fShininess = m_kMaterialNew.Shininess;

        MaterialState kMaterial = new MaterialState();
        kMaterial.Ambient.Copy( m_kAmbient );  
        kMaterial.Emissive.Copy( m_kEmissive );
        kMaterial.Diffuse.Copy( m_kDiffuse );
        kMaterial.Specular.Copy( m_kSpecular );
        kMaterial.Shininess = m_fShininess;
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
     * Up to 6 preset materials can be defined.
     */
    private void initMaterialPreset() {
        m_akMaterialPreset = new MaterialState[6];
        for ( int i = 0; i < 6; i++ )
        {
            m_akMaterialPreset[i] = new MaterialState();
        }
        m_akMaterialPreset[0].Ambient.Set(0f, 0f, 0f); /* ambient */
        m_akMaterialPreset[0].Emissive.Set(0f, 0f, 0f); /* emissive */
        m_akMaterialPreset[0].Diffuse.Set(1f, 0f, 0f); /* diffuse */
        m_akMaterialPreset[0].Specular.Set(0f, 0f, 0f); /* specular */
        m_akMaterialPreset[0].Shininess = 0f; /* shininess */

        m_akMaterialPreset[1].Ambient.Set(0f, 0f, 0f); /* ambient */
        m_akMaterialPreset[1].Emissive.Set(0f, 0f, 0f); /* emissive */
        m_akMaterialPreset[1].Diffuse.Set(0f, 0f, 1f); /* diffuse */
        m_akMaterialPreset[1].Specular.Set(1f, 1f, 1f); /* specular */
        m_akMaterialPreset[1].Shininess = 128f; /* shininess */

        m_akMaterialPreset[2].Ambient.Set(0f, 0f, 0f); /* ambient */
        m_akMaterialPreset[2].Emissive.Set(0f, 0f, 0f); /* emissive */
        m_akMaterialPreset[2].Diffuse.Set(0f, 0.5f, 0f); /* diffuse */
        m_akMaterialPreset[2].Specular.Set(0.5f, 0.5f, 0f); /* specular */
        m_akMaterialPreset[2].Shininess = 33f; /* shininess */

        m_akMaterialPreset[3].Ambient.Set(0f, 0f, 0f); /* ambient */
        m_akMaterialPreset[3].Emissive.Set(0f, 0f, 0f); /* emissive */
        m_akMaterialPreset[3].Diffuse.Set(1f, 0f, 0f); /* diffuse */
        m_akMaterialPreset[3].Specular.Set(1f, 0.5f, 0f); /* specular */
        m_akMaterialPreset[3].Shininess = 25f; /* shininess */

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

        m_kAmbient.Copy( m_kMaterialSave.Ambient );
        m_kEmissive.Copy( m_kMaterialSave.Emissive );
        m_kDiffuse.Copy(  m_kMaterialSave.Diffuse );
        m_kSpecular.Copy( m_kMaterialSave.Specular );
        m_fShininess = m_kMaterialSave.Shininess;

        // restore the per-vertex color and the old Material, then restore the material
        MaterialState kMaterial = new MaterialState();
        kMaterial.Ambient.Copy( m_kAmbient );
        kMaterial.Emissive.Copy( m_kEmissive );
        kMaterial.Diffuse.Copy( m_kDiffuse );
        kMaterial.Specular.Copy( m_kSpecular );
        kMaterial.Shininess = m_fShininess;

        m_akSurfaceMaterial[1].setMaterial( kMaterial );
        m_kParent.setMaterial( kMaterial, m_iSurfaceIndex );
        //m_kParent.restorePerVertexColor( kMaterial, m_iSurfaceIndex );

        m_kMaterialNew.Ambient.Copy( m_kAmbient );
        m_kMaterialNew.Emissive.Copy( m_kEmissive );
        m_kMaterialNew.Diffuse.Copy( m_kDiffuse );
        m_kMaterialNew.Specular.Copy( m_kSpecular );
        m_kMaterialNew.Shininess = m_fShininess;

        m_kAmbientColorButton.setBackground(new Color( m_kAmbient.R, m_kAmbient.G, m_kAmbient.B ));
        m_kDiffuseColorButton.setBackground(new Color( m_kDiffuse.R, m_kDiffuse.G, m_kDiffuse.B ));
        m_kSpecularColorButton.setBackground(new Color( m_kSpecular.R, m_kSpecular.G, m_kSpecular.B ));
        m_kEmissiveColorButton.setBackground(new Color( m_kEmissive.R, m_kEmissive.G, m_kEmissive.B ));
        m_kShininessSlider.setValue((int) m_fShininess);
    }

    /**
     * Sets up the user interface. Button and slider.
     */
    private void setupInterface() {

        /* Color buttons and labels, both are JButtons and can be pressed to
         * change the Ambient, Diffuse, Specular, or Emissive colors: */
        m_kAmbientColorButton = new JButton("       ");
        m_kAmbientColorButton.setBackground(new Color( m_kAmbient.R, m_kAmbient.G, m_kAmbient.B ));
        m_kAmbientColorButton.setToolTipText("Change surface ambient color");
        m_kAmbientColorButton.addActionListener(this);
        m_kAmbientColorButton.setActionCommand("Ambient");

        JButton kAmbientColorButtonLabel = new JButton("Ambient Color");
        kAmbientColorButtonLabel.addActionListener(this);
        kAmbientColorButtonLabel.setActionCommand("Ambient");

        m_kDiffuseColorButton = new JButton("       ");
        m_kDiffuseColorButton.setBackground(new Color( m_kDiffuse.R, m_kDiffuse.G, m_kDiffuse.B ));
        m_kDiffuseColorButton.setToolTipText("Change surface diffuse color");
        m_kDiffuseColorButton.addActionListener(this);
        m_kDiffuseColorButton.setActionCommand("Diffuse");

        JButton kDiffuseColorButtonLabel = new JButton("Diffuse Color");
        kDiffuseColorButtonLabel.addActionListener(this);
        kDiffuseColorButtonLabel.setActionCommand("Diffuse");

        m_kSpecularColorButton = new JButton("       ");
        m_kSpecularColorButton.setBackground(new Color( m_kSpecular.R, m_kSpecular.G, m_kSpecular.B ));
        m_kSpecularColorButton.setToolTipText("Change surface specular color");
        m_kSpecularColorButton.addActionListener(this);
        m_kSpecularColorButton.setActionCommand("Specular");

        JButton kSpecularColorButtonLabel = new JButton("Specular Color");
        kSpecularColorButtonLabel.addActionListener(this);
        kSpecularColorButtonLabel.setActionCommand("Specular");

        m_kEmissiveColorButton = new JButton("       ");
        m_kEmissiveColorButton.setBackground(new Color( m_kEmissive.R, m_kEmissive.G, m_kEmissive.B ));
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
    private void setupMaterials(MaterialState kMaterial) {
        m_kAmbient = new ColorRGB(kMaterial.Ambient);
        m_kEmissive = new ColorRGB(kMaterial.Emissive);
        m_kDiffuse = new ColorRGB(kMaterial.Diffuse);
        m_kSpecular = new ColorRGB(kMaterial.Specular);
        m_fShininess = kMaterial.Shininess;

        /* The new color values: */
        m_kMaterialNew = new MaterialState();
        m_kMaterialNew.Ambient.Copy( m_kAmbient );
        m_kMaterialNew.Emissive.Copy( m_kEmissive );
        m_kMaterialNew.Diffuse.Copy( m_kDiffuse );
        m_kMaterialNew.Specular.Copy(  m_kSpecular );
        m_kMaterialNew.Shininess = m_fShininess;

        /* Save the original: */
        m_kMaterialSave = new MaterialState();
        m_kMaterialSave.Ambient.Copy( m_kAmbient );
        m_kMaterialSave.Emissive.Copy( m_kEmissive );
        m_kMaterialSave.Diffuse.Copy( m_kDiffuse );
        m_kMaterialSave.Specular.Copy( m_kSpecular );
        m_kMaterialSave.Shininess = m_fShininess;

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
     * button is pressed, the "After" sphere changes each time one of the color buttons or the shininess slider is
     * changed
     * @param  akGeneralLights  GeneralLight[] light array
     */
    private void setupScene(Light[] akGeneralLights) {

        /* Create the Before/After displays: */
        m_akSurfaceMaterial = new SurfaceMaterialDisplay[2 + m_iNumPreset];


        m_akSurfaceMaterial[0] = new SurfaceMaterialDisplay( m_kMaterialSave, akGeneralLights, true );
        m_akSurfaceMaterial[1] = new SurfaceMaterialDisplay( m_kMaterialNew, akGeneralLights, true );
        for (int i = 2; i < (2 + m_iNumPreset); i++) {
            m_akSurfaceMaterial[i] = new SurfaceMaterialDisplay(m_akMaterialPreset[i - 2], akGeneralLights, false);
        }
        
        m_kImagePanel = new JPanel[2 + m_iNumPreset];

        for (int i = 0; i < (2 + m_iNumPreset); i++) {
            m_kAnimator.add(m_akSurfaceMaterial[i].GetCanvas());
            m_kImagePanel[i] = new JPanel(new BorderLayout());
            m_kImagePanel[i].add(m_akSurfaceMaterial[i].GetCanvas(), BorderLayout.CENTER);

            if (i < 2) {
                m_kImagePanel[i].setMinimumSize(new Dimension(300, 300));
            } else {
                m_kImagePanel[i].setMinimumSize(new Dimension(600 / m_iNumPreset, 300));
            }
        }
    }
}
