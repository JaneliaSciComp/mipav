package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;

import gov.nih.mipav.view.renderer.WildMagic.*;


/**
 */
public abstract class JInterfaceBase extends JDialog implements ActionListener
{
    /** The main control. */
    protected JPanel mainPanel = null;    

    /**
     * Cancel button is used on most dialogs. Defining it in the base allows default actions if the user presses return
     * and the button is in focus.
     */
    protected JButton cancelButton;
    /**
     * OK button is used on most dialogs. Defining it in the base allows default actions if the user presses return and
     * the button is in focus.
     */
    protected JButton OKButton;
    
    /** Raycast based renderer reference, raycast renderer or shear warp renderer. */
    protected VolumeTriPlanarRender rayBasedRenderWM = null;

    /** Render base. */
    protected VolumeTriPlanarInterface m_kVolumeViewer = null;

    public JInterfaceBase() {}
    
    /**
     * 3D texture surface renderer clipping dialog control.
     *
     * @param  xBox    float unit box x length
     * @param  yBox    float unit box y length
     * @param  zBox    float unit box z length
     */
    public JInterfaceBase(VolumeTriPlanarInterface kVolumeViewer)
    {
        m_kVolumeViewer = kVolumeViewer;
        rayBasedRenderWM = m_kVolumeViewer.getVolumeGPU();
    }

    /**
     * Get the main control panel.
     *
     * @return  mainPanel the whole control panel.
     */
    public JPanel getMainPanel() {
        return mainPanel;
    }


    /**
     * Builds the cancel button. Sets it internally as well return the just-built button.
     *
     * @return JButton cancel button
     */
    protected JButton buildCancelButton() {
        cancelButton = new JButton("Cancel");
        cancelButton.addActionListener(this);

        // cancelButton.setToolTipText("Cancel action.");
        cancelButton.setMinimumSize(MipavUtil.defaultButtonSize);
        cancelButton.setPreferredSize(MipavUtil.defaultButtonSize);
        cancelButton.setFont(MipavUtil.font12B);

        return cancelButton;
    }
    
    /**
     * Builds the OK button. Sets it internally as well return the just-built button.
     *
     * @return  JButton ok button
     */
    protected JButton buildOKButton() {
        OKButton = new JButton("OK");
        OKButton.addActionListener(this);

        // OKButton.setToolTipText("Accept values and perform action.");
        OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
        OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
        OKButton.setFont(MipavUtil.font12B);

        return OKButton;
    }

    /**
     * Builds a titled border with the given title, an etched border, and the
     * proper font and color.  Changed to public static member so that it can
     * be used for other JPanels not inherited from this base class.
     * @param   title  Title of the border
     *
     * @return  The titled border.
     */
    public static TitledBorder buildTitledBorder(String title) {
        return new TitledBorder(new EtchedBorder(), title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B,
                                Color.black);
    }
    

}
