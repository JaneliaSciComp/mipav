package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJColorChooser;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarRender;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;


/**
 */
public abstract class JInterfaceBase extends JDialog implements ActionListener
{

    /**  */
    private static final long serialVersionUID = 1185755612726454935L;

    /**
     * Pick up the selected color and call method to change the color.
     */
    class OkColorListener implements ActionListener {

        /** Color Button */
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
         * Get color from chooser and set button and color.
         *
         * @param  e  Event that triggered function.
         */
        public void actionPerformed(ActionEvent e) {
            Color color = colorChooser.getColor();

            button.setBackground(color);
            setButtonColor(button, color);
        }
    }
    
    /**
     * Does nothing.
     */
    public class CancelListener implements ActionListener {

        /* (non-Javadoc)
         * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
         */
        public void actionPerformed(ActionEvent e) { }
    }

    /**
     * Wrapper in order to hold the control panel layout in the JScrollPane.
     */
    public class DrawingPanel extends JPanel {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -6456589720445279985L;

        /* (non-Javadoc)
         * @see javax.swing.JComponent#paintComponent(java.awt.Graphics)
         */
        protected void paintComponent(Graphics g) {
            super.paintComponent(g);
        }
    }
    

    /** Color chooser dialog. */
    protected ViewJColorChooser colorChooser;
    
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
    protected transient VolumeTriPlanarRender rayBasedRenderWM = null;

    /** Render base. */
    protected transient VolumeTriPlanarInterface m_kVolumeViewer = null;

    /**
     * Default constructor.
     */
    public JInterfaceBase() {}
    
    /**
     * Construct Base.
     * @param kVolumeViewer parent frame.
     */
    public JInterfaceBase(VolumeTriPlanarInterface kVolumeViewer)
    {
        m_kVolumeViewer = kVolumeViewer;
        rayBasedRenderWM = m_kVolumeViewer.getVolumeGPU();
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
        cancelButton.setActionCommand("Cancel");

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
        OKButton.setActionCommand("OK");

        // OKButton.setToolTipText("Accept values and perform action.");
        OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
        OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
        OKButton.setFont(MipavUtil.font12B);

        return OKButton;
    }
    
    /**
     * Set the color of the button. Derived classes may also perform other functions.
     * @param _button button.
     * @param _color color.
     */
    public void setButtonColor(JButton _button, Color _color)
    {
        if ( (_button != null) && (_color != null) )
        {
            _button.setBackground(_color);
        }
    }
    
    public void updateColorButton( float[] afColor, float fColor ) {}

}
