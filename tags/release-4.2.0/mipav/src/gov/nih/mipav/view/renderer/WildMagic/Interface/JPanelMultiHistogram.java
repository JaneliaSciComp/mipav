package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.view.ViewJColorChooser;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImageHistogram;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.ActionEvent;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;

import com.sun.opengl.util.Animator;

public class JPanelMultiHistogram extends JInterfaceBase {


    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 926266253314679850L;

    /** Color button for changing color. */
    protected JButton colorButton;

    /** The scroll pane holding the panel content. Useful when the screen is small. */
    private JScrollPane scroller;
    
    /** Scroll panel that holding the all the control components. */
    private DrawingPanel scrollPanel;
    
    private Animator m_kAnimator;
    private VolumeImageHistogram m_kMultiHistogram;



    /**
     * Creates new dialog for turning bounding box frame on and off.
     * @param  parent  parent frame.
     */
    public JPanelMultiHistogram(VolumeTriPlanarInterface parent,
                                Animator kAnimator, VolumeImage kVolumeImage) {
        m_kVolumeViewer = parent;
        m_kAnimator = kAnimator;
        m_kMultiHistogram = new VolumeImageHistogram( parent, kVolumeImage);
        m_kMultiHistogram.SetAnimator(m_kAnimator);

        //VolumeImageHistogram.main(parent, kVolumeImage, true);
        init();
    }
    
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        if ( source == colorButton )
        {
        colorChooser = new ViewJColorChooser(new Frame(), "Pick surface color", new OkColorListener(colorButton),
                new CancelListener());
        }
    }

    /**
     * Dispose memory.
     */
    public void dispose() {
        colorButton = null;
        colorChooser = null;
    }

    /**
     * Resizing the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   int width
     * @param  frameHeight  int height
     */
    public void resizePanel(int panelWidth, int frameHeight) {
        int iWidth = Math.max( panelWidth, m_kMultiHistogram.GetWidth() );
        int iHeight = Math.max( frameHeight - 40, m_kMultiHistogram.GetHeight() );
        scroller.setPreferredSize(new Dimension(iWidth, iHeight));
        scroller.setSize(new Dimension(iWidth, iHeight));
        scroller.revalidate();
        System.err.println( "JPanelMultiHistogram: resizePanel " + panelWidth + " " + frameHeight );
    }
    
    public void setButtonColor(JButton _button, Color _color)
    {
        super.setButtonColor( _button, _color );
        m_kMultiHistogram.setColor( new ColorRGBA( _color.getRed()/255.0f, _color.getGreen()/255.0f, _color.getBlue()/255.0f, 1.0f ) );
    }
    
    /**
     * Initializes GUI components.
     */
    private void init() {
        JPanel buttonPanel = new JPanel();
        colorButton = new JButton();
        colorButton.setPreferredSize(new Dimension(25, 25));
        colorButton.setToolTipText("Change box frame color");
        colorButton.addActionListener(this);
        colorButton.setBackground(Color.red);
        colorButton.setEnabled(true);   
        buttonPanel.add( colorButton );

        JPanel panel = new JPanel(new BorderLayout());
        panel.add(m_kMultiHistogram.GetCanvas(), BorderLayout.CENTER);
        //panel.add(colorButton, BorderLayout.CENTER);
        panel.setPreferredSize(new Dimension(m_kMultiHistogram.GetWidth(), m_kMultiHistogram.GetHeight()));
        panel.setMinimumSize(new Dimension(m_kMultiHistogram.GetWidth(), m_kMultiHistogram.GetHeight()));

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        scrollPanel = new DrawingPanel();
        scrollPanel.setLayout(new BorderLayout());
        scrollPanel.add(panel, BorderLayout.CENTER);   
        scrollPanel.add(buttonPanel, BorderLayout.SOUTH);   

        scroller = new JScrollPane(scrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        mainPanel = new JPanel();
        mainPanel.add(scroller);
    }
}
