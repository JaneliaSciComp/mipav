package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.view.ViewJColorChooser;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;
import gov.nih.mipav.view.renderer.WildMagic.Render.MultiDimensionalTransfer.VolumeImageMultiDimensionalTransfer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;

import com.sun.opengl.util.Animator;

public class JPanelMultiDimensionalTransfer extends JInterfaceBase implements ChangeListener {


    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 926266253314679850L;

    /** Color button for changing color. */
    protected JButton colorButton;
    protected JSlider alphaSlider;
    private JSlider boundaryEmphasisSlider;
    private ButtonGroup m_kGroup = new ButtonGroup();

    /** The scroll pane holding the panel content. Useful when the screen is small. */
    private JScrollPane scroller;
    
    /** Scroll panel that holding the all the control components. */
    private DrawingPanel scrollPanel;
    
    private Animator m_kAnimator;
    private VolumeImageMultiDimensionalTransfer m_kMultiHistogram;



    /**
     * Creates new dialog for turning bounding box frame on and off.
     * @param  parent  parent frame.
     */
    public JPanelMultiDimensionalTransfer(VolumeTriPlanarInterface parent,
                                Animator kAnimator, VolumeImage kVolumeImage) {
        m_kVolumeViewer = parent;
        m_kAnimator = kAnimator;
        //m_kAnimator = new Animator();
        m_kMultiHistogram = new VolumeImageMultiDimensionalTransfer( parent, kVolumeImage);
        m_kMultiHistogram.SetAnimator(m_kAnimator);
        m_kMultiHistogram.SetInterface(this);

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
        if ( event.getActionCommand().equals("SquareWidget") )
        {
            m_kMultiHistogram.setWidget( "Square" );
        }
        else if ( event.getActionCommand().equals("TriWidget") )
        {
            m_kMultiHistogram.setWidget( "Triangle" );
        }
    }

    /**
     * Dispose memory.
     */
    public void dispose()
    {
        colorButton = null;
        colorChooser = null;
        boundaryEmphasisSlider = null;
        m_kGroup = null;
        scroller = null;
        scrollPanel = null;
        m_kAnimator = null;
        m_kMultiHistogram.dispose();
        m_kMultiHistogram = null;
        super.dispose();
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
    }
    
    public void setButtonColor(JButton _button, Color _color)
    {
        super.setButtonColor( _button, _color );
        float fAlpha = alphaSlider.getValue()/100.0f;
        m_kMultiHistogram.setColor( new ColorRGBA( _color.getRed()/255.0f, _color.getGreen()/255.0f, _color.getBlue()/255.0f, fAlpha ) );
    }
    
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        if (source == alphaSlider) {
            float fAlpha = alphaSlider.getValue()/100.0f;
            Color kColor = colorButton.getBackground();
            m_kMultiHistogram.setColor( new ColorRGBA( kColor.getRed()/255.0f, kColor.getGreen()/255.0f, kColor.getBlue()/255.0f, fAlpha ) );
        }
        

        if (source == boundaryEmphasisSlider) {
            float fAlpha = boundaryEmphasisSlider.getValue()/100.0f;
            m_kMultiHistogram.setBoundary( fAlpha );
        }
    }


    public void updateColorButton( float[] afColor, float fColor )
    {
        colorButton.setBackground( new Color( afColor[0], afColor[1], afColor[2]) );
        alphaSlider.setValue( (int)(afColor[3] * 100) );
        boundaryEmphasisSlider.setValue( (int)(fColor * 100) );
    }
    
    /**
     * Initializes GUI components.
     */
    private void init() {
        GridBagConstraints kGBC = new GridBagConstraints();
        GridBagLayout kGrid = new GridBagLayout();
        kGBC.gridx = 0;
        kGBC.gridy = 0;
        JPanel buttonPanel = new JPanel( kGrid );
        JRadioButton kSquare = new JRadioButton( "Square", true );
        kSquare.addActionListener(this);
        kSquare.setActionCommand("SquareWidget");
        buttonPanel.add( new JLabel( "Select Widget Type: "), kGBC );
        kGBC.gridx++;
        buttonPanel.add( kSquare, kGBC );
        kGBC.gridx++;
        JRadioButton kTriangle = new JRadioButton( "Triangle", true );
        kTriangle.addActionListener(this);
        kTriangle.setActionCommand("TriWidget");
        buttonPanel.add( kTriangle, kGBC);
        m_kGroup.add(kSquare);
        m_kGroup.add(kTriangle);
        
        colorButton = new JButton();
        colorButton.setPreferredSize(new Dimension(25, 25));
        colorButton.setToolTipText("Change histogram color");
        colorButton.addActionListener(this);
        colorButton.setBackground(Color.white);
        colorButton.setEnabled(true);   

        kGBC.gridx = 0;
        kGBC.gridy++;
        buttonPanel.add( new JLabel( "Histogram Constant Color: "), kGBC );
        kGBC.gridx++;
        buttonPanel.add( colorButton, kGBC );

        kGBC.gridx = 0;
        kGBC.gridy++;
        alphaSlider = new JSlider();
        alphaSlider.addChangeListener(this);
        buttonPanel.add( new JLabel( "Histogram opacity: "), kGBC );
        kGBC.gridx++;
        buttonPanel.add( alphaSlider, kGBC );
        
        kGBC.gridx = 0;
        kGBC.gridy++;
        boundaryEmphasisSlider = new JSlider();
        boundaryEmphasisSlider.addChangeListener(this);
        buttonPanel.add( new JLabel( "Boundary Emphasis Slider: "), kGBC );
        kGBC.gridx++;
        buttonPanel.add( boundaryEmphasisSlider, kGBC );

        JPanel panel = new JPanel(new BorderLayout());
        panel.add(m_kMultiHistogram.GetCanvas(), BorderLayout.CENTER);
        panel.setPreferredSize(new Dimension(256, 256));
        panel.setBackground(Color.white);
        //panel.add(colorButton, BorderLayout.CENTER);
        //panel.setPreferredSize(new Dimension(m_kMultiHistogram.GetWidth(), m_kMultiHistogram.GetHeight()));
        //panel.setMinimumSize(new Dimension(m_kMultiHistogram.GetWidth(), m_kMultiHistogram.GetHeight()));

        // Scroll panel that hold the control panel layout in order to use JScrollPane

        GridBagLayout gbLayout = new GridBagLayout();
        GridBagConstraints gbConstraints = new GridBagConstraints();
        
        scrollPanel = new DrawingPanel();
        scrollPanel.setLayout(gbLayout);
        gbConstraints.gridx = 0;
        gbConstraints.gridy = 0;
        //scrollPanel.add(panel, BorderLayout.CENTER);   
        //scrollPanel.add(buttonPanel, BorderLayout.SOUTH);   
        scrollPanel.add(panel, gbConstraints);   
        gbConstraints.gridy++;
        scrollPanel.add(buttonPanel, gbConstraints);   

        scroller = new JScrollPane(scrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        mainPanel = new JPanel();
        mainPanel.add(scroller);
    }
}
