package gov.nih.mipav.view.renderer.WildMagic.Interface;

import gov.nih.mipav.view.ViewJColorChooser;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibGraphics.Rendering.AlphaState;


public class JPanelCustomBlend extends JInterfaceBase implements ChangeListener {
    /**  */
    private static final long serialVersionUID = -6719268114423090548L;
    protected JButton colorButton;

    protected JSlider alphaSlider;

    /** The scroll pane holding the panel content. Useful when the screen is small. */
    private JScrollPane scroller;

    /** The combo box for the Blend Equation mode to display. */
    private JComboBox m_kBlendEquationColor;
    //private JComboBox m_kBlendEquationAlpha;

    /** The combo box for the Logic Operation mode to display. */
    private JComboBox m_kLogicOp;

    private JComboBox m_kSrcBlend;
    private JComboBox m_kDstBlend;
    private boolean m_bUpdate = true;

    /**
     * Constructor.
     * @param kVolumeViewer parent frame.
     */
    public JPanelCustomBlend( VolumeTriPlanarInterface kVolumeViewer )
    {
        super(kVolumeViewer);
        init();
    }

    public void actionPerformed(ActionEvent event)
    {
        Object source = event.getSource();
        if ( source == colorButton )
        {
            colorChooser = new ViewJColorChooser(new Frame(), "Pick surface color", new OkColorListener(colorButton),
                    new CancelListener());
        }
        else
        {
            updateVolumeRenderer();
        }
    }

    /**
     * Dispose the local memory.
     */
    public void disposeLocal() {
        colorButton = null;
        alphaSlider = null;
        scroller = null;
        m_kBlendEquationColor = null;
        m_kLogicOp = null;
        m_kSrcBlend = null;
        super.dispose();
    }

    public int getEquation()
    {
        return m_kBlendEquationColor.getSelectedIndex();
    }
    public void setEquation(int i)
    {
        m_kBlendEquationColor.setSelectedIndex(i);
    }

    public int getSource()
    {
        return m_kSrcBlend.getSelectedIndex();
    }

    public void setSource(int i)
    {
        m_kSrcBlend.setSelectedIndex(i);
    }

    public int getDestination()
    {
        return m_kDstBlend.getSelectedIndex();
    }

    public void setDestination(int i)
    {
        m_kDstBlend.setSelectedIndex(i);
    }

    public Color getColor()
    {
        return colorButton.getBackground();
    }

    public void setColor( Color c )
    {
        setButtonColor( colorButton, c );
    }

    public int getAlpha()
    {
        return alphaSlider.getValue();
    }

    public void setAlpha( int value )
    {
        alphaSlider.setValue(value);
    }

    public void setUpdate( boolean value )
    {
        m_bUpdate = value;
    }

    public void setButtonColor(JButton _button, Color _color)
    {
        super.setButtonColor( _button, _color );
        updateVolumeRenderer();
    }


    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        if (source == alphaSlider) {
            updateVolumeRenderer();
        }
    }

    /**
     * Initializes the GUI components.
     */
    private void init()
    {

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        JPanel mainScrollPanel = new JPanel();
        mainScrollPanel.setLayout(new BorderLayout());

        scroller = new JScrollPane(mainScrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel = new JPanel(new BorderLayout());

        String[] akBlendEq = new String[AlphaState.BlendEquationMap.size()-1];
        for ( int i = 0; i < AlphaState.BlendEquationMap.size()-1; i++ )
        {
            akBlendEq[i] = new String( AlphaState.BlendEquationMap.get(i).Name().substring(3) );

        }
        m_kBlendEquationColor = new JComboBox(akBlendEq);
        m_kBlendEquationColor.addActionListener(this);
        m_kBlendEquationColor.setActionCommand("BlendEquationColor");

        m_kLogicOp = new JComboBox(new String[]
            { "CLEAR",
              "COPY",
              "NOOP",
              "SET",
              "COPY_INVERTED",
              "AND_REVERSE",
              "OR_REVERSE",
              "AND",
              "OR",
              "NAND",
              "NOR",
              "XOR",
              "EQUIV",
              "AND_INVERTED",
              "OR_INVERTED"
            });
        m_kLogicOp.addActionListener(this);
        m_kLogicOp.setActionCommand("LogicOp");

        String[] akSrcBlend = new String[AlphaState.SrcBlendModeMap.size()-1];
        for ( int i = 0; i < AlphaState.SrcBlendModeMap.size()-1; i++ )
        {
            akSrcBlend[i] = new String( AlphaState.SrcBlendModeMap.get(i).Name().substring(4) );

        }

        m_kSrcBlend = new JComboBox(akSrcBlend);
        m_kSrcBlend.addActionListener(this);
        m_kSrcBlend.setActionCommand("SrcBlend");

        String[] akDstBlend = new String[AlphaState.DstBlendModeMap.size()-1];
        for ( int i = 0; i < AlphaState.DstBlendModeMap.size()-1; i++ )
        {
            akDstBlend[i] = new String( AlphaState.DstBlendModeMap.get(i).Name().substring(4) );

        }
        m_kDstBlend = new JComboBox(akDstBlend);
        m_kDstBlend.addActionListener(this);
        m_kDstBlend.setActionCommand("DstBlend");



        JPanel cbPanel = new JPanel(new GridLayout(0,2));
        cbPanel.add(new JLabel("RGB Blend Equation:"));
        cbPanel.add(m_kBlendEquationColor);
//         cbPanel.add(new JLabel("Logic Operations:"));
//         cbPanel.add(m_kLogicOp);
        cbPanel.add(new JLabel("Source Blend:"));
        cbPanel.add(m_kSrcBlend);
        cbPanel.add(new JLabel("Destination Blend:"));
        cbPanel.add(m_kDstBlend);


        colorButton = new JButton();
        colorButton.setPreferredSize(new Dimension(25, 25));
        colorButton.setToolTipText("Change constant blend color");
        colorButton.addActionListener(this);
        colorButton.setBackground(Color.white);
        colorButton.setEnabled(true);


        GridBagConstraints kGBC = new GridBagConstraints();
        GridBagLayout kGrid = new GridBagLayout();
        kGBC.gridx = 0;
        kGBC.gridy = 0;
        JPanel buttonPanel = new JPanel( kGrid );

        kGBC.gridx = 0;
        kGBC.gridy++;
        buttonPanel.add( new JLabel( "Constant Blend Color: "), kGBC );
        kGBC.gridx++;
        buttonPanel.add( colorButton, kGBC );

        kGBC.gridx = 0;
        kGBC.gridy++;
        alphaSlider = new JSlider();
        alphaSlider.addChangeListener(this);
        buttonPanel.add( new JLabel( "Constant Blend Alpha: "), kGBC );
        kGBC.gridx++;
        buttonPanel.add( alphaSlider, kGBC );


        Box contentBox = new Box(BoxLayout.Y_AXIS);
        contentBox.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        contentBox.add(cbPanel);
        contentBox.add(buttonPanel);

        mainScrollPanel.add(contentBox, BorderLayout.NORTH);

        mainPanel.add(scroller, BorderLayout.CENTER);
    }

    public void updateVolumeRenderer()
    {
        float fAlpha = alphaSlider.getValue()/100.0f;
        Color kColor = colorButton.getBackground();
        ColorRGBA kColorAlpha =  new ColorRGBA( kColor.getRed()/255.0f,
                                                kColor.getGreen()/255.0f,
                                                kColor.getBlue()/255.0f,
                                                fAlpha );
        if ( m_bUpdate )
        {
            m_kVolumeViewer.SetCustomBlend( m_kBlendEquationColor.getSelectedIndex(),
                    m_kLogicOp.getSelectedIndex(),
                    m_kSrcBlend.getSelectedIndex(),
                    m_kDstBlend.getSelectedIndex(), kColorAlpha  );
        }
    }

}
