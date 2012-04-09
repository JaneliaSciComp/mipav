package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;


public class JPanelVolume4D extends JInterfaceBase
    implements ChangeListener {

    /**  */
    private static final long serialVersionUID = -5083630753484806459L;
    /** The scroll pane holding the panel content. Useful when the screen is small. */
    private JScrollPane scroller;
    private JSlider m_k4DSlider;
    private JSlider m_kFrameRateSlider;
    /**
     * Constructor.
     * @param kVolumeViewer parent frame.
     */
    public JPanelVolume4D( VolumeTriPlanarInterface kVolumeViewer )
    {
        super(kVolumeViewer);
        init();
    }
    
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Play4D")) 
        {
            m_kVolumeViewer.play4D(true);
        } 
        else if (command.equals("Stop4D")) 
        {
            m_kVolumeViewer.play4D(false);
        }
    }

    /**
     * Dispose the local memory.
     */
    public void disposeLocal() {}



    /**
     * Resizing the control panel with ViewJFrameVolumeView's frame width and height.
     *
     * @param  panelWidth   int width
     * @param  frameHeight  int height
     */
    public void resizePanel(int panelWidth, int frameHeight) {
        frameHeight = frameHeight - (40 * 2);
        scroller.setPreferredSize(new Dimension(panelWidth, frameHeight));
        scroller.setSize(new Dimension(panelWidth, frameHeight));
        scroller.revalidate();
    }
    

    /* (non-Javadoc)
     * @see javax.swing.event.ChangeListener#stateChanged(javax.swing.event.ChangeEvent)
     */
    public void stateChanged(ChangeEvent event)
    {
        if (event.getSource() == m_k4DSlider)
        {
            m_kVolumeViewer.setTimeSlice( m_k4DSlider.getValue() );
        }
        if (event.getSource() == m_kFrameRateSlider)
        {
            m_kVolumeViewer.setAnimationSpeed( m_kFrameRateSlider.getValue()/100.0f );
        }
    }
    
    public void setTimeSlice( int iTSlice )
    {
        m_k4DSlider.removeChangeListener(this);
        m_k4DSlider.setValue(iTSlice);
        m_k4DSlider.addChangeListener(this);
    }
    
    /**
     * Initializes the GUI components.
     */
    private void init() {
        // setSize(400, 256);

        // Scroll panel that hold the control panel layout in order to use JScrollPane
        JPanel mainScrollPanel = new JPanel();
        mainScrollPanel.setLayout(new BorderLayout());

        scroller = new JScrollPane(mainScrollPanel, JScrollPane.VERTICAL_SCROLLBAR_AS_NEEDED,
                                   JScrollPane.HORIZONTAL_SCROLLBAR_AS_NEEDED);

        mainPanel = new JPanel(new BorderLayout());



        JButton playButton = new JButton("Play");
        playButton.addActionListener(this);
        playButton.setActionCommand("Play4D");
        playButton.setFont(MipavUtil.font12B);
        playButton.setPreferredSize(MipavUtil.defaultButtonSize);

        JButton stopButton = new JButton("Stop");
        stopButton.addActionListener(this);
        stopButton.setActionCommand("Stop4D");
        stopButton.setFont(MipavUtil.font12B);
        stopButton.setPreferredSize(MipavUtil.defaultButtonSize);

        
        JLabel kTimeSlice = new JLabel("Time: ");
        m_k4DSlider = new JSlider(0, m_kVolumeViewer.getImageA().getExtents()[3]-1, 0 );
        m_k4DSlider.setMajorTickSpacing(1);
        m_k4DSlider.setMinorTickSpacing(1);
        m_k4DSlider.setPaintTicks(true);
        m_k4DSlider.addChangeListener(this);
        m_k4DSlider.setPaintLabels(true);
        m_k4DSlider.setSnapToTicks(true);
        m_k4DSlider.setEnabled(true);


        JLabel kMin = new JLabel("Animation speed:  (min)");
        JLabel kMax = new JLabel("(max) ");
        m_kFrameRateSlider = new JSlider(0, 100, 100);
        m_kFrameRateSlider.addChangeListener(this);

        GridBagConstraints kGBC = new GridBagConstraints();
        GridBagLayout kGrid = new GridBagLayout();
        kGBC.gridx = 0;
        kGBC.gridy = 0;
        
        JPanel kAnimatePanel = new JPanel(kGrid);
        kAnimatePanel.setBorder(buildTitledBorder("Volume Animation"));
        kAnimatePanel.add( playButton, kGBC ); kGBC.gridx++;
        kAnimatePanel.add( stopButton, kGBC );
        kGBC.gridx = 0; kGBC.gridy++;  kGBC.gridy++;
        kAnimatePanel.add( kMin, kGBC ); kGBC.gridx++;
        kAnimatePanel.add( m_kFrameRateSlider, kGBC ); kGBC.gridx++;
        kAnimatePanel.add( kMax, kGBC );
        
        JPanel kPanel = new JPanel(kGrid);
        kPanel.setBorder(buildTitledBorder("Select Sub-Volume"));
        kGBC.gridx = 0;
        kGBC.gridy = 0;
        kPanel.add( kTimeSlice, kGBC ); kGBC.gridx++;
        kPanel.add( m_k4DSlider, kGBC );
        
        Box contentBox = new Box(BoxLayout.Y_AXIS);
        contentBox.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        contentBox.add(kPanel);
        contentBox.add(kAnimatePanel);

        mainScrollPanel.add(contentBox);
 
        mainPanel.add(scroller, BorderLayout.CENTER);
    }
}
