package gov.nih.mipav.view.renderer.WildMagic.Interface;


import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.renderer.WildMagic.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 */
public class JDialogStereoControls extends JInterfaceBase
implements ActionListener, ChangeListener {

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 844490628214011726L;

    /** Label that gives current value of slider. */
    private JLabel mkCurrent;
    
    /** IPD number. */
    private float m_fIPD;

    /** Opacity slider. */
    private JSlider m_kIPDSlider;

    //~ Constructors ---------------------------------------------------------------------------------------------------
    /**
     * Creates new dialog with a slider and close button.
     *
     * @param  theParentFrame  The parent frame
     * @param  initVal         The initial value of the opacity.
     */
    public JDialogStereoControls(VolumeTriPlanarInterface kVolumeViewer, float initVal) {
        super(kVolumeViewer);
        init(initVal);
    }

    /**
     * Sets opacity once close button is pressed.
     *
     * @param  event  Event that triggered function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Close") || command.equals("Apply")) {
            m_fIPD = m_kIPDSlider.getValue() / 1000f;
            if ( m_kVolumeViewer != null )
            {
                m_kVolumeViewer.setIPD( m_fIPD );
            }
            dispose();
        }
    }
    
    public float getIPD()
    {
        System.err.println( m_fIPD );
        return m_fIPD;
    }
    
    public void setIPD(float value)
    {
        m_fIPD = value;
        System.err.println( m_fIPD );
        m_kIPDSlider.setValue((int)(m_fIPD*1000));
        mkCurrent.setText(String.valueOf(m_fIPD));
        if ( m_kVolumeViewer != null )
        {
            m_kVolumeViewer.setIPD( m_fIPD );
        }
    }
    
    /**
     * Sets values based on knob along slider.
     *
     * @param  e  Event that triggered this function
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        if (source == m_kIPDSlider) {
            m_fIPD = m_kIPDSlider.getValue() / 1000f;

            mkCurrent.setText(String.valueOf(m_fIPD));
            if ( m_kVolumeViewer != null )
            {
                m_kVolumeViewer.setIPD( m_fIPD );
            }
        }
    }

    /**
     * Called to close dialog.
     */
    public void close()
    {
        dispose();
    }

    /**
     * Makes slider to set opacity of VOI. Opaque = 1, Transparency = 0.
     *
     * @param  initValue  Initial value of slider.
     */
    private void init(float initValue) {
        setTitle("Stereo IPD");
        m_kIPDSlider = new JSlider(JSlider.HORIZONTAL, 0, 100, (int) (initValue * 100));

        m_kIPDSlider.setMajorTickSpacing(20);
        m_kIPDSlider.setPaintTicks(true);
        m_kIPDSlider.setEnabled(true);
        m_kIPDSlider.addChangeListener(this);

        JLabel maximum = new JLabel(String.valueOf(1));
        maximum.setForeground(Color.black);
        maximum.setFont(MipavUtil.font12);

        mkCurrent = new JLabel(String.valueOf(m_kIPDSlider.getValue() / 100.0f));
        mkCurrent.setForeground(Color.black);
        mkCurrent.setFont(MipavUtil.font12B);

        JLabel minimum = new JLabel(String.valueOf(0));
        minimum.setForeground(Color.black);
        minimum.setFont(MipavUtil.font12);

        JPanel sliderPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel.add(m_kIPDSlider, gbc);

        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.weightx = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.fill = GridBagConstraints.NONE;

        sliderPanel.add(minimum, gbc);

        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = .5;

        sliderPanel.add(mkCurrent, gbc);

        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;

        sliderPanel.add(maximum, gbc);
        sliderPanel.setBorder(buildTitledBorder("IPD"));

        JPanel buttonPanel = new JPanel();
        buildCancelButton();
        cancelButton.setText("Close");
        buttonPanel.add(cancelButton);

        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(sliderPanel);
        mainPanel.add(buttonPanel, BorderLayout.SOUTH);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        getContentPane().add(mainPanel);

        pack();
        setVisible(true);
    }
}
