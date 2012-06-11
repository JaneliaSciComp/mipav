package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.renderer.J3D.surfaceview.*;
import gov.nih.mipav.view.renderer.WildMagic.Interface.*;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManagerInterface;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * Simple dialog to change Opacity Settings (1 = opaque, 0 = transparent).
 *
 * @version  1.0 Sept 17, 1999
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class JDialogOpacityControls extends JDialogBase implements ChangeListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 844490628214011726L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Controls affected by opacity changes. */
    private ViewControlsImage controls;

    /** Label that gives current value of slider. */
    private JLabel current;

    /** Opacity number. */
    private float opacity;

    /** Opacity slider. */
    private JSlider opacitySlider;

    /** SurfacePaint reference */
    private SurfacePaint surfacePaint = null;
    /** SurfacePaint reference */
    private SurfacePaint_WM surfacePaint_WM = null;
    
    private VOIManagerInterface voiManager = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog with a slider and close button.
     *
     * @param  theParentFrame  The parent frame
     * @param  cntrls          The controls that the opacity will apply to.
     */
    public JDialogOpacityControls(Frame theParentFrame, ViewControlsImage cntrls) {
        super(theParentFrame, false);
        controls = cntrls;
        String prefOpacity = Preferences.getProperty(Preferences.PREF_PAINT_OPACITY);
    	if (prefOpacity != null && ! prefOpacity.trim().equals("")) {
    		try{
    			float prefOpacityFloat = Float.valueOf(prefOpacity).floatValue();
    			if(prefOpacityFloat < 0 || prefOpacityFloat > 1) {
    				init(controls.getTools().getOpacity());
    			}else {
    				init(prefOpacityFloat);
    			}
    			
    		}catch(Exception e) {
    			e.printStackTrace();
    			init(controls.getTools().getOpacity());
    		}
    	}else {
    		init(controls.getTools().getOpacity());
    	}
    }

    /**
     * Creates new dialog with a slider and close button.
     *
     * @param  theParentFrame  The parent frame
     * @param  initVal         The initial value of the opacity.
     */
    public JDialogOpacityControls(Frame theParentFrame, float initVal) {
        super(theParentFrame, false);
        init(initVal);
    }

    /**
     * Creates new dialog with a slider and close button.
     *
     * @param  theParentFrame  The parent frame
     * @param  cntrls          The controls that the opacity will apply to.
     * @param  isVisible       Dialog visible or not
     */
    public JDialogOpacityControls(Frame theParentFrame, ViewControlsImage cntrls, boolean isVisible) {
        super(theParentFrame, false);
        controls = cntrls;
        // init(controls.getTools().getOpacity());
    }

    /**
     * Creates new dialog with a slider and close button.
     *
     * @param  theParentFrame  The parent frame
     * @param  initVal         The initial value of the opacity.
     * @param  isVisible       Dialog visible or not
     */
    public JDialogOpacityControls(Frame theParentFrame, float initVal, boolean isVisible) {
        super(theParentFrame, false);
        // init(initVal);
    }

    /**
     * Creates new dialog with a slider and close button.
     *
     * @param  theParentFrame  The parent frame
     * @param  initVal         The initial value of the opacity.
     */
    public JDialogOpacityControls(Frame theParentFrame, SurfacePaint surfacePaint, float initVal) {
        super(theParentFrame, false);
        this.surfacePaint = surfacePaint;
        init(initVal);
    }
    
    /**
     * Creates new dialog with a slider and close button.
     *
     * @param  theParentFrame  The parent frame
     * @param  initVal         The initial value of the opacity.
     */
    public JDialogOpacityControls(Frame theParentFrame, SurfacePaint_WM surfacePaint, float initVal) {
        super(theParentFrame, false);
        this.surfacePaint_WM = surfacePaint;
        init(initVal);
    }
    
    public JDialogOpacityControls(Frame theParentFrame, VOIManagerInterface voiManager, float initVal) {
        super(theParentFrame, false);
        this.voiManager = voiManager;
        init(initVal);
    }


    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Sets opacity once close button is pressed.
     *
     * @param  event  Event that triggered function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("Close") || command.equals("Apply")) {
            opacity = opacitySlider.getValue() / 100f;

            if (controls != null) {
                controls.getTools().setOpacity(opacity);
            }
            if ( surfacePaint != null )
            {
                surfacePaint.setOpacity( opacity );
            }
            if ( surfacePaint_WM != null )
            {
                surfacePaint_WM.setOpacity( opacity );
            }
            if ( voiManager != null )
            {
                voiManager.setOpacity( opacity );
            }

            dispose();
        }
    }

    /**
     * Makes slider to set opacity of VOI. Opaque = 1, Transparency = 0.
     *
     * @param   initValue  Initial value of slider.
     *
     * @return  DOCUMENT ME!
     */
    public JPanel getMainPanel(float initValue) {
        setTitle("Paint Opacity");
        opacitySlider = new JSlider(JSlider.HORIZONTAL, 0, 100, (int) (initValue * 100));

        opacitySlider.setMajorTickSpacing(20);
        opacitySlider.setPaintTicks(true);
        opacitySlider.setEnabled(true);
        opacitySlider.addChangeListener(this);

        JLabel maximum = new JLabel(String.valueOf(1));
        maximum.setForeground(Color.black);
        maximum.setFont(serif12);

        current = new JLabel(String.valueOf(opacitySlider.getValue() / 100.0f));
        current.setForeground(Color.black);
        current.setFont(serif12B);

        JLabel minimum = new JLabel(String.valueOf(0));
        minimum.setForeground(Color.black);
        minimum.setFont(serif12);

        JPanel sliderPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel.add(opacitySlider, gbc);

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

        sliderPanel.add(current, gbc);

        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;

        sliderPanel.add(maximum, gbc);
        sliderPanel.setBorder(buildTitledBorder("Opacity"));

        JPanel buttonPanel = new JPanel();
        buildCancelButton();
        cancelButton.setText("Apply");
        buttonPanel.add(cancelButton);

        JPanel mainPanel = new JPanel();
        mainPanel.add(sliderPanel);
        mainPanel.add(buttonPanel);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        return mainPanel;
    }

    /**
     * Accessor that returns the new opacity.
     *
     * @return  Opacity.
     */
    public float getOpacity() {
        return opacity;
    }

    /**
     * Sets values based on knob along slider.
     *
     * @param  e  Event that triggered this function
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        if (source == opacitySlider) {
            opacity = opacitySlider.getValue() / 100f;
            Preferences.setProperty(Preferences.PREF_PAINT_OPACITY, String.valueOf(opacity));
            current.setText(String.valueOf(opacity));

            if (!opacitySlider.getValueIsAdjusting()) {
                if (controls != null) {
                    controls.getTools().setOpacity(opacity);
                }
                
                if (parentFrame instanceof ViewJFrameTriImage) {
                	for (int i = 0; i < ViewJFrameTriImage.MAX_TRI_IMAGES; i++) {

                		if (((ViewJFrameTriImage)parentFrame).getTriImage(i) != null) {
                			((ViewJFrameTriImage)parentFrame).getTriImage(i).getActiveImage().notifyImageDisplayListeners(null, true);
                		}
                	}
                } else if (parentFrame instanceof ViewJFrameImage) {
                	((ViewJFrameImage)parentFrame).getComponentImage().getActiveImage().notifyImageDisplayListeners(null, true);
                } else {
                	if ( parentFrame != null )
                	{                	                	
                		((ViewJFrameBase) parentFrame).updateImages(true);
                	}
                }
                if ( surfacePaint != null )
                {
                    surfacePaint.setOpacity( opacity );
                }
                if ( surfacePaint_WM != null )
                {
                    surfacePaint_WM.setOpacity( opacity );
                }
                if ( voiManager != null )
                {
                    voiManager.setOpacity( opacity );
                }
            }
        }
    }

    /**
     * Makes slider to set opacity of VOI. Opaque = 1, Transparency = 0.
     *
     * @param  initValue  Initial value of slider.
     */
    private void init(float initValue) {
        setTitle("Paint Opacity");
        opacitySlider = new JSlider(JSlider.HORIZONTAL, 0, 100, (int) (initValue * 100));

        opacitySlider.setMajorTickSpacing(20);
        opacitySlider.setPaintTicks(true);
        opacitySlider.setEnabled(true);
        opacitySlider.addChangeListener(this);

        JLabel maximum = new JLabel(String.valueOf(1));
        maximum.setForeground(Color.black);
        maximum.setFont(serif12);

        current = new JLabel(String.valueOf(opacitySlider.getValue() / 100.0f));
        current.setForeground(Color.black);
        current.setFont(serif12B);

        JLabel minimum = new JLabel(String.valueOf(0));
        minimum.setForeground(Color.black);
        minimum.setFont(serif12);

        JPanel sliderPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridwidth = 3;
        gbc.weightx = 1;
        gbc.gridheight = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;

        sliderPanel.add(opacitySlider, gbc);

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

        sliderPanel.add(current, gbc);

        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;

        sliderPanel.add(maximum, gbc);
        sliderPanel.setBorder(buildTitledBorder("Opacity"));

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
