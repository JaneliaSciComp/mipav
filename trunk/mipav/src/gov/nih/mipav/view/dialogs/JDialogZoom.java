package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * This is a custom swing dialog that sets variables for zooming in and out.
 *
 * @version  1.1 May 3, 1999
 * @author   Neva Cherniavsky
 * @author   Matthew McAuliffe, Ph.D.
 */

public class JDialogZoom extends JDialogBase implements ChangeListener, WindowListener, MouseListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5271517126304850595L;

    /** DOCUMENT ME! */
    private static final long MAX_MAGNIFICATION = 64000000;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JRadioButton bilinear;

    /** DOCUMENT ME! */
    private ViewJComponentEditImage componentImage;

    /** DOCUMENT ME! */
    private JRadioButton cubic;

    /** DOCUMENT ME! */
    private JLabel current;

    /** DOCUMENT ME! */
    private JSlider magSlider;

    /** DOCUMENT ME! */
    private int max;

    /** DOCUMENT ME! */
    private int min;

    /** DOCUMENT ME! */
    private JRadioButton nearest;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new zoom dialog.
     *
     * @param  parent    Parent frame
     * @param  im        Image component of the image model
     * @param  initZoom  Initial zoom
     */
    public JDialogZoom(Frame parent, ViewJComponentEditImage im, float initZoom) {
        super(parent, false);

        parentFrame = parent;
        componentImage = im;
        init(initZoom);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and sets zoom.
     *
     * @param  event  Event that triggers this function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();
        String command = event.getActionCommand();

        if (source == OKButton) {

            if (nearest.isSelected()) {
                componentImage.setInterpolationMode(ViewJComponentBase.NEAREST);
                
            } else if (bilinear.isSelected()) {
                componentImage.setInterpolationMode(ViewJComponentBase.BILINEAR);
                

            }

            int zoom = magSlider.getValue();
            ((ViewJFrameImage) parentFrame).updateFrame(zoom / 100.0f, zoom / 100.0f);
        } else if (source == cancelButton) {
            ((ViewJFrameImage) parentFrame).setZoomDialogNull();
            dispose();
        }
        
        if(command.equals("nearest")) {
        	componentImage.setInterpolationMode(ViewJComponentBase.NEAREST);
        	componentImage.getActiveImage().notifyImageDisplayListeners(componentImage.getLUTa(), true, -50, ViewJComponentBase.NEAREST);
        } else if(command.equals("bilinear")) {
        	componentImage.setInterpolationMode(ViewJComponentBase.BILINEAR);
        	componentImage.getActiveImage().notifyImageDisplayListeners(componentImage.getLUTa(), true, -50, ViewJComponentBase.BILINEAR);
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * Sets values based on knob along slider.
     *
     * @param  e  event that triggered this function
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        if (source == magSlider) {
            current.setText(String.valueOf(magSlider.getValue() / (float) 100));
            magSlider.setValue((int) ((magSlider.getValue() / (float) 100) + 0.5) * 100);
            
            if (nearest.isSelected()) {
                componentImage.setInterpolationMode(ViewJComponentBase.NEAREST);
                
            } else if (bilinear.isSelected()) {
                componentImage.setInterpolationMode(ViewJComponentBase.BILINEAR);
                

            }

            //int zoom = magSlider.getValue();
            //((ViewJFrameImage) parentFrame).updateFrame(zoom / 100.0f, zoom / 100.0f);
            
        }
    }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowActivated(WindowEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowClosed(WindowEvent event) { }

    /**
     * Calls dispose.
     *
     * @param  event  Event that triggered function
     */
    public void windowClosing(WindowEvent event) {
        ((ViewJFrameImage) parentFrame).setZoomDialogNull();
        dispose();
    }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowDeactivated(WindowEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowDeiconified(WindowEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowIconified(WindowEvent event) { }

    // ************************************************************************
    // **************************** Window Events *****************************
    // ************************************************************************

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowOpened(WindowEvent event) { }

    /**
     * Finds the maximum allowable magnification for the image.
     *
     * @return  maximum possible magnification without running out of memory
     */
    @SuppressWarnings("unused")
    private int findMax() {
        long freeMemory = MipavUtil.getFreeHeapMemory();
        int xDim = componentImage.getImageA().getExtents()[0];
        int yDim = componentImage.getImageA().getExtents()[1];
        float zoom = componentImage.getZoomX();

        long used = (long) (xDim * zoom * yDim * zoom * 8);
        freeMemory = freeMemory + used;

        if (freeMemory > MAX_MAGNIFICATION) {
            freeMemory = MAX_MAGNIFICATION;
        }

        double max = freeMemory / (xDim * yDim * 8);

        return (int) (Math.sqrt(max) * 100);
    }

    /**
     * DOCUMENT ME!
     *
     * @param  initZoom  DOCUMENT ME!
     */
    private void init(float initZoom) {
        setTitle(((ViewJFrameImage) parentFrame).getTitle());
        max = 3200;
        min = 25;

        if ((componentImage.getZoomX() * 100) > max) {
            max = (int) (componentImage.getZoomX() * 100);
        }

        magSlider = new JSlider(JSlider.HORIZONTAL, min, max, (int) (componentImage.getZoomX() * 100));

        magSlider.setMajorTickSpacing((max - min) / 5);
        magSlider.setPaintTicks(true);
        magSlider.setEnabled(true);
        magSlider.setValue(Math.round(100 * initZoom));
        magSlider.addChangeListener(this);
        magSlider.addMouseListener(this);

        JLabel maximum = new JLabel(String.valueOf(max / 100f));
        maximum.setForeground(Color.black);
        maximum.setFont(serif12);

        current = new JLabel(String.valueOf(magSlider.getValue() / 100.0f));
        current.setForeground(Color.black);
        current.setFont(serif12B);

        JLabel minimum = new JLabel(String.valueOf(min / 100f));
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

        sliderPanel.add(magSlider, gbc);

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
        sliderPanel.setBorder(buildTitledBorder("Magnification"));

        JPanel radioPanel = new JPanel(new GridBagLayout());
        radioPanel.setBorder(buildTitledBorder("Interpolation"));

        ButtonGroup radioGroup = new ButtonGroup();
        nearest = new JRadioButton("Nearest");
        nearest.setFont(serif12);
        nearest.addActionListener(this);
        nearest.setActionCommand("nearest");
        radioGroup.add(nearest);

        bilinear = new JRadioButton("BiLinear          ");
        bilinear.setFont(serif12);
        bilinear.addActionListener(this);
        bilinear.setActionCommand("bilinear");
        radioGroup.add(bilinear);

        cubic = new JRadioButton("Cubic");
        cubic.setFont(serif12);
        cubic.setEnabled(false);
        radioGroup.add(cubic);

        switch (componentImage.getInterpMode()) {
        case ViewJComponentBase.NEAREST_BOTH:
        case ViewJComponentBase.NEAREST:
            nearest.setSelected(true);
            break;

        case ViewJComponentBase.BILINEAR:
            bilinear.setSelected(true);
            break;
        }

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        radioPanel.add(nearest, gbc);
        gbc.gridy = 1;
        radioPanel.add(bilinear, gbc);
        gbc.gridy = 2;
        radioPanel.add(cubic, gbc);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        OKButton.setText("Apply");
        buildCancelButton();
        cancelButton.setText("Close");
        //buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(sliderPanel);
        mainPanel.add(radioPanel, BorderLayout.EAST);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setDefaultCloseOperation(DISPOSE_ON_CLOSE);
    }

	public void mouseClicked(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mouseEntered(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mouseExited(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mousePressed(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

	public void mouseReleased(MouseEvent e) {
		Object source = e.getSource();
		
		if (source == magSlider) {
            current.setText(String.valueOf(magSlider.getValue() / (float) 100));
            magSlider.setValue((int) ((magSlider.getValue() / (float) 100) + 0.5) * 100);
            
            if (nearest.isSelected()) {
                componentImage.setInterpolationMode(ViewJComponentBase.NEAREST);
                
            } else if (bilinear.isSelected()) {
                componentImage.setInterpolationMode(ViewJComponentBase.BILINEAR);
                

            }

            int zoom = magSlider.getValue();
            ((ViewJFrameImage) parentFrame).updateFrame(zoom / 100.0f, zoom / 100.0f);
            
        }
		
		
	}

}
