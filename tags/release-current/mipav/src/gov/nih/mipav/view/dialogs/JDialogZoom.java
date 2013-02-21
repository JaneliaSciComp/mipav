package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.Preferences.InterpolateDisplay;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * This is a custom swing dialog that sets variables for zooming in and out.
 *
 * @version  1.2 2012
 * @author   Justin Senseney
 * @author   Neva Cherniavsky
 * @author   Matthew McAuliffe, Ph.D.
 */

public class JDialogZoom extends JDialogBase implements ChangeListener, WindowListener, MouseListener, KeyListener {

    
    public enum ZoomMode {
        IMAGE,
        SQUARE,
        CIRCLE;
    }
    
    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5271517126304850595L;

    /** Maximum possible magnification. */
    private static final long MAX_MAGNIFICATION = 64000000;
    
    /** Action command for nearest neighbor interpolation. */
    public static final String NEAREST = "NEAREST";
    
    /** Action command for bilinear interpolation. */
    public static final String BILINEAR = "BILINEAR";
    
    /** Action command for cubic interpolation. */
    public static final String CUBIC = "CUBIC";

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Radio buttons for interpolation methods. */
    protected JRadioButton bilinear, cubic, nearest;

    /** Component image which this dialog belongs to. */
    protected ViewJComponentEditImage componentImage;

    /** Value of current slider number. */
    private JLabel currentLabel;

    /** Slider used for zooming. */
    protected JSlider magSlider;

    /** Text fields for user to enter max/min slider values. */
    private JTextField maximumValueField, minimumValueField;

    /** Buttons to set max/min slider values. */
    private JButton maximumValueButton, minimumValueButton;

    /** Labels to display max/min slider values. */
    private JLabel maximumLabel, minimumLabel;

    /** Mode of operation for the dialog */
    protected ZoomMode mode;
    
    /** Method of interpolation */
    protected InterpolateDisplay interpType;

    /** Checkbox for displaying intensity values. */
    private JCheckBox intensityCheckbox;
    
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
        mode = ZoomMode.IMAGE;
        interpType = Preferences.getInterpolateDisplay();
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

        if(source.equals(intensityCheckbox)) {
            componentImage.setShowMagIntensity(intensityCheckbox.isSelected());
        } else if (source.equals(cancelButton)) {
            ((ViewJFrameImage) parentFrame).setZoomDialogNull();
            dispose();
        } else if (source.equals(maximumValueButton)) {
            try {
                String str = currentLabel.getText();
                int i = (int) (Double.valueOf(maximumValueField.getText())*100.0);
                if(i < 0 || i < magSlider.getMinimum()) {
                    throw new NumberFormatException();
                }
               
                maximumLabel.setText(String.valueOf(i/100.0));
                if(i < magSlider.getValue()) {
                    magSlider.setValue(i);
                    OKButton.doClick();
                }
                magSlider.setMaximum(i);
                currentLabel.setText(str); //needed for switching levels of precision
                
            } catch(NumberFormatException nfe) {
                MipavUtil.displayError(maximumValueField.getText()+" is not a valid zoom value.");
            }
        } else if(source.equals(minimumValueButton)) {
            try {
                String str = currentLabel.getText();
                int i = (int) (Double.valueOf(minimumValueField.getText())*100.0);
                if(i < 0 || i > magSlider.getMaximum()) {
                    throw new NumberFormatException();
                }
               
                minimumLabel.setText(String.valueOf(i/100.0));
                if(i > magSlider.getValue()) {
                    magSlider.setValue(i);
                    OKButton.doClick();
                }
                magSlider.setMinimum(i);
                currentLabel.setText(str); //needed for switching levels of precision
                
                
            } catch(NumberFormatException nfe) {
                MipavUtil.displayError(minimumValueField.getText()+" is not a valid zoom value.");
            }
        } else if(command.equals(NEAREST)) {
            interpType = InterpolateDisplay.NEAREST;
        } else if(command.equals(BILINEAR)) {
            interpType = InterpolateDisplay.BILINEAR;
        } else if(command.equals(CUBIC)) {
            interpType = InterpolateDisplay.BICUBIC;
        } else {
            super.actionPerformed(event);
        }
        
        if(mode.equals(ZoomMode.IMAGE)) {
            Preferences.setInterpolationMode(interpType);
        }
        
        if(command.equals(NEAREST)) {
            componentImage.getActiveImage().notifyImageDisplayListeners(componentImage.getLUTa(), true, -50, ViewJComponentBase.NEAREST_BOTH);
        } else if(command.equals(BILINEAR)) {
            componentImage.getActiveImage().notifyImageDisplayListeners(componentImage.getLUTa(), true, -50, ViewJComponentBase.INTERPOLATE_BOTH);
        } else if(command.equals(CUBIC)) {
            componentImage.getActiveImage().notifyImageDisplayListeners(componentImage.getLUTa(), true, -50, ViewJComponentBase.INTERPOLATE_BOTH);
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
            double precision = 10;
            double offset = 10;
            
            if(magSlider.getMaximum() - magSlider.getMinimum() < 101) {
                precision = 1;
                offset = 100;
            } 
            
            double displayValue  = Math.round(magSlider.getValue()/precision)/offset;
            
            currentLabel.setText(String.valueOf(displayValue));
            
            int width = componentImage.MAGR_WIDTH;
            int height = componentImage.MAGR_HEIGHT;
            float mag = (float) displayValue;
            if(mode.equals(ZoomMode.IMAGE)) {
                width = componentImage.getParent().getWidth();
                height = componentImage.getParent().getHeight();
            }
            
            intensityCheckbox.setEnabled(componentImage.getShowMagIntensityEnabled(width, height, mag) && mode != ZoomMode.IMAGE);
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
    protected void init(float initZoom) {
        setTitle(((ViewJFrameBase) parentFrame).getTitle());
        
        JPanel buttonPanel = new JPanel();
        buildOKButton();
        OKButton.setText("Apply");
        buildCancelButton();
        cancelButton.setText("Close");
        //buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        getContentPane().add(buildMainPanel(initZoom), BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setDefaultCloseOperation(DISPOSE_ON_CLOSE);
    }
    
    protected JPanel buildMainPanel(float initZoom) {
        int max = 3200;
        int min = 25;

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
        magSlider.addKeyListener(this);

        maximumLabel = new JLabel(String.valueOf(max / 100f));
        maximumLabel.setForeground(Color.black);
        maximumLabel.setFont(serif12);

        currentLabel = new JLabel(String.valueOf(magSlider.getValue() / 100.0f));
        currentLabel.setForeground(Color.black);
        currentLabel.setFont(serif12B);

        minimumLabel = new JLabel(String.valueOf(min / 100f));
        minimumLabel.setForeground(Color.black);
        minimumLabel.setFont(serif12);

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

        sliderPanel.add(minimumLabel, gbc);

        gbc.gridx = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.weightx = .5;

        sliderPanel.add(currentLabel, gbc);

        gbc.gridx = 2;
        gbc.anchor = GridBagConstraints.EAST;
        gbc.weightx = 0;

        sliderPanel.add(maximumLabel, gbc);
        
        gbc.gridy++;
        gbc.gridx = 0;
        gbc.gridwidth = 3;
        gbc.insets = new Insets(10, 0, 0, 0);
        
        JPanel setSliderPanel = buildSettingsPanel(min / 100f, max / 100f);
        sliderPanel.add(setSliderPanel, gbc);
        
        sliderPanel.setBorder(buildTitledBorder("Magnification"));

        JPanel radioPanel = new JPanel(new GridBagLayout());
        radioPanel.setBorder(buildTitledBorder("Interpolation"));

        ButtonGroup radioGroup = new ButtonGroup();
        nearest = new JRadioButton("Nearest");
        nearest.setFont(serif12);
        nearest.addActionListener(this);
        nearest.setActionCommand(NEAREST);
        radioGroup.add(nearest);

        bilinear = new JRadioButton("Bilinear          ");
        bilinear.setFont(serif12);
        bilinear.addActionListener(this);
        bilinear.setActionCommand(BILINEAR);
        radioGroup.add(bilinear);

        cubic = new JRadioButton("Cubic");
        cubic.setFont(serif12);
        cubic.setEnabled(true);
        cubic.setActionCommand(CUBIC);
        cubic.addActionListener(this);
        radioGroup.add(cubic);

        switch (Preferences.getInterpolateDisplay()) {
        case NEAREST:
            nearest.setSelected(true);
            break;

        case BILINEAR:
            bilinear.setSelected(true);
            break;
            
        case BICUBIC:
            cubic.setSelected(true);
            break;
        }

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 0, 0, 0);
        gbc.weightx = 1;
        radioPanel.add(nearest, gbc);
        gbc.gridy = 1;
        radioPanel.add(bilinear, gbc);
        gbc.gridy = 2;
        radioPanel.add(cubic, gbc);

        

        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(sliderPanel);
        mainPanel.add(radioPanel, BorderLayout.EAST);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
        
        return mainPanel;
    }

	private JPanel buildSettingsPanel(float min, float max) {
	    GuiBuilder gui = new GuiBuilder(this);
	    
	    JPanel maxMinPanel = new JPanel();
	    maxMinPanel.setLayout(new GridBagLayout());
	    GridBagConstraints gbc = new GridBagConstraints();
	    
	    gbc.gridx = 0;
	    gbc.gridy = 0;
	    gbc.fill = GridBagConstraints.BOTH;
	    
	    intensityCheckbox = gui.buildCheckBox("Display intensity values", false);
	    intensityCheckbox.setEnabled(false);
	    intensityCheckbox.addActionListener(this);
	    maxMinPanel.add(intensityCheckbox, gbc);
	    
	    gbc.gridx = 0;
	    gbc.weightx = .9;
	    gbc.gridy++;
	    gbc.fill = GridBagConstraints.NONE;
	    
	    minimumValueField = gui.buildDecimalField("Slider minimum value: ", min);
        maxMinPanel.add(minimumValueField.getParent(), gbc);
       
        minimumValueButton = buildSetButton(gui, "Change slider minimum value.");
        gbc.gridx++;
        gbc.weightx = .1;
        maxMinPanel.add(minimumValueButton, gbc);
        gbc.gridy++;
        gbc.gridx = 0;
	    
	    maximumValueField = gui.buildDecimalField("Slider maximum value:", max);
	    maxMinPanel.add(maximumValueField.getParent(), gbc);
	   
	    maximumValueButton = buildSetButton(gui, "Change slider maximum value.");
	    gbc.gridx++;
	    gbc.weightx = .2;
	    maxMinPanel.add(maximumValueButton, gbc);
        
	    return maxMinPanel;
    }
	
	protected JButton buildSetButton(GuiBuilder gui, String toolTip) {
	    JButton button = gui.buildButton("Set");
	    button.addActionListener(this);
	    button.setMinimumSize(new Dimension(40, 20));
	    button.setPreferredSize(new Dimension(40, 20));
	    button.setMaximumSize(new Dimension(40, 20));
	    button.setMargin(new Insets(2, 7, 2, 7));
	    button.setToolTipText(toolTip);
	    
	    return button;
	}
	
	/**
     * @return the interpType
     */
    public InterpolateDisplay getInterpType() {
        return interpType;
    }

    /**
     * @return the mode
     */
    public ZoomMode getMode() {
        return mode;
    }

    private void inputEvent(InputEvent e) {
	    Object source = e.getSource();
        
        if (source == magSlider) {
            float zoom = Float.valueOf(currentLabel.getText());
            switch(mode) {
            
            case IMAGE:
                ((ViewJFrameImage) parentFrame).updateFrame(zoom, zoom); 
                break;
            case SQUARE:
                componentImage.MAGR_MAG = zoom*componentImage.getZoomX();
                break;
            }
        }
	}

    public void mouseClicked(MouseEvent e) {}

	public void mouseEntered(MouseEvent e) {}

	public void mouseExited(MouseEvent e) {}

	public void mousePressed(MouseEvent e) {}

	@Override
	public void mouseReleased(MouseEvent e) {
	    inputEvent(e);
	}

    public void keyPressed(KeyEvent e) {}

    @Override
    public void keyReleased(KeyEvent e) {
        inputEvent(e);
    }

    public void keyTyped(KeyEvent e) {}
}
