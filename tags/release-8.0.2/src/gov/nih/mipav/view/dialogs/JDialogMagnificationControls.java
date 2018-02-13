package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.Preferences.InterpolateDisplay;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.event.*;


/**
 * Simple dialog to change Magnification Box Settings.
 *
 * @version  1.0 Sep 15, 2012
 * @author   Justin Senseney
 * @author   Harman Singh
 * 
 */

public class JDialogMagnificationControls extends JDialogZoom {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 2838490243455666340L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Width and height of zoom box */
    private int boxWidth, boxHeight;
    
    /** Buttons for width, height values */
    private JButton widthButton, heightButton;

    /** Text fields for zoom box */
    private JTextField widthText, heightText;

    /** Text fields for zoom circle. */
    private JTextField radiusText;

    /** Button for setting radius value. */
    private JButton radiusButton;

    /** Buttons for selecting magnifying shape. */
    private JRadioButton circleButton, squareButton;

    /** Radius of zoom circle */
    private int radius;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new magnification controls dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Image
     * @param  initZoom        Initial zoom
     * @param  title           Title
     */
    public JDialogMagnificationControls(Frame theParentFrame, ViewJComponentEditImage im, float initZoom, String title) {
        super(theParentFrame, im, initZoom);
        mode = ZoomMode.SQUARE;
        interpType = InterpolateDisplay.NEAREST;
        nearest.setSelected(true);  //default to nearest neighbor interpolation
        magSlider.setValue(400);
    }
    
    protected void init(float initZoom) {
        setTitle(((ViewJFrameBase) parentFrame).getTitle());
        
        JPanel buttonPanel = new JPanel();
        buildOKButton();
        OKButton.setText("Apply");
        buildCancelButton();
        cancelButton.setText("Close");
        //buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        JPanel outerPanel = new JPanel();
        BoxLayout y = new BoxLayout(outerPanel, BoxLayout.Y_AXIS);
        outerPanel.setLayout(y);
        
        outerPanel.add(buildMainPanel(initZoom), y);
        outerPanel.add(buildDimPanel(), y);
        
        getContentPane().add(outerPanel, BorderLayout.CENTER);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        pack();
        setDefaultCloseOperation(DISPOSE_ON_CLOSE);
    }
    
    private JPanel buildDimPanel() {
        GuiBuilder gui = new GuiBuilder(this);
        
        JPanel dimPanel = new JPanel();
        BoxLayout y = new BoxLayout(dimPanel, BoxLayout.Y_AXIS);
        dimPanel.setLayout(y);
        dimPanel.setBorder(buildTitledBorder("Size options"));

        JPanel shapePanel = new JPanel();
        BoxLayout x = new BoxLayout(shapePanel, BoxLayout.X_AXIS);
        shapePanel.setLayout(x);
        
        circleButton = gui.buildRadioButton("Circle", false);
        shapePanel.add(circleButton, x);
        circleButton.setEnabled(false); //TODO: implement circle shape for magnification
        
        squareButton = gui.buildRadioButton("Square", true);
        shapePanel.add(squareButton, x);
        
        dimPanel.add(shapePanel, y);
        
        ButtonGroup shape = new ButtonGroup();
        shape.add(circleButton);
        shape.add(squareButton);
        
        final JPanel circlePanel = buildCirclePanel();
        dimPanel.add(circlePanel, y);
        circlePanel.setVisible(false);
        
        final JPanel squarePanel = buildSquarePanel();
        dimPanel.add(squarePanel, y);
        
        circleButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if(circleButton.isSelected()) {
                    circlePanel.setVisible(true);
                    squarePanel.setVisible(false);
                    mode = ZoomMode.CIRCLE;
                }
            }
        });
        
        squareButton.addActionListener(new ActionListener() {
            public void actionPerformed(ActionEvent e) {
                if(squareButton.isSelected()) {
                    circlePanel.setVisible(false);
                    squarePanel.setVisible(true);
                    mode = ZoomMode.SQUARE;
                }
            }
        });
        
        return dimPanel;
    }
    
    private JPanel buildSquarePanel() {
        GuiBuilder gui = new GuiBuilder(this);
        
        JPanel dimPanel = new JPanel();
        dimPanel.setLayout(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.weightx = .9;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.NONE;

        heightText = gui.buildIntegerField("Height: ", componentImage.MAGR_HEIGHT);
        dimPanel.add(heightText.getParent(), gbc);
        
        heightButton = buildSetButton(gui, "Change zoom height");
        gbc.gridx++;
        gbc.weightx = .1;
        dimPanel.add(heightButton, gbc);
        gbc.gridy++;
        gbc.gridx = 0;
        
        widthText = gui.buildIntegerField("Width: ", componentImage.MAGR_WIDTH);
        dimPanel.add(widthText.getParent(), gbc);
        
        widthButton = buildSetButton(gui, "Change zoom width");
        gbc.gridx++;
        gbc.weightx = .1;
        dimPanel.add(widthButton, gbc);

        return dimPanel;
    }
    
    private JPanel buildCirclePanel() {
        GuiBuilder gui = new GuiBuilder(this);
        
        JPanel dimPanel = new JPanel();
        dimPanel.setLayout(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.weightx = .9;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.NONE;

        radiusText = gui.buildIntegerField("Radius: ", componentImage.MAGR_HEIGHT/2);
        dimPanel.add(radiusText.getParent(), gbc);
        
        radiusButton = buildSetButton(gui, "Change radius");
        gbc.gridx++;
        gbc.weightx = .1;
        dimPanel.add(radiusButton, gbc);

        return dimPanel;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // **************************** Action Events *****************************
    // ************************************************************************

    /**
     * Calls various methods depending on the action.
     *
     * @param  event  event that triggered function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource();

        if(source.equals(widthButton)) {
            try {
                boxWidth = Integer.valueOf(widthText.getText());
                if(boxWidth > (int)(componentImage.getActiveImage().getExtents()[0]*.98)) {
                    throw new NumberFormatException();
                }
                componentImage.MAGR_WIDTH = boxWidth;
            } catch(NumberFormatException nfe) {
                MipavUtil.displayError("Width is not a valid value.");
                return;
            }
        } else if(source.equals(heightButton)) {
            try {
                boxHeight = Integer.valueOf(heightText.getText());
                if(boxHeight > (int)(componentImage.getActiveImage().getExtents()[1]*.98)) {
                    throw new NumberFormatException();
                }
                componentImage.MAGR_HEIGHT = boxHeight;
            } catch(NumberFormatException nfe) {
                MipavUtil.displayError("Height is not a valid value.");
                return;
            }
        } else if(source.equals(radiusButton)) {
            try {
                radius = Integer.valueOf(radiusText.getText());
                if(radius > (int)componentImage.getActiveImage().getExtents()[0]*2) {
                    throw new NumberFormatException();
                }
                componentImage.MAGR_WIDTH = radius*2;
            } catch(NumberFormatException nfe) {
                MipavUtil.displayError("Radius is not a valid value.");
                return;
            }
        }
        
        if(source.equals(widthButton) || source.equals(heightButton)) {
            paint(getGraphics());
        } else {
            super.actionPerformed(event);
        }
    }

    public void setWidthText(int i) {
        widthText.setText(String.valueOf(i));
    }

    public void setHeightText(int i) {
        heightText.setText(String.valueOf(i));
    }
}
