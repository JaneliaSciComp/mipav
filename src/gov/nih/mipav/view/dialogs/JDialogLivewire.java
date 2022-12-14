package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.FileNotFoundException;

import javax.swing.*;


/**
 * Simple dialog to choose which cost function to use for the live wire.
 *
 * @author   Neva Cherniavsky
 * @version  1.0
 * @see      RubberbandLivewire
 */
public class JDialogLivewire extends JDialogBase{

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -5986380794955471822L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Radio button for Gradient magnitude and direction */
    private JRadioButton radioGradient;

    /** Radio button for Intensity */
    private JRadioButton radioIntensity;

    /** Radio button for Laplacian medialness */
    private JRadioButton radioMedial;

    /** Radio button for Gradient and Medialness */
    private JRadioButton radioGradientMed;

    /** Radio button for Gradient and Intensity */
    private JRadioButton radioGradientInt;

    /** Radio button for Gradient, Medialness and Intensity */
    private JRadioButton radioGradientAll;

    /** DOCUMENT ME! */
    private int selection = 0;
    

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates dialog for choosing cost function for live wire.
     * 
     * Note:  When user hits "Enter" keystroke, it is same as if user clicked "OK"
     *        This code was already in the super constructor, but was not working
     *        I brought it over to this constructor...but what made it work was by moving the
     *        setVisible(true) from the end of the init() to the end of the constructor...weird.
     *
     * @param  parent  DOCUMENT ME!
     */
    public JDialogLivewire(Frame parent) {
        super(parent, true);
        init();     
        setVisible(true);
        
        
        
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Sets the selection based on which radio button is selected. If dialog is cancelled, sets the cancelFlag to <code>
     * true</code>.
     *
     * @param  e  Event that triggered function.
     */
    public void actionPerformed(ActionEvent e) {
        String command = e.getActionCommand();

        if (command.equals("OK")) {

            if (radioGradient.isSelected()) {
                selection = RubberbandLivewire.GRADIENT_MAG;
            } else if (radioMedial.isSelected()) {
                selection = RubberbandLivewire.MEDIALNESS;
            } else if (radioIntensity.isSelected()) {
                selection = RubberbandLivewire.INTENSITY;
            } else if (radioGradientMed.isSelected()) {
            	selection = RubberbandLivewire.GRADIENT_MAG_MED;
            } else if (radioGradientInt.isSelected()) {
            	selection = RubberbandLivewire.GRADIENT_MAG_INT;
            } else if (radioGradientAll.isSelected()) {
            	selection = RubberbandLivewire.GRADIENT_ALL;
            }

            dispose();
        } else if (command.equals("Cancel")) {
            cancelFlag = true;
            dispose();
        } else {
            super.actionPerformed(e);
        }

    }

    /**
     * Accessor that returns the selected cost function.
     *
     * @return  Cost function selection.
     */
    public int getSelection() {
        return selection;
    }

    /**
     * Initialized GUI components and displays dialog.
     */
    private void init() {
        try {
            setIconImage(MipavUtil.getIconImage("davinci_32x32.gif"));
        } catch (final FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage()
                    + ">.  Check that this file is available.\n");
        }
        
        setTitle("Live wire cost function");

        mainDialogPanel.setLayout(new GridLayout(3, 2));

        mainDialogPanel.setBorder(buildTitledBorder("Choose cost function for live wire"));

        ButtonGroup group = new ButtonGroup();

        radioGradient = new JRadioButton("Gradient magnitude and direction");
        radioGradient.setForeground(Color.black);
        radioGradient.setFont(serif12);
        radioGradient.setSelected(true);
        
        group.add(radioGradient);
        mainDialogPanel.add(radioGradient);

        radioMedial = new JRadioButton("Laplacian medialness");
        radioMedial.setForeground(Color.black);
        radioMedial.setFont(serif12);
        group.add(radioMedial);
        mainDialogPanel.add(radioMedial);

        radioIntensity = new JRadioButton("Intensity");
        radioIntensity.setForeground(Color.black);
        radioIntensity.setFont(serif12);
        group.add(radioIntensity);
        mainDialogPanel.add(radioIntensity);
        
        radioGradientMed = new JRadioButton("Gradient and Medialness");
        radioGradientMed.setForeground(Color.black);
        radioGradientMed.setFont(serif12);
        group.add(radioGradientMed);
        mainDialogPanel.add(radioGradientMed);
        
        radioGradientInt = new JRadioButton("Gradient and Intensity");
        radioGradientInt.setForeground(Color.black);
        radioGradientInt.setFont(serif12);
        group.add(radioGradientInt);
        mainDialogPanel.add(radioGradientInt);
        
        radioGradientAll = new JRadioButton("Gradient, Medialness and Intensity");
        radioGradientAll.setForeground(Color.black);
        radioGradientAll.setFont(serif12);
        group.add(radioGradientAll);
        mainDialogPanel.add(radioGradientAll);
        
        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);

        buildCancelButton();
        buttonPanel.add(cancelButton);
        
        getContentPane().add(mainDialogPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        
        pack();

    }



}
