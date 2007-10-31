package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.JDialogBase.OKAction;

import java.awt.*;
import java.awt.event.*;

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

    /** DOCUMENT ME! */
    private JRadioButton radioGradient;

    /** DOCUMENT ME! */
    private JRadioButton radioIntensity;

    /** DOCUMENT ME! */
    private JRadioButton radioMedial;

    /** DOCUMENT ME! */
    private int selection = 0;
    
    JPanel mainPanel;

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
        
        // bind ENTER to okay button, ESC to cancel button
        Action okAction = new OKAction();
        
        //Action helpAction = new HelpAction();
        mainPanel.getInputMap(JComponent.WHEN_ANCESTOR_OF_FOCUSED_COMPONENT).put(KeyStroke.getKeyStroke("ENTER"),"OK");
        
        mainPanel.getActionMap().put("OK", okAction);
        
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
            }

            dispose();
        } else if (command.equals("Cancel")) {
            cancelFlag = true;
            dispose();
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
        setTitle("Live wire cost function");

        mainPanel = new JPanel(new GridLayout(3, 1));

        mainPanel.setBorder(buildTitledBorder("Choose cost function for live wire"));

        ButtonGroup group = new ButtonGroup();

        radioGradient = new JRadioButton("Gradient magnitude and direction");
        radioGradient.setForeground(Color.black);
        radioGradient.setFont(serif12);
        radioGradient.setSelected(true);
        
        group.add(radioGradient);
        mainPanel.add(radioGradient);

        radioMedial = new JRadioButton("Laplacian medialness");
        radioMedial.setForeground(Color.black);
        radioMedial.setFont(serif12);
        group.add(radioMedial);
        mainPanel.add(radioMedial);

        radioIntensity = new JRadioButton("Intensity");
        radioIntensity.setForeground(Color.black);
        radioIntensity.setFont(serif12);
        group.add(radioIntensity);
        mainPanel.add(radioIntensity);
        
        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);

        buildCancelButton();
        buttonPanel.add(cancelButton);
        
        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        
        pack();
        

    }



	
	
	
	/**
     * Handler for keys which should invoke the OK button (such as ENTER). Should be registered by inheriting classes on
     * their main JPanel using getInputMap().put() and getActionMap().put().
     */
    protected class OKAction extends AbstractAction {

        /** Use serialVersionUID for interoperability. */
        private static final long serialVersionUID = -7409235372458546982L;

        /**
         * Key action event handler.
         *
         * @param  event  key action event
         */
        public void actionPerformed(ActionEvent event) {

            if (OKButton != null) {
                ((JDialogBase) OKButton.getTopLevelAncestor()).actionPerformed(new ActionEvent(OKButton, 1, "OK"));
            }
        }
    }
    
    

}
