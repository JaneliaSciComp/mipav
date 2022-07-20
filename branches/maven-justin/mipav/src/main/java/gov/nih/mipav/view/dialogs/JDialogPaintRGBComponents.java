package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Simple dialog to change Opacity Settings (1 = opaque, 0 = transparent).
 *
 * @version  1.0 Sept 17, 1999
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class JDialogPaintRGBComponents extends JDialogBase implements ItemListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 844490628214011726L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    private JCheckBox useRBox = null;
    
    private JCheckBox useGBox = null;
    
    private JCheckBox useBBox = null;
    
    //~ Constructors ---------------------------------------------------------------------------------------------------



    /**
     * Creates new dialog with a slider and close button.
     *
     * @param  theParentFrame  The parent frame
     * @param  cntrls          The controls that the opacity will apply to.
     * @param  isVisible       Dialog visible or not
     */
    public JDialogPaintRGBComponents(Frame theParentFrame, String rgbStr) {
        super(theParentFrame, false);
        init(rgbStr);
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
            
        	boolean useRed = useRBox.isSelected();
        	boolean useGreen = useGBox.isSelected();
        	boolean useBlue = useBBox.isSelected();
        	
        	if (!useRed && !useGreen && !useBlue) {
        		MipavUtil.displayError("Must select at least one of R, G, or B");
        		return;
        	} else {
        		String rgbStr = "";
        		
        		if (useRed) {
        			rgbStr += "R";
        		}
        		if (useGreen) {
        			rgbStr += "G";
        		}
        		if (useBlue) {
        			rgbStr += "B";
        		}
        		Preferences.setProperty(Preferences.PREF_RGB_PAINT_COMPONENTS, rgbStr);
        		((ViewJFrameImage)parentFrame).getComponentImage().setRGBPaintComponents(rgbStr);
        		
        		
        	}
        	
            dispose();
        } else {
            super.actionPerformed(event);
        }
    }

    /**
     * Makes slider to set opacity of VOI. Opaque = 1, Transparency = 0.
     *
     * @param  initValue  Initial value of slider.
     */
    private void init(String rgbStr) {
        setTitle("Paint to Mask RGB");
        
        JPanel compPanel = new JPanel();
        compPanel.setLayout(new BoxLayout(compPanel, BoxLayout.Y_AXIS));
        
        
        useRBox = new JCheckBox("Use red component of paint", rgbStr.indexOf("R") != -1);
        useRBox.setFont(serif12);
        useRBox.addItemListener(this);
        
        useGBox = new JCheckBox("Use green component of paint", rgbStr.indexOf("G") != -1);
        useGBox.setFont(serif12);
        useGBox.addItemListener(this);
        
        useBBox = new JCheckBox("Use blue component of paint", rgbStr.indexOf("B") != -1);
        useBBox.setFont(serif12);
        useBBox.addItemListener(this);
        
        compPanel.add(useRBox);
        compPanel.add(useGBox);
        compPanel.add(useBBox);
        
        JPanel buttonPanel = new JPanel();
        buildCancelButton();
        cancelButton.setText("Apply");
        buttonPanel.add(cancelButton);
        
        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(compPanel);
        mainPanel.add(buttonPanel, BorderLayout.SOUTH);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        getContentPane().add(mainPanel);

        pack();
        setVisible(true);
    }

}
