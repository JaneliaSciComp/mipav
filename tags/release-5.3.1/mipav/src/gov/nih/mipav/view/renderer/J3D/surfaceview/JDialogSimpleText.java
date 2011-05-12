package gov.nih.mipav.view.renderer.J3D.surfaceview;


import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.renderer.J3D.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Simple dialog that requests a name for the view.
 *
 * @author  Ruida Cheng
 * @see     JDialogView
 */
public class JDialogSimpleText extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 1577016550517413391L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Text field to enter name in. */
    JTextField field;

    /** Name for the view. */
    String name = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates the dialog, using the input parameters to place it on the screen.
     *
     * @param  dialog  Parent dialog that created this.
     * @param  parent  Parent frame.
     */
    public JDialogSimpleText(JPanelRendererJ3D dialog, RenderViewBase parent) {

        // super( parent, true );
        setTitle("Name the current view");

        JLabel label = new JLabel("Enter a name for this view: ");

        label.setFont(serif12);
        label.setForeground(Color.black);
        field = new JTextField(10);
        OKButton = buildOKButton();
        cancelButton = buildCancelButton();
        setLocation(dialog.getLocation());

        JPanel textPanel = new JPanel();

        textPanel.add(label);
        textPanel.add(field);

        JPanel buttonPanel = new JPanel();

        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);
        getContentPane().add(textPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

    /**
     * Creates the dialog, using the input parameters to place it on the screen.
     *
     * @param  dialog  Parent dialog that created this.
     * @param  parent  Parent frame.
     */
    public JDialogSimpleText(JPanelMouse dialog, RenderViewBase parent) {

        // super( parent, true );
        setTitle("Name the current view");

        JLabel label = new JLabel("Enter a name for this view: ");

        label.setFont(serif12);
        label.setForeground(Color.black);
        field = new JTextField(10);
        OKButton = buildOKButton();
        cancelButton = buildCancelButton();
        setLocation(dialog.getLocation());

        JPanel textPanel = new JPanel();

        textPanel.add(label);
        textPanel.add(field);

        JPanel buttonPanel = new JPanel();

        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);
        getContentPane().add(textPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * On "OK", sets the name variable to the text entered. On "Cancel" disposes of this dialog and sets cancel flag.
     *
     * @param  event  Event that triggered this method.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {
            name = field.getText();
            dispose();
        } else if (command.equals("Cancel")) {
            cancelFlag = true;
            dispose();
        }
    }

    /**
     * Accessor that returns the name entered in this dialog.
     *
     * @return  The name the user entered.
     */
    public String getName() {
        return name;
    }
}
