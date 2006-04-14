package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;
import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;

/**
*   Simple dialog to choose which cost function to use
*   for the live wire.
*   @author     Neva Cherniavsky
*   @version    1.0
*   @see        RubberbandLivewire
*/
public class JDialogLivewire extends JDialogBase {

    private JRadioButton radioGradient;
    private JRadioButton radioMedial;
    private JRadioButton radioIntensity;

    private int          selection = 0;

    /**
    *   Creates dialog for choosing cost function
    *   for live wire.
    */
    public JDialogLivewire(Frame parent) {
        super(parent, true);
        init();
    }

    /**
    *   Initialized GUI components and displays dialog.
    */
    private void init() {
        setTitle("Live wire cost function");
        JPanel mainPanel = new JPanel(new GridLayout(3,1));
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
        setVisible(true);
    }

    /**
    *   Sets the selection based on which radio
    *   button is selected.  If dialog is cancelled,
    *   sets the cancelFlag to <code>true</code>.
    *   @param e    Event that triggered function.
    */
    public void actionPerformed(ActionEvent e) {
        String command = e.getActionCommand();

        if (command.equals("OK")) {
            if (radioGradient.isSelected()) {
                selection = RubberbandLivewire.GRADIENT_MAG;
            }
            else if (radioMedial.isSelected()) {
                selection = RubberbandLivewire.MEDIALNESS;
            }
            else if (radioIntensity.isSelected()) {
                selection = RubberbandLivewire.INTENSITY;
            }
            dispose();
        }
        else if (command.equals("Cancel")) {
            cancelFlag = true;
            dispose();
        }

    }

    /**
    *   Accessor that returns the selected cost function.
    *   @return Cost function selection.
    */
    public int getSelection() {
        return selection;
    }

}
