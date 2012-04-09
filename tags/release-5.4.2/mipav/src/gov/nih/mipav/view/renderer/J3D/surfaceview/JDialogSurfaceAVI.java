package gov.nih.mipav.view.renderer.J3D.surfaceview;


import gov.nih.mipav.view.*;
import gov.nih.mipav.view.dialogs.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog that gets input needed for saving the 3D rendering motions to an AVI file. User can enter subsample and frame
 * rate.
 *
 * @version  0.1 Oct 2001
 * @author   Neva Cherniavsky
 * @see      JDialogMouseRecorder
 */
public class JDialogSurfaceAVI extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -475364340269669481L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Frame rate per second. */
    private double rate;

    /** Text field for the frame rate per second. */
    private JTextField rateBox;

    /** Subsample flag. */
    private int subSample;

    /** Sub sampel combo box. */
    private JComboBox subSampleCBox;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor to create surface AVI dialog.
     *
     * @param  theParentFrame  parent frame reference
     */
    public JDialogSurfaceAVI(Frame theParentFrame) {
        super(theParentFrame, true);
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Takes the following actions:
     *
     * <ul>
     *   <li>OK Button - gets the frame rate from the text box and the subsample parameter from the combo boxes</li>
     *   <li>Cancel Button - closes the dialog without doing anything</li>
     * </ul>
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (testParameter(rateBox.getText(), 0, 100)) {
                rate = Double.valueOf(rateBox.getText()).doubleValue();
            }

            subSample = Integer.valueOf((String) subSampleCBox.getSelectedItem()).intValue();
            dispose();
        } else if (command.equals("Cancel")) {
            cancelFlag = true;
            dispose();
        }
    }

    /**
     * Accessor that gets the frame rate.
     *
     * @return  The frame rate.
     */
    public double getFrameRate() {
        return rate;
    }

    /**
     * Accessor that gets the subsample size.
     *
     * @return  The subsample size.
     */
    public int getSubSample() {
        return subSample;
    }

    /**
     * Accessor that sets the frame rate.
     *
     * @param  rate  Frame rate to set.
     */
    public void setFrameRate(double rate) {
        this.rate = rate;
    }

    /**
     * Accessor that sets the subsample size.
     *
     * @param  type  The subsample size.
     */
    public void setSubSample(int type) {
        subSample = type;
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Set AVI options");

        JLabel subSampleLabel = new JLabel("Subsample image");
        subSampleLabel.setFont(MipavUtil.font12);
        subSampleLabel.setForeground(Color.black);

        subSampleCBox = new JComboBox();
        subSampleCBox.setFont(MipavUtil.font12);
        subSampleCBox.setToolTipText("Subsample by");
        subSampleCBox.addItem("1");
        subSampleCBox.addItem("2");
        subSampleCBox.addItem("3");
        subSampleCBox.addItem("4");

        JPanel subSamplePanel = new JPanel();
        subSamplePanel.add(subSampleLabel);
        subSamplePanel.add(subSampleCBox);

        JLabel rateLabel = new JLabel("Frame rate");
        rateLabel.setFont(MipavUtil.font12);
        rateLabel.setForeground(Color.black);

        rateBox = new JTextField("21", 3);

        JLabel rate2Label = new JLabel("frames per second");
        rate2Label.setFont(MipavUtil.font12);
        rate2Label.setForeground(Color.black);

        JPanel ratePanel = new JPanel();
        ratePanel.add(rateLabel);
        ratePanel.add(rateBox);
        ratePanel.add(rate2Label);

        JPanel topPanel = new JPanel();
        topPanel.setLayout(new BoxLayout(topPanel, BoxLayout.Y_AXIS));
        topPanel.add(subSamplePanel);
        topPanel.add(ratePanel);
        topPanel.setBorder(buildTitledBorder("AVI options"));

        JPanel buttonPanel = new JPanel();

        OKButton = buildOKButton();
        cancelButton = buildCancelButton();
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BorderLayout());
        mainPanel.add(topPanel, BorderLayout.CENTER);
        mainPanel.add(buttonPanel, BorderLayout.SOUTH);

        getContentPane().add(mainPanel);
        pack();
        setVisible(true);
    }

}
