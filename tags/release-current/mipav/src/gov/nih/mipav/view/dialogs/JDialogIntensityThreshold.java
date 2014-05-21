package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * <p>Title:</p>
 *
 * <p>Description:</p>
 *
 * <p>Copyright: Copyright (c) 2003</p>
 *
 * <p>Company:</p>
 *
 * @author   not attributable
 * @version  1.0
 */

public class JDialogIntensityThreshold extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 988706874933899996L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private ViewJComponentEditImage component;
    private VOIHandlerInterface voiHandler;

    /** DOCUMENT ME! */
    private boolean doAverage;

    /** DOCUMENT ME! */
    private JTextField threshField = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogIntensityThreshold object.
     *
     * @param  theParentFrame  DOCUMENT ME!
     * @param  compImage       DOCUMENT ME!
     * @param  average         DOCUMENT ME!
     */
    public JDialogIntensityThreshold(JFrame theParentFrame, ViewJComponentEditImage compImage, boolean average) {
        super(theParentFrame, false);
        this.component = compImage;
        doAverage = average;
        init();
        setVisible(true);
    }
    /**
     * Creates a new JDialogIntensityThreshold object.
     *
     * @param  theParentFrame  DOCUMENT ME!
     * @param  compImage       DOCUMENT ME!
     * @param  average         DOCUMENT ME!
     */
    public JDialogIntensityThreshold(JFrame theParentFrame, VOIHandlerInterface voiHandler, boolean average) {
        super(theParentFrame, false);
        this.voiHandler = voiHandler;
        doAverage = average;
        init();
        setVisible(true);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * DOCUMENT ME!
     *
     * @param  e  DOCUMENT ME!
     */
    public void actionPerformed(ActionEvent e) {

        /**
         * @todo  Implement this java.awt.event.ActionListener abstract method
         */
        Object source = e.getSource();

        if (source == OKButton) {
            float threshold = 0.0f;

            try {
                threshold = new Float(threshField.getText()).floatValue();
            } catch (Exception ex) {
                MipavUtil.displayError("Threshold must be a float value");

                return;
            }

            setVisible(false);
            if ( voiHandler != null )
            {
                voiHandler.graph25VOI_CalcInten(!doAverage, true, threshold);
            }
            else if ( component != null )
            {
                component.getVOIHandler().graph25VOI_CalcInten(!doAverage, true, threshold);
            }
            this.dispose();
        } else if (source == cancelButton) {
            setVisible(false);
            this.dispose();
        } else {
            super.actionPerformed(e);
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void init() {
        setForeground(Color.black);

        if (doAverage) {
            setTitle("2.5D Average Intensity");
        } else {
            setTitle("2.5D Total Intensity");
        }

        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.X_AXIS));
        mainPanel.setForeground(Color.black);

        mainPanel.setBorder(buildTitledBorder(""));

        JLabel threshLabel = new JLabel("Intensity threshold   ");
        threshLabel.setForeground(Color.black);
        threshLabel.setFont(serif12);

        threshField = new JTextField(4);

        mainPanel.add(threshLabel);
        mainPanel.add(threshField);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        getContentPane().add(mainPanel, BorderLayout.NORTH);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
    }

}
