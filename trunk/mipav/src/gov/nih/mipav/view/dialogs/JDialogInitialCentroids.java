package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Creates the dialog to input inital centroid values for each class for AlgorithmFuzzyCMeans.
 */
public class JDialogInitialCentroids extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -7070007899490674843L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private float[] centroids;

    /** DOCUMENT ME! */
    private JTextField[] defaultValueInput;

    /** DOCUMENT ME! */
    private GridBagConstraints gbc;

    /** DOCUMENT ME! */
    private GridBagLayout gbl;

    /** DOCUMENT ME! */
    private int i;

    /** DOCUMENT ME! */
    private float minimum, maximum;

    /** DOCUMENT ME! */
    private int nClasses;

    /** DOCUMENT ME! */
    private String tmpStr;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates the dialog.
     *
     * @param  theParentFrame  parent frame
     * @param  _nClasses       number of segmentation classes
     * @param  _min            image minimum
     * @param  _max            image maximum
     */
    public JDialogInitialCentroids(Frame theParentFrame, int _nClasses, float _min, float _max) {
        super(theParentFrame, true);


        getContentPane().setLayout(new BorderLayout());
        setTitle("Initial Centroids");
        setSize(350, 230);
        setForeground(Color.black);
        cancelFlag = false;

        nClasses = _nClasses;
        minimum = _min;
        maximum = _max;
        centroids = new float[nClasses];


        Box contentBox = new Box(BoxLayout.Y_AXIS);

        // default margin value
        JPanel defaultValuePanel = new JPanel();
        defaultValuePanel.setBorder(buildTitledBorder("Enter Inital Centroids"));


        // set layout
        gbl = new GridBagLayout();
        gbc = new GridBagConstraints();
        defaultValuePanel.setLayout(gbl);
        gbc.anchor = GridBagConstraints.NORTHWEST;
        defaultValueInput = new JTextField[nClasses];

        for (i = 0; i < nClasses; i++) {

            // make content, place into layout
            JLabel defaultLabel = new JLabel("Centroid[" + String.valueOf(i + 1) + "] (" + String.valueOf(minimum) +
                                             " to " + String.valueOf(maximum) + ")");
            defaultLabel.setFont(serif12);
            defaultLabel.setForeground(Color.black);
            defaultLabel.setRequestFocusEnabled(false);
            gbc.gridwidth = 2;
            gbl.setConstraints(defaultLabel, gbc);
            defaultValuePanel.add(defaultLabel);
            defaultValuePanel.add(Box.createHorizontalStrut(10));
            defaultValueInput[i] = new JTextField(Float.toString(minimum +
                                                                 ((maximum - minimum) * (i + 1) / (nClasses + 1))), 8);
            defaultValueInput[i].addActionListener(this);
            MipavUtil.makeNumericsOnly(defaultValueInput[i], true);
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(defaultValueInput[i], gbc);
            defaultValuePanel.add(defaultValueInput[i]);
        } // for (i = 0; i < nClasses; i++)

        contentBox.add(defaultValuePanel);

        JPanel OKCancelPanel = new JPanel(new FlowLayout());
        buildOKButton();
        buildCancelButton();
        OKCancelPanel.add(OKButton);
        OKCancelPanel.add(cancelButton);
        contentBox.add(OKCancelPanel);

        getContentPane().add(contentBox);
        pack();
        setVisible(true);
    }
    
    public JDialogInitialCentroids(Frame theParentFrame, int _nClasses, float _min, float _max, boolean flag) {
        super(theParentFrame, true);


        getContentPane().setLayout(new BorderLayout());
        setTitle("Initial Centroids");
        setSize(350, 230);
        setForeground(Color.black);
        cancelFlag = false;

        nClasses = _nClasses;
        minimum = _min;
        maximum = _max;
        centroids = new float[nClasses];


        Box contentBox = new Box(BoxLayout.Y_AXIS);

        // default margin value
        JPanel defaultValuePanel = new JPanel();
        defaultValuePanel.setBorder(buildTitledBorder("Enter Inital Centroids"));


        // set layout
        gbl = new GridBagLayout();
        gbc = new GridBagConstraints();
        defaultValuePanel.setLayout(gbl);
        gbc.anchor = GridBagConstraints.NORTHWEST;
        defaultValueInput = new JTextField[nClasses];

        for (i = 0; i < nClasses; i++) {

            // make content, place into layout
            JLabel defaultLabel = new JLabel("Centroid[" + String.valueOf(i + 1) + "] (" + String.valueOf(minimum) +
                                             " to " + String.valueOf(maximum) + ")");
            defaultLabel.setFont(serif12);
            defaultLabel.setForeground(Color.black);
            defaultLabel.setRequestFocusEnabled(false);
            gbc.gridwidth = 2;
            gbl.setConstraints(defaultLabel, gbc);
            defaultValuePanel.add(defaultLabel);
            defaultValuePanel.add(Box.createHorizontalStrut(10));
            defaultValueInput[i] = new JTextField(Float.toString(minimum +
                                                                 ((maximum - minimum) * (i + 1) / (nClasses + 1))), 8);
            defaultValueInput[i].addActionListener(this);
            MipavUtil.makeNumericsOnly(defaultValueInput[i], true);
            gbc.gridwidth = GridBagConstraints.REMAINDER;
            gbl.setConstraints(defaultValueInput[i], gbc);
            defaultValuePanel.add(defaultValueInput[i]);
        } // for (i = 0; i < nClasses; i++)

        contentBox.add(defaultValuePanel);

        JPanel OKCancelPanel = new JPanel(new FlowLayout());
        buildOKButton();
        buildCancelButton();
        OKCancelPanel.add(OKButton);
        OKCancelPanel.add(cancelButton);
        contentBox.add(OKCancelPanel);

        getContentPane().add(contentBox);
        pack();
    }


    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        Object source = event.getSource(); // whatever the user clicked on

        if (source == OKButton) {

            for (i = 0; i < nClasses; i++) {
                tmpStr = defaultValueInput[i].getText();

                if (testParameter(tmpStr, minimum, maximum)) {
                    centroids[i] = Float.parseFloat(tmpStr);
                } else {
                    defaultValueInput[i].requestFocus();
                    defaultValueInput[i].selectAll();

                    return;
                }
            }

            dispose();
        } else if (source == cancelButton) {
            cancelFlag = true;
            dispose();
        }
    }

    public void run() {
    	   for (i = 0; i < nClasses; i++) {
               tmpStr = defaultValueInput[i].getText();

               if (testParameter(tmpStr, minimum, maximum)) {
                   centroids[i] = Float.parseFloat(tmpStr);
               } else {
                   defaultValueInput[i].requestFocus();
                   defaultValueInput[i].selectAll();

                   return;
               }
           }
    }
    
    /**
     * Accessor that returns the array of initial centroid values.
     *
     * @return  array of initial centroid values
     */
    public float[] getCentroids() {
        return centroids;
    }

}
