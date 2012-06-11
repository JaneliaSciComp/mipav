package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.AlgorithmHistogram;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;

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

public class JDialogIntensityHistogram extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    //private static final long serialVersionUID;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */

    /** DOCUMENT ME! */
    private JTextField minField = null;
    private JTextField maxField = null;
    private JTextField binsField = null;
    private double minValue;
    private double maxValue;
    private int numBins = 256;
    private double imageMin;
    private double imageMax;
    private int maxBins;
    private ModelImage image;
    private ButtonGroup colorGroup;
    private JRadioButton redButton;
    private JRadioButton greenButton;
    private JRadioButton blueButton;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogIntensityHistogram object.
     *
     * @param  theParentFrame  DOCUMENT ME!
     * @param  compImage       DOCUMENT ME!
     * @param  average         DOCUMENT ME!
     */
    public JDialogIntensityHistogram(JFrame theParentFrame, ModelImage image) {
        super(theParentFrame, false);
        this.image = image;
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
            minValue = imageMin;

            try {
                minValue = new Double(minField.getText()).doubleValue();
            } catch (Exception ex) {
                MipavUtil.displayError("minValue must be a double value");

                return;
            }
            if (minValue < imageMin) {
                MipavUtil.displayError("Minimum must be at least " + imageMin);
                return;
            }
            
            try {
                maxValue = new Double(maxField.getText()).doubleValue();
            } catch (Exception ex) {
                MipavUtil.displayError("maxValue must be a double value");

                return;
            }
            if (maxValue > imageMax) {
                MipavUtil.displayError("maxValue cannot exceed " + imageMax);
                return;
            }
            if (maxValue <= minValue) {
                MipavUtil.displayError("maxValue must be greater than " + minValue);
                return;
            }
            
            try {
                numBins = new Integer(binsField.getText()).intValue();
            }
            catch (Exception ex) {
                MipavUtil.displayError("numBins must be an integer value");
                return;
            }
            
            if (numBins < 2) {
                MipavUtil.displayError("Number of bins must be at least 2");
                return;
            }
            
            if (numBins > maxBins) {
                MipavUtil.displayError("Number of bins cannot exceed " + maxBins);
                return;
            }

            setVisible(false);
            
            AlgorithmHistogram histAlgo;
            boolean entireImage = false;
            boolean displayGraph = true;
            boolean userLimits = true;
            if (image.isColorImage()) {
                int  RGBOffset;
                if (redButton.isSelected()) {
                    RGBOffset = 1;    
                }
                else if (greenButton.isSelected()) {
                    RGBOffset = 2;
                }
                else {
                    RGBOffset = 3;
                }
                histAlgo = new AlgorithmHistogram(image, numBins, RGBOffset, entireImage, displayGraph,
                        userLimits, minValue, maxValue);
            } // if (image.isColorImage())
            else {
                histAlgo = new AlgorithmHistogram(image, numBins, entireImage, displayGraph,
                                                                 userLimits, minValue, maxValue);
            }
            histAlgo.run();
            
            this.dispose();
        } else if (source == cancelButton) {
            setVisible(false);
            this.dispose();
        }
    }

    /**
     * DOCUMENT ME!
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Histogram Intensity");

        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new GridBagLayout());
        GridBagConstraints gbc = new GridBagConstraints();
        mainPanel.setForeground(Color.black);

        mainPanel.setBorder(buildTitledBorder(""));
        imageMin = image.getMin();
        imageMax = image.getMax();

        JLabel minLabel = new JLabel("Minimum value   ");
        minLabel.setForeground(Color.black);
        minLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 0;
        mainPanel.add(minLabel, gbc);

        minField = new JTextField(10);
        minField.setText(String.valueOf(imageMin));
        minField.setForeground(Color.black);
        minField.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(minField, gbc);
        
        JLabel maxLabel = new JLabel("Maximum value   ");
        maxLabel.setForeground(Color.black);
        maxLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(maxLabel, gbc);

        maxField = new JTextField(10);
        maxField.setText(String.valueOf(imageMax));
        maxField.setForeground(Color.black);
        maxField.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(maxField, gbc);

        int dataType = image.getFileInfo()[0].getDataType();
        if ((dataType != ModelStorageBase.FLOAT) && (dataType != ModelStorageBase.DOUBLE) &&
            (dataType != ModelStorageBase.ARGB_FLOAT)) {
            maxBins = (int)Math.round(imageMax - imageMin + 1);
        }
        else {
            maxBins = 4096;
        }
        numBins = Math.min(256, maxBins);
        
        JLabel binsLabel = new JLabel("Number of bins   ");
        binsLabel.setForeground(Color.black);
        binsLabel.setFont(serif12);
        gbc.gridx = 0;
        gbc.gridy = 2;
        mainPanel.add(binsLabel, gbc);

        binsField = new JTextField(10);
        binsField.setText(String.valueOf(numBins));
        binsField.setForeground(Color.black);
        binsField.setFont(serif12);
        gbc.gridx = 1;
        mainPanel.add(binsField, gbc);
        
        if (image.isColorImage()) {
            colorGroup = new ButtonGroup();
            redButton = new JRadioButton("Red", true);
            redButton.setForeground(Color.black);
            redButton.setFont(serif12);
            gbc.anchor = gbc.WEST;
            gbc.gridx = 0;
            gbc.gridy = 3;
            colorGroup.add(redButton);
            mainPanel.add(redButton, gbc);
            
            greenButton = new JRadioButton("Green", true);
            greenButton.setForeground(Color.black);
            greenButton.setFont(serif12);
            gbc.gridx = 0;
            gbc.gridy = 4;
            colorGroup.add(greenButton);
            mainPanel.add(greenButton, gbc);
            
            blueButton = new JRadioButton("Blue", true);
            blueButton.setForeground(Color.black);
            blueButton.setFont(serif12);
            gbc.gridx = 0;
            gbc.gridy = 5;
            colorGroup.add(blueButton);
            mainPanel.add(blueButton, gbc);
        } // if (image.isColorImage())
        
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
