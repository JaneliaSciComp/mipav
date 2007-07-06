package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to create Hough transform with x0, y0, x1, y1, 2*a output for ellipse detection in
 * binary image, where sqrt((x - x0)**2 + (y - y0)**2) + sqrt((x - x1)**2 + (y - y1)**2) = 2*a,
 * where (x0, y0) and (x2, y1) are the 2 foci and 2*a is the length of the major axis.
 */
public class JDialogHoughEllipse extends JDialogBase implements AlgorithmInterface, ItemListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 0L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmHoughEllipse hAlgo = null;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private int x0;

    /** DOCUMENT ME! */
    private JTextField x0Text;

    /** DOCUMENT ME! */
    private int y0;
    
    private JTextField y0Text;
    
    private int numEllipses;
    
    private JTextField numEllipsesText;
    
    private double minMajorAxisLength;
    
    private JTextField minAxisText;
    
    private double maxMajorAxisLength;
    
    private JTextField maxAxisText;
    
    private JCheckBox default2aCheckBox;
    
    private JLabel twoaLabel;
    
    private JTextField twoaText;
    
    private int twoa;
    
    private double maxAxesRatio;
    
    private JTextField maxRatioText;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogHoughEllipse object.
     *
     * @param  image  DOCUMENT ME!
     */
    public JDialogHoughEllipse(ModelImage image) {
        super();
        this.image = image;
        parentFrame = image.getParentFrame();
    }

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogHoughEllipse(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Help")) {
            // MipavUtil.showHelp("");
        } else if (command.equals("Cancel")) {
            dispose();
        }
    }


    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {


        if (algorithm instanceof AlgorithmHoughEllipse) {
            Preferences.debug("Hough Ellipse: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((hAlgo.isCompleted() == true) && (resultImage != null)) {


                resultImage.clearMask();

                try {
                    new ViewJFrameImage(resultImage);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();

            }

            // insertScriptLine(algorithm);
        } // if (algorithm instanceof AlgorithmHoughEllipse)

        if (hAlgo != null) {
            hAlgo.finalize();
            hAlgo = null;
        }

        dispose();
    }

    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * itemStateChanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) {
        Object source = event.getSource();
        if (source == default2aCheckBox) {
            if (default2aCheckBox.isSelected()) {
                twoaLabel.setEnabled(false);
                twoaText.setEnabled(false);
            }
            else {
                twoaLabel.setEnabled(true);
                twoaText.setEnabled(true);
            }
        }
    }


    /**
     * Disposes of error dialog, then frame. Sets cancelled to <code>true</code>.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowClosing(WindowEvent event) {
        cancelFlag = true;
        dispose();
    }

    /**
     * DOCUMENT ME!
     */
    private void callAlgorithm() {

        try {
            String name = makeImageName(image.getImageName(), "_hough_ellipse");
            resultImage = new ModelImage(image.getType(), image.getExtents(), name);
            resultImage.setImageName(name);

            // Make algorithm
            hAlgo = new AlgorithmHoughEllipse(resultImage, image, x0, y0, minMajorAxisLength,
                                              maxMajorAxisLength, twoa, maxAxesRatio, numEllipses);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            hAlgo.addListener(this);
            
            createProgressBar(image.getImageName(), hAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (hAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                hAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("Dialog Hough Ellipse: unable to allocate enough memory");

            return;
        }
    }


    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        JLabel mainLabel;
        JLabel x0Label;
        JLabel y0Label;
        JLabel minAxisLabel;
        JLabel maxAxisLabel;
        JLabel maxRatioLabel;
        JLabel numEllipsesLabel;
        int xDim = Math.min(512, image.getExtents()[0]);
        int yDim = Math.min(512, image.getExtents()[1]);
        int rDim = Math.min(512, Math.max(image.getExtents()[0], image.getExtents()[1]));
        setForeground(Color.black);
        setTitle("Hough transform for ellipse detection");

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Hough transform parameters"));

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = gbc.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        mainLabel = new JLabel("sqrt((x - x0)**2 + (y - y0)**2) + sqrt((x - x1)**2 + (y - y1)**2) = 2*a");
        mainLabel.setForeground(Color.black);
        mainLabel.setFont(serif12);
        mainLabel.setEnabled(true);
        paramPanel.add(mainLabel, gbc);

        x0Label = new JLabel("x0 dimension of Hough transform image ");
        x0Label.setForeground(Color.black);
        x0Label.setFont(serif12);
        x0Label.setEnabled(true);
        gbc.gridy = 1;
        paramPanel.add(x0Label, gbc);

        x0Text = new JTextField(10);
        x0Text.setText(String.valueOf(xDim));
        x0Text.setFont(serif12);
        x0Text.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(x0Text, gbc);

        y0Label = new JLabel("y0 dimension of Hough transform image ");
        y0Label.setForeground(Color.black);
        y0Label.setFont(serif12);
        y0Label.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 2;
        paramPanel.add(y0Label, gbc);

        y0Text = new JTextField(10);
        y0Text.setText(String.valueOf(yDim));
        y0Text.setFont(serif12);
        y0Text.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(y0Text, gbc);
        
        minAxisLabel = new JLabel("Minimum 2*a length");
        minAxisLabel.setForeground(Color.black);
        minAxisLabel.setFont(serif12);
        minAxisLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 3;
        paramPanel.add(minAxisLabel, gbc);
        
        minAxisText = new JTextField(10);
        minAxisText.setText("1");
        minAxisText.setFont(serif12);
        minAxisText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(minAxisText, gbc);
        
        maxAxisLabel = new JLabel("Maximum 2*a length");
        maxAxisLabel.setForeground(Color.black);
        maxAxisLabel.setFont(serif12);
        maxAxisLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 4;
        paramPanel.add(maxAxisLabel, gbc);
        
        maxAxisText = new JTextField(10);
        maxAxisText.setText(String.valueOf(rDim));
        maxAxisText.setFont(serif12);
        maxAxisText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(maxAxisText, gbc);
        
        default2aCheckBox = new JCheckBox("Use default 2*a dim = 2*(max - min) + 1");
        default2aCheckBox.setSelected(true);
        default2aCheckBox.setFont(serif12);
        default2aCheckBox.setEnabled(true);
        default2aCheckBox.addItemListener(this);
        gbc.gridx = 0;
        gbc.gridy = 5;
        paramPanel.add(default2aCheckBox, gbc);
        
        twoaLabel = new JLabel("2*a dimension of hough transform image");
        twoaLabel.setForeground(Color.black);
        twoaLabel.setFont(serif12);
        twoaLabel.setEnabled(false);
        gbc.gridx = 0;
        gbc.gridy = 6;
        paramPanel.add(twoaLabel, gbc);
        
        twoaText = new JTextField(10);
        twoaText.setText("64");
        twoaText.setFont(serif12);
        twoaText.setEnabled(false);
        gbc.gridx = 1;
        paramPanel.add(twoaText, gbc);
        
        maxRatioLabel = new JLabel("Maximum major to minor axis ratio");
        maxRatioLabel.setForeground(Color.black);
        maxRatioLabel.setFont(serif12);
        maxRatioLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 7;
        paramPanel.add(maxRatioLabel, gbc);
        
        maxRatioText = new JTextField(10);
        maxRatioText.setText("2.0");
        maxRatioText.setFont(serif12);
        maxRatioText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(maxRatioText, gbc);
        
        numEllipsesLabel = new JLabel("Number of ellipses ");
        numEllipsesLabel.setForeground(Color.black);
        numEllipsesLabel.setFont(serif12);
        numEllipsesLabel.setEnabled(true);
        gbc.gridx = 0;
        gbc.gridy = 8;
        paramPanel.add(numEllipsesLabel, gbc);
        
        numEllipsesText = new JTextField(3);
        numEllipsesText.setText("1");
        numEllipsesText.setFont(serif12);
        numEllipsesText.setEnabled(true);
        gbc.gridx = 1;
        paramPanel.add(numEllipsesText, gbc);

        getContentPane().add(paramPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {

        if (!testParameter(x0Text.getText(), 5, 1000000)) {
            x0Text.requestFocus();
            x0Text.selectAll();

            return false;
        } else {
            x0 = Integer.valueOf(x0Text.getText()).intValue();
        }

        if (!testParameter(y0Text.getText(), 5, 1000000)) {
            y0Text.requestFocus();
            y0Text.selectAll();

            return false;
        } else {
            y0 = Integer.valueOf(y0Text.getText()).intValue();
        }
        
        if (!testParameter(minAxisText.getText(), 1.0, Math.max(image.getExtents()[0],image.getExtents()[1]))) {
            minAxisText.requestFocus();
            minAxisText.selectAll();

            return false;
        } else {
            minMajorAxisLength = Double.valueOf(minAxisText.getText()).doubleValue();
        }
        
        if (!testParameter(maxAxisText.getText(), 1.0, Math.max(image.getExtents()[0],image.getExtents()[1]))) {
            maxAxisText.requestFocus();
            maxAxisText.selectAll();

            return false;
        } else {
            maxMajorAxisLength = Double.valueOf(maxAxisText.getText()).doubleValue();
            if (maxMajorAxisLength < minMajorAxisLength) {
                MipavUtil.displayError("Maximum major axis length must be >= minimum major axis length");
                return false;
            }
        }
        
        if (default2aCheckBox.isSelected()) {
            twoa = (int)Math.round(2.0*(maxMajorAxisLength - minMajorAxisLength) + 1.0);
        }
        else {
            if (!testParameter(twoaText.getText(), 1, 1000000)) {
                twoaText.requestFocus();
                twoaText.selectAll();

                return false;
            } else {
                twoa = Integer.valueOf(x0Text.getText()).intValue();
            }    
        }
        
        if (!testParameter(maxRatioText.getText(), 1.0, 100.0)) {
            maxRatioText.requestFocus();
            maxRatioText.selectAll();

            return false;
        } else {
            maxAxesRatio = Double.valueOf(maxRatioText.getText()).intValue();
        }
        
        if (!testParameter(numEllipsesText.getText(), 1, 100)) {
            numEllipsesText.requestFocus();
            numEllipsesText.selectAll();

            return false;
        } else {
            numEllipses = Integer.valueOf(numEllipsesText.getText()).intValue();
        }

        return true;
    }
}
