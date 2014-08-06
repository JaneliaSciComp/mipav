package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to create Hough transform with x0, y0, rad output for cardioid detection in
 * binary image, where sqrt((x - x0)**2 + (y - y0)**2) = rad*(1 + cos(theta)) for cusp on left
 * sqrt((x - x0)**2 + (y - y0)**2) = rad*(1 - cos(theta)) for cusp on right
 * sqrt((x - x0)**2 + (y - y0)**2) = rad*(1 + sin(theta)) for cusp on top
 * sqrt((x - x0)**2 + (y - y0)**2) = rad*(1 - sin(theta)) for cusp on bottom
 */
public class JDialogHoughCardioid extends JDialogBase implements AlgorithmInterface, ItemListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 0L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmHoughCardioid hAlgo = null;

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
    
    private JCheckBox leftBox;
    
    private boolean leftSelected;
    
    private JCheckBox rightBox;
    
    private boolean rightSelected;
    
    private JCheckBox topBox;
    
    private boolean topSelected;
    
    private JCheckBox bottomBox;
    
    private boolean bottomSelected;
    
    private int rad;

    /** DOCUMENT ME! */
    private JTextField radText;
    
    private int numCardioids;
    
    private JTextField numCardioidsText;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogHoughCardioid object.
     *
     * @param  image  DOCUMENT ME!
     */
    public JDialogHoughCardioid(ModelImage image) {
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
    public JDialogHoughCardioid(Frame theParentFrame, ModelImage im) {
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
            MipavUtil.showWebHelp("Hough_Transform#Running_the_Hough_Transform_for_Cardioid_Detection_algorithm");
        } else if (command.equals("Cancel")) {
            dispose();
        } else {
            super.actionPerformed(event);
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


        if (algorithm instanceof AlgorithmHoughCardioid) {
            Preferences.debug("Hough Cardioid: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((hAlgo.isCompleted() == true) && (resultImage != null)) {


                resultImage.clearMask();

                try {
                    openNewFrame(resultImage);
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
        } // if (algorithm instanceof AlgorithmHoughCircle)

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
            String name = makeImageName(image.getImageName(), "_hough_cardioid");
            resultImage = new ModelImage(image.getType(), image.getExtents(), name);
            resultImage.setImageName(name);

            // Make algorithm
            hAlgo = new AlgorithmHoughCardioid(resultImage, image, leftSelected, rightSelected,
            		    topSelected, bottomSelected, x0, y0, rad, numCardioids);

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
            MipavUtil.displayError("Dialog Hough Cardioid: unable to allocate enough memory");

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
        JLabel radLabel;
        JLabel numCardioidsLabel;
        int xDim = Math.min(512, image.getExtents()[0]);
        int yDim = Math.min(512, image.getExtents()[1]);
        int rDim = Math.min(512, Math.max(image.getExtents()[0], image.getExtents()[1]));
        setForeground(Color.black);
        setTitle("Hough transform for cardioid detection");

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Hough transform parameters"));

        GridBagConstraints gbc6 = new GridBagConstraints();

        gbc6.gridwidth = 1;
        gbc6.gridheight = 1;
        gbc6.anchor = GridBagConstraints.WEST;
        gbc6.weightx = 1;
        gbc6.insets = new Insets(3, 3, 3, 3);
        gbc6.fill = GridBagConstraints.HORIZONTAL;
        gbc6.gridx = 0;
        gbc6.gridy = 0;
        
        mainLabel = new JLabel("Look for cusp positions:");
        mainLabel.setForeground(Color.black);
        mainLabel.setFont(serif12);
        mainLabel.setEnabled(true);
        paramPanel.add(mainLabel, gbc6);
        
        leftBox = new JCheckBox("Left", true);
        leftBox.setForeground(Color.black);
        leftBox.setFont(serif12);
        leftBox.setEnabled(true);
        gbc6.gridy = 1;
        paramPanel.add(leftBox, gbc6);
        
        rightBox = new JCheckBox("Right", false);
        rightBox.setForeground(Color.black);
        rightBox.setFont(serif12);
        rightBox.setEnabled(true);
        gbc6.gridy = 2;
        paramPanel.add(rightBox, gbc6);
        
        topBox = new JCheckBox("Top", false);
        topBox.setForeground(Color.black);
        topBox.setFont(serif12);
        topBox.setEnabled(true);
        gbc6.gridy = 3;
        paramPanel.add(topBox, gbc6);
        
        bottomBox = new JCheckBox("Bottom", false);
        bottomBox.setForeground(Color.black);
        bottomBox.setFont(serif12);
        bottomBox.setEnabled(true);
        gbc6.gridy = 4;
        paramPanel.add(bottomBox, gbc6);

        x0Label = new JLabel("x0 dimension of Hough transform image ");
        x0Label.setForeground(Color.black);
        x0Label.setFont(serif12);
        x0Label.setEnabled(true);
        gbc6.gridy = 5;
        paramPanel.add(x0Label, gbc6);

        x0Text = new JTextField(10);
        x0Text.setText(String.valueOf(xDim));
        x0Text.setFont(serif12);
        x0Text.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(x0Text, gbc6);

        y0Label = new JLabel("y0 dimension of Hough transform image ");
        y0Label.setForeground(Color.black);
        y0Label.setFont(serif12);
        y0Label.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy = 6;
        paramPanel.add(y0Label, gbc6);

        y0Text = new JTextField(10);
        y0Text.setText(String.valueOf(yDim));
        y0Text.setFont(serif12);
        y0Text.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(y0Text, gbc6);
        
        radLabel = new JLabel("Rad dimension of Hough transform image ");
        radLabel.setForeground(Color.black);
        radLabel.setFont(serif12);
        radLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy = 7;
        paramPanel.add(radLabel, gbc6);

        radText = new JTextField(10);
        radText.setText(String.valueOf(rDim));
        radText.setFont(serif12);
        radText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(radText, gbc6);
        
        numCardioidsLabel = new JLabel("Number of cardioids ");
        numCardioidsLabel.setForeground(Color.black);
        numCardioidsLabel.setFont(serif12);
        numCardioidsLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy = 8;
        paramPanel.add(numCardioidsLabel, gbc6);
        
        numCardioidsText = new JTextField(3);
        numCardioidsText.setText("1");
        numCardioidsText.setFont(serif12);
        numCardioidsText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(numCardioidsText, gbc6);

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
    	
    	leftSelected = leftBox.isSelected();
    	rightSelected = rightBox.isSelected();
    	topSelected = topBox.isSelected();
    	bottomSelected = bottomBox.isSelected();

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
        
        if (!testParameter(radText.getText(), 5, 1000000)) {
            radText.requestFocus();
            radText.selectAll();

            return false;
        } else {
            rad = Integer.valueOf(radText.getText()).intValue();
        }
        
        if (!testParameter(numCardioidsText.getText(), 1, 100)) {
            numCardioidsText.requestFocus();
            numCardioidsText.selectAll();

            return false;
        } else {
            numCardioids = Integer.valueOf(numCardioidsText.getText()).intValue();
        }

        return true;
    }
}
