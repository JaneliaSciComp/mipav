package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to create Hough transform with x0, y0, rad output for circle detection in
 * binary image, where (x - x0)**2 + (y - y0)**2 = rad**2
 */
public class JDialogHoughCircle extends JDialogBase implements AlgorithmInterface, ItemListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 0L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmHoughCircle hAlgo = null;

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
    
    private int rad;

    /** DOCUMENT ME! */
    private JTextField radText;
    
    private int numCircles;
    
    private JTextField numCirclesText;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogHoughCircle object.
     *
     * @param  image  DOCUMENT ME!
     */
    public JDialogHoughCircle(ModelImage image) {
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
    public JDialogHoughCircle(Frame theParentFrame, ModelImage im) {
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
            MipavUtil.showHelp("HoughCircle002");
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


        if (algorithm instanceof AlgorithmHoughCircle) {
            Preferences.debug("Hough Circle: " + algorithm.getElapsedTime());
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
            String name = makeImageName(image.getImageName(), "_hough_circle");
            resultImage = new ModelImage(image.getType(), image.getExtents(), name);
            resultImage.setImageName(name);

            // Make algorithm
            hAlgo = new AlgorithmHoughCircle(resultImage, image, x0, y0, rad, numCircles);

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
            MipavUtil.displayError("Dialog Hough Circle: unable to allocate enough memory");

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
        JLabel numCirclesLabel;
        int xDim = Math.min(512, image.getExtents()[0]);
        int yDim = Math.min(512, image.getExtents()[1]);
        int rDim = Math.min(512, Math.max(image.getExtents()[0], image.getExtents()[1]));
        setForeground(Color.black);
        setTitle("Hough transform for circle detection");

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
        
        mainLabel = new JLabel("(x - x0)**2 + (y - y0)**2 = rad**2");
        mainLabel.setForeground(Color.black);
        mainLabel.setFont(serif12);
        mainLabel.setEnabled(true);
        paramPanel.add(mainLabel, gbc6);

        x0Label = new JLabel("x0 dimension of Hough transform image ");
        x0Label.setForeground(Color.black);
        x0Label.setFont(serif12);
        x0Label.setEnabled(true);
        gbc6.gridy = 1;
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
        gbc6.gridy = 2;
        paramPanel.add(y0Label, gbc6);

        y0Text = new JTextField(10);
        y0Text.setText(String.valueOf(yDim));
        y0Text.setFont(serif12);
        y0Text.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(y0Text, gbc6);
        
        radLabel = new JLabel("rad dimension of Hough transform image ");
        radLabel.setForeground(Color.black);
        radLabel.setFont(serif12);
        radLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy = 3;
        paramPanel.add(radLabel, gbc6);

        radText = new JTextField(10);
        radText.setText(String.valueOf(rDim));
        radText.setFont(serif12);
        radText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(radText, gbc6);
        
        numCirclesLabel = new JLabel("Number of circles ");
        numCirclesLabel.setForeground(Color.black);
        numCirclesLabel.setFont(serif12);
        numCirclesLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy = 4;
        paramPanel.add(numCirclesLabel, gbc6);
        
        numCirclesText = new JTextField(3);
        numCirclesText.setText("1");
        numCirclesText.setFont(serif12);
        numCirclesText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(numCirclesText, gbc6);

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
        
        if (!testParameter(radText.getText(), 5, 1000000)) {
            radText.requestFocus();
            radText.selectAll();

            return false;
        } else {
            rad = Integer.valueOf(radText.getText()).intValue();
        }
        
        if (!testParameter(numCirclesText.getText(), 1, 100)) {
            numCirclesText.requestFocus();
            numCirclesText.selectAll();

            return false;
        } else {
            numCircles = Integer.valueOf(numCirclesText.getText()).intValue();
        }

        return true;
    }
}
