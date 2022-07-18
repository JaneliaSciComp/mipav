package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to create Hough transform with x0, y0, rad output for cardioid detection in
 * binary image, where sqrt((x - x0)**2 + (y - y0)**2) = a*(1 - cos(theta + theta0))
 * sqrt((x - x0)**2 + (y - y0)**2) = a*(1 + cos(theta)) for cusp on left
 * sqrt((x - x0)**2 + (y - y0)**2) = a*(1 - cos(theta)) for cusp on right
 * sqrt((x - x0)**2 + (y - y0)**2) = a*(1 + sin(theta)) for cusp on top
 * sqrt((x - x0)**2 + (y - y0)**2) = a*(1 - sin(theta)) for cusp on bottom
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
    private int theta0Num;

    /** DOCUMENT ME! */
    private JTextField theta0Text;
    
    private int a0Num;

    /** DOCUMENT ME! */
    private JTextField aText;
    
    private int numCardioids;
    
    private JTextField numCardioidsText;
    
    /** Maximum number of points to take from each side of a point on a curve in determining a curvature */
    private int sidePointsForCurvature;
    
    private JTextField sideText;


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
            hAlgo = new AlgorithmHoughCardioid(resultImage, image,
            		    theta0Num, a0Num, numCardioids, sidePointsForCurvature);

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
        JLabel mainLabel2;
        JLabel theta0Label;
        JLabel a0Label;
        JLabel numCardioidsLabel;
        JLabel sideLabel;

        int aDim = Math.min(512, 2*Math.max(image.getExtents()[0], image.getExtents()[1]));
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
        
        mainLabel = new JLabel("theta = arctan((y-y0)/(x-x0))");
        mainLabel.setForeground(Color.black);
        mainLabel.setFont(serif12);
        mainLabel.setEnabled(true);
        paramPanel.add(mainLabel, gbc6);
        
        mainLabel2 = new JLabel("sqrt((x - x0)**2 + (y - y0)**2) = a*(1 - cos(theta + theta0))");
        mainLabel2.setForeground(Color.black);
        mainLabel2.setFont(serif12);
        mainLabel2.setEnabled(true);
        gbc6.gridy++;
        paramPanel.add(mainLabel2, gbc6);

        theta0Label = new JLabel("theta0 dimension of Hough transform image ");
        theta0Label.setForeground(Color.black);
        theta0Label.setFont(serif12);
        theta0Label.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy++;
        paramPanel.add(theta0Label, gbc6);

        theta0Text = new JTextField(10);
        theta0Text.setText(String.valueOf(720));
        theta0Text.setFont(serif12);
        theta0Text.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(theta0Text, gbc6);
        
        a0Label = new JLabel("a dimension of Hough transform image ");
        a0Label.setForeground(Color.black);
        a0Label.setFont(serif12);
        a0Label.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy++;
        paramPanel.add(a0Label, gbc6);

        aText = new JTextField(10);
        aText.setText(String.valueOf(aDim));
        aText.setFont(serif12);
        aText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(aText, gbc6);
        
        numCardioidsLabel = new JLabel("Number of cardioids ");
        numCardioidsLabel.setForeground(Color.black);
        numCardioidsLabel.setFont(serif12);
        numCardioidsLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy++;
        paramPanel.add(numCardioidsLabel, gbc6);
        
        numCardioidsText = new JTextField(3);
        numCardioidsText.setText("1");
        numCardioidsText.setFont(serif12);
        numCardioidsText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(numCardioidsText, gbc6);
        
        sideLabel = new JLabel("Maximum curve points on each side for curvature ");
        sideLabel.setForeground(Color.black);
        sideLabel.setFont(serif12);
        sideLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy++;
        paramPanel.add(sideLabel, gbc6);

        sideText = new JTextField(10);
        sideText.setText("5");
        sideText.setFont(serif12);
        sideText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(sideText, gbc6);


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

        if (!testParameter(theta0Text.getText(), 5, 1000000)) {
            theta0Text.requestFocus();
            theta0Text.selectAll();

            return false;
        } else {
            theta0Num = Integer.valueOf(theta0Text.getText()).intValue();
        }
        
        if (!testParameter(aText.getText(), 5, 1000000)) {
            aText.requestFocus();
            aText.selectAll();

            return false;
        } else {
            a0Num = Integer.valueOf(aText.getText()).intValue();
        }
        
        if (!testParameter(numCardioidsText.getText(), 1, 100)) {
            numCardioidsText.requestFocus();
            numCardioidsText.selectAll();

            return false;
        } else {
            numCardioids = Integer.valueOf(numCardioidsText.getText()).intValue();
        }
        
        if (!testParameter(sideText.getText(), 1, 10)) {
            sideText.requestFocus();
            sideText.selectAll();

            return false;
        } else {
            sidePointsForCurvature = Integer.valueOf(sideText.getText()).intValue();
        }


        return true;
    }
}

