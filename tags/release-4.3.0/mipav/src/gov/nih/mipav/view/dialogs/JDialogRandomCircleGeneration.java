package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to create an image with randomly space circles of the same size
 */
public class JDialogRandomCircleGeneration extends JDialogBase implements AlgorithmInterface, ItemListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 0L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private AlgorithmRandomCircleGeneration rAlgo = null;

    private int extents[];

    /** DOCUMENT ME! */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private int xDim = 512;

    /** DOCUMENT ME! */
    private JTextField xDimText;

    /** DOCUMENT ME! */
    private int yDim = 512;
    
    private JTextField yDimText;
    
    private int radius = 5;

    /** DOCUMENT ME! */
    private JTextField radiusText;
    
    private int numCircles = 200;
    
    private JTextField numCirclesText;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogRandomCircleGeneration(Frame theParentFrame) {
        super(theParentFrame, false);
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
            //MipavUtil.showHelp("");
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


        if (algorithm instanceof AlgorithmRandomCircleGeneration) {
            Preferences.debug("Random Circle Generation: " + algorithm.getElapsedTime());

            if ((rAlgo.isCompleted() == true) && (resultImage != null)) {


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
        } // if (algorithm instanceof AlgorithmRandomCircleGeneration)

        if (rAlgo != null) {
            rAlgo.finalize();
            rAlgo = null;
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
            String name = "Randomly_generated_circles";
            extents = new int[2];
            extents[0] = xDim;
            extents[1] = yDim;
            resultImage = new ModelImage(ModelStorageBase.BYTE, extents, name);
            resultImage.setImageName(name);

            // Make algorithm
            rAlgo = new AlgorithmRandomCircleGeneration(resultImage, radius, numCircles);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            rAlgo.addListener(this);
            
            createProgressBar(name, rAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (rAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                rAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("Dialog Random Circle Generation: unable to allocate enough memory");

            return;
        }
    }


    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        JLabel mainLabel;
        JLabel xDimLabel;
        JLabel yDimLabel;
        JLabel radiusLabel;
        JLabel numCirclesLabel;
        
        setForeground(Color.black);
        setTitle("Random spaced circle generation");

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Random circle parameters"));

        GridBagConstraints gbc6 = new GridBagConstraints();

        gbc6.gridwidth = 1;
        gbc6.gridheight = 1;
        gbc6.anchor = gbc6.WEST;
        gbc6.weightx = 1;
        gbc6.insets = new Insets(3, 3, 3, 3);
        gbc6.fill = GridBagConstraints.HORIZONTAL;
        gbc6.gridx = 0;
        gbc6.gridy = 0;

        xDimLabel = new JLabel("Image x dimension ");
        xDimLabel.setForeground(Color.black);
        xDimLabel.setFont(serif12);
        xDimLabel.setEnabled(true);
        gbc6.gridy = 0;
        paramPanel.add(xDimLabel, gbc6);

        xDimText = new JTextField(10);
        xDimText.setText(String.valueOf(xDim));
        xDimText.setFont(serif12);
        xDimText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(xDimText, gbc6);

        yDimLabel = new JLabel("Image y dimension ");
        yDimLabel.setForeground(Color.black);
        yDimLabel.setFont(serif12);
        yDimLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy = 1;
        paramPanel.add(yDimLabel, gbc6);

        yDimText = new JTextField(10);
        yDimText.setText(String.valueOf(yDim));
        yDimText.setFont(serif12);
        yDimText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(yDimText, gbc6);
        
        radiusLabel = new JLabel("Circle radius ");
        radiusLabel.setForeground(Color.black);
        radiusLabel.setFont(serif12);
        radiusLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy = 2;
        paramPanel.add(radiusLabel, gbc6);

        radiusText = new JTextField(10);
        radiusText.setText(String.valueOf(radius));
        radiusText.setFont(serif12);
        radiusText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(radiusText, gbc6);
        
        numCirclesLabel = new JLabel("Number of circles ");
        numCirclesLabel.setForeground(Color.black);
        numCirclesLabel.setFont(serif12);
        numCirclesLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy = 3;
        paramPanel.add(numCirclesLabel, gbc6);
        
        numCirclesText = new JTextField(3);
        numCirclesText.setText(String.valueOf(numCircles));
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

        if (!testParameter(xDimText.getText(), 5, 1000000)) {
            xDimText.requestFocus();
            xDimText.selectAll();

            return false;
        } else {
            xDim = Integer.valueOf(xDimText.getText()).intValue();
        }

        if (!testParameter(yDimText.getText(), 5, 1000000)) {
            yDimText.requestFocus();
            yDimText.selectAll();

            return false;
        } else {
            yDim = Integer.valueOf(yDimText.getText()).intValue();
        }
        
        if (!testParameter(radiusText.getText(), 1, 1000000)) {
            radiusText.requestFocus();
            radiusText.selectAll();

            return false;
        } else {
            radius = Integer.valueOf(radiusText.getText()).intValue();
        }
        
        if (!testParameter(numCirclesText.getText(), 1, 100000)) {
            numCirclesText.requestFocus();
            numCirclesText.selectAll();

            return false;
        } else {
            numCircles = Integer.valueOf(numCirclesText.getText()).intValue();
        }

        return true;
    }
}