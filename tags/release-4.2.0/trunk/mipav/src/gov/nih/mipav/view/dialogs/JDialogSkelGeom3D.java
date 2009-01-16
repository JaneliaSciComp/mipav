package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * DOCUMENT ME!
 *
 * @version  1.0 February 8, 2006
 * @author   William Gandler
 */
public class JDialogSkelGeom3D extends JDialogBase implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2336835186270980754L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean bad = false;

    /** DOCUMENT ME! */
    private float deep = 0.3f;

    /** DOCUMENT ME! */
    private boolean defer = false;

    /** DOCUMENT ME! */
    private float estR = 0.6f;

    /** DOCUMENT ME! */
    private ModelImage image = null; // source image

    /** DOCUMENT ME! */
    private float minObjectValue;

    /** DOCUMENT ME! */
    private JCheckBox noPropCheckBox;

    /** DOCUMENT ME! */
    private AlgorithmSkelGeom3D skelGeom3DAlgo;

    /** DOCUMENT ME! */
    private boolean sliceHoleFilling = true;

    /** DOCUMENT ME! */
    private JCheckBox sliceHoleFillingCheckBox;

    /** DOCUMENT ME! */
    private JTextField textMinObjectValue;

    /** DOCUMENT ME! */
    private JTextField textSamplingDensity;

    /** DOCUMENT ME! */
    private JTextField textWCosine;

    /** DOCUMENT ME! */
    private JCheckBox throwAwayCheckBox;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Sets the appropriate variables. Does not actually create a dialog that is visible because no user input is
     * necessary at present.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogSkelGeom3D(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        setForeground(Color.black);
        image = im;
        userInterface = ViewUserInterface.getReference();

        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Presently only the script function calls this method. When the script sends this dialog the action command, this
     * method calls run.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
            dispose();
        }

    }

    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete, so that the dialog can display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {

        if (algorithm instanceof AlgorithmSkeletonize3D) {

            // These next lines set the titles in all frames where the source image is displayed to
            // image name so as to indicate that the image is now unlocked!
            // The image frames are enabled and then registed to the userinterface.
            Vector imageFrames = image.getImageFrameVector();

            for (int i = 0; i < imageFrames.size(); i++) {
                ((Frame) (imageFrames.elementAt(i))).setTitle(titles[i]);
                ((Frame) (imageFrames.elementAt(i))).setEnabled(true);

                if ((((Frame) (imageFrames.elementAt(i))) != parentFrame) && (parentFrame != null)) {
                    userInterface.registerFrame((Frame) (imageFrames.elementAt(i)));
                }
            }

            if (parentFrame != null) {
                userInterface.registerFrame(parentFrame);
            }

            image.calcMinMax();
            image.notifyImageDisplayListeners(null, true);
        }
    }

    /**
     * Calls the algorithm.
     */
    protected void callAlgorithm() {

        try {

            System.gc();

            // Make algorithm

            skelGeom3DAlgo = new AlgorithmSkelGeom3D(image, minObjectValue, sliceHoleFilling, estR, bad, defer, deep);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            skelGeom3DAlgo.addListener(this);

            createProgressBar(image.getImageName(), skelGeom3DAlgo);
            
            // Hide dialog
            setVisible(false);

            // These next lines set the titles in all frames where the source image is displayed to
            // "locked - " image name so as to indicate that the image is now read/write locked!
            // The image frames are disabled and then unregisted from the userinterface until the
            // algorithm has completed.
            Vector imageFrames = image.getImageFrameVector();
            titles = new String[imageFrames.size()];

            for (int i = 0; i < imageFrames.size(); i++) {
                titles[i] = ((Frame) (imageFrames.elementAt(i))).getTitle();
                ((Frame) (imageFrames.elementAt(i))).setTitle("Locked: " + titles[i]);
                ((Frame) (imageFrames.elementAt(i))).setEnabled(false);
                userInterface.unregisterFrame((Frame) (imageFrames.elementAt(i)));
            }

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (skelGeom3DAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                skelGeom3DAlgo.run();
            }
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog SkelGeom 3D : unable to allocate enough memory");

            return;
        }
    }

    /**
     * Makes the GUI elements of the dialog.
     */
    private void init() {
        setTitle("Skeletonize 3D with Voronoi diagrams");
        getContentPane().setLayout(new BorderLayout());

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Parameters"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridwidth = 3;
        gbc.weightx = 1;

        JLabel labelMinObjectValue = new JLabel("Minimum value for outer pixel to be object");
        labelMinObjectValue.setForeground(Color.black);
        labelMinObjectValue.setFont(serif12);
        paramPanel.add(labelMinObjectValue, gbc);

        minObjectValue = (float) (image.getMin() + 1.0);
        textMinObjectValue = new JTextField(10);
        textMinObjectValue.setText(String.valueOf(minObjectValue));
        textMinObjectValue.setFont(serif12);
        textMinObjectValue.setForeground(Color.black);
        gbc.gridwidth = 1;
        gbc.gridx = 3;
        paramPanel.add(textMinObjectValue, gbc);

        sliceHoleFillingCheckBox = new JCheckBox("Slice by slice hole filling");
        sliceHoleFillingCheckBox.setFont(serif12);
        sliceHoleFillingCheckBox.setForeground(Color.black);
        sliceHoleFillingCheckBox.setSelected(true);
        gbc.gridwidth = 4;
        gbc.gridx = 0;
        gbc.gridy = 1;
        paramPanel.add(sliceHoleFillingCheckBox, gbc);

        JLabel labelSamplingDensity = new JLabel("Estimate for sampling density constant");
        labelSamplingDensity.setForeground(Color.black);
        labelSamplingDensity.setFont(serif12);
        gbc.gridwidth = 3;
        gbc.gridy = 2;
        paramPanel.add(labelSamplingDensity, gbc);

        textSamplingDensity = new JTextField(10);
        textSamplingDensity.setText("0.6");
        textSamplingDensity.setFont(serif12);
        textSamplingDensity.setForeground(Color.black);
        gbc.gridwidth = 1;
        gbc.gridx = 3;
        paramPanel.add(textSamplingDensity, gbc);

        throwAwayCheckBox = new JCheckBox("Throw away both poles for cells not long and skinny");
        throwAwayCheckBox.setFont(serif12);
        throwAwayCheckBox.setForeground(Color.black);
        throwAwayCheckBox.setSelected(false);
        gbc.gridwidth = 4;
        gbc.gridx = 0;
        gbc.gridy = 3;
        paramPanel.add(throwAwayCheckBox, gbc);

        noPropCheckBox = new JCheckBox("No propagation of 1st pole for cells not long and skinny");
        noPropCheckBox.setFont(serif12);
        noPropCheckBox.setForeground(Color.black);
        noPropCheckBox.setSelected(false);
        gbc.gridy = 4;
        paramPanel.add(noPropCheckBox, gbc);

        JLabel labelWCosine = new JLabel("Cosine for trying to label unlabled poles");
        labelWCosine.setForeground(Color.black);
        labelWCosine.setFont(serif12);
        gbc.gridwidth = 3;
        gbc.gridy = 5;
        paramPanel.add(labelWCosine, gbc);

        textWCosine = new JTextField(10);
        textWCosine.setText("0.3");
        textWCosine.setFont(serif12);
        textWCosine.setForeground(Color.black);
        gbc.gridwidth = 1;
        gbc.gridx = 3;
        paramPanel.add(textWCosine, gbc);

        getContentPane().add(paramPanel);

        JPanel buttonPanel = new JPanel();
        buildOKButton();
        buttonPanel.add(OKButton);
        buildCancelButton();
        buttonPanel.add(cancelButton);

        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setResizable(true);
        setVisible(true);
    }


    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        String tmpStr;

        tmpStr = textMinObjectValue.getText();
        minObjectValue = Float.parseFloat(tmpStr);

        if (minObjectValue <= image.getMin()) {
            MipavUtil.displayError("Minimum object value must exceed image min of " + image.getMin());
            textMinObjectValue.requestFocus();
            textMinObjectValue.selectAll();

            return false;
        } else if (minObjectValue > image.getMax()) {
            MipavUtil.displayError("Minimum object value must not exceed image max of " + image.getMax());
            textMinObjectValue.requestFocus();
            textMinObjectValue.selectAll();

            return false;
        }

        sliceHoleFilling = sliceHoleFillingCheckBox.isSelected();

        tmpStr = textSamplingDensity.getText();
        estR = Float.parseFloat(tmpStr);

        if (estR <= 0.0) {
            MipavUtil.displayError("Sampling density must be positive");
            textSamplingDensity.requestFocus();
            textSamplingDensity.selectAll();

            return false;
        }

        bad = throwAwayCheckBox.isSelected();

        defer = noPropCheckBox.isSelected();

        tmpStr = textWCosine.getText();
        deep = Float.parseFloat(tmpStr);

        if (deep <= 0.0) {
            MipavUtil.displayError("Cosine must be positive");
            textWCosine.requestFocus();
            textWCosine.selectAll();

            return false;
        } else if (deep > 1.0) {
            MipavUtil.displayError("Cosine must not exceed 1.0");
            textWCosine.requestFocus();
            textWCosine.selectAll();

            return false;
        }

        return true;
    }

}
