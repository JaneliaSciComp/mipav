package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * Dialog to get user input signal 1 VOI, optional signal 2 VOI, background VOI, and number of NMR receivers needed for
 * MRI image SNR calculation.
 */
public class JDialogSingleMRIImageSNR extends JDialogBase implements AlgorithmInterface, ItemListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4621550176907564448L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    // private static final long serialVersionUID;

    /** DOCUMENT ME! */
    private JRadioButton backgroundButton;

    /** DOCUMENT ME! */
    private int backgroundIndex = -1;

    /** DOCUMENT ME! */
    private ViewJComponentEditImage componentImage;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private JLabel labelReceiver;

    /** DOCUMENT ME! */
    private int nBoundingVOIs;

    /** DOCUMENT ME! */
    private int numReceivers = 1;

    /** DOCUMENT ME! */
    private JRadioButton signal2Button;

    /** DOCUMENT ME! */
    private int signal2Index = -1;

    /** DOCUMENT ME! */
    private JRadioButton signalButton;

    /** DOCUMENT ME! */
    private int signalIndex = -1;

    /** DOCUMENT ME! */
    private AlgorithmSingleMRIImageSNR snrAlgo = null;

    /** DOCUMENT ME! */
    private JTextField textReceiver;

    /** DOCUMENT ME! */
    private ButtonGroup VOIGroup;

    /** DOCUMENT ME! */
    private ViewVOIVector VOIs;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogSingleMRIImageSNR object.
     *
     * @param  image  DOCUMENT ME!
     */
    public JDialogSingleMRIImageSNR(ModelImage image) {
        super();
        this.image = image;
        parentFrame = image.getParentFrame();
        componentImage = ((ViewJFrameImage) parentFrame).getComponentImage();
    }

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogSingleMRIImageSNR(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        componentImage = ((ViewJFrameImage) theParentFrame).getComponentImage();
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
        Object source = event.getSource();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Help")) {
            // MipavUtil.showHelp("");
        } else if (command.equals("Cancel")) {
            componentImage.getVOIHandler().setPresetHue(-1.0f);
            dispose();
        } else if ((source == signalButton) || (source == signal2Button) || (source == backgroundButton)) {

            if (signalButton.isSelected()) {
                componentImage.getVOIHandler().newVOI(0.0f);
                //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                //componentImage.getVOIHandler().setPresetHue(0.0f); // red
            } else if (signal2Button.isSelected()) {
                componentImage.getVOIHandler().newVOI(1.0f / 3.0f);
                //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                //componentImage.getVOIHandler().setPresetHue(1.0f / 3.0f); // green
            } else if (backgroundButton.isSelected()) {
                componentImage.getVOIHandler().newVOI(2.0f / 3.0f);
                //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
                //componentImage.getVOIHandler().setPresetHue(2.0f / 3.0f); // blue
            }
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


        if (snrAlgo.isCompleted() == true) { }

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
        componentImage.getVOIHandler().setPresetHue(-1.0f);
        cancelFlag = true;
        dispose();
    }

    /**
     * DOCUMENT ME!
     */
    protected void callAlgorithm() {

        try {

            componentImage.getVOIHandler().setPresetHue(-1.0f);

            // Make algorithm
            snrAlgo = new AlgorithmSingleMRIImageSNR(image, signalIndex, signal2Index, backgroundIndex, numReceivers);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            snrAlgo.addListener(this);

            createProgressBar(image.getImageName(), snrAlgo);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (snrAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {
                snrAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("Dialog Single MRI Image SNR: unable to allocate enough memory");

            return;
        }
    }


    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("MRI image SNR");

        JPanel VOIPanel = new JPanel(new GridBagLayout());
        VOIPanel.setBorder(buildTitledBorder("Select VOIs"));

        GridBagConstraints gbc4 = new GridBagConstraints();
        gbc4.gridwidth = 1;
        gbc4.gridheight = 1;
        gbc4.anchor = GridBagConstraints.WEST;
        gbc4.weightx = 1;
        gbc4.insets = new Insets(3, 3, 3, 3);
        gbc4.fill = GridBagConstraints.HORIZONTAL;
        gbc4.gridx = 0;
        gbc4.gridy = 0;

        VOIGroup = new ButtonGroup();

        signalButton = new JRadioButton("Add required signal VOI", true);
        signalButton.setForeground(Color.red);
        signalButton.setFont(serif12);
        signalButton.addActionListener(this);
        VOIGroup.add(signalButton);
        VOIPanel.add(signalButton, gbc4);
        componentImage.getVOIHandler().newVOI(0.0f);
        //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
        //componentImage.getVOIHandler().setPresetHue(0.0f); // red

        signal2Button = new JRadioButton("Add an optional second signal VOI", false);
        signal2Button.setForeground(Color.green.darker());
        signal2Button.setFont(serif12);
        signal2Button.addActionListener(this);
        VOIGroup.add(signal2Button);
        gbc4.gridy = 1;
        VOIPanel.add(signal2Button, gbc4);

        backgroundButton = new JRadioButton("Add required background VOI", false);
        backgroundButton.setForeground(Color.blue);
        backgroundButton.setFont(serif12);
        backgroundButton.addActionListener(this);
        VOIGroup.add(backgroundButton);
        gbc4.gridy = 2;
        VOIPanel.add(backgroundButton, gbc4);

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Input parameters"));

        GridBagConstraints gbc6 = new GridBagConstraints();

        gbc6.gridwidth = 1;
        gbc6.gridheight = 1;
        gbc6.anchor = GridBagConstraints.WEST;
        gbc6.weightx = 1;
        gbc6.insets = new Insets(3, 3, 3, 3);
        gbc6.fill = GridBagConstraints.HORIZONTAL;
        gbc6.gridx = 0;
        gbc6.gridy = 0;

        labelReceiver = new JLabel("Number of NMR receivers ");
        labelReceiver.setForeground(Color.black);
        labelReceiver.setFont(serif12);
        labelReceiver.setEnabled(true);
        paramPanel.add(labelReceiver, gbc6);

        textReceiver = new JTextField(10);
        textReceiver.setText("1");
        textReceiver.setFont(serif12);
        textReceiver.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(textReceiver, gbc6);

        getContentPane().add(VOIPanel, BorderLayout.NORTH);
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
        int i;
        int nVOIs;
        float[] hsb;
        float hue;
        VOIs = image.getVOIs();
        nVOIs = VOIs.size();
        nBoundingVOIs = 0;
        signalIndex = -1;
        backgroundIndex = -1;

        if (!testParameter(textReceiver.getText(), 1, 256)) {
            textReceiver.requestFocus();
            textReceiver.selectAll();

            return false;
        } else {
            numReceivers = Integer.valueOf(textReceiver.getText()).intValue();
        }

        for (i = 0; i < nVOIs; i++) {

            if ((VOIs.VOIAt(i).getCurveType() == VOI.CONTOUR) || (VOIs.VOIAt(i).getCurveType() == VOI.POLYLINE)) {
                nBoundingVOIs++;
                hsb = Color.RGBtoHSB(VOIs.VOIAt(i).getColor().getRed(), VOIs.VOIAt(i).getColor().getGreen(),
                                     VOIs.VOIAt(i).getColor().getBlue(), null);
                hue = hsb[0];

                if ((Math.abs(hue - 0.0f)) < 0.0001f) {

                    if (signalIndex == -1) {
                        signalIndex = i;
                        VOIs.VOIAt(i).setName("Signal");
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 signal VOI");

                        return false;
                    }
                } else if ((Math.abs(hue - (1.0f / 3.0f))) < 0.0001f) {

                    if (signal2Index == -1) {
                        signal2Index = i;
                        VOIs.VOIAt(i).setName("Signal2");
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 signal2 VOI");

                        return false;
                    }
                } else if ((Math.abs(hue - (2.0f / 3.0f))) < 0.0001f) {

                    if (backgroundIndex == -1) {
                        backgroundIndex = i;
                        VOIs.VOIAt(i).setName("Background");
                    } else {
                        MipavUtil.displayError("Cannot have more than 1 background VOI");

                        return false;
                    }
                } else {
                    MipavUtil.displayError("VOI hue = " + hue + " Must be 0 for red " + "or 2/3 for blue");

                    return false;
                }
            }
        } // for (i = 0; i < nVOIs; i++)

        if (signalIndex == -1) {
            MipavUtil.displayError("Must specify a signal VOI");

            return false;
        }

        if (backgroundIndex == -1) {
            MipavUtil.displayError("Must specify a background VOI");

            return false;
        }


        return true;
    }
}
