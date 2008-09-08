package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call the algorithm. The user has the option to generate a new image or replace the
 * source image. In addition the user can indicate if you wishes to have the algorithm applied to whole image or to the
 * VOI regions. In should be noted, that the algorithms are executed in their own thread.
 *
 * @version  0.9 Oct 10, 2002
 * @author   Matthew J. McAuliffe, Ph.D.
 */
public class JDialogFrameLinker extends JDialogBase {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 8115834657630127291L;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private JComboBox comboBoxImage;

    /** DOCUMENT ME! */
    private ViewJFrameImage frameB;

    /** DOCUMENT ME! */
    private ModelImage imageA; // source image

    /** DOCUMENT ME! */
    private ModelImage imageB;

    /** DOCUMENT ME! */
    private int imageRef;

    /** DOCUMENT ME! */
    private JButton linkButton;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Used primarily for the script to store variables and run the algorithm. No actual dialog will appear but the set
     * up info and result image will be stored here.
     *
     * @param  im  Source image.
     */
    public JDialogFrameLinker(ModelImage im) {
        super();
        userInterface = ViewUserInterface.getReference();
        imageA = im;
    }

    /**
     * Creates new image calculator dialog and displays.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogFrameLinker(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        imageA = im;
        userInterface = ViewUserInterface.getReference();
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  event that triggers function
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();
        Object source = event.getSource();

        if (command.equals("Link")) {

            String selectedName = (String) comboBoxImage.getSelectedItem();
            imageB = userInterface.getRegisteredImageByName(selectedName);
            frameB = userInterface.getFrameContainingImage(imageB);

            ((ViewJFrameBase) (parentFrame)).setLinkedFrame(frameB);

        } else if (command.equals("Cancel")) {
            ((ViewJFrameBase) (parentFrame)).setLinkedFrame(null);
            dispose();
        }
    }

    /**
     * windowClosing - calls close.
     *
     * @param  event  event that triggered function
     */
    public void windowClosing(WindowEvent event) {
        ((ViewJFrameBase) (parentFrame)).setLinkedFrame(null);
        super.windowClosing(event);
    }


    /**
     * Not used.
     */
    protected void callAlgorithm() { }


    /**
     * Builds a list of images to operate on from the template image.
     */
    private void buildComboBoxImage() {
        int j;
        ViewUserInterface UI;
        boolean sameDims = true;

        comboBoxImage = new JComboBox();
        comboBoxImage.setFont(serif12);
        comboBoxImage.setBackground(Color.white);

        UI = ViewUserInterface.getReference();

        Enumeration names = UI.getRegisteredImageNames();

        // Add images from user interface that have the same exact dimensionality
        // Guaranteed to have at least one unique potential image B, because it's
        // tested for in ViewJFrameImage before this dialog is created.
        while (names.hasMoreElements()) {
            String name = (String) names.nextElement();
            sameDims = true;

            if (!imageA.getImageName().equals(name)) {
                ModelImage img = UI.getRegisteredImageByName(name);

                if (UI.getFrameContainingImage(img) != null) {

                    if (imageA.getNDims() == img.getNDims()) {

                        // only check z and t dimensions
                        for (j = 2; j < imageA.getNDims(); j++) {

                            if (imageA.getExtents()[j] != img.getExtents()[j]) {
                                sameDims = false;
                            }
                        }

                        if (sameDims == true) {
                            comboBoxImage.addItem(name);
                        }
                    }
                }
            }
        }
    }

    /**
     * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
     */
    private void init() {
        setForeground(Color.black);
        setTitle("Image Frame Linker");

        JPanel inputPanel = new JPanel(new GridBagLayout());
        inputPanel.setForeground(Color.black);

        inputPanel.setBorder(buildTitledBorder("Link to:"));

        JLabel labelUse = new JLabel("Image A:");
        labelUse.setForeground(Color.black);
        labelUse.setFont(serif12);

        JLabel labelImageA = new JLabel(imageA.getImageName());
        labelImageA.setForeground(Color.black);
        labelImageA.setFont(serif12);

        JLabel labelImageB = new JLabel("Image B: ");
        labelImageB.setForeground(Color.black);
        labelImageB.setFont(serif12);

        buildComboBoxImage();

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridheight = 1;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(5, 5, 5, 5);
        inputPanel.add(labelUse, gbc);
        gbc.gridx = 1;
        inputPanel.add(labelImageA, gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.fill = GridBagConstraints.NONE;
        inputPanel.add(labelImageB, gbc);
        gbc.gridx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        inputPanel.add(comboBoxImage, gbc);

        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(inputPanel);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel buttonPanel = new JPanel();

        // buildOKButton();
        linkButton = new JButton("Link");
        linkButton.addActionListener(this);
        linkButton.setMinimumSize(MipavUtil.defaultButtonSize);
        linkButton.setPreferredSize(MipavUtil.defaultButtonSize);
        linkButton.setFont(serif12B);
        buildCancelButton();

        // buttonPanel.add(OKButton);
        buttonPanel.add(linkButton);
        buttonPanel.add(cancelButton);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }


}
