package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.vabra.VabraAlgorithm;
import gov.nih.mipav.model.algorithms.utilities.*;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;
import java.util.List;

import javax.swing.*;



public class JDialogVABRA extends JDialogBase implements AlgorithmInterface {


    /** source image. */
    private ModelImage imageA;

    /** DOCUMENT ME! */
    private ModelImage imageB;

    /** DOCUMENT ME! */
    private boolean isColor = false;
    
    /** DOCUMENT ME! */
    private JComboBox comboBoxImage;

    /** DOCUMENT ME! */
    private String[] titles;

    /** DOCUMENT ME! */
    private ViewUserInterface userInterface;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogVABRA() { }

    /**
     * Creates new image calculator dialog and displays.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogVABRA(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        imageA = im;
        isColor = im.isColorImage();
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

    	if (command.equals("OK")) {
    		setVisible(false);
    		callAlgorithm();
    	} else if (command.equals("Cancel")) {
    		dispose();
    	}
    }


    /**
     * Once all the necessary variables are set, call the Gaussian Blur algorithm based on what type of image this is
     * and whether or not there is a separate destination image.
     */
    protected void callAlgorithm() {

        String selectedName = (String) comboBoxImage.getSelectedItem();
        imageB = ViewUserInterface.getReference().getRegisteredImageByName(selectedName);
        

		
		VabraAlgorithm vabra = new VabraAlgorithm();
		// if you pass in null as the second argument it will extract the B0 image to use as the subject:
		vabra.solve( imageA, imageB );
		// Opens the Deformation Field in a window:
		new ViewJFrameImage( vabra.getDeformationField() );		
		new ViewJFrameImage( vabra.getRegisteredResults() );
		
    }

    /**
     * Builds a list of images to operate on from the template image.
     */
    private void buildComboBoxImage() {
        int j;
        ViewUserInterface UI;
        boolean sameDims = true;

        comboBoxImage.removeAllItems();

        UI = ViewUserInterface.getReference();

        Enumeration<String> names = UI.getRegisteredImageNames();

        // Add images from user interface that have the same exact dimensionality
        // Guaranteed to have at least one unique potential image B, because it's
        // tested for in ViewJFrameImage before this dialog is created.
        while (names.hasMoreElements()) {
            String name = names.nextElement();
            sameDims = true;

            if (!imageA.getImageName().equals(name)) {
                ModelImage img = UI.getRegisteredImageByName(name);

                if (UI.getFrameContainingImage(img) != null) {

                    if (imageA.getNDims() == img.getNDims()) {

                        //for (j = 0; j < imageA.getNDims(); j++) {

                        //    if (imageA.getExtents()[j] != img.getExtents()[j]) {
                        //        sameDims = false;
                        //    }
                        //}

                        //if ((sameDims == true) && (isColor == img.isColorImage())) {
                            comboBoxImage.addItem(name);
                        //}
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
        setTitle("Image Calculator");

        JPanel inputPanel = new JPanel(new GridBagLayout());
        inputPanel.setForeground(Color.black);

        inputPanel.setBorder(buildTitledBorder("VABRA Deformation Field"));

        JLabel labelUse = new JLabel("DWI B0 Image:");
        labelUse.setForeground(Color.black);
        labelUse.setFont(serif12);

        JLabel labelImageA = new JLabel(imageA.getImageName());
        labelImageA.setForeground(Color.black);
        labelImageA.setFont(serif12);

        JLabel labelImageB = new JLabel("Structural Image: ");
        labelImageB.setForeground(Color.black);
        labelImageB.setFont(serif12);

        comboBoxImage = new JComboBox();
        comboBoxImage.setFont(serif12);
        comboBoxImage.setBackground(Color.white);
        
        buildComboBoxImage();

        if(comboBoxImage.getModel().getSize() == 0) {
            MipavUtil.displayWarning("Two images required!");
            return;
        }

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.gridheight = 1;
        gbc.gridwidth = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 0;
        gbc.insets = new Insets(5, 5, 5, 5);
        inputPanel.add(labelUse, gbc);
        gbc.gridx = 1;
        gbc.weightx = 1;
        inputPanel.add(labelImageA, gbc);
        gbc.gridx = 0;
        gbc.weightx = 0;
        gbc.gridy = 1;
        gbc.fill = GridBagConstraints.NONE;
        inputPanel.add(labelImageB, gbc);
        gbc.gridx = 1;
        gbc.weightx = 1;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        inputPanel.add(comboBoxImage, gbc);



        JPanel mainPanel = new JPanel(new BorderLayout());
        mainPanel.add(inputPanel);
        mainPanel.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));

        JPanel buttonPanel = new JPanel();

        buildOKButton();
        buildCancelButton();
        
        
        buttonPanel.add(OKButton);
        buttonPanel.add(cancelButton);

        getContentPane().add(mainPanel);
        getContentPane().add(buttonPanel, BorderLayout.SOUTH);
        pack();
        setVisible(true);
    }


	@Override
	public void algorithmPerformed(AlgorithmBase algorithm) {
		// TODO Auto-generated method stub
		
	}

}
