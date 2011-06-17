package gov.nih.mipav.view.renderer.WildMagic.BrainSubcortical;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.provenance.ProvenanceRecorder;
import gov.nih.mipav.model.scripting.*;
import gov.nih.mipav.model.scripting.actions.ActionMaskToVOI;
import gov.nih.mipav.model.scripting.parameters.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.components.WidgetFactory;
import gov.nih.mipav.view.dialogs.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;

import java.util.*;

import javax.swing.*;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;



public class JDialogBrainSubcortical extends JDialogBase implements AlgorithmInterface
{

    /** DOCUMENT ME! */
    private ModelImage imageA; // source image
    private ModelImage imageB;
    /** The main user interface. */
    private ViewUserInterface UI;


    // GUI interface 
    private JLabel labelImageA, labelImageB;
    private JTextField textFieldImageA, textFieldImageB;
    private JButton buttonImageA, buttonImageB;
    
    private JPanel imageSelectionPanel;
    private JPanel buttonPanel;
    
    
    // Image instances
    private ImageInstanceBrainSubcortical imageAreader;
    private ImageInstanceBrainSubcortical imageBreader;
    
    
    public JDialogBrainSubcortical(Frame theParentFrame) {
        super(theParentFrame, false);
        UI = ViewUserInterface.getReference();
        init();
        imageAreader = new ImageInstanceBrainSubcortical(".mgz", textFieldImageA, this);
        imageBreader = new ImageInstanceBrainSubcortical(".mgz", textFieldImageB, this);
    }
    
   
    public void actionPerformed(ActionEvent event) {
    	String command = event.getActionCommand();

        if (command.equals("OK")) {
        	
            if (setVariables()) {
                callAlgorithm();
            }
        } 
        else if (command.equals("Cancel")) {
            dispose();
        }
        else if (command.equals("Help")) {
            // MipavUtil.showHelp("19074");
        }
        
        else if ( command.equals("ChooseImageA")) {
             imageAreader.selectFile();	
        }
        else if ( command.equals("ChooseImageB")) {
        	imageBreader.selectFile();
        }
        
    }

    public void init() {
        setTitle("Brain Sub-Cortical Structures Analysis");
        
        final JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
        
        final GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.EAST;

        imageSelectionPanel = new JPanel();
        imageSelectionPanel.setLayout(new GridLayout(2, 3));
        imageSelectionPanel.setBorder(buildTitledBorder("Select two image files to compare"));
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        // imageA
        labelImageA = new JLabel("ImageA: ");
        labelImageA.setFont(serif12);
        labelImageA.setForeground(Color.black);
        
        imageSelectionPanel.add(labelImageA, gbc);
        
        textFieldImageA = new JTextField(20);
        textFieldImageA.setFont(serif12);
        
        gbc.gridx = 1;
        imageSelectionPanel.add(textFieldImageA, gbc);
        
        buttonImageA = new JButton("Choose");
        buttonImageA.addActionListener(this);
        buttonImageA.setActionCommand("ChooseImageA");
        buttonImageA.setFont(serif12B);
        buttonImageA.setPreferredSize(MipavUtil.defaultButtonSize);
        
        gbc.gridx = 2;
        imageSelectionPanel.add(buttonImageA, gbc);
        
        
        // imageB
        gbc.gridx = 0;
        gbc.gridy = 1;
        
        labelImageB = new JLabel("ImageB: ");
        labelImageB.setFont(serif12);
        labelImageB.setForeground(Color.black);
        
        imageSelectionPanel.add(labelImageB, gbc);
        
        textFieldImageB = new JTextField(20);
        textFieldImageB.setFont(serif12);
        
        gbc.gridx = 1;
        imageSelectionPanel.add(textFieldImageB, gbc);
        
        buttonImageB = new JButton("Choose");
        buttonImageB.addActionListener(this);
        buttonImageB.setActionCommand("ChooseImageB");
        buttonImageB.setFont(serif12B);
        buttonImageB.setPreferredSize(MipavUtil.defaultButtonSize);
        
        gbc.gridx = 2;
        imageSelectionPanel.add(buttonImageB, gbc);
        
        
        // button Panel
        buttonPanel = new JPanel();
        buttonPanel.setLayout(new GridLayout(1, 3));
        gbc.gridx = 0;
        gbc.gridy = 0;
        buttonPanel.add(buildOKButton(), gbc);
        gbc.gridy = 1;
        buttonPanel.add(buildCancelButton(), gbc);
        gbc.gridy = 2;
        buttonPanel.add(buildHelpButton(), gbc);

        mainPanel.add(imageSelectionPanel);
        mainPanel.add(buttonPanel);

        getContentPane().add(mainPanel);
        pack();
        setVisible(true);

    }

    /**
     * Builds a titled border with the given title, an etched border, and the proper font and color.
     *
     * @param   title  Title of the border
     *
     * @return  The titled border.
     */
    protected TitledBorder buildTitledBorder(String title) {
        return new TitledBorder(new EtchedBorder(), title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B,
                                Color.black);
    }
    
    private boolean setVariables() {
    	return true;
    }
    
    private void loadImages() {
    	imageAreader.readImage();
    	imageBreader.readImage();
    	
    }
    
    public void callAlgorithm() {
    	
        // Step1. load the image file
    	loadImages();
    	
    	// Step2. threshold the image into sub-cortical structures
    	threshold();
    	
    	// Step3. Image registration on the related sub structures. 
    	registration();
    	
    	// Step4. Save threshold images and registered images. 
    	saveImages();
    	
    	// Step5. Comparison
    	doComparison();
    	 	
    	// Step6. Static data reporting
    	printReport();
    	
    	// Step 7. dispose memory
    	imageAreader.disposeLocal();
    	imageBreader.disposeLocal();
    	
    	
    }
    
    
    public void printReport() {
    	imageAreader.printReport();
    }
    
    public void doComparison() {
    	imageAreader.originImageThresholdBinary();
    	imageAreader.registeredImageThresholdBinary();
    	imageAreader.statisticsDataGeneration();
    }
    
    
    public void saveImages() {
    	imageAreader.saveImages();
    	imageBreader.saveImages();
    	imageAreader.saveRegisteredImages();
    }
    
    public void registration() {
    	imageAreader.registration(imageBreader);
    }
    
    public void threshold() {
    	imageAreader.threshold();
    	imageBreader.threshold();
    }

    
    public void algorithmPerformed(AlgorithmBase algorithm) {
    	imageAreader.algorithmPerformed(algorithm);
    	dispose();
    }


}

