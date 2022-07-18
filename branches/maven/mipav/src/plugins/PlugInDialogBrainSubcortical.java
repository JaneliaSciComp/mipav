import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.view.dialogs.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;

import java.util.*;

import javax.swing.*;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;



public class PlugInDialogBrainSubcortical implements AlgorithmInterface
{
	/** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -6812105487936807310L;
    
    /** The main user interface. */
    private ViewUserInterface UI;

    // GUI interface 
    /** image repository directory. */
    private JLabel labelImageDir;
    
    /** textfield for image repository directory. */
    private JTextField textFieldImageDir;
    
    /** image directory selection button. */
    private JButton buttonImageDir;
   
    /** image selection panel. */ 
    private JPanel imageSelectionPanel;
    
    /** OK, cancel button pane. */
    private JPanel buttonPanel;
   
    // Directory variables
    private JFileChooser chooser = new JFileChooser();
   
    /** image repository directory */ 
    private String inputDir;
    
    /** saved image and report directory. */
    private String outputDir;
    
    /** case comparison file directory. */
    private String caseCompareDir;
    
    /** registered section names. */
    private Vector<Integer> regSection;
    
    /** Algorithm to run brain subcotrical registration. */
    private PlugInAlgorithmBrainSubcortical subCorticalAlgo;
    
    /**
     * Constructor of Brain Subcortical Dialog.  Called from the MIPAV menu. 
     * @param theParentFrame  parent frame.
     */
    public PlugInDialogBrainSubcortical(Frame theParentFrame) {
        UI = ViewUserInterface.getReference();
        //init();
    }

    /**
     * Constructor of Brain subcortical dialog.  Called from Plugin dialog. 
     * @param theParentFrame
     * @param _inputDir    brain MRI image repository input directory.
     * @param _outputDir   output result directory. 
     * @param _regSection  registered sections in vector. 
     */
    public PlugInDialogBrainSubcortical(Frame theParentFrame, String _inputDir, String _outputDir, String _caseCompareDir, Vector<Integer> _regSection) {
    	inputDir = _inputDir;
    	outputDir = _outputDir;
    	caseCompareDir = _caseCompareDir;
    	if ( outputDir == null ) {
    		outputDir = inputDir;
    	}
    	regSection = _regSection;
    	UI = ViewUserInterface.getReference();
    
        callAlgorithm();
         
    }
    
    /**
     * Action performed 
     * @param action event, buttion click events
     */
    public void actionPerformed(ActionEvent event) {
    	String command = event.getActionCommand();

        if (command.equals("OK")) {
               callAlgorithm();
        } 
        else if (command.equals("Cancel")) {
            //dispose();
        }
        else if (command.equals("Help")) {
            // MipavUtil.showHelp("19074");
        }
        
        else if ( command.equals("ChooseImageDir")) {
             /// imageAreader.selectFile();	
        	readDirectory();
        }
        else if ( command.equals("ChooseImageB")) {
        	
        }
        
    }

    /**
     * Construct the GUI interface for Brain subcotical analysis. 
     */
    /*public void init() {
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
        imageSelectionPanel.setLayout(new GridLayout(1, 3));
        imageSelectionPanel.setBorder(buildTitledBorder("Select image directory"));
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        // image directory
        labelImageDir = new JLabel("Image Directory: ");
        labelImageDir.setFont(serif12);
        labelImageDir.setForeground(Color.black);
        
        imageSelectionPanel.add(labelImageDir, gbc);
        
        textFieldImageDir = new JTextField(20);
        textFieldImageDir.setFont(serif12);
        
        gbc.gridx = 1;
        imageSelectionPanel.add(textFieldImageDir, gbc);
        
        buttonImageDir = new JButton("Choose");
        buttonImageDir.addActionListener(this);
        buttonImageDir.setActionCommand("ChooseImageDir");
        buttonImageDir.setFont(serif12B);
        buttonImageDir.setPreferredSize(MipavUtil.defaultButtonSize);
        
        gbc.gridx = 2;
        imageSelectionPanel.add(buttonImageDir, gbc);
       
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

    }*/

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
 
    
    /**
     * Let the user choose the input image repository directory.   Filter out the selected case that needs to 
     * do comparison.   
     */
    private void readDirectory() {
    	chooser.setDialogTitle("Select Archived Directory");
    	chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
    	
        if (UI.getDefaultDirectory() != null) {
            final File file = new File(UI.getDefaultDirectory());

            if (file != null) {
                chooser.setCurrentDirectory(file);
            }
        } else {
            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }

        // chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] {file_suffix}));

        final int returnValue = chooser.showOpenDialog(UI.getMainFrame());
        String fileName;
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            fileName = chooser.getSelectedFile().getName();
            inputDir = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar + fileName;
            textFieldImageDir.setText(inputDir);
            /*
            File fileDir = new File(inputDir);
            System.err.println(inputDir);
             
            FilenameFilter fileNameFilter = new asegFilter();
            File[] fileNames = fileDir.listFiles(fileNameFilter);
            for ( int i = 0; i < fileNames.length; i++ ) {            
            	System.err.println("name: = " + fileNames[i]);
            }
            */
            // traverse(fileDir);
        } else {
            return;
        }
        /*
        for ( int i = 0; i < caseVector.size(); i++ ) {
        	System.err.println(caseVector.get(i));
        }
        */
    }
    
   
    /**
     * The first instance is used as the reference.   We do comparison by using the reference image as the base, 
     * compare it with rest images.  
     */
	public void callAlgorithm() {

		subCorticalAlgo = new PlugInAlgorithmBrainSubcortical(inputDir, outputDir, caseCompareDir, this, regSection);
	    subCorticalAlgo.run();
	}
    
	public void algorithmPerformed(AlgorithmBase algorithm) {
		
	}

    /** 
     * Filter out aseg.mgz file for each case. 
     * @author ruida
     *
     */
    class asegFilter implements FilenameFilter {
        public boolean accept(File dir, String name) {
            // return (name.endsWith("aseg.mgz"));
        	return name.equals("aseg.mgz");
        }
    }
   
   
}

