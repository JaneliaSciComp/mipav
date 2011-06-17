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
import java.util.regex.*;

import javax.swing.*;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;



public class JDialogBrainSubcorticalDir extends JDialogBrainSubcortical implements AlgorithmInterface
{

    /** DOCUMENT ME! */
    // private ModelImage imageA; // source image
    // private ModelImage imageB;
    /** The main user interface. */
    private ViewUserInterface UI;


    // GUI interface 
    private JLabel labelImageDir;
    private JTextField textFieldImageDir;
    private JButton buttonImageDir;
    
    private JPanel imageSelectionPanel;
    private JPanel buttonPanel;
    
    
    // Image instances
    private ImageInstanceBrainSubcorticalDir imageAreader;
    private ImageInstanceBrainSubcorticalDir imageBreader;
    
    private Vector<ImageInstanceBrainSubcorticalDir> instances = new  Vector<ImageInstanceBrainSubcorticalDir>();
    
    
    // Directory variables
    private JFileChooser chooser = new JFileChooser();
    // private String rootDirectory;
    
    // Case number directory
    private Vector<String> caseVector = new Vector<String>();
    
    private String inputDir;
    private String outputDir;
    private Vector regSection;
    
    public JDialogBrainSubcorticalDir(Frame theParentFrame) {
        super(theParentFrame);
        UI = ViewUserInterface.getReference();
        init();
        // imageAreader = new ImageInstanceBrainSubcortical(".mgz", textFieldImageA, this);
        // imageBreader = new ImageInstanceBrainSubcortical(".mgz", textFieldImageB, this);
    }
    
    public JDialogBrainSubcorticalDir(Frame theParentFrame, String _inputDir, String _outputDir, Vector _regSection) {
    	super(theParentFrame);
    	// setVisible(false);
    	inputDir = _inputDir;
    	outputDir = _outputDir;
    	if ( outputDir == null ) {
    		outputDir = inputDir;
    	}
    	regSection = _regSection;
    	UI = ViewUserInterface.getReference();
    	processFromCommandLine();
    	if (setVariables()) {
            callAlgorithm();
         }
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
        
        else if ( command.equals("ChooseImageDir")) {
             /// imageAreader.selectFile();	
        	readDirectory();
        }
        else if ( command.equals("ChooseImageB")) {
        	
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
    	int start; 
    	int end;
    	int index;
    	String numberRegex = "[0123456789]{1,}";
    	Pattern p = Pattern.compile(numberRegex);
    	for ( int i = 0; i < caseVector.size(); i++ ) {
    		String dir = caseVector.get(i);
    		
    		// System.err.println(dir);
    		
    		index = dir.lastIndexOf(File.separator);
    		String directory = new String(dir.substring(0, index+1));
    		String fileName = new String(dir.substring(index+1, dir.length()));
    		
    		// System.err.println(directory);
    		// System.err.println(fileName);
    		
    		Matcher m = p.matcher(dir);
    		if ( m.find() ) {
    			// System.err.println(m.group());
    			start = m.start();
    			end = m.end();
    			int caseNumber = Integer.valueOf(dir.substring(start, end));
    			// System.err.println(caseNumber);
    			/*
    			System.err.println("caseNumber = " + caseNumber);
    			System.err.println("fileName = " + fileName);
    			System.err.println("directory = " + directory);
    			*/
    			ImageInstanceBrainSubcorticalDir temp = new ImageInstanceBrainSubcorticalDir(caseNumber, fileName, directory, regSection, this); 
    			/*
    			System.err.println("temp.caseNumber = " + temp.caseNumber);
    			System.err.println("temp.fileName = " + temp.fileName);
    			System.err.println("temp.directory = " + temp.directory);
    			*/
    			instances.add(temp);
    		
    		}
    	}
    	
    	return true;
    }
    
    private void readDirectory() {
    	chooser.setDialogTitle("Select Archived Directory");
    	chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
    	
        if (UI.getDefaultDirectory() != null) {
            final File file = new File(UI.getDefaultDirectory());

            if (file != null) {
                chooser.setCurrentDirectory(file);
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
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
            File fileDir = new File(inputDir);
            System.err.println(inputDir);
             /*
            FilenameFilter fileNameFilter = new asegFilter();
            File[] fileNames = fileDir.listFiles(fileNameFilter);
            for ( int i = 0; i < fileNames.length; i++ ) {            
            	System.err.println("name: = " + fileNames[i]);
            }
            */
            traverse(fileDir);
        } else {
            return;
        }
        /*
        for ( int i = 0; i < caseVector.size(); i++ ) {
        	System.err.println(caseVector.get(i));
        }
        */
    }
    
    private void processDir(File dir) {

        // System.out.print( (dir.isDirectory() ? "[D] : " : "[F] : "));
        // System.out.println(dir);
    	String dirName = dir.toString();
    	int begin = dirName.lastIndexOf(File.separator)+1;
    	int end = dirName.length();
    	// System.err.println(dirName.substring(begin, end));
        if ( dirName.substring(begin, end).equals("aseg.mgz")) {
        	caseVector.add(dir.toString());
        }

    }

    private void traverse(File dir) {

        processDir(dir);

        if (dir.isDirectory()) {
            String[] children = dir.list();
            for (int i=0; i<children.length; i++) {
            	// System.err.println(dir.getAbsolutePath() + children[i]);
                traverse(new File(dir, children[i]));
            }
        }

    }
    
    private void loadImages() {
    	imageAreader.readImage();
    	imageBreader.readImage();
    	
    }
    
    public void callAlgorithm() {
  
    	for (int i = 1; i < instances.size(); i++) {
    		
    		try {
    		
    		imageAreader = instances.get(0);
			imageBreader = instances.get(i);
			
			// step0. validate registered subsection
			imageAreader.validateRegSubSection();
			imageAreader.validateRegSubSection();
			
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
		
    		} catch ( Exception e) {
    			e.printStackTrace();
    		}
		}
    	
    	System.gc();
    }
    
    
    public void printReport() {
    	imageAreader.printReport(outputDir + File.separator + imageAreader.getCaseNumber() + "vs" + imageBreader.getCaseNumber());	
    }
    
    public void doComparison() {
    	imageAreader.originImageThresholdBinary();
    	imageAreader.registeredImageThresholdBinary();   
        imageAreader.statisticsDataGeneration(outputDir + File.separator + imageAreader.getCaseNumber() + "vs" + imageBreader.getCaseNumber());
    }
    
    
    public void saveImages() {
    	imageAreader.saveImages();
    	imageBreader.saveImages();
    	imageAreader.saveRegisteredImages(outputDir + File.separator + imageAreader.getCaseNumber() + "vs" + imageBreader.getCaseNumber());
    	
    }
    
    public void registration() {
    	imageAreader.registration(imageBreader);
    }
    
    public void threshold() {
    	imageAreader.threshold();
    	imageBreader.threshold();
    }

    public void processFromCommandLine() {
    	File fileDir = new File(inputDir);
        System.err.println(inputDir);
        traverse(fileDir);
    }
    
    public void algorithmPerformed(AlgorithmBase algorithm) {
    	imageAreader.algorithmPerformed(algorithm);
    	dispose();
    }

    class asegFilter implements FilenameFilter {
        public boolean accept(File dir, String name) {
            // return (name.endsWith("aseg.mgz"));
        	return name.equals("aseg.mgz");
        }
    }
   
}

