package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.io.*;

import javax.swing.*;

import gov.nih.mipav.view.renderer.WildMagic.BallPivoting.*;
import gov.nih.mipav.view.renderer.WildMagic.Poisson.*;

/**
 * 
 * @version 09 Jan, 2009
 * @author Ruida Cheng
 */
public class JDialogSurfaceReconstruction extends JDialogBase {

   
   
    /**
     * Button panel to hold the OK button, Cancel button, and Help button.
     */
    public JPanel buttonPanel;

    /**
     * Msg box to show the Prostate surface analysis related info.
     */
    public JPanel msgPanel;

    private JPanel BallPivotingPanel;
    
    private JPanel PoissonPanel;
    
    private JPanel plyInputFilePanel;
    
    private JPanel plyOutputFilePanel;
    
    private JTextField textFieldBallRadius;
    private JTextField textFieldClusteringRadius;
    private JTextField textFieldAngleThreshold;

    private JTextField textOctreeDepth;
    private JTextField textSolverDivide;
    private JTextField textSamplePerNode;
    
    private JTextField textFieldInputFile;
    
    private JTextField textFieldOutputFile;

    private JButton buttonInputFile;
    private JButton buttonOutputFile;
    
    // File chooser for selecting the .ply surface file and the ply file.
    private JFileChooser chooser = new JFileChooser();

    // xml, ply file name.
    private String fileNameInput;

    // xml, ply file directory
    private String directoryInput;

    // .xml or .ply filtering in file chooser
    private String file_suffix = ".ply";
    
    // default user interface.
    private ViewUserInterface UI;
    
    // xml, ply file name.
    private String fileNameOutput;

    // xml, ply file directory
    private String directoryOutput;

    
    
    
    /**
     * Constructor for Merging the 3 VOIs and save into one cloudy points file.
     * 
     * @param theParentFrame
     */
    public JDialogSurfaceReconstruction(Frame theParentFrame) {
        super(theParentFrame, false);
        UI = ViewUserInterface.getReference();
        init();
    }

    /**
     * handler the button click evens.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("ChooseInputFile")) {
        	selectFileInput();
        } else if (command.equals("ChooseOutputFile")) {
        	selectFileOutput();
        } else if (command.equals("Cancel")) {
            dispose();
        } else if (command.equals("Help")) {

        } else if ( command.equals("Save") ) {
            processAlgorithm();	
        }

    }

    public void processAlgorithm() {
    	// Run the Ball-pivoting algorithm
    	float Radius = Float.parseFloat(textFieldBallRadius.getText());
    	float Clustering = Integer.parseInt(textFieldClusteringRadius.getText());
    	float CreaseThr = ToRad(Integer.parseInt(textFieldAngleThreshold.getText()));	
    	
    	boolean DeleteFaces = true; //  = par.getBool("DeleteFaces");
		MeshModel m = new MeshModel();
		
		PlyReader reader = new PlyReader();
	    
		reader.readPlyAsciiMesh(m.cm, directoryInput, fileNameInput);
		
	    if(DeleteFaces) {
			m.cm.fn=0;
			m.cm.face.setSize(0);
	    }
    	
	    m.cm.initFaceIMark();
		m.cm.initVertexIMark();
		// int startingFn=m.cm.fn;			
		Clustering /= 100.0f;
		BallPivoting pivot = new BallPivoting(m.cm, Radius, Clustering, CreaseThr); 
	    // the main processing
		double tt=System.currentTimeMillis();
		System.err.println("BuildMesh");
		
	    pivot.buildMesh();
	    System.err.println("#              Total Time: " + ((double)(System.currentTimeMillis()-tt) / 1000.0));
		
	    System.err.println("finish");
	    m.clearDataMask(MeshModel.MM_FACEFACETOPO | MeshModel.MM_FACEFLAGBORDER);
	    System.err.println("m.cm.fn = " + m.cm.fn);
	    
	    PlyWriter writer = new PlyWriter();
	    writer.writePlyAsciiMesh(m.cm, directoryInput, "bpt_output.ply");
	    
	    // Run Poisson Algorithm
	    PoissonRun poisson = new PoissonRun();
	    String inputFileName = new String(directoryInput + File.separator + "bpt_output.ply");
	    String outputFileName = new String(directoryOutput + File.separator + fileNameOutput);
	    
	    int octreeDepth = Integer.parseInt(textOctreeDepth.getText());
    	int solverDivide = Integer.parseInt(textSolverDivide.getText());
    	int samplePerNode = Integer.parseInt(textSamplePerNode.getText());	
    	
	    poisson.Execute(inputFileName, outputFileName, octreeDepth, solverDivide,samplePerNode, 2);
	    dispose();
    }
    
    public float ToRad(float a){return (float)(Math.PI)*a/180.0f;}
    
    /**
     * Select input .ply file.
     */
    public void selectFileInput() {
        chooser.setDialogTitle("Open .ply cloudy points file");

        if (UI.getDefaultDirectory() != null) {
            File file = new File(UI.getDefaultDirectory());

            if (file != null) {
                chooser.setCurrentDirectory(file);
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }
        } else {
            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }

        chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] {file_suffix}));

        int returnValue = chooser.showOpenDialog(UI.getMainFrame());

        if (returnValue == JFileChooser.APPROVE_OPTION) {
            fileNameInput = chooser.getSelectedFile().getName();
            directoryInput = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            // UI.setDefaultDirectory(directory);
            textFieldInputFile.setText(fileNameInput);
        } else {
            return;
        }

    }

    /**
     * Select input .ply file.
     */
    public void selectFileOutput() {
        chooser.setDialogTitle("Save .ply output file");

        if (UI.getDefaultDirectory() != null) {
            File file = new File(UI.getDefaultDirectory());

            if (file != null) {
                chooser.setCurrentDirectory(file);
            } else {
                chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
            }
        } else {
            chooser.setCurrentDirectory(new File(System.getProperty("user.dir")));
        }

        chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] {file_suffix}));

        int returnValue = chooser.showOpenDialog(UI.getMainFrame());

        if (returnValue == JFileChooser.APPROVE_OPTION) {
            fileNameOutput = chooser.getSelectedFile().getName();
            directoryOutput = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
            // UI.setDefaultDirectory(directory);
            textFieldOutputFile.setText(fileNameOutput);
        } else {
            return;
        }

    }

    
    /**
     * Write the cloud points from the 3 VOIs into one .ply file. The .ply file is readable for MeshLab.
     */
    public void writePlyFile() {
        /*
    	int i;
        FileWriter fwp;
        File filePly = null;
        PrintWriter plyFileWriter;
        int ptSize = AxialVOIs.myContourVector.size() + SagittalVOIs.myContourVector.size()
                + CoronalVOIs.myContourVector.size();

        try {
            filePly = new File(PlyInstance.directory + PlyInstance.fileName);

            fwp = new FileWriter(filePly);
            plyFileWriter = new PrintWriter(fwp);
            // write header
            plyFileWriter.println("ply"); // object is ModelTriangleMesh
            plyFileWriter.println("format ascii 1.0");
            plyFileWriter.println("element vertex " + ptSize);
            plyFileWriter.println("property float32 x");
            plyFileWriter.println("property float32 y");
            plyFileWriter.println("property float32 z");
            plyFileWriter.println("element face " + 0);
            plyFileWriter.println("property list uint8 int32 vertex_indices");
            plyFileWriter.println("end_header");

            Vector3f v;
            for (i = 0; i < AxialVOIs.myContourVector.size(); i++) {
                v = (Vector3f) AxialVOIs.myContourVector.get(i);
                plyFileWriter.print(v.X);
                plyFileWriter.print(" ");
                plyFileWriter.print(v.Y);
                plyFileWriter.print(" ");
                plyFileWriter.println(v.Z);
            }
          
            plyFileWriter.close();
            dispose();
        } catch (Exception e) {
            System.err.println("CAUGHT EXCEPTION WITHIN writeXML() of FileVOI");
            e.printStackTrace();
        }
         */
    }

    /**
     * Sets up GUI and displays the dialog.
     */
    private void init() {
        setTitle("Surface Reconstruction");
        //setResizable(false);
        cancelFlag = false;

        JPanel mainPanel = new JPanel();
        mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

        GridBagConstraints gbc = new GridBagConstraints();

        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.EAST;
        
        /************  Ply input file panel **********************/
        plyInputFilePanel = new JPanel();
        plyInputFilePanel.setLayout(new GridLayout(1, 2));
        plyInputFilePanel.setBorder(buildTitledBorder("Input File (.ply)"));
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        textFieldInputFile = new JTextField("", 3);
        textFieldInputFile.setFont(serif12);
        
        plyInputFilePanel.add(textFieldInputFile, gbc);
        
        buttonInputFile = new JButton("Choose");
        buttonInputFile.addActionListener(this);
        buttonInputFile.setActionCommand("ChooseInputFile");
        buttonInputFile.setFont(serif12B);
        buttonInputFile.setPreferredSize(MipavUtil.defaultButtonSize);
        gbc.gridx = 1;
        plyInputFilePanel.add(buttonInputFile, gbc);
        
        /************  Ply Output file panel **********************/
        plyOutputFilePanel = new JPanel();
        plyOutputFilePanel.setLayout(new GridLayout(1, 2));
        plyOutputFilePanel.setBorder(buildTitledBorder("Output File (.ply)"));
        
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        textFieldOutputFile = new JTextField("", 3);
        textFieldOutputFile.setFont(serif12);
        
        plyOutputFilePanel.add(textFieldOutputFile, gbc);
        
        buttonOutputFile = new JButton("Choose");
        buttonOutputFile.addActionListener(this);
        buttonOutputFile.setActionCommand("ChooseOutputFile");
        buttonOutputFile.setFont(serif12B);
        buttonInputFile.setPreferredSize(MipavUtil.defaultButtonSize);
        gbc.gridx = 1;
        plyOutputFilePanel.add(buttonOutputFile, gbc);
        
        
        /************   Ball Pivoting Panel **********************/
        BallPivotingPanel = new JPanel();
        BallPivotingPanel.setLayout(new GridLayout(3, 2));
        BallPivotingPanel.setBorder(buildTitledBorder("Ball Pivoting"));

        gbc.gridx = 0;
        gbc.gridy = 0;
        
        JLabel BallRadiusLabel = new JLabel("Ball Radius");
        BallRadiusLabel.setFont(serif12);
        BallRadiusLabel.setForeground(Color.black);

        BallPivotingPanel.add(BallRadiusLabel, gbc);

        textFieldBallRadius = new JTextField(Float.toString(4.82f), 3);
        textFieldBallRadius.setFont(serif12);

        gbc.gridx = 1;
        BallPivotingPanel.add(textFieldBallRadius, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 1;
        JLabel clusteringRadiusLabel = new JLabel("Clustering Radius");
        clusteringRadiusLabel.setFont(serif12);
        clusteringRadiusLabel.setForeground(Color.black);
         
        BallPivotingPanel.add(clusteringRadiusLabel, gbc);

        textFieldClusteringRadius = new JTextField(Integer.toString(10), 3);
        textFieldClusteringRadius.setFont(serif12);

        gbc.gridx = 1;
        BallPivotingPanel.add(textFieldClusteringRadius, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 2;
        JLabel angleThreshold = new JLabel("Angle Threshold(degrees)");
        angleThreshold.setFont(serif12);
        angleThreshold.setForeground(Color.black);
         
        BallPivotingPanel.add(angleThreshold, gbc);

        textFieldAngleThreshold = new JTextField(Integer.toString(90), 3);
        textFieldAngleThreshold.setFont(serif12);

        gbc.gridx = 1;
        BallPivotingPanel.add(textFieldAngleThreshold, gbc);
        
        /********   Poisson Panel *********************/
        PoissonPanel = new JPanel();
        PoissonPanel.setLayout(new GridLayout(3, 2));
        PoissonPanel.setBorder(buildTitledBorder("Poisson"));

        gbc.gridx = 0;
        gbc.gridy = 0;
        
        JLabel octreeDepthLabel = new JLabel("Octree Depth");
        octreeDepthLabel.setFont(serif12);
        octreeDepthLabel.setForeground(Color.black);

        PoissonPanel.add(octreeDepthLabel, gbc);

        textOctreeDepth = new JTextField(Integer.toString(8), 3);
        textOctreeDepth.setFont(serif12);

        gbc.gridx = 1;
        PoissonPanel.add(textOctreeDepth, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 1;
        
        JLabel solverDivideLabel = new JLabel("Solver Divide");
        solverDivideLabel.setFont(serif12);
        solverDivideLabel.setForeground(Color.black);

        PoissonPanel.add(solverDivideLabel, gbc);

        textSolverDivide = new JTextField(Integer.toString(8), 3);
        textSolverDivide.setFont(serif12);

        gbc.gridx = 1;
        PoissonPanel.add(textSolverDivide, gbc);
        
        gbc.gridx = 0;
        gbc.gridy = 2;
        
        JLabel samplePerNodeLabel = new JLabel("Sample per Node");
        samplePerNodeLabel.setFont(serif12);
        samplePerNodeLabel.setForeground(Color.black);

        PoissonPanel.add(samplePerNodeLabel, gbc);

        textSamplePerNode = new JTextField(Integer.toString(1), 3);
        textSamplePerNode.setFont(serif12);

        gbc.gridx = 1;
        PoissonPanel.add(textSamplePerNode, gbc);

        // button Panel
        buttonPanel = new JPanel();
        buttonPanel.setLayout(new GridLayout(1, 2));
        gbc.gridx = 0;
        gbc.gridy = 0;
        buttonPanel.add(buildOKButton(), gbc);
        gbc.gridx = 1;
        buttonPanel.add(buildCancelButton(), gbc);
        
        mainPanel.add(plyInputFilePanel);
        mainPanel.add(plyOutputFilePanel);
        mainPanel.add(BallPivotingPanel);
        mainPanel.add(PoissonPanel);
        mainPanel.add(buttonPanel);
        
        
        getContentPane().add(mainPanel);
        pack();
        setVisible(true);

    }

    /**
     * Builds button panel consisting of OK, Cancel and Help buttons.
     * 
     * @return JPanel that has ok, cancel, and help buttons
     */
    protected JPanel buildButtons() {
        JPanel buttonPanel = new JPanel();

        buttonPanel.add(buildOKButton());
        buttonPanel.add(buildCancelButton());
        buttonPanel.add(buildHelpButton());

        return buttonPanel;
    }

    /**
     * Builds the OK button. Sets it internally as well return the just-built button.
     * 
     * @return JButton ok button
     */
    protected JButton buildOKButton() {
        OKButton = new JButton("Save");
        OKButton.addActionListener(this);
        OKButton.setActionCommand("Save");

        // OKButton.setToolTipText("Accept values and perform action.");
        OKButton.setMinimumSize(MipavUtil.defaultButtonSize);
        OKButton.setPreferredSize(MipavUtil.defaultButtonSize);
        OKButton.setFont(serif12B);

        return OKButton;
    }

    /**
     * Builds the cancel button. Sets it internally as well return the just-built button.
     * 
     * @return JButton cancel button
     */
    protected JButton buildCancelButton() {
        cancelButton = new JButton("Cancel");
        cancelButton.addActionListener(this);
        cancelButton.setActionCommand("Cancel");

        cancelButton.setMinimumSize(MipavUtil.defaultButtonSize);
        cancelButton.setPreferredSize(MipavUtil.defaultButtonSize);
        cancelButton.setFont(serif12B);

        return cancelButton;
    }

    /**
     * Builds the help button. Sets it internally as well return the just-built button.
     * 
     * @return JButton help button
     */
    protected JButton buildHelpButton() {
        helpButton = new JButton("Help");
        helpButton.addActionListener(this);
        helpButton.setActionCommand("Help");
        helpButton.setToolTipText("Find help for this screen.");
        helpButton.setMinimumSize(MipavUtil.defaultButtonSize);
        helpButton.setPreferredSize(MipavUtil.defaultButtonSize);
        helpButton.setFont(serif12B);

        return helpButton;
    }

}


