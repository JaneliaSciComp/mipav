import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;
import java.io.IOException;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextArea;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.LineBorder;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.AlgorithmArcLength;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoDicom;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIPoint;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.dialogs.JDialogBase;


public class PlugInDialogWormStraightening extends JDialogBase implements AlgorithmInterface {
	
	
	private ModelImage wormImage;
	
	private File pointsFile;
	
	private JButton wormImageBrowseButton, pointsFileBrowseButton;
	
	private JTextField wormImageTextField, pointsFileTextField, interpolationPtsTextField, diameterTextField;
	
	private JTextArea outputTextArea;

	private JLabel wormImageLabel, pointsFileLabel, interpolationPtsLabel, diameterLabel;
	
	private JCheckBox algorithmCheck;
	
	private JPanel mainPanel;
	
	private JScrollPane scrollPane;
	
	private GridBagConstraints gbc;
	
	private float resx = .16f;
	
	private float resy = .16f;
	
	private float resz = 1.0f;
	
	private float origx = 0.0f;
	
	private float origy = 0.0f;
	
	private float origz = 0.0f;
	
	private float sliceThickness = 0.0f;
	
	private int unitOfMeasure = Unit.MICROMETERS.getLegacyNum();
	
	//private String currDir;
	private String currDir = "C:\\images\\hari\\";
	
    private ViewJFrameImage vjf;
    
    private PlugInAlgorithmWormStraightening alg;
	
	
	
	
	
	
	
	
	public PlugInDialogWormStraightening() {
		
	}
	
	
	
	
	public PlugInDialogWormStraightening(boolean modal) {
		init();
	}
	
	
	
	
	
	
	
	
	public void init() {
		setTitle("Worm Straightening");

		gbc = new GridBagConstraints();
		
		
		mainPanel = new JPanel(new GridBagLayout());
		
		
		wormImageLabel = new JLabel("Worm Image:");
		
		wormImageTextField = new JTextField(55);
		wormImageTextField.setEditable(false);
		wormImageTextField.setBackground(Color.white);
		wormImageTextField.setBorder(new LineBorder(Color.black));
		
		wormImageBrowseButton = new JButton("Browse");
		wormImageBrowseButton.addActionListener(this);
		wormImageBrowseButton.setActionCommand("wormImageBrowse");
		
		pointsFileLabel = new JLabel("Points File:");
		
		pointsFileTextField = new JTextField(55);
		pointsFileTextField.setEditable(false);
		pointsFileTextField.setBackground(Color.white);
		pointsFileTextField.setBorder(new LineBorder(Color.black));
		
		pointsFileBrowseButton = new JButton("Browse");
		pointsFileBrowseButton.addActionListener(this);
		pointsFileBrowseButton.setActionCommand("pointsFileBrowse");
		
		interpolationPtsLabel = new JLabel("Interpolation Points:");
		
		interpolationPtsTextField = new JTextField(5);
		interpolationPtsTextField.setBackground(Color.white);
		interpolationPtsTextField.setBorder(new LineBorder(Color.black));
		
		diameterLabel = new JLabel( "Estimate diameter (voxels): " );
		diameterTextField = new JTextField(5);
		diameterTextField.setBackground(Color.white);
		diameterTextField.setBorder(new LineBorder(Color.black));
		
		
		outputTextArea = new JTextArea(15, 70);
		outputTextArea.setEditable(false);
		outputTextArea.setBackground(Color.lightGray);
		outputTextArea.setBorder(new LineBorder(Color.black));
		outputTextArea.setForeground(Color.black);
		scrollPane = new JScrollPane(outputTextArea, ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
		
		
		gbc.anchor = GridBagConstraints.WEST;
		
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.insets = new Insets(15,5,15,0);
		mainPanel.add(wormImageLabel, gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.insets = new Insets(15,5,15,0);
		mainPanel.add(wormImageTextField, gbc);
		
		gbc.gridx = 2;
		gbc.gridy = 0;
		gbc.insets = new Insets(15,5,15,0);
		mainPanel.add(wormImageBrowseButton, gbc);
		
		
		gbc.gridx = 0;
		gbc.gridy = 1;
		gbc.insets = new Insets(15,5,15,0);
		mainPanel.add(pointsFileLabel, gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 1;
		gbc.insets = new Insets(15,5,15,0);
		mainPanel.add(pointsFileTextField, gbc);
		
		gbc.gridx = 2;
		gbc.gridy = 1;
		gbc.insets = new Insets(15,5,15,0);
		mainPanel.add(pointsFileBrowseButton, gbc);
		
		

		
		gbc.gridx = 0;
		gbc.gridy = 2;
		gbc.insets = new Insets(15,5,15,0);
		mainPanel.add(interpolationPtsLabel, gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 2;
		gbc.insets = new Insets(15,5,15,0);
		mainPanel.add(interpolationPtsTextField, gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 3;
		gbc.insets = new Insets(15,5,15,0);
		mainPanel.add(diameterLabel, gbc);
		
		gbc.gridx = 1;
		gbc.gridy = 3;
		gbc.insets = new Insets(15,5,15,0);
		mainPanel.add(diameterTextField, gbc);
		
		algorithmCheck = new JCheckBox( "Use Automatic Straightening", false );		
		gbc.gridx = 0;
		gbc.gridy = 4;
		gbc.insets = new Insets(15,5,15,0);
		mainPanel.add(algorithmCheck, gbc);
		
		gbc.gridx = 0;
		gbc.gridy = 5;
		gbc.gridwidth = 3;
		gbc.insets = new Insets(15,5,15,0);
		mainPanel.add(scrollPane, gbc);
		
		JPanel OKCancelPanel = new JPanel();
        buildOKButton();
        OKButton.setActionCommand("ok");
        OKCancelPanel.add(OKButton);
        buildCancelButton();
        cancelButton.setActionCommand("cancel");
        OKCancelPanel.add(cancelButton);
		
        getContentPane().add(mainPanel, BorderLayout.CENTER);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
        
        
        
        pack();
        setResizable(false);
        setVisible(true);
		
		
		
	}
	
	
	
	
	private void updateRes() {
		 FileInfoBase[] fileInfo = wormImage.getFileInfo();
		 
		 float[] resolutions = new float[3];
		 float[] origin = new float[3];
		 resolutions[0] = resx;
		 resolutions[1] = resy;
		 resolutions[2] = resz;
		 
		 origin[0] = origx;
		 origin[1] = origy;
		 origin[2] = origz;
		 
		 
		 
         for (int i = 0; i < wormImage.getExtents()[2]; i++) {
             fileInfo[i].setResolutions(resolutions);
             fileInfo[i].setUnitsOfMeasure(unitOfMeasure, 0);
             fileInfo[i].setUnitsOfMeasure(unitOfMeasure, 1);
             fileInfo[i].setUnitsOfMeasure(unitOfMeasure, 2);
             fileInfo[i].setSliceThickness(sliceThickness);
             
             
             
             
             
             
             
             
             fileInfo[i].setOrigin(origin);

             int axisOrient = fileInfo[i].getAxisOrientation(2);

             if ( (axisOrient == FileInfoBase.ORI_R2L_TYPE) || (axisOrient == FileInfoBase.ORI_P2A_TYPE)
                     || (axisOrient == FileInfoBase.ORI_I2S_TYPE) || (axisOrient == FileInfoBase.ORI_UNKNOWN_TYPE)) {
                 origin[2] += resolutions[2];
             } else { // ORI_L2R_TYPE, ORI_A2P_TYPE, ORI_S2I_TYPE
                 origin[2] -= resolutions[2];
             }
             
             
             
         }
	}
	
	protected void callAlgorithm() {
		int interpolationPts = Integer.parseInt(interpolationPtsTextField.getText().trim());
		int diameter = Integer.parseInt(diameterTextField.getText().trim());
		
		if ( algorithmCheck.isSelected() )
		{
			alg = new PlugInAlgorithmWormStraighteningAutomatic(wormImage,interpolationPts, diameter);

			alg.addListener(this);


			if (isRunInSeparateThread()) {

				// Start the thread as a low priority because we wish to still
				// have user interface work fast.
				if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
					MipavUtil.displayError("A thread is already running on this object");
				}
			} else {
				alg.run();
			}
		}
		else
		{
			alg = new PlugInAlgorithmWormStraightening(wormImage,interpolationPts);

			alg.addListener(this);


			if (isRunInSeparateThread()) {

				// Start the thread as a low priority because we wish to still
				// have user interface work fast.
				if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
					MipavUtil.displayError("A thread is already running on this object");
				}
			} else {
				alg.run();
			}
		}


		
	}
	
	
	
	
	
	public void algorithmPerformed(AlgorithmBase algorithm) {
		if(alg.isCompleted()) {
			System.out.println("done");
			
			
		}

	}

	
	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		
		if (command.equalsIgnoreCase("wormImageBrowse")) {
            JFileChooser chooser = new JFileChooser();
            chooser.setDialogTitle("Choose image");
            if (currDir != null) {
				chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                currDir = chooser.getSelectedFile().getAbsolutePath();
                FileIO fileIO = new FileIO();
                if(wormImage != null) {
                	wormImage.disposeLocal();
                	wormImage = null;
                }
                wormImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory() + File.separator, false, null);
                vjf = new ViewJFrameImage(wormImage);
                
                wormImageTextField.setText(currDir);
                //updateRes();
                
                wormImage.getParentFrame().initResolutions();
                wormImage.getParentFrame().updateImageExtents();
                wormImage.getParentFrame().componentResized(null);

                int minExtent = Math.min( wormImage.getExtents()[0], wormImage.getExtents()[1]);
                if ( wormImage.getExtents().length > 2 )
                {
                	minExtent = Math.min( minExtent, wormImage.getExtents()[2]);
                }
                diameterTextField.setText(String.valueOf(minExtent));
            }
        }else if(command.equals("pointsFileBrowse")) {
        	JFileChooser chooser = new JFileChooser();
            chooser.setDialogTitle("Choose points file");
            if (!currDir.equals("")) {
				chooser.setCurrentDirectory(new File(currDir));
            }
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            chooser.addChoosableFileFilter(new ViewImageFileFilter(new String[] { ".xml", ".voi" }));
            int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
                String fileName = chooser.getSelectedFile().getName();
                String directory = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;
                currDir = chooser.getSelectedFile().getAbsolutePath();
                try {
                	FileVOI fileVOI = new FileVOI(fileName, directory, wormImage);
                	
                	VOI[] voi = fileVOI.readVOI(false);
                	
                	for (int i = 0; i < voi.length; i++) {
                		wormImage.registerVOI(voi[i]);
                    }

                	wormImage.notifyImageDisplayListeners();
                	
                	pointsFileTextField.setText(currDir);
                	
                	
                }catch(IOException ex) {
                	return;
                }
                
                
                
               //need to set default # of interpolation points
               VOIVector VOIs = wormImage.getVOIs();
               Vector<VOIBase> contours = VOIs.VOIAt(0).getCurves();                                           
               int nPoints = contours.size();
               int defaultPts = 100; // 10 * (nPoints - 1)
               interpolationPtsTextField.setText(String.valueOf(defaultPts));
            }
        }else if(command.equals("ok")) {
        	callAlgorithm();
        	
        } else {
            super.actionPerformed(e);
        }

	}

}
