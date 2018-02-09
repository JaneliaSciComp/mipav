import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.io.File;
import java.util.Iterator;
import java.util.LinkedHashMap;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoNIFTI;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.MatrixHolder;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewFileChooserBase;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;


public class PlugInDialogTriPlanarVolumesCreator extends JDialogStandaloneScriptablePlugin implements AlgorithmInterface {


	/** grid bag constraints * */
    private GridBagConstraints gbc;

    /** main panel * */
    private JPanel mainPanel, OKCancelPanel;
    
    /** src image * */
    private ModelImage srcImage;
    
    /** output directory * */
    private String outputDir;
    
    /** output name stub * */
    private String outputNamePrefix;
    
    /** current directory * */
    private String currDir;
    
    /** label **/
    private  JLabel srcImageLabel, outputDirLabel, outputNamePrefixLabel, outputFormatLabel;
    
    /** textfields * */
    private JTextField srcImageFilePathTextField, outputDirPathTextField, outputNamePrefixTextField;
    
    /** browse button * */
    private JButton srcImageBrowseButton, outputDirBrowseButton;
    
    /** save as JP2000 radio **/
    private JRadioButton saveAsJP2Radio;
    
    /** save as jpeg radio **/
    private JRadioButton saveAsJPEGRadio;
    
    /** save as png radio **/
    private JRadioButton saveAsPNGRadio;
    
    /** button group for radio buttons **/
    private ButtonGroup optionsGroup;
    
    /** src image file info **/
   /* private FileInfoBase fileInfo;
    private FileInfoNIFTI   fileInfoNIFTI;*/
    
   /* private AlgorithmTransform algoTrans,algoResample;
    
    private int orientationIndex = 0;*/
    
    private boolean launchedFromGUI = false;
    
    private PlugInAlgorithmTriPlanarVolumesCreator alg;

    
   
	////////////////////////////////////////////
   /* private     int[]       or = new int[3];
    private     int[]       newOr = new int[3];
    private     int[]       coronalNewOr = {FileInfoBase.ORI_R2L_TYPE,FileInfoBase.ORI_S2I_TYPE,FileInfoBase.ORI_A2P_TYPE};
    private     int[]       axialNewOr = {FileInfoBase.ORI_R2L_TYPE,FileInfoBase.ORI_A2P_TYPE,FileInfoBase.ORI_I2S_TYPE};
    private     int[]       sagittalNewOr = {FileInfoBase.ORI_A2P_TYPE,FileInfoBase.ORI_S2I_TYPE,FileInfoBase.ORI_R2L_TYPE};
    
    private     int[]       axisOrder = new int[3];
    private     boolean[]   axisFlip = new boolean[3];*/
	
	
    public PlugInDialogTriPlanarVolumesCreator(boolean modal) {
    	super(modal);
    	init();
    }
    
    
    public PlugInDialogTriPlanarVolumesCreator() {
    	init();
    }
    
    
    public void init() {
    	 setForeground(Color.black);
         setTitle("Tri-Planar Volumes Creator");
         mainPanel = new JPanel(new GridBagLayout());
         gbc = new GridBagConstraints();
         
         
         srcImageLabel = new JLabel("Image");
         srcImageFilePathTextField = new JTextField(55);
         srcImageFilePathTextField.setEditable(false);
         srcImageFilePathTextField.setBackground(Color.white);
         srcImageBrowseButton = new JButton("Browse");
         srcImageBrowseButton.addActionListener(this);
         srcImageBrowseButton.setActionCommand("srcImageBrowse");
         
         
         outputDirLabel = new JLabel("Output Directory");
         outputDirPathTextField = new JTextField(55);
         outputDirPathTextField.setEditable(false);
         outputDirPathTextField.setBackground(Color.white);
         outputDirBrowseButton = new JButton("Browse");
         outputDirBrowseButton.addActionListener(this);
         outputDirBrowseButton.setActionCommand("outputDirBrowse");
         
         outputNamePrefixLabel = new JLabel("Output Image File Name Prefix");
         outputNamePrefixTextField = new JTextField(55);
         outputNamePrefixTextField.setBackground(Color.white);
         
         outputFormatLabel = new JLabel("Save Volumes In Following Format");
         optionsGroup = new ButtonGroup();
         
         saveAsJP2Radio = new JRadioButton("Lossless JPEG 2000");
         saveAsJP2Radio.setSelected(true);
         optionsGroup.add(saveAsJP2Radio);
         
         saveAsJPEGRadio = new JRadioButton("Lossy JPEG");
         saveAsJPEGRadio.setSelected(false);
         optionsGroup.add(saveAsJPEGRadio);
         
         saveAsPNGRadio = new JRadioButton("PNG");
         saveAsPNGRadio.setSelected(false);
         optionsGroup.add(saveAsPNGRadio);
          
         gbc.gridx = 0;
         gbc.gridy = 0;
         gbc.insets = new Insets(15, 5, 5, 15);
         gbc.gridwidth = 1;
         gbc.anchor = GridBagConstraints.EAST;
         mainPanel.add(srcImageLabel, gbc);
         gbc.gridx = 1;
         mainPanel.add(srcImageFilePathTextField, gbc);
         gbc.gridx = 2;
         mainPanel.add(srcImageBrowseButton, gbc);
         gbc.gridy = 1;
         gbc.gridx = 0;
         mainPanel.add(outputDirLabel, gbc);
         gbc.gridx = 1;
         mainPanel.add(outputDirPathTextField, gbc);
         gbc.gridx = 2;
         mainPanel.add(outputDirBrowseButton, gbc);
         gbc.gridy = 2;
         gbc.gridx = 0;
         mainPanel.add(outputNamePrefixLabel, gbc);
         gbc.gridx = 1;
         mainPanel.add(outputNamePrefixTextField, gbc);
         gbc.gridy = 3;
         gbc.gridx = 0;
         mainPanel.add(outputFormatLabel, gbc);
         gbc.anchor = GridBagConstraints.WEST;
         //gbc.gridx = 1;
         //mainPanel.add(saveAsJP2Radio, gbc);
         gbc.gridy = 4;
         gbc.gridx = 1;
         mainPanel.add(saveAsJPEGRadio, gbc);
         /*gbc.gridy = 5;
         gbc.gridx = 1;
         mainPanel.add(saveAsPNGRadio, gbc);*/
         
         OKCancelPanel = new JPanel();
         buildOKButton();
         OKButton.setActionCommand("ok");
         OKCancelPanel.add(OKButton, BorderLayout.WEST);
         buildCancelButton();
         cancelButton.setActionCommand("cancel");
         OKCancelPanel.add(cancelButton, BorderLayout.EAST);
         
         getContentPane().add(mainPanel, BorderLayout.CENTER);
         getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);
         setResizable(false);
         pack();
         if(ViewUserInterface.getReference().isAppFrameVisible()) {
        	 setVisible(true);
         }
         
         
    	
    }
    
    
    
    public void actionPerformed(final ActionEvent e) {
    	String command = e.getActionCommand();
    	if (command.equalsIgnoreCase("srcImageBrowse")) {
    		
    		final ViewFileChooserBase fileChooser = new ViewFileChooserBase(true, false);
            fileChooser.setMulti(true);

            final JFileChooser chooser = fileChooser.getFileChooser();
            chooser.setCurrentDirectory(new File(ViewUserInterface.getReference().getDefaultDirectory()));
    		
    		
    		
    		
    		
    		/*JFileChooser chooser = new JFileChooser();
    		if (currDir != null) {
                chooser.setCurrentDirectory(new File(currDir));
    		 }*/
            chooser.setDialogTitle("Choose Source Image");
            chooser.setFileSelectionMode(JFileChooser.FILES_ONLY);
            int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
            	currDir = chooser.getSelectedFile().getAbsolutePath();
            	//ViewFileOpenUI

            	FileIO fileIO = new FileIO();
            	 srcImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory() + File.separator, true, null);
            	 srcImageFilePathTextField.setText(currDir);
            	 launchedFromGUI = true;
            }
           
    	} else if (command.equalsIgnoreCase("outputDirBrowse")) {
    		JFileChooser chooser = new JFileChooser();
    		if (currDir != null) {
                chooser.setCurrentDirectory(new File(currDir));
    		}
            chooser.setDialogTitle("Choose Output Directory");
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
            	currDir = chooser.getSelectedFile().getAbsolutePath();
            	outputDirPathTextField.setText(currDir);
            	outputDir = currDir;
            }
            
    	} else if (command.equalsIgnoreCase("ok")) {
            if ( !validateInput()) {
                return;
            } else {
                callAlgorithm();
            }

        }
    }
    
	
    
    /**
     * validate vars
     * 
     * @return success
     */
    private boolean validateInput() {
    	 boolean valid = true;
    	 if (outputNamePrefixTextField.getText().trim().equals("")) {
             MipavUtil.displayError("Output Image File Name Prefix must be entered");
             valid = false;
             return valid;
         }else {
        	 outputNamePrefix = outputNamePrefixTextField.getText().trim();
         }
    	 
    	 if (outputDirPathTextField.getText().trim().equals("")) {
             MipavUtil.displayError("Output Directory must be entered");
             valid = false;
             return valid;
         }
    	 
    	 return valid;
    }
    
    
    
    protected void callAlgorithm() {
    	alg = new PlugInAlgorithmTriPlanarVolumesCreator(srcImage,outputDir,outputNamePrefix,launchedFromGUI);
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
    
    
    
    public void algorithmPerformed(AlgorithmBase algorithm) {
    	if(alg.isCompleted()) {
    		insertScriptLine();
    		
    		alg.finalize();
    		alg = null;
    		
    		
    		System.out.println("plugin complete");
    		this.setComplete(true); 
    		if (isExitRequired()) {
                System.exit(0);
                // ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
            } else {
                dispose();
            }
    		
    	}
    }
    
    
    /*protected void callAlgorithmOLD() {
    	System.out.println("TriPlanarVolumesCreator...");
    	
    	//get axis orientations...if any are unknown, exit and display error
    	System.out.println("Determining axes orientations...");
    	for (int i = 0; i <=2; i++) {
            or[i] = srcImage.getFileInfo()[0].getAxisOrientation()[i];
            if(or[i] == FileInfoBase.ORI_UNKNOWN_TYPE) {
            	MipavUtil.displayError("One or more axis orientations is of unknown type");
            	return;
            }
        }
    	

    	//resample image to resolution of 1,1,1 becasue output png and jp2 images dont store resoltions
    	System.out.println("Resampling image to resolution of 1.0f, 1.0f, 1.0f...");
    	float cXres = srcImage.getFileInfo()[0].getResolutions()[0];
    	float cYres = srcImage.getFileInfo()[0].getResolutions()[1];
    	float cZres = srcImage.getFileInfo()[0].getResolutions()[2];
    	
    	int cXdim = srcImage.getExtents()[0];
    	int cYdim = srcImage.getExtents()[1];
    	int cZdim = srcImage.getExtents()[2];
    	
    	float oXres = 1.0f;
    	float oYres = 1.0f;
    	float oZres = 1.0f;
    	
    	int constantFOV = 1;
    	
    	int[] iDims = new int[3];

    	float fovX = (cXdim - constantFOV) * cXres;
    	float fovY = (cYdim - constantFOV) * cYres;
    	float fovZ = (cZdim - constantFOV) * cZres;
    	
    	iDims[0] = Math.round(fovX / oXres + constantFOV);
    	iDims[1] = Math.round(fovY / oYres + constantFOV);
    	iDims[2] = Math.round(fovZ / oZres + constantFOV);
    	
    	
    	TransMatrix transMat = new TransMatrix(4);
    	TransMatrix xfrm;
    	transMat.identity();
        xfrm = transMat;
        
        int interp = AlgorithmTransform.TRILINEAR;
        
        boolean doVOI = false;
        boolean doClip = true;
        boolean doPad = false;
        boolean doRotateCenter = false;
        Vector3f center = null;
        
        float fillValue = (float)srcImage.getMin();
        boolean doUpdateOrigin = true;
        boolean isSATransform = false;
        
        int[] units = new int[srcImage.getUnitsOfMeasure().length];
        for (int i = 0; i < units.length; i++) {
            units[i] = srcImage.getUnitsOfMeasure(i);
        }
    	
        
        algoResample = new AlgorithmTransform(srcImage, xfrm, interp, oXres, oYres, oZres, iDims[0], iDims[1], iDims[2], units,
                doVOI, doClip, doPad, doRotateCenter, center);
        algoResample.setFillValue(fillValue);
        algoResample.setUpdateOriginFlag(doUpdateOrigin);
        algoResample.setUseScannerAnatomical(isSATransform);
        
        algoResample.run();
        
		
        transformedImage = 	algoResample.getTransformedImage();
        transformedImage.calcMinMax();
        
        if (algoResample != null) {
        	algoResample.finalize();
        	algoResample = null;
        }
        
        

    	
    	//for each orientation, call th reorientation algorithm to create new volum
        System.out.println("Calling algorithm to reorient image for each orienation ...");
    	for(orientationIndex=0;orientationIndex<3;orientationIndex++) {
    		if (algoTrans != null) {
	        	algoTrans.finalize();
	        	algoTrans = null;
	        }
    		
    		//set up the newOr array
    		if(orientationIndex == 0) {
    			//lets do coronal first
    			System.out.println("reorienting for coronal...");
    			for(int w=0;w<coronalNewOr.length;w++) {
    				newOr[w] = coronalNewOr[w];
    			}	
    		}else if(orientationIndex == 1) {
    			//now do axial
    			System.out.println("reorienting for axial...");
    			for(int w=0;w<axialNewOr.length;w++) {
    				newOr[w] = axialNewOr[w];
    			}
    		}else if(orientationIndex == 2) {
    			//now do sagittal
    			System.out.println("reorienting for sagittal...");
    			for(int w=0;w<sagittalNewOr.length;w++) {
    				newOr[w] = sagittalNewOr[w];
    			}
    		}

    		int i, j;
            boolean found;
            int newOrient;
            float ri[] = new float[3];
            int   ni[] = new int[3];
            float r0[] = new float[3];
            int   n0[] = new int[3];
            float org[] = new float[3];
    		
    		float origin[];
    		float newOrigin[];
    		float originalOr[] = new float[3];
    		float flippedOr[] = new float[3];
    		float xOr = 0.0f;
    	    float yOr = 0.0f;;
    	    float zOr = 0.0f;
    	    Vector3f position;
    	    Vector3f out;

    	    if (transformedImage.getFileInfo(0) instanceof FileInfoNIFTI) {
    	    	fileInfoNIFTI = (FileInfoNIFTI)(transformedImage.getFileInfo()[0].clone());
    	    }
    	    else {
    	        fileInfo = (FileInfoBase)(transformedImage.getFileInfo()[0].clone());
    	    }
       	
       	// set resampled resolutions, dimensions
   		ri[0] = transformedImage.getFileInfo()[0].getResolutions()[0];
   		ri[1] = transformedImage.getFileInfo()[0].getResolutions()[1];
   		ri[2] = transformedImage.getFileInfo()[0].getResolutions()[2];
   		
   		ni[0] = transformedImage.getExtents()[0];
   		ni[1] = transformedImage.getExtents()[1];
   		ni[2] = transformedImage.getExtents()[2];
   		
   		origin = transformedImage.getFileInfo()[0].getOrigin();
   		newOrigin = origin.clone();
       	
   		float r[] = new float[3];
   	    int   n[] = new int[3];
           for (i = 0; i <= 2; i++) {
               r[i] = ri[i];
               n[i] = ni[i];
           }

           double X[][] = new double[4][4];
           for (j = 0; j <= 2; j++) {
               switch (or[j]) {
                   case FileInfoBase.ORI_R2L_TYPE:
                       found = false;
                       for (i = 0; (i <= 2) && (!found); i++) {
                           if (newOr[i] == FileInfoBase.ORI_R2L_TYPE) {
                           	axisOrder[i] = j;
                           	axisFlip[i] = false;
                               found = true;
                               X[i][j] = 1.0;
                               r0[i] = r[j];
                               n0[i] = n[j];
                           }
                           else if (newOr[i] == FileInfoBase.ORI_L2R_TYPE) {
                           	axisOrder[i] = j;
                           	axisFlip[i] = true;
                               found = true;
                               X[i][j] = -1.0;
                               X[i][3] = ri[j]*(ni[j] - 1);
                               r0[i] = r[j];
                               n0[i] = n[j];
                           }
                       }
                       break;
                   case FileInfoBase.ORI_L2R_TYPE:
                       found = false;
                       for (i = 0; (i <= 2) && (!found); i++) {
                           if (newOr[i] == FileInfoBase.ORI_L2R_TYPE) {
                           	axisOrder[i] = j;
                           	axisFlip[i] = false;
                               found = true;
                               X[i][j] = 1.0;
                               r0[i] = r[j];
                               n0[i] = n[j];
                           }
                           else if (newOr[i] == FileInfoBase.ORI_R2L_TYPE) {
                           	axisOrder[i] = j;
                           	axisFlip[i] = true;
                               found = true;
                               X[i][j] = -1.0;
                               X[i][3] = ri[j]*(ni[j] - 1);
                               r0[i] = r[j];
                               n0[i] = n[j];
                           }
                       }
                       break;
                   case FileInfoBase.ORI_A2P_TYPE:
                       found = false;
                       for (i = 0; (i <= 2) && (!found); i++) {
                           if (newOr[i] == FileInfoBase.ORI_A2P_TYPE) {
                           	axisOrder[i] = j;
                           	axisFlip[i] = false;
                               found = true;
                               X[i][j] = 1.0;
                               r0[i] = r[j];
                               n0[i] = n[j];
                           }
                           else if (newOr[i] == FileInfoBase.ORI_P2A_TYPE) {
                           	axisOrder[i] = j;
                           	axisFlip[i] = true;
                               found = true;
                               X[i][j] = -1.0;
                               X[i][3] = ri[j]*(ni[j] - 1);
                               r0[i] = r[j];
                               n0[i] = n[j];
                           }
                       }
                       break;
                   case FileInfoBase.ORI_P2A_TYPE:
                       found = false;
                       for (i = 0; (i <= 2) && (!found); i++) {
                           if (newOr[i] == FileInfoBase.ORI_P2A_TYPE) {
                           	axisOrder[i] = j;
                           	axisFlip[i] = false;
                               found = true;
                               X[i][j] = 1.0;
                               r0[i] = r[j];
                               n0[i] = n[j];
                           }
                           else if (newOr[i] == FileInfoBase.ORI_A2P_TYPE) {
                           	axisOrder[i] = j;
                           	axisFlip[i] = true;
                               found = true;
                               X[i][j] = -1.0;
                               X[i][3] = ri[j]*(ni[j] - 1);
                               r0[i] = r[j];
                               n0[i] = n[j];
                           }
                       }
                       break;
                   case FileInfoBase.ORI_I2S_TYPE:
                       found = false;
                       for (i = 0; (i <= 2) && (!found); i++) {
                           if (newOr[i] == FileInfoBase.ORI_I2S_TYPE) {
                           	axisOrder[i] = j;
                           	axisFlip[i] = false;
                               found = true;
                               X[i][j] = 1.0;
                               r0[i] = r[j];
                               n0[i] = n[j];
                           }
                           else if (newOr[i] == FileInfoBase.ORI_S2I_TYPE) {
                           	axisOrder[i] = j;
                           	axisFlip[i] = true;
                               found = true;
                               X[i][j] = -1.0;
                               X[i][3] = ri[j]*(ni[j] - 1);
                               r0[i] = r[j];
                               n0[i] = n[j];
                           }
                       }
                       break;
                   case FileInfoBase.ORI_S2I_TYPE:
                       found = false;
                       for (i = 0; (i <= 2) && (!found); i++) {
                           if (newOr[i] == FileInfoBase.ORI_S2I_TYPE) {
                           	axisOrder[i] = j;
                           	axisFlip[i] = false;
                               found = true;
                               X[i][j] = 1.0;
                               r0[i] = r[j];
                               n0[i] = n[j];
                           }
                           else if (newOr[i] == FileInfoBase.ORI_I2S_TYPE) {
                           	axisOrder[i] = j;
                           	axisFlip[i] = true;
                               found = true;
                               X[i][j] = -1.0;
                               X[i][3] = ri[j]*(ni[j] - 1);
                               r0[i] = r[j];
                               n0[i] = n[j];
                           }
                       }
                       break;
               }
           } // for (j = 0; j <= 2; j++)
       	
       	
           for (i = 0; i <= 2; i++) {
           	if (transformedImage.getFileInfo(0) instanceof FileInfoNIFTI) {
           		fileInfoNIFTI.setResolutions(r0[i], i);   
                   fileInfoNIFTI.setExtents(n0[i], i);
                   fileInfoNIFTI.setAxisOrientation(newOr[i], i);	
           	}
           	else {
                   fileInfo.setResolutions(r0[i], i);   
                   fileInfo.setExtents(n0[i], i);
                   fileInfo.setAxisOrientation(newOr[i], i);
           	}
           }
           
           for (i = 0; i < 3; i++) {
               if (i == 0) {
               	originalOr[0] = 0.0f;
               	flippedOr[0] = ni[0] - 1;
               }
               else if (i == 1) {
               	originalOr[1] = 0.0f;
               	flippedOr[1] = ni[1] - 1;
               }
               else {
               	originalOr[2] = 0.0f;
               	flippedOr[2] = ni[2] - 1;
               }
           }
           
           for (i = 0; i < 3; i++) {
           	if (axisFlip[i]) {
           		if (axisOrder[i] == 0) {
           		    xOr = flippedOr[0];	
           		}
           		else if (axisOrder[i] == 1) {
           			yOr = flippedOr[1];
           		}
           		else {
           			zOr = flippedOr[2];
           		}
           	}
           	else {
           		if (axisOrder[i] == 0) {
           			xOr = originalOr[0];
           		}
           		else if (axisOrder[i] == 1) {
           			yOr = originalOr[1];
           		}
           		else {
           			zOr = originalOr[2];
           		}
           	}
           }
       	
           position = new Vector3f(xOr, yOr, zOr);
           out = new Vector3f(position);
           MipavCoordinateSystems.fileToScanner(position, out, transformedImage);
           for (i = 0; i < 3; i++) {
   	        if ((or[i] == FileInfoBase.ORI_R2L_TYPE) || (or[i] == FileInfoBase.ORI_L2R_TYPE)) {
   	            org[i] = out.X;
   	        }
   	        else if ((or[i] == FileInfoBase.ORI_A2P_TYPE) || (or[i] == FileInfoBase.ORI_P2A_TYPE)) {
   	            org[i] = out.Y;
   	        }
   	        else {
   	            org[i] = out.Z;
   	        }
           }

           for (i = 0; i < 3; i++) {
               newOrigin[i] = org[axisOrder[i]];
               if (Math.abs(newOrigin[i]) < .000001f) {
                   newOrigin[i] = 0f;
               }
           }
       	
       	
           if (transformedImage.getFileInfo(0) instanceof FileInfoNIFTI) {
           	fileInfoNIFTI.setOrigin(newOrigin);
           }
           else {
               fileInfo.setOrigin(newOrigin);
           }
           
           
           
           
           if ((newOr[2] == FileInfoBase.ORI_I2S_TYPE) || (newOr[2] == FileInfoBase.ORI_S2I_TYPE)) {
               newOrient = FileInfoBase.AXIAL;
           }
           else if ((newOr[2] == FileInfoBase.ORI_A2P_TYPE) || (newOr[2] == FileInfoBase.ORI_P2A_TYPE)) {
               newOrient = FileInfoBase.CORONAL;
           }
           else if ((newOr[2] == FileInfoBase.ORI_L2R_TYPE) || (newOr[2] == FileInfoBase.ORI_R2L_TYPE)) {
               newOrient = FileInfoBase.SAGITTAL;
           }
           else {
               newOrient = FileInfoBase.UNKNOWN_ORIENT;
           } 
           if (transformedImage.getFileInfo(0) instanceof FileInfoNIFTI) {
           	fileInfoNIFTI.setImageOrientation(newOrient);
           }
           else {
               fileInfo.setImageOrientation(newOrient);
           }
           

           TransMatrix transform = new TransMatrix(4);
           transform.setMatrix(0, 2, 0, 3, X); 
    		
           
    		
    		
           algoTrans = new AlgorithmTransform(transformedImage, transform, interp, r0[0], r0[1], r0[2], n0[0], n0[1], n0[2], 
					true, false, false);
           algoTrans.setUpdateOriginFlag(true);
    		
    		
           algoTrans.addListener(this);
    		
    		
           algoTrans.run();
    		
    		
    		
    		
    		
    	}//end for each orientation
	
    	transformedImage.disposeLocal();
    }*/
    
    
    
    
    
    
    
	
	
	/*public void algorithmPerformedOLD(AlgorithmBase algorithm) {
		if ( algorithm instanceof AlgorithmTransform) {
        	TransMatrix newMatrix = null;
        	TransMatrix newMatrix2 = null;
            resultImage = algoTrans.getTransformedImage();
			if (algorithm.isCompleted() == true && resultImage != null) {
				
				
				
				if(orientationIndex == 0) {
					insertScriptLine();
				}

                // The algorithm has completed and produced a new image to be displayed.
				if (srcImage.getFileInfo()[0].getFileFormat() == FileUtility.NIFTI) {
					fileInfoNIFTI.setMin(resultImage.getMin());
					fileInfoNIFTI.setMax(resultImage.getMax());
                    if (resultImage.getNDims() == 3) {
                    	for (int i = 0; i < resultImage.getExtents()[2]; i++) {
                    		resultImage.setFileInfo((FileInfoNIFTI)fileInfoNIFTI.clone(),i);
                    	}
                    }
                    else if (resultImage.getNDims() == 4) {
                    	for (int i = 0; i < resultImage.getExtents()[2]*resultImage.getExtents()[3]; i++) {
                    		resultImage.setFileInfo((FileInfoNIFTI)fileInfoNIFTI.clone(),i);
                    	}	
                    }
				}
				else {
				    JDialogBase.updateFileInfoStatic(fileInfo, resultImage);
				}
				if (resultImage.getNDims() >= 3) {
		        	// Update any destImage NIFTI matrices
		            MatrixHolder matHolder = null;
		            int i;
		            int j;
		            int t;
		            int index;
		            int tDim;
		            matHolder = resultImage.getMatrixHolder();
		            
		            float loc;
		            int orient;

		            if (matHolder != null) {
		            	
		                LinkedHashMap<String, TransMatrix> matrixMap = matHolder.getMatrixMap();
		                Iterator<String> iter = matrixMap.keySet().iterator();
		                String nextKey = null;
		                
		                TransMatrix tempMatrix = null;
		                

			            
		                while (iter.hasNext()) {
		                    nextKey = iter.next();
		                    tempMatrix = matrixMap.get(nextKey);
		                    if (tempMatrix.isNIFTI()) {
		                    	if (newMatrix == null) {
		                    	    newMatrix = new TransMatrix(4);
			                    	for (i = 0; i < 3; i++) {
			                            for (j = 0; j < 3; j++) {
			                            	if (axisFlip[i]) {
			                            		newMatrix.set(j, i, -tempMatrix.get(j, axisOrder[i]));
			                            	}
			                            	else {
			                                    newMatrix.set(j, i, tempMatrix.get(j, axisOrder[i]));
			                            	}
			                            }
			                            loc = tempMatrix.get(axisOrder[i], 3);
			                            if (axisFlip[i]) {
			                            	orient = srcImage.getFileInfo(0).getAxisOrientation(axisOrder[i]);
			                            	if ((orient == FileInfoBase.ORI_R2L_TYPE) || 
			                                        (orient == FileInfoBase.ORI_A2P_TYPE) || 
			                                        (orient == FileInfoBase.ORI_I2S_TYPE)) {
			                                	loc = loc + ((srcImage.getFileInfo(0).getExtents()[axisOrder[i]] - 1) * srcImage.getFileInfo(0).getResolutions()[axisOrder[i]]);
			                                }
			                                else {
			                                	loc = loc - ((srcImage.getFileInfo(0).getExtents()[axisOrder[i]] - 1) * srcImage.getFileInfo(0).getResolutions()[axisOrder[i]]);	
			                                }
			                            }
			                            newMatrix.set(i, 3, loc);
			                    	} // for (i = 0; i < 3; i++)
			                    	tempMatrix.Copy(newMatrix);
			                    	if (srcImage.getFileInfo(0) instanceof FileInfoNIFTI) {
				                        if (tempMatrix.isQform()) {
				                            if (resultImage.getNDims() == 3) {
				                                for (i = 0; i < resultImage.getExtents()[2]; i++) {
				                                    ((FileInfoNIFTI)resultImage.getFileInfo(i)).setMatrixQ(newMatrix);
				                                }
				                            }
				                            else if (resultImage.getNDims() == 4) {
				                                for (i = 0; i < resultImage.getExtents()[2]*resultImage.getExtents()[3]; i++) {
				                                    ((FileInfoNIFTI)resultImage.getFileInfo(i)).setMatrixQ(newMatrix);    
				                                }
				                            }
				                        } // if (tempMatrix.isQform())
				                        else { // tempMatrix is sform
				                            if (resultImage.getNDims() == 3) {
				                                for (i = 0; i < resultImage.getExtents()[2]; i++) {
				                                    ((FileInfoNIFTI)resultImage.getFileInfo(i)).setMatrixS(newMatrix);
				                                }
				                            }
				                            else if (resultImage.getNDims() == 4) {
				                                for (i = 0; i < resultImage.getExtents()[2]*resultImage.getExtents()[3]; i++) {
				                                    ((FileInfoNIFTI)resultImage.getFileInfo(i)).setMatrixS(newMatrix);    
				                                }
				                            }    
				                        } // else tempMatrix is sform
			                    	} // if (destImage.getFileInfo(0) instanceof FileInfoNIFTI)
		                    	} // if (newMatrix == null)
		                    	else {
		                    		newMatrix2 = new TransMatrix(4);
		                    		for (i = 0; i < 3; i++) {
			                            for (j = 0; j < 3; j++) {
			                            	if (axisFlip[i]) {
			                            		newMatrix2.set(j, i, -tempMatrix.get(j, axisOrder[i]));
			                            	}
			                            	else {
			                                    newMatrix2.set(j, i, tempMatrix.get(j, axisOrder[i]));
			                            	}
			                            }
			                            loc = tempMatrix.get(axisOrder[i], 3);
			                            if (axisFlip[i]) {
			                            	orient = srcImage.getFileInfo(0).getAxisOrientation(axisOrder[i]);
			                            	if ((orient == FileInfoBase.ORI_R2L_TYPE) || 
			                                        (orient == FileInfoBase.ORI_A2P_TYPE) || 
			                                        (orient == FileInfoBase.ORI_I2S_TYPE)) {
			                                	loc = loc + ((srcImage.getFileInfo(0).getExtents()[axisOrder[i]] - 1) * srcImage.getFileInfo(0).getResolutions()[axisOrder[i]]);
			                                }
			                                else {
			                                	loc = loc - ((srcImage.getFileInfo(0).getExtents()[axisOrder[i]] - 1) * srcImage.getFileInfo(0).getResolutions()[axisOrder[i]]);	
			                                }
			                            }
			                            newMatrix2.set(i, 3, loc);
			                    	} // for (i = 0; i < 3; i++)
			                    	tempMatrix.Copy(newMatrix2);
			                    	if (srcImage.getFileInfo(0) instanceof FileInfoNIFTI) {
				                        if (tempMatrix.isQform()) {
				                            if (resultImage.getNDims() == 3) {
				                                for (i = 0; i < resultImage.getExtents()[2]; i++) {
				                                    ((FileInfoNIFTI)resultImage.getFileInfo(i)).setMatrixQ(newMatrix2);
				                                }
				                            }
				                            else if (resultImage.getNDims() == 4) {
				                                for (i = 0; i < resultImage.getExtents()[2]*resultImage.getExtents()[3]; i++) {
				                                    ((FileInfoNIFTI)resultImage.getFileInfo(i)).setMatrixQ(newMatrix2);    
				                                }
				                            }
				                        } // if (tempMatrix.isQform())
				                        else { // tempMatrix is sform
				                            if (resultImage.getNDims() == 3) {
				                                for (i = 0; i < resultImage.getExtents()[2]; i++) {
				                                    ((FileInfoNIFTI)resultImage.getFileInfo(i)).setMatrixS(newMatrix2);
				                                }
				                            }
				                            else if (resultImage.getNDims() == 4) {
				                                for (i = 0; i < resultImage.getExtents()[2]*resultImage.getExtents()[3]; i++) {
				                                    ((FileInfoNIFTI)resultImage.getFileInfo(i)).setMatrixS(newMatrix2);    
				                                }
				                            }    
				                        } // else tempMatrix is sform
			                    	} // if (destImage.getFileInfo(0) instanceof FileInfoNIFTI)
		                    	}
		                    } // if (tempMatrix.isNIFTI())
		                }
		                if (newMatrix != null) {
		                	matHolder.clearMatrices();
		                	matHolder.addMatrix(newMatrix);
		                	if (newMatrix2 != null) {
		                		matHolder.addMatrix(newMatrix2);
		                	}
		                }
		            } // if (matHolder != null)  
		            
		            if ( (srcImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL))
		                    || (srcImage.getFileInfo()[0].getFileFormat() == FileUtility.DICOM)) {
		            	TransMatrix dicomMatrix = null;
		            	dicomMatrix = srcImage.getMatrix();
		            	newMatrix = new TransMatrix(4);
		            	for (i = 0; i < 3; i++) {
		                    for (j = 0; j < 3; j++) {
		                    	if (axisFlip[i]) {
		                    		newMatrix.set(j, i, -dicomMatrix.get(j, axisOrder[i]));
		                    	}
		                    	else {
		                            newMatrix.set(j, i, dicomMatrix.get(j, axisOrder[i]));
		                    	}
		                    }
		            	} // for (i = 0; i < 3; i++)
		            	newMatrix.setTransformID(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL);
		            	resultImage.getMatrixHolder().clearMatrices();
		            	resultImage.getMatrixHolder().addMatrix(newMatrix);
		            	if (resultImage.getNDims() >= 4) {
		            		tDim = resultImage.getExtents()[3];
		            	}
		            	else {
		            		tDim = 1;
		            	}
		            	
		            	for (t = 0; t < tDim; t++) {
			            	for (i = 0; i < resultImage.getExtents()[2]; i++) {
			            		index = i + t * resultImage.getExtents()[2];
			    	            Vector3f pos = new Vector3f(0, 0, i);
			    	        	Vector3f out = new Vector3f(pos);
			    	            MipavCoordinateSystems.fileToScanner(pos, out, resultImage);
			    	            float origin[] = new float[3];
			    	            origin[0] = out.X;
			    	            origin[1] = out.Y;
			    	            origin[2] = out.Z;
			    	            for (j = 0; j < 3; j++) {
			                		if ((resultImage.getAxisOrientation()[j] == FileInfoBase.ORI_R2L_TYPE) ||
			                			(resultImage.getAxisOrientation()[j] == FileInfoBase.ORI_L2R_TYPE)) {
			                	        resultImage.getFileInfo()[index].setOrigin(origin[0],j);
			                		}
			                		else if ((resultImage.getAxisOrientation()[j] == FileInfoBase.ORI_A2P_TYPE) ||
			                		        (resultImage.getAxisOrientation()[j] == FileInfoBase.ORI_P2A_TYPE)) {
			                		    resultImage.getFileInfo()[index].setOrigin(origin[1], j);
			                		}
			                		else {
			                		    resultImage.getFileInfo()[index].setOrigin(origin[2], j);        	
			                	    }
			                	}
			                }
		            	}
		            } // if ( (srcImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL))
		        } // if (destImage.getNDims() >= 3)
                resultImage.clearMask();
				resultImage.calcMinMax();
				
				
				int[] resultOrient = resultImage.getFileInfo()[0].getAxisOrientation();
				String resultImageName = "";
 
				if(resultOrient[2] == FileInfoBase.ORI_A2P_TYPE) {
					resultImageName = outputNamePrefix + "_coronal";
				}else if(resultOrient[2] == FileInfoBase.ORI_I2S_TYPE) {
					resultImageName = outputNamePrefix + "_axial";
				}else {
					resultImageName = outputNamePrefix + "_sagittal";
				}
				
				resultImage.setImageName(resultImageName);
				
				
				 FileIO fileIO = new FileIO();
			     fileIO.setQuiet(true);
			     FileWriteOptions opts = new FileWriteOptions(true);
			     if(!outputDir.endsWith(File.separator)) {
			    	 outputDir = outputDir + File.separator;    
			     }
			     
			     opts.setFileDirectory(outputDir);

		        
		       
		        if (resultImage.getNDims() == 3) {
	                opts.setBeginSlice(0);
	                opts.setEndSlice(resultImage.getExtents()[2] - 1);
	            } else if (resultImage.getNDims() == 4) {
	                opts.setBeginSlice(0);
	                opts.setEndSlice(resultImage.getExtents()[2] - 1);
	                opts.setBeginTime(0);
	                opts.setEndTime(resultImage.getExtents()[3] - 1);
	            }
			       
			        
			        
			        

				//now save the image as slices
				if(saveAsJPEGRadio.isSelected()) {
					//save as JPEG
					opts.setFileType(FileUtility.JPEG);
					opts.setFileName(resultImageName + ".jpg");
					
				}else if(saveAsJP2Radio.isSelected()) {
					//save as JPEG 2000
					opts.setFileType(FileUtility.JP2);
					opts.setFileName(resultImageName + ".jp2");
				}else {
					//save as PNG
					opts.setFileType(FileUtility.PNG);
					opts.setFileName(resultImageName + ".png");
					
				}
				 opts.setOptionsSet(true);
				 opts.setMultiFile(true); 
				
				 
				System.out.println("Saving image...");
				fileIO.writeImage(resultImage, opts,true,false);
				System.out.println("image saved to " + opts.getFileDirectory());
				
                try {
					new ViewJFrameImage(resultImage, null, new Dimension(610, 200) );
                } catch (OutOfMemoryError error) {
                    System.gc();
                    JOptionPane.showMessageDialog(null, 
                                                "Out of memory: unable to open new frame",
                                                "Error", JOptionPane.ERROR_MESSAGE);
                }

				resultImage.disposeLocal();
				resultImage = null;

            } else if (resultImage != null) {
                //algorithm failed but result image still has garbage
                resultImage = null;
                System.gc();
            }
			
			if(orientationIndex == 2) {
				 //if opened via browse button..then i can dispose...otherwise dont
		        if(launchedFromGUI) {
		        	srcImage.disposeLocal();
		        }
		        
		        if (algoTrans != null) {
		        	algoTrans.finalize();
		        	algoTrans = null;
		        }
				System.out.println("plugin complete");
				this.setComplete(true); 
				if (isExitRequired()) {
	                System.exit(0);
	                // ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
	            } else {
	                dispose();
	            }
			}
       }
		
		
		

	}*/





	@Override
	protected void setGUIFromParams() {
		
		srcImage = scriptParameters.retrieveInputImage();
		
		
		srcImageFilePathTextField.setText(srcImage.getImageDirectory() + srcImage.getImageFileName());
		

		String oDir = scriptParameters.getParams().getString("output_dir");
		File dirFile = new File(oDir);
		if ( !dirFile.exists()) {
            MipavUtil.displayError("The directory selected does not exist.  Please choose another.");
            return;
        } else if ( !dirFile.isDirectory()) {
            MipavUtil.displayError("Please select a directory.");
            return;
        } else {
        	outputDirPathTextField.setText(oDir);
        	outputDir = oDir;
        }
		
		outputNamePrefix = scriptParameters.getParams().getString("output_prefix");
		outputNamePrefixTextField.setText(outputNamePrefix);
		
		String oFormat = scriptParameters.getParams().getString("output_format");
		
		if(oFormat != null && !oFormat.trim().equals("")) {
			//if(oFormat.trim().equalsIgnoreCase("jp2")) {
				//saveAsJP2Radio.setSelected(true);
			//}//else if(oFormat.trim().equalsIgnoreCase("png")) {
				//saveAsPNGRadio.setSelected(true);
			if(oFormat.trim().equalsIgnoreCase("jpg") || oFormat.trim().equalsIgnoreCase("jpeg")){
				saveAsJPEGRadio.setSelected(true);
			}
		    else {
				 MipavUtil.displayError("Invalid output format type...must be jpg or jpeg");
		         return;
			}
		}else {
			saveAsJPEGRadio.setSelected(true);
		}

		
	}





	@Override
	protected void storeParamsFromGUI() throws ParserException {
		scriptParameters.storeInputImage(srcImage);
		scriptParameters.getParams().put(ParameterFactory.newParameter("output_dir", outputDir));
		scriptParameters.getParams().put(ParameterFactory.newParameter("output_prefix", outputNamePrefix));
		String oFormat = "";
		if(saveAsJP2Radio.isSelected()) {
			oFormat = "jp2";
		}else if(saveAsPNGRadio.isSelected()) {
			oFormat = "png";
		}else {
			oFormat = "jpg";
		}
		scriptParameters.getParams().put(ParameterFactory.newParameter("output_format", oFormat));
	}

}
