package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import gov.nih.mipav.model.algorithms.registration.vabra.RegistrationUtilities;
import gov.nih.mipav.model.algorithms.registration.vabra.RegistrationUtilities.InterpolationType;
import gov.nih.mipav.model.algorithms.registration.vabra.VabraAlgorithm;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubset;
import gov.nih.mipav.model.file.DTIParameters;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeImage;

import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import Jama.Matrix;

public class JPanelEPIDistortionCorrection extends JPanel implements ActionListener
{

	private static final long serialVersionUID = 5817025147038892937L;

	public static void buildLoadPanel( ActionListener listener, JPanel mainPanel, 
			JLabel label, JTextField imageName, String tooltip, String actionCommand ) {

		final JPanel DTIloadPanel = new JPanel();
		DTIloadPanel.setLayout(new GridBagLayout());
		DTIloadPanel.setBorder(JInterfaceBase.buildTitledBorder(""));

		final GridBagConstraints gbc = new GridBagConstraints();

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.weightx = 1;
		gbc.fill = GridBagConstraints.CENTER;
		gbc.anchor = GridBagConstraints.WEST;

		JButton openT2imageButton = new JButton("Browse");
		openT2imageButton.setToolTipText( tooltip );
		openT2imageButton.addActionListener(listener);
		openT2imageButton.setActionCommand( actionCommand );
		openT2imageButton.setEnabled(true);

		imageName.setPreferredSize(new Dimension(275, 21));
		imageName.setEditable(true);
		imageName.setBackground(Color.white);
		imageName.setFont(MipavUtil.font12);

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.anchor = GridBagConstraints.WEST;
		DTIloadPanel.add(label, gbc);

		gbc.gridx = 1;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.EAST;
		DTIloadPanel.add(imageName, gbc);

		gbc.gridx = 2;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.anchor = GridBagConstraints.EAST;
		DTIloadPanel.add(openT2imageButton, gbc);
		mainPanel.add(DTIloadPanel);
	}

	public static ModelImage extractSubVolume( ModelImage dwiImage, int index ) {
        int[] destB0Extents = new int[3];
        destB0Extents[0] = dwiImage.getExtents()[0];
        destB0Extents[1] = dwiImage.getExtents()[1];
        destB0Extents[2] = dwiImage.getExtents()[2];
        String resultB0String = dwiImage.getImageName() + "T_" + index;
        ModelImage resultB0Image = new ModelImage(dwiImage.getType(), destB0Extents, resultB0String);        
        AlgorithmSubset subsetAlgo = new AlgorithmSubset(dwiImage, resultB0Image, AlgorithmSubset.REMOVE_T, index);
        subsetAlgo.run();

        if ( subsetAlgo.isCompleted() == true )
        {
        	return resultB0Image;
        }
        resultB0Image.disposeLocal(false);
        return null;
    }

	/** reference to the DTI Pipeline */
	private DTIPipeline pipeline;
	/** main panel */
	private JPanel mainPanel;
	/** Text fields for entering the input images: */
	private JTextField refImageNumText = new JTextField("0", 2);
	private JTextField resampledT2Text = new JTextField();
	private JTextField registeredDWIText = new JTextField();
	private JTextField deformationB0T2Text = new JTextField();
	private JTextField matricesFile = new JTextField();

	private JTextField B0MatrixFile = new JTextField();
	private JTextField outputDir = new JTextField();
	/** output image display options: */
	private JCheckBox displayDeformationField = new JCheckBox( "Display Deformation Field" );

	private JCheckBox displayRegisteredB0 = new JCheckBox( "Display B0 registered to T2" );
	private JCheckBox displayEPIResult = new JCheckBox( "Display EPI-Distortion Corrected Image" );
	/** button commands */
	private String loadT2Command = new String("browseT2File");
	private String loadDWICommand = new String("browseDWIFile");
	private String loadDeformationFieldCommand = new String("browseDefFieldFile");
	private String outputCommand = new String("browseOutput");
	private String computeDefFieldCommand = new String("vabra");
	private String computeEpiCommand = new String("epi");

	private String loadB0MatrixComand = new String("browseB0MatrixFile");
	private String loadAllMatrixComand = new String("browseAllMatrixFile");
	
	/** button for computing the deformation field: */
	private JButton computeDeformationFieldButton = new JButton("Compute Deformation Field");
	/** button for applying the deformation field to the input 4D image */
	private JButton computeEpiCorrectionButton = new JButton("Compute Epi-Distortion Correction");
	/** Structural T2 image, resampled to match the DWI image series. */
	private ModelImage resampledT2 = null;
	/** DWI image series that has been registered within series to the B0 volume. */
	private ModelImage registeredDWI = null;
	/** B0 volume extracted from the DWI series. */
	private ModelImage registeredB0 = null;
	
	/** deformation field from registering the B0 volume to the resampled T2 volume with VABRA */
	private ModelImage deformationB0T2 = null;
	/** DWI image series after applying the list of deformation fields */
	private ModelImage deformedDWI = null;
    
    /** Transformation matrix from registering B0 to T2. */
	private TransMatrix matB0T2;
    
    /** Array of transformation matrices after registering the DWI image series to the B0 reference volume. */
	private TransMatrix[] matRegistered;
    
    /**
     * Creates the EPI-Distortion correction panel for the DTI Pipeline
     * @param pipeline
     */
    public JPanelEPIDistortionCorrection(DTIPipeline pipeline)
    {
		super(new GridBagLayout());
        this.pipeline = pipeline;
        init();
    }
    
    /* (non-Javadoc)
     * @see java.awt.event.ActionListener#actionPerformed(java.awt.event.ActionEvent)
     */
    public void actionPerformed(ActionEvent event)
	{
		final String command = event.getActionCommand();

		if (command.equals(loadT2Command))
		{
			setResampledT2Image( loadFile( resampledT2Text ) );
		}
		else if (command.equals(loadDWICommand))
		{
			setRegisteredDWIImage( loadFile( registeredDWIText ) );
		}
		else if (command.equals(outputCommand))
		{
            final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));

            chooser.setDialogTitle("Choose dir");
            chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
            final int returnValue = chooser.showOpenDialog(this);
            if (returnValue == JFileChooser.APPROVE_OPTION) {
            	outputDir.setText(chooser.getSelectedFile().getAbsolutePath() + File.separator);
            }			
		}	
		else if (command.equals(computeDefFieldCommand))
		{
			runVabra();
		}
		else if (command.equals(loadDeformationFieldCommand))
		{
			deformationB0T2 = loadFile( deformationB0T2Text );
			if ( outputDir.getText().isEmpty() )
			{
				outputDir.setText( deformationB0T2.getImageDirectory() );
			}			
		}
		else if (command.equals(loadB0MatrixComand))
		{
			matB0T2 = readTransformMatrix(B0MatrixFile);
		}
		else if (command.equals(loadAllMatrixComand))
		{
			matRegistered = readMultiTransformMatrix( matricesFile );
			if ( matRegistered != null )
			{
				for ( int i = 0; i < matRegistered.length; i++ )
				{
					System.err.println( matRegistered[i] );
				}
			}
		}
		else if (command.equals(computeEpiCommand))
		{
	        int indexB0 = Integer.parseInt(refImageNumText.getText());
    		deformedDWI = applyDeformationField( indexB0, matB0T2, matRegistered, deformationB0T2, registeredDWI );
    		if ( deformedDWI != null )
    		{
    			if ( displayEPIResult.isEnabled() && displayEPIResult.isSelected() )
    			{
    				new ViewJFrameImage(deformedDWI);
    			}
    			pipeline.finishEPIPanel();
    		}
		}
		enableComputeVabra();
		enableComputeEpiDistortion();
	}
    
    /**
     * Returns the distortion-corrected DWI 4D image series.
     * @return the distortion-corrected DWI 4D image series.
     */
    public ModelImage getResult()
    {
    	return deformedDWI;
    }

    /**
     * Sets the B0 to T2 matrix. Set from the DTI Pipeline pre-processing panel, or by the
     * user loading the .mtx file from disk.
     * @param matrix
     * @param fileName
     */
    public void setB0toT2Matrix( TransMatrix matrix, String fileName )
    {
    	if ( fileName != null )
		{
			B0MatrixFile.setText(new String(fileName) );
		}
		matB0T2 = new TransMatrix(matrix);    	
		enableComputeEpiDistortion();
    }
    
    /**
     * Sets the with-in volume registered 4D DWI image. Can be set from the DTI Pipeline pre-processing panel 
     * or by the user loading the file from disk.
     * @param registeredImage
     */
    public void setRegisteredDWIImage( ModelImage registeredImage )
    {
    	if ( registeredImage == null )
    	{
    		return;
    	}
    	registeredDWI = registeredImage;
    	registeredDWIText.setText( registeredDWI.getImageDirectory() + registeredDWI.getImageName() );
		if ( outputDir.getText().isEmpty() )
		{
			outputDir.setText( registeredDWI.getImageDirectory() );
		}
		if ( registeredDWI != null )
		{
			DTIParameters dtiparams = registeredDWI.getDTIParameters();
			if ( (dtiparams != null) && (dtiparams.getGradients() != null) && (dtiparams.getGradients() != null) )
			{
				for ( int i = 0; i < dtiparams.getbValues().length; i++ ){
					if ( (dtiparams.getbValues()[i] == 0) && 
						 (dtiparams.getGradients()[i][0] == 0) && 
						 (dtiparams.getGradients()[i][1] == 0) && 
						 (dtiparams.getGradients()[i][2] == 0) )
					{
						refImageNumText.setText(String.valueOf(i));
						break;
					}   
				}
			}
		}
		enableComputeVabra();
    }
        
    
	/**
     * Sets the within-volume array of matrices. Set from the DTI Pipeline pre-processing panel, or by the
     * user loading the .mtx file from disk.
     * @param matrix
     * @param fileName
     */
    public void setRegisteredMatrices( TransMatrix[] matrix, String fileName )
    {
		int nMats = matrix.length;
		matRegistered = new TransMatrix[nMats];
		for ( int i = 0; i < nMats; i++ )
		{
			matRegistered[i] = new TransMatrix( matrix[i] );
		}
		if ( fileName != null )
		{
			matricesFile.setText( new String( fileName ) );
		}
		enableComputeEpiDistortion();
    }

	/**
     * Sets the resampled T2 structural image, from the DTIPipeline pre-processing panel, 
     * or by the user loading the file disk.
     * @param resampledImage
     */
    public void setResampledT2Image( ModelImage resampledImage )
    {
		resampledT2 = resampledImage;
		resampledT2Text.setText( resampledT2.getImageDirectory() + resampledT2.getImageName() );
		if ( outputDir.getText().isEmpty() )
		{
			outputDir.setText( resampledT2.getImageDirectory() );
		}
		enableComputeVabra();
    }


	/**
	 * Creates a deformation field from the input matrix and deformation field and applies it to the input image.
	 * Returns the new image result.
	 * @param m input matrix
	 * @param image input image
	 * @param defField input deformation field
	 * @param resolutions resolutions of the input image
	 * @return deformed input image
	 */
	private ModelImage applyDefField( Matrix m, ModelImage image, ModelImage defField, float[] resolutions )
	{
		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;

		double[] b0Def = new double[3];
		double[] newVec = new double[3];
		double[] currentVec = new double[3];
        ModelImage matrixDef = new ModelImage( ModelStorageBase.FLOAT, new int[]{dimX,dimY,dimZ,3}, "matDefField" );
        ModelImage combinedDef = new ModelImage( ModelStorageBase.FLOAT, new int[]{dimX,dimY,dimZ,3}, "defField" );
        
		// create a new Deformation Field from the matrix m...
		for(int i = 0; i < dimX; i++)
		{
			for(int j = 0; j < dimY; j++)
			{
				for(int k = 0; k < dimZ; k++)
				{ 
					newVec[0] = i*resolutions[0];
					newVec[1] = j*resolutions[1];
					newVec[2] = k*resolutions[2];

					Matrix v = new Matrix(new double[]{newVec[0],newVec[1],newVec[2],1},4);
					Matrix vp = m.solve(v);
					for (int ch = 0; ch < 3; ch++)
					{
						newVec[ch] = vp.get(ch,0) - newVec[ch];
						matrixDef.set( i, j, k, ch, newVec[ch]/resolutions[ch] );
						combinedDef.set( i, j, k, ch, newVec[ch]/resolutions[ch] );
					}
				}
			}
		}

		// create combined deformation field w/above and the input defField:
		for (int i = 0; i < dimX; i++)
		{
			for(int j = 0; j < dimY; j++)
			{
				for(int k = 0; k < dimZ; k++)
				{ 
					for (int c = 0; c < 3; c++)
					{
						b0Def[c] = defField.getDouble(i, j, k, c); 
					}
                    
                   //get current deformation at where the new deformation is pointing
					for (int c = 0; c < 3; c++)
					{
						currentVec[c] = RegistrationUtilities.TrilinearInterpolateDefField(matrixDef, dimX, dimY, dimZ, 
								b0Def[0]+i, b0Def[1]+j, b0Def[2]+k, c); 
					}
					for (int c = 0; c < 3; c++)
					{
						combinedDef.set( i, j, k, c, b0Def[c]+currentVec[c]);
					}
				}
			}
		}
		
		// apply combined deformation field to the input image:
		ModelImage defSub = new ModelImage( ModelStorageBase.FLOAT, new int[]{dimX,dimY,dimZ}, "d" );

		RegistrationUtilities.DeformImage3D(image, defSub, combinedDef, dimX,
				dimY, dimZ, InterpolationType.TRILINEAR); 
		defSub.setImageName(image.getImageName() + "_reg");
		
		matrixDef.disposeLocal(false);
		combinedDef.disposeLocal(false);
		return defSub;
	}

	/**
	 * Applies the input B0 to T2 matrix, list of within-volume matrices and input deformation field to the 4D volume series.
	 * @param indexB0 index of the B0 image in the 4D image
	 * @param b0toStructMatrix B0 to T2 transformation matrix
	 * @param arrayTransMatrix array of transformation matrices from the within-volume registration
	 * @param defField B0 to T2 deformation field, computed from VABRA
	 * @param result35RegImage input 4D image
	 * @return new image result
	 */
	private ModelImage applyDeformationField( int indexB0, TransMatrix b0toStructMatrix, TransMatrix[] arrayTransMatrix, ModelImage defField, ModelImage result35RegImage )
	{        
		Matrix b0 = new Matrix(4,4);
        for(int j=0; j<4; j++)
        {
            for(int k=0; k<4; k++)
            {   
            	b0.set( j, k, b0toStructMatrix.Get(j, k) );     
            }                   
        }
		
		float[] res = result35RegImage.getResolutions(0);
		
		int dimX = result35RegImage.getExtents().length > 0 ? result35RegImage.getExtents()[0] : 1;
		int dimY = result35RegImage.getExtents().length > 1 ? result35RegImage.getExtents()[1] : 1;
		int dimZ = result35RegImage.getExtents().length > 2 ? result35RegImage.getExtents()[2] : 1;
		int numVolumes = result35RegImage.getExtents().length > 3 ? result35RegImage.getExtents()[3] : 1;
		int volSize = dimX*dimY*dimZ;
		float[] buffer = new float[volSize];
		ModelImage temp = new ModelImage( ModelStorageBase.FLOAT, new int[]{dimX,dimY,dimZ}, "temp" );
		ModelImage deformedDWI = new ModelImage( result35RegImage.getType(), result35RegImage.getExtents(), result35RegImage.getImageName() + "_epi_corrected" );

        final ViewJProgressBar progressBar = new ViewJProgressBar("epi-distortion correction",
                "epi-distortion correction...", 0, 100, false, null, null);
        progressBar.setVisible(true);
        progressBar.updateValueImmed(0);
		for ( int i = 0; i < numVolumes; i++ )
		{
			try {
				result35RegImage.exportData( i * volSize, volSize, buffer );
			} catch (IOException e) {}
			try {
				temp.importData( 0, buffer, false );
			} catch (IOException e) { }     
			Matrix combinedMatrix = new Matrix(4,4);
			for (int j=0; j<4; j++)
			{
				for (int k=0; k<4; k++)
				{   
					combinedMatrix.set(j, k, arrayTransMatrix[i].Get(j, k));
				}                   
			}
			//Multiple B0 to Struct trans matrix from trans matrix corresponding to DWI volume
			//combinedMatrix = b0.times(combinedMatrix);
			if ( i == indexB0 )
			{
				combinedMatrix = b0;
			}
			ModelImage deformedSubVol = applyDefField( combinedMatrix, temp, defField, res );
			try {
				deformedSubVol.exportData( 0, volSize, buffer );
				deformedDWI.importData( i * volSize, buffer, false );
			}  catch (IOException e) { }     
			deformedSubVol.disposeLocal(false);
			
			progressBar.updateValueImmed( (int)(100 * i / (float)numVolumes) );
		}
		temp.disposeLocal(false);
		
        progressBar.updateValueImmed(100);
        progressBar.dispose();

        deformedDWI.setDTIParameters( new DTIParameters( result35RegImage.getDTIParameters() ) );
		JDialogBase.updateFileInfo( result35RegImage, deformedDWI );
		deformedDWI.calcMinMax();
		ModelImage.saveImage( deformedDWI, deformedDWI.getImageName() + ".xml", outputDir.getText() );
		return deformedDWI;
	}

	/**
	 * Checks that all the parameters necessary to run the epi-distortion correction are set
	 * and enables the compute button and display check-box. 
	 */
	private void enableComputeEpiDistortion()
	{
		if ( deformationB0T2 != null && registeredDWI != null )
		{
			int nMats = registeredDWI.getExtents().length > 3 ? registeredDWI.getExtents()[3] : 1;
			if ( matB0T2 != null && matRegistered != null && matRegistered.length == nMats )
			{
				computeEpiCorrectionButton.setEnabled(true);
		    	displayEPIResult.setEnabled(true);
			}
		}
	}

    /**
     * Checks that all the parameters necessary to run the VABRA registration are set
     * and enables the compute button and display check-boxes.
     */
    private void enableComputeVabra()
	{
		if ( resampledT2 != null && registeredDWI != null )
		{
			if ( !VolumeImage.checkImage( resampledT2, registeredDWI ) )
			{
				MipavUtil.displayError( resampledT2.getImageName() + " does not match " + registeredDWI.getImageName() );
			}
			else
			{
				computeDeformationFieldButton.setEnabled(true);
		    	displayDeformationField.setEnabled(true);
		    	displayRegisteredB0.setEnabled(true);
			}
		}
	}
    
    /**
     * User-interface intialization.
     */
    private void init()
    {
    	displayDeformationField.setEnabled(false);
    	displayRegisteredB0.setEnabled(false);
    	displayEPIResult.setEnabled(false);
    	displayDeformationField.setSelected(false);
    	displayRegisteredB0.setSelected(false);
    	displayEPIResult.setSelected(true);

		mainPanel = new JPanel();
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));
		mainPanel.setAlignmentX(Component.LEFT_ALIGNMENT);


        JPanel optPanel = new JPanel();
        optPanel.setLayout(new GridBagLayout());
		
		JLabel labelInternal = new JLabel("Reference DWI Volume Number: ");
        labelInternal.setForeground(Color.black);

        refImageNumText.setEnabled(true);
        
        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.weightx = 0;
        gbc.fill = GridBagConstraints.NONE;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.insets = new Insets(0, 2, 0, 2);
        optPanel.add(labelInternal, gbc);

        gbc.gridx = 1;
        gbc.gridy = 1;
        gbc.weightx = 0.15;
        gbc.fill = GridBagConstraints.REMAINDER;
        optPanel.add(refImageNumText, gbc);
        
        mainPanel.add(optPanel);
        

        buildLoadPanel( this, mainPanel, new JLabel("Resampled T2 image:"), resampledT2Text, "Browse resampled T2 image file", loadT2Command );
        buildLoadPanel( this, mainPanel, new JLabel("Registered DWI image:" ), registeredDWIText, "Browse registered 4D DWI image file", loadDWICommand );
        buildLoadPanel( this, mainPanel, new JLabel("Output dir:" ), outputDir, "Browse output directory", outputCommand );
		

		GridBagConstraints gbc2 = new GridBagConstraints();
		gbc2.fill = GridBagConstraints.NONE;
		gbc2.weightx = 1;
		gbc2.weighty = 1;
		gbc2.gridx = 0;
		gbc2.gridy = 0;
		gbc2.anchor = GridBagConstraints.NORTHWEST;
		this.add(mainPanel, gbc2);
		
		

		// build button panel
		JPanel buttonPanel1 = new JPanel();
		buttonPanel1.setLayout(new GridBagLayout());
		buttonPanel1.setBorder(JInterfaceBase.buildTitledBorder(""));
		gbc = new GridBagConstraints();

		computeDeformationFieldButton.setToolTipText("Compute deformation field B0 to T2");
		computeDeformationFieldButton.addActionListener(this);
		computeDeformationFieldButton.setActionCommand( computeDefFieldCommand );
		computeDeformationFieldButton.setVisible(true);
		computeDeformationFieldButton.setEnabled(false);
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.weightx = 1;
		gbc.fill = GridBagConstraints.CENTER;
		gbc.anchor = GridBagConstraints.WEST;
		buttonPanel1.add(displayDeformationField, gbc);
		gbc.gridy++;
		buttonPanel1.add(displayRegisteredB0, gbc);
		gbc.gridy++;
		buttonPanel1.add(computeDeformationFieldButton, gbc);
		mainPanel.add(buttonPanel1);


		buildLoadPanel( this, mainPanel, new JLabel("Deformation Field:" ), deformationB0T2Text, "Browse deformation field", loadDeformationFieldCommand );
		buildLoadPanel( this, mainPanel, new JLabel("B0 to T2 Transformation Matrix:" ), B0MatrixFile, "Browse matrix file", loadB0MatrixComand );
		buildLoadPanel( this, mainPanel, new JLabel("4D Transformation Matrix:" ), matricesFile, "Browse all matrix files", loadAllMatrixComand );
		


		// build button panel
		JPanel buttonPanel2 = new JPanel();
		buttonPanel2.setLayout(new GridBagLayout());
		buttonPanel2.setBorder(JInterfaceBase.buildTitledBorder(""));
		gbc = new GridBagConstraints();

		computeEpiCorrectionButton.setToolTipText("Compute epi-distortion correction");
		computeEpiCorrectionButton.addActionListener(this);
		computeEpiCorrectionButton.setActionCommand( computeEpiCommand );
		computeEpiCorrectionButton.setVisible(true);
		computeEpiCorrectionButton.setEnabled(false);
		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.gridwidth = 1;
		gbc.gridheight = 1;
		gbc.weightx = 1;
		gbc.fill = GridBagConstraints.CENTER;
		gbc.anchor = GridBagConstraints.WEST;
		buttonPanel2.add(displayEPIResult, gbc);
		gbc.gridy++;
		buttonPanel2.add(computeEpiCorrectionButton, gbc);
		mainPanel.add(buttonPanel2);
    }

	/**
	 * Loads a user-selected image file. Sets the input text field to the file name.
	 * @param textField
	 * @return
	 */
	private ModelImage loadFile( JTextField textField ) {
		ModelImage loadedImage = null;
		
		final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
		chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
		chooser.setDialogTitle("Choose image file");
		final int returnValue = chooser.showOpenDialog(this);
		if (returnValue == JFileChooser.APPROVE_OPTION) {
			final FileIO fileIO = new FileIO();

			loadedImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
					+ File.separator);

			textField.setText(chooser.getSelectedFile().getAbsolutePath());
			Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
			new ViewJFrameImage(loadedImage);
		}
		return loadedImage;
	}
	
	/**
     * Reads the multiple-matrix file from disk. Sets the input text field to list the file name.
     * @param textField
     * @return
     */
    private TransMatrix[] readMultiTransformMatrix( JTextField textField )
	{
		TransMatrix[] newMatrix = null;
		
		final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
		chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MATRIX));
		chooser.setDialogTitle("Choose matrix file");
		final int returnValue = chooser.showOpenDialog(this);
		if (returnValue == JFileChooser.APPROVE_OPTION) {

			final File file = new File(chooser.getSelectedFile().getAbsolutePath());
			RandomAccessFile raFile;
			try {
				raFile = new RandomAccessFile(file, "r");
				newMatrix = TransMatrix.readMultiMatrix(raFile);
				raFile.close();
			} catch (FileNotFoundException e) {} catch (IOException e) {}

			if ( textField != null )
			{
				textField.setText(chooser.getSelectedFile().getAbsolutePath());
			}
			Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
		}		
		return newMatrix;
	}
	
	

	/**
     * Reads a single matrix file from disk. Sets the input text field to match the file name.
     * @param textField
     * @return
     */
    private TransMatrix readTransformMatrix( JTextField textField )
	{
		TransMatrix newMatrix = new TransMatrix(4);
		
		final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
		chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.MATRIX));
		chooser.setDialogTitle("Choose matrix file");
		final int returnValue = chooser.showOpenDialog(this);
		if (returnValue == JFileChooser.APPROVE_OPTION) {

			final File file = new File(chooser.getSelectedFile().getAbsolutePath());
			RandomAccessFile raFile;
			try {
				raFile = new RandomAccessFile(file, "r");
				newMatrix.readMatrix(raFile, false);
				raFile.close();
			} catch (FileNotFoundException e) {} catch (IOException e) {}

			if ( textField != null )
			{
				textField.setText(chooser.getSelectedFile().getAbsolutePath());
			}
			Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
		}		
		return newMatrix;
	}
	
	/**
	 * Run the VABRA registration. Saves the deformation field and the registered B0 image to disk. 
	 * Enables the epi-distortion correction.
	 */
	private void runVabra()
    {
    	if ( registeredB0 == null && registeredDWI != null )
    	{
    		// extract from registered DWI image:
            int indexB0 = Integer.parseInt(refImageNumText.getText());
    		registeredB0 = extractSubVolume( registeredDWI, indexB0 );
    	}
    	if ( resampledT2 != null && registeredB0 != null)
    	{
    		VabraAlgorithm vabra = new VabraAlgorithm();
    		vabra.solve( registeredB0, resampledT2 );
    		// Opens the Deformation Field in a window:
    		deformationB0T2 = vabra.getDeformationField();
    		ModelImage registeredB0 = vabra.getRegisteredResults();
    		if ( displayDeformationField.isEnabled() && displayDeformationField.isSelected() )
    		{
    			new ViewJFrameImage( deformationB0T2 );
    		}
    		if ( displayRegisteredB0.isEnabled() && displayRegisteredB0.isSelected() )
    		{
    			new ViewJFrameImage( registeredB0 );
    		}
			ModelImage.saveImage( deformationB0T2, deformationB0T2.getImageName() + ".xml", outputDir.getText() );
			ModelImage.saveImage( registeredB0, registeredB0.getImageName() + ".xml", outputDir.getText() );
			
			deformationB0T2Text.setText( outputDir.getText() + File.separator + deformationB0T2.getImageName() + ".xml");
			enableComputeEpiDistortion();
    	}
    }
	
}
