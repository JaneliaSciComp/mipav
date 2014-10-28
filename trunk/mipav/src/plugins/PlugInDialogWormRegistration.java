
//MIPAV is freely available from http://mipav.cit.nih.gov

//THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, 
//EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES 
//OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND 
//NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT 
//HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
//WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
//FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE 
//OR OTHER DEALINGS IN THE SOFTWARE. 

/*****************************************************************
******************************************************************

The MIPAV application is intended for research use only.
This application has NOT been approved for ANY diagnostic use 
by the Food and Drug Administration. There is currently no 
approval process pending. 

This software may NOT be used for diagnostic purposes.

******************************************************************
******************************************************************/

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmCostFunctions;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR3D;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.Render.LatticeModel;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.HashSet;
import java.util.Vector;

import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.JTextField;

public class PlugInDialogWormRegistration extends JDialogStandalonePlugin implements AlgorithmInterface {
        
	private static final long serialVersionUID = 2476025001402032629L;
	
	/** This source image is typically set by the constructor */
    private ModelImage wormImageA;     
    private ModelImage prevImageA;     
    private int[] maxExtents;
    private JTextField  baseFileLocText;
    private JTextField  matrixFileLocText;
    private JTextField  outputFileLocText;
    private JTextField  baseFileNameText;
    private JTextField rangeFusionText;
    private JPanel okCancelPanel;    
    private String baseFileDir; 
    private String matrixFileDir;
    private String outputFileDir;
    private Vector<Integer> includeRange;
    private File registrationDir;
    private File matrixDir;


    public PlugInDialogWormRegistration()
    {
        init();
        setVisible(true);
    }
    
    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     *
     * @param  event  Event that triggers function.
     */
    public void actionPerformed(ActionEvent event)
    {
        String command = event.getActionCommand();

        if (command.equals("OK")) {
            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Cancel")) {
        	this.windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
        } else {
            super.actionPerformed(event);
        }
    } 
    
    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) { } 
    
    public void itemStateChanged(final ItemEvent event)
    {    
    }


    /**
     * Once all the necessary variables are set, call the kidney segmentation algorithm
     */
    protected void callAlgorithm()
    {
    	if ( setVariables() )
    	{
    		if ( includeRange != null )
    		{
    			maxExtents = new int[]{0,0,0};
    			for ( int i = 0; i < includeRange.size(); i++ )
    			{
//    	    		String fileName = baseFileDir + File.separator + baseFileNameText.getText() + "_" + includeRange.elementAt(i) + ".tif";
    	    		String fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + "_straight" + ".tif";
    	            File voiFile = new File(baseFileDir + File.separator + fileName);
    	            if ( voiFile.exists() )
    	            {
    	            	System.err.println( fileName );
    	                FileIO fileIO = new FileIO();
    	                if(wormImageA != null) {
    	                	wormImageA.disposeLocal();
    	                	wormImageA = null;
    	                }
    	                wormImageA = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);  
	                	int dimX = wormImageA.getExtents().length > 0 ? wormImageA.getExtents()[0] : 0;  
	                	int dimY = wormImageA.getExtents().length > 1 ? wormImageA.getExtents()[1] : 0;  
	                	int dimZ = wormImageA.getExtents().length > 2 ? wormImageA.getExtents()[2] : 0;
	                	maxExtents[0] = Math.max(maxExtents[0], dimX);
	                	maxExtents[1] = Math.max(maxExtents[1], dimY);
	                	maxExtents[2] = Math.max(maxExtents[2], dimZ);
    	            }    				
    			}
    			
    			System.err.println( maxExtents[0] + " " + maxExtents[1] + " " + maxExtents[2] );
    			setupOutputDirectories();
    			boolean first = true;
    			for ( int i = 0; i < includeRange.size(); i++ )
    			{
//    	    		String fileName = baseFileDir + File.separator + baseFileNameText.getText() + "_" + includeRange.elementAt(i) + ".tif";
    	    		String fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + "_straight" + ".tif";
    	            File voiFile = new File(baseFileDir + File.separator + fileName);
    	            if ( voiFile.exists() )
    	            {
    	            	System.err.println( fileName );
    	                FileIO fileIO = new FileIO();
    	                if((wormImageA != null) && (wormImageA != prevImageA)) {
    	                	wormImageA.disposeLocal();
    	                	wormImageA = null;
    	                }
    	                wormImageA = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);  
    	                

    	                TransMatrix matrix = null;
    	                String matName = "registration_" + includeRange.elementAt(i) + ".mtx";
    	                final File matFile = new File( matrixFileDir + File.separator + matName );
    	                if ( matFile.exists() )
    	                {
							try {
								RandomAccessFile raFile = new RandomAccessFile(matFile, "r");
								matrix = new TransMatrix(4);
	    	                	matrix.readMatrix(raFile, false);
	    	                	raFile.close();
					        } catch (final IOException error) {
//					            MipavUtil.displayError("Matrix read error");
					        	matrix = null;
					        }
    	                }
    	                
    	                register( registrationDir, first, matrix );
    	                if ( first )
    	                {
    	                	first = false;
    	                }
    	            }    				
    			}
    		}
    		else
    		{
    			int fileCount = 0;
    			boolean fileExists = true;
    			maxExtents = new int[]{0,0,0};
    			while ( fileExists )
    			{    	    	
    				String fileName = baseFileNameText.getText() + "_" + fileCount + "_straight"  + ".tif";
    				File voiFile = new File(baseFileDir + File.separator + fileName);
    	            if ( voiFile.exists() )
    	            {
    	            	System.err.println( fileName );
    	                FileIO fileIO = new FileIO();
    	                if(wormImageA != null) {
    	                	wormImageA.disposeLocal();
    	                	wormImageA = null;
    	                }
    	                wormImageA = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);  
	                	int dimX = wormImageA.getExtents().length > 0 ? wormImageA.getExtents()[0] : 0;  
	                	int dimY = wormImageA.getExtents().length > 1 ? wormImageA.getExtents()[1] : 0;  
	                	int dimZ = wormImageA.getExtents().length > 2 ? wormImageA.getExtents()[2] : 0;
	                	maxExtents[0] = Math.max(maxExtents[0], dimX);
	                	maxExtents[1] = Math.max(maxExtents[1], dimY);
	                	maxExtents[2] = Math.max(maxExtents[2], dimZ); 
	                	
        	    		fileCount++;
    	            }    				
    	            else
    	            {
    	            	fileExists = false;
    	            }
    			}
    			

    			System.err.println( maxExtents[0] + " " + maxExtents[1] + " " + maxExtents[2] );
    			setupOutputDirectories();
	            
    			fileCount = 0;
    			fileExists = true;
    			boolean first = true;
    			while ( fileExists )
    			{    	    	
    				String fileName = baseFileNameText.getText() + "_" + fileCount + "_straight"  + ".tif";
    				File voiFile = new File(baseFileDir + File.separator + fileName);
    	            if ( voiFile.exists() )
    	            {
    	            	System.err.println( fileName );
    	                FileIO fileIO = new FileIO();
    	                if((wormImageA != null) && (wormImageA != prevImageA)) {
    	                	wormImageA.disposeLocal();
    	                	wormImageA = null;
    	                }
    	                wormImageA = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);  

    	                TransMatrix matrix = null;
    	                String matName = "registration_" + fileCount + ".mtx";
    	                final File matFile = new File( matrixFileDir + File.separator + matName );
    	                if ( matFile.exists() )
    	                {
							try {
								RandomAccessFile raFile = new RandomAccessFile(matFile, "r");
								matrix = new TransMatrix(4);
	    	                	matrix.readMatrix(raFile, false);
	    	                	raFile.close();
					        } catch (final IOException error) {
//					            MipavUtil.displayError("Matrix read error");
					        	matrix = null;
					        }
    	                }
    	                register( registrationDir, first, matrix );
    	                if ( first )
    	                {
    	                	first = false;
    	                }
	                	
        	    		fileCount++;
    	            }    				
    	            else
    	            {
    	            	fileExists = false;
    	            }
    			}
    		}
    		MipavUtil.displayInfo( "Done Batch Registration" );
    	}
        if(wormImageA != null) {
        	wormImageA.disposeLocal();
        	wormImageA = null;
        }
        if(prevImageA != null) {
        	prevImageA.disposeLocal();
        	prevImageA = null;
        }
    	setVisible(false);
    }     

	private void init()
    {
    	setResizable(true);
    	setForeground(Color.black);
    	setTitle("Worm Registration");
    	try {
    		setIconImage(MipavUtil.getIconImage("divinci.gif"));
    	} catch (FileNotFoundException e) {
    		Preferences.debug("Failed to load default icon", Preferences.DEBUG_MINOR);
    	}

    	GuiBuilder gui = new GuiBuilder(this);

    	GridBagConstraints gbc = new GridBagConstraints();
    	gbc.gridwidth = 1;
    	gbc.gridheight = 1;
    	gbc.anchor = GridBagConstraints.WEST;
    	gbc.weightx = 1;
    	gbc.insets = new Insets(3, 3, 3, 3);
    	gbc.fill = GridBagConstraints.HORIZONTAL;
    	gbc.gridx = 0;
    	gbc.gridy = 0;

    	JPanel panel = new JPanel(new GridBagLayout());
    	panel.setBorder(buildTitledBorder("Input Options"));
    	panel.setForeground(Color.black);

    	baseFileLocText = gui.buildFileField("Directory containing input images: ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
    	panel.add(baseFileLocText.getParent(), gbc);
    	gbc.gridy++;
    	
    	matrixFileLocText = gui.buildFileField("Directory containing transformation matrices: ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
    	panel.add(matrixFileLocText.getParent(), gbc);
    	gbc.gridy++;

    	baseFileNameText = gui.buildField("Base images name: ", "Decon");
    	panel.add(baseFileNameText.getParent(), gbc);
    	gbc.gridy++;

    	rangeFusionText = gui.buildField("Range of images to segment (ex. 3-7, 12, 18-21, etc.): ", " ");
    	panel.add(rangeFusionText.getParent(), gbc);
    	gbc.gridy++;

    	getContentPane().add(panel, BorderLayout.NORTH);

    	gbc.gridx = 0;
    	gbc.gridy = 0;
    	panel = new JPanel(new GridBagLayout());
    	panel.setBorder(buildTitledBorder("Output Options"));
    	panel.setForeground(Color.black);
    	outputFileLocText = gui.buildFileField("Save images to: ", "", false, JFileChooser.DIRECTORIES_ONLY, this);
    	panel.add(outputFileLocText.getParent(), gbc);
    	gbc.gridy++;

    	getContentPane().add(panel, BorderLayout.CENTER);
    	
    	okCancelPanel = gui.buildOKCancelPanel();
    	getContentPane().add(okCancelPanel, BorderLayout.SOUTH);
    	
    	pack();
    	setResizable(true);

    	System.gc();

    }

	
	
	
	
	/**
     * This method could ensure everything in your dialog box has been set correctly
     * 
     * @return
     */
	private boolean setVariables()
	{	    
	    baseFileDir = baseFileLocText.getText();
	    matrixFileDir = matrixFileLocText.getText();
	    outputFileDir = outputFileLocText.getText();
//	    System.err.println( "matrixFileDir = " + matrixFileDir );
	    includeRange = new Vector<Integer>();
	    String rangeFusion = rangeFusionText.getText();
	    if(rangeFusion != null) {  
	        String[] ranges = rangeFusion.split("[,;]");
	        for(int i=0; i<ranges.length; i++) {
	            String[] subset = ranges[i].split("-");
	            int lowerBound = -1, bound = -1;
	            for(int j=0; j<subset.length; j++) {
	                try {
	                    bound = Integer.valueOf(subset[j].trim());
	                    if(lowerBound == -1) {
	                        lowerBound = bound;
	                        includeRange.add(lowerBound);
	                    } 
	                } catch(NumberFormatException e) {
	                    Preferences.debug("Invalid range specified: "+bound, Preferences.DEBUG_ALGORITHM);
	                }
	            }
	            
	            for(int k=lowerBound+1; k<=bound; k++) {
                    includeRange.add(k);
                }
	        }
	    }
	    
	    if(includeRange.size() == 0) {
	        includeRange = null;
	    }
	   
		return true;
	}
	
	private ModelImage getTransformedImage( AlgorithmRegOAR3D reg3, ModelImage refImage, ModelImage matchImage )
	{

		final int xdimA = refImage.getExtents()[0];
		final int ydimA = refImage.getExtents()[1];
		final int zdimA = refImage.getExtents()[2];
		final float xresA = refImage.getFileInfo(0).getResolutions()[0];
		final float yresA = refImage.getFileInfo(0).getResolutions()[1];
		final float zresA = refImage.getFileInfo(0).getResolutions()[2];
		if (reg3.isCompleted()) {
			final TransMatrix finalMatrix = reg3.getTransform();
			saveMatrix( finalMatrix, matchImage );

			final String name = JDialogBase.makeImageName(matchImage.getImageName(), "_register");

			AlgorithmTransform transform = new AlgorithmTransform(matchImage, finalMatrix, 0, xresA, yresA, zresA, xdimA,
					ydimA, zdimA, true, false, false);

			transform.setUpdateOriginFlag(true);
			transform.setFillValue((float)matchImage.getMin());
			transform.run();
			ModelImage resultImage = transform.getTransformedImage();
			if ( resultImage != null )
			{
				resultImage.calcMinMax();
				resultImage.setImageName(name);

				resultImage.getMatrixHolder().replaceMatrices(refImage.getMatrixHolder().getMatrices());

				for (int i = 0; i < resultImage.getExtents()[2]; i++) {
					resultImage.getFileInfo(i).setOrigin(refImage.getFileInfo(i).getOrigin());
				}
			}

			transform.finalize();
			if (transform != null) {
				transform.disposeLocal();
				transform = null;
			}
			return resultImage;
		}
		return null;

	}
	
	
	private ModelImage getTransformedImage( ModelImage refImage, ModelImage matchImage, TransMatrix finalMatrix )
	{

		final int xdimA = refImage.getExtents()[0];
		final int ydimA = refImage.getExtents()[1];
		final int zdimA = refImage.getExtents()[2];
		final float xresA = refImage.getFileInfo(0).getResolutions()[0];
		final float yresA = refImage.getFileInfo(0).getResolutions()[1];
		final float zresA = refImage.getFileInfo(0).getResolutions()[2];


		final String name = JDialogBase.makeImageName(matchImage.getImageName(), "_register");

		AlgorithmTransform transform = new AlgorithmTransform(matchImage, finalMatrix, 0, xresA, yresA, zresA, xdimA,
				ydimA, zdimA, true, false, false);

		transform.setUpdateOriginFlag(true);
		transform.setFillValue((float)matchImage.getMin());
		transform.run();
		ModelImage resultImage = transform.getTransformedImage();
		if ( resultImage != null )
		{
			resultImage.calcMinMax();
			resultImage.setImageName(name);

			resultImage.getMatrixHolder().replaceMatrices(refImage.getMatrixHolder().getMatrices());

			for (int i = 0; i < resultImage.getExtents()[2]; i++) {
				resultImage.getFileInfo(i).setOrigin(refImage.getFileInfo(i).getOrigin());
			}
		}

		transform.finalize();
		if (transform != null) {
			transform.disposeLocal();
			transform = null;
		}
		return resultImage;

	}
	
    private void saveTransformImage( File dir, ModelImage image  ) 
	{
    	String imageName = image.getImageName();
    	if ( imageName.contains("_pad") )
    	{
    		imageName = imageName.replaceAll("_pad", "" );
    	}
    	if ( imageName.contains("_register") )
    	{
    		imageName = imageName.replaceAll("_register", "" );
    	}
		String voiDir = dir.getAbsolutePath() + File.separator;
//        ModelImage.saveImage( image, image.getImageName() + ".xml", voiDir );
//		System.err.println( imageName + ".tif" + "   " + voiDir );
		image.setImageName(imageName);
        ModelImage.saveImage( image, imageName + ".tif", voiDir, false ); 
    }

	
    private void saveMatrix( TransMatrix matrix, ModelImage image  ) 
	{
    	String imageName = image.getImageName();
    	if ( imageName.contains("_pad") )
    	{
    		imageName = imageName.replaceAll("_pad", "" );
    	}
    	if ( imageName.contains("_register") )
    	{
    		imageName = imageName.replaceAll("_register", "" );
    	}
    	if ( imageName.contains("Decon_") )
    	{
    		imageName = imageName.replaceAll("Decon_", "" );
    	}
    	if ( imageName.contains("Neuron_") )
    	{
    		imageName = imageName.replaceAll("Neuron_", "" );
    	}
    	if ( imageName.contains("_straight") )
    	{
    		imageName = imageName.replaceAll("_straight", "" );
    	}
		String matDir = matrixDir.getAbsolutePath() + File.separator;
		matrix.saveMatrix( matDir + "registration_" + imageName + ".mtx" );
    }
    
    private void setupOutputDirectories()
    {
        registrationDir = new File(outputFileDir);
        if ( !registrationDir.exists() )
        {
        	registrationDir.mkdir();
        }
        else 
        {
        	if ( includeRange != null )
        	{
        		String[] list = registrationDir.list();
        		for ( int i = 0; i < list.length; i++ )
        		{
        			for ( int j = 0; j < includeRange.size(); j++ )
        			{
        				if ( list[i].contains("_" + includeRange.elementAt(j) + "_straight") )
        				{
//        					System.err.println( "Deleting " + list[i] );
        					File regFile = new File( outputFileDir + File.separator + "registration" + File.separator + list[i] );
        					regFile.delete();
        					break;
        				}
        			}
        		}
        	}
        	else
        	{
        		String[] list = registrationDir.list();
        		for ( int i = 0; i < list.length; i++ )
        		{
//        			System.err.println( "Deleting " + list[i] );
        			File regFile = new File( outputFileDir + File.separator + "registration" + File.separator + list[i] );
        			regFile.delete();
        		}
        	}
        }
        
        System.err.println( matrixFileDir.equals("") );
        if ( !matrixFileDir.equals("") )
        {
        	return;
        }
        matrixDir = new File(baseFileDir + File.separator + "matrix");
        if ( !matrixDir.exists() )
        {
        	matrixDir.mkdir();
        }
        else
        {
        	if ( includeRange != null )
        	{
        		String[] list = matrixDir.list();
        		for ( int i = 0; i < list.length; i++ )
        		{
        			for ( int j = 0; j < includeRange.size(); j++ )
        			{
        				if ( list[i].contains("registration_" + includeRange.elementAt(j) ) )
        				{
//        					System.err.println( "Deleting " + list[i] );
        					File regFile = new File( baseFileDir + File.separator + "matrix" + File.separator + list[i] );
        					regFile.delete();
        					break;
        				}
        			}
        		}
        	}
        	else
        	{
        		String[] list = matrixDir.list();
        		for ( int i = 0; i < list.length; i++ )
        		{
        			System.err.println( "Deleting " + list[i] );
        			File regFile = new File( baseFileDir + File.separator + "matrix" + File.separator + list[i] );
        			regFile.delete();
        		}
        	}
        }
    }
    
    
    private void register( File registrationDir, boolean first, TransMatrix matrix )
    {
		int[] marginX = new int[2];
		int[] marginY = new int[2];
		int[] marginZ = new int[2];

    	int dimX = wormImageA.getExtents().length > 0 ? wormImageA.getExtents()[0] : 0;  
    	int dimY = wormImageA.getExtents().length > 1 ? wormImageA.getExtents()[1] : 0;  
    	int dimZ = wormImageA.getExtents().length > 2 ? wormImageA.getExtents()[2] : 0;

    	if ( dimX != maxExtents[0] || dimY != maxExtents[1] || dimZ != maxExtents[2] )
    	{
    		int diff = maxExtents[0] - dimX;
    		marginX[0] = diff/2;
    		marginX[1] = diff - marginX[0];
    		
    		diff = maxExtents[1] - dimY;
    		marginY[0] = diff/2;
    		marginY[1] = diff - marginY[0];
    		
    		diff = maxExtents[2] - dimZ;
    		marginZ[0] = 0;
    		marginZ[1] = diff;
    		

    		ModelImage destImage = new ModelImage(wormImageA.getType(), maxExtents, wormImageA.getImageName() + "_pad" );
    		JDialogBase.updateFileInfo( wormImageA, destImage );
    		AlgorithmAddMargins pad = new AlgorithmAddMargins(wormImageA, destImage, marginX, marginY, marginZ );
    		pad.setRunningInSeparateThread(false);
    		pad.run();
    		if ( first )
    		{
    			saveTransformImage( registrationDir, destImage );
		        if(prevImageA != null) {
		        	prevImageA.disposeLocal();
		        	prevImageA = null;
		        }
    			prevImageA = destImage;
    			first = false;
    		}
    		else if ( matrix == null )
    		{
    			AlgorithmRegOAR3D reg = new AlgorithmRegOAR3D(prevImageA, destImage,
						AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED, 6, 0,
						-30, 30, 15, 6, -30, 30, 15, 6, -30, 30, 15, 6, true, true, false, 
						false, 2, 3);
    			reg.setRunningInSeparateThread(false);
    			reg.run();
    			ModelImage result = getTransformedImage( reg, prevImageA, destImage );
    			if ( result != null )
    			{
    				saveTransformImage( registrationDir, result );
    		        if(prevImageA != null) {
    		        	prevImageA.disposeLocal();
    		        	prevImageA = null;
    		        }
    				prevImageA = result;
    			}
    			reg.finalize();
    			reg = null;

    			destImage.disposeLocal();
    			destImage = null;
    		}
    		else if ( matrix != null )
    		{
    			ModelImage result = getTransformedImage( prevImageA, destImage, matrix );
    			if ( result != null )
    			{
    				saveTransformImage( registrationDir, result );
    		        if(prevImageA != null) {
    		        	prevImageA.disposeLocal();
    		        	prevImageA = null;
    		        }
    				prevImageA = result;
    			}

    			destImage.disposeLocal();
    			destImage = null;
    		}
    		pad.finalize();
    		pad = null;
    	}
    	else
    	{
    		if ( first )
    		{
    			saveTransformImage( registrationDir, wormImageA );
		        if(prevImageA != null) {
		        	prevImageA.disposeLocal();
		        	prevImageA = null;
		        }
    			prevImageA = wormImageA;
    			first = false;
    		}
    		else if ( prevImageA != null )
    		{
    			if ( matrix == null )
    			{
    				AlgorithmRegOAR3D reg = new AlgorithmRegOAR3D(prevImageA, wormImageA, 
    						AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED, 6, 0,
    						-30, 30, 15, 6, -30, 30, 15, 6, -30, 30, 15, 6, true, true, false, 
    						false, 2, 3);
    				reg.setRunningInSeparateThread(false);
    				reg.run();
    				ModelImage result = getTransformedImage( reg, prevImageA, wormImageA );
    				if ( result != null )
    				{
    					saveTransformImage( registrationDir, result );

    					if(prevImageA != null) {
    						prevImageA.disposeLocal();
    						prevImageA = null;
    					}
    					prevImageA = result;
    				}
    				reg.finalize();
    				reg = null;
    			}
        		else if ( matrix != null )
        		{
        			ModelImage result = getTransformedImage( prevImageA, wormImageA, matrix );
        			if ( result != null )
        			{
        				saveTransformImage( registrationDir, result );
        		        if(prevImageA != null) {
        		        	prevImageA.disposeLocal();
        		        	prevImageA = null;
        		        }
        				prevImageA = result;
        			}
        		}
    		}
    	}
    }
	
}
