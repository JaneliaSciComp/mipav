
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
import gov.nih.mipav.model.algorithms.utilities.AlgorithmMaximumIntensityProjection;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
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
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.HashSet;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.JTextField;

import WildMagic.LibFoundation.Mathematics.Vector3f;

public class PlugInDialogWormLatticeStraighten extends JDialogStandalonePlugin implements AlgorithmInterface {

	private static final long serialVersionUID = 2476025001402032629L;

	/** This source image is typically set by the constructor */
	private ModelImage wormImageA;    
	private ModelImage wormImageB;    
    private ModelImage prevImageA;     
    private ModelImage maximumProjectionImage;
    private int currentMP;
	private JTextField  baseFileLocText;
	private JTextField  baseFileNameText;
	private JTextField rangeFusionText;
	private JPanel okCancelPanel;    
	private String baseFileDir;
	private Vector<Integer> includeRange;

	private JCheckBox includeNuclearImage;
	private JTextField  nuclearFileNameText;

	private JCheckBox latticeStraighten;
	private JCheckBox calcStatistics;
	private JCheckBox registerImages;
	private JCheckBox calcMaxProjection;

	public PlugInDialogWormLatticeStraighten() {}

	public PlugInDialogWormLatticeStraighten(boolean modal)
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
			if ( latticeStraighten.isSelected() )
			{
				latticeStraighten();
			}
			if ( calcStatistics.isSelected() )
			{
				calcStatistics();
			}
			if ( registerImages.isSelected() )
			{
				registerImages();
			}
			if ( calcMaxProjection.isSelected() )
			{
				calcMaxProjection();
			}
		}
		setVisible(false);
	}

	private void latticeStraighten()
	{
		if ( includeRange != null )
		{
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				//    	    		String fileName = baseFileDir + File.separator + baseFileNameText.getText() + "_" + includeRange.elementAt(i) + ".tif";
				String fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{
					System.err.println( fileName );
					FileIO fileIO = new FileIO();
					if(wormImageA != null) {
						if ( (i%10) == 0 )
						{
							wormImageA.disposeLocal(true);
						}
						else
						{
							wormImageA.disposeLocal();
						}
						wormImageA = null;
					}
					wormImageA = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);  
					if(wormImageB != null) {
						wormImageB.disposeLocal();
						wormImageB = null;
					}
					if ( includeNuclearImage.isSelected() )
					{    	    	
						fileName = nuclearFileNameText.getText() + "_" + includeRange.elementAt(i) + ".tif";
						voiFile = new File(baseFileDir + File.separator + fileName);
						if ( voiFile.exists() )
						{
							fileIO = new FileIO();
							wormImageB = fileIO.readImage(fileName, baseFileDir + File.separator, false, null); 
						}
					}

					fileName = baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + "lattice_1";
					VOIVector lattice = new VOIVector();
					String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
					loadAllVOIsFrom(voiDir, false, lattice, false);

					if ( (lattice.elementAt(0) != null) && (lattice.elementAt(0).getCurves().size() == 2) )
					{
						LatticeModel model = new LatticeModel( wormImageA, wormImageB, lattice.elementAt(0) );

						fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + File.separator + "annotations";            	    		
						VOIVector annotations = new VOIVector();
						voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
						loadAllVOIsFrom(voiDir, true, annotations, false);
						if ( annotations.size() > 0 )
						{
							model.setAnnotations( annotations.elementAt(0) );
						}
						model.interpolateLattice( false );
						model.dispose();
						model = null;
					}
				}    				
			}
		}
		else
		{
			int fileCount = 0;
			boolean fileExists = true;
			while ( fileExists )
			{    	    	
				String fileName = baseFileNameText.getText() + "_" + fileCount + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{
					System.err.println( fileName );
					FileIO fileIO = new FileIO();
					if(wormImageA != null) {
						if ( (fileCount%10) == 0 )
						{
							wormImageA.disposeLocal(true);
						}
						else
						{
							wormImageA.disposeLocal();
						}
						wormImageA = null;
					}
					wormImageA = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);  
					if(wormImageB != null) {
						wormImageB.disposeLocal();
						wormImageB = null;
					}
					if ( includeNuclearImage.isSelected() )
					{    	    	
						fileName = nuclearFileNameText.getText() + "_" + fileCount + ".tif";
						voiFile = new File(baseFileDir + File.separator + fileName);
						if ( voiFile.exists() )
						{
							fileIO = new FileIO();
							wormImageB = fileIO.readImage(fileName, baseFileDir + File.separator, false, null); 
						}
					}

					fileName = baseFileNameText.getText() + "_" + fileCount + File.separator + "lattice_1";
					VOIVector lattice = new VOIVector();
					String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
					loadAllVOIsFrom(voiDir, false, lattice, false);

					if ( (lattice.elementAt(0) != null) && (lattice.elementAt(0).getCurves().size() == 2) )
					{
						LatticeModel model = new LatticeModel( wormImageA, wormImageB, lattice.elementAt(0) );

						fileName = baseFileNameText.getText() + "_" + fileCount + File.separator + "annotations";            	    		
						VOIVector annotations = new VOIVector();
						voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
						loadAllVOIsFrom(voiDir, true, annotations, false);
						if ( annotations.size() > 0 )
						{
							model.setAnnotations( annotations.elementAt(0) );
						}

						model.interpolateLattice( false );
						model.dispose();
						model = null;
					}
					fileCount++;
				}    				
				else
				{
					fileExists = false;
				}
			}
		}

		if(wormImageA != null) {
			wormImageA.disposeLocal();
			wormImageA = null;
		}
		if(wormImageB != null) {
			wormImageB.disposeLocal();
			wormImageB = null;
		}
	}

	private void calcStatistics()
	{
		if ( includeRange != null )
		{
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String fileName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + "statistics" + File.separator + "LatticePositions_before.csv";
				readLatticePositions( fileName, includeRange.elementAt(i) );
			}
		}
		else
		{
			int fileCount = 0;
			boolean fileExists = true;
			while ( fileExists )
			{    	    	
				String fileName = baseFileNameText.getText() + "_" + fileCount + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{
					fileName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + fileCount + File.separator + "statistics" + File.separator + "LatticePositions_before.csv";
					readLatticePositions( fileName, fileCount );
					fileCount++;
				}    				
				else
				{
					fileExists = false;
				}
			}
		}

		if ( (leftPositions != null) && (rightPositions != null) )
		{
			String fileName = baseFileDir + File.separator + "stats";
			calcStatistics(fileName);

			leftPositions = null;
			rightPositions = null;
			IDs = null;
			volumes = null;
		}
	}     

	private void registerImages()
	{

		long startTime = System.currentTimeMillis();
		int[] maxExtents = new int[]{0,0,0};
		if ( includeRange != null )
		{
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String fileName = baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + 
						"output_images" + File.separator + baseFileNameText.getText() + "_" + includeRange.elementAt(i) + "_straight" + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{
					System.err.println( fileName );
					FileIO fileIO = new FileIO();
					if(wormImageA != null) {
						if ( (i%10) == 0 )
						{
							wormImageA.disposeLocal(true);
						}
						else
						{
							wormImageA.disposeLocal();
						}
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
			
			boolean first = true;
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String fileName = baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + 
						"output_images" + File.separator + baseFileNameText.getText() + "_" + includeRange.elementAt(i) + "_straight" + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				
				File registrationDir = new File(baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + 
						"output_images");
				if ( voiFile.exists() )
				{
					System.err.println( fileName );
					FileIO fileIO = new FileIO();
					if((wormImageA != null) && (wormImageA != prevImageA)) {
						if ( (i%10) == 0 )
						{
							wormImageA.disposeLocal(true);
						}
						else
						{
							wormImageA.disposeLocal();
						}
						wormImageA = null;
					}
					wormImageA = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);  

					register( registrationDir, first, maxExtents );
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
				String fileName = baseFileNameText.getText() + "_"  + fileCount + File.separator + 
						"output_images" + File.separator + baseFileNameText.getText() + "_" + fileCount + "_straight" + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{
					System.err.println( fileName );
					FileIO fileIO = new FileIO();
					if(wormImageA != null) {
						if ( (fileCount%10) == 0 )
						{
							wormImageA.disposeLocal(true);
						}
						else
						{
							wormImageA.disposeLocal();
						}
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

			fileCount = 0;
			fileExists = true;
			boolean first = true;
			while ( fileExists )
			{    	    	
				String fileName = baseFileNameText.getText() + "_"  + fileCount + File.separator + 
						"output_images" + File.separator + baseFileNameText.getText() + "_" + fileCount + "_straight" + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				
				File registrationDir = new File(baseFileDir + File.separator + baseFileNameText.getText() + "_"  + fileCount + File.separator + 
						"output_images");
				if ( voiFile.exists() )
				{
					System.err.println( fileName );
					FileIO fileIO = new FileIO();
					if((wormImageA != null) && (wormImageA != prevImageA)) {
						if ( (fileCount%10) == 0 )
						{
							wormImageA.disposeLocal(true);
						}
						else
						{
							wormImageA.disposeLocal();
						}
						wormImageA = null;
					}
					wormImageA = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);  

					register( registrationDir, first, maxExtents );
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

		if(wormImageA != null) {
			wormImageA.disposeLocal();
			wormImageA = null;
		}
		if(prevImageA != null) {
			prevImageA.disposeLocal();
			prevImageA = null;
		}
		

        long now = System.currentTimeMillis();

        double elapsedTime = (double) (now - startTime);

        // if elasedTime is invalid, then set it to 0
        if (elapsedTime <= 0) {
            elapsedTime = (double) 0.0;
        }

        System.err.println( "Elapsed time = " + (elapsedTime / 1000.0) ); // return in seconds!!
	}

	
	private void calcMaxProjection()
	{
		if ( includeRange != null )
		{
			int mpSliceCount = 0;
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String fileName = baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + 
						"output_images" + File.separator + baseFileNameText.getText() + "_" + includeRange.elementAt(i) + "_straight_register" + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{					
					mpSliceCount++;
				}    				
			}
			
			boolean first = true;
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String fileName = baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + 
						"output_images" + File.separator + baseFileNameText.getText() + "_" + includeRange.elementAt(i) + "_straight_register" + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				
				if ( voiFile.exists() )
				{
					FileIO fileIO = new FileIO();
					if((wormImageA != null) && (wormImageA != prevImageA)) {
						if ( (i%10) == 0 )
						{
							wormImageA.disposeLocal(true);
						}
						else
						{
							wormImageA.disposeLocal();
						}
						wormImageA = null;
					}
					wormImageA = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);  

					if ( first )
					{				
						int dimX = wormImageA.getExtents().length > 0 ? wormImageA.getExtents()[0] : 1; // dimX
						int dimY = wormImageA.getExtents().length > 2 ? wormImageA.getExtents()[2] : 1; // use dimZ for y projection
						maximumProjectionImage = new ModelImage( wormImageA.getType(), new int[]{ dimX, dimY, mpSliceCount}, baseFileNameText.getText() + "_MP_Y.tif" );
						currentMP = 0;
						first = false;
					}
			        calcMaximumProjectionY( wormImageA );
				}    				
			}
		}
		else
		{
			int mpSliceCount = 0;
			boolean fileExists = true;
			while ( fileExists )
			{    	    	
				String fileName = baseFileNameText.getText() + "_"  + mpSliceCount + File.separator + 
						"output_images" + File.separator + baseFileNameText.getText() + "_" + mpSliceCount + "_straight_register" + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{
					mpSliceCount++;
				}    				
				else
				{
					fileExists = false;
				}
			}
			int fileCount = 0;
			fileExists = true;
			boolean first = true;
			while ( fileExists )
			{    	    	
				String fileName = baseFileNameText.getText() + "_"  + fileCount + File.separator + 
						"output_images" + File.separator + baseFileNameText.getText() + "_" + fileCount + "_straight_register" + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				
				if ( voiFile.exists() )
				{
					FileIO fileIO = new FileIO();
					if((wormImageA != null) && (wormImageA != prevImageA)) {
						if ( (fileCount%10) == 0 )
						{
							wormImageA.disposeLocal(true);
						}
						else
						{
							wormImageA.disposeLocal();
						}
						wormImageA = null;
					}
					wormImageA = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);  

					if ( first )
					{
						int dimX = wormImageA.getExtents().length > 0 ? wormImageA.getExtents()[0] : 1; // dimX
						int dimY = wormImageA.getExtents().length > 2 ? wormImageA.getExtents()[2] : 1; // use dimZ for y projection
						maximumProjectionImage = new ModelImage( wormImageA.getType(), new int[]{ dimX, dimY, mpSliceCount}, baseFileNameText.getText() + "_MP_Y.tif" );
						currentMP = 0;
						first = false;
					}
			        calcMaximumProjectionY( wormImageA );

					fileCount++;
				}    				
				else
				{
					fileExists = false;
				}
			}
		}

		if( wormImageA != null )
		{
			wormImageA.disposeLocal();
			wormImageA = null;
		}
		
		if ( maximumProjectionImage != null )
		{
			String fileName = baseFileDir + File.separator;
	        ModelImage.saveImage( maximumProjectionImage, maximumProjectionImage.getImageName() + ".tif", fileName, false ); 
			maximumProjectionImage.calcMinMax();
			new ViewJFrameImage(maximumProjectionImage);
		}
	}

	
	
    private void register( File registrationDir, boolean first, int[] maxExtents )
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
    		else
    		{
    			AlgorithmRegOAR3D reg = new AlgorithmRegOAR3D(prevImageA, destImage,
    					AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED, 6, 0,
    					-10, 10, 5, 2, -10, 10, 5, 2, -15, 15, 7, 2, true, true, true, 
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
    			AlgorithmRegOAR3D reg = new AlgorithmRegOAR3D(prevImageA, wormImageA, 
    					AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED, 6, 0,
    					-10, 10, 5, 2, -10, 10, 5, 2, -15, 15, 7, 2, true, true, true, 
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
    	}
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
    	imageName = imageName + "_register";
		String voiDir = dir.getAbsolutePath() + File.separator;
//        ModelImage.saveImage( image, image.getImageName() + ".xml", voiDir );
//		System.err.println( imageName + ".tif" + "   " + voiDir );
		image.setImageName(imageName);
        ModelImage.saveImage( image, imageName + ".tif", voiDir, false ); 
    }

    private void calcMaximumProjectionY( ModelImage image )
    {
    	int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
    	AlgorithmMaximumIntensityProjection mipAlgo = null;
    	// Make algorithm
    	if (image.isColorImage()) {
    		mipAlgo = new AlgorithmMaximumIntensityProjection(image, 
    				0, dimY -1, dimY,
    				image.getMinR(), image.getMaxR(),
    				image.getMinG(), image.getMaxG(), 
    				image.getMinB(), image.getMaxB(),
    				true, false, 1);    
    	}
    	else {
    		mipAlgo = new AlgorithmMaximumIntensityProjection(image, 
    				0, dimY -1, dimY,
    				image.getMin(), image.getMax(),
    				true, false, 1 );
    	}
    	mipAlgo.setRunningInSeparateThread(false);
    	mipAlgo.run();

		Vector<ModelImage> resultImages = ((AlgorithmMaximumIntensityProjection)mipAlgo).getResultImage();
		if ( resultImages.size() > 0 )
		{
			ModelImage mp = resultImages.elementAt(0);
			int dimX = maximumProjectionImage.getExtents().length > 0 ? maximumProjectionImage.getExtents()[0] : 1;
			dimY = maximumProjectionImage.getExtents().length > 1 ? maximumProjectionImage.getExtents()[1] : 1;
			
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					maximumProjectionImage.set(x,  y, currentMP, mp.getFloat(x, y, 0) );
				}
			}

			currentMP++;
		}
		mipAlgo.finalize();
    }
	
	
	private void init()
	{
		setResizable(true);
		setForeground(Color.black);
		setTitle("Lattice Straighten 1.0");
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

		includeNuclearImage = gui.buildCheckBox("include nuclear image", false );
		panel.add(includeNuclearImage.getParent(), gbc);
		gbc.gridy++;

		nuclearFileNameText = gui.buildField("Nuclear image name: ", "Neuron");
		panel.add(nuclearFileNameText.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		latticeStraighten = gui.buildCheckBox("Straighten Lattices", true );
		panel.add(latticeStraighten.getParent(), gbc);
		gbc.gridy++;
		
		
		gbc.gridx = 0;
		calcStatistics = gui.buildCheckBox("calculate staticstics", false );
		panel.add(calcStatistics.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		registerImages = gui.buildCheckBox("register images", false );
		panel.add(registerImages.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		calcMaxProjection = gui.buildCheckBox("calculate and display maximum intensity projections", false );
		panel.add(calcMaxProjection.getParent(), gbc);
		gbc.gridy++;

		getContentPane().add(panel, BorderLayout.CENTER);

		okCancelPanel = gui.buildOKCancelPanel();
		getContentPane().add(okCancelPanel, BorderLayout.SOUTH);

		pack();
		setResizable(true);

		System.gc();

	}



	/**
	 * This method loads all VOIs to the active image from a given directory.
	 * @param voiDir the directory to load voi's from
	 * @param quietMode if true indicates that warnings should not be displayed.
	 */
	private void loadAllVOIsFrom(final String voiDir, boolean quietMode, VOIVector resultVector, boolean registerVOIs) {

		int i, j;
		VOI[] VOIs;
		FileVOI fileVOI;

		try {

			// if voiDir does not exist, then return
			// if voiDir exists, then get list of voi's from directory (*.voi)
			final File voiFileDir = new File(voiDir);
			final Vector<String> filenames = new Vector<String>();
			final Vector<Boolean> isLabel = new Vector<Boolean>();

			if (voiFileDir.exists() && voiFileDir.isDirectory()) {

				// get list of files
				final File[] files = voiFileDir.listFiles();

				for (final File element : files) {

					if (element.getName().endsWith(".voi") || element.getName().endsWith(".xml")) {
						filenames.add(element.getName());
						isLabel.add(false);
					} else if (element.getName().endsWith(".lbl")) {
						filenames.add(element.getName());
						isLabel.add(true);
					}
				}
			} else { // voiFileDir either doesn't exist, or isn't a directory

				if ( !quietMode) {
					MipavUtil.displayError("No VOIs are found in directory: " + voiDir);
				}

				return;
			}

			// open each voi array, then register voi array to this image
			for (i = 0; i < filenames.size(); i++) {

				fileVOI = new FileVOI( (filenames.elementAt(i)), voiDir, wormImageA);

				VOIs = fileVOI.readVOI(isLabel.get(i));

				for (j = 0; j < VOIs.length; j++) {

					if ( resultVector != null )
					{
						resultVector.add(VOIs[j]);
					}
				}
			}

		} catch (final Exception error) {

			if ( !quietMode) {
				MipavUtil.displayError("Error loading all VOIs from " + voiDir + ": " + error);
			}
		}

	} // end loadAllVOIsFrom()



	/**
	 * This method could ensure everything in your dialog box has been set correctly
	 * 
	 * @return
	 */
	private boolean setVariables()
	{	    
		baseFileDir = baseFileLocText.getText();
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

	Vector<VOIContour> leftPositions = null;
	Vector<VOIContour> rightPositions = null;
	Vector<Integer> IDs = null;
	Vector<Integer> volumes = null;
	private void readLatticePositions( String fileName, int ID )
	{
		if ( leftPositions == null )
		{
			leftPositions = new Vector<VOIContour>();
			rightPositions = new Vector<VOIContour>();
			IDs = new Vector<Integer>();
			volumes = new Vector<Integer>();
		}
		File file = new File(fileName);
		if ( file.exists() )
		{
			//        	System.err.println( fileName );
			FileReader fr;
			try {
				fr = new FileReader(file);
				BufferedReader br = new BufferedReader(fr);
				String line = br.readLine();
				line = br.readLine();
				VOIContour left = new VOIContour(false);
				VOIContour right = new VOIContour(false);
				while ( line != null )
				{
					float volume;
					Vector3f pos = new Vector3f();
					StringTokenizer st = new StringTokenizer(line, ",");
					if (st.hasMoreTokens()) {
						String name = st.nextToken();
					}
					if (st.hasMoreTokens()) {
						pos.X = Float.valueOf(st.nextToken());
					}
					if (st.hasMoreTokens()) {
						pos.Y = Float.valueOf(st.nextToken());
					}
					if (st.hasMoreTokens()) {
						pos.Z = Float.valueOf(st.nextToken());
					}
					if (st.hasMoreTokens()) {
						float temp = Float.valueOf(st.nextToken());
					}
					if (st.hasMoreTokens()) {
						float temp = Float.valueOf(st.nextToken());
					}
					if (st.hasMoreTokens()) {
						float temp = Float.valueOf(st.nextToken());
					}
					if (st.hasMoreTokens()) {
						volume = Float.valueOf(st.nextToken());
					}
					left.add(pos);

					line = br.readLine();
					if ( line != null )
					{
						pos = new Vector3f();
						st = new StringTokenizer(line, ",");
						if (st.hasMoreTokens()) {
							String name = st.nextToken();
						}
						if (st.hasMoreTokens()) {
							pos.X = Float.valueOf(st.nextToken());
						}
						if (st.hasMoreTokens()) {
							pos.Y = Float.valueOf(st.nextToken());
						}
						if (st.hasMoreTokens()) {
							pos.Z = Float.valueOf(st.nextToken());
						}
						if (st.hasMoreTokens()) {
							float temp = Float.valueOf(st.nextToken());
						}
						if (st.hasMoreTokens()) {
							float temp = Float.valueOf(st.nextToken());
						}
						if (st.hasMoreTokens()) {
							float temp = Float.valueOf(st.nextToken());
						}
						if (st.hasMoreTokens()) {
							volume = Float.valueOf(st.nextToken());
						}
						right.add(pos);		        		
					}
					line = br.readLine();
				}
				leftPositions.add(left);
				rightPositions.add(right);
				IDs.add(ID);
				fr.close();
			} catch (FileNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

		}
	}

	private void calcStatistics( String fileName )
	{

		File file = new File(fileName + ".csv");
		if ( file.exists() )
		{
			file.delete();
			file = new File(fileName + ".csv");
		}
		FileWriter fw;
		try {
			fw = new FileWriter(file);
			BufferedWriter bw = new BufferedWriter(fw);
			bw.write( "ID" + "," + "minPair" + "," + "maxPair" + "," +  "average_pair" + "," + "minDistance" + "," + "maxDistance" + "\n" );


			for ( int i = 0; i < Math.min( IDs.size(), Math.min(leftPositions.size(), rightPositions.size())); i++ )
			{
				double averageDistance = 0;
				int count = 0;
				double minPair = Float.MAX_VALUE;
				double maxPair = -Float.MAX_VALUE;
				double minDistance = Float.MAX_VALUE;
				double maxDistance = -Float.MAX_VALUE;

				// average pair distance:
				VOIContour left = leftPositions.elementAt(i);
				VOIContour right = rightPositions.elementAt(i);

				for ( int j = 0; j < Math.min(left.size(), right.size()); j++ )
				{
					float distance = left.elementAt(j).distance(right.elementAt(j) );
					if ( distance < minPair )
					{
						minPair = distance;
					}
					if ( distance > maxPair )
					{
						maxPair = distance;
					}

					averageDistance += distance;
					count++;
				}

				// min distances overall:
				for ( int j = 0; j < left.size(); j++ )
				{
					for ( int k = j+1; k < left.size(); k++ )
					{
						float distance = left.elementAt(j).distance(left.elementAt(k) );
						if ( distance < minDistance )
						{
							minDistance = distance;
						}
						if ( distance > maxDistance )
						{
							maxDistance = distance;
						}
					}
					for ( int k = 0; k < right.size(); k++ )
					{
						if ( j == k ) // don't include pairs:
							continue;
						float distance = left.elementAt(j).distance(right.elementAt(k) );
						if ( distance < minDistance )
						{
							minDistance = distance;
						}
						if ( distance > maxDistance )
						{
							maxDistance = distance;
						}
					}
				}
				for ( int j = 0; j < right.size(); j++ )
				{
					for ( int k = j+1; k < right.size(); k++ )
					{
						float distance = right.elementAt(j).distance(right.elementAt(k) );
						if ( distance < minDistance )
						{
							minDistance = distance;
						}
						if ( distance > maxDistance )
						{
							maxDistance = distance;
						}
					}
					for ( int k = 0; k < left.size(); k++ )
					{
						if ( j == k ) // don't include pairs:
							continue;
						float distance = right.elementAt(j).distance(left.elementAt(k) );
						if ( distance < minDistance )
						{
							minDistance = distance;
						}
						if ( distance > maxDistance )
						{
							maxDistance = distance;
						}
					}
				}
				//			System.err.println( "Average pair distance = " + averageDistance / (double)count );
				//			System.err.println( "Minimum distance overall = " + minDistance );	
				bw.write( IDs.elementAt(i) + "," + minPair + "," + maxPair +  "," + averageDistance / (double)count + "," + minDistance + "," + maxDistance + "\n" );
			}
			bw.close();

		} catch (IOException e) {
		}
	}

}
