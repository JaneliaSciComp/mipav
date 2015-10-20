
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
import gov.nih.mipav.model.algorithms.registration.AlgorithmConstrainedOAR3D;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR3D;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmMaximumIntensityProjection;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelImageToImageJConversion;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.plugins.JDialogStandaloneScriptablePlugin;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;
import gov.nih.mipav.view.renderer.WildMagic.Interface.SurfaceState;
import gov.nih.mipav.view.renderer.WildMagic.Render.LatticeModel;
import gov.nih.mipav.view.renderer.WildMagic.Render.VolumeSurface;
import gov.nih.mipav.view.renderer.WildMagic.Render.WormSegmentation;
import gov.nih.mipav.view.renderer.WildMagic.Render.WormSegmentationKMeans;
import gov.nih.mipav.view.renderer.WildMagic.Render.WormSegmentationLoG;
import gov.nih.mipav.view.renderer.WildMagic.Render.WormSegmentationWindowing;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOILatticeManagerInterface;

import ij.ImagePlus;
import ij.ImageStack;

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
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Random;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.JProgressBar;
import javax.swing.JTextField;

import WildMagic.LibFoundation.Containment.ContBox3f;
import WildMagic.LibFoundation.Mathematics.Box3f;
import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector2d;
import WildMagic.LibFoundation.Mathematics.Vector3d;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector4f;
import WildMagic.LibGraphics.SceneGraph.TriMesh;

public class PlugInDialogWormLatticeStraighten extends JDialogStandalonePlugin implements AlgorithmInterface {

	private static final long serialVersionUID = 2476025001402032629L;

	/** This source image is typically set by the constructor */
	private ModelImage wormImageA;     
	private ModelImage maximumProjectionImage;
	private int currentMP;
	private JTextField  baseFileLocText;
	private JTextField  baseFileNameText;
	private JTextField rangeFusionText;
	private JPanel okCancelPanel;    
	private String baseFileDir;
	private Vector<Integer> includeRange;

	private JCheckBox latticeStraighten;
	private JCheckBox buildLattice;
	private JCheckBox calcStatistics;
	private JCheckBox generateTrainingData;
	private JCheckBox segmentSeamCells;
	private JCheckBox segmentAnnotations;
	private JCheckBox calcAnnotationStats;
	private JCheckBox registerImages;
	private JCheckBox calcMaxProjection;
	private JCheckBox annotationAnimation;
	private JCheckBox annotationAnimationFromSpreadSheet;
	private JCheckBox clean;
	private JCheckBox flipLattices;
	private VolumeTriPlanarInterface triVolume;
	private VOIVector annotationList;

	public static final String autoSeamCellSegmentationOutput = new String("seam_cells");
	public static final String editSeamCellOutput = new String("seam_cell_final");
	public static final String autoLatticeGenerationOutput = new String("lattice_");
	public static final String editLatticeOutput = new String("lattice_final");
	public static final String editAnnotationInput = new String("annotation");
	public static final String editAnnotationOutput = new String("annotation_final");
	
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
	public void algorithmPerformed(AlgorithmBase algorithm) 
	{ 
		triVolume.addVOIS( annotationList, annotationNames );
		triVolume.displayAnnotationSpheres();
		triVolume.display3DWindowOnly();
	} 

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
			if ( clean.isSelected() )
			{
				clean();
			}
			if ( segmentSeamCells.isSelected() )
			{
//				segmentOutline();
				segmentSeamCells( null, includeRange, baseFileDir, baseFileNameText.getText() );
//				seamCellInfo();
			}
			if ( buildLattice.isSelected() )
			{
				buildLattice( null, includeRange,  baseFileDir, baseFileNameText.getText() );
			}
			if ( latticeStraighten.isSelected() )
			{
				latticeStraighten( null, includeRange,  baseFileDir, baseFileNameText.getText() );
			}
			if ( calcStatistics.isSelected() )
			{
				calcStatistics( true );
			}
			if ( registerImages.isSelected() )
			{
				registerImages( null, includeRange,  baseFileDir, baseFileNameText.getText() );
			}
			if ( calcMaxProjection.isSelected() )
			{
				calcMaxProjection( null, includeRange,  baseFileDir, baseFileNameText.getText() );
			}
			if ( generateTrainingData.isSelected() )
			{
				generateTrainingData();
			}
			if ( segmentAnnotations.isSelected() )
			{
				segmentAnnotations();
			}
			if ( calcAnnotationStats.isSelected() )
			{
				annotationStats();
			}
			if ( annotationAnimationFromSpreadSheet.isSelected() )
			{
				annotationAnimationFromSpreadSheet();
			}
			if ( annotationAnimation.isSelected() )
			{
				generateAnnotationAnimation();
			}
			if ( flipLattices.isSelected() )
			{
				flipLattices();
			}
		}
		setVisible(false);
	}

	private void generateTrainingData()
	{
		VOIVector trainingData = new VOIVector();
		if ( includeRange != null )
		{
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{
					//					System.err.println( fileName );
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

					fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + File.separator + "annotations";            	    		
					VOIVector annotations = new VOIVector();
					String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
					loadAllVOIsFrom(wormImageA, voiDir, true, annotations, false);
					if ( annotations.size() > 0 )
					{
						System.err.println( fileName + " " + annotations.elementAt(0).getCurves().size() );
						boolean allFound = false;
						boolean[] lablesFound = new boolean[annotations.elementAt(0).getCurves().size()];
						String[] lables = new String[annotations.elementAt(0).getCurves().size()];
						for ( int j = 0; j < lablesFound.length; j++ )
						{
							lablesFound[j] = false;
						}
						lables[0] = "L1";   lables[10] = "R1";
						lables[1] = "L2";   lables[11] = "R2";
						lables[2] = "L3";   lables[12] = "R3";
						lables[3] = "L4";   lables[13] = "R4";
						lables[4] = "L5";   lables[14] = "R5";
						lables[5] = "L6";   lables[15] = "R6";
						lables[6] = "L7";   lables[16] = "R7";
						lables[7] = "L8";   lables[17] = "R8";
						lables[8] = "L9";   lables[18] = "R9";
						lables[9] = "L10";   lables[19] = "R10";
						for ( int j = 0; j < annotations.elementAt(0).getCurves().size(); j++ )
						{
							VOIText text = (VOIText) annotations.elementAt(0).getCurves().elementAt(j);
							for ( int k = 0; k < lables.length; k++ )
							{
								if ( text.getText().equals( lables[k] ) )
								{
									lablesFound[k] = true;
									break;
								}
							}
							//							if ( text.getText().contains("L" + (j+1)) || text.getText().contains("R" + (j+1)) )
							//							{
							//								System.err.println( "            " + fileName + " " + annotations.elementAt(0).getCurves().size() );								
							//							}	
						}
						trainingData.add(annotations.elementAt(0));
						allFound = true;
						for ( int k = 0; k < lables.length; k++ )
						{
							allFound &= lablesFound[k];
						}
						if ( !allFound )
						{
							System.err.println( "            " + fileName + " " + allFound );
						}
						//						System.err.println( "            " + fileName + " " + wormImageA.getExtents()[0] + " " + wormImageA.getExtents()[1] + " " +wormImageA.getExtents()[2] + " " );	
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
					//					System.err.println( fileName );
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

					fileName = baseFileNameText.getText() + "_" + fileCount + File.separator + "annotations";            	    		
					VOIVector annotations = new VOIVector();
					String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
					loadAllVOIsFrom(wormImageA, voiDir, true, annotations, false);
					if ( annotations.size() > 0 )
					{
						System.err.println( "fileName " + annotations.elementAt(0).getCurves().size() );

						trainingData.add(annotations.elementAt(0));
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

		System.err.println( trainingData.size() );
		saveTrainingData( trainingData );
		System.err.println( trainingData.size() );
		saveCVData( trainingData );
		System.err.println( trainingData.size() );
		saveTestData( trainingData );
		System.err.println( trainingData.size() );
	}


	public static void segmentSeamCells( JProgressBar batchProgress, final Vector<Integer> includeRange, final String baseFileDir, final String baseFileName )
	{
		ModelImage image = null; 
		if ( includeRange != null )
		{
			int numSteps = 6;
			int step = 0;
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String fileName = baseFileName + "_" + includeRange.elementAt(i) + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{
					step = 1;
					System.err.print( fileName + "   " );
					FileIO fileIO = new FileIO();
					if(image != null) {
						if ( (i%10) == 0 )
						{
							image.disposeLocal(true);
						}
						else
						{
							image.disposeLocal();
						}
						image = null;
					}
					image = fileIO.readImage(fileName, baseFileDir + File.separator, false, null); 
					image.calcMinMax();
					String imageName = image.getImageName();
					if (imageName.contains("_clone")) {
						imageName = imageName.replaceAll("_clone", "");
					}
					if (imageName.contains("_laplace")) {
						imageName = imageName.replaceAll("_laplace", "");
					}
					if (imageName.contains("_gblur")) {
						imageName = imageName.replaceAll("_gblur", "");
					}
					if ( batchProgress != null )
					{
						batchProgress.setValue((int)(100 * (float)(i*numSteps + step++)/(float)(numSteps*includeRange.size())));
						batchProgress.update(batchProgress.getGraphics());
					}

					Vector<Vector3f> dataPoints = new Vector<Vector3f>();
					// calculate a robust histogram of the data values, used to find the left-right marker thresholds:
					float[] max_LR = WormSegmentationWindowing.robustHistogram(image, dataPoints);	
					if ( batchProgress != null )
					{
						batchProgress.setValue((int)(100 * (float)(i*numSteps + step++)/(float)(numSteps*includeRange.size())));
						batchProgress.update(batchProgress.getGraphics());
					}
					
					Vector<Vector3f> annotationskMeans = WormSegmentationKMeans.seamCellSegmentation(image);
					if ( batchProgress != null )
					{
						batchProgress.setValue((int)(100 * (float)(i*numSteps + step++)/(float)(numSteps*includeRange.size())));
						batchProgress.update(batchProgress.getGraphics());
					}
					
					Vector<Vector3f> annotationsWindow = WormSegmentationWindowing.seamCellSegmentation(image, max_LR[0], max_LR[1]);
					if ( batchProgress != null )
					{
						batchProgress.setValue((int)(100 * (float)(i*numSteps + step++)/(float)(numSteps*includeRange.size())));
						batchProgress.update(batchProgress.getGraphics());
					}
					
					Vector<Vector3f> annotationsLoG = WormSegmentationLoG.seamCellSegmentation(image);	
					if ( batchProgress != null )
					{
						batchProgress.setValue((int)(100 * (float)(i*numSteps + step++)/(float)(numSteps*includeRange.size())));
						batchProgress.update(batchProgress.getGraphics());
					}
					
					Vector<Vector3f> seamCells = new Vector<Vector3f>();
					Vector<Vector3f> tempSeamCells = new Vector<Vector3f>();
					for ( int k = 0; k < annotationsWindow.size(); k++ )
					{
						tempSeamCells.add( new Vector3f(annotationsWindow.elementAt(k)) );
					}
					for ( int k = 0; k < annotationskMeans.size(); k++ )
					{
						tempSeamCells.add( new Vector3f(annotationskMeans.elementAt(k)) );
					}
					for ( int k = 0; k < annotationsLoG.size(); k++ )
					{
						tempSeamCells.add( new Vector3f(annotationsLoG.elementAt(k)) );						
					}
					int prevSize = tempSeamCells.size();
					int newSize = WormSegmentation.reduceDuplicates(image, tempSeamCells, 5, 15, false);
					while ( (prevSize > newSize) && (newSize > 22) )
					{
						prevSize = newSize;
						newSize = WormSegmentation.reduceDuplicates(image, tempSeamCells, 5, 15, false);
					}
					if ( newSize > 22 )
					{
						newSize = WormSegmentation.reduceDuplicates(image, tempSeamCells);
					}
					if ( batchProgress != null )
					{
						batchProgress.setValue((int)(100 * (float)(i*numSteps + step++)/(float)(numSteps*includeRange.size())));
						batchProgress.update(batchProgress.getGraphics());
					}

					Vector3f negCenter = new Vector3f(-1,-1,-1);
					for ( int j = 0; j < tempSeamCells.size(); j++ )
					{
						if ( !tempSeamCells.elementAt(j).equals(negCenter) )
						{
							seamCells.add(tempSeamCells.elementAt(j));
						}
					}
					WormSegmentation.saveAnnotations(image, seamCells, Color.blue );
					fileName = baseFileName + "_" + includeRange.elementAt(i) + File.separator + autoSeamCellSegmentationOutput;  
					String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
					WormSegmentation.saveAllVOIsTo(voiDir, image);
				}
			}
		}
		
		if(image != null) {
			image.disposeLocal();
			image = null;
		}

		System.err.println( "Done segmentation" );
	}
	

	private void segmentAnnotations()
	{
		String fileNameStats = baseFileDir + File.separator + "annotation_statistics";		
		File statsFile = new File(fileNameStats + ".csv");
		if ( statsFile.exists() )
		{
			statsFile.delete();
			statsFile = new File(fileNameStats + ".csv");
		}
		FileWriter statisticsFW;
		try {
			statisticsFW = new FileWriter(statsFile);
			BufferedWriter statsFileBW = new BufferedWriter(statisticsFW);

			statsFileBW.write( "time" + "," + "ID" + "," + "pair distance" + "," + "side previous" + "," + "opposite previous" + "," + "side next" + "," + "opposite next" + "," +
					"volume" + "," + "value" + "," + "DOG value" + "," + "fill value" + "\n" );

			if ( includeRange != null )
			{
				for ( int i = 0; i < includeRange.size(); i++ )
				{
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

						fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + File.separator + "annotations";            	    		
						VOIVector annotations = new VOIVector();
						String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
						loadAllVOIsFrom(wormImageA, voiDir, true, annotations, false);
						if ( annotations.size() == 0 )
						{
							fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + File.separator + "annotation";            	    		
							annotations = new VOIVector();
							voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
							loadAllVOIsFrom(wormImageA, voiDir, true, annotations, false);
						}
						if ( annotations.size() > 0 )
						{
							ModelImage segmentation = LatticeModel.segmentAnnotations( wormImageA, annotations.elementAt(0), includeRange.elementAt(i), statsFileBW );

							voiDir = baseFileDir + File.separator + baseFileNameText.getText() + "_" + includeRange.elementAt(i) + File.separator + "segmentation" + File.separator;
							File voiFileDir = new File(voiDir);
							if (voiFileDir.exists() && voiFileDir.isDirectory()) { 
							} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) {
							} else { // voiFileDir does not exist
								voiFileDir.mkdir();
							}
							String imageName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + "_annotations.xml";
							segmentation.setImageName( imageName );
							ModelImage.saveImage( segmentation, imageName, voiDir, false );
							segmentation.disposeLocal();
							segmentation = null;
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
						fileName = baseFileNameText.getText() + "_" + fileCount + File.separator + "annotations";            	    		
						VOIVector annotations = new VOIVector();
						String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
						loadAllVOIsFrom(wormImageA, voiDir, true, annotations, false);
						if ( annotations.size() == 0 )
						{
							fileName = baseFileNameText.getText() + "_" + fileCount + File.separator + "annotation";            	    		
							annotations = new VOIVector();
							voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
							loadAllVOIsFrom(wormImageA, voiDir, true, annotations, false);
						}
						if ( annotations.size() > 0 )
						{
							ModelImage segmentation = LatticeModel.segmentAnnotations( wormImageA, annotations.elementAt(0), fileCount, statsFileBW );
						}


						fileCount++;
					}    				
					else
					{
						fileExists = false;
					}
				}
			}


			statsFileBW.write( "\n" );
			statsFileBW.close();
		} catch (IOException e) {
		}

		if(wormImageA != null) {
			wormImageA.disposeLocal();
			wormImageA = null;
		}
	}

	private void annotationStats()
	{
		try {
			String fileNameStats = baseFileDir + File.separator + "annotation_statistics";		
			File statsFile = new File(fileNameStats + ".csv");
			FileReader statisticsFR = new FileReader(statsFile);
			BufferedReader statsFileBR = new BufferedReader(statisticsFR);


			fileNameStats = baseFileDir + File.separator + "L1_statistics";		
			statsFile = new File(fileNameStats + ".csv");
			if ( statsFile.exists() )
			{
				statsFile.delete();
				statsFile = new File(fileNameStats + ".csv");
			}
			FileWriter L1StatsFW = new FileWriter(statsFile);
			BufferedWriter L1StatsBW = new BufferedWriter(L1StatsFW);

			L1StatsBW.write( "time" + "," + "volume\n");

			String line = statsFileBR.readLine();
			line = statsFileBR.readLine();
			while ( (line != null) && (line.length() > 0) )
			{
				String[] parsed = line.split( "," );
				int time = Integer.valueOf(parsed[0]);
				String ID = parsed[1];
				if ( ID.equals("L1") || ID.equals("1L") )
				{
					L1StatsBW.write( time + "," + parsed[7] + "\n");
				}
				line = statsFileBR.readLine();
			}
			L1StatsBW.write( "\n" );
			L1StatsBW.close();
			statsFileBR.close();
		} catch (IOException e) {
		}

		System.err.println( "Done annotationStats" );
	}


//	private void seamCellInfo()
//	{
//		if ( includeRange != null )
//		{
////			try {
////				String fileNameStats = baseFileDir + File.separator + "SeamCell_RelativeIntensities";		
////				File statsFile = new File(fileNameStats + ".csv");
////				if ( statsFile.exists() )
////				{
////					statsFile.delete();
////					statsFile = new File(fileNameStats + ".csv");
////				}
////				FileWriter L1StatsFW = new FileWriter(statsFile);
////				BufferedWriter L1StatsBW = new BufferedWriter(L1StatsFW);
////
////				L1StatsBW.write( "time" + "," + "image min" + "," + "image max" + "," + 
////				"1L" + "," + "1R" + "," + 
////				"2L" + "," + "2R" + "," + 
////				"3L" + "," + "3R" + "," + 
////				"4L" + "," + "4R" + "," + 
////				"5L" + "," + "5R" + "," + 
////				"6L" + "," + "6R" + "," + 
////				"7L" + "," + "7R" + "," + 
////				"8L" + "," + "8R" + "," + 
////				"9L" + "," + "9R" + "," +
////				"10L" + "," + "10R" + "\n");
//
//				HashMap<String, Float> seamCellIntensities = new HashMap<String, Float>();
//			for ( int i = 0; i < includeRange.size(); i++ )
//			{
//				String fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + ".tif";
//				File voiFile = new File(baseFileDir + File.separator + fileName);
//				if ( voiFile.exists() )
//				{
//					System.err.println( fileName );
//					FileIO fileIO = new FileIO();
//					if(wormImageA != null) 
//					{
//						wormImageA.disposeLocal();
//						wormImageA = null;
//					}
//					ModelImage temp = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);
//					wormImageA = LatticeModel.LoG(temp);  
//					wormImageA.calcMinMax();
//					temp.disposeLocal();
//					temp = null;
//					float[] targetValue = new float[]{(float) (wormImageA.getMax()/2f)};
//					HashMap<Float, Integer> histogram = estimateHistogram(wormImageA, targetValue);
//
//					fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + File.separator + "annotations";            	    		
//					VOIVector annotations = new VOIVector();
//					String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
//					loadAllVOIsFrom(voiDir, true, annotations, false);
//					if ( annotations.size() <= 0 )
//					{
//						fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + File.separator + "annotation";            	    		
//						annotations = new VOIVector();
//						voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
//						loadAllVOIsFrom(voiDir, true, annotations, false);
//					}
//					if ( annotations.size() > 0 )
//					{
//						String imageName = wormImageA.getImageName();
//						if (imageName.contains("_clone")) {
//							imageName = imageName.replaceAll("_clone", "");
//						}
//						if (imageName.contains("_laplace")) {
//							imageName = imageName.replaceAll("_laplace", "");
//						}
//						if (imageName.contains("_gblur")) {
//							imageName = imageName.replaceAll("_gblur", "");
//						}
//						imageName = imageName + "_segmentation";
//						ModelImage segmentationImage = new ModelImage(ModelStorageBase.FLOAT, wormImageA.getExtents(), imageName);
//						JDialogBase.updateFileInfo(wormImageA, segmentationImage);
//						
//						imageName = imageName + "_visited";
//						ModelImage visited = new ModelImage(ModelStorageBase.INTEGER, wormImageA.getExtents(), imageName);
//						JDialogBase.updateFileInfo(wormImageA, visited);   
//						
//						Vector<Vector3f> seedList = new Vector<Vector3f>();
//						for ( int j = 0; j < annotations.elementAt(0).getCurves().size(); j++ )
//						{
//							VOIText text1 = (VOIText) annotations.elementAt(0).getCurves().elementAt(j);
//							Vector3f pos1 = text1.elementAt(0);
//							float value = wormImageA.getFloat( (int)pos1.X, (int)pos1.Y, (int)pos1.Z );
//							seamCellIntensities.put( text1.getText(), value );
//							if ( text1.getText().contains("L") || text1.getText().contains("R") )
//							{
//								seedList.add(pos1);
//								int fillCount = LatticeModel.fill(wormImageA, targetValue[0], segmentationImage, null, seedList, visited, j, null);
//								System.err.println( "     " + text1.getText() + "   " + fillCount );
//								seedList.clear();
//							}
//						}
////						segmentationImage.calcMinMax();
////						new ViewJFrameImage(segmentationImage);
//						segmentationImage.disposeLocal();
//						segmentationImage = null;
//						visited.disposeLocal();
//						visited = null;
//
////						System.err.print( includeRange.elementAt(i) + "," + histogram.get((float)wormImageA.getMin())/(float)wormImageA.getDataSize() + "," + histogram.get((float)wormImageA.getMax())/(float)wormImageA.getDataSize() + "," ); 
////						System.err.print( histogram.get(seamCellIntensities.get("1L"))/(float)wormImageA.getDataSize() + "," + histogram.get(seamCellIntensities.get("1R"))/(float)wormImageA.getDataSize() + ","  );  
////						System.err.print( histogram.get(seamCellIntensities.get("2L"))/(float)wormImageA.getDataSize() + "," + histogram.get(seamCellIntensities.get("2R"))/(float)wormImageA.getDataSize() + ","  ); 
////						System.err.print( histogram.get(seamCellIntensities.get("3L"))/(float)wormImageA.getDataSize() + "," + histogram.get(seamCellIntensities.get("3R"))/(float)wormImageA.getDataSize() + ","  ); 
////						System.err.print( histogram.get(seamCellIntensities.get("4L"))/(float)wormImageA.getDataSize() + "," + histogram.get(seamCellIntensities.get("4R"))/(float)wormImageA.getDataSize() + ","  );  
////						System.err.print( histogram.get(seamCellIntensities.get("5L"))/(float)wormImageA.getDataSize() + "," + histogram.get(seamCellIntensities.get("5R"))/(float)wormImageA.getDataSize() + ","  ); 
////						System.err.print( histogram.get(seamCellIntensities.get("6L"))/(float)wormImageA.getDataSize() + "," + histogram.get(seamCellIntensities.get("6R"))/(float)wormImageA.getDataSize() + ","  );  
////						System.err.print( histogram.get(seamCellIntensities.get("7L"))/(float)wormImageA.getDataSize() + "," + histogram.get(seamCellIntensities.get("7R"))/(float)wormImageA.getDataSize() + ","  ); 
////						System.err.print( histogram.get(seamCellIntensities.get("8L"))/(float)wormImageA.getDataSize() + "," + histogram.get(seamCellIntensities.get("8R"))/(float)wormImageA.getDataSize() + ","  );  
////						System.err.print( histogram.get(seamCellIntensities.get("9L"))/(float)wormImageA.getDataSize() + "," + histogram.get(seamCellIntensities.get("9R"))/(float)wormImageA.getDataSize() + ","  ); 
////						System.err.print( histogram.get(seamCellIntensities.get("10L"))/(float)wormImageA.getDataSize() + "," + histogram.get(seamCellIntensities.get("10R"))/(float)wormImageA.getDataSize() + "\n");
//
//
////						L1StatsBW.write( includeRange.elementAt(i) + "," + histogram.get((float)wormImageA.getMin())/(float)wormImageA.getDataSize() + "," + histogram.get((float)wormImageA.getMax())/(float)wormImageA.getDataSize() + "," + 
////								histogram.get(seamCellIntensities.get("1L"))/(float)wormImageA.getDataSize() + "," + histogram.get(seamCellIntensities.get("1R"))/(float)wormImageA.getDataSize() + "," + 
////								histogram.get(seamCellIntensities.get("2L"))/(float)wormImageA.getDataSize() + "," + histogram.get(seamCellIntensities.get("2R"))/(float)wormImageA.getDataSize() + "," + 
////								histogram.get(seamCellIntensities.get("3L"))/(float)wormImageA.getDataSize() + "," + histogram.get(seamCellIntensities.get("3R"))/(float)wormImageA.getDataSize() + "," + 
////								histogram.get(seamCellIntensities.get("4L"))/(float)wormImageA.getDataSize() + "," + histogram.get(seamCellIntensities.get("4R"))/(float)wormImageA.getDataSize() + "," + 
////								histogram.get(seamCellIntensities.get("5L"))/(float)wormImageA.getDataSize() + "," + histogram.get(seamCellIntensities.get("5R"))/(float)wormImageA.getDataSize() + "," + 
////								histogram.get(seamCellIntensities.get("6L"))/(float)wormImageA.getDataSize() + "," + histogram.get(seamCellIntensities.get("6R"))/(float)wormImageA.getDataSize() + "," + 
////								histogram.get(seamCellIntensities.get("7L"))/(float)wormImageA.getDataSize() + "," + histogram.get(seamCellIntensities.get("7R"))/(float)wormImageA.getDataSize() + "," + 
////								histogram.get(seamCellIntensities.get("8L"))/(float)wormImageA.getDataSize() + "," + histogram.get(seamCellIntensities.get("8R"))/(float)wormImageA.getDataSize() + "," + 
////								histogram.get(seamCellIntensities.get("9L"))/(float)wormImageA.getDataSize() + "," + histogram.get(seamCellIntensities.get("9R"))/(float)wormImageA.getDataSize() + "," +
////								histogram.get(seamCellIntensities.get("10L"))/(float)wormImageA.getDataSize() + "," + histogram.get(seamCellIntensities.get("10R"))/(float)wormImageA.getDataSize() + "\n");
//
////						L1StatsBW.write( includeRange.elementAt(i) + "," + (float)wormImageA.getMin()/(float)wormImageA.getMax() + "," + (float)wormImageA.getMax()/(float)wormImageA.getMax() + "," + 
////								seamCellIntensities.get("1L")/(float)wormImageA.getMax() + "," + seamCellIntensities.get("1R")/(float)wormImageA.getMax() + "," + 
////								seamCellIntensities.get("2L")/(float)wormImageA.getMax() + "," + seamCellIntensities.get("2R")/(float)wormImageA.getMax() + "," + 
////								seamCellIntensities.get("3L")/(float)wormImageA.getMax() + "," + seamCellIntensities.get("3R")/(float)wormImageA.getMax() + "," + 
////								seamCellIntensities.get("4L")/(float)wormImageA.getMax() + "," + seamCellIntensities.get("4R")/(float)wormImageA.getMax() + "," + 
////								seamCellIntensities.get("5L")/(float)wormImageA.getMax() + "," + seamCellIntensities.get("5R")/(float)wormImageA.getMax() + "," + 
////								seamCellIntensities.get("6L")/(float)wormImageA.getMax() + "," + seamCellIntensities.get("6R")/(float)wormImageA.getMax() + "," + 
////								seamCellIntensities.get("7L")/(float)wormImageA.getMax() + "," + seamCellIntensities.get("7R")/(float)wormImageA.getMax() + "," + 
////								seamCellIntensities.get("8L")/(float)wormImageA.getMax() + "," + seamCellIntensities.get("8R")/(float)wormImageA.getMax() + "," + 
////								seamCellIntensities.get("9L")/(float)wormImageA.getMax() + "," + seamCellIntensities.get("9R")/(float)wormImageA.getMax() + "," +
////								seamCellIntensities.get("10L")/(float)wormImageA.getMax() + "," + seamCellIntensities.get("10R")/(float)wormImageA.getMax() + "\n");
//
//						seamCellIntensities.clear();
////						histogram.clear();
////						histogram = null;
//					}
//				}
//			}    				
////			L1StatsBW.write( "\n" );
////			L1StatsBW.close();
////		} catch (IOException e) {
////		}
//		}
//		if ( wormImageA != null )
//		{
//			wormImageA.disposeLocal();
//			wormImageA = null;
//		}
//		System.err.println( "Done seam cell info" );
//	}
	
	public static void latticeStraighten( JProgressBar batchProgress, final Vector<Integer> includeRange, final String baseFileDir, final String baseFileName )
	{
		ModelImage image = null;
		if ( includeRange != null )
		{
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				//    	    		String fileName = baseFileDir + File.separator + baseFileNameText.getText() + "_" + includeRange.elementAt(i) + ".tif";
				String fileName = baseFileName + "_" + includeRange.elementAt(i) + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{
					System.err.println( fileName );
					FileIO fileIO = new FileIO();
					if(image != null) {
						if ( (i%10) == 0 )
						{
							image.disposeLocal(true);
						}
						else
						{
							image.disposeLocal();
						}
						image = null;
					}
					image = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);  

					fileName = baseFileName + "_"  + includeRange.elementAt(i) + File.separator + editLatticeOutput;
					VOIVector lattice = new VOIVector();
					String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
					loadAllVOIsFrom(image, voiDir, false, lattice, false);
					if ( lattice.size() == 0 )
					{
						fileName = baseFileName + "_"  + includeRange.elementAt(i) + File.separator + autoLatticeGenerationOutput + "1";
						lattice = new VOIVector();
						voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
						loadAllVOIsFrom(image, voiDir, false, lattice, false);						
					}

					if ( (lattice.elementAt(0) != null) && (lattice.elementAt(0).getCurves().size() == 2) )
					{
						LatticeModel model = new LatticeModel( image, lattice.elementAt(0) );

						fileName = baseFileName + "_" + includeRange.elementAt(i) + File.separator + "annotations";            	    		
						VOIVector annotations = new VOIVector();
						voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
						loadAllVOIsFrom(image, voiDir, true, annotations, false);
						if ( annotations.size() > 0 )
						{
							model.setAnnotations( annotations.elementAt(0) );
						}
						else
						{
							fileName = baseFileName + "_" + includeRange.elementAt(i) + File.separator + "annotation";            	    		
							annotations = new VOIVector();
							voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
							loadAllVOIsFrom(image, voiDir, true, annotations, false);
							if ( annotations.size() > 0 )
							{
								model.setAnnotations( annotations.elementAt(0) );
							}							
						}
						model.interpolateLattice( false );
						model.dispose();
						model = null;
					}
				}
				if ( batchProgress != null )
				{
					batchProgress.setValue((int)(100 * (float)(i+1)/(float)includeRange.size()));
					batchProgress.update(batchProgress.getGraphics());
				}
			}
		}

		if ( image != null )
		{
			image.disposeLocal();
			image = null;
		}
	}


	private void calcStatistics( boolean calcAutomaticLatticeData )
	{
		VOIVector annotationStatsBefore = new VOIVector();
		VOIVector annotationStatsAfter = new VOIVector();
		if ( includeRange != null )
		{
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String fileName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + "statistics" + File.separator + "AnnotationInfo_before.csv";
				readLatticePositions( fileName, includeRange.elementAt(i), annotationStatsBefore );

				fileName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + "statistics" + File.separator + "AnnotationInfo_after.csv";
				readLatticePositions( fileName, includeRange.elementAt(i), annotationStatsAfter );
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
					fileName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + fileCount + File.separator + "statistics" + File.separator + "AnnotationInfo_before.csv";
					readLatticePositions( fileName, fileCount, annotationStatsBefore );

					fileName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + fileCount + File.separator + "statistics" + File.separator + "AnnotationInfo_after.csv";
					readLatticePositions( fileName, fileCount, annotationStatsAfter );
					fileCount++;
				}    				
				else
				{
					fileExists = false;
				}
			}
		}

		if ( (annotationStatsBefore != null) && (annotationStatsAfter != null) )
		{
			String fileName = baseFileDir + File.separator + "annotation_statistics";
			File statsFile = new File(fileName);
			if ( !statsFile.exists() )
			{
				System.err.println( "mkdir " + fileName );
				statsFile.mkdir();
			}
//			calcStatistics(fileName, annotationStatsBefore, "_before", calcAutomaticLatticeData );
//			calcStatistics(fileName, annotationStatsAfter, "_after", calcAutomaticLatticeData );
			calcStatistics3(fileName, annotationStatsBefore, "_before" );
//			calcStatistics2(fileName, annotationStatsBefore, "_before", calcAutomaticLatticeData );
//			calcStatistics2(fileName, annotationStatsAfter, "_after", calcAutomaticLatticeData );

			annotationList = annotationStatsAfter;

			annotationStatsBefore = null;
			annotationStatsAfter = null;
		}
	}     

	public static void buildLattice( JProgressBar batchProgress, final Vector<Integer> includeRange, final String baseFileDir, final String baseFileName )
	{
		ModelImage image = null;
		if ( includeRange != null )
		{
			int count = 0;
			int foundCount = 0;
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				int step = 1;
				String fileName = baseFileName + "_" + includeRange.elementAt(i) + ".tif";
				FileIO fileIO = new FileIO();
				if ( image != null )
				{
					image.disposeLocal();
				}
				image = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);
				System.err.println( fileName );
				
				VOIVector seamCells = new VOIVector();
//				fileName = baseFileName + "_" + includeRange.elementAt(i) + File.separator + editAnnotationInput;
//				String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
//				loadAllVOIsFrom(image, voiDir, true, seamCells, false);
				fileName = baseFileName + "_" + includeRange.elementAt(i) + File.separator + editSeamCellOutput;
				String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
				loadAllVOIsFrom(image, voiDir, true, seamCells, false);
				if ( seamCells.size() <= 0 )
				{
					fileName = baseFileName + "_" + includeRange.elementAt(i) + File.separator + autoSeamCellSegmentationOutput;
					voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
					loadAllVOIsFrom(image, voiDir, true, seamCells, false);
				}
				if ( seamCells.size() == 0 )
				{
					continue;
				}
				VOI nose = new VOI( (short)1, "nose", VOI.ANNOTATION, 1);
				for ( int j = seamCells.elementAt(0).getCurves().size() - 1; j >=0; j-- )
				{
					VOIText text = (VOIText) seamCells.elementAt(0).getCurves().elementAt(j);
					// skip extra annotations (origin, nose)
					// use annotations list?
					if ( text.getText().contains("nose") || text.getText().contains("Nose") )
					{
						nose.getCurves().add(seamCells.elementAt(0).getCurves().remove(j));
					}
//					if ( text.getText().equalsIgnoreCase("origin") )
//					{
//						nose.getCurves().add(seamCells.elementAt(0).getCurves().remove(j));
//					}
				}
				if ( buildLattice( batchProgress, i, includeRange.size(), image, seamCells.elementAt(0), nose, includeRange.elementAt(i), baseFileDir, baseFileName ) )
				{
					foundCount++;
				}

				count++;
			}
			System.err.println( "Tested " + count + " success " + foundCount + " success rate = " + 100 * (float)foundCount/(float)count +"%" );
			MipavUtil.displayInfo( "Found lattices for " + foundCount + " out of " + count + " volumes tested (" + (int)(100 * (float)foundCount/(float)count) + "%)" );
		}

		if ( image != null )
		{
			image.disposeLocal();
			image = null;
		}
		System.err.println( "Done lattice building" );
		
	}     

	private static boolean buildLattice( JProgressBar batchProgress, int imageCount, int numImages, ModelImage image, VOI annotations, VOI nose, int time, String baseFileDir, String baseFileName )
	{
		int numSteps = 3;
		int step = 1;
		long startTime = System.currentTimeMillis();
		boolean print = false;
		/** Step (1) Attempt to find the tenth pair of seam cells in the lattice. 
		 * The 10th pair is distinct in that it has the smallest between-cell distance */
		Vector<int[]> tenthPairs = new Vector<int[]>();
		Vector<Vector3f> positions = new Vector<Vector3f>();
		for ( int i = 0; i < annotations.getCurves().size(); i++ )
		{
			VOIText text = (VOIText) annotations.getCurves().elementAt(i);
			// skip extra annotations (origin, nose)
			// use annotations list?
			if ( !text.getText().equalsIgnoreCase("origin") )
			{
				Vector3f pos = new Vector3f(text.elementAt(0));
				pos.scale( VOILatticeManagerInterface.VoxelSize );
				positions.add( pos );
			}
		}
		Vector<Vector3f> nosePts = new Vector<Vector3f>();
		if ( nose != null )
		{
			for ( int i = 0; i < nose.getCurves().size(); i++ )
			{
				VOIText text = (VOIText)nose.getCurves().elementAt(i);
				nosePts.add(new Vector3f(text.elementAt(0)));
				nosePts.lastElement().scale( VOILatticeManagerInterface.VoxelSize );
			}
		}

		// pair distance > 50 time step for 1-9 is in the range of 5-15 um
		// pair distance > 50 time step for 10 is in the range of 1-5 um

		// look for potential 10 pair:
		float maxDist = -1;
		int count = 0;
		float tempMin = tenMinDist;
		float tempMax = tenMaxDist;
		while ( (count == 0) && (tempMin > 0.5) )
		{
			count = 0;
			for ( int i = 0; i < positions.size(); i++ )
			{
				for ( int j = i + 1; j < positions.size(); j++ )
				{
					float distance = positions.elementAt(i).distance( positions.elementAt(j) );
					if ( distance > maxDist )
					{
						maxDist = distance;
					}
					if (  (distance > tempMin) && (distance < tempMax) )
					{
						tenthPairs.add( new int[]{i,j} );
						count++;
					}
				}
			}
			if ( count == 0 )
			{
				tempMin -= 0.1;
				tempMax += 0.1;
			}
		}

		if ( tenthPairs.size() > 1 )
		{
			for ( int i = tenthPairs.size() - 1; i >=0; i-- )
			{
				if ( midPointFail( tenthPairs.elementAt(i), positions ) )
				{
					tenthPairs.remove(i);
				}
			}
		}
		if ( batchProgress != null )
		{
			batchProgress.setValue((int)(100 * (float)(imageCount*numSteps + step++)/(float)(numSteps*numImages)));
			batchProgress.update(batchProgress.getGraphics());
		}
		
		LatticeModel model = new LatticeModel( image );
		int[] total = new int[]{0};
		Vector<Vector<int[]>> sequenceList = new Vector<Vector<int[]>>();
		
		// Loop over all potential tenthPairs:
		for ( int i = 0; i < tenthPairs.size(); i++ )
		{
			int[] tenthPair = tenthPairs.elementAt(i);
			int[][] pairs = new int[positions.size()][positions.size()];
			for ( int j = 0; j < positions.size(); j++ )
			{
				int countJ = 0;
				for ( int k = 0; k < positions.size(); k++ )
				{
					pairs[j][k] = -1;
					if ( (j != tenthPair[0]) && (j != tenthPair[1]) )
					{
						if ( (k != j) && (k != tenthPair[0]) && (k != tenthPair[1]) )
						{
							float distance = positions.elementAt(j).distance( positions.elementAt(k) );
							if (  (distance >= minPairDist) && (distance <= maxPairDist) )
							{
								if ( !midPointFail(j,k, positions) )
								{
									pairs[j][k] = 1;
									countJ++;
								}
							}
						}
					}
				}
				if ( (j != tenthPair[0]) && (j != tenthPair[1]) )
				{
					if ( countJ == 0 )
					{
						if ( print ) System.err.println( "   No pairs found for " + j );
					}
				}
			}
			if ( print ) System.err.println( "   Initial Pairset " + countPairs(pairs, tenthPair) );
			Vector<int[]> pairLists = new Vector<int[]>();
//			
			for ( int j = 0; j < pairs.length; j++ )
			{
				for ( int k = j+1; k < pairs[j].length; k++ )
				{
					if ( pairs[j][k] != -1 )
					{
						pairLists.add( new int[]{j,k} );
					}
				}
			}

			pairLists.add( tenthPair );

			// This step looks for seam cell pairs
			// where one member of the pair is only in
			// one potential pair. When that's the case
			// the pair is essential to the list
			// this step removes all other potential pairs
			// that link to the non-singleton member of the
			// essential pair:
			int prevSize = pairLists.size();
			if ( checkPairs( pairLists, positions.size() ) )
			{
				while ( prevSize > pairLists.size() )
				{
					prevSize = pairLists.size();
					checkPairs( pairLists, positions.size() );
				}
			}
			checkPairs( pairLists, positions.size() );
			
			Vector<int[]> sequence = new Vector<int[]>();
			int targetLength = (positions.size() % 2) == 0 ? positions.size() : positions.size() -1;
			targetLength = Math.max(0, targetLength);
			targetLength = Math.min(20, targetLength);
			sequencePairs( startTime, model, nosePts, positions, sequence, pairLists, tenthPair, targetLength, total, sequenceList );
		}
		if ( batchProgress != null )
		{
			batchProgress.setValue((int)(100 * (float)(imageCount*numSteps + step++)/(float)(numSteps*numImages)));
			batchProgress.update(batchProgress.getGraphics());
		}
//		System.err.println( "buildLattice time 1 = " + AlgorithmBase.computeElapsedTime(startTime) + " " + sequenceList.size() );
		startTime = System.currentTimeMillis();
		
		VOIVector finalLatticeList = new VOIVector();
		orderSequences( startTime, image, model, nosePts, positions, sequenceList, finalLatticeList );
//		System.err.println( "buildLattice time 2 = " + AlgorithmBase.computeElapsedTime(startTime) );

		if ( batchProgress != null )
		{
			batchProgress.setValue((int)(100 * (float)(imageCount*numSteps + step++)/(float)(numSteps*numImages)));
			batchProgress.update(batchProgress.getGraphics());
		}
		for ( int j = 0; j < Math.min( 5, finalLatticeList.size()); j++ )
		{
			image.unregisterAllVOIs();
			VOI lattice = finalLatticeList.elementAt(j);
			image.registerVOI(lattice);
			String fileName = baseFileDir + File.separator + baseFileName + "_"  + time + File.separator + autoLatticeGenerationOutput + j + File.separator;
			File outputFileDir = new File(fileName);
			if ( !outputFileDir.exists() )
			{
				outputFileDir.mkdir();
			}

			LatticeModel.saveAllVOIsTo(fileName, image);
		}
		
		model.dispose();
		model = null;
		return (finalLatticeList.size() > 0);
	}
	
	/**
	 * Checks the table with seam-cells pairs to make sure all seam cells are included in at least one pair:
	 * @param pairs
	 * @param tenthPair
	 * @return
	 */
	private static boolean countPairs( final int[][] pairs, final int[] tenthPair )
	{
		boolean allFound = true;
		for ( int i = 0; i < pairs.length; i++ )
		{
			int count = 0;
			for ( int j = 0; j < pairs[i].length; j++ )
			{
				if ( pairs[i][j] != -1 )
				{
					count++;
				}
			}
			if ( (i != tenthPair[0]) && (i != tenthPair[1]) )
			{
				if ( count == 0 )
				{
					allFound = false;
				}
			}
		}
		return allFound;
	}

	/**
	 * Measures the total amount of folding in the lattice.
	 * @param positions
	 * @param sequence
	 * @return
	 */
	private static float measureCurvature( Vector<Vector3f> left, Vector<Vector3f> right )
	{
		float bendAngles = 0;
		for ( int i = 0; i < left.size()-2; i++ )
		{
			// Calculate the mid point of the first pair in the sequence:
			Vector3f mid1 = Vector3f.add(left.elementAt(i), right.elementAt(i));
			mid1.scale(0.5f);			

			// Calculate the mid point of the second pair in the sequence:
			Vector3f mid2 = Vector3f.add(left.elementAt(i+1), right.elementAt(i+1));
			mid2.scale(0.5f);

			// Calculate the mid point of the third pair in the sequence:
			Vector3f mid3 = Vector3f.add(left.elementAt(i+2), right.elementAt(i+2));
			mid3.scale(0.5f);
			
			// The bend is calculated as the angle change from midpoint1 to
			// midpoint3 bending around midpoint 2.
			Vector3f vec1 = Vector3f.sub( mid2, mid1 );
			Vector3f vec2 = Vector3f.sub( mid3, mid2 );
			vec1.normalize();
			vec2.normalize();
			float angle1 = Math.abs(vec1.angle(vec2));
			float angle2 = Math.abs(vec2.angle(vec1));
			if ( angle1 < angle2 )
			{
				bendAngles += angle1;
			}
			else
			{
				bendAngles += angle2;
			}
		}
		// total bend (folding) along the sequence
		return bendAngles;
	}
	
	private static VOI makeLattice( Vector<Vector3f> positions, Vector<Vector3f> nose, int index, Vector<int[]> sequence )
	{
		VOIContour left = new VOIContour( false );
		VOIContour right = new VOIContour( false );
		VOI lattice = new VOI((short) index, "lattice", 1, VOI.POLYLINE );
		lattice.getCurves().add(left);
		lattice.getCurves().add(right);
		for ( int i = 0; i < sequence.size(); i++ )
		{
			int[] pair = sequence.elementAt(i);
			Vector3f p1 = new Vector3f(positions.elementAt(pair[0]));
			Vector3f p2 = new Vector3f(positions.elementAt(pair[1]));
			p1.scale(1f/VOILatticeManagerInterface.VoxelSize);
			p2.scale(1f/VOILatticeManagerInterface.VoxelSize);
			left.add(0, p1);
			right.add(0, p2);
		}
		if ( nose != null )
		{
			if ( nose.size() == 1 )
			{
				Vector3f nosePt = new Vector3f(nose.elementAt(0));
				nosePt.scale(1f/VOILatticeManagerInterface.VoxelSize);
				left.add(0, nosePt);
				right.add(0, nosePt);
			}
			else if ( nose.size() >= 2 )
			{
				Vector3f nosePt0 = new Vector3f(nose.elementAt(0));
				nosePt0.scale(1f/VOILatticeManagerInterface.VoxelSize);
				Vector3f nosePt1 = new Vector3f(nose.elementAt(1));
				nosePt1.scale(1f/VOILatticeManagerInterface.VoxelSize);

				float distanceL0 = left.elementAt(0).distance(nosePt0);
				float distanceL1 = left.elementAt(0).distance(nosePt1);
				if ( distanceL0 < distanceL1 )
				{
					left.add(0, nosePt0);
					right.add(0, nosePt1);
				}
				else if ( distanceL0 > distanceL1 )
				{
					left.add(0, nosePt1);
					right.add(0, nosePt0);					
				}
				else
				{
					float distanceR0 = right.elementAt(0).distance(nosePt0);
					float distanceR1 = right.elementAt(0).distance(nosePt1);
					if ( distanceR0 < distanceR1 )
					{
						right.add(0, nosePt0);
						left.add(0, nosePt1);
					}
					else
					{
						right.add(0, nosePt1);			
						left.add(0, nosePt0);		
					}					
				}
			}
		}
		return lattice;
	}
	
	private static int intersectionCount( ModelImage image, Vector<Vector3f> left, Vector<Vector3f> right )
	{
		int intersectionCount = 0;
		Box3f[] boxes = new Box3f[left.size()-1];
		Vector<Vector3f> vertices = new Vector<Vector3f>();
		for ( int i = 0; i < left.size()-1; i++ )
		{
			vertices.add( left.elementAt(i) );
			vertices.add( right.elementAt(i) );
			vertices.add( left.elementAt(i+1) );
			vertices.add( right.elementAt(i+1) );
			Vector3f mid0 = Vector3f.add( left.elementAt(i), right.elementAt(i) );
			mid0.scale(0.5f);
			Vector3f mid1 = Vector3f.add( left.elementAt(i+1), right.elementAt(i+1) );
			mid1.scale(0.5f);
			
			Vector3f horizontal = Vector3f.sub( right.elementAt(i), left.elementAt(i) );
			Vector3f length = Vector3f.sub( mid1, mid0 );
			horizontal.normalize();
			length.normalize();
			Vector3f up = Vector3f.cross(horizontal, length);
			up.normalize();
			float width = left.elementAt(i).distance(right.elementAt(i) );
			width /= 4f;
			vertices.add( Vector3f.scaleAddS(width, up, left.elementAt(i)) );
			vertices.add( Vector3f.scaleAddS(-width, up, left.elementAt(i)) );
			vertices.add( Vector3f.scaleAddS(width, up, right.elementAt(i)) );
			vertices.add( Vector3f.scaleAddS(-width, up, right.elementAt(i)) );

			
			horizontal = Vector3f.sub( right.elementAt(i+1), left.elementAt(i+1) );
			horizontal.normalize();
			up = Vector3f.cross(horizontal, length);
			up.normalize();
			width = left.elementAt(i+1).distance(right.elementAt(i+1) );
			width /= 4f;
			vertices.add( Vector3f.scaleAddS(width, up, left.elementAt(i+1)) );
			vertices.add( Vector3f.scaleAddS(-width, up, left.elementAt(i+1)) );
			vertices.add( Vector3f.scaleAddS(width, up, right.elementAt(i+1)) );
			vertices.add( Vector3f.scaleAddS(-width, up, right.elementAt(i+1)) );
			
			boxes[i] = ContBox3f.ContOrientedBox( vertices.size(), vertices );
			vertices.clear();
		}
		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 0;  
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 0;  
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 0;
		Vector3f pt = new Vector3f();
		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					pt.set(x, y, z);
					int overlapCount = 0;
					for ( int b = 0; b < boxes.length; b++ )
					{
						if ( ContBox3f.InBox( pt, boxes[b]) )
						{
							overlapCount++;
						}
					}
					if ( overlapCount > 1 )
					{
						intersectionCount++;
					}
				}
			}
		}
		
		
		return intersectionCount;
	}

	private static void orderSequences( long startTime, ModelImage image, LatticeModel model, Vector<Vector3f> nose, Vector<Vector3f> positions, Vector<Vector<int[]>> sequenceList, VOIVector finalLatticeList )
	{
		if ( sequenceList.size() <= 0 )
		{
			return;
		}
		VOIVector lattices = new VOIVector();
		for ( int i = 0; i < sequenceList.size(); i++ )
		{
			lattices.add( makeLattice( positions, nose, i, sequenceList.elementAt(i) ) );
		}
		
		Vector2d[] angles = new Vector2d[sequenceList.size()];
		int[] intersections = new int[sequenceList.size()];
		for ( int i = 0; i < sequenceList.size(); i++ )
		{
			VOI lattice = lattices.elementAt(i);
			angles[i] = new Vector2d(measureCurvature(lattice.getCurves().elementAt(0), lattice.getCurves().elementAt(1)), i);
			intersections[i] = -1;
		}
		Arrays.sort(angles);
		Vector3d[] minCount = new Vector3d[sequenceList.size()];
		
		for ( int i = 0; i < sequenceList.size(); i++ )
		{
			int index = (int) angles[i].Y;
			VOI lattice = lattices.elementAt(index);
			int intersection = Integer.MAX_VALUE;

			double elapsedTime = AlgorithmBase.computeElapsedTime(startTime);
			if ( elapsedTime < 600 )
			{
				intersection = intersectionCount( image, lattice.getCurves().elementAt(0), lattice.getCurves().elementAt(1) );
				minCount[i] = new Vector3d(Math.round(angles[i].X), intersection, index);
			}
			else
			{
				minCount[i] = new Vector3d( Double.MAX_VALUE, Double.MAX_VALUE, index);
			}
//			System.err.println( i + " " + Math.round(angles[i].X) + " " + intersection + " " + index );
		}
		Arrays.sort(minCount);
		for ( int i = 0; i < minCount.length; i++ )
		{
			if ( minCount[i].Z != -1 )
			{
				int minIndex = (int) minCount[i].Z;
				finalLatticeList.add(lattices.elementAt(minIndex));
				VOIContour left = (VOIContour) lattices.elementAt(minIndex).getCurves().elementAt(0);
				VOIContour right = (VOIContour) lattices.elementAt(minIndex).getCurves().elementAt(1);
				Vector3f mid0 = Vector3f.add(left.elementAt(0), right.elementAt(0)); mid0.scale(0.5f);
				Vector3f mid1 = Vector3f.add(left.elementAt(1), right.elementAt(1)); mid1.scale(0.5f);
//				System.err.println( i + " " + minIndex + " " + mid0.distance(mid1) );
			}
		}
	}

	private static boolean testLattice( Vector<Vector3f> left, Vector<Vector3f> right, Vector<Vector3f> nose )
	{
		if ( (left.size() != right.size()) || (left.size() == 0) )
		{
			return false;
		}

		if ( nose != null )
		{
			Vector3f mid = Vector3f.add(left.lastElement(), right.lastElement());
			mid.scale(0.5f);
			
			float distance = -1;
			if ( nose.size() == 1 )
			{
				distance = mid.distance(nose.elementAt(0));
			}
			else if ( nose.size() >= 2 )
			{
				Vector3f midNose = Vector3f.add(nose.elementAt(0), nose.elementAt(1));
				midNose.scale(0.5f);
				distance = mid.distance(midNose);
			}
			if ( (distance != -1) && ((distance < noseP1MinDist) || (distance > noseP1MaxDist)) )
			{
//				System.err.println( distance );
				return false;
			}
		}
		
		for ( int i = 0; i < left.size(); i++ )
		{
			Vector3f mid1 = Vector3f.add(left.elementAt(i), right.elementAt(i));
			mid1.scale(0.5f);
			// distance measures the witdh of the worm at this point in the lattice model:
			float distance = left.elementAt(i).distance(right.elementAt(i));
			distance /= 2;

			// Check distance of sequential mid-points to make
			// sure they are not closer together than the 'width' of the worm
			// at this point:
			for ( int j = i+1; j < left.size(); j++ )
			{
				Vector3f mid2 = Vector3f.add(left.elementAt(j), right.elementAt(j));
				mid2.scale(0.5f);
				
				if ( mid1.distance(mid2) < distance)
				{
					return false;
				}
			}
		}
		
		float length = 0;
		// Check the sequence distances between midpoints
		// the distance expected ranges vary based on the pair number:
		for ( int i = 0; i < left.size() - 1; i++ )
		{
			Vector3f mid1 = Vector3f.add(left.elementAt(i), right.elementAt(i));     mid1.scale(0.5f);
			Vector3f mid2 = Vector3f.add(left.elementAt(i+1), right.elementAt(i+1)); mid2.scale(0.5f);
			float sequenceDistance = mid1.distance(mid2);
			length += sequenceDistance;
			if ( (i <= 3) && ((sequenceDistance < 5) || (sequenceDistance > 26)) )
			{
				return false;
			}
			if ( (i > 3) && (i <= 6) && ((sequenceDistance < 5) || (sequenceDistance > 16)) )
			{
				return false;
			}
			if ( (i == 7) && ((sequenceDistance < 10) || (sequenceDistance > 26)) )
			{
				return false;
			}
			if ( (i == 8) && ((sequenceDistance < 5) || (sequenceDistance > 16)) )
			{
				return false;
			}
		}
		
		// Check total length:
		if ( (length < 100) || (length > 140) )
		{
			return false;
		}		

		// Check the 'width' of the pairs:
		float maxWidth = -Float.MAX_VALUE;
		int maxIndex = -1;
		float minWidth = Float.MAX_VALUE;
		int minIndex = -1;
		int countFirst = 0;
		int countSecond = 0;
		float avgWidthFirst4 = 0;
		float avgWidthLast5 = 0;
		for ( int i = 0; i < left.size(); i++ )
		{
			float dist = left.elementAt(i).distance( right.elementAt(i) );
			if ( i < 5 )
			{
				avgWidthLast5 += dist;
				countSecond++;
			}
			else
			{
				avgWidthFirst4 += dist;	
				countFirst++;
			}
			if ( dist > maxWidth )
			{
				maxWidth = dist;
				maxIndex = i;
			}			
			if ( dist < minWidth )
			{
				minWidth = dist;
				minIndex = i;
			}				
		}
		avgWidthFirst4 /= (float)countFirst;
		avgWidthLast5 /= (float)countSecond;
		if ( (avgWidthFirst4 > avgWidthLast5) && (maxIndex >= 4) && (minIndex <= 4) )
		{			
			// Check the amount of 'bend' in the worm:
			if ( nose != null )
			{
				if ( nose.size() == 1 )
				{
					Vector3f nosePt = new Vector3f(nose.elementAt(0));
					left.add(nosePt);
					right.add(nosePt);
				}
				else if ( nose.size() >= 2 )
				{
					Vector3f nosePt0 = new Vector3f(nose.elementAt(0));
					Vector3f nosePt1 = new Vector3f(nose.elementAt(1));

					float distanceL0 = left.elementAt(0).distance(nosePt0);
					float distanceL1 = left.elementAt(0).distance(nosePt1);
					if ( distanceL0 < distanceL1 )
					{
						left.add(nosePt0);
						right.add(nosePt1);
					}
					else if ( distanceL0 > distanceL1 )
					{
						left.add(nosePt1);
						right.add(nosePt0);					
					}
					else
					{
						float distanceR0 = right.elementAt(0).distance(nosePt0);
						float distanceR1 = right.elementAt(0).distance(nosePt1);
						if ( distanceR0 < distanceR1 )
						{
							right.add(nosePt0);
							left.add(nosePt1);
						}
						else
						{
							right.add(nosePt1);			
							left.add(nosePt0);		
						}					
					}
				}
			}
			float bendSum = measureCurvature(left, right);
			if ( (bendSum < 6) || (bendSum > 12) )
			{
				return false;
			}
			
			return true;
		}		
		
		return false;
	}
	
	
	private static void sequencePairs( long startTime, LatticeModel model, Vector<Vector3f> nose, Vector<Vector3f> positions, Vector<int[]> sequence, Vector<int[]> pairs, int[] lastPair, int count, int[] total, Vector<Vector<int[]>> sequenceList )
	{		
		Vector<int[]> newSequence = new Vector<int[]>();
		newSequence.addAll(sequence);
		newSequence.add(lastPair);
		
		boolean allFound = (count == newSequence.size() * 2);
		if ( allFound )
		{
			Vector<Vector3f> left = new Vector<Vector3f>();
			Vector<Vector3f> right = new Vector<Vector3f>();
			for ( int i = 0; i < newSequence.size(); i++ )
			{
				int[] pair = newSequence.elementAt(i);
				left.add(positions.elementAt(pair[0]));
				right.add(positions.elementAt(pair[1]));
			}
			if ( testLattice(left, right, nose) )
			{
				total[0]++;
				sequenceList.add(newSequence);				
			}
			return;
		}
		if ( count <= (newSequence.size() * 2) )
		{
			return;
		}
		double elapsedTime = AlgorithmBase.computeElapsedTime(startTime);
		if ( elapsedTime > 600 )
		{
			return;
		}
		
		// Add a new 'rung' to the lattice:
		Vector<int[]> newPairs = new Vector<int[]>();
		newPairs.addAll(pairs);
		newPairs.remove(lastPair);
		for ( int i = 0; i < newPairs.size(); i++ )
		{
			int[] pair = newPairs.elementAt(i);
			boolean found = false;
			for ( int j = 0; j < newSequence.size(); j++ )
			{
				int[] pair2 = newSequence.elementAt(j);
				if ( (pair2[0] == pair[0]) || (pair2[0] == pair[1]) || (pair2[1] == pair[0]) || (pair2[1] == pair[1]) )
				{
					found = true;
					break;
				}
			}
			if ( !found )
			{

				Vector3f edge00 = Vector3f.sub( positions.elementAt(pair[0]), positions.elementAt(lastPair[0]) );  float L00 = edge00.normalize();
				Vector3f edge11 = Vector3f.sub( positions.elementAt(pair[1]), positions.elementAt(lastPair[1]) );  float L11 = edge11.normalize();
				float angle1 = edge00.angle(edge11);

				Vector3f edge01 = Vector3f.sub( positions.elementAt(pair[0]), positions.elementAt(lastPair[1]) );  float L01 = edge01.normalize();
				Vector3f edge10 = Vector3f.sub( positions.elementAt(pair[1]), positions.elementAt(lastPair[0]) );  float L10 = edge10.normalize();
				float angle2 = edge01.angle(edge10);
				float diff = Math.abs(L01-L10);
				if ( (angle1 < angle2) && (angle1 < (Math.PI/2f)) )
				{
					diff = Math.abs(L00-L11);
					if ( diff <= 12 )
					{
						Vector3f midPt = Vector3f.add(positions.elementAt(pair[0]), positions.elementAt(pair[1]));
						midPt.scale(0.5f);
						Vector3f midPtSequence = Vector3f.add(positions.elementAt(lastPair[0]), positions.elementAt(lastPair[1]));
						midPtSequence.scale(0.5f);
						float midDistance = midPt.distance(midPtSequence);
						

						if ( (midDistance > 4) && (midDistance < 30) && (L00 > 4) && (L00 < 25) && (L11 > 4) && (L11 < 25))
						{
							sequencePairs( startTime, model, nose, positions, newSequence, newPairs, pair, count, total, sequenceList );
						}
					}
				}
				else if ( angle2 < (Math.PI/2f) )
				{
					diff = Math.abs(L01-L10);
					if ( diff <= 12 )
					{
						Vector3f midPt = Vector3f.add(positions.elementAt(pair[0]), positions.elementAt(pair[1]));
						midPt.scale(0.5f);
						Vector3f midPtSequence = Vector3f.add(positions.elementAt(lastPair[0]), positions.elementAt(lastPair[1]));
						midPtSequence.scale(0.5f);
						float midDistance = midPt.distance(midPtSequence);
						

						if ( (midDistance > 4) && (midDistance < 30) && (L01 > 4) && (L01 < 25) && (L10 > 4) && (L10 < 25) )
						{
							sequencePairs( startTime, model, nose, positions, newSequence, newPairs, new int[]{pair[1],pair[0]}, count, total, sequenceList );
//							sequencePairs( newSequence, newPairs, pair, count, total, sequenceList );
						}
					}
				}
			}
		}

	}
	
	/**
	 * @param pairs  list of potential seam cell pairs
	 * @param size   the total number of seam cells
	 * @return
	 */
	private static boolean checkPairs( Vector<int[]> pairs, int size )
	{
		boolean allFound = true;
		// if a positions only occurs in one pair, remove all other occurances of it's partner:
		int[] counts = new int[size];
		for ( int i = 0; i < size; i++ )
		{
			counts[i] = 0;
			for ( int j = 0; j < pairs.size(); j++ )
			{
				int[] pair = pairs.elementAt(j);
				if ( pair[0] == i || pair[1] == i )
				{
					counts[i]++;
				}
			}
			if ( counts[i] == 0 )
			{
				// seam cell is not in any pair:
				allFound = false;
			}
		}
		
		// Create the essential pair list of seam cell pairs where one of the
		// seam cells in the pair occurs in only one pair:
		Vector<int[]> essentialPairs = new Vector<int[]>();
		for ( int i = 0; i < size; i++ )
		{
			if ( counts[i] == 1 )
			{
				// found an essential pair:
				for ( int j = pairs.size() - 1; j >= 0; j-- )
				{
					int[] pair = pairs.elementAt(j);
					// remove essential pair from the pairs list and add
					// to essential list:
					if ( pair[0] == i || pair[1] == i )
					{
						essentialPairs.add(pairs.remove(j));
					}
				}				
			}
		}
		
		// When a seam cell is part of an essential pair both
		// seam cells are limited to that pair only
		// this removes pairs that contain the partner of the
		// singleton seam cell:
		for ( int i = 0; i < essentialPairs.size(); i++ )
		{
			// get the essential pair:
			int[] essentialPair = essentialPairs.elementAt(i);
			// look through remaining non-essential pairs:
			for ( int j = pairs.size() - 1; j >= 0; j-- )
			{
				int[] pair = pairs.elementAt(j);
				// if the non-essential pair contains the partner to the
				// singleton of the essential pair remove it:
				if ( pair[0] == essentialPair[0] || pair[1] == essentialPair[0] ||
					 pair[0] == essentialPair[1] || pair[1] == essentialPair[1] )
				{
					pairs.remove(j);
				}
			}				
		}

		// Add all essential pairs back in:
		pairs.addAll( essentialPairs );
		return allFound;
	}

	/**
	 * Checks two seam cells to see if they form a potential pair.
	 * The test looks at the mid point between the two seam cells
	 * and if there is another seam cell that is closer to the mid point
	 * than 1/2 the distances between the two seam cells the seam cells can
	 * be ruled out as a potential pair.
	 * @param pair
	 * @param positions
	 * @return
	 */
	private static boolean midPointFail( int[] pair, Vector<Vector3f> positions )
	{
		// Get the positions of the two seam cells:
		Vector3f p1 = positions.elementAt(pair[0]);
		Vector3f p2 = positions.elementAt(pair[1]);
		// calculate the mid point:
		Vector3f midPt = Vector3f.add(p1,p2);
		midPt.scale(0.5f);
		// calculate the distance between the two seam cells and divide by 2:
		float distance = p1.distance(p2);
		distance /= 2f;
		// Loop through all seam cells:
		for ( int i = 0; i < positions.size(); i++ )
		{
			if ( (i != pair[0]) && (i != pair[1]) )
			{
				if ( midPt.distance(positions.elementAt(i)) < distance )
				{
					// Fails to pass the mid-point test and 
					// can be ruled out as a potential pair:
					return true;
				}
			}
		}
		// MidPoint Test Pass:
		return false;
	}

	/**
	 * Checks two seam cells to see if they form a potential pair.
	 * The test looks at the mid point between the two seam cells
	 * and if there is another seam cell that is closer to the mid point
	 * than 1/2 the distances between the two seam cells the seam cells can
	 * be ruled out as a potential pair.
	 * @param pair
	 * @param positions
	 * @return
	 */
	private static boolean midPointFail( int pair0, int pair1, Vector<Vector3f> positions )
	{
		// Get the positions of the two seam cells:
		Vector3f p1 = positions.elementAt(pair0);
		Vector3f p2 = positions.elementAt(pair1);
		// calculate the mid point:
		Vector3f midPt = Vector3f.add(p1,p2);
		midPt.scale(0.5f);
		// calculate the distance between the two seam cells and divide by 2:
		float distance = p1.distance(p2);
		distance /= 2f;
		// Loop through all seam cells:
		for ( int i = 0; i < positions.size(); i++ )
		{
			if ( (i != pair0) && (i != pair1) )
			{
				if ( midPt.distance(positions.elementAt(i)) < distance )
				{
					// Fails to pass the mid-point test and 
					// can be ruled out as a potential pair:
					return true;
				}
			}
		}
		// MidPoint Test Pass:
		return false;
	}


	private boolean equalSequence( int[] sequence1, int[] sequence2 )
	{
		if ( sequence1.length != sequence2.length )
		{
			return false;
		}
		for ( int i = 0; i < sequence1.length; i++ )
		{
			if ( sequence1[i] != sequence2[i] )
			{
				return false;
			}
		}
		return true;
	}


	private int minSequenceDist = 4;
	private int maxSequenceDist = 20;
	private int sequenceDistDiff = 10;
	private static final int minPairDist = 5;
	private static final int maxPairDist = 15;
	private static final float tenMinDist = 1;
	private static final float tenMaxDist = 5;
	private static final float noseP1MinDist = 10;
	private static final float noseP1MaxDist = 30;
	private int sequenceTwist1 = 70;
	private int sequenceTwist2 = 100;
	private int sequenceBendMax = 170;


	private boolean equalLatticePairSet( VOI lattice1, VOI lattice2 )
	{
		VOIContour left1 = (VOIContour) lattice1.getCurves().elementAt(0);
		VOIContour right1 = (VOIContour) lattice1.getCurves().elementAt(1);

		VOIContour left2 = (VOIContour) lattice2.getCurves().elementAt(0);
		VOIContour right2 = (VOIContour) lattice2.getCurves().elementAt(1);

		if ( (left1.size() != left2.size()) || (right1.size() != right2.size()) )
		{
			return false;
		}

		boolean matches = true;
		for ( int j = 0; j < left1.size(); j++ )
		{
			if ( !left1.elementAt(j).equals( left2.elementAt(j)) || !right1.elementAt(j).equals( right2.elementAt(j)) )
			{
				matches = false;
			}
		}
		if ( matches )
		{
			return true;
		}
		for ( int j = 0; j < left1.size(); j++ )
		{
			if ( !left1.elementAt(j).equals( right2.elementAt(j)) || !right1.elementAt(j).equals( left2.elementAt(j)) )
			{
				matches = false;
			}
		}
		if ( matches )
		{
			return true;
		}
		return false;
	}

	private void flipLattices()
	{
		if ( includeRange != null )
		{
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{
					flipLattice( includeRange.elementAt(i) );
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
					flipLattice( fileCount );
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

		//		System.err.println( "Done segmentation" );
	}



	private void flipLatticesFromCSV()
	{
		String fileName = baseFileDir + File.separator + "Timepoints with left-right switch.csv";

		File file = new File(fileName);
		if ( file.exists() )
		{
			FileReader fr;
			try {
				fr = new FileReader(file);
				BufferedReader br = new BufferedReader(fr);
				String line = br.readLine();
				line = br.readLine();
				while ( line != null )
				{
					int index = -1;
					String[] parsed = line.split( "," );
					for ( int i = 0; i < parsed.length; i++ )
					{
						if ( parsed[i].length() > 0 )
						{
							index = Integer.valueOf( parsed[i] );
							//							System.err.println(line + "    " + index );
							break;
						}
					}
					line = br.readLine();

					if ( index != -1 )
					{
						flipLattice(index);
					}
				}
			} catch (FileNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	private void flipLattice( int index )
	{

		String fileName = baseFileNameText.getText() + "_" + index + ".tif";
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

			fileName = baseFileNameText.getText() + "_"  + index + File.separator + "lattice_0";
			VOIVector lattice = new VOIVector();
			String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
			loadAllVOIsFrom(wormImageA, voiDir, false, lattice, false);
			if ( lattice.size() > 0 )
			{
				if ( (lattice.elementAt(0) != null) && (lattice.elementAt(0).getCurves().size() == 2) )
				{
					VOIContour left = (VOIContour) lattice.elementAt(0).getCurves().elementAt(0);
					VOIContour right = (VOIContour) lattice.elementAt(0).getCurves().elementAt(1);

					VOI swapLattice = new VOI((short) 0, "lattice", 1, VOI.POLYLINE );
					swapLattice.getCurves().add(right);
					swapLattice.getCurves().add(left);
					wormImageA.unregisterAllVOIs();
					LatticeModel model = new LatticeModel( wormImageA, swapLattice );
					model.saveLattice( baseFileDir + File.separator + baseFileNameText.getText() + "_"  + index + File.separator, "lattice_0" );
					model.dispose();
					model = null;
				}	
			}

			fileName = baseFileNameText.getText() + "_"  + index + File.separator + "lattice_1";
			lattice = new VOIVector();
			voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
			loadAllVOIsFrom(wormImageA, voiDir, false, lattice, false);
			if ( lattice.size() > 0 )
			{
				if ( (lattice.elementAt(0) != null) && (lattice.elementAt(0).getCurves().size() == 2) )
				{
					VOIContour left = (VOIContour) lattice.elementAt(0).getCurves().elementAt(0);
					VOIContour right = (VOIContour) lattice.elementAt(0).getCurves().elementAt(1);

					VOI swapLattice = new VOI((short) 0, "lattice", 1, VOI.POLYLINE );
					swapLattice.getCurves().add(right);
					swapLattice.getCurves().add(left);
					wormImageA.unregisterAllVOIs();
					LatticeModel model = new LatticeModel( wormImageA, swapLattice );
					model.saveLattice( baseFileDir + File.separator + baseFileNameText.getText() + "_"  + index + File.separator, "lattice_1" );
					model.dispose();
					model = null;
				}
			}
		}
	}

	private void generateAnnotationAnimation()
	{
		calcStatistics( false );
		int[] extents = new int[3];
		Vector3f min = new Vector3f(  Float.MAX_VALUE,  Float.MAX_VALUE,  Float.MAX_VALUE );
		Vector3f max = new Vector3f( -Float.MAX_VALUE, -Float.MAX_VALUE, -Float.MAX_VALUE );

		for ( int i = 0; i < annotationList.size(); i++ )
		{
			VOI annotation = annotationList.elementAt(i);
			for ( int j = 0; j < annotation.getCurves().size(); j++ )
			{
				VOIText text = (VOIText)annotation.getCurves().elementAt(j);
				min.min(text.elementAt(0));
				max.max(text.elementAt(0));
			}
		}
		extents[0] = (int)Math.max( 30, (max.X - min.X) + 10);
		extents[1] = (int)Math.max( 30, (max.Y - min.Y) + 10);
		extents[2] = (int)Math.max( 30, (max.Z - min.Z) + 10);

		System.err.println( "generateAnnotationAnimation " + annotationList.size() );		
		ModelImage animationImage = new ModelImage( ModelStorageBase.BOOLEAN, extents, "animationImage" );
		//		JDialogBase.updateFileInfo( wormImageA, animationImage );
		String outputDirName = baseFileDir + File.separator + "animation" + File.separator;
		final File outputFileDir = new File(outputDirName);

		if (outputFileDir.exists() && outputFileDir.isDirectory()) {
			String[] list = outputFileDir.list();
			for ( int i = 0; i < list.length; i++ )
			{
				File lrFile = new File( outputFileDir + list[i] );
				lrFile.delete();
			}
		} else if (outputFileDir.exists() && !outputFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			outputFileDir.mkdir();
		}

		animationImage.setImageDirectory( outputDirName );


		int maxCount = -1;
		for ( int i = 0; i < annotationList.size(); i++ )
		{
			if ( annotationList.elementAt(i).getCurves().size() > maxCount )
			{
				maxCount = annotationList.elementAt(i).getCurves().size();
			}
		}
		annotationNames = new Vector<String>();
		for ( int i = 0; i < annotationList.size(); i++ )
		{
			if  ( annotationList.elementAt(i).getCurves().size() == maxCount )
			{
				for ( int j = 0; j < maxCount; j++ )
				{
					VOIText text = (VOIText) annotationList.elementAt(i).getCurves().elementAt(j);
					annotationNames.add( new String(text.getText()) );
				}
				break;
			}
		}

		triVolume = new VolumeTriPlanarInterface(animationImage, null);
		triVolume.addConfiguredListener(this);

		if(wormImageA != null) {
			wormImageA.disposeLocal();
			wormImageA = null;
		}
	}

	Vector<String> annotationNames;
	private void annotationAnimationFromSpreadSheet()
	{
		int[] extents = new int[3];
		VOIVector tempList = new VOIVector();
		Vector< int[] > timesList = new Vector< int[] >();
		ViewJProgressBar progress = new  ViewJProgressBar( "Generating Animation", "", 0, 100, false);
		progress.dispose();
		progress = null;

		String inputDirName = baseFileDir + File.separator;
		//		System.err.println( inputDirName );
		final File inputFileDir = new File(inputDirName);

		Vector3f min = new Vector3f(  Float.MAX_VALUE,  Float.MAX_VALUE,  Float.MAX_VALUE );
		Vector3f max = new Vector3f( -Float.MAX_VALUE, -Float.MAX_VALUE, -Float.MAX_VALUE );
		int timeCount, maxTimeCount = -1, minTimeCount = Integer.MAX_VALUE;
		int maxIndex = -1;
		int fileIndex = 0;
		if (inputFileDir.exists() && inputFileDir.isDirectory()) {
			String[] list = inputFileDir.list();
			for ( int i = 0; i < list.length; i++ )
			{
				File annotationFile = new File( inputFileDir + File.separator + list[i] );
				if ( annotationFile.isDirectory() )
				{
					continue;
				}
				if ( !list[i].endsWith(".csv") )
				{
					continue;
				}
				int index = list[i].indexOf(".");
				String annotationName = new String(list[i]);
				if ( index != -1 )
				{
					annotationName = annotationName.substring(0, index);
				}
				//        		System.err.println( annotationName );

				int startTime = -1;
				int endTime = -1;
				timeCount = 0;
				VOI annotation = new VOI( (short)i, annotationName, VOI.ANNOTATION, 0 );
				Vector<Integer> times = new Vector<Integer>();
				FileReader fr;
				try {
					int red = 255;
					int green = 255;
					int blue = 255;
					fr = new FileReader(annotationFile);
					BufferedReader br = new BufferedReader(fr);

					String line = br.readLine();
					line = br.readLine();
					//    				System.err.println(line);
					String[] parsed = line.split( "," );
					if ( parsed.length > 0 )
					{
						if ( parsed[0].equals("color") )
						{
							red    = (parsed.length > 1) ? (parsed[1].length() > 0) ? Integer.valueOf( parsed[1] ) : 0 : 0; 
							green  = (parsed.length > 2) ? (parsed[2].length() > 0) ? Integer.valueOf( parsed[2] ) : 0 : 0; 
							blue   = (parsed.length > 3) ? (parsed[3].length() > 0) ? Integer.valueOf( parsed[3] ) : 0 : 0; 

							line = br.readLine();
						}
					}
					while ( line != null )
					{
						//        				System.err.println(line);
						int time = -1;
						float x = 0, y = 0, z = 0;
						parsed = line.split( "," );
						time = (parsed.length > 0) ? (parsed[0].length() > 0) ? Integer.valueOf( parsed[0] ) : -1 : -1; 
						x    = (parsed.length > 1) ? (parsed[1].length() > 0) ? Float.valueOf( parsed[1] ) : 0 : 0; 
						y    = (parsed.length > 2) ? (parsed[2].length() > 0) ? Float.valueOf( parsed[2] ) : 0 : 0; 
						z    = (parsed.length > 3) ? (parsed[3].length() > 0) ? Float.valueOf( parsed[3] ) : 0 : 0; 

						if ( time != -1 )
						{
							if ( startTime == -1 )
							{
								startTime = time;
							}
							endTime = time;
						}
						if ( (time != -1) && (z >= 0) && (parsed.length > 3) )
						{
							VOIText text = new VOIText();
							text.setText( list[i] );
							text.setColor( new Color(red, green, blue) );
							text.add( new Vector3f( x, y, z ) );
							text.add( new Vector3f( x, y, z ) );
							text.setText(annotationName);
							annotation.getCurves().add(text);

							times.add( time );
							timeCount++;

							min.min( text.elementAt(0) );
							max.max( text.elementAt(0) );

							//    						if ( text.elementAt(0).Z < 0 )
							//    						{
							//    							System.err.println(list[i] );
							//    						}
						}
						line = br.readLine();
					}
					br.close();
					fr.close();

					tempList.add(annotation);

					int[] timesArray = new int[times.size()];
					for ( int j = 0; j < times.size(); j++ )
					{
						timesArray[j] = times.elementAt(j);
					}
					timesList.add( timesArray );
				} catch (FileNotFoundException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				if ( timeCount < minTimeCount )
				{
					minTimeCount = timeCount;
					maxIndex = fileIndex;
				}
				if ( timeCount > maxTimeCount )
				{
					maxTimeCount = timeCount;
					maxIndex = fileIndex;
				}
				fileIndex++;
			}
		}

		//        System.err.println( minTimeCount + " " + maxTimeCount + " " + (minTimeCount == maxTimeCount ) );

		//        System.err.println( timesList.size() + " " + tempList.size() );
		int[] times = timesList.elementAt( maxIndex );
		VOI curve = tempList.elementAt( maxIndex );
		//		for ( int j = 0; j < times.length; j++ )
		//		{
		//			System.err.println( curve.getName() + " " + times[j] );
		//		}


		annotationList = new VOIVector();
		for ( int i = 0; i < times.length; i++ )
		{
			int timeStep = times[i];
			VOI annotation = new VOI( (short)i, "time" + timeStep, VOI.ANNOTATION, 0 );

			//			System.err.print( timeStep );
			for ( int j = 0; j < timesList.size(); j++ )
			{
				int[] currentTimes = timesList.elementAt(j);
				for ( int k = 0; k < currentTimes.length; k++ )
				{
					if ( timeStep == currentTimes[k] )
					{
						VOIText text = new VOIText(tempList.elementAt(j).getCurves().elementAt(k));

						text.setText( ((VOIText)tempList.elementAt(j).getCurves().elementAt(k)).getText() );
						text.setColor( ((VOIText)tempList.elementAt(j).getCurves().elementAt(k)).getColor() );
						annotation.getCurves().add( text );

						//        				System.err.print( " " + text.getText() );
						break;
					}
				}
			}
			//			System.err.println( "" );  		


			annotationList.add(annotation);
		}
		tempList = null;
		timesList = null;


		int maxCount = -1;
		int maxCountIndex = -1;
		for ( int i = 0; i < annotationList.size(); i++ )
		{
			if ( annotationList.elementAt(i).getCurves().size() > maxCount )
			{
				maxCount = annotationList.elementAt(i).getCurves().size();
				maxCountIndex = i;
			}
		}
		annotationNames = new Vector<String>();
		for ( int i = 0; i < annotationList.elementAt(maxCountIndex).getCurves().size(); i++ )
		{
			VOIText text = (VOIText) annotationList.elementAt(maxCountIndex).getCurves().elementAt(i);
			annotationNames.add( new String(text.getText()) );
		}

		//		System.err.println( min );
		//		System.err.println( max );

		extents[0] = (int)Math.max( 30, (max.X - min.X) + 10);
		extents[1] = (int)Math.max( 30, (max.Y - min.Y) + 10);
		extents[2] = (int)Math.max( 30, (max.Z - min.Z) + 10);

		ModelImage animationImage = new ModelImage( ModelStorageBase.BOOLEAN, extents, "animationImage" );
		String outputDirName = baseFileDir + File.separator + "animation" + File.separator;
		final File outputFileDir = new File(outputDirName);

		if (outputFileDir.exists() && outputFileDir.isDirectory()) {
			String[] list = outputFileDir.list();
			for ( int i = 0; i < list.length; i++ )
			{
				File lrFile = new File( outputFileDir + list[i] );
				lrFile.delete();
			}
		} else if (outputFileDir.exists() && !outputFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			outputFileDir.mkdir();
		}

		animationImage.setImageDirectory( outputDirName );		
		triVolume = new VolumeTriPlanarInterface(animationImage, null);
		triVolume.addConfiguredListener(this);
	}


	private void clean()
	{
		if ( includeRange != null )
		{
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String dirName;
//				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + "output_images" + File.separator;
//				deleteDirectory(dirName);
//				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + "segmentation" + File.separator;
//				deleteDirectory(dirName);
//				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + "straightened_lattice" + File.separator;
//				deleteDirectory(dirName);
//				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + "straightened_annotations" + File.separator;
//				deleteDirectory(dirName);
//				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + "statistics" + File.separator;
//				deleteDirectory(dirName);
				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + autoSeamCellSegmentationOutput + File.separator;
				deleteDirectory(dirName);
				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + editSeamCellOutput + File.separator;
				deleteDirectory(dirName);
				for ( int j = 0; j < 15; j++ )
				{
					dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + autoLatticeGenerationOutput + j + File.separator;
					deleteDirectory(dirName);
				}
				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + editLatticeOutput + File.separator;
				deleteDirectory(dirName);
			}
		}
	}     

	private void deleteDirectory( String dirName )
	{
		final File fileDir = new File(dirName);

		if (fileDir.exists() && fileDir.isDirectory()) {
			String[] list = fileDir.list();
			for ( int i = 0; i < list.length; i++ )
			{
				File file = new File( dirName + list[i] );
				file.delete();
				//        		System.err.println( "Deleting " + dirName + list[i] );
			}
			fileDir.delete();
		}
	}
	
	public static void createMaximumProjectionAVI( JProgressBar batchProgress, final Vector<Integer> includeRange, final String baseFileDir, final String baseFileName)
	{
		registerImages(batchProgress, includeRange, baseFileDir, baseFileName);
		calcMaxProjection(batchProgress, includeRange, baseFileDir, baseFileName);		
	}

	private static void registerImages( JProgressBar batchProgress, final Vector<Integer> includeRange, final String baseFileDir, final String baseFileName)
	{
//		long startTime = System.currentTimeMillis();
		ModelImage image = null, prevImage = null;
		int[] maxExtents = new int[]{0,0,0};
		if ( includeRange != null )
		{
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String fileName = baseFileName + "_"  + includeRange.elementAt(i) + File.separator + 
						"output_images" + File.separator + baseFileName + "_" + includeRange.elementAt(i) + "_straight" + ".xml";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{
					FileIO fileIO = new FileIO();
					image = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);  
					int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 0;  
					int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 0;  
					int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 0;
					maxExtents[0] = Math.max(maxExtents[0], dimX);
					maxExtents[1] = Math.max(maxExtents[1], dimY);
					maxExtents[2] = Math.max(maxExtents[2], dimZ);
					if ( image != null )
					{
						image.disposeLocal();
						image = null;
					}
				}
			}

			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String fileName = baseFileName + "_"  + includeRange.elementAt(i) + File.separator + 
						"output_images" + File.separator + baseFileName + "_" + includeRange.elementAt(i) + "_straight" + ".xml";
				File voiFile = new File(baseFileDir + File.separator + fileName);

				File registrationDir = new File(baseFileDir + File.separator + baseFileName + "_"  + includeRange.elementAt(i) + File.separator + 
						"output_images");
				if ( voiFile.exists() )
				{
//					System.err.println( fileName );
					FileIO fileIO = new FileIO();
					image = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);  

					ModelImage result = register( prevImage, image, registrationDir, maxExtents );
					if ( result == null )
					{
						prevImage = image;
					}
					else
					{
						if ( image != null )
						{
							image.disposeLocal();
							image = null;
						}
						if ( prevImage != null )
						{
							prevImage.disposeLocal();
							prevImage = null;
						}
						prevImage = result;
					}
				}

				if ( batchProgress != null )
				{
					batchProgress.setValue((int)(100 * (float)(i+1)/(float)includeRange.size()));
					batchProgress.update(batchProgress.getGraphics());
				}
			}
		}

		if ( image != null )
		{
			image.disposeLocal();
			image = null;
		}
		if ( prevImage != null )
		{
			prevImage.disposeLocal();
			prevImage = null;
		}


//		long now = System.currentTimeMillis();
//
//		double elapsedTime = (double) (now - startTime);
//
//		// if elasedTime is invalid, then set it to 0
//		if (elapsedTime <= 0) {
//			elapsedTime = (double) 0.0;
//		}
//
//		System.err.println( "Elapsed time = " + (elapsedTime / 1000.0) ); // return in seconds!!
	}


	private static void calcMaxProjection(JProgressBar batchProgress, final Vector<Integer> includeRange, final String baseFileDir, final String baseFileName)
	{
		ModelImage image = null, maximumProjectionImage = null;
		int[] currentMPImage = new int[]{0};
		if ( includeRange != null )
		{
			int mpSliceCount = 0;
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String fileName = baseFileName + "_"  + includeRange.elementAt(i) + File.separator + 
						"output_images" + File.separator + baseFileName + "_" + includeRange.elementAt(i) + "_straight_register" + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{					
					mpSliceCount++;
				}
			}
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String fileName = baseFileName + "_"  + includeRange.elementAt(i) + File.separator + 
						"output_images" + File.separator + baseFileName + "_" + includeRange.elementAt(i) + "_straight_register" + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);

//				System.err.println( fileName );
				if ( voiFile.exists() )
				{
					FileIO fileIO = new FileIO();
					if( image != null )
					{
						if ( (i%10) == 0 )
						{
							image.disposeLocal(true);
						}
						else
						{
							image.disposeLocal();
						}
						image = null;
					}
					image = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);
					if ( maximumProjectionImage == null )
					{
						int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1; // dimX
						int dimY = image.getExtents().length > 2 ? image.getExtents()[2] : 1; // use dimZ for y projection
						maximumProjectionImage = new ModelImage( image.getType(), new int[]{ dimX, dimY, mpSliceCount}, baseFileName + "_MP_Y.tif" );
						currentMPImage[0] = 0;
					}
					calcMaximumProjectionY( image, maximumProjectionImage, currentMPImage );
				}
				if ( batchProgress != null )
				{
					batchProgress.setValue((int)(100 * (float)(i+1)/(float)includeRange.size()));
					batchProgress.update(batchProgress.getGraphics());
				}
			}
		}
		
		if ( image != null )
		{
			image.disposeLocal();
			image = null;
		}

//		String fileName = baseFileName + "_MP_Y.tif";
//		File voiFile = new File(baseFileDir + File.separator + fileName);		
//		FileIO fileIO = new FileIO();
//		maximumProjectionImage = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);
		
		if ( maximumProjectionImage != null )
		{
//			fileName = baseFileDir + File.separator;
////			System.err.println( "Saving mp image to : " + fileName + " " + maximumProjectionImage.getImageName() + ".tif" );
//			ModelImage.saveImage( maximumProjectionImage, maximumProjectionImage.getImageName() + ".avi", fileName, false ); 
			
            final ImageStack is = ModelImageToImageJConversion.convert3D(maximumProjectionImage);
            new ImagePlus("ImageJ:" + maximumProjectionImage.getImageName(), is).show();
            new ij.ImageJ();
            
//			maximumProjectionImage.calcMinMax();
//			new ViewJFrameImage(maximumProjectionImage);
//			maximumProjectionImage.disposeLocal();
//			maximumProjectionImage = null;
		}
	}


	private void segmentOutline()
	{
		if ( includeRange != null )
		{
			int mpSliceCount = 0;
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String fileName = baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{					
					mpSliceCount++;
				}    				
			}
			currentMP = 0;

			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String fileName = baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{
					FileIO fileIO = new FileIO();
					if ( wormImageA != null )
					{
						wormImageA.disposeLocal();
						wormImageA = null;
					}
					wormImageA = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);
					if ( currentMP == 0 )
					{
						int dimX = wormImageA.getExtents().length > 0 ? wormImageA.getExtents()[0] : 1;
						int dimY = wormImageA.getExtents().length > 1 ? wormImageA.getExtents()[1] : 1;
						int dimZ = wormImageA.getExtents().length > 2 ? wormImageA.getExtents()[2] : 1;
						maximumProjectionImage = new ModelImage( wormImageA.getType(), new int[]{ dimX, dimY, dimZ}, baseFileNameText.getText() + "Outlines_MP_Z.tif" );
					}
					WormSegmentation.outline( wormImageA, maximumProjectionImage, currentMP++ );
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
			System.err.println( "Saving mp image to : " + fileName + " " + maximumProjectionImage.getImageName() + ".tif" );
			ModelImage.saveImage( maximumProjectionImage, maximumProjectionImage.getImageName() + ".tif", fileName, false ); 
			maximumProjectionImage.calcMinMax();
			new ViewJFrameImage(maximumProjectionImage);
		}
	}



	private static ModelImage register( ModelImage prevImage, ModelImage image, final File registrationDir, final int[] maxExtents )
	{
		int[] marginX = new int[2];
		int[] marginY = new int[2];
		int[] marginZ = new int[2];

		int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 0;  
		int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 0;  
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 0;

		ModelImage regInput;
		ModelImage regTo = prevImage;
		ModelImage result = null;
		ModelImage padResult = null;
		AlgorithmAddMargins pad = null;
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


			padResult = new ModelImage( image.getType(), maxExtents, image.getImageName() + "_pad" );
			JDialogBase.updateFileInfo( image, padResult );
			pad = new AlgorithmAddMargins(image, padResult, marginX, marginY, marginZ );
			pad.setRunningInSeparateThread(false);
			pad.run();
			regInput = padResult;

			pad.finalize();
			pad = null;
		}
		else
		{
			regInput = image;
		}
		if ( regTo == null )
		{
			result = regInput;
			saveTransformImage( registrationDir, result );
		}
		else
		{
//			AlgorithmConstrainedOAR3D reg = new AlgorithmConstrainedOAR3D( regTo, regInput, AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED, 6, 0,
//                    -10, 20, -10, 20, -10, 20, 3, 3, 3, new float[][]{{-30,-30,-30},{30,30,30}},                     
//                    true, true, false, true, 10, 10, 5);
			AlgorithmConstrainedOAR3D reg = new AlgorithmConstrainedOAR3D( regTo, regInput, AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED, 6, 0,
                    -10, 20, -10, 20, -10, 20, 3, 3, 3, new float[][]{{-30,-30,-30},{30,30,30}},                     
                    true, false, true, false, 10, 10, 5);
                    
//			AlgorithmRegOAR3D reg = new AlgorithmRegOAR3D(regTo, regInput,
//					AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED, 6, 0,
//					-10, 10, 5, 2, -10, 10, 5, 2, -15, 15, 7, 2, 
//					true, true, true, false, 2, 3);
			reg.setRunningInSeparateThread(false);
			reg.run();
			result = getTransformedImage( reg, regTo, regInput );
			if ( result != null )
			{
				saveTransformImage( registrationDir, result );
			}
			reg.finalize();
			reg = null;
		}
		if ( (result != padResult) && (padResult != null) )
		{
			padResult.disposeLocal();
			padResult = null;
		}
		
		return result;
	}

	private static ModelImage getTransformedImage( AlgorithmConstrainedOAR3D reg3, ModelImage refImage, ModelImage matchImage )
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



	private static void saveTransformImage( File dir, ModelImage image  ) 
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

	private static void calcMaximumProjectionY( ModelImage image, ModelImage maximumProjectionImage, int[] zSlice )
	{
//		System.err.println( image.getExtents()[0] + " " + image.getExtents()[1] + " " + image.getExtents()[2] );
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
					maximumProjectionImage.set(x,  y, zSlice[0], mp.getFloat(x, y, 0) );
				}
			}

			zSlice[0]++;
		}
		mipAlgo.finalize();
	}


	private void calcMaximumProjectionZ( ModelImage image )
	{
		int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
		AlgorithmMaximumIntensityProjection mipAlgo = null;
		// Make algorithm
		if (image.isColorImage()) {
			mipAlgo = new AlgorithmMaximumIntensityProjection(image, 
					0, dimZ -1, dimZ,
					image.getMinR(), image.getMaxR(),
					image.getMinG(), image.getMaxG(), 
					image.getMinB(), image.getMaxB(),
					true, false, 2);    
		}
		else {
			mipAlgo = new AlgorithmMaximumIntensityProjection(image, 
					0, dimZ -1, dimZ,
					image.getMin(), image.getMax(),
					true, false, 2 );
		}
		mipAlgo.setRunningInSeparateThread(false);
		mipAlgo.run();

		Vector<ModelImage> resultImages = ((AlgorithmMaximumIntensityProjection)mipAlgo).getResultImage();
		if ( resultImages.size() > 0 )
		{
			ModelImage mp = resultImages.elementAt(0);
			int dimX = maximumProjectionImage.getExtents().length > 0 ? maximumProjectionImage.getExtents()[0] : 1;
			int dimY = maximumProjectionImage.getExtents().length > 1 ? maximumProjectionImage.getExtents()[1] : 1;

			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					maximumProjectionImage.set(x,  y, currentMP, mp.getFloat(x, y, 0) );
				}
			}

			currentMP++;
			
			mp.disposeLocal();
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
		panel.setBorder(buildTitledBorder("Algorithms"));
		panel.setForeground(Color.black);

		gbc.gridx = 0;
		segmentSeamCells = gui.buildCheckBox("segment seam cells", false );
		panel.add(segmentSeamCells.getParent(), gbc);
		gbc.gridy++;
		
		gbc.gridx = 0;
		buildLattice = gui.buildCheckBox("build lattice", false );
		//		buildLattice.setEnabled(false);
		panel.add(buildLattice.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		latticeStraighten = gui.buildCheckBox("Straighten Lattices", false );
		panel.add(latticeStraighten.getParent(), gbc);
		gbc.gridy++;


		gbc.gridx = 0;
		calcStatistics = gui.buildCheckBox("calculate staticstics", false );
		panel.add(calcStatistics.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		generateTrainingData = gui.buildCheckBox("generate training data", false );
		//		panel.add(generateTrainingData.getParent(), gbc);
		//		gbc.gridy++;

		gbc.gridx = 0;
		segmentAnnotations = gui.buildCheckBox("segment from annotations", false );
		//		panel.add(segmentAnnotations.getParent(), gbc);
		//		gbc.gridy++;

		gbc.gridx = 0;
		calcAnnotationStats = gui.buildCheckBox("calculate statistics from annotations", false );
		//		panel.add(calcAnnotationStats.getParent(), gbc);
		//		gbc.gridy++;

		gbc.gridx = 0;
		registerImages = gui.buildCheckBox("register images", false );
		panel.add(registerImages.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		calcMaxProjection = gui.buildCheckBox("calculate and display maximum intensity projections", false );
		panel.add(calcMaxProjection.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		annotationAnimationFromSpreadSheet = gui.buildCheckBox("generate annotation animation from spreadsheets", false );
		panel.add(annotationAnimationFromSpreadSheet.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		annotationAnimation = gui.buildCheckBox("generate annotation animation from straightened annotation files", false );
		panel.add(annotationAnimation.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		clean = gui.buildCheckBox("clean up and remove all generated images and files", false );
		panel.add(clean.getParent(), gbc);
		gbc.gridy++;

		gbc.gridx = 0;
		flipLattices = gui.buildCheckBox("flip lattices", false );
		panel.add(flipLattices.getParent(), gbc);
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
	public static void loadAllVOIsFrom(ModelImage image, final String voiDir, boolean quietMode, VOIVector resultVector, boolean registerVOIs) {

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

				fileVOI = new FileVOI( (filenames.elementAt(i)), voiDir, image);

				VOIs = fileVOI.readVOI(isLabel.get(i));

				for (j = 0; j < VOIs.length; j++) {

                    if ( registerVOIs )
                    {   	
                    	image.registerVOI(VOIs[j]);
                    }
					if ( resultVector != null )
					{
						resultVector.add(VOIs[j]);
					}
				}
			}
            // when everything's done, notify the image listeners
            if ( registerVOIs )
            {   	
            	image.notifyImageDisplayListeners();
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

	private void readLatticePositions( String fileName, int ID, Vector<VOI> annotationStats )
	{
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

				VOI annotationVOI = new VOI( (short)ID, "time" + ID, VOI.ANNOTATION, 0 );
				while ( line != null )
				{
					String[] parsed = line.split( "," );
					if ( parsed.length != 0 )
					{
						String name  = (parsed.length > 0) ? (parsed[0].length() > 0) ? new String( parsed[0] ) : "" : "";
						float x    = (parsed.length > 1) ? (parsed[1].length() > 0) ? Float.valueOf( parsed[1] ) : 0 : 0; 
						float y    = (parsed.length > 2) ? (parsed[2].length() > 0) ? Float.valueOf( parsed[2] ) : 0 : 0; 
						float z    = (parsed.length > 3) ? (parsed[3].length() > 0) ? Float.valueOf( parsed[3] ) : 0 : 0; 

						x    = (parsed.length > 4) ? (parsed[4].length() > 0) ? Float.valueOf( parsed[4] ) : 0 : 0; 
						y    = (parsed.length > 5) ? (parsed[5].length() > 0) ? Float.valueOf( parsed[5] ) : 0 : 0; 
						z    = (parsed.length > 6) ? (parsed[6].length() > 0) ? Float.valueOf( parsed[6] ) : 0 : 0; 

						if ( !name.equals("") )
						{
							VOIText text = new VOIText();
							text.setText( name );
							text.add( new Vector3f( x, y, z ) );
							text.add( new Vector3f( x+1, y, z ) );
							annotationVOI.getCurves().add(text);
						}
					}
					line = br.readLine();
				}
				annotationStats.add( annotationVOI );
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

	private VOI readLatticePositions( File file, int ID )
	{
		VOIContour left = new VOIContour(false);
		VOIContour right = new VOIContour(false);
		FileReader fr;
		try {
			fr = new FileReader(file);
			BufferedReader br = new BufferedReader(fr);
			String line = br.readLine();
			line = br.readLine();
			while ( line != null )
			{
				float leftVolume = 0, rightVolume = 0;
				Vector3f leftPos = new Vector3f();
				StringTokenizer st = new StringTokenizer(line, ",");
				if (st.hasMoreTokens()) {
					String name = st.nextToken();
				}
				if (st.hasMoreTokens()) {
					leftPos.X = Float.valueOf(st.nextToken());
				}
				if (st.hasMoreTokens()) {
					leftPos.Y = Float.valueOf(st.nextToken());
				}
				if (st.hasMoreTokens()) {
					leftPos.Z = Float.valueOf(st.nextToken());
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
					leftVolume = Float.valueOf(st.nextToken());
				}

				line = br.readLine();
				if ( line != null )
				{
					Vector3f rightPos = new Vector3f();
					st = new StringTokenizer(line, ",");
					if (st.hasMoreTokens()) {
						String name = st.nextToken();
					}
					if (st.hasMoreTokens()) {
						rightPos.X = Float.valueOf(st.nextToken());
					}
					if (st.hasMoreTokens()) {
						rightPos.Y = Float.valueOf(st.nextToken());
					}
					if (st.hasMoreTokens()) {
						rightPos.Z = Float.valueOf(st.nextToken());
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
						rightVolume = Float.valueOf(st.nextToken());
					}

					if ( (leftVolume != 0) && (rightVolume != 0) )
					{
						left.add(leftPos);
						right.add(rightPos);
					}
				}
				line = br.readLine();
			}

			fr.close();
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		if ( (left.size() > 0) && (right.size() > 0) )
		{
			VOI pairs = new VOI((short)ID, "pairs" + ID);
			pairs.getCurves().add(left);
			pairs.getCurves().add(right);
			return pairs;
		}
		return null;
	}

	private void calcStatistics( String dirName, VOIVector annotationList, String postScript, boolean generateAutomaticLatticeData )
	{
		String fileName = dirName + File.separator + "annotation_data" + postScript;
		File statsFile = new File(fileName);
		if ( !statsFile.exists() )
		{
			statsFile.mkdir();
			//			System.err.println( "mkdir " + fileName );
		}

		Vector<String> annotationNames = new Vector<String>();
		for ( int i = 0; i < annotationList.size(); i++ )
		{
			VOI annotation = annotationList.elementAt(i);
			for ( int j = 0; j < annotation.getCurves().size(); j++ )
			{
				VOIText text = (VOIText)annotation.getCurves().elementAt(j);
				String name = text.getText();
				if ( !annotationNames.contains(name) )
				{
					annotationNames.add(name);
				}
			}
		}
		for ( int i = 0; i < annotationNames.size(); i++ )
		{
			String annotationFile = fileName + File.separator + annotationNames.elementAt(i) + ".csv";
			//			System.err.println( annotationFile );
			File file = new File(annotationFile);
			if ( file.exists() )
			{
				file.delete();
				file = new File(annotationFile);
			}
			FileWriter fw;
			try {
				fw = new FileWriter(file);
				BufferedWriter bw = new BufferedWriter(fw);

				bw.write( "time,x-um,y-um,z-um,Distance\n");

				for ( int j = 0; j < annotationList.size(); j++ )
				{
					VOI annotation = annotationList.elementAt(j);
					Vector3f origin = new Vector3f();
					for ( int k = 0; k < annotation.getCurves().size(); k++ )
					{
						VOIText text = (VOIText)annotation.getCurves().elementAt(k);
						String name = text.getText();
						if ( annotationNames.elementAt(i).equals("origin") )
						{
							origin = text.elementAt(0);
							break;
						}
					}

					boolean found = false;
					for ( int k = 0; k < annotation.getCurves().size(); k++ )
					{
						VOIText text = (VOIText)annotation.getCurves().elementAt(k);
						String name = text.getText();
						if ( annotationNames.elementAt(i).equals(name) )
						{
							Vector3f pos = text.elementAt(0);
							bw.write( j + "," + pos.X + "," + pos.Y + "," + pos.Z + "," + origin.distance(pos) + "\n" );
							found = true;
							break;
						}
					}
					if ( !found )
					{
						bw.write( j + "\n" );						
					}
				}

				bw.close();

			} catch (IOException e) {
			}

		}
		if ( !generateAutomaticLatticeData )
		{
			return;
		}
		fileName = dirName + File.separator + "automatic_lattice_data";
		statsFile = new File(fileName);
		if ( !statsFile.exists() )
		{
			statsFile.mkdir();
			//			System.err.println( "mkdir " + fileName );
		}

		if ( postScript.contains("after") )
		{
			for ( int i = 0; i < annotationNames.size(); i++ )
			{
				String pair = annotationNames.elementAt(i);
				if ( pair.equals("origin" ) )
				{
					continue;
				}
				String annotationFile = fileName + File.separator + annotationNames.elementAt(i) + "_pair_distances.csv";
				//			System.err.println( annotationFile );
				File file = new File(annotationFile);
				if ( file.exists() )
				{
					file.delete();
					file = new File(annotationFile);
				}
				try {
					FileWriter fw = new FileWriter(file);
					BufferedWriter bw = new BufferedWriter(fw);

					bw.write( "time,Distance\n");

					for ( int j = 0; j < annotationList.size(); j++ )
					{
						VOI annotation = annotationList.elementAt(j);					
						boolean found = false;
						for ( int k = 0; k < annotation.getCurves().size(); k++ )
						{
							VOIText text = (VOIText)annotation.getCurves().elementAt(k);
							String name = text.getText();
							if ( pair.equals(name) )
							{
								Vector3f opposite = new Vector3f(text.elementAt(0));
								String oppositePair = new String(pair);
								if ( oppositePair.contains("L" ) )
								{
									oppositePair = oppositePair.replace("L", "R" );
								}
								else if ( oppositePair.contains("R") )
								{
									oppositePair = oppositePair.replace("R", "L" );
								}
								for ( int p = 0; p < annotation.getCurves().size(); p++ )
								{
									if ( p != k )
									{
										VOIText textP = (VOIText)annotation.getCurves().elementAt(p);
										String nameP = textP.getText();
										if ( nameP.equals(oppositePair) )
										{
											opposite.copy( textP.elementAt(0) );
											break;
										}
									}
								}

								Vector3f pos = text.elementAt(0);
								bw.write( j + "," + opposite.distance(pos) + "\n" );
								found = true;
								break;
							}
						}
						if ( !found )
						{
							bw.write( j + "," + 0.0 + "\n" );
						}
					}

					bw.close();

				} catch (IOException e) {
				}




				annotationFile = fileName + File.separator + annotationNames.elementAt(i) + "_sequence_distances.csv";
				file = new File(annotationFile);
				if ( file.exists() )
				{
					file.delete();
					file = new File(annotationFile);
				}
				try {
					FileWriter fw = new FileWriter(file);
					BufferedWriter bw = new BufferedWriter(fw);

					bw.write( "time,Distance\n");

					for ( int j = 0; j < annotationList.size(); j++ )
					{
						VOI annotation = annotationList.elementAt(j);					
						boolean found = false;
						for ( int k = 0; k < annotation.getCurves().size(); k++ )
						{
							VOIText text = (VOIText)annotation.getCurves().elementAt(k);
							String name = text.getText();
							if ( pair.equals(name) )
							{
								Vector3f next = new Vector3f(text.elementAt(0));
								String nextPair = new String(pair);
								if ( nextPair.contains("L" ) )
								{
									nextPair = nextPair.replace("L", "" );
									int value = Integer.valueOf(nextPair);
									value++;
									nextPair = new String( value + "L" );
								}
								else if ( nextPair.contains("R") )
								{
									nextPair = nextPair.replace("R", "" );
									int value = Integer.valueOf(nextPair);
									value++;
									nextPair = new String( value + "R" );
								}
								for ( int p = 0; p < annotation.getCurves().size(); p++ )
								{
									if ( p != k )
									{
										VOIText textP = (VOIText)annotation.getCurves().elementAt(p);
										String nameP = textP.getText();
										if ( nameP.equals(nextPair) )
										{
											next.copy( textP.elementAt(0) );
											break;
										}
									}
								}

								Vector3f pos = text.elementAt(0);
								bw.write( j + "," + next.distance(pos) + "\n" );
								found = true;
								break;
							}
						}
						if ( !found )
						{
							bw.write( j + "," + 0.0 + "\n" );
						}
					}

					bw.close();

				} catch (IOException e) {
				}

			}
		}


		if ( postScript.contains("before") )
		{
			VOIVector latticeList = convertToLattice( annotationList );
			for ( int i = 0; i < annotationNames.size(); i++ )
			{
				String pair = annotationNames.elementAt(i);
				if ( pair.equals("origin" ) )
				{
					continue;
				}			
				if ( pair.contains("L" ) )
				{
					pair = pair.replace( "L", "" );
				}
				if ( pair.contains("R" ) )
				{
					pair = pair.replace( "R", "" );
				}
				int value = Integer.valueOf(pair);
				if ( value >= 10 )
				{
					continue;
				}
				int index = value - 1;

				String annotationFile;
				File file;

				annotationFile = fileName + File.separator + value + "_sequence_distances_twisted.csv";
				file = new File(annotationFile);
				if ( file.exists() )
				{
					file.delete();
					file = new File(annotationFile);
				}
				try {
					FileWriter fw = new FileWriter(file);
					BufferedWriter bw = new BufferedWriter(fw);

					bw.write( "time,distL,distR,Difference\n");

					for ( int j = 0; j < latticeList.size(); j++ )
					{
						VOI lattice = latticeList.elementAt(j);		

						VOIContour left = (VOIContour) lattice.getCurves().elementAt(0);
						VOIContour right = (VOIContour) lattice.getCurves().elementAt(1);
						if ( (index+1) >= left.size() )
						{
							bw.write( j + "," + 0.0 + "\n" );	
						}
						else
						{
							Vector3f leftPt = left.elementAt(index);
							Vector3f rightPt = right.elementAt(index);

							Vector3f leftP1Pt = left.elementAt(index+1);
							Vector3f rightP1Pt = right.elementAt(index+1);

							float distanceL = leftPt.distance(leftP1Pt);
							float distanceR = rightPt.distance(rightP1Pt);

							float diff = Math.abs( distanceL - distanceR );
							bw.write( j + "," + distanceL + "," + distanceR + "," + diff + "\n" );
						}
					}

					bw.close();

				} catch (IOException e) {
				}



				annotationFile = fileName + File.separator + value + "_midPoint_distances_twisted.csv";
				file = new File(annotationFile);
				if ( file.exists() )
				{
					file.delete();
					file = new File(annotationFile);
				}
				try {
					FileWriter fw = new FileWriter(file);
					BufferedWriter bw = new BufferedWriter(fw);

					bw.write( "time,distance\n");

					for ( int j = 0; j < latticeList.size(); j++ )
					{
						VOI lattice = latticeList.elementAt(j);		

						VOIContour left = (VOIContour) lattice.getCurves().elementAt(0);
						VOIContour right = (VOIContour) lattice.getCurves().elementAt(1);
						if ( (index+1) >= left.size() )
						{
							bw.write( j + "," + 0.0 + "\n" );	
						}
						else
						{
							Vector3f leftPt = left.elementAt(index);
							Vector3f rightPt = right.elementAt(index);
							Vector3f midPt = Vector3f.add(leftPt, rightPt);
							midPt.scale(0.5f);

							Vector3f leftP1Pt = left.elementAt(index+1);
							Vector3f rightP1Pt = right.elementAt(index+1);
							Vector3f midPt1 = Vector3f.add(leftP1Pt, rightP1Pt);
							midPt1.scale(0.5f);

							float distance = midPt.distance(midPt1);
							bw.write( j + "," + distance + "\n" );
						}
					}

					bw.close();

				} catch (IOException e) {
				}



				annotationFile = fileName + File.separator + value + "_sequence_twist.csv";
				file = new File(annotationFile);
				if ( file.exists() )
				{
					file.delete();
					file = new File(annotationFile);
				}
				try {
					FileWriter fw = new FileWriter(file);
					BufferedWriter bw = new BufferedWriter(fw);

					bw.write( "time,Angle\n");

					for ( int j = 0; j < latticeList.size(); j++ )
					{
						VOI lattice = latticeList.elementAt(j);		

						VOIContour left = (VOIContour) lattice.getCurves().elementAt(0);
						VOIContour right = (VOIContour) lattice.getCurves().elementAt(1);
						if ( (index+1) >= left.size() )
						{
							bw.write( j + "," + 0.0 + "\n" );	
						}
						else
						{
							Vector3f leftPt = left.elementAt(index);
							Vector3f rightPt = right.elementAt(index);
							Vector3f v1 = Vector3f.sub( rightPt, leftPt );
							v1.normalize();

							Vector3f leftP1Pt = left.elementAt(index+1);
							Vector3f rightP1Pt = right.elementAt(index+1);
							Vector3f v2 = Vector3f.sub( rightP1Pt, leftP1Pt );
							v2.normalize();
							float angle = (float) (180 * v1.angle(v2) / Math.PI);

							bw.write( j + "," + angle + "\n" );
						}
					}

					bw.close();

				} catch (IOException e) {
				}


				if ( value >= 9 )
				{
					continue;
				}
				annotationFile = fileName + File.separator + value + "_sequence_bend.csv";
				file = new File(annotationFile);
				if ( file.exists() )
				{
					file.delete();
					file = new File(annotationFile);
				}
				try {
					FileWriter fw = new FileWriter(file);
					BufferedWriter bw = new BufferedWriter(fw);

					bw.write( "time,AngleL,AngleR,Difference\n");

					for ( int j = 0; j < latticeList.size(); j++ )
					{
						VOI lattice = latticeList.elementAt(j);		

						VOIContour left = (VOIContour) lattice.getCurves().elementAt(0);
						VOIContour right = (VOIContour) lattice.getCurves().elementAt(1);
						if ( (index+2) >= left.size() )
						{
							bw.write( j + "," + 0.0 + "\n" );	
						}
						else
						{
							Vector3f leftPt = left.elementAt(index);
							Vector3f rightPt = right.elementAt(index);

							Vector3f leftP1Pt = left.elementAt(index+1);
							Vector3f rightP1Pt = right.elementAt(index+1);

							Vector3f leftP2Pt = left.elementAt(index+2);
							Vector3f rightP2Pt = right.elementAt(index+2);

							Vector3f v1L = Vector3f.sub( leftP1Pt, leftPt );
							Vector3f v2L = Vector3f.sub( leftP2Pt, leftP1Pt );

							Vector3f v1R = Vector3f.sub( rightP1Pt, rightPt );
							Vector3f v2R = Vector3f.sub( rightP2Pt, rightP1Pt );

							v1L.normalize();
							v2L.normalize();
							v1R.normalize();
							v2R.normalize();

							float angleL = (float) (180 * v1L.angle(v2L) / Math.PI);
							float angleR = (float) (180 * v1R.angle(v2R) / Math.PI);

							bw.write( j + "," + angleL + "," + angleR + "," + Math.abs(angleL - angleR) + "\n" );
						}
					}

					bw.close();

				} catch (IOException e) {
				}
			}
		}
	}



	private void calcStatistics3( String dirName, VOIVector annotationList, String postScript )
	{
		String fileName = dirName + File.separator + "annotation_data" + postScript;
		File statsFile = new File(fileName);
		if ( !statsFile.exists() )
		{
			statsFile.mkdir();
			//			System.err.println( "mkdir " + fileName );
		}

		Vector<String> annotationNames = new Vector<String>();
		for ( int i = 0; i < annotationList.size(); i++ )
		{
			VOI annotation = annotationList.elementAt(i);
			for ( int j = 0; j < annotation.getCurves().size(); j++ )
			{
				VOIText text = (VOIText)annotation.getCurves().elementAt(j);
				String name = text.getText();
				if ( !annotationNames.contains(name) )
				{
					annotationNames.add(name);
				}
			}
		}
		
		fileName = dirName + File.separator + "automatic_lattice_data";
		statsFile = new File(fileName);
		if ( !statsFile.exists() )
		{
			statsFile.mkdir();
			//			System.err.println( "mkdir " + fileName );
		}

		if ( postScript.contains("before") )
		{
			String annotationFile = fileName + File.separator + "Nose_MidPoint_Distances_twisted.csv";
			File file = new File(annotationFile);
			if ( file.exists() )
			{
				file.delete();
				file = new File(annotationFile);
			}
			try {
				FileWriter fw = new FileWriter(file);
				BufferedWriter bw = new BufferedWriter(fw);

				bw.write( "time,P1 length,P2 length,P3 length,P4 length, P5 length,P6 length,P7 length,P8 length,P9 length,P10 length\n");
				
				Vector3f nose = null;
				Vector3f midPt = null;
				Vector3f left = null;
				Vector3f right = null;
				float[] distances = new float[10];
				for ( int i = 0; i < annotationList.size(); i++ )
				{
					VOI annotation = annotationList.elementAt(i);	
					for ( int p = 1; p <= 10; p++ )
					{
						nose = null;
						midPt = null;
						left = null;
						right = null;
						distances[p-1] = 0;
						for ( int j = 0; j < annotation.getCurves().size(); j++ )
						{
							VOIText text = (VOIText)annotation.getCurves().elementAt(j);
							String name = text.getText();
							if ( name.equalsIgnoreCase("origin") )
							{
								nose = new Vector3f(text.elementAt(0) );
							}
							if ( name.equalsIgnoreCase( p+"L") )
							{
								left = new Vector3f(text.elementAt(0) );
							}
							if ( name.equalsIgnoreCase( p+"R") )
							{
								right = new Vector3f(text.elementAt(0) );
							}
						}
//						if ( (p == 1) && ((nose == null) || (left == null) || (right == null)) )
//						{
//							break;
//						}
//						if ( p == 1 )
//						{
//							bw.write( i );
//							System.err.print( i );
//						}
						if ( (nose != null) && (left != null) && (right != null) )
						{
							midPt = Vector3f.add(left, right);
							midPt.scale(0.5f);
							distances[p-1] = nose.distance(midPt);
						}
						else
						{
							distances[p-1] = 0;
						}
						if ( p == 10 )
						{
							bw.write( i + "," + distances[0] + "," + distances[1] + "," + distances[2] + "," + distances[3] + "," + distances[4] + "," + distances[5] + "," + distances[6] + "," + distances[7] + "," + distances[8] + "," + distances[9] + "\n" );
							System.err.print( i + "," + distances[0] + "," + distances[1] + "," + distances[2] + "," + distances[3] + "," + distances[4] + "," + distances[5] + "," + distances[6] + "," + distances[7] + "," + distances[8] + "," + distances[9] + "\n" );
						}
//						bw.write( i + "," + nose.distance(midPt) + "\n" );
//						bw.write( i + "\n" );
					}
				}
				bw.close();
			} catch (IOException e) {
			}			
		}
		System.err.println( "DONE CalcStatistics3" );
	}



	private void calcStatistics2( String dirName, VOIVector annotationList, String postScript, boolean generateAutomaticLatticeData )
	{
		String fileName = dirName + File.separator + "annotation_data" + postScript;
		File statsFile = new File(fileName);
		if ( !statsFile.exists() )
		{
			statsFile.mkdir();
			//			System.err.println( "mkdir " + fileName );
		}

		Vector<String> annotationNames = new Vector<String>();
		for ( int i = 0; i < annotationList.size(); i++ )
		{
			VOI annotation = annotationList.elementAt(i);
			for ( int j = 0; j < annotation.getCurves().size(); j++ )
			{
				VOIText text = (VOIText)annotation.getCurves().elementAt(j);
				String name = text.getText();
				if ( !annotationNames.contains(name) )
				{
					annotationNames.add(name);
				}
			}
		}
		
		fileName = dirName + File.separator + "automatic_lattice_data";
		statsFile = new File(fileName);
		if ( !statsFile.exists() )
		{
			statsFile.mkdir();
			//			System.err.println( "mkdir " + fileName );
		}

		if ( postScript.contains("before") )
		{
			VOIVector latticeList = convertToLattice( annotationList );
			



//			String annotationFile = fileName + File.separator + "LatticeMinMaxPairLength_twisted.csv";
//			File file = new File(annotationFile);
//			if ( file.exists() )
//			{
//				file.delete();
//				file = new File(annotationFile);
//			}
//			try {
//				FileWriter fw = new FileWriter(file);
//				BufferedWriter bw = new BufferedWriter(fw);
//
//				bw.write( "time,min length,max length, min index, max index\n");
//				for ( int i = 0; i < latticeList.size(); i++ )
//				{
//					VOI lattice = latticeList.elementAt(i);
//					VOIContour left = (VOIContour) lattice.getCurves().elementAt(0);
//					VOIContour right = (VOIContour) lattice.getCurves().elementAt(1);
//					float minLength = Float.MAX_VALUE;
//					float maxLength = -Float.MAX_VALUE;
//					int minIndex = -1;
//					int maxIndex = -1;
//					for ( int j = 0; j < Math.min(left.size()-1, right.size()-1); j++ )
//					{
//						Vector3f pL = left.elementAt(j);
//						Vector3f pR = right.elementAt(j);
//						float dist = pL.distance(pR);
//						if ( dist < minLength )
//						{
//							minLength = dist;
//							minIndex = j;
//						}
//						if ( dist > maxLength )
//						{
//							maxLength = dist;
//							maxIndex = j;
//						}
//					}
//					
//					bw.write( i + "," + minLength + "," + maxLength + "," + minIndex + "," + maxIndex + "\n" );
//				}
//				bw.close();
//			} catch (IOException e) {
//			}
			


//			String annotationFile = fileName + File.separator + "LatticeMinMaxSequenceLength_twisted.csv";
//			File file = new File(annotationFile);
//			if ( file.exists() )
//			{
//				file.delete();
//				file = new File(annotationFile);
//			}
//			try {
//				FileWriter fw = new FileWriter(file);
//				BufferedWriter bw = new BufferedWriter(fw);
//
//				bw.write( "time,min length,max length, min index, max index\n");
//				for ( int i = 0; i < latticeList.size(); i++ )
//				{
//					VOI lattice = latticeList.elementAt(i);
//					VOIContour left = (VOIContour) lattice.getCurves().elementAt(0);
//					VOIContour right = (VOIContour) lattice.getCurves().elementAt(1);
//					float minLength = Float.MAX_VALUE;
//					float maxLength = -Float.MAX_VALUE;
//					int minIndex = -1;
//					int maxIndex = -1;
//					for ( int j = 0; j < Math.min(left.size()-1, right.size()-1); j++ )
//					{
//						Vector3f pL = left.elementAt(j);
//						Vector3f pL1 = left.elementAt(j+1);
//						float dist = pL.distance(pL1);
//						if ( dist < minLength )
//						{
//							minLength = dist;
//							minIndex = j;
//						}
//						if ( dist > maxLength )
//						{
//							maxLength = dist;
//							maxIndex = j;
//						}
//						Vector3f pR = right.elementAt(j);
//						Vector3f pR1 = right.elementAt(j+1);
//						dist = pR.distance(pR1);
//						if ( dist < minLength )
//						{
//							minLength = dist;
//							minIndex = j;
//						}
//						if ( dist > maxLength )
//						{
//							maxLength = dist;
//							maxIndex = j;
//						}
//					}
//					
//					bw.write( i + "," + minLength + "," + maxLength + "," + minIndex + "," + maxIndex + "\n" );
//				}
//				bw.close();
//			} catch (IOException e) {
//			}
			

			String annotationFile = fileName + File.separator + "LatticeBends_twisted.csv";
			File file = new File(annotationFile);
			if ( file.exists() )
			{
				file.delete();
				file = new File(annotationFile);
			}
			try {
				FileWriter fw = new FileWriter(file);
				BufferedWriter bw = new BufferedWriter(fw);

				bw.write( "time,bend,min parallel, max parallel\n");
				for ( int i = 0; i < latticeList.size(); i++ )
				{
					VOI lattice = latticeList.elementAt(i);
					VOIContour left = (VOIContour) lattice.getCurves().elementAt(0);
					VOIContour right = (VOIContour) lattice.getCurves().elementAt(1);
					float bendAngles = 0;
					float minP = Float.MAX_VALUE;
					float maxP = -Float.MAX_VALUE;
					for ( int j = 0; j < Math.min(left.size() - 2, right.size()-2); j++ )
					{
						Vector3f pL = left.elementAt(j);
						Vector3f pR = right.elementAt(j);
						Vector3f mid1 = Vector3f.add(pL, pR);
						mid1.scale(0.5f);
						
						pL = left.elementAt(j+1);
						pR = right.elementAt(j+1);
						Vector3f mid2 = Vector3f.add(pL, pR);
						mid2.scale(0.5f);
						
						pL = left.elementAt(j+2);
						pR = right.elementAt(j+2);
						Vector3f mid3 = Vector3f.add(pL, pR);
						mid3.scale(0.5f);

						Vector3f vec1 = Vector3f.sub( mid2, mid1 );
						Vector3f vec2 = Vector3f.sub( mid3, mid2 );
						vec1.normalize();
						vec2.normalize();
						float angle1 = Math.abs(vec1.angle(vec2));
						float angle2 = Math.abs(vec2.angle(vec1));
						if ( angle1 < angle2 )
						{
							bendAngles += angle1;
						}
						else
						{
							bendAngles += angle2;
						}

						vec1 = Vector3f.sub( left.elementAt(j+1), left.elementAt(j) );
						vec2 = Vector3f.sub( right.elementAt(j+1), right.elementAt(j) );
						vec1.normalize();
						vec2.normalize();
						angle1 = Math.abs(vec1.angle(vec2));
						if ( angle1 < minP )
						{
							minP = angle1;
						}
						if ( angle1 > maxP )
						{
							maxP = angle1;
						}
					}
					
					bw.write( i + "," + bendAngles + "," + minP + "," + maxP + "\n" );
				}
				bw.close();
			} catch (IOException e) {
			}
			


//			String annotationFile = fileName + File.separator + "LatticeLength_twisted.csv";
//			File file = new File(annotationFile);
//			if ( file.exists() )
//			{
//				file.delete();
//				file = new File(annotationFile);
//			}
//			try {
//				FileWriter fw = new FileWriter(file);
//				BufferedWriter bw = new BufferedWriter(fw);
//
//				bw.write( "time,length\n");
//				for ( int i = 0; i < latticeList.size(); i++ )
//				{
//					VOI lattice = latticeList.elementAt(i);
//					VOIContour left = (VOIContour) lattice.getCurves().elementAt(0);
//					VOIContour right = (VOIContour) lattice.getCurves().elementAt(1);
//					float length = 0;
//					for ( int j = 0; j < Math.min(left.size() - 1, right.size()-1); j++ )
//					{
//						Vector3f pL = left.elementAt(j);
//						Vector3f pR = right.elementAt(j);
//						Vector3f mid1 = Vector3f.add(pL, pR);
//						mid1.scale(0.5f);
//						
//						pL = left.elementAt(j+1);
//						pR = right.elementAt(j+1);
//						Vector3f mid2 = Vector3f.add(pL, pR);
//						mid2.scale(0.5f);
//						
//						length += mid1.distance(mid2);
//					}
//					
//					bw.write( i + "," + length + "\n" );
//				}
//				bw.close();
//			} catch (IOException e) {
//			}
			
			
//			for ( int i = 0; i < annotationNames.size(); i++ )
//			{
//				String pair = annotationNames.elementAt(i);
//				if ( pair.equals("origin" ) )
//				{
//					continue;
//				}			
//				if ( pair.contains("L" ) )
//				{
//					pair = pair.replace( "L", "" );
//				}
//				if ( pair.contains("R" ) )
//				{
//					pair = pair.replace( "R", "" );
//				}
//				int value = Integer.valueOf(pair);
//				if ( value >= 10 )
//				{
//					continue;
//				}
//				int index = value - 1;
//
//				String annotationFile;
//				File file;
//
//				//				annotationFile = fileName + File.separator + value + "_sequence_distances_twisted.csv";
//				//				file = new File(annotationFile);
//				//				if ( file.exists() )
//				//				{
//				//					file.delete();
//				//					file = new File(annotationFile);
//				//				}
//				//				try {
//				//					FileWriter fw = new FileWriter(file);
//				//					BufferedWriter bw = new BufferedWriter(fw);
//				//
//				//					bw.write( "time,distL,distR,Difference\n");
//				//
//				//					for ( int j = 0; j < latticeList.size(); j++ )
//				//					{
//				//						VOI lattice = latticeList.elementAt(j);		
//				//						
//				//						VOIContour left = (VOIContour) lattice.getCurves().elementAt(0);
//				//						VOIContour right = (VOIContour) lattice.getCurves().elementAt(1);
//				//						if ( (index+1) >= left.size() )
//				//						{
//				//							bw.write( j + "," + 0.0 + "\n" );	
//				//						}
//				//						else
//				//						{
//				//							Vector3f leftPt = left.elementAt(index);
//				//							Vector3f rightPt = right.elementAt(index);
//				//
//				//							Vector3f leftP1Pt = left.elementAt(index+1);
//				//							Vector3f rightP1Pt = right.elementAt(index+1);
//				//
//				//							float distanceL = leftPt.distance(leftP1Pt);
//				//							float distanceR = rightPt.distance(rightP1Pt);
//				//
//				//							float diff = Math.abs( distanceL - distanceR );
//				//							bw.write( j + "," + distanceL + "," + distanceR + "," + diff + "\n" );
//				//						}
//				//					}
//				//
//				//					bw.close();
//				//
//				//				} catch (IOException e) {
//				//				}
//
//
//
//				annotationFile = fileName + File.separator + value + "_midPoint_distances_twisted.csv";
//				file = new File(annotationFile);
//				if ( file.exists() )
//				{
//					file.delete();
//					file = new File(annotationFile);
//				}
//				try {
//					FileWriter fw = new FileWriter(file);
//					BufferedWriter bw = new BufferedWriter(fw);
//
//					bw.write( "time,distance\n");
//
//					for ( int j = 0; j < latticeList.size(); j++ )
//					{
//						VOI lattice = latticeList.elementAt(j);		
//
//						VOIContour left = (VOIContour) lattice.getCurves().elementAt(0);
//						VOIContour right = (VOIContour) lattice.getCurves().elementAt(1);
//						if ( ((index+1) >= left.size()) || ((index+1) >= right.size()) )
//						{
//							bw.write( j + "," + 0.0 + "\n" );	
//						}
//						else
//						{
//							Vector3f leftPt = left.elementAt(index);
//							Vector3f rightPt = right.elementAt(index);
//							Vector3f midPt = Vector3f.add(leftPt, rightPt);
//							midPt.scale(0.5f);
//
//							Vector3f leftP1Pt = left.elementAt(index+1);
//							Vector3f rightP1Pt = right.elementAt(index+1);
//							Vector3f midPt1 = Vector3f.add(leftP1Pt, rightP1Pt);
//							midPt1.scale(0.5f);
//
//							float distance = midPt.distance(midPt1);
//							bw.write( j + "," + distance + "\n" );
//						}
//					}
//
//					bw.close();
//
//				} catch (IOException e) {
//				}
//
//
//				if ( value >= 9 )
//				{
//					continue;
//				}
//				annotationFile = fileName + File.separator + value + "_midPoint_sequence_bend.csv";
//				file = new File(annotationFile);
//				if ( file.exists() )
//				{
//					file.delete();
//					file = new File(annotationFile);
//				}
//				try {
//					FileWriter fw = new FileWriter(file);
//					BufferedWriter bw = new BufferedWriter(fw);
//
//					bw.write( "time,Angle\n");
//
//					for ( int j = 0; j < latticeList.size(); j++ )
//					{
//						VOI lattice = latticeList.elementAt(j);		
//
//						VOIContour left = (VOIContour) lattice.getCurves().elementAt(0);
//						VOIContour right = (VOIContour) lattice.getCurves().elementAt(1);
//						if ( ((index+2) >= left.size()) || ((index+2) >= right.size()) )
//						{
//							bw.write( j + "," + 0.0 + "\n" );	
//						}
//						else
//						{
//							Vector3f leftPt = left.elementAt(index);
//							Vector3f rightPt = right.elementAt(index);
//							Vector3f midPt = Vector3f.add(leftPt, rightPt);
//							midPt.scale(0.5f);
//
//							Vector3f leftP1Pt = left.elementAt(index+1);
//							Vector3f rightP1Pt = right.elementAt(index+1);
//							Vector3f midPt1 = Vector3f.add(leftP1Pt, rightP1Pt);
//							midPt1.scale(0.5f);
//
//							Vector3f leftP2Pt = left.elementAt(index+2);
//							Vector3f rightP2Pt = right.elementAt(index+2);
//							Vector3f midPt2 = Vector3f.add(leftP2Pt, rightP2Pt);
//							midPt2.scale(0.5f);
//
//							Vector3f v1 = Vector3f.sub( midPt1, midPt );
//							Vector3f v2 = Vector3f.sub( midPt2, midPt1 );
//
//							v1.normalize();
//							v2.normalize();
//
//							float angle = (float) (180 * v1.angle(v2) / Math.PI);
//
//							bw.write( j + "," + angle + "\n" );
//						}
//					}
//
//					bw.close();
//
//				} catch (IOException e) {
//				}
//			}
		}
	}




	private VOIVector convertToLattice( VOIVector annotationList )
	{
		VOIVector latticeList = new VOIVector();
		for ( int i = 0; i < annotationList.size(); i++ )
		{
			VOI annotation = annotationList.elementAt(i);	

			VOIContour left = new VOIContour( false );
			VOIContour right = new VOIContour( false );
			VOI lattice = new VOI((short) i, "lattice", 1, VOI.POLYLINE );
			lattice.getCurves().add(left);
			lattice.getCurves().add(right);


			for ( int j = 0; j < annotation.getCurves().size(); j++ )
			{
				int value = j+1;
				for ( int k = 0; k < annotation.getCurves().size(); k++ )
				{
					VOIText text = (VOIText)annotation.getCurves().elementAt(k);
					String name = text.getText();
					if ( name.equals(value+"L") )
					{
						//						System.err.println( "adding to left " + name );
						left.add( text.elementAt(0) );
						break;
					}
				}
				for ( int k = 0; k < annotation.getCurves().size(); k++ )
				{
					VOIText text = (VOIText)annotation.getCurves().elementAt(k);
					String name = text.getText();
					if ( name.equals(value+"R") )
					{
						//						System.err.println( "adding to right " + name );
						right.add( text.elementAt(0) );
						break;
					}
				}
			}

			latticeList.add(lattice);
		}

		return latticeList;
	}

	private void saveTrainingData( VOIVector trainingData )
	{
		String fileNameX = baseFileDir + File.separator + "trainingX";		
		File fileX = new File(fileNameX + ".txt");
		if ( fileX.exists() )
		{
			fileX.delete();
			fileX = new File(fileNameX + ".txt");
		}
		String fileNameY = baseFileDir + File.separator + "trainingY";
		File fileY = new File(fileNameY + ".txt");
		if ( fileY.exists() )
		{
			fileY.delete();
			fileY = new File(fileNameY + ".txt");
		}
		FileWriter fwX;
		FileWriter fwY;
		try {
			fwX = new FileWriter(fileX);
			BufferedWriter bwX = new BufferedWriter(fwX);
			//			int count40 = 0;
			int count40 = (int) (trainingData.size() * .20f);
			bwX.write( "# name: trainX"  + "\n");
			bwX.write( "# type: matrix"  + "\n");
			bwX.write( "# rows: " + (int)numSamples*(trainingData.size() - count40) + "\n");
			bwX.write( "# columns: 60"  + "\n");


			fwY = new FileWriter(fileY);
			BufferedWriter bwY = new BufferedWriter(fwY);
			bwY.write( "# name: trainY"  + "\n");
			bwY.write( "# type: matrix"  + "\n");
			bwY.write( "# rows: " + (int)numSamples*(trainingData.size() - count40) + "\n");
			bwY.write( "# columns: 1"  + "\n");

			Random randomGen = new Random();
			while ( trainingData.size() > count40)
			{
				int index = (int) (randomGen.nextFloat() * (trainingData.size()));
				saveTrainingData( trainingData.remove(index), bwX, bwY, 0 );
			}

			bwX.write( "\n" );
			bwY.write( "\n" );
			bwX.close();
			bwY.close();

		} catch (IOException e) {
		}
	}

	private void saveCVData( VOIVector trainingData )
	{
		String fileNameX = baseFileDir + File.separator + "CVX";		
		File fileX = new File(fileNameX + ".txt");
		if ( fileX.exists() )
		{
			fileX.delete();
			fileX = new File(fileNameX + ".txt");
		}
		String fileNameY = baseFileDir + File.separator + "CVY";
		File fileY = new File(fileNameY + ".txt");
		if ( fileY.exists() )
		{
			fileY.delete();
			fileY = new File(fileNameY + ".txt");
		}
		FileWriter fwX;
		FileWriter fwY;
		try {
			fwX = new FileWriter(fileX);
			BufferedWriter bwX = new BufferedWriter(fwX);
			int count50 = 0;//(int) (trainingData.size() * .50f);
			bwX.write( "# name: cvX"  + "\n");
			bwX.write( "# type: matrix"  + "\n");
			bwX.write( "# rows: " + (int)numSamples*(trainingData.size() - count50) + "\n");
			bwX.write( "# columns: 60"  + "\n");


			fwY = new FileWriter(fileY);
			BufferedWriter bwY = new BufferedWriter(fwY);
			bwY.write( "# name: cvY"  + "\n");
			bwY.write( "# type: matrix"  + "\n");
			bwY.write( "# rows: " + (int)numSamples*(trainingData.size() - count50) + "\n");
			bwY.write( "# columns: 1"  + "\n");


			Random randomGen = new Random();
			while ( trainingData.size() > count50)
			{
				int index = (int) (randomGen.nextFloat() * (trainingData.size()));
				saveTrainingData( trainingData.remove(index), bwX, bwY, 0 );
			}

			bwX.write( "\n" );
			bwY.write( "\n" );
			bwX.close();
			bwY.close();

		} catch (IOException e) {
		}
	}

	private void saveTestData( VOIVector trainingData )
	{
		String fileNameX = baseFileDir + File.separator + "TestX";		
		File fileX = new File(fileNameX + ".txt");
		if ( fileX.exists() )
		{
			fileX.delete();
			fileX = new File(fileNameX + ".txt");
		}
		String fileNameY = baseFileDir + File.separator + "TestY";
		File fileY = new File(fileNameY + ".txt");
		if ( fileY.exists() )
		{
			fileY.delete();
			fileY = new File(fileNameY + ".txt");
		}
		FileWriter fwX;
		FileWriter fwY;
		try {
			fwX = new FileWriter(fileX);
			BufferedWriter bwX = new BufferedWriter(fwX);
			bwX.write( "# name: testX"  + "\n");
			bwX.write( "# type: matrix"  + "\n");
			bwX.write( "# rows: " + numSamples*trainingData.size() + "\n");
			bwX.write( "# columns: 60"  + "\n");


			fwY = new FileWriter(fileY);
			BufferedWriter bwY = new BufferedWriter(fwY);
			bwY.write( "# name: testY"  + "\n");
			bwY.write( "# type: matrix"  + "\n");
			bwY.write( "# rows: " + numSamples*trainingData.size() + "\n");
			bwY.write( "# columns: 1"  + "\n");


			Random randomGen = new Random();
			while ( trainingData.size() > 0)
			{
				int index = (int) (randomGen.nextFloat() * (trainingData.size()));
				saveTrainingData( trainingData.remove(index), bwX, bwY, 0 );
			}

			bwX.write( "\n" );
			bwY.write( "\n" );
			bwX.close();
			bwY.close();

		} catch (IOException e) {
		}
	}

	private int numSamples = 100;
	private void saveTrainingData( VOI trainingData, BufferedWriter bwX, BufferedWriter bwY, int pairIndex ) throws IOException
	{
		Random randomGen = new Random();

		for ( int i = 0; i < numSamples; i++ )
		{
			VOIText text = (VOIText) trainingData.getCurves().elementAt(pairIndex);
			if ( text.getText().equalsIgnoreCase("origin") )
			{
				continue;
			}
			Vector3f pos = text.elementAt(0);
			bwX.write( (int)pos.X + " "+ (int)pos.Y + " " + (int)pos.Z + " " );

			int[] indexJs = new int[trainingData.getCurves().size()-1];
			boolean[] writtenJ = new boolean[trainingData.getCurves().size()];
			for ( int j = 0; j < writtenJ.length; j++ )
			{
				writtenJ[j] = false;
			}
			writtenJ[pairIndex] = true;
			boolean doneJ = false;
			int count = 0;
			while ( !doneJ )
			{
				int indexJ = (int) (randomGen.nextFloat() * (trainingData.getCurves().size()));
				if ( (indexJ != pairIndex) && !writtenJ[indexJ] )
				{					
					indexJs[count++] = indexJ;
					writtenJ[indexJ] = true;
				}

				doneJ = true;
				for ( int j = 0; j < writtenJ.length; j++ )
				{
					doneJ &= writtenJ[j];
				}
			}

			int match = -1;
			for ( int j = 0; j < indexJs.length; j++ )
			{
				if ( indexJs[j] != pairIndex )
				{
					VOIText text2 = (VOIText) trainingData.getCurves().elementAt(indexJs[j]);
					Vector3f pos2 = text2.elementAt(0);
					bwX.write( (int)pos2.X + " "+ (int)pos2.Y + " " + (int)pos2.Z + " " );

					if ( text.getText().substring(1).equals( text2.getText().substring(1) ) )
					{
						match = j;
					}

					//					System.err.print( indexJs[j] + " " );
				}
			}
			//			System.err.println("");

			bwX.write( "\n" );
			bwY.write( (match + 2) + "\n" );
			if ( ((match + 1) > 20) || ((match + 1) <= 0) )
			{
				System.err.println( "Error " + (match + 1) );
			}
		}
	}

	private void saveTrainingData2( VOI trainingData, BufferedWriter bwX, BufferedWriter bwY ) throws IOException
	{
		Random randomGen = new Random();
		boolean[] writtenI = new boolean[trainingData.getCurves().size()];
		for ( int j = 0; j < writtenI.length; j++ )
		{
			writtenI[j] = false;
		}
		boolean doneI = false;
		while ( !doneI )
		{
			int indexI = (int) (randomGen.nextFloat() * (trainingData.getCurves().size()));
			if ( writtenI[indexI] )
			{
				continue;
			}
			writtenI[indexI] = true;
			VOIText text = (VOIText) trainingData.getCurves().elementAt(indexI);
			if ( text.getText().equalsIgnoreCase("origin") )
			{
				continue;
			}
			Vector3f pos = text.elementAt(0);
			//			int posIndex = (int) (pos.Z * wormExtents[1] * wormExtents[0] + pos.Y * wormExtents[0] + pos.X);
			//			bwX.write( posIndex + " " );
			//			bwX.write( (int)pos.X + " "+ (int)pos.Y + " " + (int)pos.Z + " " );
			//			bwY.write( 0 + " " );

			boolean[] writtenJ = new boolean[trainingData.getCurves().size()];
			for ( int j = 0; j < writtenJ.length; j++ )
			{
				writtenJ[j] = false;
			}
			writtenJ[indexI] = true;
			int count = 1;
			int match = -1;
			boolean doneJ = false;
			while ( !doneJ )
			{
				int indexJ = (int) (randomGen.nextFloat() * (trainingData.getCurves().size()));
				if ( (indexJ != indexI) && !writtenJ[indexJ] )
				{
					VOIText text2 = (VOIText) trainingData.getCurves().elementAt(indexJ);
					Vector3f pos2 = text2.elementAt(0);
					//					posIndex = (int) (pos.Z * wormExtents[1] * wormExtents[0] + pos.Y * wormExtents[0] + pos.X);
					//					bwX.write( posIndex + " " );
					//					bwX.write( (int)pos.X + " "+ (int)pos.Y + " " + (int)pos.Z + " " );
					bwX.write( pos.distance(pos2) + " " );

					if ( text.getText().substring(1).equals( text2.getText().substring(1) ) )
					{
						//						bwY.write( 1 + " " );
						match = indexJ;
					}
					else
					{
						//						bwY.write( 0 + " " );
					}

					writtenJ[indexJ] = true;
					count++;
				}

				doneJ = true;
				for ( int j = 0; j < writtenJ.length; j++ )
				{
					doneJ &= writtenJ[j];
				}
			}

			bwX.write( "\n" );
			bwY.write( (match + 1) + "\n" );
			//			if ( Integer.valueOf(text.getText().substring(1)) == 1 )
			//			{
			//				bwY.write( text.getText().substring(1) + "\n" );				
			//			}
			//			else
			//			{
			//				bwY.write( "2 \n" );
			//			}
			//			bwY.write( text.getText().substring(1) + "\n" );
			if ( ((match + 1) > 20) || ((match + 1) <= 0) )
			{
				System.err.println( "Error " + (match + 1) );
			}

			doneI = true;
			for ( int j = 0; j < writtenI.length; j++ )
			{
				doneI &= writtenI[j];
			}
		}
	}

}
