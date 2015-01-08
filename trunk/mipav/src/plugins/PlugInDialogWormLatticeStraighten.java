
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
import java.util.Random;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.JTextField;

import WildMagic.LibFoundation.Mathematics.ColorRGBA;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibGraphics.SceneGraph.TriMesh;

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
			if ( generateTrainingData.isSelected() )
			{
				generateTrainingData();
			}
			if ( segmentSeamCells.isSelected() )
			{
				segmentKMeans();
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
					loadAllVOIsFrom(voiDir, true, annotations, false);
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
					loadAllVOIsFrom(voiDir, true, annotations, false);
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


	private void segmentKMeans()
	{
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
					wormImageA.calcMinMax();
					float minValue = (float) (0.1 * wormImageA.getMax());
					float maxValue = (float) (1.0 * wormImageA.getMax());
					ModelImage imageNoSeam = LatticeModel.segmentAll1(wormImageA, minValue, maxValue, 20, 1 );
					minValue = (float) (0.1 * imageNoSeam.getMax());
					maxValue = (float) (1.0 *  imageNoSeam.getMax());
					LatticeModel.segmentAll2(wormImageA, imageNoSeam, minValue, maxValue, 10, 1 );
					imageNoSeam.disposeLocal();
					imageNoSeam = null;
					//					float minValue = (float) (0.1 * wormImageA.getMax());
					//					float maxValue = (float) (1.0 * wormImageA.getMax());
					//					LatticeModel.segmentAll(wormImageA, minValue, maxValue, 20, 1, false );
					//					wormImageA.calcMinMax();
					//					minValue = (float) (0.25 * wormImageA.getMax());
					//					maxValue = (float) (1.0 * wormImageA.getMax());
					//					LatticeModel.segmentAll(wormImageA, minValue, maxValue, 10, 0.75f, true ); 
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
					wormImageA.calcMinMax();
					float minValue = (float) (0.1 * wormImageA.getMax());
					float maxValue = (float) (1.0 * wormImageA.getMax());
					ModelImage imageNoSeam = LatticeModel.segmentAll1(wormImageA, minValue, maxValue, 20, 1 );
					minValue = (float) (0.25 * imageNoSeam.getMax());
					maxValue = (float) (1.0 *  imageNoSeam.getMax());
					LatticeModel.segmentAll2(wormImageA, imageNoSeam, minValue, maxValue, 10, 1 );
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
						loadAllVOIsFrom(voiDir, true, annotations, false);
						if ( annotations.size() == 0 )
						{
							fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + File.separator + "annotation";            	    		
							annotations = new VOIVector();
							voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
							loadAllVOIsFrom(voiDir, true, annotations, false);
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
						loadAllVOIsFrom(voiDir, true, annotations, false);
						if ( annotations.size() == 0 )
						{
							fileName = baseFileNameText.getText() + "_" + fileCount + File.separator + "annotation";            	    		
							annotations = new VOIVector();
							voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
							loadAllVOIsFrom(voiDir, true, annotations, false);
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
						else
						{
							fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + File.separator + "annotation";            	    		
							annotations = new VOIVector();
							voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
							loadAllVOIsFrom(voiDir, true, annotations, false);
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
						else
						{
							fileName = baseFileNameText.getText() + "_" + fileCount + File.separator + "annotation";            	    		
							annotations = new VOIVector();
							voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
							loadAllVOIsFrom(voiDir, true, annotations, false);
							if ( annotations.size() > 0 )
							{
								model.setAnnotations( annotations.elementAt(0) );
							}							
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

		System.err.println( "Done segmentation" );
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
			loadAllVOIsFrom(voiDir, false, lattice, false);
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
					LatticeModel model = new LatticeModel( wormImageA, null, swapLattice );
					model.saveLattice( baseFileDir + File.separator + baseFileNameText.getText() + "_"  + index + File.separator, "lattice_0" );
					model.dispose();
					model = null;
				}	
			}

			fileName = baseFileNameText.getText() + "_"  + index + File.separator + "lattice_1";
			lattice = new VOIVector();
			voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
			loadAllVOIsFrom(voiDir, false, lattice, false);
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
					LatticeModel model = new LatticeModel( wormImageA, null, swapLattice );
					model.saveLattice( baseFileDir + File.separator + baseFileNameText.getText() + "_"  + index + File.separator, "lattice_1" );
					model.dispose();
					model = null;
				}
			}
		}
	}

	private void generateAnnotationAnimation()
	{
		int[] extents = new int[3];
		annotationList = new VOIVector();
		ViewJProgressBar progress = new  ViewJProgressBar( "Generating Animation", "", 0, 100, false);
		if ( includeRange != null )
		{
			progress.setVisible(true);
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String fileName = baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + 
						"output_images" + File.separator + baseFileNameText.getText() + "_" + includeRange.elementAt(i) + "_straight_register" + ".tif";
//				String fileName = baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + 
//						"output_images" + File.separator + baseFileNameText.getText() + "_" + includeRange.elementAt(i) + "_straight" + ".tif";
//				String fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{
					progress.updateValue((int) (100 * (float)i/(float)includeRange.size()));
					System.err.println( fileName );
					FileIO fileIO = new FileIO();
					fileIO.setSuppressProgressBar(true);
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
					extents[0] = Math.max(extents[0], wormImageA.getExtents()[0] );
					extents[1] = Math.max(extents[1], wormImageA.getExtents()[1] );
					extents[2] = Math.max(extents[2], wormImageA.getExtents()[2] );

					fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + File.separator + "straightened_annotations"; 
//					fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + File.separator + "annotations";            	    		
					VOIVector annotations = new VOIVector();
					String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
					loadAllVOIsFrom(voiDir, true, annotations, false);
					if ( annotations.size() == 0 )
					{
						fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + File.separator + "straightened_annotation";   
//						fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + File.separator + "annotation";            	    		
						annotations = new VOIVector();
						voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
						loadAllVOIsFrom(voiDir, true, annotations, false);
					}
					if ( annotations.size() > 0 )
					{
						annotationList.add( annotations.elementAt(0) );
					}
				}
			}
			progress.setVisible(false);
		}
		else
		{
			int fileCount = 0;
			boolean fileExists = true;
			while ( fileExists )
			{    	    	
				String fileName = baseFileNameText.getText() + "_"  + fileCount + File.separator + 
						"output_images" + File.separator + baseFileNameText.getText() + "_" + fileCount + "_straight_register" + ".tif";
//				String fileName = baseFileNameText.getText() + "_" + fileCount + ".tif";
				File voiFile = new File(baseFileDir + File.separator + fileName);
				if ( voiFile.exists() )
				{
					System.err.println( fileName );
					FileIO fileIO = new FileIO();
					fileIO.setSuppressProgressBar(true);
					if(wormImageA != null) {
						wormImageA.disposeLocal();
						wormImageA = null;
					}
					wormImageA = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);  
					extents[0] = Math.max(extents[0], wormImageA.getExtents()[0] );
					extents[1] = Math.max(extents[1], wormImageA.getExtents()[1] );
					extents[2] = Math.max(extents[2], wormImageA.getExtents()[2] );

					fileName = baseFileNameText.getText() + "_" + fileCount + File.separator + "straightened_annotations";          
//					fileName = baseFileNameText.getText() + "_" + fileCount + File.separator + "annotations";            	    		
					VOIVector annotations = new VOIVector();
					String voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
					loadAllVOIsFrom(voiDir, true, annotations, false);
					if ( annotations.size() == 0 )
					{
						fileName = baseFileNameText.getText() + "_" + fileCount + File.separator + "straightened_annotation";    
//						fileName = baseFileNameText.getText() + "_" + fileCount + File.separator + "annotation";            	    		
						annotations = new VOIVector();
						voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
						loadAllVOIsFrom(voiDir, true, annotations, false);
					}
					if ( annotations.size() > 0 )
					{
						annotationList.add( annotations.elementAt(0) );
					}
					fileCount++;
				}    				
				else
				{
					fileExists = false;
				}
			}
		}
		progress.dispose();
		progress = null;
		
		
		System.err.println( "generateAnnotationAnimation " + annotationList.size() );		
		ModelImage animationImage = new ModelImage( ModelStorageBase.BOOLEAN, extents, "animationImage" );
		JDialogBase.updateFileInfo( wormImageA, animationImage );
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
		
		triVolume = new VolumeTriPlanarInterface(animationImage, null, this);
//		triVolume = new VolumeTriPlanarInterface(wormImageA, null, this);

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
    						text.add( new Vector3f( x+1, y, z ) );
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
		
		extents[0] = (int)(max.X - min.X) + 1;
		extents[1] = (int)(max.Y - min.Y) + 1;
		extents[2] = (int)(max.Z - min.Z) + 1;
		
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
		triVolume = new VolumeTriPlanarInterface(animationImage, null, this);
	}


	private void clean()
	{
		if ( includeRange != null )
		{
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + "output_images" + File.separator;
				deleteDirectory(dirName);
				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + "segmentation" + File.separator;
				deleteDirectory(dirName);
				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + "straightened_lattice" + File.separator;
				deleteDirectory(dirName);
				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + "straightened_annotations" + File.separator;
				deleteDirectory(dirName);
				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + "statistics" + File.separator;
				deleteDirectory(dirName);
			}
		}
		else
		{
			int fileCount = 0;
			boolean fileExists = true;
			while ( fileExists )
			{    	    	
				File voiFile = new File(baseFileDir + File.separator + baseFileNameText.getText() + "_"  + fileCount + File.separator);
				if ( voiFile.exists() )
				{
					String dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + fileCount + File.separator + "output_images" + File.separator;
					deleteDirectory(dirName);
					dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + fileCount + File.separator + "segmentation" + File.separator;
					deleteDirectory(dirName);
					dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + fileCount + File.separator + "straightened_lattice" + File.separator;
					deleteDirectory(dirName);
					dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + fileCount + File.separator + "straightened_annotations" + File.separator;
					deleteDirectory(dirName);
					dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + fileCount + File.separator + "statistics" + File.separator;
					deleteDirectory(dirName);
					fileCount++;
				}    				
				else
				{
					fileExists = false;
				}
			}
		}
	}     

	private void deleteDirectory( String dirName )
	{
		final File fileDir = new File(dirName);

		if (fileDir.exists() && fileDir.isDirectory()) { // do nothing
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
			System.err.println( "Saving mp image to : " + fileName + " " + maximumProjectionImage.getImageName() + ".tif" );
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
		latticeStraighten = gui.buildCheckBox("Straighten Lattices", false );
		panel.add(latticeStraighten.getParent(), gbc);
		gbc.gridy++;


		gbc.gridx = 0;
		calcStatistics = gui.buildCheckBox("calculate staticstics", false );
		//		panel.add(calcStatistics.getParent(), gbc);
		//		gbc.gridy++;

		gbc.gridx = 0;
		generateTrainingData = gui.buildCheckBox("generate training data", false );
		//		panel.add(generateTrainingData.getParent(), gbc);
		//		gbc.gridy++;

		gbc.gridx = 0;
		segmentSeamCells = gui.buildCheckBox("segment seam cells", false );
//		panel.add(segmentSeamCells.getParent(), gbc);
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
		//		panel.add(flipLattices.getParent(), gbc);
		//		gbc.gridy++;

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
