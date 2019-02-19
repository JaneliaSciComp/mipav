
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
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.filters.OpenCL.filters.OpenCLAlgorithmGaussianBlur;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIText;
import gov.nih.mipav.model.structures.VOIVector;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewJProgressBar;
import gov.nih.mipav.view.dialogs.GuiBuilder;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.renderer.WildMagic.VolumeTriPlanarInterface;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.LatticeModel;
import gov.nih.mipav.view.renderer.WildMagic.WormUntwisting.WormSegmentation;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.BitSet;
import java.util.Random;
import java.util.StringTokenizer;
import java.util.Vector;

import javax.swing.JCheckBox;
import javax.swing.JFileChooser;
import javax.swing.JPanel;
import javax.swing.JTextField;

import WildMagic.LibFoundation.Mathematics.Vector3f;

/**
 * This class is a work in progress for developing algorithms to support the worm untwisting project.
 */
public class PlugInDialogWormLatticeStraighten extends JDialogStandalonePlugin implements AlgorithmInterface
{

	private static final long serialVersionUID = 2476025001402032629L;
	
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

	
	Vector<String> annotationNames;

	private int numSamples = 100;

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
				segmentOutline();
//				PlugInAlgorithmWormUntwisting.segmentSeamCells( null, includeRange, baseFileDir, baseFileNameText.getText() );
//				seamCellInfo();
			}
			if ( buildLattice.isSelected() )
			{
				PlugInAlgorithmWormUntwisting.buildLattice( null, includeRange,  baseFileDir, baseFileNameText.getText() );
			}
			if ( latticeStraighten.isSelected() )
			{
				PlugInAlgorithmWormUntwisting.latticeStraighten( null, includeRange, baseFileDir, null, baseFileNameText.getText(), 0, true );
			}
			if ( calcStatistics.isSelected() )
			{
				calcStatistics( true );
			}
			if ( registerImages.isSelected() )
			{
				PlugInAlgorithmWormUntwisting.registerImages( null, includeRange, baseFileDir, null, baseFileNameText.getText() );
			}
			if ( calcMaxProjection.isSelected() )
			{
				PlugInAlgorithmWormUntwisting.calcMaxProjection( null, includeRange, baseFileDir, null, baseFileNameText.getText() );
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
					MipavUtil.displayError("Error reading file: " + inputFileDir + File.separator + list[i] + " " + e.getCause() );
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (IOException e) {
					MipavUtil.displayError("Error reading file: " + inputFileDir + File.separator + list[i] + " " + e.getCause() );
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
		if ( maxIndex == -1 )
		{
			MipavUtil.displayError("Error reading files from: " + inputDirName + " no .csv files found." );
			return;
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
	
	/**
	 * Returns a blurred image of the input image.
	 * 
	 * @param image
	 * @param sigma
	 * @return
	 */
	private ModelImage blur(final ModelImage image, final int sigma) {
		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		imageName = imageName + "_gblur";

		final float[] sigmas = new float[] {sigma, sigma, sigma * getCorrectionFactor(image)};
		OpenCLAlgorithmGaussianBlur blurAlgo;

		final ModelImage resultImage = new ModelImage(image.getType(), image.getExtents(), imageName);
		JDialogBase.updateFileInfo(image, resultImage);
		blurAlgo = new OpenCLAlgorithmGaussianBlur(resultImage, image, sigmas, true, true, false);

		blurAlgo.setRed(true);
		blurAlgo.setGreen(true);
		blurAlgo.setBlue(true);
		blurAlgo.run();

		return blurAlgo.getDestImage();
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
	private void clean()
	{
		if ( includeRange != null )
		{
			for ( int i = 0; i < includeRange.size(); i++ )
			{
				String dirName;
				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + "output_images" + File.separator;
				deleteDirectory(dirName);
				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + "segmentation" + File.separator;
				deleteDirectory(dirName);
				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + "straightened_lattice" + File.separator;
				deleteDirectory(dirName);
				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + "straightened_annotations" + File.separator;
				deleteDirectory(dirName);
				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + "statistics" + File.separator;
				deleteDirectory(dirName);
				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + PlugInAlgorithmWormUntwisting.autoSeamCellSegmentationOutput + File.separator;
				deleteDirectory(dirName);
				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + PlugInAlgorithmWormUntwisting.autoSeamCellSegmentationOutput + "W" + File.separator;
				deleteDirectory(dirName);
				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + PlugInAlgorithmWormUntwisting.autoSeamCellSegmentationOutput + "K" + File.separator;
				deleteDirectory(dirName);
				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + PlugInAlgorithmWormUntwisting.autoSeamCellSegmentationOutput + "LoG" + File.separator;
				deleteDirectory(dirName);
				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + PlugInAlgorithmWormUntwisting.editSeamCellOutput + File.separator;
				deleteDirectory(dirName);
				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + PlugInAlgorithmWormUntwisting.editAnnotationOutput + File.separator;
				deleteDirectory(dirName);
				for ( int j = 0; j < 15; j++ )
				{
					dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + PlugInAlgorithmWormUntwisting.autoLatticeGenerationOutput + j + File.separator;
					deleteDirectory(dirName);
				}
				dirName = baseFileDir + File.separator + baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + File.separator + PlugInAlgorithmWormUntwisting.editLatticeOutput + File.separator;
				deleteDirectory(dirName);
			}
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
	/**
	 * IN PROGRESS to support k-means segmentation.
	 * 
	 * @param image
	 * @param intensityMin
	 * @param seedList
	 * @return
	 */
	private int fill(final ModelImage image, final float intensityMin, final Vector<Vector3f> seedList) {
		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;

		int count = 0;

		final BitSet mask = new BitSet(dimX * dimY * dimZ);
		while (seedList.size() > 0) {
			final Vector3f seed = seedList.remove(0);

			final int z = Math.round(seed.Z);
			final int y = Math.round(seed.Y);
			final int x = Math.round(seed.X);
			int index = z * dimX * dimY + y * dimX + x;
			if (mask.get(index)) {
				continue;
			}
			mask.set(index);
			float value;
			if (image.isColorImage()) {
				value = image.getFloatC(x, y, z, 2);
			} else {
				value = image.getFloat(x, y, z);
			}
			if ( (value >= intensityMin)) {
				for (int z1 = Math.max(0, z - 1); z1 <= Math.min(dimZ - 1, z + 1); z1++) {
					for (int y1 = Math.max(0, y - 1); y1 <= Math.min(dimY - 1, y + 1); y1++) {
						for (int x1 = Math.max(0, x - 1); x1 <= Math.min(dimX - 1, x + 1); x1++) {
							if ( ! ( (x == x1) && (y == y1) && (z == z1))) {
								index = z1 * dimX * dimY + y1 * dimX + x1;
								if ( !mask.get(index)) {
									if (image.isColorImage()) {
										value = image.getFloatC(x1, y1, z1, 2);
									} else {
										value = image.getFloat(x1, y1, z1);
									}
									if (value >= intensityMin) {
										seedList.add(new Vector3f(x1, y1, z1));
									}
								}
							}
						}
					}
				}
				count++;
			}
		}

		image.getMask().or(mask);
		return count;
	}

	/**
	 * IN PROGRESS Segmentation
	 * @param image
	 * @param cutoff
	 * @return
	 */
	private ModelImage fillImage( final ModelImage image, float cutoff )
    {
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
		imageName = imageName + "_5";

		final ModelImage resultImage = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), imageName);
		JDialogBase.updateFileInfo(image, resultImage);   
		
		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;  	

    	double min = image.getMin();

		for ( int z = 0; z < dimZ; z++ )
		{
			for ( int y = 0; y < dimY; y++ )
			{
				for ( int x = 0; x < dimX; x++ )
				{
					float value = image.getFloat(x,y,z);
					if ( value >= cutoff )
					{
						resultImage.set(x,  y, z, value);
					}
					else
					{
						resultImage.set(x,  y, z, min);
					}
				}
			}
    	}
    	return resultImage;
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
			PlugInAlgorithmWormUntwisting.loadAllVOIsFrom(wormImageA, voiDir, false, lattice, false);
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
			PlugInAlgorithmWormUntwisting.loadAllVOIsFrom(wormImageA, voiDir, false, lattice, false);
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
					PlugInAlgorithmWormUntwisting.loadAllVOIsFrom(wormImageA, voiDir, true, annotations, false);
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
					PlugInAlgorithmWormUntwisting.loadAllVOIsFrom(wormImageA, voiDir, true, annotations, false);
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



	/**
	 * Returns the amount of correction which should be applied to the z-direction sigma (assuming that correction is
	 * requested).
	 * 
	 * @return the amount to multiply the z-sigma by to correct for resolution differences
	 */
	private float getCorrectionFactor(final ModelImage image) {
		final int index = image.getExtents()[2] / 2;
		final float xRes = image.getFileInfo(index).getResolutions()[0];
		final float zRes = image.getFileInfo(index).getResolutions()[2];

		return xRes / zRes;
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
			bwX.write( "# rows: " + numSamples*(trainingData.size() - count50) + "\n");
			bwX.write( "# columns: 60"  + "\n");


			fwY = new FileWriter(fileY);
			BufferedWriter bwY = new BufferedWriter(fwY);
			bwY.write( "# name: cvY"  + "\n");
			bwY.write( "# type: matrix"  + "\n");
			bwY.write( "# rows: " + numSamples*(trainingData.size() - count50) + "\n");
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
			bwX.write( "# rows: " + numSamples*(trainingData.size() - count40) + "\n");
			bwX.write( "# columns: 60"  + "\n");


			fwY = new FileWriter(fileY);
			BufferedWriter bwY = new BufferedWriter(fwY);
			bwY.write( "# name: trainY"  + "\n");
			bwY.write( "# type: matrix"  + "\n");
			bwY.write( "# rows: " + numSamples*(trainingData.size() - count40) + "\n");
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

	/**
	 * IN PROGRESS to support difference-of-gaussian segmentation
	 * 
	 * @param image
	 * @return
	 */
	private ModelImage segmentAll2(final ModelImage image) {
		ModelImage blurs = blur(image, 3);
		blurs.calcMinMax();
		// new ViewJFrameImage(blurs);

		ModelImage blurb = blur(image, 5);
		blurb.calcMinMax();
		// new ViewJFrameImage(blurb);

		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}
		imageName = imageName + "_gblur";
		final ModelImage resultImage = new ModelImage(ModelStorageBase.FLOAT, image.getExtents(), imageName);
		JDialogBase.updateFileInfo(image, resultImage);

		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
		for (int z = 0; z < dimZ; z++) {
			for (int y = 0; y < dimY; y++) {
				for (int x = 0; x < dimX; x++) {
					resultImage.set(x, y, z, Math.max(0, blurs.getFloat(x, y, z) - blurb.getFloat(x, y, z)));
				}
			}
		}
		blurs.disposeLocal();
		blurs = null;
		blurb.disposeLocal();
		blurb = null;
		resultImage.calcMinMax();
		resultImage.restoreVOIs(image.getVOIsCopy());
		return resultImage;
	}
	
	/**
	 * 
	 * IN PROGRESS new segmentation algorithm based on K-means clustering. Segments the digestive tract.
	 * 
	 * @param image
	 * @param imageNoSeam
	 * @param minValue
	 * @param maxValue
	 * @param numClusters
	 * @param color
	 */
	private void segmentAll2(final ModelImage image, final ModelImage imageNoSeam, final float minValue, final float maxValue, final int numClusters,
			final float color) {
		String imageName = image.getImageName();
		if (imageName.contains("_clone")) {
			imageName = imageName.replaceAll("_clone", "");
		}

		System.err.println("segmentAll " + minValue + " " + maxValue);

		final Vector<Vector3f> positions = new Vector<Vector3f>();
		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;
		for (int z = 0; z < dimZ; z++) {
			for (int y = 0; y < dimY; y++) {
				for (int x = 0; x < dimX; x++) {
					if ( (imageNoSeam.getFloat(x, y, z) > minValue) && (imageNoSeam.getFloat(x, y, z) < maxValue)) {
						positions.add(new Vector3f(x, y, z));
					}
				}
			}
		}
		System.err.println("segmentAll " + positions.size());

		final int numAttempts = 1000;
		final Random randomGen = new Random();
		double minCost = Float.MAX_VALUE;
		Vector3f[] potentialClusters = null;
		for (int i = 0; i < numAttempts; i++) {
			// generate random potential cluster centers:
			final Vector<Integer> indexList = new Vector<Integer>();
			while (indexList.size() < numClusters) {
				final int index = (int) (randomGen.nextFloat() * (positions.size()));
				if ( !indexList.contains(index)) {
					indexList.add(index);
				}
			}
			final Vector3f[] centers = new Vector3f[numClusters];
			for (int j = 0; j < numClusters; j++) {
				centers[j] = new Vector3f(positions.elementAt(indexList.elementAt(j)));
			}

			boolean done = false;
			while ( !done) {
				VOIContour[] groups = new VOIContour[numClusters];
				// for each position find closest center and put in that group:
				for (int j = 0; j < positions.size(); j++) {
					int minGroupIndex = -1;
					float minGroupDist = Float.MAX_VALUE;
					for (int k = 0; k < numClusters; k++) {
						final float distance = positions.elementAt(j).distance(centers[k]);
						if (distance < minGroupDist) {
							minGroupDist = distance;
							minGroupIndex = k;
						}
					}
					if (groups[minGroupIndex] == null) {
						groups[minGroupIndex] = new VOIContour(false);
					}
					groups[minGroupIndex].add(positions.elementAt(j));
				}

				// calculate new center positions based on the average of group:
				Vector3f[] listCenters = new Vector3f[numClusters];
				for (int j = 0; j < groups.length; j++) {
					listCenters[j] = new Vector3f();
					if (groups[j] != null) {
						for (int k = 0; k < groups[j].size(); k++) {
							listCenters[j].add(groups[j].elementAt(k));
						}
						listCenters[j].scale(1f / groups[j].size());
					}
				}
				float maxMoved = -Float.MAX_VALUE;
				// check distance moved, if less than threshold, done:
				for (int j = 0; j < numClusters; j++) {
					final float dist = centers[j].distance(listCenters[j]);
					if (dist > maxMoved) {
						maxMoved = dist;
					}
					centers[j].copy(listCenters[j]);
				}
				if (maxMoved < 2) {
					// done:
					done = true;
					// calculate cost:
					double cost = 0;
					for (int j = 0; j < groups.length; j++) {
						if (groups[j] != null) {
							for (int k = 0; k < groups[j].size(); k++) {
								cost += centers[j].distance(groups[j].elementAt(k));
							}
						}
					}
					cost /= positions.size();
					if (cost < minCost) {
						minCost = cost;
						if (potentialClusters != null) {
							potentialClusters = null;
						}
						potentialClusters = new Vector3f[numClusters];
						for (int j = 0; j < centers.length; j++) {
							potentialClusters[j] = new Vector3f(centers[j]);
						}
					}
				} else {
					for (int j = 0; j < numClusters; j++) {
						if (groups[j] != null) {
							groups[j].clear();
						}
					}
					groups = null;
					listCenters = null;
				}
			}
		}

		ModelImage result = segmentAll2(imageNoSeam);
		final Vector<Vector3f> seeds = new Vector<Vector3f>();
		final Vector<Vector3f> newClusters = new Vector<Vector3f>();
		final Vector<Vector3f> midLineClusters = new Vector<Vector3f>();
		float maxSize = -Float.MAX_VALUE;
		int maxIndex = -1;
		for (int i = 0; i < potentialClusters.length; i++) {
			seeds.clear();
			seeds.add(potentialClusters[i]);
			final float size = fill(result, (float) (0.1 * result.getMax()), seeds);
			if (size > maxSize) {
				maxSize = size;
				maxIndex = i;
			}
			System.err.println("potential clusters " + i + " " + size);
			final BitSet mask = result.getMask();
			int count = 0;
			for (int j = 0; j < potentialClusters.length; j++) {
				final int index = (int) (potentialClusters[j].Z * dimX * dimY + potentialClusters[j].Y * dimX + potentialClusters[j].X);
				if (mask.get(index)) {
					count++;
				}
			}
			if ( (size > 0) && (size < 10000)) {
				newClusters.add(potentialClusters[i]);
			} else if (size > 0) {
				midLineClusters.add(potentialClusters[i]);
			}
			mask.clear();
		}

		System.err.println("newClusters " + newClusters.size());
		System.err.println("midLineClusters " + midLineClusters.size());

		String voiDir = image.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator + "segmentation" + File.separator;
		final File voiFileDir = new File(voiDir);
		if (voiFileDir.exists() && voiFileDir.isDirectory()) {} else if (voiFileDir.exists() && !voiFileDir.isDirectory()) { // voiFileDir.delete();
		} else { // voiFileDir does not exist
			voiFileDir.mkdir();
		}

		if (midLineClusters.size() > 0) {
			for (int i = 0; i < midLineClusters.size(); i++) {
				seeds.clear();
				seeds.add(potentialClusters[maxIndex]);
				final float size = fill(result, (float) (0.1 * result.getMax()), seeds);
				System.err.println("Midline clusters " + i + " " + size);
			}

			image.setMask((BitSet) result.getMask().clone());

			final ModelImage tmpImage = segmentAll2(image);

			final VOI seamCells = image.getVOIs().elementAt(0);
			final Vector<Vector3f> newMidLineClusters = new Vector<Vector3f>();
			final boolean[] removeSeam = new boolean[seamCells.getCurves().size()];
			for (int i = 0; i < seamCells.getCurves().size(); i++) {
				tmpImage.getMask().clear();
				seeds.clear();
				seeds.add(seamCells.getCurves().elementAt(i).elementAt(0));
				final float size = fill(tmpImage, (float) (0.1 * result.getMax()), seeds);
				System.err.println("Original clusters " + i + " " + size);
				if ( (size >= 10000)) {
					newMidLineClusters.add(seamCells.getCurves().elementAt(i).elementAt(0));
				}
				boolean midLine = false;
				for (int j = 0; j < midLineClusters.size(); j++) {
					final int index = (int) (midLineClusters.elementAt(j).Z * dimX * dimY + midLineClusters.elementAt(j).Y * dimX + midLineClusters
							.elementAt(j).X);
					if (tmpImage.getMask().get(index)) {
						System.err.println("found midLine");
						midLine = true;
						break;
					}
				}
				if (midLine) {
					midLineClusters.add(seamCells.getCurves().elementAt(i).elementAt(0));
					image.getMask().or(tmpImage.getMask());
					removeSeam[i] = true;
				}
			}
			for (int i = removeSeam.length - 1; i >= 0; i--) {
				if (removeSeam[i]) {
					seamCells.getCurves().remove(i);
				}
			}
			System.err.println("newMidLineClusters " + newMidLineClusters.size());

			for (int z = 0; z < dimZ; z++) {
				for (int y = 0; y < dimY; y++) {
					for (int x = 0; x < dimX; x++) {
						final int index = z * dimY * dimX + y * dimX + x;
						if (image.getMask().get(index)) {
							result.set(x, y, z, image.get(x, y, z));
						} else {
							result.set(x, y, z, image.getMin());
						}
					}
				}
			}

			voiDir = image.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator + "segmentation" + File.separator;
			result.setImageName(imageName + "_midLine.xml");
			ModelImage.saveImage(result, imageName + "_midLine.xml", voiDir, false);
			new ViewJFrameImage((ModelImage)result.clone());
			result.disposeLocal();
			result = null;
		}

		if (newClusters.size() > 0) {
			final short sID = (short) (image.getVOIs().getUniqueID());
			final VOI clusters = new VOI(sID, "clusters2", VOI.POINT, color);
			for (int i = 0; i < newClusters.size(); i++) {
				System.err.println("new cluster " + newClusters.elementAt(i));
				clusters.importPoint(newClusters.elementAt(i));
			}
			image.registerVOI(clusters);
		}
		voiDir = image.getImageDirectory() + JDialogBase.makeImageName(imageName, "") + File.separator + "segmentation" + File.separator + "seam_cells"
				+ File.separator;
		LatticeModel.saveAllVOIsTo(voiDir, image);
//		 new ViewJFrameImage((ModelImage)image.clone());
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
						PlugInAlgorithmWormUntwisting.loadAllVOIsFrom(wormImageA, voiDir, true, annotations, false);
						if ( annotations.size() == 0 )
						{
							fileName = baseFileNameText.getText() + "_" + includeRange.elementAt(i) + File.separator + "annotation";            	    		
							annotations = new VOIVector();
							voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
							PlugInAlgorithmWormUntwisting.loadAllVOIsFrom(wormImageA, voiDir, true, annotations, false);
						}
						if ( annotations.size() > 0 )
						{
							ModelImage segmentation = segmentAnnotations( wormImageA, annotations.elementAt(0), includeRange.elementAt(i), statsFileBW );

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
						PlugInAlgorithmWormUntwisting.loadAllVOIsFrom(wormImageA, voiDir, true, annotations, false);
						if ( annotations.size() == 0 )
						{
							fileName = baseFileNameText.getText() + "_" + fileCount + File.separator + "annotation";            	    		
							annotations = new VOIVector();
							voiDir = new String(baseFileDir + File.separator + fileName + File.separator);
							PlugInAlgorithmWormUntwisting.loadAllVOIsFrom(wormImageA, voiDir, true, annotations, false);
						}
						if ( annotations.size() > 0 )
						{
							ModelImage segmentation = segmentAnnotations( wormImageA, annotations.elementAt(0), fileCount, statsFileBW );
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

	/**
	 * IN PROGRESS segmentation based on difference of gaussians.
	 * 
	 * @param image
	 * @param annotations
	 * @param time
	 * @param statistics
	 * @return
	 */
	private ModelImage segmentAnnotations(final ModelImage image, final VOI annotations, final int time, final BufferedWriter statistics) {
		final int dimX = image.getExtents().length > 0 ? image.getExtents()[0] : 1;
		final int dimY = image.getExtents().length > 1 ? image.getExtents()[1] : 1;
		final int dimZ = image.getExtents().length > 2 ? image.getExtents()[2] : 1;

		final ModelImage result = segmentAll2(image);

		final float cutoff = (float) (0.125 * result.getMax());
		final float[] minValue = new float[annotations.getCurves().size()];
		final Vector<Vector3f> positions = new Vector<Vector3f>();
		for (int i = 0; i < annotations.getCurves().size(); i++) {
			final VOIText text = (VOIText) annotations.getCurves().elementAt(i);
			if (text.getText().equalsIgnoreCase("nose") || text.getText().equalsIgnoreCase("origin")) {
				continue;
			}
			final Vector3f pos = text.elementAt(0);
			final Vector3f seed = new Vector3f(pos);
			minValue[i] = result.getFloat((int) pos.X, (int) pos.Y, (int) pos.Z);

			if (minValue[i] < cutoff) {
				for (int z = (int) (pos.Z - 4); z <= pos.Z + 4; z++) {
					for (int y = (int) (pos.Y - 4); y <= pos.Y + 4; y++) {
						for (int x = (int) (pos.X - 4); x <= pos.X + 4; x++) {

							final float value = result.getFloat(x, y, z);
							if ( (value >= cutoff) && (value > minValue[i])) {
								minValue[i] = value;
								seed.set(x, y, z);
							} else if ( (minValue[i] < cutoff) && (value > minValue[i])) {
								minValue[i] = value;
								seed.set(x, y, z);
							}
						}
					}
				}
			}
			positions.add(seed);
		}

		// System.err.println( minValue + "     " + (float)(0.1 * result.getMax()) + "     " + 0.75f * minValue );
		// if ( minValue < (float)(0.1 * result.getMax()) )
		// {
		// minValue = 0.75f * minValue;
		// }
		final BitSet allNodes = new BitSet(dimX * dimY * dimZ);
		final Vector<Vector3f> seeds = new Vector<Vector3f>();
		int posCount = 0;
		for (int i = 0; i < annotations.getCurves().size(); i++) {
			final VOIText text = (VOIText) annotations.getCurves().elementAt(i);
			if (text.getText().equalsIgnoreCase("nose") || text.getText().equalsIgnoreCase("origin")) {
				continue;
			}
			final Vector3f pos = positions.elementAt(posCount++);
			seeds.clear();
			seeds.add(pos);
			float size = fill(result, minValue[i], seeds);
			// System.err.println( text.getText() + " " + size );
			// System.err.println( minValue[i] + "     " + (float)(0.1 * result.getMax()) + "     " + overallMin + " " +
			// 100* (overallMin/result.getMax()) + " " + size );

			if (size == 0) {
				System.err.println(i + "    " + minValue[i] + "     " + 100 * (minValue[i] / result.getMax()) + "  " + size);
			}

			if (size > 20000) {
				result.getMask().clear();
				System.err.println(i + "    " + minValue[i] + "     " + 100 * (minValue[i] / result.getMax()) + "  " + size);

				// pos = text.elementAt(0);
				size = 0;
				for (int z = (int) (pos.Z - 4); z <= pos.Z + 4; z++) {
					for (int y = (int) (pos.Y - 4); y <= pos.Y + 4; y++) {
						for (int x = (int) (pos.X - 4); x <= pos.X + 4; x++) {
							if (pos.distance(new Vector3f(x, y, z)) <= 4) {
								final float value = result.getFloat(x, y, z);
								if (value >= minValue[i]) {
									final int index = z * dimY * dimX + y * dimX + x;
									result.getMask().set(index);
									size++;
								}
							}
						}
					}
				}
			}

			System.err.println(i + "    " + minValue[i] + "     " + 100 * (minValue[i] / result.getMax()) + "  " + size);

			allNodes.or(result.getMask());
			result.getMask().clear();

			final String sameSide = text.getText().contains("L") ? "L" : "R";
			final String opposite = text.getText().contains("L") ? "R" : "L";
			final String[] ids = text.getText().split(sameSide);
			int index = -1;
			for (int j = 0; j < ids.length; j++) {
				if (ids[j].length() > 0) {
					index = Integer.valueOf(ids[j]);
				}
			}
			if (index != -1) {
				float distancePair = 0;
				float distancePrevSame = 0;
				float distancePrevOpposite = 0;
				float distanceNextSame = 0;
				float distanceNextOpposite = 0;
				final String pair = new String(index + opposite);
				final String prevSame = new String( (index - 1) + sameSide);
				final String prevOpposite = new String( (index - 1) + opposite);
				final String nextSame = new String( (index + 1) + sameSide);
				final String nextOpposite = new String( (index + 1) + opposite);
				for (int j = 0; j < annotations.getCurves().size(); j++) {
					final VOIText text2 = (VOIText) annotations.getCurves().elementAt(j);
					if (text2.getText().equals(pair)) {
						distancePair = text.elementAt(0).distance(text2.elementAt(0));
					}
					if (text2.getText().equals(prevSame)) {
						distancePrevSame = text.elementAt(0).distance(text2.elementAt(0));
					}
					if (text2.getText().equals(prevOpposite)) {
						distancePrevOpposite = text.elementAt(0).distance(text2.elementAt(0));
					}
					if (text2.getText().equals(nextSame)) {
						distanceNextSame = text.elementAt(0).distance(text2.elementAt(0));
					}
					if (text2.getText().equals(nextOpposite)) {
						distanceNextOpposite = text.elementAt(0).distance(text2.elementAt(0));
					}
				}
				final float imageValue = image.getFloat((int) pos.X, (int) pos.Y, (int) pos.Z);
				final float dogValue = result.getFloat((int) pos.X, (int) pos.Y, (int) pos.Z);
				try {
					statistics.write(time + "," + text.getText() + "," + distancePair + "," + distancePrevSame + "," + distancePrevOpposite + ","
							+ distanceNextSame + "," + distanceNextOpposite + "," + size + "," + imageValue + "," + dogValue + "," + minValue[i] + "\n");
				} catch (final IOException e) {}
			}
		}

		for (int z = 0; z < dimZ; z++) {
			for (int y = 0; y < dimY; y++) {
				for (int x = 0; x < dimX; x++) {
					final int index = z * dimY * dimX + y * dimX + x;
					if (allNodes.get(index)) {
						result.set(x, y, z, image.get(x, y, z));
					} else {
						result.set(x, y, z, image.getMin());
					}
				}
			}
		}
		// new ViewJFrameImage(result);
		result.calcMinMax();
		return result;
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
					if ( maximumProjectionImage != null )
					{
						maximumProjectionImage.disposeLocal();
						maximumProjectionImage = null;
					}
					wormImageA = fileIO.readImage(fileName, baseFileDir + File.separator, false, null);
					maximumProjectionImage = new ModelImage( ModelStorageBase.ARGB_FLOAT, wormImageA.getExtents(), baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + "_surface.tif" );
					JDialogBase.updateFileInfo(wormImageA, maximumProjectionImage);

//					ModelImage contourImage = new ModelImage( wormImageA.getType(), wormImageA.getExtents(), baseFileNameText.getText() + "_"  + includeRange.elementAt(i) + "_inside.tif" );
//					JDialogBase.updateFileInfo(wormImageA, contourImage);
					
					fileName = baseFileDir + File.separator;
					WormSegmentation.outside( fileName, wormImageA, maximumProjectionImage, 5 /*(float) (wormImageA.getMax() * 0.01) */, 3 );
					
					
					System.err.println("Done segmentation");
				}    				
			}
		}
		if( wormImageA != null )
		{
			wormImageA.disposeLocal();
			wormImageA = null;
		}

	}

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


}
