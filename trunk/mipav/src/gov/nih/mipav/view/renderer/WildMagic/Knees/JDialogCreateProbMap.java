package gov.nih.mipav.view.renderer.WildMagic.Knees;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;

import WildMagic.LibFoundation.Curves.BSplineCurve3f;
import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * This code is used to create VOI from Alexnet generated energy maps. 
 * We applie this piece of work to MICCAI 2016, get rejected.   So, leave alone. 
 * 
 * ------------------------------------------------------------------
 * no longer used, just for backup purpose only. 
 * ------------------------------------------------------------------
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogCreateProbMap extends JDialogBase
		implements AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194259L;

	/** The main user interface. */
	private ViewUserInterface UI;

	/**
	 * bounding box for crop the image. Currently set from 0 to 512, the orginal
	 * image slice size.
	 */
	private int boxYmin, boxYmax;
	private int boxXmin, boxXmax;

	/** X cropped region bounds. */
	private int[] xBounds = new int[2];

	/** Y cropped region bounds. */
	private int[] yBounds = new int[2];

	/** Z cropped region bound. */
	private int[] zBounds = new int[2];

	/** crop margin algorithm. */
	private AlgorithmAddMargins cropAlgo;

	/** result cropped image. */
	private ModelImage cropImage = null;

	/** key image directory. */
	private JLabel labelKeyImage;
	private JTextField textFieldKeyImage;
	private JButton buttonKeyImage;

	private JPanel imageSelectionPanel;

	/** key images variables. */
	private JFileChooser keyImageChooser = new JFileChooser();
	private String keyImageDirectory;

	// axis region
	private JComboBox axisList;
	private JLabel labelAxis;

	private static int Axial = 0;
	private static int Saggital = 1;
	private static int Coronal = 2;
	private int axis = Axial;

	/** image vector to hold the image names. */
	private Vector<String> keyImageVector = new Vector<String>();

	/** image vector to hold the actual images. */
	private Vector<ModelImage> keyImages = new Vector<ModelImage>();

	/** voi vector to hold the VOI names. */
	private Vector<String> keyImageVOIVector = new Vector<String>();

	/** voi vector to hold the actual vois. */
	private Vector<VOI[]> keyImageVOIs = new Vector<VOI[]>();

	/** cropped key image vector. */
	private Vector<ModelImage> cropKeyImages = new Vector<ModelImage>();

	/** saved 2D slices atlas dir. */
	private JLabel labelSaveImage;
	private JTextField textFieldSaveImage;
	private JButton buttonSaveImage;
	private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageDirectory;
	
	private ModelImage srcImage;

	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogCreateProbMap(Frame theParentFrame, ModelImage _srcImage) {
		super(theParentFrame, false);
		srcImage = _srcImage;
		

	}
	
	
	public void test() {
		
		// String inputFileName = "C:\\TestMorePatchesBatches\\1\\fc512-11pct_test_predictions.txt";
		// String outputFileName = "C:\\TestMorePatchesBatches\\1\\";
		// String coordinateFileName = "C:\\TestMorePatches\\1\\patchCoordinate.txt";
		
		// String inputFileName = "C:\\50_dp_Batch\\1\\fc512-11pct_test_predictions.txt";
		// String outputFileName = "C:\\50_dp_Batch\\1\\";
		// String coordinateFileName = "C:\\50_dp_Test\\1\\patchCoordinate.txt";
		
		// String inputFileName = "C:\\50_test_Batch\\20\\fc512-11pct_test_predictions.txt";
		// String outputFileName = "C:\\50_test_Batch\\20\\";
		// String coordinateFileName = "C:\\50_dp_Test\\20\\patchCoordinate.txt";
		
		// String inputFileName = "C:/Knees_test_batches/1/fc512-11pct_test_predictions.txt";
		// String outputFileName = "C:/Knees_test_batches/1/";
		// String coordinateFileName = "C:/Knees_dp_Test/1/patchCoordinate.txt";
		
		// String inputFileName = "/scratch/MiccaiRebut/results/fold1/"+ num + "/";
		// String outputFileName = "/scratch/MiccaiRebut/run1Batches/"+ num + "/fc512-11pct_test_predictions.txt";
		
		
		// String inputFileName = "/scratch/MiccaiRebut/run1Batches/1/fc512-11pct_test_predictions.txt";
		// String outputFileName = "C:/Knees_test_batches/1/";
		// String coordinateFileName = "C:/Knees_dp_Test/1/patchCoordinate.txt";
		
		String inputFileName = "/scratch/MiccaiRebut/run1Batches/1/fc512-11pct_test_predictions.txt";
	    String outputFileName = "/scratch/MiccaiRebut/run1Batches/1/";
		String coordinateFileName = "/scratch/MiccaiRebut/run1Patches/1/patchCoordinate.txt";
		
		
		try {
			FileReader coordReader = new FileReader(coordinateFileName);
			FileReader reader = new FileReader(inputFileName);
			Scanner in = new Scanner(reader);
			Scanner inCoord = new Scanner(coordReader);
			
			String firstToken, secondToken, thirdToken;
			int len, startIndex, endIndex;
			float possibility;
			float xCoord, yCoord;
			in.nextLine();
			
			int numSlices = Integer.valueOf(inCoord.nextLine());
			int []ext = new int[3];
			ext = srcImage.getExtents();
			
			ModelImage probabilityMap = new ModelImage(ModelStorageBase.FLOAT, ext, "probMap");
			
			for (int i = 0; i < ext[2]; i++) { 
			    probabilityMap.getMatrixHolder().replaceMatrices(srcImage.getMatrixHolder().getMatrices());
				// probabilityMap.getFileInfo(i).setOrigin(srcImage.getFileInfo(i).getOrigin());
				probabilityMap.getFileInfo(i).setModality(srcImage.getFileInfo()[i].getModality());
				// probabilityMap.getFileInfo(i).setFileDirectory(srcImage.getFileInfo()[i].getFileDirectory());
				probabilityMap.getFileInfo(i).setEndianess(srcImage.getFileInfo()[i].getEndianess());
				probabilityMap.getFileInfo(i).setUnitsOfMeasure(srcImage.getFileInfo()[i].getUnitsOfMeasure());
				probabilityMap.getFileInfo(i).setResolutions(srcImage.getFileInfo()[i].getResolutions());
				probabilityMap.getFileInfo(i).setExtents(srcImage.getExtents());
				// probabilityMap.getFileInfo(i).setMax(srcImage.getMax());
				// probabilityMap.getFileInfo(i).setMin(srcImage.getMin());
				probabilityMap.getFileInfo(i).setImageOrientation(srcImage.getImageOrientation());
				probabilityMap.getFileInfo(i).setAxisOrientation(srcImage.getFileInfo()[i].getAxisOrientation());
				probabilityMap.getFileInfo(i).setOrigin(srcImage.getFileInfo()[i].getOrigin());
				probabilityMap.getFileInfo(i).setPixelPadValue(srcImage.getFileInfo()[i].getPixelPadValue());
				probabilityMap.getFileInfo(i).setPhotometric(srcImage.getFileInfo()[i].getPhotometric());
			
			}
			int sliceSize = ext[0] * ext[1];
			
		    float[] sliceBuffer = new float[sliceSize];
		    float[] buffer = new float[sliceSize * ext[2]];
		    probabilityMap.importData(0, buffer, false);
		    
		    int sliceNum = 0;
		    int previousNum = 0;
		    
			while (in.hasNextLine()) {
				
				int count = 39;
				
			    while ( in.hasNextLine() ) {
			    	String lineCoord = inCoord.nextLine();
			    	StringTokenizer tokenCoord = new StringTokenizer(lineCoord, "_");
			    	tokenCoord.nextToken();
			    	String sliceStr = tokenCoord.nextToken();
			    	sliceStr = sliceStr.substring(5, sliceStr.length());
			    	sliceNum = Integer.valueOf(sliceStr);
			    	
			    	if ( sliceNum != previousNum ) {
			    		probabilityMap.importData(previousNum * sliceSize, sliceBuffer, false);
				        sliceBuffer = null;
				        sliceBuffer = new float[sliceSize];
			    	}
			    	
			    	// System.err.println("sliceNum = " + sliceNum);
			    	tokenCoord.nextToken();
			    	tokenCoord.nextToken();
			    	String coordStr = tokenCoord.nextToken();
			    	StringTokenizer tokenNum = new StringTokenizer(coordStr, " ");
			    	tokenNum.nextToken();
			    	// String xStr = tokenCoord.nextToken();
			    	// String yStr = tokenCoord.nextToken();
			    	xCoord = Float.valueOf(tokenNum.nextToken());
			    	yCoord = Float.valueOf(tokenNum.nextToken());
			    	// System.err.println("xCoord = " + xCoord + "   yCoord = " + yCoord);
			    	
			    	String line = in.nextLine();
			    	StringTokenizer token = new StringTokenizer(line, ",");
			    	token.nextToken();
			    	token.nextToken();
			    	thirdToken = token.nextToken();
			    	
			    	startIndex = 0;
					endIndex = thirdToken.length();
					startIndex++;
					endIndex--;
					// System.err.println(thirdToken);
					while ( !Character.isDigit(thirdToken.charAt(startIndex)) ) {
						if ( thirdToken.charAt(startIndex) == ' ' )
							startIndex++;
					}
					
					thirdToken = thirdToken.substring(startIndex, endIndex);
					token = new StringTokenizer(thirdToken, " ");
					token.nextToken();
			    	possibility = Float.valueOf(token.nextToken());
			        
			    	if ( yCoord < 0 )  {
			    		sliceBuffer[(int)(xCoord) + (int)(1) * ext[0]] = 0.9f;
			    	} else {
			    	    sliceBuffer[(int)(xCoord) + (int)(yCoord) * ext[0]] = possibility;
			    	}
			    	previousNum = sliceNum;
			    	
			    	if ( count != 1 ) {
			    		// writer.print(possibility + ", ");
			    	} else {
			    		// writer.print(possibility + "])\n");
			    		break;
			    	}
			    	
			    	count--;
			    	
			    }
		    	
			}
		    
			probabilityMap.importData(sliceNum * sliceSize, sliceBuffer, false);
	        
		    in.close();
		    new ViewJFrameImage(probabilityMap);
		    probabilityMap.saveImage(outputFileName, "probMap.xml", FileUtility.XML, false);
		    
		} catch ( IOException e ) {
			e.printStackTrace();
		}
		
	}
	
	
    public void generateVOI() {
		
		// String inputFileName = "C:\\TestMorePatchesBatches\\1\\fc512-11pct_test_predictions.txt";
		// String outputFileName = "C:\\TestMorePatchesBatches\\1\\";
		// String coordinateFileName = "C:\\TestMorePatches\\1\\patchCoordinate.txt";
		
		// String inputFileName = "C:\\50_dp_Batch\\20\\fc512-11pct_test_predictions.txt";
		// String outputFileName = "C:\\50_dp_Batch\\20\\";
		// String coordinateFileName = "C:\\50_dp_Test\\20\\patchCoordinate.txt";
		
		// String inputFileName = "C:\\50_test_Batch\\20\\fc512-11pct_test_predictions.txt";
		// String outputFileName = "C:\\50_test_Batch\\20\\";
		// String coordinateFileName = "C:\\50_dp_Test\\20\\patchCoordinate.txt";
		
    	
    	// String inputFileName = "C:/test64Batches/20/fc512-11pct_test_predictions.txt";
		// String outputFileName = "C:/test64Batches/20/";
		// String coordinateFileName = "C:/test64/20/patchCoordinate.txt";
		
		// String inputFileName = "C:/Knees_test_batches/1/fc512-11pct_test_predictions.txt";
		// String outputFileName = "C:/Knees_test_batches/1/";
		// String coordinateFileName = "C:/Knees_dp_Test/1/patchCoordinate.txt";
    	
		// String inputFileName = "/scratch/KneesTestBatches/fold1/1/fc512-11pct_test_predictions.txt";
		// String outputFileName = "/scratch/KneesTestBatches/fold1/1/";
		// String coordinateFileName = "/scratch/KneesTestPatches/fold1/1/patchCoordinate.txt";
		
		// String inputFileName = "/scratch/KneesTestBatches/fold1/1/fc512-11pct_test_predictions.txt";
		// String outputFileName = "/scratch/KneesTestBatches/fold1/1/";
		// String coordinateFileName = "/scratch/kneesTestBackup/patchesFold2/0/patchCoordinate.txt";
		
		String inputFileName = "/scratch/MiccaiRebut/run1Batches/1/fc512-11pct_test_predictions.txt";
	    String outputFileName = "/scratch/MiccaiRebut/run1Batches/1/";
		String coordinateFileName = "/scratch/MiccaiRebut/run1Patches/1/patchCoordinate.txt";
    	
		try {
			FileReader coordReader = new FileReader(coordinateFileName);
			FileReader reader = new FileReader(inputFileName);
			Scanner in = new Scanner(reader);
			Scanner inCoord = new Scanner(coordReader);
			
			String firstToken, secondToken, thirdToken;
			int len, startIndex, endIndex;
			float possibility;
			float xCoord, yCoord;
			in.nextLine();
			
			int numSlices = Integer.valueOf(inCoord.nextLine());
			int []ext = new int[3];
			ext = srcImage.getExtents();
			
			ModelImage probabilityMap = new ModelImage(ModelStorageBase.FLOAT, ext, "probMap");
			
			for (int i = 0; i < ext[2]; i++) { 
			    probabilityMap.getMatrixHolder().replaceMatrices(srcImage.getMatrixHolder().getMatrices());
				// probabilityMap.getFileInfo(i).setOrigin(srcImage.getFileInfo(i).getOrigin());
				probabilityMap.getFileInfo(i).setModality(srcImage.getFileInfo()[i].getModality());
				// probabilityMap.getFileInfo(i).setFileDirectory(srcImage.getFileInfo()[i].getFileDirectory());
				probabilityMap.getFileInfo(i).setEndianess(srcImage.getFileInfo()[i].getEndianess());
				probabilityMap.getFileInfo(i).setUnitsOfMeasure(srcImage.getFileInfo()[i].getUnitsOfMeasure());
				probabilityMap.getFileInfo(i).setResolutions(srcImage.getFileInfo()[i].getResolutions());
				probabilityMap.getFileInfo(i).setExtents(srcImage.getExtents());
				// probabilityMap.getFileInfo(i).setMax(srcImage.getMax());
				// probabilityMap.getFileInfo(i).setMin(srcImage.getMin());
				probabilityMap.getFileInfo(i).setImageOrientation(srcImage.getImageOrientation());
				probabilityMap.getFileInfo(i).setAxisOrientation(srcImage.getFileInfo()[i].getAxisOrientation());
				probabilityMap.getFileInfo(i).setOrigin(srcImage.getFileInfo()[i].getOrigin());
				probabilityMap.getFileInfo(i).setPixelPadValue(srcImage.getFileInfo()[i].getPixelPadValue());
				probabilityMap.getFileInfo(i).setPhotometric(srcImage.getFileInfo()[i].getPhotometric());
			
			}
			int sliceSize = ext[0] * ext[1];
			
		    float[] sliceBuffer = new float[sliceSize];
		    float[] buffer = new float[sliceSize * ext[2]];
		    probabilityMap.importData(0, buffer, false);
		    
		    int sliceNum = 0;
		    int previousNum = -1;
		    
		    float prob = 0f, x_h = 0f, y_h = 0f;
		    int nPts = 30;
		    float x_current[] = new float[nPts];
		    float y_current[] = new float[nPts];
		    float z_current[] = new float[nPts];
		    int pt_index = 0;
		    
		    VOIVector voiVectorNew = new VOIVector();
			VOI voiNew = new VOI((short) 0, "ImageVOI");
			voiVectorNew.add(voiNew);
		    
		    
			while (in.hasNextLine()) {
				
				int count = 39;
				
			    while ( in.hasNextLine() ) {
			    	String lineCoord = inCoord.nextLine();
			    	StringTokenizer tokenCoord = new StringTokenizer(lineCoord, "_");
			    	tokenCoord.nextToken();
			    	String sliceStr = tokenCoord.nextToken();
			    	sliceStr = sliceStr.substring(5, sliceStr.length());
			    	sliceNum = Integer.valueOf(sliceStr);
			    	
			    	if ( sliceNum != previousNum && previousNum != -1 ) {
			    		probabilityMap.importData(previousNum * sliceSize, sliceBuffer, false);
				        sliceBuffer = null;
				        sliceBuffer = new float[sliceSize];
				       
				        
				        Vector3f[] BsplinePoints = new Vector3f[nPts];
						for (int w = 0; w < nPts; w++) {
							BsplinePoints[w] = new Vector3f(x_current[w], y_current[w], 0);
						}
                        
						BSplineCurve3f curve = BSplineCurve3f.CreateApproximation(BsplinePoints, nPts - 5, 2);
						float minTime = curve.GetMinTime();
						float maxTime = curve.GetMaxTime();
						float step = (maxTime - minTime) / 30;

						Vector<Point> extractedPoints = new Vector<Point>();

						int ptCount = 0;
						for (float t = minTime; t <= maxTime; t += 4*step) {
							Vector3f pt = curve.GetPosition(t);
							extractedPoints.add(new Point((int) pt.X, (int) pt.Y));
							ptCount++;
						}

						// nPts = 32;
						float []x_arr = new float[ptCount];
						float []y_arr = new float[ptCount];
						float []z_arr = new float[ptCount];
						
						for (int j = 0; j < ptCount; j++) {
							Point p = extractedPoints.get(j);
							x_arr[j] = p.x;
							y_arr[j] = p.y;
							z_arr[j] = z_current[j];
						}
				       
				        
				        VOIBase vTemp = new VOIContour(true);   // (VOIBase) current_v.clone();
				        vTemp.importArrays(x_current, y_current, z_current, nPts);
				        voiNew.importCurve(vTemp);
				        vTemp = null;
				       
						
				        pt_index = 0;
				        prob = 0f;
			    		x_h = 0f;
			    		y_h = 0f;
				        
			    	}
			    	
			    	System.err.println("sliceNum = " + sliceNum);
			    	tokenCoord.nextToken();
			    	tokenCoord.nextToken();
			    	String coordStr = tokenCoord.nextToken();
			    	StringTokenizer tokenNum = new StringTokenizer(coordStr, " ");
			    	tokenNum.nextToken();
			    	// String xStr = tokenCoord.nextToken();
			    	// String yStr = tokenCoord.nextToken();
			    	// xCoord = Float.valueOf(tokenNum.nextToken());
			    	// yCoord = Float.valueOf(tokenNum.nextToken());
			    	
			    	xCoord = Float.parseFloat(tokenNum.nextToken());
			    	yCoord = Float.parseFloat(tokenNum.nextToken());
			    	
			    	System.err.println("xCoord = " + xCoord + "   yCoord = " + yCoord);
			    	
			    	String line = in.nextLine();
			    	StringTokenizer token = new StringTokenizer(line, ",");
			    	token.nextToken();
			    	token.nextToken();
			    	thirdToken = token.nextToken();
			    	
			    	startIndex = 0;
					endIndex = thirdToken.length();
					startIndex++;
					endIndex--;
					// System.err.println(thirdToken);
					while ( !Character.isDigit(thirdToken.charAt(startIndex)) ) {
						if ( thirdToken.charAt(startIndex) == ' ' )
							startIndex++;
					}
					
					thirdToken = thirdToken.substring(startIndex, endIndex);
					token = new StringTokenizer(thirdToken, " ");
					token.nextToken();
			    	possibility = Float.valueOf(token.nextToken());
			        
			    	
			    	if ( possibility > prob ) {
			    		prob = possibility;
			    		x_h = xCoord;
			    		y_h = yCoord;
			    	}
			    	
			    	if ( yCoord < 0 )  {
			    		sliceBuffer[(int)(xCoord) + (int)(1) * ext[0]] = 0.9f;
			    	} else {
			    	    sliceBuffer[(int)(xCoord) + (int)(yCoord) * ext[0]] = possibility;
			    	}
			    	
			    	previousNum = sliceNum;
			    	
			    	if ( count != 1 ) {
			    		// writer.print(possibility + ", ");
			    	} else {
			    		// writer.print(possibility + "])\n");
			    		x_current[pt_index] = x_h;
			    		y_current[pt_index] = y_h;
			    		z_current[pt_index] = previousNum;
			    		pt_index++;
			    		
			    		prob = 0f;
			    		x_h = 0f;
			    		y_h = 0f;
			    		break;
			    	}
			    	
			    	count--;
			    	
			    }
		    	
			}
		    
			
			
			probabilityMap.importData(sliceNum * sliceSize, sliceBuffer, false);
			
			   
			   Vector3f[] BsplinePoints = new Vector3f[nPts];
				for (int w = 0; w < nPts; w++) {
					BsplinePoints[w] = new Vector3f(x_current[w], y_current[w], 0);
				}

				BSplineCurve3f curve = BSplineCurve3f.CreateApproximation(BsplinePoints, nPts - 5, 2);
				float minTime = curve.GetMinTime();
				float maxTime = curve.GetMaxTime();
				float step = (maxTime - minTime) / 30;

				Vector<Point> extractedPoints = new Vector<Point>();

				int ptCount = 0;
				for (float t = minTime; t <= maxTime; t += 4*step) {
					Vector3f pt = curve.GetPosition(t);
					extractedPoints.add(new Point((int) pt.X, (int) pt.Y));
					ptCount++;
				}

				// nPts = 32;
				float []x_arr = new float[ptCount];
				float []y_arr = new float[ptCount];
				float []z_arr = new float[ptCount];
				
				for (int j = 0; j < ptCount; j++) {
					Point p = extractedPoints.get(j);
					x_arr[j] = p.x;
					y_arr[j] = p.y;
					z_arr[j] = sliceNum;
				}
		        
		        
		        VOIBase vTemp = new VOIContour(true);   // (VOIBase) current_v.clone();
		        vTemp.importArrays(x_current, y_current, z_current, nPts);
		        voiNew.importCurve(vTemp);
		        vTemp = null;
			
			
			
			
		    in.close();
		    new ViewJFrameImage(probabilityMap);
		    probabilityMap.saveImage(outputFileName, "probMap.xml", FileUtility.XML, false);
		    probabilityMap.addVOIs(voiVectorNew);
		    smoothVOI30(probabilityMap, probabilityMap);
		    
		} catch ( IOException e ) {
			e.printStackTrace();
		}
		
	}
	

	
	/**
	 * dispose memory
	 * */
	public void disposeLocal() {
		int i;
		for (i = 0; i < keyImageVector.size(); i++) {
			String temp = keyImageVector.get(i);
			temp = null;
		}
		keyImageVector = null;

		for (i = 0; i < keyImages.size(); i++) {
			ModelImage temp = keyImages.get(i);
			temp.disposeLocal();
		}
		keyImages = null;

		for (i = 0; i < keyImageVOIVector.size(); i++) {
			String temp = keyImageVOIVector.get(i);
			temp = null;
		}
		keyImageVOIVector = null;

		for (i = 0; i < keyImageVOIs.size(); i++) {
			VOI[] temp = keyImageVOIs.get(i);
			temp = null;
		}
		keyImageVOIs = null;

		for (i = 0; i < cropKeyImages.size(); i++) {
			ModelImage temp = cropKeyImages.get(i);
			temp.disposeLocal();
		}
		cropKeyImages = null;

	}

	/**
	 * Dialog local actionPerformed handler.
	 */
	public void actionPerformed(ActionEvent event) {

		String command = event.getActionCommand();
		if (command.equals("OK")) {
			callAlgorithm();
		} else if (command.equals("Cancel")) {
			dispose();
		} else if (command.equals("Help")) {
			// MipavUtil.showHelp("Haral1001");
		} else if (command.equals("ChooseKeyImageDir")) {
			readKeyImageDir();
			// printImages();
			// sortKeyImage();
		} else if (command.equals("ChooseSaveImageDir")) {
			recordSaveImageDir();
		}
	}

	/**
	 * Let user specify the saved 2D slices atlas, record the save directory.
	 */
	private void recordSaveImageDir() {
		String saveImageName;
		saveImageChooser.setDialogTitle("Open Saved Images Directory");
		saveImageChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

		final int returnValue = saveImageChooser.showOpenDialog(UI
				.getMainFrame());

		if (returnValue == JFileChooser.APPROVE_OPTION) {
			saveImageName = saveImageChooser.getSelectedFile().getName();

			saveImageDirectory = String.valueOf(saveImageChooser
					.getCurrentDirectory())
					+ File.separatorChar
					+ saveImageName + File.separatorChar;
			textFieldSaveImage.setText(saveImageDirectory);

		} else {
			return;
		}
	}

	/**
	 * Read 3D images atlas directory.
	 */
	private void readKeyImageDir() {
		String keyImageName;
		keyImageChooser.setDialogTitle("Open Key Images Directory");
		keyImageChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

		final int returnValue = keyImageChooser.showOpenDialog(UI.getMainFrame());

		if (returnValue == JFileChooser.APPROVE_OPTION) {
			keyImageName = keyImageChooser.getSelectedFile().getName();

			keyImageDirectory = String.valueOf(keyImageChooser
					.getCurrentDirectory())
					+ File.separatorChar
					+ keyImageName
					+ File.separatorChar;

			textFieldKeyImage.setText(keyImageDirectory);

			File fileDir = new File(keyImageDirectory);
			System.err.println("check = " + keyImageDirectory);
			
			// Ruida 2
			traverse(fileDir);
			// processDirSingleImage(fileDir);
		} else {
			return;
		}

	}

	/**
	 * Debugger for test the image and VOis reading.
	 */
	public void printImages() {
		int len = keyImageVOIVector.size();
		for (int i = 0; i < len; i++) {
			System.err.println(keyImageVector.get(i));
			System.err.println(keyImageVOIVector.get(i));
		}
	}

	/**
	 * Recursively traverse the image directory.
	 * 
	 * @param dir
	 *            image dir.
	 */
	private void traverse(File dir) {
		processDir(dir);

		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse(new File(dir, children[i]));
			}
		}

	}

	private void processDir(File dir) {
		String dirName = dir.toString();
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();
	
		String axisString = "";
		if (axis == Axial) {
			axisString = "ax";
		} else if (axis == Saggital) {
			axisString = "sag";
		} else if (axis == Coronal) {
			axisString = "cor";
		}
		if (dirName.substring(begin, end).startsWith("img")
				&& dirName.substring(begin, end).endsWith(".xml")
				&& dirName.contains(axisString)) {
			keyImageVector.add(dir.toString());
			
		}
	
		if (dirName.substring(begin, end).startsWith("voi")
				&& dirName.substring(begin, end).endsWith(".xml")
				&& dirName.contains(axisString)) {
			keyImageVOIVector.add(dir.toString());
		}
		
		
	}

	
	/**
	 * This method is required if the AlgorithmPerformed interface is
	 * implemented. It is called by the algorithms when it has completed or
	 * failed to to complete, so that the dialog can be display the result image
	 * and/or clean up.
	 * 
	 * @param algorithm
	 *            Algorithm that caused the event.
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {

	}

	/**
	 * Driver function to read image and VOIs, and convert each 3D image to 2D
	 * slices.
	 */
	public void callAlgorithm() {
		long startTime = System.currentTimeMillis();

		loadFiles();

		
		disposeLocal();

		setVisible(false);
		
		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();

	}


	/**
	 * Save the 2D slices and VOIs to user specified dir.
	 */
	public void saveImages() {

		for (int i = 0; i < cropKeyImages.size(); i++) {
			try {
				ModelImage cropKeyImage = cropKeyImages.get(i);

				int xDim = cropKeyImage.getExtents()[0];
				int yDim = cropKeyImage.getExtents()[1];
				int zDim = cropKeyImage.getExtents()[2];
				int size = xDim * yDim;

				int[] newExtents = new int[2];
				newExtents[0] = xDim;
				newExtents[1] = yDim;

				VOIVector targetImageVOI = cropKeyImage.getVOIs();

				// Vector<ModelImage> ceImageVector = new Vector<ModelImage>();
				for (int j = 3; j <= 20; j++) {

					try {

						System.err.println(" image number = " + (0 + i)
								+ "   slice number = " + j);

						ModelImage targetImageSlice = new ModelImage(
								ModelStorageBase.FLOAT, newExtents, "target"
										+ j);
						float[] targetBuffer = new float[size];
						cropKeyImage.exportData(j * size, size, targetBuffer);
						targetImageSlice.importData(0, targetBuffer, true);

						// find the intersection of the lower bound with the
						// VOI.
						Vector<VOIBase>[] vArray = targetImageVOI.VOIAt(0)
								.getSortedCurves(VOIBase.ZPLANE, zDim);

						if (vArray[j].size() > 0) {
							VOIBase v = vArray[j].get(0);
							VOIBase vTemp = (VOIBase) v.clone();
							int nPts = vTemp.size();

							// zero out the z dimension VOI
							float[] xPts = new float[nPts];
							float[] yPts = new float[nPts];
							float[] zPts = new float[nPts];
							float[] zPtsZero = new float[nPts];

							vTemp.exportArrays(xPts, yPts, zPts);

							// rotate to the starting point
							// rotateToStartingPoint_yMid(xPts, yPts,
							// newExtents[0], newExtents[1]);
							rotateToStartingPoint_yMid(xPts, yPts,
									newExtents[0], newExtents[1]);
							vTemp.importArrays(xPts, yPts, zPtsZero, nPts);

							// VOIVector voiVectorNew = new VOIVector();
							VOI voiNew = new VOI((short) 0, "blank");
							voiNew.importCurve(vTemp);
							// voiVectorNew.add(voiNew);
							vTemp = null;
							xPts = null;
							zPts = null;
							zPtsZero = null;

							// convert the one contour to two contours
							targetImageSlice.registerVOI(voiNew);
							smoothVOI60(targetImageSlice, targetImageSlice);
							voiNew = targetImageSlice.getVOIs().elementAt(0);

							VOIBaseVector curves = voiNew.getCurves();
							VOIBase srcContour = null;
							for (int index = 0; index < curves.size(); index++) {
								srcContour = curves.elementAt(index);
								generateBoundaryContours(srcContour,
										targetImageSlice);

							}

							// new ViewJFrameImage(targetImageSlice);
							voiNew = targetImageSlice.getVOIs().elementAt(0);
							// 1) save image
							String sliceDir = saveImageDirectory + "slice" + j
									+ File.separator;
							File dir = new File(sliceDir);
							if (!dir.isDirectory()) {
								dir.mkdir();
							}
							String imgName = "image" + (0 + i) + ".xml";
							// String imageFileToSave = sliceDir +
							// File.separator + imgName;
							// targetImageSlice.saveImage(directory, fileName,
							// fileType, isActive, bDisplayProgress)
							targetImageSlice.saveImage(sliceDir, imgName,
									FileUtility.XML, false);
							// 2) save VOI
							FileVOI fileVOI = new FileVOI("voi" + (0 + i)
									+ ".xml", sliceDir, targetImageSlice);
							fileVOI.writeVOI(voiNew, true);

						}
					} catch (IOException e) {

					}
				}

				// cropKeyImagesCE.add(ceImageVector);
				// new ViewJFrameImage(coherenceEnhancingDiffusionImage);

			} catch (OutOfMemoryError x) {
				MipavUtil
						.displayError("Dialog GaborFilter: unable to allocate enough memory");
			}
		}
	}

	/**
	 * Re-oriented the starting and ending VOIs.
	 * 
	 * @param xPts
	 *            voi x coordinate
	 * @param yPts
	 *            voi y coordinate
	 * @param xDim
	 *            image x dimension
	 * @param yDim
	 *            image y dimension
	 */
	private void rotateToStartingPoint_yMid(float xPts[], float yPts[],
			int xDim, int yDim) {
		float yMid = yDim / 2f;
		float xMid = xDim / 2f;
		// 1. find the starting point
		// min point distance to (0, yMid) is the ideal starting point
		int startIndex = 0;
		int len = yPts.length;
		float yDist = -1;
		float xDist = -1;
		float minYDist = 1000;
		for (int i = 0; i < len; i++) {
			yDist = (float) Math.abs(yPts[i] - yMid);
			xDist = (float) Math.abs(xPts[i] - 0);
			if (yDist < minYDist && xDist < xMid) {
				minYDist = yDist;
				startIndex = i;
			}
		}
		// 2. rotate the x, y arrary according to the starting point
		float[] x = new float[len];
		float[] y = new float[len];
		int index = 0;
		for (int i = startIndex; i < len; i++) {
			x[index] = xPts[i];
			y[index] = yPts[i];
			index++;
		}
		for (int i = 0; i < startIndex; i++) {
			x[index] = xPts[i];
			y[index] = yPts[i];
			index++;
		}
		// copy back
		for (int i = 0; i < len; i++) {
			xPts[i] = x[i];
			yPts[i] = y[i];
		}

	}
	
	public void smoothVOI30(ModelImage maskImage, ModelImage resultImage) {

		VOIVector v = maskImage.getVOIs();
		if (v.size() == 0)
			return;
		v.VOIAt(0).setActive(true);
		v.VOIAt(0).setAllActive(true);

		try {
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage, v.VOIAt(0), 30, false);
			smoothAlgo.run();

			VOIVector resultVOIs = resultImage.getVOIs();
			VOI resultVOI = smoothAlgo.getResultVOI();
			resultVOIs.VOIAt(0).removeCurves();
			resultVOIs.VOIAt(0).setCurves(resultVOI.getCurves());
			resultImage.notifyImageDisplayListeners(null, true);
			// new ViewJFrameImage(resultImage);
			smoothAlgo.setCompleted(true);
			smoothAlgo.finalize();
			smoothAlgo = null;
		} catch (OutOfMemoryError x) {
			MipavUtil
					.displayError("Dialog Smooth: unable to allocate enough memory");

			return;
		}
	}

	
	/**
	 * Smooth VOIs to 60 points.
	 * 
	 * @param maskImage
	 * @param resultImage
	 */
	public void smoothVOI60(ModelImage maskImage, ModelImage resultImage) {

		VOIVector v = maskImage.getVOIs();
		if (v.size() == 0)
			return;
		v.VOIAt(0).setActive(true);
		v.VOIAt(0).setAllActive(true);

		// new ViewJFrameImage(maskImage);
		try {
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage,
					v.VOIAt(0), 60, false);
			
			smoothAlgo.run();

			VOIVector resultVOIs = resultImage.getVOIs();
			VOI resultVOI = smoothAlgo.getResultVOI();
			resultVOIs.VOIAt(0).removeCurves();
			resultVOIs.VOIAt(0).setCurves(resultVOI.getCurves());
			resultImage.notifyImageDisplayListeners(null, true);
			// new ViewJFrameImage(resultImage);
			smoothAlgo.setCompleted(true);
			smoothAlgo.finalize();
			smoothAlgo = null;
		} catch (OutOfMemoryError x) {
			MipavUtil
					.displayError("Dialog Smooth: unable to allocate enough memory");

			return;
		}
	}

	/**
	 * Convert VOI from one contour to two contours
	 * 
	 * @param srcContour
	 *            single VOI contour
	 * @param targetImageSlice
	 *            target 2D image slice
	 */
	private void generateBoundaryContours(VOIBase srcContour,
			ModelImage targetImageSlice) {
		int sidePointsForTangent = 1;
		int innerDistance = 4;
		int outerDistance = 4;
		boolean doInner = true;
		boolean doOuter = true;
		int index;
		int i;
		int j;
		int k;
		int m;
		if (srcContour == null || (srcContour.size() == 0)) {
			return;
		}
		int slice = (int) srcContour.elementAt(0).Z;

		int nPoints = srcContour.size();
		Vector3f point;
		float xPoints[] = new float[nPoints + 2 * sidePointsForTangent];
		float yPoints[] = new float[nPoints + 2 * sidePointsForTangent];
		Vector<Vector3f> innerV = new Vector<Vector3f>();
		Vector<Vector3f> outerV = new Vector<Vector3f>();
		float tangentX;
		float tangentY;
		float xCenteredPoints[] = new float[2 * sidePointsForTangent + 1];
		float yCenteredPoints[] = new float[2 * sidePointsForTangent + 1];
		double xSqSum;
		double ySqSum;
		double xySum;
		double var;
		double x1t;
		double x2t;
		double y1t;
		double y2t;
		double slope;
		double d1;
		double d2;
		double xDist;
		double yDist;
		for (i = 0; i < nPoints; i++) {
			point = (srcContour.get(i));
			xPoints[i + sidePointsForTangent] = point.X;
			yPoints[i + sidePointsForTangent] = point.Y;
		}
		for (i = sidePointsForTangent - 1, j = 0; i >= 0; i--, j++) {
			xPoints[i] = xPoints[nPoints - 1 - j];
			yPoints[i] = yPoints[nPoints - 1 - j];
		}
		for (i = nPoints, j = 0; i <= nPoints + sidePointsForTangent - 1; i++, j++) {
			xPoints[i] = xPoints[j];
			yPoints[i] = yPoints[j];
		}
		for (i = sidePointsForTangent, j = 0; i <= sidePointsForTangent
				+ nPoints - 1; i++, j++) {
			if (sidePointsForTangent == 1) {
				tangentX = (xPoints[i + 1] - xPoints[i - 1]) / 2.0f;
				tangentY = (yPoints[i + 1] - yPoints[i - 1]) / 2.0f;
				if (tangentY == 0.0f) {
					slope = Double.POSITIVE_INFINITY;
				} else {
					slope = -tangentX / tangentY;
				}
			} // if (sidePointsForTangent == 1)
			else { // sidePointsForTangent > 1
					// Center all points for tangent point touching curve at (0,
					// 0)
					// That is, use an x axis and a y axis going thru the
					// tangent point
				for (k = 0, m = i - sidePointsForTangent; m <= i
						+ sidePointsForTangent; m++, k++) {
					xCenteredPoints[k] = xPoints[m] - xPoints[i];
					yCenteredPoints[k] = yPoints[m] - yPoints[i];
				}
				xSqSum = 0.0;
				ySqSum = 0.0;
				xySum = 0.0;
				for (k = 0; k < xCenteredPoints.length; k++) {
					xSqSum += xCenteredPoints[k] * xCenteredPoints[k];
					ySqSum += yCenteredPoints[k] * yCenteredPoints[k];
					xySum += xCenteredPoints[k] * yCenteredPoints[k];
				}
				if (xySum != 0.0) {
					var = Math.sqrt(ySqSum * ySqSum - 2.0 * xSqSum * ySqSum
							+ xSqSum * xSqSum + 4.0 * xySum * xySum);
					x1t = 0.5 * ((-ySqSum + xSqSum + var) / xySum);
					x2t = 0.5 * ((-ySqSum + xSqSum - var) / xySum);
					y1t = 1.0;
					y2t = 1.0;
				} else {
					// If all points are symmetric to either this new x axis or
					// this new y axis, then
					// their product sum is 0 and the tangentX, tangentY must be
					// 1,0 or 0,1
					x1t = 1.0;
					x2t = 0.0;
					y1t = 0.0;
					y2t = 1.0;
				}
				// x1t, y1t and x2t, y2t are perpindicular. To find the
				// solution, calculate the sum of
				// distances from the curve points to the line for the 2 cases
				// The shortest distance is the correct solution
				// Distance from AX + BY + C = 0 to P1 is
				// abs((A*x1 + B*y1 + C))/sqrt(A**2 + B**2)
				// Here A = slope, B = -1, and C = 0.
				d1 = 0.0;
				for (k = 0; k < xCenteredPoints.length; k++) {
					if (x1t == 0.0) {
						// Infinite slope thru (0,0)
						d1 += Math.abs(yCenteredPoints[k]);
					} else if (y1t == 0.0) {
						// Zero slope thru (0, 0)
						d1 += Math.abs(xCenteredPoints[k]);
					} else {
						slope = y1t / x1t;
						d1 += Math
								.abs((slope * xCenteredPoints[k] - yCenteredPoints[k])
										/ Math.sqrt(slope * slope + 1));
					}
				}
				d2 = 0.0;
				for (k = 0; k < xCenteredPoints.length; k++) {
					if (x2t == 0.0) {
						// Infinite slope thru (0,0)
						d2 += Math.abs(yCenteredPoints[k]);
					} else if (y2t == 0.0) {
						// Zero slope thru (0, 0)
						d2 += Math.abs(xCenteredPoints[k]);
					} else {
						slope = y2t / x2t;
						d2 += Math
								.abs((slope * xCenteredPoints[k] - yCenteredPoints[k])
										/ Math.sqrt(slope * slope + 1));
					}
				}
				if (d1 < d2) {
					tangentX = (float) x1t;
					tangentY = (float) y1t;
				} else {
					tangentX = (float) x2t;
					tangentY = (float) y2t;
				}
				if (tangentY == 0.0f) {
					slope = Double.POSITIVE_INFINITY;

				} else {
					slope = -tangentX / tangentY;
				}
			} // else sidePointsForTangent > 1
			if (doInner) {
				if (Double.isInfinite(slope)) {
					if (srcContour.contains(xPoints[i], yPoints[i]
							+ innerDistance)) {
						point = new Vector3f(xPoints[i], yPoints[i]
								+ innerDistance, slice);
						innerV.add(point);
					} else if (srcContour.contains(xPoints[i], yPoints[i]
							- innerDistance)) {
						point = new Vector3f(xPoints[i], yPoints[i]
								- innerDistance, slice);
						innerV.add(point);
					}
				} // if (Double.isInfinite(slope))
				else {
					xDist = innerDistance / Math.sqrt(1.0 + slope * slope);
					yDist = xDist * slope;
					if (srcContour.contains((float) (xPoints[i] + xDist),
							(float) (yPoints[i] + yDist))) {
						point = new Vector3f((float) (xPoints[i] + xDist),
								(float) (yPoints[i] + yDist), slice);
						innerV.add(point);
					} else if (srcContour.contains(
							(float) (xPoints[i] - xDist),
							(float) (yPoints[i] - yDist))) {
						point = new Vector3f((float) (xPoints[i] - xDist),
								(float) (yPoints[i] - yDist), slice);
						innerV.add(point);
					}
				}
			} // if (doInner)
			if (doOuter) {
				if (Double.isInfinite(slope)) {
					if (!srcContour.contains(xPoints[i], yPoints[i]
							+ outerDistance)) {
						point = new Vector3f(xPoints[i], yPoints[i]
								+ outerDistance, slice);
						outerV.add(point);
					} else if (!srcContour.contains(xPoints[i], yPoints[i]
							- outerDistance)) {
						point = new Vector3f(xPoints[i], yPoints[i]
								- outerDistance, slice);
						outerV.add(point);
					}
				} // if (Double.isInfinite(slope))
				else {
					xDist = outerDistance / Math.sqrt(1.0 + slope * slope);
					yDist = xDist * slope;
					if (!srcContour.contains((float) (xPoints[i] + xDist),
							(float) (yPoints[i] + yDist))) {
						point = new Vector3f((float) (xPoints[i] + xDist),
								(float) (yPoints[i] + yDist), slice);
						outerV.add(point);
					} else if (!srcContour.contains(
							(float) (xPoints[i] - xDist),
							(float) (yPoints[i] - yDist))) {
						point = new Vector3f((float) (xPoints[i] - xDist),
								(float) (yPoints[i] - yDist), slice);
						outerV.add(point);
					}
				}
			} // if (doOuter)
		} // for (i = sidePointsForTangent, j = 0; i <= sidePointsForTangent +
			// nPoints - 1; i++, j++)

		short sID = (short) (targetImageSlice.getVOIs().getUniqueID());
		String kName = srcContour.getClass().getName();
		index = kName.lastIndexOf('.') + 1;
		kName = kName.substring(index);
		VOI resultVOI = new VOI(sID, kName + "_" + sID, srcContour.getType(),
				-1);

		if (doInner) {
			Vector3f pt[] = new Vector3f[innerV.size()];
			for (i = 0; i < innerV.size(); i++) {
				pt[i] = innerV.elementAt(i);
			}
			resultVOI.importCurve(pt);
		}
		if (doOuter) {
			Vector3f pt[] = new Vector3f[outerV.size()];
			for (i = 0; i < outerV.size(); i++) {
				pt[i] = outerV.elementAt(i);
			}
			resultVOI.importCurve(pt);
		}

		targetImageSlice.getVOIs().removeElementAt(0);

		targetImageSlice.registerVOI(resultVOI);

	}

	/**
	 * Crop key images.
	 */
	public void cropKeyImages() {

		int zDim;

		// Crop key images. VOIs
		for (int i = 0; i < keyImages.size(); i++) {

			ModelImage image = keyImages.get(i);

			int[] extents = (int[]) image.getFileInfo(0).getExtents();
			zDim = extents[2] - 1;

			// manually set the crop image starting point and ending point
			boxYmin = 0;
			boxYmax = 512 - 1;

			boxXmin = 0;
			boxXmax = 512 - 1;

			xBounds[0] = boxXmin;
			xBounds[1] = boxXmax;

			yBounds[0] = boxYmin;
			yBounds[1] = boxYmax;

			zBounds[0] = 0;
			zBounds[1] = zDim;

			int borderSize = 0;
			try {
				int[] destExtents = null;
				if (image.getNDims() == 3) {

					if (Math.abs(zBounds[1] - zBounds[0]) == 0) { // crop 3D
																	// image
						// to 2D image
						destExtents = new int[2];
						destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1
								+ (2 * borderSize);
						destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1
								+ (2 * borderSize);
					} else {
						destExtents = new int[3];
						destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1
								+ (2 * borderSize);
						destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1
								+ (2 * borderSize);
						destExtents[2] = Math.abs(zBounds[1] - zBounds[0] + 1);
					}
				} else {
					return;
				}

				System.err.println("destExtents[0] = " + destExtents[0]
						+ "  destExtents[1] = " + destExtents[1]);

				// create crop images
				cropKeyImages.add(i,
						new ModelImage(image.getType(), destExtents,
								makeImageName(image.getImageName(), "_crop")));

				int[] xCrop = new int[] { 0, 0 };
				int[] yCrop = new int[] { 0, 0 };
				int[] zCrop = new int[] { 0, 0 };
				if (destExtents.length > 0) {
					xCrop[0] = -1 * (xBounds[0] - borderSize);
					xCrop[1] = -1 * (xBounds[1] - destExtents[0] - 1);
				}
				if (destExtents.length > 1) {
					yCrop[0] = -1 * (yBounds[0] - borderSize);
					yCrop[1] = -1 * (yBounds[1] - destExtents[1] - 1);
				}
				if (destExtents.length > 2) {
					zCrop[0] = -1 * (zBounds[0]);
					zCrop[1] = -1 * (zBounds[1] - destExtents[2] - 1);
				} else // 3D to 2D
				{
					zCrop[0] = -1 * (zBounds[0]);
					zCrop[1] = -1 * (zBounds[1] + 1);
				}

				System.err.println("xCrop[0] = " + xCrop[0] + "   xCrop[1] = "
						+ xCrop[1]);
				System.err.println("yCrop[0] = " + yCrop[0] + "   yCrop[1] = "
						+ yCrop[1]);

				cropAlgo = new AlgorithmAddMargins(image, cropKeyImages.get(i),
						xCrop, yCrop, zCrop);


				// Hide the dialog since the algorithm is about to run.
				setVisible(false);

				cropAlgo.run();

			} catch (OutOfMemoryError e) {
				MipavUtil
						.displayError("Dialog Crop: unable to allocate enough memory");

				return;
			}
		} // end for loop

		// crop target image

	}

	/**
	 * load image files and voi files
	 */
	public void loadFiles() {
		readFiles();
		System.err.println("finish image I/O");

	}

	/**
	 * Debugger to test dir deletion.
	 * 
	 * @param dir
	 * @return
	 */
	public static boolean deleteDir(File dir) {
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				boolean success = deleteDir(new File(dir, children[i]));
				if (!success) {
					return false;
				}
			}
		}
		return dir.delete();
	}

	/**
	 * Read image and VOIs file names.
	 */
	public void readFiles() {

		int index;

		try {
			// read key images and VOIs
			for (int i = 0; i < keyImageVector.size(); i++) {

				// read key image
				String dir = keyImageVector.get(i);
				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1,
						dir.length()));

				System.err.println("Key Image: fileName = " + fileName
						+ "  directory = " + directory);
				FileIO keyImageIO = new FileIO();
				keyImages.add(i, keyImageIO.readImage(fileName, directory));

				// read corresponding VOI
				String voiDir = keyImageVOIVector.get(i);
				System.err.println("voiDir = " + voiDir);
				index = voiDir.lastIndexOf(File.separator);
				String voiDirectory = new String(voiDir.substring(0, index + 1));
				String voiFileName = new String(voiDir.substring(index + 1,
						voiDir.length()));

				FileVOI fileVOI = null;
				fileVOI = new FileVOI(voiFileName, voiDirectory,
						keyImages.get(i));
				System.err.println("fileDirectory = " + directory
						+ "fileName = " + fileName);
				System.err.println("voiDirectory = " + voiDirectory
						+ "voiFileName = " + voiFileName);
				keyImageVOIs.add(i, fileVOI.readVOI(false));

				keyImages.get(i).registerVOI(keyImageVOIs.get(i)[0]);

			}

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Initial panel
	 */
	public void init() {

		JPanel mainPanel;

		mainPanel = new JPanel();
		mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
		mainPanel.setLayout(new BorderLayout());

		buildKeyImagePanel();

		mainPanel.add(imageSelectionPanel, BorderLayout.CENTER);
		mainPanel.add(buildButtons(), BorderLayout.SOUTH);

		getContentPane().add(mainPanel);
		pack();
		setResizable(true);
	}

	/**
	 * Panel contains both the 3D image dir and saved 2D slices atlas dir.
	 */
	public void buildKeyImagePanel() {

		final GridBagConstraints gbc = new GridBagConstraints();

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.fill = GridBagConstraints.NONE;
		gbc.anchor = GridBagConstraints.EAST;

		imageSelectionPanel = new JPanel();
		imageSelectionPanel.setLayout(new GridLayout(3, 3));
		imageSelectionPanel.setBorder(buildTitledBorder("Auto Train"));

		// axis label
		String[] axisStrings = { "Axial", "Saggital", "Coronal" };

		axisList = new JComboBox(axisStrings);
		axisList.setSelectedIndex(0);
		axisList.setActionCommand("SetAxis");
		axisList.addActionListener(this);

		labelAxis = new JLabel("Axis: ");
		labelAxis.setFont(serif12);
		labelAxis.setForeground(Color.black);

		imageSelectionPanel.add(labelAxis, gbc);

		gbc.gridx = 1;
		imageSelectionPanel.add(axisList, gbc);

		gbc.gridx = 2;
		JLabel emptyLabel = new JLabel("");
		imageSelectionPanel.add(emptyLabel, gbc);

		// Key image directory
		gbc.gridx = 0;
		gbc.gridy = 1;
		labelKeyImage = new JLabel("Key Image Directory: ");
		labelKeyImage.setFont(serif12);
		labelKeyImage.setForeground(Color.black);

		imageSelectionPanel.add(labelKeyImage, gbc);

		textFieldKeyImage = new JTextField(20);
		textFieldKeyImage.setFont(serif12);

		gbc.gridx = 1;
		imageSelectionPanel.add(textFieldKeyImage, gbc);

		buttonKeyImage = new JButton("Choose");
		buttonKeyImage.addActionListener(this);
		buttonKeyImage.setActionCommand("ChooseKeyImageDir");
		buttonKeyImage.setFont(serif12B);
		buttonKeyImage.setPreferredSize(MipavUtil.defaultButtonSize);

		gbc.gridx = 2;
		imageSelectionPanel.add(buttonKeyImage, gbc);

		// Save image directory
		gbc.gridx = 0;
		gbc.gridy = 2;
		labelSaveImage = new JLabel("Saved Image Directory: ");
		labelSaveImage.setFont(serif12);
		labelSaveImage.setForeground(Color.black);

		imageSelectionPanel.add(labelSaveImage, gbc);

		textFieldSaveImage = new JTextField(20);
		textFieldSaveImage.setFont(serif12);

		gbc.gridx = 1;
		imageSelectionPanel.add(textFieldSaveImage, gbc);

		buttonSaveImage = new JButton("Choose");
		buttonSaveImage.addActionListener(this);
		buttonSaveImage.setActionCommand("ChooseSaveImageDir");
		buttonSaveImage.setFont(serif12B);
		buttonSaveImage.setPreferredSize(MipavUtil.defaultButtonSize);

		gbc.gridx = 2;
		imageSelectionPanel.add(buttonSaveImage, gbc);

	}

}