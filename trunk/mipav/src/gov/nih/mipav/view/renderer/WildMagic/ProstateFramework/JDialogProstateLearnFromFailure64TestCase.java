package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.filters.*;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.JDialogProstateLearnFromFailure64TrainingCase.ImageAttributes;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import WildMagic.LibFoundation.Mathematics.Vector3f;

/**
 * This class convert the 3D images to 2D slices based atlas. Users specify the
 * 3D prostate images dir, and output 2D slices based dir, the algorithm auto
 * convert each 3D image to 2D slices with corresponding VOIs. Each saved VOIs
 * is 2-contour based VOI, which is used to build the Active Appearance Model
 * (AAM).
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstateLearnFromFailure64TestCase extends JDialogBase implements AlgorithmInterface {

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

	private Vector<String> keyImageVOIVectorAAM = new Vector<String>();

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

	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstateLearnFromFailure64TestCase(Frame theParentFrame) {
		super(theParentFrame, false);
		UI = ViewUserInterface.getReference();
		init();
		setVisible(true);

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
			initTable();
			readKeyImageDir();
			extractTable();
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

		final int returnValue = saveImageChooser.showOpenDialog(UI.getMainFrame());

		if (returnValue == JFileChooser.APPROVE_OPTION) {
			saveImageName = saveImageChooser.getSelectedFile().getName();

			saveImageDirectory = String.valueOf(saveImageChooser.getCurrentDirectory()) + File.separatorChar + saveImageName + File.separatorChar;
			textFieldSaveImage.setText(saveImageDirectory);
			System.err.println("saveImageDirectory = " + saveImageDirectory);

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
			// System.err.println("check = " + keyImageDirectory);
			
			// Ruida 2
			
			// generate deep learning patch from errors.   Learn from failure. 
			traverseCurrent(fileDir);
			// test case patch generation 
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
			System.err.println(keyImageVOIVectorAAM.get(i));
		}
	}

	/**
	 * Recursively traverse the image directory.
	 * 
	 * @param dir
	 *            image dir.
	 */
	private void traverse(File dir) {
		// processDir(dir);
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse(new File(dir, children[i]));
			}
		}

	}

	private void traverseCurrent(File dir) {
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverseLevel1(new File(dir, children[i]), i);
			}
		}
		System.err.println("end travese current");

	}
	

	private void traverseLevel1(File dir, int index) {
		if (dir.isDirectory()) {
			String[] children = dir.list();
			// System.err.println("dir name = " + dir.toString());
		
			// String dirName = dir.toString();
			for (int i = 0; i < children.length; i++) {
				  processDirLevel1(new File(dir, children[i]), index);
				  // pause();
			}
		
		}
	}
	
	private void processDirLevel1(File dir, int i) {

		String dirName = dir.toString();
		// System.err.println("dirName = " + dirName);
	
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();
		String subString = dirName.substring(begin, end); 
		int indexOfDot = subString.lastIndexOf(".");
		// System.err.println("num = " + num);
		String imageName = null, voiName = null, aamName = null;
		Hashtable<Integer, ImageAttributes> table = null; 
		
		try {

			if (subString.startsWith("image") && subString.endsWith(".xml")) {
				// keyImageVector.add(dir.toString());
				String numString = subString.substring(5, indexOfDot);
				int num = new Integer(numString).intValue();
				imageName = dirName.toString();
				// System.err.println("imageName = " + imageName);
				// table.get(num).imageName = dirName.toString();
				if ( i == 0 ) {
					table1.get(num).imageName = imageName;
				} else if ( i == 1 ) {
					table2.get(num).imageName = imageName;
				} else if ( i == 2 ) {
					table3.get(num).imageName = imageName;
				} else if ( i == 3 ) {
					table4.get(num).imageName = imageName;
				} else if ( i == 4 ) {
					table5.get(num).imageName = imageName;
				}

			}

			if (subString.startsWith("test_voi") && subString.endsWith(".xml")) {
				// keyImageVOIVector.add(dir.toString());
				String numString = subString.substring(8, indexOfDot);
				int num = new Integer(numString).intValue();
				imageName = dirName.toString();
				voiName = dirName.toString();
				// System.err.println("voiName = " + voiName);
				// table.get(num).voiName = dirName.toString();
				if ( i == 0 ) {
					table1.get(num).voiName = voiName;
				} else if ( i == 1 ) {
					table2.get(num).voiName = voiName;
				} else if ( i == 2 ) {
					table3.get(num).voiName = voiName;
				} else if ( i == 3 ) {
					table4.get(num).voiName = voiName;
				} else if ( i == 4 ) {
					table5.get(num).voiName = voiName;
				}
			}

		} catch ( Exception e ) {
			e.printStackTrace();
			System.exit(1);
		}
		// System.err.println("globalCounter = " + globalCounter);
		// table.put(num, new ImageAttributes(imageName, voiName, aamName));
		
	}


	class ImageAttributes {
		String imageName;
		String voiName;
		String AAMName;

		public ImageAttributes(String _imageName, String _voiName, String _AAMName) {
			imageName = _imageName;
			voiName = _voiName;
			AAMName = _AAMName;
		}
	}

	Hashtable<Integer, ImageAttributes> table1 = new Hashtable<Integer, ImageAttributes>();
	Hashtable<Integer, ImageAttributes> table2 = new Hashtable<Integer, ImageAttributes>();
	Hashtable<Integer, ImageAttributes> table3 = new Hashtable<Integer, ImageAttributes>();
	Hashtable<Integer, ImageAttributes> table4 = new Hashtable<Integer, ImageAttributes>();
	Hashtable<Integer, ImageAttributes> table5 = new Hashtable<Integer, ImageAttributes>();

	private void initTable() {
		for ( int i = 0; i < 50; i++ ) {
			table1.put(i, new ImageAttributes(null, null, null));
			table2.put(i, new ImageAttributes(null, null, null));
			table3.put(i, new ImageAttributes(null, null, null));
			table4.put(i, new ImageAttributes(null, null, null));
			table5.put(i, new ImageAttributes(null, null, null));
		}
	}


	void extractTable() {
		for ( int i = 0; i < 50; i++ ) {
			ImageAttributes attri = table5.get(i);
			if ( attri.imageName != null ) {
				keyImageVector.add(attri.imageName);
				keyImageVOIVector.add(attri.voiName);
			}
		}
	}

	/**
	 * Process the dir, read image and corresponding VOI file names.
	 * 
	 * @param dir
	 *            3D atlas image dir.
	 */
	private void processDirSingleImage(File dir) {

		String[] children = dir.list();
		String directoryName = dir.toString();
		String dirName;
		for (int i = 0; i < children.length; i++) {
			dirName = directoryName + File.separator + children[i];

			System.err.println("dirName = " + dirName);

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
			if (dirName.substring(begin, end).startsWith("img") && dirName.substring(begin, end).endsWith(".xml") && dirName.contains(axisString)) {
				keyImageVector.add(dirName);
			}

			// if (dirName.substring(begin, end).startsWith("voi") && dirName.substring(begin, end).endsWith(".xml") && dirName.contains(axisString)) {
			// 	keyImageVOIVector.add(dirName);
			// }
			
			if (dirName.substring(begin, end).startsWith("08_14") && dirName.substring(begin, end).endsWith(".xml")) {
				keyImageVOIVector.add(dirName);
			}
			

		}

		System.err.println("test image name: " + keyImageVector.get(0));
		System.err.println("test voi name: " + keyImageVOIVector.get(0));
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

		if (algorithm instanceof AlgorithmAddMargins) {

			if ((cropAlgo.isCompleted() == true) && (cropImage != null)) {
				/*
				 * try { new ViewJFrameImage(cropImage, null, new Dimension(610,
				 * 200)); } catch (OutOfMemoryError error) { MipavUtil
				 * .displayError("Out of memory: unable to open new frame"); }
				 */
				// cropAlgo.finalize();
				// cropAlgo = null;

			}
		}

		dispose();
		System.gc();
	}

	/**
	 * Driver function to read image and VOIs, and convert each 3D image to 2D
	 * slices.
	 */
	public void callAlgorithm() {
		long startTime = System.currentTimeMillis();

		loadFiles();

		// Ruida 1.
		// createTrainingPatches();

		createTestingPatches();

		// System.err.println("saveImage");

		// disposeLocal();

		setVisible(false);

		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();

	}

	/*
	 * public void generatePatches() { for (int i = 0; i <= keyImages.size();
	 * i++) { try { // single threading ModelImage keyImage = keyImages.get(0);
	 * for ( int k = 3; k <= 20; k++ ) { run(keyImage, i, k, k, true); } } catch
	 * (OutOfMemoryError x) {
	 * MipavUtil.displayError("Dialog GaborFilter: unable to allocate enough memory"
	 * ); } } System.err.println("done finish run"); }
	 */

	public void createTestingPatches() {

		// String sliceDir = "C:\\TestMorePatches\\22" + File.separator;
		String coodindateFileName = "patchCoordinate.txt";

		try {

			

			for (int i = 0; i < keyImages.size(); i++) {
				try {
					
					ModelImage keyImage = keyImages.get(i);
					
					String imageName = keyImage.getImageFileName(); 
					System.err.println("test imageName = " + imageName);
					int begin = imageName.lastIndexOf(File.separator) + 1;
					int end = imageName.length();
					String subString = imageName.substring(begin, end); 
					int indexOfDot = subString.lastIndexOf(".");
					String numString = subString.substring(5, indexOfDot);
					int num = new Integer(numString).intValue();
					
					String currentSaveDir = saveImageDirectory + File.separator + num + File.separator;
					
					File dir = new File(currentSaveDir);
					if (!dir.isDirectory()) {
						dir.mkdir();
					}
					File file = new File(currentSaveDir + coodindateFileName);
					System.err.println("test = " + file.toString());
					if (!file.exists())
						file.createNewFile();
					BufferedWriter outStream = new BufferedWriter(new FileWriter(file.getAbsoluteFile()));
					
					// single thread processing !!!!!!!!!!!!!!!!!!
					

					int[] extents = keyImage.getExtents();
					int size = extents[0] * extents[1] * extents[2];

					outStream.write(extents[2] + "\n");

					float[] imageBuffer = new float[size];

					try {
						keyImage.exportData(0, imageBuffer.length, imageBuffer);
					} catch (final Exception e) {
						e.printStackTrace();
					}

					// set 10% to enhanced the contrast
					float percentile = 0.10f;
					int minIndex = (int) (size * percentile);
					int maxIndex = (int) (size * (1.0f - percentile));

					Arrays.sort(imageBuffer);

					float min = imageBuffer[minIndex];
					float max = imageBuffer[maxIndex];

					// for (int k = 12; k < 13; k++) {
					for (int k = 3; k <= 20; k++) {
						testingPatches(keyImage, i, k, k, min, max, false, outStream, currentSaveDir);
					}

					imageBuffer = null;
					extents = null;

					// ****************** processing end **********************************
					outStream.close();
					
				} catch (OutOfMemoryError x) {
					MipavUtil.displayError("Dialog GaborFilter: unable to allocate enough memory");
				}
			}
			

		} catch (IOException e) {
			e.printStackTrace();
		}
		System.err.println("done finish test case patches generation");
	}

	private void scaleDown(ModelImage image) {

		double min = image.getMin();
		double max = image.getMax();
		float value;

		int[] extents = image.getExtents();
		int size = extents[0] * extents[1] * extents[2];
		float[] imageBuffer = new float[size];
		float[] clonedBuffer = new float[size];

		try {
			image.exportData(0, imageBuffer.length, imageBuffer);
		} catch (final Exception e) {
			e.printStackTrace();
		}

		float percentile = 0.10f;
		clonedBuffer = imageBuffer.clone();
		int minIndex = (int) (size * percentile);
		int maxIndex = (int) (size * (1.0f - percentile));

		Arrays.sort(clonedBuffer);

		min = clonedBuffer[minIndex];
		max = clonedBuffer[maxIndex];

		for (int i = 0; i < size; i++) {
			value = imageBuffer[i];
			if (value >= min && value <= max) {
				value = (float) ((value - min) * 255d / (max - min));
			} else if (value > max)
				value = 255;
			else if (value < min)
				value = 0;
			imageBuffer[i] = value;
		}

		try {
			image.importData(0, imageBuffer, true);
			new ViewJFrameImage(image);
		} catch (IOException e) {
			e.printStackTrace();
		}
		System.err.println("finish scale down");

	}

	public void createTrainingPatches() {

		// trick
		for (int i = 0; i < keyImages.size(); i++) {
			// for (int i = 35; i < 36; i++) {
			try {
				// single threading
				ModelImage keyImage = keyImages.get(i);

				// scaleDown((ModelImage)keyImage.clone());

				// new ViewJFrameImage(keyImage);

				int[] extents = keyImage.getExtents();
				int size = extents[0] * extents[1] * extents[2];
				float[] imageBuffer = new float[size];

				try {
					keyImage.exportData(0, imageBuffer.length, imageBuffer);
				} catch (final Exception e) {
					e.printStackTrace();
				}

				// set 10% to enhance the contrast
				float percentile_left = 0.10f;
				float percentile_right = 0.10f;
				int minIndex = (int) (size * percentile_left);
				int maxIndex = (int) (size * (1.0f - percentile_right));

				Arrays.sort(imageBuffer);

				float min = imageBuffer[minIndex];
				float max = imageBuffer[maxIndex];

				for (int k = 3; k <= 20; k++) {
					// int k = 8;
					trainingPatches(keyImage, i, k, k, min, max, true);
				}

				imageBuffer = null;
				extents = null;

				/*
				 * int numberCore = (Runtime.getRuntime().availableProcessors()
				 * - 2) > 1 ? Runtime.getRuntime().availableProcessors() - 2 :
				 * 1; System.err.println("numCore = " + numberCore);
				 * 
				 * ModelImage keyImage = keyImages.get(0);
				 * 
				 * ExecutorService exec =
				 * Executors.newFixedThreadPool(numberCore); // 3 to 20
				 * exec.execute(createTask((ModelImage)keyImage.clone(), 5, 3,
				 * 5)); exec.execute(createTask((ModelImage)keyImage.clone(), 5,
				 * 6, 8)); exec.execute(createTask((ModelImage)keyImage.clone(),
				 * 5, 9, 11));
				 * exec.execute(createTask((ModelImage)keyImage.clone(), 5, 12,
				 * 14)); exec.execute(createTask((ModelImage)keyImage.clone(),
				 * 5, 15, 17));
				 * exec.execute(createTask((ModelImage)keyImage.clone(), 5, 18,
				 * 20));
				 * 
				 * exec.shutdown();
				 * 
				 * // System.err.println("shut down"); // setup the upper limit
				 * waiting time. // try { // exec.awaitTermination(30,
				 * TimeUnit.MINUTES); // } catch (InterruptedException e) { //
				 * MipavUtil.displayError("Program did not execute correctly");
				 * // e.printStackTrace(); // }
				 * 
				 * while ( !exec.isTerminated() ) {};
				 * 
				 * System.err.println("Finished all thread");
				 */

			} catch (OutOfMemoryError x) {
				MipavUtil.displayError("Dialog GaborFilter: unable to allocate enough memory");
			}
		}
		System.err.println("done finish run");

	}

	public void testingPatches(final ModelImage keyImage, final int i, final int startSliceIndex, final int endSliceIndex, float minIntensity,
			float maxIntensity, boolean train, BufferedWriter outStream, String currentSaveDir) {
		int xDim = keyImage.getExtents()[0];
		int yDim = keyImage.getExtents()[1];
		int zDim = keyImage.getExtents()[2];
		int size = xDim * yDim;

		int[] newExtents = new int[2];
		newExtents[0] = xDim;
		newExtents[1] = yDim;

		VOIVector targetImageVOI = keyImage.getVOIs();

		// 3 to 20
		// Vector<ModelImage> ceImageVector = new Vector<ModelImage>();
		// for (int j = 12; j < 13; j++) {
		for (int j = startSliceIndex; j <= endSliceIndex; j++) {

			try {

				System.err.println(" image number = " + (0 + i) + "   slice number = " + j);

				ModelImage targetImageSlice = new ModelImage(keyImage.getDataType(), newExtents, "target" + j);
				float[] targetBuffer = new float[size];
				keyImage.exportData(j * size, size, targetBuffer);
				targetImageSlice.importData(0, targetBuffer, true);

				// new ViewJFrameImage(targetImageSlice);

				// find the intersection of the lower bound with the VOI.
				Vector<VOIBase>[] vArray = targetImageVOI.VOIAt(0).getSortedCurves(VOIBase.ZPLANE, zDim);

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
					rotateToStartingPoint_yMid(xPts, yPts, newExtents[0], newExtents[1]);
					vTemp.importArrays(xPts, yPts, zPtsZero, nPts);

					// VOIVector voiVectorNew = new VOIVector();
					VOI voiNew = new VOI((short) 0, "blank");
					voiNew.importCurve(vTemp);
					voiNew.setColor(Color.yellow);
					// voiVectorNew.add(voiNew);

					// vTemp = null;
					// xPts = null;
					// zPts = null;
					// zPtsZero = null;

					// convert the one contour to two contours
					targetImageSlice.registerVOI(voiNew);
					// smoothVOI128(targetImageSlice, targetImageSlice);

					if ( targetImageSlice.getVOIs().size() < 1 ) return;
					
					AlgorithmLearnFromFailure64 algo = new AlgorithmLearnFromFailure64(targetImageSlice, i, j, minIntensity, maxIntensity, train, outStream,
							currentSaveDir);
					algo.run();

					// algo.disposeLocal();
					// algo = null;

				}

				/*
				 * if ( targetImageSlice != null ) {
				 * targetImageSlice.disposeLocal(); targetImageSlice = null; }
				 * 
				 * if ( targetBuffer != null ) { targetBuffer = null; }
				 */

				// System.gc();

			} catch (Exception e) {
				e.printStackTrace();
			}
		} // end for loop

	}

	public void trainingPatches(final ModelImage keyImage, final int i, final int startSliceIndex, final int endSliceIndex, float minIntensity,
			float maxIntensity, boolean train) {

		int xDim = keyImage.getExtents()[0];
		int yDim = keyImage.getExtents()[1];
		int zDim = keyImage.getExtents()[2];
		int size = xDim * yDim;

		int[] newExtents = new int[2];
		newExtents[0] = xDim;
		newExtents[1] = yDim;

		VOIVector targetImageVOI = keyImage.getVOIs();
		System.err.println("targe vOI size = " + targetImageVOI.size());
		// if the image slice doesn't contain both the GT VOI and AAM VOI, do
		// nothing.
		if (targetImageVOI.size() < 2)
			return;

		// 3 to 20
		// Vector<ModelImage> ceImageVector = new Vector<ModelImage>();
		for (int j = startSliceIndex; j <= endSliceIndex; j++) {

			try {

				System.err.println(" image number = " + (0 + i) + "   slice number = " + j);

				ModelImage targetImageSlice = new ModelImage(keyImage.getDataType(), newExtents, "target" + j);
				float[] targetBuffer = new float[size];
				keyImage.exportData(j * size, size, targetBuffer);
				targetImageSlice.importData(0, targetBuffer, true);

				// new ViewJFrameImage(targetImageSlice);

				// Due to the ground truth VOI vs the Wrong VOI, load two
				// VOI contours into the target image slice.
				for (int k = 0; k < 2; k++) {

					Vector<VOIBase>[] vArray = targetImageVOI.VOIAt(k).getSortedCurves(VOIBase.ZPLANE, zDim);

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
						rotateToStartingPoint_yMid(xPts, yPts, newExtents[0], newExtents[1]);
						vTemp.importArrays(xPts, yPts, zPtsZero, nPts);

						// VOIVector voiVectorNew = new VOIVector();
						VOI voiNew = new VOI((short) 0, "blank");
						voiNew.importCurve(vTemp);
						// voiVectorNew.add(voiNew);

						// vTemp = null;
						// xPts = null;
						// zPts = null;
						// zPtsZero = null;

						// convert the one contour to two contours
						targetImageSlice.registerVOI(voiNew);

					}

				} // end k loop

				if (targetImageSlice.getVOIs().size() < 2)
					return;

				smoothVOI128(targetImageSlice, targetImageSlice);

				AlgorithmLearnFromFailure64 algo = new AlgorithmLearnFromFailure64(targetImageSlice, i, j, minIntensity, maxIntensity, train,
						saveImageDirectory);
				algo.run();

				algo.disposeLocal();
				algo = null;

				/*
				 * if ( targetImageSlice != null ) {
				 * targetImageSlice.disposeLocal(); targetImageSlice = null; }
				 * 
				 * if ( targetBuffer != null ) { targetBuffer = null; }
				 */

				// System.gc();

			} catch (Exception e) {
				e.printStackTrace();
			}
		} // end for loop

	}

	/*
	 * private Runnable createTask(final ModelImage keyImage, final int i, final
	 * int startSliceIndex, final int endSliceIndex) { // 3 to 20 return new
	 * Runnable() { public void run() { int xDim = keyImage.getExtents()[0]; int
	 * yDim = keyImage.getExtents()[1]; int zDim = keyImage.getExtents()[2]; int
	 * size = xDim * yDim;
	 * 
	 * int[] newExtents = new int[2]; newExtents[0] = xDim; newExtents[1] =
	 * yDim;
	 * 
	 * VOIVector targetImageVOI = keyImage.getVOIs();
	 * 
	 * // 3 to 20 // Vector<ModelImage> ceImageVector = new
	 * Vector<ModelImage>(); for (int j = startSliceIndex; j <= endSliceIndex;
	 * j++) {
	 * 
	 * try {
	 * 
	 * 
	 * System.err.println(" image number = " + (0 + i) + "   slice number = " +
	 * j);
	 * 
	 * ModelImage targetImageSlice = new ModelImage( keyImage.getDataType(),
	 * newExtents, "target" + j); float[] targetBuffer = new float[size];
	 * keyImage.exportData(j * size, size, targetBuffer);
	 * targetImageSlice.importData(0, targetBuffer, true);
	 * 
	 * // new ViewJFrameImage(targetImageSlice);
	 * 
	 * // find the intersection of the lower bound with the VOI.
	 * Vector<VOIBase>[] vArray = targetImageVOI.VOIAt(0)
	 * .getSortedCurves(VOIBase.ZPLANE, zDim);
	 * 
	 * if (vArray[j].size() > 0) { VOIBase v = vArray[j].get(0); VOIBase vTemp =
	 * (VOIBase) v.clone(); int nPts = vTemp.size();
	 * 
	 * // zero out the z dimension VOI float[] xPts = new float[nPts]; float[]
	 * yPts = new float[nPts]; float[] zPts = new float[nPts]; float[] zPtsZero
	 * = new float[nPts];
	 * 
	 * vTemp.exportArrays(xPts, yPts, zPts);
	 * 
	 * // rotate to the starting point // rotateToStartingPoint_yMid(xPts, yPts,
	 * // newExtents[0], newExtents[1]); rotateToStartingPoint_yMid(xPts, yPts,
	 * newExtents[0], newExtents[1]); vTemp.importArrays(xPts, yPts, zPtsZero,
	 * nPts);
	 * 
	 * // VOIVector voiVectorNew = new VOIVector(); VOI voiNew = new VOI((short)
	 * 0, "blank"); voiNew.importCurve(vTemp); // voiVectorNew.add(voiNew);
	 * 
	 * // convert the one contour to two contours
	 * targetImageSlice.registerVOI(voiNew); smoothVOI30(targetImageSlice,
	 * targetImageSlice);
	 * 
	 * // System.err.println("out of smooth.");
	 * 
	 * AlgorithmLearnFromFailure algo = new
	 * AlgorithmLearnFromFailure(targetImageSlice, i, j, 0, 0, true,
	 * saveImageDirectory); algo.run();
	 * 
	 * 
	 * algo.disposeLocal(); algo = null;
	 * 
	 * vTemp = null; xPts = null; zPts = null; zPtsZero = null;
	 * 
	 * 
	 * }
	 * 
	 * 
	 * if ( targetImageSlice != null ) { targetImageSlice.disposeLocal();
	 * targetImageSlice = null; }
	 * 
	 * if ( targetBuffer != null ) { targetBuffer = null; }
	 * 
	 * // System.gc();
	 * 
	 * } catch (Exception e) { e.printStackTrace(); } } // end for loop
	 * 
	 * } }; }
	 */

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

						System.err.println(" image number = " + (0 + i) + "   slice number = " + j);

						ModelImage targetImageSlice = new ModelImage(ModelStorageBase.FLOAT, newExtents, "target" + j);
						float[] targetBuffer = new float[size];
						cropKeyImage.exportData(j * size, size, targetBuffer);
						targetImageSlice.importData(0, targetBuffer, true);

						// find the intersection of the lower bound with the
						// VOI.
						Vector<VOIBase>[] vArray = targetImageVOI.VOIAt(0).getSortedCurves(VOIBase.ZPLANE, zDim);

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
							rotateToStartingPoint_yMid(xPts, yPts, newExtents[0], newExtents[1]);
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
								generateBoundaryContours(srcContour, targetImageSlice);

							}

							// new ViewJFrameImage(targetImageSlice);
							voiNew = targetImageSlice.getVOIs().elementAt(0);
							// 1) save image
							String sliceDir = saveImageDirectory + "slice" + j + File.separator;
							File dir = new File(sliceDir);
							if (!dir.isDirectory()) {
								dir.mkdir();
							}
							String imgName = "image" + (0 + i) + ".xml";
							// String imageFileToSave = sliceDir +
							// File.separator + imgName;
							// targetImageSlice.saveImage(directory, fileName,
							// fileType, isActive, bDisplayProgress)
							targetImageSlice.saveImage(sliceDir, imgName, FileUtility.XML, false);
							// 2) save VOI
							FileVOI fileVOI = new FileVOI("voi" + (0 + i) + ".xml", sliceDir, targetImageSlice);
							fileVOI.writeVOI(voiNew, true);

						}
					} catch (IOException e) {

					}
				}

				// cropKeyImagesCE.add(ceImageVector);
				// new ViewJFrameImage(coherenceEnhancingDiffusionImage);

			} catch (OutOfMemoryError x) {
				MipavUtil.displayError("Dialog GaborFilter: unable to allocate enough memory");
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
	private void rotateToStartingPoint_yMid(float xPts[], float yPts[], int xDim, int yDim) {
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

	// debugger to test.
	private void rotateToStartingPoint_leftMost(float xPts[], float yPts[], int xDim, int yDim) {
		// float yMid = yDim / 2f;
		// float xMid = xDim / 2f;
		// 1. find the starting point
		// min point distance to (0, yMid) is the ideal starting point
		int startIndex = 0;
		int len = yPts.length;
		// float yDist = -1;
		float xDist = -1;
		// float minYDist = 1000;
		float minXDist = 1000;
		for (int i = 0; i < len; i++) {
			// yDist = (float)Math.abs(yPts[i] - yMid);
			xDist = (float) Math.abs(xPts[i] - 0);
			if (xDist < minXDist) {
				minXDist = xDist;
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
			MipavUtil.displayError("Dialog Smooth: unable to allocate enough memory");

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
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage, v.VOIAt(0), 60, false);

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
			MipavUtil.displayError("Dialog Smooth: unable to allocate enough memory");

			return;
		}
	}

	public void smoothVOI128(ModelImage maskImage, ModelImage resultImage) {

		VOIVector v = maskImage.getVOIs();
		// System.err.println("check voi size = " + v.size());
		if (v.size() == 0)
			return;
		v.VOIAt(0).setActive(true);
		v.VOIAt(0).setAllActive(true);

		// v.VOIAt(1).setActive(true);
		// v.VOIAt(1).setAllActive(true);

		// new ViewJFrameImage(maskImage);
		try {

			// first VOI ground truth
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage, v.VOIAt(0), 128, false);
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

			/*
			 * // second VOI AAM contour smoothAlgo = new
			 * AlgorithmBSmooth(maskImage, v.VOIAt(1), 128, false);
			 * smoothAlgo.run(); resultVOIs = resultImage.getVOIs(); resultVOI =
			 * smoothAlgo.getResultVOI(); resultVOIs.VOIAt(1).removeCurves();
			 * resultVOIs.VOIAt(1).setCurves(resultVOI.getCurves());
			 * resultImage.notifyImageDisplayListeners(null, true); // new
			 * ViewJFrameImage(resultImage); smoothAlgo.setCompleted(true);
			 * smoothAlgo.finalize(); smoothAlgo = null;
			 */

		} catch (OutOfMemoryError x) {
			MipavUtil.displayError("Dialog Smooth: unable to allocate enough memory");

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
	private void generateBoundaryContours(VOIBase srcContour, ModelImage targetImageSlice) {
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
		for (i = sidePointsForTangent, j = 0; i <= sidePointsForTangent + nPoints - 1; i++, j++) {
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
				for (k = 0, m = i - sidePointsForTangent; m <= i + sidePointsForTangent; m++, k++) {
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
					var = Math.sqrt(ySqSum * ySqSum - 2.0 * xSqSum * ySqSum + xSqSum * xSqSum + 4.0 * xySum * xySum);
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
						d1 += Math.abs((slope * xCenteredPoints[k] - yCenteredPoints[k]) / Math.sqrt(slope * slope + 1));
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
						d2 += Math.abs((slope * xCenteredPoints[k] - yCenteredPoints[k]) / Math.sqrt(slope * slope + 1));
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
					if (srcContour.contains(xPoints[i], yPoints[i] + innerDistance)) {
						point = new Vector3f(xPoints[i], yPoints[i] + innerDistance, slice);
						innerV.add(point);
					} else if (srcContour.contains(xPoints[i], yPoints[i] - innerDistance)) {
						point = new Vector3f(xPoints[i], yPoints[i] - innerDistance, slice);
						innerV.add(point);
					}
				} // if (Double.isInfinite(slope))
				else {
					xDist = innerDistance / Math.sqrt(1.0 + slope * slope);
					yDist = xDist * slope;
					if (srcContour.contains((float) (xPoints[i] + xDist), (float) (yPoints[i] + yDist))) {
						point = new Vector3f((float) (xPoints[i] + xDist), (float) (yPoints[i] + yDist), slice);
						innerV.add(point);
					} else if (srcContour.contains((float) (xPoints[i] - xDist), (float) (yPoints[i] - yDist))) {
						point = new Vector3f((float) (xPoints[i] - xDist), (float) (yPoints[i] - yDist), slice);
						innerV.add(point);
					}
				}
			} // if (doInner)
			if (doOuter) {
				if (Double.isInfinite(slope)) {
					if (!srcContour.contains(xPoints[i], yPoints[i] + outerDistance)) {
						point = new Vector3f(xPoints[i], yPoints[i] + outerDistance, slice);
						outerV.add(point);
					} else if (!srcContour.contains(xPoints[i], yPoints[i] - outerDistance)) {
						point = new Vector3f(xPoints[i], yPoints[i] - outerDistance, slice);
						outerV.add(point);
					}
				} // if (Double.isInfinite(slope))
				else {
					xDist = outerDistance / Math.sqrt(1.0 + slope * slope);
					yDist = xDist * slope;
					if (!srcContour.contains((float) (xPoints[i] + xDist), (float) (yPoints[i] + yDist))) {
						point = new Vector3f((float) (xPoints[i] + xDist), (float) (yPoints[i] + yDist), slice);
						outerV.add(point);
					} else if (!srcContour.contains((float) (xPoints[i] - xDist), (float) (yPoints[i] - yDist))) {
						point = new Vector3f((float) (xPoints[i] - xDist), (float) (yPoints[i] - yDist), slice);
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
		VOI resultVOI = new VOI(sID, kName + "_" + sID, srcContour.getType(), -1);

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
						destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1 + (2 * borderSize);
						destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1 + (2 * borderSize);
					} else {
						destExtents = new int[3];
						destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1 + (2 * borderSize);
						destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1 + (2 * borderSize);
						destExtents[2] = Math.abs(zBounds[1] - zBounds[0] + 1);
					}
				} else {
					return;
				}

				System.err.println("destExtents[0] = " + destExtents[0] + "  destExtents[1] = " + destExtents[1]);

				// create crop images
				cropKeyImages.add(i, new ModelImage(image.getType(), destExtents, makeImageName(image.getImageName(), "_crop")));

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

				System.err.println("xCrop[0] = " + xCrop[0] + "   xCrop[1] = " + xCrop[1]);
				System.err.println("yCrop[0] = " + yCrop[0] + "   yCrop[1] = " + yCrop[1]);

				cropAlgo = new AlgorithmAddMargins(image, cropKeyImages.get(i), xCrop, yCrop, zCrop);

				// Hide the dialog since the algorithm is about to run.
				setVisible(false);

				cropAlgo.run();

			} catch (OutOfMemoryError e) {
				MipavUtil.displayError("Dialog Crop: unable to allocate enough memory");

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
				String fileName = new String(dir.substring(index + 1, dir.length()));

				System.err.println("Key Image: fileName = " + fileName + "  directory = " + directory);
				FileIO keyImageIO = new FileIO();
				keyImages.add(i, keyImageIO.readImage(fileName, directory));

				// read corresponding VOI
				String voiDir = keyImageVOIVector.get(i);
				System.err.println("voiDir = " + voiDir);
				index = voiDir.lastIndexOf(File.separator);
				String voiDirectory = new String(voiDir.substring(0, index + 1));
				String voiFileName = new String(voiDir.substring(index + 1, voiDir.length()));

				FileVOI fileVOI = null;
				fileVOI = new FileVOI(voiFileName, voiDirectory, keyImages.get(i));
				System.err.println("fileDirectory = " + directory + "fileName = " + fileName);
				System.err.println("voiDirectory = " + voiDirectory + "voiFileName = " + voiFileName);
				keyImageVOIs.add(i, fileVOI.readVOI(false));

				keyImages.get(i).registerVOI(keyImageVOIs.get(i)[0]);

				/*
				 * // read AAM VOI voiDir = keyImageVOIVectorAAM.get(i);
				 * System.err.println("voiDir = " + voiDir); index =
				 * voiDir.lastIndexOf(File.separator); voiDirectory = new
				 * String(voiDir.substring(0, index + 1)); voiFileName = new
				 * String(voiDir.substring(index + 1, voiDir.length()));
				 * 
				 * // FileVOI fileVOI = null; fileVOI = new FileVOI(voiFileName,
				 * voiDirectory, keyImages.get(i));
				 * System.err.println("fileDirectory = " + directory +
				 * "fileName = " + fileName);
				 * System.err.println("voiDirectory = " + voiDirectory +
				 * "voiFileName = " + voiFileName); keyImageVOIs.add(i,
				 * fileVOI.readVOI(false));
				 * 
				 * keyImages.get(i).registerVOI(keyImageVOIs.get(i)[0]);
				 */
				// new ViewJFrameImage(keyImages.get(i));

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