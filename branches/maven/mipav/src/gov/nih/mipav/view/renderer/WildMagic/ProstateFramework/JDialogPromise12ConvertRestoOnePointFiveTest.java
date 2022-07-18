package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import ar.com.hjg.pngj.*;



/**
 * Atfer N4 correction, take those images with binary masks, transform again
 * to 0.5mm x 0.5mm x 1.5 mm images.   For test case only. 
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogPromise12ConvertRestoOnePointFiveTest extends JDialogBase implements
		AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194259L;

	/** The main user interface. */
	private ViewUserInterface UI;

	/** key image directory. */
	private JLabel labelKeyImage;
	private JTextField textFieldKeyImage;
	private JButton buttonKeyImage;

	private JPanel imageSelectionPanel;

	/** image vector to hold the actual images. */
	private Vector<ModelImage> keyImages = new Vector<ModelImage>();
	private Vector<ModelImage> keyImagesTransform = new Vector<ModelImage>();
	/** voi vector to hold the actual vois. */
	private Vector<ModelImage> keyImageVOIs = new Vector<ModelImage>();

	private Vector<String> keyImageVector1 = new Vector<String>();
	
	private Vector<String> keyImageVector2 = new Vector<String>();
	
	/** saved 2D slices atlas dir. */
	private JLabel labelSaveImage;
	private JTextField textFieldSaveImage;
	private JButton buttonSaveImage;
	private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageDirectory;

	private AlgorithmTransform algoTrans;

	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogPromise12ConvertRestoOnePointFiveTest(Frame theParentFrame) {
		super(theParentFrame, false);
		UI = ViewUserInterface.getReference();
		init();
		setVisible(true);

	}

	/**
	 * dispose memory
	 * */
	public void disposeLocal() {
	
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
			
		} else if (command.equals("ChooseKeyImageDir")) {
			readKeyImageDir();
			sortKeyImage_1();
			// sortKeyImage_2();

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

		File fileDir_1 = new File("/scratch/miccai/N4Test");
		// File fileDir_1 = new File("/data/ruida/MICAI2012/test");
		// File fileDir_2 = new File("/scratch/miccai/Preprocessing/N4TestTransform");

		traverse_folder_1(fileDir_1);
		// traverse_folder_2(fileDir_2); 

	}

	private void traverse_folder_1(File dir) {
		processDir_folder_1(dir);
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse_folder_1(new File(dir, children[i]));
			}
		}

	}

	private void processDir_folder_1(File dir) {
		String dirName = dir.toString();
		// System.err.println("dirName = " + dirName);
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();

		if (dirName.substring(begin, end).startsWith("Case")
				&& dirName.substring(begin, end).endsWith(".mhd")
				&& !dirName.contains("segmentation")) {
			// System.err.println(dir.toString());
			keyImageVector1.add(dir.toString());
		}

	}

	public void sortKeyImage_1() {
		int i;
		int len = keyImageVector1.size();
		String imageName;
		String voiName;
		int start, end;
		Hashtable<Integer, String> imageNameTable = new Hashtable<Integer, String>();
		Hashtable<Integer, String> imageVOITable = new Hashtable<Integer, String>();
		int index;

		for (i = 0; i < len; i++) {
			imageName = keyImageVector1.get(i);
			start = imageName.lastIndexOf(File.separator) + 1;
			end = imageName.lastIndexOf(".");
			// System.err.println(imageName.substring(start+4, end));
			// index = Integer.valueOf(imageName.substring(start+5, end));
			index = Integer.parseInt(imageName.substring(start + 4, end));
			// System.err.println("index = " + index);
			imageNameTable.put(index, imageName);
		}

		keyImageVector1.clear();
		for (i = 0; i < len; i++) {
			imageName = imageNameTable.get(i);
			if (imageName != null) {
				keyImageVector1.add(imageName);
			}
		}

		// test for printing

		i = 0;
		for (String entry : keyImageVector1) {
			System.err.println(i + " = " + entry);
			i++;
		}

		/*
		System.err.println("VOI:");
		i = 0;
		for (String entry : keyImageVOIVector1) {
			System.err.println(i + " = " + entry);
			i++;
		}
		*/ 

	}

	private void traverse_folder_2(File dir) {
		processDir_folder_2(dir);
		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				traverse_folder_2(new File(dir, children[i]));
			}
		}

	}

	private void processDir_folder_2(File dir) {
		String dirName = dir.toString();
		// System.err.println("dirName = " + dirName);
		int begin = dirName.lastIndexOf(File.separator) + 1;
		int end = dirName.length();

		if (dirName.substring(begin, end).startsWith("Case")
				&& dirName.substring(begin, end).endsWith(".mhd")
				&& dirName.contains("segmentation")) {
			// System.err.println(dir.toString());
			keyImageVector2.add(dir.toString());
		}

	}

	public void sortKeyImage_2() {
		int i;
		int len = keyImageVector2.size();
		String imageName;

		int start, end;
		Hashtable<Integer, String> imageNameTable = new Hashtable<Integer, String>();

		int index;

		for (i = 0; i < len; i++) {
			imageName = keyImageVector2.get(i);
			start = imageName.lastIndexOf(File.separator) + 1;
			end = imageName.lastIndexOf("_");
			// System.err.println(imageName.substring(start+4, end));
			// index = Integer.valueOf(imageName.substring(start+5, end));
			index = Integer.parseInt(imageName.substring(start + 4, end));
			// System.err.println("index = " + index);
			imageNameTable.put(index, imageName);
		}

		keyImageVector2.clear();
		for (i = 0; i < len; i++) {
			imageName = imageNameTable.get(i);
			if (imageName != null) {
				keyImageVector2.add(imageName);
			}
		}

		// test for printing

		i = 0;
		for (String entry : keyImageVector2) {
			System.err.println(i + " = " + entry);
			i++;
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

		transform();

		System.err.println("saveImage");
		
		saveTestedImages();
		
		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();

	}

	public void transform() {

		int size = keyImages.size();

		for (int imageIndex = 0; imageIndex < size; imageIndex++) {

			ModelImage keyImage = keyImages.get(imageIndex);

			ModelImage transKeyImage = calculateTransform(keyImage);
			keyImagesTransform.add(imageIndex, transKeyImage);
		
			
			// new ViewJFrameImage(keyImageVOIsTransformN4.get(imageIndex));
			System.err.println("imageIndex = " + imageIndex);
			
		}

	}

	public ModelImage calculateTransform(ModelImage keyImage) {

		ModelImage resultImage;
		TransMatrix xfrm;
		int interp;
		float oXres, oYres, oZres, cXres, cYres, cZres;
		int oXdim, oYdim, oZdim, cXdim, cYdim, cZdim;
		int[] units;
		boolean doVOI, doClip, doPad, preserveFOV, doUpdateOrigin, doInvMat;
		boolean doRotateCenter;
		float fillValue = 0.0f;
		boolean isSATransform = false;
		Vector3f center = null;

		float[] dims = new float[3];
		float[] resols = new float[3];
	
		dims[0] = keyImage.getFileInfo()[0].getExtents()[0];
		dims[1] = keyImage.getFileInfo()[0].getExtents()[1];
		dims[2] = keyImage.getFileInfo()[0].getExtents()[2];
		resols[0] = keyImage.getFileInfo()[0].getResolutions()[0];
		resols[1] = keyImage.getFileInfo()[0].getResolutions()[1];
		resols[2] = keyImage.getFileInfo()[0].getResolutions()[2];

		doVOI = false;
		doClip = false;
		doPad = false;
		doRotateCenter = false;
		center = null;

		fillValue = 0.0f;
		doUpdateOrigin = true;
		isSATransform = false;

		interp = 0;
		xfrm = new TransMatrix(3);
		xfrm.identity();

		units = new int[3];
		units[0] = units[1] = units[2] = Unit.MILLIMETERS.getLegacyNum();

		float iXdim = dims[0];
		float iYdim = dims[1];
		float iZdim = dims[2];

		float iXres = resols[0];
		float iYres = resols[1];
		float iZres = resols[2];

		oXres = 0.5f;
		oYres = 0.5f;
		oZres = 1.5f;

		int constantFOV = 1;
		
		float fovX = iXres * (iXdim - constantFOV);
	    float fovY = iYres * (iYdim - constantFOV);
	    float fovZ = iZres * (iZdim - constantFOV);
        oXdim = Math.round(fovX / oXres) + constantFOV;
        oYdim = Math.round(fovY / oYres) + constantFOV;
        oZdim = Math.round(fovZ / oZres) + constantFOV;
		
		System.err.println("oXdim = " + oXdim + " oYdim = " + oYdim);

		algoTrans = new AlgorithmTransform(keyImage, xfrm, interp, oXres,
				oYres, oZres, oXdim, oYdim, oZdim, units, doVOI, doClip, doPad,
				doRotateCenter, center);
		algoTrans.setFillValue(fillValue);
		algoTrans.setUpdateOriginFlag(doUpdateOrigin);
		algoTrans.setUseScannerAnatomical(isSATransform);
		algoTrans.run();

		resultImage = algoTrans.getTransformedImage();
		resultImage.calcMinMax();

		algoTrans.disposeLocal();
		algoTrans = null;
		return resultImage;
	}

	public void saveTestedImages() {
		
			int size = keyImagesTransform.size();
            System.err.println("size = " + size);
			for (int imageIndex = 0; imageIndex < size; imageIndex++) {
				
				ModelImage targetImageSlice = keyImagesTransform.get(imageIndex);
				
				String sliceDir = saveImageDirectory;
				File sliceDirFile = new File(sliceDir);
				if (!sliceDirFile.isDirectory())
					sliceDirFile.mkdir();
				sliceDir += File.separator + "N4Test1_5" + File.separator;
				File dir = new File(sliceDir);
				if (!dir.isDirectory()) {
					dir.mkdir();
				}
			
				String imgName = keyImageVector1.get(imageIndex);
				System.err.println("imgName = " + imgName);
				int start = imgName.lastIndexOf(File.separator) + 1;
				int end = imgName.length();
				imgName = imgName.substring(start, end);
				
			    System.err.println(sliceDir + imgName);
				targetImageSlice.saveImage(sliceDir, imgName, FileUtility.METAIMAGE, false);
		
			}
          
	}

	/**
	 * Save the 2D slices and VOIs to user specified dir.
	 */
	public void saveImages() {

		int index = 0;
		for (int i = 0; i < keyImages.size(); i++) {
			try {

				ModelImage keyImage = keyImages.get(i);
				ModelImage keyImageVOI = keyImageVOIs.get(i);

				int xDim = keyImage.getExtents()[0];
				int yDim = keyImage.getExtents()[1];
				int zDim = keyImage.getExtents()[2];

				int size_3D = xDim * yDim * zDim;
				float[] imageBuffer = new float[size_3D];

				try {
					keyImage.exportData(0, imageBuffer.length, imageBuffer);
				} catch (Exception e) {
					e.printStackTrace();
				}

				float percentile_left = 0.1f;
				float percentile_right = 0.1f;

				int minIndex = (int) (size_3D * percentile_left);
				int maxIndex = (int) (size_3D * (1.0 - percentile_right));

				Arrays.sort(imageBuffer);

				float min = imageBuffer[minIndex];
				float max = imageBuffer[maxIndex];

				int size = xDim * yDim;

				int[] newExtents = new int[2];
				newExtents[0] = xDim;
				newExtents[1] = yDim;

				for (int j = 0; j <= zDim; j++) {

					try {

						System.err.println(" image number = " + (0 + i)
								+ "   slice number = " + j);

						ModelImage targetImageSlice = new ModelImage(
								ModelStorageBase.FLOAT, newExtents, "target"
										+ j);
						float[] targetBuffer = new float[size];
						keyImage.exportData(j * size, size, targetBuffer);
						targetImageSlice.importData(0, targetBuffer, true);

						// 1) save image
					
						String sliceDir = saveImageDirectory;
						File sliceDirFile = new File(sliceDir);
						if (!sliceDirFile.isDirectory())
							sliceDirFile.mkdir();

						System.err.println("index = " + index);

						String imgName = "image_" + index + ".png";
						savePNGfile(sliceDir, imgName, targetImageSlice, min,
								max, xDim, yDim, false);

						// Save mask image
						ModelImage maskImage = new ModelImage(
								ModelStorageBase.INTEGER, newExtents, "voi" + j);
						int[] voiBuffer = new int[size];
						keyImageVOI.exportData(j * size, size, voiBuffer);
						maskImage.importData(0, voiBuffer, true);

						String maskName = "voi_" + index + ".png";
						savePNGfile(sliceDir, maskName, maskImage, 0, 1, xDim,
								yDim, true);

						targetBuffer = null;
						targetImageSlice.disposeLocal();
						targetImageSlice = null;

						maskImage.disposeLocal();
						maskImage = null;

						index++;

					} catch (IOException e) {

					}
				}

			} catch (OutOfMemoryError x) {
				MipavUtil
						.displayError("Dialog GaborFilter: unable to allocate enough memory");
			}
		}

	}

	

	private void savePNGfile(String dirName, String fileName,
			ModelImage srcImage, float minIntensity, float maxIntensity,
			int xDim, int yDim, boolean isMask) {
		File file = null;
		boolean alpha = false;
		boolean gray = true;
		boolean indexed = false;
		try {

			ImageInfo imi = new ImageInfo(xDim, yDim, 8, alpha, gray, indexed);
			ImageLineByte line = new ImageLineByte(imi);
			int yMin = 0, yMax = xDim;
			int xMin = 0, xMax = yDim;
			int x, y;
			file = new File(dirName + File.separator + fileName);
			if (!file.exists()) {
				file.createNewFile();
			}

			OutputStream os = new FileOutputStream(file);

			PngWriter pngw = new PngWriter(os, imi);

			for (int j = yMin; j < yMax; j++) {

				y = j - yMin;

				for (int i = xMin; i < xMax; i++) {

					x = i - xMin;

					if (isMask == false) {
						float intensity = srcImage.getFloat(i, j);
						float r = 0;
						if (intensity >= minIntensity
								&& intensity <= maxIntensity) {
							r = (float) ((intensity - minIntensity) * 255d / (maxIntensity - minIntensity));
						} else if (intensity > maxIntensity)
							r = 255;
						else if (intensity < minIntensity)
							r = 0;

						ImageLineHelper.setPixelGray8(line, x, (int) r);
					} else {

						short intensity = srcImage.getShort(i, j);

						if (intensity == 1) {
							// System.err.println("intensity = " + intensity);
							ImageLineHelper.setPixelGray8(line, x, (int) 255);
						} else {
							ImageLineHelper.setPixelGray8(line, x, (int) 0);
						}

					}

				}
				pngw.writeRow(line, y);
			}
			pngw.end();
			pngw.close();
			pngw = null;
			os.close();
			os = null;
			line = null;
			imi = null;

		} catch (Exception e) {
			System.err.println("image find wrong : " + file.getAbsolutePath());
			e.printStackTrace();

			System.exit(0);
		}
	}

	/**
	 * load image files and voi files
	 */
	public void loadFiles() {
		readFile();
		System.err.println("finish image I/O");

	}


	public void readFile() {

		int index;
		try {
			
			int len = keyImageVector1.size();
			int currentIndex = 0;

			for (int imageIndex = 0; imageIndex < len; imageIndex++) {
				// read key image

				String dir = keyImageVector1.get(imageIndex);

				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1,
						dir.length()));

				System.err.println("Key Image: fileName = " + fileName
						+ "  directory = " + directory);
				FileIO keyImageIO = new FileIO();
				keyImages.add(currentIndex,
						keyImageIO.readImage(fileName, directory));
				
				currentIndex++;
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
		imageSelectionPanel.setLayout(new GridLayout(2, 3));
		imageSelectionPanel.setBorder(buildTitledBorder("Auto Train"));

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