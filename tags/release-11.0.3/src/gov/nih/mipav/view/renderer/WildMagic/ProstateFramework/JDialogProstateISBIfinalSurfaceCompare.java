package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.*;

import javax.swing.*;

import WildMagic.LibFoundation.Mathematics.Vector3f;
import WildMagic.LibFoundation.Mathematics.Vector2f;



/**
 * This class converts 3D prostate surface into VOIs; saving them for comparison. 
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstateISBIfinalSurfaceCompare extends JDialogBase
		implements AlgorithmInterface {

	private static final long serialVersionUID = -7360089445417194259L;

	/** The main user interface. */
	private ViewUserInterface UI;

	/** key image directory. */
	private JLabel labelKeyImage;
	private JTextField textFieldKeyImage;
	private JButton buttonKeyImage;

	private JPanel imageSelectionPanel;


	/** saved 2D slices atlas dir. */
	private JLabel labelSaveImage;
	private JTextField textFieldSaveImage;
	private JButton buttonSaveImage;
	private JFileChooser saveImageChooser = new JFileChooser();
	private String saveImageDirectory;

	Hashtable<String, Hashtable<String, String>> imageNameHashtable = new Hashtable<String, Hashtable<String, String>>();
	Hashtable<String, String> voiNameHashtable = new Hashtable<String, String>();

	Hashtable<String, Hashtable<String, ModelImage>> imageHashtable = new Hashtable<String, Hashtable<String, ModelImage>>();
	Hashtable<String, Vector<VOI>> voiHashtable = new Hashtable<String, Vector<VOI>>();

	Hashtable<String, Vector<ModelImage>> srcImageTable = new Hashtable<String, Vector<ModelImage>>();
	Hashtable<String, Vector<VOI>> srcVOITable = new Hashtable<String, Vector<VOI>>();

	Hashtable<String, Hashtable<String, ModelImage>> origImageTable = new Hashtable<String, Hashtable<String, ModelImage>>();

	Hashtable<String, Integer> dicomTable = new Hashtable<String, Integer>();
	Hashtable<String, Integer> voiTable = new Hashtable<String, Integer>();

	/**
	 * Constructor.
	 * 
	 * @param theParentFrame
	 */
	public JDialogProstateISBIfinalSurfaceCompare(Frame theParentFrame) {
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

		File fileDir_1 = new File(
				"/scratch/gitrepo/ProstateSeg/ISBI2017/dataset");
		traverse_Layer(fileDir_1);

	}

	private void traverse_Layer(File dir) {

		if (dir.isDirectory()) {
			String[] children = dir.list();
			for (int i = 0; i < children.length; i++) {
				imageNameHashtable.put(children[i],
						new Hashtable<String, String>());
				traverse_firstLayer(dir, children[i]);
			}
		}

	}

	private void traverse_firstLayer(File firstDir, String child) {

		File firstLayer = new File(firstDir, child);
		traverse_secondLayer(firstLayer, child);

	}

	private void traverse_secondLayer(File firstLayer, String hashID) {

		String[] children = firstLayer.list();

		for (int i = 0; i < children.length; i++) {

			if (children[i].startsWith("image") && children[i].endsWith("xml")) {

				String imgString = firstLayer + File.separator + children[i];

				if (children[i].contains("Axial")) {
					// System.err.print("Axial : ");
					imageNameHashtable.get(hashID).put("Axial", imgString);
				} else if (children[i].contains("Sagittal")) {
					// System.err.print("Sagittal : ");
					imageNameHashtable.get(hashID).put("Sagittal", imgString);
				} else if (children[i].contains("Coronal")) {
					// System.err.print("Coronal : ");
					imageNameHashtable.get(hashID).put("Coronal", imgString);
				}
				// System.err.println(imgString);

			} else if (children[i].equals("output.ply")) {
				String voiString = firstLayer + File.separator + children[i];
				// System.err.println(voiString);
				voiNameHashtable.put(hashID, voiString);
			}

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

		System.err.println("saveImage");

		saveGroundTruth();

		long endTime = System.currentTimeMillis();
		int min = (int) ((endTime - startTime) / 1000f / 60f);
		int sec = (int) ((endTime - startTime) / 1000f % 60f);
		System.err.println("time elapse = " + min + "  mins  " + sec + "  sec");

		System.gc();

	}

	public void saveGroundTruth() {

		Set<String> hashKeys = origImageTable.keySet();
		for (String hashKey : hashKeys) {

			Hashtable<String, ModelImage> imageHash = origImageTable
					.get(hashKey);
			ModelImage axialImage = imageHash.get("axial");
			ModelImage sagittalImage = imageHash.get("sagittal");
			ModelImage coronalImage = imageHash.get("coronal");

			try {
				String subDir = saveImageDirectory + File.separator + hashKey
						+ File.separator;
				File subDirFile = new File(subDir);
				if (!subDirFile.isDirectory())
					subDirFile.mkdir();

				VOI voiAxial = axialImage.getVOIs().elementAt(0);
				axialImage.saveImage(subDir, "imageAxial.xml", FileUtility.XML,
						false);
				FileVOI fileVOIAxial = new FileVOI("voiAxial.xml", subDir,
						axialImage);
				fileVOIAxial.writeVOI(voiAxial, true);

				VOI voiSagittal = sagittalImage.getVOIs().elementAt(0);
				sagittalImage.saveImage(subDir, "imageSagittal.xml",
						FileUtility.XML, false);
				FileVOI fileVOISagittal = new FileVOI("voiSagittal.xml",
						subDir, sagittalImage);
				fileVOISagittal.writeVOI(voiSagittal, true);

				VOI voiCoronal = coronalImage.getVOIs().elementAt(0);
				coronalImage.saveImage(subDir, "imageCoronal.xml",
						FileUtility.XML, false);
				FileVOI fileVOICoronal = new FileVOI("voiCoronal.xml", subDir,
						coronalImage);
				fileVOICoronal.writeVOI(voiCoronal, true);

			} catch (Exception e) {
				e.printStackTrace();
			}
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
			smoothAlgo.addListener(this);
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
	 * load image files and voi files
	 */
	public void loadFiles() {
		readFile();
		System.err.println("finish image I/O");

	}

	public void readFile() {

		srcImageTable.put("axial", new Vector<ModelImage>());
		srcImageTable.put("sagittal", new Vector<ModelImage>());
		srcImageTable.put("coronal", new Vector<ModelImage>());

		try {

			Set<String> keys = imageNameHashtable.keySet();

			System.err.println("keys size = " + keys.size());

			for (String hashID : keys) {

				System.err.println("hashID = " + hashID);
				Hashtable<String, String> imageSet = imageNameHashtable
						.get(hashID);
				String voiName = voiNameHashtable.get(hashID);

				Set<String> orientationKeys = imageSet.keySet();

				for (String orientation : orientationKeys) {
					// System.err.println("orientation = " + orientation);

					String imageFullName = imageSet.get(orientation);
					// System.err.println("imageFullName = " + imageFullName);

					FileIO fileIO = new FileIO();
					fileIO.setQuiet(true);
					String voiFileName = null;

					int index = imageFullName.lastIndexOf(File.separator);
					String fileName = imageFullName.substring(index + 1,
							imageFullName.length());
					String directory = imageFullName.substring(0, index + 1);
					System.err.println("filename = " + fileName);
					System.err.println("directory = " + directory);

					ModelImage image = fileIO.readImage(fileName, directory);
					image.setImageName(hashID + "_" + orientation);
					int imageOrientation = image.getImageOrientation();
					// new ViewJFrameImage(image);
					voiFileName = voiName;

					loadPlyAsciiMesh(new File(voiFileName), image,
							imageOrientation, hashID);

				}

			}

		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private void loadPlyAsciiMesh(File file, ModelImage kImage,
			int imageOrientation, String hashID) {

		try {
			FileInputStream data;
			data = new FileInputStream(file);
			readPlyAscii(data, kImage, imageOrientation, hashID);

		} catch (FileNotFoundException e) {
			System.err.println("ERROR: Can't find file " + file);

		} catch (IOException e) {

		}
	}

	private void readPlyAscii(FileInputStream data, ModelImage kImage,
			int imageOrientation, String hashID) throws IOException {

		float zMin = 999;
		float zMax = 0;

		Vector<Vector3f> vertexArray = new Vector<Vector3f>();

		float x, y, z;
		Vector<Integer> connectivity = new Vector<Integer>();

		float _res[] = kImage.getFileInfo(0).getResolutions();
		float[] _startLocation = kImage.getFileInfo()[0].getOrigin();
		int[] _direction = MipavCoordinateSystems.getModelDirections(kImage);

		Vector3f ptIn = new Vector3f();
		Vector3f ptOut = new Vector3f();

		int i;
		int iVertexCount = 0;
		int iTriangleCount = 0;
		boolean readHeader = true;
		// float x = 0f, y = 0f, z = 0f;
		int idx1 = 0, idx2 = 0, idx3 = 0;

		/*
		 * Vector<Vector3f> vertexArray = new Vector<Vector3f>();
		 * Vector<Integer> connectivity = new Vector<Integer>(); VertexBuffer
		 * kVBuffer; int[] aiConnect; float _res[] =
		 * kImage.getFileInfo(0).getResolutions(); float[] _startLocation =
		 * kImage.getFileInfo()[0].getOrigin(); int[] _direction =
		 * MipavCoordinateSystems.getModelDirections(kImage);
		 * 
		 * Vector3f ptIn = new Vector3f(); Vector3f ptOut = new Vector3f();
		 */
		try {

			DataInputStream in = new DataInputStream(new BufferedInputStream(
					data));
			String s, token;
			while ((s = readLine(in)).length() > 0) {
				StringTokenizer st = new StringTokenizer(s);
				while (st.hasMoreTokens() && readHeader) {
					// System.err.print(st.nextToken() + " ");
					token = st.nextToken();
					if (token.equals("vertex")) {
						iVertexCount = Integer.valueOf(st.nextToken());
					} else if (token.equals("face")) {
						iTriangleCount = Integer.valueOf(st.nextToken());
						readLine(in);
						readLine(in); // skip two lines follow the face count
										// attribute in PLY file format.
						readHeader = false;
						break;
					}
				}
				if (readHeader == false)
					break;
			}

			// read Vertex
			for (i = 0; i < iVertexCount; i++) {
				s = readLine(in);
				StringTokenizer st = new StringTokenizer(s);
				x = Float.valueOf(st.nextToken());
				y = Float.valueOf(st.nextToken());
				z = Float.valueOf(st.nextToken());

				ptIn.X = x;
				ptIn.Y = y;
				ptIn.Z = z;

				MipavCoordinateSystems.scannerToFile(ptIn, ptOut, kImage);
				// x = (ptOut.X * _res[0] * _direction[0]) + _startLocation[0];
				// y = (ptOut.Y * _res[1] * _direction[1]) + _startLocation[1];
				// z = (ptOut.Z * _res[2] * _direction[2]) + _startLocation[2];

				x = ptOut.X;
				y = ptOut.Y;
				z = ptOut.Z;

				if (z > zMax) {
					zMax = z;
				}
				if (z < zMin) {
					zMin = z;
				}

				vertexArray.add(new Vector3f(x, y, z));
			}

			// read connectivity
			for (i = 0; i < iTriangleCount; i++) {
				s = readLine(in);
				StringTokenizer st = new StringTokenizer(s);
				st.nextToken(); // skip 3
				idx1 = Integer.valueOf(st.nextToken());
				connectivity.add(idx1);
				idx2 = Integer.valueOf(st.nextToken());
				connectivity.add(idx2);
				idx3 = Integer.valueOf(st.nextToken());
				connectivity.add(idx3);
			}

		} catch (FileNotFoundException e) {
			e.printStackTrace();

		} catch (IOException e) {

		}

		/********************************************************************/
		int vertexCount = vertexArray.size();
		VOIVector voiVectorNew = new VOIVector();
		VOI voiNew = new VOI((short) 0, "ImageVOI");
		voiVectorNew.add(voiNew);

		int startIndex = Math.round(zMin + 1);
		int endIndex = Math.round(zMax - 1);
		for (int j = startIndex; j < endIndex; j++) {

			// 1. filter out closed points
			Vector<Vector3f> ptsArray = new Vector<Vector3f>();
			float centerX = 0;
			float centerY = 0;

			for (i = 0; i < vertexCount; i++) {
				Vector3f pos = vertexArray.elementAt(i);
				if (Math.abs(pos.Z - j) < 0.5) {
					ptsArray.add(new Vector3f(pos.X, pos.Y, j));
					centerX += pos.X;
					centerY += pos.Y;
				}
			}

			// 2. compute center;
			int numPts = ptsArray.size();

			if (numPts <= 20)
				continue;

			centerX = centerX / numPts;
			centerY = centerY / numPts;
			Vector3f center = new Vector3f(centerX, centerY, j);

			Hashtable<Float, Vector3f> ptsTable = new Hashtable<Float, Vector3f>();

			// 3. compute polar coordinate
			for (i = 0; i < numPts; i++) {
				Vector3f loc = ptsArray.get(i);
				Vector2f in = new Vector2f(loc.X, loc.Y);
				Vector2f out = new Vector2f(0, 0);
				MipavCoordinateSystems.CartesianToPolar2D(in, out, center);
				ptsTable.put(out.Y, new Vector3f(loc.X, loc.Y, j));

			}

			ArrayList<Float> ptsList = Collections.list(ptsTable.keys());
			Collections.sort(ptsList);
			Iterator<Float> it = ptsList.iterator();
			Vector3f[] ptsResult = new Vector3f[numPts];
			int idx = 0;
			while (it.hasNext()) {
				float key = it.next();
				ptsResult[idx] = ptsTable.get(key);
				idx++;
			}

			VOIBase vTemp = new VOIContour(true);
			vTemp.importPoints(ptsResult);
			voiNew.importCurve(vTemp);

		} // int j = startIndex; j <= endIndex; j++

		kImage.addVOIs(voiVectorNew);
		smoothVOI60(kImage, kImage);
		// new ViewJFrameImage(kImage);

		if (origImageTable.get(hashID) == null) {
			origImageTable.put(hashID, new Hashtable<String, ModelImage>());
		}

		if (imageOrientation == FileInfoBase.AXIAL) {
			origImageTable.get(hashID).put("axial", kImage);
		} else if (imageOrientation == FileInfoBase.SAGITTAL) {
			origImageTable.get(hashID).put("sagittal", kImage);
		} else if (imageOrientation == FileInfoBase.CORONAL) {
			origImageTable.get(hashID).put("coronal", kImage);
		}

	}// End of readPlyAscii

	/**
	 * Read a line of ASCII text from the input stream.
	 * 
	 * @param in
	 *            InputStream
	 * @return line of the ascii file as a String
	 * @throws IOException
	 *             I/O exception
	 */
	private static String readLine(InputStream in) throws IOException {
		StringBuffer buf = new StringBuffer();
		int c;
		while ((c = in.read()) > -1 && c != '\n') {
			buf.append((char) c);
		}
		return buf.toString();
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