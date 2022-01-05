package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmCostFunctions2D;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.renderer.WildMagic.AAM.*;
import gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.ShapeSimilarity.*;
import gov.nih.mipav.view.renderer.WildMagic.ProstateFramework.ShapeSimilarity.POINT;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import javax.swing.*;
import java.util.*;

/**
 * This class exhaustively trains the 2D slices based Active Appearance Model
 * (AAM). For example, 100 3D prostate MRI images are converted to 2D slices
 * atlas, each slice saved directory contains 100 2D slice images and
 * corresponding VOIs. Based on the 2D slices and VOIs, this class performs
 * shape based similarity measure between closed shape VOIs, sub-groups the
 * image and VOIs, then apply the AAM training to each sub-group to generate the
 * AAM model. The exhaustive search algorithm subdivides the atlas into smaller
 * groups with a similar shape measure. After forming the smaller group, the
 * search algorithm marks the relevant images and VOIs as visited, and
 * iteratively searches the data in the atlas, terminating when no more
 * similarity shapes can be found. AAM training follow the work of Stegmann et
 * al. Cootes. et al. to create AAM model.
 * 
 * M. B. Stegmann, B. K. Ersboll, and R. Larsen, "FAME-A Flexible Appearance
 * Modeling Environment", IEEE Trans. Med. Imag., 22(10):1319-1331, October,
 * 2003.
 * 
 * T. F. Cootes, G. J. Edwards, and C. J. Taylor, "Active appearance models",
 * Proc. Eur. Conf. Comput. Vis. 2:484-498, 1998.
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstateImageCategorize extends JDialogBase implements
		AlgorithmInterface {

	private static final long serialVersionUID = -7360089445412224259L;

	/** The main user interface. */
	private ViewUserInterface UI;

	private JPanel imageSelectionPanel;
	private JPanel buttonPanel;

	/** axis region */
	private JComboBox axisList;
	private JLabel labelAxis;

	/** axis orietation. */
	private static int Axial = 0;
	private static int Saggital = 1;
	private static int Coronal = 2;
	private int axis = Axial;

	/** Key image related variables. */
	private JLabel labelKeyImage;
	private JTextField textFieldKeyImage;
	private JButton buttonKeyImage;

	/** Save group directory. */
	private JLabel labelGroup;
	private JTextField textFieldGroup;
	private JButton buttonGroup;

	/** key images variables. */
	private JFileChooser keyImageChooser = new JFileChooser();
	private String keyImageName;
	private String keyImageDirectory;

	/** image and voi string vector. */
	private Vector<String> imageVector = new Vector<String>();
	private Vector<String> voiVector = new Vector<String>();

	/** group variables. */
	private JFileChooser groupChooser = new JFileChooser();
	private String groupName;
	private String groupDirectory;

	/** image vector. */
	private Vector<ModelImage> sliceImages = new Vector<ModelImage>();

	/** VOI vector. */
	private Vector<VOI[]> imageVOIs = new Vector<VOI[]>();

	/** group index. */
	private int groupIndex = 0;

	/**
	 * Constructor
	 * 
	 * @param theParentFrame
	 *            parent frame reference.
	 */
	public JDialogProstateImageCategorize(Frame theParentFrame) {
		super(theParentFrame, false);
		UI = ViewUserInterface.getReference();
		init();
	}

	/**
	 * empty function to implement the algorithm interface.
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
	}

	/**
	 * ActionPerformed handler.
	 */
	public void actionPerformed(ActionEvent event) {
		String command = event.getActionCommand();
		if (command.equals("OK")) {
			groupImages();
		} else if (command.equals("SetAxis")) {
			axis = axisList.getSelectedIndex();
		} else if (command.equals("Cancel")) {
			dispose();
		} else if (command.equals("ChooseKeyImageDir")) {
			imageVector.clear();
			voiVector.clear();
			readKeyImageDir();
		} else if (command.equals("ChooseGroupDir")) {
			selectGroupDir();
		}
	}

	/**
	 * Dialog GUI interface initialization.
	 */
	public void init() {
		setTitle("Prostate Trained Images Categorization");

		final JPanel mainPanel = new JPanel();
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

		final GridBagConstraints gbc = new GridBagConstraints();

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.fill = GridBagConstraints.NONE;
		gbc.anchor = GridBagConstraints.EAST;

		imageSelectionPanel = new JPanel();
		imageSelectionPanel.setLayout(new GridLayout(3, 3));
		imageSelectionPanel.setBorder(buildTitledBorder("Groups"));

		gbc.gridx = 0;
		gbc.gridy = 0;

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

		// groups save directory
		gbc.gridx = 2;
		imageSelectionPanel.add(buttonKeyImage, gbc);

		gbc.gridx = 0;
		gbc.gridy = 2;
		labelGroup = new JLabel("Group Directory: ");
		labelGroup.setFont(serif12);
		labelGroup.setForeground(Color.black);

		imageSelectionPanel.add(labelGroup, gbc);

		textFieldGroup = new JTextField(20);
		textFieldGroup.setFont(serif12);

		gbc.gridx = 1;
		imageSelectionPanel.add(textFieldGroup, gbc);

		buttonGroup = new JButton("Choose");
		buttonGroup.addActionListener(this);
		buttonGroup.setActionCommand("ChooseGroupDir");
		buttonGroup.setFont(serif12B);
		buttonGroup.setPreferredSize(MipavUtil.defaultButtonSize);

		gbc.gridx = 2;
		imageSelectionPanel.add(buttonGroup, gbc);

		// button Panel
		buttonPanel = new JPanel();
		buttonPanel.setLayout(new GridLayout(1, 3));
		gbc.gridx = 0;
		gbc.gridy = 0;
		buttonPanel.add(buildOKButton(), gbc);
		gbc.gridy = 1;
		buttonPanel.add(buildCancelButton(), gbc);
		gbc.gridy = 2;
		buttonPanel.add(buildHelpButton(), gbc);

		mainPanel.add(imageSelectionPanel);
		mainPanel.add(buttonPanel);
		getContentPane().add(mainPanel);
		pack();
		setVisible(true);
	}

	/**
	 * Read the key image director and generate image name and voi name vectors.
	 */
	private void readKeyImageDir() {
		String keyImageName;
		keyImageChooser.setDialogTitle("Open Key Images Directory");
		keyImageChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
		final int returnValue = keyImageChooser.showOpenDialog(UI
				.getMainFrame());

		if (returnValue == JFileChooser.APPROVE_OPTION) {
			keyImageName = keyImageChooser.getSelectedFile().getName();

			keyImageDirectory = String.valueOf(keyImageChooser
					.getCurrentDirectory())
					+ File.separatorChar
					+ keyImageName
					+ File.separatorChar;
			// UI.setDefaultDirectory(directory);
			textFieldKeyImage.setText(keyImageDirectory);

			File fileDir = new File(keyImageDirectory);
			System.err.println("check = " + keyImageDirectory);
			processDir(fileDir);
		} else {
			return;
		}
	}

	/**
	 * Process the directory.
	 * 
	 * @param dir
	 */
	private void processDir(File dir) {
		String[] children = dir.list();

		try {
		for (int i = 0; i < children.length; i++) {

			File file = new File(dir, children[i]);
			String dirName = file.toString();
			int begin = dirName.lastIndexOf(File.separator) + 1;
			int end = dirName.length();

			if (dirName.substring(begin, end).startsWith("image")
					&& dirName.substring(begin, end).endsWith(".xml")) {
				imageVector.add(file.toString());
				// System.err.println(file.toString());
			}
			if (dirName.substring(begin, end).startsWith("voi")
					&& dirName.substring(begin, end).endsWith(".xml")) {
				voiVector.add(file.toString());
				// System.err.println(file.toString());
			}
		}
		} catch (Exception  e ) {
			e.printStackTrace();
		}

		System.err.println("imageVector.size() = " + imageVector.size());
		
		System.err.println("Test");
		sortImageVectorString(imageVector);
		sortVOIVectorString(voiVector);

		for (int i = 0; i < imageVector.size(); i++) {
		   System.err.println(imageVector.get(i));
		   System.err.println(voiVector.get(i));
		 }
		// System.err.println();

	}

	/**
	 * Sort image vector in ascending order.
	 * 
	 * @param imageVector
	 *            image name vector.
	 */
	private void sortImageVectorString(Vector<String> imageVector) {

		Hashtable<Integer, String> table = new Hashtable<Integer, String>();
		int len = imageVector.size();
		for (int i = 0; i < len; i++) {
			String dirName = imageVector.get(i);
			int begin = dirName.lastIndexOf(File.separator) + 1 + 5;
			int end = dirName.lastIndexOf(".");
			String imageNumberStr = dirName.substring(begin, end);
			int imgNum = new Integer(imageNumberStr).intValue();
			table.put(imgNum, dirName);
		}

		imageVector.clear();

		for (int i = 0; i < 288; i++) {
			String name = (String) table.get(i);
			if (name != null) {
				imageVector.add(name);
			}
		}

	}

	/**
	 * Sort VOI name vector in ascending order.
	 * 
	 * @param voiVector
	 */
	private void sortVOIVectorString(Vector<String> voiVector) {

		Hashtable<Integer, String> table = new Hashtable<Integer, String>();
		int len = voiVector.size();

		for (int i = 0; i < len; i++) {
			String dirName = voiVector.get(i);
			int begin = dirName.lastIndexOf(File.separator) + 1 + 3;
			int end = dirName.lastIndexOf(".");
			String imageNumberStr = dirName.substring(begin, end);
			int imgNum = new Integer(imageNumberStr).intValue();
			table.put(imgNum, dirName);
		}

		voiVector.clear();

		for (int i = 0; i < 288; i++) {
			String name = (String) table.get(i);
			if (name != null) {
				voiVector.add(name);
			}
		}
	}

	/**
	 * Let user choose the Group trained model directory to save.
	 */
	private void selectGroupDir() {

		groupChooser.setDialogTitle("Select saved group directory");
		groupChooser.setDialogTitle("Open Groups Directory");
		groupChooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);

		final int returnValue = groupChooser.showOpenDialog(UI.getMainFrame());

		if (returnValue == JFileChooser.APPROVE_OPTION) {
			groupName = groupChooser.getSelectedFile().getName();
			groupDirectory = String.valueOf(groupChooser.getCurrentDirectory())
					+ File.separatorChar + groupName + File.separator;
			textFieldGroup.setText(groupDirectory);
			System.err.println("groupDir = " + groupDirectory);
		} else {
			return;
		}

	}

	/**
	 * Group images and exhaustively train the AAM model.
	 */
	private void groupImages() {
		sliceImages.clear();
		imageVOIs.clear();
		readImagesAndVOIs();
		categorizeByShape();

		
		sliceImages.clear();
		imageVOIs.clear();
		readImagesAndVOIsInReverse();
		categorizeByShape();

		sliceImages.clear();
		imageVOIs.clear();
		readImagesAndVOIsFromMid();
		categorizeByShape();
        
		// categorizeByTexture();
		// categorizeByShape();
		System.err.println("finish");
		dispose();
	}

	/**
	 * Shape similarity measure cost class.
	 *  
	 * @author Ruida Cheng
	 */
	class perShapeCost implements Comparable {
		public int index;
		public double metric;
		public float area;
		public boolean byArea = false;

		public perShapeCost(int _index, double _metric, float _area) {
			index = _index;
			metric = _metric;
			area = _area;
		}

		public void setByArea(boolean flag) {
			byArea = flag;
		}

		public final boolean lessThan(perShapeCost p) {
			if (byArea == false) {
				if (metric < p.metric) {
					return true;
				} else
					return false;
			} else {
				if (area < p.area) {
					return true;
				} else
					return false;
			}
		}

		public final boolean equals(perShapeCost l) {
			return metric == l.metric;
		}

		public final int compareTo(Object o) {
			perShapeCost other = (perShapeCost) o;
			if (lessThan(other))
				return -1;
			else if (other.lessThan(this))
				return 1;
			else
				return 0;
		}
	}

	/**
	 * NMI based image similarity measure cost class.
	 * 
	 * @author Ruida Cheng
	 */
	class perImageCost implements Comparable {
		public int index;
		public double cost_CRS, cost_MIS, cost_NMIS, cost_NXS;

		public perImageCost(int _index, double _cost_CRS, double _cost_MIS,
				double _cost_NMIS, double _cost_NXS) {
			index = _index;
			cost_CRS = _cost_CRS;
			cost_MIS = _cost_MIS;
			cost_NMIS = _cost_NMIS;
			cost_NXS = _cost_NXS;
		}

		public final boolean lessThan(perImageCost p) {

			int count = 0;
			if (cost_CRS < p.cost_CRS) {
				count++;
			}
			if (cost_MIS < p.cost_MIS) {
				count++;
			}
			if (cost_NMIS < p.cost_NMIS) {
				count++;
			}
			if (cost_NXS < p.cost_NXS) {
				count++;
			}

			if (count >= 3)
				return true;
			else
				return false;
		}

		public final boolean equals(perImageCost l) {
			return cost_CRS == l.cost_CRS && cost_MIS == l.cost_MIS
					&& cost_NMIS == l.cost_NMIS && cost_NXS == l.cost_NXS;
		}

		public final int compareTo(Object o) {
			perImageCost other = (perImageCost) o;
			if (lessThan(other))
				return -1;
			else if (other.lessThan(this))
				return 1;
			else
				return 0;
		}

	}

	/**
	 * Shape area computation
	 * 
	 * @param xPts
	 *            VOI points x coordinate
	 * @param yPts
	 *            VOI points y coordinate
	 * @param nPts
	 *            number of VOI points.
	 * @return
	 */
	private float area(float[] xPts, float[] yPts, int nPts) {
		float area;
		int carea = 0;
		int iminus1;
		for (int i = 0; i < nPts; i++) {
			iminus1 = i - 1;
			if (iminus1 < 0)
				iminus1 = nPts - 1;
			carea += (xPts[i] + xPts[iminus1]) * (yPts[i] - yPts[iminus1]);
		}
		area = Math.abs(carea / 2.0f);
		return area;
	}

	/**
	 * Shape based similarity measure. Exhaustive training.
	 */
	private void categorizeByShape() {

		int len = sliceImages.size();
		ShapeSimilarity shapeCompare = new ShapeSimilarity();
		Vector<perShapeCost> costVector = null;
		Vector<perShapeCost> areaVector = null;
		int[] filled = new int[len];

		float currentImageArea;
		CAAMShape currentImageShape = new CAAMShape();

		ModelImage currentImage = null;
		for (int i = 0; i < len; i++) {

			if (filled[i] != 1) {
				currentImage = sliceImages.get(i);

				costVector = new Vector<perShapeCost>();
				areaVector = new Vector<perShapeCost>();

				// if ( currentImage.getVOIs().size() == 0 ) continue;
				
				Vector<VOIBase>[] vArray = currentImage.getVOIs().VOIAt(0).getSortedCurves(VOIBase.ZPLANE, 1);
				currentImageShape.ReadASFfromVOI(currentImage);

				VOIBase v = vArray[0].get(0);

				VOIBase vTemp = (VOIBase) v.clone();
				int nPts = vTemp.size();

				float[] xPts = new float[nPts];
				float[] yPts = new float[nPts];
				float[] zPts = new float[nPts];
				vTemp.exportArrays(xPts, yPts, zPts);

				currentImageArea = (float) currentImageShape.ShapeSize();

				counterClockwise(xPts, yPts, zPts, nPts);

				poly polygon1 = new poly();
				polygon1.n = nPts;

				for (int z = 0; z < nPts; z++) {
					polygon1.pt[z] = new POINT();
					polygon1.pt[z].x = xPts[z];
					polygon1.pt[z].y = yPts[z];
				}

				for (int j = 0; j < len; j++) {
					if (j != i && filled[j] != 1) {
						CAAMShape comparedImageShape = new CAAMShape();

						ModelImage comparedImage = sliceImages.get(j);

					    comparedImageShape.ReadASFfromVOI(comparedImage);
					    
						System.err.println("comparedImage name = " + comparedImage.getImageFileName());
						vArray = comparedImage.getVOIs().VOIAt(0).getSortedCurves(VOIBase.ZPLANE, 1);
						v = vArray[0].get(0);
						vTemp = (VOIBase) v.clone();
						nPts = vTemp.size();
						float[] xPtsCmp = new float[nPts];
						float[] yPtsCmp = new float[nPts];
						float[] zPtsCmp = new float[nPts];
						vTemp.exportArrays(xPtsCmp, yPtsCmp, zPtsCmp);
						float compareImageArea = (float) comparedImageShape.ShapeSize();
						counterClockwise(xPtsCmp, yPtsCmp, zPtsCmp, nPts);
						poly polygon2 = new poly();
						polygon2.n = nPts;
						for (int z = 0; z < nPts; z++) {
							polygon2.pt[z] = new POINT();
							polygon2.pt[z].x = xPtsCmp[z];
							polygon2.pt[z].y = yPtsCmp[z];
						}
						double[] result = new double[6];
						shapeCompare.comparePolygon(polygon2, polygon1, result);
						perShapeCost p = new perShapeCost(j, result[0], Math.abs(compareImageArea - currentImageArea));
						p.setByArea(false);
						areaVector.add(p);
					}
				} // end of j

				Collections.sort(areaVector);
				System.err.println("test sort");
				costVector.add(new perShapeCost(i, 0, 0));
				int searchRange = Math.min(5, areaVector.size());
				for (int k = 0; k < searchRange; k++) {
					perShapeCost p = areaVector.get(k);
					p.setByArea(true);
					costVector.add(p);
				}

				System.err.println("costVector.size = " + costVector.size());
				Collections.sort(costVector);

				searchRange = Math.min(3, costVector.size());
				System.err.println("step " + i + " : ");
				Vector<ModelImage> modelImageVector = new Vector<ModelImage>();
				Vector<Integer> imageList = new Vector<Integer>();
				for (int k = 0; k < searchRange; k++) {
					perShapeCost p = costVector.get(k);
					// new ViewJFrameImage(sliceImages.get(p.index));
					modelImageVector.add(sliceImages.get(p.index));
					imageList.add(p.index);
					System.err.println("p.index = " + p.index + "\t"
							+ "p.metric = " + p.metric + "\t" + "p.area = "
							+ p.area);
				}
				System.err.println();
				// pause();

				// write the model

				CAAMConsoleModeB b = new CAAMConsoleModeB();
				// System.err.println("modelImageVector.size = " +
				// modelImageVector.size());
				if (modelImageVector.size() <= 1)
					continue;
				boolean buildSuccess = b.buildModel(modelImageVector);
				if (buildSuccess == false)
					continue;

				// for ( int k = 0; k < modelImageVector.size(); k++ ) {
				// 	new ViewJFrameImage(modelImageVector.get(k));
				// }
			
				
				C_AAMMODEL model = b.getModel();

				if (model.getNumberShapeParameters() >= 1) {

					System.err.println("number shape parameters = "
							+ model.getNumberShapeParameters());

					System.err.print("group" + groupIndex + " = ");

					String savedDirectory = groupDirectory + File.separator
							+ "Group" + groupIndex + File.separator;

					File dir = new File(savedDirectory);
					if (!dir.exists()) {
						dir.mkdir();
						// System.err.println("saved dir = " + dir);
					}

					String modelName = "model";
					model.WriteModel(savedDirectory + modelName);
					// mark the filled flag

					for (int k = 0; k < searchRange; k++) {
						perShapeCost p = costVector.get(k);
						filled[p.index] = 1;
						System.err.print("\t" + p.index);
						// new ViewJFrameImage(sliceImages.get(p.index));
						if (k == 0) {
							try {
								ModelImage m = sliceImages.get(p.index);
								m.saveImage(savedDirectory, "sample.xml",
										FileUtility.XML, false);
								VOI voiNew = m.getVOIs().elementAt(0);
								FileVOI fileVOI = new FileVOI("samplevoi"
										+ ".xml", savedDirectory, m);
								fileVOI.writeVOI(voiNew, true);
							} catch (IOException e) {
								e.printStackTrace();
							}
						}
					}

					groupIndex++;

				} else {
					// iteratively train
					int vecLen = imageList.size();
					Vector<ModelImage> trainList = new Vector<ModelImage>();
					Vector<Integer> resultList = new Vector<Integer>();
					trainList.add(sliceImages.get(imageList.get(0)));
					trainList.add(sliceImages.get(imageList.get(1)));
					resultList.add(imageList.get(0));
					resultList.add(imageList.get(1));
					C_AAMMODEL bestModel = null;
					int maxParameters = 0;
					for (int z = 2; z < vecLen; z++) {
						int imgNum = imageList.get(z);
						ModelImage img = sliceImages.get(imgNum);
						trainList.add(img);
						b = new CAAMConsoleModeB();
						boolean isSuccess = b.buildModel(trainList);
						if (isSuccess == false)
							continue;
						model = b.getModel();
						if (model.getNumberShapeParameters() >= 1) {
							if (model.getNumberShapeParameters() >= maxParameters) {
								maxParameters = model
										.getNumberShapeParameters();
								bestModel = model;
								resultList.add(imgNum);
							}
						} else {
							trainList.remove(img);
						}
					}

					if (bestModel != null) {
						System.err.println("extra number shape parameters = "
								+ bestModel.getNumberShapeParameters());

						System.err.print("extra group" + groupIndex + " = ");

						String savedDirectory = groupDirectory + File.separator
								+ "Group" + groupIndex + File.separator;

						File dir = new File(savedDirectory);
						if (!dir.exists()) {
							dir.mkdir();
							// System.err.println("saved dir = " + dir);
						}

						String modelName = "model";
						bestModel.WriteModel(savedDirectory + modelName);
						// mark the filled flag

						for (int k = 0; k < resultList.size(); k++) {
							int index = resultList.get(k);
							filled[index] = 1;
							System.err.print("\t" + index);
							// new ViewJFrameImage(sliceImages.get(p.index));
							try {
								if (k == 0) {
									ModelImage m = sliceImages.get(index);
									m.saveImage(savedDirectory, "sample.xml",
											FileUtility.XML, false);
									VOI voiNew = m.getVOIs().elementAt(0);
									FileVOI fileVOI = new FileVOI("samplevoi"
											+ ".xml", savedDirectory, m);
									fileVOI.writeVOI(voiNew, true);
								}
							} catch (IOException e) {
								e.printStackTrace();
							}

						}

						groupIndex++;
					}
				}
				System.err.println("\n\n");
				// pause();

			}// end if ( filled[i] != 1 )
		} // end of i

		System.err.println("end categorization");
	}

	/**
	 * Pauses the display until the user hits enter.
	 */
	public static void pause() {
		int count = 0;

		try {
			// eat any pending characters
			for (int av = System.in.available(); av > 0; av--) {
				System.in.read();
			}
			System.in.read();// wait for user to hit Enter, discard result
		} catch (IOException e) {
			System.err.println("keyboard failed: " + e);
		}

	}

	private void counterClockwise(float[] x, float[] y, float[] z, int nPts) {
		float[] x1 = new float[nPts];
		float[] y1 = new float[nPts];
		float[] z1 = new float[nPts];
		int index = 0;
		for (int i = nPts - 1; i >= 0; i--) {
			x1[index] = x[i];
			y1[index] = y[i];
			z1[index] = z[i];
			index++;
		}

		for (int i = 0; i < nPts; i++) {
			x[i] = x1[i];
			y[i] = y1[i];
			z[i] = z1[i];
		}

	}

	/**
	 * Texture bases similarity measure. Currently disabled.
	 */
	private void categorizeByTexture() {
		int len = sliceImages.size();
		double cost_CRS, cost_MIS, cost_NMIS, cost_NXS;
		Vector<perImageCost> costVector = null;

		for (int i = 0; i < 1; i++) {
			ModelImage currentImage = sliceImages.get(i);
			new ViewJFrameImage(currentImage);
			ModelSimpleImage simpleImg1 = new ModelSimpleImage(
					currentImage.getExtents(), currentImage.getFileInfo(0)
							.getResolutions(), currentImage);

			costVector = new Vector<perImageCost>();

			for (int j = 0; j < len; j++) {
				if (j != i) {

					ModelImage comparedImage = sliceImages.get(j);
					ModelSimpleImage simpleImg2 = new ModelSimpleImage(
							comparedImage.getExtents(), comparedImage
									.getFileInfo(0).getResolutions(),
							comparedImage);

					TransMatrix tMatrix = new TransMatrix(3);
					int costChoice;
					int bin = 256;
					float smoothSize = 1.0f;

					costChoice = AlgorithmCostFunctions2D.CORRELATION_RATIO_SMOOTHED;
					AlgorithmCostFunctions2D algoCost_CRS = new AlgorithmCostFunctions2D(
							simpleImg1, simpleImg2, costChoice, bin, smoothSize);
					cost_CRS = algoCost_CRS.cost(tMatrix);
					// System.err.println("cost_CRS = " + cost_CRS);
					algoCost_CRS.disposeLocal();
					algoCost_CRS = null;

					tMatrix = new TransMatrix(3);
					costChoice = AlgorithmCostFunctions2D.MUTUAL_INFORMATION_SMOOTHED;
					AlgorithmCostFunctions2D algoCost_MIS = new AlgorithmCostFunctions2D(
							simpleImg1, simpleImg2, costChoice, bin, smoothSize);
					cost_MIS = algoCost_MIS.cost(tMatrix);
					// System.err.println("cost_MIS = " + cost_MIS);
					algoCost_MIS.disposeLocal();
					algoCost_MIS = null;

					tMatrix = new TransMatrix(3);
					costChoice = AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED;
					AlgorithmCostFunctions2D algoCost_NMIS = new AlgorithmCostFunctions2D(
							simpleImg1, simpleImg2, costChoice, bin, smoothSize);
					cost_NMIS = algoCost_NMIS.cost(tMatrix);
					// System.err.println("cost_NMIS = " + cost_NMIS);
					algoCost_NMIS.disposeLocal();
					algoCost_NMIS = null;

					tMatrix = new TransMatrix(3);
					costChoice = AlgorithmCostFunctions2D.NORMALIZED_XCORRELATION_SMOOTHED;
					AlgorithmCostFunctions2D algoCost_NXS = new AlgorithmCostFunctions2D(
							simpleImg1, simpleImg2, costChoice, bin, smoothSize);
					cost_NXS = algoCost_NXS.cost(tMatrix);
					// System.err.println("cost_NXS = " + cost_NXS);
					algoCost_NXS.disposeLocal();
					algoCost_NXS = null;

					costVector.add(new perImageCost(j, cost_CRS, cost_MIS,
							cost_NMIS, cost_NXS));
				}
			} // end for j

			Collections.sort(costVector);
			len = costVector.size();
			System.err.println("test sort");
			for (int k = 0; k < len; k++) {
				perImageCost p = costVector.get(k);
				System.err.println(p.index + "\t" + p.cost_CRS + "\t"
						+ p.cost_MIS + "\t" + p.cost_NMIS + "\t" + p.cost_NXS);
				new ViewJFrameImage(sliceImages.get(p.index));
			}

		} // end for i
	}

	/**
	 * Read image and corresponding VOI.
	 */
	private void readImagesAndVOIs() {

		int len = imageVector.size();
		System.err.println("f len = " + len);
		int index;
		int i = 0;
		try {
			for (i = 0; i < len; i++) {

				// read key image
				String dir = imageVector.get(i);
				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1,
						dir.length()));

				// System.err.println("Key Image: fileName = " + fileName +
				// "  directory = " + directory);
				FileIO keyImageIO = new FileIO();
				sliceImages.add(i, keyImageIO.readImage(fileName, directory));

				// read corresponding VOI
				String voiDir = voiVector.get(i);
				index = voiDir.lastIndexOf(File.separator);
				String voiDirectory = new String(voiDir.substring(0, index + 1));
				String voiFileName = new String(voiDir.substring(index + 1,
						voiDir.length()));

				// System.err.println("voiDirectory = " + voiDirectory +
				// "  voiFileName = " + voiFileName);

				FileVOI fileVOI = null;
				fileVOI = new FileVOI(voiFileName, voiDirectory,
						sliceImages.get(i));
				System.err.println("sliceImages.get(i) = " + sliceImages.get(i).getImageName());
				imageVOIs.add(i, fileVOI.readVOI(false));
				sliceImages.get(i).registerVOI(imageVOIs.get(i)[0]);
                System.err.println("i = " + i);
				//new ViewJFrameImage(sliceImages.get(j));
			} // end for j loop
		} catch (Exception e) {
			    System.err.println("error i = " + i);
			    System.err.println("sliceImages.get(i) = " + sliceImages.get(i).getImageName());
                e.printStackTrace();
		}
	}

	/**
	 * Read image and VOI in reverse order.
	 */
	private void readImagesAndVOIsInReverse() {

		int len = imageVector.size();
		int index;
		try {
			for (int j = len - 1; j > 0; j--) {

				// read key image
				String dir = imageVector.get(j);
				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1,
						dir.length()));

				// System.err.println("Key Image: fileName = " + fileName +
				// "  directory = " + directory);
				FileIO keyImageIO = new FileIO();
				sliceImages.add(len - 1 - j,
						keyImageIO.readImage(fileName, directory));

				// read corresponding VOI
				String voiDir = voiVector.get(j);
				index = voiDir.lastIndexOf(File.separator);
				String voiDirectory = new String(voiDir.substring(0, index + 1));
				String voiFileName = new String(voiDir.substring(index + 1,
						voiDir.length()));

				// System.err.println("voiDirectory = " + voiDirectory +
				// "  voiFileName = " + voiFileName);

				FileVOI fileVOI = null;
				fileVOI = new FileVOI(voiFileName, voiDirectory,
						sliceImages.get(len - 1 - j));
				imageVOIs.add(len - 1 - j, fileVOI.readVOI(false));
				sliceImages.get(len - 1 - j).registerVOI(
						imageVOIs.get(len - 1 - j)[0]);

				// new ViewJFrameImage(sliceImages.get(j));
			} // end for j loop
		} catch (Exception e) {

		}
	}

	/**
	 * Read image and VOIs start from the mid slice.
	 */
	private void readImagesAndVOIsFromMid() {

		int len = imageVector.size();
		int oneThird = (int) len / 3;
		int twoThird = (int) len * (2 / 3);
		int index;
		int count = 0;
		try {
			for (int j = oneThird; j < twoThird; j++) {

				// read key image
				String dir = imageVector.get(j);
				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1,
						dir.length()));

				// System.err.println("Key Image: fileName = " + fileName +
				// "  directory = " + directory);
				FileIO keyImageIO = new FileIO();
				sliceImages.add(count,
						keyImageIO.readImage(fileName, directory));

				// read corresponding VOI
				String voiDir = voiVector.get(j);
				index = voiDir.lastIndexOf(File.separator);
				String voiDirectory = new String(voiDir.substring(0, index + 1));
				String voiFileName = new String(voiDir.substring(index + 1,
						voiDir.length()));

				// System.err.println("voiDirectory = " + voiDirectory +
				// "  voiFileName = " + voiFileName);

				FileVOI fileVOI = null;
				fileVOI = new FileVOI(voiFileName, voiDirectory,
						sliceImages.get(count));
				imageVOIs.add(count, fileVOI.readVOI(false));
				sliceImages.get(count).registerVOI(imageVOIs.get(count)[0]);

				count++;
				// new ViewJFrameImage(sliceImages.get(j));
			} // end for j loop

			for (int j = twoThird; j < len; j++) {

				// read key image
				String dir = imageVector.get(j);
				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1,
						dir.length()));

				// System.err.println("Key Image: fileName = " + fileName +
				// "  directory = " + directory);
				FileIO keyImageIO = new FileIO();
				sliceImages.add(count,
						keyImageIO.readImage(fileName, directory));

				// read corresponding VOI
				String voiDir = voiVector.get(j);
				index = voiDir.lastIndexOf(File.separator);
				String voiDirectory = new String(voiDir.substring(0, index + 1));
				String voiFileName = new String(voiDir.substring(index + 1,
						voiDir.length()));

				// System.err.println("voiDirectory = " + voiDirectory +
				// "  voiFileName = " + voiFileName);

				FileVOI fileVOI = null;
				fileVOI = new FileVOI(voiFileName, voiDirectory,
						sliceImages.get(count));
				imageVOIs.add(count, fileVOI.readVOI(false));
				sliceImages.get(count).registerVOI(imageVOIs.get(count)[0]);

				count++;
				// new ViewJFrameImage(sliceImages.get(j));
			} // end for j loop

			for (int j = 0; j < oneThird; j++) {

				// read key image
				String dir = imageVector.get(j);
				index = dir.lastIndexOf(File.separator);
				String directory = new String(dir.substring(0, index + 1));
				String fileName = new String(dir.substring(index + 1,
						dir.length()));

				// System.err.println("Key Image: fileName = " + fileName +
				// "  directory = " + directory);
				FileIO keyImageIO = new FileIO();
				sliceImages.add(count,
						keyImageIO.readImage(fileName, directory));

				// read corresponding VOI
				String voiDir = voiVector.get(j);
				index = voiDir.lastIndexOf(File.separator);
				String voiDirectory = new String(voiDir.substring(0, index + 1));
				String voiFileName = new String(voiDir.substring(index + 1,
						voiDir.length()));

				// System.err.println("voiDirectory = " + voiDirectory +
				// "  voiFileName = " + voiFileName);

				FileVOI fileVOI = null;
				fileVOI = new FileVOI(voiFileName, voiDirectory,
						sliceImages.get(count));
				imageVOIs.add(count, fileVOI.readVOI(false));
				sliceImages.get(count).registerVOI(imageVOIs.get(count)[0]);

				count++;
				// new ViewJFrameImage(sliceImages.get(j));
			} // end for j loop

		} catch (Exception e) {

		}
	}

}