
import javax.swing.JComboBox;
import WildMagic.LibFoundation.Mathematics.Vector3f;
import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmTransform;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoNIFTI;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.MatrixHolder;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;
import gov.nih.mipav.util.MipavCoordinateSystems;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.util.Iterator;
import java.util.LinkedHashMap;

import gov.nih.mipav.model.algorithms.AlgorithmInterface;

/**
 * PlugIn for Reorient
 *
 *
 * @version April 2006
 * @author Pilou Bazin
 *
 */
public class PlugInAlgorithmReorient extends AlgorithmBase implements AlgorithmInterface {

	private AlgorithmTransform algoTrans = null;
	private ModelImage image; // source image
	private ModelImage resultImage = null; // result image

	public static final int AXIAL_INDEX = 0;

	public static final int CORONAL_INDEX = 1;

	public static final int SAGITTAL_INDEX = 2;

	public static final int USER_INDEX = 3;

	// parameters
	private String[] orientTypes = { "Axial", "Coronal", "Sagittal", "Unknown" };
	private String[] newOrientTypes = { "Axial", "Coronal", "Sagittal", "User defined" };

	private String[] resolutionTypes = { "Unchanged", "Finest Cubic", "Coarsest Cubic", "Same as template" };
	private int resolutionIndex = 0;

	private String[] interpTypes = { "Nearest Neighbor", "Trilinear", "Bspline 3rd order", "Bspline 4th order",
			"Cubic Lagrangian", "Quintic Lagrangian", "Heptic Lagrangian", "Windowed Sinc" };
	private String interpType = "Trilinear";
	private String[] orients = { "Unknown", "Patient Right to Left", "Patient Left to Right",
			"Patient Posterior to Anterior", "Patient Anterior to Posterior", "Patient Inferior to Superior",
			"Patient Superior to Inferior" };

	// storage for header information
	FileInfoBase fileInfo;
	FileInfoNIFTI fileInfoNIFTI;

	// dialog elements
	private JComboBox comboResType;
	private JComboBox comboInterpType;
	private JComboBox presentOrientBoxX;
	private JComboBox presentOrientBoxY;
	private JComboBox presentOrientBoxZ;
	private int[] or = new int[3];
	private JComboBox newOrientBox;
	private JComboBox newOrientBoxX;
	private JComboBox newOrientBoxY;
	private JComboBox newOrientBoxZ;
	private int[] newOr = new int[3];
	private int[] axisOrder = new int[3];
	private boolean[] axisFlip = new boolean[3];

	/**
	 * constructor
	 * 
	 * @param ModelImage
	 *            src image
	 */
	public PlugInAlgorithmReorient(ModelImage im) {
		image = im;
		init();
	}

	/**
	 * Sets up the GUI (panels, buttons, etc) and displays it on the screen.
	 */
	private void init() {
		int i;
		boolean isDicom = false;

		float rx = image.getFileInfo()[0].getResolutions()[0];
		float ry = image.getFileInfo()[0].getResolutions()[1];
		float rz = image.getFileInfo()[0].getResolutions()[2];
		float minResolution = Math.min(rx, Math.min(ry, rz));
		float maxResolution = Math.max(rx, Math.max(ry, rz));
		
		presentOrientBoxX = new JComboBox(orients);
		for (i = 0; i <= 2; i++) {
			or[i] = image.getFileInfo()[0].getAxisOrientation()[i];
		}
		presentOrientBoxX.setSelectedIndex(or[0]);

		presentOrientBoxY = new JComboBox(orients);
		presentOrientBoxY.setSelectedIndex(or[1]);

		presentOrientBoxZ = new JComboBox(orients);
		presentOrientBoxZ.setSelectedIndex(or[2]);

		newOrientBox = new JComboBox(newOrientTypes);
		for (i = 0; i <= 2; i++) {
			newOr[i] = image.getFileInfo()[0].getAxisOrientation()[i];
		}
		if ((newOr[0] == FileInfoBase.ORI_R2L_TYPE) && (newOr[1] == FileInfoBase.ORI_A2P_TYPE)
				&& (newOr[2] == FileInfoBase.ORI_I2S_TYPE)) {
			isDicom = true;
			newOrientBox.setSelectedIndex(AXIAL_INDEX);
		} else if ((newOr[0] == FileInfoBase.ORI_R2L_TYPE) && (newOr[1] == FileInfoBase.ORI_S2I_TYPE)
				&& (newOr[2] == FileInfoBase.ORI_A2P_TYPE)) {
			isDicom = true;
			newOrientBox.setSelectedIndex(CORONAL_INDEX);
		} else if ((newOr[0] == FileInfoBase.ORI_A2P_TYPE) && (newOr[1] == FileInfoBase.ORI_S2I_TYPE)
				&& (newOr[2] == FileInfoBase.ORI_R2L_TYPE)) {
			isDicom = true;
			newOrientBox.setSelectedIndex(SAGITTAL_INDEX);
		} else {
			newOrientBox.setSelectedIndex(USER_INDEX);
		}

		newOrientBoxX = new JComboBox(orients);
		newOrientBoxX.setSelectedIndex(newOr[0]);
		if (isDicom) {
			newOrientBoxX.setEnabled(false);
		}

		newOrientBoxY = new JComboBox(orients);
		newOrientBoxY.setSelectedIndex(newOr[1]);
		if (isDicom) {
			newOrientBoxY.setEnabled(false);
		}

		newOrientBoxZ = new JComboBox(orients);
		newOrientBoxZ.setSelectedIndex(newOr[2]);
		if (isDicom) {
			newOrientBoxZ.setEnabled(false);
		}

		resolutionTypes[1] = "Finest Cubic " + Float.toString(minResolution);
		resolutionTypes[2] = "Coarsest Cubic " + Float.toString(maxResolution);
		comboResType = new JComboBox(resolutionTypes);

		comboInterpType = new JComboBox(interpTypes);

	} // end init()
	
	public void set_axial_orientation() {
		newOrientBoxX.setSelectedIndex(FileInfoBase.ORI_R2L_TYPE);
		newOrientBoxX.setEnabled(false);
		newOrientBoxY.setSelectedIndex(FileInfoBase.ORI_A2P_TYPE);
		newOrientBoxY.setEnabled(false);
		newOrientBoxZ.setSelectedIndex(FileInfoBase.ORI_I2S_TYPE);
		newOrientBoxZ.setEnabled(false);
	}

	public void set_sagittal_orientation() {
		newOrientBoxX.setSelectedIndex(FileInfoBase.ORI_A2P_TYPE);
		newOrientBoxX.setEnabled(false);
		newOrientBoxY.setSelectedIndex(FileInfoBase.ORI_S2I_TYPE);
		newOrientBoxY.setEnabled(false);
		newOrientBoxZ.setSelectedIndex(FileInfoBase.ORI_R2L_TYPE);
		newOrientBoxZ.setEnabled(false);
	}

	public void set_coronal_orientation() {
		newOrientBoxX.setSelectedIndex(FileInfoBase.ORI_R2L_TYPE);
		newOrientBoxX.setEnabled(false);
		newOrientBoxY.setSelectedIndex(FileInfoBase.ORI_S2I_TYPE);
		newOrientBoxY.setEnabled(false);
		newOrientBoxZ.setSelectedIndex(FileInfoBase.ORI_A2P_TYPE);
		newOrientBoxZ.setEnabled(false);
	}

	public void doRun() {
		if (setVariables()) {
			runAlgorithm();
		}
	}

	/**
	 * Accessor that returns the image.
	 * 
	 * @return The result image.
	 */
	public ModelImage getResultImage() {
		return resultImage;
	}

	/**
	 * Use the GUI results to set up the variables needed to run the algorithm.
	 * 
	 * @return <code>true</code> if parameters set successfully, <code>false</code>
	 *         otherwise.
	 */
	private boolean setVariables() {
		int rl, is, ap;
		int i;

		or[0] = presentOrientBoxX.getSelectedIndex();
		or[1] = presentOrientBoxY.getSelectedIndex();
		or[2] = presentOrientBoxZ.getSelectedIndex();

		rl = 0;
		ap = 0;
		is = 0;
		for (i = 0; i <= 2; i++) {
			if ((or[i] == FileInfoBase.ORI_L2R_TYPE) || (or[i] == FileInfoBase.ORI_R2L_TYPE)) {
				rl++;
			} else if ((or[i] == FileInfoBase.ORI_A2P_TYPE) || (or[i] == FileInfoBase.ORI_P2A_TYPE)) {
				ap++;
			} else if ((or[i] == FileInfoBase.ORI_I2S_TYPE) || (or[i] == FileInfoBase.ORI_S2I_TYPE)) {
				is++;
			}
		}
		if ((rl != 1) || (ap != 1) || (is != 1)) {
			MipavUtil.displayError("Error! Present orientation must have one RL, one AP, and one IS axis");
			return false;
		}

		newOr[0] = newOrientBoxX.getSelectedIndex();
		newOr[1] = newOrientBoxY.getSelectedIndex();
		newOr[2] = newOrientBoxZ.getSelectedIndex();

		rl = 0;
		ap = 0;
		is = 0;
		for (i = 0; i <= 2; i++) {
			if ((newOr[i] == FileInfoBase.ORI_L2R_TYPE) || (newOr[i] == FileInfoBase.ORI_R2L_TYPE)) {
				rl++;
			} else if ((newOr[i] == FileInfoBase.ORI_A2P_TYPE) || (newOr[i] == FileInfoBase.ORI_P2A_TYPE)) {
				ap++;
			} else if ((newOr[i] == FileInfoBase.ORI_I2S_TYPE) || (newOr[i] == FileInfoBase.ORI_S2I_TYPE)) {
				is++;
			}
		}
		if ((rl != 1) || (ap != 1) || (is != 1)) {
			MipavUtil.displayError("Error! New orientation must have one RL, one AP, and one IS axis");
			return false;
		}
		resolutionIndex = comboResType.getSelectedIndex();
		interpType = (String) comboInterpType.getSelectedItem();

		// String name = (String) comboTemplate.getSelectedItem();
		// template = ViewUserInterface.getReference().getRegisteredImageByName(name);

		// System.out.println(getParameterString("|"));

		return true;
	} // end setVariables()

	public void runAlgorithm() {
		int i, j;
		boolean found;
		int newOrient;
		float ri[] = new float[3];
		int ni[] = new int[3];
		float r0[] = new float[3];
		int n0[] = new int[3];
		float org[] = new float[3];
		float origin[];
		float newOrigin[];
		float originalOr[] = new float[3];
		float flippedOr[] = new float[3];
		float xOr = 0.0f;
		float yOr = 0.0f;
		;
		float zOr = 0.0f;
		Vector3f position;
		Vector3f out;

		if (image.getFileInfo(0) instanceof FileInfoNIFTI) {
			fileInfoNIFTI = (FileInfoNIFTI) (image.getFileInfo()[0].clone());
		} else {
			fileInfo = (FileInfoBase) (image.getFileInfo()[0].clone());
		}

		// set resampled resolutions, dimensions
		ri[0] = image.getFileInfo()[0].getResolutions()[0];
		ri[1] = image.getFileInfo()[0].getResolutions()[1];
		ri[2] = image.getFileInfo()[0].getResolutions()[2];

		ni[0] = image.getExtents()[0];
		ni[1] = image.getExtents()[1];
		ni[2] = image.getExtents()[2];

		origin = image.getFileInfo()[0].getOrigin();
		newOrigin = origin.clone();

		float r[] = new float[3];
		int n[] = new int[3];
		for (i = 0; i <= 2; i++) {
			r[i] = ri[i];
			n[i] = ni[i];
		}

		if (resolutionIndex == 1) {
			// Finest cubic
			float rn = Math.min(r[0], Math.min(r[1], r[2]));
			n[0] = (int) Math.ceil(n[0] * r[0] / rn);
			r[0] = rn;
			n[1] = (int) Math.ceil(n[1] * r[1] / rn);
			r[1] = rn;
			n[2] = (int) Math.ceil(n[2] * r[2] / rn);
			r[2] = rn;
		} else if (resolutionIndex == 2) {
			// Coarsest cubic
			float rn = Math.max(r[0], Math.max(r[1], r[2]));
			n[0] = (int) Math.ceil(n[0] * r[0] / rn);
			r[0] = rn;
			n[1] = (int) Math.ceil(n[1] * r[1] / rn);
			r[1] = rn;
			n[2] = (int) Math.ceil(n[2] * r[2] / rn);
			r[2] = rn;
		} else if (resolutionIndex == 3) {
			// Same as template
			r[0] = image.getFileInfo()[0].getResolutions()[0];
			r[1] = image.getFileInfo()[0].getResolutions()[1];
			r[2] = image.getFileInfo()[0].getResolutions()[2];
			n[0] = image.getExtents()[0];
			n[1] = image.getExtents()[1];
			n[2] = image.getExtents()[2];
		}

		double X[][] = new double[4][4];
		for (j = 0; j <= 2; j++) {
			switch (or[j]) {
			case FileInfoBase.ORI_R2L_TYPE:
				found = false;
				for (i = 0; (i <= 2) && (!found); i++) {
					if (newOr[i] == FileInfoBase.ORI_R2L_TYPE) {
						axisOrder[i] = j;
						axisFlip[i] = false;
						found = true;
						X[i][j] = 1.0;
						r0[i] = r[j];
						n0[i] = n[j];
					} else if (newOr[i] == FileInfoBase.ORI_L2R_TYPE) {
						axisOrder[i] = j;
						axisFlip[i] = true;
						found = true;
						X[i][j] = -1.0;
						X[i][3] = ri[j] * (ni[j] - 1);
						r0[i] = r[j];
						n0[i] = n[j];
					}
				}
				break;
			case FileInfoBase.ORI_L2R_TYPE:
				found = false;
				for (i = 0; (i <= 2) && (!found); i++) {
					if (newOr[i] == FileInfoBase.ORI_L2R_TYPE) {
						axisOrder[i] = j;
						axisFlip[i] = false;
						found = true;
						X[i][j] = 1.0;
						r0[i] = r[j];
						n0[i] = n[j];
					} else if (newOr[i] == FileInfoBase.ORI_R2L_TYPE) {
						axisOrder[i] = j;
						axisFlip[i] = true;
						found = true;
						X[i][j] = -1.0;
						X[i][3] = ri[j] * (ni[j] - 1);
						r0[i] = r[j];
						n0[i] = n[j];
					}
				}
				break;
			case FileInfoBase.ORI_A2P_TYPE:
				found = false;
				for (i = 0; (i <= 2) && (!found); i++) {
					if (newOr[i] == FileInfoBase.ORI_A2P_TYPE) {
						axisOrder[i] = j;
						axisFlip[i] = false;
						found = true;
						X[i][j] = 1.0;
						r0[i] = r[j];
						n0[i] = n[j];
					} else if (newOr[i] == FileInfoBase.ORI_P2A_TYPE) {
						axisOrder[i] = j;
						axisFlip[i] = true;
						found = true;
						X[i][j] = -1.0;
						X[i][3] = ri[j] * (ni[j] - 1);
						r0[i] = r[j];
						n0[i] = n[j];
					}
				}
				break;
			case FileInfoBase.ORI_P2A_TYPE:
				found = false;
				for (i = 0; (i <= 2) && (!found); i++) {
					if (newOr[i] == FileInfoBase.ORI_P2A_TYPE) {
						axisOrder[i] = j;
						axisFlip[i] = false;
						found = true;
						X[i][j] = 1.0;
						r0[i] = r[j];
						n0[i] = n[j];
					} else if (newOr[i] == FileInfoBase.ORI_A2P_TYPE) {
						axisOrder[i] = j;
						axisFlip[i] = true;
						found = true;
						X[i][j] = -1.0;
						X[i][3] = ri[j] * (ni[j] - 1);
						r0[i] = r[j];
						n0[i] = n[j];
					}
				}
				break;
			case FileInfoBase.ORI_I2S_TYPE:
				found = false;
				for (i = 0; (i <= 2) && (!found); i++) {
					if (newOr[i] == FileInfoBase.ORI_I2S_TYPE) {
						axisOrder[i] = j;
						axisFlip[i] = false;
						found = true;
						X[i][j] = 1.0;
						r0[i] = r[j];
						n0[i] = n[j];
					} else if (newOr[i] == FileInfoBase.ORI_S2I_TYPE) {
						axisOrder[i] = j;
						axisFlip[i] = true;
						found = true;
						X[i][j] = -1.0;
						X[i][3] = ri[j] * (ni[j] - 1);
						r0[i] = r[j];
						n0[i] = n[j];
					}
				}
				break;
			case FileInfoBase.ORI_S2I_TYPE:
				found = false;
				for (i = 0; (i <= 2) && (!found); i++) {
					if (newOr[i] == FileInfoBase.ORI_S2I_TYPE) {
						axisOrder[i] = j;
						axisFlip[i] = false;
						found = true;
						X[i][j] = 1.0;
						r0[i] = r[j];
						n0[i] = n[j];
					} else if (newOr[i] == FileInfoBase.ORI_I2S_TYPE) {
						axisOrder[i] = j;
						axisFlip[i] = true;
						found = true;
						X[i][j] = -1.0;
						X[i][3] = ri[j] * (ni[j] - 1);
						r0[i] = r[j];
						n0[i] = n[j];
					}
				}
				break;
			}
		} // for (j = 0; j <= 2; j++)

		for (i = 0; i <= 2; i++) {
			if (image.getFileInfo(0) instanceof FileInfoNIFTI) {
				fileInfoNIFTI.setResolutions(r0[i], i);
				fileInfoNIFTI.setExtents(n0[i], i);
				fileInfoNIFTI.setAxisOrientation(newOr[i], i);
			} else {
				fileInfo.setResolutions(r0[i], i);
				fileInfo.setExtents(n0[i], i);
				fileInfo.setAxisOrientation(newOr[i], i);
			}
		}

		for (i = 0; i < 3; i++) {
			if (i == 0) {
				originalOr[0] = 0.0f;
				flippedOr[0] = ni[0] - 1;
			} else if (i == 1) {
				originalOr[1] = 0.0f;
				flippedOr[1] = ni[1] - 1;
			} else {
				originalOr[2] = 0.0f;
				flippedOr[2] = ni[2] - 1;
			}
		}

		for (i = 0; i < 3; i++) {
			if (axisFlip[i]) {
				if (axisOrder[i] == 0) {
					xOr = flippedOr[0];
				} else if (axisOrder[i] == 1) {
					yOr = flippedOr[1];
				} else {
					zOr = flippedOr[2];
				}
			} else {
				if (axisOrder[i] == 0) {
					xOr = originalOr[0];
				} else if (axisOrder[i] == 1) {
					yOr = originalOr[1];
				} else {
					zOr = originalOr[2];
				}
			}
		}

		position = new Vector3f(xOr, yOr, zOr);
		out = new Vector3f(position);
		MipavCoordinateSystems.fileToScanner(position, out, image);
		for (i = 0; i < 3; i++) {
			if ((or[i] == FileInfoBase.ORI_R2L_TYPE) || (or[i] == FileInfoBase.ORI_L2R_TYPE)) {
				org[i] = out.X;
			} else if ((or[i] == FileInfoBase.ORI_A2P_TYPE) || (or[i] == FileInfoBase.ORI_P2A_TYPE)) {
				org[i] = out.Y;
			} else {
				org[i] = out.Z;
			}
		}

		for (i = 0; i < 3; i++) {
			newOrigin[i] = org[axisOrder[i]];
			if (Math.abs(newOrigin[i]) < .000001f) {
				newOrigin[i] = 0f;
			}
		}

		if (image.getFileInfo(0) instanceof FileInfoNIFTI) {
			fileInfoNIFTI.setOrigin(newOrigin);
		} else {
			fileInfo.setOrigin(newOrigin);
		}

		if ((newOr[2] == FileInfoBase.ORI_I2S_TYPE) || (newOr[2] == FileInfoBase.ORI_S2I_TYPE)) {
			newOrient = FileInfoBase.AXIAL;
		} else if ((newOr[2] == FileInfoBase.ORI_A2P_TYPE) || (newOr[2] == FileInfoBase.ORI_P2A_TYPE)) {
			newOrient = FileInfoBase.CORONAL;
		} else if ((newOr[2] == FileInfoBase.ORI_L2R_TYPE) || (newOr[2] == FileInfoBase.ORI_R2L_TYPE)) {
			newOrient = FileInfoBase.SAGITTAL;
		} else {
			newOrient = FileInfoBase.UNKNOWN_ORIENT;
		}
		if (image.getFileInfo(0) instanceof FileInfoNIFTI) {
			fileInfoNIFTI.setImageOrientation(newOrient);
		} else {
			fileInfo.setImageOrientation(newOrient);
		}

		TransMatrix transform = new TransMatrix(4);
		transform.setMatrix(0, 2, 0, 3, X);

		// System.out.println(transform.toString());

		int interp = AlgorithmTransform.TRILINEAR;
		if (interpType.equals("Nearest Neighbor")) {
			interp = AlgorithmTransform.NEAREST_NEIGHBOR;
		} else if (interpType.equals("Trilinear")) {
			interp = AlgorithmTransform.TRILINEAR;
		} else if (interpType.equals("Bspline 3rd order")) {
			interp = AlgorithmTransform.BSPLINE3;
		} else if (interpType.equals("Bspline 4th order")) {
			interp = AlgorithmTransform.BSPLINE4;
		} else if (interpType.equals("Cubic Lagrangian")) {
			interp = AlgorithmTransform.CUBIC_LAGRANGIAN;
		} else if (interpType.equals("Quintic Lagrangian")) {
			interp = AlgorithmTransform.QUINTIC_LAGRANGIAN;
		} else if (interpType.equals("Heptic Lagrangian")) {
			interp = AlgorithmTransform.HEPTIC_LAGRANGIAN;
		} else if (interpType.equals("Windowed Sinc")) {
			interp = AlgorithmTransform.WSINC;
		}

		algoTrans = new AlgorithmTransform(image, transform, interp, r0[0], r0[1], r0[2], n0[0], n0[1], n0[2], true,
				false, false);
		algoTrans.setUpdateOriginFlag(true);

		// This is very important. Adding this object as a listener allows
		// the algorithm to notify this object when it has completed of failed.
		// See algorithm performed event. This is made possible by implementing
		algoTrans.addListener(this);

		algoTrans.run();

	} // end callAlgorithm()

	/**
	 * This method is required if the AlgorithmPerformed interface is implemented.
	 * It is called by the algorithm when it has completed or failed to to complete,
	 * so that the dialog can be display the result image and/or clean up.
	 * 
	 * @param algorithm
	 *            Algorithm that caused the event.
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {

		if (algorithm instanceof AlgorithmTransform) {
			TransMatrix newMatrix = null;
			TransMatrix newMatrix2 = null;
			resultImage = algoTrans.getTransformedImage();
			if (algorithm.isCompleted() == true && resultImage != null) {
				// The algorithm has completed and produced a new image to be displayed.
				if (image.getFileInfo()[0].getFileFormat() == FileUtility.NIFTI) {
					fileInfoNIFTI.setMin(resultImage.getMin());
					fileInfoNIFTI.setMax(resultImage.getMax());
					if (resultImage.getNDims() == 3) {
						for (int i = 0; i < resultImage.getExtents()[2]; i++) {
							resultImage.setFileInfo((FileInfoNIFTI) fileInfoNIFTI.clone(), i);
						}
					} else if (resultImage.getNDims() == 4) {
						for (int i = 0; i < resultImage.getExtents()[2] * resultImage.getExtents()[3]; i++) {
							resultImage.setFileInfo((FileInfoNIFTI) fileInfoNIFTI.clone(), i);
						}
					}
				} else {
					JDialogBase.updateFileInfoStatic(fileInfo, resultImage);
				}
				if (resultImage.getNDims() >= 3) {
					// Update any destImage NIFTI matrices
					MatrixHolder matHolder = null;
					int i;
					int j;
					int t;
					int index;
					int tDim;
					matHolder = resultImage.getMatrixHolder();

					float loc;
					int orient;

					if (matHolder != null) {

						LinkedHashMap<String, TransMatrix> matrixMap = matHolder.getMatrixMap();
						Iterator<String> iter = matrixMap.keySet().iterator();
						String nextKey = null;

						TransMatrix tempMatrix = null;

						while (iter.hasNext()) {
							nextKey = iter.next();
							tempMatrix = matrixMap.get(nextKey);
							if (tempMatrix.isNIFTI()) {
								if (newMatrix == null) {
									newMatrix = new TransMatrix(4);
									for (i = 0; i < 3; i++) {
										for (j = 0; j < 3; j++) {
											if (axisFlip[i]) {
												newMatrix.set(j, i, -tempMatrix.get(j, axisOrder[i]));
											} else {
												newMatrix.set(j, i, tempMatrix.get(j, axisOrder[i]));
											}
										}
										loc = tempMatrix.get(axisOrder[i], 3);
										if (axisFlip[i]) {
											orient = image.getFileInfo(0).getAxisOrientation(axisOrder[i]);
											if ((orient == FileInfoBase.ORI_R2L_TYPE)
													|| (orient == FileInfoBase.ORI_A2P_TYPE)
													|| (orient == FileInfoBase.ORI_I2S_TYPE)) {
												loc = loc + ((image.getFileInfo(0).getExtents()[axisOrder[i]] - 1)
														* image.getFileInfo(0).getResolutions()[axisOrder[i]]);
											} else {
												loc = loc - ((image.getFileInfo(0).getExtents()[axisOrder[i]] - 1)
														* image.getFileInfo(0).getResolutions()[axisOrder[i]]);
											}
										}
										newMatrix.set(i, 3, loc);
									} // for (i = 0; i < 3; i++)
									tempMatrix.Copy(newMatrix);
									if (image.getFileInfo(0) instanceof FileInfoNIFTI) {
										if (tempMatrix.isQform()) {
											if (resultImage.getNDims() == 3) {
												for (i = 0; i < resultImage.getExtents()[2]; i++) {
													((FileInfoNIFTI) resultImage.getFileInfo(i)).setMatrixQ(newMatrix);
												}
											} else if (resultImage.getNDims() == 4) {
												for (i = 0; i < resultImage.getExtents()[2]
														* resultImage.getExtents()[3]; i++) {
													((FileInfoNIFTI) resultImage.getFileInfo(i)).setMatrixQ(newMatrix);
												}
											}
										} // if (tempMatrix.isQform())
										else { // tempMatrix is sform
											if (resultImage.getNDims() == 3) {
												for (i = 0; i < resultImage.getExtents()[2]; i++) {
													((FileInfoNIFTI) resultImage.getFileInfo(i)).setMatrixS(newMatrix);
												}
											} else if (resultImage.getNDims() == 4) {
												for (i = 0; i < resultImage.getExtents()[2]
														* resultImage.getExtents()[3]; i++) {
													((FileInfoNIFTI) resultImage.getFileInfo(i)).setMatrixS(newMatrix);
												}
											}
										} // else tempMatrix is sform
									} // if (destImage.getFileInfo(0) instanceof FileInfoNIFTI)
								} // if (newMatrix == null)
								else {
									newMatrix2 = new TransMatrix(4);
									for (i = 0; i < 3; i++) {
										for (j = 0; j < 3; j++) {
											if (axisFlip[i]) {
												newMatrix2.set(j, i, -tempMatrix.get(j, axisOrder[i]));
											} else {
												newMatrix2.set(j, i, tempMatrix.get(j, axisOrder[i]));
											}
										}
										loc = tempMatrix.get(axisOrder[i], 3);
										if (axisFlip[i]) {
											orient = image.getFileInfo(0).getAxisOrientation(axisOrder[i]);
											if ((orient == FileInfoBase.ORI_R2L_TYPE)
													|| (orient == FileInfoBase.ORI_A2P_TYPE)
													|| (orient == FileInfoBase.ORI_I2S_TYPE)) {
												loc = loc + ((image.getFileInfo(0).getExtents()[axisOrder[i]] - 1)
														* image.getFileInfo(0).getResolutions()[axisOrder[i]]);
											} else {
												loc = loc - ((image.getFileInfo(0).getExtents()[axisOrder[i]] - 1)
														* image.getFileInfo(0).getResolutions()[axisOrder[i]]);
											}
										}
										newMatrix2.set(i, 3, loc);
									} // for (i = 0; i < 3; i++)
									tempMatrix.Copy(newMatrix2);
									if (image.getFileInfo(0) instanceof FileInfoNIFTI) {
										if (tempMatrix.isQform()) {
											if (resultImage.getNDims() == 3) {
												for (i = 0; i < resultImage.getExtents()[2]; i++) {
													((FileInfoNIFTI) resultImage.getFileInfo(i)).setMatrixQ(newMatrix2);
												}
											} else if (resultImage.getNDims() == 4) {
												for (i = 0; i < resultImage.getExtents()[2]
														* resultImage.getExtents()[3]; i++) {
													((FileInfoNIFTI) resultImage.getFileInfo(i)).setMatrixQ(newMatrix2);
												}
											}
										} // if (tempMatrix.isQform())
										else { // tempMatrix is sform
											if (resultImage.getNDims() == 3) {
												for (i = 0; i < resultImage.getExtents()[2]; i++) {
													((FileInfoNIFTI) resultImage.getFileInfo(i)).setMatrixS(newMatrix2);
												}
											} else if (resultImage.getNDims() == 4) {
												for (i = 0; i < resultImage.getExtents()[2]
														* resultImage.getExtents()[3]; i++) {
													((FileInfoNIFTI) resultImage.getFileInfo(i)).setMatrixS(newMatrix2);
												}
											}
										} // else tempMatrix is sform
									} // if (destImage.getFileInfo(0) instanceof FileInfoNIFTI)
								}
							} // if (tempMatrix.isNIFTI())
						}
						if (newMatrix != null) {
							matHolder.clearMatrices();
							matHolder.addMatrix(newMatrix);
							if (newMatrix2 != null) {
								matHolder.addMatrix(newMatrix2);
							}
						}
					} // if (matHolder != null)

					if ((image.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL))
							|| (image.getFileInfo()[0].getFileFormat() == FileUtility.DICOM)) {
						TransMatrix dicomMatrix = null;
						dicomMatrix = image.getMatrix();
						newMatrix = new TransMatrix(4);
						for (i = 0; i < 3; i++) {
							for (j = 0; j < 3; j++) {
								if (axisFlip[i]) {
									newMatrix.set(j, i, -dicomMatrix.get(j, axisOrder[i]));
								} else {
									newMatrix.set(j, i, dicomMatrix.get(j, axisOrder[i]));
								}
							}
						} // for (i = 0; i < 3; i++)
						newMatrix.setTransformID(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL);
						resultImage.getMatrixHolder().clearMatrices();
						resultImage.getMatrixHolder().addMatrix(newMatrix);
						if (resultImage.getNDims() >= 4) {
							tDim = resultImage.getExtents()[3];
						} else {
							tDim = 1;
						}

						for (t = 0; t < tDim; t++) {
							for (i = 0; i < resultImage.getExtents()[2]; i++) {
								index = i + t * resultImage.getExtents()[2];
								Vector3f pos = new Vector3f(0, 0, i);
								Vector3f out = new Vector3f(pos);
								MipavCoordinateSystems.fileToScanner(pos, out, resultImage);
								float origin[] = new float[3];
								origin[0] = out.X;
								origin[1] = out.Y;
								origin[2] = out.Z;
								for (j = 0; j < 3; j++) {
									if ((resultImage.getAxisOrientation()[j] == FileInfoBase.ORI_R2L_TYPE)
											|| (resultImage.getAxisOrientation()[j] == FileInfoBase.ORI_L2R_TYPE)) {
										resultImage.getFileInfo()[index].setOrigin(origin[0], j);
									} else if ((resultImage.getAxisOrientation()[j] == FileInfoBase.ORI_A2P_TYPE)
											|| (resultImage.getAxisOrientation()[j] == FileInfoBase.ORI_P2A_TYPE)) {
										resultImage.getFileInfo()[index].setOrigin(origin[1], j);
									} else {
										resultImage.getFileInfo()[index].setOrigin(origin[2], j);
									}
								}
							}
						}
					} // if (
						// (srcImage.getMatrixHolder().containsType(TransMatrix.TRANSFORM_SCANNER_ANATOMICAL))
				} // if (destImage.getNDims() >= 3)
				resultImage.clearMask();
				resultImage.calcMinMax();
				/*
				 * try { new ViewJFrameImage(resultImage, null, new Dimension(610, 200) ); }
				 * catch (OutOfMemoryError error) { System.gc();
				 * JOptionPane.showMessageDialog(null,
				 * "Out of memory: unable to open new frame", "Error",
				 * JOptionPane.ERROR_MESSAGE); }
				 */

			} else if (resultImage != null) {
				// algorithm failed but result image still has garbage
				resultImage = null;
				System.gc();
			}
		}
		algorithm.finalize();
		algorithm = null;

	} // end AlgorithmPerformed()

}
