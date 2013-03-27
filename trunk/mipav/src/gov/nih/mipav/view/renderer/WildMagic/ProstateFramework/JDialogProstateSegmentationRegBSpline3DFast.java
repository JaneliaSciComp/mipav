package gov.nih.mipav.view.renderer.WildMagic.ProstateFramework;

import gov.nih.mipav.util.*;
import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.algorithms.filters.*;

import gov.nih.mipav.model.algorithms.utilities.AlgorithmAddMargins;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.dialogs.*;
import gov.nih.mipav.view.dialogs.JDialogRegistrationBSpline.Controls;

import gov.nih.mipav.view.*;
import gov.nih.mipav.model.file.*;
import gov.nih.mipav.model.file.FileInfoBase.Unit;

import java.awt.*;
import java.awt.event.*;
import java.io.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;

import java.util.*;

import javax.swing.*;

import WildMagic.LibFoundation.Mathematics.Vector2f;
import WildMagic.LibFoundation.Mathematics.Vector3f;

/**
 * Semi-automatic MR Prostate segmentation - Registration guided segmentation
 * model.
 * 
 * The algorithm uses B-Spline registration to guide the MR prostate
 * segmentation. Basic steps: 1) User open the axial, sagittal, coronal images
 * in the MIPAV frame. 2) User manually outline the three contours on separate
 * slices for each axial, sagittal, and coronal images to define the middle, the
 * apex and the base of the prostate. 3) User click the OK button to let a fully
 * automatic registration guided algorithm to segment the prostate of all the
 * three images.
 * 
 * @author Ruida Cheng
 * 
 */
public class JDialogProstateSegmentationRegBSpline3DFast extends JDialogBase
		implements AlgorithmInterface {

	/** global user interface to get the three active images. */
	private ViewUserInterface UI;

	/** number of slices to trace, start from the apex and base VOIs */
	private int tracingSliceNumber = 3;

	// image type constants
	/** Axial image type */
	private static int Axial = 0;
	/** Sagittal image type */
	private static int Sagittal = 1;
	/** Coronal image type */
	private static int Coronal = 2;

	// Axial start, end and mid slice number of VOI contours
	private JLabel labelAxis;
	private JLabel labelStartVOIAxial;
	private JTextField textFieldStartVOIAxial;

	private JLabel labelEndVOIAxial;
	private JTextField textFieldEndVOIAxial;

	private JLabel labelMidVOIAxial;
	private JTextField textFieldMidVOIAxial;

	int startVOIAxial, endVOIAxial, midVOIAxial;

	// Sagittal start, end and mid slice number of VOI contours
	private JLabel labelSagittal;
	private JLabel labelStartVOISagittal;
	private JTextField textFieldStartVOISagittal;

	private JLabel labelEndVOISagittal;
	private JTextField textFieldEndVOISagittal;

	private JLabel labelMidVOISagittal;
	private JTextField textFieldMidVOISagittal;

	int startVOISagittal, endVOISagittal, midVOISagittal;

	// Coronal start, end and mid slice number of VOI contours
	private JLabel labelCoronal;
	private JLabel labelStartVOICoronal;
	private JTextField textFieldStartVOICoronal;

	private JLabel labelEndVOICoronal;
	private JTextField textFieldEndVOICoronal;

	private JLabel labelMidVOICoronal;
	private JTextField textFieldMidVOICoronal;

	private JRadioButton radioBSpline;
	private JRadioButton radioOAR;

	private int startVOICoronal, endVOICoronal, midVOICoronal;
	private ModelImage imageAxial, imageSagittal, imageCoronal;

	/** Flag to indicate to use B-Spline registration or OAR registration */
	private boolean useBSpline = false;

	/**
	 * GUI interface for semi-automatic MR prostate segmentation.
	 * 
	 * @param theParentFrame
	 *            the MIPAV main frame.
	 */
	public JDialogProstateSegmentationRegBSpline3DFast(Frame theParentFrame) {
		super(theParentFrame, false);
		UI = ViewUserInterface.getReference();

		Enumeration<ModelImage> images = UI.getRegisteredImages();
		while (images.hasMoreElements()) {
			ModelImage tempImage = images.nextElement();
			String name = tempImage.getImageName();
			if (name.contains("ax") || name.contains("Axial")) {
				imageAxial = tempImage;
			} else if (name.contains("sag") || name.contains("Sagittal")) {
				imageSagittal = tempImage;
			} else if (name.contains("cor") || name.contains("Coronal")) {
				imageCoronal = tempImage;
			}
		}

		autoConfigVOIsNumbers();
		init();
		setVisible(true);

	}

	/**
	 * After user manually draw the three VOIs on each axial, sagittal, coronal
	 * image, the drawn VOIs slices number is auto configured to appear in the
	 * dialog GUI interface.
	 */
	public void autoConfigVOIsNumbers() {
		int[] slices = new int[3];
		configVOIsNumbers(imageAxial, slices);
		startVOIAxial = slices[0];
		midVOIAxial = slices[1];
		endVOIAxial = slices[2];
		slices = new int[3];
		configVOIsNumbers(imageSagittal, slices);
		startVOISagittal = slices[0];
		midVOISagittal = slices[1];
		endVOISagittal = slices[2];
		slices = new int[3];
		configVOIsNumbers(imageCoronal, slices);
		startVOICoronal = slices[0];
		midVOICoronal = slices[1];
		endVOICoronal = slices[2];
	}

	/**
	 * Configure the mid, apex and base VOIs, sort them in order.
	 * 
	 * @param image
	 *            oringial MRI image
	 * @param slices
	 *            Slice array to store the mid, apex and base VOIs slice number.
	 */
	public void configVOIsNumbers(ModelImage image, int[] slices) {

		VOIVector src = image.getVOIs();
		int zDim = image.getExtents()[2];
		int count = 0;
		for (int j = 0; j < 3; j++) {
			Vector<VOIBase>[] vArray = src.VOIAt(j).getSortedCurves(
					VOIBase.ZPLANE, zDim);
			for (int i = 0; i < zDim; i++) {
				if (vArray[i].size() > 0) {
					VOIBase v = vArray[i].get(0);
					if (v != null && v.size() > 0) {
						slices[count] = i;
						// System.out.println(" i = " + slices[count]);
						count++;
					}
				}
			} // end for i loop
		}
		int min, max, mid = 0;
		min = Math.min(slices[2], Math.min(slices[0], slices[1]));
		max = Math.max(slices[2], Math.max(slices[0], slices[1]));
		for (int i = 0; i < 3; i++) {
			if (slices[i] != min && slices[i] != max) {
				mid = slices[i];
				break;
			}
		}
		slices[0] = min;
		slices[1] = mid;
		slices[2] = max;

	}

	/**
	 * Just handle GUI button clicks event.
	 */
	public void actionPerformed(ActionEvent event) {
		Object source = event.getSource();
		String command = event.getActionCommand();
		if (command.equals("OK")) {

			if (radioBSpline.isEnabled()) {
				useBSpline = true;
			} else {
				useBSpline = false;
			}

			long startTime = System.currentTimeMillis();

			// Configure the mid, start, and end VOI numbers for each axial,
			// sagittal, and coronal images.
			startVOIAxial = Integer.valueOf(textFieldStartVOIAxial.getText());
			endVOIAxial = Integer.valueOf(textFieldEndVOIAxial.getText());
			midVOIAxial = Integer.valueOf(textFieldMidVOIAxial.getText());

			startVOISagittal = Integer.valueOf(textFieldStartVOISagittal
					.getText());
			endVOISagittal = Integer.valueOf(textFieldEndVOISagittal.getText());
			midVOISagittal = Integer.valueOf(textFieldMidVOISagittal.getText());

			startVOICoronal = Integer.valueOf(textFieldStartVOICoronal
					.getText());
			endVOICoronal = Integer.valueOf(textFieldEndVOICoronal.getText());
			midVOICoronal = Integer.valueOf(textFieldMidVOICoronal.getText());

			adjustVOIs(imageAxial);
			adjustVOIs(imageSagittal);
			adjustVOIs(imageCoronal);
			
			// Parallel processing the prostate segmentation concurrently.
			int numberCore = (Runtime.getRuntime().availableProcessors() - 2) > 1 ? Runtime
					.getRuntime().availableProcessors() - 2 : 1;
			ExecutorService exec = Executors.newFixedThreadPool(numberCore);
			exec.execute(createTask(imageAxial, midVOIAxial, startVOIAxial,
					endVOIAxial));
			exec.execute(createTask(imageSagittal, midVOISagittal,
					startVOISagittal, endVOISagittal));
			exec.execute(createTask(imageCoronal, midVOICoronal,
					startVOICoronal, endVOICoronal));
			exec.shutdown();

			// setup the upper limit waiting time.
			try {
				exec.awaitTermination(5, TimeUnit.MINUTES);
			} catch (InterruptedException e) {
				MipavUtil.displayError("Program did not execute correctly");
				e.printStackTrace();
			}

			// When the VOIs is over interpolated at the apex and base,
			// use central gland contours based projections to eliminate the
			// extra contours at the two ends.
			removeContourFromOtherOrientation();

			// computer the VOI binary mask based volume.
			calculateVOIsVolume();

			long endTime = System.currentTimeMillis();
			int min = (int) ((endTime - startTime) / 1000f / 60f);
			int sec = (int) ((endTime - startTime) / 1000f % 60f);
			System.out.println("time elapse = " + min + "  mins  " + sec
					+ "  sec");
			Preferences.debug("time elapse = " + min + "  mins  " + sec
					+ "  sec\n");

		} else if (command.equals("Cancel")) {
			dispose();
		} else if (command.equals("Help")) {
			//MipavUtil.showHelp("Haral1001");
		    MipavUtil.showWebHelp("Filters_(Spatial):_Haralick_Texture");
		} else {
            // super.actionPerformed(event);
        }

	}

	/**
	 * Compute the VOIs binary mask based volumes.
	 */
	public void calculateVOIsVolume() {
		VOIVector voiAxial;
		VOIVector voiSagittal;
		VOIVector voiCoronal;

		voiAxial = imageAxial.getVOIs();
		voiSagittal = imageSagittal.getVOIs();
		voiCoronal = imageCoronal.getVOIs();

		printVolume(imageAxial, voiAxial, Axial);
		printVolume(imageSagittal, voiSagittal, Sagittal);
		printVolume(imageCoronal, voiCoronal, Coronal);
	}

	/**
	 * Print out the three VOIs volume in system console out.
	 * 
	 * @param srcImage
	 *            original MRI image
	 * @param voiVector
	 *            segmented resulting VOIs contours
	 * @param viewOrientation
	 *            view axis orientation: axial, or sagittal or coronal.
	 */
	public void printVolume(ModelImage srcImage, VOIVector voiVector,
			int viewOrientation) {
		int nVox;
		float volume, area;
		VOI calcSelectedVOI = voiVector.get(0);

		int xDim = srcImage.getExtents().length > 0 ? srcImage.getExtents()[0]
				: 1;
		int yDim = srcImage.getExtents().length > 1 ? srcImage.getExtents()[1]
				: 1;
		int zDim = srcImage.getExtents().length > 2 ? srcImage.getExtents()[2]
				: 1;
		FileInfoBase fileInfo = srcImage.getFileInfo()[zDim / 2];

		xDim = srcImage.getExtents().length > 0 ? srcImage.getExtents()[0] : 1;
		yDim = srcImage.getExtents().length > 1 ? srcImage.getExtents()[1] : 1;
		zDim = srcImage.getExtents().length > 2 ? srcImage.getExtents()[2] : 1;

		BitSet mask = new BitSet(xDim * yDim * zDim);
		calcSelectedVOI.createBinaryMask3D(mask, xDim, yDim, false, false);

		nVox = mask.cardinality();

		area = nVox
				* (fileInfo.getResolutions()[0] * fileInfo.getResolutions()[1]);
		volume = nVox
				* (fileInfo.getResolutions()[0] * fileInfo.getResolutions()[1] * fileInfo
						.getResolutions()[2]);

		String unitString;

		int xUnits = srcImage.getFileInfo(0).getUnitsOfMeasure()[0];
		unitString = Unit.getUnitFromLegacyNum(xUnits).getAbbrev();

		if (viewOrientation == Axial) {
			System.out.println("Axial image VOIs:  number of voxels = " + nVox
					+ "  volume = " + volume + " " + unitString + "^3");
			Preferences.debug("Axial image VOIs:  number of voxels = " + nVox
					+ "  volume = " + volume + " " + unitString + "^3\n");
		} else if (viewOrientation == Sagittal) {
			System.out.println("Sagittal image VOIs:  number of voxels = "
					+ nVox + "  volume = " + volume + " " + unitString + "^3");
			Preferences.debug("Sagittal image VOIs:  number of voxels = "
					+ nVox + "  volume = " + volume + " " + unitString + "^3\n");
		} else if (viewOrientation == Coronal) {
			System.out.println("Coronal image VOIs:  number of voxels = "
					+ nVox + "  volume = " + volume + " " + unitString + "^3");
			Preferences.debug("Coronal image VOIs:  number of voxels = "
					+ nVox + "  volume = " + volume + " " + unitString + "^3\n");
		}

	}

	/**
	 * User central gland contours projection to remove the extra interpolated
	 * VOIs contours at the apex and base. Parallel processing the algorithm
	 * with axial, sagittal and coronal images concurrently.
	 */
	public void removeContourFromOtherOrientation() {

		VOIVector voiAxial;
		VOIVector voiSagittal;
		VOIVector voiCoronal;

		Vector<Vector3f> voiAxialDicom = new Vector<Vector3f>();
		Vector<Vector3f> voiSagittalDicom = new Vector<Vector3f>();
		Vector<Vector3f> voiCoronalDicom = new Vector<Vector3f>();

		voiAxial = imageAxial.getVOIs();
		voiSagittal = imageSagittal.getVOIs();
		voiCoronal = imageCoronal.getVOIs();

		// convert the VOIs contours from image space to dicom space.
		convertVOIToDicomSpace(voiAxial, voiAxialDicom, imageAxial,
				startVOIAxial, endVOIAxial);
		convertVOIToDicomSpace(voiSagittal, voiSagittalDicom, imageSagittal,
				startVOISagittal, endVOISagittal);
		convertVOIToDicomSpace(voiCoronal, voiCoronalDicom, imageCoronal,
				startVOICoronal, endVOICoronal);

		int numberCore = (Runtime.getRuntime().availableProcessors() - 2) > 1 ? Runtime
				.getRuntime().availableProcessors() - 2 : 1;
		ExecutorService exec = Executors.newFixedThreadPool(numberCore);
		exec.execute(trimVOIsInSingleImage(imageAxial, startVOIAxial,
				endVOIAxial, voiSagittalDicom, voiCoronalDicom, Axial));
		exec.execute(trimVOIsInSingleImage(imageSagittal, startVOISagittal,
				endVOISagittal, voiAxialDicom, voiCoronalDicom, Sagittal));
		exec.execute(trimVOIsInSingleImage(imageCoronal, startVOICoronal,
				endVOICoronal, voiAxialDicom, voiSagittalDicom, Coronal));
		exec.shutdown();

		try {
			exec.awaitTermination(10, TimeUnit.MINUTES);
		} catch (InterruptedException e) {
			MipavUtil.displayError("Program did not execute correctly");
			e.printStackTrace();
		}
	}

	/**
	 * In Dicom space, central contours projections to eliminate the extra
	 * contours at the apex and base. For example, axial image, project the
	 * sagittal, coronal VOIs onto the axial image Dicom space. The central
	 * gland part sagittal, coronal VOIs contours is projected on the axial
	 * image slices as points based contours. If there is no points on the
	 * corresponding axial image slice, however it has contour, the contour is
	 * eliminated.
	 * 
	 * @param image
	 *            original MRI image. i.e. axial image
	 * @param startVOI
	 *            apex VOI slice number
	 * @param endVOI
	 *            base VOI slice number
	 * @param firstVOI
	 *            sagittal image VOIs in Dicom space
	 * @param secondVOI
	 *            coronal image VOIs in Dicom space.
	 * @param viewOrientation
	 *            image axis orientation
	 * @return
	 */
	public Runnable trimVOIsInSingleImage(final ModelImage image,
			final int startVOI, final int endVOI,
			final Vector<Vector3f> firstVOI, final Vector<Vector3f> secondVOI,
			final int viewOrientation) {
		return new Runnable() {
			public void run() {
				int startSlice = startVOI - 1;
				int endSlice = 0;
				Vector3f center = image.getImageCenter();
				float zDicom = 0;
				for (int z = startSlice; z > endSlice; z--) {
					Vector3f ptImg = new Vector3f(center.X, center.Y, z);
					Vector3f ptDicom = new Vector3f();
					MipavCoordinateSystems.fileToScanner(ptImg, ptDicom, image);
					if (viewOrientation == Axial) {
						zDicom = ptDicom.Z;
					} else if (viewOrientation == Sagittal) {
						zDicom = ptDicom.X;
					} else if (viewOrientation == Coronal) {
						zDicom = ptDicom.Y;
					}
					removeVOIsInDicomSpace(z, zDicom, image, firstVOI,
							secondVOI, viewOrientation);
				} // end z loop
				int zDim = image.getExtents()[2];
				startSlice = endVOI + 1;
				for (int z = startSlice; z < zDim; z++) {
					Vector3f ptImg = new Vector3f(center.X, center.Y, z);
					Vector3f ptDicom = new Vector3f();
					MipavCoordinateSystems.fileToScanner(ptImg, ptDicom, image);
					if (viewOrientation == Axial) {
						zDicom = ptDicom.Z;
					} else if (viewOrientation == Sagittal) {
						zDicom = ptDicom.X;
					} else if (viewOrientation == Coronal) {
						zDicom = ptDicom.Y;
					}
					removeVOIsInDicomSpace(z, zDicom, image, firstVOI,
							secondVOI, viewOrientation);
				}
			}
		};
	}

	/**
	 * Remove the extra VOIs contours in Dicom space.
	 * 
	 * @param zSlice
	 *            axial z slice number
	 * @param zDicom
	 *            z slice number corresponding Dicom value
	 * @param image
	 *            original MRI image
	 * @param firstVOI
	 *            sagittal VOI
	 * @param secondVOI
	 *            coronal VOI
	 * @param viewOrientation
	 *            view axis
	 */
	public void removeVOIsInDicomSpace(int zSlice, float zDicom,
			ModelImage image, Vector<Vector3f> firstVOI,
			Vector<Vector3f> secondVOI, int viewOrientation) {

		Vector<Vector3f> pointsFind = new Vector<Vector3f>();
		Vector3f v;
		for (int i = 0; i < firstVOI.size(); i++) {
			v = (Vector3f) firstVOI.get(i);
			if (viewOrientation == Axial) {
				if (zDicom <= (v.Z + 0.5f) && zDicom >= (v.Z - 0.5f)) {
					pointsFind.add(new Vector3f(v.X, v.Y, v.Z));
				}
			} else if (viewOrientation == Sagittal) {
				if (zDicom <= (v.X + 0.5f) && zDicom >= (v.X - 0.5f)) {
					pointsFind.add(new Vector3f(v.X, v.Y, v.Z));
				}
			} else if (viewOrientation == Coronal) {
				if (zDicom <= (v.Y + 0.5f) && zDicom >= (v.Y - 0.5f)) {
					pointsFind.add(new Vector3f(v.X, v.Y, v.Z));
				}
			}
		}
		for (int i = 0; i < secondVOI.size(); i++) {
			v = (Vector3f) secondVOI.get(i);
			if (viewOrientation == Axial) {
				if (zDicom <= (v.Z + 0.5f) && zDicom >= (v.Z - 0.5f)) {
					pointsFind.add(new Vector3f(v.X, v.Y, v.Z));
				}
			} else if (viewOrientation == Sagittal) {
				if (zDicom <= (v.X + 0.5f) && zDicom >= (v.X - 0.5f)) {
					pointsFind.add(new Vector3f(v.X, v.Y, v.Z));
				}
			} else if (viewOrientation == Coronal) {
				if (zDicom <= (v.Y + 0.5f) && zDicom >= (v.Y - 0.5f)) {
					pointsFind.add(new Vector3f(v.X, v.Y, v.Z));
				}
			}
		}

		int size = pointsFind.size();

		// if there is no VOI points, remove the voi
		if (size < 30) {
			VOIVector voiVectorNew = image.getVOIs();
			VOI voi = voiVectorNew.get(0);
			Vector<VOIBase> curves = voi.getSliceCurves(zSlice);
			for (int k = 0; k < curves.size(); k++) {
				VOIBase curve = curves.get(k);
				voi.removeCurve(curve);
			}
		}

	}

	/**
	 * Conver the VOIs from image space to Dicom space
	 * 
	 * @param src
	 *            source VOIs
	 * @param target
	 *            converted target VOIs
	 * @param image
	 *            original MRI image
	 * @param startSlice
	 *            starting slice
	 * @param endSlice
	 *            ending slice
	 */
	public void convertVOIToDicomSpace(VOIVector src, Vector<Vector3f> target,
			ModelImage image, int startSlice, int endSlice) {

		int zDim = image.getExtents()[2];
		Vector<VOIBase>[] vArray = src.VOIAt(0).getSortedCurves(VOIBase.ZPLANE,
				zDim);

		for (int i = startSlice; i <= endSlice; i++) {

			if (vArray[i].size() > 0) {
				VOIBase v = vArray[i].get(0);
				if (v != null && v.size() > 0) {
					int nPts = v.size();
					float[] xPts = new float[nPts];
					float[] yPts = new float[nPts];
					float[] zPts = new float[nPts];

					v.exportArrays(xPts, yPts, zPts);

					for (int j = 0; j < nPts; j++) {
						Vector3f ptIn;
						final Vector3f ptOut = new Vector3f();
						ptIn = new Vector3f(xPts[j], yPts[j], zPts[j]);
						MipavCoordinateSystems
								.fileToScanner(ptIn, ptOut, image);
						target.add(new Vector3f(ptOut.X, ptOut.Y, ptOut.Z));
					}
				}
			}
		} // end for i loop
	}

	/**
	 * Parallel processing the automatic segmentation algorithm to all the three
	 * images concurrently.
	 * 
	 * @param image
	 *            original MRI image
	 * @param midVOI
	 *            mid slice VOI
	 * @param startVOI
	 *            apex slice VOI
	 * @param endVOI
	 *            base slice VOI
	 * @param axis
	 *            view axis
	 * @return
	 */
	private Runnable createTask(final ModelImage image, final int midVOI,
			final int startVOI, final int endVOI) {
		return new Runnable() {

			public void run() {
				ModelImage cropImage;
				ModelImage coherenceEnhancingDiffusionImage;
				Vector<Vector<ModelImage>> imageStackFuzzyC = new Vector<Vector<ModelImage>>();
				int[] xBounds = new int[2];
				int[] yBounds = new int[2];
				int[] zBounds = new int[2];
				int[] boundingBox = new int[4];

				cropImage = cropROI(image, xBounds, yBounds, zBounds,
						boundingBox, midVOI);
				coherenceEnhancingDiffusionImage = calculateCoherenceEnhancingDiffusion(cropImage);
				sortImageVOIsInit(image, midVOI, startVOI, endVOI);
				sortCEDImageVOIs(coherenceEnhancingDiffusionImage, midVOI,
						startVOI, endVOI);
				doRegistrationMid(image, coherenceEnhancingDiffusionImage,
						imageStackFuzzyC, midVOI, startVOI, endVOI, boundingBox);
				doRegistrationStart(image, coherenceEnhancingDiffusionImage,
						imageStackFuzzyC, startVOI, boundingBox);
				doRegistrationEnd(image, coherenceEnhancingDiffusionImage,
						imageStackFuzzyC, endVOI, boundingBox);
				sortImageVOIs(image);

				// dispose function local memory
				cropImage.disposeLocal();
				cropImage = null;
				coherenceEnhancingDiffusionImage.disposeLocal();
				coherenceEnhancingDiffusionImage = null;

				for (int i = 0; i < imageStackFuzzyC.size(); i++) {
					Vector<ModelImage> element = imageStackFuzzyC.get(i);
					for (int j = 0; j < element.size(); j++) {
						ModelImage temp = element.get(j);
						temp.disposeLocal();
						temp = null;
					}
				}
				imageStackFuzzyC.clear();
				imageStackFuzzyC = null;
				xBounds = null;
				yBounds = null;
				zBounds = null;
				boundingBox = null;

				// System.gc();

			}
		};

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
			System.out.println("keyboard failed: " + e);
		}
	}

	/**
	 * Apply the coherence enhancing diffusion filter to the croppred MR image.
	 * 
	 * @param cropImage
	 *            cropped image
	 * @return the coherence enhancing diffusion image.
	 */
	private ModelImage calculateCoherenceEnhancingDiffusion(ModelImage cropImage) {

		int numIterations;
		float diffusitivityDenom;
		float derivativeScale;
		float gaussianScale;
		boolean do25D;
		boolean entireImage;

		derivativeScale = 0.5f;
		diffusitivityDenom = 0.001f;
		gaussianScale = 2.0f;
		numIterations = 50;
		do25D = true;
		entireImage = true;
		ModelImage coherenceEnhancingDiffusionImage;

		try {
			coherenceEnhancingDiffusionImage = (ModelImage) cropImage.clone();
			coherenceEnhancingDiffusionImage.setType(ModelStorageBase.FLOAT);
			coherenceEnhancingDiffusionImage.reallocate(ModelStorageBase.FLOAT);

			AlgorithmCoherenceEnhancingDiffusion coherenceEnhancingDiffusionAlgo = new AlgorithmCoherenceEnhancingDiffusion(
					coherenceEnhancingDiffusionImage, cropImage, numIterations,
					diffusitivityDenom, derivativeScale, gaussianScale, do25D,
					entireImage);

			coherenceEnhancingDiffusionAlgo.addListener(this);

			coherenceEnhancingDiffusionAlgo.run();

			coherenceEnhancingDiffusionAlgo.finalize();
			coherenceEnhancingDiffusionAlgo = null;

		} catch (OutOfMemoryError x) {
			MipavUtil
					.displayError("Dialog GaborFilter: unable to allocate enough memory");

			return null;
		}

		return coherenceEnhancingDiffusionImage;
	}

	/**
	 * Check the registration accuracy.  Check to see if the registration runs into errors.   Check criterion include VOI area, VOI center, etc. 
	 * If area and center difference exceed specified threshold, registration runs into error.  
	 * @param currentSlice     current image slice
	 * @param resultImage      result image after registration
	 * @param refImage         registration reference slice
	 * @return   true          registration wrong, false, registration wrong. 
	 */
	private boolean isVOIsCloseEnough(int currentSlice, ModelImage resultImage,
			ModelImage refImage) {

		VOIVector VOIsRef = refImage.getVOIs();
		VOIVector VOIsSolution = resultImage.getVOIs();

		float areaRef = 0, areaSolution = 0;
		float centerRefX = 0, centerRefY = 0;
		float centerSolutionX = 0, centerSolutionY = 0;

		if (VOIsRef.size() <= 0)
			return true;

		Vector<VOIBase>[] vArray = VOIsRef.VOIAt(0).getSortedCurves(
				VOIBase.ZPLANE, 1);
		VOIBase v = vArray[0].get(0);
		// VOI totalVOI = v.getGroup();
		if (v != null) {
			VOIBase vTemp = (VOIBase) v.clone();

			int nPtsRef = vTemp.size();

			// zero out the z dimension VOI
			float[] xPtsRef = new float[nPtsRef];
			float[] yPtsRef = new float[nPtsRef];
			float[] zPtsRef = new float[nPtsRef];
			vTemp.exportArrays(xPtsRef, yPtsRef, zPtsRef);

			int carea = 0;
			int iminus1;
			for (int i = 0; i < nPtsRef; i++) {
				iminus1 = i - 1;
				if (iminus1 < 0)
					iminus1 = nPtsRef - 1;
				carea += (xPtsRef[i] + xPtsRef[iminus1])
						* (yPtsRef[i] - yPtsRef[iminus1]);

				centerRefX += xPtsRef[i];
				centerRefY += yPtsRef[i];

			}
			areaRef = Math.abs(carea / 2.0f);
			centerRefX /= nPtsRef;
			centerRefY /= nPtsRef;

		}

		if (VOIsSolution.size() == 0)
			return false;

		vArray = VOIsSolution.VOIAt(0).getSortedCurves(VOIBase.ZPLANE, 1);
		v = vArray[0].get(0);
		// VOI totalVOI = v.getGroup();
		if (v != null) {
			VOIBase vTemp = (VOIBase) v.clone();

			int nPtsSolution = vTemp.size();

			// zero out the z dimension VOI
			float[] xPtsSolution = new float[nPtsSolution];
			float[] yPtsSolution = new float[nPtsSolution];
			float[] zPtsSolution = new float[nPtsSolution];
			vTemp.exportArrays(xPtsSolution, yPtsSolution, zPtsSolution);

			int carea = 0;
			int iminus1;
			for (int i = 0; i < nPtsSolution; i++) {
				iminus1 = i - 1;
				if (iminus1 < 0)
					iminus1 = nPtsSolution - 1;
				carea += (xPtsSolution[i] + xPtsSolution[iminus1])
						* (yPtsSolution[i] - yPtsSolution[iminus1]);

				centerSolutionX += xPtsSolution[i];
				centerSolutionY += yPtsSolution[i];
			}
			areaSolution = Math.abs(carea / 2.0f);
			centerSolutionX /= nPtsSolution;
			centerSolutionY /= nPtsSolution;
		}

		float areaDiff = Math.abs(areaSolution - areaRef);
		float centerDiff = (float) Math.sqrt((centerSolutionX - centerRefX)
				* (centerSolutionX - centerRefX)
				+ (centerSolutionY - centerRefY)
				* (centerSolutionY - centerRefY));

		System.out.println("Slice: " + currentSlice + "\tareaDiff = "
				+ areaDiff + " \tcenterDiff = " + centerDiff);

		if (centerDiff >= 150 || areaDiff >= 15000) {
			return false;
		} else {
			return true;
		}
	}

	/**
	 * Convert the mask image to VOI with 50 points
	 * @param maskImage        binary mask image
	 * @param resultImage      result image
	 */
	public void smoothVOI50(ModelImage resultImage) {

		ModelImage maskImage = (ModelImage) resultImage.clone();
		maskImage.getVOIs().removeAllElements();
		maskImage.addVOIs(resultImage.getVOIs());

		VOIVector v = maskImage.getVOIs();
		if (v.size() == 0)
			return;
		v.VOIAt(0).setActive(true);
		v.VOIAt(0).setAllActive(true);

		// new ViewJFrameImage(maskImage);
		try {
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage,
					v.VOIAt(0), 50, false);
			smoothAlgo.addListener(this);
			smoothAlgo.run();

			VOIVector resultVOIs = resultImage.getVOIs();
			VOI resultVOI = smoothAlgo.getResultVOI();
			resultVOIs.VOIAt(0).removeCurves();
			resultVOIs.VOIAt(0).setCurves(resultVOI.getCurves());
			resultImage.notifyImageDisplayListeners(null, true);
			// new ViewJFrameImage(resultImage);
			smoothAlgo.finalize();
			smoothAlgo = null;
		} catch (OutOfMemoryError x) {
			MipavUtil
					.displayError("Dialog Smooth: unable to allocate enough memory");

			return;
		}
	}

	/**
	 * Convert the mask image to VOI with 120 points. 
	 * @param maskImage          binary mask image
	 * @param resultImage		result image
	 */
	public void smoothVOI120(ModelImage resultImage) {

		ModelImage maskImage = (ModelImage) resultImage.clone();
		maskImage.getVOIs().removeAllElements();
		maskImage.addVOIs(resultImage.getVOIs());

		VOIVector v = maskImage.getVOIs();
		if (v.size() == 0)
			return;
		v.VOIAt(0).setActive(true);
		v.VOIAt(0).setAllActive(true);

		// new ViewJFrameImage(maskImage);
		try {
			AlgorithmBSmooth smoothAlgo = new AlgorithmBSmooth(maskImage,
					v.VOIAt(0), 120, false);
			smoothAlgo.addListener(this);
			smoothAlgo.run();

			VOIVector resultVOIs = resultImage.getVOIs();
			VOI resultVOI = smoothAlgo.getResultVOI();
			resultVOIs.VOIAt(0).removeCurves();
			resultVOIs.VOIAt(0).setCurves(resultVOI.getCurves());
			resultImage.notifyImageDisplayListeners(null, true);
			// new ViewJFrameImage(resultImage);
			smoothAlgo.finalize();
			smoothAlgo = null;
		} catch (OutOfMemoryError x) {
			MipavUtil
					.displayError("Dialog Smooth: unable to allocate enough memory");

			return;
		}
	}

	/**
	 * Transform the the resulting VOI from cropped image to oringal MR image.
	 * 
	 * @param image
	 *            original MR image
	 * @param resultImage
	 *            resulting image after segmentation
	 * @param boundingBox
	 *            cropped image bounding box
	 * @param currentSlice
	 *            current image slice
	 */
	public void transformVOI(ModelImage image, ModelImage resultImage,
			ModelImage refImage, ModelImage testImage, int[] boundingBox,
			int currentSlice) {

		VOIVector resultingVOIs = null;

		// new ViewJFrameImage((ModelImage) refImage.clone());
		// new ViewJFrameImage((ModelImage) testImage.clone());
		// new ViewJFrameImage((ModelImage) resultImage.clone());

		try {
			if (isVOIsCloseEnough(currentSlice, resultImage, refImage)) {
				resultingVOIs = resultImage.getVOIs();
			} else {
				// If the registration went wrong, roll back the source image
				// from testImage.
				// This ensures the continuity of the segmentation process.
				resultingVOIs = refImage.getVOIs();
				int sliceSize;
				int[] ext = testImage.getExtents();
				sliceSize = ext[0] * ext[1];
				double[] sourceBuffer;
				sourceBuffer = new double[sliceSize];
				testImage.exportData(0, sliceSize, sourceBuffer);
				resultImage.importData(0, sourceBuffer, true);
				resultImage.getVOIs().removeAllElements();
				resultImage.addVOIs(resultingVOIs);

			}
		} catch (IOException error) {
			error.printStackTrace();
			// System.gc();
		}

		smoothVOI50(resultImage);
		smoothVOI120(resultImage);

		resultingVOIs = resultImage.getVOIs();
		Vector<VOIBase>[] vArray = resultingVOIs.VOIAt(0).getSortedCurves(
				VOIBase.ZPLANE, 1);

		VOIBase v = vArray[0].get(0);
		// VOI totalVOI = v.getGroup();
		if (v != null) {
			VOIBase vTemp = (VOIBase) v.clone();

			int nPts = vTemp.size();

			// zero out the z dimension VOI
			float[] xPts = new float[nPts];
			float[] yPts = new float[nPts];
			float[] zPts = new float[nPts];
			float[] zPtsZero = new float[nPts];

			vTemp.exportArrays(xPts, yPts, zPts);

			for (int u = 0; u < nPts; u++) {
				xPts[u] = xPts[u] + boundingBox[0]; // boxXmin;
				yPts[u] = yPts[u] + boundingBox[2]; // boxYmin;
				zPtsZero[u] = currentSlice;
			}

			vTemp.importArrays(xPts, yPts, zPtsZero, nPts);

			VOIVector voiVectorNew = image.getVOIs();
			VOI voiNew = voiVectorNew.get(0);
			voiNew.importCurve(vTemp);

		}
	}

	/**
	 * Sorting image VOI
	 * 
	 * @param image
	 *            original MR image
	 */
	public void sortImageVOIs(ModelImage image) {
		int zDim = image.getExtents()[2] - 1;
		image.getVOIs().VOIAt(0).getSortedCurves(VOIBase.ZPLANE, zDim);
	}

	/**
	 * Sort the original MR image's specified VOIs
	 * 
	 * @param image
	 *            original MR image
	 * @param midVOI
	 *            mid slice VOI
	 * @param startVOI
	 *            apex slice VOI
	 * @param endVOI
	 *            base slice VOI
	 */
	public void sortImageVOIsInit(ModelImage image, int midVOI, int startVOI,
			int endVOI) {
		VOIVector VOIsSrc;
		VOI voiNew = new VOI((short) 0, "ImageVOI");
		int zDim = image.getExtents()[2] - 1;
		// if (image.getVOIs() != null) {
		VOIsSrc = image.getVOIs();
		// }

		Vector<VOIBase>[] vArrayCenter = VOIsSrc.VOIAt(1).getSortedCurves(
				VOIBase.ZPLANE, zDim);
		VOIBase vCenter = vArrayCenter[midVOI].get(0);
		voiNew.importCurve(vCenter);

		Vector<VOIBase>[] vArrayStart = VOIsSrc.VOIAt(0).getSortedCurves(
				VOIBase.ZPLANE, zDim);
		VOIBase vStart = vArrayStart[startVOI].get(0);
		voiNew.importCurve(vStart);

		Vector<VOIBase>[] vArrayEnd = VOIsSrc.VOIAt(2).getSortedCurves(
				VOIBase.ZPLANE, zDim);
		VOIBase vEnd = vArrayEnd[endVOI].get(0);
		voiNew.importCurve(vEnd);

		voiNew.getSortedCurves(zDim);

		VOIsSrc.removeAllElements();
		VOIsSrc.add(voiNew);
	}

	/**
	 * Sort the CED image VOIs
	 * 
	 * @param coherenceEnhancingDiffusionImage
	 *            CED image
	 * @param midVOI
	 *            mid slice VOI
	 * @param startVOI
	 *            apex slice VOI
	 * @param endVOI
	 *            base slice VOI
	 */
	public void sortCEDImageVOIs(ModelImage coherenceEnhancingDiffusionImage,
			int midVOI, int startVOI, int endVOI) {

		VOI voiNew = new VOI((short) 0, "CED_VOI");
		VOIVector VOIs;
		int zDim = coherenceEnhancingDiffusionImage.getExtents()[2] - 1;

		// if (coherenceEnhancingDiffusionImage.getVOIs() != null) {
		VOIs = coherenceEnhancingDiffusionImage.getVOIs();
		// }

		Vector<VOIBase>[] vArrayCenter = VOIs.VOIAt(1).getSortedCurves(
				VOIBase.ZPLANE, zDim);
		VOIBase vCenter = vArrayCenter[midVOI].get(0);
		voiNew.importCurve(vCenter);

		Vector<VOIBase>[] vArrayStart = VOIs.VOIAt(0).getSortedCurves(
				VOIBase.ZPLANE, zDim);
		VOIBase vStart = vArrayStart[startVOI].get(0);
		voiNew.importCurve(vStart);

		Vector<VOIBase>[] vArrayEnd = VOIs.VOIAt(2).getSortedCurves(
				VOIBase.ZPLANE, zDim);
		VOIBase vEnd = vArrayEnd[endVOI].get(0);
		voiNew.importCurve(vEnd);

		voiNew.getSortedCurves(zDim);

		VOIs.removeAllElements();
		VOIs.add(voiNew);

	}

	/**
	 * Generate the Fuzzy-C means classes from the CED image
	 * 
	 * @param coherenceEnhancingDiffusionImage
	 *            CED image
	 * @param imageStackFuzzyC
	 *            Fuzzy-C means classes image stack.
	 */
	public void doFuzzyCmean(ModelImage coherenceEnhancingDiffusionImage,
			Vector<Vector<ModelImage>> imageStackFuzzyC) {

		int xDim = coherenceEnhancingDiffusionImage.getExtents()[0];
		int yDim = coherenceEnhancingDiffusionImage.getExtents()[1];
		int zDim = coherenceEnhancingDiffusionImage.getExtents()[2];

		int[] destExtents = new int[2];
		destExtents[0] = coherenceEnhancingDiffusionImage.getExtents()[0];
		destExtents[1] = coherenceEnhancingDiffusionImage.getExtents()[1];

		int sliceSize = xDim * yDim;
		int midSlice = (int) ((zDim) / 2f);
		System.out.println("loadMask: midSlice = " + midSlice);
		for (int z = 0; z < zDim; z++) {

			try {
				// currentSlice = z;
				int start = z * sliceSize;
				double[] sourceBuffer = new double[sliceSize];
				ModelImage[] resultImageFuzzyC = null;
				coherenceEnhancingDiffusionImage.exportData(start, sliceSize,
						sourceBuffer);

				ModelImage testImageFuzzyC = new ModelImage(
						coherenceEnhancingDiffusionImage.getType(),
						destExtents, "testFuzzyC" + z);
				testImageFuzzyC.importData(0, sourceBuffer, true);
				boolean endianness = coherenceEnhancingDiffusionImage
						.getFileInfo(0).getEndianess();
				int modality = coherenceEnhancingDiffusionImage.getFileInfo(0)
						.getModality();
				float[] res = coherenceEnhancingDiffusionImage.getFileInfo(0)
						.getResolutions();
				float[] newRes = { res[0], res[1] };
				int[] exts = coherenceEnhancingDiffusionImage.getFileInfo(0)
						.getExtents();
				int[] newExts = { exts[0], exts[1] };
				int[] units = coherenceEnhancingDiffusionImage.getFileInfo(0)
						.getUnitsOfMeasure();
				int[] newUnits = { units[0], units[1] };
				int imageOrient = coherenceEnhancingDiffusionImage.getFileInfo(
						0).getImageOrientation();
				int[] axisOrient = coherenceEnhancingDiffusionImage
						.getFileInfo(0).getAxisOrientation();
				int[] newAxisOrient = { axisOrient[0], axisOrient[1] };
				float[] origin = coherenceEnhancingDiffusionImage
						.getFileInfo(0).getOrigin();
				float[] newOrigin = { origin[0], origin[1] };

				FileInfoImageXML fileInfo = new FileInfoImageXML(
						coherenceEnhancingDiffusionImage.getImageName(), null,
						FileUtility.XML);
				fileInfo.setDataType(ModelStorageBase.FLOAT);
				fileInfo.setEndianess(endianness);
				fileInfo.setExtents(newExts);
				fileInfo.setModality(modality);
				fileInfo.setResolutions(newRes);
				fileInfo.setUnitsOfMeasure(newUnits);
				fileInfo.setImageOrientation(imageOrient);
				fileInfo.setAxisOrientation(newAxisOrient);
				fileInfo.setOrigin(newOrigin);

				FileInfoImageXML[] fileInfos = { fileInfo };
				testImageFuzzyC.setFileInfo(fileInfos);

				boolean regionFlag = true;
				int nClasses = 5;
				float q = 2.0f;
				boolean cropBackground = false;
				float endTol = 0.01f;
				int maxIter = 200;
				int segmentation = AlgorithmFuzzyCMeans.BOTH_FUZZY_HARD;
				int resultNumber = 6;
				int nPyramid = 4;
				int oneJacobiIter = 1;
				float oneSmooth = 20000;
				boolean outputGainField = false;
				int twoJacobiIter = 2;
				float twoSmooth = 200000;
				float threshold = 0;

				try {
					resultImageFuzzyC = new ModelImage[resultNumber];
					int presentNumber = 0;

					for (int i = 0; i < nClasses; i++) {
						String name = makeImageName(
								testImageFuzzyC.getImageName(), "_class"
										+ (i + 1));
						resultImageFuzzyC[presentNumber++] = new ModelImage(
								ModelStorageBase.FLOAT, destExtents, name);
					}
					resultImageFuzzyC[presentNumber++] = new ModelImage(
							ModelStorageBase.UBYTE, destExtents, makeImageName(
									testImageFuzzyC.getImageName(), "_seg"));

					// fuzzyC parameters

					// Make algorithm
					AlgorithmFuzzyCMeans fcmAlgo = new AlgorithmFuzzyCMeans(
							resultImageFuzzyC, testImageFuzzyC, nClasses,
							nPyramid, oneJacobiIter, twoJacobiIter, q,
							oneSmooth, twoSmooth, outputGainField,
							segmentation, cropBackground, threshold, maxIter,
							endTol, regionFlag);

					fcmAlgo.addListener(this);

					float[] centroids = null;
					centroids = getCentroids(testImageFuzzyC, fcmAlgo,
							centroids);

					fcmAlgo.setCentroids(centroids);
					// fcmAlgo.setThreshold(threshold);
					fcmAlgo.run();

					centroids = null;

					int ii;
					testImageFuzzyC.clearMask();
					Vector<ModelImage> imageStackElement = new Vector<ModelImage>();
					for (ii = 0; ii < resultNumber; ii++) {
						updateFileInfo(testImageFuzzyC, resultImageFuzzyC[ii]);
						resultImageFuzzyC[ii].clearMask();
						imageStackElement.add(resultImageFuzzyC[ii]);
					}
					imageStackFuzzyC.add(imageStackElement);
					testImageFuzzyC.disposeLocal();
					testImageFuzzyC = null;

					fcmAlgo.finalize();
					fcmAlgo = null;

				} catch (OutOfMemoryError x) {

					if (resultImageFuzzyC != null) {
						for (int i = 0; i < resultNumber; i++) {
							if (resultImageFuzzyC[i] != null) {
								resultImageFuzzyC[i].disposeLocal();
								resultImageFuzzyC[i] = null;
							}
						}
						resultImageFuzzyC = null;
					}

					// System.gc();
					MipavUtil
							.displayError("MS Fuzzy CMeans: unable to allocate enough memory");
					return;
				}

				// z = endSlice; // debug ending condition.
				sourceBuffer = null;

			} catch (IOException e) {
				e.printStackTrace();
			}

			// z = endSlice;

		} // end z loop

	}

	/**
	 * Gets the minimum and maximum of each image and initializes the centroids
	 * dialog appropriately.
	 * 
	 * @return Flag indicating a successful get.
	 */
	private float[] getCentroids(ModelImage srcImage,
			AlgorithmFuzzyCMeans fcmAlgo, float[] centroids) {
		boolean regionFlag = true;
		int nClasses = 5;
		float q = 2.0f;
		boolean cropBackground = false;
		float endTol = 0.01f;
		int maxIter = 200;
		int segmentation = AlgorithmFuzzyCMeans.BOTH_FUZZY_HARD;
		int resultNumber = 6;
		int nPyramid = 4;
		int oneJacobiIter = 1;
		float oneSmooth = 20000;
		boolean outputGainField = false;
		int twoJacobiIter = 2;
		float twoSmooth = 200000;
		float threshold = 0;

		int i;
		float minimum, maximum;
		int xDim = srcImage.getExtents()[0];
		int yDim = srcImage.getExtents()[1];
		int zDim;

		if (srcImage.getNDims() > 2) {
			zDim = srcImage.getExtents()[2];
		} else {
			zDim = 1;
		}

		int sliceSize = xDim * yDim;
		int volSize = xDim * yDim * zDim;
		float[] buffer = null;
		int yStepIn, yStepOut, zStepIn, zStepOut;
		int x, y, z, index, newXDim, newYDim, newZDim, newSliceSize;

		try {
			buffer = new float[volSize];
			srcImage.exportData(0, volSize, buffer);

			srcImage.calcMinMax();
			minimum = (float) srcImage.getMin();
			maximum = (float) srcImage.getMax();

			if (!regionFlag) {
				maximum = -Float.MAX_VALUE;
				minimum = Float.MAX_VALUE;

				for (i = 0; i < volSize; i++) {

					if (fcmAlgo.getMask().get(i)) {

						if (buffer[i] > maximum) {
							maximum = buffer[i];
						}

						if (buffer[i] < minimum) {
							minimum = buffer[i];
						}
					}
				}
			} // if (!wholeImage)

			int xLow = 0;
			int yLow = 0;
			int zLow = 0;
			int xHigh = xDim - 1;
			int yHigh = yDim - 1;
			int zHigh = zDim - 1;

			if (cropBackground) {

				// Find the smallest bounding box for the data
				xLow = xDim - 1;
				yLow = yDim - 1;
				zLow = zDim - 1;
				xHigh = 0;
				yHigh = 0;
				zHigh = 0;

				for (z = 0; z < zDim; z++) {
					zStepIn = z * sliceSize;

					for (y = 0; y < yDim; y++) {
						yStepIn = (y * xDim) + zStepIn;

						for (x = 0; x < xDim; x++) {
							index = x + yStepIn;

							if (buffer[index] >= threshold) {

								if (x < xLow) {
									xLow = x;
								}

								if (x > xHigh) {
									xHigh = x;
								}

								if (y < yLow) {
									yLow = y;
								}

								if (y > yHigh) {
									yHigh = y;
								}

								if (z < zLow) {
									zLow = z;
								}

								if (z > zHigh) {
									zHigh = z;
								}
							} // if (buffer[index] > threshold)
						} // for (x = 0; x < xDim; x++)
					} // for (y = 0; y < yDim; y++)
				} // for (z = 0; z < zDim; z++)

				if ((xLow > 0) || (xHigh < (xDim - 1)) || (yLow > 0)
						|| (yHigh < (yDim - 1)) || (zLow > 0)
						|| (zHigh < (zDim - 1))) {

					// A smaller bounding box has been found for the data
					// Recopy area to smaller data array to save space
					newXDim = xHigh - xLow + 1;
					newYDim = yHigh - yLow + 1;
					newZDim = zHigh - zLow + 1;

					float[] buffer2 = new float[newXDim * newYDim * newZDim];
					newSliceSize = newXDim * newYDim;

					for (z = zLow; z <= zHigh; z++) {
						zStepOut = z * sliceSize;
						zStepIn = ((z - zLow) * newSliceSize) - xLow
								- (yLow * newXDim);

						for (y = yLow; y <= yHigh; y++) {
							yStepIn = (y * newXDim) + zStepIn;
							yStepOut = (y * xDim) + zStepOut;

							for (x = xLow; x <= xHigh; x++) {
								buffer2[x + yStepIn] = buffer[x + yStepOut];
							} // for (x = xLow; x <= xHigh; x++)
						} // for (y = yLow; y <= yHigh; y++)
					} // for (z = zLow; z <= zHigh; z++)

					xDim = newXDim;
					yDim = newYDim;
					zDim = newZDim;
					sliceSize = xDim * yDim;
					volSize = sliceSize * zDim;
					buffer = new float[volSize];

					for (i = 0; i < sliceSize; i++) {
						buffer[i] = buffer2[i];
					}

					buffer2 = null;

					// Find the new minimum
					minimum = maximum;

					for (i = 0; i < volSize; i++) {

						if (buffer[i] < minimum) {
							minimum = buffer[i];
						} // if (buffer[i] < minimum)
					} // for (i = 0; i < sliceSize; i++)
				} // if ((xLow > 0) || (xHigh < (xDim-1)) || (yLow > 0) ||
					// (yHigh < (yDim - 1)))
			} // if (cropBackground)
		} catch (java.io.IOException ioe) {
			buffer = null;
			// System.gc();
			MipavUtil.displayError("Error trying to get centroids.");

			return null;
		} catch (OutOfMemoryError error) {
			buffer = null;
			// System.gc();
			MipavUtil.displayError("Algorithm FuzzyCMeans reports:\n"
					+ error.toString());

			return null;
		}

		buffer = null;

		// Autodetect initial centroids
		centroids = new float[nClasses];

		JDialogInitialCentroids dialogInitialCentroids = new JDialogInitialCentroids(
				parentFrame, nClasses, minimum, maximum, false);
		dialogInitialCentroids.run();

		if (dialogInitialCentroids.isCancelled()) {
			centroids = null;
			return null;
		} else {

			for (i = 0; i < centroids.length; i++) {
				centroids[i] = dialogInitialCentroids.getCentroids()[i];
			}
		}

		return centroids;
	}

	/**
	 * The proposed model uses 2D registration to register 2D slices from the
	 * middle slice to the adjacent slice. It then generates a new VOI, and
	 * takes the VOI as the new initial estimate for the next slice in the
	 * series. This process is then iteratively propagated in both forward and
	 * backward directions.
	 * 
	 * @param image
	 *            original MR image
	 * @param coherenceEnhancingDiffusionImage
	 *            coherence enhance diffusion filter
	 * @param imageStackFuzzyC
	 *            Fuzzy-C segmentation stack
	 * @param midVOI
	 *            mid slice VOI
	 * @param startVOI
	 *            apex slice VOI
	 * @param endVOI
	 *            base slice VOI
	 * @param boundingBox
	 *            Cropped image bounding box
	 * @param axis
	 *            axis orientation
	 */
	public void doRegistrationMid(ModelImage image,
			ModelImage coherenceEnhancingDiffusionImage,
			Vector<Vector<ModelImage>> imageStackFuzzyC, int midVOI,
			int startVOI, int endVOI, int[] boundingBox) {

		VOIVector VOIs;
		VOIVector VOIsSrc;
		ModelImage refImage = null;
		ModelImage testImage = null;

		// Registration OAR 2D
		int costChoice = AlgorithmCostFunctions2D.CORRELATION_RATIO_SMOOTHED;
		int DOF = 7;
		int interp = AlgorithmTransform.BSPLINE3;
		float rotateBegin = -10.0f;
		float rotateEnd = 10.0f;
		float coarseRate = 2.0f;
		float fineRate = 6.0f;
		boolean doSubsample = true;
		boolean doMultiThread = true;
		int bracketBound = 10;
		int baseNumIter = 2;
		int numMinima = 3;
		boolean doJTEM = false;

		int currentSlice;

		ModelImage m_kImageDef = null;

		// ************************ Bspline parameters start
		// ***********************************
		AlgorithmRegBSpline.Options m_kOptionsPass1 = new AlgorithmRegBSpline.Options();
		AlgorithmRegBSpline.Options m_kOptionsPass2 = null;
		Controls m_kControlsPass1;
		RegistrationMeasure m_kRegMeasure = null;

		m_kControlsPass1 = new Controls(this);
		// Set defaults for Pass 1.
		m_kOptionsPass1.iBSplineDegree = 2;
		m_kOptionsPass1.iBSplineNumControlPoints = 8;
		m_kOptionsPass1.fGradientDescentMinimizeStepSize = 1.0f;
		m_kOptionsPass1.iGradientDescentMinimizeMaxSteps = 10;
		m_kOptionsPass1.fConvergenceLimit = 0.05f;
		m_kOptionsPass1.iMaxIterations = 50;
		m_kOptionsPass1.bSubsample = true;
		m_kControlsPass1.setValues(m_kOptionsPass1);

		// What is the text that appears in the combo box?
		final String kStrDescription = "Normalized Mutual Information";
		// "Least Squares"
		// "Correlation Ratio",
		// "Normalized Mutual Information"

		// Match that text to the known cost functions.
		if (RegistrationMeasureLeastSquares.getStaticName() == kStrDescription) {
			m_kRegMeasure = new RegistrationMeasureLeastSquares();
		} else if (RegistrationMeasureCorrelationRatio.getStaticName() == kStrDescription) {
			m_kRegMeasure = new RegistrationMeasureCorrelationRatio();
		} else if (RegistrationMeasureNormalizedMutualInformation
				.getStaticName() == kStrDescription) {
			m_kRegMeasure = new RegistrationMeasureNormalizedMutualInformation();
		}
		// *********************** end
		// ********************************************************

		int xDim = coherenceEnhancingDiffusionImage.getExtents()[0];
		int yDim = coherenceEnhancingDiffusionImage.getExtents()[1];
		int zDim = coherenceEnhancingDiffusionImage.getExtents()[2] - 1;

		int[] destExtents = new int[2];
		destExtents[0] = coherenceEnhancingDiffusionImage.getExtents()[0];
		destExtents[1] = coherenceEnhancingDiffusionImage.getExtents()[1];

		int sliceSize = xDim * yDim;
		int midSlice = midVOI; // (int) ((zDim) / 2f);
		System.out.println("loadMask: midSlice = " + midSlice);

		double[] midBuffer = new double[sliceSize];

		// new ViewJFrameImage(coherenceEnhancingDiffusionImage);

		// if (coherenceEnhancingDiffusionImage.getVOIs() != null) {
		VOIs = coherenceEnhancingDiffusionImage.getVOIs();
		// }

		// if (image.getVOIs() != null) {
		VOIsSrc = image.getVOIs();
		// }

		// find the intersection of the lower bound with the VOI.
		Vector<VOIBase>[] vArray = VOIs.VOIAt(0).getSortedCurves(
				VOIBase.ZPLANE, zDim);
		VOIBase v = vArray[midSlice].get(0);
		// VOI totalVOI = v.getGroup();

		Vector<VOIBase>[] vArraySrc = VOIsSrc.VOIAt(0).getSortedCurves(
				VOIBase.ZPLANE, zDim);
		VOIBase vSrc = vArraySrc[midSlice].get(0);
		VOI totalVOISrc = vSrc.getGroup();

		VOIBase vTemp = (VOIBase) v.clone();

		int nPts = vTemp.size();

		// zero out the z dimension VOI
		float[] xPts = new float[nPts];
		float[] yPts = new float[nPts];
		float[] zPts = new float[nPts];
		float[] zPtsZero = new float[nPts];

		vTemp.exportArrays(xPts, yPts, zPts);
		vTemp.importArrays(xPts, yPts, zPtsZero, nPts);

		VOIVector voiVectorNew = new VOIVector();
		VOI voiNew = new VOI((short) 0, "blank");
		voiNew.importCurve(vTemp);
		voiVectorNew.add(voiNew);

		// get the mid slice
		try {
			refImage = new ModelImage(
					coherenceEnhancingDiffusionImage.getType(), destExtents,
					"ref" + midSlice);
			coherenceEnhancingDiffusionImage.exportData(midSlice * sliceSize,
					sliceSize, midBuffer);
			refImage.importData(0, midBuffer, true);
			refImage.addVOIs(voiVectorNew);
			// new ViewJFrameImage(refImage);

			FileInfoImageXML fileInfoTarget = new FileInfoImageXML(
					coherenceEnhancingDiffusionImage.getImageName(), null,
					FileUtility.XML);
			float[] resTarget = coherenceEnhancingDiffusionImage.getFileInfo(0)
					.getResolutions();
			float[] newResTarget = { resTarget[0], resTarget[1] };
			int[] extsTarget = coherenceEnhancingDiffusionImage.getFileInfo(0)
					.getExtents();
			int[] newExtsTarget = { extsTarget[0], extsTarget[1] };
			fileInfoTarget.setExtents(newExtsTarget);
			fileInfoTarget.setResolutions(newResTarget);
			FileInfoImageXML[] fileInfosTarget = { fileInfoTarget };
			refImage.setFileInfo(fileInfosTarget);

		} catch (IOException e) {
			e.printStackTrace();
		}

		int startSlice = midSlice - 1;
		int endSlice = startVOI;

		for (int z = startSlice; z > endSlice; z--) {

			try {
				currentSlice = z;
				int start = z * sliceSize;
				double[] sourceBuffer = new double[sliceSize];
				coherenceEnhancingDiffusionImage.exportData(start, sliceSize,
						sourceBuffer);

				testImage = new ModelImage(
						coherenceEnhancingDiffusionImage.getType(),
						destExtents, "test" + z);
				testImage.importData(0, sourceBuffer, true);
				boolean endianness = coherenceEnhancingDiffusionImage
						.getFileInfo(0).getEndianess();
				int modality = coherenceEnhancingDiffusionImage.getFileInfo(0)
						.getModality();
				float[] res = coherenceEnhancingDiffusionImage.getFileInfo(0)
						.getResolutions();
				float[] newRes = { res[0], res[1] };
				int[] exts = coherenceEnhancingDiffusionImage.getFileInfo(0)
						.getExtents();
				int[] newExts = { exts[0], exts[1] };
				int[] units = coherenceEnhancingDiffusionImage.getFileInfo(0)
						.getUnitsOfMeasure();
				int[] newUnits = { units[0], units[1] };
				int imageOrient = coherenceEnhancingDiffusionImage.getFileInfo(
						0).getImageOrientation();
				int[] axisOrient = coherenceEnhancingDiffusionImage
						.getFileInfo(0).getAxisOrientation();
				int[] newAxisOrient = { axisOrient[0], axisOrient[1] };
				float[] origin = coherenceEnhancingDiffusionImage
						.getFileInfo(0).getOrigin();
				float[] newOrigin = { origin[0], origin[1] };

				// testImage.addVOIs(voiVectorNew);
				FileInfoImageXML fileInfo = new FileInfoImageXML(
						coherenceEnhancingDiffusionImage.getImageName(), null,
						FileUtility.XML);
				fileInfo.setDataType(ModelStorageBase.FLOAT);
				fileInfo.setEndianess(endianness);
				fileInfo.setExtents(newExts);
				fileInfo.setModality(modality);
				fileInfo.setResolutions(newRes);
				fileInfo.setUnitsOfMeasure(newUnits);
				fileInfo.setImageOrientation(imageOrient);
				fileInfo.setAxisOrientation(newAxisOrient);
				fileInfo.setOrigin(newOrigin);

				FileInfoImageXML[] fileInfos = { fileInfo };
				testImage.setFileInfo(fileInfos);

				ModelImage resultImage = new ModelImage(testImage.getType(),
						destExtents, "result" + z);
				resultImage.importData(0, sourceBuffer, true);
				resultImage.getVOIs().removeAllElements();

				if (useBSpline) {
					int[] aiExtentsReg = new int[2];
					aiExtentsReg = testImage.getExtents();

					m_kControlsPass1.getValues(m_kOptionsPass1, aiExtentsReg);

					AlgorithmRegBSpline2D m_kAlgorithmReg = new AlgorithmRegBSpline2D(
							resultImage, refImage, testImage, m_kImageDef,
							m_kRegMeasure, m_kOptionsPass1, m_kOptionsPass2);
					m_kAlgorithmReg.addListener(this);
					m_kAlgorithmReg.run();

					if (m_kAlgorithmReg != null
							&& m_kAlgorithmReg.isCompleted()) {
						m_kAlgorithmReg.setCompleted(true);
						m_kAlgorithmReg.disposeLocal();
						m_kAlgorithmReg = null;
					}
				} else {
					AlgorithmRegOAR2D reg2 = new AlgorithmRegOAR2D(testImage,
							refImage, costChoice, DOF, interp, rotateBegin,
							rotateEnd, coarseRate, fineRate, doSubsample,
							doMultiThread, bracketBound, baseNumIter);

					reg2.setJTEM(doJTEM);
					reg2.addListener(this);
					reg2.run();

					int interp2 = AlgorithmTransform.BSPLINE3;
					float fillValue = 0.0f;
					boolean findSolution = false;
					if (reg2 != null && reg2.isCompleted()) {
						final TransMatrix finalMatrix = reg2.getTransform();

						if (finalMatrix != null) {
							findSolution = true;
							ModelImage tempImage = testImage;

							final int xdimA = tempImage.getExtents()[0];
							final int ydimA = tempImage.getExtents()[1];

							final float xresA = tempImage.getFileInfo(0)
									.getResolutions()[0];
							final float yresA = tempImage.getFileInfo(0)
									.getResolutions()[1];

							// final String name =
							// JDialogBase.makeImageName(refImageSlice.getImageName(),
							// "_register1");

							AlgorithmTransform transform = new AlgorithmTransform(
									refImage, finalMatrix, interp2, xresA,
									yresA, xdimA, ydimA, true, false, false);

							transform.setUpdateOriginFlag(true);
							transform.setFillValue(fillValue);
							transform.run();
							resultImage = transform.getTransformedImage();
							transform.finalize();

							resultImage.calcMinMax();
							// resultImage.setImageName(name);
							resultImage.setType(refImage.getType());
							if (transform != null) {
								transform.disposeLocal();
								transform = null;
							}
						}
					}
				}
				// new ViewJFrameImage(resultImage);
				// segmentFromFuzzyC(imageStackFuzzyC, currentSlice,
				// resultImage, refImage, testImage);
				transformVOI(image, resultImage, refImage, testImage,
						boundingBox, currentSlice);

				// Switch the images, copy current slice image to refImage
				// testImage will be the next slice.
				refImage.importData(0, sourceBuffer, true);
				refImage.getVOIs().removeAllElements();
				VOIVector resultingVOIs = resultImage.getVOIs();
				refImage.addVOIs(resultingVOIs);
				refImage.setImageName("ref" + (z - 1));

				testImage.disposeLocal();
				testImage = null;
				resultImage.disposeLocal();
				resultImage = null;
				sourceBuffer = null;

				// z = endSlice; // debug ending condition.
			} catch (IOException e) {
				e.printStackTrace();
			}

			// z = endSlice;

		} // end z loop

		refImage.disposeLocal();
		refImage = null;

		/************* copy the middle slice VOI *****************/

		vArray = VOIs.VOIAt(0).getSortedCurves(VOIBase.ZPLANE, zDim);
		v = vArray[midSlice].get(0);
		vTemp = (VOIBase) v.clone();
		nPts = vTemp.size();
		// zero out the z dimension VOI
		xPts = new float[nPts];
		yPts = new float[nPts];
		zPts = new float[nPts];
		zPtsZero = new float[nPts];
		vTemp.exportArrays(xPts, yPts, zPts);
		vTemp.importArrays(xPts, yPts, zPtsZero, nPts);
		voiVectorNew = new VOIVector();
		voiNew = new VOI((short) 0, "blank");
		voiNew.importCurve(vTemp);
		voiVectorNew.add(voiNew);

		/************* copy the middle slice VOI end ***************/
		try {
			refImage = new ModelImage(
					coherenceEnhancingDiffusionImage.getType(), destExtents,
					"ref" + midSlice);
			coherenceEnhancingDiffusionImage.exportData(midSlice * sliceSize,
					sliceSize, midBuffer);
			refImage.importData(0, midBuffer, true);
			refImage.addVOIs(voiVectorNew);
			// new ViewJFrameImage(refImage);

			FileInfoImageXML fileInfoTarget = new FileInfoImageXML(
					coherenceEnhancingDiffusionImage.getImageName(), null,
					FileUtility.XML);
			float[] resTarget = coherenceEnhancingDiffusionImage.getFileInfo(0)
					.getResolutions();
			float[] newResTarget = { resTarget[0], resTarget[1] };
			int[] extsTarget = coherenceEnhancingDiffusionImage.getFileInfo(0)
					.getExtents();
			int[] newExtsTarget = { extsTarget[0], extsTarget[1] };
			fileInfoTarget.setExtents(newExtsTarget);
			fileInfoTarget.setResolutions(newResTarget);
			FileInfoImageXML[] fileInfosTarget = { fileInfoTarget };
			refImage.setFileInfo(fileInfosTarget);

		} catch (IOException e) {
			e.printStackTrace();
		}

		startSlice = midSlice + 1;
		endSlice = endVOI;

		for (int z = startSlice; z < endSlice; z++) {

			try {
				currentSlice = z;
				int start = z * sliceSize;
				double[] sourceBuffer = new double[sliceSize];
				coherenceEnhancingDiffusionImage.exportData(start, sliceSize,
						sourceBuffer);

				testImage = new ModelImage(
						coherenceEnhancingDiffusionImage.getType(),
						destExtents, "test" + z);
				testImage.importData(0, sourceBuffer, true);
				boolean endianness = coherenceEnhancingDiffusionImage
						.getFileInfo(0).getEndianess();
				int modality = coherenceEnhancingDiffusionImage.getFileInfo(0)
						.getModality();
				float[] res = coherenceEnhancingDiffusionImage.getFileInfo(0)
						.getResolutions();
				float[] newRes = { res[0], res[1] };
				int[] exts = coherenceEnhancingDiffusionImage.getFileInfo(0)
						.getExtents();
				int[] newExts = { exts[0], exts[1] };
				int[] units = coherenceEnhancingDiffusionImage.getFileInfo(0)
						.getUnitsOfMeasure();
				int[] newUnits = { units[0], units[1] };
				int imageOrient = coherenceEnhancingDiffusionImage.getFileInfo(
						0).getImageOrientation();
				int[] axisOrient = coherenceEnhancingDiffusionImage
						.getFileInfo(0).getAxisOrientation();
				int[] newAxisOrient = { axisOrient[0], axisOrient[1] };
				float[] origin = coherenceEnhancingDiffusionImage
						.getFileInfo(0).getOrigin();
				float[] newOrigin = { origin[0], origin[1] };

				FileInfoImageXML fileInfo = new FileInfoImageXML(
						coherenceEnhancingDiffusionImage.getImageName(), null,
						FileUtility.XML);
				fileInfo.setDataType(ModelStorageBase.FLOAT);
				fileInfo.setEndianess(endianness);
				fileInfo.setExtents(newExts);
				fileInfo.setModality(modality);
				fileInfo.setResolutions(newRes);
				fileInfo.setUnitsOfMeasure(newUnits);
				fileInfo.setImageOrientation(imageOrient);
				fileInfo.setAxisOrientation(newAxisOrient);
				fileInfo.setOrigin(newOrigin);

				FileInfoImageXML[] fileInfos = { fileInfo };
				testImage.setFileInfo(fileInfos);

				ModelImage resultImage = new ModelImage(testImage.getType(),
						destExtents, "result" + z);
				resultImage.importData(0, sourceBuffer, true);
				resultImage.getVOIs().removeAllElements();

				if (useBSpline) {
					int[] aiExtentsReg = new int[2];
					aiExtentsReg = testImage.getExtents();

					m_kControlsPass1.getValues(m_kOptionsPass1, aiExtentsReg);

					AlgorithmRegBSpline2D m_kAlgorithmReg = new AlgorithmRegBSpline2D(
							resultImage, refImage, testImage, m_kImageDef,
							m_kRegMeasure, m_kOptionsPass1, m_kOptionsPass2);
					m_kAlgorithmReg.addListener(this);
					m_kAlgorithmReg.run();

					if (m_kAlgorithmReg != null
							&& m_kAlgorithmReg.isCompleted()) {
						m_kAlgorithmReg.setCompleted(true);
						m_kAlgorithmReg.disposeLocal();
						m_kAlgorithmReg = null;
					}
				} else {
					AlgorithmRegOAR2D reg2 = new AlgorithmRegOAR2D(testImage,
							refImage, costChoice, DOF, interp, rotateBegin,
							rotateEnd, coarseRate, fineRate, doSubsample,
							doMultiThread, bracketBound, baseNumIter);

					reg2.setJTEM(doJTEM);
					reg2.addListener(this);
					reg2.run();

					int interp2 = AlgorithmTransform.BSPLINE3;
					float fillValue = 0.0f;
					boolean findSolution = false;
					if (reg2 != null && reg2.isCompleted()) {
						final TransMatrix finalMatrix = reg2.getTransform();

						if (finalMatrix != null) {
							findSolution = true;
							ModelImage tempImage = testImage;

							final int xdimA = tempImage.getExtents()[0];
							final int ydimA = tempImage.getExtents()[1];

							final float xresA = tempImage.getFileInfo(0)
									.getResolutions()[0];
							final float yresA = tempImage.getFileInfo(0)
									.getResolutions()[1];

							// final String name =
							// JDialogBase.makeImageName(refImageSlice.getImageName(),
							// "_register1");

							AlgorithmTransform transform = new AlgorithmTransform(
									refImage, finalMatrix, interp2, xresA,
									yresA, xdimA, ydimA, true, false, false);

							transform.setUpdateOriginFlag(true);
							transform.setFillValue(fillValue);
							transform.run();
							resultImage = transform.getTransformedImage();
							transform.finalize();

							resultImage.calcMinMax();
							// resultImage.setImageName(name);
							resultImage.setType(refImage.getType());
							if (transform != null) {
								transform.disposeLocal();
								transform = null;
							}
						}
					}
				}
				// segmentFromFuzzyC(imageStackFuzzyC, currentSlice,
				// resultImage, refImage, testImage);
				transformVOI(image, resultImage, refImage, testImage,
						boundingBox, currentSlice);

				refImage.importData(0, sourceBuffer, true);
				refImage.getVOIs().removeAllElements();
				VOIVector resultingVOIs = resultImage.getVOIs();
				refImage.addVOIs(resultingVOIs);
				refImage.setImageName("ref" + (z - 1));

				testImage.disposeLocal();
				testImage = null;
				resultImage.disposeLocal();
				resultImage = null;
				sourceBuffer = null;

				// z = endSlice; // debug ending condition.
			} catch (IOException e) {
				e.printStackTrace();
			}

		} // end z loop

		midBuffer = null;
	}

	/**
	 * Near the apex, apex contour starts as the initial estimate, and repeat
	 * the same process toward the end slices.
	 * 
	 * @param image
	 *            original MR image
	 * @param coherenceEnhancingDiffusionImage
	 *            coherence enhance diffusion filter
	 * @param imageStackFuzzyC
	 *            Fuzzy-C segmentation stack
	 * @param startVOI
	 *            apex starting slice VOI number
	 * @param boundingBox
	 *            Cropped image bounding box
	 * @param axis
	 *            axis orientation
	 */
	public void doRegistrationStart(ModelImage image,
			ModelImage coherenceEnhancingDiffusionImage,
			Vector<Vector<ModelImage>> imageStackFuzzyC, int startVOI,
			int[] boundingBox) {

		VOIVector VOIs;
		VOIVector VOIsSrc;
		ModelImage refImage = null;
		ModelImage testImage = null;
		ModelImage m_kImageDef = null;
		int currentSlice;

		// Registration OAR 2D
		int costChoice = AlgorithmCostFunctions2D.CORRELATION_RATIO_SMOOTHED;
		int DOF = 7;
		int interp = AlgorithmTransform.BSPLINE3;
		float rotateBegin = -10.0f;
		float rotateEnd = 10.0f;
		float coarseRate = 2.0f;
		float fineRate = 6.0f;
		boolean doSubsample = true;
		boolean doMultiThread = true;
		int bracketBound = 10;
		int baseNumIter = 2;
		int numMinima = 3;
		boolean doJTEM = false;

		// ************************** Bspline parameters start
		// *******************************************
		AlgorithmRegBSpline.Options m_kOptionsPass1 = new AlgorithmRegBSpline.Options();
		AlgorithmRegBSpline.Options m_kOptionsPass2 = null;
		Controls m_kControlsPass1;
		RegistrationMeasure m_kRegMeasure = null;

		m_kControlsPass1 = new Controls(this);
		// Set defaults for Pass 1.
		m_kOptionsPass1.iBSplineDegree = 2;
		m_kOptionsPass1.iBSplineNumControlPoints = 8;
		m_kOptionsPass1.fGradientDescentMinimizeStepSize = 1.0f;
		m_kOptionsPass1.iGradientDescentMinimizeMaxSteps = 10;
		m_kOptionsPass1.fConvergenceLimit = 0.05f;
		m_kOptionsPass1.iMaxIterations = 50;
		m_kOptionsPass1.bSubsample = true;
		m_kControlsPass1.setValues(m_kOptionsPass1);

		// What is the text that appears in the combo box?
		final String kStrDescription = "Normalized Mutual Information";
		// "Least Squares"
		// "Correlation Ratio",
		// "Normalized Mutual Information"

		// Match that text to the known cost functions.
		if (RegistrationMeasureLeastSquares.getStaticName() == kStrDescription) {
			m_kRegMeasure = new RegistrationMeasureLeastSquares();
		} else if (RegistrationMeasureCorrelationRatio.getStaticName() == kStrDescription) {
			m_kRegMeasure = new RegistrationMeasureCorrelationRatio();
		} else if (RegistrationMeasureNormalizedMutualInformation
				.getStaticName() == kStrDescription) {
			m_kRegMeasure = new RegistrationMeasureNormalizedMutualInformation();
		}
		// ******************************** end
		// *********************************************

		int xDim = coherenceEnhancingDiffusionImage.getExtents()[0];
		int yDim = coherenceEnhancingDiffusionImage.getExtents()[1];
		int zDim = coherenceEnhancingDiffusionImage.getExtents()[2] - 1;

		int[] destExtents = new int[2];
		destExtents[0] = coherenceEnhancingDiffusionImage.getExtents()[0];
		destExtents[1] = coherenceEnhancingDiffusionImage.getExtents()[1];

		int sliceSize = xDim * yDim;
		int midSlice = startVOI; // (int) ((zDim) / 2f);
		System.out.println("loadMask: startSlice = " + midSlice);

		double[] midBuffer = new double[sliceSize];

		// UI.registerImage(coherenceEnhancingDiffusionImage);
		// new ViewJFrameImage(coherenceEnhancingDiffusionImage);

		// if (coherenceEnhancingDiffusionImage.getVOIs() != null) {
		VOIs = coherenceEnhancingDiffusionImage.getVOIs();
		// }

		// if (image.getVOIs() != null) {
		VOIsSrc = image.getVOIs();
		// }

		// find the intersection of the lower bound with the VOI.
		Vector<VOIBase>[] vArray = VOIs.VOIAt(0).getSortedCurves(
				VOIBase.ZPLANE, zDim);
		VOIBase v = vArray[midSlice].get(0);
		// VOI totalVOI = v.getGroup();

		Vector<VOIBase>[] vArraySrc = VOIsSrc.VOIAt(0).getSortedCurves(
				VOIBase.ZPLANE, zDim);
		VOIBase vSrc = vArraySrc[midSlice].get(0);
		VOI totalVOISrc = vSrc.getGroup();

		VOIBase vTemp = (VOIBase) v.clone();

		int nPts = vTemp.size();

		// zero out the z dimension VOI
		float[] xPts = new float[nPts];
		float[] yPts = new float[nPts];
		float[] zPts = new float[nPts];
		float[] zPtsZero = new float[nPts];

		vTemp.exportArrays(xPts, yPts, zPts);
		vTemp.importArrays(xPts, yPts, zPtsZero, nPts);

		VOIVector voiVectorNew = new VOIVector();
		VOI voiNew = new VOI((short) 0, "blank");
		voiNew.importCurve(vTemp);
		voiVectorNew.add(voiNew);

		// get the mid slice
		try {
			refImage = new ModelImage(
					coherenceEnhancingDiffusionImage.getType(), destExtents,
					"ref" + midSlice);
			coherenceEnhancingDiffusionImage.exportData(midSlice * sliceSize,
					sliceSize, midBuffer);
			refImage.importData(0, midBuffer, true);
			refImage.addVOIs(voiVectorNew);
			// new ViewJFrameImage(refImage);

			FileInfoImageXML fileInfoTarget = new FileInfoImageXML(
					coherenceEnhancingDiffusionImage.getImageName(), null,
					FileUtility.XML);
			float[] resTarget = coherenceEnhancingDiffusionImage.getFileInfo(0)
					.getResolutions();
			float[] newResTarget = { resTarget[0], resTarget[1] };
			int[] extsTarget = coherenceEnhancingDiffusionImage.getFileInfo(0)
					.getExtents();
			int[] newExtsTarget = { extsTarget[0], extsTarget[1] };
			fileInfoTarget.setExtents(newExtsTarget);
			fileInfoTarget.setResolutions(newResTarget);
			FileInfoImageXML[] fileInfosTarget = { fileInfoTarget };
			refImage.setFileInfo(fileInfosTarget);

		} catch (IOException e) {
			e.printStackTrace();
		}

		int startSlice = midSlice - 1;
		int endSlice = midSlice - tracingSliceNumber;

		for (int z = startSlice; z > endSlice; z--) {

			try {
				currentSlice = z;
				int start = z * sliceSize;
				double[] sourceBuffer = new double[sliceSize];
				coherenceEnhancingDiffusionImage.exportData(start, sliceSize,
						sourceBuffer);

				testImage = new ModelImage(
						coherenceEnhancingDiffusionImage.getType(),
						destExtents, "test" + z);
				testImage.importData(0, sourceBuffer, true);
				boolean endianness = coherenceEnhancingDiffusionImage
						.getFileInfo(0).getEndianess();
				int modality = coherenceEnhancingDiffusionImage.getFileInfo(0)
						.getModality();
				float[] res = coherenceEnhancingDiffusionImage.getFileInfo(0)
						.getResolutions();
				float[] newRes = { res[0], res[1] };
				int[] exts = coherenceEnhancingDiffusionImage.getFileInfo(0)
						.getExtents();
				int[] newExts = { exts[0], exts[1] };
				int[] units = coherenceEnhancingDiffusionImage.getFileInfo(0)
						.getUnitsOfMeasure();
				int[] newUnits = { units[0], units[1] };
				int imageOrient = coherenceEnhancingDiffusionImage.getFileInfo(
						0).getImageOrientation();
				int[] axisOrient = coherenceEnhancingDiffusionImage
						.getFileInfo(0).getAxisOrientation();
				int[] newAxisOrient = { axisOrient[0], axisOrient[1] };
				float[] origin = coherenceEnhancingDiffusionImage
						.getFileInfo(0).getOrigin();
				float[] newOrigin = { origin[0], origin[1] };

				// testImage.addVOIs(voiVectorNew);
				FileInfoImageXML fileInfo = new FileInfoImageXML(
						coherenceEnhancingDiffusionImage.getImageName(), null,
						FileUtility.XML);
				fileInfo.setDataType(ModelStorageBase.FLOAT);
				fileInfo.setEndianess(endianness);
				fileInfo.setExtents(newExts);
				fileInfo.setModality(modality);
				fileInfo.setResolutions(newRes);
				fileInfo.setUnitsOfMeasure(newUnits);
				fileInfo.setImageOrientation(imageOrient);
				fileInfo.setAxisOrientation(newAxisOrient);
				fileInfo.setOrigin(newOrigin);

				FileInfoImageXML[] fileInfos = { fileInfo };
				testImage.setFileInfo(fileInfos);

				ModelImage resultImage = new ModelImage(testImage.getType(),
						destExtents, "result" + z);
				resultImage.importData(0, sourceBuffer, true);
				resultImage.getVOIs().removeAllElements();

				if (useBSpline) {
					int[] aiExtentsReg = new int[2];
					aiExtentsReg = testImage.getExtents();

					m_kControlsPass1.getValues(m_kOptionsPass1, aiExtentsReg);

					AlgorithmRegBSpline2D m_kAlgorithmReg = new AlgorithmRegBSpline2D(
							resultImage, refImage, testImage, m_kImageDef,
							m_kRegMeasure, m_kOptionsPass1, m_kOptionsPass2);
					m_kAlgorithmReg.addListener(this);
					m_kAlgorithmReg.run();

					if (m_kAlgorithmReg != null
							&& m_kAlgorithmReg.isCompleted()) {
						m_kAlgorithmReg.setCompleted(true);
						m_kAlgorithmReg.disposeLocal();
						m_kAlgorithmReg = null;
					}
				} else {
					AlgorithmRegOAR2D reg2 = new AlgorithmRegOAR2D(testImage,
							refImage, costChoice, DOF, interp, rotateBegin,
							rotateEnd, coarseRate, fineRate, doSubsample,
							doMultiThread, bracketBound, baseNumIter);

					reg2.setJTEM(doJTEM);
					reg2.addListener(this);
					reg2.run();

					int interp2 = AlgorithmTransform.BSPLINE3;
					float fillValue = 0.0f;
					boolean findSolution = false;
					if (reg2 != null && reg2.isCompleted()) {
						final TransMatrix finalMatrix = reg2.getTransform();

						if (finalMatrix != null) {
							findSolution = true;
							ModelImage tempImage = testImage;

							final int xdimA = tempImage.getExtents()[0];
							final int ydimA = tempImage.getExtents()[1];

							final float xresA = tempImage.getFileInfo(0)
									.getResolutions()[0];
							final float yresA = tempImage.getFileInfo(0)
									.getResolutions()[1];

							// final String name =
							// JDialogBase.makeImageName(refImageSlice.getImageName(),
							// "_register1");

							AlgorithmTransform transform = new AlgorithmTransform(
									refImage, finalMatrix, interp2, xresA,
									yresA, xdimA, ydimA, true, false, false);

							transform.setUpdateOriginFlag(true);
							transform.setFillValue(fillValue);
							transform.run();
							resultImage = transform.getTransformedImage();
							transform.finalize();

							resultImage.calcMinMax();
							// resultImage.setImageName(name);
							resultImage.setType(refImage.getType());
							if (transform != null) {
								transform.disposeLocal();
								transform = null;
							}
						}
					}
				}
				// segmentFromFuzzyC(imageStackFuzzyC, currentSlice,
				// resultImage, refImage, testImage);
				transformVOI(image, resultImage, refImage, testImage,
						boundingBox, currentSlice);
				// new ViewJFrameImage(resultImage);

				// Switch the images, copy current slice image to refImage
				// testImage will be the next slice.
				refImage.importData(0, sourceBuffer, true);
				refImage.getVOIs().removeAllElements();
				VOIVector resultingVOIs = resultImage.getVOIs();
				refImage.addVOIs(resultingVOIs);
				refImage.setImageName("ref" + (z - 1));

				testImage.disposeLocal();
				testImage = null;
				resultImage.disposeLocal();
				resultImage = null;
				sourceBuffer = null;

				// z = endSlice; // debug ending condition.
			} catch (IOException e) {
				e.printStackTrace();
			}

			// z = endSlice;

		} // end z loop

		refImage.disposeLocal();
		refImage = null;
		midBuffer = null;

	}

	/**
	 * Near the base, base contour starts as the initial estimate, and repeat
	 * the same process toward the end slices.
	 * 
	 * @param image
	 *            original MR image
	 * @param coherenceEnhancingDiffusionImage
	 *            coherence enhance diffusion filter
	 * @param imageStackFuzzyC
	 *            Fuzzy-C segmentation stack
	 * @param startVOI
	 *            apex starting slice VOI number
	 * @param boundingBox
	 *            Cropped image bounding box
	 * @param axis
	 *            axis orientation
	 */
	public void doRegistrationEnd(ModelImage image,
			ModelImage coherenceEnhancingDiffusionImage,
			Vector<Vector<ModelImage>> imageStackFuzzyC, int endVOI,
			int[] boundingBox) {
		ModelImage refImage = null;
		ModelImage testImage = null;
		VOIVector VOIs;
		VOIVector VOIsSrc;
		ModelImage m_kImageDef = null;
		int currentSlice;

		// Registration OAR 2D
		int costChoice = AlgorithmCostFunctions2D.CORRELATION_RATIO_SMOOTHED;
		int DOF = 7;
		int interp = AlgorithmTransform.BSPLINE3;
		float rotateBegin = -10.0f;
		float rotateEnd = 10.0f;
		float coarseRate = 2.0f;
		float fineRate = 6.0f;
		boolean doSubsample = true;
		boolean doMultiThread = true;
		int bracketBound = 10;
		int baseNumIter = 2;
		int numMinima = 3;
		boolean doJTEM = false;

		// ****************** Bspline parameters start
		// **************************************
		AlgorithmRegBSpline.Options m_kOptionsPass1 = new AlgorithmRegBSpline.Options();
		AlgorithmRegBSpline.Options m_kOptionsPass2 = null;
		Controls m_kControlsPass1;
		RegistrationMeasure m_kRegMeasure = null;

		m_kControlsPass1 = new Controls(this);
		// Set defaults for Pass 1.
		m_kOptionsPass1.iBSplineDegree = 2;
		m_kOptionsPass1.iBSplineNumControlPoints = 8;
		m_kOptionsPass1.fGradientDescentMinimizeStepSize = 1.0f;
		m_kOptionsPass1.iGradientDescentMinimizeMaxSteps = 10;
		m_kOptionsPass1.fConvergenceLimit = 0.05f;
		m_kOptionsPass1.iMaxIterations = 50;
		m_kOptionsPass1.bSubsample = true;
		m_kControlsPass1.setValues(m_kOptionsPass1);

		// What is the text that appears in the combo box?
		final String kStrDescription = "Normalized Mutual Information";
		// "Least Squares"
		// "Correlation Ratio",
		// "Normalized Mutual Information"

		// Match that text to the known cost functions.
		if (RegistrationMeasureLeastSquares.getStaticName() == kStrDescription) {
			m_kRegMeasure = new RegistrationMeasureLeastSquares();
		} else if (RegistrationMeasureCorrelationRatio.getStaticName() == kStrDescription) {
			m_kRegMeasure = new RegistrationMeasureCorrelationRatio();
		} else if (RegistrationMeasureNormalizedMutualInformation
				.getStaticName() == kStrDescription) {
			m_kRegMeasure = new RegistrationMeasureNormalizedMutualInformation();
		}
		// ************************* end
		// ***************************************************

		int xDim = coherenceEnhancingDiffusionImage.getExtents()[0];
		int yDim = coherenceEnhancingDiffusionImage.getExtents()[1];
		int zDim = coherenceEnhancingDiffusionImage.getExtents()[2] - 1;

		int[] destExtents = new int[2];
		destExtents[0] = coherenceEnhancingDiffusionImage.getExtents()[0];
		destExtents[1] = coherenceEnhancingDiffusionImage.getExtents()[1];

		int sliceSize = xDim * yDim;
		int midSlice = endVOI; // (int) ((zDim) / 2f);
		System.out.println("loadMask: startSlice = " + midSlice);

		double[] midBuffer = new double[sliceSize];

		// UI.registerImage(coherenceEnhancingDiffusionImage);
		// new ViewJFrameImage(coherenceEnhancingDiffusionImage);

		// if (coherenceEnhancingDiffusionImage.getVOIs() != null) {
		VOIs = coherenceEnhancingDiffusionImage.getVOIs();
		// }

		// if (image.getVOIs() != null) {
		VOIsSrc = image.getVOIs();
		// }

		// find the intersection of the lower bound with the VOI.
		Vector<VOIBase>[] vArray = VOIs.VOIAt(0).getSortedCurves(
				VOIBase.ZPLANE, zDim);
		VOIBase v = vArray[midSlice].get(0);
		// VOI totalVOI = v.getGroup();

		Vector<VOIBase>[] vArraySrc = VOIsSrc.VOIAt(0).getSortedCurves(
				VOIBase.ZPLANE, zDim);
		VOIBase vSrc = vArraySrc[midSlice].get(0);
		VOI totalVOISrc = vSrc.getGroup();

		VOIBase vTemp = (VOIBase) v.clone();

		int nPts = vTemp.size();

		// zero out the z dimension VOI
		float[] xPts = new float[nPts];
		float[] yPts = new float[nPts];
		float[] zPts = new float[nPts];
		float[] zPtsZero = new float[nPts];

		vTemp.exportArrays(xPts, yPts, zPts);
		vTemp.importArrays(xPts, yPts, zPtsZero, nPts);

		VOIVector voiVectorNew = new VOIVector();
		VOI voiNew = new VOI((short) 0, "blank");
		voiNew.importCurve(vTemp);
		voiVectorNew.add(voiNew);

		// get the mid slice
		try {
			refImage = new ModelImage(
					coherenceEnhancingDiffusionImage.getType(), destExtents,
					"ref" + midSlice);
			coherenceEnhancingDiffusionImage.exportData(midSlice * sliceSize,
					sliceSize, midBuffer);
			refImage.importData(0, midBuffer, true);
			refImage.addVOIs(voiVectorNew);
			// new ViewJFrameImage(refImage);

			FileInfoImageXML fileInfoTarget = new FileInfoImageXML(
					coherenceEnhancingDiffusionImage.getImageName(), null,
					FileUtility.XML);
			float[] resTarget = coherenceEnhancingDiffusionImage.getFileInfo(0)
					.getResolutions();
			float[] newResTarget = { resTarget[0], resTarget[1] };
			int[] extsTarget = coherenceEnhancingDiffusionImage.getFileInfo(0)
					.getExtents();
			int[] newExtsTarget = { extsTarget[0], extsTarget[1] };
			fileInfoTarget.setExtents(newExtsTarget);
			fileInfoTarget.setResolutions(newResTarget);
			FileInfoImageXML[] fileInfosTarget = { fileInfoTarget };
			refImage.setFileInfo(fileInfosTarget);

		} catch (IOException e) {
			e.printStackTrace();
		}

		int startSlice = midSlice + 1;
		int endSlice = midSlice + tracingSliceNumber;

		for (int z = startSlice; z < endSlice; z++) {

			try {
				currentSlice = z;
				int start = z * sliceSize;
				double[] sourceBuffer = new double[sliceSize];
				coherenceEnhancingDiffusionImage.exportData(start, sliceSize,
						sourceBuffer);

				testImage = new ModelImage(
						coherenceEnhancingDiffusionImage.getType(),
						destExtents, "test" + z);
				testImage.importData(0, sourceBuffer, true);
				boolean endianness = coherenceEnhancingDiffusionImage
						.getFileInfo(0).getEndianess();
				int modality = coherenceEnhancingDiffusionImage.getFileInfo(0)
						.getModality();
				float[] res = coherenceEnhancingDiffusionImage.getFileInfo(0)
						.getResolutions();
				float[] newRes = { res[0], res[1] };
				int[] exts = coherenceEnhancingDiffusionImage.getFileInfo(0)
						.getExtents();
				int[] newExts = { exts[0], exts[1] };
				int[] units = coherenceEnhancingDiffusionImage.getFileInfo(0)
						.getUnitsOfMeasure();
				int[] newUnits = { units[0], units[1] };
				int imageOrient = coherenceEnhancingDiffusionImage.getFileInfo(
						0).getImageOrientation();
				int[] axisOrient = coherenceEnhancingDiffusionImage
						.getFileInfo(0).getAxisOrientation();
				int[] newAxisOrient = { axisOrient[0], axisOrient[1] };
				float[] origin = coherenceEnhancingDiffusionImage
						.getFileInfo(0).getOrigin();
				float[] newOrigin = { origin[0], origin[1] };

				// testImage.addVOIs(voiVectorNew);
				FileInfoImageXML fileInfo = new FileInfoImageXML(
						coherenceEnhancingDiffusionImage.getImageName(), null,
						FileUtility.XML);
				fileInfo.setDataType(ModelStorageBase.FLOAT);
				fileInfo.setEndianess(endianness);
				fileInfo.setExtents(newExts);
				fileInfo.setModality(modality);
				fileInfo.setResolutions(newRes);
				fileInfo.setUnitsOfMeasure(newUnits);
				fileInfo.setImageOrientation(imageOrient);
				fileInfo.setAxisOrientation(newAxisOrient);
				fileInfo.setOrigin(newOrigin);

				FileInfoImageXML[] fileInfos = { fileInfo };
				testImage.setFileInfo(fileInfos);

				ModelImage resultImage = new ModelImage(testImage.getType(),
						destExtents, "result" + z);
				resultImage.importData(0, sourceBuffer, true);
				resultImage.getVOIs().removeAllElements();
				if (useBSpline) {
					int[] aiExtentsReg = new int[2];
					aiExtentsReg = testImage.getExtents();

					m_kControlsPass1.getValues(m_kOptionsPass1, aiExtentsReg);

					AlgorithmRegBSpline2D m_kAlgorithmReg = new AlgorithmRegBSpline2D(
							resultImage, refImage, testImage, m_kImageDef,
							m_kRegMeasure, m_kOptionsPass1, m_kOptionsPass2);
					m_kAlgorithmReg.addListener(this);
					m_kAlgorithmReg.run();

					if (m_kAlgorithmReg != null
							&& m_kAlgorithmReg.isCompleted()) {
						m_kAlgorithmReg.setCompleted(true);
						m_kAlgorithmReg.disposeLocal();
						m_kAlgorithmReg = null;
					}
				} else {
					AlgorithmRegOAR2D reg2 = new AlgorithmRegOAR2D(testImage,
							refImage, costChoice, DOF, interp, rotateBegin,
							rotateEnd, coarseRate, fineRate, doSubsample,
							doMultiThread, bracketBound, baseNumIter);

					reg2.setJTEM(doJTEM);
					reg2.addListener(this);
					reg2.run();

					int interp2 = AlgorithmTransform.BSPLINE3;
					float fillValue = 0.0f;
					boolean findSolution = false;
					if (reg2 != null && reg2.isCompleted()) {
						final TransMatrix finalMatrix = reg2.getTransform();

						if (finalMatrix != null) {
							findSolution = true;
							ModelImage tempImage = testImage;

							final int xdimA = tempImage.getExtents()[0];
							final int ydimA = tempImage.getExtents()[1];

							final float xresA = tempImage.getFileInfo(0)
									.getResolutions()[0];
							final float yresA = tempImage.getFileInfo(0)
									.getResolutions()[1];

							// final String name =
							// JDialogBase.makeImageName(refImageSlice.getImageName(),
							// "_register1");

							AlgorithmTransform transform = new AlgorithmTransform(
									refImage, finalMatrix, interp2, xresA,
									yresA, xdimA, ydimA, true, false, false);

							transform.setUpdateOriginFlag(true);
							transform.setFillValue(fillValue);
							transform.run();
							resultImage = transform.getTransformedImage();
							transform.finalize();

							resultImage.calcMinMax();
							// resultImage.setImageName(name);
							resultImage.setType(refImage.getType());
							if (transform != null) {
								transform.disposeLocal();
								transform = null;
							}
						}
					}
				}

				// segmentFromFuzzyC(imageStackFuzzyC, currentSlice,
				// resultImage, refImage, testImage);
				transformVOI(image, resultImage, refImage, testImage,
						boundingBox, currentSlice);
				// new ViewJFrameImage(resultImage);

				// Switch the images, copy current slice image to refImage
				// testImage will be the next slice.
				refImage.importData(0, sourceBuffer, true);
				refImage.getVOIs().removeAllElements();
				VOIVector resultingVOIs = resultImage.getVOIs();
				refImage.addVOIs(resultingVOIs);
				refImage.setImageName("ref" + (z - 1));

				testImage.disposeLocal();
				testImage = null;
				resultImage.disposeLocal();
				resultImage = null;
				sourceBuffer = null;
				// z = endSlice; // debug ending condition.
			} catch (IOException e) {
				e.printStackTrace();
			}

		} // end z loop

		refImage.disposeLocal();
		refImage = null;
		midBuffer = null;

	}

	
	/**
	 * When user draws the 3 VOIs out of order, adjust the VOIs in ascending order
	 * @param image  source image, axial, sagittal or coronal 
	 */
    public void adjustVOIs(ModelImage image) {
		
		int[] extents = (int[]) image.getFileInfo(0).getExtents();
		int zDim = extents[2] - 1;
		int i;
		int indexFirst = 0, indexSecond = 0, indexThird = 0;
		int minIndex, maxIndex, midIndex = 0;
		
		VOIVector VOIs;
		VOIs = image.getVOIs();
		
		// get the three index
		Vector<VOIBase>[] vArrayFirst = VOIs.VOIAt(0).getSortedCurves(VOIBase.ZPLANE, zDim);
		for (i = 0; i < vArrayFirst.length; i++ ) {
			if ( vArrayFirst[i].size() > 0 ) {
				indexFirst = i;
				break;
			}
		}
		
		Vector<VOIBase>[] vArraySecond = VOIs.VOIAt(1).getSortedCurves(VOIBase.ZPLANE, zDim);
		for (i = 0; i < vArraySecond.length; i++ ) {
			if ( vArraySecond[i].size() > 0 ) {
				indexSecond = i;
				break;
			}
		}
		
		Vector<VOIBase>[] vArrayThird = VOIs.VOIAt(2).getSortedCurves(VOIBase.ZPLANE, zDim);
		for (i = 0; i < vArrayThird.length; i++ ) {
			if ( vArrayThird[i].size() > 0 ) {
				indexThird = i;
				break;
			}
		}
		
		// sort index
	    minIndex = Math.min(indexFirst, Math.min(indexSecond, indexThird));
	    maxIndex = Math.max(indexFirst, Math.max(indexSecond, indexThird));
		
	    if ( minIndex == indexFirst && maxIndex == indexSecond ) {
	             midIndex = indexThird;
	    } else if ( minIndex == indexFirst && maxIndex == indexThird ) {
	    	     midIndex = indexSecond;
	    } else if ( minIndex == indexSecond && maxIndex == indexThird ) {
	    	     midIndex = indexFirst;
	    }
	    
	    image.getVOIs().removeAllElements();
	    
	    VOIVector voiFirstNew = new VOIVector();
	    VOI voiFirst = new VOI((short) 0, "blank");
	    if ( minIndex == indexFirst) {
	    	voiFirst.importCurve(vArrayFirst[indexFirst].get(0));	
	    } else if ( minIndex == indexSecond ) {
	    	voiFirst.importCurve(vArraySecond[indexSecond].get(0));
	    } else if ( minIndex == indexThird ) {
	    	voiFirst.importCurve(vArrayThird[indexThird].get(0));
	    }
	    voiFirstNew.add(voiFirst);
	    image.addVOIs(voiFirstNew);
	    
	    
	    VOIVector voiSecondNew = new VOIVector();
	    VOI voiSecond = new VOI((short) 0, "blank");
	    if ( midIndex == indexFirst) {
	    	voiSecond.importCurve(vArrayFirst[indexFirst].get(0));	
	    } else if ( midIndex == indexSecond ) {
	    	voiSecond.importCurve(vArraySecond[indexSecond].get(0));
	    } else if ( midIndex == indexThird ) {
	    	voiSecond.importCurve(vArrayThird[indexThird].get(0));
	    }
	    voiSecondNew.add(voiSecond);
	    image.addVOIs(voiSecondNew);
	    
	    
	    VOIVector voiThirdNew = new VOIVector();
	    VOI voiThird = new VOI((short) 0, "blank");
	    if ( maxIndex == indexFirst) {
	    	voiThird.importCurve(vArrayFirst[indexFirst].get(0));	
	    } else if ( maxIndex == indexSecond ) {
	    	voiThird.importCurve(vArraySecond[indexSecond].get(0));
	    } else if ( maxIndex == indexThird ) {
	    	voiThird.importCurve(vArrayThird[indexThird].get(0));
	    }
	    voiThirdNew.add(voiThird);
	    image.addVOIs(voiThirdNew);
	    	
	}

	
	/**
	 * Crop MR image with specified X, Y, Z bounds Use the mid slice as the
	 * initial guidance to find the bounds.
	 * 
	 * @param image
	 *            Original MR image
	 * @param xBounds
	 *            X bounds
	 * @param yBounds
	 *            Y bounds
	 * @param zBounds
	 *            Z bounds
	 * @param boundingBox
	 *            Cropped region rounding box
	 * @param midVOI
	 *            mid slice VOI
	 * @return
	 */
	public ModelImage cropROI(ModelImage image, int[] xBounds, int[] yBounds,
			int[] zBounds, int[] boundingBox, int midVOI) {

		VOIVector VOIs;
		int minX = 999, maxX = -999;
		int minY = 999, maxY = -999;

		int top, bottom, left, right;
		int zDim;
		// int middleSlice;
		int x, y;
		float r, theta;
		int i;

		ModelImage cropImage;

		int[] extents = (int[]) image.getFileInfo(0).getExtents();

		zDim = extents[2] - 1;

		// middleSlice = (int) (zDim / 2);

		// System.out.println("cropROI: middleSlice = " + middleSlice);

		// if (image.getVOIs() != null) {
		VOIs = image.getVOIs();
		// }

		// find the intersection of the lower bound with the VOI.
		Vector<VOIBase>[] vArray = VOIs.VOIAt(1).getSortedCurves(
				VOIBase.ZPLANE, zDim);
		VOIBase v = vArray[midVOI].get(0);

		int nPts = v.size();

		for (i = 0; i < nPts; i++) {
			x = (int) ((Vector3f) (v.elementAt(i))).X;
			y = (int) ((Vector3f) (v.elementAt(i))).Y;

			if (x < minX) {
				minX = x;
			}

			if (y < minY) {
				minY = y;
			}

			if (x > maxX) {
				maxX = x;
			}

			if (y > maxY) {
				maxY = y;
			}
		}

		int boxYmin = minY - ((int) ((maxY - minY) / 4f));
		int boxYmax = maxY + ((int) ((maxY - minY) / 4f));

		int boxXmin = minX - ((int) ((maxX - minX) / 4f));
		int boxXmax = maxX + ((int) ((maxX - minX) / 4f));

		xBounds[0] = boxXmin;
		xBounds[1] = boxXmax;

		yBounds[0] = boxYmin;
		yBounds[1] = boxYmax;

		boundingBox[0] = boxXmin;
		boundingBox[1] = boxXmax;
		boundingBox[2] = boxYmin;
		boundingBox[3] = boxYmax;

		zBounds[0] = 0;
		zBounds[1] = zDim;
		System.out.println("zBound[1] = " + zBounds[1]);

		int borderSize = 0;

		try {
			int[] destExtents = null;

			if (image.getNDims() == 2) {
				destExtents = new int[2];
				destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1
						+ (2 * borderSize);
				destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1
						+ (2 * borderSize);
			} else if (image.getNDims() == 3) {

				if (Math.abs(zBounds[1] - zBounds[0]) == 0) { // crop 3D image
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
			} else if (image.getNDims() == 4) {

				if (Math.abs(zBounds[1] - zBounds[0]) == 0) { // crop 4D image
					// to 3D image
					destExtents = new int[3];
					destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1
							+ (2 * borderSize);
					destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1
							+ (2 * borderSize);
					destExtents[2] = image.getExtents()[3];
				} else {
					destExtents = new int[4];
					destExtents[0] = Math.abs(xBounds[1] - xBounds[0]) + 1
							+ (2 * borderSize);
					destExtents[1] = Math.abs(yBounds[1] - yBounds[0]) + 1
							+ (2 * borderSize);
					destExtents[2] = Math.abs(zBounds[1] - zBounds[0] + 1);
					destExtents[3] = image.getExtents()[3];
				}
			} else {
				return null;
			}

			// Make result image
			cropImage = new ModelImage(image.getType(), destExtents,
					makeImageName(image.getImageName(), "_crop"));

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
			AlgorithmAddMargins cropAlgo = new AlgorithmAddMargins(image,
					cropImage, xCrop, yCrop, zCrop);

			cropAlgo.addListener(this);

			// Hide the dialog since the algorithm is about to run.
			setVisible(false);
			cropAlgo.run();
			cropAlgo.finalize();
			cropAlgo = null;

		} catch (OutOfMemoryError e) {
			MipavUtil
					.displayError("Dialog Crop: unable to allocate enough memory");

			return null;
		}
		return cropImage;

	}

	/**
	 * Create the dialog interface.
	 */
	public void init() {

		JPanel mainPanel;

		mainPanel = new JPanel();
		mainPanel.setBorder(BorderFactory.createEmptyBorder(3, 3, 3, 3));
		mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

		final GridBagConstraints gbc = new GridBagConstraints();

		gbc.gridx = 0;
		gbc.gridy = 0;
		gbc.weightx = 0;
		gbc.fill = GridBagConstraints.NONE;
		gbc.anchor = GridBagConstraints.EAST;

		// ************************* Axial Image ******************************
		JPanel imageSelectionPanelAxial = new JPanel();
		imageSelectionPanelAxial.setLayout(new GridLayout(4, 2));
		imageSelectionPanelAxial.setBorder(buildTitledBorder("Axial Image"));

		gbc.gridx = 0;
		gbc.gridy = 0;
		labelAxis = new JLabel("Axis: Axial");
		labelAxis.setFont(serif12);
		labelAxis.setForeground(Color.black);

		JLabel emptyLabelAxial = new JLabel("");
		imageSelectionPanelAxial.add(labelAxis, gbc);
		gbc.gridx = 1;
		imageSelectionPanelAxial.add(emptyLabelAxial, gbc);

		// Enter the start, end and mid slice numbers

		// start VOI
		gbc.gridx = 0;
		gbc.gridy = 1;
		labelStartVOIAxial = new JLabel("Start VOI: ");
		labelStartVOIAxial.setFont(serif12);
		labelStartVOIAxial.setForeground(Color.black);

		imageSelectionPanelAxial.add(labelStartVOIAxial, gbc);

		textFieldStartVOIAxial = new JTextField(10);
		textFieldStartVOIAxial.setFont(serif12);
		textFieldStartVOIAxial.setText(String.valueOf(startVOIAxial));

		gbc.gridx = 1;
		imageSelectionPanelAxial.add(textFieldStartVOIAxial, gbc);

		// Mid VOI
		gbc.gridx = 0;
		gbc.gridy = 2;
		labelMidVOIAxial = new JLabel("Center VOI: ");
		labelMidVOIAxial.setFont(serif12);
		labelMidVOIAxial.setForeground(Color.black);

		imageSelectionPanelAxial.add(labelMidVOIAxial, gbc);

		textFieldMidVOIAxial = new JTextField(10);
		textFieldMidVOIAxial.setFont(serif12);
		textFieldMidVOIAxial.setText(String.valueOf(midVOIAxial));
		gbc.gridx = 1;
		imageSelectionPanelAxial.add(textFieldMidVOIAxial, gbc);

		// end VOI
		gbc.gridx = 0;
		gbc.gridy = 3;
		labelEndVOIAxial = new JLabel("End VOI: ");
		labelEndVOIAxial.setFont(serif12);
		labelEndVOIAxial.setForeground(Color.black);

		imageSelectionPanelAxial.add(labelEndVOIAxial, gbc);

		textFieldEndVOIAxial = new JTextField(10);
		textFieldEndVOIAxial.setFont(serif12);
		textFieldEndVOIAxial.setText(String.valueOf(endVOIAxial));

		gbc.gridx = 1;
		imageSelectionPanelAxial.add(textFieldEndVOIAxial, gbc);

		// mainPanel.add(svmOptionsPanel, BorderLayout.CENTER);
		mainPanel.add(imageSelectionPanelAxial);

		// ************************* Sagittal Image ***************************
		JPanel imageSelectionPanelSagittal = new JPanel();
		imageSelectionPanelSagittal.setLayout(new GridLayout(4, 2));
		imageSelectionPanelSagittal
				.setBorder(buildTitledBorder("Sagittal Image"));

		gbc.gridx = 0;
		gbc.gridy = 0;
		labelSagittal = new JLabel("Axis: Sagittal");
		labelSagittal.setFont(serif12);
		labelSagittal.setForeground(Color.black);

		JLabel emptyLabelSagittal = new JLabel("");
		imageSelectionPanelSagittal.add(labelSagittal, gbc);
		gbc.gridx = 1;
		imageSelectionPanelSagittal.add(emptyLabelSagittal, gbc);

		// Enter the start, end and mid slice numbers

		// start VOI
		gbc.gridx = 0;
		gbc.gridy = 1;
		labelStartVOISagittal = new JLabel("Start VOI: ");
		labelStartVOISagittal.setFont(serif12);
		labelStartVOISagittal.setForeground(Color.black);

		imageSelectionPanelSagittal.add(labelStartVOISagittal, gbc);

		textFieldStartVOISagittal = new JTextField(10);
		textFieldStartVOISagittal.setFont(serif12);
		textFieldStartVOISagittal.setText(String.valueOf(startVOISagittal));

		gbc.gridx = 1;
		imageSelectionPanelSagittal.add(textFieldStartVOISagittal, gbc);

		// Mid VOI
		gbc.gridx = 0;
		gbc.gridy = 2;
		labelMidVOISagittal = new JLabel("Center VOI: ");
		labelMidVOISagittal.setFont(serif12);
		labelMidVOISagittal.setForeground(Color.black);

		imageSelectionPanelSagittal.add(labelMidVOISagittal, gbc);

		textFieldMidVOISagittal = new JTextField(10);
		textFieldMidVOISagittal.setFont(serif12);
		textFieldMidVOISagittal.setText(String.valueOf(midVOISagittal));
		gbc.gridx = 1;
		imageSelectionPanelSagittal.add(textFieldMidVOISagittal, gbc);

		// end VOI
		gbc.gridx = 0;
		gbc.gridy = 3;
		labelEndVOISagittal = new JLabel("End VOI: ");
		labelEndVOISagittal.setFont(serif12);
		labelEndVOISagittal.setForeground(Color.black);

		imageSelectionPanelSagittal.add(labelEndVOISagittal, gbc);

		textFieldEndVOISagittal = new JTextField(10);
		textFieldEndVOISagittal.setFont(serif12);
		textFieldEndVOISagittal.setText(String.valueOf(endVOISagittal));

		gbc.gridx = 1;
		imageSelectionPanelSagittal.add(textFieldEndVOISagittal, gbc);

		mainPanel.add(imageSelectionPanelSagittal);

		// ************************* Coronal Image ***************************
		JPanel imageSelectionPanelCoronal = new JPanel();
		imageSelectionPanelCoronal.setLayout(new GridLayout(4, 2));
		imageSelectionPanelCoronal
				.setBorder(buildTitledBorder("Coronal Image"));

		gbc.gridx = 0;
		gbc.gridy = 0;
		labelCoronal = new JLabel("Axis: Coronal");
		labelCoronal.setFont(serif12);
		labelCoronal.setForeground(Color.black);

		JLabel emptyLabelCoronal = new JLabel("");
		imageSelectionPanelCoronal.add(labelCoronal, gbc);
		gbc.gridx = 1;
		imageSelectionPanelCoronal.add(emptyLabelCoronal, gbc);

		// Enter the start, end and mid slice numbers

		// start VOI
		gbc.gridx = 0;
		gbc.gridy = 1;
		labelStartVOICoronal = new JLabel("Start VOI: ");
		labelStartVOICoronal.setFont(serif12);
		labelStartVOICoronal.setForeground(Color.black);

		imageSelectionPanelCoronal.add(labelStartVOICoronal, gbc);

		textFieldStartVOICoronal = new JTextField(10);
		textFieldStartVOICoronal.setFont(serif12);
		textFieldStartVOICoronal.setText(String.valueOf(startVOICoronal));

		gbc.gridx = 1;
		imageSelectionPanelCoronal.add(textFieldStartVOICoronal, gbc);

		// Mid VOI
		gbc.gridx = 0;
		gbc.gridy = 2;
		labelMidVOICoronal = new JLabel("Center VOI: ");
		labelMidVOICoronal.setFont(serif12);
		labelMidVOICoronal.setForeground(Color.black);

		imageSelectionPanelCoronal.add(labelMidVOICoronal, gbc);

		textFieldMidVOICoronal = new JTextField(10);
		textFieldMidVOICoronal.setFont(serif12);
		textFieldMidVOICoronal.setText(String.valueOf(midVOICoronal));
		gbc.gridx = 1;
		imageSelectionPanelCoronal.add(textFieldMidVOICoronal, gbc);

		// end VOI
		gbc.gridx = 0;
		gbc.gridy = 3;
		labelEndVOICoronal = new JLabel("End VOI: ");
		labelEndVOICoronal.setFont(serif12);
		labelEndVOICoronal.setForeground(Color.black);

		imageSelectionPanelCoronal.add(labelEndVOICoronal, gbc);

		textFieldEndVOICoronal = new JTextField(10);
		textFieldEndVOICoronal.setFont(serif12);
		textFieldEndVOICoronal.setText(String.valueOf(endVOICoronal));

		gbc.gridx = 1;
		imageSelectionPanelCoronal.add(textFieldEndVOICoronal, gbc);

		mainPanel.add(imageSelectionPanelCoronal);

		// *************** Registration Panel *****************************
		JPanel registrationPanel = new JPanel();
		registrationPanel.setLayout(new GridLayout(1, 2));
		registrationPanel.setBorder(buildTitledBorder("Registration"));

		final ButtonGroup group1 = new ButtonGroup();
		radioBSpline = new JRadioButton("B-Spline", true);
		radioBSpline.setFont(MipavUtil.font12);
		group1.add(radioBSpline);
		radioBSpline.addActionListener(this);
		registrationPanel.add(radioBSpline);

		radioOAR = new JRadioButton("OAR", false);
		radioOAR.setFont(MipavUtil.font12);
		group1.add(radioOAR);
		radioOAR.addActionListener(this);
		registrationPanel.add(radioOAR);

		mainPanel.add(registrationPanel);

		// *********************** Ok Buttons ***********************
		mainPanel.add(buildButtons());

		getContentPane().add(mainPanel);
		pack();
		setResizable(true);
	}

	class Edge {

		float weight;
		float rNearest;
		float rAvg;
		float orientation;
		int orientationRank;
		boolean visited = false;
		boolean isUshape = false;

		Edge prev;
		Edge next;

		Hashtable table = new Hashtable();

		public int size() {
			return table.size();
		}

		public void insert(int x, int y, float r, float theta) {
			String key = x + ":" + y;
			String value = r + ":" + theta;
			table.put(key, value);
		}

		public String find(int x, int y) {
			String value;
			String key = x + ":" + y;
			value = (String) table.get((String) key);
			return value;
		}

		public Enumeration getKeys() {
			return table.keys();
		}

		public void clear() {
			table.clear();
		}

		public void remove(String key) {
			table.remove(key);
		}
	}

	class PolarPoint implements Comparator {
		float r; // Polar coordinate R
		float theta; // Polar coordinate theta
		int x; // Relative pixel coordinate x
		int y; // Relative pixel coordinate y

		public PolarPoint() {

		}

		public PolarPoint(int _x, int _y, float _r, float _theta) {
			x = _x;
			y = _y;
			r = _r;
			theta = _theta;
		}

		public void assign(PolarPoint p) {
			this.x = p.x;
			this.y = p.y;
			this.r = p.r;
			this.theta = p.theta;
		}

		public int compare(final Object v1, final Object v2) {
			PolarPoint a, b;

			a = (PolarPoint) v1;
			b = (PolarPoint) v2;

			float va, vb;

			va = a.theta;
			vb = b.theta;

			int first = va > vb ? 1 : 0;
			int second = va < vb ? 1 : 0;

			return first - second;

		}

	}

}
