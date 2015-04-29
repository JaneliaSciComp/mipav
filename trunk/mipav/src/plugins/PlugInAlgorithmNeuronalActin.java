import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmThresholdDual;
import gov.nih.mipav.model.algorithms.filters.AlgorithmMean;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;

import java.awt.Color;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.HashSet;
import java.util.Iterator;
import java.util.PriorityQueue;

import javax.swing.JTextPane;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.SimpleAttributeSet;
import javax.swing.text.StyleConstants;

import WildMagic.LibFoundation.Mathematics.Vector3f;


public class PlugInAlgorithmNeuronalActin extends AlgorithmBase {

	private String filFile;

	private String imgFile;

	private ArrayList<Vector3f> swcLines;

	private int width;

	private int length;

	private int height;

	private int depth;

	private JTextPane textArea;

	private final SimpleAttributeSet blackText;

	private final SimpleAttributeSet redText;

	private int[] extents;

	private float sigma = 5f;

	private float sensitivity = 0.01f;

	private float radialThreshold = 0.80f;

	private int actinChannel = 1;

	public PlugInAlgorithmNeuronalActin(String imageFile, String swcFile, JTextPane txtArea) {
		super();
		imgFile = imageFile;
		filFile = swcFile;
		swcLines = new ArrayList<Vector3f>();
		textArea = txtArea;

		blackText = new SimpleAttributeSet();
		StyleConstants.setFontFamily(blackText, "Serif");
		StyleConstants.setFontSize(blackText, 12);

		redText = new SimpleAttributeSet(blackText);
		StyleConstants.setForeground(redText, Color.red.darker());
	}

	@Override
	public void runAlgorithm() {

		FileIO reader = new FileIO();
		try {
			append("Opening image...", blackText);
			srcImage = reader.readImage(imgFile);
		} catch (Exception e) {
			e.printStackTrace();
			append("Could not open image file", redText);
			return;
		}

		extents = srcImage.getExtents();

		if (extents.length != 3) {
			append("This image is not a 3D image", redText);
			return;
		}

		width = extents[0];
		height = extents[1];
		depth = extents[2];
		length = width * height * depth;

		if (srcImage.isColorImage()) {

			short[] buffer = new short[length];
			try {
				srcImage.exportRGBData(actinChannel, 0, length, buffer);
				srcImage = new ModelImage(ModelImage.SHORT, extents, srcImage.getImageName() + "_actin");
				srcImage.importData(0, buffer, true);
			} catch (IOException e) {
				e.printStackTrace();
				append("Could not extract actin channel from color image", redText);
				return;
			}
		}

		ViewJFrameImage srcFrame = new ViewJFrameImage(srcImage);
		srcFrame.setVisible(true);

		try {
			append("Reading swc...", blackText);
			readSWC();
		} catch (IOException e) {
			e.printStackTrace();
			append("Could not read the CSV file", redText);
			return;
		}

		if (swcLines.size() < 3) {
			append("Not enough points to calculate the spline", redText);
			return;
		}

		try {
			float[] res = srcImage.getResolutions(0);

			append("Calculating axon spline...", blackText);
			PlugInAlgorithm3DSpline spline = new PlugInAlgorithm3DSpline(swcLines, res[0], res[1]);
			spline.run();

			ArrayList<Vector3f> xBases = spline.getXBases();
			ArrayList<Vector3f> yBases = spline.getYBases();
			ArrayList<Vector3f> zBases = spline.getZBases();

			append("Calculating radii...", blackText);

			ModelImage cloneImage = (ModelImage) srcImage.clone();
			filterShotNoiseMean(cloneImage);
			AlgorithmChangeType changeZ =
					new AlgorithmChangeType(cloneImage, ModelImage.UBYTE, cloneImage.getMin(), cloneImage.getMax(), 0,
							255, false);
			changeZ.run();

			ModelImage probImage = probabilityMap(cloneImage);

			float[] threshold = {0, (float) (-sensitivity * Math.log(sensitivity))};

			AlgorithmThresholdDual nThresh = new AlgorithmThresholdDual(probImage, threshold, 1, 1, true, false);
			nThresh.run();

			ViewJFrameImage probFrame = new ViewJFrameImage(probImage);
			probFrame.setVisible(true);

			ArrayList<float[]> results = new ArrayList<float[]>();

			float distance = 0;
			Vector3f lastPt = swcLines.get(0);

			BitSet totalMask = new BitSet(length);

			for (int i = 0; i < swcLines.size(); i++) {
				Vector3f center = swcLines.get(i);

				ArrayList<Vector3f> rotated = spline.rotatePlane(xBases.get(i), yBases.get(i), zBases.get(i));

				BitSet radiusMask = calcRadius(probImage, center, rotated);
				totalMask.or(radiusMask);

				int sum = 0;

				for (int j = radiusMask.nextSetBit(0); j != -1; j = radiusMask.nextSetBit(j + 1)) {
					sum += srcImage.get(j).intValue();
				}

				float dx = center.X - lastPt.X;
				float dy = center.Y - lastPt.Y;
				float dz = center.Z - lastPt.Z;

				distance += (float) Math.sqrt(dx * dx + dy * dy + dz * dz);

				lastPt = center;

				results.add(new float[] {center.X, center.Y, center.Z, distance, (float) sum});
			}

			// ViewJFrameImage srcFrame = new ViewJFrameImage(cloneImage);
			// srcFrame.setVisible(true);
			// srcFrame.setPaintMask(totalMask);

			append("Writing results...", blackText);
			writeResults(results);

			append("Complete!", blackText);
		} catch (IOException e) {
			e.printStackTrace();
			append("Could not write out results", redText);
			return;
		} catch (Exception e) {
			e.printStackTrace();
			append(e.toString(), redText);
			for (StackTraceElement t : e.getStackTrace())
				append(t.toString(), redText);
			return;
		}

	}

	public void setActinChannel(int channel) {
		actinChannel = channel;
	}

	private void append(String message, AttributeSet a) {
		Document doc = textArea.getDocument();
		try {
			doc.insertString(doc.getLength(), message + "\n", a);
		} catch (BadLocationException e) {
			e.printStackTrace();
		}

		textArea.setCaretPosition(doc.getLength());
	}

	private BitSet calcRadius(ModelImage image, Vector3f center, ArrayList<Vector3f> plane) {

		float[] origin = srcImage.getOrigin();
		float[] res = srcImage.getResolutions(0);
		Vector3f orgVec = new Vector3f(origin[0], origin[1], origin[2]);
		PriorityQueue<RadialElement> pq = new PriorityQueue<RadialElement>();

		// Translate points from plane to nearest integer coordinates
		// and place in priority queue so that you work from the
		// center outwards
		for (int i = 0; i < plane.size(); i++) {
			Vector3f v = plane.get(i);
			Vector3f vec = new Vector3f(v);
			vec.sub(orgVec);
			vec.add(center);
			vec.X /= res[0];
			vec.Y /= res[1];
			vec.Z /= res[2];
			int x = Math.round(vec.X);
			int y = Math.round(vec.Y);
			int z = Math.round(vec.Z);
			int ind = x + y * width + z * width * height;
			RadialElement re = new RadialElement(v, ind);

			if (x > 0 && x < width && y > 0 && y < height && z > 0 && z < depth)
				pq.add(re);
		}

		HashSet<Integer> hash = new HashSet<Integer>();
		ArrayDeque<RadialElement> queue = new ArrayDeque<RadialElement>();

		// Only add the closest point if multiple points have the
		// same integer coordinate
		while (!pq.isEmpty()) {
			RadialElement re = pq.poll();
			Integer index = new Integer(re.ind);
			if (!hash.contains(index)) {
				hash.add(index);
				queue.add(re);
			}
		}

		BitSet mask = new BitSet(length);

		float cnt = 0;
		float area = 0;
		float cutoff = res[0];

		ArrayList<Integer> intList = new ArrayList<Integer>();

		/*
		 * Progress outwards from the center point and see if the point is considered foreground or background (neuron
		 * or not). Reaching a point not in the neuron will not automatically constrain the radius. A moving threshold
		 * is used to determine whether the radius has been reached to avoid spurious noise from prematurely ending the
		 * measurement.
		 * 
		 * The basic idea is to grow a circle around the point until it encompasses the cross section at that point.
		 */
		while (!queue.isEmpty()) {
			RadialElement re = queue.poll();
			boolean include = image.getBoolean(re.getIndex());
			intList.add(re.getIndex());
			if (include) {
				cnt++;
			}
			area++;
			RadialElement next = queue.peek();
			// Automatically allow points within the smallest
			// resolution to be considered foreground
			if (re.getIndex() < res[2]) {
				mask.set(re.getIndex());
			}
			if (next != null) {
				if (next.getRadius() > cutoff) {

					cutoff += res[0];
					float fraction = cnt / area;
					// Use a sigmoidal function as the moving threshold
					float threshold = radialThreshold / (1 + (float) Math.exp(-area / sigma));

					if (fraction < threshold)
						break;

					for (int i : intList) {
						mask.set(i);
					}
					intList.clear();
				}
			} else {
				// Exhasted all points, should probably be using a larger plane
				System.err.println("Ran out of points! Need to search further next time.");
				break;
			}
		}

		return mask;

	}

	private ModelImage probabilityMap(ModelImage image) {

		int value;
		ModelImage outImage;

		int bins = 4; // Histogram bins are for 4 pixels
		int num = 256 / 4;
		float[] p = new float[num];
		int[] buffer = new int[length];
		int[] histo = new int[num];

		float[] pImage = new float[length];

		try {
			image.exportData(0, length, buffer);
		} catch (IOException e) {
			MipavUtil.displayError("Image locked");
			e.printStackTrace();
		}

		// accumulating pixel information
		for (int i = 0; i < length; i++) {
			value = buffer[i] / bins;
			histo[value]++;
		}

		for (int i = 0; i < num; i++) {
			p[i] = (float) histo[i] / (float) length;
		}

		float pVal;
		for (int i = 0; i < length; i++) {
			pVal = (p[buffer[i] / bins]);
			pImage[i] = (float) (-pVal * Math.log(pVal));
		}

		outImage = new ModelImage(ModelImage.FLOAT, extents, image.getImageName() + "_prob");
		try {
			outImage.importData(0, pImage, true);
		} catch (IOException e) {
			MipavUtil.displayError("Image locked");
			e.printStackTrace();
		}

		return outImage;

	}

	private void filterShotNoiseMean(ModelImage image) {
		int dataType = image.getType();
		int maxDiff;

		if (dataType == ModelImage.BYTE || dataType == ModelImage.UBYTE)
			maxDiff = 3;
		else if (dataType == ModelImage.SHORT || dataType == ModelImage.USHORT)
			maxDiff = 600;
		else
			return;

		ModelImage meanImage = (ModelImage) image.clone();
		AlgorithmMean mean = new AlgorithmMean(meanImage, 3, false, true);
		mean.run();
		int[] buffer = new int[length];
		int[] medBuffer = new int[length];
		int[] outBuffer = new int[length];
		int diff;


		try {
			image.exportData(0, length, buffer);
			meanImage.exportData(0, length, medBuffer);
		} catch (IOException e) {
			MipavUtil.displayError("Could not export data from original image");
			e.printStackTrace();
		}

		HashSet<Integer> adjustPtsHash = new HashSet<Integer>();
		HashSet<Integer> addPtsHash = new HashSet<Integer>();

		for (int i = 0; i < length; i++) {
			diff = Math.abs(buffer[i] - medBuffer[i]);
			if (diff >= maxDiff) {
				buffer[i] = medBuffer[i];
				int x = i % width;
				int y = (i % (width * height)) / width;
				int z = i / (width * height);
				for (int nx = x - 1; nx <= x + 1; nx++) {
					if (nx < 0 || nx >= width)
						continue;
					for (int ny = y - 1; ny <= y + 1; ny++) {
						if (ny < 0 || ny >= height)
							continue;
						for (int nz = z - 1; nz <= z + 1; nz++) {
							if (nz < 0 || nz >= depth)
								continue;
							int ind = nx + ny * width + nz * width * height;
							adjustPtsHash.add(ind);
						}
					}
				}
			}
		}

		medBuffer = null;

		System.arraycopy(buffer, 0, outBuffer, 0, length);

		while (adjustPtsHash.size() > 0) {
			Iterator<Integer> iter = adjustPtsHash.iterator();
			while (iter.hasNext()) {
				int i = iter.next();
				int x = i % width;
				int y = (i % (width * height)) / width;
				int z = i / (width * height);
				int kMed = findMean(buffer, i);
				if (Math.abs(buffer[i] - kMed) >= maxDiff) {
					outBuffer[i] = kMed;
					for (int nx = x - 1; nx <= x + 1; nx++) {
						if (nx < 0 || nx >= width)
							continue;
						for (int ny = y - 1; ny <= y + 1; ny++) {
							if (ny < 0 || ny >= height)
								continue;
							for (int nz = z - 1; nz <= z + 1; nz++) {
								if (nz < 0 || nz >= depth)
									continue;
								int ind = nx + ny * width + nz * width * height;
								addPtsHash.add(ind);
							}
						}
					}
				}
			}
			iter = adjustPtsHash.iterator();
			while (iter.hasNext()) {
				int i = iter.next();
				buffer[i] = outBuffer[i];
			}
			adjustPtsHash.clear();
			adjustPtsHash.addAll(addPtsHash);
			addPtsHash.clear();
		}

		meanImage.disposeLocal();
		try {
			image.importData(0, outBuffer, true);
		} catch (IOException e) {
			MipavUtil.displayError("Unable to import filtered image");
			e.printStackTrace();
		}

	}

	private int findMean(int[] buffer, int i) {
		int x = i % width;
		int y = (i % (width * height)) / width;
		int z = i / (width * height);
		int kWidth = Math.min(3, 2 + Math.min(x, width - 1 - x));
		int kHeight = Math.min(3, 2 + Math.min(y, height - 1 - y));
		int kDepth = Math.min(3, 2 + Math.min(z, depth - 1 - z));
		int cnt = kWidth * kHeight * kDepth;
		int sum = 0;

		for (int nx = x - 1; nx <= x + 1; nx++) {
			if (nx < 0 || nx >= width)
				continue;
			for (int ny = y - 1; ny <= y + 1; ny++) {
				if (ny < 0 || ny >= height)
					continue;
				for (int nz = z - 1; nz <= z + 1; nz++) {
					if (nz < 0 || nz >= depth)
						continue;
					sum += buffer[nx + ny * width + nz * width * height];
				}
			}
		}
		int kMean = (int) ((float) sum / (float) cnt);
		return kMean;
	}

	private void readSWC() throws IOException {

		BufferedReader input = new BufferedReader(new FileReader(filFile));
		String line = null;
		while ((line = input.readLine()) != null) {
			if (line.startsWith("#"))
				continue;
			String[] lineArray = line.split(" ");
			if (lineArray.length > 5 && lineArray[1].equals("2")) {// Only care about axon
				float x = Float.valueOf(lineArray[2]);
				float y = Float.valueOf(lineArray[3]);
				float z = Float.valueOf(lineArray[4]);
				swcLines.add(new Vector3f(x, y, z));
			}
		}

		input.close();
	}

	private void writeResults(ArrayList<float[]> results) throws IOException {

		File file = new File(filFile);
		String parent = file.getParent();
		String name = file.getName();
		int ind = name.lastIndexOf(".");
		if(ind != -1){
			name = name.substring(0, ind);
		}
		
		name += "_actin.csv";
		
		String fileOut = parent + File.separator + name;
		
		FileWriter fw = new FileWriter(new File(fileOut));
		
		fw.append("Image name," + srcImage.getImageFileName() + "\n");
		fw.append("Filament name," + file.getName() + "\n");
		fw.append("\n");
		fw.append("X,Y,Z,Distance along axon,Radial actin sum\n");
		for (int i = 0; i < results.size(); i++) {
			float[] line = results.get(i);
			for (int j = 0; j < line.length; j++) {
				fw.append(line[j] + ",");
			}
			fw.append("\n");
		}
		
		fw.close();

	}

	private class RadialElement implements Comparable<RadialElement> {

		private float radius;

		private int ind;

		private RadialElement(Vector3f point, int index) {
			radius = point.distance(new Vector3f());
			ind = index;
		}

		public float getRadius() {
			return radius;
		}

		public int getIndex() {
			return ind;
		}

		@Override
		public int compareTo(RadialElement o) {
			float ro = o.radius;
			if (radius > ro)
				return 1;
			else if (radius < ro)
				return -1;
			return 0;
		}

	}

}
