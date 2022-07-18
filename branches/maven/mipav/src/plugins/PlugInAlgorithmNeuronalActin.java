import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmThresholdDual;
import gov.nih.mipav.model.algorithms.filters.AlgorithmMean;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmChangeType;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmSubset;
import gov.nih.mipav.model.structures.ModelImage;

import gov.nih.mipav.view.MipavUtil;

import java.io.*;
import java.util.*;

import javax.swing.JTextPane;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;

import WildMagic.LibFoundation.Mathematics.Vector3f;


public class PlugInAlgorithmNeuronalActin extends AlgorithmBase {

    private final String filFile;

    private final ArrayList<Vector3f> swcLines;

    private int width;

    private int length;

    private int height;

    private int depth;

    private final int timePt;

    private final JTextPane textArea;

    private int[] extents;

    private final float sigma = 5f;

    private float sensitivity = 0.01f;

    private final float radialThreshold = 0.80f;

    private int actinChannel = 1;

    private float[] resolution;

    private float[] origin;

    private ModelImage imgVolume;

    public PlugInAlgorithmNeuronalActin(final ModelImage img, final int t, final String swcFile, final JTextPane txtArea) {
        super();
        srcImage = img;
        timePt = t;
        filFile = swcFile;
        swcLines = new ArrayList<Vector3f>();
        textArea = txtArea;
    }

    /**
     * Prepares this class for destruction.
     */
    @Override
    public void finalize() {
        if (imgVolume != srcImage) {
            imgVolume.disposeLocal();
            imgVolume = null;
        }

        super.finalize();
    }

    @Override
    public void runAlgorithm() {
        extents = srcImage.getExtents();

        if (extents.length == 2) {
            append("This image is a 2D image", PlugInDialogNeuronalActin.RED_TEXT);
        }

        width = extents[0];
        height = extents[1];
        depth = extents[2];
        length = width * height * depth;

        resolution = srcImage.getResolutions(0);
        origin = srcImage.getOrigin();

        final int timeOffset = timePt * length;

        if (srcImage.isColorImage()) {
            final short[] buffer = new short[length];
            try {
                int[] extents3D = new int[] {extents[0], extents[1], extents[2]};
                ModelImage image3D = null;
                if (srcImage.is4DImage()) {
                    append("Extracting 3D volume from time point " + timePt, PlugInDialogNeuronalActin.BLACK_TEXT);
                    image3D = new ModelImage(srcImage.getType(), extents3D, srcImage.getImageName() + "_3dVol");
                    AlgorithmSubset subsetAlgo = new AlgorithmSubset(srcImage, image3D, AlgorithmSubset.REMOVE_T, timePt);
                    subsetAlgo.run();
                    
                    append("Extracting actin channel", PlugInDialogNeuronalActin.BLACK_TEXT);
                    image3D.exportRGBData(actinChannel, 0, length, buffer);
                    imgVolume = new ModelImage(ModelImage.SHORT, extents3D, srcImage.getImageName() + "_actin");
                    imgVolume.importData(0, buffer, true);
                    
                    image3D.disposeLocal();
                    image3D = null;
                } else {
                    append("Extracting actin channel", PlugInDialogNeuronalActin.BLACK_TEXT);
                    srcImage.exportRGBData(actinChannel, 0, length, buffer);
                    imgVolume = new ModelImage(ModelImage.SHORT, extents3D, srcImage.getImageName() + "_actin");
                    imgVolume.importData(0, buffer, true);
                }
            } catch (final IOException e) {
                e.printStackTrace();
                append("Could not extract actin channel from color image", PlugInDialogNeuronalActin.RED_TEXT);
                return;
            }
        } else {
            if (srcImage.is4DImage()) {
                append("Extracting 3D volume from time point " + timePt, PlugInDialogNeuronalActin.BLACK_TEXT);

                final short[] buffer = new short[length];
                try {
                    srcImage.exportData(timeOffset, length, buffer);
                    imgVolume = new ModelImage(ModelImage.SHORT, new int[] {width, height, depth}, srcImage.getImageName() + "_3dVol");
                    imgVolume.importData(0, buffer, true);
                } catch (final IOException e) {
                    e.printStackTrace();
                    append("Could not extract 3D time point from 4D image", PlugInDialogNeuronalActin.RED_TEXT);
                    return;
                }
            }
        }

        // original image is not color or 4D, so we can use it directly
        if (imgVolume == null) {
            imgVolume = srcImage;
        }

        try {
            append("Reading swc...", PlugInDialogNeuronalActin.BLACK_TEXT);
            readSWC();
        } catch (final IOException e) {
            e.printStackTrace();
            append("Could not read the CSV file", PlugInDialogNeuronalActin.RED_TEXT);
            return;
        }

        if (swcLines.size() < 3) {
            append("Not enough points to calculate the spline", PlugInDialogNeuronalActin.RED_TEXT);
            return;
        }

        ModelImage cloneImage = null;
        ModelImage probImage = null;
        try {
            append("Calculating axon spline...", PlugInDialogNeuronalActin.BLACK_TEXT);
            final PlugInAlgorithm3DSpline spline = new PlugInAlgorithm3DSpline(swcLines, resolution[0], resolution[1]);
            spline.run();

            final ArrayList<Vector3f> xBases = spline.getXBases();
            final ArrayList<Vector3f> yBases = spline.getYBases();
            final ArrayList<Vector3f> zBases = spline.getZBases();

            append("Calculating radii...", PlugInDialogNeuronalActin.BLACK_TEXT);

            cloneImage = (ModelImage) imgVolume.clone();
            filterShotNoiseMean(cloneImage);
            final AlgorithmChangeType changeZ = new AlgorithmChangeType(cloneImage, ModelImage.UBYTE, cloneImage.getMin(), cloneImage.getMax(), 0, 255, false);
            changeZ.run();

            probImage = probabilityMap(cloneImage);

            if (sensitivity > 0) {
                final float[] threshold = {0, (float) ( -sensitivity * Math.log(sensitivity))};
    
                final AlgorithmThresholdDual nThresh = new AlgorithmThresholdDual(probImage, threshold, 1, 1, true, false);
                nThresh.run();
            }

            // ViewJFrameImage probFrame = new ViewJFrameImage(probImage);
            // probFrame.setVisible(true);

            final ArrayList<float[]> results = new ArrayList<float[]>();

            float distance = 0;
            Vector3f lastPt = swcLines.get(0);

            final BitSet totalMask = new BitSet(length);

            for (int i = 0; i < swcLines.size(); i++) {
                final Vector3f center = swcLines.get(i);

                final ArrayList<Vector3f> rotated = spline.rotatePlane(xBases.get(i), yBases.get(i), zBases.get(i));

                final BitSet radiusMask = calcRadius(probImage, center, rotated);
                totalMask.or(radiusMask);

                int sum = 0;

                for (int j = radiusMask.nextSetBit(0); j != -1; j = radiusMask.nextSetBit(j + 1)) {
                    sum += imgVolume.get(j).intValue();
                }

                final float dx = center.X - lastPt.X;
                final float dy = center.Y - lastPt.Y;
                final float dz = center.Z - lastPt.Z;

                distance += (float) Math.sqrt(dx * dx + dy * dy + dz * dz);

                lastPt = center;

                results.add(new float[] {center.X, center.Y, center.Z, distance, sum});
            }

            // ViewJFrameImage srcFrame = new ViewJFrameImage(srcImage);
            // srcFrame.getComponentImage().setPaintMask(totalMask);
            // srcFrame.setVisible(true);

            append("Writing results...", PlugInDialogNeuronalActin.BLACK_TEXT);
            writeResults(results);

            append("Complete!", PlugInDialogNeuronalActin.BLACK_TEXT);

            setCompleted(true);
        } catch (final IOException e) {
            e.printStackTrace();
            append("Could not write out results", PlugInDialogNeuronalActin.RED_TEXT);
            return;
        } catch (final Exception e) {
            e.printStackTrace();
            append(e.toString(), PlugInDialogNeuronalActin.RED_TEXT);
            for (final StackTraceElement t : e.getStackTrace()) {
                append(t.toString(), PlugInDialogNeuronalActin.RED_TEXT);
            }
            return;
        } finally {
            if (cloneImage != null) {
                cloneImage.disposeLocal();
                cloneImage = null;
            }
            if (probImage != null) {
                probImage.disposeLocal();
                probImage = null;
            }
        }

    }

    public void setActinChannel(final int channel) {
        actinChannel = channel;
    }

    public void setSensitivity(final float val) {
        sensitivity = val;
    }
    
    private void append(final String message, final AttributeSet a) {
        final Document doc = textArea.getDocument();
        try {
            doc.insertString(doc.getLength(), message + "\n", a);
        } catch (final BadLocationException e) {
            e.printStackTrace();
        }

        textArea.setCaretPosition(doc.getLength());
    }

    /**
     * Calculates the radius at the given coordinate in the given plane which is normal to the filament.
     * 
     * @param image
     * @param coord
     * @param plane
     * @return
     */
    private BitSet calcRadius(final ModelImage image, final Vector3f center, final ArrayList<Vector3f> plane) {

        final Vector3f orgVec = new Vector3f(origin[0], origin[1], origin[2]);
        final PriorityQueue<RadialElement> pq = new PriorityQueue<RadialElement>();

        // Translate points from plane to nearest integer coordinates
        // and place in priority queue so that you work from the
        // center outwards
        for (int i = 0; i < plane.size(); i++) {
            final Vector3f v = plane.get(i);
            final Vector3f vec = new Vector3f(v);
            vec.sub(orgVec);
            vec.add(center);
            vec.X /= resolution[0];
            vec.Y /= resolution[1];
            vec.Z /= resolution[2];
            final int x = Math.round(vec.X);
            final int y = Math.round(vec.Y);
            final int z = Math.round(vec.Z);
            final int ind = x + y * width + z * width * height;
            final RadialElement re = new RadialElement(v, ind);

            if (x > 0 && x < width && y > 0 && y < height && z > 0 && z < depth) {
                pq.add(re);
            }
        }

        final HashSet<Integer> hash = new HashSet<Integer>();
        final ArrayDeque<RadialElement> queue = new ArrayDeque<RadialElement>();

        // Only add the closest point if multiple points have the
        // same integer coordinate
        while ( !pq.isEmpty()) {
            final RadialElement re = pq.poll();
            final Integer index = new Integer(re.ind);
            if ( !hash.contains(index)) {
                hash.add(index);
                queue.add(re);
            }
        }

        final BitSet mask = new BitSet(length);

        float cnt = 0;
        float area = 0;
        float cutoff = resolution[0];

        final ArrayList<Integer> intList = new ArrayList<Integer>();

        /*
         * Progress outwards from the center point and see if the point is considered foreground or background (neuron
         * or not). Reaching a point not in the neuron will not automatically constrain the radius. A moving threshold
         * is used to determine whether the radius has been reached to avoid spurious noise from prematurely ending the
         * measurement.
         * 
         * The basic idea is to grow a circle around the point until it encompasses the cross section at that point.
         */
        while ( !queue.isEmpty()) {
            final RadialElement re = queue.poll();
            final boolean include = image.getBoolean(re.getIndex());
            intList.add(re.getIndex());
            if (include) {
                cnt++;
            }
            area++;
            final RadialElement next = queue.peek();
            // Automatically allow points within the smallest
            // resolution to be considered foreground
            if (re.getIndex() < resolution[2]) {
                mask.set(re.getIndex());
            }
            if (next != null) {
                if (next.getRadius() > cutoff) {

                    cutoff += resolution[0];
                    final float fraction = cnt / area;
                    // Use a sigmoidal function as the moving threshold
                    final float threshold = radialThreshold / (1 + (float) Math.exp( -area / sigma));

                    if (fraction < threshold) {
                        break;
                    }

                    for (final int i : intList) {
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

    /**
     * Method to generate a pixel probability map as part of the pre-processing. Basically the first step in entropy
     * maximization calculations, each pixel is given a probability based on how likely that pixel value is in THAT
     * image. Due to the nature of the image, background is usually a much higher probability than signal, resulting in
     * an image where darker pixels are more likely to be signal.
     * 
     * This has since been changed to output entropy: -p*ln(p) instead of just p.
     * 
     * @param image to transform to probability.
     * @return the transformed probability map
     */
    private ModelImage probabilityMap(final ModelImage image) {

        int value;
        ModelImage outImage;

        final int bins = 4; // Histogram bins are for 4 pixels
        final int num = 256 / 4;
        final float[] p = new float[num];
        final int[] buffer = new int[length];
        final int[] histo = new int[num];

        final float[] pImage = new float[length];

        try {
            image.exportData(0, length, buffer);
        } catch (final IOException e) {
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
            pImage[i] = (float) ( -pVal * Math.log(pVal));
        }

        outImage = new ModelImage(ModelImage.FLOAT, extents, image.getImageName() + "_prob");
        try {
            outImage.importData(0, pImage, true);
        } catch (final IOException e) {
            MipavUtil.displayError("Image locked");
            e.printStackTrace();
        }

        return outImage;

    }

    private void filterShotNoiseMean(final ModelImage image) {
        final int dataType = image.getType();
        int maxDiff;

        if (dataType == ModelImage.BYTE || dataType == ModelImage.UBYTE) {
            maxDiff = 3;
        } else if (dataType == ModelImage.SHORT || dataType == ModelImage.USHORT) {
            maxDiff = 600;
        } else {
            return;
        }

        final ModelImage meanImage = (ModelImage) image.clone();
        final AlgorithmMean mean = new AlgorithmMean(meanImage, 3, false, true);
        mean.run();
        final int[] buffer = new int[length];
        int[] medBuffer = new int[length];
        final int[] outBuffer = new int[length];
        int diff;

        try {
            image.exportData(0, length, buffer);
            meanImage.exportData(0, length, medBuffer);
        } catch (final IOException e) {
            MipavUtil.displayError("Could not export data from original image");
            e.printStackTrace();
        }

        final HashSet<Integer> adjustPtsHash = new HashSet<Integer>();
        final HashSet<Integer> addPtsHash = new HashSet<Integer>();

        for (int i = 0; i < length; i++) {
            diff = Math.abs(buffer[i] - medBuffer[i]);
            if (diff >= maxDiff) {
                buffer[i] = medBuffer[i];
                final int x = i % width;
                final int y = (i % (width * height)) / width;
                final int z = i / (width * height);
                for (int nx = x - 1; nx <= x + 1; nx++) {
                    if (nx < 0 || nx >= width) {
                        continue;
                    }
                    for (int ny = y - 1; ny <= y + 1; ny++) {
                        if (ny < 0 || ny >= height) {
                            continue;
                        }
                        for (int nz = z - 1; nz <= z + 1; nz++) {
                            if (nz < 0 || nz >= depth) {
                                continue;
                            }
                            final int ind = nx + ny * width + nz * width * height;
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
                final int i = iter.next();
                final int x = i % width;
                final int y = (i % (width * height)) / width;
                final int z = i / (width * height);
                final int kMed = findMean(buffer, i);
                if (Math.abs(buffer[i] - kMed) >= maxDiff) {
                    outBuffer[i] = kMed;
                    for (int nx = x - 1; nx <= x + 1; nx++) {
                        if (nx < 0 || nx >= width) {
                            continue;
                        }
                        for (int ny = y - 1; ny <= y + 1; ny++) {
                            if (ny < 0 || ny >= height) {
                                continue;
                            }
                            for (int nz = z - 1; nz <= z + 1; nz++) {
                                if (nz < 0 || nz >= depth) {
                                    continue;
                                }
                                final int ind = nx + ny * width + nz * width * height;
                                addPtsHash.add(ind);
                            }
                        }
                    }
                }
            }
            iter = adjustPtsHash.iterator();
            while (iter.hasNext()) {
                final int i = iter.next();
                buffer[i] = outBuffer[i];
            }
            adjustPtsHash.clear();
            adjustPtsHash.addAll(addPtsHash);
            addPtsHash.clear();
        }

        meanImage.disposeLocal();
        try {
            image.importData(0, outBuffer, true);
        } catch (final IOException e) {
            MipavUtil.displayError("Unable to import filtered image");
            e.printStackTrace();
        }

    }

    private int findMean(final int[] buffer, final int i) {
        final int x = i % width;
        final int y = (i % (width * height)) / width;
        final int z = i / (width * height);
        final int kWidth = Math.min(3, 2 + Math.min(x, width - 1 - x));
        final int kHeight = Math.min(3, 2 + Math.min(y, height - 1 - y));
        final int kDepth = Math.min(3, 2 + Math.min(z, depth - 1 - z));
        final int cnt = kWidth * kHeight * kDepth;
        int sum = 0;

        for (int nx = x - 1; nx <= x + 1; nx++) {
            if (nx < 0 || nx >= width) {
                continue;
            }
            for (int ny = y - 1; ny <= y + 1; ny++) {
                if (ny < 0 || ny >= height) {
                    continue;
                }
                for (int nz = z - 1; nz <= z + 1; nz++) {
                    if (nz < 0 || nz >= depth) {
                        continue;
                    }
                    sum += buffer[nx + ny * width + nz * width * height];
                }
            }
        }
        final int kMean = (int) ((float) sum / (float) cnt);
        return kMean;
    }

    private void readSWC() throws IOException {

        final BufferedReader input = new BufferedReader(new FileReader(filFile));
        String line = null;
        while ( (line = input.readLine()) != null) {
            if (line.startsWith("#")) {
                continue;
            }
            final String[] lineArray = line.split(" ");
            if (lineArray.length > 5 && lineArray[1].equals("2")) {// Only care about axon
                final float x = Float.valueOf(lineArray[2]);
                final float y = Float.valueOf(lineArray[3]);
                final float z = Float.valueOf(lineArray[4]);
                swcLines.add(new Vector3f(x, y, z));
            }
        }

        input.close();
    }

    private void writeResults(final ArrayList<float[]> results) throws IOException {

        final File file = new File(filFile);
        final String parent = file.getParent();
        String name = file.getName();
        final int ind = name.lastIndexOf(".");
        if (ind != -1) {
            name = name.substring(0, ind);
        }

        name += "_actin.csv";

        final String fileOut = parent + File.separator + name;

        final FileWriter fw = new FileWriter(new File(fileOut));

        fw.append("Image name," + srcImage.getImageFileName() + "\n");
        fw.append("Filament name," + file.getName() + "\n");
        fw.append("\n");
        fw.append("X,Y,Z,Distance along axon,Radial actin sum\n");
        for (int i = 0; i < results.size(); i++) {
            final float[] line = results.get(i);
            for (int j = 0; j < line.length; j++) {
                fw.append(line[j] + ",");
            }
            fw.append("\n");
        }

        fw.close();

    }

    private class RadialElement implements Comparable<RadialElement> {

        private final float radius;

        private final int ind;

        private RadialElement(final Vector3f point, final int index) {
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
        public int compareTo(final RadialElement o) {
            final float ro = o.radius;
            if (radius > ro) {
                return 1;
            } else if (radius < ro) {
                return -1;
            }
            return 0;
        }

    }

}
