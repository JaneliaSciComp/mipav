import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;

import gov.nih.mipav.view.ViewJFrameImage;

import java.awt.Point;
import java.awt.event.WindowEvent;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.*;
import java.util.Map.Entry;

import javax.swing.JTextPane;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;

import quickhull3d.Point3d;
import quickhull3d.QuickHull3D;
import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * A sister plugin to the 3DSWCStats set of plugins. The dialog opens up a very rudimentary 3D viewer of the neuron
 * skeleton. The user can then choose which branch to use as the axon when exported to a SWC and in the stats CSV. This
 * algorithm is very similar to the original 3DSWCStats since all the methods still occur. However, the dialog allows
 * the user to choose an axon and thus requires the connections in this algorithm to be rearranged.
 * 
 * @see PlugInAlgorithm3DSWCStats
 * @author wangvg
 * 
 */
public class PlugInAlgorithm3DSWCViewer extends AlgorithmBase {

    private ArrayList<ArrayList<Integer>> connections;

    private int currentAxon;

    private boolean disconnected;

    private ArrayList<float[]> joints;

    private TransMatrix mat;

    private final String resolutionUnit;

    private ArrayList<float[]> spacePts;

    private final ArrayList<ArrayList<float[]>> curTimeCoordinates;

    private final String parentDir;

    private final String baseName;

    private final JTextPane textArea;

    private boolean axonUseLength;

    private boolean showAxon;

    // private String imageFile;

    private final boolean showViewer;

    private boolean viewerOpen;

    private final boolean branchDensity;

    private int[][] faceVerticies;

    private ArrayList<Integer> tips;

    private int[] vertexInd;

    private float splitDist;

    private boolean haveSplitDist = false;

    public PlugInAlgorithm3DSWCViewer(/* String imFile, */final ArrayList<ArrayList<float[]>> coords, final String dir, final String name,
            final JTextPane text, final String resUnit, final boolean useLength, final boolean showView) {

        super();

        // imageFile = imFile;

        // destImage stores the image to be used in the viewer
        // It is RGB so that the axon and convex hull can be highlighted in different colors
        // and shown concurrently
        destImage = new ModelImage(ModelImage.ARGB, new int[] {512, 512}, "3D Neuron Viewer");
        parentDir = dir;
        baseName = name;

        destImage.setImageName(name);

        curTimeCoordinates = coords;

        textArea = text;
        resolutionUnit = resUnit;
        axonUseLength = useLength;
        showAxon = true;
        showViewer = showView;
        branchDensity = false;

        viewerOpen = false;
    }

    /**
     * For branch density, since no input image is needed.
     * 
     * @param file
     * @param text
     * @param resUnit
     */
    public PlugInAlgorithm3DSWCViewer(final ArrayList<ArrayList<float[]>> coords, final String dir, final String name, final JTextPane text,
            final String resUnit) {
        super();

        destImage = new ModelImage(ModelImage.ARGB, new int[] {512, 512}, "3D Neuron Viewer");
        parentDir = dir;
        baseName = name;
        curTimeCoordinates = coords;

        textArea = text;
        resolutionUnit = resUnit;
        axonUseLength = false;
        showAxon = true;
        showViewer = true;
        branchDensity = true;

        viewerOpen = false;
    }

    @Override
    /**
     * In this algorithm, only the setup steps are carried out. 
     * These steps are reading the Imaris file, attempting to
     * create connections, and building the basic structure 
     * for output to the 3D viewer. The writing steps are handled
     * in a different method. 
     */
    public void runAlgorithm() {

        try {
            disconnected = false;

            mat = new TransMatrix(3);

            // Attempt to make connections between all of the filaments
            connections = makeConnections(curTimeCoordinates);

            for (int i = 1; i < curTimeCoordinates.size(); i++) { // **MODIFICATION** (curTimeCoordinates replaced
                                                                  // swcCoordinates)
                final ArrayList<float[]> fil = curTimeCoordinates.get(i); // **MODIFICATION**
                if (fil.get(0)[4] == Float.NEGATIVE_INFINITY) {
                    // No connection was made, something is wrong
                    disconnected = true;
                    break;
                }
            }

            // In some cases, the Imaris IV file is not going to overlap,
            // in which case the connections must be determined using
            // a slight tolerance
            if (disconnected) {
                connections = makeConnectionsTol(curTimeCoordinates); // **MODIFICATION** (curTimeCoordinates replaced
                                                                      // swcCoordinates)
                for (int i = 1; i < curTimeCoordinates.size(); i++) { // **MODIFICATION**
                    final ArrayList<float[]> fil = curTimeCoordinates.get(i); // **MODIFICATION**
                    if (fil.get(0)[4] == Float.NEGATIVE_INFINITY) {
                        // No connection was made, something is wrong
                        append(baseName + ": Filament " + i + " is not connected properly.", PlugInDialog3DSWCStats.RED_TEXT);
                        setCompleted(false);
                        return;
                    }
                }
            }

            // Generate the indicies of which filaments in
            // swcCoordinates are tips (no forward connections)
            tips = new ArrayList<Integer>();
            for (int i = 0; i < connections.size(); i++) {
                final ArrayList<Integer> branches = connections.get(i);
                if (branches.size() == 0) {
                    tips.add(i);
                }
            }

            // Generate the 3D viewer if needed
            joints = new ArrayList<float[]>();
            if (showViewer) {
                joints.add(curTimeCoordinates.get(0).get(0)); // **MODIFICATION** (curTimeCoordinates replaced
                                                              // swcCoordinates)
                for (int i = 0; i < curTimeCoordinates.size(); i++) { // **MODIFICATION**
                    final ArrayList<float[]> fil = curTimeCoordinates.get(i); // **MODIFICATION**
                    joints.add(fil.get(fil.size() - 1));
                }

                // Make the viewer image here
                setupImage();
                viewerOpen = true;
            }

            // The output is a 2D array, with the last array being
            // the vertex indicies so that it all fits in one output
            // array
            final int[][] tempArray = calculateConvexHull(curTimeCoordinates, tips); // **MODIFICATION**
                                                                                     // (curTimeCoordinates replaced
                                                                                     // swcCoordinates)
            faceVerticies = new int[tempArray.length - 1][];
            vertexInd = tempArray[tempArray.length - 1];
            for (int i = 0; i < faceVerticies.length; i++) {
                faceVerticies[i] = tempArray[i];
            }

            if (branchDensity) {
                currentAxon = tips.get(0);
            } else {
                // append("Opening image " + imageFile, blackText);
                // FileIO reader = new FileIO();
                // srcImage = reader.readImage(imageFile);
            }

            setCompleted(true);

        } catch (final Exception e) {
            append("The following Java error has occured:", PlugInDialog3DSWCStats.RED_TEXT);
            append(e.toString(), PlugInDialog3DSWCStats.RED_TEXT);
            for (final StackTraceElement t : e.getStackTrace()) {
                append(t.toString(), PlugInDialog3DSWCStats.RED_TEXT);
            }
            setCompleted(false);
            return;
        }

        setCompleted(true);

    }

    /**
     * This method handles the actual writing and calculation steps after the user has chosen an axon. Other than
     * rearranging the order of forward connections, this portion is more or less the same as the back half of the
     * sibling algorithm.
     * 
     * @see PlugInAlgorithm3DSWCStats
     */
    public void write() {

        // allTimeCoordinates = new ArrayList<ArrayList<ArrayList<float[]>>>();

        setCompleted(false);

        final PlugInAlgorithm3DSWCViewer alg = this;

        final Thread writeThread = new Thread() {
            @Override
            public void run() {
                try {
                    viewerOpen = false;

                    int maxOrder;

                    // Pre-processing before all the statistics are
                    // calculated. Makes sure the logical ordering
                    // of branches is correct

                    calculateDistances(curTimeCoordinates); // **MODIFICATION** (curTimeCoordinates replaced
                                                            // swcCoordinates)
                    if (axonUseLength) {
                        maxOrder = determineOrder_useLength(curTimeCoordinates, connections, currentAxon); // **MODIFICATION**
                    } else {
                        maxOrder = determineOrder(curTimeCoordinates, connections, currentAxon);
                    }

                    if ( !showViewer) {

                        currentAxon = tips.get(0);
                        for (final int i : tips) {
                            if (curTimeCoordinates.get(i).get(0)[5] == 1) { // **MODIFICATION**
                                currentAxon = i;
                                break;
                            }
                        }
                    }

                    if (branchDensity) {
                        // We aren't doing any sort of split point work
                        final float hullVolume = convexHullVolumeNew(curTimeCoordinates, vertexInd, faceVerticies); // **MODIFICATION**
                                                                                                                    // (curTimeCoordinates
                                                                                                                    // replaced
                                                                                                                    // swcCoordinates)

                        final ArrayList<String> messages = consolidateFilaments(curTimeCoordinates, connections, maxOrder); // **MODIFICATION**
                        final float[] branchLengths = recalculateDistances(curTimeCoordinates, connections); // **MODIFICATION**
                        addToMessages(curTimeCoordinates, messages); // **MODIFICATION**

                        try {
                            final String output = exportStatsToCSV(curTimeCoordinates, connections, messages, branchLengths, /*-1.0f,*/hullVolume, maxOrder); // **MODIFICATION**
                            append("Exported stats to CSV -> " + output, PlugInDialog3DSWCStats.BLACK_TEXT);
                        } catch (final IOException e) {
                            append("Could not export stats to CSV for " + baseName, PlugInDialog3DSWCStats.RED_TEXT);
                        }

                        try {
                            final String output = writeSWC(curTimeCoordinates, messages, branchLengths); // **MODIFICATION**
                            append("Converted to SWC -> " + output, PlugInDialog3DSWCStats.BLACK_TEXT);
                        } catch (final IOException e) {
                            append("Could not write SWC for " + baseName, PlugInDialog3DSWCStats.RED_TEXT);
                        }

                        final ArrayList<float[]> stats = branchDensity(curTimeCoordinates); // **MODIFICATION**

                        append("Writing branch density information", PlugInDialog3DSWCStats.BLACK_TEXT);
                        // write stats out

                        final String output = parentDir + File.separator + "branch_density.csv";
                        final File outputFile = new File(output);

                        final FileWriter fw = new FileWriter(outputFile, true);

                        fw.append(baseName + "\n");

                        final String[] rows = new String[] {"Distance,", "# of Branches,", "Branch Lengths,"};

                        // Writes out the branch frequency stats in
                        // their respective bins
                        float distance = 5;
                        final float increment = 5;
                        int num = 0;
                        float lengths = 0;

                        for (int i = 0; i < stats.size(); i++) {
                            final float[] stat = stats.get(i);
                            while (stat[0] > distance) {
                                rows[0] += distance + ",";
                                rows[1] += num + ",";
                                rows[2] += lengths + ",";
                                distance += increment;
                                num = 0;
                                lengths = 0;
                            }
                            num += stat[1];
                            lengths += stat[2];
                        }

                        rows[0] += distance;
                        rows[1] += num;
                        rows[2] += lengths;

                        for (int i = 0; i < 3; i++) {
                            rows[i] += "\n";
                            fw.append(rows[i]);
                        }

                        fw.close();

                        append("Finished writing stats and SWC files", PlugInDialog3DSWCStats.GREEN_TEXT);
                        append("-----------------------------------------", PlugInDialog3DSWCStats.BLACK_TEXT);
                    } else {

                        // Need to use the split point provided to calculate stats
                        float[] splitLoc = null;
                        int filIndex = currentAxon;

                        // Find the location of the split point of the growth cone
                        ArrayList<float[]> piece = curTimeCoordinates.get(currentAxon); // **MODIFICATION**
                                                                                        // (curTimeCoordinates replaced
                                                                                        // swcCoordinates)
                        float sDist = splitDist;
                        int ind = piece.size() - 1;
                        while (sDist > 0) {
                            final float[] fa = piece.get(ind);
                            float[] fa2;
                            if (ind == 0) {
                                final int con = (int) fa[4];
                                if (con == -1) {
                                    append("Growth cone is longer than axon", PlugInDialog3DSWCStats.RED_TEXT);
                                    setCompleted(false);
                                    alg.notifyListeners(alg);
                                    return;
                                }
                                piece = curTimeCoordinates.get(con); // **MODIFICATION**
                                filIndex = con;
                                ind = piece.size() - 1;
                                fa2 = piece.get(ind);
                            } else {
                                fa2 = piece.get(ind - 1);
                                ind--;
                            }
                            float dist = 0;
                            for (int i = 0; i < 3; i++) {
                                final float d = fa[i] - fa2[i];
                                dist += d * d;
                            }
                            // Accumulate distances until you reach
                            // the provided value
                            dist = (float) Math.sqrt(dist);
                            if (sDist - dist > 0) {
                                sDist -= dist;
                            } else {
                                // Choose whichever one is closer to the
                                // given distance
                                if (Math.abs(sDist) > Math.abs(sDist - dist)) {
                                    splitLoc = fa2;
                                } else {
                                    splitLoc = fa;
                                }
                                break;
                            }
                        }

                        // axonIndex contains the currentAxon value in the
                        // new set of filaments in the growth cone
                        final int[] axonIndex = new int[1];
                        final ArrayList<ArrayList<float[]>> growthCone = filterGrowthCone(splitLoc, filIndex, axonIndex);

                        // Redo all the preprocessing steps for the growth
                        // cone to prevent any weird issues arising
                        ArrayList<ArrayList<Integer>> gcConnections;
                        if (disconnected) {
                            gcConnections = makeConnectionsTol(growthCone);
                        } else {
                            gcConnections = makeConnections(growthCone);
                        }

                        calculateDistances(growthCone);
                        int gcOrder;
                        if (axonUseLength) {
                            gcOrder = determineOrder_useLength(growthCone, gcConnections, axonIndex[0]);
                        } else {
                            gcOrder = determineOrder(growthCone, gcConnections, axonIndex[0]);
                        }

                        // The output is a 2D array, with the last array being
                        // the vertex indicies so that it all fits in one output
                        // array
                        final int[][] tempArray = calculateConvexHull(growthCone, null);
                        final int[][] gcFaceVerticies = new int[tempArray.length - 1][];
                        final int[] gcVertexInd = tempArray[tempArray.length - 1];
                        for (int i = 0; i < gcFaceVerticies.length; i++) {
                            gcFaceVerticies[i] = tempArray[i];
                        }

                        final float gcHullVolume = convexHullVolumeNew(growthCone, gcVertexInd, gcFaceVerticies);

                        final ArrayList<String> gcMessages = consolidateFilaments(growthCone, gcConnections, gcOrder);
                        final float[] gcLengths = recalculateDistances(growthCone, gcConnections);
                        addToMessages(growthCone, gcMessages);

                        // Need to use the split point provided to calculate stats
                        float[] splitLocMax = null;
                        int filIndexMax = currentAxon;

                        // Find the location of the split point of the maximum growth cone
                        piece = curTimeCoordinates.get(currentAxon); // **MODIFICATION** (curTimeCoordinates replaced
                                                                     // swcCoordinates)
                        ind = piece.size() - 1;
                        int lastCon = 0;
                        while (true) {
                            final float[] fa = piece.get(ind);
                            if (ind == 0) {

                                final int con = (int) fa[4];
                                if (con == -1) {
                                    filIndexMax = lastCon;
                                    splitLocMax = fa;
                                    break;
                                }
                                lastCon = con;
                                piece = curTimeCoordinates.get(con); // **MODIFICATION**
                                ind = piece.size() - 1;
                            } else {
                                ind--;
                            }
                        }

                        // axonIndexMax contains the currentAxon value in the
                        // new set of filaments in the growth cone
                        final int[] axonIndexMax = new int[1];
                        final ArrayList<ArrayList<float[]>> growthConeMax = filterGrowthCone(splitLocMax, filIndexMax, axonIndexMax);

                        // Redo all the preprocessing steps for the growth
                        // cone to prevent any weird issues arising
                        ArrayList<ArrayList<Integer>> gcConnectionsMax;
                        if (disconnected) {
                            gcConnectionsMax = makeConnectionsTol(growthConeMax);
                        } else {
                            gcConnectionsMax = makeConnections(growthConeMax);
                        }

                        calculateDistances(growthConeMax);
                        int gcOrderMax;
                        if (axonUseLength) {
                            gcOrderMax = determineOrder_useLength(growthConeMax, gcConnectionsMax, axonIndexMax[0]);
                        } else {
                            gcOrderMax = determineOrder(growthConeMax, gcConnectionsMax, axonIndexMax[0]);
                        }

                        // The output is a 2D array, with the last array being
                        // the vertex indicies so that it all fits in one output
                        // array
                        final int[][] tempArrayMax = calculateConvexHull(growthConeMax, null);
                        final int[][] gcFaceVerticiesMax = new int[tempArrayMax.length - 1][];
                        final int[] gcVertexIndMax = tempArrayMax[tempArrayMax.length - 1];
                        for (int i = 0; i < gcFaceVerticiesMax.length; i++) {
                            gcFaceVerticiesMax[i] = tempArrayMax[i];
                        }

                        final float gcHullVolumeMax = convexHullVolumeNew(growthConeMax, gcVertexIndMax, gcFaceVerticiesMax);

                        final ArrayList<String> gcMessagesMax = consolidateFilaments(growthConeMax, gcConnectionsMax, gcOrderMax);
                        final float[] gcLengthsMax = recalculateDistances(growthConeMax, gcConnectionsMax);
                        addToMessages(growthConeMax, gcMessagesMax);

                        // PlugInAlgorithmSWCVolume alg = new PlugInAlgorithmSWCVolume(srcImage, growthCone);
                        // alg.run();

                        try {
                            append("Calculating volumes", PlugInDialog3DSWCStats.BLACK_TEXT);
                            final String output = exportStatsToCSV(growthCone, gcConnections, gcMessages, gcLengths, /*
                                                                                                                      * alg
                                                                                                                      * .
                                                                                                                      * getVolume
                                                                                                                      * (
                                                                                                                      * )
                                                                                                                      * ,
                                                                                                                      */gcHullVolume, gcOrder, growthConeMax,
                                    gcConnectionsMax, gcMessagesMax, gcLengthsMax, gcHullVolumeMax, gcOrderMax);
                            append("Exported stats to CSV -> " + output, PlugInDialog3DSWCStats.BLACK_TEXT);
                        } catch (final IOException e) {
                            append("Could not export stats to CSV for " + baseName, PlugInDialog3DSWCStats.RED_TEXT);
                        }

                        try {
                            final String output = writeSWC(growthCone, gcMessages, gcLengths);
                            append("Converted to SWC -> " + output, PlugInDialog3DSWCStats.BLACK_TEXT);
                        } catch (final IOException e) {
                            append("Could not write SWC for " + baseName, PlugInDialog3DSWCStats.RED_TEXT);
                        }

                        append("Finished writing stats and SWC files", PlugInDialog3DSWCStats.GREEN_TEXT);
                        append("-----------------------------------------", PlugInDialog3DSWCStats.BLACK_TEXT);

                    }
                    setCompleted(true);

                } catch (final Exception e) {
                    append("The following Java error has occured:", PlugInDialog3DSWCStats.RED_TEXT);
                    append(e.toString(), PlugInDialog3DSWCStats.RED_TEXT);
                    for (final StackTraceElement t : e.getStackTrace()) {
                        append(t.toString(), PlugInDialog3DSWCStats.RED_TEXT);
                    }
                }

                alg.notifyListeners(alg);
            }
        };

        writeThread.start();
    }

    public void setSplit(final float dist) {
        splitDist = dist;
        haveSplitDist = true;
    }

    public void setAxon(final int axon) {
        currentAxon = axon;
    }

    public int getAxon() {
        return currentAxon;
    }

    public ArrayList<Integer> getTips() {
        return tips;
    }

    public boolean isTipInHull(final int row) {

        if (vertexInd == null) {
            return false;
        }

        final int branch = tips.get(row);
        for (int i = 0; i < vertexInd.length; i++) {
            if (branch == vertexInd[i]) {
                return true;
            }
        }

        return false;
    }

    /**
     * Used to prevent actions from occuring in the master dialog if the viewer is open
     * 
     * @return
     */
    public boolean isViewerOpen() {
        return viewerOpen;
    }

    public void viewerClosed() {
        viewerOpen = false;
    }

    /**
     * Used as part of the action for dragging the mouse with the left click. This applies a rotation based on the mouse
     * movement and returns the new rotation angles, which is then used in the transform method.
     * 
     * @param rx
     * @param ry
     * @return
     */
    public int[] mouseRotate(final int rx, final int ry) {
        // Want to rotate about center of neuron, so recenter the points

        mat.setRotate(rx, ry, 0, TransMatrix.DEGREES);

        final Vector3f[] bases = new Vector3f[3];
        bases[0] = new Vector3f(1, 0, 0);
        bases[1] = new Vector3f(0, 1, 0);
        bases[2] = new Vector3f(0, 0, 1);

        final Vector3f[] rBases = new Vector3f[3];
        for (int i = 0; i < 3; i++) {
            rBases[i] = new Vector3f();
            mat.transformAsPoint3Df(bases[i], rBases[i]);
        }

        final float rxRad = (float) Math.atan2(rBases[1].Z, rBases[2].Z);
        final float ryRad = (float) Math.atan2( -rBases[0].Z, Math.sqrt(Math.pow(rBases[1].Z, 2) + Math.pow(rBases[2].Z, 2)));
        final float rzRad = (float) Math.atan2(rBases[0].Y, rBases[0].X);

        final int rxDeg = (int) Math.round(rxRad * 180.0 / Math.PI);
        final int ryDeg = (int) Math.round(ryRad * 180.0 / Math.PI);
        final int rzDeg = (int) Math.round(rzRad * 180.0 / Math.PI);

        return new int[] {rxDeg, ryDeg, rzDeg};
    }

    /**
     * Used to translate the neuron structure in the X-Y viewing plane. Unlike in mouse rotate, which simply passes the
     * new parameters back to the dialog to carry out the transform, this one takes care of the translation by itself
     * (since it is a fairly trivial endeavour).
     * 
     * @param tx
     * @param ty
     * @param zoom
     */
    public void mouseTranslate(final int tx, final int ty, final double zoom) {
        for (int i = 0; i < spacePts.size(); i++) {
            final float[] pt = spacePts.get(i);
            pt[0] += tx * zoom;
            pt[1] += ty * zoom;
        }

        makeViewImage();

        highlightAxon(currentAxon);

        final ViewJFrameImage frame = destImage.getParentFrame();

        if ( !showAxon) {
            displayConvexHull();
        }

        frame.updateImages(true);

    }

    /**
     * Rotates the image and makes the projection to be displayed in the image frame. Also includes zoom factor now.
     * 
     * @param rx
     * @param ry
     * @param rz
     * @param zoom
     */
    public void transformImage(final int tx, final int ty, final int rx, final int ry, final int rz, final double zoom) {

        mat = new TransMatrix(3);
        // if you want to zoom, do it AFTER rotate
        mat.setRotate(rx, ry, rz, TransMatrix.DEGREES);
        mat.setZoom(zoom, zoom, zoom);

        // Rotate about the center of the image
        for (int i = 0; i < joints.size(); i++) {
            final float[] joint = joints.get(i);
            final float[] transJoint = new float[3];
            for (int j = 0; j < 2; j++) {
                transJoint[j] = joint[j] - 256;
            }
            transJoint[2] = joint[2];
            final float[] rotJoint = new float[3];
            mat.transform(transJoint, rotJoint);
            for (int j = 0; j < 2; j++) {
                rotJoint[j] += 256;
            }
            rotJoint[0] += tx * zoom;
            rotJoint[1] += ty * zoom;

            spacePts.set(i, rotJoint);

        }

        makeViewImage();

        final ViewJFrameImage frame = destImage.getParentFrame();

        highlightAxon(currentAxon);
        if ( !showAxon) {
            displayConvexHull();
        }

        frame.updateImages(true);
    }

    public void showAxon() {
        showAxon = true;

    }

    public void showHull() {
        showAxon = false;
    }

    public void setUseLength(final boolean useLength) {
        axonUseLength = useLength;
    }

    /**
     * Check to see whether a vector in space denoted by vecOrigin and vecEnd will potentially intersect which a plane.
     * We only care about intersection within the vector, so we do not check the entirety of the line.
     * 
     * @param tip branch tip to be adde
     * @param ptA one of the other points in the new plane to check
     * @param ptB another point in the new plane to check
     * @param vecOrigin start point of the vector to check against
     * @param vecEnd end point of the vector to check against
     * @return the point of intersection, or null if it will not intersect
     */
    private Vector3f planeVectorIntersection(final Vector3f tip, final Vector3f ptA, final Vector3f ptB, final Vector3f vecOrigin, final Vector3f vecEnd) {

        final Vector3f planeA = Vector3f.sub(ptA, tip);
        final Vector3f planeB = Vector3f.sub(ptB, tip);
        final Vector3f normal = Vector3f.cross(planeA, planeB);

        final Vector3f vec = Vector3f.sub(vecEnd, vecOrigin);

        final Vector3f c = Vector3f.sub(vecOrigin, tip);

        final float ncd = c.dot(normal);
        final float ndd = vec.dot(normal);

        final float t = -ncd / ndd;
        final float epsilon = 0.01f;// We can handle a little overlap due to floating point error
        if (t > epsilon && t < 1 - epsilon) {
            return vecOrigin.add(vec.scale(t));
        }

        return null;
    }

    /**
     * Tests to see if a given face contains the intersection point returned in planeVectorIntersection. Checks within
     * the triangular region of the face and not to infinity.
     * 
     * @param tip branch tip to be added
     * @param ptA one of the other points in the new plane to check
     * @param ptB another point in the new plane to check
     * @param intersect the intersection point
     * @return whether or not the intersection lies in the face
     */
    private boolean faceFilamentIntersection(final Vector3f tip, final Vector3f ptA, final Vector3f ptB, final Vector3f intersect) {

        Vector3f u = Vector3f.sub(ptA, tip);
        Vector3f v = Vector3f.sub(ptB, tip);
        Vector3f i = Vector3f.sub(intersect, tip);

        float scale = u.X * v.Y - v.X * u.Y;
        float a = (i.X * v.Y - i.Y * v.X) / scale;
        float b = (i.Y * u.X - i.X * u.Y) / scale;

        if (a >= 0 && b >= 0) {

            u = Vector3f.sub(tip, ptA);
            v = Vector3f.sub(ptB, ptA);
            i = Vector3f.sub(intersect, ptA);

            scale = u.X * v.Y - v.X * u.Y;
            a = (i.X * v.Y - i.Y * v.X) / scale;
            b = (i.Y * u.X - i.X * u.Y) / scale;
            if (a >= 0 && b >= 0) {
                return true;
            }
        }

        return false;
    }

    /**
     * Method used to add branches that are not part of the calculated convex hull. Currently a bit rudimentary. For
     * each missing tip, check to see if creating a new set of faces from each face would create a valid hull.
     * 
     * Right now, this is achieved by checking to see if any new set of faces collides with the neuron structure.
     * 
     * Only the closest face that does not collide with the neuron structure is chosen to make the new set of faces.
     * This is probably not the best, but has shown to work relatively well in many cases.
     * 
     * This is not very fool-proof, so this would be something to remove. Hard to verify the accuracy of the results.
     * 
     * @param swcCoordinates
     * @param tips
     * @param hull
     * @param verticies
     * @return a combo array of the hull face indicies as well as the vertex indicies. The last index in the array
     *         contains the verticies.
     */

    private int[][] addBranchesToHull(final ArrayList<ArrayList<float[]>> swcCoordinates, final ArrayList<Integer> tips, final int[][] hull,
            final int[] verticies) {

        append("Attempting to add all tips to hull", PlugInDialog3DSWCStats.BLACK_TEXT);
        final ArrayList<Integer> missing = new ArrayList<Integer>(tips);
        // Find which tips are not included in the hull
        for (int i = 0; i < verticies.length; i++) {
            if (missing.contains(verticies[i])) {
                missing.remove(new Integer(verticies[i]));
            }
        }

        // Convert hull and verticies to ArrayLists so that they can grow
        final ArrayList<int[]> hullList = new ArrayList<int[]>();
        final ArrayList<Integer> vertexList = new ArrayList<Integer>();
        for (int i = 0; i < hull.length; i++) {
            hullList.add(hull[i]);
        }
        for (int i = 0; i < verticies.length; i++) {
            vertexList.add(verticies[i]);
        }

        final ArrayDeque<Integer> deque = new ArrayDeque<Integer>(missing);
        final int factor = 3;
        final int maxIterations = factor * missing.size();
        int cnt = 0;
        // There are some cases where you could get into an infinite loop, so just
        // make sure that doesn't happen.
        while ( !deque.isEmpty() && cnt < maxIterations) {
            cnt++;
            float minDist = Float.MAX_VALUE;
            int[] minHull = null;
            final int tipNum = deque.pop();
            ArrayList<float[]> fil = swcCoordinates.get(tipNum);
            final Vector3f tipVec = new Vector3f(fil.get(fil.size() - 1));

            // Check every currently registered face
            for (int j = 0; j < hullList.size(); j++) {
                final int[] hullFace = hullList.get(j);
                fil = swcCoordinates.get(vertexList.get(hullFace[0]));
                final Vector3f ptA = new Vector3f(fil.get(fil.size() - 1));
                fil = swcCoordinates.get(vertexList.get(hullFace[1]));
                final Vector3f ptB = new Vector3f(fil.get(fil.size() - 1));
                fil = swcCoordinates.get(vertexList.get(hullFace[2]));
                final Vector3f ptC = new Vector3f(fil.get(fil.size() - 1));
                boolean add = true;
                // Need to check new set of faces against every single
                // filament in the neuron, so check all 3 new faces
                for (int k = 0; k < swcCoordinates.size(); k++) {
                    fil = swcCoordinates.get(k);
                    final Vector3f vecOrigin = new Vector3f(fil.get(0));
                    final Vector3f vecEnd = new Vector3f(fil.get(fil.size() - 1));
                    Vector3f intersect = planeVectorIntersection(tipVec, ptA, ptB, vecOrigin, vecEnd);
                    if (intersect != null) {// if intersect is null, there is no intersection
                        if (faceFilamentIntersection(tipVec, ptA, ptB, intersect)) {
                            add = false;
                            break;
                        }
                    }
                    intersect = planeVectorIntersection(tipVec, ptA, ptC, vecOrigin, vecEnd);
                    if (intersect != null) {
                        if (faceFilamentIntersection(tipVec, ptA, ptC, intersect)) {
                            add = false;
                            break;
                        }
                    }
                    intersect = planeVectorIntersection(tipVec, ptB, ptC, vecOrigin, vecEnd);
                    if (intersect != null) {
                        if (faceFilamentIntersection(tipVec, ptB, ptC, intersect)) {
                            add = false;
                            break;
                        }
                    }
                }

                // If no collision occurs between the new set of faces and the current
                // neuron structure, see if it is the minimum distance face
                if (add) {
                    final float distance = distanceVectorToPlaneNew(ptA, ptB, ptC, tipVec);
                    if (distance < minDist) {
                        minDist = distance;
                        minHull = hullFace;
                    }
                }
            }

            // If for some reason no valid face is available, push to the end of the
            // queue, wait for new faces to be generated, and then try again
            if (minHull == null) {
                deque.addLast(tipNum);
            } else {
                // Break up the original face into 3 new faces and add them
                // back into the list
                final int[] face = minHull;
                hullList.remove(face);
                final int vertexSize = vertexList.size();
                vertexList.add(tipNum);
                hullList.add(new int[] {face[0], face[1], vertexSize});
                hullList.add(new int[] {face[0], face[2], vertexSize});
                hullList.add(new int[] {face[1], face[2], vertexSize});
            }

        }

        if ( !deque.isEmpty()) {
            System.out.println("Fell through, a tip was not added");
        }

        final int[][] outArray = new int[hullList.size() + 1][];
        for (int i = 0; i < hullList.size(); i++) {
            outArray[i] = hullList.get(i);
        }
        final int[] vertexArray = new int[vertexList.size()];
        for (int i = 0; i < vertexArray.length; i++) {
            vertexArray[i] = vertexList.get(i);
        }
        outArray[hullList.size()] = vertexArray;

        return outArray;
    }

    /**
     * Adds the branch length and distance along the axon/parent to the output messages.
     * 
     * @param messages
     */
    private void addToMessages(final ArrayList<ArrayList<float[]>> swcCoordinates, final ArrayList<String> messages) {
        for (int i = 0; i < messages.size(); i++) {
            String message = messages.get(i);
            final ArrayList<float[]> fil = swcCoordinates.get(i);
            message += String.format("# Branch Length: %3.5f %s\n", fil.get(fil.size() - 1)[3], resolutionUnit);
            String parent;
            if (i != 0) {
                if (i == 1) {
                    parent = "axon";
                } else {
                    parent = "parent branch";
                }
                message += String.format("# Length along %s: %3.5f %s\n", parent, fil.get(0)[3], resolutionUnit);
            }
            message += "#------------------------------------\n";
            messages.set(i, message);
        }
    }

    /**
     * Used to write messages with certain fonts to the text area that can be used to monitor progress and errors in
     * lieu of using the java console.
     * 
     * @param message
     * @param a
     */
    private void append(final String message, final AttributeSet a) {
        final Document doc = textArea.getDocument();
        try {
            doc.insertString(doc.getLength(), message + "\n", a);
        } catch (final BadLocationException e) {
            e.printStackTrace();
        }

        textArea.setCaretPosition(doc.getLength());
    }

    private ArrayList<float[]> branchDensity(final ArrayList<ArrayList<float[]>> curTimeCoordinates) {
        // Would be based on the consolidated branches, so comes after that method
        // Could reuse the messages to figure out where the branches fall

        // Get the indicies of the first order branches
        final ArrayList<Integer> indicies = new ArrayList<Integer>();
        for (int i = 1; i < curTimeCoordinates.size(); i++) { // **MODIFICATION**
            final ArrayList<float[]> fil = curTimeCoordinates.get(i);
            if (fil.get(0)[5] == 2) {
                indicies.add(i);
            }
        }

        final ArrayList<float[]> axon = curTimeCoordinates.get(0);
        final float axonLength = axon.get(axon.size() - 1)[3];

        final ArrayList<float[]> stats = new ArrayList<float[]>();
        for (int i = indicies.size() - 1; i >= 0; i--) {
            final int ind = indicies.get(i);
            int until;
            if (i == indicies.size() - 1) {
                until = curTimeCoordinates.size();
            } else {
                until = indicies.get(i + 1);
            }
            final float axonDistance = axonLength - curTimeCoordinates.get(ind).get(0)[3];
            final int numBranches = until - ind;
            float branchLengths = 0;
            for (int j = ind; j < until; j++) {
                final ArrayList<float[]> fil = curTimeCoordinates.get(j);
                branchLengths += fil.get(fil.size() - 1)[3];
            }

            final float[] branchStat = new float[] {axonDistance, numBranches, branchLengths};
            stats.add(branchStat);

        }

        return stats;
    }

    /**
     * Bresenham line algorithm used to draw the paths between two points. This version is used with the subclasses to
     * keep track of which lines are displayed in the active slice to make it easier to add nodes between points
     * 
     * @param p0
     * @param p1
     * @return
     */
    private ArrayList<Point> bresenham(final Point p0, final Point p1) {

        int x0 = p0.x;
        final int x1 = p1.x;
        int y0 = p0.y;
        final int y1 = p1.y;

        final ArrayList<Point> pts = new ArrayList<Point>();
        final int dx = Math.abs(x1 - x0);
        final int dy = Math.abs(y1 - y0);
        int sx, sy;
        int err, e2;
        if (x0 < x1) {
            sx = 1;
        } else {
            sx = -1;
        }
        if (y0 < y1) {
            sy = 1;
        } else {
            sy = -1;
        }
        err = dx - dy;

        while (true) {
            pts.add(new Point(x0, y0));

            if (x0 == x1 && y0 == y1) {
                break;
            }
            e2 = 2 * err;
            if (e2 > -dy) {
                err -= dy;
                x0 += sx;
            }
            if (e2 < dx) {
                err += dx;
                y0 += sy;
            }
        }

        return pts;
    }

    /**
     * Calculates the convex hull using the QuickHull3D jar. Also recently included is the functionality to add every
     * single tip into the hull, but this has been hazardous and probably should not be done.
     * 
     * 
     * @param swcCoordinates
     * @param tips
     * @return
     */
    private int[][] calculateConvexHull(final ArrayList<ArrayList<float[]>> swcCoordinates, final ArrayList<Integer> tips) {

        final ArrayList<Point3d> ptList = new ArrayList<Point3d>();
        final float[] originPt = swcCoordinates.get(0).get(0);
        final Point3d originPt3d = new Point3d(originPt[0], originPt[1], originPt[2]);
        ptList.add(originPt3d);

        for (int i = 0; i < swcCoordinates.size(); i++) {
            final ArrayList<float[]> fil = swcCoordinates.get(i);
            final float[] pt = fil.get(fil.size() - 1);
            final Point3d pt3d = new Point3d(pt[0], pt[1], pt[2]);

            ptList.add(pt3d);
        }

        final Point3d[] pts = new Point3d[ptList.size()];
        ptList.toArray(pts);

        final QuickHull3D hull = new QuickHull3D(pts);

        final Point3d[] verticies = hull.getVertices();
        final int[][] faceVerticiesA = hull.getFaces();

        final int[][] faceVerticies = new int[faceVerticiesA.length + 1][];

        for (int i = 0; i < faceVerticiesA.length; i++) {
            faceVerticies[i] = faceVerticiesA[i];
        }

        final int[] temp = new int[verticies.length];

        int cnt = 0;
        for (int i = 1; i < verticies.length; i++) {
            final Point3d vPt = verticies[i];
            Point3d lPt = ptList.get(cnt);
            while (vPt.x != lPt.x || vPt.y != lPt.y || vPt.z != lPt.z) {
                cnt++;
                if (cnt >= ptList.size()) {// Something messed up
                    System.out.println("Something is wrong with convex hull calculations");
                    break;
                }
                lPt = ptList.get(cnt);
            }
            temp[i] = cnt - 1;

        }

        faceVerticies[faceVerticies.length - 1] = temp;

        // faceVerticies = addBranchesToHull(swcCoordinates, tips, faceVerticiesA, temp);

        return faceVerticies;

    }

    /**
     * Determines the length of each individual filament read from the Imaris trace. These will be used to determine the
     * axon filaments.
     */
    private void calculateDistances(final ArrayList<ArrayList<float[]>> swcCoordinates) {
        if (disconnected) {// Distance at head of filament is not 0
            for (int i = 0; i < swcCoordinates.size(); i++) {
                final ArrayList<float[]> fil = swcCoordinates.get(i);
                if (i == 0) {
                    fil.get(0)[3] = 0;
                } else {
                    final float[] head = fil.get(0);
                    final int c = (int) fil.get(0)[4];
                    final ArrayList<float[]> conn = swcCoordinates.get(c);
                    final float[] tail = conn.get(conn.size() - 1);

                    float dist = 0;
                    for (int j = 0; j < 3; j++) {
                        final float diff = tail[j] - head[j];
                        dist += diff * diff;
                    }
                    head[3] = (float) Math.sqrt(dist);
                }
                float[] currPt = fil.get(0);
                for (int j = 1; j < fil.size(); j++) {
                    final float[] nextPt = fil.get(j);
                    float dist = 0;
                    for (int k = 0; k < 3; k++) {
                        final float diff = currPt[k] - nextPt[k];
                        dist += diff * diff;
                    }
                    nextPt[3] = currPt[3] + (float) Math.sqrt(dist);
                    currPt = nextPt;
                }
            }
        } else {// Distance at head of filament is 0
            for (final ArrayList<float[]> alf : swcCoordinates) {
                float[] currPt = alf.get(0);
                currPt[3] = 0;
                for (int i = 1; i < alf.size(); i++) {
                    final float[] nextPt = alf.get(i);
                    float dist = 0;
                    for (int j = 0; j < 3; j++) {
                        final float diff = currPt[j] - nextPt[j];
                        dist += diff * diff;
                    }
                    nextPt[3] = currPt[3] + (float) Math.sqrt(dist);
                    currPt = nextPt;
                }
            }
        }
    }

    /**
     * Consolidate the multitude of filaments from the input to make stat tracking easier. Also organizes the output SWC
     * so that branch numbers are logical and go from beggining of the axon to the end of the axon.
     * 
     * @param connections
     * @param maxOrder
     * @return
     */
    private ArrayList<String> consolidateFilaments(final ArrayList<ArrayList<float[]>> swcCoordinates, final ArrayList<ArrayList<Integer>> connections,
            final int maxOrder) {

        int offset;
        if (disconnected) {
            offset = 0;
        } else {
            offset = 1;
        }

        final ArrayList<ArrayList<float[]>> newFilaments = new ArrayList<ArrayList<float[]>>();

        ArrayList<float[]> current = new ArrayList<float[]>();
        final ArrayList<String> messages = new ArrayList<String>();
        // Keep track of which branches we need to add.
        // We need deque to make sure branches are added in the
        // correct and logical order.
        final ArrayList<ArrayDeque<Integer>> dequeList = new ArrayList<ArrayDeque<Integer>>();
        for (int i = 0; i < maxOrder; i++) {
            dequeList.add(new ArrayDeque<Integer>());
        }

        // To keep track of branch numbering.
        final int[] branchNumber = new int[maxOrder];

        // Keep track of which line in the SWC this coordinate
        // is connected to.
        int counter = 1;
        int currentOrder = 0;// Will do zero indexing here
        ArrayList<float[]> fil = swcCoordinates.get(0);
        fil.get(0)[4] = -1; // set first line's connection to -1 (origin for SWC)
        fil.get(1)[4] = 1;
        fil.get(1)[5] = 1;
        current.add(fil.get(0));
        dequeList.get(0).add(0);

        boolean isFinished = false;

        while ( !isFinished) {
            // This looks confusing, but maybe the comments will be helpful
            final ArrayDeque<Integer> currentFil = dequeList.get(currentOrder);
            final int ind = currentFil.poll();
            final ArrayList<Integer> connected = connections.get(ind);
            fil = swcCoordinates.get(ind);
            final float thisOrder = fil.get(0)[5];
            final float[] second = fil.get(offset);
            second[5] = thisOrder;
            current.add(second);
            counter++;
            for (int i = offset + 1; i < fil.size(); i++) {
                final float[] fa = fil.get(i);
                fa[4] = counter;
                fa[5] = thisOrder;
                current.add(fa);
                counter++;
            }

            // Reached the end of a branch, need to see if there
            // are child branches that need to be added as well
            if (connected.size() == 0) {
                // Add the consolidated branch
                newFilaments.add(current);
                /*
                 * Concurrently prepare information regarding this branch. This includes branch order, number, length,
                 * and distance along the axon/parent branch that the branch originates from. The latter two are added
                 * later after recalculating distances.
                 */
                String message = "";
                if (currentOrder == 0) {
                    message = "#------------------------------------\n" + "# Axon\n";
                } else {
                    message = "#------------------------------------\n" + "# Branch " + String.valueOf(branchNumber[1] + 1);
                    for (int i = 2; i <= currentOrder; i++) {
                        message += "." + String.valueOf(branchNumber[i] + 1);
                    }
                    message += "\n";
                    message += "# Branch Order: " + String.valueOf(currentOrder);
                    message += "\n";
                }

                messages.add(message);
                current = new ArrayList<float[]>();
                // Highest order branch, can't keep going higher
                if (currentOrder == maxOrder - 1) {
                    // Find the highest order which still needs
                    // to consolidate branches
                    if (dequeList.get(currentOrder).isEmpty()) {
                        while (dequeList.get(currentOrder).isEmpty()) {
                            branchNumber[currentOrder] = 0;
                            currentOrder--;
                            if (currentOrder == 0) {
                                isFinished = true;
                                break;
                            }
                        }
                        branchNumber[currentOrder]++;
                    }
                }
                // Not highest order, check to see if you added any
                // child branches
                else if (dequeList.get(currentOrder + 1).isEmpty()) {
                    while (dequeList.get(currentOrder).isEmpty()) {
                        branchNumber[currentOrder] = 0;
                        currentOrder--;
                        if (currentOrder == 0) {
                            isFinished = true;
                            break;
                        }
                    }
                    branchNumber[currentOrder]++;
                } else {
                    currentOrder++;// Go to higher branch since you populated it
                }

            }
            // This is not an endpoint
            else {
                for (int i = 0; i < connected.size(); i++) {
                    /*
                     * Add forward connections to the respective deques. First element goes into the same deque because
                     * it is the same order as this current branch. Add at the front so that you prioritize continuing
                     * the same branch.
                     * 
                     * Child branches (non-first element) are added to the end of the deques so that earlier children
                     * are written first.
                     */
                    final int next = connected.get(i);
                    fil = swcCoordinates.get(next);
                    fil.get(offset)[4] = counter;
                    final int order = (int) (fil.get(0)[5] - 1);
                    if (i == 0) {
                        dequeList.get(order).addFirst(next);
                    } else {
                        dequeList.get(order).add(next);
                    }

                }
            }
        }

        swcCoordinates.clear();
        swcCoordinates.addAll(newFilaments);

        return messages;
    }

    /**
     * 
     * Convex hull volume. Using the faces and some linear algebra, finds the volume within the convex hull. Each face
     * corresponds to a pyramid when connected to an arbitrary interior point (in this case, the centroid of all tip
     * points). The volume of all pyramids are added together.
     * 
     * For unconsolidated filaments, not consolidated ones.
     * 
     * @param swcCoordinates
     * @param vertexInd
     * @param faceVerticies
     * @return
     */
    private float convexHullVolumeNew(final ArrayList<ArrayList<float[]>> swcCoordinates, final int[] vertexInd, final int[][] faceVerticies) {

        float volume = 0;

        final Point3d[] verticies = new Point3d[vertexInd.length];
        final float[] origin = swcCoordinates.get(0).get(0);
        verticies[0] = new Point3d(origin[0], origin[1], origin[2]);

        for (int i = 1; i < vertexInd.length; i++) {
            final int index = vertexInd[i];
            final ArrayList<float[]> fil = swcCoordinates.get(index);
            final float[] tipPt = fil.get(fil.size() - 1);
            verticies[i] = new Point3d(tipPt[0], tipPt[1], tipPt[2]);
        }

        final Point3d centroid = new Point3d();
        for (int i = 0; i < verticies.length; i++) {
            final Point3d pt = verticies[i];
            centroid.add(pt);
        }

        final double num = verticies.length;
        centroid.x /= num;
        centroid.y /= num;
        centroid.z /= num;

        for (int i = 0; i < faceVerticies.length; i++) {

            final Point3d ptA = new Point3d(verticies[faceVerticies[i][0]]);
            final Point3d ptB = new Point3d(verticies[faceVerticies[i][1]]);
            final Point3d ptC = new Point3d(verticies[faceVerticies[i][2]]);
            final Point3d ptCentroid = new Point3d(centroid);

            ptB.sub(ptA);
            ptC.sub(ptA);
            ptCentroid.sub(ptA);

            final Vector3f vecA = new Vector3f((float) ptB.x, (float) ptB.y, (float) ptB.z);
            final Vector3f vecB = new Vector3f((float) ptC.x, (float) ptC.y, (float) ptC.z);
            final Vector3f vecC = new Vector3f((float) ptCentroid.x, (float) ptCentroid.y, (float) ptCentroid.z);
            final Vector3f axb = Vector3f.cross(vecA, vecB);

            volume += Math.abs(axb.dot(vecC));
        }

        volume /= 6.0f;

        return volume;
    }

    /**
     * Imaris filament files are patterned in a way that branch organization can be inferred from it. Simplified the
     * algorithm to determine the axon/branch number so that it is based on the pattern seen in the files.
     * 
     * Basically take the last step of the three pass method earlier since in forward connections, the lower number is
     * the one that goes towards the axon and others are child branches.
     */
    private int determineOrder(final ArrayList<ArrayList<float[]>> swcCoordinates, final ArrayList<ArrayList<Integer>> connections, final int branch) {

        if (showViewer) {
            rearrangeBranches(swcCoordinates, connections, branch);
        }

        int maxOrder = 1;

        final ArrayDeque<Integer> queue = new ArrayDeque<Integer>();

        ArrayList<float[]> fil = swcCoordinates.get(0);
        ArrayList<Integer> branches = connections.get(0);
        float[] head = fil.get(0);

        head[5] = 1;

        for (int i = 0; i < branches.size(); i++) {
            final int ind = branches.get(i);
            queue.add(ind);
            fil = swcCoordinates.get(i);
        }

        for (int i = 0; i < branches.size(); i++) {
            final int ind = branches.get(i);
            fil = swcCoordinates.get(ind);
            if (i == 0) {
                fil.get(0)[5] = head[5];
            } else {
                fil.get(0)[5] = head[5] + 1;
            }
        }

        while ( !queue.isEmpty()) {
            final int i = queue.poll();
            fil = swcCoordinates.get(i);
            head = fil.get(0);
            branches = connections.get(i);
            if (branches.size() == 0) {
                continue;
            }
            for (int j = 0; j < branches.size(); j++) {
                final int ind = branches.get(j);
                queue.add(ind);
                fil = swcCoordinates.get(j);
            }

            for (int j = 0; j < branches.size(); j++) {
                final int ind = branches.get(j);
                fil = swcCoordinates.get(ind);
                if (j == 0) {
                    fil.get(0)[5] = head[5];
                } else {
                    fil.get(0)[5] = head[5] + 1;
                    if (head[5] + 1 > maxOrder) {
                        maxOrder = (int) (head[5] + 1);
                    }
                }
            }
        }

        return maxOrder;
    }

    /**
     * Three pass process to determine branch ordering and which filaments are the axon. Determines by finding the
     * longest path from the first filament.
     * 
     * Changed name to longest length to allow for axon to determined by either longest length or by filament ordering
     * 
     * @param connections
     */

    private int determineOrder_useLength(final ArrayList<ArrayList<float[]>> swcCoordinates, final ArrayList<ArrayList<Integer>> connections, final int branch) {

        final ArrayDeque<Integer> queue = new ArrayDeque<Integer>();

        ArrayList<float[]> fil = swcCoordinates.get(0);
        float[] head = fil.get(0);
        float[] tail = fil.get(fil.size() - 1);
        float dist = tail[3];

        int maxOrder = 1;

        ArrayList<Integer> branches = connections.get(0);
        for (final int i : branches) {
            queue.add(i);
            fil = swcCoordinates.get(i);
            fil.get(0)[3] = dist;
        }

        final TreeMap<Float, Integer> tmap = new TreeMap<Float, Integer>(new Comparator<Float>() {
            @Override
            public int compare(final Float o1, final Float o2) {
                final float f1 = o1.floatValue();
                final float f2 = o2.floatValue();
                if (f1 > f2) {
                    return -1;
                } else if (f1 < f2) {
                    return 1;
                } else {
                    return 0;
                }
            }
        });

        // Pass 1
        // Accumulate the length of each filament. At the head
        // of the filament, store the cumulative length for
        // later use. Once a tip has been reached, put the
        // length to that tip in a tree map.
        //
        // Pretty much a BFS as the ArrayDeque is used as a queue

        while ( !queue.isEmpty()) {
            final int i = queue.poll();
            fil = swcCoordinates.get(i);
            head = fil.get(0);
            tail = fil.get(fil.size() - 1);
            dist = tail[3] + head[3];
            head[3] = dist;
            branches = connections.get(i);
            if (branches.isEmpty()) {
                tmap.put(dist, i);
                tail[3] = dist;
            }
            for (final int j : branches) {
                queue.add(j);
                fil = swcCoordinates.get(j);
                fil.get(0)[3] = dist;
            }
        }

        // Pass 2
        // Tree map is based on length to the tips. Working
        // backwards from the furthest points to the closest
        // points should increase efficiency.
        //
        // Trace back from the tips and replace the tail
        // length values to the length at the tip. If you
        // are tracing backwards and the connected filament
        // already has been reached by a longer filament
        // (which should always be the case because of the
        // tree map), then move on to the next tip.

        // Rearrange forward connections so that the first one
        // in the list is the longest path away from this
        // filament.

        while ( !tmap.isEmpty()) {
            final Entry<Float, Integer> entry = tmap.pollFirstEntry();
            final int i = entry.getValue();
            dist = entry.getKey();
            fil = swcCoordinates.get(i);
            int c = (int) fil.get(0)[4];
            int prev = i;
            while (c > -1) {
                fil = swcCoordinates.get(c);
                head = fil.get(0);
                tail = fil.get(fil.size() - 1);
                if (tail[3] < dist) {
                    tail[3] = dist;
                    branches = connections.get(c);
                    branches.remove(new Integer(prev));
                    branches.add(0, prev);
                    prev = c;
                    c = (int) head[4];
                } else {
                    c = -1;
                }
            }
        }

        if (showViewer) {
            rearrangeBranches(swcCoordinates, connections, branch);
        }

        // Pass 3
        // Forward connections are organized so that the longest
        // path is the first element. Increment the branch order
        // of all the other elements in the list. Traverse the
        // entire neuron in a BFS.

        fil = swcCoordinates.get(0);
        head = fil.get(0);
        head[5] = 1;

        branches = connections.get(0);
        for (int i = 0; i < branches.size(); i++) {
            final int ind = branches.get(i);
            queue.add(ind);
            fil = swcCoordinates.get(i);
        }

        for (int i = 0; i < branches.size(); i++) {
            final int ind = branches.get(i);
            fil = swcCoordinates.get(ind);
            if (i == 0) {
                fil.get(0)[5] = head[5];
            } else {
                fil.get(0)[5] = head[5] + 1;
            }
        }

        while ( !queue.isEmpty()) {
            final int i = queue.poll();
            fil = swcCoordinates.get(i);
            head = fil.get(0);
            branches = connections.get(i);
            if (branches.size() == 0) {
                continue;
            }
            for (int j = 0; j < branches.size(); j++) {
                final int ind = branches.get(j);
                queue.add(ind);
                fil = swcCoordinates.get(j);
            }

            for (int j = 0; j < branches.size(); j++) {
                final int ind = branches.get(j);
                fil = swcCoordinates.get(ind);
                if (j == 0) {
                    fil.get(0)[5] = head[5];
                } else {
                    fil.get(0)[5] = head[5] + 1;
                    if (head[5] + 1 > maxOrder) {
                        maxOrder = (int) (head[5] + 1);
                    }
                }
            }
        }

        return maxOrder;

    }

    private void displayConvexHull() {

        for (int i = 0; i < faceVerticies.length; i++) {
            for (int j = 0; j < faceVerticies[i].length; j++) {

                int j1 = j + 1;
                if (j1 == faceVerticies[i].length) {
                    j1 = 0;
                }
                final int ind1 = faceVerticies[i][j];
                final int ind2 = faceVerticies[i][j1];
                float[] pt0f;
                float[] pt1f;
                if (ind1 == 0) {
                    pt0f = spacePts.get(0);
                    pt1f = spacePts.get(vertexInd[ind2] + 1);
                } else if (ind2 == 0) {
                    pt0f = spacePts.get(vertexInd[ind1] + 1);
                    pt1f = spacePts.get(0);
                } else {
                    pt0f = spacePts.get(vertexInd[ind1] + 1);
                    pt1f = spacePts.get(vertexInd[ind2] + 1);
                }

                final Point pt0 = new Point((int) pt0f[0], (int) pt0f[1]);
                final Point pt1 = new Point((int) pt1f[0], (int) pt1f[1]);

                final ArrayList<Point> line = bresenham(pt0, pt1);

                for (int k = 0; k < line.size(); k++) {
                    final Point pt = line.get(k);
                    if (pt.x > 0 && pt.x < 512 && pt.y > 0 && pt.y < 512) {
                        destImage.setC(pt.x + pt.y * 512, 2, 255);
                    }
                }
            }
        }
    }

    private float distanceVectorToPlaneNew(final Vector3f originPt, final Vector3f vecA, final Vector3f vecB, final Vector3f headPt) {
        final Vector3f a = Vector3f.sub(vecA, originPt);
        final Vector3f b = Vector3f.sub(vecB, originPt);
        final Vector3f pt = Vector3f.sub(headPt, originPt);
        // Vector vecC is already relative since it contains direction information

        final Vector3f d = Vector3f.cross(a, b);
        final float mag = d.length();
        final float num = d.dot(pt);
        final float dist = num / mag;

        return Math.abs(dist);
    }

    private String exportStatsToCSV(final ArrayList<ArrayList<float[]>> swcCoordinates, final ArrayList<ArrayList<Integer>> connections,
            final ArrayList<String> messages, final float[] branchLengths, /* float neuronVolume, */final float hullVolume, final int maxOrder)
            throws IOException {

        final String output = parentDir + File.separator + baseName + "_stats.csv";
        final File outputFile = new File(output);

        final FileWriter fw = new FileWriter(outputFile);

        fw.append("Units," + resolutionUnit + "\n");

        // Write the new branch info here

        writeBranchInformation(swcCoordinates, connections, fw, /* neuronVolume, */hullVolume, maxOrder);

        /*
         * String branchInfo = ""; branchInfo += "Total branch length," + String.valueOf(branchLengths[0]) + "\n";
         * branchInfo += "Minus axon," + String.valueOf(branchLengths[1]) + "\n\n";
         * 
         * fw.append(branchInfo);
         */

        final String header = "Branch Number,Branch Order,Branch Length,Length along parent \n";

        fw.append(header);

        for (final String s : messages) {
            final StringBuilder sb = new StringBuilder(30);
            final String[] rows = s.split("\n");
            int rowNum = 1;

            // Write branch number (or axon);
            final String branch = rows[rowNum].replace("#", "").trim();
            final String[] branchSplit = branch.split(" ");
            if (branchSplit.length == 1) {
                sb.append("Axon");
                sb.append(",");
                sb.append("0"); // Write axon
                rowNum++;
            } else {
                sb.append(branchSplit[1]);
                sb.append(",");
                rowNum++;
                // Write branch order
                final String order = rows[rowNum].replace("#", "").trim();
                final String[] orderSplit = order.split(" ");
                sb.append(orderSplit[2]);
                rowNum++;
            }
            sb.append(",");

            // Write length
            final String length = rows[rowNum].replace("#", "").trim();
            final String[] lengthSplit = length.split(" ");
            sb.append(lengthSplit[2]);
            sb.append(",");
            rowNum++;

            // Write length along parent
            if (branchSplit.length == 1) {
                sb.append("Axon");
            } else {
                final String along = rows[rowNum].replace("#", "").trim();
                final String[] alongSplit = along.split(" ");
                sb.append(alongSplit[alongSplit.length - 2]);
            }
            sb.append("\n");
            fw.append(sb.toString());
            // System.out.println(sb.toString());
        }

        fw.close();

        return output;
    }

    private String exportStatsToCSV(final ArrayList<ArrayList<float[]>> swcCoordinates, final ArrayList<ArrayList<Integer>> connections,
            final ArrayList<String> messages, final float[] branchLengths, /* float neuronVolume, */final float hullVolume, final int maxOrder,
            final ArrayList<ArrayList<float[]>> swcCoordinatesMax, final ArrayList<ArrayList<Integer>> connectionsMax, final ArrayList<String> messagesMax,
            final float[] branchLengthsMax, /* float neuronVolume, */final float hullVolumeMax, final int maxOrderMax) throws IOException {

        final String output = parentDir + File.separator + baseName + "_stats.csv";
        final File outputFile = new File(output);

        final FileWriter fw = new FileWriter(outputFile);

        fw.append("Units," + resolutionUnit + "\n");

        // Write the new branch info here

        writeBranchInformation(swcCoordinates, connections, fw, /* neuronVolume, */hullVolume, maxOrder, swcCoordinatesMax, connectionsMax,/*
                                                                                                                                            * neuronVolume
                                                                                                                                            * ,
                                                                                                                                            */hullVolumeMax,
                maxOrderMax);

        /*
         * String branchInfo = ""; branchInfo += "Total branch length," + String.valueOf(branchLengths[0]) + "\n";
         * branchInfo += "Minus axon," + String.valueOf(branchLengths[1]) + "\n\n";
         * 
         * fw.append(branchInfo);
         */

        final String header = "Branch Number,Branch Order,Branch Length,Length along parent \n";

        fw.append(header);

        final int mLength = messages.size();
        final int mMaxLength = messagesMax.size();
        final int mDiff = mMaxLength - mLength;
        int index = 0;
        for (final String s : messagesMax) {
            final StringBuilder sb = new StringBuilder(30);
            final String[] rows = s.split("\n");
            int rowNum = 1;

            // Write branch number (or axon);
            final String branch = rows[rowNum].replace("#", "").trim();
            final String[] branchSplit = branch.split(" ");
            if (branchSplit.length == 1) {
                sb.append("Axon");
                sb.append(",");
                sb.append("0"); // Write axon
                rowNum++;
            } else {
                sb.append(branchSplit[1]);
                sb.append(",");
                rowNum++;
                // Write branch order
                final String order = rows[rowNum].replace("#", "").trim();
                final String[] orderSplit = order.split(" ");
                sb.append(orderSplit[2]);
                rowNum++;
            }
            sb.append(",");

            // Write length
            final String length = rows[rowNum].replace("#", "").trim();
            final String[] lengthSplit = length.split(" ");
            sb.append(lengthSplit[2]);
            sb.append(",");
            rowNum++;

            // Write length along parent
            if (branchSplit.length == 1) {
                sb.append("Axon");
            } else {
                final String along = rows[rowNum].replace("#", "").trim();
                final String[] alongSplit = along.split(" ");
                sb.append(alongSplit[alongSplit.length - 2]);
            }
            sb.append("\n");
            index++;
            if (index == (mDiff + 1)) {
                sb.append("\n");
            }
            fw.append(sb.toString());
        }

        fw.close();

        return output;
    }

    /**
     * Creates a new set of points based on the provided split point. Only the portions of the original filament that
     * occur after the split point are output.
     * 
     * @param splitPt
     * @param filIndex
     * @param axonIndex
     * @return
     */
    private ArrayList<ArrayList<float[]>> filterGrowthCone(final float[] splitPt, final int filIndex, final int[] axonIndex) {
        final ArrayList<ArrayList<float[]>> growthCone = new ArrayList<ArrayList<float[]>>();
        final ArrayDeque<Integer> indexStack = new ArrayDeque<Integer>();

        ArrayList<float[]> fil = curTimeCoordinates.get(filIndex);
        ArrayList<float[]> addFil = new ArrayList<float[]>();
        boolean add = false;
        // Only add points from the given filament that
        // occur at or after the split point
        for (int i = 0; i < fil.size(); i++) {
            final float[] pt = fil.get(i);
            if (splitPt == pt) {
                add = true;
            }
            if (add) {
                final float[] newPt = new float[pt.length];
                System.arraycopy(pt, 0, newPt, 0, pt.length);
                addFil.add(newPt);
            }
        }

        growthCone.add(addFil);

        ArrayList<Integer> forward = connections.get(filIndex);
        for (int i = forward.size() - 1; i >= 0; i--) {
            indexStack.addFirst(forward.get(i));
        }

        // Add the rest of the filaments based on the forward
        // connections. Try to maintain original ordering as
        // much as possible.
        while ( !indexStack.isEmpty()) {
            final int index = indexStack.poll();
            if (axonIndex != null && index == currentAxon) {
                axonIndex[0] = growthCone.size();
            }
            fil = curTimeCoordinates.get(index);
            addFil = new ArrayList<float[]>();
            for (int i = 0; i < fil.size(); i++) {
                final float[] pt = fil.get(i);
                final float[] newPt = new float[pt.length];
                System.arraycopy(pt, 0, newPt, 0, pt.length);
                addFil.add(newPt);
            }
            growthCone.add(addFil);
            forward = connections.get(index);
            for (int i = forward.size() - 1; i >= 0; i--) {
                indexStack.addFirst(forward.get(i));
            }

        }
        return growthCone;
    }

    private String formatSWCLine(final int lineNum, final float[] line) {
        final String format = "%d %d %4.5f %4.5f %4.5f %4.2f %d \n";
        int type;
        if (line[5] == 1.0F) {
            type = 2;
        } else {
            type = 3;
        }

        float radius = line[6];
        if (radius < 0) {
            radius = 0.1f;
        }

        return String.format(format, lineNum, type, line[0], line[1], line[2], radius, (int) line[4]);
    }

    /**
     * Using the selected branch, create a bitset to be used as the image mask that overlays both the branch that was
     * selected as well as the connections all the way back to the origin.
     * 
     * @param branch
     * @return
     */
    private void highlightAxon(int branch) {

        currentAxon = branch;

        ArrayList<float[]> fil = curTimeCoordinates.get(branch);
        int c = (int) fil.get(0)[4];

        final ArrayList<ArrayList<Point>> lines = new ArrayList<ArrayList<Point>>();

        float[] oPt = spacePts.get(0);
        Point origin = new Point(Math.round(oPt[0]), Math.round(oPt[1]));
        float[] nPt = spacePts.get(1);
        Point pt = new Point(Math.round(nPt[0]), Math.round(nPt[1]));

        lines.add(bresenham(origin, pt));

        while (c > -1) {
            oPt = spacePts.get(branch + 1);
            nPt = spacePts.get(c + 1);
            origin = new Point(Math.round(oPt[0]), Math.round(oPt[1]));
            pt = new Point(Math.round(nPt[0]), Math.round(nPt[1]));
            lines.add(bresenham(origin, pt));

            fil = curTimeCoordinates.get(c);
            branch = c;
            c = (int) fil.get(0)[4];
        }

        for (int i = 0; i < lines.size(); i++) {
            final ArrayList<Point> line = lines.get(i);
            for (int j = 0; j < line.size(); j++) {
                pt = line.get(j);
                if (pt.x > 0 && pt.x < 512 && pt.y > 0 && pt.y < 512) {
                    destImage.setC(pt.x + pt.y * 512, 2, 0);
                    destImage.setC(pt.x + pt.y * 512, 3, 0);
                }
            }
        }

    }

    /**
     * Build both forward and backwards connections based on the coordinates read from the Imaris trace. The backwards
     * connection routine is taken from the Drosophila Registration algorithm written by Nish Pandya.
     * 
     * @return
     */
    private ArrayList<ArrayList<Integer>> makeConnections(final ArrayList<ArrayList<float[]>> swcCoordinates) {

        // Forward connections
        final ArrayList<ArrayList<Integer>> connections = new ArrayList<ArrayList<Integer>>();
        for (int i = 0; i < swcCoordinates.size(); i++) {
            final ArrayList<Integer> a = new ArrayList<Integer>();
            connections.add(a);
        }
        swcCoordinates.get(0).get(0)[4] = -1;
        for (int i = 1; i < swcCoordinates.size(); i++) {
            ArrayList<float[]> fil = swcCoordinates.get(i);
            final float[] head = fil.get(0);
            for (int j = i - 1; j >= 0; j--) {
                fil = swcCoordinates.get(j);
                final float[] tail = fil.get(fil.size() - 1);

                if (head[0] == tail[0] && head[1] == tail[1] && head[2] == tail[2]) {
                    head[4] = j;
                    connections.get(j).add(i);
                    break;
                }
            }
        }

        return connections;
    }

    /**
     * Build both forward and backwards connections based on the coordinates read from the Imaris trace. The backwards
     * connection routine is taken from the Drosophila Registration algorithm written by Nish Pandya. This version
     * includes a tolerance because the traces Akanni gave me do not overlap and thus need to be connected a little more
     * loosely.
     * 
     * @return
     */
    private ArrayList<ArrayList<Integer>> makeConnectionsTol(final ArrayList<ArrayList<float[]>> swcCoordinates) {

        final float tolerance = 0.15F;

        // Forward connections
        final ArrayList<ArrayList<Integer>> connections = new ArrayList<ArrayList<Integer>>();
        for (int i = 0; i < swcCoordinates.size(); i++) {
            final ArrayList<Integer> a = new ArrayList<Integer>();
            connections.add(a);
        }
        swcCoordinates.get(0).get(0)[4] = -1;
        for (int i = 1; i < swcCoordinates.size(); i++) {
            ArrayList<float[]> fil = swcCoordinates.get(i);
            final float[] head = fil.get(0);
            for (int j = i - 1; j >= 0; j--) {
                fil = swcCoordinates.get(j);
                final float[] tail = fil.get(fil.size() - 1);
                float dist = 0;
                for (int k = 0; k < 3; k++) {
                    final float diff = head[k] - tail[k];
                    dist += diff * diff;
                }

                if (dist < tolerance) {// To deal with non-overlapping filaments
                    head[4] = j;
                    connections.get(j).add(i);
                    break;
                }
            }
        }

        return connections;
    }

    /**
     * Draws all the branches on the image based on the transform variables made in the earlier portions.
     */
    private void makeViewImage() {

        // BitSet skeleton = new BitSet(512*512);
        final byte[] skeleton = new byte[512 * 512];

        final ArrayList<ArrayList<Point>> lines = new ArrayList<ArrayList<Point>>();

        float[] oPt = spacePts.get(0);
        Point origin = new Point(Math.round(oPt[0]), Math.round(oPt[1]));
        float[] nPt = spacePts.get(1);
        Point pt = new Point(Math.round(nPt[0]), Math.round(nPt[1]));

        lines.add(bresenham(origin, pt));

        for (int i = 0; i < connections.size(); i++) {
            oPt = spacePts.get(i + 1);
            origin = new Point(Math.round(oPt[0]), Math.round(oPt[1]));
            final ArrayList<Integer> branches = connections.get(i);
            for (final int j : branches) {
                nPt = spacePts.get(j + 1);
                pt = new Point(Math.round(nPt[0]), Math.round(nPt[1]));
                lines.add(bresenham(origin, pt));
            }
        }
        for (int i = 0; i < lines.size(); i++) {
            final ArrayList<Point> line = lines.get(i);
            for (int j = 0; j < line.size(); j++) {
                pt = line.get(j);
                if (pt.x > 0 && pt.x < 512 && pt.y > 0 && pt.y < 512) {
                    // skeleton.set(pt.x + pt.y*512);
                    skeleton[pt.x + pt.y * 512] = (byte) 255;
                }
            }
        }

        try {
            // destImage.importData(0, skeleton, true);
            for (int i = 0; i < 4; i++) {
                destImage.importRGBData(i, 0, skeleton, true);
            }
        } catch (final IOException e) {
            e.printStackTrace();
        }

    }

    /**
     * Using the selected branch, make sure that it comes first in the forward connections.
     */
    private void rearrangeBranches(final ArrayList<ArrayList<float[]>> swcCoordinates, final ArrayList<ArrayList<Integer>> connections, final int branch) {

        ArrayList<float[]> fil = swcCoordinates.get(branch);
        int change = branch;
        int c = (int) fil.get(0)[4];

        while (c > -1) {
            final ArrayList<Integer> branches = connections.get(c);
            branches.remove(new Integer(change));
            branches.add(0, change);
            fil = swcCoordinates.get(c);
            change = c;
            c = (int) fil.get(0)[4];
        }
    }

    /**
     * Recalculate distances for the consolidated branches so that it also includes the branch length and distance along
     * the axon/parent this branch originates from.
     */
    private float[] recalculateDistances(final ArrayList<ArrayList<float[]>> swcCoordinates, final ArrayList<ArrayList<Integer>> connections) {
        // 0 => Total branch length, 1=> Higher order branch length
        final float[] branchLengths = new float[2];

        for (int i = 0; i < swcCoordinates.size(); i++) {
            final ArrayList<float[]> fil = swcCoordinates.get(i);
            float parentLength = 0;
            if (i == 0) {
                fil.get(0)[3] = 0;
            } else {
                final float[] head = fil.get(0);
                int connection = (int) fil.get(0)[4] - 1;
                ArrayList<float[]> list = null;
                for (int j = 0; j < swcCoordinates.size(); j++) {
                    list = swcCoordinates.get(j);
                    if (connection >= list.size()) {
                        connection -= list.size();
                    } else {
                        break;
                    }
                }
                final float[] pt = list.get(connection);
                parentLength = pt[3];
                float dist = 0;
                for (int j = 0; j < 3; j++) {
                    final float diff = pt[j] - head[j];
                    dist += diff * diff;
                }
                head[3] = (float) Math.sqrt(dist);
            }
            float[] currPt = fil.get(0);
            for (int j = 1; j < fil.size(); j++) {
                final float[] nextPt = fil.get(j);
                float dist = 0;
                for (int k = 0; k < 3; k++) {
                    final float diff = currPt[k] - nextPt[k];
                    dist += diff * diff;
                }
                nextPt[3] = currPt[3] + (float) Math.sqrt(dist);
                currPt = nextPt;
            }
            branchLengths[0] += currPt[3];
            fil.get(0)[3] = parentLength;// head hold length along parent branch
        }

        final ArrayList<float[]> axon = swcCoordinates.get(0);
        final float axonLength = axon.get(axon.size() - 1)[3];
        branchLengths[1] = branchLengths[0] - axonLength;

        // Should now have length for all branches

        return branchLengths;
    }

    /**
     * The setup for making the 3D viewer. Translates the branch into the center of a 512x512 image with a at least a 20
     * pixel pad on each dimension in the base (no rotation) image.
     */
    private void setupImage() {
        final float[] minBounds = new float[] {Float.MAX_VALUE, Float.MAX_VALUE, Float.MAX_VALUE};
        final float[] maxBounds = new float[] { -Float.MAX_VALUE, -Float.MAX_VALUE, -Float.MAX_VALUE};

        for (int i = 0; i < joints.size(); i++) {
            final float[] joint = joints.get(i);
            for (int j = 0; j < 3; j++) {
                if (joint[j] < minBounds[j]) {
                    minBounds[j] = joint[j];
                }
                if (joint[j] > maxBounds[j]) {
                    maxBounds[j] = joint[j];
                }
            }
        }

        final float xDiff = maxBounds[0] - minBounds[0];
        final float yDiff = maxBounds[1] - minBounds[1];
        final float zSum = maxBounds[2] + minBounds[2];

        final float scale = 471.0F / Math.max(xDiff, yDiff);

        final float xPad = (512.0F - xDiff * scale) / 2.0F;
        final float yPad = (512.0F - yDiff * scale) / 2.0F;
        final float zCenter = zSum / 2.0F;

        spacePts = new ArrayList<float[]>();

        for (int i = 0; i < joints.size(); i++) {
            final float[] joint = joints.get(i);
            float x = joint[0];
            float y = joint[1];
            float z = joint[2];

            x = (x - minBounds[0]) * scale + xPad;
            y = (y - minBounds[1]) * scale + yPad;
            z = (z - zCenter) * scale;

            final float[] fPt = new float[] {x, y, z};
            final float[] fPt2 = new float[] {x, y, z};
            joints.set(i, fPt);
            spacePts.add(fPt2);
        }

        makeViewImage();

        currentAxon = tips.get(0);

    }

    private void writeBranchInformation(final ArrayList<ArrayList<float[]>> swcCoordinates, final ArrayList<ArrayList<Integer>> connections,
            final FileWriter fw, /* float neuronVolume, */final float hullVolume, final int maxOrder) throws IOException {

        final float[] lengths = new float[maxOrder];
        for (int i = 0; i < lengths.length; i++) {
            lengths[i] = 0.0F;
        }

        for (int i = 1; i < swcCoordinates.size(); i++) {
            final ArrayList<float[]> fil = swcCoordinates.get(i);
            final float filLength = fil.get(fil.size() - 1)[3];
            final int order = (int) fil.get(0)[5];
            lengths[order - 1] += filLength;
        }

        float allBranches = 0;
        float higherOrder = 0;

        for (int i = 1; i < maxOrder; i++) {
            allBranches += lengths[i];
            if (i != 1) {
                higherOrder += lengths[i];
            }
        }

        // if(neuronVolume >= 0){
        // fw.append("\nVolumes\n");
        // fw.append("Neuron volume," + neuronVolume + "\n");
        // }

        fw.append("Convex hull volume," + hullVolume + "\n\n");
        fw.append("Branch lengths\n");
        fw.append("Total Branches," + String.valueOf(allBranches) + "\n");
        fw.append("Higher order," + String.valueOf(higherOrder) + "\n\n");

        for (int i = 1; i < maxOrder; i++) {
            fw.append("Order " + String.valueOf(i) + "," + String.valueOf(lengths[i]) + "\n");
        }

        fw.append("\n");

    }

    private void writeBranchInformation(final ArrayList<ArrayList<float[]>> swcCoordinates, final ArrayList<ArrayList<Integer>> connections,
            final FileWriter fw, /* float neuronVolume, */final float hullVolume, final int maxOrder, final ArrayList<ArrayList<float[]>> swcCoordinatesMax,
            final ArrayList<ArrayList<Integer>> connectionsMax,
            /* float neuronVolume, */final float hullVolumeMax, final int maxOrderMax) throws IOException {

        final float[] lengthsMax = new float[maxOrderMax];
        for (int i = 0; i < lengthsMax.length; i++) {
            lengthsMax[i] = 0.0F;
        }

        int primaryNumberEntire = 0;
        float primaryTotalEntire = 0.0f;
        float primaryAverageEntire = 0.0f;
        int secondaryNumberEntire = 0;
        float secondaryTotalEntire = 0.0f;
        float secondaryAverageEntire = 0.0f;
        int tertiaryNumberEntire = 0;
        float tertiaryTotalEntire = 0.0f;
        float tertiaryAverageEntire = 0.0f;
        int quartenaryNumberEntire = 0;
        float quartenaryTotalEntire = 0.0f;
        float quartenaryAverageEntire = 0.0f;
        int quinaryNumberEntire = 0;
        float quinaryTotalEntire = 0.0f;
        float quinaryAverageEntire = 0.0f;
        int branchNumberEntire = 0;
        float branchTotalEntire = 0.0f;
        float branchAverageEntire = 0.0f;
        int higherNumberEntire = 0;
        float higherTotalEntire = 0.0f;
        float higherAverageEntire = 0.0f;

        for (int i = 1; i < swcCoordinatesMax.size(); i++) {
            final ArrayList<float[]> fil = swcCoordinatesMax.get(i);
            final float filLength = fil.get(fil.size() - 1)[3];
            final int order = (int) fil.get(0)[5] - 1;
            lengthsMax[order] += filLength;
            if (order == 1) {
                primaryNumberEntire++;
                primaryTotalEntire += filLength;
            } else if (order == 2) {
                secondaryNumberEntire++;
                secondaryTotalEntire += filLength;
            } else if (order == 3) {
                tertiaryNumberEntire++;
                tertiaryTotalEntire += filLength;
            } else if (order == 4) {
                quartenaryNumberEntire++;
                quartenaryTotalEntire += filLength;
            } else if (order == 5) {
                quinaryNumberEntire++;
                quinaryTotalEntire += filLength;
            }
            branchNumberEntire++;
            branchTotalEntire += filLength;
            if (order != 1) {
                higherNumberEntire++;
                higherTotalEntire += filLength;
            }
        }
        primaryAverageEntire = primaryTotalEntire / primaryNumberEntire;
        if (maxOrderMax - 1 >= 2) {
            secondaryAverageEntire = secondaryTotalEntire / secondaryNumberEntire;
        }
        if (maxOrderMax - 1 >= 3) {
            tertiaryAverageEntire = tertiaryTotalEntire / tertiaryNumberEntire;
        }
        if (maxOrderMax - 1 >= 4) {
            quartenaryAverageEntire = quartenaryTotalEntire / quartenaryNumberEntire;
        }
        if (maxOrderMax - 1 >= 5) {
            quinaryAverageEntire = quinaryTotalEntire / quinaryNumberEntire;
        }
        branchAverageEntire = branchTotalEntire / branchNumberEntire;
        if (maxOrderMax - 1 >= 2) {
            higherAverageEntire = higherTotalEntire / higherNumberEntire;
        }

        final float[] lengthsGrowth = new float[maxOrder];
        for (int i = 0; i < lengthsGrowth.length; i++) {
            lengthsGrowth[i] = 0.0F;
        }

        int primaryNumberGrowth = 0;
        float primaryTotalGrowth = 0.0f;
        float primaryAverageGrowth = 0.0f;
        int secondaryNumberGrowth = 0;
        float secondaryTotalGrowth = 0.0f;
        float secondaryAverageGrowth = 0.0f;
        int tertiaryNumberGrowth = 0;
        float tertiaryTotalGrowth = 0.0f;
        float tertiaryAverageGrowth = 0.0f;
        int quartenaryNumberGrowth = 0;
        float quartenaryTotalGrowth = 0.0f;
        float quartenaryAverageGrowth = 0.0f;
        int quinaryNumberGrowth = 0;
        float quinaryTotalGrowth = 0.0f;
        float quinaryAverageGrowth = 0.0f;
        int branchNumberGrowth = 0;
        float branchTotalGrowth = 0.0f;
        float branchAverageGrowth = 0.0f;
        int higherNumberGrowth = 0;
        float higherTotalGrowth = 0.0f;
        float higherAverageGrowth = 0.0f;

        for (int i = 1; i < swcCoordinates.size(); i++) {
            final ArrayList<float[]> fil = swcCoordinates.get(i);
            final float filLength = fil.get(fil.size() - 1)[3];
            final int order = (int) fil.get(0)[5] - 1;
            lengthsGrowth[order] += filLength;
            if (order == 1) {
                primaryNumberGrowth++;
                primaryTotalGrowth += filLength;
            } else if (order == 2) {
                secondaryNumberGrowth++;
                secondaryTotalGrowth += filLength;
            } else if (order == 3) {
                tertiaryNumberGrowth++;
                tertiaryTotalGrowth += filLength;
            } else if (order == 4) {
                quartenaryNumberGrowth++;
                quartenaryTotalGrowth += filLength;
            } else if (order == 5) {
                quinaryNumberGrowth++;
                quinaryTotalGrowth += filLength;
            }
            branchNumberGrowth++;
            branchTotalGrowth += filLength;
            if (order != 1) {
                higherNumberGrowth++;
                higherTotalGrowth += filLength;
            }
        }
        primaryAverageGrowth = primaryTotalGrowth / primaryNumberGrowth;
        if (maxOrder - 1 >= 2) {
            secondaryAverageGrowth = secondaryTotalGrowth / secondaryNumberGrowth;
        }
        if (maxOrder - 1 >= 3) {
            tertiaryAverageGrowth = tertiaryTotalGrowth / tertiaryNumberGrowth;
        }
        if (maxOrder - 1 >= 4) {
            quartenaryAverageGrowth = quartenaryTotalGrowth / quartenaryNumberGrowth;
        }
        if (maxOrder - 1 >= 5) {
            quinaryAverageGrowth = quinaryTotalGrowth / quinaryNumberGrowth;
        }
        branchAverageGrowth = branchTotalEntire / branchNumberGrowth;
        if (maxOrder - 1 >= 2) {
            higherAverageGrowth = higherTotalGrowth / higherNumberGrowth;
        }

        final int primaryNumberNonGrowth = primaryNumberEntire - primaryNumberGrowth;
        final float primaryTotalNonGrowth = primaryTotalEntire - primaryTotalGrowth;
        final float primaryAverageNonGrowth = primaryTotalNonGrowth / primaryNumberNonGrowth;
        final int secondaryNumberNonGrowth = secondaryNumberEntire - secondaryNumberGrowth;
        final float secondaryTotalNonGrowth = secondaryTotalEntire - secondaryTotalGrowth;
        float secondaryAverageNonGrowth = 0.0f;
        if (secondaryNumberNonGrowth >= 1) {
            secondaryAverageNonGrowth = secondaryTotalNonGrowth / secondaryNumberNonGrowth;
        }
        final int tertiaryNumberNonGrowth = tertiaryNumberEntire - tertiaryNumberGrowth;
        final float tertiaryTotalNonGrowth = tertiaryTotalEntire - tertiaryTotalGrowth;
        float tertiaryAverageNonGrowth = 0.0f;
        if (tertiaryNumberNonGrowth >= 1) {
            tertiaryAverageNonGrowth = tertiaryTotalNonGrowth / tertiaryNumberNonGrowth;
        }
        final int quartenaryNumberNonGrowth = quartenaryNumberEntire - quartenaryNumberGrowth;
        final float quartenaryTotalNonGrowth = quartenaryTotalEntire - quartenaryTotalGrowth;
        float quartenaryAverageNonGrowth = 0.0f;
        if (quartenaryNumberNonGrowth >= 1) {
            quartenaryAverageNonGrowth = quartenaryTotalNonGrowth / quartenaryNumberNonGrowth;
        }
        final int quinaryNumberNonGrowth = quinaryNumberEntire - quinaryNumberGrowth;
        final float quinaryTotalNonGrowth = quinaryTotalEntire - quinaryTotalGrowth;
        float quinaryAverageNonGrowth = 0.0f;
        if (quinaryNumberNonGrowth >= 1) {
            quinaryAverageNonGrowth = quinaryTotalNonGrowth / quinaryNumberNonGrowth;
        }
        final int branchNumberNonGrowth = branchNumberEntire - branchNumberGrowth;
        final float branchTotalNonGrowth = branchTotalEntire - branchTotalGrowth;
        final float branchAverageNonGrowth = branchTotalNonGrowth / branchNumberNonGrowth;
        final int higherNumberNonGrowth = higherNumberEntire - higherNumberGrowth;
        final float higherTotalNonGrowth = higherTotalEntire - higherTotalGrowth;
        float higherAverageNonGrowth = 0.0f;
        if (higherNumberNonGrowth >= 1) {
            higherAverageNonGrowth = higherTotalNonGrowth / higherNumberNonGrowth;
        }

        // if(neuronVolume >= 0){
        // fw.append("\nVolumes\n");
        // fw.append("Neuron volume," + neuronVolume + "\n");
        // }
        fw.append("Convex hull volume," + hullVolumeMax + "," + "," + "Growth cone length input,," + "For entire axon," + "Primary number,"
                + "Primary average length," + "Primary total length");
        if (maxOrderMax - 1 >= 2) {
            fw.append("," + "Secondary number," + "Secondary average length," + "Secondary total length");
        }
        if (maxOrderMax - 1 >= 3) {
            fw.append("," + "Tertiary number," + "Tertiary average length," + "Tertiary total length");
        }
        if (maxOrderMax - 1 >= 4) {
            fw.append("," + "Quartenary number," + "Quartenary average length," + "Quartenary total length");
        }
        if (maxOrderMax - 1 >= 5) {
            fw.append("," + "Quinary number," + "Quinary average length," + "Quinary total length");
        }
        fw.append("," + "Branch number," + "Branch average length," + "Branch total length");
        if (maxOrderMax >= 2) {
            fw.append("," + "High order number," + "High order average length," + "High order total length");
        }
        fw.append("\n");
        fw.append("," + "," + "," + String.valueOf(splitDist) + "," + "," + "," + String.valueOf(primaryNumberEntire) + ","
                + String.valueOf(primaryAverageEntire) + "," + String.valueOf(primaryTotalEntire));
        if (maxOrderMax - 1 >= 2) {
            fw.append("," + String.valueOf(secondaryNumberEntire) + "," + String.valueOf(secondaryAverageEntire) + "," + String.valueOf(secondaryTotalEntire));
        }
        if (maxOrderMax - 1 >= 3) {
            fw.append("," + String.valueOf(tertiaryNumberEntire) + "," + String.valueOf(tertiaryAverageEntire) + "," + String.valueOf(tertiaryTotalEntire));
        }
        if (maxOrderMax - 1 >= 4) {
            fw.append("," + String.valueOf(quartenaryNumberEntire) + "," + String.valueOf(quartenaryAverageEntire) + ","
                    + String.valueOf(quartenaryTotalEntire));
        }
        if (maxOrderMax - 1 >= 5) {
            fw.append("," + String.valueOf(quinaryNumberEntire) + "," + String.valueOf(quinaryAverageEntire) + "," + String.valueOf(quinaryTotalEntire));
        }
        fw.append("," + String.valueOf(branchNumberEntire) + "," + String.valueOf(branchAverageEntire) + "," + String.valueOf(branchTotalEntire));
        if (maxOrderMax - 1 >= 2) {
            fw.append("," + String.valueOf(higherNumberEntire) + "," + String.valueOf(higherAverageEntire) + "," + String.valueOf(higherTotalEntire));
        }
        fw.append("\n");

        fw.append("Branch lengths\n");
        fw.append("Total Branches," + String.valueOf(branchTotalEntire) + "," + "," + "," + "," + "For growth cone," + "Primary number,"
                + "Primary average length," + "Primary total length");
        if (maxOrderMax - 1 >= 2) {
            fw.append("," + "Secondary number," + "Secondary average length," + "Secondary total length");
        }
        if (maxOrderMax - 1 >= 3) {
            fw.append("," + "Tertiary number," + "Tertiary average length," + "Tertiary total length");
        }
        if (maxOrderMax - 1 >= 4) {
            fw.append("," + "Quartenary number," + "Quartenary average length," + "Quartenary total length");
        }
        if (maxOrderMax - 1 >= 5) {
            fw.append("," + "Quinary number," + "Quinary average length," + "Quinary total length");
        }
        fw.append("," + "Branch number," + "Branch average length," + "Branch total length");
        if (maxOrderMax >= 2) {
            fw.append("," + "High order number," + "High order average length," + "High order total length");
        }
        fw.append("\n");
        fw.append("Higher order," + String.valueOf(higherTotalEntire) + "," + "," + "," + "," + "," + String.valueOf(primaryNumberGrowth) + ","
                + String.valueOf(primaryAverageGrowth) + "," + String.valueOf(primaryTotalGrowth));
        if (maxOrderMax - 1 >= 2) {
            fw.append("," + String.valueOf(secondaryNumberGrowth) + "," + String.valueOf(secondaryAverageGrowth) + "," + String.valueOf(secondaryTotalGrowth));
        }
        if (maxOrderMax - 1 >= 3) {
            fw.append("," + String.valueOf(tertiaryNumberGrowth) + "," + String.valueOf(tertiaryAverageGrowth) + "," + String.valueOf(tertiaryTotalGrowth));
        }
        if (maxOrderMax - 1 >= 4) {
            fw.append("," + String.valueOf(quartenaryNumberGrowth) + "," + String.valueOf(quartenaryAverageGrowth) + ","
                    + String.valueOf(quartenaryTotalGrowth));
        }
        if (maxOrderMax - 1 >= 5) {
            fw.append("," + String.valueOf(quinaryNumberGrowth) + "," + String.valueOf(quinaryAverageGrowth) + "," + String.valueOf(quinaryTotalGrowth));
        }
        fw.append("," + String.valueOf(branchNumberGrowth) + "," + String.valueOf(branchAverageGrowth) + "," + String.valueOf(branchTotalGrowth));
        if (maxOrderMax - 1 >= 2) {
            fw.append("," + String.valueOf(higherNumberGrowth) + "," + String.valueOf(higherAverageGrowth) + "," + String.valueOf(higherTotalGrowth));
        }
        fw.append("\n\n");

        fw.append("Order " + "1" + "," + String.valueOf(lengthsMax[1]) + "," + "," + "," + "," + "Non growth cone," + "Primary number,"
                + "Primary average length," + "Primary total length");
        if (maxOrderMax - 1 >= 2) {
            fw.append("," + "Secondary number," + "Secondary average length," + "Secondary total length");
        }
        if (maxOrderMax - 1 >= 3) {
            fw.append("," + "Tertiary number," + "Tertiary average length," + "Tertiary total length");
        }
        if (maxOrderMax - 1 >= 4) {
            fw.append("," + "Quartenary number," + "Quartenary average length," + "Quartenary total length");
        }
        if (maxOrderMax - 1 >= 5) {
            fw.append("," + "Quinary number," + "Quinary average length," + "Quinary total length");
        }
        fw.append("," + "Branch number," + "Branch average length," + "Branch total length");
        if (maxOrderMax >= 2) {
            fw.append("," + "High order number," + "High order average length," + "High order total length");
        }
        fw.append("\n");

        if (maxOrderMax >= 2) {
            fw.append("Order " + "2" + "," + String.valueOf(lengthsMax[2]));
            fw.append("," + "," + "," + "," + ",");
        } else {
            fw.append("," + "," + "," + "," + "," + "," + ",");
        }
        fw.append(String.valueOf(primaryNumberNonGrowth) + "," + String.valueOf(primaryAverageNonGrowth) + "," + String.valueOf(primaryTotalNonGrowth));
        if (maxOrderMax - 1 >= 2) {
            fw.append("," + String.valueOf(secondaryNumberNonGrowth) + "," + String.valueOf(secondaryAverageNonGrowth) + ","
                    + String.valueOf(secondaryTotalNonGrowth));
        }
        if (maxOrderMax - 1 >= 3) {
            fw.append("," + String.valueOf(tertiaryNumberNonGrowth) + "," + String.valueOf(tertiaryAverageNonGrowth) + ","
                    + String.valueOf(tertiaryTotalNonGrowth));
        }
        if (maxOrderMax - 1 >= 4) {
            fw.append("," + String.valueOf(quartenaryNumberNonGrowth) + "," + String.valueOf(quartenaryAverageNonGrowth) + ","
                    + String.valueOf(quartenaryTotalNonGrowth));
        }
        if (maxOrderMax - 1 >= 5) {
            fw.append("," + String.valueOf(quinaryNumberNonGrowth) + "," + String.valueOf(quinaryAverageNonGrowth) + ","
                    + String.valueOf(quinaryTotalNonGrowth));
        }
        fw.append("," + String.valueOf(branchNumberNonGrowth) + "," + String.valueOf(branchAverageNonGrowth) + "," + String.valueOf(branchTotalNonGrowth));
        if (maxOrderMax - 1 >= 2) {
            fw.append("," + String.valueOf(higherNumberNonGrowth) + "," + String.valueOf(higherAverageNonGrowth) + "," + String.valueOf(higherTotalNonGrowth));
        }
        fw.append("\n");

        for (int i = 3; i < maxOrderMax; i++) {
            fw.append("Order " + String.valueOf(i) + "," + String.valueOf(lengthsMax[i]) + "\n");
        }

        fw.append("\n");

    }

    private String writeSWC(final ArrayList<ArrayList<float[]>> swcCoordinates, final ArrayList<String> messages, final float[] branchLengths)
            throws IOException {

        final String output = parentDir + File.separator + baseName + ".swc";
        final File outputFile = new File(output);

        final FileWriter fw = new FileWriter(outputFile);

        final String header = "#-----------------------------------------------------------------\n" + "# SWC generated in MIPAV\n"
                + "#-----------------------------------------------------------------\n" + "# Organization of branches is as such:\n"
                + "# -Axon is the first filament written (and noted as such)\n" + "# -Branches are written in order of closest to its parent's\n"
                + "#  origin\n" + "# -Higher order branches are given further identification\n" + "# \n" + "# For example: \n"
                + "# Branch 1 is the closest child branch of where the axon\n" + "# originates and Branch 2 is the second closest child branch.\n"
                + "# Branch 1.1 is the closest child branch from where the\n" + "# first branch originated from.\n"
                + "#-----------------------------------------------------------------\n" + "# Branch Length Information\n" + "# Total branch length: "
                + String.valueOf(branchLengths[0]) + " " + resolutionUnit + "\n" + "# Minus axon: " + String.valueOf(branchLengths[1]) + " " + resolutionUnit
                + "\n" + "#-----------------------------------------------------------------\n" + "# Begin SWC Coordinates\n";

        fw.append(header);

        int counter = 1;

        for (int i = 0; i < swcCoordinates.size(); i++) {
            final ArrayList<float[]> fil = swcCoordinates.get(i);
            final String message = messages.get(i);
            fw.append(message);

            for (int j = 0; j < fil.size(); j++, counter++) {
                fw.append(formatSWCLine(counter, fil.get(j)));
            }
        }

        fw.close();

        return output;
    }

    @Override
    public void windowClosed(final WindowEvent e) {
        append("Canceling...", PlugInDialog3DSWCStats.BLACK_TEXT);
        append("-----------------------------------------", PlugInDialog3DSWCStats.BLACK_TEXT);
        setCompleted(true);
        notifyListeners(this);
    }

    @Override
    public void windowOpened(final WindowEvent e) {}

    @Override
    public void windowClosing(final WindowEvent e) {}

    @Override
    public void windowIconified(final WindowEvent e) {}

    @Override
    public void windowDeiconified(final WindowEvent e) {}

    @Override
    public void windowActivated(final WindowEvent e) {}

    @Override
    public void windowDeactivated(final WindowEvent e) {}

}
