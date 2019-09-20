import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.file.FileIO;

import java.awt.Color;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.util.*;
import java.util.Map.Entry;

import javax.swing.JTextPane;
import javax.swing.text.AttributeSet;
import javax.swing.text.BadLocationException;
import javax.swing.text.Document;
import javax.swing.text.StyleConstants;


/**
 * New plugin for Akanni Clarke of the Giniger Lab. Conceptually similar to the DrosophilaCreateSWC plugin written by
 * Nish Pandya, this one als o includes keeping track of statistics. These statistics are: branch order, number, and
 * length as well as the distance along the axon/parent that the branch originates from.
 * 
 * This class isn't being used currently as it was superceded by PlugInAlgorithm3DSWCViewer
 * 
 * @author wangvg
 * 
 */
public class PlugInAlgorithm3DSWCStats extends AlgorithmBase {

    private ArrayList<ArrayList<float[]>> swcCoordinates;

    private final File surfaceFile;

    private final String resolutionUnit;

    private final JTextPane textArea;

    private boolean disconnected;

    private boolean axonUseLength;

    private boolean saveData;

    private final String imageFile;

    public PlugInAlgorithm3DSWCStats(final String imFile, final File surface, final String units, final JTextPane textBox) {
        super();

        imageFile = imFile;

        surfaceFile = surface;
        resolutionUnit = units;
        textArea = textBox;
        saveData = true;

        swcCoordinates = new ArrayList<ArrayList<float[]>>();
    }

    @Override
    public void runAlgorithm() {
        boolean allGood = true;

        try {
            try {
                if (textArea != null) {
                    textArea.getDocument().remove(0, textArea.getDocument().getLength());
                }
            } catch (final BadLocationException e) {}

            swcCoordinates.clear();

            append("Reading " + surfaceFile.getName(), PlugInDialog3DSWCStats.BLACK_TEXT);
            readSurfaceFile(surfaceFile);

            disconnected = false;

            // Attempt to make connections between all of the filaments
            ArrayList<ArrayList<Integer>> forward = makeConnections();

            for (int i = 1; i < swcCoordinates.size(); i++) {
                final ArrayList<float[]> fil = swcCoordinates.get(i);
                if (fil.get(0)[4] == Float.NEGATIVE_INFINITY) {
                    // No connection was made, something is wrong
                    disconnected = true;
                    break;
                }
            }
            if (disconnected) {
                // Try version with tolerance because in some cases the
                // filaments are not always end to end
                forward = makeConnectionsTol();
                // Test out one more time
                for (int i = 1; i < swcCoordinates.size(); i++) {
                    final ArrayList<float[]> fil = swcCoordinates.get(i);
                    if (fil.get(0)[4] == Float.NEGATIVE_INFINITY) {
                        // No connection was made, something is wrong
                        append(surfaceFile.getName() + " is not connected properly.", PlugInDialog3DSWCStats.RED_TEXT);
                        allGood = false;
                    }
                }
            }

            calculateDistances();
            int maxOrder;
            if (axonUseLength) {
                maxOrder = determineOrder_useLength(forward);
            } else {
                maxOrder = determineOrder(forward);
            }

            // Arrange all continuous branches of a single order into single
            // filaments as oppossed to several connected branches
            final ArrayList<String> messages = consolidateFilaments(forward, maxOrder);
            final float[] branchLengths = recalculateDistances();
            addToMessages(messages);
            append("Opening image " + imageFile, PlugInDialog3DSWCStats.BLACK_TEXT);
            final FileIO reader = new FileIO();
            srcImage = reader.readImage(imageFile);

            if (saveData) {

                try {
                    final String output = exportStatsToCSV(surfaceFile, messages, branchLengths, maxOrder);
                    append("Exported stats to CSV -> " + output, PlugInDialog3DSWCStats.BLACK_TEXT);
                } catch (final IOException e) {
                    append("Could not export stats to CSV for " + surfaceFile.getName(), PlugInDialog3DSWCStats.RED_TEXT);
                    allGood = false;
                }
                try {
                    final String output = writeSWC(surfaceFile, messages, branchLengths);
                    append("Converted to SWC -> " + output, PlugInDialog3DSWCStats.BLACK_TEXT);
                } catch (final IOException e) {
                    append("Could not write SWC for " + surfaceFile.getName(), PlugInDialog3DSWCStats.RED_TEXT);
                    allGood = false;
                }
            }

            if (allGood) {
                append("All conversions completed.", PlugInDialog3DSWCStats.GREEN_TEXT);
            } else {
                append("Some conversions failed to complete.", PlugInDialog3DSWCStats.RED_TEXT);
            }

        } catch (final Exception e) {
            append("The following Java error has occured:", PlugInDialog3DSWCStats.RED_TEXT);
            append(e.toString(), PlugInDialog3DSWCStats.RED_TEXT);
            for (final StackTraceElement t : e.getStackTrace()) {
                append(t.toString(), PlugInDialog3DSWCStats.RED_TEXT);
            }
            allGood = false;
        }
        setCompleted(allGood);
    }

    public void useAxonLength(final boolean useLength) {
        axonUseLength = useLength;
    }

    public void setSaveData(final boolean save) {
        saveData = save;
    }

    public ArrayList<ArrayList<float[]>> getFilaments() {
        return swcCoordinates;
    }

    private void append(final String message, final AttributeSet a) {

        if (textArea == null) {
            if (a.getAttribute(StyleConstants.Foreground) == Color.red.darker()) {
                System.err.println(message);
            } else {
                System.out.println(message);
            }
        } else {
            final Document doc = textArea.getDocument();
            try {
                doc.insertString(doc.getLength(), message + "\n", a);
            } catch (final BadLocationException e) {
                e.printStackTrace();
            }

            textArea.setCaretPosition(doc.getLength());
        }
    }

    /**
     * Determines the length of each individual filament read from the Imaris trace. These will be used to determine the
     * axon filaments.
     */
    private void calculateDistances() {
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
     * Adds the branch length and distance along the axon/parent to the output messages.
     * 
     * @param messages
     */
    private void addToMessages(final ArrayList<String> messages) {
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

    private String exportStatsToCSV(final File file, final ArrayList<String> messages, final float[] branchLengths, final int maxOrder) throws IOException {
        final String parent = file.getParent();
        String name = file.getName();
        name = name.substring(0, name.lastIndexOf("."));
        final String output = parent + File.separator + name + "_stats.csv";
        final File outputFile = new File(output);

        final FileWriter fw = new FileWriter(outputFile);

        fw.append("Units," + resolutionUnit + "\n");

        final PlugInAlgorithmSWCVolume alg = new PlugInAlgorithmSWCVolume(srcImage, swcCoordinates);
        alg.run();
        fw.append("Neuron volume," + alg.getVolume() + "\n");
        // fw.append("Convex hull volume," + convexHullVolume + "\n");
        writeBranchInformation(fw, maxOrder);

        /*
         * String branchInfo = ""; branchInfo += "Total branch length," + String.valueOf(branchLengths[0]) + "\n";
         * branchInfo += "Minus axon," + String.valueOf(branchLengths[1]) + "\n\n";
         * 
         * fw.append(branchInfo);
         */

        final String header = "Branch Number, Branch Order, Branch Length, Length along parent \n";

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
        }

        fw.close();

        return output;
    }

    /**
     * Recalculate distances for the consolidated branches so that it also includes the branch length and distance along
     * the axon/parent this branch originates from.
     */
    private float[] recalculateDistances() {
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
     * Build both forward and backwards connections based on the coordinates read from the Imaris trace. The backwards
     * connection routine is taken from the Drosophila Registration algorithm written by Nish Pandya.
     * 
     * @return
     */
    private ArrayList<ArrayList<Integer>> makeConnections() {

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
    private ArrayList<ArrayList<Integer>> makeConnectionsTol() {

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

    private String formatSWCLine(final int lineNum, final float[] line) {
        final String format = "%d %d %4.5f %4.5f %4.5f %4.2f %d \n";
        int type;
        if (line[5] == 1.0F) {
            type = 2;
        } else {
            type = 3;
        }

        return String.format(format, lineNum, type, line[0], line[1], line[2], line[6], (int) line[4]);
    }

    private String writeSWC(final File file, final ArrayList<String> messages, final float[] branchLengths) throws IOException {
        final String parent = file.getParent();
        String name = file.getName();
        name = name.substring(0, name.lastIndexOf("."));
        final String output = parent + File.separator + name + ".swc";
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

    /**
     * Consolidate the multitude of filaments from the input to make stat tracking easier. Also organizes the output SWC
     * so that branch numbers are logical and go from beggining of the axon to the end of the axon.
     * 
     * @param connections
     * @param maxOrder
     * @return
     */
    private ArrayList<String> consolidateFilaments(final ArrayList<ArrayList<Integer>> connections, final int maxOrder) {

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

        swcCoordinates = newFilaments;

        return messages;
    }

    /**
     * Imaris filament files are patterned in a way that branch organization can be inferred from it. Simplified the
     * algorithm to determine the axon/branch number so that it is based on the pattern seen in the files.
     * 
     * Basically take the last step of the three pass method earlier since in forward connections, the lower number is
     * the one that goes towards the axon and others are child branches.
     */
    private int determineOrder(final ArrayList<ArrayList<Integer>> connections) {

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

    private int determineOrder_useLength(final ArrayList<ArrayList<Integer>> connections) {

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

    private void writeBranchInformation(final FileWriter fw, final int maxOrder) throws IOException {

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

        fw.append("Branch lengths\n");
        fw.append("Total Branches," + String.valueOf(allBranches) + "\n");
        fw.append("Higher order," + String.valueOf(higherOrder) + "\n\n");

        for (int i = 1; i < maxOrder; i++) {
            fw.append("Order " + String.valueOf(i) + "," + String.valueOf(lengths[i]) + "\n");
        }

    }

    /**
     * Reads surface file. Taken from drosophila registration dialog written by Nish Pandya.
     * 
     * @param surfaceFile
     * @return
     */
    private boolean readSurfaceFile(final File surfaceFile) {
        final boolean success = true;
        RandomAccessFile raFile = null;
        try {

            raFile = new RandomAccessFile(surfaceFile, "r");

            String line;

            while ( (line = raFile.readLine()) != null) {
                line = line.trim();
                if (line.startsWith("Translate1Dragger")) {
                    break;
                }
                if (line.contains("Coordinate3")) {
                    final ArrayList<float[]> filamentCoords = new ArrayList<float[]>();
                    while ( ! ( (line = raFile.readLine()).endsWith("}"))) {
                        line = line.trim();
                        if ( !line.equals("")) {
                            if (line.startsWith("point [")) {
                                line = line.substring(line.indexOf("point [") + 7, line.length()).trim();
                                if (line.equals("")) {
                                    continue;
                                }
                            }
                            if (line.endsWith("]")) {
                                line = line.substring(0, line.indexOf("]")).trim();
                                if (line.equals("")) {
                                    continue;
                                }
                            }
                            if (line.endsWith(",")) {
                                line = line.substring(0, line.indexOf(",")).trim();
                                if (line.equals("")) {
                                    continue;
                                }
                            }
                            final String[] splits = line.split("\\s+");
                            splits[0] = splits[0].trim();
                            splits[1] = splits[1].trim();
                            splits[2] = splits[2].trim();
                            final float coord_x = new Float(splits[0]).floatValue();
                            final float coord_y = new Float(splits[1]).floatValue();
                            final float coord_z = new Float(splits[2]).floatValue();

                            /**
                             * Changing from previous versions. Order is now: X, Y, Z coordinates (0, 1, 2) Distance (3)
                             * Backwards connection (4) Branch order (5) Radius (6)
                             */
                            final float[] coords = {coord_x, coord_y, coord_z, 0, Float.NEGATIVE_INFINITY, 0, -1.0f};

                            filamentCoords.add(coords);
                        }
                    }
                    swcCoordinates.add(filamentCoords);
                }
            }
            raFile.close();
        } catch (final Exception e) {
            try {
                if (raFile != null) {
                    raFile.close();
                }
            } catch (final Exception ex) {

            }
            e.printStackTrace();
            return false;
        }

        return success;
    }

}
