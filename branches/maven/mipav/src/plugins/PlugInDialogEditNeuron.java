import gov.nih.mipav.model.algorithms.filters.AlgorithmMean;
import gov.nih.mipav.model.algorithms.utilities.AlgorithmExtractSlices;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase.Unit;
import gov.nih.mipav.model.file.FileInfoBase.UnitType;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIBase;
import gov.nih.mipav.model.structures.VOIBaseVector;
import gov.nih.mipav.model.structures.VOIContour;
import gov.nih.mipav.model.structures.VOIPoint;
import gov.nih.mipav.plugins.JDialogStandalonePlugin;
import gov.nih.mipav.view.JFrameHistogram;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewJComponentEditImage;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.components.PanelManager;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.GridLayout;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ItemEvent;
import java.awt.event.ItemListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.awt.event.WindowEvent;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.FilenameFilter;
import java.io.IOException;
import java.text.ParseException;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.PriorityQueue;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JSpinner;
import javax.swing.JTextField;
import javax.swing.SpinnerNumberModel;
import javax.swing.WindowConstants;

import WildMagic.LibFoundation.Mathematics.Vector3f;


public class PlugInDialogEditNeuron extends JDialogStandalonePlugin implements MouseListener, MouseMotionListener, ItemListener {

    /**
	 * 
	 */
    private static final long serialVersionUID = 2685558849611300514L;

    /**
     * Slice in the subvolume that is considered the "center" which means which slice is editable
     */
    private int activeSlice;

    private JRadioButton addRB;

    private JCheckBox aviBox;

    /**
     * VOI containing the points to edit the neuron traces
     */
    private VOI controlPts;

    private JCheckBox csvBox;

    /**
     * The active slice's actual depth in the full volume
     */
    private int currentSlice;

    private JRadioButton deleteRB;

    private JCheckBox deleteBox;

    /**
     * Depth of the current subvolume
     */
    private int depth;

    private JTextField dirText;

    /**
     * These two buttons choose whether the editor displays the polygon or the control points
     */
    private JRadioButton editPolyRB;

    private JRadioButton editTraceRB;

    private JFileChooser fileChooser;

    private int height;

    private final ArrayList<File> images;

    private int[] imBuffer;

    private JLabel imName;

    /**
     * Used to keep track of which node to delete
     */
    private VOIBase lastActive;

    private int length;

    /**
     * List to keep track of which nodes are connected in the active slice
     */
    private Linking links;

    /**
     * Store LUT so it is the same between images in the series
     */
    private ModelLUT lut;

    /**
     * Neuron traces are overlayed on the subvolume
     */
    private BitSet mask;

    private JCheckBox noiseBox;

    private int numImages;

    /**
     * Keeps track of the original start of the neuron
     */
    private Point origin;

    private JRadioButton originRB;

    private VOIPoint originVOI;

    /**
     * Keeps track of the paths in the active slice so that they can be quickly moved/deleted/split
     */
    private LineList paths;

    private VOI polyVOI;

    /**
     * Which slice was the previous active slice. Should only be a difference of 1, but if an image list is implemented
     * later, could be differet
     */
    private int prevSlice;

    /**
     * Keeps track of which VOI has been force labeled as the primary branch tip
     */
    private Point primaryPt;

    private JRadioButton primaryRB;

    private VOIPoint primaryVOI;

    /**
     * Keeps track of which VOI has been force labeled as the progenitor axon tip
     */
    private Point progenitorPt;

    private JRadioButton progenitorRB;

    private VOIPoint progenitorVOI;

    private JCheckBox saveBox;

    private int sliceRange;

    /**
     * Keeps track of which VOI is considered the split point between the growth cone and the axon
     */
    private JRadioButton splitRB;

    private Point splitPt;

    private VOIPoint splitVOI;

    private JSpinner rangeField;

    /**
     * Default resolution right now, in um
     */
    private double resolution = 0.211;

    @SuppressWarnings("rawtypes")
    private JComboBox resUnits;

    private FileWriter statsCSV;

    /**
     * Part of the 3D image to display on screen
     */
    private ModelImage subVolume;

    /**
     * The associated frame for the subvolume image
     */
    private ViewJFrameImage subVolumeFrame;

    private final ArrayList<File> swcList;

    /**
     * Which node we are changing when a move action occurs
     */
    private Point toChange;

    private int width;

    private JTextField xResField;

    private ModelImage imStack = null;

    private JCheckBox disableBox;

    private JCheckBox hideVOIBox;

    private boolean addingBranch = false;

    private VOIPoint newBranchVOI;

    private Point lastSpot;

    private boolean blockClick = false;

    private Point originalMovePt;

    /**
     * Primary constructor. Initializes a dialog to ask the user for a directory to use
     */
    public PlugInDialogEditNeuron() {
        super();

        images = new ArrayList<File>();
        swcList = new ArrayList<File>();

        currentSlice = 0;
        prevSlice = 0;

        init();

    }

    @Override
    public void actionPerformed(final ActionEvent event) {
        final String command = event.getActionCommand();

        if (command.equals("ApproveSelection")) {
            dirText.setText(fileChooser.getSelectedFile().toString());
            Preferences.setImageDirectory(fileChooser.getSelectedFile());
        } else if (command.equals("Cancel")) {
            if (isExitRequired()) {
                ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
            } else {
                dispose();
            }
        } else if (command.equals("Choose")) {
            chooseDir();
        } else if (command.equals("OK")) {
            final File directory = new File(dirText.getText());
            if ( !directory.exists()) {
                MipavUtil.displayError("File does not exist");
                return;
            }
            try {
                rangeField.commitEdit();
                final Object val = rangeField.getValue();
                if (val instanceof Integer) {
                    sliceRange = (Integer) rangeField.getValue();
                } else {
                    throw new NumberFormatException();
                }
            } catch (final NumberFormatException e) {
                MipavUtil.displayError("Range is not an integer");
                return;
            } catch (final ParseException e) {
                MipavUtil.displayError("Could not read range value");
                e.printStackTrace();
            }
            if (populateImages(directory)) {
                if (initVars()) {
                    initEditor();
                    openImage();
                }
            } else {
                MipavUtil.displayError("No compatible SWC files were found");
            }
        } else if (command.equals("Next")) {
            prevSlice = currentSlice;
            currentSlice++;
            lut = subVolumeFrame.getLUTa();
            final float zX = subVolumeFrame.getComponentImage().getZoomX();
            final float zY = subVolumeFrame.getComponentImage().getZoomY();

            if (currentSlice < numImages) {
                final Point loc = subVolumeFrame.getLocation();
                final Dimension dimSize = subVolumeFrame.getSize();
                subVolumeFrame.close();
                openImage();
                subVolumeFrame.setLocation(loc);
                subVolumeFrame.getComponentImage().setZoom(zX, zY);
                subVolumeFrame.setSize(dimSize);
                subVolumeFrame.updateImages();
            } else {
                actionPerformed(new ActionEvent(this, 0, "End"));
            }
        } else if (command.equals("Prev")) {
            if (currentSlice > 0) {
                final float zX = subVolumeFrame.getComponentImage().getZoomX();
                final float zY = subVolumeFrame.getComponentImage().getZoomY();
                lut = subVolumeFrame.getLUTa();
                prevSlice = currentSlice;
                currentSlice--;
                final Point loc = subVolumeFrame.getLocation();
                final Dimension dimSize = subVolumeFrame.getSize();
                subVolumeFrame.close();
                openImage();
                subVolumeFrame.setLocation(loc);
                subVolumeFrame.getComponentImage().setZoom(zX, zY);
                subVolumeFrame.setSize(dimSize);
                subVolumeFrame.updateImages();
            }
        } else if (command.equals("Reset")) {
            subVolume.unregisterAllVOIs();
            try {
                readSWC(Math.max(0, currentSlice - sliceRange));
            } catch (final IOException e) {
                MipavUtil.displayError("Could not read SWC file(s)");
                e.printStackTrace();
            }
        } else if (command.equals("LUT")) {
            openHisto();
        } else if (command.equals("Save")) {
            try {
                saveNewSWC();
            } catch (final IOException e) {
                e.printStackTrace();
            }
        } else if (command.equals("End")) {
            try {
                statsCSV.close();
            } catch (final IOException e1) {
                MipavUtil.displayError("Could not close CSV output");
                e1.printStackTrace();
                return;
            }
            if (aviBox.isSelected()) {
                String dir = dirText.getText();
                if ( !dir.endsWith(File.separator)) {
                    dir += File.separator;
                }
                dir += "Branch_Images" + File.separator;
                new PlugInDialogSaveTraceAsAVI(dir);
                subVolumeFrame.close();
                if (imStack != null) {
                    imStack.disposeLocal();
                }
                dispose();
            } else {

                if (isExitRequired()) {
                    System.exit(0);
                    ViewUserInterface.getReference().windowClosing(new WindowEvent(this, WindowEvent.WINDOW_CLOSING));
                } else {
                    subVolumeFrame.close();
                    if (imStack != null) {
                        imStack.disposeLocal();
                    }
                    dispose();
                }
            }
        } else if (command.equals("Zoom In")) {
            float zX = subVolumeFrame.getComponentImage().getZoomX();
            float zY = subVolumeFrame.getComponentImage().getZoomY();
            if (zX <= 1) {
                zX *= 2f;
                zY *= 2f;
            } else {
                zX += 1f;
                zY += 1f;
            }
            subVolumeFrame.getComponentImage().setZoom(zX, zY);
            subVolumeFrame.updateFrame(zX, zY);
            subVolumeFrame.updateImages();
        } else if (command.equals("Zoom Out")) {
            float zX = subVolumeFrame.getComponentImage().getZoomX();
            float zY = subVolumeFrame.getComponentImage().getZoomY();
            if (zX <= 1) {
                zX *= 0.5f;
                zY *= 0.5;
            } else {
                zX -= 1f;
                zY -= 1f;
            }
            subVolumeFrame.getComponentImage().setZoom(zX, zY);
            subVolumeFrame.updateFrame(zX, zY);
            subVolumeFrame.updateImages();
        } else if (command.equals("Edit Trace")) {
            // Remove polygon, replace with control points
            subVolume.unregisterAllVOIs();
            if ( !hideVOIBox.isSelected()) {
                subVolume.registerVOI(controlPts);
                subVolume.notifyImageDisplayListeners();
            }

            addRB.setEnabled(true);
            deleteRB.setEnabled(true);
            progenitorRB.setEnabled(true);
            splitRB.setEnabled(true);
            originRB.setEnabled(true);
            primaryRB.setEnabled(true);

        } else if (command.equals("Edit Polygon")) {
            // Remove control points, replace with polygon
            if (polyVOI.getSize() == 0) {
                polygonDisplay();
            }
            subVolume.unregisterAllVOIs();
            if ( !hideVOIBox.isSelected()) {
                subVolume.registerVOI(polyVOI);
                subVolume.notifyImageDisplayListeners();
            }

            addRB.setEnabled(false);
            deleteRB.setEnabled(false);
            progenitorRB.setEnabled(false);
            splitRB.setEnabled(false);
            originRB.setEnabled(false);
            primaryRB.setEnabled(false);
        }
    }

    private boolean adjustPolygon(final Point p1, final Point p2) {
        // find if line between p1 and p2 intersects any lines
        // of the polygonal VOI
        // p1 should be the one being adjusted, p2 its connected node
        final VOIBaseVector vbv = polyVOI.getCurves();
        final VOIBase curve = vbv.get(0);
        if (curve instanceof VOIContour) {
            for (int i = 0; i < curve.size(); i++) {
                final Vector3f v1 = curve.get(i);
                final int next = i + 1 < curve.size() ? i + 1 : 0;
                final Vector3f v2 = curve.get(next);
                final Point p3 = new Point((int) v1.X, (int) v1.Y);
                final Point p4 = new Point((int) v2.X, (int) v2.Y);
                if (intersects(p1, p2, p3, p4)) {
                    final Vector3f newVec = new Vector3f(p1.x, p1.y, v1.Z);
                    if (next == 0) {
                        curve.add(newVec);
                    } else {
                        curve.add(i + 1, newVec);
                    }
                    return true;
                }
            }
        }
        return false;

    }

    private boolean intersects(final Point p1, final Point p2, final Point p3, final Point p4) {
        // p1 and p2 are for the branch, p3 and p4 for the polygon VOI
        final int x1 = p1.x;
        final int y1 = p1.y;
        final int x2 = p2.x;
        final int y2 = p2.y;
        final int x3 = p3.x;
        final int y3 = p3.y;
        final int x4 = p4.x;
        final int y4 = p4.y;

        final float denom = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4);

        if (denom != 0) {
            final float numx = (x1 * y2 - y1 * x2) * (x3 - x4) - (x1 - x2) * (x3 * y4 - y3 * x4);
            final float numy = (x1 * y2 - y1 * x2) * (y3 - y4) - (y1 - y2) * (x3 * y4 - y3 * x4);
            final float intx = numx / denom;
            final float inty = numy / denom;
            final int xmin = Math.min(x1, x2);
            final int ymin = Math.min(y1, y2);
            final int xmax = Math.max(x1, x2);
            final int ymax = Math.max(y1, y2);
            final int xmin2 = Math.min(x3, x4);
            final int ymin2 = Math.min(y3, y4);
            final int xmax2 = Math.max(x3, x4);
            final int ymax2 = Math.max(y3, y4);
            if (xmin < intx && intx < xmax && ymin < inty && inty < ymax && xmin2 < intx && intx < xmax2 && ymin2 < inty && inty < ymax2) {
                return true;
            }
        }
        return false;
    }

    /**
     * Modified DFS to check for if a cycle in the neuron exists, which will cause a stack overflow in the neuron length
     * portion
     * 
     * @return
     */
    private boolean checkCycles() {

        final LinkElement originEle = links.get(origin);
        ArrayList<LinkElement> list = originEle.linked;
        final HashSet<LinkElement> visited = new HashSet<LinkElement>();
        final ArrayDeque<LinkElement> stack = new ArrayDeque<LinkElement>();
        final ArrayDeque<LinkElement> prevEle = new ArrayDeque<LinkElement>();
        for (int i = 0; i < list.size(); i++) {
            prevEle.add(originEle);
        }

        stack.addAll(list);
        visited.addAll(list);

        while ( !stack.isEmpty()) {
            final LinkElement e = stack.poll();
            final LinkElement prev = prevEle.poll();
            list = e.copyList();
            list.remove(prev);
            for (final LinkElement l : list) {
                if (visited.contains(l)) {
                    return true;
                }
                stack.add(l);
                prevEle.add(e);
            }
        }

        return false;
    }

    /**
     * Bresenham line algorithm used to draw the paths between two points. Uses the points taken from the SWC files to
     * draw the paths in the mask to overlay on the subvolume image.
     * 
     * @param x0
     * @param y0
     * @param x1
     * @param y1
     * @param z
     */
    private void bresenham(int x0, int y0, final int x1, final int y1, final int z) {
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

        int i;

        while (true) {
            i = x0 + y0 * width;
            mask.set(i + z * length);

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
    }

    /**
     * Bresenham line algorithm used to draw the paths between two points. This version is used with the subclasses to
     * keep track of which lines are displayed in the active slice to make it easier to add nodes between points
     * 
     * @param p0
     * @param p1
     * @return
     */
    private LinkedHashSet<Point> bresenham(final Point p0, final Point p1) {

        int x0 = p0.x;
        final int x1 = p1.x;
        int y0 = p0.y;
        final int y1 = p1.y;

        final LinkedHashSet<Point> pts = new LinkedHashSet<Point>();
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

        int i;

        while (true) {
            pts.add(new Point(x0, y0));
            i = x0 + y0 * width;

            mask.set(i + activeSlice * length);

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
     * Recursive function to traverse the entire linked list as a means for assigning branch tips a length and order. At
     * each branch point, the longest branch is allowed to continue while the shorter branch no longer is accounted for
     * and thus has its order incremented. The increment occurs through the entire shorter branch, which requires an
     * update of all child branches.
     * 
     * The exception is in the case of branches labeled as progenitor or primary. The primary branch will always take
     * priority, with the progrenitor taking second priority.
     * 
     * @param e
     * @param prev
     * @param length
     * @return
     * @throws IOException
     */
    private PriorityQueue<NeuronLength> calcLengths(final LinkElement e, final LinkElement prev, final double length) throws IOException {
        LinkElement l;
        final PriorityQueue<NeuronLength> nList = new PriorityQueue<NeuronLength>();
        NeuronLength nl;

        if (e.linked.size() == 1 && prev != null) {
            nl = new NeuronLength(e.pt, length);
            nList.add(nl);
            return nList;
        } else if (e.linked.size() == 2 && prev != null) {
            l = e.linked.get(0) == prev ? e.linked.get(1) : e.linked.get(0);
            return calcLengths(l, e, length + e.pt.distance(l.pt));
        } else {
            final ArrayList<LinkElement> list = e.copyList();
            PriorityQueue<NeuronLength> result = new PriorityQueue<NeuronLength>();
            final ArrayList<PriorityQueue<NeuronLength>> listQ = new ArrayList<PriorityQueue<NeuronLength>>();
            if (prev != null) {
                list.remove(prev);
            }
            final PriorityQueue<NeuronLength> pq = new PriorityQueue<NeuronLength>();
            for (int i = 0; i < list.size(); i++) {
                l = list.get(i);
                result = calcLengths(l, e, e.pt.distance(l.pt));
                nList.addAll(result);
                listQ.add(result);
                pq.add(result.peek());
            }
            nl = pq.peek();

            nl.length += length;
            for (int i = 0; i < list.size(); i++) {
                final PriorityQueue<NeuronLength> temp = listQ.get(i);
                if ( !temp.contains(nl)) {
                    final Iterator<NeuronLength> iter = temp.iterator();
                    while (iter.hasNext()) {
                        final NeuronLength n = iter.next();
                        n.order++;
                    }
                }
            }

            return nList;
        }
    }

    /**
     * Takes the polygonal area that has been edited to determine both the centroid and the area of the polygonal
     * region.
     * 
     * @return centroid and area in an array. Element 0 is x coordinate, Element 1 is y coordinate, and Element 2 is the
     *         area.
     */
    private double[] calcPolyArea() {

        final double[] output = new double[3];

        int xi, yi;

        final BitSet polyMask = polyVOI.createBinaryMask(width, height, activeSlice);
        int sumX = 0;
        int sumY = 0;
        int area = 0;
        for (int i = polyMask.nextSetBit(0); i >= 0; i = polyMask.nextSetBit(i + 1)) {
            xi = i % width;
            yi = i / width;
            sumX += xi;
            sumY += yi;
            area++;
        }

        output[0] = (double) sumX / (double) area;
        output[1] = (double) sumY / (double) area;
        output[2] = area * Math.pow(resolution, 2);

        return output;

    }

    /**
     * Opens the file chooser dialog to provide a directory in which to look for the images and SWC files
     */
    private void chooseDir() {
        final String dirText = Preferences.getImageDirectory();
        fileChooser = new JFileChooser(dirText);
        fileChooser.setFileSelectionMode(JFileChooser.FILES_AND_DIRECTORIES);
        fileChooser.addActionListener(this);
        fileChooser.showOpenDialog(this);
    }

    /**
     * Depth-first search for the origin (from the split point) using a recursive implementation.
     * 
     * @param e
     * @param prev
     * @return
     */
    /*
     * private boolean dfsOrigin2(LinkElement e, LinkElement prev){ if(e.pt.equals(origin)) return true;
     * 
     * ArrayList<LinkElement> list = e.copyList(); list.remove(prev); if(list.size() > 0){ for(int
     * i=0;i<list.size();i++){ if(dfsOrigin(list.get(i), e)) return true; } } return false; }
     */

    /**
     * Depth-first search for the origin (from the split point) to prevent loops from throwing off the program
     * 
     * @param e
     * @param prev
     * @return
     */
    private boolean dfsOrigin(final LinkElement e, final LinkElement prev) {
        if (e.pt.equals(origin)) {
            return true;
        }

        final ArrayDeque<LinkElement> stack = new ArrayDeque<LinkElement>();
        final HashSet<LinkElement> visited = new HashSet<LinkElement>();
        ArrayList<LinkElement> pts = e.linked;
        visited.add(prev);

        for (int i = 0; i < pts.size(); i++) {
            final LinkElement ele = pts.get(i);
            if (visited.contains(ele)) {
                continue;
            }
            if (ele.pt.equals(origin)) {
                return true;
            }
            stack.addFirst(ele);
            visited.add(ele);
        }

        while ( !stack.isEmpty()) {
            final LinkElement ele = stack.poll();
            pts = ele.linked;
            for (int i = 0; i < pts.size(); i++) {
                final LinkElement ele2 = pts.get(i);
                if (visited.contains(ele2)) {
                    continue;
                }
                if (ele2.pt.equals(origin)) {
                    return true;
                }
                stack.addFirst(ele2);
                visited.add(ele2);
            }
        }

        return false;
    }

    /**
     * Crude filter to get rid of shot noise resulting from the max-projected neuron images. It implements a mean
     * filter, but only adjusts pixels that are changing over a certain threshold. The mean filter is then repeated
     * locally around points that were changed until there are no longer any differences greater than the given
     * threshold.
     * 
     * For 8-bit integer images, the threshold is set to 3, while for 16-bit integer images the threshold is 66. This
     * does not work with float and double valued images right now.
     * 
     * This filter runs in place.
     * 
     * @param image input image to filter. Is also the result image as this filter places the results back into the
     *            original image
     */

    private int[] filterShotNoiseMean(final ModelImage image) {
        final int dataType = image.getType();
        int maxDiff;

        if (dataType == ModelImage.BYTE || dataType == ModelImage.UBYTE) {
            maxDiff = 3;
        } else if (dataType == ModelImage.SHORT || dataType == ModelImage.USHORT) {
            maxDiff = 600;
        } else {
            return null;
        }

        final ModelImage meanImage = (ModelImage) image.clone();
        AlgorithmMean mean;
        if (imStack == null) {
            mean = new AlgorithmMean(meanImage, 3, true);
        } else {
            mean = new AlgorithmMean(meanImage, 3, true, true);
        }
        mean.run();
        int length = width * height;
        if (imStack != null) {
            length *= numImages;
        }
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

        final ArrayList<Integer> adjustPts = new ArrayList<Integer>();
        final ArrayList<Integer> addPts = new ArrayList<Integer>();
        for (int i = 0; i < length; i++) {
            diff = Math.abs(buffer[i] - medBuffer[i]);
            if (diff >= maxDiff) {
                // adjustPts.add(i);
                buffer[i] = medBuffer[i];
                final int x = i % width;
                int y = i / width;
                if (imStack != null) {
                    y = y % numImages;
                }
                final int z = imStack == null ? 0 : i / (width * height);
                for (int nx = x - 1; nx <= x + 1; nx++) {
                    if (nx < 0 || nx >= width) {
                        continue;
                    }
                    for (int ny = y - 1; ny <= y + 1; ny++) {
                        if (ny < 0 || ny >= height) {
                            continue;
                        }
                        final int ind = nx + ny * width + z * (width * height);
                        if ( !adjustPts.contains(ind)) {
                            adjustPts.add(ind);
                        }
                    }
                }
            }
        }

        medBuffer = null;

        System.arraycopy(buffer, 0, outBuffer, 0, length);

        while (adjustPts.size() > 0) {
            final int size = adjustPts.size();
            for (int j = 0; j < size; j++) {
                final int i = adjustPts.get(j);
                final int x = i % width;
                int y = i / width;
                if (imStack != null) {
                    y = y % numImages;
                }
                final int z = imStack == null ? 0 : i / (width * height);
                final int kMed = findMean(buffer, i);
                if (Math.abs(buffer[i] - kMed) >= maxDiff) {
                    outBuffer[i] = kMed;
                    // adjustPts.add(i);
                    for (int nx = x - 1; nx <= x + 1; nx++) {
                        if (nx < 0 || nx >= width) {
                            continue;
                        }
                        for (int ny = y - 1; ny <= y + 1; ny++) {
                            if (ny < 0 || ny >= height) {
                                continue;
                            }
                            final int ind = nx + ny * width + z * (width * height);
                            if ( !addPts.contains(ind)) {
                                addPts.add(ind);
                            }
                        }
                    }
                }
            }
            for (int j = 0; j < size; j++) {
                final int i = adjustPts.remove(0);
                buffer[i] = outBuffer[i];
            }
            adjustPts.addAll(addPts);
            addPts.clear();
        }

        meanImage.disposeLocal();
        return outBuffer;
    }

    private int findMean(final int[] buffer, final int i) {
        final int x = i % width;
        int y = i / width;
        if (imStack != null) {
            y = y % numImages;
        }
        final int z = imStack == null ? 0 : i / (width * height);
        final int kWidth = Math.min(3, 2 + Math.min(x, width - 1 - x));
        final int kHeight = Math.min(3, 2 + Math.min(y, height - 1 - y));
        final int cnt = kWidth * kHeight;
        int sum = 0;

        for (int nx = x - 1; nx <= x + 1; nx++) {
            if (nx < 0 || nx >= width) {
                continue;
            }
            for (int ny = y - 1; ny <= y + 1; ny++) {
                if (ny < 0 || ny >= height) {
                    continue;
                }
                sum += buffer[nx + ny * width + z * width * height];
            }
        }
        final int kMean = (int) ((float) sum / (float) cnt);
        return kMean;
    }

    /**
     * Used with the primary constructor to allow the user to choose the directory where the images and SWC files are
     */
    private void init() {

        getContentPane().removeAll();

        setForeground(Color.black);
        setTitle("Neuron Segmentation: Edit SWC");

        final JPanel dirPanel = new JPanel();
        dirPanel.setForeground(Color.black);
        dirPanel.setBorder(buildTitledBorder("Choose Image File/Directory"));

        final String desc = "<html><b>Directions: </b><br>" + "Please choose a folder that contains neurons that have an<br>" + "accompanying SWC file</html>";

        final JLabel dirLabel = new JLabel(desc);
        dirLabel.setForeground(Color.black);
        dirLabel.setFont(serif12);
        dirPanel.add(dirLabel);
        getContentPane().add(dirPanel, BorderLayout.NORTH);

        final JPanel choosePanel = new JPanel();

        dirText = new JTextField(30);
        dirText.setText(Preferences.getImageDirectory());
        dirText.setFont(serif12);
        choosePanel.add(dirText);

        final JButton dirButton = new JButton("Choose");
        dirButton.setFont(serif12);
        dirButton.addActionListener(this);
        choosePanel.add(dirButton);

        final JPanel rangePanel = new JPanel();
        rangePanel.setForeground(Color.black);

        final JLabel rangeLabel = new JLabel("Slice Range: +/-");
        rangeLabel.setFont(serif12);
        rangePanel.add(rangeLabel);

        rangeField = new JSpinner(new SpinnerNumberModel(5, 1, 10, 1));
        rangeField.setFont(serif12);
        rangePanel.add(rangeField);

        final JPanel csvPanel = new JPanel();
        csvPanel.setForeground(Color.black);

        csvBox = new JCheckBox("Delete previous stats CSV");
        csvBox.setFont(serif12);
        csvPanel.add(csvBox);

        final JPanel noisePanel = new JPanel();
        noisePanel.setForeground(Color.black);

        noiseBox = new JCheckBox("Apply shot noise filter");
        noiseBox.setFont(serif12);
        noisePanel.add(noiseBox);

        final PanelManager manage = new PanelManager();
        manage.add(choosePanel);
        manage.addOnNextLine(rangePanel);
        manage.addOnNextLine(csvPanel);
        manage.addOnNextLine(noisePanel);
        getContentPane().add(manage.getPanel(), BorderLayout.CENTER);

        final JPanel OKCancelPanel = new JPanel();

        buildOKButton();
        OKCancelPanel.add(OKButton, BorderLayout.WEST);

        buildCancelButton();
        OKCancelPanel.add(cancelButton, BorderLayout.EAST);
        getContentPane().add(OKCancelPanel, BorderLayout.SOUTH);

        pack();
        setVisible(true);
        setResizable(false);
        System.gc();
    }

    /**
     * The main dialog of this plugin, used to control the editor portions which include adding/deleting nodes.
     */
    @SuppressWarnings({"unchecked", "rawtypes"})
    private void initEditor() {

        getContentPane().removeAll();

        final PanelManager manage = new PanelManager();
        final ButtonGroup group = new ButtonGroup();

        String parentPath = swcList.get(0).getParent();
        parentPath += File.separator + "neuron_stats.csv";
        final File statFile = new File(parentPath);
        String csvHeader = "";
        if ( !statFile.exists() || csvBox.isSelected()) {
            /*
             * csvHeader = "Image,Progenitor.x,Progenitor.y,Split.x,Split.y,Centroid.x,Centroid.y,Polygonal Area," +
             * "Split to Origin Length, Split to Primary Length,Longest Length," +
             * "Total Branch Length,Total Higher Order Branch Length,Max Branch Order\n";
             */
            csvHeader = "Timepoint,Branch #,Split to Progenitor Length,Split to Origin Length,Polygonal Area," + "Order 1 Branch Lengths";
            for (int i = 0; i < 50; i++) {
                csvHeader += ",";
            }
            csvHeader += "Order 2 Branch Lengths";
            for (int i = 0; i < 10; i++) {
                csvHeader += ",";
            }
            csvHeader += "Order 3 Branch Lengths";
            for (int i = 0; i < 5; i++) {
                csvHeader += ",";
            }
            csvHeader += ",\n";
        }
        try {
            statsCSV = new FileWriter(statFile, !csvBox.isSelected());
            statsCSV.append(csvHeader);
        } catch (final IOException e) {
            MipavUtil.displayError("Cannot open CSV file for editing");
            e.printStackTrace();
        }

        final JPanel descPanel = new JPanel();
        descPanel.setForeground(Color.black);
        descPanel.setBorder(buildTitledBorder("Editor Instructions"));

        final String descStr = "<html><b>Choose add or delete. When adding nodes to the <br>" + "trace, double click on a section between two connected<br>"
                + "nodes. When deleting nodes, double click on a node to<br>" + "remove it as a control point. To edit a node's location,<br>"
                + "drag the node to a new location. The trace will update<br>" + "once you release the mouse button.</b></html>";
        final JLabel descLabel = new JLabel(descStr);
        descLabel.setFont(serif12);
        descPanel.add(descLabel);

        getContentPane().add(descPanel, BorderLayout.NORTH);

        final JPanel labelPanel = new JPanel();
        labelPanel.setForeground(Color.black);
        labelPanel.setBorder(buildTitledBorder("Current Image"));

        imName = new JLabel("Current Image");
        imName.setFont(serif12B);
        labelPanel.add(imName);

        manage.add(labelPanel);

        final JPanel editPanel = new JPanel(new GridLayout(0, 2));
        editPanel.setForeground(Color.black);
        editPanel.setBorder(buildTitledBorder("Editor Mode"));

        editTraceRB = new JRadioButton("Edit Neuron Trace");
        editTraceRB.setFont(serif12);
        editTraceRB.setActionCommand("Edit Trace");
        editTraceRB.setSelected(true);
        editTraceRB.addActionListener(this);

        editPolyRB = new JRadioButton("Edit Polygon Area");
        editPolyRB.setFont(serif12);
        editPolyRB.setActionCommand("Edit Polygon");
        editPolyRB.addActionListener(this);

        manage.addOnNextLine(editPanel);

        final ButtonGroup editGroup = new ButtonGroup();

        editGroup.add(editTraceRB);
        editGroup.add(editPolyRB);
        editPanel.add(editTraceRB);
        editPanel.add(editPolyRB);

        final JPanel radioPanel = new JPanel(new GridLayout(0, 2));
        radioPanel.setForeground(Color.black);
        radioPanel.setBorder(buildTitledBorder("Node Editor"));

        addRB = new JRadioButton("Add Node");
        addRB.setFont(serif12);
        addRB.setSelected(true);
        radioPanel.add(addRB);
        group.add(addRB);

        deleteRB = new JRadioButton("Delete Node");
        deleteRB.setFont(serif12);
        radioPanel.add(deleteRB);
        group.add(deleteRB);

        manage.addOnNextLine(radioPanel);

        final JPanel labelRBPanel = new JPanel(new GridLayout(0, 2));
        labelRBPanel.setForeground(Color.black);
        labelRBPanel.setBorder(buildTitledBorder("Label Editor"));

        progenitorRB = new JRadioButton("Label Progenitor (P)");
        progenitorRB.setFont(serif12);
        labelRBPanel.add(progenitorRB);
        group.add(progenitorRB);

        splitRB = new JRadioButton("Label Split Point (S)");
        splitRB.setFont(serif12);
        labelRBPanel.add(splitRB);
        group.add(splitRB);

        originRB = new JRadioButton("Label Origin (O)");
        originRB.setFont(serif12);
        labelRBPanel.add(originRB);
        group.add(originRB);

        primaryRB = new JRadioButton("Label Primary Branch (1)");
        primaryRB.setFont(serif12);
        labelRBPanel.add(primaryRB);
        group.add(primaryRB);

        manage.addOnNextLine(labelRBPanel);

        final JPanel resPanel = new JPanel();
        resPanel.setForeground(Color.black);
        resPanel.setBorder(buildTitledBorder("Image Resolutions"));

        final JLabel xRes = new JLabel("Resolution: ");
        xRes.setForeground(Color.black);
        xRes.setFont(serif12);
        resPanel.add(xRes);

        xResField = new JTextField(5);
        xResField.setText(Double.toString(resolution));
        xResField.setHorizontalAlignment(JTextField.RIGHT);
        xResField.setFont(serif12);
        resPanel.add(xResField);

        final Unit[] allSame = UnitType.getUnitsOfType(UnitType.LENGTH);
        final int[] allSameMeasure = new int[allSame.length];
        for (int i = 0; i < allSameMeasure.length; i++) {
            allSameMeasure[i] = allSame[i].getLegacyNum();
        }
        final String[] unitArr = new String[allSameMeasure.length];
        for (int i = 0; i < allSameMeasure.length; i++) {
            final Unit unit = Unit.getUnitFromLegacyNum(allSameMeasure[i]);
            unitArr[i] = unit.getAbbrev();
        }
        resUnits = new JComboBox(unitArr);
        resUnits.setFont(serif12);
        resUnits.setSelectedItem("um");
        resPanel.add(resUnits);

        manage.addOnNextLine(resPanel);

        final JPanel optionPanel = new JPanel(new GridLayout(0, 3));
        optionPanel.setForeground(Color.black);
        optionPanel.setBorder(buildTitledBorder("Options"));

        saveBox = new JCheckBox("Save trace as image");
        saveBox.setFont(serif12);
        optionPanel.add(saveBox);

        deleteBox = new JCheckBox("Delete old SWC");
        deleteBox.setFont(serif12);
        optionPanel.add(deleteBox);

        aviBox = new JCheckBox("Save AVI");
        aviBox.setFont(serif12);
        optionPanel.add(aviBox);

        disableBox = new JCheckBox("Hide trace");
        disableBox.setFont(serif12);
        disableBox.addItemListener(this);
        optionPanel.add(disableBox);

        hideVOIBox = new JCheckBox("Hide Points");
        hideVOIBox.setFont(serif12);
        hideVOIBox.addItemListener(this);
        optionPanel.add(hideVOIBox);

        manage.addOnNextLine(optionPanel);

        final JPanel boxPanel = new JPanel(new GridLayout(1, 4));
        boxPanel.setForeground(Color.black);

        final JButton zoomIn = new JButton("Zoom In");
        zoomIn.setFont(serif12);
        zoomIn.addActionListener(this);
        boxPanel.add(zoomIn);

        final JButton zoomOut = new JButton("Zoom Out");
        zoomOut.setFont(serif12);
        zoomOut.addActionListener(this);
        boxPanel.add(zoomOut);

        final JButton prevButton = new JButton("Prev");
        prevButton.setFont(serif12);
        prevButton.addActionListener(this);
        boxPanel.add(prevButton);

        final JButton nextButton = new JButton("Next");
        nextButton.setFont(serif12);
        nextButton.addActionListener(this);
        boxPanel.add(nextButton);

        manage.addOnNextLine(boxPanel);

        getContentPane().add(manage.getPanel(), BorderLayout.CENTER);

        final JPanel buttonPanel = new JPanel(new GridLayout(0, 4));
        buttonPanel.setForeground(Color.black);

        final JButton resetButton = new JButton("Reset");
        resetButton.setFont(serif12);
        resetButton.addActionListener(this);
        buttonPanel.add(resetButton);

        final JButton saveButton = new JButton("Save");
        saveButton.setFont(serif12);
        saveButton.addActionListener(this);
        buttonPanel.add(saveButton);

        final JButton lutButton = new JButton("LUT");
        lutButton.setFont(serif12);
        lutButton.addActionListener(this);
        buttonPanel.add(lutButton);

        final JButton endButton = new JButton("End");
        endButton.setFont(serif12);
        endButton.addActionListener(this);
        buttonPanel.add(endButton);

        getContentPane().add(buttonPanel, BorderLayout.SOUTH);

        // initVars();

        pack();
        setVisible(true);
        System.gc();

    }

    private boolean initVars() {

        if (imStack == null) {
            final FileIO imReader = new FileIO();
            final ModelImage sliceIm = imReader.readImage(images.get(0).getAbsolutePath());
            final int[] extents = sliceIm.getExtents();
            width = extents[0];
            height = extents[1];
            length = width * height;

            sliceIm.disposeLocal();
        } else {
            final int[] extents = imStack.getExtents();
            width = extents[0];
            height = extents[1];
            numImages = 1;
            if (extents.length > 2) {
                numImages = extents[2];
            }
            if (numImages == 1) {
                MipavUtil.displayError("This image is not 3D");
                imStack = null;
                return false;
            }
            length = width * height;

            if (noiseBox.isSelected()) {
                final int[] buffer = filterShotNoiseMean(imStack);
                try {
                    imStack.importData(0, buffer, true);
                } catch (final IOException e) {
                    e.printStackTrace();
                }
            }
        }

        return true;
    }

    private void openHisto() {
        final ViewJComponentEditImage comp = subVolumeFrame.getComponentImage();
        final JFrameHistogram histoFrame = new JFrameHistogram(parentFrame, subVolume, null, comp.getLUTa(), null);
        histoFrame.histogramLUT(true, true);
    }

    private void openImage() {
        if (imStack == null) {
            openImageFile();
        } else {
            openImageStack();
        }
    }

    /**
     * Opens up the new subvolume. To keep read times for the images low, it will only load images as necessary. It will
     * use previous portions of the images as you go up and down the stack.
     */
    private void openImageFile() {

        final int lowerBound = Math.max(0, currentSlice - sliceRange);
        final int upperBound = Math.min(numImages - 1, currentSlice + sliceRange);
        depth = upperBound - lowerBound + 1;
        int[] sliceBuffer = new int[length];

        final FileIO imReader = new FileIO();
        ModelImage sliceIm = null;

        try {
            int type = ModelImage.USHORT;
            if (currentSlice == prevSlice) {// Initial case
                imBuffer = new int[length * depth];
                for (int i = 0; i < depth; i++) {
                    sliceIm = imReader.readImage(images.get(i + lowerBound).getAbsolutePath());
                    if (noiseBox.isSelected()) {
                        sliceBuffer = filterShotNoiseMean(sliceIm);
                    } else {
                        sliceIm.exportData(0, length, sliceBuffer);
                    }
                    System.arraycopy(sliceBuffer, 0, imBuffer, i * length, length);
                    type = sliceIm.getType();
                    sliceIm.disposeLocal();
                }
            } else if (currentSlice < prevSlice) {// Going backwards
                final int[] tempBuffer = new int[length * depth];
                final int prevLBound = Math.max(0, prevSlice - sliceRange);
                final int copyDepth = upperBound - prevLBound + 1;
                final int lBoundDiff = prevLBound - lowerBound;
                System.arraycopy(imBuffer, 0, tempBuffer, lBoundDiff * length, copyDepth * length);
                if (lBoundDiff != 0) {
                    sliceIm = imReader.readImage(images.get(lowerBound).getAbsolutePath());
                    if (noiseBox.isSelected()) {
                        sliceBuffer = filterShotNoiseMean(sliceIm);
                    } else {
                        sliceIm.exportData(0, length, sliceBuffer);
                    }
                    System.arraycopy(sliceBuffer, 0, tempBuffer, 0, length);
                    sliceIm.disposeLocal();
                }
                imBuffer = tempBuffer;
            } else {// Going forwards
                final int[] tempBuffer = new int[length * depth];
                final int prevLBound = Math.max(0, prevSlice - sliceRange);
                final int prevUBound = Math.min(numImages - 1, prevSlice + sliceRange);
                final int copyDepth = prevUBound - lowerBound + 1;
                final int lBoundDiff = lowerBound - prevLBound;
                final int uBoundDiff = upperBound - prevUBound;
                System.arraycopy(imBuffer, lBoundDiff * length, tempBuffer, 0, copyDepth * length);
                if (uBoundDiff != 0) {
                    sliceIm = imReader.readImage(images.get(upperBound).getAbsolutePath());
                    if (noiseBox.isSelected()) {
                        sliceBuffer = filterShotNoiseMean(sliceIm);
                    } else {
                        sliceIm.exportData(0, length, sliceBuffer);
                    }
                    System.arraycopy(sliceBuffer, 0, tempBuffer, copyDepth * length, length);
                    sliceIm.disposeLocal();
                }
                imBuffer = tempBuffer;
            }

            activeSlice = currentSlice - lowerBound;

            imName.setText(images.get(currentSlice).getName());

            subVolume = new ModelImage(type, new int[] {width, height, depth}, "Sub-Volume");
            subVolume.importData(0, imBuffer, true);
            subVolumeFrame = new ViewJFrameImage(subVolume, lut);
            subVolumeFrame.setSlice(activeSlice);
            subVolumeFrame.setVisible(true);
            subVolumeFrame.addWindowListener(this);

            if (isExitRequired()) {
                subVolumeFrame.removeWindowListener(subVolumeFrame);
                subVolumeFrame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
            }

            readSWC(lowerBound);

        } catch (final IOException e) {
            MipavUtil.displayError("Could not read SWC file(s)");
            e.printStackTrace();
        }

        editTraceRB.setSelected(true);
        actionPerformed(new ActionEvent(this, 0, "Edit Trace"));

        /**
         * Useful for if you choose to implement an image list to jump around in the image
         */
        /*
         * if(Math.abs(sliceDiff) >= sliceRange){ imBuffer = new int[length*depth]; for(int i=0;i<depth;i++){ sliceIm =
         * imReader.readImage(images.get(i+lowerBound).getAbsolutePath()); sliceIm.exportData(0, length, sliceBuffer);
         * System.arraycopy(sliceBuffer, 0, imBuffer, i*length, length); } } else{ int prevLBound = Math.max(0,
         * prevSlice - sliceRange); int prevUBound = Math.min(numImages - 1, prevSlice + sliceRange); if(sliceDiff > 0){
         * for(int i=0;i<=prevUBound-lowerBound;i++){
         * 
         * } } else{ for(int i=0;i<=upperBound-prevLBound;i++){
         * 
         * } } }
         */

    }

    private void openImageStack() {

        final int lowerBound = Math.max(0, currentSlice - sliceRange);
        final int upperBound = Math.min(numImages - 1, currentSlice + sliceRange);
        depth = upperBound - lowerBound + 1;

        final String[] slices = new String[depth];
        for (int i = 0; i < depth; i++) {
            slices[i] = String.valueOf(lowerBound + i);
        }

        if (subVolume != null) {
            subVolume.disposeLocal();
        }

        subVolume = new ModelImage(ModelImage.USHORT, new int[] {width, height, depth}, "Sub-Volume");

        final AlgorithmExtractSlices extract = new AlgorithmExtractSlices(imStack, subVolume, slices);
        extract.run();

        activeSlice = currentSlice - lowerBound;

        imName.setText("Current slice is: " + currentSlice);

        subVolumeFrame = new ViewJFrameImage(subVolume, lut);
        subVolumeFrame.setSlice(activeSlice);
        subVolumeFrame.setVisible(true);
        subVolumeFrame.addWindowListener(this);

        if (isExitRequired()) {
            subVolumeFrame.removeWindowListener(subVolumeFrame);
            subVolumeFrame.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE);
        }

        try {
            readSWC(lowerBound);
        } catch (final IOException e) {
            e.printStackTrace();
        }

        editTraceRB.setSelected(true);
        actionPerformed(new ActionEvent(this, 0, "Edit Trace"));
    }

    /**
     * Unlike the openImage method, the SWC files take far less time to read so you can just read them all every single
     * time. This also takes care of when some SWCs change due to editing, so that will show up in later subvolumes.
     * 
     * Reads the SWC file and draws the lines on the displayed paint mask. Point VOIs are also used to control the
     * editing.
     * 
     * @param lowerBound
     * @throws FileNotFoundException
     * @throws IOException
     */
    private void readSWC(final int lowerBound) throws IOException {

        // Reset things regarding the mask/control points here
        mask = new BitSet(length * depth);
        controlPts = new VOI((short) 0, "Control Points", VOI.POINT, 0);
        paths = new LineList();
        links = new Linking();
        progenitorPt = null;
        splitPt = null;
        primaryPt = null;
        progenitorVOI = null;
        splitVOI = null;
        primaryVOI = null;
        polyVOI = new VOI((short) 1, "Polygon Area", VOI.CONTOUR, -1);

        ArrayList<String[]> points = null;
        final ArrayList<Point> polyPt = new ArrayList<Point>();
        String[] lineArray;

        for (int k = 0; k < depth; k++) {
            if (k + lowerBound >= swcList.size()) {
                continue;
            }
            final BufferedReader input = new BufferedReader(new FileReader(swcList.get(k + lowerBound)));
            String line = null;
            points = new ArrayList<String[]>();
            while ( (line = input.readLine()) != null) {
                // Only need to recall information if we are reading
                // the active slice
                if (k == activeSlice) {
                    if (line.startsWith("# Progenitor")) {
                        line = line.substring(line.indexOf("(") + 1);
                        int ind = line.indexOf(",");
                        final int xp = Integer.valueOf(line.substring(0, ind));
                        line = line.substring(ind + 1);
                        ind = line.indexOf(")");
                        final int yp = Integer.valueOf(line.substring(0, ind));
                        progenitorPt = new Point(xp, yp);
                        continue;
                    } else if (line.startsWith("# Split")) {
                        line = line.substring(line.indexOf("(") + 1);
                        int ind = line.indexOf(",");
                        final int xs = Integer.valueOf(line.substring(0, ind));
                        line = line.substring(ind + 1);
                        ind = line.indexOf(")");
                        final int ys = Integer.valueOf(line.substring(0, ind));
                        splitPt = new Point(xs, ys);
                        continue;
                    } else if (line.startsWith("# Poly Pt")) {
                        line = line.substring(line.indexOf("(") + 1);
                        int ind = line.indexOf(",");
                        final int xs = Integer.valueOf(line.substring(0, ind));
                        line = line.substring(ind + 1);
                        ind = line.indexOf(")");
                        final int ys = Integer.valueOf(line.substring(0, ind));
                        final Point pt = new Point(xs, ys);
                        polyPt.add(pt);
                        continue;
                    } else if (line.startsWith("# Primary")) {
                        line = line.substring(line.indexOf("(") + 1);
                        int ind = line.indexOf(",");
                        final int xs = Integer.valueOf(line.substring(0, ind));
                        line = line.substring(ind + 1);
                        ind = line.indexOf(")");
                        final int ys = Integer.valueOf(line.substring(0, ind));
                        final Point pt = new Point(xs, ys);
                        primaryPt = pt;
                        continue;
                    }
                }
                if (line.startsWith("#")) {
                    continue;
                }
                lineArray = line.split(" ");
                if (lineArray.length > 0) {
                    points.add(lineArray);
                }
            }
            input.close();

            if (polyPt.size() > 0 && k == activeSlice) {
                final Vector3f[] tipVec = new Vector3f[polyPt.size()];
                for (int i = 0; i < polyPt.size(); i++) {
                    final Point pt = polyPt.get(i);
                    tipVec[i] = new Vector3f(pt.x, pt.y, activeSlice);
                }
                final VOIContour curve = new VOIContour(true);
                curve.importPoints(tipVec);

                polyVOI.importCurve(curve);
            }

            int linkedTo;
            int x0, x1, y0, y1;
            Point p0, p1;
            String num;
            LinkElement e0, e1;

            lineArray = points.get(0);
            num = lineArray[2];
            num = num.substring(0, num.indexOf("."));
            x0 = Integer.parseInt(num);
            num = lineArray[3];
            num = num.substring(0, num.indexOf("."));
            y0 = height - Integer.parseInt(num);

            VOIBaseVector pts = new VOIBaseVector();

            if (k == activeSlice) {
                origin = new Point(x0, y0);
                final VOIPoint voi = new VOIPoint(VOI.POINT, new Vector3f(x0, y0, activeSlice));
                voi.setLabel("O");
                controlPts.importCurve(voi);

                pts = controlPts.getCurves();
                originVOI = (VOIPoint) pts.get(0);
            }

            for (int i = points.size() - 1; i > 0; i--) {
                lineArray = points.get(i);
                num = lineArray[2];
                num = num.substring(0, num.indexOf("."));
                x0 = Integer.parseInt(num);
                num = lineArray[3];
                num = num.substring(0, num.indexOf("."));
                y0 = height - Integer.parseInt(num);

                // Make the various line VOIs
                linkedTo = Integer.parseInt(lineArray[6]);

                lineArray = points.get(linkedTo - 1);
                num = lineArray[2];
                num = num.substring(0, num.indexOf("."));
                x1 = Integer.parseInt(num);
                num = lineArray[3];
                num = num.substring(0, num.indexOf("."));
                y1 = height - Integer.parseInt(num);

                if (k != activeSlice) {
                    bresenham(x0, y0, x1, y1, k);
                } else {
                    p0 = new Point(x0, y0);
                    p1 = new Point(x1, y1);
                    e0 = links.get(p0);
                    e1 = links.get(p1);

                    if (e0 == null) {
                        e0 = new LinkElement(p0);
                    }
                    if (e1 == null) {
                        e1 = new LinkElement(p1);
                    }

                    if (e0.addLinkTo(e1)) {
                        paths.add(p0, p1);
                    }

                    boolean contains = false;
                    for (int m = 0; m < pts.size(); m++) {
                        final VOIBase voibase = pts.get(m);
                        if (voibase.contains(x0, y0)) {
                            contains = true;
                            break;
                        }

                    }

                    if ( !contains) {
                        final VOIPoint voi = new VOIPoint(VOI.POINT, new Vector3f(x0, y0, activeSlice));
                        voi.setLabel("");
                        pts.add(voi);
                    }

                }
            }
            if (k == activeSlice) {
                controlPts.setCurves(pts);
            }
        }

        final VOIBaseVector base = controlPts.getCurves();
        VOIPoint ptVOI;
        Vector3f ptVec;

        for (int i = 0; i < base.size(); i++) {
            ptVOI = (VOIPoint) base.get(i);
            ptVec = ptVOI.exportPoint();
            final Point checkPt = new Point((int) ptVec.X, (int) ptVec.Y);
            if (splitPt != null && splitPt.equals(checkPt)) {
                splitVOI = ptVOI;
                splitVOI.setLabel("S");
            } else if (progenitorPt != null && progenitorPt.equals(checkPt)) {
                progenitorVOI = ptVOI;
                progenitorVOI.setLabel("P");
            } else if (primaryPt != null && primaryPt.equals(checkPt)) {
                primaryVOI = ptVOI;
                primaryVOI.setLabel("1");
            }
        }

        if ( !hideVOIBox.isSelected()) {
            subVolume.getVOIs().add(controlPts);
            subVolume.notifyImageDisplayListeners(null, true, 0, -1);
        }

        subVolumeFrame.getComponentImage().setPaintMask(mask);
        subVolumeFrame.getComponentImage().addMouseListener(this);
        subVolumeFrame.getComponentImage().addMouseMotionListener(this);

        float opacity = 1.0f;
        if (disableBox.isSelected()) {
            opacity = 0.0f;
        }
        subVolumeFrame.getControls().getTools().setOpacity(opacity);
        subVolumeFrame.getControls().getTools().setPaintColor(Color.GREEN);
        subVolumeFrame.updateImages();

    }

    /**
     * Saves the new neuron trace into the old SWC file while moving the old file into a new file by adding a number to
     * the end of the file (after the extension). Larger numbers indicate newer files.
     * 
     * @throws IOException
     */
    private void saveNewSWC() throws IOException {

        if (checkCycles()) {
            MipavUtil.displayError("Loop exists in neuron trace. Remove loop before" + "saving.");
            return;
        }

        try {
            resolution = Double.valueOf(xResField.getText());
        } catch (final NumberFormatException n) {
            MipavUtil.displayError("Resolution must be a number");
            return;
        }

        if (originVOI == null) {
            MipavUtil.displayError("No origin is set. Please choose an origin point.");
            return;
        }

        int counter = 0;
        File currentSWC = swcList.get(currentSlice);

        String header = "";
        String csvOut = currentSlice + ",";

        final String saveName = currentSWC.getPath();
        String name = currentSWC.getName();
        final String parent = currentSWC.getParent();

        if (deleteBox.isSelected()) {
            currentSWC.delete();
        } else {
            // Rename old files by appending .# to them
            // Higher number means more recent
            File oldSWC;
            do {
                final String newName = name + "." + counter;
                oldSWC = new File(parent + File.separator + newName);
                counter++;
            } while (oldSWC.exists());

            currentSWC.renameTo(oldSWC);
        }
        currentSWC = new File(saveName);
        final FileWriter writer = new FileWriter(currentSWC);

        if (polyVOI.getSize() == 0) {
            polygonDisplay();
        }

        // Write out any pertinent information that can be used
        // if opened later
        final double[] areaResults = calcPolyArea();
        header += "########################################################\n" + "#                Landmark Coordinates                  #\n"
                + "########################################################\n";

        header += String.format("# Polygonal Area: %5.2f %s\n", areaResults[2], resUnits.getSelectedItem());
        header += String.format("# Centroid: (%4.2f,%4.2f)\n", areaResults[0], areaResults[1]);
        header += String.format("# Origin (%d,%d)\n", origin.x, origin.y);
        if (progenitorPt != null) {
            header += String.format("# Progenitor Point (%d,%d)\n", progenitorPt.x, progenitorPt.y);
            // csvOut += progenitorPt.x + "," + progenitorPt.y + ",";
        } // else csvOut += ",,";
        if (splitPt != null) {
            header += String.format("# Split Point (%d,%d)\n", splitPt.x, splitPt.y);
            // csvOut += splitPt.x + "," + splitPt.y + ",";
        } // else csvOut += ",,";
        if (primaryPt != null) {
            header += String.format("# Primary Branch Point (%d,%d)\n", primaryPt.x, primaryPt.y);
        }

        // csvOut += areaResults[0] + "," + areaResults[1] + "," + areaResults[2] + ",";

        final VOIBase base = polyVOI.getCurves().get(0);

        final int[] xBuffer = new int[base.size()];
        final int[] yBuffer = new int[base.size()];
        final int[] zBuffer = new int[base.size()];
        base.exportArrays(xBuffer, yBuffer, zBuffer);

        for (int i = 0; i < base.size(); i++) {
            header += String.format("# Poly Pt (%d,%d)\n", xBuffer[i], yBuffer[i]);
        }

        writer.append(header);

        // Save out the trace as an image, 255 is pixel value of output
        if (saveBox.isSelected()) {
            final int[] buffer = new int[length];
            for (int i = mask.nextSetBit(activeSlice * length); i >= 0 && i < (activeSlice + 1) * length; i = mask.nextSetBit(i + 1)) {
                buffer[i - activeSlice * length] = 255;
            }

            final ModelImage trace = new ModelImage(ModelImage.USHORT, new int[] {width, height}, "Skel");
            trace.importData(0, buffer, true);
            final File current = images.get(currentSlice);
            name = current.getName();
            final int splitAt = name.lastIndexOf(".");
            final String fileName = name.substring(0, splitAt) + "_trace";

            trace.saveImage(parent + File.separator, fileName, FileUtility.TIFF, true);
            trace.disposeLocal();
        }

        int line = 1;
        LinkElement start = links.get(origin);
        final String lengthHeader = "#\n# Coordinates are for endpoints of projections\n" + "# Distances are to branch point of this projection's order\n";

        int maxOrder = 0;
        float totalLength = 0f;
        float orderLength = 0f;

        PriorityQueue<NeuronLength> pq = new PriorityQueue<NeuronLength>();
        writer.append(lengthHeader);
        final ArrayList<Double> order1 = new ArrayList<Double>();
        final ArrayList<Double> order2 = new ArrayList<Double>();
        final ArrayList<Double> order3 = new ArrayList<Double>();
        double progLength = -1;
        double origLength = -1;
        if (splitPt != null) {
            double longestLength = 0;
            final LinkElement split = links.get(splitPt);
            LinkElement toOrigin = null;
            final ArrayList<LinkElement> list = split.linked;

            // Starting from the split point, use
            // a DFS on each link to see which one goes
            // towards the origin so you can separate
            // the two lengths
            for (int i = 0; i < list.size(); i++) {
                final LinkElement next = list.get(i);
                if (dfsOrigin(next, split)) {
                    toOrigin = next;
                }
            }

            // Find lengths towards the origin:

            writer.append("#\n########################################################\n" + "#               Lengths towards origin                 #\n"
                    + "########################################################\n");
            pq.addAll(calcLengths(toOrigin, split, toOrigin.pt.distance(split.pt)));
            NeuronLength nl;
            nl = pq.peek();
            origLength = nl.length;
            longestLength += nl.length;
            // csvOut += longestLength*resolution + ",";
            while ( (nl = pq.poll()) != null) {
                if (nl.order > maxOrder) {
                    maxOrder = nl.order;
                }
                if (nl.order >= 1) {
                    orderLength += nl.length;
                }
                if (nl.order == 1) {
                    order1.add(nl.length);
                } else if (nl.order == 2) {
                    order2.add(nl.length);
                } else if (nl.order == 3) {
                    order3.add(nl.length);
                }
                totalLength += nl.length;
                writePoint(nl, writer);
            }
            writer.append("#\n########################################################\n" + "#             Lengths towards progenitor               #\n"
                    + "########################################################\n");
            pq.clear();
            pq.addAll(calcLengths(split, toOrigin, 0));
            nl = pq.peek();
            progLength = nl.length;
            longestLength += nl.length;
            // csvOut += nl.length*resolution + "," + longestLength*resolution + ",";
            while ( (nl = pq.poll()) != null) {
                if (nl.order > maxOrder) {
                    maxOrder = nl.order;
                }
                if (nl.order >= 1) {
                    orderLength += nl.length;
                }
                if (nl.order == 1) {
                    order1.add(nl.length);
                } else if (nl.order == 2) {
                    order2.add(nl.length);
                } else if (nl.order == 3) {
                    order3.add(nl.length);
                }
                totalLength += nl.length;
                writePoint(nl, writer);
            }

            totalLength *= resolution;
            orderLength *= resolution;
            // csvOut += totalLength + "," + orderLength + ",";
            // csvOut += maxOrder + ",\n";
            final String lengthStr = String.format("%4.2f %s", longestLength * resolution, resUnits.getSelectedItem());
            writer.append("#\n# Origin to Primary length: " + lengthStr + "\n");
            final String totalStr = String.format("%4.2f %s", totalLength, resUnits.getSelectedItem());
            writer.append("# Total branch length: " + totalStr + "\n");
            final String orderStr = String.format("%4.2f %s", orderLength, resUnits.getSelectedItem());
            writer.append("# Total higher order branch length: " + orderStr + "\n");

        } else {
            pq = calcLengths(start, null, 0);
            NeuronLength nl;
            nl = pq.peek();
            // csvOut += ",," + nl.length*resolution + ",";
            while ( (nl = pq.poll()) != null) {
                if (nl.order > maxOrder) {
                    maxOrder = nl.order;
                }
                if (nl.order >= 1) {
                    orderLength += nl.length;
                }
                if (nl.order == 1) {
                    order1.add(nl.length);
                } else if (nl.order == 2) {
                    order2.add(nl.length);
                } else if (nl.order == 3) {
                    order3.add(nl.length);
                }
                totalLength += nl.length;
                writePoint(nl, writer);
            }
            totalLength *= resolution;
            orderLength *= resolution;
            // csvOut += totalLength + "," + orderLength + ",";
            // csvOut += maxOrder + ",\n";

            final String totalStr = String.format("%4.2f %s", totalLength, resUnits.getSelectedItem());
            writer.append("#\n# Total branch length: " + totalStr + "\n");
            final String orderStr = String.format("%4.2f %s", orderLength, resUnits.getSelectedItem());
            writer.append("# Total higher order branch length: " + orderStr + "\n");
        }

        csvOut += order1.size() + order2.size() + order3.size() + ",";
        csvOut += progLength * resolution + "," + origLength * resolution + ",";
        csvOut += areaResults[2] + ",";
        for (int i = 0; i < 50; i++) {
            if (i >= order1.size()) {
                csvOut += ",";
            } else {
                csvOut += order1.get(i) * resolution + ",";
            }
        }
        for (int i = 0; i < 10; i++) {
            if (i >= order2.size()) {
                csvOut += ",";
            } else {
                csvOut += order2.get(i) * resolution + ",";
            }
        }
        for (int i = 0; i < 5; i++) {
            if (i >= order3.size()) {
                csvOut += ",";
            } else {
                csvOut += order3.get(i) * resolution + ",";
            }
        }
        csvOut += "\n";
        statsCSV.append(csvOut);

        final String dimStr = String.format("# Width %d\n# Height %d\n", width, height);
        final String resStr = "# Resolution " + xResField.getText() + " " + (String) resUnits.getSelectedItem() + "\n";
        final String noteStr = "#\n########################################################\n" + "#              START OF SWC COORDINATES                #\n"
                + "########################################################\n" + "# NOTE: The above coordinates are in image space coordinates\n"
                + "# Y-coordinates for SWC format are inverted in image space.\n" + "# Image Y-coordinate = Image Height - SWC Y-coordinate\n";
        writer.append(dimStr);
        writer.append(resStr);
        writer.append(noteStr);

        // Read the origin point and write all necessary information
        Point pt = start.pt;
        final int[] parts = new int[7];

        parts[0] = line;
        parts[1] = 2;
        parts[2] = pt.x;
        parts[3] = height - pt.y;
        parts[4] = 0;
        parts[5] = 1;
        parts[6] = -1;

        String outString = String.valueOf(parts[0]) + " ";
        outString += String.valueOf(parts[1]) + " ";
        outString += String.valueOf(parts[2]) + ".0 ";
        outString += String.valueOf(parts[3]) + ".0 ";
        outString += String.valueOf(parts[4]) + ".0 ";
        outString += String.valueOf(parts[5]) + ".0 ";
        outString += String.valueOf(parts[6]) + "\n";

        writer.append(outString);

        final ArrayDeque<Integer> parentLine = new ArrayDeque<Integer>();
        final ArrayDeque<LinkElement> parentPt = new ArrayDeque<LinkElement>();

        final HashSet<LinkElement> visited = new HashSet<LinkElement>();
        visited.add(start);
        LinkElement next;

        ArrayList<LinkElement> linkedTo = start.linked;
        for (int i = 0; i < linkedTo.size(); i++) {
            next = linkedTo.get(i);
            parentLine.addFirst(line);
            parentPt.addFirst(next);
            visited.add(next);
        }

        // Traverse linked list and write each point out
        while (parentPt.peek() != null) {
            line++;
            start = parentPt.pollFirst();
            pt = start.pt;
            linkedTo = start.linked;
            parts[0] = line;
            if (linkedTo.size() == 1) {
                parts[1] = 6;
            } else if (linkedTo.size() == 2) {
                parts[1] = 7;
            } else {
                parts[1] = 5;
            }
            parts[2] = pt.x;
            parts[3] = height - pt.y;
            parts[4] = 0;
            parts[5] = 1;
            parts[6] = parentLine.pollFirst();

            outString = String.valueOf(parts[0]) + " ";
            outString += String.valueOf(parts[1]) + " ";
            outString += String.valueOf(parts[2]) + ".0 ";
            outString += String.valueOf(parts[3]) + ".0 ";
            outString += String.valueOf(parts[4]) + ".0 ";
            outString += String.valueOf(parts[5]) + ".0 ";
            outString += String.valueOf(parts[6]) + "\n";

            writer.append(outString);

            for (int i = 0; i < linkedTo.size(); i++) {
                next = linkedTo.get(i);
                if (visited.add(next)) {
                    parentLine.addFirst(line);
                    parentPt.addFirst(next);
                }
            }
        }

        writer.close();
    }

    private void writePoint(final NeuronLength n, final FileWriter writer) throws IOException {
        final Point pt = n.endPt;
        final double length = resolution * n.length;
        final int o = n.order;
        String p = "";
        if (progenitorPt != null && pt.equals(progenitorPt)) {
            p = "(Progenitor)";
        } else if (primaryPt != null && pt.equals(primaryPt)) {
            p = "(Primary)";
        } else if (splitPt != null && pt.equals(origin)) {
            p = "(Origin)";
        }
        final String coord = String.format("# (%d, %d) ", pt.x, pt.y);
        final String order = String.format("Branch order: %d ", o);
        final String value = String.format("Length: %4.2f %s ", length, resUnits.getSelectedItem());
        final String outString = coord + order + value + p + "\n";
        writer.append(outString);

    }

    /**
     * Finds which links only have one, and thus are tips, which are used to draw the polygonal area
     */
    private void polygonDisplay() {

        final ArrayList<Point> polyPt = new ArrayList<Point>();
        polyVOI = new VOI((short) 1, "Polygon Area", VOI.CONTOUR, -1);
        for (int i = 0; i < links.size(); i++) {
            final LinkElement l = links.get(i);
            if (l.linked.size() == 1) {
                polyPt.add(l.pt);
            }
        }

        final Vector3f[] tipVec = new Vector3f[polyPt.size()];
        for (int i = 0; i < polyPt.size(); i++) {
            final Point pt = polyPt.get(i);
            tipVec[i] = new Vector3f(pt.x, pt.y, activeSlice);
        }
        final VOIContour curve = new VOIContour(true);
        curve.importPoints(tipVec);
        curve.convexHull();

        polyVOI.importCurve(curve);

    }

    /**
     * Method used to determine which files in the chosen directory to add, which are then added to the list
     * <code>images</code>. A filename filter is used to choose images with filetype tif or lsm. This method also works
     * recursively to add candidate images in folder within the main directory.
     * 
     * @param dir the directory to search through for files
     * @return true if the list is non-empty
     */

    private boolean populateImages(final File dir) {

        if ( !dir.exists()) {
            return false;
        }
        if (dir.isFile()) {
            final FileIO imReader = new FileIO();
            imStack = imReader.readImage(dir.getPath());

            String dirStr = dir.getParent();
            if ( !dirStr.endsWith(File.separator)) {
                dirStr = dirStr + File.separator;
            }
            dirStr = dirStr.concat("Branch_Images" + File.separator);

            final FilenameFilter swcFilter = new FilenameFilter() {
                @Override
                public boolean accept(final File dir, final String name) {
                    return name.toLowerCase().endsWith(".swc");
                }
            };

            final File[] files = (new File(dirStr)).listFiles(swcFilter);
            for (int i = 0; i < files.length; i++) {
                swcList.add(files[i]);
            }

            return true;
        }

        File skelName;
        String stripped;

        final FilenameFilter imFilter = new FilenameFilter() {
            @Override
            public boolean accept(final File dir, final String name) {
                return (name.toLowerCase().endsWith(".tif") || name.toLowerCase().endsWith(".tiff") || name.toLowerCase().endsWith(".lsm"));
            }
        };
        final File[] files = dir.listFiles(imFilter);

        // Sort list into correct order
        Arrays.sort(files, new Comparator<File>() {

            @Override
            public int compare(final File o1, final File o2) {
                final String s1 = o1.getName();
                final String s2 = o2.getName();

                final String s1NoNum = s1.replaceAll("[0-9]", "");
                final String s2NoNum = s2.replaceAll("[0-9]", "");

                final int compare = s1NoNum.compareTo(s2NoNum);

                if (compare == 0) {
                    // Without numbers, the two are the same
                    String s1Num = "";
                    String s2Num = "";
                    
                    final Pattern p = Pattern.compile("(\\d+)\\.\\w+$");
                    Matcher m = p.matcher(s1);
                    if (m.find()) {
                        s1Num = m.group(1);
                    }
                    m = p.matcher(s2);
                    if (m.find()) {
                        s2Num = m.group(1);
                    }

                    // Compare the left over numbers
                    final int s1Int = Integer.valueOf(s1Num);
                    final int s2Int = Integer.valueOf(s2Num);

                    return Integer.valueOf(s1Int).compareTo(Integer.valueOf(s2Int));
                } else {
                    return compare;
                }
            }
        });

        String dirStr = dir.toString();
        if ( !dirStr.endsWith(File.separator)) {
            dirStr = dirStr + File.separator;
        }
        dirStr = dirStr.concat("Branch_Images" + File.separator);

        for (final File im : files) {

            stripped = im.getName();
            stripped = stripped.substring(0, stripped.indexOf("."));
            stripped = stripped.concat("_branches.swc");
            skelName = new File(dirStr.concat(stripped));
            if (skelName.exists()) {
                images.add(im);
                swcList.add(skelName);
            }
        }

        final File[] directories = dir.listFiles(new FileFilter() {
            @Override
            public boolean accept(final File path) {
                return (path.isDirectory() && !path.getName().equals("Branch_Images"));
            }
        });

        for (int i = 0; i < directories.length; i++) {
            populateImages(directories[i]);
        }
        numImages = images.size();

        return numImages > 0;
    }

    @Override
    public void mouseDragged(final MouseEvent e) {
        if (editPolyRB.isSelected()) {
            return;
        }

        // Actively adjust lines as you move the VOI
        if (lastActive != null) {
            final Vector3f ptVec = ((VOIPoint) lastActive).exportPoint();
            final int x = (int) ptVec.X;
            final int y = (int) ptVec.Y;
            final Point pt = new Point(x, y);

            if (toChange.equals(origin)) {
                origin = pt;
            } else if (toChange.equals(progenitorPt)) {
                progenitorPt = pt;
            } else if (toChange.equals(splitPt)) {
                splitPt = pt;
            } else if (toChange.equals(primaryPt)) {
                primaryPt = pt;
            }

            // We know that the drag happened on a VOI, so now update all links and lines
            links.moveNode(toChange, pt);

            toChange = pt;
        }

    }

    @Override
    public void mouseMoved(final MouseEvent e) {
        if (addingBranch) {
            final Vector3f pt = newBranchVOI.getPosition();
            final int ox = (int) pt.X;
            final int oy = (int) pt.Y;

            final float zoomX = subVolumeFrame.getComponentImage().getZoomX();
            final float zoomY = subVolumeFrame.getComponentImage().getZoomY();
            final int x = (int) (e.getX() / zoomX);
            final int y = (int) (e.getY() / zoomY);

            final Point oldPt = new Point(ox, oy);
            final Point newPt = new Point(x, y);

            if (lastSpot != null) {
                paths.remove(oldPt, lastSpot);
            }
            paths.add(oldPt, newPt);
            lastSpot = newPt;

            subVolume.notifyImageDisplayListeners();
        }
    }

    /**
     * Checks for double clicks of the left mouse. If a double click is detected, a node is either added or deleted from
     * the editable neuron trace (depending on which radio button is selected).
     */
    @Override
    public void mouseClicked(final MouseEvent e) {

        final float zoomX = subVolumeFrame.getComponentImage().getZoomX();
        final float zoomY = subVolumeFrame.getComponentImage().getZoomY();
        final int x = (int) (e.getX() / zoomX);
        final int y = (int) (e.getY() / zoomY);

        final int numClicks = e.getClickCount();
        final int button = e.getButton();

        if (button != MouseEvent.BUTTON1) {
            return;
        }

        if (blockClick) {// block clicks after adding a new branch to prevent issues
            if (numClicks == 1) {
                blockClick = false;
            } else {
                return;
            }
        }

        if (addingBranch && numClicks == 1) {
            final Vector3f branchVOIPt = newBranchVOI.getPosition();
            final Point newPt = new Point((int) branchVOIPt.X, (int) branchVOIPt.Y);
            if (lastSpot != null) {
                paths.remove(newPt, lastSpot);
            }

            // check to make sure this point is at least on a line
            // or on a node
            int dx, dy;
            int maxDistSqr = Integer.MAX_VALUE;
            int ind;
            int outX = -1;
            int outY = -1;
            boolean isNode = false;

            if (mask.get(x + y * width + activeSlice * length)) {
                outX = x;
                outY = y;
            } else {
                loop: for (int ny = y - 4; ny <= y + 4; ny++) {
                    if (ny < 0 || ny >= height) {
                        continue;
                    }
                    for (int nx = x - 4; nx <= x + 4; nx++) {
                        if (nx < 0 || nx >= width) {
                            continue;
                        }
                        ind = nx + ny * width;
                        if (links.get(new Point(nx, ny)) != null) {
                            outX = nx;
                            outY = ny;
                            isNode = true;
                            break loop;
                        }
                        if (mask.get(ind + activeSlice * length)) {
                            dx = x - nx;
                            dx *= dx;
                            dy = y - ny;
                            dy *= dy;
                            final int dSqr = dx + dy;
                            if (dSqr < maxDistSqr) {
                                outX = nx;
                                outY = ny;
                                maxDistSqr = dSqr;
                            }
                        }
                    }
                }
            }

            if (outX == -1 || outY == -1) {
                // did not attach to anything
                controlPts.removeCurve(newBranchVOI);
            } else {
                LinkElement toLink;
                final Point linkPt = new Point(outX, outY);
                if (isNode) {
                    toLink = links.get(linkPt);
                    // logic for progenitor and origin
                    if (linkPt.equals(progenitorPt)) {
                        progenitorVOI.setLabel("");
                        newBranchVOI.setLabel("P");
                        progenitorVOI = newBranchVOI;
                        progenitorPt = newPt;
                    } else if (linkPt.equals(origin)) {
                        originVOI.setLabel("");
                        newBranchVOI.setLabel("O");
                        originVOI = newBranchVOI;
                        origin = newPt;
                    } else if (linkPt.equals(primaryPt)) {
                        primaryVOI.setLabel("");
                        newBranchVOI.setLabel("1");
                        primaryVOI = newBranchVOI;
                        primaryPt = newPt;
                    }
                } else {
                    toLink = links.addNode(linkPt);
                }
                final LinkElement newNode = new LinkElement(newPt);
                newNode.addLinkTo(toLink);
                paths.add(newPt, linkPt);

                // check if the new branch intersects with the polygonal VOI
                adjustPolygon(newPt, linkPt);
            }

            addingBranch = false;
            newBranchVOI = null;
            lastSpot = null;

            subVolume.notifyImageDisplayListeners();

            blockClick = true;
            // update all the line lists and linked elements
            return; // So nothing else happens
        }

        if (editPolyRB.isSelected()) {
            return;
        }

        final int thisSlice = subVolumeFrame.getViewableSlice();
        if (thisSlice != activeSlice) {
            return;
        }

        if (numClicks != 2) {
            return;
        }

        if (addRB.isSelected()) {

            if (lastActive != null) {
                lastActive = null;
                return;
            }

            int dx, dy;
            int maxDistSqr = Integer.MAX_VALUE;
            int ind;
            int outX = -1;
            int outY = -1;

            // Search for closest point on line in 5x5 box
            // to attach the node to
            if (mask.get(x + y * width + activeSlice * length)) {
                outX = x;
                outY = y;
            } else {
                for (int ny = y - 2; ny <= y + 2; ny++) {
                    if (ny < 0 || ny >= height) {
                        continue;
                    }
                    for (int nx = x - 2; nx <= x + 2; nx++) {
                        if (nx < 0 || nx >= width) {
                            continue;
                        }
                        ind = nx + ny * width;
                        if (mask.get(ind + activeSlice * length)) {
                            dx = x - nx;
                            dx *= dx;
                            dy = y - ny;
                            dy *= dy;
                            final int dSqr = dx + dy;
                            if (dSqr < maxDistSqr) {
                                outX = nx;
                                outY = ny;
                                maxDistSqr = dSqr;
                            }
                        }
                    }
                }
            }

            if (outX == -1 || outY == -1) {
                // Not clicking near any branches, so either don't do anything, or possibly do
                // something fancy (it might work)
                addingBranch = true;
                // Add voi? And also line path? Or just VOI and start drawing the line?

                newBranchVOI = new VOIPoint(VOI.POINT, new Vector3f(x, y, activeSlice));
                newBranchVOI.setLabel("");
                controlPts.importCurve(newBranchVOI);
                final VOIBaseVector vbv = controlPts.getCurves();
                newBranchVOI = (VOIPoint) vbv.get(vbv.size() - 1);

                subVolume.notifyImageDisplayListeners();
                return;
            }

            final LinePath onPath = paths.findPath(new Point(outX, outY));
            if (onPath != null) {
                final Point pt = new Point(outX, outY);
                links.addNode(pt, onPath);
            }

        } else if (deleteRB.isSelected()) {

            final VOIBase activeVOI = lastActive;

            if (activeVOI == null) {
                return;
            }

            VOIPoint ptVOI = (VOIPoint) activeVOI;

            Vector3f ptVec = ptVOI.exportPoint();
            final Point coord = new Point((int) ptVec.X, (int) ptVec.Y);

            final LinkElement elem = links.get(coord);
            final Point tempPt = elem.linked.get(0).pt;

            final boolean delete = links.removeNode(coord);

            if ( !delete) {
                return;
            }

            controlPts.removeCurve(activeVOI);

            if (coord.equals(origin)) {

                origin = tempPt;
                final VOIBaseVector base = controlPts.getCurves();
                for (int i = 0; i < base.size(); i++) {
                    ptVOI = (VOIPoint) base.get(i);
                    ptVec = ptVOI.exportPoint();
                    final Point checkPt = new Point((int) ptVec.X, (int) ptVec.Y);
                    if (origin.equals(checkPt)) {
                        originVOI = ptVOI;
                        originVOI.setLabel("O");

                        break;
                    }
                }
            } else if (coord.equals(progenitorPt)) {
                final LinkElement tempNode = links.get(tempPt);
                if (tempNode.linked.size() == 1) {// if it has more than 1 link, don't bother
                    progenitorPt = tempPt;
                    final VOIBaseVector base = controlPts.getCurves();
                    for (int i = 0; i < base.size(); i++) {
                        ptVOI = (VOIPoint) base.get(i);
                        ptVec = ptVOI.exportPoint();
                        final Point checkPt = new Point((int) ptVec.X, (int) ptVec.Y);
                        if (progenitorPt.equals(checkPt)) {
                            progenitorVOI = ptVOI;
                            progenitorVOI.setLabel("P");

                            break;
                        }
                    }
                }
            } else if (coord.equals(primaryPt)) {
                final LinkElement tempNode = links.get(tempPt);
                if (tempNode.linked.size() == 1) {// if it has more than 1 link, don't bother
                    primaryPt = tempPt;
                    final VOIBaseVector base = controlPts.getCurves();
                    for (int i = 0; i < base.size(); i++) {
                        ptVOI = (VOIPoint) base.get(i);
                        ptVec = ptVOI.exportPoint();
                        final Point checkPt = new Point((int) ptVec.X, (int) ptVec.Y);
                        if (primaryPt.equals(checkPt)) {
                            primaryVOI = ptVOI;
                            primaryVOI.setLabel("1");

                            break;
                        }
                    }
                }
            }
            subVolume.notifyImageDisplayListeners();
            lastActive = null;

        } else if (progenitorRB.isSelected() || splitRB.isSelected() || originRB.isSelected() || primaryRB.isSelected()) {
            final VOIBase activeVOI = lastActive;
            if (activeVOI == null) {
                return;
            }
            final VOIPoint ptVOI = (VOIPoint) activeVOI;
            final Vector3f ptVec = ptVOI.exportPoint();
            final Point pt = new Point((int) ptVec.X, (int) ptVec.Y);

            // If this VOI already has a label, remove it
            if (activeVOI == progenitorVOI) {
                progenitorVOI.setLabel("");
                progenitorVOI = null;
                progenitorPt = null;
            } else if (activeVOI == splitVOI) {
                splitVOI.setLabel("");
                splitVOI = null;
                splitPt = null;
            } else if (activeVOI == originVOI) {
                originVOI.setLabel("");
                originVOI = null;
                origin = null;
            } else if (activeVOI == primaryVOI) {
                primaryVOI.setLabel("");
                primaryVOI = null;
                primaryPt = null;
            }

            // Add correct label and set the correct variables
            if (progenitorRB.isSelected()) {
                if (progenitorVOI != null) {
                    progenitorVOI.setLabel("");
                }
                progenitorPt = pt;
                progenitorVOI = ptVOI;
                progenitorVOI.setLabel("P");
            } else if (splitRB.isSelected()) {
                if (splitVOI != null) {
                    splitVOI.setLabel("");
                }
                splitPt = pt;
                splitVOI = ptVOI;
                splitVOI.setLabel("S");
            } else if (originRB.isSelected()) {
                if (originVOI != null) {
                    originVOI.setLabel("");
                }
                origin = pt;
                originVOI = ptVOI;
                originVOI.setLabel("O");
            } else if (primaryRB.isSelected()) {
                if (primaryVOI != null) {
                    primaryVOI.setLabel("");
                }
                primaryPt = pt;
                primaryVOI = ptVOI;
                primaryVOI.setLabel("1");
            }

            subVolume.notifyImageDisplayListeners();
        }

        controlPts.setAllActive(false);
    }

    /**
     * The precursor to a drag movement involves a button press, so check to see if a VOI was clicked. If not, then any
     * following move or delete actions will not occur.
     */
    @Override
    public void mousePressed(final MouseEvent e) {

        if (editPolyRB.isSelected()) {
            return;
        }

        final VOIBaseVector pts = controlPts.getCurves();
        VOIBase activeVOI = null;

        for (int i = 0; i < pts.size(); i++) {
            final VOIBase current = pts.get(i);
            if (current.isActive()) {
                activeVOI = current;
                break;
            }
        }

        lastActive = activeVOI;

        if (activeVOI != null) {
            final VOIPoint ptVOI = (VOIPoint) activeVOI;
            final Vector3f vPt = ptVOI.getPosition();
            toChange = new Point((int) vPt.X, (int) vPt.Y);
            originalMovePt = new Point((int) vPt.X, (int) vPt.Y);
        }
    }

    /**
     * Once the mouse is released, check to see if it was preceeded by a drag event, in which case you need to move
     * whichever node was just dragged to another spot.
     */
    @Override
    public void mouseReleased(final MouseEvent e) {

        if (editPolyRB.isSelected()) {
            polyVOI = subVolume.getVOIs().get(0);
            return;
        } else if (originalMovePt != null) {
            final VOIBaseVector vbv = polyVOI.getCurves();
            final float zoomX = subVolumeFrame.getComponentImage().getZoomX();
            final float zoomY = subVolumeFrame.getComponentImage().getZoomY();
            boolean onVOI = false;
            final int x = (int) (e.getX() / zoomX);
            final int y = (int) (e.getY() / zoomY);
            for (final VOIBase v : vbv) {
                for (final Vector3f vec : v) {
                    if (vec.X == originalMovePt.x && vec.Y == originalMovePt.y) {
                        onVOI = true;
                        vec.X = x;
                        vec.Y = y;
                    }
                }
            }
            if ( !onVOI) {
                final Vector3f lastVec = lastActive.get(0);
                final Point lastPt = new Point((int) lastVec.X, (int) lastVec.Y);
                final LinkElement l = links.get(lastPt);
                final ArrayList<LinkElement> linked = l.linked;
                for (final LinkElement le : linked) {
                    if (adjustPolygon(lastPt, le.pt)) {
                        break;
                    }
                }
                // adjustPolygon(new Point(x,y));
            }

            originalMovePt = null;
        } else {
            // Check to see if a was node was moved and to see if it now
            // crosses the boundary
        }

        controlPts.setAllActive(false);
        subVolume.notifyImageDisplayListeners();
        subVolumeFrame.updateImages();

    }

    @Override
    public void mouseEntered(final MouseEvent e) {

    }

    @Override
    public void mouseExited(final MouseEvent e) {

    }

    @Override
    public void windowClosing(final WindowEvent e) {
        if (e.getSource() == subVolumeFrame) {
            try {
                statsCSV.close();
            } catch (final IOException e1) {
                MipavUtil.displayError("Could not close CSV output");
                e1.printStackTrace();
            }
            if (isExitRequired()) {
                ViewUserInterface.getReference().windowClosing(e);
            } else {
                dispose();
            }
        } else if (e.getSource() == this) {
            if (isExitRequired()) {
                ViewUserInterface.getReference().windowClosing(e);
            } else {
                dispose();
            }
        }
    }

    @Override
    public void itemStateChanged(final ItemEvent e) {
        final Object source = e.getSource();
        if (source == disableBox) {
            if (disableBox.isSelected()) {
                subVolumeFrame.getControls().getTools().setOpacity(0.0f);
                subVolumeFrame.updateImages();
            } else {
                subVolumeFrame.getControls().getTools().setOpacity(1.0f);
                subVolumeFrame.getControls().getTools().setPaintColor(Color.GREEN);
                subVolumeFrame.updateImages();
            }
        } else if (source == hideVOIBox) {
            if (hideVOIBox.isSelected()) {
                if (editTraceRB.isSelected()) {
                    subVolume.getVOIs().remove(controlPts);
                } else if (editPolyRB.isSelected()) {
                    subVolume.getVOIs().remove(polyVOI);
                }
                subVolume.notifyImageDisplayListeners(null, true, 0, -1);
            } else {
                if (editTraceRB.isSelected() && !subVolume.getVOIs().contains(controlPts)) {
                    subVolume.getVOIs().add(controlPts);
                    subVolume.notifyImageDisplayListeners(null, true, 0, -1);
                } else if (editPolyRB.isSelected() && !subVolume.getVOIs().contains(polyVOI)) {
                    subVolume.getVOIs().add(polyVOI);
                    subVolume.notifyImageDisplayListeners(null, true, 0, -1);
                }
            }
        }
    }

    /**
     * Subclass list used to keep track of which lines are displayed in the active slice. This is mainly used to find
     * the endpoints of the line when a node is added on a line so that connections can be properly updated.
     * 
     * Implements an ArrayList, but adds a couple of methods that are specific to finding elements based on coordinates
     * rather than pointers to objects.
     * 
     * @author wangvg
     * 
     */
    private class LineList extends ArrayList<LinePath> {

        /**
		 * 
		 */
        private static final long serialVersionUID = 6977450762745726017L;

        private LineList() {
            super();
        }

        /**
         * Add a new line to the list/mask based on the two endpoints.
         * 
         * @param p1
         * @param p2
         */
        private void add(final Point p1, final Point p2) {
            final LinkedHashSet<Point> list = bresenham(p1, p2);
            this.add(new LinePath(p1, p2, list));
        }

        /**
         * Remove the line that has these two endpoints from the list
         * 
         * @param p1
         * @param p2
         */
        private void remove(final Point p1, final Point p2) {
            for (int i = 0; i < size(); i++) {
                final LinePath p = get(i);
                if ( (p1.equals(p.pt1) && p2.equals(p.pt2)) || (p2.equals(p.pt1) && p1.equals(p.pt2))) {
                    final LinkedHashSet<Point> pts = p.pts;
                    Point pt;
                    int index;
                    final Iterator<Point> iter = pts.iterator();
                    while (iter.hasNext()) {
                        pt = iter.next();
                        index = pt.x + pt.y * width;
                        if ( !searchOtherPaths(p, pt)) {
                            mask.set(index + activeSlice * length, false);
                        }
                    }
                    remove(p);
                    return;
                }
            }
        }

        private boolean searchOtherPaths(final LinePath path, final Point pt) {
            for (int i = 0; i < size(); i++) {
                if (get(i) == path) {
                    continue;
                }
                if (get(i).contains(pt)) {
                    return true;
                }
            }
            return false;
        }

        /**
         * Finds the path (and thus the endpoints) that contains the given point. Basically searches through each
         * LinePath list to find this point.
         * 
         * @param pt
         * @return
         */
        private LinePath findPath(final Point pt) {
            for (int i = 0; i < this.size(); i++) {
                final LinePath path = this.get(i);
                if (path.contains(pt)) {
                    return path;
                }
            }
            return null;
        }

    }

    /**
     * Subclass, mostly used as a container. Keeps track of the endpoints and which points are assigned to the endpoints
     * (as determined by Bresenham's Line Algorithm).
     * 
     * @author wangvg
     * 
     */
    private class LinePath {

        private final Point pt1;

        private final Point pt2;

        private final LinkedHashSet<Point> pts;

        private LinePath(final Point p1, final Point p2, final LinkedHashSet<Point> list) {
            pt1 = p1;
            pt2 = p2;
            pts = list;
        }

        private boolean contains(final Point pt) {
            return pts.contains(pt);
        }

    }

    /**
     * Subclass which is basically just an implementation of a multi- linked list. Keeps track of an individual node and
     * which other nodes it is connected to.
     * 
     * @author wangvg
     * 
     */
    private class LinkElement {

        private Point pt;

        private final ArrayList<LinkElement> linked;

        private LinkElement(final Point node) {
            pt = node;
            linked = new ArrayList<LinkElement>();
            links.add(this);
        }

        private boolean addLinkTo(final LinkElement to) {
            if ( !linked.contains(to)) {
                linked.add(to);
                to.linked.add(this);
                return true;
            }
            return false;
        }

        private ArrayList<LinkElement> copyList() {
            final ArrayList<LinkElement> list = new ArrayList<LinkElement>();
            list.addAll(linked);
            return list;
        }

        private void removeLinkTo(final LinkElement to) {
            linked.remove(to);
            to.linked.remove(this);
        }

    }

    /**
     * A subclass used to keep track of which nodes are connected. Used when adding/deleting nodes in order to maintain
     * continuous connections between nodes. Also used when saving the new SWC file in order to properly link lines in
     * the file.
     * 
     * @author wangvg
     * 
     */
    private class Linking extends ArrayList<LinkElement> {

        /**
		 * 
		 */
        private static final long serialVersionUID = -7735099954298324932L;

        private Linking() {
            super();
        }

        /**
         * Gets an element from the list based on its point instead of reference
         * 
         * @param p
         * @return
         */
        private LinkElement get(final Point p) {

            for (int i = 0; i < size(); i++) {
                final LinkElement e = get(i);
                if (p.equals(e.pt)) {
                    return e;
                }
            }

            return null;
        }

        private LinkElement addNode(final Point node) {
            final LinePath path = paths.findPath(node);
            return addNode(node, path);
        }

        /**
         * Adds a node at the given point which lies on the given path. Breaks the previous line into two parts and adds
         * the VOI to control the new lines.
         * 
         * @param node
         * @param path
         */
        private LinkElement addNode(final Point node, final LinePath path) {
            final Point to = path.pt1;
            final Point from = path.pt2;
            final LinkElement eTo = get(to);
            final LinkElement eFrom = get(from);
            final LinkElement newLink = new LinkElement(node);

            eTo.removeLinkTo(eFrom);
            eTo.addLinkTo(newLink);
            eFrom.addLinkTo(newLink);

            paths.remove(to, from);
            paths.add(to, node);
            paths.add(node, from);

            final VOIPoint voi = new VOIPoint(VOI.POINT, new Vector3f(node.x, node.y, activeSlice));
            voi.setLabel("");
            controlPts.importCurve(voi);

            subVolume.notifyImageDisplayListeners();

            return newLink;
        }

        /**
         * The move node is implemented as simply breaking the old connection, moving the node, and reconnecting the
         * nodes at the new location.
         * 
         * @param from
         * @param to
         */
        private synchronized void moveNode(final Point from, final Point to) {
            final LinkElement node = get(from);
            node.pt = to;
            final ArrayList<LinkElement> list = node.linked;
            for (int i = 0; i < list.size(); i++) {
                paths.remove(from, list.get(i).pt);
            }
            for (int i = 0; i < list.size(); i++) {
                paths.add(to, list.get(i).pt);
            }

        }

        /**
         * Removes the node in question. It then uses the links provided to attempt to reconnect the nodes. This is not
         * optimized and may result in strange behavior possibly occuring if deleting a node with 3+ connections
         * 
         * @param node
         */
        private boolean removeNode(final Point node) {

            LinkElement e1;
            final LinkElement eNode = get(node);
            final ArrayList<LinkElement> pts = eNode.linked;
            final int size = pts.size();

            boolean delete = false;

            // This is a tip pt, just remove it, no new connections
            if (size == 1) {
                final LinkElement e = pts.get(0);
                eNode.removeLinkTo(e);
                paths.remove(eNode.pt, e.pt);
                delete = true;
            }
            // Need to remove this point and then reconnect the two neighbors
            else if (size == 2) {
                e1 = pts.get(0);
                final LinkElement e2 = pts.get(1);
                e1.addLinkTo(e2);
                paths.add(e1.pt, e2.pt);

                for (int i = 0; i < size; i++) {
                    e1 = pts.get(0);
                    e1.removeLinkTo(eNode);
                    paths.remove(node, e1.pt);
                }

                delete = true;
            } else if (size == 3) {
                // 3 is annoying. Need to set certain priorities
                // Should set highest priority target as 1 (current) link
                // 0 current links should be the lowest priority
                // Link highest priority targets first
                // Move to lower priority, try to link to highest priority

                // Nodes within 5 units of each other have the highest
                // priority when deleting, so if two points are close
                // together, they will combine
                final Comparator<LinkElement> linkComp = new Comparator<LinkElement>() {

                    @Override
                    public int compare(final LinkElement o1, final LinkElement o2) {
                        final int s1 = o1.linked.size();
                        final int s2 = o2.linked.size();

                        final double dist1 = o1.pt.distance(eNode.pt);
                        final double dist2 = o2.pt.distance(eNode.pt);

                        if (s1 > 0 && dist1 < 5 && dist1 < dist2) {
                            return -1;
                        } else if (s1 > 0 && dist1 > 5 && dist2 < 5) {
                            return 1;
                        } else if (s1 == 1 || s2 == 0) {
                            return -1;
                        } else if (s2 == 1 || s1 == 0 || s1 == s2) {
                            return 1;
                        } else if (s1 > s2) {
                            return 1;
                        } else if (s1 < s2) {
                            return -1;
                        } else {
                            return 0;
                        }
                    }
                };
                final PriorityQueue<LinkElement> linkPriority = new PriorityQueue<LinkElement>(5, linkComp);

                // Remove all the current links to the deleted node
                for (int i = 0; i < size; i++) {
                    e1 = pts.get(0);
                    e1.removeLinkTo(eNode);
                    paths.remove(node, e1.pt);
                    linkPriority.add(e1);
                }

                // Connect the remaining neighbors to the highest
                // priority node
                final LinkElement connectTo = linkPriority.poll();
                while (linkPriority.peek() != null) {
                    e1 = linkPriority.poll();
                    e1.addLinkTo(connectTo);
                    paths.add(e1.pt, connectTo.pt);
                }

                delete = true;

            }// 4 neighbors is tricky since it could be in a loop or
             // just two branches at the same origin
            else if (size == 4) {

                // Check to see if this point is a place where two
                // branches overlap (not really overlap, just check
                // for if the point is in a cycle)
                if (isOverlapPoint(eNode)) {
                    // Determine angles between each branch
                    final double[][] angles = new double[4][3];
                    final int[] maxAngle = new int[4];
                    final Point center = eNode.pt;
                    for (int i = 0; i < pts.size(); i++) {
                        int cnt = 0;
                        e1 = pts.get(i);
                        double angleMax = Double.MIN_VALUE;
                        // For each branch, also determine which other branch
                        // has the largest angle
                        for (int j = 0; j < pts.size(); j++) {
                            if (i == j) {
                                continue;
                            }
                            final Point to = pts.get(j).pt;
                            final double angle = pointAngle(e1.pt, to, center);
                            angles[i][cnt] = angle;
                            if (angle > angleMax) {
                                maxAngle[i] = j;
                                angleMax = angle;
                            }
                            cnt++;
                        }
                    }

                    // Figure out if the differences work to create the correct pairs or not
                    boolean disjoint = true;
                    for (int i = 0; i < 4; i++) {
                        final int pointer = maxAngle[i];
                        if (i != maxAngle[pointer]) {
                            disjoint = false;
                            break;
                        }
                    }

                    // If the pairings end up with an unconnected node, just go through each
                    // possible case to determine which case is best of those that can exist
                    if ( !disjoint) {
                        System.err.println("We have a problem");
                        final double[] choices = new double[3];
                        choices[0] = Math.pow(angles[0][0] - 180, 2) + Math.pow(angles[2][2] - 180, 2);
                        choices[1] = Math.pow(angles[0][2] - 180, 2) + Math.pow(angles[1][1] - 180, 2);
                        choices[2] = Math.pow(angles[0][1] - 180, 2) + Math.pow(angles[1][2] - 180, 2);

                        final double maxChoice = Math.min(choices[0], Math.min(choices[1], choices[2]));
                        if (maxChoice == choices[0]) {
                            maxAngle[0] = 1;
                            maxAngle[2] = 3;
                        } else if (maxChoice == choices[1]) {
                            maxAngle[0] = 3;
                            maxAngle[1] = 2;
                        } else {
                            maxAngle[0] = 2;
                            maxAngle[1] = 3;
                        }
                    }

                    // Check here to make sure that the new connections you make
                    // are logical and don't create two disjoint sets in the
                    // node graph

                    e1 = pts.get(0);
                    LinkElement to = pts.get(maxAngle[0]);
                    int next;
                    if (maxAngle[0] == 1) {
                        next = 2;
                    } else {
                        next = 1;
                    }

                    boolean allConnected = dfsOrigin(e1, to) || dfsOrigin(to, e1);

                    e1 = pts.get(next);
                    to = pts.get(maxAngle[next]);

                    allConnected &= dfsOrigin(e1, to) || dfsOrigin(to, e1);

                    if (allConnected) {

                        e1 = pts.get(0);
                        to = pts.get(maxAngle[0]);
                        e1.addLinkTo(to);
                        paths.add(e1.pt, to.pt);
                        if (maxAngle[0] == 1) {
                            next = 2;
                        } else {
                            next = 1;
                        }

                        e1 = pts.get(next);
                        to = pts.get(maxAngle[next]);
                        e1.addLinkTo(to);
                        paths.add(e1.pt, to.pt);

                        delete = true;
                    }
                    // Right now, if it does result in disjoint sets, it won't delete the
                    // node, but if it does result in disjoint sets, should probably go to
                    // the else case (but not an easy way to do that)

                } else {
                    // Need to reconnect everything logically, or replace? Not really sure what
                    // to do with his case
                    // Maybe split into two different points? <-- Would make most sense, as you
                    // can now recombine them

                    // Add new point right next to the point, rework connections
                    // Should add TOWARDS the origin, so use dfsOrigin

                    LinkElement toOrigin = null;

                    // Figure out which neighbor goes towards the origin
                    for (final LinkElement e : pts) {
                        if (dfsOrigin(e, eNode)) {
                            toOrigin = e;
                            break;
                        }
                    }

                    if (toOrigin == null) {
                        MipavUtil.displayError("No origin labeled");
                        return false;
                    }

                    final Point p1 = eNode.pt;
                    final Point p2 = toOrigin.pt;

                    Point nextPt = null;
                    for (final LinePath p : paths) {
                        // Get the path that these points are on, then get the
                        // point that is right next to the chosen point
                        if ( (p1.equals(p.pt1) && p2.equals(p.pt2)) || (p2.equals(p.pt1) && p1.equals(p.pt2))) {
                            final LinkedHashSet<Point> linePts = p.pts;
                            final Iterator<Point> iter = linePts.iterator();
                            if (iter.next().equals(p1)) {
                                nextPt = iter.next();
                            } else {// Need to go to the next to end
                                nextPt = iter.next();
                                while (iter.hasNext()) {
                                    final Point temp = iter.next();
                                    if ( !iter.hasNext()) {
                                        break;
                                    }
                                    nextPt = temp;
                                }
                            }
                        }
                    }

                    if (nextPt == null) {
                        MipavUtil.displayError("Could not find point");
                        return false;
                    }

                    final LinkElement toSwitch = shortestEnd(eNode, toOrigin);
                    final LinePath onPath = paths.findPath(nextPt);
                    final LinkElement newNode = addNode(nextPt, onPath);
                    // Node has been added. Now need to move some stuff around;

                    // Switch the connection from the shortest path to the new
                    // node that was created
                    toSwitch.removeLinkTo(eNode);
                    toSwitch.addLinkTo(newNode);
                    paths.remove(toSwitch.pt, eNode.pt);
                    paths.add(toSwitch.pt, newNode.pt);

                    subVolume.notifyImageDisplayListeners();
                    // Now we have neighboring point to place stuff at

                }

                if (delete) {
                    for (int i = 0; i < size; i++) {
                        e1 = pts.get(0);
                        e1.removeLinkTo(eNode);
                        paths.remove(node, e1.pt);
                    }
                }
            } else {
                // Size is greater than 4, not really sure what to do with this
                System.err.println("More than 4 neighbors");
            }

            if (delete) {
                remove(eNode);
            }

            subVolume.notifyImageDisplayListeners();

            return delete;
        }

        /**
         * Finds the angle between two points connected at the center point.
         * 
         * @param pt1
         * @param pt2
         * @param center
         * @return
         */
        private double pointAngle(final Point pt1, final Point pt2, final Point center) {

            double angle = 0;

            final Point p1 = new Point(pt1.x - center.x, pt1.y - center.y);
            final Point p2 = new Point(pt2.x - center.x, pt2.y - center.y);

            final double dotProduct = p1.x * p2.x + p1.y * p2.y;
            final double mag1 = Math.sqrt(p1.x * p1.x + p1.y * p1.y);
            final double mag2 = Math.sqrt(p2.x * p2.x + p2.y * p2.y);

            angle = Math.acos(dotProduct / (mag1 * mag2));
            angle *= (180d / Math.PI);

            return angle;
        }

        /**
         * Used as part of the routine to find whether the point is part of a loop. Just does a DFS for the other
         * neighbors, given by the list of targets
         * 
         * @param e
         * @param prev
         * @param targets
         * @return
         */
        private boolean dfsTarget(final LinkElement e, final LinkElement prev, final ArrayList<LinkElement> targets) {

            final ArrayDeque<LinkElement> stack = new ArrayDeque<LinkElement>();
            final HashSet<LinkElement> visited = new HashSet<LinkElement>();
            ArrayList<LinkElement> pts = e.linked;
            visited.add(prev);

            for (int i = 0; i < pts.size(); i++) {
                final LinkElement ele = pts.get(i);
                if (visited.contains(ele)) {
                    continue;
                }
                if (targets.contains(ele)) {
                    return true;
                }
                stack.addFirst(ele);
                visited.add(ele);
            }

            while ( !stack.isEmpty()) {
                final LinkElement ele = stack.poll();
                pts = ele.linked;
                for (int i = 0; i < pts.size(); i++) {
                    final LinkElement ele2 = pts.get(i);
                    if (visited.contains(ele2)) {
                        continue;
                    }
                    if (targets.contains(ele2)) {
                        return true;
                    }
                    stack.addFirst(ele2);
                    visited.add(ele2);
                }
            }

            return false;
        }

        /**
         * Checks to see if the point is an overlap point. In reality, it just checks to see if the point is a part of a
         * loop
         * 
         * @param e
         * @return
         */
        private boolean isOverlapPoint(final LinkElement e) {

            final ArrayList<LinkElement> pts = e.linked;

            for (int i = 0; i < pts.size(); i++) {
                final LinkElement next = pts.get(i);
                final ArrayList<LinkElement> list = e.copyList();
                list.remove(next);
                if (dfsTarget(next, e, list)) {
                    return true;
                }
            }

            return false;
        }

        /**
         * A modified Djikstra's to find the shortest path to a tip point
         * 
         * @param e the point in question
         * @param toOrigin the direction we know not to take
         * @return
         */
        private LinkElement shortestEnd(final LinkElement e, final LinkElement toOrigin) {

            final ArrayList<LinkElement> list = e.copyList();
            list.remove(toOrigin);

            LinkElement shortest = null;
            double shortestDist = Double.MAX_VALUE;

            for (final LinkElement l : list) {
                if (l.linked.size() == 1) {
                    return l;
                }
                final PriorityQueue<PriorityElement> pq = new PriorityQueue<PriorityElement>();
                final HashSet<LinkElement> visited = new HashSet<LinkElement>();
                visited.add(e);
                visited.add(l);
                final ArrayList<LinkElement> neighbors = l.copyList();
                neighbors.remove(e);
                for (final LinkElement ele : neighbors) {
                    final double dist = ele.pt.distance(e.pt);
                    pq.add(new PriorityElement(ele, dist));
                    visited.add(ele);
                }
                while ( !pq.isEmpty()) {
                    final PriorityElement pe = pq.poll();
                    final LinkElement le = pe.element;
                    final double depth = pe.depth;
                    if (le.linked.size() == 1) {
                        if (depth < shortestDist) {
                            shortestDist = depth;
                            shortest = l;
                        }
                        break;
                    } else {
                        for (final LinkElement ele : le.linked) {
                            if (visited.add(ele)) {
                                final double dist = ele.pt.distance(le.pt);
                                pq.add(new PriorityElement(ele, depth + dist));
                            }
                        }
                    }

                }
            }

            return shortest;

        }

        private class PriorityElement implements Comparable<PriorityElement> {

            private final LinkElement element;

            private final double depth;

            private PriorityElement(final LinkElement e, final double d) {
                element = e;
                depth = d;
            }

            @Override
            public int compareTo(final PriorityElement o) {
                if (depth > o.depth) {
                    return 1;
                } else if (depth < o.depth) {
                    return -1;
                } else {
                    return 0;
                }
            }
        }

    }

    /**
     * Subclass used mostly just to keep track of higher order branches in the neuron. The longest branch is considered
     * the first order branch, with branches coming off this branch considered second order, etc.
     * 
     * The comparator is flipped from natural ordering so that elements in a Priority Queue are in descending order
     * 
     * @author wangvg
     * 
     */
    private class NeuronLength implements Comparable<NeuronLength> {

        private final Point endPt;

        private double length;

        private int order;

        private NeuronLength(final Point pt, final double l) {
            endPt = pt;
            length = l;
            order = 0;
        }

        @Override
        /**
         * Flipped ordering so that highest priority is highest length
         * 
         * Changed so labeled primary branch has highest priority, with the
         * progenitor haveing second highest priority
         */
        public int compareTo(final NeuronLength o) {

            // Make primary branch highest priority
            /*
             * if(primaryPt != null){ if(endPt.equals(primaryPt)) return -1; else if(o.endPt.equals(primaryPt)) return
             * 1; }
             */
            // Progenitor branch has second highest priority
            // Origin branch has the same prioirty, however with the
            // split point, only one will occur in any given search
            if (progenitorPt != null) {
                if (endPt.equals(progenitorPt) || endPt.equals(origin)) {
                    return -1;
                } else if (o.endPt.equals(progenitorPt) || o.endPt.equals(origin)) {
                    return 1;
                }
            }
            // Longest branch (in the absense of the above) has
            // the third highest priority
            final double l0 = this.length;
            final double l1 = o.length;
            if (l0 > l1) {
                return -1;
            } else if (l0 < l1) {
                return 1;
            } else {
                return 0;
            }
        }

    }

}
