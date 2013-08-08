package gov.nih.mipav.view;

import WildMagic.LibFoundation.Mathematics.Vector3f;


import gov.nih.mipav.model.algorithms.AlgorithmArcLength;
import gov.nih.mipav.model.algorithms.AlgorithmBSmooth;
import gov.nih.mipav.model.algorithms.AlgorithmEdgeLaplacian;
import gov.nih.mipav.model.algorithms.AlgorithmEdgeLaplacianSep;
import gov.nih.mipav.model.algorithms.AlgorithmLapMedianess;
import gov.nih.mipav.model.algorithms.filters.AlgorithmGradientMagnitudeSep;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.model.structures.VOIContour;

import java.awt.Color;
import java.awt.Component;
import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Polygon;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.WindowEvent;
import java.awt.event.WindowListener;
import java.io.IOException;
import java.util.BitSet;
import java.util.Vector;

import javax.swing.SwingUtilities;


/**
 * A Livewire rubberband. It makes segmentation easier by "snapping to" appropriate pixels to outline features. The
 * pixels it snaps to are those with the minimum cost; cost is a function of gradient magnitude, gradient direction, and
 * zero x-crossings. Whenever an anchor is established, a directed graph of costs is created, so that from any pixel in
 * the image, the shortest path from that pixel to the anchor can be drawn. Once the user is satisfied with that path
 * they can put down another anchor, for a new directed cost graph.
 *
 * <p>This is based on the method described by Eric N Mortensen and William A Barrett from Brigham Young University, in
 * their paper "Interactive Segmentation with Intelligent Scissors", Graphical Models and Image Processing, v 60, n 5,
 * September 1998, p.349-384.</p>
 *
 * <p>A description of the color gradient direction is given in The Doctoral Dissertation of Eric N. Mortensen:
 * "Simultaneous Multi-Frame Subpixel Boundary Definition using Toboggan-Based Intelligent Scissors for Image and Movie
 * Editing, Department of Computer Science, Brigham Young University, Provo, Utah, December, 2000, Section 4.3.1.1
 * Domain projection vector: vx. The description of summing gradient magnitudes of the different color bands is
 * described in Section 3.1.1 Discontinuity Measure: Multi-scale Gradient Magnitude. This is in contrast to the earlier
 * 1998 paper which used the maximum of the color band gradient magnitudes.</p>
 *
 * @version  1.0, 4/1/2002
 * @author   Neva Cherniavsky
 * @see      Rubberband
 */
public class RubberbandLivewire extends Rubberband implements ActionListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2803229062633426608L;

    /** DOCUMENT ME! */
    public static int GRADIENT_MAG = 1;

    /** DOCUMENT ME! */
    public static int MEDIALNESS = 2;

    /** DOCUMENT ME! */
    public static int INTENSITY = 3;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    float currContourCost = 0f;

    /** DOCUMENT ME! */
    float prevContourCost = 0f;

    /** DOCUMENT ME! */
    float[] prevXPoints;

    /** DOCUMENT ME! */
    float[] prevYPoints;

    /** DOCUMENT ME! */
    ViewJProgressBar progressBar;

    /** DOCUMENT ME! */
    private ActiveTree activeTree;

    /** DOCUMENT ME! */
    private Vector<Vector3f> clickPoints = new Vector<Vector3f>();

    /** DOCUMENT ME! */
    private VOIContour contour = new VOIContour(false);

    /** DOCUMENT ME! */
    private byte[] costGraph = null;

    /** DOCUMENT ME! */
    private int count;

    /** DOCUMENT ME! */
    private boolean firstPoint = true;

    /** DOCUMENT ME! */
    private float grad_weight = 0.20f; // used to remember gradient weight

    /** DOCUMENT ME! */
    private float[] localCosts = null;

    /** DOCUMENT ME! */
    private float presetHue = -1.0f;

    /** DOCUMENT ME! */
    private BitSet processedIndicies;

    /** DOCUMENT ME! */
    private float[] seededCosts;

    /** DOCUMENT ME! */
    private int seedPoint;

    /** DOCUMENT ME! */
    private int selection;

    /** DOCUMENT ME! */
    private boolean smoothVOIFlag = false;

    /** DOCUMENT ME! */
    private Point tmpPt = new Point(0, 0);

    /** DOCUMENT ME! */
    private Cursor waitCursor = new Cursor(Cursor.WAIT_CURSOR);

    /** DOCUMENT ME! */
    private int xDim, yDim;

    /** DOCUMENT ME! */
    private float[] xDirections;

    /** DOCUMENT ME! */
    private float[] xPoints;

    /** DOCUMENT ME! */
    private int[] xPointsDraw;

    /** DOCUMENT ME! */
    private float[] yDirections;

    /** DOCUMENT ME! */
    private float[] yPoints;

    /** DOCUMENT ME! */
    private int[] yPointsDraw;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Sets up local costs graph by calling AlgorithmGradientMagnitude and AlgorithmEdgeLaplacianSep. Initializes
     * necessary global arrays.
     *
     * @param  component  component to add to
     * @param  selection  GRADIENT_MAG, MEDIALNESS, or INTENSITY
     */
    public RubberbandLivewire(Component component, int selection) {
        super(component);
        component.addMouseMotionListener(this);
        component.addMouseListener(this);
        this.selection = selection;

        int[] extents = new int[2];
        xDim = ((ViewJComponentEditImage) component).getActiveImage().getExtents()[0];
        yDim = ((ViewJComponentEditImage) component).getActiveImage().getExtents()[1];
        extents[0] = xDim;
        extents[1] = yDim;

        int length = xDim * yDim;

        progressBar = new ViewJProgressBar(((ViewJComponentEditImage) component).getActiveImage().getImageName(),
                                           "Livewire: Computing cost function ...", 0, 100, false, this, this);


        // for color images, arrays need to be 4 times bigger
        if (((ViewJComponentEditImage) component).getActiveImage().isColorImage()) {

            // direction of the unit vector of the partial derivative in the x direction
            xDirections = new float[length * 4];

            // direction of the unit vector of the partial derivative in the y direction
            yDirections = new float[length * 4];
        } else {

            // direction of the unit vector of the partial derivative in the x direction
            xDirections = new float[length];

            // direction of the unit vector of the partial derivative in the y direction
            yDirections = new float[length];
        }
        
        localCosts = getLocalCosts( ((ViewJComponentEditImage) component).getActiveImage(), selection, 
                ((ViewJComponentEditImage) component).getActiveImageSliceBuffer(),
                xDirections, yDirections, progressBar );
        
        costGraph = new byte[localCosts.length]; // Graph with arrows from each node to next one

        // A node is a location i in the array; where it
        // points to is the value costGraph[i].
        processedIndicies = new BitSet(localCosts.length); // Boolean indicating if pixel at location
                                                           // has been processed.
        seededCosts = new float[localCosts.length]; // Seeded costs, so looking up a cost that has been
                                                    // set is easy
        activeTree = new ActiveTree(); // List of active nodes to expand; reset on seed(pt) call

        // comment in the below to see localCosts as gradient mag image
        /*
         * ModelImage image = new ModelImage(ModelStorageBase.FLOAT,new int[] {xDim, yDim}, "Test",
         * ((ViewJComponentEditImage)component).getActiveImage().getUserInterface()); try { image.importData(0,
         * localCosts, true); } catch (IOException e) { } new ViewJFrameImage(image, null, new Dimension(200, 200),
         * image.getUserInterface());
         */
        if (progressBar != null) {
            progressBar.dispose();
        }
        // System.err.println("____Done with first part");
    }

    /**
     * Sets up local costs graph by calling AlgorithmGradientMagnitudeSep and AlgorithmEdgeLaplacianSep. Initializes
     * necessary global arrays. Only for non-color images and GRADIENT_MAG path calculation (ie- in RFASegTool).
     *
     * @param  component       component to add to
     * @param  grad_sigmas     DOCUMENT ME!
     * @param  edgelap_sigmas  DOCUMENT ME!
     * @param  kern_weight     DOCUMENT ME!
     * @param  grad_weight     DOCUMENT ME!
     * @param  smoothVOIFlag   whether to smooth the VOI once the livewire selection is done
     */
    public RubberbandLivewire(Component component, float[] grad_sigmas, float[] edgelap_sigmas, float kern_weight,
                              float grad_weight, boolean smoothVOIFlag) {
        super(component);
        component.addMouseMotionListener(this);
        component.addMouseListener(this);
        this.grad_weight = grad_weight;
        this.smoothVOIFlag = smoothVOIFlag;

        int[] extents = new int[2];
        xDim = ((ViewJComponentEditImage) component).getActiveImage().getExtents()[0];
        yDim = ((ViewJComponentEditImage) component).getActiveImage().getExtents()[1];
        extents[0] = xDim;
        extents[1] = yDim;

        int length = xDim * yDim;

        progressBar = new ViewJProgressBar(((ViewJComponentEditImage) component).getActiveImage().getImageName(),
                                           "Livewire: Computing cost function ...", 0, 100, false, this, this);

        int xScreen = Toolkit.getDefaultToolkit().getScreenSize().width;
        int yScreen = 100; // Toolkit.getDefaultToolkit().getScreenSize().height;
        progressBar.setLocation(xScreen / 2, yScreen / 2);
        progressBar.setVisible(true);

        // direction of the unit vector of the partial derivative in the x direction
        xDirections = new float[length];

        // direction of the unit vector of the partial derivative in the y direction
        yDirections = new float[length];

        progressBar.updateValueImmed(30);

        // calculates gradient magnitude and stores in localCosts; stores normalized xDirections
        // and yDirections in the corresponding arrays.
        AlgorithmGradientMagnitudeSep magnitude = new AlgorithmGradientMagnitudeSep(((ViewJComponentEditImage) component).getActiveImage(), grad_sigmas, true, true);
        magnitude.setDirectionNeeded(true);
        magnitude.setNormalized(true);
        magnitude.run();
        localCosts = magnitude.getResultBuffer();
        xDirections = magnitude.getXDerivativeDirections();
        yDirections = magnitude.getYDerivativeDirections();
        progressBar.updateValueImmed(65);

        //BitSet smallKernel;
        BitSet largeKernel;
        AlgorithmEdgeLaplacianSep lap;

        // small kernel edge Laplacian - higher noise, better localization sigmas[0]   = 0.75f; sigmas[1]   = 0.75f; lap
        //         = new AlgorithmEdgeLaplacianSep(null, null, sigmas, true, false, 0, 0); smallKernel =
        // lap.calcZeroXMaskBitset(((ViewJComponentEditImage)component).getActiveImageSliceBuffer(), extents,
        // ((ViewJComponentEditImage)component).getActiveImage().getUserInterface());

        // large kernel edge Laplacian - less noise, poorer localization sigmas[0]   = 2f; sigmas[1]   = 2f;
        lap = new AlgorithmEdgeLaplacianSep(null, null, edgelap_sigmas, true, false);
        progressBar.updateValueImmed(75);
        largeKernel = lap.calcZeroXMaskBitset(((ViewJComponentEditImage) component).getActiveImageSliceBuffer(),
                                              extents);
        progressBar.updateValueImmed(85);

        // invert gradient mag so high gradients mean low cost
        float maximum = -Float.MAX_VALUE;
        float minimum = Float.MAX_VALUE;

        for (int i = 0; i < localCosts.length; i++) {

            if (localCosts[i] > maximum) {
                maximum = localCosts[i];
            }

            if (localCosts[i] < minimum) {
                minimum = localCosts[i];
            }
        }

        maximum = maximum - minimum;
        progressBar.updateValueImmed(90);

        // add in zero edge crossing, weighted slightly higher for the large kernel
        for (int i = 0; i < localCosts.length; i++) {
            float temp = 1f;

            // if (smallKernel.get(i)) temp -= .45;
            // if (largeKernel.get(i)) temp -= .55;
            if (largeKernel.get(i)) {
                temp = 0;
            }

            // leave .27 for the gradient direction, which is calculated on the fly
            float gmCost = (localCosts[i] - minimum) / maximum;

            //            if (gmCost < 0.001f){
            //                gmCost = -5;
            //            }
            localCosts[i] = ((1.0f - gmCost) * kern_weight) + (temp * kern_weight);
        }

        progressBar.updateValueImmed(100);

        costGraph = new byte[localCosts.length]; // Graph with arrows from each node to next one

        // A node is a location i in the array; where it
        // points to is the value costGraph[i].
        processedIndicies = new BitSet(localCosts.length); // Boolean indicating if pixel at location
                                                           // has been processed.
        seededCosts = new float[localCosts.length]; // Seeded costs, so looking up a cost that has been
                                                    // set is easy
        activeTree = new ActiveTree(); // List of active nodes to expand; reset on seed(pt) call

        // comment in the below to see localCosts as gradient mag image
        /*  ModelImage image = new ModelImage(ModelStorageBase.FLOAT,new int[] {xDim, yDim}, "Test",
         * ((ViewJComponentEditImage)component).getActiveImage().getUserInterface());
         * try {   image.importData(0, localCosts, true); } catch (IOException e) { } new ViewJFrameImage(image, null,
         * new Dimension(200, 200), image.getUserInterface());
         */
        progressBar.dispose();
        // System.err.println("done with first part");
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    // ************************************************************************
    // **************************** Action Events *****************************
    // ************************************************************************
    /**
     * Stops the thread, and disposes the progress bar.
     *
     * @param  event  Event that triggered function.
     */
    public void actionPerformed(ActionEvent event) {

        if ((progressBar != null) && !progressBar.isComplete()) {
            progressBar.dispose();
        }
    }

    /**
     * Calls seed on this point to set up costGraph array; then anchors point.
     *
     * @param  pt  Point to anchor.
     */
    public void anchor(Point pt) {
        super.anchor(pt);
        seed(pt);
    }

    /**
     * Draws a line based on the rubberband's last bounds.
     *
     * @param  graphics  graphics to draw in
     */
    public void drawLast(Graphics graphics) {

        // System.err.println("Drawing last");
        // xPoints and yPoints were set when drawNext was called, just before drawLast.
        if ((xPointsDraw != null) && (yPointsDraw != null)) {
            graphics.drawPolyline(xPointsDraw, yPointsDraw, count + 1);
        }
    }

    /**
     * Draws a line by following the graph from the current location back to the anchor point. The anchor point has its
     * parent = -1.
     *
     * @param  graphics  Graphics to draw in
     */
    public void drawNext(Graphics graphics) {

        // System.err.println("drawing next");
        Point pt = new Point(stretchedPt.x, stretchedPt.y);
        pt.x = Math.round(pt.x /
                              (((ViewJComponentEditImage) (component)).getZoomX() *
                                   ((ViewJComponentEditImage) (component)).getResolutionX()));
        pt.y = Math.round(pt.y /
                              (((ViewJComponentEditImage) (component)).getZoomY() *
                                   ((ViewJComponentEditImage) (component)).getResolutionY()));

        //      rename location to mouseIndex
        int mouseIndex = (pt.y * xDim) + pt.x;

        //      rename temp to liveWirePtIndex
        int liveWirePtIndex = mouseIndex + convertGraphToInt(costGraph[mouseIndex]);

        /*
                if (xPoints != null) {
                    prevContourCost = currContourCost;
                    prevXPoints = new float [xPoints.length];
                    prevYPoints = new float [yPoints.length];
                    for (int i = 0; i < xPoints.length; i++) {
                        prevXPoints[i] = xPoints[i];
                        prevYPoints[i] = yPoints[i];
                    }

        //            System.out.println("Prev contour length: " + prevYPoints.length + "   Total cost: " + prevContourCost);
                }
        */

        //        currContourCost = 0f;
        count = 0;

        while (liveWirePtIndex != seedPoint) {
            //            currContourCost += seededCosts[mouseIndex];
            mouseIndex = liveWirePtIndex;
            liveWirePtIndex = mouseIndex + convertGraphToInt(costGraph[mouseIndex]);
            count++;
        }

        //        System.out.println("Curr contour length: " + count + "  Total cost: " + currContourCost);

        xPoints = new float[count + 1];
        yPoints = new float[count + 1];

        xPointsDraw = new int[count + 1];
        yPointsDraw = new int[count + 1];

        count = 0;
        mouseIndex = (pt.y * xDim) + pt.x;
        liveWirePtIndex = mouseIndex + convertGraphToInt(costGraph[mouseIndex]);

        if (liveWirePtIndex == seedPoint) {
            return;
        }

        // System.out.println("_______________________________________________________");
        // System.out.println(" Resolution x = " + ((ViewJComponentEditImage)(component)).getResolutionX());
        while (liveWirePtIndex != seedPoint) {

            // scale up to Graphics's coordinate system
            xPoints[count] = ((mouseIndex % xDim) * ((ViewJComponentEditImage) (component)).getZoomX() *
                                  ((ViewJComponentEditImage) (component)).getResolutionX()) + 1;
            yPoints[count] = ((mouseIndex / xDim) * ((ViewJComponentEditImage) (component)).getZoomY() *
                                  ((ViewJComponentEditImage) (component)).getResolutionY()) + 1;

            // System.out.println(" Draw next x: = " + xPoints[count] + " y = " + yPoints[count] + "  seed = " +
            // seedPoint + " location = " +  location);
            mouseIndex = liveWirePtIndex;
            liveWirePtIndex = mouseIndex + convertGraphToInt(costGraph[mouseIndex]);
            count++;
        }

        // scale up to Graphics's coordinate system
        xPoints[count] = ((mouseIndex % xDim) *
                              (((ViewJComponentEditImage) (component)).getZoomX() *
                                   ((ViewJComponentEditImage) (component)).getResolutionX())) + 1;
        yPoints[count] = ((mouseIndex / xDim) *
                              (((ViewJComponentEditImage) (component)).getZoomY() *
                                   ((ViewJComponentEditImage) (component)).getResolutionY())) + 1;


        for (int c = 0; c < xPoints.length; c++) {
            //            System.out.println("Drawing point  X: " + xPoints[c] + "  Y: " + yPoints[c]);

            xPointsDraw[c] = Math.round(xPoints[c]);
            yPointsDraw[c] = Math.round(yPoints[c]);

        }

        graphics.drawPolyline(xPointsDraw, yPointsDraw, count + 1);

        /*

                // compute the RMS distance between corresponding points
                // the thought is that when the contour jumps we should see
                // a significant change in the RMS distance
                float totalSquaredDistance = 0f;
                float rmsDistance = 0f;
                float dx, dy;
                int numContourPoints = 0;
                if (prevXPoints != null) {
                    if (prevXPoints.length < xPoints.length) {
                        for(int idx = 0; idx < prevXPoints.length; idx++) {
                            dx = prevXPoints[idx] - xPoints[idx];
                            dy = prevYPoints[idx] - yPoints[idx];
                            totalSquaredDistance += dx * dx + dy * dy;
                            numContourPoints++;
        //                  System.out.print(idx + "\t" + prevXPoints[idx] + "  " + prevYPoints[idx] + "    ");
        //                  System.out.print(xPoints[idx] + "  " + yPoints[idx] + "    ");
        //                  System.out.println(Math.sqrt(dx * dx + dy * dy));
                        }
        //              for(int idx = prevXPoints.length; idx < xPoints.length; idx++) {
        //                  System.out.println(idx + "\t" + "                "  + xPoints[idx] + "  " + yPoints[idx]);
        //              }
                    } else {
                        for(int idx = 0; idx < xPoints.length; idx++) {
                            dx = prevXPoints[idx] - xPoints[idx];
                            dy = prevYPoints[idx] - yPoints[idx];
                            totalSquaredDistance += Math.sqrt(dx * dx + dy * dy);
                            numContourPoints++;
        //                  System.out.print(idx + "\t" + prevXPoints[idx] + "  " + prevYPoints[idx] + "    ");
        //                  System.out.print(xPoints[idx] + "  " + yPoints[idx] + "    ");
        //                  System.out.println(Math.sqrt(dx * dx + dy * dy));
                        }
        //              for(int idx = xPoints.length; idx < prevXPoints.length; idx++) {
        //                  System.out.println(idx + "\t" + prevXPoints[idx] + "  " + prevYPoints[idx]);
        //              }
                    }
                }

                // We have the total squared distance between corresponding contour points
                // find the square root of the mean
                rmsDistance = (float)Math.sqrt(totalSquaredDistance / numContourPoints);
                if (rmsDistance > 2.0 && (Math.abs(prevYPoints.length - count) > 5)) {
                    System.out.println("Prev contour length: " + prevYPoints.length + "   Total cost: " + prevContourCost);
                    System.out.println("Curr contour length: " + count + "  Total cost: " + currContourCost);
                    System.out.println("RMS distance: " + rmsDistance);
                    System.out.println("************************************************");
                    System.out.println();
                }
        */


    }

    public static float[] getLocalCosts( ModelImage kImage, int selection, float[] activeSliceBuffer, float[] xDirections, float[] yDirections, ViewJProgressBar progressBar )
    {
        float[] localCosts = null;

        int[] extents = new int[2];
        int xDim = kImage.getExtents()[0];
        int yDim = kImage.getExtents()[1];
        extents[0] = xDim;
        extents[1] = yDim;

        int length = xDim * yDim;
        

        if (selection == GRADIENT_MAG) {
            if ( progressBar != null )
            {
                progressBar.setVisible(true);
            }


            // AlgorithmGradientMagnitude magnitude = new AlgorithmGradientMagnitude(null, new float[] {1.75f, 1.75f},
            // true, false);
            ModelImage mi;
            if (kImage.isColorImage()) {
                mi = new ModelImage(ModelStorageBase.FLOAT, new int[]{4*xDim, yDim}, kImage.getImageName());    
            }
            else {
                mi = new ModelImage(ModelStorageBase.FLOAT, new int[]{xDim, yDim}, kImage.getImageName());
            }
            try{
                mi.importData(0, activeSliceBuffer, true);
            }catch(IOException e){
                MipavUtil.displayError("RubberbandLiveWire: IOException on extracting active slice" + ".importData(0, ((ViewJComponentEditImage) component).getActiveImageSliceBuffer(), true)" + e);

            }
            AlgorithmGradientMagnitudeSep magnitude = new AlgorithmGradientMagnitudeSep(mi,
                                                                                        new float[] { 1.75f, 1.75f },
                                                                                        true, true);
            magnitude.setDirectionNeeded(true);
            if ( progressBar != null )
            {
                progressBar.updateValueImmed(10);
            }
            if (kImage.isColorImage()) {
                magnitude.setNormalized(false);
                magnitude.run();
                localCosts = magnitude.getResultBuffer();
                xDirections = magnitude.getXDerivativeDirections();
                yDirections = magnitude.getYDerivativeDirections();
                if ( progressBar != null )
                {
                    progressBar.updateValueImmed(20);
                }
                float[] localCostsTemp = new float[length];

                for (int i = 0; i < localCostsTemp.length; i++) {
                    localCostsTemp[i] = (float) Math.sqrt((localCosts[(4 * i) + 1] * localCosts[(4 * i) + 1]) +
                                                          (localCosts[(4 * i) + 2] * localCosts[(4 * i) + 2]) +
                                                          (localCosts[(4 * i) + 3] * localCosts[(4 * i) + 3]));
                }

                localCosts = new float[length];

                for (int i = 0; i < localCostsTemp.length; i++) {
                    localCosts[i] = localCostsTemp[i];
                }

                if ( progressBar != null )
                {
                    progressBar.updateValueImmed(25);
                }
                localCostsTemp = null;

                float R2;
                float G2;
                float B2;
                float IxMax;
                float IyMax;
                int sign;
                float IxR;
                float IyR;
                float IxG;
                float IyG;
                float IxB;
                float IyB;
                float[] xDirectionsTemp = new float[length];
                float[] yDirectionsTemp = new float[length];
                float Ix;
                float Iy;
                float I2;
                int mod = length / 35;

                for (int i = 0; i < length; i++) {
                    R2 = (xDirections[(4 * i) + 1] * xDirections[(4 * i) + 1]) +
                         (yDirections[(4 * i) + 1] * yDirections[(4 * i) + 1]);
                    G2 = (xDirections[(4 * i) + 2] * xDirections[(4 * i) + 2]) +
                         (yDirections[(4 * i) + 2] * yDirections[(4 * i) + 2]);
                    B2 = (xDirections[(4 * i) + 3] * xDirections[(4 * i) + 3]) +
                         (yDirections[(4 * i) + 3] * yDirections[(4 * i) + 3]);

                    if ((R2 > G2) && (R2 > B2)) {
                        IxMax = xDirections[(4 * i) + 1];
                        IyMax = yDirections[(4 * i) + 1];
                    } else if ((G2 > R2) && (G2 > B2)) {
                        IxMax = xDirections[(4 * i) + 2];
                        IyMax = yDirections[(4 * i) + 2];
                    } else {
                        IxMax = xDirections[(4 * i) + 3];
                        IyMax = yDirections[(4 * i) + 3];
                    }

                    if (((xDirections[(4 * i) + 1] * IxMax) + (yDirections[(4 * i) + 1] * IyMax)) > 0) {
                        sign = 1;
                    } else {
                        sign = -1;
                    }

                    IxR = sign * xDirections[(4 * i) + 1];
                    IyR = sign * yDirections[(4 * i) + 1];

                    if (((xDirections[(4 * i) + 2] * IxMax) + (yDirections[(4 * i) + 2] * IyMax)) > 0) {
                        sign = 1;
                    } else {
                        sign = -1;
                    }

                    IxG = sign * xDirections[(4 * i) + 2];
                    IyG = sign * yDirections[(4 * i) + 2];

                    if (((xDirections[(4 * i) + 3] * IxMax) + (yDirections[(4 * i) + 3] * IyMax)) > 0) {
                        sign = 1;
                    } else {
                        sign = -1;
                    }

                    IxB = sign * xDirections[(4 * i) + 3];
                    IyB = sign * yDirections[(4 * i) + 3];
                    R2 = (float) Math.sqrt(R2);
                    G2 = (float) Math.sqrt(G2);
                    B2 = (float) Math.sqrt(B2);
                    Ix = (R2 * IxR) + (G2 * IxG) + (B2 * IxB);
                    Iy = (R2 * IyR) + (G2 * IyG) + (B2 * IyB);
                    I2 = (float) Math.sqrt((Ix * Ix) + (Iy * Iy));
                    xDirectionsTemp[i] = Ix / I2;
                    yDirectionsTemp[i] = Iy / I2;

                    if ((length % mod) == 0) {
                        if ( progressBar != null )
                        {
                            progressBar.updateValueImmed(25 + (i * 35 / length));
                        }
                    }
                }

                xDirections = new float[length];
                yDirections = new float[length];

                for (int i = 0; i < length; i++) {
                    xDirections[i] = xDirectionsTemp[i];
                    yDirections[i] = yDirectionsTemp[i];
                }

                if ( progressBar != null )
                {
                    progressBar.updateValueImmed(60);
                }
                xDirectionsTemp = null;
                yDirectionsTemp = null;
            } // if (((ViewJComponentEditImage)component).getActiveImage().isColorImage())
            else { // not color

                // calculates gradient magnitude and stores in localCosts; stores normalized xDirections
                // and yDirections in the corresponding arrays.
                magnitude.setNormalized(true);
                magnitude.run();
                localCosts = magnitude.getResultBuffer();
                xDirections = magnitude.getXDerivativeDirections();
                yDirections = magnitude.getYDerivativeDirections();
                if ( progressBar != null )
                {
                    progressBar.updateValueImmed(65);
                }

            } // not color

            float[] sigmas = new float[2];
            //BitSet smallKernel;
            BitSet largeKernel;
            AlgorithmEdgeLaplacian lap;

            if (kImage.isColorImage()) {
                //BitSet smallKernelR;
                BitSet largeKernelR;
                //BitSet smallKernelG;
                BitSet largeKernelG;
                //BitSet smallKernelB;
                BitSet largeKernelB;
                //smallKernel = new BitSet(length);
                largeKernel = new BitSet(length);

                float[] colorBuffer = new float[4 * xDim * yDim];
                colorBuffer = activeSliceBuffer;

                float[] singleBuffer = new float[xDim * yDim];

                for (int i = 0; i < singleBuffer.length; i++) {
                    singleBuffer[i] = colorBuffer[(4 * i) + 1];
                }

                //sigmas[0] = 0.75f;
                //sigmas[1] = 0.75f;
                //lap = new AlgorithmEdgeLaplacian(null, null, sigmas, true, false);
                //smallKernelR = lap.calcZeroXMaskBitset(singleBuffer, extents);

                // large kernel edge Laplacian - less noise, poorer localization
                sigmas[0] = 1.25f;
                sigmas[1] = 1.25f;
                lap = new AlgorithmEdgeLaplacian(null, null, sigmas, true, false);
                largeKernelR = lap.calcZeroXMaskBitset(singleBuffer, extents);
                if ( progressBar != null )
                {
                    progressBar.updateValueImmed(65);
                }
                for (int i = 0; i < singleBuffer.length; i++) {
                    singleBuffer[i] = colorBuffer[(4 * i) + 2];
                }

                //sigmas[0] = 0.75f;
                //sigmas[1] = 0.75f;
                //lap = new AlgorithmEdgeLaplacian(null, null, sigmas, true, false);
                //smallKernelG = lap.calcZeroXMaskBitset(singleBuffer, extents);

                // large kernel edge Laplacian - less noise, poorer localization
                sigmas[0] = 1.25f;
                sigmas[1] = 1.25f;
                lap = new AlgorithmEdgeLaplacian(null, null, sigmas, true, false);
                largeKernelG = lap.calcZeroXMaskBitset(singleBuffer, extents);
                if ( progressBar != null )
                {
                    progressBar.updateValueImmed(70);
                }
                for (int i = 0; i < singleBuffer.length; i++) {
                    singleBuffer[i] = colorBuffer[(4 * i) + 3];
                }

                //sigmas[0] = 0.75f;
                //sigmas[1] = 0.75f;
                //lap = new AlgorithmEdgeLaplacian(null, null, sigmas, true, false);
                //smallKernelB = lap.calcZeroXMaskBitset(singleBuffer, extents);

                // large kernel edge Laplacian - less noise, poorer localization
                sigmas[0] = 1.25f;
                sigmas[1] = 1.25f;
                lap = new AlgorithmEdgeLaplacian(null, null, sigmas, true, false);
                largeKernelB = lap.calcZeroXMaskBitset(singleBuffer, extents);
                if ( progressBar != null )
                {
                    progressBar.updateValueImmed(75);
                }
                for (int i = 0; i < length; i++) {

                    //if ((smallKernelR.get(i)) || (smallKernelG.get(i)) || (smallKernelB.get(i))) {
                        //smallKernel.set(i);
                    //} else {
                        //smallKernel.clear(i);
                    //}

                    if ((largeKernelR.get(i)) || (largeKernelG.get(i)) || (largeKernelB.get(i))) {
                        largeKernel.set(i);
                    } else {
                        largeKernel.clear(i);
                    }
                }

                if ( progressBar != null )
                {
                    progressBar.updateValueImmed(85);
                }
                //smallKernelR = null;
                largeKernelR = null;
                //smallKernelG = null;
                largeKernelG = null;
                //smallKernelB = null;
                largeKernelB = null;
                colorBuffer = null;
                singleBuffer = null;
            } // if (((ViewJComponentEditImage)component).getActiveImage().isColorImage())
            else { // not color

                // small kernel edge Laplacian - higher noise, better localization sigmas[0]   = 0.75f; sigmas[1]   =
                // 0.75f; lap         = new AlgorithmEdgeLaplacianSep(null, null, sigmas, true, false, 0, 0);
                // smallKernel =
                // lap.calcZeroXMaskBitset(((ViewJComponentEditImage)component).getActiveImageSliceBuffer(), extents);

                // large kernel edge Laplacian - less noise, poorer localization
                sigmas[0] = 1.85f;
                sigmas[1] = 1.85f;
                lap = new AlgorithmEdgeLaplacian(null, null, sigmas, true, true);
                lap.setZeroDetectionType(AlgorithmEdgeLaplacian.MARCHING_SQUARES);
                if ( progressBar != null )
                {
                    progressBar.updateValueImmed(75);
                }
                largeKernel = lap.calcZeroXMaskBitset(activeSliceBuffer,
                                                      extents);
                if ( progressBar != null )
                {
                    progressBar.updateValueImmed(85);
                }
            } // not color

            // invert gradient mag so high gradients mean low cost
            float maximum = -Float.MAX_VALUE;
            float minimum = Float.MAX_VALUE;

            for (int i = 0; i < localCosts.length; i++) {

                if (localCosts[i] > maximum) {
                    maximum = localCosts[i];
                }

                if (localCosts[i] < minimum) {
                    minimum = localCosts[i];
                }
            }

            maximum = maximum - minimum;
            if ( progressBar != null )
            {
                progressBar.updateValueImmed(90);
            }
            // add in zero edge crossing, weighted slightly higher for the large kernel
            for (int i = 0; i < localCosts.length; i++) {
                float temp = 1f;

                // if (smallKernel.get(i)) temp -= .45;
                // if (largeKernel.get(i)) temp -= .55;
                if (largeKernel.get(i)) {
                    temp = 0;
                }

                // leave .27 for the gradient direction, which is calculated on the fly
                float gmCost = (localCosts[i] - minimum) / maximum;

                //                if (gmCost < 0.001f){
                //                    gmCost = -5;
                //                }
                localCosts[i] = ((1.0f - gmCost) * 0.40f) + (temp * 0.40f);
            }

            if ( progressBar != null )
            {
                progressBar.updateValueImmed(100);
            }
            
            mi.disposeLocal();
            mi = null;
        } else if (selection == MEDIALNESS) {
            if ( progressBar != null )
            {
                progressBar.setVisible(false);
            }
            AlgorithmLapMedianess medialness = new AlgorithmLapMedianess(null, new float[] { 1.0f, 1.0f }, true, false,
                                                                         1f, false);

            // progressBar.updateValueImmed(25);
            if (kImage.isColorImage()) {
                float[] localCostsR;
                float[] localCostsG;
                float[] localCostsB;
                localCosts = new float[length];

                float[] colorBuffer = new float[4 * length];
                colorBuffer = activeSliceBuffer;

                float[] singleBuffer = new float[length];

                for (int i = 0; i < singleBuffer.length; i++) {
                    singleBuffer[i] = colorBuffer[(4 * i) + 1];
                }

                // progressBar.updateValueImmed(30);
                localCostsR = medialness.calcInBuffer2D(singleBuffer, extents);

                for (int i = 0; i < singleBuffer.length; i++) {
                    singleBuffer[i] = colorBuffer[(4 * i) + 2];
                }

                // progressBar.updateValueImmed(35);
                localCostsG = medialness.calcInBuffer2D(singleBuffer, extents);

                for (int i = 0; i < singleBuffer.length; i++) {
                    singleBuffer[i] = colorBuffer[(4 * i) + 3];
                }

                // progressBar.updateValueImmed(40);
                localCostsB = medialness.calcInBuffer2D(singleBuffer, extents);

                for (int i = 0; i < length; i++) {
                    localCosts[i] = Math.max(localCostsR[i], Math.max(localCostsG[i], localCostsB[i]));
                }

                // progressBar.updateValueImmed(45);
                colorBuffer = null;
                singleBuffer = null;
                localCostsR = null;
                localCostsG = null;
                localCostsB = null;
            } // if (((ViewJComponentEditImage)component).getActiveImage().isColorImage())
            else { // not color
                localCosts = medialness.calcInBuffer2D(activeSliceBuffer,
                                                       extents);
                // progressBar.updateValueImmed(45);
            } // else not color

            /*
             * AlgorithmFrequencyFilter freq = new
             * AlgorithmFrequencyFilter(((ViewJComponentEditImage)component).getActiveImage(),
             * false, false); localCosts = freq.calc2DMedialness();
             */
            // invert medialness so high medialness mean low cost
            float maximum = -Float.MAX_VALUE;
            float minimum = Float.MAX_VALUE;

            for (int i = 0; i < localCosts.length; i++) {

                if (localCosts[i] > maximum) {
                    maximum = localCosts[i];
                }

                if (localCosts[i] < minimum) {
                    minimum = localCosts[i];
                }
            }

            maximum = maximum - minimum;
            // progressBar.updateValueImmed(70);

            for (int i = 0; i < localCosts.length; i++) {
                localCosts[i] = (1 - ((localCosts[i] - minimum) / maximum));
            }
            // progressBar.updateValueImmed(100);
        } else if (selection == INTENSITY) {
            if ( progressBar != null )
            {
                progressBar.setVisible(true);
            }
            localCosts = activeSliceBuffer;
/*
            if (kImage.isColorImage()) {
                float[] localCostsTemp = new float[length];

                for (int i = 0; i < length; i++) {
                    localCostsTemp[i] = localCosts[(4 * i) + 1] + localCosts[(4 * i) + 2] + localCosts[(4 * i) + 3];
                }

                if ( progressBar != null )
                {
                    progressBar.updateValueImmed(25);
                }
                localCosts = new float[length];

                for (int i = 0; i < length; i++) {
                    localCosts[i] = localCostsTemp[i];
                }

                localCostsTemp = null;
                if ( progressBar != null )
                {
                    progressBar.updateValueImmed(50);
                }
            }
*/
            // invert intensity so high intensity means low cost
            float maximum = -Float.MAX_VALUE;
            float minimum = Float.MAX_VALUE;

            for (int i = 0; i < localCosts.length; i++) {

                if (localCosts[i] > maximum) {
                    maximum = localCosts[i];
                }

                if (localCosts[i] < minimum) {
                    minimum = localCosts[i];
                }
            }

            maximum = maximum - minimum;
            if ( progressBar != null )
            {
                progressBar.updateValueImmed(75);
            }

            for (int i = 0; i < localCosts.length; i++) {
                localCosts[i] = (1 - ((localCosts[i] - minimum) / maximum));
            }

            if ( progressBar != null )
            {
                progressBar.updateValueImmed(100);
            }
        }

        return localCosts;
    }
    
    
    /**
     * Unchanged.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseClicked(MouseEvent mouseEvent) { }

    /**
     * Makes a contour out of the curve drawn.
     *
     * @param  mouseEvent  event that triggered this function
     */
    public void mouseDragged(MouseEvent mouseEvent) {
        /*     Point pt = mouseEvent.getPoint();
         *
         * mouseDragged = true;
         *
         * tmpPt.x = Math.round( getEnd().x / (((ViewJComponentEditImage)(component)).getZoomX() *
         * ((ViewJComponentEditImage)(component)).getResolutionX()));  tmpPt.y = Math.round( getEnd().y /
         * (((ViewJComponentEditImage)(component)).getZoomY() *
         * ((ViewJComponentEditImage)(component)).getResolutionY()));
         *
         *
         * if (isActive() && testPoint(pt) && (xS != tmpPt.x || yS != tmpPt.y) ) {      if (xPoints != null) {
         * for (int i=0; i<xPoints.length; i++) {              // need to scale back down to image space.
         * contour.addElement(Math.round(xPoints[xPoints.length-1-i] /
         * (((ViewJComponentEditImage)(component)).getZoomX() *
         * ((ViewJComponentEditImage)(component)).getResolutionX())),
         * Math.round(yPoints[yPoints.length-1-i] / (((ViewJComponentEditImage)(component)).getZoomY() *
         * ((ViewJComponentEditImage)(component)).getResolutionY())),
         * ((ViewJComponentEditImage)(component)).getSlice());          }      }      else {
         * contour.addElement(xS, yS, ((ViewJComponentEditImage)(component)).getSlice());      }      Graphics g =
         * super.component.getGraphics();      if (firstPoint) {          anchor(pt);          firstPoint = false ;
         * }      else {          g.setXORMode(Color.black);          g.setColor(Color.yellow);          if
         * (contour.size() > 2) {              if (contour.size() != 3) {                  g.drawRect((int)anchorPt.x-3,
         * (int)anchorPt.y-3, 6, 6);              }              g.drawRect(pt.x-3, pt.y-3, 6, 6);          }
         * stretch(pt);          end(pt);          anchor(pt);      }  }*/
    }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseEntered(MouseEvent mouseEvent) { }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseExited(MouseEvent mouseEvent) { }

    /**
     * Stretches if the VOI is active.
     *
     * @param  mouseEvent  event that triggered this function
     */
    public void mouseMoved(MouseEvent mouseEvent) {
        Point pt = mouseEvent.getPoint();

        // System.err.println("Active is: " + isActive() + " firstPoint is: " + firstPoint + " testPoint is: " +
        // testPoint(pt));
        if (isActive() && (firstPoint == false) && testPoint(pt)) {
            stretch(pt);
        }
    }

    /**
     * Unchanged.
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mousePressed(MouseEvent mouseEvent) { }

    /**
     * Makes a VOI out of the contours upon a mouse release event.
     *
     * @param  mouseEvent  event that triggered function
     */
    public void mouseReleased(MouseEvent mouseEvent) {
        VOI newVOI;
        Point pt = mouseEvent.getPoint();

        int xxS = Math.round(pt.x /
                                 (((ViewJComponentEditImage) (component)).getZoomX() *
                                      ((ViewJComponentEditImage) (component)).getResolutionX()));
        int yyS = Math.round(pt.y /
                                 (((ViewJComponentEditImage) (component)).getZoomY() *
                                      ((ViewJComponentEditImage) (component)).getResolutionY()));
        int index;
        int colorID;
        ViewVOIVector VOIs;
        int i;
        int nVOI;
        String name;
        boolean open;
        boolean done;

        // User clicked with right mouse button. Find where she last clicked and remove points to that click point
        // (undo last click, basically)
        if (isActive() && (clickPoints.size() > 1) && SwingUtilities.isRightMouseButton(mouseEvent)) {

            for (i = contour.size() - 1;
                     (i >= 0) && !contour.elementAt(i).equals(clickPoints.elementAt(clickPoints.size() - 2)); i--) {
                contour.removeElementAt(i);
            }

            clickPoints.removeElementAt(clickPoints.size() - 1);

            // reanchor contour at last clicked point
            Point newAnchor = new Point((int) ((int) ((Vector3f) (contour.lastElement())).X *
                                                   (((ViewJComponentEditImage) (component)).getZoomX() *
                                                        ((ViewJComponentEditImage) (component)).getResolutionX())),
                                        (int) ((int) ((Vector3f) (contour.lastElement())).Y *
                                                   (((ViewJComponentEditImage) (component)).getZoomY() *
                                                        ((ViewJComponentEditImage) (component)).getResolutionY())));


            end(newAnchor);
            anchor(newAnchor);

            Graphics g = super.component.getGraphics();
            ((ViewJComponentEditImage) component).update(g);
            g.setColor(Color.yellow);
            /*
            contour.drawSelf(((ViewJComponentEditImage) (component)).getZoomX(),
                             ((ViewJComponentEditImage) (component)).getZoomY(),
                             ((ViewJComponentEditImage) (component)).getResolutionX(),
                             ((ViewJComponentEditImage) (component)).getResolutionY(), 0f, 0f,
                             ((ViewJComponentEditImage) (component)).getActiveImage().getFileInfo(0).getResolutions(),
                             ((ViewJComponentEditImage) (component)).getActiveImage().getFileInfo(0).getUnitsOfMeasure(),
                             ((ViewJComponentEditImage) (component)).getOrientation(), g, false, Preferences.getVOIThickness());
*/
            // draw boxes around the first and last points on the curve
            g.setXORMode(Color.black);

            int ptx = (int) ((int) ((Vector3f) (contour.elementAt(0))).X *
                                 (((ViewJComponentEditImage) (component)).getZoomX() *
                                      ((ViewJComponentEditImage) (component)).getResolutionX()));
            int pty = (int) ((int) ((Vector3f) (contour.elementAt(0))).Y *
                                 (((ViewJComponentEditImage) (component)).getZoomY() *
                                      ((ViewJComponentEditImage) (component)).getResolutionY()));
            g.drawRect(ptx - 3, pty - 3, 6, 6);
            ptx = (int) ((int) ((Vector3f) (contour.lastElement())).X *
                             (((ViewJComponentEditImage) (component)).getZoomX() *
                                  ((ViewJComponentEditImage) (component)).getResolutionX()));
            pty = (int) ((int) ((Vector3f) (contour.lastElement())).Y *
                             (((ViewJComponentEditImage) (component)).getZoomY() *
                                  ((ViewJComponentEditImage) (component)).getResolutionY()));

            if (clickPoints.size() > 1) {
                g.drawRect(ptx - 3, pty - 3, 6, 6);
            }

            open = true;
            done = false;
        }
        // User clicked again on last point.  Finish off the contour, leave it open
        else if (isActive() && (contour.size() > 2) &&
                     (distance(xxS, (int) ((Vector3f) (contour.lastElement())).X, yyS,
                                   (int) ((Vector3f) (contour.lastElement())).Y) < 3 /*&& mouseDragged == false*/)) {
            open = true;
            done = true;
        }
        // User clicked back again on first point.  Finish off contour and close it.
        else if (isActive() && (contour.size() > 2) &&
                     (distance(xxS, (int) ((Vector3f) (contour.elementAt(0))).X, yyS,
                                   (int) ((Vector3f) (contour.elementAt(0))).Y) < 3)) {
            open = false;
            done = true;
        } else {
            open = false;
            done = false;
        }

        if (done) {
            // System.err.println("Livewire done: " + done + " open: " + open);

            // the below only happens if it's time to close off the contour

            if (!open) {

                // add in last contour before contour is closed off.
                if (xPoints != null) {

                    for (i = 0; i < xPoints.length; i++) {

                        // need to scale back down to image space.
                        // System.out.println("Hey");
                        contour.addElement((xPoints[xPoints.length - 1 - i]) /
                                               (((ViewJComponentEditImage) (component)).getZoomX() *
                                                    ((ViewJComponentEditImage) (component)).getResolutionX()),
                                           (yPoints[yPoints.length - 1 - i]) /
                                               (((ViewJComponentEditImage) (component)).getZoomY() *
                                                    ((ViewJComponentEditImage) (component)).getResolutionY()),
                                           ((ViewJComponentEditImage) (component)).getSlice());
                    }
                }
            }

            ModelImage image = ((ViewJComponentEditImage) (component)).getActiveImage();

            int constant;

            if (open == true) {
                constant = VOI.POLYLINE;
            } else {
                contour.setClosed(true);
                constant = VOI.CONTOUR;
            }

            if (((ViewJComponentEditImage) (component)).getVOIHandler().isNewVoiNeeded(constant)) {

                try {
                    VOIs = image.getVOIs();
                    index = VOIs.size();
                    colorID = 0;

                    if (image.getVOIs().size() > 0) {
                        colorID = ((VOI) (image.getVOIs().lastElement())).getID() + 1;
                    }

                    nVOI = VOIs.size();
                    name = "livewire" + (index + 1);

                    int test;


                    do {
                        test = 0;

                        for (i = 0; i < nVOI; i++) {

                            if (name.equals(VOIs.VOIAt(i).getName())) {
                                index++;
                                name = "livewire" + (index + 1);
                                test = 1;
                            }
                        }
                    } while (test == 1);

                    /*
                     * do{ test = 0; for (i = 0; i < nVOI; i++) {  if (colorID ==((int)VOIs.VOIAt(i).getID())) {
                     * colorID++;      test=1;  } } } while(test==1);
                     */
                    newVOI = new VOI((short) colorID, name, constant, presetHue);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to form new livewire");
                    ((ViewJComponentEditImage) (component)).setCursorMode(ViewJComponentEditImage.DEFAULT);

                    return;
                }

                if (smoothVOIFlag) {
                    AlgorithmBSmooth smoothAlgo;
                    float[] xPts = null;
                    float[] yPts = null;
                    Polygon gon = contour.exportPolygon();
                    int nPts;

                    xPts = new float[gon.npoints + 5];
                    yPts = new float[gon.npoints + 5];

                    xPts[0] = gon.xpoints[gon.npoints - 2];
                    yPts[0] = gon.ypoints[gon.npoints - 2];

                    xPts[1] = gon.xpoints[gon.npoints - 1];
                    yPts[1] = gon.ypoints[gon.npoints - 1];

                    for (i = 0; i < gon.npoints; i++) {
                        xPts[i + 2] = gon.xpoints[i];
                        yPts[i + 2] = gon.ypoints[i];
                    }

                    xPts[gon.npoints + 2] = gon.xpoints[0];
                    yPts[gon.npoints + 2] = gon.ypoints[0];

                    xPts[gon.npoints + 3] = gon.xpoints[1];
                    yPts[gon.npoints + 3] = gon.ypoints[1];

                    xPts[gon.npoints + 4] = gon.xpoints[2];
                    yPts[gon.npoints + 4] = gon.ypoints[2];

                    AlgorithmArcLength arcLenAlgo = new AlgorithmArcLength(xPts, yPts);
                    nPts = Math.round(arcLenAlgo.getTotalArcLength() / 3);

                    contour.setClosed(!open);
                    contour.setActive(true);
                    newVOI.importCurve(contour);

                    try {
                        smoothAlgo = new AlgorithmBSmooth(image, newVOI, nPts, true);
                        smoothAlgo.run();
                    } catch (OutOfMemoryError x) {
                        MipavUtil.displayError("Smooth: unable to allocate enough memory");

                        return;
                    }

                    Color voiColor = newVOI.getColor();
                    newVOI = smoothAlgo.getResultVOI();
                    newVOI.setColor(voiColor);
                } else {
                    contour.trimPoints(Preferences.getTrimVoi(), Preferences.getTrimAdjacient());
                    contour.setClosed(!open);
                    newVOI.importCurve(contour);
                }

                image.registerVOI(newVOI);
                image.notifyImageDisplayListeners();

                if (mouseEvent.isShiftDown() != true) {
                    ((ViewJComponentEditImage) (component)).setCursorMode(ViewJComponentEditImage.DEFAULT);
                }

                ((ViewJComponentEditImage) (component)).getVOIHandler().setVOI_IDs(newVOI.getID(), newVOI.getUID());

                // setup for next time this class is used
                firstPoint = true;
                xPoints = null;
                yPoints = null;

                try {
                    contour = new VOIContour(false);
                    clickPoints = new Vector<Vector3f>();
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to form new livewire");
                    ((ViewJComponentEditImage) (component)).setCursorMode(ViewJComponentEditImage.DEFAULT);

                    return;
                }
            } // if (((ViewJComponentEditImage)(component)).isNewVoiNeeded(constant))
            else {

                // get selected VOI
                VOIs = image.getVOIs();
                nVOI = VOIs.size();

                for (i = 0; i < nVOI; i++) {

                    if (VOIs.VOIAt(i).getID() == ((ViewJComponentEditImage) (component)).getVOIHandler().getVOI_ID()) {

                        if (VOIs.VOIAt(i).getCurveType() == constant) {

                            if (smoothVOIFlag) {
                                AlgorithmBSmooth smoothAlgo;
                                float[] xPts = null;
                                float[] yPts = null;
                                Polygon gon = contour.exportPolygon();
                                int nPts;

                                xPts = new float[gon.npoints + 5];
                                yPts = new float[gon.npoints + 5];

                                xPts[0] = gon.xpoints[gon.npoints - 2];
                                yPts[0] = gon.ypoints[gon.npoints - 2];

                                xPts[1] = gon.xpoints[gon.npoints - 1];
                                yPts[1] = gon.ypoints[gon.npoints - 1];

                                for (int j = 0; j < gon.npoints; j++) {
                                    xPts[j + 2] = gon.xpoints[j];
                                    yPts[j + 2] = gon.ypoints[j];
                                }

                                xPts[gon.npoints + 2] = gon.xpoints[0];
                                yPts[gon.npoints + 2] = gon.ypoints[0];

                                xPts[gon.npoints + 3] = gon.xpoints[1];
                                yPts[gon.npoints + 3] = gon.ypoints[1];

                                xPts[gon.npoints + 4] = gon.xpoints[2];
                                yPts[gon.npoints + 4] = gon.ypoints[2];

                                AlgorithmArcLength arcLenAlgo = new AlgorithmArcLength(xPts, yPts);
                                nPts = Math.round(arcLenAlgo.getTotalArcLength() / 3);

                                contour.setClosed(!open);
                                contour.setActive(true);

                                // VOIs.VOIAt(i).getCurves()[((ViewJComponentEditImage)(component)).getSlice()].addElement(contour);
                                VOIs.VOIAt(i).importCurve(contour);

                                try {
                                    smoothAlgo = new AlgorithmBSmooth(image, VOIs.VOIAt(i), nPts, true);
                                    smoothAlgo.run();
                                } catch (OutOfMemoryError x) {
                                    MipavUtil.displayError("Smooth: unable to allocate enough memory");

                                    return;
                                }

                                VOIs.VOIAt(i).getCurves().removeElement(contour);
                                VOIs.VOIAt(i).getCurves().addElement((VOIContour)
                                                                                smoothAlgo.getResultVOI().getCurves().lastElement());
                            } else {
                                contour.trimPoints(Preferences.getTrimVoi(), Preferences.getTrimAdjacient());
                                contour.setClosed(!open);
                                //                                VOIs.VOIAt(i).getCurves()[((ViewJComponentEditImage)(component)).getSlice()].addElement(contour);

                                VOIs.VOIAt(i).importCurve(contour);
                            }
                        } else {
                            MipavUtil.displayError("Can't add Livewire VOI to other VOI structure.");
                        }
                    }
                }

                image.notifyImageDisplayListeners();

                if (mouseEvent.isShiftDown() != true) {
                    ((ViewJComponentEditImage) (component)).setCursorMode(ViewJComponentEditImage.DEFAULT);
                }

                // setup for next time this class is used
                firstPoint = true;
                xPoints = null;
                yPoints = null;

                try {
                    contour = new VOIContour(false);
                    clickPoints = new Vector<Vector3f>();
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: unable to form new livewire");
                    ((ViewJComponentEditImage) (component)).setCursorMode(ViewJComponentEditImage.DEFAULT);

                    return;
                }
            }

            return;
        } // if (done)

        if (!open) {
            tmpPt.x = Math.round(getEnd().x /
                                     (((ViewJComponentEditImage) (component)).getZoomX() *
                                          ((ViewJComponentEditImage) (component)).getResolutionX()));
            tmpPt.y = Math.round(getEnd().y /
                                     (((ViewJComponentEditImage) (component)).getZoomY() *
                                          ((ViewJComponentEditImage) (component)).getResolutionY()));

            if (isActive() && testPoint(pt) && ((xS != tmpPt.x) || (yS != tmpPt.y))) {

                if (xPoints != null) {
                    clickPoints.addElement(new Vector3f(xPoints[0] /
                                                            (((ViewJComponentEditImage) (component)).getZoomX() *
                                                                 ((ViewJComponentEditImage) (component))
                                                                 .getResolutionX()),
                                                        yPoints[0] /
                                                            (((ViewJComponentEditImage) (component)).getZoomY() *
                                                                 ((ViewJComponentEditImage) (component))
                                                                 .getResolutionY()),
                                                        ((ViewJComponentEditImage) (component)).getSlice()));

                    for (i = 0; i < xPoints.length; i++) {

                        // need to scale back down to image space.
                        contour.addElement((xPoints[xPoints.length - 1 - i]) /
                                               (((ViewJComponentEditImage) (component)).getZoomX() *
                                                    ((ViewJComponentEditImage) (component)).getResolutionX()),
                                           (yPoints[yPoints.length - 1 - i]) /
                                               (((ViewJComponentEditImage) (component)).getZoomY() *
                                                    ((ViewJComponentEditImage) (component)).getResolutionY()),
                                           ((ViewJComponentEditImage) (component)).getSlice());
                    }
                } else {
                    contour.addElement(xS, yS, ((ViewJComponentEditImage) (component)).getSlice());
                    clickPoints.addElement(new Vector3f(xS, yS, ((ViewJComponentEditImage) (component)).getSlice()));
                }

                Graphics g = super.component.getGraphics();

                if (firstPoint) {

                    // System.err.println("setting first point to false");
                    anchor(pt);
                    firstPoint = false;
                } else {
                    g.setXORMode(Color.black);
                    g.setColor(Color.yellow);

                    if (contour.size() > 2) {

                        if (contour.size() != 3) {
                            g.drawRect((int) anchorPt.x - 3, (int) anchorPt.y - 3, 6, 6);
                        }

                        g.drawRect(pt.x - 3, pt.y - 3, 6, 6);
                    }

                    end(pt);
                    anchor(pt);
                }
            } // if( isActive() && testPoint(pt)  && (xS != tmpPt.x || yS != tmpPt.y) )
        } // if (!open)
    }

    /**
     * Sets the presetHue.
     *
     * @param  presetHue  DOCUMENT ME!
     */
    public void setPresetHue(float presetHue) {
        this.presetHue = presetHue;
    }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowActivated(WindowEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowClosed(WindowEvent event) { }

    /**
     * Sets completed to false, disposes the progress bar and notifies all listeners that the algorithm is stopped.
     *
     * @param  event  event that triggered function
     */
    public void windowClosing(WindowEvent event) {

        if (progressBar != null) {
            progressBar.dispose();
        }
    }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowDeactivated(WindowEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowDeiconified(WindowEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowIconified(WindowEvent event) { }

    // ************************************************************************
    // **************************** Window Events *****************************
    // ************************************************************************

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowOpened(WindowEvent event) { }


    /**
     * Takes a byte and gets appropriate addition from current position.
     *
     * @param   next  Byte to check (0-8).
     *
     * @return  Value to add to current location.
     */
    private int convertGraphToInt(int next) {

        switch (next) {

            case 0:
                return (-xDim - 1);

            case 1:
                return (-xDim);

            case 2:
                return (-xDim + 1);

            case 3:
                return (-1);

            case 5:
                return (1);

            case 6:
                return (xDim - 1);

            case 7:
                return (xDim);

            case 8:
                return (xDim + 1);
        }

        return 0;
    }

    /**
     * Sets up directed graph from the seed point. A point (x,y) is mapped to its absolute value in the image, y*xDim +
     * x. This along with its cost is stored in Node. The structure costGraph holds the "edges" of the graph. Each
     * location has an integer associated with it which represents where that node is pointing to. Thus costGraph[7] = 8
     * would mean the node at 7 (really (0,7)) is pointing to position 8 (really (0,8)). The only possibilities for a
     * location in costGraph are the 8 neighbors surrounding that node. For this reason we might use a byte array
     * instead of an integer array. The seed point points nowhere, to indicate that it's the seed;
     * costGraph[seed.location] = -1. We also need to know if a point has been processed, that is, expanded with the
     * cost set. For this we use a BitSet whose size is the same as the number of pixels in the image. Once a point is
     * processed, its location in the BitSet is set to <code>true</code>.
     *
     * <p>The array seededCosts holds the costs so far for a location. If a cost has not been assigned yet, the cost is
     * -2. ActiveList is simply a linked list of Integers; the Integer refers to the location in the seededCosts array.
     * ActiveList is sorted by cost, so that the minimum cost in the ActiveList is the first element of the linked list.
     * Thus finding the minimum is O(1). Finding out if an element is in the ActiveList is also O(1), because for an
     * element to be in the list, it must have already been assigned a cost. Therefore, if seededCosts[location] != -2,
     * it is in the ActiveList. And finding the cost of an item in the ActiveList is O(1), because it's just
     * seededCosts[location], where location is the Integer in the ActiveList. Obviously we're winning speed at the
     * expense of memory, but it's not too much memory in the overall scheme of things.</p>
     *
     * <p>The gradient direction component of the cost is added in on the fly. This is because to precalculate would
     * mean an array of 8n, where n is the size of the image. The link from p to q is not the same as the link from q to
     * p. Furthermore, it may never be necessary to calculate some of the links, because the graph would never look at
     * that pair. For more information on how the gradient direction cost is calculated, look at the comments directly
     * above the code.</p>
     *
     * @param  pt  Point to seed with.
     */
    private void seed(Point pt) {

        Cursor cursor = component.getCursor();
        component.setCursor(waitCursor);

        int x = Math.round(pt.x /
                               (((ViewJComponentEditImage) (component)).getZoomX() *
                                    ((ViewJComponentEditImage) (component)).getResolutionX()));
        int y = Math.round(pt.y /
                               (((ViewJComponentEditImage) (component)).getZoomX() *
                                    ((ViewJComponentEditImage) (component)).getResolutionX()));

        int location = (y * xDim) + x; // Location in array that represents image.

        costGraph[location] = -1; // No parent of seed point
        seedPoint = location;

        // processedIndicies.clear(); only in JVM 1.4.
        for (int c = 0; c < processedIndicies.size(); c++) {
            processedIndicies.clear(c);
        }


        for (int i = 0; i < seededCosts.length; i++) {
            seededCosts[i] = -2;
        }

        activeTree.reset();

        seededCosts[location] = 0;
        activeTree.insert(location, 0f);

        float temp, cost, gradDir;
        long tiempo = System.currentTimeMillis();
        int count = 0;

        float gdConst = (float) (2f / (3 * Math.PI));
        float pi = (float) Math.PI;

        while (activeTree.isEmpty() == false) { // while active list has more elements to process

            location = activeTree.pop();
            cost = seededCosts[location];
            processedIndicies.set(location); // set this point as processed
            x = location % xDim;
            y = location / xDim;

            count++;

            for (int iy = -1; iy <= 1; iy++) {
                int yOffset = (y + iy) * xDim;

                if (((y + iy) >= 0) && ((y + iy) < yDim)) { // in bounds in the y dimension

                    for (int ix = -1; ix <= 1; ix++) {
                        int position = yOffset + (x + ix);

                        if (((x + ix) >= 0) && ((x + ix) < xDim) && // in bounds in the x dimension
                                !processedIndicies.get(position)) { // not yet processed - this will rule out current
                                                                    // node

                            if (selection == GRADIENT_MAG) {
                                // Gradient Direction Cost Let D(p) be a unit vector of the gradient direction at point
                                // p. Define D'(p) as the unit vector perpendicular to D(p). In our implementation, D(p)
                                // = (xDirections[location], yDirections[location]) where location is the absolute
                                // position of point p; that is, location = p.y*xDim + p.x So       D(p) =
                                // (xDirections[location], yDirections[location])       D'(p) = (yDirections[location],
                                // -xDirections[location])
                                //
                                // The formulation of the gradient direction feature cost is      f(p,q) = 2/(3pi) *
                                // (acos(dp(p,q) + acos(dq(p,q)))) where      dp(p,q) = D'(p) dot L(p,q)      dq(p,q) =
                                // L(p,q) dot D'(q) and
                                //
                                //     L(p,q) =    1     {q - p if D'(p) dot (q - p) >= 0               ------- *{
                                //       ||p-q||  {p - q if D'(p) dot (q - p) < 0

                                float Lx, Ly;
                                float divide;

                                // divide is || p - q || = || ( (x - (x+ix)), (y - (y+iy)) ) ||
                                // = || ( -ix, -iy ) ||
                                // = sqrt(ix^2 + iy^2)
                                // = sqrt(2) or sqrt(1)
                                // because ix and iy will never both be 0 (current node is already processed)
                                // so if they are both non-zero, it's sqrt(2), otherwise it's sqrt(1)
                                if ((ix != 0) && (iy != 0)) {
                                    divide = 0.7071068f;
                                } else {
                                    divide = 1f;
                                }

                                // if D'(p) dot (q - p) >= 0
                                // becomes
                                // if ( yDirections[location]*(x+ix-x) + (-xDirections[location]*(y+iy-y) )
                                // becomes
                                if (((yDirections[location] * ix) - (xDirections[location] * iy)) >= 0) {
                                    Lx = ix * divide;
                                    Ly = iy * divide;
                                }
                                // D'(p) dot (q - p) < 0
                                else {
                                    Lx = -ix * divide;
                                    Ly = -iy * divide;
                                }
                                // if (yDirections[location]*ix - xDirections[location]*iy >= 0)
                                // dp = divide * (yDirections[location]*ix - xDirections[location]*iy)
                                // so dp >= 0
                                // if (yDirections[location]*ix - xDirections[location]*iy < 0)
                                // dp = -divide * (yDirections[location]*ix - xDirectios[locations]*iy)
                                // so dp > 0
                                // Hence always have 1 >= dp >= 0
                                // dq = sign(yDirections[location]*ix - xDirections[location]*iy) *
                                // divide * (yDirections[position]*ix - xDirections[position]*iy)
                                // Thus, if (yDirections[location]*ix - xDirections[location]*iy) and
                                // (yDirections[position]*ix - xDirections[position]*iy) have the same
                                // sign, then dq is positive.  Otherwise, dq is negative.
                                // 1 > = dq > = -1
                                // acos_dp can vary from 0 to pi/2 and acos_dq can vary from 0 to
                                // pi so the gradient direction feature cost can vary from 0 to 1.
                                // The Taylor series for acos(x) =
                                // pi/2 - x - (x**3)/(2*3) - 1*3*(x**5)/(2*4*5) - 1*3*5*(x**7)/(2*4*6*7)

                                float dp = (yDirections[location] * Lx) - (xDirections[location] * Ly);
                                float dq = (yDirections[position] * Lx) - (xDirections[position] * Ly);

                                if (dp > 1) {
                                    dp = 1f;
                                }

                                if (dq > 1) {
                                    dq = 1f;
                                } else if (dq < -1) {
                                    dq = -1f;
                                }

                                // float acos_dp = (float)(Math.acos(dp));
                                // float acos_dq = (float)(Math.acos(dq));
                                // float gradDir = (gdConst)*(acos_dp + acos_dq);

                                // The above is the original formula as described.
                                // The below is what we're using because it's much faster since
                                // it only uses up to the third power term in the Taylor series.
                                gradDir = gdConst * (pi - dp - (dp * dp * dp / 6) - dq - (dq * dq * dq / 6));
                            } else { // ((selection == MEDIALNESS) || (selection == INTENSITY))
                                gradDir = 0f;
                            }

                            if ((ix != 0) && (iy != 0)) { // diagonal costs more
                                temp = cost + ((localCosts[position] + (grad_weight * gradDir)) * 1.4142f); // * square root of 2, Euclidean distance
                            } else {
                                temp = cost + (localCosts[position] + (grad_weight * gradDir)); // temp cost
                            }

                            if (temp < seededCosts[position]) { // not set seededCosts == -2 which will always be less
                                                                // than temp, temp must be positive
                                activeTree.remove(position, seededCosts[position]);
                                seededCosts[position] = -2;
                            }

                            // if not in active or was just removed from active list
                            if (seededCosts[position] == -2) {

                                // put into active list or back into active list with new cost
                                // set pointer back to parent
                                int temploc = ((-iy + 1) * 3) - ix + 1;

                                costGraph[position] = (byte) temploc;
                                activeTree.insert(position, temp);
                                seededCosts[position] = temp;

                            }
                        }
                    }
                }
            }
        }

        Preferences.debug("Livewire anchor took: " + ((System.currentTimeMillis() - tiempo) / 1000f) + "\n");
        component.setCursor(cursor);
        // comment in the below to see seededCosts as image.
        /*
         * ModelImage image = new ModelImage(ModelStorageBase.FLOAT, new int[] {xDim, yDim}, "Seed",
         * ((ViewJComponentEditImage)component).getActiveImage().getUserInterface()); try { image.importData(0,
         * seededCosts, true); } catch (IOException e) { } new ViewJFrameImage(image, null, new Dimension(200, 200),
         * image.getUserInterface());
         */
    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * Binary search tree, modified so it will work with elements that have the same cost. In theory this should not be
     * a problem because we have a unique identifier in the parameter <code>position</code>. New nodes of the same cost
     * as an existing node are inserted to the left of the first existing node of the same cost. The children of the
     * first existing node are transferred to the left of the new node. The new node is not put all the way to the left
     * of a chain of nodes of the same cost so as to save tree traversal time. This convention must be followed in the
     * remove and pop or else some nodes get lost and others are never removed.
     *
     * <p>The main reason for using a Binary Search Tree is that the cost of a "find" (required for remove, pop, and
     * insert) is log n if the tree is reasonably balanced. To insure balance we could have used a red-black tree, but
     * test runs indicate that this tree stays fairly balanced on its own due to the nature of the insertions and
     * deletions.</p>
     */
    public class ActiveTree {

        /** Pointer to root of tree. */
        TreeNode root;

        /**
         * Sets root node to null.
         */
        public ActiveTree() {
            root = null;
        }

        /**
         * Prints out tree in sorted order. Level allows user to see the structure of the tree.
         */
        public void inOrder() {
            inOrder(root, 0);
        }

        /**
         * Inserts node with given position and cost at the appropriate location in the tree. If the cost is the same as
         * one previously in the tree, inserts the node on the left of the original.
         *
         * @param  position  Position in image array.
         * @param  cost      Cost at this position.
         */
        public void insert(int position, float cost) {

            if (root == null) { // set root to a new node
                root = new TreeNode(position, cost);
            } else { // find appropriate parent of node we wish to insert

                TreeNode node = root;
                TreeNode parent = null;

                // leaf node, add there   this should never happen
                while ((node != null) && (node.position != position)) {
                    parent = node;

                    if (node.cost > cost) {
                        node = node.left;
                    } else if (node.cost < cost) {
                        node = node.right;
                    } else { // inserting a node of the same cost, special case
                        node = node.left;

                        break;
                    }
                }

                if (parent.cost > cost) {
                    parent.left = new TreeNode(position, cost);
                } else if (parent.cost < cost) {
                    parent.right = new TreeNode(position, cost);
                } else { // inserting node of same cost
                    parent.left = new TreeNode(position, cost);

                    // if old node had children, make them child of new node
                    if (node != null) {
                        parent.left.left = node;
                    }
                }
            }
        }

        /**
         * Returns a flag indicating if this tree is empty.
         *
         * @return  <code>true</code> if tree is empty.
         */
        public final boolean isEmpty() {
            return root == null;
        }

        /**
         * Pops off minimum cost in tree. Returns position of minimum cost and also removes that node from the tree.
         *
         * @return  Position of minimum cost.
         */
        public final int pop() {
            TreeNode node = root;
            TreeNode parent = null;

            // find leftmost node, this is the minimum
            while (node.left != null) {
                parent = node;
                node = node.left;
            }

            // if both node.left and node.right are null, leaf node.
            if (node.right == null) {

                if (parent != null) {
                    parent.left = null;
                } else {
                    root = null;
                }
            } else { // not a leaf node, there is still a right node attached.

                if (parent != null) {
                    parent.left = node.right;
                } else {
                    root = node.right;
                }
            }

            return node.position;
        }

        /**
         * Removes node of given position and cost. <code>cost</code> is used to find where the node is; <code>
         * position</code> is used to uniquely identify the node.
         *
         * @param  position  Position in image array of node to remove.
         * @param  cost      Cost of function at position.
         */
        public void remove(int position, float cost) {
            TreeNode node = root;
            TreeNode parent = null;

            // Find node and parent node
            while ((node != null) && (node.position != position)) {
                parent = node;

                if (node.cost > cost) {
                    node = node.left;
                } else if (node.cost < cost) {
                    node = node.right;
                } else {
                    node = node.left;
                }
            }

            if (node == null) {
                Preferences.debug("Tried to remove " + position + " " + cost + " which does not exist.\n");
            }

            if ((node.right == null) && (node.left == null)) { // Leaf node

                if (node != root) {

                    if (parent.cost >= cost) {
                        parent.left = null;
                    } else {
                        parent.right = null;
                    }
                } else {
                    root = null;
                }
            } else if ((node.right == null) && (node.left != null)) { // right subtree empty, left subtree not.

                if (node != root) {

                    if (parent.cost >= cost) {
                        parent.left = node.left;
                    } else {
                        parent.right = node.left;
                    }
                } else {
                    root = node.left;
                }
            } else if ((node.right != null) && (node.left == null)) { // left subtree empty, right subtree not.

                if (node != root) {

                    if (parent.cost >= cost) {
                        parent.left = node.right;
                    } else {
                        parent.right = node.right;
                    }
                } else {
                    root = node.right;
                }
            } else { // left and right subtrees not empty

                TreeNode rightmost = node.left;
                TreeNode rightmostP = node;

                while (rightmost.right != null) {
                    rightmostP = rightmost;
                    rightmost = rightmost.right;
                }

                node.position = rightmost.position;
                node.cost = rightmost.cost;

                if (rightmostP != node) {
                    rightmostP.right = rightmost.left;
                } else {
                    node.left = rightmost.left;
                }
            }
        }

        /**
         * Sets root to null.
         */
        public void reset() {
            root = null;
        }

        /**
         * Prints out tree in sorted order. Level allows user to see the structure of the tree. Will print out tree from
         * given node on down.
         *
         * @param  node   Root of tree to print.
         * @param  level  Level of tree to print.
         */
        private void inOrder(TreeNode node, int level) {

            if (node != null) {
                inOrder(node.left, level + 1);
                //System.out.println("Node " + node + " at level " + level);
                inOrder(node.right, level + 1);
            }
        }
    }

    /**
     * Tree node. Contains data (the integer position and float cost) and pointers to the left and right children.
     */
    public class TreeNode {

        /** Cost of position. */
        public float cost;

        /** Node to the left in the tree. */
        public TreeNode left;

        /** Position in array seededCosts. */
        public int position;

        /** Node to the right in the tree. */
        public TreeNode right;

        /**
         * Creates a new tree node with the given data.
         *
         * @param  position  Position in array seededCosts.
         * @param  cost      Cost of node.
         */
        public TreeNode(int position, float cost) {
            right = null;
            left = null;
            this.position = position;
            this.cost = cost;
        }

        /**
         * Returns readable representation of this node.
         *
         * @return  Readable representation of node.
         */
        public String toString() {
            return "position: " + position + " cost: " + cost;
        }
    }
}
