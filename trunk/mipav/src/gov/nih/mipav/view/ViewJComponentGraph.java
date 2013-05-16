package gov.nih.mipav.view;


import java.awt.*;
import java.awt.event.*;

import javax.swing.*;


/**
 * This is a custom made Swing component. It is a graph that takes a panel in the constructor and then draws itself. The
 * graph has the option of labels for the x and y axis, which are set with an accessor. Gridlines can also be turned on
 * and off. The function calling ViewJComponentGraph may specify a number of gridlines to have on the graph. The number
 * of gridlines is the same as the number of tick marks. Points are plotted in the graph by calling an accessor with an
 * array of x coordinates and an array of y coordinates. The number of coordinates in each array must be equal.
 *
 * @version  0.1 Aug 1, 1998
 * @author   Neva Cherniavsky (primary)
 * @author   Harman Singh
 * @see      ViewJFrameGraph
 * @see      ViewJComponentFunct
 */
public class ViewJComponentGraph extends JComponent implements MouseListener, MouseMotionListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 4064300190247654824L;

    /** Maximum number of functions that the graph may display. */
    public static final int MAX_NUM_FUNCTS = 5;

    /** DOCUMENT ME! */
    private static float[] copiedXs = null;

    /** DOCUMENT ME! */
    private static float[] copiedYs = null;

    /** DOCUMENT ME! */
    private static String copiedName;

    /** DOCUMENT ME! */
    private static ViewJComponentFunct copiedFunct;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private Color backgroundColor = new Color(200, 200, 200);

    /** DOCUMENT ME! */
    private Rectangle bounds;

    /** DOCUMENT ME! */
    private Cursor crosshairCursor;

    /** DOCUMENT ME! */
    private Cursor defaultCursor;

    /** DOCUMENT ME! */
    private float defaultMaxDomain;

    /** DOCUMENT ME! */
    private float defaultMaxRange;

    /** DOCUMENT ME! */
    private float defaultMinDomain;

    /** DOCUMENT ME! */
    private float defaultMinRange;

    /** DOCUMENT ME! */
    private ViewJComponentFunct[] fittedFunctions;

    /** DOCUMENT ME! */
    private Font font10;

    /** DOCUMENT ME! */
    private Font font12;

    /** DOCUMENT ME! */
    private Font font12B;

    /** DOCUMENT ME! */
    private ImageIcon fun2Icon = MipavUtil.getIcon("fun2.gif");

    /** DOCUMENT ME! */
    private ImageIcon fun2PrintIcon = MipavUtil.getIcon("fun2print.gif");

    /** DOCUMENT ME! */
    private ImageIcon fun3Icon = MipavUtil.getIcon("fun3.gif");

    /** DOCUMENT ME! */
    private ImageIcon fun3PrintIcon = MipavUtil.getIcon("fun3print.gif");

    /** DOCUMENT ME! */
    private ImageIcon fun4Icon = MipavUtil.getIcon("fun4.gif");

    /** DOCUMENT ME! */
    private ImageIcon fun4PrintIcon = MipavUtil.getIcon("fun4print.gif");

    /** DOCUMENT ME! */
    private ImageIcon fun5Icon = MipavUtil.getIcon("fun5.gif");

    /** DOCUMENT ME! */
    private ImageIcon fun5PrintIcon = MipavUtil.getIcon("fun5print.gif");

    /** DOCUMENT ME! */
    private ViewJComponentFunct[] functions;

    /** DOCUMENT ME! */
    private Rectangle graphBounds;

    /** DOCUMENT ME! */
    private float maxDomain;

    /** DOCUMENT ME! */
    private float maxRange;

    /** DOCUMENT ME! */
    private float minDomain;

    /** DOCUMENT ME! */
    private float minRange;

    /** DOCUMENT ME! */
    private int[] newX;

    /** DOCUMENT ME! */
    private int[] newY;

    /** DOCUMENT ME! */
    private Rectangle oldRubberband;

    /** DOCUMENT ME! */
    private JFrame parentFrame;

    /** DOCUMENT ME! */
    private RubberbandRectangle rubberbandRect;

    /** DOCUMENT ME! */
    private boolean showFittedFunctions = false;

    /** DOCUMENT ME! */
    private boolean showFunctions = true;

    /** DOCUMENT ME! */
    private boolean showGridLines = true;

    /** DOCUMENT ME! */
    private boolean showLegend = false;

    /** DOCUMENT ME! */
    private boolean showMinorTickMarks = true;
    
    public static final int SHOW_LINES_ONLY = 0;
    
    public static final int SHOW_POINTS_AND_LINES = 1;
    
    public static final int SHOW_POINTS_ONLY = 2;

    /** DOCUMENT ME! */
    private int showPointsAndLines = SHOW_LINES_ONLY;

    /** DOCUMENT ME! */
    private String title = null;


    /** DOCUMENT ME! */
    private int xGridLines = -1;

    /** DOCUMENT ME! */
    private String xLabel = null;

    /** DOCUMENT ME! */
    private double xScale = 1;

    /** DOCUMENT ME! */
    private double xTick;

    /** DOCUMENT ME! */
    private int yGridLines = -1;

    /** DOCUMENT ME! */
    private String yLabel = null;

    /** DOCUMENT ME! */
    private double yScale = 1;

    /** DOCUMENT ME! */
    private double yTick;
    
    private boolean doLog = false;
    
    private boolean zeroYMin = false;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor - creates graph within the JPanel using graphics.
     *
     * @param  frame   frame that this component is in
     * @param  width   initial width for the component
     * @param  height  initial height for the component
     */
    public ViewJComponentGraph(JFrame frame, int width, int height) {


        try {
            crosshairCursor = new Cursor(Cursor.CROSSHAIR_CURSOR);
            defaultCursor = new Cursor(Cursor.DEFAULT_CURSOR);
            rubberbandRect = new RubberbandRectangle(this);
            oldRubberband = rubberbandRect.getBounds();
            font10 = MipavUtil.courier10;
            font12 = MipavUtil.courier12;
            font12B = MipavUtil.courier12B;
            bounds = new Rectangle(0, 0, width, height);
            newX = new int[1000];
            newY = new int[1000];
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJComponentGraph constructor");

            return;
        }

        setBounds(bounds);

        parentFrame = frame;

        setVisible(true);
        addMouseListener(this);
        addMouseMotionListener(this);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Calculates the range and domain (according to min and max y values). and expands range if necessary. (does not
     * shrink range)
     */
    public void calculateCustomRange() {

        // calculate range according to min and y values,
        // to be used by plotGraph()
        float yMin = Float.MAX_VALUE;
        float yMax = Float.MIN_VALUE;
        int len = -1;
        int i;

        for (i = 0; i < functions.length; i++) {

            if (functions[i].getXs().length != functions[i].getYs().length) {
                return;
            }

            if (len < functions[i].getXs().length) {
                len = functions[i].getXs().length;
            }
        }

        // Find mins and maxs, set up int arrays
        for (int k = 0; k < functions.length; k++) {

            if (functions[k].getFunctionVisible()) {

                for (i = 0; i < functions[k].getXs().length; i++) {

                    if (functions[k].getYs()[i] < yMin) {
                        yMin = functions[k].getYs()[i];
                    }

                    if (functions[k].getYs()[i] > yMax) {
                        yMax = functions[k].getYs()[i];
                    }
                }
            }

            if (functions[k].getFitFunctionVisible()) {

                for (i = 0; i < fittedFunctions[k].getXs().length; i++) {

                    if (fittedFunctions[k].getYs()[i] < yMin) {
                        yMin = fittedFunctions[k].getYs()[i];
                    }

                    if (fittedFunctions[k].getYs()[i] > yMax) {
                        yMax = fittedFunctions[k].getYs()[i];
                    }
                }
            }

        }

        if (yMin < minRange) {
            minRange = yMin;
        }

        if (yMax > maxRange) {
            maxRange = yMax;
        }

    }

    /**
     * Calculates the default range and domain (according to min and max x and y values). Modifies minRange, maxRange,
     * minDomain, maxDomain accordingly.
     */
    public void calculateDefaultRangeDomain() {

        if (functions.length == 0) {
            defaultMinRange = 0;
            defaultMaxRange = 0;
            defaultMinDomain = 0;
            defaultMaxDomain = 0;

            return;
        }


        // calculate domain and range according to min and max x and y values,
        // to be used by plotGraph()
        float xMin = Float.MAX_VALUE;
        float xMax = -Float.MAX_VALUE;
        float yMin = Float.MAX_VALUE;
        float nonZeroYMin = Float.MAX_VALUE;
        float yMax = -Float.MAX_VALUE;
        int len = -1;
        int i;
        boolean functVisible = false;

        for (i = 0; i < functions.length; i++) {

            if (functions[i].getXs().length != functions[i].getYs().length) {
                return;
            }

            if (len < functions[i].getXs().length) {
                len = functions[i].getXs().length;
            }
        }

        // Find mins and maxs, set up int arrays
        for (int k = 0; k < functions.length; k++) {

            if (functions[k].getFunctionVisible()) {

                for (i = 0; i < functions[k].getXs().length; i++) {
                    functVisible = true;

                    if (functions[k].getXs()[i] < xMin) {
                        xMin = functions[k].getXs()[i];
                    }

                    if (functions[k].getXs()[i] > xMax) {
                        xMax = functions[k].getXs()[i];
                    }

                    if (functions[k].getYs()[i] < yMin) {
                        yMin = functions[k].getYs()[i];
                    }
                    
                    if (functions[k].getYs()[i] != 0.0) {
                        if (functions[k].getYs()[i] < nonZeroYMin) {
                            nonZeroYMin = functions[k].getYs()[i];
                        }    
                    }

                    if (functions[k].getYs()[i] > yMax) {
                        yMax = functions[k].getYs()[i];
                    }
                    
                }
            }

            if (functions[k].getFitFunctionVisible()) {

                for (i = 0; i < fittedFunctions[k].getXs().length; i++) {

                    if (fittedFunctions[k].getXs()[i] < xMin) {
                        xMin = fittedFunctions[k].getXs()[i];
                    }

                    if (fittedFunctions[k].getXs()[i] > xMax) {
                        xMax = fittedFunctions[k].getXs()[i];
                    }

                    if (fittedFunctions[k].getYs()[i] < yMin) {
                        yMin = fittedFunctions[k].getYs()[i];
                    }
                    
                    if (fittedFunctions[k].getYs()[i] != 0.0) {
                        if (fittedFunctions[k].getYs()[i] < nonZeroYMin) {
                            nonZeroYMin = fittedFunctions[k].getYs()[i];
                        }    
                    }

                    if (fittedFunctions[k].getYs()[i] > yMax) {
                        yMax = fittedFunctions[k].getYs()[i];
                    }
                    
                }
            }

        }

        if ((functions.length == 0) || !functVisible) {
            xMin = xMax = yMin = yMax = 0;
        }

        if (doLog) {
            if (yMin == 0.0) {
                defaultMinRange = (float)(Math.log10(nonZeroYMin) - 1.0);
                zeroYMin = true;
            }
            else {
                defaultMinRange = (float)Math.log10(yMin);
                zeroYMin = false;
            }
            defaultMaxRange = (float)Math.log10(yMax);
        }
        else {
            defaultMinRange = yMin;
            defaultMaxRange = yMax;
        }
        defaultMinDomain = xMin;
        defaultMaxDomain = xMax;
    }

    /**
     * Copies the indicated function's properties, to be pasted later as a new function.
     *
     * @param  index  - index of the function to be copied
     */
    public void copyFunct(int index) {
        ViewJComponentFunct tempf;

        try {
            tempf = functions[index];
            copiedXs = new float[tempf.getOriginalXs().length];
            copiedYs = new float[tempf.getOriginalYs().length];

            for (int i = 0; i < copiedXs.length; i++) {
                copiedXs[i] = tempf.getOriginalXs()[i];
            }

            for (int i = 0; i < copiedYs.length; i++) {
                copiedYs[i] = tempf.getOriginalYs()[i];
            }

            copiedName = tempf.getFunctName();
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJComponentGraph.copyFunct()");

            return;
        }
    }

    /**
     * Deletes the indicated function.
     *
     * @param  index  - index of the function to be deleted
     */
    public void deleteFunct(int index) {
        ViewJComponentFunct[] tempFuncts;
        ViewJComponentFunct[] tempFittedFuncts;

        try {
            tempFuncts = new ViewJComponentFunct[functions.length - 1];
            tempFittedFuncts = new ViewJComponentFunct[functions.length - 1];

            for (int i = 0; i < functions.length; i++) {

                if (i < index) {
                    tempFuncts[i] = functions[i];
                    tempFittedFuncts[i] = fittedFunctions[i];
                } else if (i > index) {
                    tempFuncts[i - 1] = functions[i];
                    tempFittedFuncts[i - 1] = fittedFunctions[i];
                }
            }
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJComponentGraph.deleteFunct");

            return;
        }

        setFuncts(tempFuncts);
        setFittedFuncts(tempFittedFuncts);
    }

    /**
     * Accessor that gets the background color.
     *
     * @return  DOCUMENT ME!
     */
    public Color getBackgroundColor() {
        return backgroundColor;
    }

    /**
     * Accessor that returns the bounds of the component.
     *
     * @return  rectangle bounds of the component
     */
    public Rectangle getBounds() {
        return bounds;
    }

    /**
     * Returns the default maximum point for the range (previously calculated).
     *
     * @return  DOCUMENT ME!
     */
    public float getDefaultMaxRange() {
        return defaultMaxRange;
    }

    /**
     * Returns the default minimum point for the range (previously calculated).
     *
     * @return  DOCUMENT ME!
     */
    public float getDefaultMinRange() {
        return defaultMinRange;
    }

    /**
     * Accessor that gets the visible flag.
     *
     * @return  the boolean visible flag
     */
    public boolean getFittedFunctionsVisible() {
        return showFittedFunctions;
    }

    /**
     * Accessor that gets the fitted functions for this graph.
     *
     * @return  an array of functions for this graph
     */
    public ViewJComponentFunct[] getFittedFuncts() {
        return fittedFunctions;
    }

    /**
     * Accessor that gets the visible flag of the functions.
     *
     * @return  the boolean visible flag
     */
    public boolean getFunctionsVisible() {
        return showFunctions;
    }

    /**
     * Accessor that gets the functions for this graph.
     *
     * @return  an array of functions for this graph
     */
    public ViewJComponentFunct[] getFuncts() {
        return functions;
    }

    /**
     * Accessor that gets the visible flag.
     *
     * @return  the boolean visible flag
     */
    public boolean getGridlinesVisible() {
        return showGridLines;
    }

    /**
     * Accessor that gets the visible flag.
     *
     * @return  the boolean visible flag
     */
    public boolean getLegendVisible() {
        return showLegend;
    }

    /**
     * Returns the current maximum point for the range.
     *
     * @return  DOCUMENT ME!
     */
    public float getMaxRange() {
        return maxRange;
    }

    /**
     * Accessor that returns the minimum size of this component.
     *
     * @return  the minimum size
     */
    public Dimension getMinimumSize() {
        Dimension dim;

        try {
            dim = new Dimension(302, 308);
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJComponentGraph.getMinimumSize");

            return null;
        }

        return dim;

    }

    /**
     * Accessor that gets the visible flag.
     *
     * @return  the boolean visible flag
     */
    public boolean getMinorTickMarksVisible() {
        return showMinorTickMarks;
    }

    /**
     * Returns the current minimum point for the range.
     *
     * @return  DOCUMENT ME!
     */
    public float getMinRange() {
        return minRange;
    }

    /**
     * Accessor that gets the number of gridlines for paint.
     *
     * @return  the number of gridlines on the x axis
     */
    public int getNumberOfXGridLines() {
        return xGridLines;
    }

    /**
     * Accessor that gets the number of gridlines for paint.
     *
     * @return  the number of gridlines on the y axis
     */
    public int getNumberOfYGridLines() {
        return yGridLines;
    }


    /**
     * Accessor that gets the visible flag.
     *
     * @return  the integer visible flag
     */
    public int getPointsAndLinesDisplay() {
        return showPointsAndLines;
    }

    /**
     * Accessor that returns the preferred size of the component.
     *
     * @return  the preferred size
     */
    public Dimension getPreferredSize() {
        Dimension dim;

        try {
            dim = new Dimension(bounds.width, bounds.height);
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJComponentGraph.getPreferredSize");

            return null;
        }

        return dim;
    }

    /**
     * Gets the title for the graph.
     *
     * @return  title for the graph
     */
    public String getTitle() {
        return title;
    }

    /**
     * Gets the label for the x axis.
     *
     * @return  label for the x axis
     */
    public String getXLabel() {
        return xLabel;
    }

    /**
     * Gets the label for the y axis.
     *
     * @return  label for the y axis
     */
    public String getYLabel() {
        return yLabel;
    }

    /**
     * DOCUMENT ME!
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseClicked(MouseEvent mouseEvent) { }

    /**
     * ******************* Mouse Motion Events ***************************.*********************************************
     * ***********************
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseDragged(MouseEvent mouseEvent) { }

    /**
     * ************************ Mouse Events *****************************.*********************************************
     * ***********************
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseEntered(MouseEvent mouseEvent) { }

    /**
     * DOCUMENT ME!
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mouseExited(MouseEvent mouseEvent) { }

    /**
     * Changes the cursor to crosshair and enables the zoom box rubberband if the cursor is in the graph; also sees if
     * cursor is near a valid point and prints it out.
     *
     * @param  mouseEvent  event that triggered this function
     */
    public void mouseMoved(MouseEvent mouseEvent) {
        Graphics g = getGraphics();
        int xS = mouseEvent.getX();
        int yS = mouseEvent.getY();
        int xindex = 0;
        int findex = 0;
        double minDistance = Double.MAX_VALUE;
        int i;
        String tmpString;
        int len;
        // float       x[]         = functions.getXs();
        // float       y[]         = function.getYs();

        if (g == null) {
            MipavUtil.displayError("ComponentGraph.mouseMoved: graphics = null");

            return;
        }

        if (graphBounds == null) {
            return;
        }

        if (graphBounds.contains(xS, yS)) {
            setCursor(crosshairCursor);
            rubberbandRect.setActive(true);

            len = -1;

            for (i = 0; i < functions.length; i++) {

                if (len < functions[i].getXs().length) {
                    len = functions[i].getXs().length;
                }
            }

            for (i = 0; i < len; i++) {

                for (int j = 0; j < functions.length; j++) {

                    if (i < functions[j].getNewXsLength()) {

                        if (distance(xS, yS, functions[j].getNewXs()[i], functions[j].getNewYs()[i]) < minDistance) {
                            minDistance = distance(xS, yS, functions[j].getNewXs()[i], functions[j].getNewYs()[i]);
                            xindex = i;
                            findex = j;
                        }
                    }
                }
            }

            if (minDistance <= 5) {
                g.setColor(Color.lightGray);

                // getGraphics().clearRect(bounds.x+5, bounds.height-26, 120, 20);
                g.fillRect(bounds.x + 5, bounds.height - 26, 120, 20);
                g.setColor(Color.black);

                try {
                    tmpString = new String("X: " + makeString(functions[findex].getXs()[xindex], 2) + "  Y: " +
                                           makeString(functions[findex].getYs()[xindex], 2));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: ViewJComponentGraph.mouseMoved");

                    return;
                }

                g.drawString(tmpString, 10, bounds.height - 13);
            } else {
                g.setColor(Color.lightGray);

                // getGraphics().clearRect(bounds.x+5, bounds.height-26, 120, 20);
                g.fillRect(bounds.x + 5, bounds.height - 26, 120, 20);
            }
        } else {
            g.setColor(Color.lightGray);

            // getGraphics().clearRect(bounds.x+5, bounds.height-26, 120, 20);
            g.fillRect(bounds.x + 5, bounds.height - 26, 120, 20);
            setCursor(defaultCursor);
            rubberbandRect.setActive(false);
        }
    }

    /**
     * DOCUMENT ME!
     *
     * @param  mouseEvent  DOCUMENT ME!
     */
    public void mousePressed(MouseEvent mouseEvent) { }

    /**
     * Checks to see that the zoom box is valid, then redraws the graph using the zoom box as bounds.
     *
     * @param  mouseEvent  event that triggered this function
     */
    public void mouseReleased(MouseEvent mouseEvent) {

        if (oldRubberband.equals(rubberbandRect.getBounds())) {
            repaint(bounds);

            return;
        } else {
            oldRubberband = rubberbandRect.getBounds();
        }

        if ((rubberbandRect.getBounds().x < graphBounds.x) ||
                ((rubberbandRect.getBounds().x + rubberbandRect.getBounds().width) >
                     (graphBounds.x + graphBounds.width)) || (rubberbandRect.getBounds().y < graphBounds.y) ||
                ((rubberbandRect.getBounds().y + rubberbandRect.getBounds().height) >
                     (graphBounds.y + graphBounds.height)) || (rubberbandRect.getBounds().width < 5) ||
                (rubberbandRect.getBounds().height < 5)) {
            repaint(bounds);

            return;
        }

        redrawGraph(rubberbandRect.getBounds());
        ((ViewJFrameGraph) parentFrame).setResetEnabled(true);
    }

    /**
     * Paints the graph. Checks if the labels are defined and draws them, checks if the gridlines should be shown and
     * draws them, then calls PolyLine with the points.
     *
     * @param  g  Graphics to paint in
     */
    public void paintComponent(Graphics g) {

        if (g == null) {
            MipavUtil.displayError("ComponentGraph.paintComponent: graphics = null");

            return;
        }

        int numf = getFuncts().length;
        int inc = (numf - 1) * 14;

        // Graph bounds are dependent on the component bounds.
        try {
            graphBounds = new Rectangle(80, 40 + inc, bounds.width - 120, (bounds.height - 100) - inc);
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ComponentGraph.paintComponent");

            return;
        }

        /*
         * g.setColor(backgroundColor); g.fill3DRect(graphBounds.x, graphBounds.y, graphBounds.width,
         * graphBounds.height, true); // The outside of the graph. g.setColor(Color.black); g.drawRect(graphBounds.x,
         * graphBounds.y, graphBounds.width, graphBounds.height);
         */

        g.setColor(backgroundColor);
        g.fill3DRect(graphBounds.x - 5, graphBounds.y - 5, graphBounds.width + 7, graphBounds.height + 7, true);

        // The outside of the graph.
        g.setColor(Color.black);
        g.drawRect(graphBounds.x - 5, graphBounds.y - 5, graphBounds.width + 7, graphBounds.height + 7);

        // Draw the title for the graph
        if (title != null) {
            Font f = MipavUtil.font16B;
            g.setFont(f);
            g.setColor(Color.black);
            g.drawString(title, ((bounds.width / 2) - (title.length() * 4)), 25 + (inc / 2));
        }

        // Draw the label for the x axis
        if (xLabel != null) {
            g.setFont(font12B);
            g.setColor(Color.black);
            g.drawString(xLabel, ((bounds.width / 2) - (xLabel.length() / 2)), bounds.height - 5);
        }

        // Draw the label for the y axis
        if (yLabel != null) {
            g.setFont(font12B);
            g.setColor(Color.black);

            int index = yLabel.length() / 2;

            for (int i = 0; i < yLabel.length(); i++) {
                g.drawString(String.valueOf(yLabel.charAt(i)), 5, ((bounds.height / 2) - ((index - i) * 10)));
            }
        }

        // draw legend
        if (showLegend) {
            numf = getFuncts().length;

            String[] name = new String[numf];

            for (int i = 0; i < numf; i++) {
                name[i] = functions[i].getFunctName();
            }


            Font f = MipavUtil.font12B;
            g.setFont(f);
            g.setColor(Color.black);

            FontMetrics fm = getFontMetrics(f);
            double maxNameLength = 0;

            for (int j = 0; j < numf; j++) {

                if (maxNameLength < fm.stringWidth(name[j])) {
                    maxNameLength = fm.stringWidth(name[j]);
                }
            }

            f = MipavUtil.font13B;
            g.setColor(Color.black);


            if (maxNameLength < 40) {
                maxNameLength = 40;
            }

            g.drawRect(bounds.width - (160), 0, 45 + (int) maxNameLength, 20 + (14 * numf));
            g.drawString("Legend", bounds.width - 135, 15);

            g.setFont(font12B);

            for (int i = 0; i < numf; i++) {

                // if (functions[i].getFunctName() == null) functions[i].setFunctName(i+1);
                g.setColor(Color.black);

                if (i == 0) {
                    g.fillRect(bounds.width - 153, 23 + (14 * i), 3, 3);
                }

                if (i == 1) {
                    fun2Icon.paintIcon(null, g, bounds.width - 155, 19 + (14 * i));
                }

                if (i == 2) {
                    fun3Icon.paintIcon(null, g, bounds.width - 155, 19 + (14 * i));
                }

                if (i == 3) {
                    fun4Icon.paintIcon(null, g, bounds.width - 155, 19 + (14 * i));
                }

                if (i == 4) {
                    fun5Icon.paintIcon(null, g, bounds.width - 155, 19 + (14 * i));
                }

                g.setColor(getFuncts()[i].getColor());
                g.drawString(name[i], bounds.width - 135, 27 + (14 * i));
            }
        }

        // Distance along the axis for the tick marks and gridlines
        xTick = graphBounds.width / ((double) xGridLines);
        yTick = graphBounds.height / ((double) yGridLines);

        // Draw grid lines
        if (showGridLines) {
            g.setColor(Color.gray);

            for (int i = 1; i < xGridLines; i++) {
                g.drawLine(graphBounds.x + (int) Math.round(i * xTick), graphBounds.y - 5,
                           graphBounds.x + (int) Math.round(i * xTick), graphBounds.y + graphBounds.height);
            }

            for (int i = 1; i < yGridLines; i++) {
                g.drawLine(graphBounds.x, graphBounds.y + (int) Math.round(i * yTick),
                           graphBounds.x + graphBounds.width, graphBounds.y + (int) Math.round(i * yTick));
            }
        }

        // Draw tick marks
        for (int i = 1; i < xGridLines; i++) {
            g.setColor(Color.black);
            g.drawLine(graphBounds.x + (int) Math.round(i * xTick), graphBounds.y + graphBounds.height - 5,
                       graphBounds.x + (int) Math.round(i * xTick), graphBounds.y + graphBounds.height + 5);
        }

        for (int i = yGridLines - 1; i > 0; i--) {
            g.setColor(Color.black);
            g.drawLine(graphBounds.x - 5, graphBounds.y + (int) Math.round(i * yTick), graphBounds.x + 5,
                       graphBounds.y + (int) Math.round(i * yTick));
        }

        // Draw minor tick marks

        if (showMinorTickMarks) {

            for (int i = 1; i < (xGridLines * 10); i++) {
                g.setColor(Color.black);

                // g.drawLine(graphBounds.x + (int)Math.round(i*(xTick/10)),
                // graphBounds.y + graphBounds.height - 2,
                // graphBounds.x + (int)Math.round(i*(xTick/10)),
                // graphBounds.y + graphBounds.height + 2);
                g.drawLine(graphBounds.x + (int) Math.round(i * (xTick / 10)), graphBounds.y + graphBounds.height,
                           graphBounds.x + (int) Math.round(i * (xTick / 10)), graphBounds.y + graphBounds.height + 4);
            }

            for (int i = (yGridLines * 10) - 1; i > 0; i--) {
                g.setColor(Color.black);

                // g.drawLine(graphBounds.x - 2,
                // graphBounds.y + (int)Math.round(i*(yTick/10)),
                // graphBounds.x + 2,
                // graphBounds.y + (int)Math.round(i*(yTick/10)));
                g.drawLine(graphBounds.x - 7, graphBounds.y + (int) Math.round(i * (yTick / 10)), graphBounds.x - 3,
                           graphBounds.y + (int) Math.round(i * (yTick / 10)));
            }
        }

        plotGraph(g);
    }

    /**
     * Paints the graph for the Printer. Certain things such as background color and graph dimensions and location
     * differ from the regular paint method
     *
     * @param  g  Graphics to paint in
     */
    public void paintComponentForPrinter(Graphics g) {

        if (g == null) {
            MipavUtil.displayError("ComponentGraph.paintComponent: graphics = null");

            return;
        }

        int numf = getFuncts().length;
        int inc = (numf - 1) * 14;

        // Graph bounds are dependent on the component bounds.
        if (bounds.width > 500) {

            try {

                if ((bounds.height > 550) && (bounds.height <= 720)) {
                    graphBounds = new Rectangle(120, 85 + inc, 455, bounds.height - 100 - inc);
                } else if (bounds.height > 720) {
                    graphBounds = new Rectangle(120, 85 + inc, 455, 620 - inc);
                } else {
                    graphBounds = new Rectangle(120, 175 + inc, 455, bounds.height - 100 - inc);
                }
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ComponentGraph.paintComponent");

                return;
            }
        } else {

            try {

                if ((bounds.height > 550) && (bounds.height <= 720)) {
                    graphBounds = new Rectangle(120, 75 + inc, (bounds.width - 120) + 75, bounds.height - 100 - inc);
                } else if (bounds.height > 720) {
                    graphBounds = new Rectangle(120, 75 + inc, (bounds.width - 120) + 75, 620 - inc);
                } else {
                    graphBounds = new Rectangle(120, 175 + inc, (bounds.width - 120) + 75, bounds.height - 100 - inc);
                }

            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ComponentGraph.paintComponent");

                return;
            }
        }

        g.setColor(Color.black);
        g.drawRect(graphBounds.x, graphBounds.y, graphBounds.width, graphBounds.height);

        // Draw the title for the graph
        if (title != null) {
            Font f = MipavUtil.font16B;
            g.setFont(f);
            g.setColor(Color.black);

            if (bounds.width > 500) {

                if (bounds.height > 550) {
                    g.drawString(title, (250 - (title.length() * 4)) + 40, 60 + (inc / 2));
                } else {
                    g.drawString(title, (250 - (title.length() * 4)) + 40, 160 + (inc / 2));
                }
            } else {

                if (bounds.height > 550) {
                    g.drawString(title, ((bounds.width / 2) - (title.length() * 4)) + 40, 60 + (inc / 2));
                } else {
                    g.drawString(title, ((bounds.width / 2) - (title.length() * 4)) + 40, 160 + (inc / 2));
                }
            }
        }


        // Draw the label for the x axis
        if (xLabel != null) {
            g.setFont(font12B);
            g.setColor(Color.black);

            if (bounds.width > 500) {

                if ((bounds.height > 550) && (bounds.height <= 720)) {
                    g.drawString(xLabel, (250 - (xLabel.length() / 2)) + 65, (bounds.height - 5) + 35);
                } else if (bounds.height > 720) {
                    g.drawString(xLabel, (250 - (xLabel.length() / 2)) + 65, 750);
                } else {
                    g.drawString(xLabel, (250 - (xLabel.length() / 2)) + 65, (bounds.height - 5) + 125);
                }
            } else {

                if ((bounds.height > 550) && (bounds.height <= 720)) {
                    g.drawString(xLabel, ((bounds.width / 2) - (xLabel.length() / 2)) + 65, (bounds.height - 5) + 35);
                } else if (bounds.height > 720) {
                    g.drawString(xLabel, ((bounds.width / 2) - (xLabel.length() / 2)) + 65, 750);
                } else {
                    g.drawString(xLabel, ((bounds.width / 2) - (xLabel.length() / 2)) + 65, (bounds.height - 5) + 125);
                }
            }
        }

        // Draw the label for the y axis
        if (yLabel != null) {
            g.setFont(font12B);
            g.setColor(Color.black);

            int index = yLabel.length() / 2;

            for (int i = 0; i < yLabel.length(); i++) {

                if ((bounds.height > 550) && (bounds.height <= 720)) {
                    g.drawString(String.valueOf(yLabel.charAt(i)), 45, ((bounds.height / 2) - ((index - i) * 10)) + 45);
                } else if (bounds.height > 720) {
                    g.drawString(String.valueOf(yLabel.charAt(i)), 45, (360 - ((index - i) * 10)) + 45);
                } else {
                    g.drawString(String.valueOf(yLabel.charAt(i)), 45,
                                 ((bounds.height / 2) - ((index - i) * 10)) + 145);
                }
            }
        }

        // draw legend
        if (showLegend) {
            numf = getFuncts().length;

            String[] name = new String[numf];

            for (int i = 0; i < numf; i++) {
                name[i] = functions[i].getFunctName();
            }

            Font f = MipavUtil.font12B;
            g.setFont(f);
            g.setColor(Color.black);

            FontMetrics fm = getFontMetrics(f);
            double maxNameLength = 0;

            for (int j = 0; j < numf; j++) {

                if (maxNameLength < fm.stringWidth(name[j])) {
                    maxNameLength = fm.stringWidth(name[j]);
                }
            }

            f = MipavUtil.font13B;
            g.setColor(Color.black);

            if (maxNameLength < 40) {
                maxNameLength = 40;
            }

            if (bounds.width > 500) {

                if (bounds.height > 550) {
                    g.drawRect(465, 47, 45 + (int) maxNameLength, 20 + (14 * numf));
                    g.drawString("Legend", 485, 15 + 35);
                } else {
                    g.drawRect(465, 137, 45 + (int) maxNameLength, 20 + (14 * numf));
                    g.drawString("Legend", 485, 15 + 135);
                }
            } else {

                if (bounds.height > 550) {
                    g.drawRect(bounds.width - 35, 37, 45 + (int) maxNameLength, 20 + (14 * numf));
                    g.drawString("Legend", bounds.width - 15, 15 + 35);
                } else {
                    g.drawRect(bounds.width - 35, 137, 45 + (int) maxNameLength, 20 + (14 * numf));
                    g.drawString("Legend", bounds.width - 15, 15 + 135);
                }
            }

            g.setFont(font12B);

            for (int i = 0; i < numf; i++) {
                g.setColor(Color.black);

                if (bounds.width > 500) {

                    if (bounds.height > 550) {

                        if (i == 0) {
                            g.fillRect(473, 55 + (14 * i), 3, 3);
                        }

                        if (i == 1) {
                            fun2Icon.paintIcon(this, g, 470, 52 + (14 * i));
                        }

                        if (i == 2) {
                            fun3Icon.paintIcon(this, g, 470, 52 + (14 * i));
                        }

                        if (i == 3) {
                            fun4Icon.paintIcon(this, g, 470, 52 + (14 * i));
                        }

                        if (i == 4) {
                            fun5Icon.paintIcon(this, g, 470, 52 + (14 * i));
                        }
                    } else {

                        if (i == 0) {
                            g.fillRect(473, 155 + (14 * i), 3, 3);
                        }

                        if (i == 1) {
                            fun2Icon.paintIcon(this, g, 470, 152 + (14 * i));
                        }

                        if (i == 2) {
                            fun3Icon.paintIcon(this, g, 470, 152 + (14 * i));
                        }

                        if (i == 3) {
                            fun4Icon.paintIcon(this, g, 470, 152 + (14 * i));
                        }

                        if (i == 4) {
                            fun5Icon.paintIcon(this, g, 470, 152 + (14 * i));
                        }
                    }

                } else {

                    if (bounds.height > 550) {

                        if (i == 0) {
                            g.fillRect(bounds.width - 27, 55 + (14 * i), 3, 3);
                        }

                        if (i == 1) {
                            fun2Icon.paintIcon(this, g, bounds.width - 30, 52 + (14 * i));
                        }

                        if (i == 2) {
                            fun3Icon.paintIcon(this, g, bounds.width - 30, 52 + (14 * i));
                        }

                        if (i == 3) {
                            fun4Icon.paintIcon(this, g, bounds.width - 30, 52 + (14 * i));
                        }

                        if (i == 4) {
                            fun5Icon.paintIcon(this, g, bounds.width - 30, 52 + (14 * i));
                        }
                    } else if (i == 0) {
                        g.fillRect(bounds.width - 27, 155 + (14 * i), 3, 3);
                    }

                    if (i == 1) {
                        fun2Icon.paintIcon(this, g, bounds.width - 30, 152 + (14 * i));
                    }

                    if (i == 2) {
                        fun3Icon.paintIcon(this, g, bounds.width - 30, 152 + (14 * i));
                    }

                    if (i == 3) {
                        fun4Icon.paintIcon(this, g, bounds.width - 30, 152 + (14 * i));
                    }

                    if (i == 4) {
                        fun5Icon.paintIcon(this, g, bounds.width - 30, 152 + (14 * i));
                    }
                }

                g.setColor(getFuncts()[i].getColor());

                if (bounds.width > 500) {

                    if (bounds.height > 550) {
                        g.drawString(name[i], 485, 62 + (14 * i));
                    } else {
                        g.drawString(name[i], 485, 162 + (14 * i));
                    }
                } else {

                    if (bounds.height > 550) {
                        g.drawString(name[i], bounds.width - 15, 62 + (14 * i));
                    } else {
                        g.drawString(name[i], bounds.width - 15, 162 + (14 * i));
                    }
                }
            }


        }

        // Distance along the axis for the tick marks and gridlines
        xTick = graphBounds.width / ((double) xGridLines);
        yTick = graphBounds.height / ((double) yGridLines);

        // Draw grid lines
        if (showGridLines) {
            g.setColor(Color.gray);

            for (int i = 1; i < xGridLines; i++) {
                g.drawLine(graphBounds.x + (int) Math.round(i * xTick), graphBounds.y,
                           graphBounds.x + (int) Math.round(i * xTick), graphBounds.y + graphBounds.height);
            }

            for (int i = 1; i < yGridLines; i++) {
                g.drawLine(graphBounds.x, graphBounds.y + (int) Math.round(i * yTick),
                           graphBounds.x + graphBounds.width, graphBounds.y + (int) Math.round(i * yTick));
            }
        }

        // Draw tick marks
        for (int i = 1; i < xGridLines; i++) {
            g.setColor(Color.black);
            g.drawLine(graphBounds.x + (int) Math.round(i * xTick), graphBounds.y + graphBounds.height - 5,
                       graphBounds.x + (int) Math.round(i * xTick), graphBounds.y + graphBounds.height + 5);
        }

        for (int i = yGridLines - 1; i > 0; i--) {
            g.setColor(Color.black);
            g.drawLine(graphBounds.x - 5, graphBounds.y + (int) Math.round(i * yTick), graphBounds.x + 5,
                       graphBounds.y + (int) Math.round(i * yTick));
        }

        // Draw minor tick marks

        if (showMinorTickMarks) {

            for (int i = 1; i < (xGridLines * 10); i++) {
                g.setColor(Color.black);
                g.drawLine(graphBounds.x + (int) Math.round(i * (xTick / 10)), graphBounds.y + graphBounds.height - 2,
                           graphBounds.x + (int) Math.round(i * (xTick / 10)), graphBounds.y + graphBounds.height + 2);
            }

            for (int i = (yGridLines * 10) - 1; i > 0; i--) {
                g.setColor(Color.black);
                g.drawLine(graphBounds.x - 2, graphBounds.y + (int) Math.round(i * (yTick / 10)), graphBounds.x + 2,
                           graphBounds.y + (int) Math.round(i * (yTick / 10)));
            }
        }

        plotGraphForPrinter(g);
    }

    /**
     * Adds the previously copied copied function to the graph.
     */
    public void pasteFunct() {

        if (copiedXs == null) {
            MipavUtil.displayError("No Function in Clipboard");

            return;
        }

        if ((functions.length != 0) &&
                ((copiedXs.length != functions[0].getOriginalXs().length) ||
                     (copiedYs.length != functions[0].getOriginalYs().length))) {
            MipavUtil.displayError("Dimensions of Copied Function Do Not Match This Graph.");

            return;
        }

        ViewJComponentFunct[] tempFuncts;
        ViewJComponentFunct[] tempFittedFuncts;

        try {
            tempFuncts = new ViewJComponentFunct[functions.length + 1];
            tempFittedFuncts = new ViewJComponentFunct[functions.length + 1];
            copiedFunct = new ViewJComponentFunct(copiedXs, copiedYs, Color.red, copiedName, null);
            copiedFunct.setColor(functions.length);

            for (int i = 0; i < functions.length; i++) {
                tempFuncts[i] = functions[i];
                tempFittedFuncts[i] = fittedFunctions[i];
            }

            tempFuncts[functions.length] = copiedFunct;
            tempFittedFuncts[functions.length] = new ViewJComponentFunct(copiedXs, copiedYs, copiedFunct.getColor(),
                                                                         null,(int[][])null);

        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJComponentGraph.pasteFunct");

            return;
        }

        setFuncts(tempFuncts);
        setFittedFuncts(tempFittedFuncts);
    }

    /**
     * Redraws the graph based on the new bounds. Used for zooming in on portions of the graph.
     *
     * @param  newBounds  the new bounds of the graph
     */
    public void redrawGraph(Rectangle newBounds) {
        float[] zoomX;
        float[] zoomY;
        int[] begin;
        int[] end;

        try {
            begin = new int[functions.length];
            end = new int[functions.length];
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJComponentGraph.redrawGraph");

            return;
        }

        for (int k = 0; k < functions.length; k++) {

            for (int i = 1; i < (functions[k].getXs().length - 1); i++) {

                if ((newBounds.x <= functions[k].getNewXs()[i]) && (newBounds.x >= functions[k].getNewXs()[i - 1])) {
                    begin[k] = i - 1; // saves where first point of function to draw on new bounds is
                }

                if (((newBounds.x + newBounds.width) >= functions[k].getNewXs()[i]) &&
                        ((newBounds.x + newBounds.width) <= functions[k].getNewXs()[i + 1])) {
                    end[k] = i + 1; // saves where last point of function to draw on new bounds is
                }
            }
        }

        try {

            for (int k = 0; k < functions.length; k++) {

                if (end[k] < begin[k]) {
                    end[k] = functions[k].getNewXsLength() - 1;
                }

                try {
                    zoomX = new float[end[k] - begin[k] + 1];
                    zoomY = new float[end[k] - begin[k] + 1];
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: ViewJComponentGraph.redrawGraph");

                    return;
                }

                int j = 0;

                for (int i = begin[k]; i <= end[k]; i++) {
                    zoomX[j] = functions[k].getXs()[i];
                    zoomY[j] = functions[k].getYs()[i];
                    j++;
                }

                functions[k].setXs(zoomX);
                functions[k].setYs(zoomY);
            }
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ComponentGraph.redrawGraph");

            return;
        }

        try {

            for (int k = 0; k < fittedFunctions.length; k++) {

                if (functions[k].getFitFunctionVisible()) {

                    if (end[k] < begin[k]) {
                        end[k] = fittedFunctions[k].getNewXsLength() - 1;
                    }

                    try {
                        zoomX = new float[end[k] - begin[k] + 1];
                        zoomY = new float[end[k] - begin[k] + 1];
                    } catch (OutOfMemoryError error) {
                        MipavUtil.displayError("Out of memory: ViewJComponentGraph.redrawGraph");

                        return;
                    }

                    int j = 0;

                    for (int i = begin[k]; i <= end[k]; i++) {
                        zoomX[j] = fittedFunctions[k].getXs()[i];
                        zoomY[j] = fittedFunctions[k].getYs()[i];
                        j++;
                    }

                    fittedFunctions[k].setXs(zoomX);
                    fittedFunctions[k].setYs(zoomY);
                }
            }
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ComponentGraph.redrawGraph");

            return;
        }

        calculateDefaultRangeDomain();
        setDefaultRangeDomain();
        repaint(bounds);
    }

    /**
     * Sets the bounds of the component.
     *
     * @param  rect  the new bounds for the component
     */
    public void resetBounds(Rectangle rect) {
        bounds = rect;
        setBounds(bounds);
    }

    /**
     * Accessor that sets the background color to paint.
     *
     * @param  color  the new background color
     */
    public void setBackgroundColor(Color color) {
        backgroundColor = color;
    }

    /**
     * Sets range and domain to defaults.
     */
    public void setDefaultRangeDomain() {
        minRange = defaultMinRange;
        maxRange = defaultMaxRange;
        minDomain = defaultMinDomain;
        maxDomain = defaultMaxDomain;
    }
    
    /**
     * 
     * @param doLog
     */
    public void setDoLog(boolean doLog) {
        this.doLog = doLog;
    }

    /**
     * Sets the domain of the graph according to the min and max parmaters.
     *
     * @param  min  minimum domain
     * @param  max  maximum domain
     */
    public void setDomain(float min, float max) {
        minDomain = min;
        maxDomain = max;
    }

    /**
     * Accessor that tells whether to show the gridlines.
     *
     * @param  visible  boolean to set it to
     */
    public void setFittedFunctionsVisible(boolean visible) {
        showFittedFunctions = visible;
    }

    /**
     * Accessor that sets the fitted functions for this graph.
     *
     * @param  functs  function to set the graph to
     */
    public void setFittedFuncts(ViewJComponentFunct[] functs) {
        fittedFunctions = functs;
    }


    /**
     * Accessor that tells whether to show the functions.
     *
     * @param  visible  boolean to set it to
     */
    public void setFunctionsVisible(boolean visible) {
        showFunctions = visible;
    }

    /**
     * Accessor that sets the function for this graph.
     *
     * @param  functs  function to set the graph to
     */
    public void setFuncts(ViewJComponentFunct[] functs) {
        functions = functs;
    }

    /**
     * Accessor that tells whether to show the gridlines.
     *
     * @param  visible  boolean to set it to
     */
    public void setGridlinesVisible(boolean visible) {
        showGridLines = visible;
    }


    /**
     * Sets the labels for the x and y axis to these strings.
     *
     * @param  xStr  label for the x axis
     * @param  yStr  label for the y axis
     */
    public void setLabels(String xStr, String yStr) {
        xLabel = xStr;
        yLabel = yStr;
    }
    
    /**
     * Sets the y axis label to yStr
     * @param yStr
     */
    public void setLabelY(String yStr) {
        yLabel = yStr;
    }

    /**
     * Accessor that tells whether to show the legend.
     *
     * @param  visible  boolean to set it to
     */
    public void setLegendVisible(boolean visible) {
        showLegend = visible;
    }

    /**
     * Accessor that tells whether to show the minor tick marks.
     *
     * @param  visible  boolean to set it to
     */
    public void setMinorTickMarksVisible(boolean visible) {
        showMinorTickMarks = visible;
    }

    /**
     * Accessor that sets the number of gridlines for paint.
     *
     * @param  xGrid  the number of gridlines on the x axis
     */
    public void setNumberOfXGridLines(int xGrid) {
        xGridLines = xGrid;
    }

    /**
     * Accessor that sets the number of gridlines for paint.
     *
     * @param  yGrid  the number of gridlines on the y axis
     */
    public void setNumberOfYGridLines(int yGrid) {
        yGridLines = yGrid;
    }

    /**
     * Accessor that sets whether to display lines only, points and lines, or points only
     *
     * @param  visible integer to set it to
     */
    public void setPointsAndLinesDisplay(int showPointsAndLines) {
        this.showPointsAndLines = showPointsAndLines;
    }

    /**
     * Sets the range of the graph according to the min and max parmaters.
     *
     * @param  min  minimum range
     * @param  max  maximum range
     */
    public void setRange(float min, float max) {
        minRange = min;
        maxRange = max;
    }

    /**
     * DOCUMENT ME!
     */
    public void setRangeSymmetric() {
        float maxValue;

        if ((maxRange > 0) && (minRange < 0)) {
            maxValue = Math.max(maxRange, Math.abs(minRange));
            maxRange = maxValue;
            minRange = -maxValue;
        }

    }

    /**
     * Sets the title to this string.
     *
     * @param  tStr  title for the graph
     */
    public void setTitle(String tStr) {
        title = tStr;
    }

    /**
     * Calls paint.
     *
     * @param  g  Graphics to paint in
     */
    public void update(Graphics g) {
        paintComponent(g);
    }

    /**
     * Tests the distance between two points.
     *
     * @param   x1  x coordinate of the first point
     * @param   y1  y coordinate of the first point
     * @param   x2  x coordinate of second point
     * @param   y2  y coordinate of second point
     *
     * @return  returns the distance
     */
    private double distance(int x1, int y1, int x2, int y2) {
        return Math.sqrt(((x2 - x1) * (x2 - x1)) + ((y2 - y1) * (y2 - y1)));
    }


    /**
     * Makes a string of a float with a specific number of decimal points.
     *
     * @param   number  number to be converted to a string
     * @param   decPts  the number of decimal points
     *
     * @return  string representation of the number
     */
    private String makeString(float number, int decPts) {
        String subStr = null;
        String str = null;

        try {
            subStr = new String();
            str = new String(String.valueOf(number));
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("FrameBase.makeString: out of memory");

            return null;
        }

        int index = str.indexOf(".");
        int length = str.length();

        if ((index + decPts) < length) {
            subStr = str.substring(0, index + decPts + 1);
        } else {
            subStr = str;
        }

        return subStr;
    }

    /**
     * Plots the graph by calling PolyLine. Makes new arrays for the x and y, scaling them to the graphics x and y. Also
     * writes the tick mark labels.
     *
     * @param  g  graphics to draw in
     */
    private void plotGraph(Graphics g) {
        int i;
        int index, indexExp;
        float tmp;
        float xMin = minDomain;
        float xMax = maxDomain;
        float yMin = minRange;
        float yMax = maxRange;
        String tmpString;

        if (g == null) {
            MipavUtil.displayError("ComponentGraph.plotGraph: graphics = null");

            return;
        }
        
        if ((minDomain == 0.0f) && (maxDomain == 0.0f) && (minRange == 0.0f) && (maxRange == 0.0f)) {
            // No visible functions to plot
            return;
        }

        xScale = graphBounds.width / (xMax - xMin);
        yScale = graphBounds.height / (yMax - yMin);

        for (int k = 0; k < functions.length; k++) {

            try {

                if (newX.length < functions[k].getXs().length) {
                    newX = new int[functions[k].getXs().length];
                }

                if (newY.length < functions[k].getYs().length) {
                    newY = new int[functions[k].getYs().length];
                }
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ViewJComponentGraph.plotGraph");

                return;
            }

            for (i = 0; i < functions[k].getXs().length; i++) {
                newX[i] = (int) Math.round((functions[k].getXs()[i] * xScale) - (xMin * xScale)) + graphBounds.x;
                if (doLog) {
                    if (functions[k].getYs()[i] == 0.0) {
                        newY[i] = (int) Math.round((yMax - yMin) * yScale) + graphBounds.y;    
                    }
                    else {
                        newY[i] = (int) Math.round((yMax - Math.log10(functions[k].getYs()[i])) * yScale) + graphBounds.y;    
                    }
                }
                else {
                    newY[i] = (int) Math.round((yMax - functions[k].getYs()[i]) * yScale) + graphBounds.y;
                }

            }

            functions[k].setNewXs(newX, functions[k].getXs().length);
            functions[k].setNewYs(newY, functions[k].getYs().length);
            g.setColor(functions[k].getColor());

            if (functions[k].getFunctionVisible()) {
                if ((showPointsAndLines == SHOW_POINTS_AND_LINES) || (showPointsAndLines == SHOW_LINES_ONLY)) {
                    g.drawPolyline(newX, newY, functions[k].getXs().length);
                }
            }
        }

        for (int k = 0; k < fittedFunctions.length; k++) {

            if (functions[k].getFitFunctionVisible()) {

                try {

                    if (newX.length < fittedFunctions[k].getXs().length) {
                        newX = new int[fittedFunctions[k].getXs().length];
                    }

                    if (newY.length < fittedFunctions[k].getYs().length) {
                        newY = new int[fittedFunctions[k].getYs().length];
                    }
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: ViewJComponentGraph.plotGraph");

                    return;
                }

                for (i = 0; i < fittedFunctions[k].getXs().length; i++) {
                    newX[i] = (int) Math.round((fittedFunctions[k].getXs()[i] * xScale) - (xMin * xScale)) +
                              graphBounds.x;
                    if (doLog) {
                        if (fittedFunctions[k].getYs()[i] == 0.0) {
                            newY[i] = (int) Math.round((yMax - yMin) * yScale) + graphBounds.y;    
                        }
                        else {
                            newY[i] = (int) Math.round((yMax - Math.log10(fittedFunctions[k].getYs()[i])) * yScale) + graphBounds.y;    
                        }
                    }
                    else {
                        newY[i] = (int) Math.round((yMax - fittedFunctions[k].getYs()[i]) * yScale) + graphBounds.y;
                    }

                }

                fittedFunctions[k].setNewXs(newX, fittedFunctions[k].getXs().length);
                fittedFunctions[k].setNewYs(newY, fittedFunctions[k].getYs().length);
                g.setColor(Color.yellow);
                if ((showPointsAndLines == SHOW_POINTS_AND_LINES) || (showPointsAndLines == SHOW_LINES_ONLY)) {
                    g.drawPolyline(newX, newY, fittedFunctions[k].getXs().length);
                }
            }
        }

        float newYMax = -1;
        float newYMin = -1;
        g.setColor(Color.blue);

        if ((yMax > 999.99) || (yMin < -999.99)) {
            newYMax = yMax;
            newYMin = yMin;

            int index2 = 0;

            while ((newYMax > 999.99) || (newYMin < -999.99)) {
                index2++;
                newYMax = newYMax / 10;
                newYMin = newYMin / 10;
            }

            g.setFont(font12);

            // g.drawString("x10", graphBounds.x, graphBounds.y - 5);
            g.drawString("x10", graphBounds.x, graphBounds.y - 10);
            g.setFont(font10);
            g.setColor(Color.lightGray);

            // g.fillRect(graphBounds.x+25, graphBounds.y-15, 10, 10);
            g.fillRect(graphBounds.x + 25, graphBounds.y - 20, 10, 10);
            g.setColor(Color.blue);

            // g.drawString(String.valueOf(index2), graphBounds.x+25, graphBounds.y-10);
            g.drawString(String.valueOf(index2), graphBounds.x + 25, graphBounds.y - 15);
            g.setFont(font12);
        } else if ((Math.abs(yMax) < 1.0) && (Math.abs(yMin) < 1.0)) {
            newYMax = yMax;
            newYMin = yMin;

            int index2 = 0;

            while ((Math.abs(newYMax) < 1.0) && (Math.abs(newYMin) < 1.0)) {
                index2--;
                newYMax = newYMax * 10;
                newYMin = newYMin * 10;
            }

            g.setFont(font12);

            // g.drawString("x10", graphBounds.x, graphBounds.y - 5);
            g.drawString("x10", graphBounds.x, graphBounds.y - 10);
            g.setFont(font10);
            g.setColor(Color.lightGray);

            // g.fillRect(graphBounds.x+25, graphBounds.y-15, 10, 10);
            g.fillRect(graphBounds.x + 25, graphBounds.y - 20, 10, 10);
            g.setColor(Color.blue);

            // g.drawString(String.valueOf(index2), graphBounds.x+25, graphBounds.y-10);
            g.drawString(String.valueOf(index2), graphBounds.x + 25, graphBounds.y - 15);
            g.setFont(font12);
        } else {
            g.setFont(font12);

            // g.drawString("x10", graphBounds.x, graphBounds.y - 5);
            g.drawString("x10", graphBounds.x, graphBounds.y - 10);
            g.setFont(font10);
            g.setColor(Color.lightGray);

            // g.fillRect(graphBounds.x+25, graphBounds.y-15, 10, 10);
            g.fillRect(graphBounds.x + 25, graphBounds.y - 20, 10, 10);
            g.setColor(Color.blue);

            // g.drawString(String.valueOf(0), graphBounds.x+25, graphBounds.y-10);
            g.drawString(String.valueOf(0), graphBounds.x + 25, graphBounds.y - 15);
            g.setFont(font12);
        }

        int j = yGridLines;

        for (i = 0; i <= yGridLines; i++) {

            if (newYMax != -1) {
                tmp = (i * (newYMax - newYMin) / yGridLines) + newYMin;
            } else {
                tmp = (i * (yMax - yMin) / yGridLines) + yMin;
            }
            if (doLog) {
                if (zeroYMin && tmp == yMin) {
                    tmp = 0.0f;
                }
                else {
                    tmp = (float)Math.pow(10.0,tmp);
                }
            }

            tmpString = String.valueOf(tmp);
            index = tmpString.indexOf('.');
            indexExp = tmpString.toUpperCase().indexOf('E');

            if ((index + 3) <= tmpString.length()) {

                if (indexExp > -1) {
                    tmpString = tmpString.substring(0, index + 3) + tmpString.substring(indexExp);
                } else {
                    tmpString = tmpString.substring(0, index + 3);
                }
            }

            g.setColor(Color.lightGray);

            // g.fillRect(graphBounds.x - 50,
            // graphBounds.y + (int)Math.round(j*yTick)-5,
            // 50, 30);
            g.fillRect(graphBounds.x - 55, graphBounds.y + (int) Math.round(j * yTick) - 10, 43, 30);
            g.setColor(Color.blue);

            // g.drawString(tmpString,
            // graphBounds.x - 50,
            // graphBounds.y + (int)Math.round(j*yTick) + 5);
            g.drawString(tmpString, graphBounds.x - 55, graphBounds.y + (int) Math.round(j * yTick) + 5);
            j--;
        }

        for (i = 0; i <= xGridLines; i = i + 2) {
            tmp = (i * (xMax - xMin) / xGridLines) + xMin;
            tmpString = String.valueOf(tmp);
            index = tmpString.indexOf('.');

            if ((index + 3) <= tmpString.length()) {
                tmpString = tmpString.substring(0, index + 3);
            }

            g.drawString(tmpString, graphBounds.x + (int) Math.round(i * xTick) - 20,
                         graphBounds.y + graphBounds.height + 20);
        }

        for (i = 1; i <= xGridLines; i = i + 2) {
            tmp = (i * (xMax - xMin) / xGridLines) + xMin;
            tmpString = String.valueOf(tmp);
            index = tmpString.indexOf('.');

            if ((index + 3) <= tmpString.length()) {
                tmpString = tmpString.substring(0, index + 3);
            }

            g.drawString(tmpString, graphBounds.x + (int) Math.round(i * xTick) - 20,
                         graphBounds.y + graphBounds.height + 30);
        }

        if ((showPointsAndLines == SHOW_POINTS_AND_LINES) || (showPointsAndLines == SHOW_POINTS_ONLY)) {

            for (int k = 0; k < functions.length; k++) {

                if (functions[k].getFunctionVisible()) {

                    for (i = 0; i < functions[k].getNewXsLength(); i++) {
                        g.setColor(Color.black);
                        if (doLog) {
                            if (functions[k].getNewYs()[i] == 0.0) {
                                if (k == 0) {
                                    g.fillRect((int) (functions[k].getNewXs()[i] - 1), (int) (yMin - 1),
                                               3, 3);
                                }
        
                                if (k == 1) {
                                    fun2Icon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                       (int) (yMin - 3));
                                }
        
                                if (k == 2) {
                                    fun3Icon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                       (int) (yMin - 3));
                                }
        
                                if (k == 3) {
                                    fun4Icon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                       (int) (yMin - 3));
                                }
        
                                if (k == 4) {
                                    fun5Icon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                       (int) (yMin - 3));
                                }    
                            }
                            else {
                                if (k == 0) {
                                    g.fillRect((int) (functions[k].getNewXs()[i] - 1), (int) (Math.log10(functions[k].getNewYs()[i]) - 1),
                                               3, 3);
                                }
        
                                if (k == 1) {
                                    fun2Icon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                       (int) (Math.log10(functions[k].getNewYs()[i]) - 3));
                                }
        
                                if (k == 2) {
                                    fun3Icon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                       (int) (Math.log10(functions[k].getNewYs()[i]) - 3));
                                }
        
                                if (k == 3) {
                                    fun4Icon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                       (int) (Math.log10(functions[k].getNewYs()[i]) - 3));
                                }
        
                                if (k == 4) {
                                    fun5Icon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                       (int) (Math.log10(functions[k].getNewYs()[i]) - 3));
                                }    
                            }
                        }
                        else {
                            if (k == 0) {
                                g.fillRect((int) (functions[k].getNewXs()[i] - 1), (int) (functions[k].getNewYs()[i] - 1),
                                           3, 3);
                            }
    
                            if (k == 1) {
                                fun2Icon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                   (int) (functions[k].getNewYs()[i] - 3));
                            }
    
                            if (k == 2) {
                                fun3Icon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                   (int) (functions[k].getNewYs()[i] - 3));
                            }
    
                            if (k == 3) {
                                fun4Icon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                   (int) (functions[k].getNewYs()[i] - 3));
                            }
    
                            if (k == 4) {
                                fun5Icon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                   (int) (functions[k].getNewYs()[i] - 3));
                            }
                        }
                    }
                }
            }
        }
    }

    /**
     * Same as the plotGraph method, except the points used in this method are readable when sent to the printer.
     *
     * @param  g  graphics to draw in
     */
    private void plotGraphForPrinter(Graphics g) {
        int i;
        int index, indexExp;
        int len;
        int[] newX;
        int[] newY;
        float xMin = minDomain;
        float xMax = maxDomain;
        float yMin = minRange;
        float yMax = maxRange;
        float tmp;
        String tmpString;

        if (g == null) {
            MipavUtil.displayError("ComponentGraph.plotGraph: graphics = null");

            return;
        }

        len = -1;

        for (i = 0; i < functions.length; i++) {

            if (functions[i].getXs().length != functions[i].getYs().length) {
                return;
            }

            if (len < functions[i].getXs().length) {
                len = functions[i].getXs().length;
            }
        }

        xScale = graphBounds.width / (xMax - xMin);
        yScale = graphBounds.height / (yMax - yMin);

        for (int k = 0; k < functions.length; k++) {

            try {
                newX = new int[functions[k].getXs().length];
                newY = new int[functions[k].getYs().length];
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ViewJComponentGraph.plotGraph");

                return;
            }

            for (i = 0; i < newX.length; i++) {
                newX[i] = (int) Math.round((functions[k].getXs()[i] * xScale) - (xMin * xScale)) + graphBounds.x;
                if (doLog) {
                    if (functions[k].getYs()[i] == 0.0) {
                        newY[i] = (int) Math.round((yMax - yMin) * yScale) + graphBounds.y;    
                    }
                    else {
                        newY[i] = (int) Math.round((yMax - Math.log10(functions[k].getYs()[i])) * yScale) + graphBounds.y;    
                    }
                }
                else {
                    newY[i] = (int) Math.round((yMax - functions[k].getYs()[i]) * yScale) + graphBounds.y;
                }
            }

            functions[k].setNewXs(newX, functions[k].getXs().length);
            functions[k].setNewYs(newY, functions[k].getYs().length);
            g.setColor(functions[k].getColor());

            if (functions[k].getFunctionVisible()) {
                if ((showPointsAndLines == SHOW_POINTS_AND_LINES) || (showPointsAndLines == SHOW_LINES_ONLY)) {    
                    g.drawPolyline(newX, newY, newX.length);
                }
            }
        }

        for (int k = 0; k < fittedFunctions.length; k++) {

            if (functions[k].getFitFunctionVisible()) {

                try {
                    newX = new int[fittedFunctions[k].getXs().length];
                    newY = new int[fittedFunctions[k].getYs().length];
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: ViewJComponentGraph.plotGraph");

                    return;
                }

                for (i = 0; i < newX.length; i++) {
                    newX[i] = (int) Math.round((fittedFunctions[k].getXs()[i] * xScale) - (xMin * xScale)) +
                              graphBounds.x;
                    if (doLog) {
                        if (fittedFunctions[k].getYs()[i] == 0.0) {
                            newY[i] = (int) Math.round((yMax - yMin) * yScale) + graphBounds.y;    
                        }
                        else {
                            newY[i] = (int) Math.round((yMax - Math.log10(fittedFunctions[k].getYs()[i])) * yScale) + graphBounds.y;    
                        }
                    }
                    else {
                        newY[i] = (int) Math.round((yMax - fittedFunctions[k].getYs()[i]) * yScale) + graphBounds.y;
                    }
                }

                fittedFunctions[k].setNewXs(newX, fittedFunctions[k].getXs().length);
                fittedFunctions[k].setNewYs(newY, fittedFunctions[k].getYs().length);
                g.setColor(fittedFunctions[k].getColor());

                if (functions[k].getFunctionVisible()) {
                    if ((showPointsAndLines == SHOW_POINTS_AND_LINES) || (showPointsAndLines == SHOW_LINES_ONLY)) {
                        g.drawPolyline(newX, newY, newX.length);
                    }
                }
            }
        }

        float newYMax = -1;
        float newYMin = -1;
        g.setColor(Color.blue);

        if ((yMax > 999.99) || (yMin < -999.99)) {
            newYMax = yMax;
            newYMin = yMin;

            int index2 = 0;

            while ((newYMax > 999.99) || (newYMin < -999.99)) {
                index2++;
                newYMax = newYMax / 10;
                newYMin = newYMin / 10;
            }

            g.setFont(font12);
            g.drawString("x10", graphBounds.x, graphBounds.y - 5);
            g.setFont(font10);
            g.drawString(String.valueOf(index2), graphBounds.x + 25, graphBounds.y - 10);
            g.setFont(font12);
        } else if ((Math.abs(yMax) < 1.0) && (Math.abs(yMin) < 1.0)) {
            newYMax = yMax;
            newYMin = yMin;

            int index2 = 0;

            while ((Math.abs(newYMax) < 1.0) && (Math.abs(newYMin) < 1.0)) {
                index2--;
                newYMax = newYMax * 10;
                newYMin = newYMin * 10;
            }

            g.setFont(font12);

            // g.drawString("x10", graphBounds.x, graphBounds.y - 5);
            g.drawString("x10", graphBounds.x, graphBounds.y - 10);
            g.setFont(font10);
            g.setColor(Color.lightGray);

            // g.fillRect(graphBounds.x+25, graphBounds.y-15, 10, 10);
            g.fillRect(graphBounds.x + 25, graphBounds.y - 20, 10, 10);
            g.setColor(Color.blue);

            // g.drawString(String.valueOf(index2), graphBounds.x+25, graphBounds.y-10);
            g.drawString(String.valueOf(index2), graphBounds.x + 25, graphBounds.y - 15);
            g.setFont(font12);
        } else {
            g.setFont(font12);
            g.drawString("x10", graphBounds.x, graphBounds.y - 5);
            g.setFont(font10);
            g.drawString(String.valueOf(0), graphBounds.x + 25, graphBounds.y - 10);
            g.setFont(font12);
        }

        for (i = 0; i <= xGridLines; i = i + 2) {
            tmp = (i * (xMax - xMin) / xGridLines) + xMin;
            tmpString = String.valueOf(tmp);
            index = tmpString.indexOf('.');

            if ((index + 3) <= tmpString.length()) {
                tmpString = tmpString.substring(0, index + 3);
            }

            g.drawString(tmpString, graphBounds.x + (int) Math.round(i * xTick) - 20,
                         graphBounds.y + graphBounds.height + 20);
        }

        for (i = 1; i <= xGridLines; i = i + 2) {
            tmp = (i * (xMax - xMin) / xGridLines) + xMin;
            tmpString = String.valueOf(tmp);
            index = tmpString.indexOf('.');

            if ((index + 3) <= tmpString.length()) {
                tmpString = tmpString.substring(0, index + 3);
            }

            g.drawString(tmpString, graphBounds.x + (int) Math.round(i * xTick) - 20,
                         graphBounds.y + graphBounds.height + 30);
        }

        int j = yGridLines;

        for (i = 0; i <= yGridLines; i++) {

            if (newYMax != -1) {
                tmp = (i * (newYMax - newYMin) / yGridLines) + newYMin;
            } else {
                tmp = (i * (yMax - yMin) / yGridLines) + yMin;
            }
            if (doLog) {
                if (zeroYMin && tmp == yMin) {
                    tmp = 0.0f;
                }
                else {
                    tmp = (float)Math.pow(10.0,tmp);
                }
            }

            tmpString = String.valueOf(tmp);
            index = tmpString.indexOf('.');
            indexExp = tmpString.toUpperCase().indexOf('E');

            if ((index + 3) <= tmpString.length()) {

                if (indexExp > -1) {
                    tmpString = tmpString.substring(0, index + 3) + tmpString.substring(indexExp);
                } else {
                    tmpString = tmpString.substring(0, index + 3);
                }
            }

            g.drawString(tmpString, graphBounds.x - 50, graphBounds.y + (int) Math.round(j * yTick) + 5);
            j--;
        }

        if ((showPointsAndLines == SHOW_POINTS_AND_LINES) || (showPointsAndLines == SHOW_POINTS_ONLY)) {

            for (int k = 0; k < functions.length; k++) {

                if (functions[k].getFunctionVisible()) {

                    for (i = 0; i < functions[k].getNewXsLength(); i++) {
                        g.setColor(Color.black);
                        if (doLog) {
                            if (functions[k].getNewYs()[i] == 0.0) {
                                if (k == 0) {
                                    g.fillRect((int) (functions[k].getNewXs()[i] - 1), (int) (yMin - 1),
                                               3, 3);
                                }
        
                                if (k == 1) {
                                    fun2PrintIcon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                       (int) (yMin - 3));
                                }
        
                                if (k == 2) {
                                    fun3PrintIcon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                       (int) (yMin - 3));
                                }
        
                                if (k == 3) {
                                    fun4PrintIcon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                       (int) (yMin - 3));
                                }
        
                                if (k == 4) {
                                    fun5PrintIcon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                       (int) (yMin - 3));
                                }    
                            }
                            else {
                                if (k == 0) {
                                    g.fillRect((int) (functions[k].getNewXs()[i] - 1), (int) (Math.log10(functions[k].getNewYs()[i]) - 1),
                                               3, 3);
                                }
        
                                if (k == 1) {
                                    fun2PrintIcon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                       (int) (Math.log10(functions[k].getNewYs()[i]) - 3));
                                }
        
                                if (k == 2) {
                                    fun3PrintIcon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                       (int) (Math.log10(functions[k].getNewYs()[i]) - 3));
                                }
        
                                if (k == 3) {
                                    fun4PrintIcon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                       (int) (Math.log10(functions[k].getNewYs()[i]) - 3));
                                }
        
                                if (k == 4) {
                                    fun5PrintIcon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                       (int) (Math.log10(functions[k].getNewYs()[i]) - 3));
                                }    
                            }
                        }
                        else {
                            if (k == 0) {
                                g.fillRect((int) (functions[k].getNewXs()[i] - 1), (int) (functions[k].getNewYs()[i] - 1),
                                           3, 3);
                            }
    
                            if (k == 1) {
                                fun2PrintIcon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                   (int) (functions[k].getNewYs()[i] - 3));
                            }
    
                            if (k == 2) {
                                fun3PrintIcon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                   (int) (functions[k].getNewYs()[i] - 3));
                            }
    
                            if (k == 3) {
                                fun4PrintIcon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                   (int) (functions[k].getNewYs()[i] - 3));
                            }
    
                            if (k == 4) {
                                fun5PrintIcon.paintIcon(null, g, (int) (functions[k].getNewXs()[i] - 3),
                                                   (int) (functions[k].getNewYs()[i] - 3));
                            }
                        }
                    }
                }
            }
        }
    }


}
