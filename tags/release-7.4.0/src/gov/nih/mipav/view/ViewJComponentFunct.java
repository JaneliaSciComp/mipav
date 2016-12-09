package gov.nih.mipav.view;


import gov.nih.mipav.model.structures.*;

import java.awt.*;

import javax.swing.*;


/**
 * This is a custom made Swing component. It is coordinates for the ViewJComponentGraph as well as the line color. The
 * graph then uses this information to draw the function.
 *
 * @version  1.0 Jun 1, 1999
 * @author   Neva Cherniavsky
 * @see      ViewJComponentGraph
 */
public class ViewJComponentFunct extends JComponent {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = -2525400551576940334L;

    /** Maximum size of the coordinate arrays. */
    public static final int MAX_NUM_COORDS = 1000;

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    public float[] X;

    /** DOCUMENT ME! */
    public float[] Y;
    
    /** xy coordinates of voi boundar **/
    public int[][] XYCoords;

    /** DOCUMENT ME! */
    private String functName;

    /** DOCUMENT ME! */
    private Color lineColor;

    /** DOCUMENT ME! */
    private int[] newX;

    /** DOCUMENT ME! */
    private int newXLength;

    /** DOCUMENT ME! */
    private int[] newY;

    /** DOCUMENT ME! */
    private int newYLength;

    /** DOCUMENT ME! */
    private float[] originalX;

    /** DOCUMENT ME! */
    private float[] originalY;

    /** DOCUMENT ME! */
    private boolean showFitFunction = false;

    /** DOCUMENT ME! */
    private boolean showFunction = true;

    /** DOCUMENT ME! */
    private VOI voi;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor - creates empty function.
     */
    public ViewJComponentFunct() {
        voi = null;
    }

    /**
     * Constructor - creates functions with specified coordinates and color.
     *
     * @param  x          x coordinates of the function
     * @param  y          y coordinates of the function
     * @param  lineColor  color for the function
     * @param  v          voi this is drawn for if it is for a point, should be null otherwise
     */
    public ViewJComponentFunct(float[] x, float[] y, Color lineColor, VOI v,int[][] xyCoords) {
        X = x;
        Y = y;
        XYCoords = xyCoords;
        originalX = x;
        originalY = y;
        this.lineColor = lineColor;
        voi = v;
    }

    /**
     * Constructor - creates functions with specified coordinates, and number for name.
     *
     * @param  x     x coordinates of the function
     * @param  y     y coordinates of the function
     * @param  name  integer for name of function
     * @param  v     voi this is drawn for if it is for a point, should be null otherwise
     */
    public ViewJComponentFunct(float[] x, float[] y, int name, VOI v, int[][] xyCoords) {
        functName = "Function " + name;
        X = x;
        Y = y;
        XYCoords = xyCoords;
        originalX = x;
        originalY = y;
        setColor(name - 1);
        voi = v;
    }

    /**
     * Constructor - creates functions with specified coordinates, color, and name string.
     *
     * @param  x          x coordinates of the function
     * @param  y          y coordinates of the function
     * @param  lineColor  color for the function
     * @param  name       name of function
     * @param  v          voi this is drawn for if it is for a point, should be null otherwise
     */
    public ViewJComponentFunct(float[] x, float[] y, Color lineColor, String name, VOI v) {
        X = x;
        Y = y;
        originalX = x;
        originalY = y;
        this.lineColor = lineColor;
        functName = name;
        voi = v;
    }

    /**
     * Constructor - creates functions with specified coordinates, color, and number for name.
     *
     * @param  x          x coordinates of the function
     * @param  y          y coordinates of the function
     * @param  lineColor  color for the function
     * @param  name       integer for name of function
     * @param  v          voi this is drawn for if it is for a point, should be null otherwise
     */
    public ViewJComponentFunct(float[] x, float[] y, Color lineColor, int name, VOI v, int[][] xyCoords) {
        functName = "Function " + name;
        X = x;
        Y = y;
        XYCoords = xyCoords;
        originalX = x;
        originalY = y;
        this.lineColor = lineColor;
        voi = v;
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * getColor - gets the color of the line on the graph.
     *
     * @return  color of the line on the graph
     */
    public Color getColor() {
        return lineColor;
    }

    /**
     * getFitFunctionVisible - accessor that gets the visible flag of the fitted function.
     *
     * @return  the boolean visible flag
     */
    public boolean getFitFunctionVisible() {
        return showFitFunction;
    }

    /**
     * getFunctionVisible - accessor that gets the visible flag of the function.
     *
     * @return  the boolean visible flag
     */
    public boolean getFunctionVisible() {
        return showFunction;
    }

    /**
     * getFunctNames - gets the names of the functions.
     *
     * @return  names of the functions
     */
    public String getFunctName() {
        return functName;
    }

    /**
     * getNewXs - accessor that gets the array of x coordinates to be plotted; used by the graphics for the actual
     * plotting of the graph.
     *
     * @return  the array of x coordinates
     */
    public int[] getNewXs() {
        return newX;
    }

    /**
     * Accessor that gets the number of x coordinates to be plotted.
     *
     * @return  DOCUMENT ME!
     */
    public int getNewXsLength() {
        return newXLength;
    }

    /**
     * getNewYs - accessor that gets the array of y coordinates to be plotted used by the graphics for the actual
     * plotting of the graph.
     *
     * @return  the array of y coordinates
     */
    public int[] getNewYs() {
        return newY;
    }

    /**
     * Accessor that gets the number of y coordinates to be plotted.
     *
     * @return  DOCUMENT ME!
     */
    public int getNewYsLength() {
        return newYLength;
    }

    /**
     * getOriginalXs - accessor that gets the array of x coordinates to be plotted; used by the zoom to reset the graph.
     *
     * @return  the array of x coordinates
     */
    public float[] getOriginalXs() {
        return originalX;
    }

    /**
     * getOriginalYs - accessor that gets the array of y coordinates to be plotted used by the zoom for the rest of the
     * graph.
     *
     * @return  the array of y coordinates
     */
    public float[] getOriginalYs() {
        return originalY;
    }

    /**
     * gets the voi associated with this function if it corresponds to a point, otherwise returns null.
     *
     * @return  DOCUMENT ME!
     */
    public VOI getVOI() {
        return voi;
    }

    /**
     * getXs - accessor that gets the array of x coordinates to be plotted.
     *
     * @return  the array of x coordinates
     */
    public float[] getXs() {
        return X;
    }
    
    
    public int[][] getXYCoords() {
    	return XYCoords;
    }

    /**
     * getYs - accessor that gets the array of y coordinates to be plotted.
     *
     * @return  the array of y coordinates
     */
    public float[] getYs() {
        return Y;
    }

    /**
     * setColor - sets the color of the line on the graph.
     *
     * @param  color  color to set the line to
     */
    public void setColor(Color color) {
        lineColor = color;
    }

    /**
     * setColor - sets the color of the line on the graph.
     *
     * @param  color  code for the color to set the line to
     */
    public void setColor(int color) {

        switch (color) {

            case 0:
                lineColor = Color.red;
                break;

            case 1:
                lineColor = Color.green.darker().darker();
                break;

            case 2:
                lineColor = Color.blue;
                break;

            case 3:
                lineColor = Color.black;
                break;

            case 4:
                lineColor = Color.yellow;
                break;
        }
    }

    /**
     * setFitFunctionVisible - accessor that tells whether to show the fitted function.
     *
     * @param  visible  boolean to set it to
     */
    public void setFitFunctionVisible(boolean visible) {
        showFitFunction = visible;
    }

    /**
     * setFunctionVisible - accessor that tells whether to show the function.
     *
     * @param  visible  boolean to set it to
     */
    public void setFunctionVisible(boolean visible) {
        showFunction = visible;
    }

    /**
     * setFunctName - sets the names of the functons.
     *
     * @param  name  - String of function name
     */
    public void setFunctName(String name) {
        functName = name;
    }

    /**
     * setFunctName - sets the names of the functions to default names (function + number).
     *
     * @param  i  - number of functions
     */
    public void setFunctName(int i) {
        functName = "Function " + i;
    }

    /**
     * setNewXs - accessor that sets the integer array of x coordinates to be plotted; used by the graphics for the
     * actual plotting of the graph.
     *
     * @param  oldX  the array of x coordinates
     * @param  len   number of coordinates
     */
    public void setNewXs(int[] oldX, int len) {

        try {
            newX = new int[oldX.length];
            newXLength = len;
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ComponentGraph.setXs");

            return;
        }

        for (int i = 0; i < oldX.length; i++) {
            newX[i] = (int) oldX[i];
        }

    }

    /**
     * setNewYs - accessor that sets the integer array of y coordinates to be plotted; used by the graphics for the
     * actual plotting of the graph.
     *
     * @param  oldY  the array of y coordinates
     * @param  len   the number of y coordinates
     */
    public void setNewYs(int[] oldY, int len) {

        try {
            newY = new int[oldY.length];
            newYLength = len;
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ComponentGraph.setYs");

            return;
        }

        for (int i = 0; i < oldY.length; i++) {
            newY[i] = (int) oldY[i];
        }
    }

    /**
     * setOriginalXs - accessor that sets the array of x coordinates to be plotted; used by the zoom to reset the graph.
     *
     * @param  oldX  the array of x coordinates
     */
    public void setOriginalXs(float[] oldX) {

        try {
            originalX = new float[oldX.length];
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ComponentGraph.setXs");

            return;
        }

        for (int i = 0; i < oldX.length; i++) {
            originalX[i] = oldX[i];
        }

    }

    /**
     * setOriginalYs - accessor that sets the array of y coordinates to be plotted; used by the zoom for reseting the
     * graph.
     *
     * @param  oldY  the array of y coordinates
     */
    public void setOriginalYs(float[] oldY) {

        try {
            originalY = new float[oldY.length];
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ComponentGraph.setYs");

            return;
        }

        for (int i = 0; i < oldY.length; i++) {
            originalY[i] = oldY[i];
        }
    }


    /**
     * setXs - accessor that sets the array of x coordinates to be plotted.
     *
     * @param  oldX  the array of x coordinates
     */
    public void setXs(float[] oldX) {

        try {
            X = new float[oldX.length];
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ComponentGraph.setXs");

            return;
        }

        for (int i = 0; i < oldX.length; i++) {
            X[i] = oldX[i];
        }

    }

    /**
     * setYs - accessor that sets the array of y coordinates to be plotted.
     *
     * @param  oldY  the array of y coordinates
     */
    public void setYs(float[] oldY) {

        try {
            Y = new float[oldY.length];
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ComponentGraph.setYs");

            return;
        }

        for (int i = 0; i < oldY.length; i++) {
            Y[i] = oldY[i];
        }
    }


}
