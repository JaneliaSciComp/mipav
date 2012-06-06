package gov.nih.mipav.view;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import java.awt.*;
import java.awt.event.*;

import java.io.*;
import java.lang.reflect.Constructor;


import java.util.*;

import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;


/**
 * This is a frame which holds a graph in one panel and a set of user buttons in the other. It uses swing components for
 * all of the graphics and calls ViewJComponentGraph for the graph.
 *
 * @version  1.0 Aug 1, 1999
 * @author   Neva Cherniavsky (primary)
 * @author   Harman Singh
 * @see      ViewJComponentGraph
 */

public class ViewJFrameGraph extends JFrame
        implements ComponentListener, ActionListener, ChangeListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 5127841803361813975L;
    
    public enum FitMode {
    	/** Mode indicates no curve fitting is taking place */
    	fitNone("None", null),
    	
    	/** Mode indicates linear fitting in progress */
    	fitLinear("Fit linear (a1*x + a0)", FitLine.class),
    	
    	/** Mode indicates exponential fitting in progress */
    	fitExp("Fit exponential (a0+a1*exp(a2*x))", FitExponential.class),
    	
    	/** Mode indicates Gaussian fitting in progress*/
    	fitGaussian("Fit Gaussian (A*exp(-(X-Xo)^2/(2sigma^2)))", FitGaussian.class),
    	
    	/** Mode indicates Laplace fitting in progress*/
    	fitLaplace("Fit Laplace (A*exp(-|x-mu|/beta))", FitLaplace.class),
    	
    	/** Mode indicates Lorentz fitting in progress */
    	fitLorentz("Fit Lorentz Distribution", FitLorentz.class),
    	
    	/** Mode indicates constant plus sum of decaying exponentials */
    	fitMultiExponential("Fit Multiexponential a0 + sum of a[2*k+1]*exp(a[2*k+2]*x)", FitMultiExponential.class),
    	
    	/** Mode indicates Rayleigh fitting in progress */
    	fitRayleigh("Fit Rayleigh Distribution scale * (2/b)*(x-a)*exp(-(x-a)*(x-a)/b)*u(x-a)", FitRayleigh.class);
    	
    	private String listEntry;
    	private Constructor cl;
    	
    	FitMode(String listEntry, Class cl) {
    		this.listEntry = listEntry;
    		if(cl != null) {
	    		try {
	    			if (cl == FitMultiExponential.class) {
					    this.cl = cl.getConstructor(int.class, int.class, float[].class, float[].class);
	    			}
	    			else {
	    				this.cl = cl.getConstructor(int.class, float[].class, float[].class);	
	    			}
				} catch (SecurityException e) {
					e.printStackTrace();
				} catch (NoSuchMethodException e) {
					e.printStackTrace();
				}
    		} else
    			this.cl = null;
    	}
    	
    	public String toString() {
    		return listEntry;
    	}
    	
    	public Constructor getImpl() {
    		return cl;
    	}
    }
    
    //~ GUI Constants --------------------------------------------------------------------------------------------------
    
    /** Minimum width of the graph panel */
    private static final int MINIMUM_WIDTH = 460;

    /** Minimum height of the graph panel */
    private static final int MINIMUM_HEIGHT = 335;
    
    /** Minimum distance between frame and subpanels */
    private static final int PANEL_OFFSET = 10;
    
    

    //~ Instance fields ------------------------------------------------------------------------------------------------

    /** DOCUMENT ME! */
    private boolean autoShrinkRange = true;

    /** DOCUMENT ME! */
    private JCheckBox autoShrinkRangeCheckbox;

    /** DOCUMENT ME! */
    private JPanel axisPanel;

    /** DOCUMENT ME! */
    private JButton backgroundButton;

    /** DOCUMENT ME! */
    private JLabel backgroundLabel;

    /** DOCUMENT ME! */
    private JPanel backgroundPanel;

    /** DOCUMENT ME! */
    private ViewJColorChooser colorChooser;

    /** DOCUMENT ME! */
    private JDialog colorDialog;

    /** DOCUMENT ME! */
    private JMenu copyMenu;

    /** DOCUMENT ME! */
    private String defaultDirectory = null;

    /** DOCUMENT ME! */
    private JMenu deleteMenu;

    /** DOCUMENT ME! */
    private JMenu editMenu;

    /** DOCUMENT ME! */
    private JMenu fileMenu;

    /** DOCUMENT ME! */
    private JPanel fitFunctPanel;

    /** DOCUMENT ME! */
    private JPanel fitFunctTypePanel;

    /** DOCUMENT ME! */
    private JCheckBox[] fitFunctVisibleCheckbox;

    /** DOCUMENT ME! */
    private JPanel fitFunctVisiblePanel;

    /** Current mode for fitting functions */
    private FitMode fitMode = FitMode.fitNone;

    /** DOCUMENT ME! */
    private VOI frameGraphVOI = null;

    /** DOCUMENT ME! */
    private int functionIndex;

    /** DOCUMENT ME! */
    private JButton[] functLineColorButton;

    /** the slope changes. */
    private JCheckBox[] functVisibleCheckbox;

    /** DOCUMENT ME! */
    private ViewJComponentGraph graph;

    /** DOCUMENT ME! */
    private JCheckBox gridlinesCheckbox; // check-box for displaying grid lines

    /** DOCUMENT ME! */
    private JPanel gridPanel;

    /** DOCUMENT ME! */
    private JMenuItem itemClose;

    /** DOCUMENT ME! */
    private JMenuItem itemModifyGraph;

    /** DOCUMENT ME! */
    private JMenuItem itemNormalize;
    
    private JMenuItem itemOpenNewGraph;

    /** DOCUMENT ME! */
    private JMenuItem itemOpenSameGraph;

    /** DOCUMENT ME! */
    private JMenuItem itemPasteFunct;

    /** DOCUMENT ME! */
    private JMenuItem itemPrintGraph;

    /** DOCUMENT ME! */
    private JMenuItem itemResetGraph;

    /** DOCUMENT ME! */
    private JMenuItem itemResetRange;

    /** DOCUMENT ME! */
    private JMenuItem itemSaveGraph;
    
    private JMenuItem itemTableOutput;

    /** DOCUMENT ME! */
    private String lastUnits = null;

    /** DOCUMENT ME! */
    private JCheckBox legendCheckbox; // check-box for displaying the legend

    /** DOCUMENT ME! */
    private JTextField[] nameField; // array of text fields for changing function names in legendDialog

    /** DOCUMENT ME! */
    private JLabel[] functionLabel; // array of labels for changing corresponding functions in legendDialog

    /** DOCUMENT ME! */
    private JPanel legendPanel;

    /** DOCUMENT ME! */
    private JPanel mainPanel;

    /** DOCUMENT ME! */
    private JTextField maxRangeField = null;

    /** DOCUMENT ME! */
    private JLabel maxRangeLabel;

    /** DOCUMENT ME! */
    private JCheckBox minorTickMarksCheckbox; // check-box for showing minor tick marks

    /** DOCUMENT ME! */
    private JTextField minRangeField = null;

    /** DOCUMENT ME! */
    private JLabel minRangeLabel;

    /** DOCUMENT ME! */
    private JDialog modifyDialog;

    /** DOCUMENT ME! */
    private JPanel modifyGraphPanel;

    /** DOCUMENT ME! */
    private JPanel functionPropPanel;

    /** DOCUMENT ME! */
    private JMenuBar openingMenuBar;

    /** DOCUMENT ME! */
    private JRadioButton linesOnlyButton;
    
    private JRadioButton pointsAndLinesButton;
    
    private JRadioButton pointsOnlyButton;
    
    private ButtonGroup pointsAndLinesGroup;

    /** DOCUMENT ME! */
    private JPanel pointsVisiblePanel;

    /** Combo box for selecting the fitting function type*/
    private JComboBox fitType;

    /** DOCUMENT ME! */
    private JPanel rangePanel;

    /** DOCUMENT ME! */
    private JButton resetRangeButton;
    
    private JCheckBox logScaleCheckbox;
    
    private boolean doLog = false;

    /** DOCUMENT ME! */
    private JPanel showPanel;

    /** DOCUMENT ME! */
    private JTabbedPane tabbedPane;

    /** DOCUMENT ME! */
    private JTextField titleField;

    /** DOCUMENT ME! */
    private JLabel titleLabel;

    /** DOCUMENT ME! */
    private String units = null;

    /** DOCUMENT ME! */
    private int updateRGBIndex = -1;

    /** DOCUMENT ME! */
    private Vector<JMenuItem> vectorCopyFunct;

    /** DOCUMENT ME! */
    private Vector<JMenuItem> vectorDeleteFunct;

    /** DOCUMENT ME! */
    private JMenu viewMenu;

    /** DOCUMENT ME! */
    private VOI voi = null;

    /** DOCUMENT ME! */
    private JLabel xAxisLabel;

    /** DOCUMENT ME! */
    private JTextField xAxisLabelField;

    /** DOCUMENT ME! */
    private JTextField xGridLineField;

    /** DOCUMENT ME! */
    private JLabel xGridLineLabel;

    /** DOCUMENT ME! */
    private JLabel yAxisLabel;

    /** DOCUMENT ME! */
    private JTextField yAxisLabelField;

    /** DOCUMENT ME! */
    private JTextField yGridLineField;

    /** DOCUMENT ME! */
    private JLabel yGridLineLabel;
    
    /**
     * this is a list of the x,y coords of the voi's boundary
     */
    private int[][] xyCoords;
    
    private JLabel numVariablesLabel;
    
    private JTextField numVariablesField;
    
    private int numVariables = 5;
    
    private ModelImage image = null;
    
    private int RGBOffset = 0;
    
    private boolean entireImage = true;
    
    private int initialSummaryBins;
    private int summaryBins;
    private double initialUserMin;
    private double userMin;
    private double initialUserMax;
    private double userMax;
    private JTextField minField = null;
    private JTextField maxField = null;
    private JTextField binsField = null;
    private double imageMin;
    private double imageMax;
    private int maxBins;
    
    
    private JPanel histogramPanel = null;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Constructor Constructs the frame, with the graph component in one panel and the user options in the other. No
     * function is loaded by default, but the user can open a new function to be displayed
     *
     * @param  title        DOCUMENT ME!
     * @param  openFileGUI  DOCUMENT ME!
     */
    public ViewJFrameGraph(String title, boolean openFileGUI) {
        super(title);
        voi = null;

        float[] x = new float[1];
        float[] y = new float[1];
        int[][] xyc = new int[1][1];
        x[0] = 0;
        y[0] = 0;
        xyc[0][0] = 0;
        setBounds(0, 0, 500, 400);

        ViewJComponentFunct[] functArray;
        ViewJComponentFunct[] fittedFuncts;

        // basic setup for frame
        try {
            mainPanel = new JPanel();
            mainPanel.setBounds(0, 0, getSize().width, getSize().height);
            graph = new ViewJComponentGraph(this, mainPanel.getBounds().width, mainPanel.getBounds().height - 60);
            functArray = new ViewJComponentFunct[1];
            functArray[0] = new ViewJComponentFunct(x, y, Color.red, 1, voi,xyc); // empty function
            fittedFuncts = new ViewJComponentFunct[1];
            fittedFuncts[0] = new ViewJComponentFunct(x, y, Color.red, voi,xyc); // empty function
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph constructor");

            return;
        }

        addNotify();
        setResizable(true);

        try {
            setIconImage(MipavUtil.getIconImage("graph.gif"));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage() +
                               ">.  Check that this file is available.\n");
        }

        setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        mainPanel.setLayout(null);

        mainPanel.setBackground(Color.lightGray);
        mainPanel.setForeground(Color.black);

        graph.setFuncts(functArray);
        graph.setFittedFuncts(fittedFuncts);

        buildMenu();

        graph.setNumberOfXGridLines(4); // default number of gridlines to be shown when there is no function
        graph.setNumberOfYGridLines(4);
        graph.setTitle(title); // sets graph's title to same as frame's title
        graph.setLabels("Position on Curve (pixels)", "Intensity"); // default axis labels
        graph.setBackground(Color.red);

        graph.setPointsAndLinesDisplay(ViewJComponentGraph.SHOW_LINES_ONLY);
        graph.setGridlinesVisible(true);
        graph.setMinorTickMarksVisible(true);

        mainPanel.add(graph);

        getContentPane().add(mainPanel);
        setJMenuBar(openingMenuBar);

        addComponentListener(this);
        addWindowListener(this);

        if (x.length != y.length) {
            MipavUtil.displayError("X and Y Arrays must be of equal length");
            this.dispose();
        }

        try {

            if (openFileGUI == true) {

                if (!openSame()) {
                    dispose();

                    return;
                }
            }
        } catch (IOException e) {
            MipavUtil.displayError("Error: " + e);
        }

        setVisible(true);

    }

    /**
     * Constructor Constructs the frame, with the graph component in one panel and the user options in the other. Draws
     * initial default graph of desired function / intensity plot.
     *
     * @param  xInit  the array of x coordinates to be plotted in the graph
     * @param  yInit  the array of y coordinates to be plotted in the graph
     * @param  title  the title of the frame
     */
    public ViewJFrameGraph(float[] xInit, float[] yInit, String title, int[][] xyCoords) {
        super(title);

        ViewJComponentFunct[] functArray;
        ViewJComponentFunct[] fittedFuncts;
        
        this.xyCoords = xyCoords;

        voi = null;
        setBounds(0, 0, 500, 400);

        float[] x = new float[xInit.length];
        float[] y = new float[xInit.length];
        int[][] xyC = new int[xInit.length][2];
        
        for (int i = 0; i < xInit.length; i++) {
            x[i] = xInit[i];
            y[i] = yInit[i];
            if ( xyCoords != null )
            {
                xyC[i][0] = xyCoords[i][0];
                xyC[i][1] = xyCoords[i][1];
            }
        }

        try {
            mainPanel = new JPanel();
            mainPanel.setBounds(0, 0, getSize().width, getSize().height);
            graph = new ViewJComponentGraph(this, mainPanel.getBounds().width, mainPanel.getBounds().height - 60);
            functArray = new ViewJComponentFunct[1];
            functArray[0] = new ViewJComponentFunct(x, y, Color.red, 1, voi,xyC); // empty function
            fittedFuncts = new ViewJComponentFunct[1];
            fittedFuncts[0] = new ViewJComponentFunct(x, y, Color.red, voi,xyC); // empty function
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph constructor");

            return;
        }

        addNotify();
        setResizable(true);

        try {
            setIconImage(MipavUtil.getIconImage("graph.gif"));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage() +
                               ">.  Check that this file is available.\n");
        }

        // setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        mainPanel.setLayout(null);
        mainPanel.setBackground(Color.lightGray);
        mainPanel.setForeground(Color.black);

        graph.setFuncts(functArray);
        graph.setFittedFuncts(fittedFuncts);

        graph.calculateDefaultRangeDomain();
        graph.setDefaultRangeDomain();

        buildMenu();

        graph.setNumberOfXGridLines(4); // draws default number of grid lines
        graph.setNumberOfYGridLines(4);
        graph.setTitle(title); // sets title above graph to same as window title
        graph.setLabels("Position on Curve (pixels)", "Intensity"); // sets default axis labels
        graph.setBackground(Color.red);

        graph.setPointsAndLinesDisplay(ViewJComponentGraph.SHOW_LINES_ONLY);
        graph.setGridlinesVisible(true);
        graph.setMinorTickMarksVisible(true);

        mainPanel.add(graph);

        getContentPane().add(mainPanel);
        setJMenuBar(openingMenuBar);

        addComponentListener(this);
        addWindowListener(this);

        if (xInit.length != yInit.length) { // must be pairs of x and y coordinates, or the graph cannot be drawn
            MipavUtil.displayError("X and Y Arrays must be of equal length");
            this.dispose();
        } else {
            setVisible(true);
        }

    }

    /**
     * Constructor Constructs the frame, with the graph component in one panel and the user options in the other. Draws
     * initial default graph of desired function / intensity plot.
     *
     * @param  xInit  the array of x coordinates to be plotted in the graph
     * @param  yInit  the array of y coordinates to be plotted in the graph
     * @param  title  the title of the frame
     */
    public ViewJFrameGraph(float[][] xInit, float[][] yInit, String title) {
        super(title);
        voi = null;
        updateRGBIndex = 0;

        ViewJComponentFunct[] functArray;
        ViewJComponentFunct[] fittedFuncts;
        Color color = Color.red;
        int i, j;
        setBounds(0, 0, 500, 400);

        float[][] x = new float[xInit.length][xInit[0].length];
        float[][] y = new float[xInit.length][xInit[0].length];

        for (i = 0; i < xInit.length; i++) {

            for (j = 0; j < xInit[0].length; j++) {
                x[i][j] = xInit[i][j];
                y[i][j] = yInit[i][j];
            }
        }

        try {
            mainPanel = new JPanel();
            mainPanel.setBounds(0, 0, getSize().width, getSize().height);
            graph = new ViewJComponentGraph(this, mainPanel.getBounds().width, mainPanel.getBounds().height - 60);
            functArray = new ViewJComponentFunct[x.length];
            fittedFuncts = new ViewJComponentFunct[x.length];

            for (i = 0; i < x.length; i++) {

                if (i == 0) {
                    color = Color.red;
                } else if (i == 1) {
                    color = Color.green.darker();
                } else if (i == 2) {
                    color = Color.blue;
                } else if (i == 3) {
                    color = Color.black;
                } else if (i == 4) {
                    color = Color.yellow;
                }

                functArray[i] = new ViewJComponentFunct(x[i], y[i], color, i + 1, voi,null); // empty function
                fittedFuncts[i] = new ViewJComponentFunct(x[i], y[i], color, voi,null); // empty function
            }
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph constructor");

            return;
        }

        addNotify();
        setResizable(true);

        try {
            setIconImage(MipavUtil.getIconImage("graph.gif"));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage() +
                               ">.  Check that this file is available.\n");
        }

        // setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        mainPanel.setLayout(null);
        mainPanel.setBackground(Color.lightGray);
        mainPanel.setForeground(Color.black);

        graph.setFuncts(functArray);
        graph.setFittedFuncts(fittedFuncts);

        graph.calculateDefaultRangeDomain();
        graph.setDefaultRangeDomain();

        buildMenu();

        graph.setNumberOfXGridLines(4); // draws default number of grid lines
        graph.setNumberOfYGridLines(4);
        graph.setTitle(title); // sets title above graph to same as window title
        graph.setLabels("Position on Curve (pixels)", "Intensity"); // sets default axis labels
        graph.setBackground(Color.red);

        graph.setPointsAndLinesDisplay(ViewJComponentGraph.SHOW_LINES_ONLY);
        graph.setGridlinesVisible(true);
        graph.setMinorTickMarksVisible(true);

        mainPanel.add(graph);

        getContentPane().add(mainPanel);
        setJMenuBar(openingMenuBar);
        addComponentListener(this);
        addWindowListener(this);

        if (xInit.length != yInit.length) { // must be pairs of x and y coordinates, or the graph cannot be drawn
            MipavUtil.displayError("X and Y Arrays must be of equal length");
            this.dispose();
        } else {
            setVisible(true);
        }

    }

    /**
     * Constructor Constructs the frame, with the graph component in one panel and the user options in the other. Graph
     * is assumed to be made from a VOIPoint. Draws initial default graph of desired function / intensity plot.
     *
     * @param  x      the array of x coordinates to be plotted in the graph
     * @param  y      the array of y coordinates to be plotted in the graph
     * @param  title  the title of the frame
     * @param  v      the VOI the graph is being made for
     */
    public ViewJFrameGraph(float[] x, float[] y, String title, VOI v) {
        this(x, y, title, v, null,null);
    }

    /**
     * Constructor Constructs the frame, with the graph component in one panel and the user options in the other. Graph
     * is assumed to be made from a VOIPoint. Draws initial default graph of desired function / intensity plot.
     *
     * @param  x      the array of x coordinates to be plotted in the graph
     * @param  y      the array of y coordinates to be plotted in the graph
     * @param  title  the title of the frame
     * @param  v      the VOI the graph is being made for
     */
    public ViewJFrameGraph(float[][] x, float[][] y, String title, VOI v) {
        this(x, y, title, v, null);
    }
    
    /**
     * Constructor Constructs the frame, with the graph component in one panel and the user options in the other. Draws
     * initial default graph of desired function / intensity plot.
     *
     * @param  xInit   the array of x coordinates to be plotted in the graph
     * @param  yInit   the array of y coordinates to be plotted in the graph
     * @param  title   the title of the frame
     * @param  labelX  x axis label
     * @param  labelY  y aixs label
     */
    public ViewJFrameGraph(ModelImage image, int RGBOffset, boolean entireImage, int initialSummaryBins,
                           double initialUserMin, double initialUserMax, 
                           float[] xInit, float[] yInit, String title, String labelX, String labelY) {
        super(title);
        
        this.image = image;
        this.RGBOffset = RGBOffset;
        this.entireImage = entireImage;
        this.initialSummaryBins = initialSummaryBins;
        this.initialUserMin = initialUserMin;
        this.initialUserMax = initialUserMax;

        ViewJComponentFunct[] functArray;
        ViewJComponentFunct[] fittedFuncts;

        voi = null;
        setBounds(0, 0, 500, 400);

        float[] x = new float[xInit.length];
        float[] y = new float[xInit.length];

        for (int i = 0; i < xInit.length; i++) {
            x[i] = xInit[i];
            y[i] = yInit[i];
        }

        try {
            mainPanel = new JPanel();
            mainPanel.setBounds(0, 0, getSize().width, getSize().height);
            graph = new ViewJComponentGraph(this, mainPanel.getBounds().width, mainPanel.getBounds().height - 60);
            functArray = new ViewJComponentFunct[1];
            functArray[0] = new ViewJComponentFunct(x, y, Color.red, 1, voi,null); // empty function
            fittedFuncts = new ViewJComponentFunct[1];
            fittedFuncts[0] = new ViewJComponentFunct(x, y, Color.red, voi,null); // empty function
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph constructor");

            return;
        }

        addNotify();
        setResizable(true);

        try {
            setIconImage(MipavUtil.getIconImage("graph.gif"));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage() +
                               ">.  Check that this file is available.\n");
        }

        // setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        mainPanel.setLayout(null);
        mainPanel.setBackground(Color.lightGray);
        mainPanel.setForeground(Color.black);

        graph.setFuncts(functArray);
        graph.setFittedFuncts(fittedFuncts);

        graph.calculateDefaultRangeDomain();
        graph.setDefaultRangeDomain();

        buildMenu();

        graph.setNumberOfXGridLines(4); // draws default number of grid lines
        graph.setNumberOfYGridLines(4);
        graph.setTitle(title); // sets title above graph to same as window title
        graph.setLabels(labelX, labelY);
        graph.setBackground(Color.red);

        graph.setPointsAndLinesDisplay(ViewJComponentGraph.SHOW_LINES_ONLY);
        graph.setGridlinesVisible(true);
        graph.setMinorTickMarksVisible(true);

        mainPanel.add(graph);

        getContentPane().add(mainPanel);
        setJMenuBar(openingMenuBar);

        addComponentListener(this);
        addWindowListener(this);

        if (xInit.length != yInit.length) { // must be pairs of x and y coordinates, or the graph cannot be drawn
            MipavUtil.displayError("X and Y Arrays must be of equal length");
            this.dispose();
        } else {
            setVisible(true);
        }

    }

    /**
     * Constructor Constructs the frame, with the graph component in one panel and the user options in the other. Draws
     * initial default graph of desired function / intensity plot.
     *
     * @param  xInit   the array of x coordinates to be plotted in the graph
     * @param  yInit   the array of y coordinates to be plotted in the graph
     * @param  title   the title of the frame
     * @param  labelX  x axis label
     * @param  labelY  y aixs label
     */
    public ViewJFrameGraph(float[] xInit, float[] yInit, String title, String labelX, String labelY) {
        super(title);

        ViewJComponentFunct[] functArray;
        ViewJComponentFunct[] fittedFuncts;

        voi = null;
        setBounds(0, 0, 500, 400);

        float[] x = new float[xInit.length];
        float[] y = new float[xInit.length];

        for (int i = 0; i < xInit.length; i++) {
            x[i] = xInit[i];
            y[i] = yInit[i];
        }

        try {
            mainPanel = new JPanel();
            mainPanel.setBounds(0, 0, getSize().width, getSize().height);
            graph = new ViewJComponentGraph(this, mainPanel.getBounds().width, mainPanel.getBounds().height - 60);
            functArray = new ViewJComponentFunct[1];
            functArray[0] = new ViewJComponentFunct(x, y, Color.red, 1, voi,null); // empty function
            fittedFuncts = new ViewJComponentFunct[1];
            fittedFuncts[0] = new ViewJComponentFunct(x, y, Color.red, voi,null); // empty function
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph constructor");

            return;
        }

        addNotify();
        setResizable(true);

        try {
            setIconImage(MipavUtil.getIconImage("graph.gif"));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage() +
                               ">.  Check that this file is available.\n");
        }

        // setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        mainPanel.setLayout(null);
        mainPanel.setBackground(Color.lightGray);
        mainPanel.setForeground(Color.black);

        graph.setFuncts(functArray);
        graph.setFittedFuncts(fittedFuncts);

        graph.calculateDefaultRangeDomain();
        graph.setDefaultRangeDomain();

        buildMenu();

        graph.setNumberOfXGridLines(4); // draws default number of grid lines
        graph.setNumberOfYGridLines(4);
        graph.setTitle(title); // sets title above graph to same as window title
        graph.setLabels(labelX, labelY);
        graph.setBackground(Color.red);

        graph.setPointsAndLinesDisplay(ViewJComponentGraph.SHOW_LINES_ONLY);
        graph.setGridlinesVisible(true);
        graph.setMinorTickMarksVisible(true);

        mainPanel.add(graph);

        getContentPane().add(mainPanel);
        setJMenuBar(openingMenuBar);

        addComponentListener(this);
        addWindowListener(this);

        if (xInit.length != yInit.length) { // must be pairs of x and y coordinates, or the graph cannot be drawn
            MipavUtil.displayError("X and Y Arrays must be of equal length");
            this.dispose();
        } else {
            setVisible(true);
        }

    }

    /**
     * Constructor Constructs the frame, with the graph component in one panel and the user options in the other. Draws
     * initial default graph of desired function / intensity plot.
     *
     * @param  xInit   the array of x coordinates to be plotted in the graph
     * @param  yInit   the array of y coordinates to be plotted in the graph
     * @param  title   the title of the frame
     * @param  labelX  x axis label
     * @param  labelY  y axis label
     */
    public ViewJFrameGraph(float[][] xInit, float[][] yInit, String title, String labelX, String labelY) {
        super(title);
        voi = null;
        updateRGBIndex = 0;

        ViewJComponentFunct[] functArray;
        ViewJComponentFunct[] fittedFuncts;
        Color color = Color.red;
        int i, j;
        float[][] x = new float[xInit.length][xInit[0].length];
        float[][] y = new float[xInit.length][xInit[0].length];

        for (i = 0; i < xInit.length; i++) {

            for (j = 0; j < xInit[0].length; j++) {
                x[i][j] = xInit[i][j];
                y[i][j] = yInit[i][j];
            }
        }

        setBounds(0, 0, 500, 400);

        try {
            mainPanel = new JPanel();
            mainPanel.setBounds(0, 0, getSize().width, getSize().height);
            graph = new ViewJComponentGraph(this, mainPanel.getBounds().width, mainPanel.getBounds().height - 60);
            functArray = new ViewJComponentFunct[x.length];
            fittedFuncts = new ViewJComponentFunct[x.length];

            for (i = 0; i < x.length; i++) {

                if (i == 0) {
                    color = Color.red;
                } else if (i == 1) {
                    color = Color.green.darker();
                } else if (i == 2) {
                    color = Color.blue;
                } else if (i == 3) {
                    color = Color.black;
                } else if (i == 4) {
                    color = Color.yellow;
                }

                functArray[i] = new ViewJComponentFunct(x[i], y[i], color, i + 1, voi,null); // empty function
                fittedFuncts[i] = new ViewJComponentFunct(x[i], y[i], color, voi,null); // empty function
            }
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph constructor");

            return;
        }

        addNotify();
        setResizable(true);

        try {
            setIconImage(MipavUtil.getIconImage("graph.gif"));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage() +
                               ">.  Check that this file is available.\n");
        }

        // setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        mainPanel.setLayout(null);
        mainPanel.setBackground(Color.lightGray);
        mainPanel.setForeground(Color.black);

        graph.setFuncts(functArray);
        graph.setFittedFuncts(fittedFuncts);

        graph.calculateDefaultRangeDomain();
        graph.setDefaultRangeDomain();

        buildMenu();

        graph.setNumberOfXGridLines(4); // draws default number of grid lines
        graph.setNumberOfYGridLines(4);
        graph.setTitle(title); // sets title above graph to same as window title
        graph.setLabels(labelX, labelY);
        graph.setBackground(Color.red);

        graph.setPointsAndLinesDisplay(ViewJComponentGraph.SHOW_LINES_ONLY);
        graph.setGridlinesVisible(true);
        graph.setMinorTickMarksVisible(true);

        mainPanel.add(graph);

        getContentPane().add(mainPanel);
        setJMenuBar(openingMenuBar);
        addComponentListener(this);
        addWindowListener(this);

        if (xInit.length != yInit.length) { // must be pairs of x and y coordinates, or the graph cannot be drawn
            MipavUtil.displayError("X and Y Arrays must be of equal length");
            this.dispose();
        } else {
            setVisible(true);
        }

    }

    /**
     * Constructor Constructs the frame, with the graph component in one panel and the user options in the other. Graph
     * is assumed to be made from a VOIPoint. Draws initial default graph of desired function / intensity plot.
     *
     * @param  xInit  the array of x coordinates to be plotted in the graph
     * @param  yInit  the array of y coordinates to be plotted in the graph
     * @param  title  the title of the frame
     * @param  v      the VOI the graph is being made for
     * @param  units  the string representing the units of the curve (x-axis)
     * @param   xyCoords     actual x,y coords of the boundary go in here if not null 
     *
     */
    public ViewJFrameGraph(float[] xInit, float[] yInit, String title, VOI v, String units, int[][] xyCoords) {
        super(title);

        this.xyCoords = xyCoords;
        
        ViewJComponentFunct[] functArray;
        ViewJComponentFunct[] fittedFuncts;

        voi = v;
        setBounds(0, 0, 500, 400);

        float[] x = new float[xInit.length];
        float[] y = new float[xInit.length];
        int[][] xyC = new int[xInit.length][2];
        

        for (int i = 0; i < xInit.length; i++) {
            x[i] = xInit[i];
            y[i] = yInit[i];
            if(xyCoords != null) {
            	xyC[i][0] = xyCoords[i][0];
            	xyC[i][1] = xyCoords[i][1];
            }
        }

        setVisible(false);

        try {
            mainPanel = new JPanel();
            mainPanel.setBounds(0, 0, getSize().width, getSize().height);
            graph = new ViewJComponentGraph(this, mainPanel.getBounds().width, mainPanel.getBounds().height - 60);
            functArray = new ViewJComponentFunct[1];
            functArray[0] = new ViewJComponentFunct(x, y, Color.red, 1, voi,xyC); // empty function
            fittedFuncts = new ViewJComponentFunct[1];
            fittedFuncts[0] = new ViewJComponentFunct(x, y, Color.red, voi,xyC); // empty function
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph constructor");

            return;
        }

        addNotify();
        setResizable(true);

        try {
            setIconImage(MipavUtil.getIconImage("graph.gif"));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage() +
                               ">.  Check that this file is available.\n");
        }

        // setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        mainPanel.setLayout(null);
        mainPanel.setBackground(Color.lightGray);
        mainPanel.setForeground(Color.black);

        graph.setFuncts(functArray);
        graph.setFittedFuncts(fittedFuncts);

        graph.calculateDefaultRangeDomain();
        graph.setDefaultRangeDomain();

        buildMenu();

        graph.setNumberOfXGridLines(4); // draws default number of grid lines
        graph.setNumberOfYGridLines(4);
        graph.setTitle(title); // sets title above graph to same as window title

        setUnitsInLabel(units);
        graph.setBackground(Color.red);

        graph.setPointsAndLinesDisplay(ViewJComponentGraph.SHOW_LINES_ONLY);
        graph.setGridlinesVisible(true);
        graph.setMinorTickMarksVisible(true);

        mainPanel.add(graph);

        getContentPane().add(mainPanel);
        setJMenuBar(openingMenuBar);
        addComponentListener(this);
        addWindowListener(this);

        if (xInit.length != yInit.length) { // must be pairs of x and y coordinates, or the graph cannot be drawn
            MipavUtil.displayError("X and Y Arrays must be of equal length");
            this.dispose();
        }

    }

    /**
     * Constructor Constructs the frame, with the graph component in one panel and the user options in the other. Graph
     * is assumed to be made from a VOIPoint. Draws initial default graph of desired function / intensity plot.
     *
     * @param  xInit  the array of x coordinates to be plotted in the graph
     * @param  yInit  the array of y coordinates to be plotted in the graph
     * @param  title  the title of the frame
     * @param  v      the VOI the graph is being made for
     * @param  units  the string representing the units of the curve (x-axis)
     */
    public ViewJFrameGraph(float[][] xInit, float[][] yInit, String title, VOI v, String units) {
        super(title);
        voi = v;
        updateRGBIndex = 0;

        ViewJComponentFunct[] functArray;
        ViewJComponentFunct[] fittedFuncts;
        Color color = Color.red;
        int i, j;
        setBounds(0, 0, 500, 400);

        float[][] x = new float[xInit.length][xInit[0].length];
        float[][] y = new float[xInit.length][xInit[0].length];

        for (i = 0; i < xInit.length; i++) {

            for (j = 0; j < xInit[0].length; j++) {
                x[i][j] = xInit[i][j];
                y[i][j] = yInit[i][j];
            }
        }

        setVisible(false);

        try {
            mainPanel = new JPanel();
            mainPanel.setBounds(0, 0, getSize().width, getSize().height);
            graph = new ViewJComponentGraph(this, mainPanel.getBounds().width, mainPanel.getBounds().height - 60);
            functArray = new ViewJComponentFunct[x.length];
            fittedFuncts = new ViewJComponentFunct[x.length];

            for (i = 0; i < x.length; i++) {

                if (i == 0) {
                    color = Color.red;
                } else if (i == 1) {
                    color = Color.green.darker();
                } else if (i == 2) {
                    color = Color.blue;
                } else if (i == 3) {
                    color = Color.black;
                } else if (i == 4) {
                    color = Color.yellow;
                }

                functArray[i] = new ViewJComponentFunct(x[i], y[i], color, i + 1, voi,null); // empty function
                fittedFuncts[i] = new ViewJComponentFunct(x[i], y[i], color, voi, null); // empty function
            }
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph constructor");

            return;
        }

        addNotify();
        setResizable(true);

        try {
            setIconImage(MipavUtil.getIconImage("graph.gif"));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage() +
                               ">.  Check that this file is available.\n");
        }

        // setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        mainPanel.setLayout(null);
        mainPanel.setBackground(Color.lightGray);
        mainPanel.setForeground(Color.black);

        graph.setFuncts(functArray);
        graph.setFittedFuncts(fittedFuncts);

        graph.calculateDefaultRangeDomain();
        graph.setDefaultRangeDomain();

        buildMenu();

        graph.setNumberOfXGridLines(4); // draws default number of grid lines
        graph.setNumberOfYGridLines(4);
        graph.setTitle(title); // sets title above graph to same as window title
        setUnitsInLabel(units);
        graph.setBackground(Color.red);

        graph.setPointsAndLinesDisplay(ViewJComponentGraph.SHOW_LINES_ONLY);
        graph.setGridlinesVisible(true);
        graph.setMinorTickMarksVisible(true);

        mainPanel.add(graph);

        getContentPane().add(mainPanel);
        setJMenuBar(openingMenuBar);
        addComponentListener(this);
        addWindowListener(this);

        if (xInit.length != yInit.length) { // must be pairs of x and y coordinates, or the graph cannot be drawn
            MipavUtil.displayError("X and Y Arrays must be of equal length");
            this.dispose();
        }

    }

    /**
     * Constructor Constructs the frame, with the graph component in one panel and the user options in the other. Draws
     * initial default graph of desired function / intensity plot.
     *
     * @param  xInit   the array of x coordinates to be plotted in the graph
     * @param  yInit   the array of y coordinates to be plotted in the graph
     * @param  title   the title of the frame
     * @param  labelX  x axis label
     * @param  labelY  y aixs label
     * @param  color   DOCUMENT ME!
     */
    public ViewJFrameGraph(float[] xInit, float[] yInit, String title, String labelX, String labelY, Color color) {
        super(title);

        ViewJComponentFunct[] functArray;
        ViewJComponentFunct[] fittedFuncts;

        voi = null;
        setBounds(0, 0, 500, 400);

        float[] x = new float[xInit.length];
        float[] y = new float[xInit.length];

        for (int i = 0; i < xInit.length; i++) {
            x[i] = xInit[i];
            y[i] = yInit[i];
        }

        try {
            mainPanel = new JPanel();
            mainPanel.setBounds(0, 0, getSize().width, getSize().height);
            graph = new ViewJComponentGraph(this, mainPanel.getBounds().width, mainPanel.getBounds().height - 60);
            functArray = new ViewJComponentFunct[1];
            functArray[0] = new ViewJComponentFunct(x, y, color, 1, voi,null); // empty function
            fittedFuncts = new ViewJComponentFunct[1];
            fittedFuncts[0] = new ViewJComponentFunct(x, y, color, voi, null); // empty function
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph constructor");

            return;
        }

        addNotify();
        setResizable(true);

        try {
            setIconImage(MipavUtil.getIconImage("graph.gif"));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage() +
                               ">.  Check that this file is available.\n");
        }

        // setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        mainPanel.setLayout(null);
        mainPanel.setBackground(Color.lightGray);
        mainPanel.setForeground(Color.black);

        graph.setFuncts(functArray);
        graph.setFittedFuncts(fittedFuncts);

        graph.calculateDefaultRangeDomain();
        graph.setDefaultRangeDomain();

        buildMenu();

        graph.setNumberOfXGridLines(4); // draws default number of grid lines
        graph.setNumberOfYGridLines(4);
        graph.setTitle(title); // sets title above graph to same as window title
        graph.setLabels(labelX, labelY);
        graph.setBackground(color);

        graph.setPointsAndLinesDisplay(ViewJComponentGraph.SHOW_LINES_ONLY);
        graph.setGridlinesVisible(true);
        graph.setMinorTickMarksVisible(true);

        mainPanel.add(graph);

        getContentPane().add(mainPanel);
        setJMenuBar(openingMenuBar);

        addComponentListener(this);
        addWindowListener(this);

        if (xInit.length != yInit.length) { // must be pairs of x and y coordinates, or the graph cannot be drawn
            MipavUtil.displayError("X and Y Arrays must be of equal length");
            this.dispose();
        } else {
            setVisible(true);
        }

    }

    /**
     * Constructor Constructs the frame, with the graph component in one panel and the user options in the other. Draws
     * initial default graph of desired function / intensity plot.
     *
     * @param  xInit       the array of x coordinates to be plotted in the graph
     * @param  yInit       the array of y coordinates to be plotted in the graph
     * @param  title       the title of the frame
     * @param  labelX      x axis label
     * @param  labelY      y axis label
     * @param  colorArray  DOCUMENT ME!
     */
    public ViewJFrameGraph(float[][] xInit, float[][] yInit, String title, String labelX, String labelY,
                           Color[] colorArray) {
        super(title);
        voi = null;
        updateRGBIndex = 0;

        ViewJComponentFunct[] functArray;
        ViewJComponentFunct[] fittedFuncts;
        Color color = colorArray[0];
        int i, j;
        setBounds(0, 0, 500, 400);

        float[][] x = new float[xInit.length][xInit[0].length];
        float[][] y = new float[xInit.length][xInit[0].length];

        for (i = 0; i < xInit.length; i++) {

            for (j = 0; j < xInit[0].length; j++) {
                x[i][j] = xInit[i][j];
                y[i][j] = yInit[i][j];
            }
        }

        try {
            mainPanel = new JPanel();
            mainPanel.setBounds(0, 0, getSize().width, getSize().height);
            graph = new ViewJComponentGraph(this, mainPanel.getBounds().width, mainPanel.getBounds().height - 60);
            functArray = new ViewJComponentFunct[x.length];
            fittedFuncts = new ViewJComponentFunct[x.length];

            for (i = 0; i < x.length; i++) {
                color = colorArray[i];

                functArray[i] = new ViewJComponentFunct(x[i], y[i], color, i + 1, voi,null); // empty function
                fittedFuncts[i] = new ViewJComponentFunct(x[i], y[i], color, voi, null); // empty function
            }
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph constructor");

            return;
        }

        addNotify();
        setResizable(true);

        try {
            setIconImage(MipavUtil.getIconImage("graph.gif"));
        } catch (FileNotFoundException error) {
            Preferences.debug("Exception ocurred while getting <" + error.getMessage() +
                              ">.  Check that this file is available.\n");
            System.err.println("Exception ocurred while getting <" + error.getMessage() +
                               ">.  Check that this file is available.\n");
        }

        // setDefaultCloseOperation(JFrame.DISPOSE_ON_CLOSE);

        mainPanel.setLayout(null);
        mainPanel.setBackground(Color.lightGray);
        mainPanel.setForeground(Color.black);

        graph.setFuncts(functArray);
        graph.setFittedFuncts(fittedFuncts);

        graph.calculateDefaultRangeDomain();
        graph.setDefaultRangeDomain();

        buildMenu();

        graph.setNumberOfXGridLines(4); // draws default number of grid lines
        graph.setNumberOfYGridLines(4);
        graph.setTitle(title); // sets title above graph to same as window title
        graph.setLabels(labelX, labelY);
        graph.setBackground(colorArray[0]);

        graph.setPointsAndLinesDisplay(ViewJComponentGraph.SHOW_LINES_ONLY);
        graph.setGridlinesVisible(true);
        graph.setMinorTickMarksVisible(true);

        mainPanel.add(graph);

        getContentPane().add(mainPanel);
        setJMenuBar(openingMenuBar);
        addComponentListener(this);
        addWindowListener(this);

        if (xInit.length != yInit.length) { // must be pairs of x and y coordinates, or the graph cannot be drawn
            MipavUtil.displayError("X and Y Arrays must be of equal length");
            this.dispose();
        } else {
            setVisible(true);
        }

    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Takes the action commands and paints if the apply button is pressed, closes if the close button is pressed.
     *
     * @param  event  event that triggers this function
     */
    public void actionPerformed(ActionEvent event) {
        String command;
        Object source;

        source = event.getSource();
        command = event.getActionCommand();

        if (command.equals("CloseGraph")) {

            if (frameGraphVOI != null) {
                frameGraphVOI.setContourGraph(null);
            }

            dispose();
        } else if (command.equals("OpenNewGraph")) {

            try {
                openNew();
            } catch (IOException e) {
                MipavUtil.displayError("Error: " + e);
            }
        } else if (command.equals("OpenSameGraph")) {

            try {
                openSame();
            } catch (IOException e) {
                MipavUtil.displayError("Error: " + e);
            }
        } else if (command.equals("ResetRange")) {
            graph.calculateDefaultRangeDomain();
            graph.setDefaultRangeDomain();
            update(getGraphics());

            if (modifyDialog != null) {
                updateModifyDialog();
            }

        } else if (command.equals("ResetRangeButton")) {
            graph.calculateDefaultRangeDomain();
            graph.setDefaultRangeDomain();
            minRangeField.setText("" + graph.getMinRange());
            maxRangeField.setText("" + graph.getMaxRange());
            minRangeLabel.setText("Min. for Range (<" + Float.toString(graph.getDefaultMinRange()) + ")");
            maxRangeLabel.setText("Max. for Range (>" + Float.toString(graph.getDefaultMaxRange()) + ")");
            update(getGraphics());
        } else if (command.equals("AutoShrinkRange")) {
            autoShrinkRange = autoShrinkRangeCheckbox.isSelected();

            if (autoShrinkRange) {
                graph.calculateDefaultRangeDomain();
                graph.setDefaultRangeDomain();
                update(getGraphics());
            }
        } else if (command.equals("ResetGraph")) { // resets the graph to the original scale, function line colors,

            // and background colors
            // redraws all the functions in the graph
            graph.setBackgroundColor(new Color(200, 200, 200));

            for (int i = 0; i < graph.getFuncts().length; i++) {
                graph.getFuncts()[i].setXs(graph.getFuncts()[i].getOriginalXs()); // gets the original x and y point
                                                                                  // values
                graph.getFuncts()[i].setYs(graph.getFuncts()[i].getOriginalYs());
                graph.getFuncts()[i].setColor(i); // sets each function to a different color
                itemResetGraph.setEnabled(false);
            }

            for (int i = 0; i < graph.getFittedFuncts().length; i++) {
                graph.getFittedFuncts()[i].setXs(graph.getFittedFuncts()[i].getOriginalXs()); // gets the original x
                                                                                              // and y point values
                graph.getFittedFuncts()[i].setYs(graph.getFittedFuncts()[i].getOriginalYs());
                graph.getFittedFuncts()[i].setColor(i); // sets each function to a different color
            }

            graph.calculateDefaultRangeDomain();
            graph.setDefaultRangeDomain();
            update(getGraphics());
        } else if (command.equals("LogScaleRange")) {
            boolean originalDoLog = doLog;
            doLog = logScaleCheckbox.isSelected();
            if (doLog != originalDoLog) {
                graph.setDoLog(doLog);
                graph.calculateDefaultRangeDomain();
                graph.setDefaultRangeDomain();
                update(getGraphics());
            }
        } else if(command.equals("FitFunctions")) {
        	fitFunctions((FitMode)fitType.getSelectedItem());
        } else if (command.equals("SaveGraph")) { // saves the graph to a file

            try {
                save();
            } catch (IOException e) {
                MipavUtil.displayError("Error: " + e);
            }
        } else if (command.equals("TableOutput")) {
            tableOutput();
        } else if (command.equals("PrintGraph")) {
            print(); // calls the os's default print dialog to print the graph to the printer
        } else if (command.equals("PasteFunct")) {

            if (graph.getFuncts().length > 5) {
                MipavUtil.displayError("A Maximum of 5 Functions are Allowed in One Graph");

                return;
            }

            graph.pasteFunct();
            graph.calculateDefaultRangeDomain();

            if (autoShrinkRange) {
                graph.setDefaultRangeDomain();
            } else {
                graph.calculateCustomRange();
            }

            int len = graph.getFuncts().length;

            vectorCopyFunct.add(new JMenuItem(graph.getFuncts()[len - 1].getFunctName()));
            ((JMenuItem) vectorCopyFunct.elementAt(len - 1)).setFont(MipavUtil.font12B);
            copyMenu.add((JMenuItem) vectorCopyFunct.elementAt(len - 1));

            vectorDeleteFunct.add(new JMenuItem(graph.getFuncts()[len - 1].getFunctName()));
            ((JMenuItem) vectorDeleteFunct.elementAt(len - 1)).setFont(MipavUtil.font12B);
            deleteMenu.add((JMenuItem) vectorDeleteFunct.elementAt(len - 1));

            ((JMenuItem) vectorCopyFunct.elementAt(len - 1)).addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent event) {
                        JMenuItem c = (JMenuItem) (event.getSource());

                        for (int i = 0; i < graph.getFuncts().length; i++) {

                            if (c == vectorCopyFunct.elementAt(i)) {
                                graph.copyFunct(i);
                                update(getGraphics());

                                break;
                            }
                        }
                    }
                });

            if (graph.getFuncts()[len - 1].getVOI() == null) {
                ((JMenuItem) vectorDeleteFunct.elementAt(len - 1)).addActionListener(new ActionListener() {
                        public void actionPerformed(ActionEvent event) {
                            JMenuItem c = (JMenuItem) (event.getSource());

                            for (int i = 0; i < graph.getFuncts().length; i++) {

                                if (c == vectorDeleteFunct.elementAt(i)) {
                                    graph.deleteFunct(i);
                                    deleteMenu.remove((JMenuItem) vectorDeleteFunct.elementAt(i));
                                    vectorDeleteFunct.removeElementAt(i);
                                    copyMenu.remove((JMenuItem) vectorCopyFunct.elementAt(i));
                                    vectorCopyFunct.removeElementAt(i);
                                    graph.calculateDefaultRangeDomain();

                                    if (autoShrinkRange) {
                                        graph.setDefaultRangeDomain();
                                    } else {
                                        graph.calculateCustomRange();
                                    }

                                    if (modifyDialog != null) {
                                        updateModifyDialog();
                                    }

                                    update(getGraphics());

                                    break;
                                }
                            }
                        }
                    });
            } else {
                ((JMenuItem) vectorDeleteFunct.elementAt(len - 1)).setEnabled(false);
            }

            if (modifyDialog != null) {
                updateModifyDialog();
            }

            update(getGraphics());
        } else if ((source == linesOnlyButton) || (source == pointsAndLinesButton) || (source == pointsOnlyButton)) { 
            // sets the points and lines of the functions visible
            if (linesOnlyButton.isSelected()) {
                graph.setPointsAndLinesDisplay(ViewJComponentGraph.SHOW_LINES_ONLY);    
            }
            else if (pointsAndLinesButton.isSelected()) {
                graph.setPointsAndLinesDisplay(ViewJComponentGraph.SHOW_POINTS_AND_LINES);
            }
            else {
                graph.setPointsAndLinesDisplay(ViewJComponentGraph.SHOW_POINTS_ONLY);
            }
            update(getGraphics());
        } else if (command.equals("Legend")) { // toggles the displaying of the legend
            graph.setLegendVisible(legendCheckbox.isSelected());
            update(getGraphics());
        } else if (command.equals("Gridlines")) { // toggles the gridlines drawn in the background of the graph
            graph.setGridlinesVisible(gridlinesCheckbox.isSelected());
            update(getGraphics());
        } else if (command.equals("MinorTickMarks")) { // draws the minor tick marks along the x and y axis
            graph.setMinorTickMarksVisible(minorTickMarksCheckbox.isSelected());
            update(getGraphics());
        } else if (command.equals("ModifyGraph")) {
            createModifyDialog();
        } else if (command.equals("ColorChange")) { // brings up the color choosing dialog box for the user to choose a

            // new line color for the function from a pallete.  The user can also
            // change the HSB and RGB.

            JButton b = (JButton) (source);

            for (int i = 0; i < graph.getFuncts().length; i++) {

                if (b == functLineColorButton[i]) {
                    functionIndex = i;

                    break;
                }
            }

            try {
                colorChooser = new ViewJColorChooser(this, // creates the color choosing dialog box
                                                     "Pick line color", new ColorListener(), new CancelListener());
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ViewJComponentGraph.ActionPerformed");

                return;
            }

        } else if (command.equals("ChangeBackground")) { // changes the color being shown in the background of the graph

            try {
                colorChooser = new ViewJColorChooser(this, "Pick background color", new bColorListener(),
                                                     new CancelListener());
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ViewJComponentGraph.ActionPerformed");

                return;
            }

        } else if (command.equals("ApplyModifyGraph")) {
            if (image != null) {
                try {
                    userMin = new Double(minField.getText()).doubleValue();
                } catch (Exception ex) {
                    MipavUtil.displayError("minValue must be a double value");

                    return;
                }
                if (userMin < imageMin) {
                    MipavUtil.displayError("Minimum must be at least " + imageMin);
                    return;
                }
                
                try {
                    userMax = new Double(maxField.getText()).doubleValue();
                } catch (Exception ex) {
                    MipavUtil.displayError("maxValue must be a double value");

                    return;
                }
                if (userMax > imageMax) {
                    MipavUtil.displayError("maxValue cannot exceed " + imageMax);
                    return;
                }
                if (userMax <= userMin) {
                    MipavUtil.displayError("maxValue must be greater than " + userMin);
                    return;
                }
                
                try {
                    summaryBins = new Integer(binsField.getText()).intValue();
                }
                catch (Exception ex) {
                    MipavUtil.displayError("numBins must be an integer value");
                    return;
                }
                
                if (summaryBins < 2) {
                    MipavUtil.displayError("Number of bins must be at least 2");
                    return;
                }
                
                if (summaryBins > maxBins) {
                    MipavUtil.displayError("Number of bins cannot exceed " + maxBins);
                    return;
                }

                if ((summaryBins != initialSummaryBins) || (userMin != initialUserMin) || (userMax != initialUserMax)) {
                    setVisible(false);
                    AlgorithmHistogram histAlgo;
                    boolean displayGraph = true;
                    boolean userLimits = true;
                    if (image.isColorImage()) {
                        histAlgo = new AlgorithmHistogram(image, summaryBins, RGBOffset, entireImage, displayGraph,
                                userLimits, userMin, userMax);
                    } // if (image.isColorImage())
                    else {
                        histAlgo = new AlgorithmHistogram(image, summaryBins, entireImage, displayGraph,
                                                                         userLimits, userMin, userMax);
                    }
                    histAlgo.run();
                    
                    this.dispose();   
                } // if ((summaryBins != initialSummaryBins) || (userMin != initialUserMin) || (userMax != initialUserMax))
            } // if (image != null)
            String xText = xGridLineField.getText(); // stores the input for the number of gridlines along the x and y
                                                     // axis
            String yText = yGridLineField.getText();

            if (testParameter(xText, 1, 50) && testParameter(yText, 1, 50)) { // tests to see if the number inputted is

                // in the appropriate range (1-50)
                graph.setNumberOfXGridLines(Integer.parseInt(xText)); // sets the appropriate number of gridlines
                graph.setNumberOfYGridLines(Integer.parseInt(yText));
            } else {
                MipavUtil.displayError("The number of gridlines must be an integer between 1 and 50.");
            }

            String xLabelText = xAxisLabelField.getText(); // gets appropriate x and y axis label inputs
            String yLabelText = yAxisLabelField.getText();
            String titleText = titleField.getText(); // gets user input for the title
            graph.setLabels(xLabelText, yLabelText); // sets the new labels and title on the graph
            graph.setTitle(titleText);

            String minRangeText = minRangeField.getText();
            String maxRangeText = maxRangeField.getText();

            if (testMinParameter(minRangeText, graph.getDefaultMinRange()) &&
                    testMaxParameter(maxRangeText, graph.getDefaultMaxRange())) {
                graph.setRange(Float.parseFloat(minRangeText), Float.parseFloat(maxRangeText));
            }

            for (int i = 0; i < graph.getFuncts().length; i++) {
                graph.getFuncts()[i].setFunctName(nameField[i].getText()); // changes the appropriate functions
                ((JMenuItem) vectorCopyFunct.elementAt(i)).setText(nameField[i].getText());
                ((JMenuItem) vectorDeleteFunct.elementAt(i)).setText(nameField[i].getText());
            }

            update(getGraphics()); // repaints the graph
        } else if (command.equals("CancelModifyGraph")) {
            modifyDialog.dispose();
        } else if (command.equals("Help")) {
            MipavUtil.showHelp("19050");
        } else if (command.equals("Normalize")) {
            // System.err.println("running median, then gaussian on function1");

            int i, j;

            int size = 7;
            int start = size / 2;
            float[] x = graph.getFuncts()[0].getXs();
            float[] y = graph.getFuncts()[0].getYs();
            float[] sortY = new float[size];
            float[] newY = new float[y.length];
            float[] gaussY = new float[y.length];
            int end = y.length - start;

            newY[0] = y[0];
            newY[1] = getMedian(new float[] { y[0], y[1], y[2] });
            newY[2] = getMedian(new float[] { y[0], y[1], y[2], y[3], y[4] });

            newY[end] = getMedian(new float[] { y[end - 2], y[end - 1], y[end], y[end + 1], y[end + 2] });
            newY[end + 1] = getMedian(new float[] { y[end], y[end + 1], y[end + 2] });
            newY[end + 2] = y[end + 2];

            // System.err.println("newY length: " + newY.length + " end: " + end);

            for (i = start; i < end; i++) {

                for (j = 0; j < size; j++) {
                    sortY[j] = y[i + j - start];
                }

                newY[i] = getMedian(sortY);
            }

            GaussianOneDimKernel g = new  GaussianOneDimKernel();
            float[] kernel = g.make(1.0f);
            // System.err.println("LENGTH IS: " + kernel.length);
            // for (int b = 0; b < kernel.length; b++) {
            // System.err.print(kernel[b] + " ");
            // }

            gaussY[0] = newY[0];
            gaussY[1] = newY[1];
            gaussY[2] = newY[2];

            // System.err.println("gaussY length: " + gaussY.length + " end: " + end);
            end = gaussY.length - 3;

            gaussY[end] = newY[end];
            gaussY[end + 1] = newY[end + 1];
            gaussY[end + 2] = newY[end + 2];

            for (i = 3; i < end; i++) {
                gaussY[i] = (newY[i - 3] * kernel[0]) + (newY[i - 2] * kernel[1]) + (newY[i - 1] * kernel[2]) +
                            (newY[i] * kernel[3]) + (newY[i + 1] * kernel[4]) + (newY[i + 2] * kernel[5]) +
                            (newY[i + 3] * kernel[6]);
                if(gaussY[i] != 0.0)
                	System.out.println("Stop");
                
            }

            new ViewJFrameGraph(x, gaussY, "Median -> Gaussian",(int[][])null);

        }
    }
    
    private void fitFunctions(FitMode mode) {
        int nPoints;
        NLFittedFunction fe = null;

        ViewJComponentFunct[] functions = graph.getFuncts();
        ViewJComponentFunct[] fittedFunctions = graph.getFittedFuncts();
        float[] x;
        float[] y;

        try {

            for (int i = 0; i < functions.length; i++) {
                nPoints = graph.getFuncts()[i].getOriginalXs().length;
                if (mode.toString().equals("Fit Multiexponential a0 + sum of a[2*k+1]*exp(a[2*k+2]*x)")) {
                	String tmpStr = numVariablesField.getText();
                	numVariables = Integer.valueOf(tmpStr).intValue();
                	if ((numVariables % 2) == 0) {
                		MipavUtil.displayError("Number of variables must be odd");
                		return;
                	}
                	fe = (NLFittedFunction)mode.getImpl().newInstance(nPoints, numVariables,
                			graph.getFuncts()[i].getOriginalXs(),
                            graph.getFuncts()[i].getOriginalYs());	
                }
                else {
                fe = (NLFittedFunction)mode.getImpl().newInstance(nPoints, graph.getFuncts()[i].getOriginalXs(),
                                        graph.getFuncts()[i].getOriginalYs());
                }
                fe.driver();
                fe.displayResults();

                x = new float[functions[i].getXs().length];
                y = new float[x.length];

                double[] yTemp = fe.getFittedY();
                
                for (int j = 0; j < x.length; j++) {
                    x[j] = (functions[i].getXs()[j]);
                	y[j] = (float)yTemp[j];
                }

                fittedFunctions[i].setXs(x);
                fittedFunctions[i].setOriginalXs(x);
                fittedFunctions[i].setYs(y);
                fittedFunctions[i].setOriginalYs(y);
            }
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Graph :  Out of memory ");

        } catch (Exception e) {
        	MipavUtil.displayError("Fitting functions failed.");
			e.printStackTrace();
		} 

        fitMode = mode;

        for (int index = 0; index < 5; index++) {

            if (index >= graph.getFuncts().length) {
                fitFunctVisibleCheckbox[index].setEnabled(false);
                fitFunctVisibleCheckbox[index].setSelected(false);
            } else {
                fitFunctVisibleCheckbox[index].setEnabled(true);
                fitFunctVisibleCheckbox[index].setSelected(graph.getFuncts()[index].getFitFunctionVisible());
            }
        }

        update(getGraphics()); 
    }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void componentHidden(ComponentEvent event) { }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void componentMoved(ComponentEvent event) { }

    // ********************************************************************
    // ********************  Component Events *****************************
    // ********************************************************************

    /**
     * Resets the bounds of component graph if the window is resized.
     *
     * @param  event  event that triggered this function
     */
    public void componentResized(ComponentEvent event) {
        Rectangle rect;
        Rectangle bounds;

        bounds = getBounds();

        if (bounds.width < MINIMUM_WIDTH) { // makes sure that the window's width has not been resized

            // to less than MINIMUM_WIDTH, else resizes the window to
            // the MINUMIM_WIDTH
            setBounds(bounds.x, bounds.y, MINIMUM_WIDTH, bounds.height);
        }

        if (bounds.height < MINIMUM_HEIGHT) { // makes sure that the window's height has not been resized

            // to less than MINIMUM_HEIGHT, else resizes the window to
            // the MINUMIM_HEIGHT
            setBounds(bounds.x, bounds.y, bounds.width, MINIMUM_HEIGHT);
        }

        bounds = getBounds();

        try {
            rect = new Rectangle(0, 0, bounds.width, bounds.height); // creates a new rectange containing the new bounds
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJComponentGraph.componentResized");

            return;
        }

        mainPanel.setBounds(rect); // sets the mainPanel's bounds appropriately
        graph.resetBounds(new Rectangle(0, 0, rect.width, rect.height - 60)); // changes the graph's bounds
                                                                              // appropriately
        update(getGraphics());

    }

    /**
     * DOCUMENT ME!
     *
     * @param  event  DOCUMENT ME!
     */
    public void componentShown(ComponentEvent event) { }

    /**
     * Deletes the indicated function from the graph.
     *
     * @param  index  the index of the function to be deleted
     */
    public void deleteFunct(int index) {

        if (updateRGBIndex != -1) {
            return;
        }

        graph.deleteFunct(index);

        // updates the copy and delete menus
        copyMenu.remove((JMenuItem) vectorCopyFunct.elementAt(index));
        vectorCopyFunct.removeElementAt(index);
        deleteMenu.remove((JMenuItem) vectorDeleteFunct.elementAt(index));
        vectorDeleteFunct.removeElementAt(index);
        update(getGraphics());

        if (modifyDialog != null) {
            updateModifyDialog();
        }
    }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void focusGained(FocusEvent event) { }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void focusLost(FocusEvent event) { }

    /**
     * Accessor that gets the default directory.
     *
     * @return  the default directory
     */
    public String getDefaultDirectory() {
        return defaultDirectory;
    }

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) { }

    /**
     * DOCUMENT ME!
     */
    public void makeRangeSymmetric() {
        graph.setRangeSymmetric();
        update(getGraphics());
    }
    
    /**
     * Opens Excel data (tab-delimited fields) into a new graph. The graphs could also have been saved using MIPAV
     *
     * @return  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public boolean openNew() throws IOException {
        FileReader instream;
        BufferedReader dataStream;
        String fileName;
        String directory;
        JFileChooser chooser;
        ViewJComponentFunct[] functions;
        String s;
        int k;
        float[] fields;
        float[][] X2 = null;
        float[][] Y2 = null;
        Color colorArray[] = null;

        try {
            fields = new float[ViewJComponentGraph.MAX_NUM_FUNCTS * 2];

            chooser = new JFileChooser();
            chooser.setCurrentDirectory(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.PLOT)); // adds a choosable file

            // filter to only show plot files
            chooser.setDialogTitle("Open Graph Data");

            int returnVal = chooser.showOpenDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {

                try { // stores the location of the desired file
                    fileName = chooser.getSelectedFile().getName();
                    directory = chooser.getCurrentDirectory() + "" + File.separatorChar;
                } catch (NullPointerException e) {
                    Preferences.debug("Returning.");

                    return false;
                }
            } else {
                return false;
            }

            instream = new FileReader(directory + fileName);
            dataStream = new BufferedReader(instream);
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, directory);

        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph.open");

            return false;
        }

        s = dataStream.readLine();
        fields = parseString(s, -1);

        if ((fields.length % 2) != 0) { // can only use files which contain pairs of x and y coordinates
            MipavUtil.displayError("There must be an even number of fields in your input file.");
        }

        try {
            functions = new ViewJComponentFunct[fields.length / 2]; // creates the required number of functions
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph.open");

            return false;
        }

        int j = 0;

        for (int i = 0; i < functions.length; i++) {

            try { // creates the appropriate sizes of the x and y coordinates of the functions in the array
                functions[i] = new ViewJComponentFunct();
                functions[i].X = new float[ViewJComponentFunct.MAX_NUM_COORDS];
                functions[i].Y = new float[ViewJComponentFunct.MAX_NUM_COORDS];
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ViewJFrameGraph.open");

                return false;
            }

            functions[i].X[0] = fields[j];
            j++;
            functions[i].Y[0] = fields[j];
            j++;
        }

        s = dataStream.readLine();
        k = 1;

        while (s != null) { // assigns the appropriate x and y coordinates to the appropriate function
            fields = parseString(s, functions.length * 2);
            j = 0;

            for (int i = 0; i < (fields.length / 2); i++) {
                functions[i].X[k] = fields[j];
                j++;
                functions[i].Y[k] = fields[j];
                j++;
            }

            k++;
            s = dataStream.readLine();
        }

        for (int i = 0; i < functions.length; i++) {
            functions[i].X[k] = Float.NaN;
            functions[i].Y[k] = Float.NaN;
        }

        dataStream.close();
        
        X2 = new float[functions.length][];
        Y2 = new float[functions.length][];
        colorArray = new Color[functions.length];

        for (int i = 0; i < functions.length; i++) {

            // Count number of points
            j = 0;

            while ((j < ViewJComponentFunct.MAX_NUM_COORDS) && !Float.isNaN(functions[i].X[j])) {
                j++;
            }

            try {
                X2[i] = new float[j];
                Y2[i] = new float[j];
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ViewJFrameGraph.open");

                return false;
            }

            for (j = 0; j < X2[i].length; j++) {
                X2[i][j] = functions[i].X[j];
                Y2[i][j] = functions[i].Y[j];
            }
            
            colorArray[i] = graph.getFuncts()[i].getColor();
        }

        
        new ViewJFrameGraph(X2, Y2, graph.getTitle(), graph.getXLabel(), graph.getYLabel(), colorArray);
        
        update(getGraphics());

        return true;
    }

    /**
     * Opens Excel data (tab-delimited fields) into this graph. The graphs could also have been saved using MIPAV
     *
     * @return  DOCUMENT ME!
     *
     * @throws  IOException  DOCUMENT ME!
     */
    public boolean openSame() throws IOException {
        FileReader instream;
        BufferedReader dataStream;
        String fileName;
        String directory;
        JFileChooser chooser;
        ViewJComponentFunct[] functions;
        ViewJComponentFunct[] fitFunctions;
        String s;
        int k;
        float[] fields;
        float[] X2 = null;
        float[] Y2 = null;

        try {
            fields = new float[ViewJComponentGraph.MAX_NUM_FUNCTS * 2];

            chooser = new JFileChooser();
            chooser.setCurrentDirectory(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.PLOT)); // adds a choosable file

            // filter to only show plot files
            chooser.setDialogTitle("Open Graph Data");

            int returnVal = chooser.showOpenDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {

                try { // stores the location of the desired file
                    fileName = chooser.getSelectedFile().getName();
                    directory = chooser.getCurrentDirectory() + "" + File.separatorChar;
                } catch (NullPointerException e) {
                    Preferences.debug("Returning.");

                    return false;
                }
            } else {
                return false;
            }

            instream = new FileReader(directory + fileName);
            dataStream = new BufferedReader(instream);
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, directory);

        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph.open");

            return false;
        }

        s = dataStream.readLine();
        fields = parseString(s, -1);

        if ((fields.length % 2) != 0) { // can only use files which contain pairs of x and y coordinates
            MipavUtil.displayError("There must be an even number of fields in your input file.");
        }

        try {
            functions = new ViewJComponentFunct[fields.length / 2]; // creates the required number of functions
            fitFunctions = new ViewJComponentFunct[fields.length / 2]; // creates the required number of functions
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph.open");

            return false;
        }

        int j = 0;

        for (int i = 0; i < functions.length; i++) {

            try { // creates the appropriate sizes of the x and y coordinates of the functions in the array
                functions[i] = new ViewJComponentFunct();
                fitFunctions[i] = new ViewJComponentFunct();
                functions[i].X = new float[ViewJComponentFunct.MAX_NUM_COORDS];
                functions[i].Y = new float[ViewJComponentFunct.MAX_NUM_COORDS];
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ViewJFrameGraph.open");

                return false;
            }

            functions[i].X[0] = fields[j];
            j++;
            functions[i].Y[0] = fields[j];
            j++;
        }

        s = dataStream.readLine();
        k = 1;

        while (s != null) { // assigns the appropriate x and y coordinates to the appropriate function
            fields = parseString(s, functions.length * 2);
            j = 0;

            for (int i = 0; i < (fields.length / 2); i++) {
                functions[i].X[k] = fields[j];
                j++;
                functions[i].Y[k] = fields[j];
                j++;
            }

            k++;
            s = dataStream.readLine();
        }

        for (int i = 0; i < functions.length; i++) {
            functions[i].X[k] = Float.NaN;
            functions[i].Y[k] = Float.NaN;
        }

        dataStream.close();

        for (int i = 0; i < functions.length; i++) {

            // Count number of points
            j = 0;

            while ((j < ViewJComponentFunct.MAX_NUM_COORDS) && !Float.isNaN(functions[i].X[j])) {
                j++;
            }

            try {
                X2 = new float[j];
                Y2 = new float[j];
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ViewJFrameGraph.open");

                return false;
            }

            for (j = 0; j < X2.length; j++) {
                X2[j] = functions[i].X[j];
                Y2[j] = functions[i].Y[j];
            }

            functions[i].setXs(X2); // sets the functions x and y coordinates to the proper values
            functions[i].setOriginalXs(X2);
            functions[i].setYs(Y2);
            functions[i].setOriginalYs(Y2);
            functions[i].setColor(i);
        }

        for (int i = 0; i < functions.length; i++) {

            try { // creates the appropriate sizes of the x and y coordinates of the functions in the array
                fitFunctions[i] = new ViewJComponentFunct();
                fitFunctions[i].X = new float[X2.length];
                fitFunctions[i].Y = new float[Y2.length];
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ViewJFrameGraph.open");

                return false;
            }

            fitFunctions[i].setColor(i);
        }

        graph.setFuncts(functions);
        graph.setFittedFuncts(fitFunctions);

        int numf = graph.getFuncts().length;

        for (int i = 0; i < numf; i++) {
            functions[i].setFunctName(i + 1);
        }

        graph.calculateDefaultRangeDomain();
        graph.setDefaultRangeDomain();
        update(getGraphics());

        return true;
    }

    /**
     * Replaces the function used with a line image.
     *
     * @param  newX  a 1-d array of which contains the x-coordinates for the function
     * @param  newY  a 1-d array of which contains the y-coordinates for the function
     */
    public void replaceFunction(float[] newX, float[] newY) {
        ViewJComponentFunct[] tempFuncts;
        ViewJComponentFunct[] tempFittedFuncts;
        float[] x;
        float[] y;
        voi = null;

        try {
            x = new float[newX.length];
            y = new float[newY.length];

            // creates copies of the paramaters, so that if the arrays pointed to by them
            // are modified in ViewJComponentEditImage, the functions are not modified
            x = new float[newX.length];

            for (int j = 0; j < x.length; j++) {
                x[j] = newX[j];
            }

            y = new float[newY.length];

            for (int j = 0; j < y.length; j++) {
                y[j] = newY[j];
            }

            tempFuncts = new ViewJComponentFunct[1];
            tempFittedFuncts = new ViewJComponentFunct[1];

            // change the first three functions to the new ones (RGB functions)
            // tempFuncts[0]       = new ViewJComponentFunct(x, y, 1, voi);  //empty function
            tempFuncts[0] = new ViewJComponentFunct(x, y, graph.getFuncts()[0].getColor(), voi,null); // empty function
            tempFittedFuncts[0] = new ViewJComponentFunct(x, y, Color.red, voi,null); // empty function

        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph.replaceFunction()");

            return;
        }

        graph.setFuncts(tempFuncts);
        graph.setFittedFuncts(tempFittedFuncts);

        // resets the zoom and range of the graph.
        for (int i = 0; i < graph.getFuncts().length; i++) {
            graph.getFuncts()[i].setXs(graph.getFuncts()[i].getOriginalXs()); // gets the original x and y point values
            graph.getFuncts()[i].setYs(graph.getFuncts()[i].getOriginalYs());
            itemResetGraph.setEnabled(false);
        }

        for (int i = 0; i < graph.getFittedFuncts().length; i++) {
            graph.getFittedFuncts()[i].setXs(graph.getFittedFuncts()[i].getOriginalXs()); // gets the original x and y
                                                                                          // point values
            graph.getFittedFuncts()[i].setYs(graph.getFittedFuncts()[i].getOriginalYs());
        }

        graph.calculateDefaultRangeDomain();

        // applies the default range if needed
        if (autoShrinkRange) {
            graph.setDefaultRangeDomain();
        } else {
            graph.calculateCustomRange();
        }

        if (modifyDialog != null) {
            updateModifyDialog();
        }

        update(getGraphics());
    }

    /**
     * Replaces the function used with a line image.
     *
     * @param  newX   a 1-d array of which contains the x-coordinates for the function
     * @param  newY   a 1-d array of which contains the y-coordinates for the function
     * @param  myVoi  the new VOI for this function
     * @param  index  DOCUMENT ME!
     */
    public void replaceFunction(float[] newX, float[] newY, int[][] newXYCoords, VOI myVoi, int index) {
        ViewJComponentFunct[] tempFuncts;
        ViewJComponentFunct[] tempFittedFuncts;
        float[] x;
        float[] y;
        int[][] xyCoords;
        voi = myVoi;

        try {
            x = new float[newX.length];
            y = new float[newY.length];
            xyCoords = new int[newXYCoords.length][2];
            
            // creates copies of the paramaters, so that if the arrays pointed to by them
            // are modified in ViewJComponentEditImage, the functions are not modified
            x = new float[newX.length];

            for (int j = 0; j < x.length; j++) {
                x[j] = newX[j];
            }

            y = new float[newY.length];

            for (int j = 0; j < y.length; j++) {
                y[j] = newY[j];
            }
            
            for (int j = 0; j < xyCoords.length; j++) {
            	xyCoords[j][0] = newXYCoords[j][0];
            	xyCoords[j][1] = newXYCoords[j][1];
            }

            tempFuncts = new ViewJComponentFunct[1];
            tempFittedFuncts = new ViewJComponentFunct[1];

            // change the first three functions to the new ones (RGB functions)
            // System.out.println ("replace function: new index = " + index);
            tempFuncts[0] = new ViewJComponentFunct(x, y, index + 1, voi, xyCoords); // empty function
            tempFittedFuncts[0] = new ViewJComponentFunct(x, y, Color.red, voi, xyCoords); // empty function

        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph.replaceFunction()");

            return;
        }

        graph.setFuncts(tempFuncts);
        graph.setFittedFuncts(tempFittedFuncts);

        // resets the zoom and range of the graph.
        for (int i = 0; i < graph.getFuncts().length; i++) {
            graph.getFuncts()[i].setXs(graph.getFuncts()[i].getOriginalXs()); // gets the original x and y point values
            graph.getFuncts()[i].setYs(graph.getFuncts()[i].getOriginalYs());
            itemResetGraph.setEnabled(false);
        }

        for (int i = 0; i < graph.getFittedFuncts().length; i++) {
            graph.getFittedFuncts()[i].setXs(graph.getFittedFuncts()[i].getOriginalXs()); // gets the original x and y
                                                                                          // point values
            graph.getFittedFuncts()[i].setYs(graph.getFittedFuncts()[i].getOriginalYs());
        }

        graph.calculateDefaultRangeDomain();

        // applies the default range if needed
        if (autoShrinkRange) {
            graph.setDefaultRangeDomain();
        } else {
            graph.calculateCustomRange();
        }

        if (modifyDialog != null) {
            updateModifyDialog();
        }

        update(getGraphics());
    }

    /**
     * Saves the current function and adds a blank new function.
     *
     * @param  newX   array of which contains the x-coordinates for the new function
     * @param  newY   an array of which contains the y-coordinates for the new functions
     * @param  index  the index of the point to be updated
     */
    public void saveNewFunction(float[] newX, float[] newY, int index) {

        if ((index > 4) || (graph.getFuncts().length > 4)) { // cannot have more than five functions
            return;
        }

        ViewJComponentFunct[] tempFuncts;
        ViewJComponentFunct[] tempFittedFuncts;
        float[] x;
        float[] y;

        try {
            x = new float[newX.length];
            y = new float[newY.length];

            for (int i = 0; i < x.length; i++) {
                x[i] = newX[i];
            }

            for (int i = 0; i < y.length; i++) {
                y[i] = newY[i];
            }

            tempFuncts = new ViewJComponentFunct[graph.getFuncts().length + 1];
            tempFuncts[index] = new ViewJComponentFunct(x, y, index + 1, voi, null); // empty function
            tempFittedFuncts = new ViewJComponentFunct[graph.getFittedFuncts().length + 1];
            tempFittedFuncts[index] = new ViewJComponentFunct(x, y, Color.red, voi, null); // empty function
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph.saveCurrentFunction()");

            return;
        }

        tempFuncts[index].setColor(graph.getFuncts().length);

        // copy functions into the temporary array, which includes the new function
        // moving the functions representing points to the beg. of the array
        for (int i = 0; i < index; i++) {
            tempFuncts[i] = graph.getFuncts()[i];
            tempFittedFuncts[i] = graph.getFittedFuncts()[i];
        }

        for (int i = index + 1; i <= graph.getFuncts().length; i++) {
            tempFuncts[i] = graph.getFuncts()[i - 1];
            tempFittedFuncts[i] = graph.getFittedFuncts()[i - 1];
        }

        // set the graph to the new functions
        graph.setFuncts(tempFuncts);
        graph.setFittedFuncts(tempFittedFuncts);

        // reset the zoom and range
        for (int i = 0; i < graph.getFuncts().length; i++) {
            graph.getFuncts()[i].setXs(graph.getFuncts()[i].getOriginalXs()); // gets the original x and y point values
            graph.getFuncts()[i].setYs(graph.getFuncts()[i].getOriginalYs());
            itemResetGraph.setEnabled(false);
        }

        for (int i = 0; i < graph.getFittedFuncts().length; i++) {
            graph.getFittedFuncts()[i].setXs(graph.getFittedFuncts()[i].getOriginalXs()); // gets the original x and y
                                                                                          // point values
            graph.getFittedFuncts()[i].setYs(graph.getFittedFuncts()[i].getOriginalYs());
        }

        graph.calculateDefaultRangeDomain();

        if (autoShrinkRange) {
            graph.setDefaultRangeDomain();
        } else {
            graph.calculateCustomRange();
        }

        // changes the menu items accordingly and rebuilds the menu if needed
        int len = graph.getFuncts().length;

        if (index == (len - 1)) {
            vectorCopyFunct.add(new JMenuItem(graph.getFuncts()[len - 1].getFunctName()));
            ((JMenuItem) vectorCopyFunct.elementAt(len - 1)).setFont(MipavUtil.font12B);
            copyMenu.add((JMenuItem) vectorCopyFunct.elementAt(len - 1));

            ((JMenuItem) vectorCopyFunct.elementAt(len - 1)).addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent event) {
                        JMenuItem c = (JMenuItem) (event.getSource());

                        for (int i = 0; i < graph.getFuncts().length; i++) {

                            if (c == vectorCopyFunct.elementAt(i)) {
                                graph.copyFunct(i);
                                update(getGraphics());

                                break;
                            }
                        }
                    }
                });

            vectorDeleteFunct.add(new JMenuItem(graph.getFuncts()[len - 1].getFunctName()));
            ((JMenuItem) vectorDeleteFunct.elementAt(len - 1)).setFont(MipavUtil.font12B);
            deleteMenu.add((JMenuItem) vectorDeleteFunct.elementAt(len - 1));

            if (graph.getFuncts()[len - 1].getVOI() == null) {
                ((JMenuItem) vectorDeleteFunct.elementAt(len - 1)).addActionListener(new ActionListener() {
                        public void actionPerformed(ActionEvent event) {
                            JMenuItem c = (JMenuItem) (event.getSource());

                            for (int i = 0; i < graph.getFuncts().length; i++) {

                                if (c == vectorDeleteFunct.elementAt(i)) {
                                    graph.deleteFunct(i);
                                    deleteMenu.remove((JMenuItem) vectorDeleteFunct.elementAt(i));
                                    vectorDeleteFunct.removeElementAt(i);
                                    copyMenu.remove((JMenuItem) vectorCopyFunct.elementAt(i));
                                    vectorCopyFunct.removeElementAt(i);
                                    update(getGraphics());

                                    if (modifyDialog != null) {
                                        updateModifyDialog();
                                    }

                                    break;
                                }
                            }
                        }
                    });
            } else {
                ((JMenuItem) vectorDeleteFunct.elementAt(len - 1)).setEnabled(false);
            }
        } else {
            String[] name = new String[len];

            for (int i = 0; i < graph.getFuncts().length; i++) {
                name[i] = graph.getFuncts()[i].getFunctName();
            }

            for (int i = 0; i < vectorCopyFunct.size(); i++) {
                copyMenu.remove((JMenuItem) vectorCopyFunct.elementAt(i));
            }

            for (int i = 0; i < vectorDeleteFunct.size(); i++) {
                deleteMenu.remove((JMenuItem) vectorDeleteFunct.elementAt(i));
            }

            vectorCopyFunct.removeAllElements();
            vectorDeleteFunct.removeAllElements();

            for (int i = 0; i < graph.getFuncts().length; i++) {

                try {
                    vectorCopyFunct.add(new JMenuItem(name[i]));
                    vectorDeleteFunct.add(new JMenuItem(name[i]));
                } catch (OutOfMemoryError error) {
                    MipavUtil.displayError("Out of memory: ViewJComponentGraph.buildMenu");

                    return;
                }

                ((JMenuItem) vectorCopyFunct.elementAt(i)).addActionListener(new ActionListener() {
                        public void actionPerformed(ActionEvent event) {
                            JMenuItem c = (JMenuItem) (event.getSource());

                            for (int j = 0; j < graph.getFuncts().length; j++) {

                                if (c == vectorCopyFunct.elementAt(j)) {
                                    graph.copyFunct(j);
                                    update(getGraphics());

                                    break;
                                }
                            }
                        }
                    });

                if (graph.getFuncts()[i].getVOI() == null) {
                    ((JMenuItem) vectorDeleteFunct.elementAt(i)).addActionListener(new ActionListener() {
                            public void actionPerformed(ActionEvent event) {
                                JMenuItem c = (JMenuItem) (event.getSource());

                                for (int j = 0; j < graph.getFuncts().length; j++) {

                                    if (c == vectorDeleteFunct.elementAt(j)) {

                                        graph.deleteFunct(j);

                                        deleteMenu.remove((JMenuItem) vectorDeleteFunct.elementAt(j));
                                        vectorDeleteFunct.removeElementAt(j);

                                        copyMenu.remove((JMenuItem) vectorCopyFunct.elementAt(j));
                                        vectorCopyFunct.removeElementAt(j);
                                        graph.calculateDefaultRangeDomain();

                                        if (autoShrinkRange) {
                                            graph.setDefaultRangeDomain();
                                        } else {
                                            graph.calculateCustomRange();
                                        }

                                        if (modifyDialog != null) {
                                            updateModifyDialog();
                                        }

                                        update(getGraphics());

                                        break;
                                    }
                                }
                            }
                        });
                } else {
                    ((JMenuItem) vectorDeleteFunct.elementAt(i)).setEnabled(false);
                }

                ((JMenuItem) vectorCopyFunct.elementAt(i)).setFont(MipavUtil.font12B);
                ((JMenuItem) vectorDeleteFunct.elementAt(i)).setFont(MipavUtil.font12B);
                copyMenu.add((JMenuItem) vectorCopyFunct.elementAt(i));
                deleteMenu.add((JMenuItem) vectorDeleteFunct.elementAt(i));

            }

        }

        if (modifyDialog != null) {
            updateModifyDialog();
        }

        update(getGraphics());
    }

    /**
     * Replaces the first three functions (used with RGB images).
     *
     * @param  newX     a 2-d array of which contains the x-coordinates for the three functions (RGB)
     * @param  newY     a 2-d array of which contains the y-coordinates for the three functions (RGB)
     * @param  ptIndex  the index of the point to be updated
     */
    public void saveNewFunction(float[][] newX, float[][] newY, int ptIndex) {
        updateRGBIndex = ptIndex;

        ViewJComponentFunct[] tempFuncts;
        ViewJComponentFunct[] tempFittedFuncts;
        float[][] x;
        float[][] y;

        try {
            x = new float[newX.length][];
            y = new float[newY.length][];

            // creates copies of the paramaters, so that if the arrays pointed to by them
            // are modified in ViewJComponentEditImage, the functions are not modified
            for (int i = 0; i < x.length; i++) {
                x[i] = new float[newX[i].length];

         
          for (int j = 0; j < x[i].length; j++) {
                    x[i][j] = newX[i][j];
                }
            }

            for (int i = 0; i < y.length; i++) {
                y[i] = new float[newY[i].length];

                for (int j = 0; j < y[i].length; j++) {
                    y[i][j] = newY[i][j];
                }
            }

            tempFuncts = new ViewJComponentFunct[3];
            tempFittedFuncts = new ViewJComponentFunct[3];

            // change the first three functions to the new ones (RGB functions)
            for (int i = 0; i < x.length; i++) {
                tempFuncts[i] = new ViewJComponentFunct(x[i], y[i], i + 1, voi,null); // empty function
                tempFittedFuncts[i] = new ViewJComponentFunct(x[i], y[i], Color.red, voi,null); // empty function
            }

            for (int i = x.length; i < tempFuncts.length; i++) {
                tempFuncts[i] = graph.getFuncts()[i];
                tempFittedFuncts[i] = graph.getFittedFuncts()[i];
            }
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph.saveNewFunction()");

            return;
        }

        graph.setFuncts(tempFuncts);
        graph.setFittedFuncts(tempFittedFuncts);

        // resets the zoom and range of the graph.
        for (int i = 0; i < graph.getFuncts().length; i++) {
            graph.getFuncts()[i].setXs(graph.getFuncts()[i].getOriginalXs()); // gets the original x and y point values
            graph.getFuncts()[i].setYs(graph.getFuncts()[i].getOriginalYs());
            itemResetGraph.setEnabled(false);
        }

        for (int i = 0; i < graph.getFittedFuncts().length; i++) {
            graph.getFittedFuncts()[i].setXs(graph.getFittedFuncts()[i].getOriginalXs()); // gets the original x and y
                                                                                          // point values
            graph.getFittedFuncts()[i].setYs(graph.getFittedFuncts()[i].getOriginalYs());
        }

        graph.calculateDefaultRangeDomain();

        // applies the default range if needed
        if (autoShrinkRange) {
            graph.setDefaultRangeDomain();
        } else {
            graph.calculateCustomRange();
        }

        if (modifyDialog != null) {
            updateModifyDialog();
        }

        update(getGraphics());
    }

    /**
     * Accessor that sets the default directory.
     *
     * @param  dir  string to set the default directory to
     */
    public void setDefaultDirectory(String dir) {
        defaultDirectory = dir;
    }

    /**
     * Sets the axis labels.
     *
     * @param  labelX  DOCUMENT ME!
     * @param  labelY  DOCUMENT ME!
     */
    public void setLabels(String labelX, String labelY) {
        graph.setLabels(labelX, labelY);
    }

    /**
     * Accessor that enables or disables the reset item.
     *
     * @param  enabled  boolean that enables if true, disables if false
     */
    public void setResetEnabled(boolean enabled) {
        itemResetGraph.setEnabled(enabled);
    }

    /**
     * Sets the units in the default axis labels.
     *
     * @param  units  the String representing the units to be displayed
     */
    public void setUnitsInLabel(String units) {
        this.lastUnits = this.units;
        this.units = units;

        if (units == null) {
            graph.setLabels("Position on Curve (pixels)", "Intensity"); // sets default axis labels
        } else {
            graph.setLabels("Position on Curve (" + units + ")", "Intensity"); // sets default axis labels
        }
    }

    /**
     * Makes the dialog visible in center of screen.
     *
     * @param  status  - flag to indicating if the dialog should be visible
     */
    public void setVisible(boolean status) {
        this.setLocation(50, 50);
        super.setVisible(status);
    }

    /**
     * Makes the dialog visible without setting position.
     *
     * @param  status  - flag to indicating if the dialog should be visible
     */
    public void setVisibleNoLocChange(boolean status) {
        super.setVisible(status);
    }

    /**
     * Accessor that sets the VOI ViewJFrameGraph was made from.
     *
     * @param  v  DOCUMENT ME!
     */
    public void setVOI(VOI v) {
        frameGraphVOI = v;
    }

    /**
     * DOCUMENT ME!
     */
    public void showXYZLegends() {
        ViewJComponentFunct[] functArray = graph.getFuncts();
        functArray[0].setFunctName(new String("x"));

        if (functArray.length >= 2) {
            functArray[1].setFunctName(new String("y"));

            if (functArray.length >= 3) {
                functArray[2].setFunctName(new String("z"));
            }
        }

        graph.setFuncts(functArray);
        graph.setLegendVisible(true);
        update(getGraphics());
    }

    /**
     * ChangeListener************************************************************************* /** Sets values based on
     * knob along slider
     *
     * @param  e  event that triggered this function
     */
    public void stateChanged(ChangeEvent e) {
        Object source = e.getSource();

        if (source == tabbedPane) { }
    }

    /**
     * Calls paint instead of erasing.
     *
     * @param  g  graphics to paint in
     */
    public void update(Graphics g) {

        if ((this.units != null) && !(this.units.equals(this.lastUnits))) {
            graph.paintComponent(graph.getGraphics());
            this.lastUnits = this.units;
        }

        paint(g);
    }

    /**
     * Updates the graph by changing the plot to the desired new function / intensity plot.
     *
     * @param  x  the array of x coordinates to be plotted in the graph
     * @param  y  the array of y coordinates to be plotted in the graph
     * @param  j  The function index of which point to be updated
     */
    public void update(float[] x, float[] y, int j) {

        if ((j > 4) || (graph.getFuncts().length <= 0)) { // cannot add have than five functions in the graph
            return;
        }

        // resets the range and zoom
        for (int i = 0; i < graph.getFuncts().length; i++) {
            graph.getFuncts()[i].setXs(graph.getFuncts()[i].getOriginalXs()); // gets the original x and y point values
            graph.getFuncts()[i].setYs(graph.getFuncts()[i].getOriginalYs());
            itemResetGraph.setEnabled(false);
        }

        for (int i = 0; i < graph.getFittedFuncts().length; i++) {
            graph.getFittedFuncts()[i].setXs(graph.getFittedFuncts()[i].getOriginalXs()); // gets the original x and y
                                                                                          // point values
            graph.getFittedFuncts()[i].setYs(graph.getFittedFuncts()[i].getOriginalYs());
        }

        graph.getFuncts()[j].setOriginalXs(x);
        graph.getFuncts()[j].setOriginalYs(y);
        graph.getFuncts()[j].setXs(x);
        graph.getFuncts()[j].setYs(y);
        graph.calculateDefaultRangeDomain();

        if (autoShrinkRange) {
            graph.setDefaultRangeDomain();
        } else {
            graph.calculateCustomRange();
        }

        // updates the dialog box's values for the default and current range
        if (minRangeField != null) {
            minRangeField.setText("" + graph.getMinRange());
        }

        if (maxRangeField != null) {
            maxRangeField.setText("" + graph.getMaxRange());
        }

        if (minRangeLabel != null) {
            minRangeLabel.setText("Min. for Range (<" + Float.toString(graph.getDefaultMinRange()) + ")");
        }

        if (maxRangeLabel != null) {
            maxRangeLabel.setText("Max. for Range (>" + Float.toString(graph.getDefaultMaxRange()) + ")");
        }

        graph.paintComponent(graph.getGraphics());

    }

    /**
     * Updates the graph by changing the plot to the desired new 3 functions (RGB).
     *
     * @param  x        the 2-d array of x coordinates to be plotted in the graph for each function
     * @param  y        the 2-d array of y coordinates to be plotted in the graph for each function
     * @param  ptIndex  the index of the point to be updated (will not do anything if trying to update of an index
     *                  different than updateRGBIndex)
     */
    public void update(float[][] x, float[][] y, int ptIndex) {

        if (ptIndex != updateRGBIndex) {
            return;
        }

        int i;

        // resets the range and zoom
        for (i = 0; i < graph.getFuncts().length; i++) {
            graph.getFuncts()[i].setXs(graph.getFuncts()[i].getOriginalXs()); // gets the original x and y point values
            graph.getFuncts()[i].setYs(graph.getFuncts()[i].getOriginalYs());
            itemResetGraph.setEnabled(false);
        }

        for (i = 0; i < graph.getFittedFuncts().length; i++) {
            graph.getFittedFuncts()[i].setXs(graph.getFittedFuncts()[i].getOriginalXs()); // gets the original x and y
                                                                                          // point values
            graph.getFittedFuncts()[i].setYs(graph.getFittedFuncts()[i].getOriginalYs());
        }

        try {

            // changes the first the functions (RGB) to the desired new ones
            for (i = 0; i < x.length; i++) {
                graph.getFuncts()[i].setOriginalXs(x[i]);
                graph.getFuncts()[i].setOriginalYs(y[i]);
                graph.getFuncts()[i].setXs(x[i]);
                graph.getFuncts()[i].setYs(y[i]);
            }
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph.update()");

            return;
        }

        // recalculates the domain and range
        graph.calculateDefaultRangeDomain();

        // sets the domain and range to the new default values if needed
        if (autoShrinkRange) {
            graph.setDefaultRangeDomain();
        } else {
            graph.calculateCustomRange();
        }

        if (x.length != y.length) { // must be pairs of x and y coordinates, or the graph cannot be drawn
            MipavUtil.displayError("X and Y Arrays must be of equal length");
            this.dispose();
        }

        // changes the items in the modify dialog which deal with range to their new values
        if (minRangeField != null) {
            minRangeField.setText("" + graph.getMinRange());
        }

        if (maxRangeField != null) {
            maxRangeField.setText("" + graph.getMaxRange());
        }

        if (minRangeLabel != null) {
            minRangeLabel.setText("Min. for Range (<" + Float.toString(graph.getDefaultMinRange()) + ")");
        }

        if (maxRangeLabel != null) {
            maxRangeLabel.setText("Max. for Range (>" + Float.toString(graph.getDefaultMaxRange()) + ")");
        }

        graph.paintComponent(graph.getGraphics());

    }

    /**
     * @deprecated If data were changed, this could be useful to refit data without pressing a new button,
     * but this function is not currently utilized.  Will be removed in future version of MIPAV unless a
     * use can be found
     * 
     * Updates Fitted Functions to accurately estimate the functions in the graph (Function data may have chaned due to
     * realtime updating, and therefore the fitted functions must be updated).
     */
    public void updateFittedFunctions() {

    	int nPoints;
        NLFittedFunction fe = null;

        ViewJComponentFunct[] functions = graph.getFuncts();
        ViewJComponentFunct[] fittedFunctions = graph.getFittedFuncts();
        float[] x;
        float[] y;

        try {

            for (int i = 0; i < functions.length; i++) {
                nPoints = graph.getFuncts()[i].getOriginalXs().length;
                fe = (NLFittedFunction)fitMode.getImpl().newInstance(nPoints, graph.getFuncts()[i].getOriginalXs(),
                                        graph.getFuncts()[i].getOriginalYs());
                fe.driver();
                fe.displayResults();

                x = new float[functions[i].getXs().length];
                y = new float[x.length];

                double[] yTemp = fe.getFittedY();
                
                for (int j = 0; j < x.length; j++) {
                    x[j] = (functions[i].getXs()[j]);
                	y[j] = (float)yTemp[j];
                }

                fittedFunctions[i].setXs(x);
                fittedFunctions[i].setOriginalXs(x);
                fittedFunctions[i].setYs(y);
                fittedFunctions[i].setOriginalYs(y);
            }
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Graph :  Out of memory ");

        } catch (Exception e) {
        	MipavUtil.displayError("Fitting functions failed.");
			e.printStackTrace();
		} 

        for (int index = 0; index < 5; index++) {

            if (index >= graph.getFuncts().length) {
                fitFunctVisibleCheckbox[index].setEnabled(false);
                fitFunctVisibleCheckbox[index].setSelected(false);
            } else {
                fitFunctVisibleCheckbox[index].setEnabled(true);
                fitFunctVisibleCheckbox[index].setSelected(graph.getFuncts()[index].getFitFunctionVisible());
            }
        }

        update(getGraphics()); 

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
     * Disposes of error dialog, then frame. Nullifies pointer to this in VOIPoint, if this graph was made from a
     * VOIPoint.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowClosing(WindowEvent event) {

        if (frameGraphVOI != null) {
            frameGraphVOI.setContourGraph(null);
        }

        dispose();
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

    /**
     * Unchanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowOpened(WindowEvent event) { }

    /**
     * Creates a panel to edit various fitted function features. (Panel is to be used in the tabbed pane of the Modify
     * Graph Dialog box
     */
    private void buildFitFunctPanel() {

        String[] name;
        JLabel functionLabel;

        try {
            fitFunctPanel = new JPanel();
            fitFunctTypePanel = new JPanel();
            fitFunctVisiblePanel = new JPanel();
            fitType = new JComboBox(FitMode.values());
            fitFunctVisibleCheckbox = new JCheckBox[5];
            functionLabel = new JLabel("Fitting function type: ");
            name = new String[5];
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJComponentGraph.buildMenu");

            return;
        }

        fitFunctPanel.setBounds(10, 10, 480, 325);
        fitFunctPanel.setLayout(null);
        fitFunctPanel.setBorder(new EtchedBorder());

        fitFunctTypePanel.setBounds(10, 10, 465, 85);
        fitFunctTypePanel.setLayout(null);
        fitFunctTypePanel.setBorder(new EtchedBorder());
        fitFunctPanel.add(fitFunctTypePanel);

        fitFunctVisiblePanel.setBounds(10, 105, 465, 245);
        fitFunctVisiblePanel.setBorder(MipavUtil.buildTitledBorder("Fitted Functions"));
        fitFunctPanel.add(fitFunctVisiblePanel);

        functionLabel.setBounds(10, 10, 110, 30);
        functionLabel.setFont(MipavUtil.font12);
        fitFunctTypePanel.add(functionLabel);
        
        fitType.setBounds(135, 10, 320, 30);
        fitType.setFont(MipavUtil.font12);
        fitType.addActionListener(this);
        fitType.setActionCommand("FitFunctions");
        fitType.setFont(MipavUtil.font12);
        fitFunctTypePanel.add(fitType);
        
        numVariablesLabel = new JLabel("Number of variables (Only for Multiexponential)");
        numVariablesLabel.setBounds(10, 45, 310, 30);
        numVariablesLabel.setFont(MipavUtil.font12);
        fitFunctTypePanel.add(numVariablesLabel);
        
        numVariablesField = new JTextField(5);
        numVariablesField.setBounds(335, 45, 120, 30);
        numVariablesField.setText("5");
        numVariablesField.setFont(MipavUtil.font12);
        numVariablesField.setForeground(Color.black);
        fitFunctTypePanel.add(numVariablesField);

        for (int index = 0; index < 5; index++) {

            if (index < graph.getFuncts().length) {
                name[index] = "Fitted " + graph.getFuncts()[index].getFunctName() + " Visible";
            } else {
                name[index] = "Fitted Function " + (index + 1) + " Visible";
            }

            try {
                fitFunctVisibleCheckbox[index] = new JCheckBox(name[index]);
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ViewJComponentGraph.buildFitFunctPanel");

                return;
            }

            if ((fitMode == FitMode.fitNone) && (index < graph.getFuncts().length)) {
                fitFunctVisibleCheckbox[index].setEnabled(false);
                fitFunctVisibleCheckbox[index].setSelected(false);
                graph.getFuncts()[index].setFitFunctionVisible(false);
            }

            if (index >= graph.getFuncts().length) {
                fitFunctVisibleCheckbox[index].setEnabled(false);
                fitFunctVisibleCheckbox[index].setSelected(false);
            } else {
                fitFunctVisibleCheckbox[index].setEnabled(true);
                fitFunctVisibleCheckbox[index].setSelected(graph.getFuncts()[index].getFitFunctionVisible());
            }

            fitFunctVisibleCheckbox[index].addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent event) {
                        JCheckBox c = (JCheckBox) (event.getSource());

                        for (int i = 0; i < graph.getFuncts().length; i++) {

                            if (c == fitFunctVisibleCheckbox[i]) {
                                graph.getFuncts()[i].setFitFunctionVisible(fitFunctVisibleCheckbox[i].isSelected());

                                break;
                            }
                        }

                        update(getGraphics());
                    }
                });

            if (index < 3) {
                fitFunctVisibleCheckbox[index].setBounds(25, 12 + (30 * index), 150, 30);
            } else {
                fitFunctVisibleCheckbox[index].setBounds(230, 12 + (30 * (index - 3)), 150, 30);
            }

            fitFunctVisibleCheckbox[index].setFont(MipavUtil.font12);
            fitFunctVisiblePanel.add(fitFunctVisibleCheckbox[index]);
        }

        tabbedPane.addTab("Fitted Functions", null, fitFunctPanel);
    }

    /**
     * Creates a panel where various features of the functions can be edited, such as showing the legend, and changing
     * function names. This panel is used by the tabbed pane in Modify Graph Dialog Box.
     */
    private void buildFunctionPanel() {

    	JLabel nameTop, colorTop, visibleTop;
    	String[] name;
        boolean show;
    	
        try {
            legendPanel = new JPanel();
            showPanel = new JPanel();
            functionPropPanel = new JPanel();
            legendCheckbox = new JCheckBox("Show Legend", graph.getLegendVisible());
            nameField = new JTextField[5]; // an array of text fields for inputting the new name for each function
            functionLabel = new JLabel[5]; // an array of labels for the corresponding function text fields
        
            pointsVisiblePanel = new JPanel();
            pointsAndLinesGroup = new ButtonGroup();
            linesOnlyButton = new JRadioButton("Show only lines", graph.getPointsAndLinesDisplay() == ViewJComponentGraph.SHOW_LINES_ONLY);
            pointsAndLinesButton = new JRadioButton("Show points and lines",
                    graph.getPointsAndLinesDisplay() == ViewJComponentGraph.SHOW_POINTS_AND_LINES);
            pointsOnlyButton = new JRadioButton("Show only points", graph.getPointsAndLinesDisplay() == ViewJComponentGraph.SHOW_POINTS_ONLY);
            functVisibleCheckbox = new JCheckBox[5];
            functLineColorButton = new JButton[5];
            name = new String[5];
            
            nameTop = new JLabel("Function name");
            colorTop = new JLabel("Color");
            visibleTop = new JLabel("Visible");
        
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJComponentGraph.buildLegendPanel");

            return;
        }

        legendPanel.setLayout(null);
        legendPanel.setBorder(new EtchedBorder());

        showPanel.setBounds(PANEL_OFFSET, PANEL_OFFSET, 228, 35);
        showPanel.setBorder(new EtchedBorder());
        showPanel.setLayout(null);
        legendPanel.add(showPanel);

        functionPropPanel.setBounds(PANEL_OFFSET, 119, 465, 260);
        functionPropPanel.setBorder(MipavUtil.buildTitledBorder("Functions"));
        functionPropPanel.setLayout(new GridBagLayout());
        legendPanel.add(functionPropPanel);

        legendCheckbox.setBounds(5, 5, 90, 22);
        legendCheckbox.addActionListener(this);
        legendCheckbox.setActionCommand("Legend");
        legendCheckbox.setFont(MipavUtil.font12);
        showPanel.add(legendCheckbox);
        
        pointsVisiblePanel.setBounds(250, PANEL_OFFSET, 222, 99);
        pointsVisiblePanel.setBorder(new EtchedBorder());
        pointsVisiblePanel.setLayout(null);
        legendPanel.add(pointsVisiblePanel);

        linesOnlyButton.addActionListener(this);
        linesOnlyButton.setBounds(5, 5, 150, 22);
        linesOnlyButton.setFont(MipavUtil.font12);
        pointsAndLinesGroup.add(linesOnlyButton);
        pointsVisiblePanel.add(linesOnlyButton);
        
        pointsAndLinesButton.addActionListener(this);
        pointsAndLinesButton.setBounds(5, 37, 150, 22);
        pointsAndLinesButton.setFont(MipavUtil.font12);
        pointsAndLinesGroup.add(pointsAndLinesButton);
        pointsVisiblePanel.add(pointsAndLinesButton);
        
        pointsOnlyButton.addActionListener(this);
        pointsOnlyButton.setBounds(5, 69, 150, 22);
        pointsOnlyButton.setFont(MipavUtil.font12);
        pointsAndLinesGroup.add(pointsOnlyButton);
        pointsVisiblePanel.add(pointsOnlyButton);

        GridBagConstraints c = new GridBagConstraints();
        c.fill = GridBagConstraints.BOTH;
        c.gridx = 1;
        c.gridy = 0;
        c.insets = new Insets(5, 20, 5, 20);
        functionPropPanel.add(nameTop, c);
        c.gridx = 2;
        c.insets = new Insets(5, 10, 5, 10);
        functionPropPanel.add(colorTop, c);
        c.gridx = 3;
        functionPropPanel.add(visibleTop, c);   
       
        for (int i = 0; i < 5; i++) { // sets the bounds for each of the text fields and label for

            // inputting the function names
            try {

                if (i < graph.getFuncts().length) {
                    nameField[i] = new JTextField(graph.getFuncts()[i].getFunctName()); // sets the default string in
                                                                                          // the text field to
                } else {
                    nameField[i] = new JTextField();
                }

                functionLabel[i] = new JLabel("Function " + (i + 1)); // sets the label to the corresponding
                                                                              // function

                // index number
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ViewJComponentGraph.buildLegendPanel");

                return;
            }

            nameField[i].setFont(MipavUtil.font12);
            functionLabel[i].setFont(MipavUtil.font12B);

            nameField[i].setMinimumSize(new Dimension(100, 28));
            nameField[i].setPreferredSize(new Dimension(100, 28));
            functionLabel[i].setForeground(Color.black);

            if (i >= graph.getFuncts().length) {
                nameField[i].setEnabled(false);
                functionLabel[i].setEnabled(false);
            }
            
            if (i < graph.getFuncts().length) {
                name[i] = graph.getFuncts()[i].getFunctName();
                show = graph.getFuncts()[i].getFunctionVisible();
            } else {
                name[i] = "Function " + (i + 1);
                show = false;
            }

            try {
                functVisibleCheckbox[i] = new JCheckBox();
                functLineColorButton[i] = new JButton();
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ViewJComponentGraph.buildFunctionPanel");

                return;
            }

            if (i >= graph.getFuncts().length) {
                functVisibleCheckbox[i].setEnabled(false);
                functVisibleCheckbox[i].setSelected(false);
                functLineColorButton[i].setEnabled(false);
            } else {
            	functVisibleCheckbox[i].setSelected(show);
                functLineColorButton[i].setBackground(graph.getFuncts()[i].getColor());
            }

            functLineColorButton[i].setActionCommand("ColorChange");
            functLineColorButton[i].addActionListener(this);
            functVisibleCheckbox[i].addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent event) {
                        JCheckBox c = (JCheckBox) (event.getSource());

                        for (int i = 0; i < graph.getFuncts().length; i++) {

                            if (c == functVisibleCheckbox[i]) {
                                graph.getFuncts()[i].setFunctionVisible(functVisibleCheckbox[i].isSelected());
                                graph.calculateDefaultRangeDomain();

                                if (autoShrinkRange) {
                                    graph.setDefaultRangeDomain();
                                } else {
                                    graph.calculateCustomRange();
                                }

                                if (minRangeField != null) {
                                    minRangeField.setText("" + graph.getMinRange());
                                }

                                if (maxRangeField != null) {
                                    maxRangeField.setText("" + graph.getMaxRange());
                                }

                                if (minRangeLabel != null) {
                                    minRangeLabel.setText("Min. for Range (<" +
                                                          Float.toString(graph.getDefaultMinRange()) + ")");
                                }

                                if (maxRangeLabel != null) {
                                    maxRangeLabel.setText("Max. for Range (>" +
                                                          Float.toString(graph.getDefaultMaxRange()) + ")");
                                }

                                break;
                            }
                        }

                        update(getGraphics());
                    }
                });


            c.gridy = i+1;
            c.gridx = 0;
            c.anchor = GridBagConstraints.CENTER;
            c.insets = new Insets(5, 15, 5, 15);
            functionPropPanel.add(functionLabel[i], c);
            c.gridx = 1;
            c.insets = new Insets(5, 20, 5, 20);
            functionPropPanel.add(nameField[i], c); // adds all of the text fields and labels
            c.gridx = 2;
            c.fill = GridBagConstraints.BOTH;
            c.ipadx = 10;
            c.ipady = 10;
            c.anchor = GridBagConstraints.CENTER;
            c.insets = new Insets(5, 10, 5, 10);
            functionPropPanel.add(functLineColorButton[i], c);
            c.gridx = 3;
            c.fill = GridBagConstraints.BOTH;
            c.ipadx = 0;
            c.ipady = 0;
            functionPropPanel.add(functVisibleCheckbox[i], c);
            

        }

        tabbedPane.addTab("Functions", null, legendPanel);

    }

    /**
     * Function that builds the menu of various tools to be used in conjunction with the graph. ie: opening / saving
     * files, changing gridlines, title / axis labels, etc.
     */
    private void buildMenu() {

        Font font12B = MipavUtil.font12B;
        String[] name;

        try {
            fileMenu = new JMenu("File");
            viewMenu = new JMenu("Views");
            editMenu = new JMenu("Edit");
            itemOpenNewGraph = new JMenuItem("Open Graph to New Frame", MipavUtil.getIcon("open.gif"));
            itemOpenSameGraph = new JMenuItem("Open Graph to Same Frame", MipavUtil.getIcon("open.gif"));
            itemSaveGraph = new JMenuItem("Save Graph", MipavUtil.getIcon("save.gif"));
            itemPrintGraph = new JMenuItem("Print Graph");
            itemTableOutput = new JMenuItem("Data to Output Window");
            itemClose = new JMenuItem("Close Graph");
            vectorCopyFunct = new Vector<JMenuItem>(graph.getFuncts().length);
            vectorDeleteFunct = new Vector<JMenuItem>(graph.getFuncts().length);
            copyMenu = new JMenu("Copy Function");
            deleteMenu = new JMenu("Delete Function");
            itemModifyGraph = new JMenuItem("Modify Graph Features");
            itemResetGraph = new JMenuItem("Reset Graph to Original");
            itemResetRange = new JMenuItem("Reset Range to Default");
            itemPasteFunct = new JMenuItem("Paste Function");
            itemNormalize = new JMenuItem("Normalize Function");
            openingMenuBar = new JMenuBar();
            name = new String[graph.getFuncts().length];
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJComponentGraph.buildMenu");

            return;
        }

        fileMenu.setFont(font12B);
        editMenu.setFont(font12B);
        viewMenu.setFont(font12B);
        copyMenu.setFont(font12B);
        deleteMenu.setFont(font12B);
        
        itemOpenNewGraph.addActionListener(this);
        itemOpenNewGraph.setActionCommand("OpenNewGraph");
        itemOpenNewGraph.setFont(font12B);
        fileMenu.add(itemOpenNewGraph);

        itemOpenSameGraph.addActionListener(this);
        itemOpenSameGraph.setAccelerator(KeyStroke.getKeyStroke('O', Event.CTRL_MASK, false));
        itemOpenSameGraph.setActionCommand("OpenSameGraph");
        itemOpenSameGraph.setFont(font12B);
        fileMenu.add(itemOpenSameGraph);

        itemSaveGraph.addActionListener(this);
        itemSaveGraph.setAccelerator(KeyStroke.getKeyStroke('S', Event.CTRL_MASK, false));
        itemSaveGraph.setActionCommand("SaveGraph");
        itemSaveGraph.setFont(font12B);
        fileMenu.add(itemSaveGraph);

        itemPrintGraph.addActionListener(this);
        itemPrintGraph.setAccelerator(KeyStroke.getKeyStroke('P', Event.CTRL_MASK, false));
        itemPrintGraph.setActionCommand("PrintGraph");
        itemPrintGraph.setFont(font12B);
        fileMenu.add(itemPrintGraph);
        
        itemTableOutput.addActionListener(this);
        itemTableOutput.setActionCommand("TableOutput");
        itemTableOutput.setFont(font12B);
        fileMenu.add(itemTableOutput);

        fileMenu.addSeparator();

        itemClose.addActionListener(this);
        itemClose.setAccelerator(KeyStroke.getKeyStroke('X', Event.CTRL_MASK, false));
        itemClose.setActionCommand("CloseGraph");
        itemClose.setFont(font12B);
        fileMenu.add(itemClose);

        for (int i = 0; i < graph.getFuncts().length; i++) {
            name[i] = graph.getFuncts()[i].getFunctName();
        }

        for (int index = 0; index < graph.getFuncts().length; index++) {

            try {
                vectorCopyFunct.add(new JMenuItem(name[index]));
                vectorDeleteFunct.add(new JMenuItem(name[index]));
            } catch (OutOfMemoryError error) {
                MipavUtil.displayError("Out of memory: ViewJComponentGraph.buildMenu");

                return;
            }

            ((JMenuItem) vectorCopyFunct.elementAt(index)).addActionListener(new ActionListener() {
                    public void actionPerformed(ActionEvent event) {
                        JMenuItem c = (JMenuItem) (event.getSource());

                        for (int i = 0; i < graph.getFuncts().length; i++) {

                            if (c == vectorCopyFunct.elementAt(i)) {
                                graph.copyFunct(i);
                                update(getGraphics());

                                break;
                            }
                        }
                    }
                });

            if (graph.getFuncts()[index].getVOI() == null) {
                ((JMenuItem) vectorDeleteFunct.elementAt(index)).addActionListener(new ActionListener() {
                        public void actionPerformed(ActionEvent event) {
                            JMenuItem c = (JMenuItem) (event.getSource());

                            for (int i = 0; i < graph.getFuncts().length; i++) {

                                if (c == vectorDeleteFunct.elementAt(i)) {

                                    graph.deleteFunct(i);

                                    deleteMenu.remove((JMenuItem) vectorDeleteFunct.elementAt(i));
                                    vectorDeleteFunct.removeElementAt(i);

                                    copyMenu.remove((JMenuItem) vectorCopyFunct.elementAt(i));
                                    vectorCopyFunct.removeElementAt(i);
                                    graph.calculateDefaultRangeDomain();

                                    if (autoShrinkRange) {
                                        graph.setDefaultRangeDomain();
                                    } else {
                                        graph.calculateCustomRange();
                                    }

                                    if (modifyDialog != null) {
                                        updateModifyDialog();
                                    }

                                    update(getGraphics());

                                    break;
                                }
                            }
                        }
                    });
            } else {
                ((JMenuItem) vectorDeleteFunct.elementAt(index)).setEnabled(false);
            }

            ((JMenuItem) vectorCopyFunct.elementAt(index)).setFont(font12B);
            ((JMenuItem) vectorDeleteFunct.elementAt(index)).setFont(font12B);
            copyMenu.add((JMenuItem) vectorCopyFunct.elementAt(index));
            deleteMenu.add((JMenuItem) vectorDeleteFunct.elementAt(index));
        }

        editMenu.add(deleteMenu);
        editMenu.add(copyMenu);

        itemPasteFunct.addActionListener(this);
        itemPasteFunct.setActionCommand("PasteFunct");
        itemPasteFunct.setFont(font12B);
        editMenu.add(itemPasteFunct);

        if ((graph.getFuncts()[0].getXs() != null) && (graph.getFuncts()[0].getXs().length > 7)) {
            itemNormalize.addActionListener(this);
            itemNormalize.setActionCommand("Normalize");
            itemNormalize.setFont(font12B);
            editMenu.add(itemNormalize);
        }

        itemModifyGraph.addActionListener(this);
        itemModifyGraph.setActionCommand("ModifyGraph");
        itemModifyGraph.setFont(font12B);
        viewMenu.add(itemModifyGraph);

        itemResetRange.addActionListener(this);
        itemResetRange.setActionCommand("ResetRange");
        itemResetRange.setFont(font12B);
        viewMenu.add(itemResetRange);

        itemResetGraph.addActionListener(this);
        itemResetGraph.setAccelerator(KeyStroke.getKeyStroke('Z', Event.CTRL_MASK, false));
        itemResetGraph.setActionCommand("ResetGraph");
        itemResetGraph.setFont(font12B);
        viewMenu.add(itemResetGraph);
        setResetEnabled(false);

        openingMenuBar.add(fileMenu); // add the menus to the bar
        openingMenuBar.add(editMenu);
        openingMenuBar.add(viewMenu);
    }

    /**
     * Creats a panel where various featuers of the graph can be edited, such as visbility of gridlines and minor tick
     * marks, number of gridlines, background color, range, and labels for the title, x and y axiis.
     */
    private void buildModifyGraphPanel() {

        try {
            modifyGraphPanel = new JPanel();
            gridPanel = new JPanel();
            axisPanel = new JPanel();
            backgroundPanel = new JPanel();
            rangePanel = new JPanel();
            gridlinesCheckbox = new JCheckBox("Gridlines Visible", graph.getGridlinesVisible());
            minorTickMarksCheckbox = new JCheckBox("Minor Tick Marks Visible", graph.getMinorTickMarksVisible());
            autoShrinkRangeCheckbox = new JCheckBox("Auto Shrink Range", autoShrinkRange);
            xGridLineField = new JTextField("" + graph.getNumberOfXGridLines()); // default # of x and y gridlines is 4
            yGridLineField = new JTextField("" + graph.getNumberOfYGridLines());
            xGridLineLabel = new JLabel("Number of X-Axis Gridlines");
            yGridLineLabel = new JLabel("Number of Y-Axis Gridlines");
            titleField = new JTextField(graph.getTitle()); // displays the current title in the text box for changing
            titleLabel = new JLabel("Title");
            xAxisLabelField = new JTextField(graph.getXLabel()); // default labels for the x and y axii
            xAxisLabel = new JLabel("X-Axis Label");
            yAxisLabelField = new JTextField(graph.getYLabel());
            yAxisLabel = new JLabel("Y-Axis Label");
            backgroundButton = new JButton();
            resetRangeButton = new JButton("Reset Default Range");
            logScaleCheckbox = new JCheckBox("Log scale range", doLog);
            backgroundLabel = new JLabel("Change Background Color");
            minRangeField = new JTextField("" + graph.getMinRange());
            minRangeLabel = new JLabel("Min. for Range (<" + Float.toString(graph.getDefaultMinRange()) + ")");
            maxRangeField = new JTextField("" + graph.getMaxRange());
            maxRangeLabel = new JLabel("Max. for Range (>" + Float.toString(graph.getDefaultMaxRange()) + ")");
            if (image != null) {
                histogramPanel = new JPanel();
            }
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJComponentGraph.buildModifyGraphPanel");

            return;
        }

        if (image != null) {
            modifyGraphPanel.setBounds(PANEL_OFFSET, PANEL_OFFSET, 480, 333);    
        }
        else {
            modifyGraphPanel.setBounds(PANEL_OFFSET, PANEL_OFFSET, 480, 253);
        }
        modifyGraphPanel.setLayout(null);
        modifyGraphPanel.setBorder(new EtchedBorder());

        gridPanel.setBounds(PANEL_OFFSET, PANEL_OFFSET, 465, 78);
        gridPanel.setBorder(new EtchedBorder());
        gridPanel.setLayout(null);
        modifyGraphPanel.add(gridPanel);

        axisPanel.setBounds(PANEL_OFFSET, 92, 250, 200);
        axisPanel.setBorder(new EtchedBorder());
        axisPanel.setLayout(null);
        modifyGraphPanel.add(axisPanel);

        backgroundPanel.setBounds(270, 92, 205, 45);
        backgroundPanel.setBorder(new EtchedBorder());
        backgroundPanel.setLayout(null);
        modifyGraphPanel.add(backgroundPanel);

        rangePanel.setBounds(270, 147, 205, 175);
        rangePanel.setBorder(new EtchedBorder());
        rangePanel.setLayout(null);
        modifyGraphPanel.add(rangePanel);
        
        if (image != null) {
            histogramPanel.setBounds(PANEL_OFFSET, 302, 250, 150);
            histogramPanel.setBorder(new EtchedBorder());
            histogramPanel.setLayout(null);
            modifyGraphPanel.add(histogramPanel);
            
            imageMin = image.getMin();
            imageMax = image.getMax();
            int dataType = image.getFileInfo()[0].getDataType();
            if ((dataType != ModelStorageBase.FLOAT) && (dataType != ModelStorageBase.DOUBLE) &&
                (dataType != ModelStorageBase.ARGB_FLOAT)) {
                maxBins = (int)Math.round(imageMax - imageMin + 1);
            }
            else {
                maxBins = 4096;
            }
            
            JLabel minLabel = new JLabel("Minimum value   ");
            minLabel.setForeground(Color.black);
            minLabel.setFont(MipavUtil.font12);
            minLabel.setBounds(120, PANEL_OFFSET, 100, 30);
            histogramPanel.add(minLabel);

            minField = new JTextField();
            minField.setText(String.valueOf(initialUserMin));
            minField.setForeground(Color.black);
            minField.setFont(MipavUtil.font12);
            minField.setBounds(PANEL_OFFSET, PANEL_OFFSET, 100, 30);
            histogramPanel.add(minField);
            
            JLabel maxLabel = new JLabel("Maximum value   ");
            maxLabel.setForeground(Color.black);
            maxLabel.setFont(MipavUtil.font12);
            maxLabel.setBounds(120, 50, 100, 30);
            histogramPanel.add(maxLabel);

            maxField = new JTextField();
            maxField.setText(String.valueOf(initialUserMax));
            maxField.setForeground(Color.black);
            maxField.setFont(MipavUtil.font12);
            maxField.setBounds(PANEL_OFFSET, 50, 100, 30);
            histogramPanel.add(maxField);
            
            JLabel binsLabel = new JLabel("Number of bins   ");
            binsLabel.setForeground(Color.black);
            binsLabel.setFont(MipavUtil.font12);
            binsLabel.setBounds(120, 90, 100, 30);
            histogramPanel.add(binsLabel);

            binsField = new JTextField();
            binsField.setText(String.valueOf(initialSummaryBins));
            binsField.setForeground(Color.black);
            binsField.setFont(MipavUtil.font12);
            binsField.setBounds(PANEL_OFFSET, 90, 100, 30);
            histogramPanel.add(binsField);
        } // if (image != null)

        gridlinesCheckbox.setBounds(PANEL_OFFSET, PANEL_OFFSET, 150, 20);
        gridlinesCheckbox.addActionListener(this);
        gridlinesCheckbox.setActionCommand("Gridlines");
        gridlinesCheckbox.setFont(MipavUtil.font12);
        gridPanel.add(gridlinesCheckbox);

        minorTickMarksCheckbox.setBounds(PANEL_OFFSET, 40, 200, 20);
        minorTickMarksCheckbox.addActionListener(this);
        minorTickMarksCheckbox.setActionCommand("MinorTickMarks");
        minorTickMarksCheckbox.setFont(MipavUtil.font12);
        gridPanel.add(minorTickMarksCheckbox);

        xGridLineField.setFont(MipavUtil.font12);
        xGridLineField.setBounds(250, PANEL_OFFSET, 30, 25);
        gridPanel.add(xGridLineField);

        xGridLineLabel.setFont(MipavUtil.font12);
        xGridLineLabel.setBounds(290, PANEL_OFFSET, 150, 25);
        xGridLineLabel.setForeground(Color.black);
        gridPanel.add(xGridLineLabel);

        yGridLineField.setFont(MipavUtil.font12);
        yGridLineField.setBounds(250, 45, 30, 25);
        gridPanel.add(yGridLineField);

        yGridLineLabel.setFont(MipavUtil.font12);
        yGridLineLabel.setBounds(290, 43, 150, 30);
        yGridLineLabel.setForeground(Color.black);
        gridPanel.add(yGridLineLabel);

        titleField.setFont(MipavUtil.font12);
        titleField.setBounds(PANEL_OFFSET, 25, 150, 30);
        axisPanel.add(titleField);

        titleLabel.setFont(MipavUtil.font12);
        titleLabel.setBounds(170, 25, 100, 30);
        titleLabel.setForeground(Color.black);
        axisPanel.add(titleLabel);

        xAxisLabelField.setFont(MipavUtil.font12);
        xAxisLabelField.setBounds(PANEL_OFFSET, 75, 150, 30);
        axisPanel.add(xAxisLabelField);

        xAxisLabel.setFont(MipavUtil.font12);
        xAxisLabel.setBounds(170, 75, 100, 30);
        xAxisLabel.setForeground(Color.black);
        axisPanel.add(xAxisLabel);

        yAxisLabelField.setFont(MipavUtil.font12);
        yAxisLabelField.setBounds(PANEL_OFFSET, 122, 150, 30);
        axisPanel.add(yAxisLabelField);

        yAxisLabel.setFont(MipavUtil.font12);
        yAxisLabel.setBounds(170, 122, 100, 30);
        yAxisLabel.setForeground(Color.black);
        axisPanel.add(yAxisLabel);

        backgroundButton.setFont(MipavUtil.font12);
        backgroundButton.setBounds(15, 12, 20, 20);
        backgroundButton.setBackground(graph.getBackgroundColor());
        backgroundButton.addActionListener(this);
        backgroundButton.setActionCommand("ChangeBackground");
        backgroundButton.setMargin(new Insets(0, 0, 0, 0));
        backgroundPanel.add(backgroundButton);

        backgroundLabel.setFont(MipavUtil.font12);
        backgroundLabel.setBounds(45, 9, 180, 30);
        backgroundLabel.setForeground(Color.black);
        backgroundPanel.add(backgroundLabel);

        autoShrinkRangeCheckbox.setBounds(PANEL_OFFSET, PANEL_OFFSET, 175, 20);
        autoShrinkRangeCheckbox.addActionListener(this);
        autoShrinkRangeCheckbox.setActionCommand("AutoShrinkRange");
        autoShrinkRangeCheckbox.setFont(MipavUtil.font12);
        rangePanel.add(autoShrinkRangeCheckbox);

        minRangeField.setFont(MipavUtil.font12);
        minRangeField.setBounds(PANEL_OFFSET, 37, 60, 25);
        rangePanel.add(minRangeField);

        minRangeLabel.setFont(MipavUtil.font12);
        minRangeLabel.setBounds(75, 35, 165, 30);
        minRangeLabel.setForeground(Color.black);
        rangePanel.add(minRangeLabel);

        maxRangeField.setFont(MipavUtil.font12);
        maxRangeField.setBounds(PANEL_OFFSET, 72, 60, 25);
        rangePanel.add(maxRangeField);

        maxRangeLabel.setFont(MipavUtil.font12);
        maxRangeLabel.setBounds(75, 72, 165, 30);
        maxRangeLabel.setForeground(Color.black);
        rangePanel.add(maxRangeLabel);

        resetRangeButton.setFont(MipavUtil.font12B);
        resetRangeButton.setBounds(PANEL_OFFSET, 105, 150, 30);
        resetRangeButton.addActionListener(this);
        resetRangeButton.setActionCommand("ResetRangeButton");
        rangePanel.add(resetRangeButton);
        
        logScaleCheckbox.setBounds(PANEL_OFFSET, 145, 175, 20);
        logScaleCheckbox.addActionListener(this);
        logScaleCheckbox.setActionCommand("LogScaleRange");
        logScaleCheckbox.setFont(MipavUtil.font12);
        rangePanel.add(logScaleCheckbox);

        tabbedPane.addTab("Graph", null, modifyGraphPanel);

    }

    /**
     * Creates a dialog box in which the user can modify various features of the graph.
     */
    private void createModifyDialog() {
        JButton applyButton;
        JButton cancelButton;
        JButton helpButton;

        try {
            modifyDialog = new JDialog(this, "Modify Graph", true);
            tabbedPane = new JTabbedPane();
            applyButton = new JButton("Apply");
            cancelButton = new JButton("Cancel");
            helpButton = new JButton ("Help");
            
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJComponentGraph.createModifyDialog");

            return;
        }

        tabbedPane.setFont(MipavUtil.font12B);

        if (image != null) {
            modifyDialog.setSize(503, 584);
        }
        else {
            modifyDialog.setSize(503, 504);
        }
        modifyDialog.getContentPane().setLayout(null);
        modifyDialog.setResizable(false);

        buildModifyGraphPanel();
        buildFunctionPanel();
        buildFitFunctPanel();

        tabbedPane.setSelectedIndex(0);
        modifyDialog.getContentPane().add(tabbedPane, "Center");
        tabbedPane.validate();
        tabbedPane.addChangeListener(this);
        if (image != null) {
            tabbedPane.setSize(490, 504);
        }
        else {
            tabbedPane.setSize(490, 424);
        }

        applyButton.setFont(MipavUtil.font12B);
        applyButton.addActionListener(this);
        applyButton.setActionCommand("ApplyModifyGraph");
        applyButton.setBounds((modifyDialog.getBounds().width / 2) - 130, modifyDialog.getBounds().height - 70, 90, 30);

        cancelButton.setFont(MipavUtil.font12B);
        cancelButton.addActionListener(this);
        cancelButton.setActionCommand("CancelModifyGraph");
        cancelButton.setBounds(modifyDialog.getBounds().width / 2 - 30, modifyDialog.getBounds().height - 70, 90, 30);
        
        helpButton.setFont(MipavUtil.font12B);
        helpButton.addActionListener(this);
        helpButton.setActionCommand("Help");
        helpButton.setBounds(modifyDialog.getBounds().width / 2 + 70, modifyDialog.getBounds().height - 70, 90, 30);


        modifyDialog.getContentPane().add(applyButton);
        modifyDialog.getContentPane().add(cancelButton);
        modifyDialog.getContentPane().add(helpButton);
        modifyDialog.setLocation(this.getLocation());
        modifyDialog.setModal(false);
        modifyDialog.setVisible(true);

    }

    /**
     * DOCUMENT ME!
     *
     * @param   toSort  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float getMedian(float[] toSort) {
        int length = toSort.length;

        Arrays.sort(toSort);

        return toSort[(length / 2)];
    }

    /**
     * DOCUMENT ME!
     *
     * @param   s    DOCUMENT ME!
     * @param   len  DOCUMENT ME!
     *
     * @return  DOCUMENT ME!
     */
    private float[] parseString(String s, int len) {
        StringTokenizer str;
        float[] array;

        try {
            str = new StringTokenizer(s, "\t", true);
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph.parseString");

            return null;
        }

        if (len == -1) {
            len = (str.countTokens() - (str.countTokens() / 2));
        }

        if (len >= ((ViewJComponentGraph.MAX_NUM_FUNCTS * 4) - 1)) {
            MipavUtil.displayError("Your input file may contain a maximum of five sets of coordinates.");
        }

        try {
            array = new float[len];
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph.open");

            return null;
        }

        for (int i = 0; i < array.length; i++) {

            try {
                array[i] = (Float.valueOf(str.nextToken())).floatValue();
            } catch (NumberFormatException e) {
                array[i] = Float.NaN;
                i++;
                array[i] = Float.NaN;
            }

            if (str.hasMoreTokens()) {
                str.nextToken();
            }
        }

        return array;
    }

    /**
     * Prints the current graph being displayed to the printer.
     */
    private void print() {
        String jobtitle = "Graph";

        PrintJob pjob = getToolkit().getPrintJob(this, jobtitle, null);

        if (pjob != null) {
            Graphics pg = pjob.getGraphics();

            if (pg != null) {
                graph.paintComponentForPrinter(pg);
                pg.dispose(); // flush page
            }

            pjob.end();
        }
    }

    /**
     * Saves this graph as Excel data so that Excel can read in the data and graph it as an Excel file. The file can
     * also be saved to be viewed at a later time with MIPAV
     *
     * @throws  IOException  DOCUMENT ME!
     */
    private void save() throws IOException {
        FileWriter outstream = null;
        JFileChooser chooser;
        String fileName;

        try {

            // else Swing is a chosen preference, and therefore the swing version of the file chooser will be used
            chooser = new JFileChooser();
            chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.PLOT)); // adds a choosable
                                                                                               // filter

            // to only show plot files
            String str = Preferences.getProperty(Preferences.PREF_IMAGE_DIR);
            if(str != null && new File(str) != null) {
            	chooser.setCurrentDirectory(new File(str));
            }else if (defaultDirectory != null) {
                chooser.setCurrentDirectory(new File(defaultDirectory));
            } else {
            	chooser.setCurrentDirectory(new File(System.getProperties().getProperty("user.dir")));
            }

            int returnVal = chooser.showSaveDialog(this);

            if (returnVal == JFileChooser.APPROVE_OPTION) {
                fileName = chooser.getSelectedFile().getName();
                if (!fileName.contains(".")) {
                    fileName = fileName.concat(".plt");
                }
                outstream = new FileWriter(chooser.getCurrentDirectory() + "" + File.separatorChar + "" +
                                           fileName);
            } else {
                return;
            }
        } catch (NullPointerException e) {
            return;
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJComponentGraph.save");

            return;
        }

        String s;
        int len = -1;

        for (int i = 0; i < graph.getFuncts().length; i++) {

            if (len < graph.getFuncts()[i].getXs().length) {
                len = graph.getFuncts()[i].getXs().length;
            }
        }

        for (int i = 0; i < len; i++) {

            for (int j = 0; j < graph.getFuncts().length; j++) { // writes to the file the pairs of x and y coordinates
                s = Float.toString(graph.getFuncts()[j].getXs()[i]);
                outstream.write(s);
                outstream.write('\t');
                s = Float.toString(graph.getFuncts()[j].getYs()[i]);
                outstream.write(s);
                if (j == graph.getFuncts().length - 1) {
                    outstream.write('\n');
                }
                else {
                    outstream.write('\t');
                }
            }
        }

        outstream.close();
    }
    
    private void tableOutput() {
        ViewUserInterface ui = ViewUserInterface.getReference();
        String s;
        int len = -1;

        for (int i = 0; i < graph.getFuncts().length; i++) {

            if (len < graph.getFuncts()[i].getXs().length) {
                len = graph.getFuncts()[i].getXs().length;
            }
        }

        ui.setDataText("\n");
        if(xyCoords != null) {
        	 ui.setDataText("x,y coords \tposition on curve \tintensity" );
        }else {
        	ui.setDataText("position on curve \tintensity" );
        }
        ui.setDataText("\n");
        for (int i = 0; i < len; i++) {

            for (int j = 0; j < graph.getFuncts().length; j++) { // writes to the file the pairs of x and y coordinates
            	/*if(xyCoords != null) {
            		String coords = xyCoords[i][0] + " , " + xyCoords[i][1];
            		ui.setDataText(coords);
                    ui.setDataText("\t");
            	}*/
            	if(graph.getFuncts()[j].getXYCoords() != null) {
	            	s = graph.getFuncts()[j].getXYCoords()[i][0] + " , " + graph.getFuncts()[j].getXYCoords()[i][1];
	            	ui.setDataText(s);
	                ui.setDataText("\t");
            	}
                s = Float.toString(graph.getFuncts()[j].getXs()[i]);
                ui.setDataText(s);
                ui.setDataText("\t");
                s = Float.toString(graph.getFuncts()[j].getYs()[i]);
                ui.setDataText(s);
                if (j == graph.getFuncts().length - 1) {
                    ui.setDataText("\n");
                }
                else {
                    ui.setDataText("\t");
                }
            }
        }
        
    } // private void tableOutput()

    /**
     * Tests that the entered parameter is above maxValue.
     *
     * @param   str       the value entered by the user
     * @param   maxValue  the value that the entered paramater must be more than
     *
     * @return  boolean result of test
     */
    private boolean testMaxParameter(String str, float maxValue) {

        double tmp;

        try {
            new Double(0);
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph.testMaxParameter");

            return false;
        }

        try {
            tmp = Double.valueOf(str).floatValue();

            if (tmp < maxValue) {
                MipavUtil.displayError("Maximum Value must be at least " + maxValue);
                return false;
            } 
            return true;
        } catch (NumberFormatException error) {
            MipavUtil.displayError("Must enter numeric value");

            return false;
        }
    }

    /**
     * Tests that the entered parameter is below minValue.
     *
     * @param   str       the value entered by the user
     * @param   minValue  the value that the variable should be less than
     *
     * @return  boolean result of test
     */
    private boolean testMinParameter(String str, float minValue) {

        double tmp;

        try {
            new Double(0);
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph.testMinParameter");
            return false;
        }

        try {
            tmp = Double.valueOf(str).floatValue();

            if (tmp > minValue) {
                MipavUtil.displayError("Minimum value must be less than or equal to " + minValue);
            	return false;
            }
            return true;
            
        } catch (NumberFormatException error) {
            MipavUtil.displayError("Must enter numeric value");
            return false;
        }
    }

    /**
     * Tests that the entered parameter is in range.
     *
     * @param   str       the value entered by the user
     * @param   minValue  the minimum value this variable may be set to
     * @param   maxValue  the maximum value this variable may be set to
     *
     * @return  boolean result of test
     */
    private boolean testParameter(String str, int minValue, int maxValue) {

        double tmp;
        @SuppressWarnings("unused")
        Double stringConv;

        try {
            stringConv = new Double(0);
        } catch (OutOfMemoryError error) {
            MipavUtil.displayError("Out of memory: ViewJFrameGraph.testParameter");
            return false;
        }

        try {
            tmp = Double.valueOf(str).doubleValue();

            if ((tmp > maxValue) || (tmp < minValue)) {
                Preferences.debug("Value must be between " + minValue + " and " + maxValue+". Current value is "+tmp);
                return false;
            } 
            return true;
        } catch (NumberFormatException error) {
            MipavUtil.displayError("Must enter numeric value");

            return false;
        }
    }

    /**
     * Updates fields of the Modify Graph Dialog which may have changed due to things such as adding/deleting functions.
     */
    private void updateModifyDialog() {

        if (modifyDialog == null) {
            return;
        }
        
        graph.calculateDefaultRangeDomain();

        if (autoShrinkRange) {
            graph.setDefaultRangeDomain();
        } else {
            graph.calculateCustomRange();
        }

        minRangeField.setText("" + graph.getMinRange());
        maxRangeField.setText("" + graph.getMaxRange());
        minRangeLabel.setText("Min. for Range (<" + Float.toString(graph.getDefaultMinRange()) + ")");
        maxRangeLabel.setText("Max. for Range (>" + Float.toString(graph.getDefaultMaxRange()) + ")");

        for (int i = 0; i < 5; i++) {

            if (i < graph.getFuncts().length) {

                // enables the change name field if needed
                if (!nameField[i].isEnabled()) {
                    nameField[i].setText(graph.getFuncts()[i].getFunctName());
                    nameField[i].setEnabled(true);
                    functionLabel[i].setEnabled(true);
                }

                // changes the function linecolor displayed according to the graph
                functLineColorButton[i].setBackground(graph.getFuncts()[i].getColor());
                functVisibleCheckbox[i].setEnabled(true);
                functVisibleCheckbox[i].setSelected(graph.getFuncts()[i].getFunctionVisible());
                functLineColorButton[i].setEnabled(true);

                // sets which fit functions are allowed to be visible
                if (fitMode == FitMode.fitNone) {
                    fitFunctVisibleCheckbox[i].setEnabled(false);
                    fitFunctVisibleCheckbox[i].setSelected(false);
                    graph.getFuncts()[i].setFitFunctionVisible(false);
                } else {
                    fitFunctVisibleCheckbox[i].setEnabled(true);
                    fitFunctVisibleCheckbox[i].setSelected(graph.getFuncts()[i].getFitFunctionVisible());
                }

            } else {

                // changes the change name fields for functions that do not exist
                nameField[i].setText("");
                nameField[i].setEnabled(false);
                functionLabel[i].setEnabled(false);

                // changes the checkbox names for functions that do not exist
                functVisibleCheckbox[i].setEnabled(false);
                functVisibleCheckbox[i].setSelected(false);
                functLineColorButton[i].setEnabled(false);
                functLineColorButton[i].setBackground(Color.lightGray);
                fitFunctVisibleCheckbox[i].setEnabled(false);
                fitFunctVisibleCheckbox[i].setSelected(false);
            }
        }
        
        fitType.setSelectedItem(fitMode);

    }

    //~ Inner Classes --------------------------------------------------------------------------------------------------

    /**
     * This class is called by the color chooser when the user selects a a color from the background color chooser
     * dialog box. It changes the background color.
     */
    class bColorListener implements ActionListener {

        /**
         * Resets the background color based on the user's choice.
         *
         * @param  e  event that called this function
         */
        public void actionPerformed(ActionEvent e) {
            graph.setBackgroundColor(colorChooser.getColor()); // sets the appropriate background color

            // chosen by the user
            backgroundButton.setBackground(colorChooser.getColor());
            itemResetGraph.setEnabled(true); // enables the resetGraph menu item
            update(getGraphics());
        }
    }


    /**
     * This class is called by the color chooser when the user cancels. It disposes of the dialog if the dialog still
     * exists.
     */
    class CancelListener implements ActionListener {

        /**
         * DOCUMENT ME!
         *
         * @param  e  DOCUMENT ME!
         */
        public void actionPerformed(ActionEvent e) {

            if (colorDialog != null) {
                colorDialog.dispose();
            }
        }
    }


    /**
     * This class is called by the color chooser when the user selects a a color from the function line color chooser
     * dialog. It changes the selected line (found by the variable index) to the color returned by the color chooser.
     */
    class ColorListener implements ActionListener {

        /**
         * DOCUMENT ME!
         *
         * @param  e  DOCUMENT ME!
         */
        public void actionPerformed(ActionEvent e) {
            graph.getFuncts()[functionIndex].setColor(colorChooser.getColor()); // sets the appropriate function

            // line color chosen by the user
            itemResetGraph.setEnabled(true); // enables the resetGraph menu item so that

            // the original colors can be restored
            update(getGraphics()); // redraws the graph
            functLineColorButton[functionIndex].setBackground(colorChooser.getColor());

            if (colorDialog != null) {
                colorDialog.dispose();
            }
        }
    }
}
