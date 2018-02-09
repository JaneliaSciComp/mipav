package gov.nih.mipav.view.dialogs;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;

import java.util.*;

import javax.swing.*;


/**
 * Dialog to get user input of counterclockwise ordered polygon points,
 * followed by selecting 4 of these vertices in counterclockwise order to be
 * the corners of the rectangle.  The first 2 vertices must select the long edge of a rectangle.
 * The output xDim and yDim of rectangle created from transformed polygon
 */
public class JDialogPolygonToRectangle extends JDialogBase
        implements AlgorithmInterface, ItemListener, WindowListener {

    //~ Static fields/initializers -------------------------------------------------------------------------------------

    /** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 0L;

    //~ Instance fields ------------------------------------------------------------------------------------------------
    private final int POLYGON_TO_RECTANGLE = 1;
    private final int CROSSRATIO_POLYGON_TO_RECTANGLE = 5;
    private int algorithm = POLYGON_TO_RECTANGLE;

    /** DOCUMENT ME! */
    int[] extents = new int[2];

    /** DOCUMENT ME! */
    private SchwarzChristoffelMapping sAlgo = null;

    /** DOCUMENT ME! */
    private ModelImage image;

    /** DOCUMENT ME! */
    private ModelImage resultImage = null;

    /** DOCUMENT ME! */
    private int xDim;

    /** DOCUMENT ME! */
    private double xSource[];

    /** DOCUMENT ME! */
    private JTextField xText;

    /** DOCUMENT ME! */
    private int yDim;

    /** DOCUMENT ME! */
    private double ySource[];
    
    private int[] corners = new int[4];

    /** DOCUMENT ME! */
    private JTextField yText;
    
    private JTextField v0Text;
    private JTextField v1Text;
    private JTextField v2Text;
    private JTextField v3Text;
    
    private JCheckBox crossRatioCheckBox;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Creates a new JDialogPolygonToRectangle object.
     *
     * @param  image  DOCUMENT ME!
     */
    public JDialogPolygonToRectangle(ModelImage image) {
        super();
        this.image = image;
        parentFrame = image.getParentFrame();
    }

    /**
     * Creates new dialog.
     *
     * @param  theParentFrame  Parent frame
     * @param  im              Source image
     */
    public JDialogPolygonToRectangle(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        image = im;
        init();
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    /**
     * Closes dialog box when the OK button is pressed and calls the algorithm.
     * 
     * @param event Event that triggers function.
     */
    public void actionPerformed(ActionEvent event) {
        String command = event.getActionCommand();

        if (command.equals("OK")) {

            if (setVariables()) {
                callAlgorithm();
            }
        } else if (command.equals("Script")) {
            callAlgorithm();
        } else if (command.equals("Help")) {
            // MipavUtil.showHelp("CMSR001");
            MipavUtil.showWebHelp("Transform:_Conformal_Mapping_Algorithms#Applying_the_Polygon_to_Rectangle_algorithm");
        } else if (command.equals("Cancel")) {
            dispose();
        } else {
            super.actionPerformed(event);
        }
    }


    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************

    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {


        if (algorithm instanceof SchwarzChristoffelMapping) {
            Preferences.debug("Polygon To Rectangle: " + algorithm.getElapsedTime());
            image.clearMask();

            if ((sAlgo.isCompleted() == true) && (resultImage != null)) {


                resultImage.clearMask();

                try {
                    openNewFrame(resultImage);
                } catch (OutOfMemoryError error) {
                    System.gc();
                    MipavUtil.displayError("Out of memory: unable to open new frame");
                }
            } else if (resultImage != null) {

                // algorithm failed but result image still has garbage
                resultImage.disposeLocal(); // clean up memory
                resultImage = null;
                System.gc();

            }

            // insertScriptLine(algorithm);
        } // if (algorithm instanceof SchwarzChristoffelMapping)

        if (sAlgo != null) {
            sAlgo.finalize();
            sAlgo = null;
        }

        dispose();
    }

    // ************************* Item Events ****************************
    // *******************************************************************

    /**
     * itemStateChanged.
     *
     * @param  event  DOCUMENT ME!
     */
    public void itemStateChanged(ItemEvent event) {
    }


    /**
     * Disposes of error dialog, then frame. Sets cancelled to <code>true</code>.
     *
     * @param  event  DOCUMENT ME!
     */
    public void windowClosing(WindowEvent event) {
        cancelFlag = true;
        dispose();
    }

    /**
     * DOCUMENT ME!
     */
    private void callAlgorithm() {

        try {
            String name = makeImageName(image.getImageName(), "_rectangle");
            extents[0] = xDim;
            extents[1] = yDim;
            resultImage = new ModelImage(image.getType(), extents, name);
            resultImage.setImageName(name);

            // Make algorithm
            sAlgo = new SchwarzChristoffelMapping(resultImage, image, xSource, ySource, corners, algorithm);

            // This is very important. Adding this object as a listener allows the algorithm to
            // notify this object when it has completed of failed. See algorithm performed event.
            // This is made possible by implementing AlgorithmedPerformed interface
            sAlgo.addListener(this);

            // Hide dialog
            setVisible(false);

            if (isRunInSeparateThread()) {

                // Start the thread as a low priority because we wish to still have user interface work fast.
                if (sAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
                    MipavUtil.displayError("A thread is already running on this object");
                }
            } else {

                sAlgo.run();
            }
        } catch (OutOfMemoryError x) {

            System.gc();
            MipavUtil.displayError("Dialog Polygon To Rectangle: unable to allocate enough memory");

            return;
        }
    }


    /**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        JLabel point1Label;
        JLabel point2Label;
        JLabel point3Label;
        JLabel xLabel;
        JLabel yLabel;
        JLabel v0Label;
        JLabel v1Label;
        JLabel v2Label;
        JLabel v3Label;
        setForeground(Color.black);
        setTitle("Polygon To Rectangle");

        JPanel pointPanel = new JPanel(new GridBagLayout());
        pointPanel.setBorder(buildTitledBorder("Select Points"));

        GridBagConstraints gbc4 = new GridBagConstraints();
        gbc4.gridwidth = 1;
        gbc4.gridheight = 1;
        gbc4.anchor = GridBagConstraints.WEST;
        gbc4.weightx = 1;
        gbc4.insets = new Insets(3, 3, 3, 3);
        gbc4.fill = GridBagConstraints.HORIZONTAL;
        gbc4.gridx = 0;
        gbc4.gridy = 0;
        
        // Clockwise order for y increasing going down.
        // Counterclockwise order for y increasing going up.
        // Invert y axis before processing to use ccw order.

        point1Label = new JLabel("Enter 4 or more polygon points in a counterclockwise path");
        point1Label.setForeground(Color.black);
        point1Label.setFont(serif12);
        pointPanel.add(point1Label, gbc4);

        point2Label = new JLabel("Enter the 4 polygon vertex numbers that will be at corners of the rectangle");
        point2Label.setForeground(Color.black);
        point2Label.setFont(serif12);
        gbc4.gridy = 1;
        pointPanel.add(point2Label, gbc4);

        point3Label = new JLabel("Go in counterclockwise order and select along a long rectangle edge first");
        point3Label.setForeground(Color.black);
        point3Label.setFont(serif12);
        gbc4.gridy = 2;
        pointPanel.add(point3Label, gbc4);
        
        v0Label = new JLabel("Index of vertex 0 of output rectangle");
        v0Label.setForeground(Color.black);
        v0Label.setFont(serif12);
        gbc4.gridy = 3;
        pointPanel.add(v0Label, gbc4);
        
        v0Text = new JTextField(10);
        v0Text.setFont(serif12);
        v0Text.setEnabled(true);
        gbc4.gridx = 1;
        pointPanel.add(v0Text, gbc4);
        
        v1Label = new JLabel("Index of vertex 1 of output rectangle");
        v1Label.setForeground(Color.black);
        v1Label.setFont(serif12);
        gbc4.gridx = 0;
        gbc4.gridy = 4;
        pointPanel.add(v1Label, gbc4);
        
        v1Text = new JTextField(10);
        v1Text.setFont(serif12);
        v1Text.setEnabled(true);
        gbc4.gridx = 1;
        pointPanel.add(v1Text, gbc4);
        
        v2Label = new JLabel("Index of vertex 2 of output rectangle");
        v2Label.setForeground(Color.black);
        v2Label.setFont(serif12);
        gbc4.gridx = 0;
        gbc4.gridy = 5;
        pointPanel.add(v2Label, gbc4);
        
        v2Text = new JTextField(10);
        v2Text.setFont(serif12);
        v2Text.setEnabled(true);
        gbc4.gridx = 1;
        pointPanel.add(v2Text, gbc4);
        
        v3Label = new JLabel("Index of vertex 3 of output rectangle");
        v3Label.setForeground(Color.black);
        v3Label.setFont(serif12);
        gbc4.gridx = 0;
        gbc4.gridy = 6;
        pointPanel.add(v3Label, gbc4);
        
        v3Text = new JTextField(10);
        v3Text.setFont(serif12);
        v3Text.setEnabled(true);
        gbc4.gridx = 1;
        pointPanel.add(v3Text, gbc4);

        JPanel paramPanel = new JPanel(new GridBagLayout());
        paramPanel.setForeground(Color.black);
        paramPanel.setBorder(buildTitledBorder("Output dimensions"));

        GridBagConstraints gbc6 = new GridBagConstraints();

        gbc6.gridwidth = 1;
        gbc6.gridheight = 1;
        gbc6.anchor = GridBagConstraints.WEST;
        gbc6.weightx = 1;
        gbc6.insets = new Insets(3, 3, 3, 3);
        gbc6.fill = GridBagConstraints.HORIZONTAL;
        gbc6.gridx = 0;
        gbc6.gridy = 0;
        
        crossRatioCheckBox = new JCheckBox("Use cross-ratio representation");
        crossRatioCheckBox.setFont(serif12);
        crossRatioCheckBox.setForeground(Color.black);
        crossRatioCheckBox.setSelected(false);
        paramPanel.add(crossRatioCheckBox, gbc6);

        xLabel = new JLabel("X dimension of output image ");
        xLabel.setForeground(Color.black);
        xLabel.setFont(serif12);
        xLabel.setEnabled(true);
        gbc6.gridy = 1;
        paramPanel.add(xLabel, gbc6);

        xText = new JTextField(10);
        xText.setText(String.valueOf(image.getExtents()[0]));
        xText.setFont(serif12);
        xText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(xText, gbc6);

        yLabel = new JLabel("Y dimension of output image ");
        yLabel.setForeground(Color.black);
        yLabel.setFont(serif12);
        yLabel.setEnabled(true);
        gbc6.gridx = 0;
        gbc6.gridy = 2;
        paramPanel.add(yLabel, gbc6);

        yText = new JTextField(10);
        yText.setText(String.valueOf(image.getExtents()[1]));
        yText.setFont(serif12);
        yText.setEnabled(true);
        gbc6.gridx = 1;
        paramPanel.add(yText, gbc6);

        getContentPane().add(pointPanel, BorderLayout.NORTH);
        getContentPane().add(paramPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }

    /**
     * Use the GUI results to set up the variables needed to run the algorithm.
     *
     * @return  <code>true</code> if parameters set successfully, <code>false</code> otherwise.
     */
    private boolean setVariables() {
        int i, j;
        Vector<VOIBase> curves;
        int nPts;
        Vector3f[] pts = null;
        
        if (crossRatioCheckBox.isSelected()) {
        	algorithm = CROSSRATIO_POLYGON_TO_RECTANGLE;
        }
        else {
        	algorithm = POLYGON_TO_RECTANGLE;
        }

        if (!testParameter(xText.getText(), 5, 1000000)) {
            xText.requestFocus();
            xText.selectAll();

            return false;
        } else {
            xDim = Integer.valueOf(xText.getText()).intValue();
        }

        if (!testParameter(yText.getText(), 5, 1000000)) {
            yText.requestFocus();
            yText.selectAll();

            return false;
        } else {
            yDim = Integer.valueOf(yText.getText()).intValue();
        }

        if ((image.getVOIs() == null) || (image.getVOIs().size() == 0)) {
            MipavUtil.displayError("At least points must be entered");
            return false;
        }
        curves = image.getVOIs().VOIAt(0).getCurves();
        nPts = curves.size();

        if (nPts < 4) {
            MipavUtil.displayError("Number of points = " + nPts + " less than required 4");

            return false;
        }

        pts = image.getVOIs().VOIAt(0).exportAllPoints();
        xSource = new double[pts.length];
        ySource = new double[pts.length];

        for (i = 0; i < pts.length; i++) {
            xSource[i] = pts[i].X;
            ySource[i] = pts[i].Y;
        }
        
        if (!testParameter(v0Text.getText(), 0, pts.length-1)) {
        	v0Text.requestFocus();
        	v0Text.selectAll();
        	MipavUtil.displayError("Index of vertex 0 must be >= 0 and <= " + (pts.length-1));
        	return false;
        }
        else {
        	corners[0] = Integer.valueOf(v0Text.getText()).intValue();
        }
        
        if (!testParameter(v1Text.getText(), 0, pts.length-1)) {
        	v1Text.requestFocus();
        	v1Text.selectAll();
        	MipavUtil.displayError("Index of vertex 1 must be >= 0 and <= " + (pts.length-1));
        	return false;
        }
        else {
        	corners[1] = Integer.valueOf(v1Text.getText()).intValue();
        }
        
        if (!testParameter(v2Text.getText(), 0, pts.length-1)) {
        	v2Text.requestFocus();
        	v2Text.selectAll();
        	MipavUtil.displayError("Index of vertex 2 must be >= 0 and <= " + (pts.length-1));
        	return false;
        }
        else {
        	corners[2] = Integer.valueOf(v2Text.getText()).intValue();
        }
        
        if (!testParameter(v3Text.getText(), 0, pts.length-1)) {
        	v3Text.requestFocus();
        	v3Text.selectAll();
        	MipavUtil.displayError("Index of vertex 3 must be >= 0 and <= " + (pts.length-1));
        	return false;
        }
        else {
        	corners[3] = Integer.valueOf(v3Text.getText()).intValue();
        }
        
        for (i = 0; i < 3; i++) {
        	for (j = i+1; j < 4; j++) {
        		if (corners[i] == corners[j]) {
        			MipavUtil.displayError("Index of vertex " + i + " and index of vertex " + j + " both equal " + corners[i]);
        			return false;
        		}
        	}
        }

        return true;
    }
}
