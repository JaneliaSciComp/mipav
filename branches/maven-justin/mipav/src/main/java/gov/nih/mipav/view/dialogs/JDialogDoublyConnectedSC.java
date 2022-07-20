package gov.nih.mipav.view.dialogs;


import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.structures.*;
import gov.nih.mipav.view.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

import javax.swing.*;

import WildMagic.LibFoundation.Mathematics.Vector3f;


/**
 * Dialog to get Schwarz-Christoffel mapping of region between 2 polygons to an annulus
 */
public class JDialogDoublyConnectedSC extends JDialogBase implements AlgorithmInterface, ItemListener, WindowListener {
	
	/** Use serialVersionUID for interoperability. */
    private static final long serialVersionUID = 0L;

	
	 private ButtonGroup VOIGroup;
	 
	 private JRadioButton outerButton;
	 
	 private JRadioButton innerButton;
	 
	 private ViewJComponentEditImage componentImage;
	 
	 private ModelImage image;
	 
	 private ModelImage resultImage = null;
	 
	 private int[] extents = new int[2];
	 
	 private int xDim;
	 
	 private DoublyConnectedSC dcscAlgo;
	 
	 private JTextField xText;
	 
	 private ViewVOIVector VOIs;
	 
	// M[0] points of outer polygon
	private double Z0[][] = null;
	// N[0] points of inner polygon
	private double Z1[][] = null;
	
	// The number of Gauss-Jacobi points
	// Recommended values for NPTQ are 2-8.
	private int NPTQ = 8;
	
	private JTextField nptqText;
	
	private JCheckBox displayCheckBox;
	// Screen display of the residual of the system as the iteration goes on, 1 for "YES", 2 for "NO"
    private int ISPRT;
    
    //IGUESS    (=0 )A NON-EQUALLY SPACED INITIAL GUESS OR(=1)THE
  	//C            OTHER  EQUALLY-SPACED  INITIAL GUESS PROVIDED, OR
  	//C            (=2)USER-SUPPLIED INITIAL GUESS WHICH IS REQUIRED
  	//C            TO BE THE ARGUMENTS OF THE INITIAL PREVERTICES.
  	//C            ROUTINE ARGUM MAY BE USED FOR COMPUTING ARGUMENTS
  	//C            IF NEEDED. NOTE: C WILL BE COMPUTED IN THE ROUTINE
  	//C            (NOT SUPPLIED!)
    private int IGUESS = 1;
    
    private ButtonGroup guessGroup;
    private JRadioButton nonequalButton;
    private JRadioButton equalButton;
    private JRadioButton preverticesButton;
    // LINEARC   INTEGER VARIABLE TO CONTROL INTEGRATION PATH.IN PATICULAR:
    //C            LINEARC=0:INTEGRATING ALONG LINE SEGMENT WHENEVER POSSIBLE
    //C            LINEARC=1:INTEGRATING ALONG CIRCULAR ARC WHENEVER POSSIBLE
    private int LINEARC = 1;
    private ButtonGroup integrationGroup;
    private JRadioButton lineButton;
    private JRadioButton circularButton;
    // TOL       A TOLERANCE TO CONTROL THE CONVERGENCE IN HYBRD
    private double TOL = 1.0E-10;
    private JTextField tolText;
	 
	//~ Constructors ---------------------------------------------------------------------------------------------------

	    /**
	     * Creates a new JDialogFRAP object.
	     *
	     * @param  image  DOCUMENT ME!
	     */
	    public JDialogDoublyConnectedSC(ModelImage image) {
	        super();
	        this.image = image;
	        parentFrame = image.getParentFrame();
	        componentImage = ((ViewJFrameImage) parentFrame).getComponentImage();
	    }

	    /**
	     * Creates new dialog.
	     *
	     * @param  theParentFrame  Parent frame
	     * @param  im              Source image
	     */
	    public JDialogDoublyConnectedSC(Frame theParentFrame, ModelImage im) {
	        super(theParentFrame, false);
	        image = im;
	        componentImage = ((ViewJFrameImage) theParentFrame).getComponentImage();
	        init();
	    }
	    
	    /**
	     * Closes dialog box when the OK button is pressed and calls the algorithm.
	     *
	     * @param  event  Event that triggers function.
	     */
	    public void actionPerformed(ActionEvent event) {
	        String command = event.getActionCommand();
	        Object source = event.getSource();

	        if (command.equals("OK")) {

	            if (setVariables()) {
	                callAlgorithm();
	            }
	        } else if (command.equals("Script")) {
	            callAlgorithm();
	        } else if (command.equals("Help")) {
	            //MipavUtil.showWebHelp("DoublyConnectedSC");
	        } else if (command.equals("Cancel")) {
	            componentImage.getVOIHandler().setPresetHue(-1.0f);
	            dispose();
	        } else if ((source == outerButton) || (source == innerButton)) {

	            if (outerButton.isSelected()) {
	                componentImage.getVOIHandler().newVOI(0.0f); // red
	                //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
	                //componentImage.getVOIHandler().setPresetHue(0.0f); // red
	            } else if (innerButton.isSelected()) {
	                componentImage.getVOIHandler().newVOI(1.0f / 3.0f); // green
	                //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
	                //componentImage.getVOIHandler().setPresetHue(1.0f / 3.0f); // green
	            }
	        } else {
	            super.actionPerformed(event);
	        }
	    }
	    
	    /**
	     * DOCUMENT ME!
	     */
	    private void callAlgorithm() {

	        try {
	            String name = makeImageName(image.getImageName(), "_annulus");
	            extents[0] = xDim;
	            extents[1] = xDim;
	            resultImage = new ModelImage(image.getType(), extents, name);
	            resultImage.setImageName(name);

	            // Make algorithm
	            dcscAlgo = new DoublyConnectedSC(resultImage, image, Z0, Z1, NPTQ, ISPRT, IGUESS, LINEARC, TOL);

	            // This is very important. Adding this object as a listener allows the algorithm to
	            // notify this object when it has completed of failed. See algorithm performed event.
	            // This is made possible by implementing AlgorithmedPerformed interface
	            dcscAlgo.addListener(this);

	            // Hide dialog
	            setVisible(false);

	            if (isRunInSeparateThread()) {

	                // Start the thread as a low priority because we wish to still have user interface work fast.
	                if (dcscAlgo.startMethod(Thread.MIN_PRIORITY) == false) {
	                    MipavUtil.displayError("A thread is already running on this object");
	                }
	            } else {

	                dcscAlgo.run();
	            }
	        } catch (OutOfMemoryError x) {

	            System.gc();
	            MipavUtil.displayError("Dialog Region between 2 polygons to annulus: unable to allocate enough memory");

	            return;
	        }
	    }
	    
	    /**
	     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithm when it
	     * has completed or failed to to complete, so that the dialog can be display the result image and/or clean up.
	     *
	     * @param  algorithm  Algorithm that caused the event.
	     */
	    public void algorithmPerformed(AlgorithmBase algorithm) {


	        if (algorithm instanceof DoublyConnectedSC) {
	            Preferences.debug("Region between 2 polygons to annulus: " + algorithm.getElapsedTime());
	            image.clearMask();

	            if ((dcscAlgo.isCompleted() == true) && (resultImage != null)) {


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
	        } // if (algorithm instanceof DoublyConnectedSC)

	        if (dcscAlgo != null) {
	            dcscAlgo.finalize();
	            dcscAlgo = null;
	        }

	        dispose();
	    }
	
	/**
     * Initializes GUI components and displays dialog.
     */
    private void init() {
        JPanel paramPanel;
        JPanel VOIPanel;

        setForeground(Color.black);
        setTitle("Transform region between 2 polygons to an annulus");

        VOIPanel = new JPanel(new GridBagLayout());
        VOIPanel.setBorder(buildTitledBorder("Select outer or inner points"));

        GridBagConstraints gbc = new GridBagConstraints();
        gbc.gridwidth = 1;
        gbc.gridheight = 1;
        gbc.anchor = GridBagConstraints.WEST;
        gbc.weightx = 1;
        gbc.insets = new Insets(3, 3, 3, 3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.gridx = 0;
        gbc.gridy = 0;
        
        JLabel requireLabel = new JLabel("Points must be entered in counterclockwise order");
        requireLabel.setForeground(Color.black);
        requireLabel.setFont(serif12);
        VOIPanel.add(requireLabel, gbc);
        
        JLabel requireLabel2 = new JLabel("The last inner vertex must be the closest (or almost closest) inner vertex to the last outer vertex");
        requireLabel2.setForeground(Color.black);
        requireLabel2.setFont(serif12);
        gbc.gridy++;
        VOIPanel.add(requireLabel2, gbc);

        VOIGroup = new ButtonGroup();

        outerButton = new JRadioButton("Add required outer VOI points", true);
        outerButton.setForeground(Color.red);
        outerButton.setFont(serif12);
        outerButton.addActionListener(this);
        VOIGroup.add(outerButton);
        gbc.gridy++;
        VOIPanel.add(outerButton, gbc);
        componentImage.getVOIHandler().newVOI(0.0f); // red
        //componentImage.setCursorMode(ViewJComponentEditImage.NEW_VOI);
        //componentImage.getVOIHandler().setPresetHue(0.0f); // red

        innerButton = new JRadioButton("Add required inner VOI points", false);
        innerButton.setForeground(Color.green.darker());
        innerButton.setFont(serif12);
        innerButton.addActionListener(this);
        VOIGroup.add(innerButton);
        gbc.gridy++;
        VOIPanel.add(innerButton, gbc);
        
        paramPanel = new JPanel(new GridBagLayout());

        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.gridwidth = 1;
        gbc2.gridheight = 1;
        gbc2.anchor = GridBagConstraints.WEST;
        gbc2.weightx = 1;
        gbc2.insets = new Insets(3, 3, 3, 3);
        gbc2.fill = GridBagConstraints.HORIZONTAL;
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        
        JLabel xLabel = new JLabel("X dimension = y dimension of output image ");
        xLabel.setForeground(Color.black);
        xLabel.setFont(serif12);
        xLabel.setEnabled(true);
        gbc2.gridx = 0;
        gbc2.gridy++;
        paramPanel.add(xLabel, gbc2);

        xText = new JTextField(10);
        xText.setText(String.valueOf(image.getExtents()[0]));
        xText.setFont(serif12);
        xText.setEnabled(true);
        gbc2.gridx = 1;
        paramPanel.add(xText, gbc2);
        
        JLabel nptqLabel = new JLabel("Number of Gauss-Jacobi points (recommended 2-8):");
        nptqLabel.setForeground(Color.black);
        nptqLabel.setFont(serif12);
        paramPanel.add(nptqLabel, gbc2);
        
        nptqText = new JTextField("8");
        nptqText.setFont(serif12);
        nptqText.setForeground(Color.black);
        gbc2.gridx = 1;
        paramPanel.add(nptqText, gbc2);
        
        displayCheckBox = new JCheckBox("Display system residuals during iterations");
        displayCheckBox.setFont(serif12);
        displayCheckBox.setForeground(Color.black);
        displayCheckBox.setSelected(false);
        gbc2.gridx = 0;
        gbc2.gridy++;
        paramPanel.add(displayCheckBox, gbc2);
        
        guessGroup = new ButtonGroup();
        
        nonequalButton = new JRadioButton("Non-equally spaced initial guess", false);
        nonequalButton.setForeground(Color.black);
        nonequalButton.setFont(serif12);
        guessGroup.add(nonequalButton);
        gbc2.gridy++;
        paramPanel.add(nonequalButton, gbc2);
        
        equalButton = new JRadioButton("Equally spaced initial guess", true);
        equalButton.setForeground(Color.black);
        equalButton.setFont(serif12);
        guessGroup.add(equalButton);
        gbc2.gridy++;
        paramPanel.add(equalButton, gbc2);
        
        preverticesButton = new JRadioButton("Arguments of initial prevertices initial guess", false);
        preverticesButton.setForeground(Color.black);
        preverticesButton.setFont(serif12);
        guessGroup.add(preverticesButton);
        gbc2.gridy++;
        paramPanel.add(preverticesButton, gbc2);
        
        integrationGroup = new ButtonGroup();
        lineButton = new JRadioButton("Integrating along line segment whenever possible", false);
        lineButton.setForeground(Color.black);
        lineButton.setFont(serif12);
        integrationGroup.add(lineButton);
        gbc2.gridy++;
        paramPanel.add(lineButton, gbc2);
        
        circularButton = new JRadioButton("Integrating along line segment whenever possible", true);
        circularButton.setForeground(Color.black);
        circularButton.setFont(serif12);
        integrationGroup.add(circularButton);
        gbc2.gridy++;
        paramPanel.add(circularButton, gbc2);
        
        JLabel tolLabel = new JLabel("A tolerance to control the convergence in HYBRD:");
        tolLabel.setForeground(Color.black);
        tolLabel.setFont(serif12);
        gbc2.gridy++;
        paramPanel.add(tolLabel, gbc2);
        
        tolText = new JTextField("1.0E-10");
        tolText.setFont(serif12);
        tolText.setForeground(Color.black);
        gbc2.gridx = 1;
        paramPanel.add(tolText, gbc2);
        
        getContentPane().add(VOIPanel, BorderLayout.NORTH);
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
        int nVOIs;
        float[] hsb;
        float hue;
        String tmpStr;
        VOIs = image.getVOIs();
        nVOIs = VOIs.size();
        VOI presentVOI;
        Vector<VOIBase> curves;
        int nPtsOuter = 0;
        int nPtsInner = 0;
        Vector3f[] tmpPts = null;
        int numInner;
        int numOuter;

        for (i = 0; i < nVOIs; i++) {
        	presentVOI = image.getVOIs().VOIAt(i);
            if (presentVOI.getCurveType() == VOI.POINT) {
            	curves = presentVOI.getCurves();
                hsb = Color.RGBtoHSB(presentVOI.getColor().getRed(), presentVOI.getColor().getGreen(),
                                     presentVOI.getColor().getBlue(), null);
                hue = hsb[0];

                if ((Math.abs(hue - 0.0f)) < 0.0001f) {
                    nPtsOuter += curves.size();
                } else if ((Math.abs(hue - (1.0f / 3.0f))) < 0.0001f) {
                    nPtsInner += curves.size();
                    
                } else {
                    MipavUtil.displayError("VOI hue = " + hue + " Must be 0 for red or " +
                                           "1/3 for green");

                    return false;
                }
            }
        } // for (i = 0; i < nVOIs; i++)
        
        if (nPtsOuter < 3) {
            MipavUtil.displayError("At least 3 outer points required");
            return false;
        }
        
        if (nPtsInner == 0) {
        	MipavUtil.displayError("No inner points found");
        	return false;
        }
        
        Z0 = new double[nPtsOuter][2];
        Z1 = new double[nPtsInner][2];
        
        numInner = 0;
        numOuter = 0;
        for (i = 0; i < nVOIs; i++) {
        	presentVOI = image.getVOIs().VOIAt(i);
            if (presentVOI.getCurveType() == VOI.POINT) {
            	tmpPts = presentVOI.exportAllPoints();
                hsb = Color.RGBtoHSB(presentVOI.getColor().getRed(), presentVOI.getColor().getGreen(),
                                     presentVOI.getColor().getBlue(), null);
                hue = hsb[0];

                if ((Math.abs(hue - 0.0f)) < 0.0001f) {
                	// Invert so y axis increases going up so can use ccw ordering
                	for (j = 0; j < tmpPts.length; j++) {
                	    Z0[numOuter][0] = tmpPts[j].X;
                	    Z0[numOuter++][1] = image.getExtents()[1] - 1 - tmpPts[j].Y;
                	}
                } else if ((Math.abs(hue - (1.0f / 3.0f))) < 0.0001f) {
                	// Invert so y axis increases going up so can use ccw ordering
                	for (j = 0; j < tmpPts.length; j++) {
                	    Z1[numInner][0] = tmpPts[j].X;
                	    Z1[numInner++][1] = image.getExtents()[1] - 1 - tmpPts[j].Y;
                	}   
                    
                } else {
                    MipavUtil.displayError("VOI hue = " + hue + " Must be 0 for red or " +
                                           "1/3 for green");

                    return false;
                }
            }
        } // for (i = 0; i < nVOIs; i++)
        
        if (!testParameter(xText.getText(), 5, 1000000)) {
            xText.requestFocus();
            xText.selectAll();

            return false;
        } else {
            xDim = Integer.valueOf(xText.getText()).intValue();
        }
        
        tmpStr = nptqText.getText();
    	
        if (testParameter(tmpStr, 2, 20)) {
            NPTQ = Integer.valueOf(tmpStr).intValue();
        } else {
            nptqText.requestFocus();
            nptqText.selectAll();

            return false;
        }
        
        if (displayCheckBox.isSelected()) {
            ISPRT = 1;	
        }
        else {
        	ISPRT = 2;
        }
        
        if (nonequalButton.isSelected()) {
        	IGUESS = 0;
        }
        else if (equalButton.isSelected()) {
        	IGUESS = 1;
        }
        else {
        	IGUESS = 2;
        }
        
        if (lineButton.isSelected()) {
        	LINEARC = 0;
        }
        else {
        	LINEARC = 1;
        }
        
        tmpStr = tolText.getText();
        
        if (testParameter(tmpStr, 1.0E-16, 1.0E-3)) {
            TOL = Double.valueOf(tmpStr).doubleValue();
        } else {
            tolText.requestFocus();
            tolText.selectAll();

            return false;
        }

        return true;
    }
}