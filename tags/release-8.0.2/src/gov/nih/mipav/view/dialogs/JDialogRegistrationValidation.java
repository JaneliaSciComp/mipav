package gov.nih.mipav.view.dialogs;

import gov.nih.mipav.model.algorithms.*;
import gov.nih.mipav.model.algorithms.registration.*;
import gov.nih.mipav.model.structures.*;

import gov.nih.mipav.view.*;

import java.awt.*;

import javax.swing.*;


/**
 * Dialog to get user input, then call algorithmRegValidation. Selects image is match image, the image that gets transformed
 * until it is registered to the base image. Algorithms are executed in their own thread.
 * 
 * @author   senseneyj
 */
public class JDialogRegistrationValidation extends JDialogRegistrationLeastSquares implements AlgorithmInterface {

    //~ Static fields/initializers -------------------------------------------------------------------------------------


    //~ Instance fields ------------------------------------------------------------------------------------------------

	/** Whether the available cost functions should be performed */
    private JCheckBox doCorrelationRatioSmoothed, doMutualInfoSmoothed,
    					doNormMutualInfoSmoothed, doNormXCorr;

    /**Description of the cost function being performed*/
	private String currentCostFunct;

	/**Algorithm that implement cost functions, either a AlgorithmCostFunctions or AlgorithmCostFunctions2D*/
	private AlgorithmOptimizeFunctionBase algoCost;

	/**Initial transmatrix for cost functions*/
	private TransMatrix tMatrix;

	/**SimpleImages to describe ModelImage data, simpleImg2 is always registered image*/
	private ModelSimpleImage simpleImg1, simpleImg2;

	/** Bin to use for pixel comparisons*/
	private int bin1;

    //~ Constructors ---------------------------------------------------------------------------------------------------

    /**
     * Empty constructor needed for dynamic instantiation (used during scripting).
     */
    public JDialogRegistrationValidation() { }

    /**
     * Creates new registration dialog to get base image name.
     *
     * @param  theParentFrame  Parent frame.
     * @param  im              Source image.
     */
    public JDialogRegistrationValidation(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, im);
    }

    /**
     * Creates a new JDialogRegistrationValidation object, used for bypassing the GUI.
     *
     * @param  theParentFrame  Parent frame.
     * @param  _mi             Source image.
     * @param  _ri             Image to register against.
     */
    public JDialogRegistrationValidation(Frame theParentFrame, ModelImage _mi, ModelImage _ri) {
    	super(theParentFrame, _mi, _ri);
    }

    //~ Methods --------------------------------------------------------------------------------------------------------

    protected void init() {
    	if (matchImage.getNDims() > 2) {
            tMatrix = new TransMatrix(4);
        } else {
            tMatrix = new TransMatrix(3);

        }
    	
    	setForeground(Color.black);
        setTitle("Least Squares with Validation");

        JPanel topPanel = new JPanel(new BorderLayout());
        topPanel.setBorder(MipavUtil.buildTitledBorder("Registration options"));
        JPanel imagePanel = buildImagePanel();
        JPanel extentsPanel = buildExtentsPanel();
        topPanel.add(imagePanel, BorderLayout.NORTH);
        topPanel.add(extentsPanel, BorderLayout.CENTER);
        
        /**set up available costs panel*/
        JPanel availableCostsPanel = new JPanel();
        availableCostsPanel.setLayout(new BoxLayout(availableCostsPanel, BoxLayout.Y_AXIS));
        availableCostsPanel.setBorder(MipavUtil.buildTitledBorder("Available cost functions"));
        
        doCorrelationRatioSmoothed = new JCheckBox("Calculate correlation ratio");
        doCorrelationRatioSmoothed.setSelected(true);
        doCorrelationRatioSmoothed.setFont(serif12);
        doMutualInfoSmoothed = new JCheckBox("Calculate mutual information");
        doMutualInfoSmoothed.setSelected(true);
        doMutualInfoSmoothed.setFont(serif12);
        doNormMutualInfoSmoothed = new JCheckBox("Calculate normalized mutual information");
        doNormMutualInfoSmoothed.setSelected(true);
        doNormMutualInfoSmoothed.setFont(serif12);
        doNormXCorr = new JCheckBox("Calculate normalized cross correlation");
        doNormXCorr.setSelected(true);
        doNormXCorr.setFont(serif12);
        
        availableCostsPanel.add(doCorrelationRatioSmoothed);
        availableCostsPanel.add(doMutualInfoSmoothed);
        availableCostsPanel.add(doNormMutualInfoSmoothed);
        availableCostsPanel.add(doNormXCorr);
        
        //note rescale panel should not be needed since image was directly registered
        
        getContentPane().add(topPanel, BorderLayout.NORTH);
        getContentPane().add(availableCostsPanel, BorderLayout.CENTER);
        getContentPane().add(buildButtons(), BorderLayout.SOUTH);

        pack();
        setVisible(true);
    }
    
    // ************************************************************************
    // ************************** Algorithm Events ****************************
    // ************************************************************************
    
    /**
     * This method is required if the AlgorithmPerformed interface is implemented. It is called by the algorithms when
     * it has completed or failed to to complete.
     *
     * @param  algorithm  Algorithm that caused the event.
     */
    public void algorithmPerformed(AlgorithmBase algorithm) {
    	super.algorithmPerformed(algorithm);
    	if (algorithm instanceof AlgorithmRegLeastSquares) {
    		if (setCostVariables()) {
                dispose();

                progressBar = new ViewJProgressBar("Calculating costs", " ", 0, 100, false, this, this);

                int xScreen = Toolkit.getDefaultToolkit().getScreenSize().width;
                int yScreen = 100; // Toolkit.getDefaultToolkit().getScreenSize().height;
                progressBar.setLocation(xScreen / 2, yScreen / 2);
                progressBar.setVisible(true);

                //match image is the active image when the plugin was started
                runCostFunctions(matchImage);
                
                //base image is the image it was being registered against
                runCostFunctions(baseImage);

                progressBar.updateValue(100, true);
                MipavUtil.displayInfo("Calculations are complete.  " +
                                      "Select the data tab in the output window to see results.");
                progressBar.dispose();
    		}
        } 
    }
    
    /**
     * calls the various cost functions
     * @param image the image to compare to the registered image
     */
    private void runCostFunctions(ModelImage image) {
    	/* Initialize the number of bins */
        bin1 = 256;
        double possibleIntValues1 = image.getMax() - image.getMin() + 1;

        ViewUserInterface.getReference().setDataText("Performing calculations between image "+
        												image.getImageName()+" and "+resultImage.getImageName()+"\n");
        
        if (((image.getType() == ModelStorageBase.BYTE) || (image.getType() == ModelStorageBase.UBYTE) ||
                 (image.getType() == ModelStorageBase.SHORT) ||
                 (image.getType() == ModelStorageBase.USHORT) ||
                 (image.getType() == ModelStorageBase.INTEGER) ||
                 (image.getType() == ModelStorageBase.UINTEGER) ||
                 (image.getType() == ModelStorageBase.LONG)) && (possibleIntValues1 < 256)) {
            bin1 = (int) Math.round(possibleIntValues1);
        }
    	
    	simpleImg1 = new ModelSimpleImage(image.getExtents(), image.getFileInfo(0).getResolutions(),
                image);
    	
    	if(doCorrelationRatioSmoothed.isSelected()) {
        	progressBar.setMessage("Calculating correlation ratio");
            currentCostFunct = "Correlation Ratio Smoothed";
            if (baseImage.getNDims() > 2) {
                callAlgorithm(AlgorithmCostFunctions.CORRELATION_RATIO_SMOOTHED);
            } else {
                callAlgorithm(AlgorithmCostFunctions2D.CORRELATION_RATIO_SMOOTHED);
            }
        }

        progressBar.updateValueImmed(25);

        if(doMutualInfoSmoothed.isSelected()) {
        	progressBar.setMessage("Calculating mutual information");
        	currentCostFunct = "Mutual Information Smoothed";
            if (baseImage.getNDims() > 2) {
                callAlgorithm(AlgorithmCostFunctions.MUTUAL_INFORMATION_SMOOTHED);
            } else {
                callAlgorithm(AlgorithmCostFunctions2D.MUTUAL_INFORMATION_SMOOTHED);
            }
        }

        progressBar.updateValueImmed(50);

        if(doNormMutualInfoSmoothed.isSelected()) {
        	progressBar.setMessage("Calculating normalized mutual information");
        	currentCostFunct = "Normalized Mutual Information Smoothed";
            if (baseImage.getNDims() > 2) {
                callAlgorithm(AlgorithmCostFunctions.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED);
            } else {
                callAlgorithm(AlgorithmCostFunctions2D.NORMALIZED_MUTUAL_INFORMATION_SMOOTHED);
            }
        }

        progressBar.setMessage("Calculating normalized cross correlation");
        progressBar.updateValueImmed(75);
        currentCostFunct = "Normalized Cross Correlation Smoothed";

        if(doNormXCorr.isSelected()) {
            if (baseImage.getNDims() > 2) {
                callAlgorithm(AlgorithmCostFunctions.NORMALIZED_XCORRELATION_SMOOTHED);
            } else {
                callAlgorithm(AlgorithmCostFunctions2D.NORMALIZED_XCORRELATION_SMOOTHED);
            }
        }
    }
    	
    private boolean setCostVariables() {
    	//always comparing to registered image
    	simpleImg2 = new ModelSimpleImage(resultImage.getExtents(), resultImage.getFileInfo(0).getResolutions(),
    			resultImage);
    	return true;
    }
    
    /**
     * Method for calling various cost algorithms, should only be called after registered image is created.
     *
     * @param  costChoice  DOCUMENT ME!
     */
    private void callAlgorithm(int costChoice) {

        try {
            // Make algorithm, use default smooth parameter of 1
            if (baseImage.getNDims() > 2) {
                algoCost = new AlgorithmCostFunctions(simpleImg1, simpleImg2, costChoice, bin1, 1);
            } else {
                algoCost = new AlgorithmCostFunctions2D(simpleImg1, simpleImg2, costChoice, bin1, 1);
            }
        } catch (OutOfMemoryError x) {
            System.gc();
            MipavUtil.displayError("Dialog Show Costs: unable to allocate enough memory");

            return;
        }

        double cost;

        cost = algoCost.cost(tMatrix);
        
        ViewUserInterface.getReference().setDataText(currentCostFunct + ":\t" + cost + "\n");

        if(algoCost != null) {
        	if(algoCost instanceof AlgorithmCostFunctions) {
        		((AlgorithmCostFunctions) algoCost).disposeLocal();
        	} else if(algoCost instanceof AlgorithmCostFunctions2D) {
        		((AlgorithmCostFunctions2D) algoCost).disposeLocal();
        	}
        	algoCost = null;
        }
    }

}
