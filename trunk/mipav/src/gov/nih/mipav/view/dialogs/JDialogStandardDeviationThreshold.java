package gov.nih.mipav.view.dialogs;

import java.awt.Color;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmStandardDeviationThreshold;
import gov.nih.mipav.model.algorithms.AlgorithmVOIProps;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.ViewVOIVector;

public class JDialogStandardDeviationThreshold extends JDialogScriptableBase implements AlgorithmInterface {


	/** DOCUMENT ME! */
    private ModelImage srcImage, resultImage;
    
    /** DOCUMENT ME! */
    private ViewVOIVector VOIs = null;
    
    /** DOCUMENT ME! */
    private int groupNum;
    
    /** DOCUMENT ME! */
    private VOI srcVOI;
    
    /** DOCUMENT ME! */
    private AlgorithmVOIProps algoVOI;
    
    /** DOCUMENT ME! **/
    private ViewUserInterface UI;
    
    /** DOCUMENT ME **/
    private JPanel mainPanel, voiStatsPanel, voiStatsLeftPanel, voiStatsRightPanel, thresholdParametersPanel, thresholdParametersLeftPanel, thresholdParametersRightPanel;
    
    /** DOCUMENT ME **/
    private JTextField stdDevTextField, valuesOutsideTextField;
    
    /** DOCUMENT ME **/
    private float maxIntensity;
    
    /** DOCUMENT ME **/
    private float minIntensity;
    
    /** DOCUMENT ME **/
    private float avgIntensity;
    
    /** DOCUMENT ME **/
    private float stdDev;
    
    /** DOCUMENT ME **/
    private float numStdDev;
    
    /** DOCUMENT ME **/
    private float valuesOutside;
    
    /** DOCUMENT ME **/
    private AlgorithmStandardDeviationThreshold alg;
    
	
	
    /**
     * 
     *
     */
    public JDialogStandardDeviationThreshold() {
    	
    }
    
	/**
	 * 
	 * @param theParentFrame
	 * @param im
	 */
	public JDialogStandardDeviationThreshold(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        srcImage = im;
        //UI = ((ViewJFrameImage) (theParentFrame)).getUserInterface();
        UI = ViewUserInterface.getReference();
        VOIs = srcImage.getVOIs();
        
        int nVOI;

        nVOI = VOIs.size();
        
        if (nVOI == 0) {
        	MipavUtil.displayError("Image must contain 1 VOI");
            dispose();
            return;
        }
        
        for (groupNum = 0; groupNum < nVOI; groupNum++) {

            if ((VOIs.VOIAt(groupNum).isActive() == true) && (VOIs.VOIAt(groupNum).getCurveType() == VOI.CONTOUR)) {
                break;
            }
        }

        if (groupNum == nVOI) {
            MipavUtil.displayError("VOI must be selected");
            dispose();

            return;
        }
        
        srcVOI = VOIs.VOIAt(groupNum);
        
        
        algoVOI = new AlgorithmVOIProps(srcImage);
        algoVOI.setDoOnlyActiveContours(true);
        algoVOI.setRunningInSeparateThread(false);
        algoVOI.run();

        if (!algoVOI.isCompleted()) {
            MipavUtil.displayError("Please make sure VOI is selected.");

            return;
        }
        
        UI.setDataText("\n -----------------------------------------------------------------------------\n");

        UI.setDataText("Image:     " + srcImage.getImageName() + "\n");
        UI.setDataText("VOI  :     " + srcVOI.getName() + "\n");
        if (srcImage.isColorImage()) {
            UI.setDataText("              Standard deviation of voxel intensity:     " + algoVOI.getStdDevR() + " R, " +
                           algoVOI.getStdDevG() + " G, " + algoVOI.getStdDevB() + " B, " + "\n");
        } else {
            UI.setDataText("              Standard deviation of voxel intensity:     " + algoVOI.getStdDev() + "\n");
            stdDev = Float.valueOf(algoVOI.getStdDev());
        }
        if (srcImage.isColorImage()) {
            UI.setDataText("              Average voxel intensity:     " + algoVOI.getAvgIntenR() + " R, " +
                           algoVOI.getAvgIntenG() + " G, " + algoVOI.getAvgIntenB() + " B, " + "\n");
        } else {
            UI.setDataText("              Average voxel intensity:     " + algoVOI.getAvgInten() + "\n");
            avgIntensity = Float.valueOf(algoVOI.getAvgInten());
        }
        
        init();
        
        
	}
        
	
	
	/**
	 * 
	 *
	 */
	public void init() {
		setForeground(Color.black);
		setTitle("Threshold using standard deviation");
		
		GridBagLayout gbl = new GridBagLayout();
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.insets = new Insets(5,0,5,0);
		
		
		//voiStatsPanel
		voiStatsPanel = new JPanel(gbl);
		voiStatsPanel.setBorder(buildTitledBorder("VOI Statistics"));
		voiStatsLeftPanel = new JPanel(gbl); 
		gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.WEST;
        JLabel stdDevVOILabel = new JLabel("Standard deviation of voxel intensity          ");
        stdDevVOILabel.setFont(serif12);
        voiStatsLeftPanel.add(stdDevVOILabel,gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        JLabel avgVoxIntVOILabel = new JLabel("Average voxel intensity");
        avgVoxIntVOILabel.setFont(serif12);
        voiStatsLeftPanel.add(avgVoxIntVOILabel,gbc);
        
        voiStatsRightPanel = new JPanel(gbl); 
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.EAST;
        JLabel stdDevVOIValue;
        if (srcImage.isColorImage()) {
        	stdDevVOIValue = new JLabel(algoVOI.getStdDevR() + " R, " + algoVOI.getStdDevG() + " G, " + algoVOI.getStdDevB() + " B");
        } else {
        	stdDevVOIValue = new JLabel(String.valueOf(algoVOI.getStdDev()));
        }
        stdDevVOIValue.setFont(serif12);
        voiStatsRightPanel.add(stdDevVOIValue,gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        JLabel avgVoxIntVOIValue;
        if (srcImage.isColorImage()) {
        	avgVoxIntVOIValue = new JLabel(algoVOI.getAvgIntenR() + " R, " + algoVOI.getAvgIntenG() + " G, " + algoVOI.getAvgIntenB() + " B");
        } else {
        	avgVoxIntVOIValue = new JLabel(String.valueOf(algoVOI.getAvgInten()));
        }
        avgVoxIntVOIValue.setFont(serif12);
        voiStatsRightPanel.add(avgVoxIntVOIValue,gbc);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.CENTER;
        voiStatsPanel.add(voiStatsLeftPanel,gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        voiStatsPanel.add(voiStatsRightPanel,gbc);
        
        
        
        
        //thresholdParamters Panel
        thresholdParametersPanel = new JPanel(gbl);
        thresholdParametersPanel.setBorder(buildTitledBorder("Threshold Parameters"));
        thresholdParametersLeftPanel = new JPanel(gbl);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.WEST;
        JLabel stdDevThresholdLabel = new JLabel("Number of standard deviations                   ");
        stdDevThresholdLabel.setFont(serif12);
        thresholdParametersLeftPanel.add(stdDevThresholdLabel,gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        JLabel valuesOutsideLabel = new JLabel("Values outside range");
        valuesOutsideLabel.setFont(serif12);
        thresholdParametersLeftPanel.add(valuesOutsideLabel,gbc);
        
        thresholdParametersRightPanel = new JPanel(gbl);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.EAST;
        stdDevTextField = new JTextField(4);
        stdDevTextField.setFont(serif12);
        stdDevTextField.setText("3");
        thresholdParametersRightPanel.add(stdDevTextField,gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        valuesOutsideTextField = new JTextField(4);
        valuesOutsideTextField.setFont(serif12);
        valuesOutsideTextField.setText("0.0");
        thresholdParametersRightPanel.add(valuesOutsideTextField,gbc);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.fill = GridBagConstraints.HORIZONTAL;
        thresholdParametersPanel.add(thresholdParametersLeftPanel,gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        thresholdParametersPanel.add(thresholdParametersRightPanel,gbc);

        //main panel
        mainPanel = new JPanel(gbl);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(0,0,0,0);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.CENTER;
        mainPanel.add(voiStatsPanel,gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(thresholdParametersPanel,gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        mainPanel.add(buildButtons(),gbc);
        
        getContentPane().add(mainPanel);
        
        pack();
        setVisible(true);
		
	}
        

    /**
     * 
     * @param event
     */ 
    public void actionPerformed(ActionEvent event) {
    	Object source = event.getSource();
    	
    	if(source == OKButton) {
    		if(validateParams()) {
    			System.out.println(minIntensity + " " + maxIntensity);
    			callAlgorithm();
    		}
    		else {
    			MipavUtil.displayError("Value entered is not a valid number");
    		}
    		
    	}else if(source == cancelButton) {
    		dispose();
    	}
    	else if(source == helpButton) {
    		
    	}
        	
    }
    
    
    
    /**
     * 
     */
    public boolean validateParams() {
    	try {
    		numStdDev = Float.valueOf(stdDevTextField.getText());
    		valuesOutside = Float.valueOf(valuesOutsideTextField.getText());
    		maxIntensity = avgIntensity + (numStdDev * stdDev);
    		minIntensity = avgIntensity - (numStdDev * stdDev);
    	}catch(NumberFormatException e) {
    		return false;
    	}
    	return true;
    }
    
    
    
    /**
     * 
     *
     */
    protected void callAlgorithm() {
    	alg = new AlgorithmStandardDeviationThreshold(srcImage,minIntensity,maxIntensity,valuesOutside);
    	
    	alg.addListener(this);


        setVisible(false);

        if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
            MipavUtil.displayError("A thread is already running on this object");
        }
    	
    }
        
     
    /**
     * 
     */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		//grab the result image
        resultImage = alg.getResultImage();
        
        new ViewJFrameImage(resultImage);
        UI.setDataText("Result:\n");
        UI.setDataText("            Number of standard deviations used to calculate range:     " + numStdDev + "\n");
        UI.setDataText("            Thresholded range:     " + minIntensity + " - " + maxIntensity + "\n");
        if(resultImage.getNDims() == 2) {
        	UI.setDataText("            Total thresholded area:     " + alg.getTotalArea() + "  mm^2\n");
        }else if(resultImage.getNDims() == 3) {
        	UI.setDataText("            Total thresholded volume:     " + alg.getTotalVolume() + "  mm^3\n");
        }
        dispose();

	}
	
	
	/**
	 * 
	 * @throws ParserException
	 */
	protected void storeParamsFromGUI() throws ParserException {
		
	}
	
	
	
	/**
	 * 
	 *
	 */
	protected void setGUIFromParams() {
		
	}
}
