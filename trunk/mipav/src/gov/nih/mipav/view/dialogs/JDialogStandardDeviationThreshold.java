package gov.nih.mipav.view.dialogs;

import java.awt.Color;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;

import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.AlgorithmStandardDeviationThreshold;
import gov.nih.mipav.model.algorithms.AlgorithmVOIProps;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.scripting.parameters.ParameterFactory;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.ViewVOIVector;




/**
 * The Standard Deviation Threshold works by first having an active VOI in which the standard deviation and other
 * statistics are calculated. The dialog allows the user to enter the number of standard deviations and the values
 * outside the range for the thersholding process. The theresholding can be done on the whole image or alternatively,
 * on other VOIS that are not active that might be on the image. The destination can either be a new image or paint in
 * existing image except if the src image is a color image.
 * 
 * @author pandyan
 */
public class JDialogStandardDeviationThreshold extends JDialogScriptableBase implements AlgorithmInterface {


	/** src image, result image */
    private ModelImage srcImage, resultImage;
    
    /** vois in image */
    private ViewVOIVector VOIs = null;
    
    /** number for id of VOI */
    private int groupNum;
    
    /** src VOI used for thresholding ME! */
    private VOI srcVOI;
    
    /** handle to algorithm */
    private AlgorithmVOIProps algoVOI;
    
    /** handle to UI **/
    private ViewUserInterface UI;
    
    /** panels**/
    private JPanel mainPanel, voiStatsPanel, voiStatsLeftPanel, voiStatsRightPanel, thresholdParametersPanel, thresholdParametersLeftPanel, thresholdParametersRightPanel, destinationPanel, thresholdPanel;
    
    /** text fields **/
    private JTextField stdDevTextField, stdDevTextFieldR, stdDevTextFieldG, stdDevTextFieldB, valuesOutsideTextField, valuesOutsideTextFieldR, valuesOutsideTextFieldG, valuesOutsideTextFieldB;
    
    /** checkbox for inverse threshold **/
    private JCheckBox inverseThresholdCheckBox, setMinThresholdCheckBox, setMinThresholdCheckBoxR, setMinThresholdCheckBoxG, setMinThresholdCheckBoxB, setMaxThresholdCheckBox, setMaxThresholdCheckBoxR, setMaxThresholdCheckBoxG, setMaxThresholdCheckBoxB;
    
    /** max intensities calculatd using std dev **/
    private float maxIntensity, maxIntensityR, maxIntensityG, maxIntensityB;
    
    /** min intensities calculatd using std dev */
    private float minIntensity, minIntensityR, minIntensityG, minIntensityB;
    
    /** average intensities in VOI **/
    private float avgIntensity, avgIntensityR, avgIntensityG, avgIntensityB;
    
    /** standard deviations in VOI **/
    private float stdDev, stdDevR, stdDevG, stdDevB;
    
    /** number of standard deviations used in thresholding **/
    private float numStdDev, numStdDevR, numStdDevG, numStdDevB;
    
    /** intensity values to be used in thresholding **/
    private float valuesOutside, valuesOutsideR, valuesOutsideG, valuesOutsideB;
    
    /** handle to algorithm **/
    private AlgorithmStandardDeviationThreshold alg;
    
    /** radio buttons**/
    private JRadioButton newImageRadio, paintRadio, wholeImageRadio, voiRegionsRadio;
    
    /** radio groups **/
    private ButtonGroup destinationRadioGroup, thresholdRadioGroup;
    
    /** flags for destination and threshold **/
    private boolean newImageDestination, wholeImageThreshold;
    
    /** boolean for color image **/
    private boolean isColorImage;
    
    /** boolean for inverse threshold **/
    private boolean inverseThreshold = true;
    
    /** image type max */
    private double outMax = 0;

    /** image type min */
    private double outMin = 0;
    
    /** boolean telling if script failed **/
    private boolean scriptFail = false;
    
    private int numPixels;
    
    double sumPixelInten, sumPixelIntenR, sumPixelIntenG, sumPixelIntenB;
    
	
	
    /**
     * empty constructor
     */
    public JDialogStandardDeviationThreshold() {
    	
    }
    
	/**
	 * constructor
	 * @param theParentFrame
	 * @param im
	 */
	public JDialogStandardDeviationThreshold(Frame theParentFrame, ModelImage im) {
        super(theParentFrame, false);
        srcImage = im;
        if (srcImage.getNDims() == 4) {
        	MipavUtil.displayError("Algorithm will not work on 4D images");
        	return;
        }
        
        if(srcImage.isColorImage()) {
        	isColorImage = true;
        }else {
        	isColorImage = false;
        }
        UI = ViewUserInterface.getReference();
        VOIs = srcImage.getVOIs();
        
        int nVOI;

        nVOI = VOIs.size();
        
        if (nVOI == 0) {
        	//do thresholding based on whole image stats
        	srcImage.calcAvgIntenStdDev();
        	UI.setDataText("\n -----------------------------------------------------------------------------\n");
	        UI.setDataText("Image:     " + srcImage.getImageName() + "\n");
	        UI.setDataText("              Number of pixels:     " + srcImage.getNumPixels() + "\n");
	        numPixels = srcImage.getNumPixels();
	        if (srcImage.isColorImage()) {
	        	UI.setDataText("              Sum pixel intensities:     "  + srcImage.getSumPixelIntenR() + " R,  " + srcImage.getSumPixelIntenG() + " G,  " + srcImage.getSumPixelIntenB() + " B, " + "\n");
	        	sumPixelIntenR = srcImage.getSumPixelIntenR();
	        	sumPixelIntenG = srcImage.getSumPixelIntenG();
	        	sumPixelIntenB = srcImage.getSumPixelIntenB();
	        	
	        }else {
	        	UI.setDataText("              Sum pixel intensities:     "  + srcImage.getSumPixelInten() + "\n");
	        	sumPixelInten = srcImage.getSumPixelInten();
	        }
	        if (srcImage.isColorImage()) {
	            UI.setDataText("              Average pixel intensity:     " + srcImage.getAvgIntenR() + " R,  " + srcImage.getAvgIntenG() + " G,  " + srcImage.getAvgIntenB() + " B, " + "\n");
	            avgIntensityR = Float.valueOf(srcImage.getAvgIntenR());
	            avgIntensityG = Float.valueOf(srcImage.getAvgIntenG());
	            avgIntensityB = Float.valueOf(srcImage.getAvgIntenB());
	        } else {
	            UI.setDataText("              Average pixel intensity:     " + srcImage.getAvgInten() + "\n");
	            avgIntensity = Float.valueOf(srcImage.getAvgInten());
	        }
	        if (srcImage.isColorImage()) {
	            UI.setDataText("              Standard deviation of pixel intensity:     " + srcImage.getStdDeviationR() + " R,  " + srcImage.getStdDeviationG() + " G,  " + srcImage.getStdDeviationB() + " B, " + "\n");
	            stdDevR = Float.valueOf(srcImage.getStdDeviationR());
	            stdDevG = Float.valueOf(srcImage.getStdDeviationG());
	            stdDevB = Float.valueOf(srcImage.getStdDeviationB());
	        } else {
	            UI.setDataText("              Standard deviation of pixel intensity:     " + srcImage.getStdDeviation() + "\n");
	            stdDev = Float.valueOf(srcImage.getStdDeviation());
	        }
        }else {
        	//do thesholding based on active voi stats
	        int numActive = 0;
	        int activeGroupNum = 0;
	        
	        for (groupNum = 0; groupNum < nVOI; groupNum++) {
	            if ((VOIs.VOIAt(groupNum).isActive() == true) && (VOIs.VOIAt(groupNum).getCurveType() == VOI.CONTOUR)) {
	            	numActive = numActive + 1;
	            }
	        }

	        if(numActive == 0) {
	        	MipavUtil.displayError("Please make sure VOI is selected and is a contour type.");
	        	return;
	        }
	        if(numActive != 1) {
	        	MipavUtil.displayError("Please make sure only 1 VOI is selected.");
	        	return;
	        }
	    
	
	        srcVOI = VOIs.VOIAt(activeGroupNum);
	        
	        algoVOI = new AlgorithmVOIProps(srcImage, getActiveVOIs(srcImage));
	        algoVOI.setDoOnlyActiveContours(true);
	        algoVOI.setRunningInSeparateThread(false);
	        algoVOI.run();
	
	        if (!algoVOI.isCompleted()) {
	            MipavUtil.displayError("Please make sure 1 VOI is selected.");
	            return;
	        }
	        
	        UI.setDataText("\n -----------------------------------------------------------------------------\n");
	        UI.setDataText("Image:     " + srcImage.getImageName() + "\n");
	        UI.setDataText("VOI  :     " + srcVOI.getName() + "\n");
	        UI.setDataText("              Number of pixels:     " + algoVOI.getNVoxels() + "\n");
	        numPixels = algoVOI.getNVoxels();
	        if (srcImage.isColorImage()) {
	        	UI.setDataText("              Sum pixel intensities:     "  + algoVOI.getSumIntensitiesR() + " R,  " + algoVOI.getSumIntensitiesG() + " G,  " + algoVOI.getSumIntensitiesB() + " B, " + "\n");
	        	sumPixelIntenR = algoVOI.getSumIntensitiesR();
	        	sumPixelIntenG = algoVOI.getSumIntensitiesG();
	        	sumPixelIntenB = algoVOI.getSumIntensitiesB();
	        }else {
	        	UI.setDataText("              Sum pixel intensities:     "  + algoVOI.getSumIntensities() + "\n");
	        	sumPixelInten = algoVOI.getSumIntensities();
	        }
	        if (srcImage.isColorImage()) {
	            UI.setDataText("              Average pixel intensity:     " + algoVOI.getAvgIntenR() + " R,  " + algoVOI.getAvgIntenG() + " G,  " + algoVOI.getAvgIntenB() + " B, " + "\n");
	            avgIntensityR = Float.valueOf(algoVOI.getAvgIntenR());
	            avgIntensityG = Float.valueOf(algoVOI.getAvgIntenG());
	            avgIntensityB = Float.valueOf(algoVOI.getAvgIntenB());
	        } else {
	            UI.setDataText("              Average pixel intensity:     " + algoVOI.getAvgInten() + "\n");
	            avgIntensity = Float.valueOf(algoVOI.getAvgInten());
	        }
	        if (srcImage.isColorImage()) {
	            UI.setDataText("              Standard deviation of pixel intensity:     " + algoVOI.getStdDevR() + " R,  " + algoVOI.getStdDevG() + " G,  " + algoVOI.getStdDevB() + " B, " + "\n");
	            stdDevR = Float.valueOf(algoVOI.getStdDevR());
	            stdDevG = Float.valueOf(algoVOI.getStdDevG());
	            stdDevB = Float.valueOf(algoVOI.getStdDevB());
	        } else {
	            UI.setDataText("              Standard deviation of pixel intensity:     " + algoVOI.getStdDev() + "\n");
	            stdDev = Float.valueOf(algoVOI.getStdDev());
	        }
	        
	       
        }
        
        init();
	}
        
	
	
	/**
	 * init
	 */
	public void init() {
		setForeground(Color.black);
		setTitle("Threshold using standard deviation");
		
		GridBagLayout gbl = new GridBagLayout();
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.insets = new Insets(5,0,5,0);
		
		
		//voiStatsPanel
		voiStatsPanel = new JPanel(gbl);
		String borderLabel;
		if (VOIs.size() == 0) {
			borderLabel = "Image Statistics";
		}else {
			borderLabel = "Active VOI Statistics";
		}
		voiStatsPanel.setBorder(buildTitledBorder(borderLabel));
		voiStatsLeftPanel = new JPanel(gbl); 
		gbc.anchor = GridBagConstraints.WEST;
        gbc.gridx = 0;
        gbc.gridy = 0;
        JLabel numPixelsVOILabel = new JLabel("Number of pixels          ");
        numPixelsVOILabel.setFont(serif12);
        voiStatsLeftPanel.add(numPixelsVOILabel,gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        JLabel sumIntensitiesVOILabel = new JLabel("Sum pixel intensities         ");
        sumIntensitiesVOILabel.setFont(serif12);
        voiStatsLeftPanel.add(sumIntensitiesVOILabel,gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        JLabel avgVoxIntVOILabel = new JLabel("Average pixel intensity");
        avgVoxIntVOILabel.setFont(serif12);
        voiStatsLeftPanel.add(avgVoxIntVOILabel,gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        JLabel stdDevVOILabel = new JLabel("Standard deviation of pixel intensity          ");
        stdDevVOILabel.setFont(serif12);
        voiStatsLeftPanel.add(stdDevVOILabel,gbc);
        voiStatsRightPanel = new JPanel(gbl); 
        gbc.anchor = GridBagConstraints.EAST;
        gbc.gridx = 0;
        gbc.gridy = 0;
        JLabel numPixelsVOIValue;
        numPixelsVOIValue = new JLabel(String.valueOf(numPixels));
        numPixelsVOIValue.setFont(serif12);
        voiStatsRightPanel.add(numPixelsVOIValue,gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        JLabel sumIntensitiesVOIValue;
        if (srcImage.isColorImage()) {
        	sumIntensitiesVOIValue = new JLabel(sumPixelIntenR + " R,  " + sumPixelIntenG + " G,  " + sumPixelIntenB + " B");
        } else {
        	sumIntensitiesVOIValue = new JLabel(String.valueOf(sumPixelInten));
        }
        sumIntensitiesVOIValue.setFont(serif12);
        voiStatsRightPanel.add(sumIntensitiesVOIValue,gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        JLabel avgVoxIntVOIValue;
        if (srcImage.isColorImage()) {
        	avgVoxIntVOIValue = new JLabel(avgIntensityR + " R,  " + avgIntensityG + " G,  " + avgIntensityB + " B");
        } else {
        	avgVoxIntVOIValue = new JLabel(String.valueOf(avgIntensity));
        }
        avgVoxIntVOIValue.setFont(serif12);
        voiStatsRightPanel.add(avgVoxIntVOIValue,gbc);
        gbc.gridx = 0;
        gbc.gridy = 3;
        JLabel stdDevVOIValue;
        if (srcImage.isColorImage()) {
        	stdDevVOIValue = new JLabel(stdDevR + " R,  " + stdDevG + " G,  " + stdDevB + " B");
        } else {
        	stdDevVOIValue = new JLabel(String.valueOf(stdDev));
        }
        stdDevVOIValue.setFont(serif12);
        voiStatsRightPanel.add(stdDevVOIValue,gbc);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.CENTER;
        voiStatsPanel.add(voiStatsLeftPanel,gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        voiStatsPanel.add(voiStatsRightPanel,gbc);
        
        
        
        
        //thresholdparameters Panel
        thresholdParametersPanel = new JPanel(gbl);
        thresholdParametersPanel.setBorder(buildTitledBorder("Threshold Parameters"));
        thresholdParametersLeftPanel = new JPanel(gbl);
        if(!isColorImage) {
	        gbc.gridx = 0;
	        gbc.gridy = 0;
	        gbc.anchor = GridBagConstraints.WEST;
	        JLabel stdDevThresholdLabel = new JLabel("Number of standard deviations                   ");
	        stdDevThresholdLabel.setFont(serif12);
	        thresholdParametersLeftPanel.add(stdDevThresholdLabel,gbc);
	        gbc.gridx = 0;
	        gbc.gridy = 1;
	        JLabel valuesOutsideLabel = new JLabel(getRangeString(srcImage.getType()) + "      ");
	        valuesOutsideLabel.setFont(serif12);
	        thresholdParametersLeftPanel.add(valuesOutsideLabel,gbc);
	        inverseThresholdCheckBox = new JCheckBox("Inverse threshold");
	        inverseThresholdCheckBox.addActionListener(this);
	        inverseThresholdCheckBox.setActionCommand("inverseThreshold");
	        inverseThresholdCheckBox.setFont(serif12);
	        inverseThresholdCheckBox.setSelected(true);
	        gbc.gridx = 0;
	        gbc.gridy = 2;
	        thresholdParametersLeftPanel.add(inverseThresholdCheckBox,gbc);
	        setMinThresholdCheckBox = new JCheckBox("Set min threshold value to image min");
	        setMinThresholdCheckBox.setFont(serif12);
	        gbc.gridx = 0;
	        gbc.gridy = 3;
	        thresholdParametersLeftPanel.add(setMinThresholdCheckBox,gbc);
	        setMaxThresholdCheckBox = new JCheckBox("Set max threshold value to image max");
	        setMaxThresholdCheckBox.setFont(serif12);
	        gbc.gridx = 0;
	        gbc.gridy = 4;
	        thresholdParametersLeftPanel.add(setMaxThresholdCheckBox,gbc);
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
	        gbc.anchor = GridBagConstraints.NORTH;
	        gbc.fill = GridBagConstraints.HORIZONTAL;
	        thresholdParametersPanel.add(thresholdParametersLeftPanel,gbc);
	        gbc.gridx = 1;
	        gbc.gridy = 0;
	        thresholdParametersPanel.add(thresholdParametersRightPanel,gbc);
        }else {
        	gbc.gridx = 0;
	        gbc.gridy = 0;
	        gbc.anchor = GridBagConstraints.WEST;
	        JLabel stdDevThresholdLabel = new JLabel("Number of standard deviations                   ");
	        stdDevThresholdLabel.setFont(serif12);
	        thresholdParametersLeftPanel.add(stdDevThresholdLabel,gbc);
	        gbc.gridx = 0;
	        gbc.gridy = 1;
	        JLabel valuesOutsideLabel = new JLabel("Values outside range (0 to 255):      ");
	        valuesOutsideLabel.setFont(serif12);
	        thresholdParametersLeftPanel.add(valuesOutsideLabel,gbc);
	        
	        inverseThresholdCheckBox = new JCheckBox("Inverse threshold");
	        inverseThresholdCheckBox.addActionListener(this);
	        inverseThresholdCheckBox.setActionCommand("inverseThreshold");
	        inverseThresholdCheckBox.setFont(serif12);
	        inverseThresholdCheckBox.setSelected(true);
	        gbc.gridx = 0;
	        gbc.gridy = 2;
	        thresholdParametersLeftPanel.add(inverseThresholdCheckBox,gbc);
	        
	        setMinThresholdCheckBoxR = new JCheckBox("Set min threshold R value to image R min      ");
	        setMinThresholdCheckBoxR.setFont(serif12);
	        gbc.gridx = 0;
	        gbc.gridy = 3;
	        thresholdParametersLeftPanel.add(setMinThresholdCheckBoxR,gbc);
	        
	        setMaxThresholdCheckBoxR = new JCheckBox("Set max threshold R value to image R max      ");
	        setMaxThresholdCheckBoxR.setFont(serif12);
	        gbc.gridx = 0;
	        gbc.gridy = 4;
	        thresholdParametersLeftPanel.add(setMaxThresholdCheckBoxR,gbc);
	        
	        setMinThresholdCheckBoxG = new JCheckBox("Set min threshold G value to image G min      ");
	        setMinThresholdCheckBoxG.setFont(serif12);
	        gbc.gridx = 0;
	        gbc.gridy = 5;
	        thresholdParametersLeftPanel.add(setMinThresholdCheckBoxG,gbc);
	        
	        setMaxThresholdCheckBoxG = new JCheckBox("Set max threshold G value to image G max      ");
	        setMaxThresholdCheckBoxG.setFont(serif12);
	        gbc.gridx = 0;
	        gbc.gridy = 6;
	        thresholdParametersLeftPanel.add(setMaxThresholdCheckBoxG,gbc);
	        
	        setMinThresholdCheckBoxB = new JCheckBox("Set min threshold B value to image B min      ");
	        setMinThresholdCheckBoxB.setFont(serif12);
	        gbc.gridx = 0;
	        gbc.gridy = 7;
	        thresholdParametersLeftPanel.add(setMinThresholdCheckBoxB,gbc);
	        
	        setMaxThresholdCheckBoxB = new JCheckBox("Set max threshold B value to image B max      ");
	        setMaxThresholdCheckBoxB.setFont(serif12);
	        gbc.gridx = 0;
	        gbc.gridy = 8;
	        thresholdParametersLeftPanel.add(setMaxThresholdCheckBoxB,gbc);
	        
	        
	        
	        thresholdParametersRightPanel = new JPanel(gbl);
	        gbc.gridx = 0;
	        gbc.gridy = 0;
	        stdDevTextFieldR = new JTextField(4);
	        stdDevTextFieldR.setFont(serif12);
	        stdDevTextFieldR.setText("3");
	        thresholdParametersRightPanel.add(stdDevTextFieldR,gbc);
	        JLabel redLabel = new JLabel("R    ");
	        redLabel.setFont(serif12);
	        gbc.gridx = 1;
	        gbc.gridy = 0;
	        thresholdParametersRightPanel.add(redLabel,gbc);
	        gbc.gridx = 2;
	        gbc.gridy = 0;
	        stdDevTextFieldG = new JTextField(4);
	        stdDevTextFieldG.setFont(serif12);
	        stdDevTextFieldG.setText("3");
	        thresholdParametersRightPanel.add(stdDevTextFieldG,gbc);
	        JLabel greenLabel = new JLabel("G    ");
	        greenLabel.setFont(serif12);
	        gbc.gridx = 3;
	        gbc.gridy = 0;
	        thresholdParametersRightPanel.add(greenLabel,gbc);
	        gbc.gridx = 4;
	        gbc.gridy = 0;
	        stdDevTextFieldB = new JTextField(4);
	        stdDevTextFieldB.setFont(serif12);
	        stdDevTextFieldB.setText("3");
	        thresholdParametersRightPanel.add(stdDevTextFieldB,gbc);
	        JLabel blueLabel = new JLabel("B    ");
	        blueLabel.setFont(serif12);
	        gbc.gridx = 5;
	        gbc.gridy = 0;
	        thresholdParametersRightPanel.add(blueLabel,gbc);
	        gbc.gridx = 0;
	        gbc.gridy = 1;
	        valuesOutsideTextFieldR = new JTextField(4);
	        valuesOutsideTextFieldR.setFont(serif12);
	        valuesOutsideTextFieldR.setText("0.0");
	        thresholdParametersRightPanel.add(valuesOutsideTextFieldR,gbc);
	        JLabel redLabel2 = new JLabel("R    ");
	        redLabel2.setFont(serif12);
	        gbc.gridx = 1;
	        gbc.gridy = 1;
	        thresholdParametersRightPanel.add(redLabel2,gbc);
	        gbc.gridx = 2;
	        gbc.gridy = 1;
	        valuesOutsideTextFieldG = new JTextField(4);
	        valuesOutsideTextFieldG.setFont(serif12);
	        valuesOutsideTextFieldG.setText("0.0");
	        thresholdParametersRightPanel.add(valuesOutsideTextFieldG,gbc);
	        JLabel greenLabel2 = new JLabel("G    ");
	        greenLabel2.setFont(serif12);
	        gbc.gridx = 3;
	        gbc.gridy = 1;
	        thresholdParametersRightPanel.add(greenLabel2,gbc);
	        gbc.gridx = 4;
	        gbc.gridy = 1;
	        valuesOutsideTextFieldB = new JTextField(4);
	        valuesOutsideTextFieldB.setFont(serif12);
	        valuesOutsideTextFieldB.setText("0.0");
	        thresholdParametersRightPanel.add(valuesOutsideTextFieldB,gbc);
	        JLabel blueLabel2 = new JLabel("B    ");
	        blueLabel2.setFont(serif12);
	        gbc.gridx = 5;
	        gbc.gridy = 1;
	        thresholdParametersRightPanel.add(blueLabel2,gbc);
	        gbc.gridx = 0;
	        gbc.gridy = 0;
	        gbc.anchor = GridBagConstraints.NORTH;
	        gbc.fill = GridBagConstraints.HORIZONTAL;
	        thresholdParametersPanel.add(thresholdParametersLeftPanel,gbc);
	        gbc.gridx = 1;
	        gbc.gridy = 0;
	        thresholdParametersPanel.add(thresholdParametersRightPanel,gbc);
        }

        
        //destination panel
        destinationPanel = new JPanel(gbl);
        destinationPanel.setBorder(buildTitledBorder("Destination"));
        destinationRadioGroup = new ButtonGroup();
        newImageRadio = new JRadioButton("New image");
        newImageRadio.setFont(serif12);
        newImageRadio.setSelected(true);
        newImageRadio.addActionListener(this);
        newImageRadio.setActionCommand("newImageDestination");
        paintRadio = new JRadioButton("Paint in current frame");
        paintRadio.setFont(serif12);
        paintRadio.addActionListener(this);
        paintRadio.setActionCommand("paintDestination");
        if(isColorImage) {
        	paintRadio.setEnabled(false);
        }
        destinationRadioGroup.add(newImageRadio);
        destinationRadioGroup.add(paintRadio);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.WEST;
        destinationPanel.add(newImageRadio,gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        destinationPanel.add(paintRadio,gbc);
        
        
        //threshold panel
        thresholdPanel = new JPanel(gbl);
        thresholdPanel.setBorder(buildTitledBorder("Threshold"));
        thresholdRadioGroup = new ButtonGroup();
        wholeImageRadio = new JRadioButton("Whole image");
        wholeImageRadio.setFont(serif12);
        wholeImageRadio.setSelected(true);
        if (VOIs.size() == 0) {
        	wholeImageRadio.setEnabled(false);
        }
        voiRegionsRadio = new JRadioButton("VOI region(s)");
        voiRegionsRadio.setFont(serif12);
        if (VOIs.size() == 0) {
        	voiRegionsRadio.setEnabled(false);
        }
        thresholdRadioGroup.add(wholeImageRadio);
        thresholdRadioGroup.add(voiRegionsRadio);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.anchor = GridBagConstraints.WEST;
        thresholdPanel.add(wholeImageRadio,gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        thresholdPanel.add(voiRegionsRadio,gbc);
        
        
        //main panel
        mainPanel = new JPanel(gbl);
        gbc.gridx = 0;
        gbc.gridy = 0;
        gbc.insets = new Insets(3,3,3,3);
        gbc.fill = GridBagConstraints.HORIZONTAL;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.gridwidth = 2;
        mainPanel.add(voiStatsPanel,gbc);
        gbc.gridx = 0;
        gbc.gridy = 1;
        mainPanel.add(thresholdParametersPanel,gbc);
        gbc.gridwidth = 1;
        gbc.gridx = 0;
        gbc.gridy = 2;
        mainPanel.add(destinationPanel,gbc);
        gbc.gridx = 1;
        gbc.gridy = 2;
        mainPanel.add(thresholdPanel,gbc);
        gbc.gridwidth = 2;
        gbc.gridx = 0;
        gbc.gridy = 3;
        mainPanel.add(buildButtons(),gbc);
        
        getContentPane().add(mainPanel);
        
        pack();
        setVisible(true);
	}
        

    /**
     * action performed
     * @param event
     */ 
    public void actionPerformed(ActionEvent event) {
    	String command = event.getActionCommand();
    	
    	if(command.equals("OK")) {
    		if(validateParams()) {
    			callAlgorithm();
    		}
    	}else if(command.equals("Cancel")) {
    		dispose();
    	}
    	else if(command.equals("Help")) {
    		if(!isColorImage) {
    			//MipavUtil.showHelp("StDev001");
    			MipavUtil.showWebHelp("Standard_Deviation_Threshold#Applying_the_algorithm_to_grayscale_images");
    		}else {
    			//MipavUtil.showHelp("StDev005");
    		    MipavUtil.showWebHelp("Standard_Deviation_Threshold#Applying_the_algorithm_to_grayscale_images");
    		}
    	}else if(command.equals("paintDestination")) {
    		if(!isColorImage) {
	    		valuesOutsideTextField.setDisabledTextColor(Color.lightGray);
	    		valuesOutsideTextField.setEnabled(false);
	    		valuesOutsideTextField.setEditable(false);
	    		valuesOutsideTextField.setBackground(Color.lightGray);
    		}else {
    			valuesOutsideTextFieldR.setDisabledTextColor(Color.lightGray);
	    		valuesOutsideTextFieldR.setEnabled(false);
	    		valuesOutsideTextFieldR.setEditable(false);
	    		valuesOutsideTextFieldR.setBackground(Color.lightGray);
	    		valuesOutsideTextFieldG.setDisabledTextColor(Color.lightGray);
	    		valuesOutsideTextFieldG.setEnabled(false);
	    		valuesOutsideTextFieldG.setEditable(false);
	    		valuesOutsideTextFieldG.setBackground(Color.lightGray);
	    		valuesOutsideTextFieldB.setDisabledTextColor(Color.lightGray);
	    		valuesOutsideTextFieldB.setEnabled(false);
	    		valuesOutsideTextFieldB.setEditable(false);
	    		valuesOutsideTextFieldB.setBackground(Color.lightGray);
    		}
    	}else if(command.equals("newImageDestination")) {
    		if(!isColorImage) {
	    		valuesOutsideTextField.setEnabled(true);
	    		valuesOutsideTextField.setEditable(true);
	    		valuesOutsideTextField.setBackground(Color.white);
    		}else {
    			valuesOutsideTextFieldR.setEnabled(true);
	    		valuesOutsideTextFieldR.setEditable(true);
	    		valuesOutsideTextFieldR.setBackground(Color.white);
	    		valuesOutsideTextFieldG.setEnabled(true);
	    		valuesOutsideTextFieldG.setEditable(true);
	    		valuesOutsideTextFieldG.setBackground(Color.white);
	    		valuesOutsideTextFieldB.setEnabled(true);
	    		valuesOutsideTextFieldB.setEditable(true);
	    		valuesOutsideTextFieldB.setBackground(Color.white);
    		}
    	}else if(command.equals("inverseThreshold")) {
    		if(inverseThresholdCheckBox.isSelected()) {
    			inverseThreshold = true;
    		}else {
    			inverseThreshold = false;
    		}
    	}
        	
    }
    
    
    
    /**
     * validate params
     */
    public boolean validateParams() {
    	try {
    		if(!isColorImage) {
	    		numStdDev = Float.valueOf(stdDevTextField.getText());
	    		valuesOutside = Float.valueOf(valuesOutsideTextField.getText());
	    		if(valuesOutside < outMin || valuesOutside > outMax) {
	    			MipavUtil.displayError("Range must be " + outMin + " - " + outMax);
	    			return false;
	    		}
	    		maxIntensity = avgIntensity + (numStdDev * stdDev);
	    		minIntensity = avgIntensity - (numStdDev * stdDev);
	    		if(minIntensity < (float)srcImage.getMin()) {
	    			minIntensity = (float)srcImage.getMin();
	    		}
	    		if(maxIntensity > (float)srcImage.getMax()) {
	    			maxIntensity = (float)srcImage.getMax();
	    		}
	    		if(setMinThresholdCheckBox.isSelected()) {
	    			minIntensity = (float)srcImage.getMin();
	    		}
	    		if(setMaxThresholdCheckBox.isSelected()) {
	    			maxIntensity = (float)srcImage.getMax();
	    		}
    		}else {
    			numStdDevR = Float.valueOf(stdDevTextFieldR.getText());
	    		valuesOutsideR = Float.valueOf(valuesOutsideTextFieldR.getText());
	    		numStdDevG = Float.valueOf(stdDevTextFieldG.getText());
	    		valuesOutsideG = Float.valueOf(valuesOutsideTextFieldG.getText());
	    		numStdDevB = Float.valueOf(stdDevTextFieldB.getText());
	    		valuesOutsideB = Float.valueOf(valuesOutsideTextFieldB.getText());
	    		if(valuesOutsideR < 0 || valuesOutsideR > 255) {
	    			MipavUtil.displayError("Range must be 0 - 255");
	    			return false;
	    		}
	    		if(valuesOutsideG < 0 || valuesOutsideG > 255) {
	    			MipavUtil.displayError("Range must be 0 - 255");
	    			return false;
	    		}
	    		if(valuesOutsideB < 0 || valuesOutsideB > 255) {
	    			MipavUtil.displayError("Range must be 0 - 255");
	    			return false;
	    		}
	    		maxIntensityR = avgIntensityR + (numStdDevR * stdDevR);
	    		minIntensityR = avgIntensityR - (numStdDevR * stdDevR);
	    		maxIntensityG = avgIntensityG + (numStdDevG * stdDevG);
	    		minIntensityG = avgIntensityG - (numStdDevG * stdDevG);
	    		maxIntensityB = avgIntensityB + (numStdDevB * stdDevB);
	    		minIntensityB = avgIntensityB - (numStdDevB * stdDevB);	
	    		if(minIntensityR < (float)srcImage.getMinR()) {
	    			minIntensityR = (float)srcImage.getMinR();
	    		}
	    		if(maxIntensityR > (float)srcImage.getMaxR()) {
	    			maxIntensityR = (float)srcImage.getMaxR();
	    		}
	    		if(minIntensityG < (float)srcImage.getMinG()) {
	    			minIntensityG = (float)srcImage.getMinG();
	    		}
	    		if(maxIntensityG > (float)srcImage.getMaxG()) {
	    			maxIntensityG = (float)srcImage.getMaxG();
	    		}
	    		if(minIntensityB < (float)srcImage.getMinB()) {
	    			minIntensityB = (float)srcImage.getMinB();
	    		}
	    		if(maxIntensityB > (float)srcImage.getMaxB()) {
	    			maxIntensityB = (float)srcImage.getMaxB();
	    		}
	    		if(setMinThresholdCheckBoxR.isSelected()) {
	    			minIntensityR = (float)srcImage.getMinR();
	    		}
	    		if(setMaxThresholdCheckBoxR.isSelected()) {
	    			maxIntensityR = (float)srcImage.getMaxR();
	    		}
	    		
	    		if(setMinThresholdCheckBoxG.isSelected()) {
	    			minIntensityG = (float)srcImage.getMinG();
	    		}
	    		if(setMaxThresholdCheckBoxG.isSelected()) {
	    			maxIntensityG = (float)srcImage.getMaxG();
	    		}
	    		
	    		if(setMinThresholdCheckBoxB.isSelected()) {
	    			minIntensityB = (float)srcImage.getMinB();
	    		}
	    		if(setMaxThresholdCheckBoxB.isSelected()) {
	    			maxIntensityB = (float)srcImage.getMaxB();
	    		}
    		}
    		if(newImageRadio.isSelected()) {
    			newImageDestination = true;
    		}else {
    			newImageDestination = false;
    		}
    		if(wholeImageRadio.isSelected()) {
    			wholeImageThreshold = true;
    		} else {
    			wholeImageThreshold = false;
    		}
    	}catch(NumberFormatException e) {
    		MipavUtil.displayError("Value entered is not a valid number");
    		return false;
    	}
    	return true;
    }
    
    
    
    /**
     * call algorithm
     */
    protected void callAlgorithm() {
    	if(!isColorImage) {
    		alg = new AlgorithmStandardDeviationThreshold(srcImage,minIntensity,maxIntensity,valuesOutside,newImageDestination,wholeImageThreshold,inverseThreshold);
    	}else {
    		alg = new AlgorithmStandardDeviationThreshold(srcImage,minIntensityR,minIntensityG,minIntensityB,maxIntensityR,maxIntensityG,maxIntensityB,valuesOutsideR,valuesOutsideG,valuesOutsideB,newImageDestination,wholeImageThreshold,inverseThreshold);
    	}
    	alg.addListener(this);

        setVisible(false);

        if(isRunInSeparateThread()) {
	        // Start the thread as a low priority because we wish to still have user interface work fast
	        if (alg.startMethod(Thread.MIN_PRIORITY) == false) {
	            MipavUtil.displayError("A thread is already running on this object");
	        }
        } else {
        	alg.run();
        }
    }
        
     
    /**
     * algorithm performed
     */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		//grab the result image if new image destination radio was selected
		if(algorithm.isCompleted() && !scriptFail) {
			if(newImageDestination) {
				resultImage = alg.getResultImage();
				openNewFrame(resultImage);
				resultImage.getParentFrame().getComponentImage().getPaintBitmap().clear();
			} else {
				srcImage.notifyImageDisplayListeners();
			}
			if(!isColorImage) {
	        	UI.setDataText("Result:\n");
	        	UI.setDataText("            Number of standard deviations used to calculate range:     " + numStdDev + "\n");
	        	UI.setDataText("            Inverse Threshold:     " + inverseThreshold + "\n");
	        	UI.setDataText("            Thresholded range:     " + minIntensity + " - " + maxIntensity + "\n");
		        if(srcImage.getNDims() == 2) {
		        	UI.setDataText("            Area:     " + alg.getTotalArea() + "  mm^2\n");
		        }else if(srcImage.getNDims() == 3) {
		        	UI.setDataText("            Volume:     " + alg.getTotalVolume() + "  mm^3\n");
		        }
	        
		        UI.setDataText("            Number of pixels:     " + alg.getNumPixels() + "\n");
		        UI.setDataText("            Sum pixel intensities:     " + alg.getSumIntensities() + "\n");
		        float avg = alg.getSumIntensities()/alg.getNumPixels();
		        UI.setDataText("            Average pixel intensity:     " + avg + "\n");
		        UI.setDataText("\n -----------------------------------------------------------------------------\n");
	        }else {
	        	UI.setDataText("Result:\n");
	        	UI.setDataText("            Number of standard deviations used to calculate red channel range:     " + numStdDevR + "\n");
	        	UI.setDataText("            Inverse Threshold:     " + inverseThreshold + "\n");
	        	UI.setDataText("            Thresholded range R:     " + minIntensityR + " - " + maxIntensityR + "\n");
	        	if(srcImage.getNDims() == 2) {
		        	UI.setDataText("            Area R:     " + alg.getTotalAreaR() + "  mm^2\n");
		        }else if(srcImage.getNDims() == 3) {
		        	UI.setDataText("            Volume R:     " + alg.getTotalVolumeR() + "  mm^3\n");
		        }
	        	UI.setDataText("            Number of pixels R:     " + alg.getNumPixelsR() + "\n");
		        UI.setDataText("            Sum pixel intensities R:     " + alg.getSumIntensitiesR() + "\n");
		        float avgR = alg.getSumIntensitiesR()/alg.getNumPixelsR();
		        UI.setDataText("            Average pixel intensity R:     " + avgR + "\n");
	        	UI.setDataText("\n");
	        	UI.setDataText("            Number of standard deviations used to calculate green channel range:     " + numStdDevG + "\n");
	        	UI.setDataText("            Thresholded range G:     " + minIntensityG + " - " + maxIntensityG + "\n");
	        	if(srcImage.getNDims() == 2) {
		        	UI.setDataText("            Area G:     " + alg.getTotalAreaG() + "  mm^2\n");
		        }else if(srcImage.getNDims() == 3) {
		        	UI.setDataText("            Volume G:     " + alg.getTotalVolumeG() + "  mm^3\n");
		        }
	        	UI.setDataText("            Number of pixels G:     " + alg.getNumPixelsG() + "\n");
		        UI.setDataText("            Sum pixel intensities G:     " + alg.getSumIntensitiesG() + "\n");
		        float avgG = alg.getSumIntensitiesG()/alg.getNumPixelsG();
		        UI.setDataText("            Average pixel intensity G:     " + avgG + "\n");
	        	UI.setDataText("\n");
	        	UI.setDataText("            Number of standard deviations used to calculate blue channel range:     " + numStdDevB + "\n");
	        	UI.setDataText("            Thresholded range B:     " + minIntensityB + " - " + maxIntensityB + "\n");
	        	if(srcImage.getNDims() == 2) {
		        	UI.setDataText("            Area B:     " + alg.getTotalAreaB() + "  mm^2\n");
		        }else if(srcImage.getNDims() == 3) {
		        	UI.setDataText("            Volume B:     " + alg.getTotalVolumeB() + "  mm^3\n");
		        }
	        	UI.setDataText("            Number of pixels B:     " + alg.getNumPixelsB() + "\n");
		        UI.setDataText("            Sum pixel intensities B:     " + alg.getSumIntensitiesB() + "\n");
		        float avgB = alg.getSumIntensitiesB()/alg.getNumPixelsB();
		        UI.setDataText("            Average pixel intensity B:     " + avgB + "\n");
	        	UI.setDataText("\n -----------------------------------------------------------------------------\n");
	        }
			insertScriptLine();
			dispose();
		}
       
	}
	
	
	
	
	/**
	 * 
	 * @param imageType
	 * @return
	 */
	private String getRangeString(int imageType) {

        String fillString = new String("Values outside threshold ");

        if (imageType == ModelStorageBase.BOOLEAN) {
            fillString += "(0 or 1):";
            outMin = 0;
            outMax = 1;
        } else if (imageType == ModelStorageBase.BYTE) {
            fillString += "(-128 to 127):";
            outMin = -128;
            outMax = 127;
        } else if (imageType == ModelStorageBase.UBYTE) {
            fillString += "(0 to 255):";
            outMin = 0;
            outMax = 255;
        } else if (imageType == ModelStorageBase.SHORT) {
            fillString += "(-32768 to 32767):";
            outMin = -32768;
            outMax = 32767;
        } else if (imageType == ModelStorageBase.USHORT) {
            fillString += "(0 to 65535):";
            outMin = 0;
            outMax = 65535;
        } else if (imageType == ModelStorageBase.INTEGER) {
            fillString += "(-2.147 E+9 to 2.147 E+9):";
            outMin = Integer.MIN_VALUE;
            outMax = Integer.MAX_VALUE;
        } else if (imageType == ModelStorageBase.UINTEGER) {
            fillString += "(0 to 4.29 E+9):";
            outMin = 0;
            outMax = 4294967295L;
        } else if (imageType == ModelStorageBase.LONG) {
            fillString += "(-9.22 E+18 to 9.22 E+18):";
            outMin = Long.MIN_VALUE;
            outMax = Long.MAX_VALUE;
        } else if (imageType == ModelStorageBase.FLOAT) {
            fillString += "(-3.40 E+38  to 3.40 E+38):";
            outMin = -Float.MAX_VALUE;
            outMax = Float.MAX_VALUE;
        } else if (imageType == ModelStorageBase.DOUBLE) {
            fillString += "(-1.8 E+308 to 1.8 E+308):";
            outMin = -Double.MAX_VALUE;
            outMax = Double.MAX_VALUE;
        }

        return fillString;
    }
	
	
	
	/**
	 * sets out min and max...used in scripting
	 * @param imageType
	 */
	private void setOutMinAndMax(int imageType) {
		if (imageType == ModelStorageBase.BOOLEAN) {
            outMin = 0;
            outMax = 1;
        } else if (imageType == ModelStorageBase.BYTE) {
            outMin = -128;
            outMax = 127;
        } else if (imageType == ModelStorageBase.UBYTE) {
            outMin = 0;
            outMax = 255;
        } else if (imageType == ModelStorageBase.SHORT) {
            outMin = -32768;
            outMax = 32767;
        } else if (imageType == ModelStorageBase.USHORT) {
            outMin = 0;
            outMax = 65535;
        } else if (imageType == ModelStorageBase.INTEGER) {
            outMin = Integer.MIN_VALUE;
            outMax = Integer.MAX_VALUE;
        } else if (imageType == ModelStorageBase.UINTEGER) {
            outMin = 0;
            outMax = 4294967295L;
        } else if (imageType == ModelStorageBase.LONG) {
            outMin = Long.MIN_VALUE;
            outMax = Long.MAX_VALUE;
        } else if (imageType == ModelStorageBase.FLOAT) {
            outMin = -Float.MAX_VALUE;
            outMax = Float.MAX_VALUE;
        } else if (imageType == ModelStorageBase.DOUBLE) {
            outMin = -Double.MAX_VALUE;
            outMax = Double.MAX_VALUE;
        }
	}
	
	
	/**
	 * store params
	 * @throws ParserException
	 */
	protected void storeParamsFromGUI() throws ParserException {
		scriptParameters.storeInputImage(srcImage);
		if(!isColorImage) {
			scriptParameters.getParams().put(ParameterFactory.newParameter("numStdDev", numStdDev));
			scriptParameters.getParams().put(ParameterFactory.newParameter("valuesOutside", valuesOutside));
			scriptParameters.getParams().put(ParameterFactory.newParameter("setMinThreshold", setMinThresholdCheckBox.isSelected()));
			scriptParameters.getParams().put(ParameterFactory.newParameter("setMaxThreshold", setMaxThresholdCheckBox.isSelected()));
			scriptParameters.getParams().put(ParameterFactory.newParameter("stdDev", stdDev));
			scriptParameters.getParams().put(ParameterFactory.newParameter("avgIntensity", avgIntensity));
		}else {
			scriptParameters.getParams().put(ParameterFactory.newParameter("numStdDevR", numStdDevR));
			scriptParameters.getParams().put(ParameterFactory.newParameter("valuesOutsideR", valuesOutsideR));
			scriptParameters.getParams().put(ParameterFactory.newParameter("numStdDevG", numStdDevG));
			scriptParameters.getParams().put(ParameterFactory.newParameter("valuesOutsideG", valuesOutsideG));
			scriptParameters.getParams().put(ParameterFactory.newParameter("numStdDevB", numStdDevB));
			scriptParameters.getParams().put(ParameterFactory.newParameter("valuesOutsideB", valuesOutsideB));
			scriptParameters.getParams().put(ParameterFactory.newParameter("setMinThresholdR", setMinThresholdCheckBoxR.isSelected()));
			scriptParameters.getParams().put(ParameterFactory.newParameter("setMaxThresholdR", setMaxThresholdCheckBoxR.isSelected()));
			scriptParameters.getParams().put(ParameterFactory.newParameter("setMinThresholdG", setMinThresholdCheckBoxG.isSelected()));
			scriptParameters.getParams().put(ParameterFactory.newParameter("setMaxThresholdG", setMaxThresholdCheckBoxG.isSelected()));
			scriptParameters.getParams().put(ParameterFactory.newParameter("setMinThresholdB", setMinThresholdCheckBoxB.isSelected()));
			scriptParameters.getParams().put(ParameterFactory.newParameter("setMaxThresholdB", setMaxThresholdCheckBoxB.isSelected()));
			scriptParameters.getParams().put(ParameterFactory.newParameter("stdDevR", stdDevR));
			scriptParameters.getParams().put(ParameterFactory.newParameter("avgIntensityR", avgIntensityR));
			scriptParameters.getParams().put(ParameterFactory.newParameter("stdDevG", stdDevG));
			scriptParameters.getParams().put(ParameterFactory.newParameter("avgIntensityG", avgIntensityG));
			scriptParameters.getParams().put(ParameterFactory.newParameter("stdDevB", stdDevB));
			scriptParameters.getParams().put(ParameterFactory.newParameter("avgIntensityB", avgIntensityB));
		}
		scriptParameters.getParams().put(ParameterFactory.newParameter("inverseThreshold", inverseThreshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("newImageDestination", newImageDestination));
        scriptParameters.getParams().put(ParameterFactory.newParameter("wholeImageThreshold", wholeImageThreshold));
        scriptParameters.getParams().put(ParameterFactory.newParameter("isColorImage", isColorImage));
        
	}
	
	
	
	/**
	 * set gui from params
	 */
	protected void setGUIFromParams() {
		srcImage = scriptParameters.retrieveInputImage();
        UI = ViewUserInterface.getReference();
        parentFrame = srcImage.getParentFrame();
        setOutMinAndMax(srcImage.getType());
        inverseThreshold = scriptParameters.getParams().getBoolean("inverseThreshold");
        
        //scripting will only work if the input images were the same color type as the one recorder
        
        if(srcImage.isColorImage() != scriptParameters.getParams().getBoolean("isColorImage")) {
        	MipavUtil.displayError("Input images must be the same color type as the image that was used to record the script");
        	scriptFail = true;
        	dispose();
			return;
        }
        
        if(!isColorImage) {
    		numStdDev = scriptParameters.getParams().getFloat("numStdDev");
    		valuesOutside = scriptParameters.getParams().getFloat("valuesOutside");
    		if(valuesOutside < outMin || valuesOutside > outMax) {
    			MipavUtil.displayError("Range must be " + outMin + " - " + outMax);
    			scriptFail = true;
    			dispose();
    			return;
    		}
    		avgIntensity = scriptParameters.getParams().getFloat("avgIntensity");
    		stdDev = scriptParameters.getParams().getFloat("stdDev");
    		
    		maxIntensity = avgIntensity + (numStdDev * stdDev);
    		minIntensity = avgIntensity - (numStdDev * stdDev);
    		
    		if(minIntensity < (float)srcImage.getMin()) {
    			minIntensity = (float)srcImage.getMin();
    		}
    		if(maxIntensity > (float)srcImage.getMax()) {
    			maxIntensity = (float)srcImage.getMax();
    		}
    		
    		boolean setMinThresh = scriptParameters.getParams().getBoolean("setMinThreshold");
    		boolean setMaxThresh = scriptParameters.getParams().getBoolean("setMaxThreshold");
    		
    		if(setMinThresh) {
    			minIntensity = (float)srcImage.getMin();
    		}
    		if(setMaxThresh) {
    			maxIntensity = (float)srcImage.getMax();
    		}
		}else {
			numStdDevR = scriptParameters.getParams().getFloat("numStdDevR");
    		valuesOutsideR = scriptParameters.getParams().getFloat("valuesOutsideR");
    		numStdDevG = scriptParameters.getParams().getFloat("numStdDevG");
    		valuesOutsideG = scriptParameters.getParams().getFloat("valuesOutsideG");
    		numStdDevB = scriptParameters.getParams().getFloat("numStdDevB");
    		valuesOutsideB = scriptParameters.getParams().getFloat("valuesOutsideB");
    		if(valuesOutsideR < 0 || valuesOutsideR > 255) {
    			MipavUtil.displayError("Range must be 0 - 255");
    			scriptFail = true;
    			dispose();
    			return;
    		}
    		if(valuesOutsideG < 0 || valuesOutsideG > 255) {
    			MipavUtil.displayError("Range must be 0 - 255");
    			scriptFail = true;
    			dispose();
    			return;
    		}
    		if(valuesOutsideB < 0 || valuesOutsideB > 255) {
    			MipavUtil.displayError("Range must be 0 - 255");
    			scriptFail = true;
    			dispose();
    			return;
    		}
    		
    		avgIntensityR = scriptParameters.getParams().getFloat("avgIntensityR");
    		stdDevR = scriptParameters.getParams().getFloat("stdDevR");
    		avgIntensityG = scriptParameters.getParams().getFloat("avgIntensityG");
    		stdDevG = scriptParameters.getParams().getFloat("stdDevG");
    		avgIntensityB = scriptParameters.getParams().getFloat("avgIntensityB");
    		stdDevB = scriptParameters.getParams().getFloat("stdDevB");
    		
    		maxIntensityR = avgIntensityR + (numStdDevR * stdDevR);
    		minIntensityR = avgIntensityR - (numStdDevR * stdDevR);
    		maxIntensityG = avgIntensityG + (numStdDevG * stdDevG);
    		minIntensityG = avgIntensityG - (numStdDevG * stdDevG);
    		maxIntensityB = avgIntensityB + (numStdDevB * stdDevB);
    		minIntensityB = avgIntensityB - (numStdDevB * stdDevB);	
    		if(minIntensityR < (float)srcImage.getMinR()) {
    			minIntensityR = (float)srcImage.getMinR();
    		}
    		if(maxIntensityR > (float)srcImage.getMaxR()) {
    			maxIntensityR = (float)srcImage.getMaxR();
    		}
    		if(minIntensityG < (float)srcImage.getMinG()) {
    			minIntensityG = (float)srcImage.getMinG();
    		}
    		if(maxIntensityG > (float)srcImage.getMaxG()) {
    			maxIntensityG = (float)srcImage.getMaxG();
    		}
    		if(minIntensityB < (float)srcImage.getMinB()) {
    			minIntensityB = (float)srcImage.getMinB();
    		}
    		if(maxIntensityB > (float)srcImage.getMaxB()) {
    			maxIntensityB = (float)srcImage.getMaxB();
    		}
    		
    		boolean setMinThreshR = scriptParameters.getParams().getBoolean("setMinThresholdR");
    		boolean setMaxThreshR = scriptParameters.getParams().getBoolean("setMaxThresholdR");
    		boolean setMinThreshG = scriptParameters.getParams().getBoolean("setMinThresholdG");
    		boolean setMaxThreshG = scriptParameters.getParams().getBoolean("setMaxThresholdG");
    		boolean setMinThreshB = scriptParameters.getParams().getBoolean("setMinThresholdB");
    		boolean setMaxThreshB = scriptParameters.getParams().getBoolean("setMaxThresholdB");
    		
    		
    		if(setMinThreshR) {
    			minIntensityR = (float)srcImage.getMinR();
    		}
    		if(setMaxThreshR) {
    			maxIntensityR = (float)srcImage.getMaxR();
    		}
    		
    		if(setMinThreshG) {
    			minIntensityG = (float)srcImage.getMinG();
    		}
    		if(setMaxThreshG) {
    			maxIntensityG = (float)srcImage.getMaxG();
    		}
    		
    		if(setMinThreshB) {
    			minIntensityB = (float)srcImage.getMinB();
    		}
    		if(setMaxThreshB) {
    			maxIntensityB = (float)srcImage.getMaxB();
    		}
		}
        newImageDestination = scriptParameters.getParams().getBoolean("newImageDestination");
        wholeImageThreshold = scriptParameters.getParams().getBoolean("wholeImageThreshold");

	}
	
	/**
     * This legacy code returns all active vois for a given source image.  PlugIns should explicitly identify VOIs they would
     * like to process using AlgorithmVOIProps, because the user may have already added other VOIs to srcImage, or VOIs
     * may be created by the algorithm in an unexpected way.  This plugin relied on <code>AlgorithmVOIProp</code>'s 
     * getActiveVOIs() code, so that code has been moved into this plugin.
     * 
     * Use of this method is discouraged, as shown by the old documentation for this method:
     * not for use. should be moved to a better location. does NOT clone the VOIs that it find to be active, and inserts
     * into a new ViewVOIVector. if no VOIs are active, the ViewVOIVector returned is <code>null</code>.
     *
     * @return  All the active VOIs for a given srcImage.
     */
    private ViewVOIVector getActiveVOIs(ModelImage srcImage) {
        ViewVOIVector voiList;

        voiList = new ViewVOIVector();

        int i;

        try {

            for (i = 0; i < srcImage.getVOIs().size(); i++) {

                if (srcImage.getVOIs().VOIAt(i).isActive()) {

                    // voi at i is the active voi
                    voiList.addElement(srcImage.getVOIs().VOIAt(i));
                }
            }
        } catch (ArrayIndexOutOfBoundsException indexException) {

            // got to the end of list and never found an active VOI.
            // return an  empty VOI list.
            return new ViewVOIVector();
        }

        return voiList;
    }
}
