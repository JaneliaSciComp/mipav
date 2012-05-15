package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;

import gov.nih.mipav.model.file.DTIParameters;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.table.DefaultTableModel;


public class DTIPipeline extends JDialogBase implements ActionListener, ChangeListener {

	private final static int IMPORT = 0;
	private final static int PRE_PROCESS = 1;
	private final static int EPI_DISTORTION = 2;
	private final static int TENSOR_ESTIMATION = 3;
	private final static int FIBER_TRACKING = 4;
	private final static int VISUALIZATION = 5;


	/** main panel * */
	public JPanel mainPanel;

	/** DOCUMENT ME! */
	protected JButton nextButton; 
	
	/** The current DWI image in the pipeline */
	private ModelImage currentImage = null;

	/** Diffusion weighted image read from file or from active image. */
	public ModelImage DWIImage;
	
	   /** Input image for tensor estimation */
    public ModelImage inputTensorImage;

	public String DWIDir;

	/** Structural image as reference space (T2 image): */
	public ModelImage T2Image;

	/** Registered images, results from pre-processing tab: */
	public ModelImage DWINewB0Image;

	/** Diffusion tensor image: */
	public ModelImage tensorImage = null;

	/** DOCUMENT ME! */
	private JButton goBackButton;

	private JPanelDTIImportData importData;

	//private JPanelDTIFiberTrack fiberTrack;

	private JPanelDTIFiberTracking fiberTrack;

	private DTIParameters dtiparams;

	private JPanelDTIVisualization visualization;

	private JPanelDTIPreprocessing DTIPreprocessing;
	
	private JPanelDTIEstimateTensor estTensorPanel;

	private JPanelEPIDistortionCorrection EPIpanel;

	/** DOCUMENT ME! */
	protected JTabbedPane tabbedPane;

	public ViewJFrameImage DWIframe;

	public ViewJFrameImage T2frame;

	public TransMatrix [] arrayTransMatrix;

	public TransMatrix b0toStructMatrix;

    public float[][] gradients;
    
    public float[][] bmatValues;
    
    public float[] bvalues;
    
    public int refImageNum;
    
    public DefaultTableModel srcBvalGradTable;

	// ~ Constructors
	// ---------------------------------------------------------------------------------------------------

	public DTIPipeline() {
		super(ViewUserInterface.getReference().getMainFrame(), false);
		init();
	}



	/**
	 * init
	 */

	public void init() {

		setForeground(Color.black);
		setTitle("DTI Pipeline");

		mainPanel = new JPanel();
		mainPanel.setLayout(new BorderLayout());
		//mainPanel.setLayout(new BoxLayout(mainPanel, BoxLayout.Y_AXIS));

		tabbedPane = new JTabbedPane();

		setTitle("DTI Pipeline");
		tabbedPane.addTab("Import Data", null, buildImportDataPanel());
		//tabbedPane.addTab("T2 Image Registration", null, buildT2Panel());
		//tabbedPane.addTab("Motion Correction/Eddy Current", null, buildRegEddyCurPanel());
		tabbedPane.addTab("Pre-processing", null, buildPreprocessingPanel());
		tabbedPane.addTab("EPI Distortion Correction", null, buildEPIPanel());
		tabbedPane.addTab("Tensor Estimation", null, buildTensorPanel());
		tabbedPane.addTab("Tensor Statistics", null, buildFiberTrackingPanel());
		tabbedPane.addTab("Visualization", null, buildVisuzalizationPanel());
		tabbedPane.addChangeListener(this);

		mainPanel.add(tabbedPane);

		final JPanel NextGoBackPanel = new JPanel();
		goBackButton = new JButton("Back");
		goBackButton.addActionListener(this);
		goBackButton.setActionCommand("back");
		goBackButton.setEnabled(false);
		NextGoBackPanel.add(goBackButton, BorderLayout.WEST);

		nextButton = new JButton("Next");
		nextButton.addActionListener(this);
		nextButton.setActionCommand("next");
		nextButton.setEnabled(false);
		NextGoBackPanel.add(nextButton, BorderLayout.EAST);


		mainPanel.setPreferredSize(new Dimension(1000, 750));
		getContentPane().add(mainPanel, BorderLayout.CENTER);
		getContentPane().add(NextGoBackPanel, BorderLayout.SOUTH);
		pack();
		setVisible(true);
		


	}

	// ~ Methods
	// --------------------------------------------------------------------------------------------------------

	private final static int DEFAULT = 0;
	private final static int LLMSE = 1;
	public final static int LINEAR = 2;
	public final static int NON_LINEAR = 3;
	public final static int RESTORE = 4;
	public final static int WEIGHTED_LINEAR = 5;
	
	/**
	 * action performed
	 */
	public void actionPerformed(final ActionEvent event) {
		final String command = event.getActionCommand();
		
		// currentImage is used in case the user skips the pre-processing or EPI distortion correction steps
		if ( (event.getSource() == nextButton) && (tabbedPane.getSelectedIndex() == EPI_DISTORTION) )
		{
			currentImage = EPIpanel.getResult();
			estTensorPanel.setImage(currentImage);
			tabbedPane.setSelectedIndex(TENSOR_ESTIMATION);
		}
		else if ( (event.getSource() == nextButton) && (tabbedPane.getSelectedIndex() == TENSOR_ESTIMATION) )
		{
			System.err.println( currentImage.getImageName() );
			estTensorPanel.calcTensor(currentImage);
		}
		// creates the derived images from the tensor image and sets up the visualization panel inputs.
		else if ( (event.getSource() == nextButton) && (tabbedPane.getSelectedIndex() == FIBER_TRACKING) )
		{  	
			if ( (fiberTrack.getTensorImage() != null) &&  (fiberTrack.getOutputDirectory() != null) )
			{
				fiberTrack.createDerivedImages();
				tabbedPane.setSelectedIndex(VISUALIZATION);
				nextButton.setEnabled(false);
				tensorImage = fiberTrack.getTensorImage();
				visualization.setDTIImage(tensorImage);
				visualization.setDTIColorImage(fiberTrack.getColorMapImage());
				visualization.setEVImage(fiberTrack.getEigenVectorImage());
				visualization.setEValueImage(fiberTrack.getEigenValueImage());
				visualization.setFAImage(fiberTrack.getFAImage());
				visualization.setTractFile(tensorImage.getImageDirectory() + JPanelDTIFiberTracking.TrackFileName);
				visualization.enableLoad();
			}
		}

		else if (command.equals("next1")){
			DWIImage = importData.m_kDWIImage;
			currentImage = DWIImage;
			DWIframe = importData.frame;
			dtiparams = DWIImage.getDTIParameters();			
			if (dtiparams.getGradients()!= null && dtiparams.getbValues()!=null){
                gradients = dtiparams.getGradients();
                bvalues =dtiparams.getbValues();
			}
			else if(dtiparams.getbMatrixVals()!= null){
			    bmatValues = dtiparams.getbMatrixVals();
			    DTIPreprocessing.correctGradTransCheckbox.setSelected(false);
                DTIPreprocessing.correctGradTransCheckbox.setEnabled(false);
			    
			}
            srcBvalGradTable = importData.srcTableModel;
			DTIPreprocessing.matrixComboBox.addItem(DWIImage.getImageDirectory());

			if(dtiparams.getGradients() != null){
    			for (int i = 0; i <dtiparams.getbValues().length-1; i++){
    			    if (dtiparams.getbValues()[i] == 0 && dtiparams.getGradients()[i][0] ==0 ){
    			    DTIPreprocessing.refImageNumText.setText(String.valueOf(i));
    			    }
    			    
    			}
			}
			DTIPreprocessing.setOutputDirectory(DWIImage.getImageDirectory());
			repaint();



			if (dtiparams.getbValues() != null && dtiparams.getGradients() != null || dtiparams.getbMatrixVals() != null ){
				tabbedPane.setSelectedIndex(1);
				nextButton.setEnabled(false);
				goBackButton.setEnabled(true);
				goBackButton.setActionCommand("back1");
			}

			else{
				MipavUtil.displayError("Please load B-values and Gradients or Bmatrix file");
			}

			if (importData.useT2CheckBox.isSelected() == false){
				if (importData.m_kT2Image !=null){
					T2Image = importData.m_kT2Image;
					T2frame = importData.t2frame;
			        DTIPreprocessing.transformMatDWICheckbox.setEnabled(true);
			        DTIPreprocessing.performEPICheckbox.setEnabled(true);
			        DTIPreprocessing.transformB0label.setEnabled(true);
			        DTIPreprocessing.transformB0MatCheckbox.setEnabled(true);
			        DTIPreprocessing.blanklabel.setEnabled(true);
			        DTIPreprocessing.transformB0Checkbox.setEnabled(true);
			        //DTIPreprocessing.epiCheckbox.setEnabled(true);
				}
				else{
		          
					MipavUtil.displayError("Error loading T2 image"); 
					tabbedPane.setSelectedIndex(0);
				}

			}
	        else{
	               DTIPreprocessing.structOptPanel.setBorder(buildGrayTitledBorder("B0 to Structural Image OAR 3D Output Options"));   
	                }
		}

		else if (command.equals("back1")){
			tabbedPane.setSelectedIndex(0);
			nextButton.setEnabled(true);
			goBackButton.setEnabled(false);
		}

		else if (command.equals("next2")){
			tabbedPane.setSelectedIndex(EPI_DISTORTION);
			nextButton.setEnabled(true);
			goBackButton.setEnabled(true);
			goBackButton.setActionCommand("back2");
		}
	      else if (command.equals("next3")){
	            if (DTIPreprocessing.result35RegImage != null){
    	            estTensorPanel.setImage(DTIPreprocessing.result35RegImage);
    	            tabbedPane.setSelectedIndex(TENSOR_ESTIMATION);
    	            goBackButton.setActionCommand("back2");
	            }
	            else{
	                estTensorPanel.setImage(DWIImage);
	                tabbedPane.setSelectedIndex(TENSOR_ESTIMATION);
	                goBackButton.setActionCommand("back2");  
	            }
	        }
	      else if (command.equals("back2")){
	            tabbedPane.setSelectedIndex(1);
	            nextButton.setEnabled(true);
	            goBackButton.setEnabled(false);
	        }
	      else if (command.equals("back3")){
	            tabbedPane.setSelectedIndex(2);
	            nextButton.setEnabled(true);
	            goBackButton.setEnabled(false);
	        }
			
			

		       

	}

    private TitledBorder buildGrayTitledBorder(String title) {
        return new TitledBorder(new EtchedBorder(), title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B,
                Color.gray);
    }

	private JScrollPane buildImportDataPanel() {

		importData = new JPanelDTIImportData(this);

		return importData.scrollPane;
	}

	private JPanel buildPreprocessingPanel() {

		DTIPreprocessing = new JPanelDTIPreprocessing(this);

		return DTIPreprocessing;
	}

	private JPanel buildEPIPanel() {

		EPIpanel = new JPanelEPIDistortionCorrection(this);

		return EPIpanel;

	}
	
	private JPanel buildTensorPanel() {	    
	    estTensorPanel = new JPanelDTIEstimateTensor(this);
	    return estTensorPanel;	    
	}

	private JPanelDTIFiberTracking buildFiberTrackingPanel() {
		fiberTrack = new JPanelDTIFiberTracking(this);
		return fiberTrack;
	}

	private JPanel buildVisuzalizationPanel() {
		visualization = new JPanelDTIVisualization(this, false);
		return visualization;
	}

	public void finishPreProcessingPanel( ModelImage registeredDWI, ModelImage resampledT2, 
			TransMatrix matB0toT2, String matB0FileName, TransMatrix[] matRegistered, String matRegisteredFileName )
	{
		EPIpanel.setRegisteredDWIImage( registeredDWI );
		EPIpanel.setResampledT2Image( resampledT2 );
		EPIpanel.setB0toT2Matrix( matB0toT2, matB0FileName );
		EPIpanel.setRegisteredMatrices( matRegistered, matRegisteredFileName );
		nextButton.setEnabled(true);
		currentImage = registeredDWI;
		System.err.println( currentImage.getImageName() );
	}
	
	public void finishEPIPanel()
	{
		nextButton.setEnabled(true);		
	}

	public void finishTensorPanel( ModelImage resultImage )
	{
		this.tensorImage = resultImage;
		// Set up the fiber tracking panel inputs:
		tabbedPane.setSelectedIndex(FIBER_TRACKING);
		fiberTrack.setInputImage( tensorImage );
		nextButton.setEnabled(true);
	}

	public void stateChanged(ChangeEvent e) {
		if ( e.getSource() == tabbedPane )
		{
			if ( tabbedPane.getSelectedIndex() == TENSOR_ESTIMATION )
			{
				if ( currentImage != null ) {
					estTensorPanel.setImage(currentImage);
					nextButton.setEnabled(true);
				}
				else {
					nextButton.setEnabled(false);
				}
				nextButton.setEnabled(true);
			}
			if ( tabbedPane.getSelectedIndex() == FIBER_TRACKING ) {
				if ( (fiberTrack.getTensorImage() != null) && (fiberTrack.getOutputDirectory() != null) ) {
					nextButton.setEnabled(true);
				}
				else {
					nextButton.setEnabled(false);
				}
			}
			if ( tabbedPane.getSelectedIndex() == VISUALIZATION )
			{
				nextButton.setEnabled(false);
			}
		}
	}	
}
