package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.AlgorithmDWI2DTI;
import gov.nih.mipav.model.algorithms.registration.AlgorithmRegOAR35D;
import gov.nih.mipav.model.algorithms.registration.vabra.VabraAlgorithm;
import gov.nih.mipav.model.file.DTIParameters;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.scripting.ParserException;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.TransMatrix;

import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewImageFileFilter;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.ActionDiscovery;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogScriptableBase;
import gov.nih.mipav.view.renderer.WildMagic.Interface.JInterfaceBase;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.ItemEvent;
import java.io.File;
import java.util.List;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.border.EtchedBorder;
import javax.swing.border.LineBorder;
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
	
	/** The current image in the pipeline */
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

	private JPanelDTIRegistrationEddyCurrent35D eddyCurReg;

	private JPanelDTIPreprocessing DTIPreprocessing;
	
	private JPanelDTIEstimateTensor estTensorPanel;

	private JPanelEPIDistortionCorrection EPIpanel;

	private JPanelT2Load t2load;

	/** DOCUMENT ME! */
	private ViewUserInterface userInterface;

	/** DOCUMENT ME! */
	protected JTabbedPane tabbedPane;

	public ViewJFrameImage DWIframe;

	public ViewJFrameImage T2frame;

	public ViewJFrameImage DWINewB0Frame;

	public TransMatrix [] arrayTransMatrix;

	public TransMatrix b0toStructMatrix;


	private JComboBox comboBoxDTI_Algorithm;
    public float[][] gradients;
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
		if ( (event.getSource() == nextButton) && (tabbedPane.getSelectedIndex() == TENSOR_ESTIMATION) /*&& (currentImage != null)*/ )
		{
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
            gradients = dtiparams.getGradients();
            bvalues =dtiparams.getbValues();
            srcBvalGradTable = importData.srcTableModel;
			DTIPreprocessing.matrixComboBox.addItem(DWIImage.getImageDirectory());
			DTIPreprocessing.highlightBorderPanel.setBorder(highlightTitledBorder(""));
			for (int i = 0; i <dtiparams.getbValues().length-1; i++){
			    if (dtiparams.getbValues()[i] == 0 && dtiparams.getGradients()[i][0] ==0 ){
			    DTIPreprocessing.refImageNumText.setText(String.valueOf(i));
			    }
			    
			}
			repaint();



			if (dtiparams.getbValues() != null && dtiparams.getGradients() != null){
				tabbedPane.setSelectedIndex(1);
				nextButton.setEnabled(false);
				goBackButton.setEnabled(true);
				goBackButton.setActionCommand("back1");
				
				/*
				VabraAlgorithm vabra = new VabraAlgorithm();
				// if you pass in null as the second argument it will extract the B0 image to use as the subject:
				vabra.solve( DWIImage, null );
				// Opens the Deformation Field in a window:
				new ViewJFrameImage( vabra.getDeformationField() );
				
				List<ModelImage> results = vabra.getRegisteredResults();
				for ( int i = 0; i < results.size(); i++ )
				{
				    // Open the registered result in a window...
					new ViewJFrameImage( results.get(i) );
				}
				*/
			}

			else{
			    DTIPreprocessing.highlightBorderPanel.setBorder(buildTitleBorder(""));
				MipavUtil.displayError("Please load B-values and Gradients");
			}

			if (importData.useT2CheckBox.isSelected() == false){
				if (importData.m_kT2Image !=null){
					T2Image = importData.m_kT2Image;
					T2frame = importData.t2frame;
			        DTIPreprocessing.transformMatDWICheckbox.setEnabled(true);
			        DTIPreprocessing.correctGradTransCheckbox.setEnabled(true);
			        DTIPreprocessing.transformB0label.setEnabled(true);
			        DTIPreprocessing.transformB0MatCheckbox.setEnabled(true);
			        DTIPreprocessing.blanklabel.setEnabled(true);
			        DTIPreprocessing.transformB0Checkbox.setEnabled(true);
			        DTIPreprocessing.epiCheckbox.setEnabled(true);
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
			if (DTIPreprocessing.result35RegImage !=null){
				DWINewB0Image = DTIPreprocessing.result35RegImage;
				refImageNum = DTIPreprocessing.refVolNum;
			}
			
			if (DTIPreprocessing.epiCheckbox.isSelected()){
			    if (T2Image != null && DWINewB0Image !=null){
	                 arrayTransMatrix = DTIPreprocessing.arrayTransMatrix;
	                 b0toStructMatrix = DTIPreprocessing.b0toStructMatrix;
    			     tabbedPane.setSelectedIndex(2);
    	             nextButton.setEnabled(false);
    	             goBackButton.setEnabled(true);
    	             goBackButton.setActionCommand("back2");
			    }
			}
			else{
			      if (DWINewB0Image !=null){
			          inputTensorImage = DWINewB0Image;  
			      }
			      else if (DTIPreprocessing.inputPreTensorImage != null){			          
			          inputTensorImage = DTIPreprocessing.inputPreTensorImage;
			      }

	              tabbedPane.setSelectedIndex(3);
	              nextButton.setEnabled(false);
	              goBackButton.setEnabled(true);
	              goBackButton.setActionCommand("back3");
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
	
    private TitledBorder highlightTitledBorder(String title){
        return new TitledBorder(new LineBorder( Color.black, 2), title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B,
                Color.black);
    }
    
    private TitledBorder buildTitleBorder(String title) {
        return new TitledBorder(new EtchedBorder(), title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B,
                Color.black);
    }
    
    private TitledBorder buildGrayTitledBorder(String title) {
        return new TitledBorder(new EtchedBorder(), title, TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B,
                Color.gray);
    }

	private JScrollPane buildImportDataPanel() {

		importData = new JPanelDTIImportData(this);

		return importData.scrollPane;
	}

	private JPanel buildT2Panel() {

		t2load = new JPanelT2Load(this);

		return t2load.mainT2Panel;

	}

	private JPanel buildRegEddyCurPanel() {

		eddyCurReg = new JPanelDTIRegistrationEddyCurrent35D(this);

		return eddyCurReg.mainRegPanel ;
	}
	private JPanel buildPreprocessingPanel() {

		DTIPreprocessing = new JPanelDTIPreprocessing(this);

		return DTIPreprocessing.mainPrePanel ;
	}

	private JPanel buildEPIPanel() {

		EPIpanel = new JPanelEPIDistortionCorrection(this);

		return EPIpanel.mainEPIPanel ;

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


	public void finishTensorPanel(ModelImage resultImage )
	{
		this.tensorImage = resultImage;
		// Set up the fiber tracking panel inputs:
		tabbedPane.setSelectedIndex(FIBER_TRACKING);
		fiberTrack.setInputImage( tensorImage );
		nextButton.setEnabled(true);
		currentImage = tensorImage;
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
