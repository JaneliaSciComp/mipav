package gov.nih.mipav.view.renderer.WildMagic.DTI_FrameWork;


import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.algorithms.DiffusionTensorImaging.AlgorithmDWI2DTI;
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
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;

import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.ScrollPaneConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;


public class DTIPipeline extends JDialogBase implements AlgorithmInterface, ActionListener, ChangeListener {

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

	/** Mask image for the tensor calculation */
	private ModelImage maskImage = null;
    JTextField textDTIimage = new JTextField();
	
	/** The current image in the pipeline */
	private ModelImage currentImage = null;

	/** Diffusion weighted image read from file or from active image. */
	public ModelImage DWIImage;

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




	// ~ Constructors
	// ---------------------------------------------------------------------------------------------------

	public DTIPipeline() {
		super(ViewUserInterface.getReference().getMainFrame(), false);

		//Testing
		/*String fileDir = "C:\\Users\\tyrieek\\Desktop\\PARRECTest\\Landman_4.2.REC";
        //String fileName = "DTI_2MM_FREE_39VOL_SET2_10.dcm";
        FileIO fileIO = new FileIO();
        image = fileIO.readImage(fileDir);
        new ViewJFrameImage(image);*/

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
		tabbedPane.addTab("Fiber Tracking/ Statistics", null, buildFiberTrackingPanel());
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

	/**
	 * action performed
	 */
	public void actionPerformed(final ActionEvent event) {
		final String command = event.getActionCommand();

		if ( command.equals("browseMaskFile") )
		{
			loadMaskImage();
		}
		
		// currentImage is used in case the user skips the pre-processing or EPI distortion correction steps
		if ( (event.getSource() == nextButton) && (tabbedPane.getSelectedIndex() == TENSOR_ESTIMATION) && (currentImage != null) )
		{
			AlgorithmDWI2DTI calcDTI = new AlgorithmDWI2DTI( currentImage, maskImage );
			calcDTI.addListener(this);
			calcDTI.run();
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
			DTIPreprocessing.matrixComboBox.addItem(DWIImage.getImageDirectory());

			if (dtiparams.getbValues() != null && dtiparams.getGradients() != null){
				tabbedPane.setSelectedIndex(1);
				nextButton.setEnabled(false);
				goBackButton.setEnabled(true);
				goBackButton.setActionCommand("back1");
			}

			else if (dtiparams.getbValues() != null && dtiparams.getGradients() != null){
				tabbedPane.setSelectedIndex(1);
				nextButton.setEnabled(false);
				goBackButton.setEnabled(true);
				goBackButton.setActionCommand("back1");
			}
			else{
				MipavUtil.displayError("Please load B-values and Gradients");
			}

			if (importData.useT2CheckBox.isSelected() == false){
				if (importData.m_kT2Image !=null){
					System.out.println("t2imagenotnull");
					T2Image = importData.m_kT2Image;
					T2frame = importData.t2frame;
					//eddyCurReg.epiCheckBox.setEnabled(true);
				}
				else{
					MipavUtil.displayError("Error loading T2 image"); 
					tabbedPane.setSelectedIndex(1);
				}
			}
		}

		else if (command.equals("back1")){
			tabbedPane.setSelectedIndex(0);
			nextButton.setEnabled(true);
			goBackButton.setEnabled(false);
		}

		else if (command.equals("next2")){
			/*if (t2load.newB0DWIRegImage !=null){
                DWINewB0Image = t2load.newB0DWIRegImage;
                tabbedPane.setSelectedIndex(2);
                nextButton.setEnabled(false);
                goBackButton.setEnabled(true);
                goBackButton.setActionCommand("back2");
            }*/

			if (DTIPreprocessing.result35RegImage !=null){
				//System.out.println("result35 not null");
				DWINewB0Image = DTIPreprocessing.result35RegImage;
				currentImage = DWINewB0Image;
				arrayTransMatrix = DTIPreprocessing.arrayTransMatrix;
				//System.out.println("arraTransMat: " +arrayTransMatrix[0]);
				b0toStructMatrix = DTIPreprocessing.b0toStructMatrix;
				//System.out.println("b0toStructMatrix: "+b0toStructMatrix);
				tabbedPane.setSelectedIndex(2);
				nextButton.setEnabled(false);
				goBackButton.setEnabled(true);
				goBackButton.setActionCommand("back2");
			}

		}       

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

		final JPanel wholePanel = new JPanel();

	        final JPanel DTIloadPanel = new JPanel();
	        DTIloadPanel.setLayout(new GridBagLayout());
	        DTIloadPanel.setBorder(JInterfaceBase.buildTitledBorder(""));

	        final GridBagConstraints gbc = new GridBagConstraints();

	        gbc.gridx = 0;
	        gbc.gridy = 0;
	        gbc.gridwidth = 1;
	        gbc.gridheight = 1;
	        gbc.weightx = 0;
	        gbc.fill = GridBagConstraints.CENTER;
	        gbc.anchor = GridBagConstraints.WEST;

	        JButton openDTIimageButton = new JButton("Browse");
	        openDTIimageButton.setToolTipText("Browse mask image file");
	        openDTIimageButton.addActionListener(this);
	        openDTIimageButton.setActionCommand("browseMaskFile");
	        openDTIimageButton.setEnabled(true);

	        textDTIimage.setPreferredSize(new Dimension(275, 21));
	        textDTIimage.setEditable(true);
	        textDTIimage.setBackground(Color.white);
	        textDTIimage.setFont(MipavUtil.font12);

	        JLabel dtiFileLabel = new JLabel("Mask Image: ");
	        dtiFileLabel.setEnabled(true);

	        gbc.gridx = 0;
	        gbc.gridy = 0;
	        gbc.weightx = 1;
	        gbc.insets = new Insets(0, 0, 10, 0);
	        gbc.fill = GridBagConstraints.CENTER;

	        DTIloadPanel.add(dtiFileLabel, gbc);
	        gbc.gridx = 1;
	        gbc.gridy = 0;
	        gbc.weightx = 1;
	        gbc.fill = GridBagConstraints.CENTER;
	        DTIloadPanel.add(textDTIimage, gbc);
	        gbc.gridx = 2;
	        gbc.gridy = 0;
	        gbc.weightx = 1;
	        gbc.insets = new Insets(0, 10, 10, 0);
	        gbc.fill = GridBagConstraints.CENTER;
	        DTIloadPanel.add(openDTIimageButton, gbc);

	        wholePanel.add(DTIloadPanel);


		return wholePanel;

	}

	private JPanelDTIFiberTracking buildFiberTrackingPanel() {

		fiberTrack = new JPanelDTIFiberTracking(this);

		return fiberTrack;

	}

	private JPanel buildVisuzalizationPanel() {
		visualization = new JPanelDTIVisualization(this);
		return visualization;
	}



	/* 
	 * Called when the AlgorithmDWI2DTI completes and the new diffusion tensor image is ready.
	 */
	public void algorithmPerformed(final AlgorithmBase algorithm) {
		if ( algorithm instanceof AlgorithmDWI2DTI && algorithm.isCompleted() )
		{
			//tensorImage = EstimateTensorLLMSE.estimate( currentImage, true );
			tensorImage = ((AlgorithmDWI2DTI)algorithm).getDTI();
			// Set up the fiber tracking panel inputs:
			tabbedPane.setSelectedIndex(FIBER_TRACKING);
			fiberTrack.setInputImage( tensorImage );
			nextButton.setEnabled(true);
			// save the tensor image
			ModelImage.saveImage( tensorImage, tensorImage.getImageName() + ".xml", tensorImage.getImageDirectory() );
			currentImage = tensorImage;
			// delete intermediate images:
			((AlgorithmDWI2DTI)algorithm).deleteImages();
			if ( maskImage != null )
			{
				maskImage.disposeLocal();
				maskImage = null;
			}
		}
	}



	public void stateChanged(ChangeEvent e) {
		if ( e.getSource() == tabbedPane )
		{
			if ( tabbedPane.getSelectedIndex() == TENSOR_ESTIMATION )
			{
				if ( currentImage != null ) {
					nextButton.setEnabled(true);
				}
				else {
					nextButton.setEnabled(false);
				}
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


    private void loadMaskImage() {
        final JFileChooser chooser = new JFileChooser(new File(Preferences.getProperty(Preferences.PREF_IMAGE_DIR)));
        chooser.addChoosableFileFilter(new ViewImageFileFilter(ViewImageFileFilter.TECH));
        chooser.setDialogTitle("Choose Diffusion Tensor Color image file");
        final int returnValue = chooser.showOpenDialog(this);
        if (returnValue == JFileChooser.APPROVE_OPTION) {
            final FileIO fileIO = new FileIO();

            maskImage = fileIO.readImage(chooser.getSelectedFile().getName(), chooser.getCurrentDirectory()
                    + File.separator);

            textDTIimage.setText(chooser.getSelectedFile().getAbsolutePath());
            Preferences.setProperty(Preferences.PREF_IMAGE_DIR, chooser.getCurrentDirectory().toString());
        }
    }
}
