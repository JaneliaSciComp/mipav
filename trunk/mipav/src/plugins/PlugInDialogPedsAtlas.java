import java.awt.AWTException;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.ComponentOrientation;
import java.awt.Cursor;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.event.ActionEvent;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.ComponentListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.event.WindowEvent;
import java.awt.image.PixelGrabber;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.BitSet;
import java.util.Dictionary;
import java.util.Hashtable;
import java.util.concurrent.atomic.AtomicBoolean;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JToggleButton;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;
import javax.swing.border.EtchedBorder;
import javax.swing.border.TitledBorder;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ViewControlsImage;
import gov.nih.mipav.view.ViewJComponentDTIImage;
import gov.nih.mipav.view.ViewJComponentPedsAtlasIconImage;
import gov.nih.mipav.view.ViewJComponentPedsAtlasImage;
import gov.nih.mipav.view.ViewJFrameBase;
import gov.nih.mipav.view.ViewJFrameImage;
import gov.nih.mipav.view.ViewToolBarBuilder;
import gov.nih.mipav.view.ViewUserInterface;
import gov.nih.mipav.view.dialogs.JDialogBase;
import gov.nih.mipav.view.dialogs.JDialogWinLevel;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManagerInterface;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManagerInterfaceListener;

/**
 * This plugin is an atlas type plugin for the pediatrics group that displays annotations also
 * @author pandyan
 *
 */
public class PlugInDialogPedsAtlas extends ViewJFrameBase implements AlgorithmInterface,ChangeListener, MouseWheelListener, VOIManagerInterfaceListener {
	
	/** grid bag constraints **/
	private GridBagConstraints gbc;
	
	/** main panel **/
	private JPanel mainPanel,toolbarPanel,atlasPanel,imagePanel,modalitiesPanel,radioPanel,axialIconPanel,coronalIconPanel,sagittalIconPanel;
	
	/** scroll panel for image **/
	private JScrollPane imageScrollPanel;
	
	/** labels **/
	private JLabel sliceLabel, ageLabel, infoLabel;

	/** buttons * */
    private JButton magButton,unMagButton,zoomOneButton,saveButton,winLevelButton, resetButton, lutButton;
    
    /** modality buttons **/
    private JToggleButton t1Button, t2Button, pdButton, dtiButton, edtiButton;
    
    /** test button **/
    private JButton testButton, testToggleButton;
    
    /** ViewToolBarBuilder * */
    private ViewToolBarBuilder toolbarBuilder;
    
    /** sliders **/
    private JSlider sliceSlider,ageSlider;
    
    /** radio buttons **/
    private JRadioButton axialRadio, coronalRadio, sagittalRadio;

    /** radio group */
    private ButtonGroup radioGroup;
    
    /** dialog title **/
    private String title = "Peds Atlas                                                   ";

    /** icon images **/
    private ModelImage axialIconImage, coronalIconImage, sagittalIconImage;
    
    /** atlas images **/
    private ModelImage[] t1AtlasImages, t2AtlasImages, pdAtlasImages;
    
    /** atlas images to be disposed **/
    private ModelImage[] t1AtlasImagesToDispose, t2AtlasImagesToDispose, pdAtlasImagesToDispose;
    
    /** active image **/
    private ModelImage activeImage;
    
    /** component images **/
    private ViewJComponentPedsAtlasImage[] t1ComponentImages, t2ComponentImages, pdComponentImages;
    
    /** current component image **/
    private ViewJComponentPedsAtlasImage currentComponentImage;
    
    /** path strings to files **/
    private String[] axialT1PathStrings,axialT2PathStrings,axialPDPathStrings;
    
    /** path strings to files **/
    private String[] coronalT1PathStrings,coronalT2PathStrings,coronalPDPathStrings;
    
    /** path strings to files **/
    private String[] sagittalT1PathStrings,sagittalT2PathStrings,sagittalPDPathStrings;
    
    /** num age levels **/
    private int numAgeTicks;
    
    /** icon component image **/
    private ViewJComponentPedsAtlasIconImage axialIconComponentImage, coronalIconComponentImage, sagittalIconComponentImage;
    
    /** Buffer used to store image intensities the presently viewed slice of image A. */
    protected float[] imageBufferA,imageBufferC,imageBufferS;

    /**
     * Integer buffer (4 bytes that stores the concatenated Alpha (1 byte), Red (1 byte), Green ( 1 byte ), Blue (1 byte )
     * data. The ARGB values are generated by using the imageA intensities as a index into a LUT.
     */
    protected int[] pixBufferA,pixBufferC,pixBufferS;
    
    /** file io object **/
    private FileIO fileIO;
    
    /** current zoom **/
    private float currentZoom = 2.0f;
    
    /** current z slice **/
    private int currentZSlice;
    
    /** test: peds home **/
    private String pedsHome = "C:" + File.separator + "images" + File.separator;
    
    /** test icon image **/
    private File fCoronal = new File(pedsHome + "pedsAtlas" + File.separator + "icons" + File.separator + "coronal.jpg");
    
    /** test icon image **/
    private File fAxial = new File(pedsHome + "pedsAtlas" + File.separator + "icons" + File.separator + "axial.jpg");
    
    /** test icon image **/
    private File fSagittal = new File(pedsHome + "pedsAtlas" + File.separator + "icons" + File.separator + "sagittal.jpg");
    
    /** annotations file dir **/
    private String annotationsFileDir = pedsHome + "pedsAtlas" + File.separator + "annotations" + File.separator;
    
    /** coronal annotations file name **/
    private String coronalAnnotationsFilename = "coronal.lbl";
    
    /** current modality **/
    private String currentModality;
    
    /** current orientation **/
    private String currentOrientation;
    
    /** current age **/
    private int currentAge;
    
    /** num z slices **/
    private int numZSlices;
    
    /** image gb constraints **/
    private GridBagConstraints imageGBC = new GridBagConstraints();
    
    /** atlas image file **/
    private File atlasFile;
    
    /** thread to load images **/
    private Thread t1;
    
    /** File VOI **/
    private FileVOI fileVOI;
    
    /** labels for age slider **/
    private Hashtable<Integer,JLabel> ageLabelTable;
    
    /** labels for slice slider **/
    private Hashtable<Integer,JLabel> sliceLabelTable;
    
    /** icon width and height **/
    private int iconHeight, iconWidth;
    
    /** voi manager **/
    private VOIManagerInterface voiManager;
    
    /** initial image panel size **/
    private int initialImagePanelSize = 600;
    
    private boolean showAnnotationsToggle = true;
    
    private int testCounter = 0;

    
    /** constants **/
    public static final String AXIAL = "axial";
    public static final String CORONAL = "coronal";
    public static final String SAGITTAL = "sagittal";
    public static final String T1 = "t1";
    public static final String T2 = "t2";
    public static final String PD = "pd";
    public static final String DTI = "dti";
    public static final String EDTI = "edti";
    
    
    
    
    
    /** 
     * constructor
     */
	public PlugInDialogPedsAtlas() {
		super(null,null);
		currentOrientation = AXIAL;
		currentModality = T1;
		currentAge = 0;
		infoLabel = new JLabel("");
		ageLabelTable = new Hashtable<Integer,JLabel>();
		sliceLabelTable = new Hashtable<Integer,JLabel>();
		
		if (!readConfigFile()) {
			return;
		}
		
		//we will start plugin with axial
		//t1 = new Thread(new PopulateModelImages(this,AXIAL), "thread1");
		t1 = new PopulateModelImages(this,AXIAL);
		try {
			t1.start();
		}catch (Exception e) {
			e.printStackTrace();
			return;
		}

		boolean test = false;
		while(!test) {
			try{
				if(getT1ComponentImage(2) != null) {
					//all the z slices should be the same
					numZSlices = t1AtlasImages[0].getExtents()[2];
					String numZSlicesString = String.valueOf(numZSlices - 1);
					sliceLabelTable.put(0, new JLabel("0"));
					sliceLabelTable.put(numZSlices-1, new JLabel(numZSlicesString));
					currentZSlice = (t1AtlasImages[0].getExtents()[2] - 1) / 2;
					init();
					test = true;
					
					this.addComponentListener(new java.awt.event.ComponentAdapter() 
					{
						public void componentResized(ComponentEvent e)
						{
							if(imageScrollPanel.getVerticalScrollBar().isVisible() || imageScrollPanel.getHorizontalScrollBar().isVisible()) {
								saveButton.setEnabled(false);
							}else {
								saveButton.setEnabled(true);
							}
						}
					});

				}
			}catch(Exception e) {
				e.printStackTrace();
			}
		}
		
		
	}
	
	
	
	
	
	

	/**
	 * reads the config file
	 * @return
	 */
	public boolean readConfigFile() {
		boolean success = true;
		FileInputStream fis = null;
		BufferedReader d = null;
		try {
			String str;
	        fis = new FileInputStream(pedsHome + "pedsAtlas" + File.separator + "config" + File.separator + "config.txt");
	        d = new BufferedReader(new InputStreamReader(fis));
	        str =  d.readLine().trim();
	        numAgeTicks = Integer.parseInt(str);
	        
	        t1AtlasImages = new ModelImage[numAgeTicks];
	        t2AtlasImages = new ModelImage[numAgeTicks];
	        pdAtlasImages = new ModelImage[numAgeTicks];
	        t1AtlasImagesToDispose = new ModelImage[numAgeTicks];
	        t2AtlasImagesToDispose = new ModelImage[numAgeTicks];
	        pdAtlasImagesToDispose = new ModelImage[numAgeTicks];
	        t1ComponentImages = new ViewJComponentPedsAtlasImage[numAgeTicks];
	        t2ComponentImages = new ViewJComponentPedsAtlasImage[numAgeTicks];
	        pdComponentImages = new ViewJComponentPedsAtlasImage[numAgeTicks];

			axialT1PathStrings = new String[numAgeTicks];
			axialT2PathStrings = new String[numAgeTicks];
			axialPDPathStrings = new String[numAgeTicks];
			
			coronalT1PathStrings = new String[numAgeTicks];
			coronalT2PathStrings = new String[numAgeTicks];
			coronalPDPathStrings = new String[numAgeTicks];
			
			sagittalT1PathStrings = new String[numAgeTicks];
			sagittalT2PathStrings = new String[numAgeTicks];
			sagittalPDPathStrings = new String[numAgeTicks];

			
	        str = d.readLine().trim();
	        if(!str.equals("")) {
	        	return false;
	        }
	        for(int i=0;i<numAgeTicks;i++) {
	        	ageLabelTable.put(i,new JLabel(d.readLine().trim()));
	        }
	        str = d.readLine().trim();
	        if(!str.equals("")) {
	        	return false;
	        }
	        for(int i=0;i<numAgeTicks;i++) {
	        	str = d.readLine().trim();
	        	str = str.replace("\\", File.separator);
	        	axialT1PathStrings[i] = str;
	        }
	        str = d.readLine().trim();
	        if(!str.equals("")) {
	        	return false;
	        }
	        for(int i=0;i<numAgeTicks;i++) {
	        	str = d.readLine().trim();
	        	str = str.replace("\\", File.separator);
	        	axialT2PathStrings[i] = str;
	        }
	        str = d.readLine().trim();
	        if(!str.equals("")) {
	        	return false;
	        }
	        for(int i=0;i<numAgeTicks;i++) {
	        	str = d.readLine().trim();
	        	str = str.replace("\\", File.separator);
	        	axialPDPathStrings[i] = str;
	        }
	        
	        
	        
	        
	        
	        str = d.readLine().trim();
	        if(!str.equals("")) {
	        	return false;
	        }
	        for(int i=0;i<numAgeTicks;i++) {
	        	str = d.readLine().trim();
	        	str = str.replace("\\", File.separator);
	        	coronalT1PathStrings[i] = str;
	        }
	        str = d.readLine().trim();
	        if(!str.equals("")) {
	        	return false;
	        }
	        for(int i=0;i<numAgeTicks;i++) {
	        	str = d.readLine().trim();
	        	str = str.replace("\\", File.separator);
	        	coronalT2PathStrings[i] = str;
	        }
	        str = d.readLine().trim();
	        if(!str.equals("")) {
	        	return false;
	        }
	        for(int i=0;i<numAgeTicks;i++) {
	        	str = d.readLine().trim();
	        	str = str.replace("\\", File.separator);
	        	coronalPDPathStrings[i] = str;
	        }
	        
	        
	        
	        str = d.readLine().trim();
	        if(!str.equals("")) {
	        	return false;
	        }
	        for(int i=0;i<numAgeTicks;i++) {
	        	str = d.readLine().trim();
	        	str = str.replace("\\", File.separator);
	        	sagittalT1PathStrings[i] = str;
	        }
	        str = d.readLine().trim();
	        if(!str.equals("")) {
	        	return false;
	        }
	        for(int i=0;i<numAgeTicks;i++) {
	        	str = d.readLine().trim();
	        	str = str.replace("\\", File.separator);
	        	sagittalT2PathStrings[i] = str;
	        }
	        str = d.readLine().trim();
	        if(!str.equals("")) {
	        	return false;
	        }
	        for(int i=0;i<numAgeTicks;i++) {
	        	str = d.readLine().trim();
	        	str = str.replace("\\", File.separator);
	        	sagittalPDPathStrings[i] = str;
	        }
	        
	        
	        
	        
	        d.close();
	        fis.close();
	        
		}catch (Exception e) {
			e.printStackTrace();
			try {
				if(d!= null) {
					d.close();
				}
				if(fis != null) {
					fis.close();
				}
			}catch(Exception ex) {
			}
			return false;
		}
		
		return success;
	}

	/**
	 * init
	 */
	public void init() {
		setForeground(Color.black);
        setTitle(title + "zoom:" + currentZoom);
       
        mainPanel = new JPanel(new GridBagLayout());
        gbc = new GridBagConstraints();
        

        // toolbar panel
        toolbarPanel = new JPanel(new GridBagLayout());
        toolbarBuilder = new ViewToolBarBuilder(this);
        magButton = toolbarBuilder.buildButton("MagImage", "Magnify Image", "zoomin2");
        unMagButton = toolbarBuilder.buildButton("UnMagImage", "Un-Mag Image", "zoomout2");
        zoomOneButton = toolbarBuilder.buildButton("ZoomOne", "Magnify Image 1.0x", "zoom1");
        saveButton = toolbarBuilder.buildButton("Save", "Save screenshot of image", "camera");
        winLevelButton = toolbarBuilder.buildButton("WinLevel", "Window/Level", "winlevel");
        resetButton = toolbarBuilder.buildButton("resetLUT","Reset LUTs of all images in current modality","resetlut");
        lutButton = toolbarBuilder.buildButton("copyLUT", "Apply LUT of current image to all images in current modailty", "histolut");
        resetButton.setEnabled(false);
        
        JButton separator = new JButton(MipavUtil.getIcon("separator.gif"));
        separator.setMargin(new Insets(0, 0, 0, 0));
        separator.setBorderPainted(false);
        separator.setFocusPainted(false);
        
        lutButton.setEnabled(false);
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridx = 0;
        gbc.gridy = 0;
        toolbarPanel.add(magButton, gbc);
        gbc.gridx = 1;
        gbc.gridy = 0;
        toolbarPanel.add(unMagButton, gbc);
        gbc.gridx = 2;
        gbc.gridy = 0;
        toolbarPanel.add(zoomOneButton, gbc);
        gbc.gridx = 3;
        gbc.gridy = 0;
        
        

        toolbarPanel.add(separator,gbc);
        gbc.gridx = 4;
        toolbarPanel.add(saveButton,gbc);
        
        separator = new JButton(MipavUtil.getIcon("separator.gif"));
        separator.setMargin(new Insets(0, 0, 0, 0));
        separator.setBorderPainted(false);
        separator.setFocusPainted(false);
        
        gbc.gridx = 5;
        toolbarPanel.add(separator, gbc);
        gbc.gridx = 6;
        gbc.gridy = 0;
        toolbarPanel.add(winLevelButton, gbc);
        gbc.gridx = 7;
        gbc.gridy = 0;
        toolbarPanel.add(resetButton, gbc);
        gbc.gridx = 8;
        gbc.gridy = 0;
        toolbarPanel.add(lutButton, gbc);
        gbc.gridx = 9;
        toolbarPanel.add(infoLabel,gbc);
        
        gbc.anchor = GridBagConstraints.CENTER;


        //atlas panel
        gbc = new GridBagConstraints();
        atlasPanel = new JPanel(new GridBagLayout());
        sliceSlider = new JSlider(JSlider.VERTICAL, 0, numZSlices-1, currentZSlice);
        sliceSlider.setMajorTickSpacing(10);
        sliceSlider.setPaintTicks(true);
        sliceSlider.addChangeListener(this);
        //sliceSlider.setInverted(true);
        sliceLabelTable.put(currentZSlice, new JLabel(String.valueOf(currentZSlice)));
        sliceSlider.setLabelTable(sliceLabelTable);
        sliceSlider.setPaintLabels(true);
        //sliceSlider.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
        sliceLabel = new JLabel("Slice");
        ageSlider = new JSlider(JSlider.HORIZONTAL, 0, numAgeTicks-1, 0);
        ageSlider.setMajorTickSpacing(1);
        ageSlider.setPaintTicks(true);
        ageSlider.addChangeListener(this);
        //ageSlider.setInverted(true);
        ageSlider.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
        ageSlider.setLabelTable(ageLabelTable);
        ageSlider.setPaintLabels(true);
        ageSlider.setSnapToTicks(true);
        ageLabel = new JLabel("Age (months)");
        imagePanel = new JPanel(new GridBagLayout());
        
        imageGBC.anchor = GridBagConstraints.CENTER;
        TitledBorder titledBorder = new TitledBorder(new EtchedBorder(), "", TitledBorder.LEFT, TitledBorder.CENTER, MipavUtil.font12B, Color.black);
        imagePanel.setMinimumSize(new Dimension(initialImagePanelSize, initialImagePanelSize));
        imageScrollPanel = new JScrollPane(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        //imageScrollPanel.setPreferredSize(new Dimension(initialImagePanelSize, initialImagePanelSize));
        imageScrollPanel.setMinimumSize(new Dimension(initialImagePanelSize+200, initialImagePanelSize+200));
        imageScrollPanel.addMouseWheelListener(this);
        imageScrollPanel.setViewportView(imagePanel);
        
        //initialize and put the 0th axial t1 time slice in the imagePanel and call show
        currentComponentImage = t1ComponentImages[0];
        this.setImageA(t1AtlasImages[0]);
        currentComponentImage.setSlice(currentZSlice);
        currentComponentImage.setZoom(currentZoom, currentZoom);
        currentComponentImage.show(0,currentZSlice,null,null,true);
        imagePanel.add(currentComponentImage,imageGBC);

        modalitiesPanel = new JPanel();
        ButtonGroup group = new ButtonGroup();

        t1Button = new JToggleButton("T1",true);
        t1Button.addActionListener(this);
        t1Button.setActionCommand("t1");
        group.add(t1Button);
        t2Button = new JToggleButton("T2");
        t2Button.addActionListener(this);
        t2Button.setActionCommand("t2");
        group.add(t2Button);
        pdButton = new JToggleButton("PD");
        pdButton.addActionListener(this);
        pdButton.setActionCommand("pd");
        group.add(pdButton);
        dtiButton = new JToggleButton("DTI");
        dtiButton.addActionListener(this);
        dtiButton.setActionCommand("dti");
        group.add(dtiButton);
        edtiButton = new JToggleButton("EDTI");
        edtiButton.addActionListener(this);
        edtiButton.setActionCommand("edti");
        group.add(edtiButton);
        testButton = new JButton("Test");
        testButton.addActionListener(this);
        testButton.setActionCommand("test");
      
        modalitiesPanel.add(t1Button);
        modalitiesPanel.add(t2Button);
        modalitiesPanel.add(pdButton);

        //modalitiesPanel.add(dtiButton);
        //modalitiesPanel.add(edtiButton);
        
        //modalitiesPanel.add(testButton);
        
        
        //create radio buttons and icons
        radioGroup = new ButtonGroup();
        axialRadio = new JRadioButton();
        axialRadio.setSelected(true);
        axialRadio.addActionListener(this);
        axialRadio.setActionCommand("axial");
        coronalRadio = new JRadioButton();
        coronalRadio.setSelected(false);
        coronalRadio.addActionListener(this);
        coronalRadio.setActionCommand("coronal");
        sagittalRadio = new JRadioButton();
        sagittalRadio.setSelected(false);
        sagittalRadio.addActionListener(this);
        sagittalRadio.setActionCommand("sagittal");
        radioGroup.add(axialRadio);
        radioGroup.add(coronalRadio);
        radioGroup.add(sagittalRadio);
        axialIconPanel = new JPanel(new GridBagLayout());
        coronalIconPanel = new JPanel(new GridBagLayout());
        sagittalIconPanel = new JPanel(new GridBagLayout());
        coronalIconImage = fileIO.readImage(fCoronal.getName(), fCoronal.getParent() + File.separator, false, null);
        axialIconImage = fileIO.readImage(fAxial.getName(), fAxial.getParent() + File.separator, false, null);
        sagittalIconImage = fileIO.readImage(fSagittal.getName(), fSagittal.getParent() + File.separator, false, null);
        imageBufferC = initImageBuffer(coronalIconImage.getExtents(), true);
        pixBufferC = initPixelBuffer(coronalIconImage.getExtents());
        imageBufferA = initImageBuffer(axialIconImage.getExtents(), true);
        pixBufferA = initPixelBuffer(axialIconImage.getExtents());
        imageBufferS = initImageBuffer(sagittalIconImage.getExtents(), true);
        pixBufferS = initPixelBuffer(sagittalIconImage.getExtents());
        coronalIconComponentImage = new ViewJComponentPedsAtlasIconImage(this, coronalIconImage, null, imageBufferC, pixBufferC, 1, coronalIconImage.getExtents(), false, FileInfoBase.UNKNOWN_ORIENT, CORONAL);
        axialIconComponentImage = new ViewJComponentPedsAtlasIconImage(this, axialIconImage, null, imageBufferA, pixBufferA, 1, axialIconImage.getExtents(), false, FileInfoBase.UNKNOWN_ORIENT, AXIAL);
        sagittalIconComponentImage = new ViewJComponentPedsAtlasIconImage(this, sagittalIconImage, null, imageBufferS, pixBufferS, 1, sagittalIconImage.getExtents(), false, FileInfoBase.UNKNOWN_ORIENT, SAGITTAL);
        //axialG = axialIconComponentImage.getGraphics();
        iconHeight = axialIconComponentImage.getHeight();
        iconWidth = axialIconComponentImage.getWidth();

        coronalIconComponentImage.addMouseWheelListener(this);
        coronalIconComponentImage.setBuffers(imageBufferC, null, pixBufferC, null);
        axialIconComponentImage.addMouseWheelListener(this);
        axialIconComponentImage.setBuffers(imageBufferA, null, pixBufferA, null);
        sagittalIconComponentImage.addMouseWheelListener(this);
        sagittalIconComponentImage.setBuffers(imageBufferS, null, pixBufferS, null);
        int linePosition = (int)(((float)currentZSlice/numZSlices)*iconHeight);
        coronalIconComponentImage.show(0,0,null,null,true,linePosition,currentOrientation);
        axialIconComponentImage.show(0,0,null,null,true,linePosition,currentOrientation);
        sagittalIconComponentImage.show(0,0,null,null,true,linePosition,currentOrientation);
        
        
        //coronalG.drawLine(0, (int)((currentZSlice/numZSlices)*iconHeight), iconWidth, (int)((currentZSlice/numZSlices)*iconHeight));
        //coronalIconComponentImage.update(coronalG);
        
        GridBagConstraints gbc3 = new GridBagConstraints();
        gbc3.anchor = GridBagConstraints.CENTER;
        gbc3.gridx = 0;
        gbc3.gridy = 0;
        axialIconPanel.add(axialIconComponentImage,gbc3);
        gbc3.gridy = 1;
        axialIconPanel.add(new JLabel("Axial"),gbc3);
        
        gbc3 = new GridBagConstraints();
        gbc3.anchor = GridBagConstraints.CENTER;
        gbc3.gridx = 0;
        gbc3.gridy = 0;
        coronalIconPanel.add(coronalIconComponentImage,gbc3);
        gbc3.gridy = 1;
        coronalIconPanel.add(new JLabel("Coronal"),gbc3);
        
        gbc3 = new GridBagConstraints();
        gbc3.anchor = GridBagConstraints.CENTER;
        gbc3.gridx = 0;
        gbc3.gridy = 0;
        sagittalIconPanel.add(sagittalIconComponentImage,gbc3);
        gbc3.gridy = 1;
        sagittalIconPanel.add(new JLabel("Sagittal"),gbc3);
        
        
        
        radioPanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc2 = new GridBagConstraints();
        gbc2.gridx = 0;
        gbc2.gridy = 0;
        gbc2.anchor = GridBagConstraints.WEST;
        //gbc2.insets = new Insets(0,10,0,10);
        radioPanel.add(axialRadio,gbc2);
        gbc2.gridy = 1;
        radioPanel.add(coronalRadio,gbc2);
        gbc2.gridy = 2;
        radioPanel.add(sagittalRadio,gbc2);
        gbc2.gridx = 1;
        gbc2.gridy = 0;
        radioPanel.add(axialIconPanel,gbc2);
        gbc2.gridy = 1;
        radioPanel.add(coronalIconPanel,gbc2);
        gbc2.gridy = 2;
        radioPanel.add(sagittalIconPanel,gbc2);
        radioPanel.setMinimumSize(new Dimension(125,300));
        
        
        
        
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.gridx = 2;
        gbc.gridy = 0;
        
        atlasPanel.add(ageLabel,gbc);
        gbc.fill = GridBagConstraints.BOTH;
        gbc.gridy = 1;
        gbc.weightx = 1;
        gbc.weighty = 0;
        gbc.insets = new Insets(5,0,10,0);
        atlasPanel.add(ageSlider,gbc);
        gbc.gridx = 0;
        gbc.gridy = 2;
        gbc.weighty = 1;
        gbc.weightx = 0;
        gbc.insets = new Insets(0,10,0,5);
        atlasPanel.add(sliceLabel,gbc);
        gbc.gridx = 1;
        gbc.insets = new Insets(0,0,0,5);
        atlasPanel.add(sliceSlider,gbc);
        gbc.gridx = 2;
        gbc.weightx = 1;
        gbc.weighty = 1;
        gbc.insets = new Insets(0,10,0,5);
        atlasPanel.add(imageScrollPanel,gbc);
        gbc.gridx = 3;
        gbc.weightx = 0;
        gbc.insets = new Insets(0,0,0,15);
        atlasPanel.add(radioPanel,gbc);
        gbc.gridx = 2;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.weighty = 0;
        gbc.insets = new Insets(10,0,10,0);
        atlasPanel.add(modalitiesPanel,gbc);

        
        gbc = new GridBagConstraints();
        gbc.fill = GridBagConstraints.NONE;
        
        gbc.anchor = GridBagConstraints.WEST;
        //gbc.gridwidth = 1;
        gbc.gridx = 0;
        gbc.gridy = 0;
       
        gbc.weighty = 0;
        gbc.weightx = .5;
        mainPanel.add(toolbarPanel,gbc);
        gbc.anchor = GridBagConstraints.EAST;
        gbc.gridx = 1;
        
        mainPanel.add(infoLabel,gbc);
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 2;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.BOTH;
        mainPanel.add(atlasPanel,gbc);
        
        getContentPane().add(mainPanel);
        pack();
        this.setMinimumSize(new Dimension(840,840));
        //setResizable(false);
        setVisible(true);
        
  
      
        
   
	}
	
	
	
	

	public void stateChanged(ChangeEvent e) {
		 Object source = e.getSource();
		 if (source == sliceSlider) {
			 if(currentZSlice != 0 && currentZSlice != (numZSlices-1)) {
				 sliceLabelTable.remove(currentZSlice);
			 }

			 currentZSlice = sliceSlider.getValue();
			 sliceLabelTable.put(currentZSlice, new JLabel(String.valueOf(currentZSlice)));
			 sliceSlider.setLabelTable(sliceLabelTable);
			 sliceSlider.repaint();
			 currentComponentImage.setSlice(currentZSlice);
			 currentComponentImage.show(0,currentZSlice,null,null,true);
			  int linePosition;
			  int invZSlice = 0;;
			  
			 	//if(currentOrientation.equals(AXIAL)) {
			 		invZSlice = Math.abs(currentZSlice - numZSlices);
			 	//}
			 	
			 	if(currentOrientation.equals(AXIAL)) {
			 		linePosition = (int)(((float)invZSlice/numZSlices)*iconHeight);
			 	}else if(currentOrientation.equals(CORONAL)) {
			 		linePosition = (int)(((float)currentZSlice/numZSlices)*iconHeight);
			 	}else {
			 		linePosition = (int)(((float)currentZSlice/numZSlices)*iconHeight);
			 	}
			 	
		        coronalIconComponentImage.show(0,0,null,null,true,linePosition,currentOrientation);
		        sagittalIconComponentImage.show(0,0,null,null,true,linePosition,currentOrientation);
		        axialIconComponentImage.show(0,0,null,null,true,linePosition,currentOrientation);
		        
		 }else if(source == ageSlider) {
			 
			 
			 
			 boolean test = false;
				while(!test) {
					try{
						//t1.wait();
						ViewJComponentPedsAtlasImage compImg = null;
						if(currentModality.equals(T1)) {
							compImg = getT1ComponentImage(currentAge);
						 }else if(currentModality.equals(T2)) {
							 compImg = getT2ComponentImage(currentAge);
						 }else {
							 compImg = getPDComponentImage(currentAge);
						 }
					
						if(compImg != null) {
							currentComponentImage = compImg;
							setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
							
							imagePanel.removeAll();
							 
							 //this.repaint();
							 currentAge = ageSlider.getValue();
							 
							 currentComponentImage.setSlice(currentZSlice);
						     currentComponentImage.setZoom(currentZoom, currentZoom);
						     currentComponentImage.show(0,currentZSlice,null,null,true);
						     //imageGBC.anchor = GridBagConstraints.CENTER;
						     
						     if(currentModality.equals(T1)) {
						    	 this.setImageA(t1AtlasImages[currentAge]);
							 }else if(currentModality.equals(T2)) {
								 this.setImageA(t2AtlasImages[currentAge]);
							 }else {
								 this.setImageA(pdAtlasImages[currentAge]);
							 }
						     imagePanel.add(currentComponentImage,imageGBC);
						     imagePanel.validate();
						
							 this.repaint();
							
							test = true;
						}else {
							setCursor(new Cursor(Cursor.WAIT_CURSOR));
						}
						
						//t1.notify();
					}catch(Exception ex) {
						
					}
				}

		 }
		
	}


	
	
	/**
	 * action performed
	 */
	public void actionPerformed(ActionEvent e) {
		String command = e.getActionCommand();
		
		if(command.equals("t1")) {
			if(!currentModality.equals(T1)) {
				boolean test = false;
				while(!test) {
					try{
						//t1.wait();
						currentComponentImage = getT1ComponentImage(currentAge);
						if(currentComponentImage != null) {
							setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
							imagePanel.removeAll();
					        currentComponentImage.setSlice(currentZSlice);
					        currentComponentImage.setZoom(currentZoom, currentZoom);
					        currentComponentImage.show(0,currentZSlice,null,null,true);
					        imagePanel.add(currentComponentImage,imageGBC);
					        
					        imagePanel.validate();
							this.repaint();
							
							currentModality = T1;

						    this.setImageA(t1AtlasImages[currentAge]);
							 
							test = true;
						}else {
							setCursor(new Cursor(Cursor.WAIT_CURSOR));
						}
						
						//t1.notify();
					}catch(Exception ex) {
						
					}
				
				
				}
			}
			
		}else if(command.equals("t2")) {
			if(!currentModality.equals(T2)) {

				boolean test = false;
				while(!test) {
					try{
						currentComponentImage = getT2ComponentImage(currentAge);
						if(currentComponentImage != null) {
							setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
							imagePanel.removeAll();
					        currentComponentImage.setSlice(currentZSlice);
					        currentComponentImage.setZoom(currentZoom, currentZoom);
					        currentComponentImage.show(0,currentZSlice,null,null,true);
					        imagePanel.add(currentComponentImage,imageGBC);
					      
					        imagePanel.validate();
							this.repaint();
							
							currentModality = T2;
							this.setImageA(t2AtlasImages[currentAge]);
							test = true;
						}else {
							setCursor(new Cursor(Cursor.WAIT_CURSOR));
							this.repaint();
						}
					}catch(Exception ex) {
						
					}
				
				
				}
			}
		}else if(command.equals("pd")) {
			if(!currentModality.equals(PD)) {
				
				boolean test = false;
				while(!test) {
					try{
						currentComponentImage = getPDComponentImage(currentAge);
						if(currentComponentImage != null) {
							setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
							imagePanel.removeAll();
					        currentComponentImage.setSlice(currentZSlice);
					        currentComponentImage.setZoom(currentZoom, currentZoom);
					        currentComponentImage.show(0,currentZSlice,null,null,true);
					        imagePanel.add(currentComponentImage,imageGBC);

					        imagePanel.validate();
							this.repaint();
							
							currentModality = PD;
							this.setImageA(pdAtlasImages[currentAge]);
							test = true;
						}else {
							setCursor(new Cursor(Cursor.WAIT_CURSOR));
							this.repaint();
						}
					}catch(Exception ex) {
						
					}
				
				
				}

			}
		}else if(command.equals("MagImage")) {
			magImage();
			if(imageScrollPanel.getVerticalScrollBar().isVisible() || imageScrollPanel.getHorizontalScrollBar().isVisible()) {
				saveButton.setEnabled(false);
			}else {
				saveButton.setEnabled(true);
			}
		}else if(command.equals("UnMagImage")) {
			unMagImage();
			if(imageScrollPanel.getVerticalScrollBar().isVisible() || imageScrollPanel.getHorizontalScrollBar().isVisible()) {
				saveButton.setEnabled(false);
			}else {
				saveButton.setEnabled(true);
			}
		}else if(command.equals("ZoomOne")) {
			zoomOne();
			if(imageScrollPanel.getVerticalScrollBar().isVisible() || imageScrollPanel.getHorizontalScrollBar().isVisible()) {
				saveButton.setEnabled(false);
			}else {
				saveButton.setEnabled(true);
			}
		}else if(command.equals("WinLevel")) {
			ModelImage img = null;
			if(currentModality.equals(T1)) {
				img = t1AtlasImages[currentAge];
			}else if(currentModality.equals(T2)) {
				img = t2AtlasImages[currentAge];
			}else if(currentModality.equals(PD)) {
				img = pdAtlasImages[currentAge];
			}
			JDialogWinLevel winLevel = new JDialogWinLevel(this, img, currentComponentImage.getActiveLUT(), currentComponentImage.getActiveImageBuffer());
			if (winLevel.tabbedPane.getSelectedIndex() == 0) {
                final int lev = winLevel.levelSlider.getValue();
                final int win = winLevel.windowSlider.getValue();
                // needed this line below b/c otherwise the event was not getting invoked
                winLevel.levelSlider.setValue(lev + 1);

                winLevel.levelSlider.setValue(lev);
                winLevel.windowSlider.setValue(win);
            } else if (winLevel.tabbedPane.getSelectedIndex() == 1) {
                final int min = winLevel.minSlider.getValue();
                final int max = winLevel.maxSlider.getValue();

                // needed this line below b/c otherwise the event was not getting invoked
                winLevel.minSlider.setValue(min + 1);

                winLevel.minSlider.setValue(min);
                winLevel.maxSlider.setValue(max);
            }
		}else if(command.equals("resetLUT")){
			ModelImage img = null;
			if(currentModality.equals(T1)) {
		    	 img = t1AtlasImages[currentAge];
			 }else if(currentModality.equals(T2)) {
				 img = t2AtlasImages[currentAge];
			 }else {
				img = pdAtlasImages[currentAge];
			 }
			currentComponentImage.resetLUT(currentComponentImage.getActiveLUT(), img);
			img.notifyImageDisplayListeners( currentComponentImage.getActiveLUT(), true );
			
			
			
			
			if(currentModality.equals(T1)) {
				for(int i=0;i<t1ComponentImages.length;i++) {
					t1ComponentImages[i].resetLUT(t1ComponentImages[i].getActiveLUT(), t1AtlasImages[i]);
					t1AtlasImages[i].notifyImageDisplayListeners( t1ComponentImages[i].getActiveLUT(), true );
				}
			}else if(currentModality.equals(T2)) {
				for(int i=0;i<t2ComponentImages.length;i++) {
					t2ComponentImages[i].resetLUT(t2ComponentImages[i].getActiveLUT(), t2AtlasImages[i]);
					t2AtlasImages[i].notifyImageDisplayListeners( t2ComponentImages[i].getActiveLUT(), true );
				}
			}else if(currentModality.equals(PD)) {
				for(int i=0;i<pdComponentImages.length;i++) {
					pdComponentImages[i].resetLUT(pdComponentImages[i].getActiveLUT(), pdAtlasImages[i]);
					pdAtlasImages[i].notifyImageDisplayListeners( pdComponentImages[i].getActiveLUT(), true );
				}
			}
			
			
			
			
			
			
			//currentComponentImage.repaint();
			//repaint();
		}else if(command.equals("axial")) {
			if(!currentOrientation.equals(AXIAL)) {
				resetButton.setEnabled(false);
		        lutButton.setEnabled(false);
				nullifyStructures();
				if(t1.isAlive()) {
					((PopulateModelImages)t1).setIsInterrupted(true);
					
				}
				currentOrientation = AXIAL;
				setCursor(new Cursor(Cursor.WAIT_CURSOR));
				//t1 = new Thread(new PopulateModelImages(this,AXIAL), "thread1");
				t1 = new PopulateModelImages(this,AXIAL);
				try {
					t1.start();
				}catch (Exception ev) {
					ev.printStackTrace();
					return;
				}
				
				switchOrientations();
				setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
				
				 int linePosition;
				  int invZSlice = 0;;
				  
				 	if(currentOrientation.equals(AXIAL)) {
				 		invZSlice = Math.abs(currentZSlice - numZSlices);
				 	}
				 	
				 	if(currentOrientation.equals(AXIAL)) {
				 		linePosition = (int)(((float)invZSlice/numZSlices)*iconHeight);
				 	}else if(currentOrientation.equals(CORONAL)) {
				 		linePosition = (int)(((float)currentZSlice/numZSlices)*iconHeight);
				 	}else {
				 		linePosition = (int)(((float)currentZSlice/numZSlices)*iconHeight);
				 	}
			        coronalIconComponentImage.show(0,0,null,null,true,linePosition,currentOrientation);
			        sagittalIconComponentImage.show(0,0,null,null,true,linePosition,currentOrientation);
			        axialIconComponentImage.show(0,0,null,null,true,linePosition,currentOrientation);
				
			}
		}else if(command.equals("coronal")) {
			if(!currentOrientation.equals(CORONAL)) {
				resetButton.setEnabled(false);
		        lutButton.setEnabled(false);
				nullifyStructures();
				if(t1.isAlive()) {
					((PopulateModelImages)t1).setIsInterrupted(true);
					
				}
				currentOrientation = CORONAL;
				setCursor(new Cursor(Cursor.WAIT_CURSOR));
				//t1 = new Thread(new PopulateModelImages(this,CORONAL), "thread1");
				t1 = new PopulateModelImages(this,CORONAL);
				try {
					t1.start();
				}catch (Exception ev) {
					ev.printStackTrace();
					return;
				}
				switchOrientations();
				setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
				
				 int linePosition;
				  int invZSlice = 0;;
				  
				 	//if(currentOrientation.equals(AXIAL)) {
				 		invZSlice = Math.abs(currentZSlice - numZSlices);
				 	//}
				 	
				 		if(currentOrientation.equals(AXIAL)) {
					 		linePosition = (int)(((float)invZSlice/numZSlices)*iconHeight);
					 	}else if(currentOrientation.equals(CORONAL)) {
					 		linePosition = (int)(((float)currentZSlice/numZSlices)*iconHeight);
					 	}else {
					 		linePosition = (int)(((float)currentZSlice/numZSlices)*iconHeight);
					 	}
			        coronalIconComponentImage.show(0,0,null,null,true,linePosition,currentOrientation);
			        sagittalIconComponentImage.show(0,0,null,null,true,linePosition,currentOrientation);
			        axialIconComponentImage.show(0,0,null,null,true,linePosition,currentOrientation);
			        
			       
			        
			        
			}
			
		}else if(command.equals("sagittal")) {
			if(!currentOrientation.equals(SAGITTAL)) {
				resetButton.setEnabled(false);
		        lutButton.setEnabled(false);
				nullifyStructures();
				if(t1.isAlive()) {
					((PopulateModelImages)t1).setIsInterrupted(true);
					
				}
				currentOrientation = SAGITTAL;
				setCursor(new Cursor(Cursor.WAIT_CURSOR));
				//t1 = new Thread(new PopulateModelImages(this,SAGITTAL), "thread1");
				t1 = new PopulateModelImages(this,SAGITTAL);
				try {
					t1.start();
				}catch (Exception ev) {
					ev.printStackTrace();
					return;
				}
				
				switchOrientations();
				setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
				
				 int linePosition;
				  int invZSlice = 0;;
				  
				 	//if(currentOrientation.equals(AXIAL)) {
				 		invZSlice = Math.abs(currentZSlice - numZSlices);
				 	//}
				 	
				 		if(currentOrientation.equals(AXIAL)) {
					 		linePosition = (int)(((float)invZSlice/numZSlices)*iconHeight);
					 	}else if(currentOrientation.equals(CORONAL)) {
					 		linePosition = (int)(((float)currentZSlice/numZSlices)*iconHeight);
					 	}else {
					 		linePosition = (int)(((float)currentZSlice/numZSlices)*iconHeight);
					 	}
			        coronalIconComponentImage.show(0,0,null,null,true,linePosition,currentOrientation);
			        sagittalIconComponentImage.show(0,0,null,null,true,linePosition,currentOrientation);
			        axialIconComponentImage.show(0,0,null,null,true,linePosition,currentOrientation);
	
			}
		}else if(command.equals("test")) {
			if(currentOrientation.equals(CORONAL)) {
				try {
					if(testCounter != 1) {
			        	ModelImage img = null;
						if(currentModality.equals(T1)) {
							img = t1AtlasImages[currentAge];
						}else if(currentModality.equals(T2)) {
							img = t2AtlasImages[currentAge];
						}else if(currentModality.equals(PD)) {
							img = pdAtlasImages[currentAge];
						}
						fileVOI = new FileVOI(coronalAnnotationsFilename, annotationsFileDir,img);
						VOI[] vois = fileVOI.readVOI(true);
						for (int k = 0; k < vois.length; k++) {
							
							if(vois[k].getColor() == null) {
								vois[k].setColor(Color.white);
			                }
							vois[k].getGeometricCenter();	
							
							img.registerVOI(vois[k]);
							
				        }
	
						img.notifyImageDisplayListeners();
						
						testCounter++;
					}
					
					 try {
						 currentComponentImage.setDrawVOIs(showAnnotationsToggle);
				            currentComponentImage.paintComponent(currentComponentImage.getGraphics());
				            // componentImage.repaint(); // problems with this method on some machines seems to eat lots of memory on
				            // JVM 1.3
				        } catch (final OutOfMemoryError error) {
				            System.gc();
				        }
					imagePanel.validate();
					this.repaint();
					
					showAnnotationsToggle = !showAnnotationsToggle;

					//System.out.println(vois.length);
				}catch(IOException ef) {
					ef.printStackTrace();
				}
			}
			
			
		}else if(command.equals("copyLUT")) {
			if(currentModality.equals(T1)) {
				for(int i=0;i<t1ComponentImages.length;i++) {
					t1ComponentImages[i].setLUTa((ModelLUT)(currentComponentImage.getActiveLUT().clone()));
				}
			}else if(currentModality.equals(T2)) {
				for(int i=0;i<t2ComponentImages.length;i++) {
					t2ComponentImages[i].setLUTa((ModelLUT)(currentComponentImage.getActiveLUT().clone()));
				}
			}else if(currentModality.equals(PD)) {
				for(int i=0;i<pdComponentImages.length;i++) {
					pdComponentImages[i].setLUTa((ModelLUT)(currentComponentImage.getActiveLUT().clone()));
				}
			}
		}else if(command.equals("Save")) {
			writeImage();
		}

	}
	
	
	
	public void writeImage() {
		int[] extents = new int[2];
		int[] pixels;
        int bufferSize, xDim, yDim;
        short[] buffer = null;
        ModelImage screenCaptureImage = null;
        Image imagePix = null;
		try {
			Robot robot = new Robot();
	
			Point p = new Point();
            p.x = 0;
            p.y = 0;
            SwingUtilities.convertPointToScreen(p, currentComponentImage);
            
            Dimension d = new Dimension();
            d.width = currentComponentImage.getWidth();
            d.height = currentComponentImage.getHeight();
            Rectangle currentRectangle = new Rectangle(p, d);

            extents[0] = currentRectangle.width;
            extents[1] = currentRectangle.height;
            
            pixels = new int[extents[0] * extents[1]];
            bufferSize = extents[0] * extents[1];
            
            screenCaptureImage = new ModelImage(ModelStorageBase.UBYTE, extents, "screen capture");
            //screenCaptureImage.getFileInfo()[0].setFileDirectory(resultImage.getFileInfo(0).getFileDirectory());
            buffer = new short[bufferSize];
            
            // Turn off image slice #'s
            currentComponentImage.setShowSliceNumber(false);
            currentComponentImage.useHighlight(false);
            
            imagePix = robot.createScreenCapture(currentRectangle);

            PixelGrabber pgTest = new PixelGrabber(imagePix, 0, 0, extents[0], extents[1], pixels, 0, extents[0]);
            pgTest.grabPixels();
            
            for (int i = 0;i < (extents[0] * extents[1]);i++) {
                buffer[i] = (byte)pixels[i];
            }

            screenCaptureImage.importData(0, buffer, false);
            
            currentComponentImage.setShowSliceNumber(true);
            boolean endianness = currentComponentImage.getFrame().getImageA().getFileInfo(0).getEndianess();
            int modality = currentComponentImage.getFrame().getImageA().getFileInfo(0).getModality();
            float[] res = currentComponentImage.getFrame().getImageA().getFileInfo(0).getResolutions();
            float[] newRes = {res[0],res[1]};
            FileInfoImageXML fileInfo = new FileInfoImageXML(screenCaptureImage.getImageName(), null, FileUtility.XML);
            
            
            fileInfo.setDataType(ModelStorageBase.UBYTE);
            fileInfo.setEndianess(endianness);
            fileInfo.setExtents(extents);
            fileInfo.setModality(modality);
            fileInfo.setResolutions(newRes);
            
            FileInfoImageXML[] fileInfos = {fileInfo};
            screenCaptureImage.setFileInfo(fileInfos);
            screenCaptureImage.calcMinMax();
            
            new ViewJFrameImage(screenCaptureImage);

            
            
            
		}catch(Exception e) {
			e.printStackTrace();
		}
	}
	
	
	/**
	 * loads the image for when switching orientations
	 */
	public void switchOrientations() {
		boolean test = false;
		while(!test) {
			try{
				//t1.wait();
				if(currentAge == 0) {
					if(currentModality.equals(T1)) {
						if(getT1ComponentImage(2) != null) {
							imagePanel.removeAll();
							currentComponentImage = t1ComponentImages[0];
					        currentComponentImage.setSlice(currentZSlice);
					        currentComponentImage.setZoom(currentZoom, currentZoom);
					        currentComponentImage.show(0,currentZSlice,null,null,true);
					        imagePanel.add(currentComponentImage,imageGBC);
					        this.setImageA(t1AtlasImages[currentAge]);
					        imagePanel.validate();
					        this.validate();
							this.repaint();
							test = true;
						}
					}else if(currentModality.equals(T2)) {
						if(getT2ComponentImage(2) != null) {
							imagePanel.removeAll();
							currentComponentImage = t2ComponentImages[0];
					        currentComponentImage.setSlice(currentZSlice);
					        currentComponentImage.setZoom(currentZoom, currentZoom);
					        currentComponentImage.show(0,currentZSlice,null,null,true);
					        imagePanel.add(currentComponentImage,imageGBC);

					        this.setImageA(t2AtlasImages[currentAge]);
					        imagePanel.validate();
							this.repaint();
							
							test = true;
						}
					}else if(currentModality.equals(PD)) {
						if(getPDComponentImage(2) != null) {
							imagePanel.removeAll();
							currentComponentImage = pdComponentImages[0];
					        currentComponentImage.setSlice(currentZSlice);
					        currentComponentImage.setZoom(currentZoom, currentZoom);
					        currentComponentImage.show(0,currentZSlice,null,null,true);
					        imagePanel.add(currentComponentImage,imageGBC);

					        this.setImageA(pdAtlasImages[currentAge]);
					        imagePanel.validate();
							this.repaint();
							
							test = true;
						}
					}
				}else if(currentAge == numAgeTicks-1) {
					if(currentModality.equals(T1)) {
						if(getT1ComponentImage(currentAge) != null && getT1ComponentImage(currentAge-1) != null && getT1ComponentImage(currentAge-2) != null) {
							imagePanel.removeAll();
							currentComponentImage = t1ComponentImages[currentAge];
					        currentComponentImage.setSlice(currentZSlice);
					        currentComponentImage.setZoom(currentZoom, currentZoom);
					        currentComponentImage.show(0,currentZSlice,null,null,true);
					        imagePanel.add(currentComponentImage,imageGBC);

					        this.setImageA(t1AtlasImages[currentAge]);
					        imagePanel.validate();
							this.repaint();
							
							test = true;
						}
					}else if(currentModality.equals(T2)) {
						if(getT2ComponentImage(currentAge) != null && getT2ComponentImage(currentAge-1) != null && getT2ComponentImage(currentAge-2) != null) {
							imagePanel.removeAll();
							currentComponentImage = t2ComponentImages[currentAge];
					        currentComponentImage.setSlice(currentZSlice);
					        currentComponentImage.setZoom(currentZoom, currentZoom);
					        currentComponentImage.show(0,currentZSlice,null,null,true);
					        imagePanel.add(currentComponentImage,imageGBC);

					        this.setImageA(t2AtlasImages[currentAge]);
					        imagePanel.validate();
							this.repaint();
							
							test = true;
						}
					}else if(currentModality.equals(PD)) {
						if(getPDComponentImage(currentAge) != null && getPDComponentImage(currentAge-1) != null && getPDComponentImage(currentAge-2) != null) {
							imagePanel.removeAll();
							currentComponentImage = pdComponentImages[currentAge];
					        currentComponentImage.setSlice(currentZSlice);
					        currentComponentImage.setZoom(currentZoom, currentZoom);
					        currentComponentImage.show(0,currentZSlice,null,null,true);
					        imagePanel.add(currentComponentImage,imageGBC);

					        this.setImageA(pdAtlasImages[currentAge]);
					        imagePanel.validate();
							this.repaint();
							
							test = true;
						}
					}
				}else {
					if(currentModality.equals(T1)) {
						if(getT1ComponentImage(currentAge) != null && getT1ComponentImage(currentAge-1) != null && getT1ComponentImage(currentAge+1) != null) {
							imagePanel.removeAll();
							currentComponentImage = t1ComponentImages[currentAge];
					        currentComponentImage.setSlice(currentZSlice);
					        currentComponentImage.setZoom(currentZoom, currentZoom);
					        currentComponentImage.show(0,currentZSlice,null,null,true);
					        imagePanel.add(currentComponentImage,imageGBC);

					        this.setImageA(t1AtlasImages[currentAge]);
					        imagePanel.validate();
							this.repaint();
							
							test = true;
						}
					}else if(currentModality.equals(T2)) {
						if(getT2ComponentImage(currentAge) != null && getT2ComponentImage(currentAge-1) != null && getT2ComponentImage(currentAge+1) != null) {
							imagePanel.removeAll();
							currentComponentImage = t2ComponentImages[currentAge];
					        currentComponentImage.setSlice(currentZSlice);
					        currentComponentImage.setZoom(currentZoom, currentZoom);
					        currentComponentImage.show(0,currentZSlice,null,null,true);
					        imagePanel.add(currentComponentImage,imageGBC);

					        this.setImageA(t2AtlasImages[currentAge]);
					        imagePanel.validate();
							this.repaint();
							
							test = true;
						}
					}else if(currentModality.equals(PD)) {
						if(getPDComponentImage(currentAge) != null && getPDComponentImage(currentAge-1) != null && getPDComponentImage(currentAge+1) != null) {
							imagePanel.removeAll();
							currentComponentImage = pdComponentImages[currentAge];
					        currentComponentImage.setSlice(currentZSlice);
					        currentComponentImage.setZoom(currentZoom, currentZoom);
					        currentComponentImage.show(0,currentZSlice,null,null,true);
					        imagePanel.add(currentComponentImage,imageGBC);

					        this.setImageA(pdAtlasImages[currentAge]);
					        imagePanel.validate();
							this.repaint();
							
							test = true;
						}
					}
				}
				
				
				//t1.notify();
			}catch(Exception ex) {
				
			}
		}
	}

    /**
     * sets image zoom to 1
     */
    public void zoomOne() {
        if ( !unMagButton.isEnabled()) {
            unMagButton.setEnabled(true);
        }
        if ( !magButton.isEnabled()) {
            magButton.setEnabled(true);
        }
        float newZoom = 1;
        currentZoom = newZoom;
        //captureImageButton.setEnabled(true);
        currentComponentImage.setZoom(newZoom, newZoom);
        validate();
        validate();
        currentComponentImage.show(0,currentZSlice,null,null,true);
        
        
        setTitle(title + "zoom:" + currentZoom);
        
        imageScrollPanel.validate();
		
		 this.repaint();
    }
	
	
	
	
	/**
     * magnifies image
     */
    public void magImage() {
        if ( !unMagButton.isEnabled()) {
            unMagButton.setEnabled(true);
        }
        float newZoom;
        
        newZoom = currentZoom + 1.0f;
        
       
        currentZoom = newZoom;

        currentComponentImage.setZoom(newZoom, newZoom);
        validate();
        currentComponentImage.show(0,currentZSlice,null,null,true);
        
        
        setTitle(title + "zoom:" + currentZoom);
       
        if (currentZoom >= 32) {
            magButton.setEnabled(false);
        }
        
        imageScrollPanel.validate();
		
		 this.repaint();
    }
	
	
	/**
     * un-magnifies image
     */
    public void unMagImage() {
    	if(currentZoom <= 1.0f) {
    		return;
    	}
        if ( !magButton.isEnabled()) {
            magButton.setEnabled(true);
        }
        float newZoom;
        newZoom = currentZoom - 1.0f;
        
        currentZoom = newZoom;

        currentComponentImage.setZoom(newZoom, newZoom);
        validate();
        currentComponentImage.show(0,currentZSlice,null,null,true);
        
        
        setTitle(title + "zoom:" + currentZoom);
        
        if (currentComponentImage.getZoomX() <= 0.125) {
            unMagButton.setEnabled(false);
        }
        
        imageScrollPanel.validate();
        
		
		 this.repaint();
    }

	
	
	public void nullifyStructures() {
		//first nullify any existing ones
		for(int i=0;i<numAgeTicks;i++) {
			if(t1AtlasImages[i] != null) {
				t1AtlasImages[i].disposeLocal(false);
				t1AtlasImages[i] = null;
			}
		}
		for(int i=0;i<numAgeTicks;i++) {
			if(t2AtlasImages[i] != null) {
				t2AtlasImages[i].disposeLocal(false);
				t2AtlasImages[i] = null;
			}
		}
		for(int i=0;i<numAgeTicks;i++) {
			if(pdAtlasImages[i] != null) {
				pdAtlasImages[i].disposeLocal(false);
				pdAtlasImages[i] = null;
			}
		}
		for(int i=0;i<numAgeTicks;i++) {
				t1ComponentImages[i] = null;
		}
		for(int i=0;i<numAgeTicks;i++) {
				t2ComponentImages[i] = null;
		}
		for(int i=0;i<numAgeTicks;i++) {
				pdComponentImages[i] = null;
		}
		System.gc();
	}
	
	
	
	/**
	 * alg performed
	 */
	public void algorithmPerformed(AlgorithmBase algorithm) {
		// TODO Auto-generated method stub

	}
	
	
	/**
     * Create the intensity buffer for an image.
     * 
     * @param extents the extents of the image
     * @param isColor whether the image is in color
     * @return a buffer which is big enough to contain the image intensity data
     */
    protected static float[] initImageBuffer(int[] extents, boolean isColor) {
        int bufferFactor = 1;

        if (isColor) {
            bufferFactor = 4;
        }

        return new float[bufferFactor * extents[0] * extents[1]];
    }

    /**
     * Create the pixel buffer for an image.
     * 
     * @param extents the extents of the image
     * @return a buffer which is big enough to contain the image pixel data
     */
    protected static int[] initPixelBuffer(int[] extents) {
        return new int[extents[0] * extents[1]];
    }
    
    
    
    
    
    
    /**
     * Cleans up the frame before closing.
     * 
     * @param event the window event that triggered this method
     */
    public void windowClosing(final WindowEvent event) {
    	if(t1.isAlive()) {
			((PopulateModelImages)t1).setIsInterrupted(true);
			
		}
    	nullifyStructures();
    	ViewUserInterface.getReference().windowClosing(event);
        close();
    }
    
    
    
    
	

	 /**
     * mouse wheel moved
     * 
     * @param event
     */
    public void mouseWheelMoved(MouseWheelEvent event) {
        int wheelRotation = event.getWheelRotation();
        Object source = event.getSource();
        if (source == currentComponentImage || source == imageScrollPanel || source == sliceSlider) {
            if (wheelRotation > 0) {
                if (currentZSlice != 0) {
                	//currentZSlice = currentZSlice + 1;
                    sliceSlider.setValue(currentZSlice-1);
                }
            } else {
                if (currentZSlice != numZSlices - 1) {
                	//currentZSlice = currentZSlice - 1;
                    sliceSlider.setValue(currentZSlice+1);
                }
            }
        }
    }
	
	/**
     * set image b
     */
    public void setImageB(ModelImage imageB) {
    // TODO Auto-generated method stub

    }

	@Override
	public ViewControlsImage getControls() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public ModelImage getImageA() {
		// TODO Auto-generated method stub
		return imageA;
	}

	@Override
	public ModelImage getImageB() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public void removeControls() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setActiveImage(int active) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setAlphaBlend(int value) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setControls() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setEnabled(boolean flag) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setPaintBitmapSwitch(boolean flag) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setRGBTA(ModelRGB RGBT) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setRGBTB(ModelRGB RGBT) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setTitle() {
		// TODO Auto-generated method stub
		
	}

	@Override
	public boolean updateImageExtents() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public void setSlice(int slice) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void setTimeSlice(int tSlice) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public boolean updateImages() {
		// TODO Auto-generated method stub
		return false;
	}

	@Override
	public boolean updateImages(boolean flag) {
		// TODO Auto-generated method stub
		return false;
	}

	/**
     * This methods calls the componentImage's update method to redraw the screen.
     * 
     * @param LUTa LUT used to update imageA
     * @param LUTb LUT used to update imageB
     * @param forceShow forces show to re import image and calc. java image
     * @param interpMode image interpolation method (Nearest or Smooth)
     * 
     * @return boolean confirming successful update
     */
    public synchronized boolean updateImages(final ModelLUT LUTa, final ModelLUT LUTb, final boolean forceShow,
            final int interpMode) {

        if (currentComponentImage == null) {
            return false;
        }

        // redraw the paintBrushCursor (quick)
        currentComponentImage.updatePaintBrushCursor();

        if(currentComponentImage.show(0,currentZSlice,LUTa,LUTb,true) == false) {
        	return false;
        }


        // update the luts in this frame
        if (LUTa != null) {
            setLUTa(LUTa);
        }

        if (LUTb != null) {
            setLUTb(LUTb);
        }

        final ViewControlsImage myControls = getControls();

        if (myControls != null) {
            myControls.repaint();
        }

        return true;
    }
	
	
	
	
	/**
	 * get t1 comp image
	 * @param index
	 * @return
	 */
	public synchronized ViewJComponentPedsAtlasImage getT1ComponentImage(int index) {
		while(t1ComponentImages[index] == null) {
			try{
				wait();
			}catch(InterruptedException e) {
				//do nothing
			}
		}
		notify();
		
		
		return t1ComponentImages[index];
	}





	/**
	 * set t1 comp image
	 * @param t1ComponentImage
	 * @param index
	 */
	public synchronized void setT1ComponentImage(ViewJComponentPedsAtlasImage t1ComponentImage, int index) {
		this.t1ComponentImages[index] = t1ComponentImage;
		notify();
	}
	
	
	/**
	 * get t2 comp image
	 * @param index
	 * @return
	 */
	public synchronized ViewJComponentPedsAtlasImage getT2ComponentImage(int index) {
		while(t2ComponentImages[index] == null) {
			try{
				wait();
			}catch(InterruptedException e) {
				//do nothing
			}
		}
		notify();
		
		
		return t2ComponentImages[index];
	}





	/**
	 * set t2 comp image
	 * @param t2ComponentImage
	 * @param index
	 */
	public synchronized void setT2ComponentImage(ViewJComponentPedsAtlasImage t2ComponentImage, int index) {
		this.t2ComponentImages[index] = t2ComponentImage;
		notify();
	}
	
	
	/**
	 * get pd comp image
	 * @param index
	 * @return
	 */
	public synchronized ViewJComponentPedsAtlasImage getPDComponentImage(int index) {
		while(pdComponentImages[index] == null) {
			try{
				wait();
			}catch(InterruptedException e) {
				//do nothing
			}
		}
		notify();
		
		
		return pdComponentImages[index];
	}





	/**
	 * set pd comp image
	 * @param pdComponentImage
	 * @param index
	 */
	public synchronized void setPDComponentImage(ViewJComponentPedsAtlasImage pdComponentImage, int index) {
		this.pdComponentImages[index] = pdComponentImage;
		notify();
	}

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	
	
	/**
	 * This inner class is the thread that gets called
	 */
	public class PopulateModelImages extends Thread {
		PlugInDialogPedsAtlas owner;
		String orient;
		String[] pathStrings;
		int c = 1;
	    public boolean isInterrupted;
	    int currAge;

		
		public PopulateModelImages(PlugInDialogPedsAtlas owner, String orient) {
			this.owner = owner;
			this.orient = orient;
			isInterrupted = false;
		}
		
		
		public synchronized void setIsInterrupted(boolean flag) {
			isInterrupted = flag;
		}
		
		public synchronized boolean isInterrupted() {
			return isInterrupted;
			
		}
		
		public void run() {
			currAge = currentAge;
			long begTime = System.currentTimeMillis();
			populateModelImages(orient);
			if(isInterrupted()) {
				resetButton.setEnabled(false);
				lutButton.setEnabled(false);
			}else {
				resetButton.setEnabled(true);
				lutButton.setEnabled(true);
			}
			long endTime = System.currentTimeMillis();
	        long diffTime = endTime - begTime;
	        float seconds = ((float) diffTime) / 1000;
	        System.out.println("**Loading images took " + seconds + " seconds \n");
		}
		
		 private void initVOI(ModelImage img, ViewJComponentPedsAtlasImage comp)
		    {
			 	owner.setActiveImage(img);
		        voiManager = new VOIManagerInterface( owner, img, null, 1, false, null );
		        voiManager.getVOIManager(0).init( owner, img, null, comp, comp, comp.getOrientation(), comp.getSlice() );
		        comp.setVOIManager(voiManager.getVOIManager(0));
		    }
		
		
		public void populateModelImages(String orient) {
			fileIO = new FileIO();
			fileIO.setQuiet(true);
			

			if(orient.equals(AXIAL)) {
				
				infoLabel.setForeground(Color.red);
				loadAxialImages();
				infoLabel.setText("");
					
			}else if(orient.equals(CORONAL)) {
				
		
				loadCoronalImages();
				infoLabel.setText("");
			}else if(orient.equals(SAGITTAL)) {
				
				loadSagittalImages();
				infoLabel.setText("");
			}
			
			
		}

		
		public void loadAxialImages() {
			if(currentModality.equals(T1)) {
				if(isInterrupted()) {
					return;
				}
				loadAxialT1();
				if(isInterrupted()) {
					return;
				}
				loadAxialT2();
				if(isInterrupted()) {
					return;
				}
				loadAxialPD();
				
			}else if(currentModality.equals(T2)) {
				if(isInterrupted()) {
					return;
				}
				loadAxialT2();
				if(isInterrupted()) {
					return;
				}
				loadAxialT1();
				if(isInterrupted()) {
					return;
				}
				loadAxialPD();
				
			}else if(currentModality.equals(PD)) {
				if(isInterrupted()) {
					return;
				}
				loadAxialPD();
				if(isInterrupted()) {
					return;
				}
				loadAxialT1();
				if(isInterrupted()) {
					return;
				}
				loadAxialT2();
				
			}
			//to do ...DTI amd EDTI
		}
		
		public void loadAxialT1() {
			for(int i=currAge;i<numAgeTicks;i++) {
				if(isInterrupted()) {
					return;
				}
				String extraSpace = " ";
				if(c>=10) {
					extraSpace = "";
				}
				infoLabel.setText("Loading axial T1 images : " + extraSpace + c + "/" + numAgeTicks + " ");
				c++;
				if(c==12) {
					c=1;
				}
				atlasFile = new File(pedsHome + axialT1PathStrings[i]);
				t1AtlasImages[i] = fileIO.readImage(atlasFile.getName(), atlasFile.getParent() + File.separator, false, null);
				t1AtlasImages[i].addImageDisplayListener(owner);
				float[] imageBuffer = initImageBuffer(t1AtlasImages[i].getExtents(), true);
				int[] pixBuffer = initPixelBuffer(t1AtlasImages[i].getExtents());
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t1AtlasImages[i], null, imageBuffer, pixBuffer, 1, t1AtlasImages[i].getExtents(), false, FileInfoBase.AXIAL);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				initVOI(t1AtlasImages[i],comp);
				setT1ComponentImage(comp,i);
			}
			for(int i=currAge-1;i>=0;i--) {
				if(isInterrupted()) {
					return;
				}
				String extraSpace = " ";
				if(c>=10) {
					extraSpace = "";
				}
				infoLabel.setText("Loading axial T1 images : " + extraSpace + c + "/" + numAgeTicks + " ");
				c++;
				if(c==12) {
					c=1;
				}
				atlasFile = new File(pedsHome + axialT1PathStrings[i]);
				t1AtlasImages[i] = fileIO.readImage(atlasFile.getName(), atlasFile.getParent() + File.separator, false, null);
				t1AtlasImages[i].addImageDisplayListener(owner);
				float[] imageBuffer = initImageBuffer(t1AtlasImages[i].getExtents(), true);
				int[] pixBuffer = initPixelBuffer(t1AtlasImages[i].getExtents());
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t1AtlasImages[i], null, imageBuffer, pixBuffer, 1, t1AtlasImages[i].getExtents(), false, FileInfoBase.AXIAL);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				initVOI(t1AtlasImages[i],comp);
				setT1ComponentImage(comp,i);
			}
			
		}
		
		
		
		
		
		public void loadAxialT2() {
			for(int i=currAge;i<numAgeTicks;i++) {
				if(isInterrupted()) {
					return;
				}
				String extraSpace = " ";
				if(c>=10) {
					extraSpace = "";
				}
				infoLabel.setText("Loading axial T2 images : " + extraSpace + c + "/" + numAgeTicks + " ");
				c++;
				if(c==12) {
					c=1;
				}
				atlasFile = new File(pedsHome + axialT2PathStrings[i]);
				t2AtlasImages[i] = fileIO.readImage(atlasFile.getName(), atlasFile.getParent() + File.separator, false, null);
				t2AtlasImages[i].addImageDisplayListener(owner);
				float[] imageBuffer = initImageBuffer(t2AtlasImages[i].getExtents(), true);
				int[] pixBuffer = initPixelBuffer(t2AtlasImages[i].getExtents());
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t2AtlasImages[i], null, imageBuffer, pixBuffer, 1, t2AtlasImages[i].getExtents(), false, FileInfoBase.AXIAL);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				initVOI(t2AtlasImages[i],comp);
				setT2ComponentImage(comp,i);
			}
			for(int i=currAge-1;i>=0;i--) {
				if(isInterrupted()) {
					return;
				}
				String extraSpace = " ";
				if(c>=10) {
					extraSpace = "";
				}
				infoLabel.setText("Loading axial T2 images : " + extraSpace + c + "/" + numAgeTicks + " ");
				c++;
				if(c==12) {
					c=1;
				}
				atlasFile = new File(pedsHome + axialT2PathStrings[i]);
				t2AtlasImages[i] = fileIO.readImage(atlasFile.getName(), atlasFile.getParent() + File.separator, false, null);
				t2AtlasImages[i].addImageDisplayListener(owner);
				float[] imageBuffer = initImageBuffer(t2AtlasImages[i].getExtents(), true);
				int[] pixBuffer = initPixelBuffer(t2AtlasImages[i].getExtents());
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t2AtlasImages[i], null, imageBuffer, pixBuffer, 1, t2AtlasImages[i].getExtents(), false, FileInfoBase.AXIAL);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				initVOI(t2AtlasImages[i],comp);
				setT2ComponentImage(comp,i);
			}
		}

		public void loadAxialPD() {
			for(int i=currAge;i<numAgeTicks;i++) {
				if(isInterrupted()) {
					return;
				}
				String extraSpace = " ";
				if(c>=10) {
					extraSpace = "";
				}
				infoLabel.setText("Loading axial PD images : " + extraSpace + c + "/" + numAgeTicks + " ");
				c++;
				if(c==12) {
					c=1;
				}
				atlasFile = new File(pedsHome + axialPDPathStrings[i]);
				pdAtlasImages[i] = fileIO.readImage(atlasFile.getName(), atlasFile.getParent() + File.separator, false, null);
				pdAtlasImages[i].addImageDisplayListener(owner);
				float[] imageBuffer = initImageBuffer(pdAtlasImages[i].getExtents(), true);
				int[] pixBuffer = initPixelBuffer(pdAtlasImages[i].getExtents());
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, pdAtlasImages[i], null, imageBuffer, pixBuffer, 1, pdAtlasImages[i].getExtents(), false, FileInfoBase.AXIAL);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				initVOI(pdAtlasImages[i],comp);
				setPDComponentImage(comp,i);
			}
			for(int i=currAge-1;i>=0;i--) {
				if(isInterrupted()) {
					return;
				}
				String extraSpace = " ";
				if(c>=10) {
					extraSpace = "";
				}
				infoLabel.setText("Loading axial PD images : " + extraSpace + c + "/" + numAgeTicks + " ");
				c++;
				if(c==12) {
					c=1;
				}
				atlasFile = new File(pedsHome + axialPDPathStrings[i]);
				pdAtlasImages[i] = fileIO.readImage(atlasFile.getName(), atlasFile.getParent() + File.separator, false, null);
				pdAtlasImages[i].addImageDisplayListener(owner);
				float[] imageBuffer = initImageBuffer(pdAtlasImages[i].getExtents(), true);
				int[] pixBuffer = initPixelBuffer(pdAtlasImages[i].getExtents());
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, pdAtlasImages[i], null, imageBuffer, pixBuffer, 1, pdAtlasImages[i].getExtents(), false, FileInfoBase.AXIAL);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				initVOI(pdAtlasImages[i],comp);
				setPDComponentImage(comp,i);
			}
		}
		
		
		public void loadCoronalImages() {
			if(currentModality.equals(T1)) {
				if(isInterrupted()) {
					return;
				}
				loadCoronalT1();
				if(isInterrupted()) {
					return;
				}
				loadCoronalT2();
				if(isInterrupted()) {
					return;
				}
				loadCoronalPD();
				
			}else if(currentModality.equals(T2)) {
				if(isInterrupted()) {
					return;
				}
				loadCoronalT2();
				if(isInterrupted()) {
					return;
				}
				loadCoronalT1();
				if(isInterrupted()) {
					return;
				}
				loadCoronalPD();
				
			}else if(currentModality.equals(PD)) {
				if(isInterrupted()) {
					return;
				}
				loadCoronalPD();
				if(isInterrupted()) {
					return;
				}
				loadCoronalT1();
				if(isInterrupted()) {
					return;
				}
				loadCoronalT2();
				
			}
			//to do ...DTI amd EDTI
				
		}
		
		public void loadCoronalT1() {
			for(int i=currAge;i<numAgeTicks;i++) {
				if(isInterrupted()) {
					return;
				}
				String extraSpace = " ";
				if(c>=10) {
					extraSpace = "";
				}
				infoLabel.setText("Loading coronal T1 images : " + extraSpace + c + "/" + numAgeTicks + " ");
				c++;
				if(c==12) {
					c=1;
				}
				atlasFile = new File(pedsHome + coronalT1PathStrings[i]);
				t1AtlasImages[i] = fileIO.readImage(atlasFile.getName(), atlasFile.getParent() + File.separator, false, null);
				t1AtlasImages[i].addImageDisplayListener(owner);
				float[] imageBuffer = initImageBuffer(t1AtlasImages[i].getExtents(), true);
				int[] pixBuffer = initPixelBuffer(t1AtlasImages[i].getExtents());
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t1AtlasImages[i], null, imageBuffer, pixBuffer, 1, t1AtlasImages[i].getExtents(), false, FileInfoBase.CORONAL);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				initVOI(t1AtlasImages[i],comp);
				setT1ComponentImage(comp,i);
				//notify();
			}
			for(int i=currAge-1;i>=0;i--) {
				if(isInterrupted()) {
					return;
				}
				String extraSpace = " ";
				if(c>=10) {
					extraSpace = "";
				}
				infoLabel.setText("Loading coronal T1 images : " + extraSpace + c + "/" + numAgeTicks + " ");
				c++;
				if(c==12) {
					c=1;
				}
				atlasFile = new File(pedsHome + coronalT1PathStrings[i]);
				t1AtlasImages[i] = fileIO.readImage(atlasFile.getName(), atlasFile.getParent() + File.separator, false, null);
				t1AtlasImages[i].addImageDisplayListener(owner);
				try {
					fileVOI = new FileVOI(coronalAnnotationsFilename, annotationsFileDir,t1AtlasImages[i]);
					VOI[] vois = fileVOI.readVOI(true);
				}catch(IOException e) {
					e.printStackTrace();
				}
				float[] imageBuffer = initImageBuffer(t1AtlasImages[i].getExtents(), true);
				int[] pixBuffer = initPixelBuffer(t1AtlasImages[i].getExtents());
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t1AtlasImages[i], null, imageBuffer, pixBuffer, 1, t1AtlasImages[i].getExtents(), false, FileInfoBase.CORONAL);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				initVOI(t1AtlasImages[i],comp);
				setT1ComponentImage(comp,i);
				//notify();
			}
		}
		
		public void loadCoronalT2() {
			for(int i=currAge;i<numAgeTicks;i++) {
				if(isInterrupted()) {
					return;
				}
				String extraSpace = " ";
				if(c>=10) {
					extraSpace = "";
				}
				infoLabel.setText("Loading coronal T2 images : " + extraSpace + c + "/" + numAgeTicks + " ");
				c++;
				if(c==12) {
					c=1;
				}
				atlasFile = new File(pedsHome + coronalT2PathStrings[i]);
				t2AtlasImages[i] = fileIO.readImage(atlasFile.getName(), atlasFile.getParent() + File.separator, false, null);
				t2AtlasImages[i].addImageDisplayListener(owner);
				float[] imageBuffer = initImageBuffer(t2AtlasImages[i].getExtents(), true);
				int[] pixBuffer = initPixelBuffer(t2AtlasImages[i].getExtents());
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t2AtlasImages[i], null, imageBuffer, pixBuffer, 1, t2AtlasImages[i].getExtents(), false, FileInfoBase.CORONAL);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				initVOI(t2AtlasImages[i],comp);
				setT2ComponentImage(comp,i);
			}
			for(int i=currAge-1;i>=0;i--) {
				if(isInterrupted()) {
					return;
				}
				String extraSpace = " ";
				if(c>=10) {
					extraSpace = "";
				}
				infoLabel.setText("Loading coronal T2 images : " + extraSpace + c + "/" + numAgeTicks + " ");
				c++;
				if(c==12) {
					c=1;
				}
				atlasFile = new File(pedsHome + coronalT2PathStrings[i]);
				t2AtlasImages[i] = fileIO.readImage(atlasFile.getName(), atlasFile.getParent() + File.separator, false, null);
				t2AtlasImages[i].addImageDisplayListener(owner);
				float[] imageBuffer = initImageBuffer(t2AtlasImages[i].getExtents(), true);
				int[] pixBuffer = initPixelBuffer(t2AtlasImages[i].getExtents());
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t2AtlasImages[i], null, imageBuffer, pixBuffer, 1, t2AtlasImages[i].getExtents(), false, FileInfoBase.CORONAL);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				initVOI(t2AtlasImages[i],comp);
				setT2ComponentImage(comp,i);
			}
		}
		
		public void loadCoronalPD() {
			for(int i=currAge;i<numAgeTicks;i++) {
				if(isInterrupted()) {
					return;
				}
				String extraSpace = " ";
				if(c>=10) {
					extraSpace = "";
				}
				infoLabel.setText("Loading coronal PD images : " + extraSpace + c + "/" + numAgeTicks + " ");
				c++;
				if(c==12) {
					c=1;
				}
				atlasFile = new File(pedsHome + coronalPDPathStrings[i]);
				pdAtlasImages[i] = fileIO.readImage(atlasFile.getName(), atlasFile.getParent() + File.separator, false, null);
				pdAtlasImages[i].addImageDisplayListener(owner);
				float[] imageBuffer = initImageBuffer(pdAtlasImages[i].getExtents(), true);
				int[] pixBuffer = initPixelBuffer(pdAtlasImages[i].getExtents());
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, pdAtlasImages[i], null, imageBuffer, pixBuffer, 1, pdAtlasImages[i].getExtents(), false, FileInfoBase.CORONAL);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				initVOI(pdAtlasImages[i],comp);
				setPDComponentImage(comp,i);
			}
			for(int i=currAge-1;i>=0;i--) {
				if(isInterrupted()) {
					return;
				}
				String extraSpace = " ";
				if(c>=10) {
					extraSpace = "";
				}
				infoLabel.setText("Loading coronal PD images : " + extraSpace + c + "/" + numAgeTicks + " ");
				c++;
				if(c==12) {
					c=1;
				}
				atlasFile = new File(pedsHome + coronalPDPathStrings[i]);
				pdAtlasImages[i] = fileIO.readImage(atlasFile.getName(), atlasFile.getParent() + File.separator, false, null);
				pdAtlasImages[i].addImageDisplayListener(owner);
				float[] imageBuffer = initImageBuffer(pdAtlasImages[i].getExtents(), true);
				int[] pixBuffer = initPixelBuffer(pdAtlasImages[i].getExtents());
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, pdAtlasImages[i], null, imageBuffer, pixBuffer, 1, pdAtlasImages[i].getExtents(), false, FileInfoBase.CORONAL);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				initVOI(pdAtlasImages[i],comp);
				setPDComponentImage(comp,i);
			}
		
		}
		
		
		
		public void loadSagittalImages() {
			if(currentModality.equals(T1)) {
				if(isInterrupted()) {
					return;
				}
				loadSagittalT1();
				if(isInterrupted()) {
					return;
				}
				loadSagittalT2();
				if(isInterrupted()) {
					return;
				}
				loadSagittalPD();
				
			}else if(currentModality.equals(T2)) {
				if(isInterrupted()) {
					return;
				}
				loadSagittalT2();
				if(isInterrupted()) {
					return;
				}
				loadSagittalT1();
				if(isInterrupted()) {
					return;
				}
				loadSagittalPD();
				
			}else if(currentModality.equals(PD)) {
				if(isInterrupted()) {
					return;
				}
				loadSagittalPD();
				if(isInterrupted()) {
					return;
				}
				loadSagittalT1();
				if(isInterrupted()) {
					return;
				}
				loadSagittalT2();
				
			}
			//to do ...DTI amd EDTI
			
			
		}
		
		
		
		public void loadSagittalT1() {
			for(int i=currAge;i<numAgeTicks;i++) {
				if(isInterrupted()) {
					return;
				}
				String extraSpace = " ";
				if(c>=10) {
					extraSpace = "";
				}
				infoLabel.setText("Loading sagittal T1 images : " + extraSpace + c + "/" + numAgeTicks +  " ");
				c++;
				if(c==12) {
					c=1;
				}
				atlasFile = new File(pedsHome + sagittalT1PathStrings[i]);
				t1AtlasImages[i] = fileIO.readImage(atlasFile.getName(), atlasFile.getParent() + File.separator, false, null);
				t1AtlasImages[i].addImageDisplayListener(owner);
				float[] imageBuffer = initImageBuffer(t1AtlasImages[i].getExtents(), true);
				int[] pixBuffer = initPixelBuffer(t1AtlasImages[i].getExtents());
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t1AtlasImages[i], null, imageBuffer, pixBuffer, 1, t1AtlasImages[i].getExtents(), false, FileInfoBase.SAGITTAL);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				initVOI(t1AtlasImages[i],comp);
				setT1ComponentImage(comp,i);
				//notify();
			}
			for(int i=currAge-1;i>=0;i--) {
				if(isInterrupted()) {
					return;
				}
				String extraSpace = " ";
				if(c>=10) {
					extraSpace = "";
				}
				infoLabel.setText("Loading sagittal T1 images : " + extraSpace + c + "/" + numAgeTicks + " ");
				c++;
				if(c==12) {
					c=1;
				}
				atlasFile = new File(pedsHome + sagittalT1PathStrings[i]);
				t1AtlasImages[i] = fileIO.readImage(atlasFile.getName(), atlasFile.getParent() + File.separator, false, null);
				t1AtlasImages[i].addImageDisplayListener(owner);
				float[] imageBuffer = initImageBuffer(t1AtlasImages[i].getExtents(), true);
				int[] pixBuffer = initPixelBuffer(t1AtlasImages[i].getExtents());
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t1AtlasImages[i], null, imageBuffer, pixBuffer, 1, t1AtlasImages[i].getExtents(), false, FileInfoBase.SAGITTAL);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				initVOI(t1AtlasImages[i],comp);
				setT1ComponentImage(comp,i);
				//notify();
			}
		}
		
		
		
		public void loadSagittalT2() {
			for(int i=currAge;i<numAgeTicks;i++) {
				if(isInterrupted()) {
					return;
				}
				String extraSpace = " ";
				if(c>=10) {
					extraSpace = "";
				}
				infoLabel.setText("Loading sagittal T2 images : " + extraSpace + c + "/" + numAgeTicks + " ");
				c++;
				if(c==12) {
					c=1;
				}
				atlasFile = new File(pedsHome + sagittalT2PathStrings[i]);
				t2AtlasImages[i] = fileIO.readImage(atlasFile.getName(), atlasFile.getParent() + File.separator, false, null);
				t2AtlasImages[i].addImageDisplayListener(owner);
				float[] imageBuffer = initImageBuffer(t2AtlasImages[i].getExtents(), true);
				int[] pixBuffer = initPixelBuffer(t2AtlasImages[i].getExtents());
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t2AtlasImages[i], null, imageBuffer, pixBuffer, 1, t2AtlasImages[i].getExtents(), false, FileInfoBase.SAGITTAL);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				initVOI(t2AtlasImages[i],comp);
				setT2ComponentImage(comp,i);
			}
			for(int i=currAge-1;i>=0;i--) {
				if(isInterrupted()) {
					return;
				}
				String extraSpace = " ";
				if(c>=10) {
					extraSpace = "";
				}
				infoLabel.setText("Loading sagittal T2 images : " + extraSpace + c + "/" + numAgeTicks + " ");
				c++;
				if(c==12) {
					c=1;
				}
				atlasFile = new File(pedsHome + sagittalT2PathStrings[i]);
				t2AtlasImages[i] = fileIO.readImage(atlasFile.getName(), atlasFile.getParent() + File.separator, false, null);
				t2AtlasImages[i].addImageDisplayListener(owner);
				float[] imageBuffer = initImageBuffer(t2AtlasImages[i].getExtents(), true);
				int[] pixBuffer = initPixelBuffer(t2AtlasImages[i].getExtents());
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t2AtlasImages[i], null, imageBuffer, pixBuffer, 1, t2AtlasImages[i].getExtents(), false, FileInfoBase.SAGITTAL);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				initVOI(t2AtlasImages[i],comp);
				setT2ComponentImage(comp,i);
			}
		}
		
		
		
		public void loadSagittalPD() {
			for(int i=currAge;i<numAgeTicks;i++) {
				if(isInterrupted()) {
					return;
				}
				c++;
				if(c==12) {
					c=1;
				}
				String extraSpace = " ";
				if(c>=10) {
					extraSpace = "";
				}
				infoLabel.setText("Loading axial PD images : " + extraSpace + c + "/" + numAgeTicks + " ");
				atlasFile = new File(pedsHome + sagittalPDPathStrings[i]);
				pdAtlasImages[i] = fileIO.readImage(atlasFile.getName(), atlasFile.getParent() + File.separator, false, null);
				pdAtlasImages[i].addImageDisplayListener(owner);
				float[] imageBuffer = initImageBuffer(pdAtlasImages[i].getExtents(), true);
				int[] pixBuffer = initPixelBuffer(pdAtlasImages[i].getExtents());
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, pdAtlasImages[i], null, imageBuffer, pixBuffer, 1, pdAtlasImages[i].getExtents(), false, FileInfoBase.SAGITTAL);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				initVOI(pdAtlasImages[i],comp);
				setPDComponentImage(comp,i);
			}
			for(int i=currAge-1;i>=0;i--) {
				if(isInterrupted()) {
					return;
				}
				String extraSpace = " ";
				if(c>=10) {
					extraSpace = "";
				}
				infoLabel.setText("Loading axial PD images : " + extraSpace + c + "/" + numAgeTicks + " ");
				c++;
				if(c==12) {
					c=1;
				}
				atlasFile = new File(pedsHome + sagittalPDPathStrings[i]);
				pdAtlasImages[i] = fileIO.readImage(atlasFile.getName(), atlasFile.getParent() + File.separator, false, null);
				pdAtlasImages[i].addImageDisplayListener(owner);
				float[] imageBuffer = initImageBuffer(pdAtlasImages[i].getExtents(), true);
				int[] pixBuffer = initPixelBuffer(pdAtlasImages[i].getExtents());
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, pdAtlasImages[i], null, imageBuffer, pixBuffer, 1, pdAtlasImages[i].getExtents(), false, FileInfoBase.SAGITTAL);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				initVOI(pdAtlasImages[i],comp);
				setPDComponentImage(comp,i);
			}
		}
		
		
		
		
	}







	@Override
	public void PointerActive(boolean bActive) {
		// TODO Auto-generated method stub
		
	}







	@Override
	public Vector3f PropDown(int iActive) {
		// TODO Auto-generated method stub
		return null;
	}







	@Override
	public Vector3f PropUp(int iActive) {
		// TODO Auto-generated method stub
		return null;
	}







	@Override
	public void create3DVOI(boolean bIntersection) {
		// TODO Auto-generated method stub
		
	}







	@Override
	public void enableBoth(boolean bEnable) {
		// TODO Auto-generated method stub
		
	}







	@Override
	public ModelImage getActiveImage() {
		// TODO Auto-generated method stub
		return activeImage;
	}







	@Override
	public ModelLUT getActiveLUT() {
		// TODO Auto-generated method stub
		return null;
	}







	@Override
	public ModelRGB getActiveRGB() {
		// TODO Auto-generated method stub
		return null;
	}







	@Override
	public Vector3f getCenterPt() {
		// TODO Auto-generated method stub
		return null;
	}







	@Override
	public JFrame getFrame() {
		// TODO Auto-generated method stub
		return null;
	}







	@Override
	public VOIManagerInterface getVOIManager() {
		// TODO Auto-generated method stub
		return null;
	}







	@Override
	public void maskToPaint() {
		// TODO Auto-generated method stub
		
	}







	@Override
	public void paintToShortMask() {
		// TODO Auto-generated method stub
		
	}







	@Override
	public void paintToUbyteMask() {
		// TODO Auto-generated method stub
		
	}







	
	public void setActiveImage(ModelImage kImage) {
		this.activeImage = kImage;
		
	}







	@Override
	public void setCenter(Vector3f kCenter) {
		// TODO Auto-generated method stub
		
	}







	@Override
	public void setModified() {
		// TODO Auto-generated method stub
		
	}







	@Override
	public void setPaintMask(BitSet mask) {
		// TODO Auto-generated method stub
		
	}







	@Override
	public void updateData(boolean bCopyToCPU) {
		// TODO Auto-generated method stub
		
	}

}
