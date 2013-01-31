

import java.awt.Color;
import java.awt.ComponentOrientation;
import java.awt.Cursor;
import java.awt.Dimension;

import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Image;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.event.ActionEvent;


import java.awt.event.ComponentEvent;


import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.awt.event.WindowEvent;
import java.awt.image.PixelGrabber;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.Method;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.BitSet;
import java.util.Vector;

import java.util.Hashtable;

import java.util.zip.GZIPInputStream;

import javax.swing.ButtonGroup;
import javax.swing.JButton;

import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JToggleButton;
import javax.swing.ScrollPaneConstants;
import javax.swing.SwingUtilities;

import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import com.ice.tar.TarEntry;
import com.ice.tar.TarInputStream;

import WildMagic.LibFoundation.Mathematics.Vector3f;

import gov.nih.mipav.model.algorithms.AlgorithmBase;
import gov.nih.mipav.model.algorithms.AlgorithmInterface;
import gov.nih.mipav.model.file.FileIO;
import gov.nih.mipav.model.file.FileInfoBase;
import gov.nih.mipav.model.file.FileInfoImageXML;
import gov.nih.mipav.model.file.FileUtility;
import gov.nih.mipav.model.file.FileVOI;
import gov.nih.mipav.model.file.FileWriteOptions;
import gov.nih.mipav.model.structures.ModelImage;
import gov.nih.mipav.model.structures.ModelLUT;
import gov.nih.mipav.model.structures.ModelRGB;
import gov.nih.mipav.model.structures.ModelStorageBase;
import gov.nih.mipav.model.structures.VOI;
import gov.nih.mipav.view.MipavUtil;
import gov.nih.mipav.view.Preferences;
import gov.nih.mipav.view.ProgressChangeEvent;
import gov.nih.mipav.view.ViewControlsImage;
import gov.nih.mipav.view.ViewImageUpdateInterface;

import gov.nih.mipav.view.ViewJComponentPedsAtlasIconImage;
import gov.nih.mipav.view.ViewJFrameBase;

import gov.nih.mipav.view.ViewToolBarBuilder;
import gov.nih.mipav.view.ViewUserInterface;

import gov.nih.mipav.view.dialogs.JDialogText;
import gov.nih.mipav.view.dialogs.JDialogWinLevel;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManagerInterface;
import gov.nih.mipav.view.renderer.WildMagic.VOI.VOIManagerInterfaceListener;
import gov.nih.mipav.view.ViewJProgressBar;

/**
 * This plugin is an atlas type plugin for the pediatrics group that displays annotations also
 * @author pandyan
 *
 */
public class PlugInDialogPedsAtlas extends ViewJFrameBase implements AlgorithmInterface,ChangeListener, MouseWheelListener, VOIManagerInterfaceListener, MouseListener {
	

	







	/** grid bag constraints **/
	private GridBagConstraints gbc;
	
	/** main panel **/
	private JPanel mainPanel,toolbarPanel,atlasPanel,imagePanel,modalitiesPanel,togglePanel,axialIconPanel,coronalIconPanel,sagittalIconPanel;
	
	/** scroll panel for image **/
	private JScrollPane imageScrollPanel;
	
	/** labels **/
	private JLabel sliceLabel, ageLabel, infoLabel, opacityLabel, helpLabel;

	/** buttons * */
    private JButton magButton,unMagButton,zoomToScreenButton, zoomOneButton,saveButton,winLevelButton, resetButton, lutButton, presetButton, sep1,sep2, sep3;
    
    /** modality buttons **/
    private JToggleButton t1Button, t2Button, pdButton, dtiButton, edtiButton, displayAnnotationsButton, hideAnnotationsButton;
    
    /** test button **/
    private JButton testButton;
    
    /** ViewToolBarBuilder * */
    private ViewToolBarBuilder toolbarBuilder;
    
    /** sliders **/
    private JSlider sliceSlider,ageSlider, opacitySlider;
    
    /** radio buttons **/
    private JRadioButton axialRadio, coronalRadio, sagittalRadio;

    /** radio group */
    private ButtonGroup radioGroup;
    
    private ButtonGroup annotationsButtonGroup;
    
    /** dialog title **/
    private String title = "Peds Atlas                                                   ";

    /** icon images **/
    private ModelImage axialIconImage, coronalIconImage, sagittalIconImage;
    
    /** atlas images **/
    private ModelImage[] t1AtlasImages, t2AtlasImages, pdAtlasImages;
    
    private ModelImage[] maskImages;
    

    
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
    
    private String[] axialMaskPathStrings, coronalMaskPathStrings, sagittalMaskPathStrings;
    
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
    //private String pedsHome = "C:" + File.separator + "images" + File.separator;
    private String pedsHome = System.getProperty("user.home") + File.separator + "mipav" + File.separator + "pedsAtlas" + File.separator;
    
    
    
    /** test icon image **/
    private File fCoronal = new File(pedsHome + "icons" + File.separator + "coronal.jpg");
    
    /** test icon image **/
    private File fAxial = new File(pedsHome + "icons" + File.separator + "axial.jpg");
    
    /** test icon image **/
    private File fSagittal = new File(pedsHome + "icons" + File.separator + "sagittal.jpg");
    
    /** annotations file dir **/
    private String annotationsFileDir = pedsHome + "annotations" + File.separator;
    
    /** coronal annotations file name **/
    private String coronalAnnotationsFilename = "coronal.lbl";
    
    /** axial annotations file name **/
    private String axialAnnotationsFilename = "axial.lbl";
    
    /** sagittal annotations file name **/
    private String sagittalAnnotationsFilename = "sagittal.lbl";
    
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
    
    /** thread to download and extract images **/
    private Thread t1;
    
    /** thread to load images **/
    private Thread t2;
    
    /** File VOI **/
    private FileVOI fileVOI;
    
    /** labels for age slider **/
    private Hashtable<Integer,JLabel> ageLabelTable;
    
    /** labels for slice slider **/
    private Hashtable<Integer,JLabel> sliceLabelTable;
    
    /** labels for slice slider **/
    private Hashtable<Integer,JLabel> opacityLabelTable;
    
    /** icon width and height **/
    private int iconHeight;
    
    /** voi manager **/
    private VOIManagerInterface voiManager;
    
    /** initial image panel size **/
    private int initialXImagePanelSize = 600;
    
    private int initialYImagePanelSize;
    
    private boolean displayAnnotations = true;
    
    private boolean finishedLoading = false;
    
    private float aspectRatio;
    
    private DecimalFormat df = new DecimalFormat("0.00");
    
    //private int testCounter = 0;
    
    // lut presets
    public static float t1LevelPreset;
    public static float t1WindowPreset;
    public static float t2LevelPreset ;
    public static float t2WindowPreset ;
    public static float pdLevelPreset ;
    public static float pdWindowPreset;
	
    public float[] m_afXWin = new float[4];

    public float[] m_afYWin = new float[4];
	
	

    
    /** constants **/
    public static final String AXIAL = "axial";
    public static final String CORONAL = "coronal";
    public static final String SAGITTAL = "sagittal";
    public static final String T1 = "t1";
    public static final String T2 = "t2";
    public static final String PD = "pd";
    public static final String DTI = "dti";
    public static final String EDTI = "edti";
    
    
    /** booleans indicating what has been successfully downloaded and extracted **/
    private boolean extractedMisc = false;
    private boolean extractedAxialT1 = false;
    private boolean extractedAxialT2 = false;
    private boolean extractedAxialPD = false;
    private boolean extractedCoronalT1 = false;
    private boolean extractedCoronalT2 = false;
    private boolean extractedCoronalPD = false;
    private boolean extractedSagittalT1 = false;
    private boolean extractedSagittalT2 = false;
    private boolean extractedSagittalPD = false;
    
    
    private ViewJProgressBar progressBar;
    
    private ModelLUT lutb, luta;
    
    private int currentOpacity;
    
   
    
    
    
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
		opacityLabelTable = new Hashtable<Integer,JLabel>();
		
		m_afXWin[0] = 0;
	    m_afXWin[3] = 255;
	    m_afYWin[0] = 255;
	    m_afYWin[3] = 0;
	    
	    int[] dimExtentsLUT = new int[2];
        dimExtentsLUT[0] = 4;
        dimExtentsLUT[1] = 256;
        lutb = new ModelLUT(ModelLUT.STRIPED, 256, dimExtentsLUT);
        
        luta = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
        
        //lutb.oneToZeroLUTAdjust();
        
       // Color zeroIndexColor = lutb.getColor(0);

        // test to see if the color is R == 0, G == 0, B == 0
        //boolean zeroIndexColorIs000 = ( (zeroIndexColor.getRed() == 0) && (zeroIndexColor.getGreen() == 0) && (zeroIndexColor
          //      .getBlue() == 0));
        //boolean zeroIndexColorIs111 = ( (zeroIndexColor.getRed() == 1) && (zeroIndexColor.getGreen() == 1) && (zeroIndexColor
                //.getBlue() == 1));
        //if (zeroIndexColorIs111 == true) {
            lutb.setColor(0, new Color(0, 0, 0));
        //}
        
        
        //lutb.zeroToOneLUTAdjust();
	    
	    //Check to see if PedsHome exists...if it doesnt, download and extract images
	    File pedsHomeFile = new File(pedsHome);
	    if(!pedsHomeFile.exists()) {
	    	//start thread to download and extract images
	    	t1 = new DownloadAndExtractImages();
	    	try {
	    		t1.start();
	    	}catch (Exception e) {
				e.printStackTrace();
				return;
			}
	    }else {
	    	extractedMisc = true;
	    	extractedAxialT1 = true;
	    	extractedAxialT2 = true;
	    	extractedAxialPD = true;
	    	extractedCoronalT1 = true;
	    	extractedCoronalT2 = true;
	    	extractedCoronalPD = true;
	    	extractedSagittalT1 = true;
	    	extractedSagittalT2 = true;
	    	extractedSagittalPD = true;
	    	
	    	
	    }
	    
	    while(!isExtractedMisc()) {
	    	//do nothing until misc has been extracted
	    }
	    
		
		if (!readConfigFile()) {
			return;
		}
		
		//we will start plugin with axial
		//t1 = new Thread(new PopulateModelImages(this,AXIAL), "thread1");
		t2 = new PopulateModelImages(this,AXIAL);
		try {
			t2.start();
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
					opacityLabelTable.put(0, new JLabel("0"));
					opacityLabelTable.put(100, new JLabel("1"));
					currentZSlice = (t1AtlasImages[0].getExtents()[2] - 1) / 2;
					aspectRatio = (float)t1AtlasImages[0].getExtents()[0]/(float)t1AtlasImages[0].getExtents()[1];
					
					initialYImagePanelSize = (int)(initialXImagePanelSize/aspectRatio);
					init();
					test = true;
					
					this.addComponentListener(new java.awt.event.ComponentAdapter() 
					{
						public void componentResized(ComponentEvent e)
						{
							if(imageScrollPanel.getVerticalScrollBar().isVisible() || imageScrollPanel.getHorizontalScrollBar().isVisible()) {
								if(finishedLoading) {
									saveButton.setEnabled(false);
								}
							}else {
								if(finishedLoading) {
									saveButton.setEnabled(true);
								}
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
	        fis = new FileInputStream(pedsHome + "config" + File.separator + "config.txt");
	        d = new BufferedReader(new InputStreamReader(fis));
	        
	        str =  d.readLine().trim();
	        if(!str.equals("num of time points")) {
	        	return false;
	        }
	        
	        str =  d.readLine().trim();
	        if(!str.equals("")) {
	        	return false;
	        }
	        
	        str =  d.readLine().trim();
	        numAgeTicks = Integer.parseInt(str);
	        
	        str = d.readLine().trim();
	        if(!str.startsWith("---")) {
	        	return false;
	        }
	        
	        
	        
	        t1AtlasImages = new ModelImage[numAgeTicks];
	        t2AtlasImages = new ModelImage[numAgeTicks];
	        pdAtlasImages = new ModelImage[numAgeTicks];
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
			
			axialMaskPathStrings = new String[numAgeTicks];
			coronalMaskPathStrings = new String[numAgeTicks];
			sagittalMaskPathStrings = new String[numAgeTicks];
			
			maskImages = new ModelImage[numAgeTicks];
			
	        str = d.readLine().trim();
	        if(!str.equals("time labels")) {
	        	return false;
	        }
	        
	        str = d.readLine().trim();
	        if(!str.equals("")) {
	        	return false;
	        }
	        
	        for(int i=0;i<numAgeTicks;i++) {
	        	ageLabelTable.put(i,new JLabel(d.readLine().trim()));
	        }
	        
	        str = d.readLine().trim();
	        if(!str.startsWith("---")) {
	        	return false;
	        }
	        
	        
	        
	        str = d.readLine().trim();
	        if(!str.equals("T1 preset Level:Window")) {
	        	return false;
	        }
	        
	        str = d.readLine().trim();
	        if(!str.equals("")) {
	        	return false;
	        }
	        
	        str = d.readLine().trim();
	        String[] strs = str.split(":");
	        if(strs.length != 2) {
	        	return false;
	        }
	        t1LevelPreset = Float.parseFloat(strs[0]);
	        t1WindowPreset = Float.parseFloat(strs[1]);
	        
	        
	        if(t1LevelPreset < 0  || t1LevelPreset > 255) {
	        	MipavUtil.displayError("Invalid T1 Level value....must be between 0 and 255");
	        	return false;
	        }
	        if(t1WindowPreset < 0  || t1WindowPreset > 510) {
	        	MipavUtil.displayError("Invalid T1 Window value....must be between 0 and 510");
	        	return false;
	        }
	        
	        str = d.readLine().trim();
	        if(!str.startsWith("---")) {
	        	return false;
	        }
	        
	        
	        
	        str = d.readLine().trim();
	        if(!str.equals("T2 preset Level:Window")) {
	        	return false;
	        }
	        
	        str = d.readLine().trim();
	        if(!str.equals("")) {
	        	return false;
	        }
	        
	        str = d.readLine().trim();
	        strs = str.split(":");
	        if(strs.length != 2) {
	        	return false;
	        }
	        t2LevelPreset = Float.parseFloat(strs[0]);
	        t2WindowPreset = Float.parseFloat(strs[1]);
	        
	        if(t2LevelPreset < 0  || t2LevelPreset > 255) {
	        	MipavUtil.displayError("Invalid T2 Level value....must be between 0 and 255");
	        	return false;
	        }
	        if(t2WindowPreset < 0  || t2WindowPreset > 510) {
	        	MipavUtil.displayError("Invalid T2 Window value....must be between 0 and 510");
	        	return false;
	        }
	        
	        str = d.readLine().trim();
	        if(!str.startsWith("---")) {
	        	return false;
	        }
	        
	        
	        
	        
	        
	        str = d.readLine().trim();
	        if(!str.equals("PD preset Level:Window")) {
	        	return false;
	        }
	        
	        str = d.readLine().trim();
	        if(!str.equals("")) {
	        	return false;
	        }
	        
	        str = d.readLine().trim();
	        strs = str.split(":");
	        if(strs.length != 2) {
	        	return false;
	        }
	        pdLevelPreset = Float.parseFloat(strs[0]);
	        pdWindowPreset = Float.parseFloat(strs[1]);
	        
	        if(pdLevelPreset < 0  || pdLevelPreset > 255) {
	        	MipavUtil.displayError("Invalid PD Level value....must be between 0 and 255");
	        	return false;
	        }
	        if(pdWindowPreset < 0  || pdWindowPreset > 510) {
	        	MipavUtil.displayError("Invalid PD Window value....must be between 0 and 510");
	        	return false;
	        }
	        
	        str = d.readLine().trim();
	        if(!str.startsWith("---")) {
	        	return false;
	        }
	        
	        
	        str = d.readLine().trim();
	        if(!str.equals("Images")) {
	        	return false;
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
	        
	        str = d.readLine().trim();
	        if(!str.startsWith("---")) {
	        	return false;
	        }
	        
	        str = d.readLine().trim();
	        if(!str.equals("Mask Images")) {
	        	return false;
	        }
	        
	        str = d.readLine().trim();
	        if(!str.equals("")) {
	        	return false;
	        }
	        
	        for(int i=0;i<numAgeTicks;i++) {
	        	str = d.readLine().trim();
	        	str = str.replace("\\", File.separator);
	        	axialMaskPathStrings[i] = str;
	        }
	        
	        str = d.readLine().trim();
	        if(!str.equals("")) {
	        	return false;
	        }
	        
	        for(int i=0;i<numAgeTicks;i++) {
	        	str = d.readLine().trim();
	        	str = str.replace("\\", File.separator);
	        	coronalMaskPathStrings[i] = str;
	        }
	        
	        str = d.readLine().trim();
	        if(!str.equals("")) {
	        	return false;
	        }
	        
	        for(int i=0;i<numAgeTicks;i++) {
	        	str = d.readLine().trim();
	        	str = str.replace("\\", File.separator);
	        	sagittalMaskPathStrings[i] = str;
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
	
	
	public void setToolBarEnabled() {
		displayAnnotationsButton.setEnabled(true);
		hideAnnotationsButton.setEnabled(true);
		magButton.setEnabled(true);
		unMagButton.setEnabled(true);
		zoomToScreenButton.setEnabled(true);
		zoomOneButton.setEnabled(true);
		saveButton.setEnabled(true);
		winLevelButton.setEnabled(true);
		resetButton.setEnabled(true);
		presetButton.setEnabled(true);
		lutButton.setEnabled(true);
		sep1.setEnabled(true);
		sep2.setEnabled(true);
		sep3.setEnabled(true);
		
	}
	
	public void setToolBarDisabled() {
		displayAnnotationsButton.setEnabled(false);
		hideAnnotationsButton.setEnabled(false);
		magButton.setEnabled(false);
		unMagButton.setEnabled(false);
		zoomToScreenButton.setEnabled(false);
		zoomOneButton.setEnabled(false);
		saveButton.setEnabled(false);
		winLevelButton.setEnabled(false);
		resetButton.setEnabled(false);
		presetButton.setEnabled(false);
		lutButton.setEnabled(false);
		sep1.setEnabled(false);
		sep2.setEnabled(false);
		sep3.setEnabled(false);
	}

	/**
	 * init
	 */
	public void init() {
		setForeground(Color.black);
        
       
        mainPanel = new JPanel(new GridBagLayout());
        gbc = new GridBagConstraints();
        

        // toolbar panel
        toolbarPanel = new JPanel(new GridBagLayout());
        toolbarBuilder = new ViewToolBarBuilder(this);
        displayAnnotationsButton = toolbarBuilder.buildToggleButton("displayAnnotations", "Display Annotations", "text");
        hideAnnotationsButton = toolbarBuilder.buildToggleButton("hideAnnotations", "Hide Annotations", "clearcurrent");
        magButton = toolbarBuilder.buildButton("MagImage", "Magnify Image", "zoomin2");
        unMagButton = toolbarBuilder.buildButton("UnMagImage", "Un-Mag Image", "zoomout2");
        zoomToScreenButton = toolbarBuilder.buildButton("ZoomToScreen", "Fit To Window", "zoomToScreen");
        zoomOneButton = toolbarBuilder.buildButton("ZoomOne", "Magnify Image 1.0x", "zoom1");
        saveButton = toolbarBuilder.buildButton("Save", "Save screenshot of image", "camera");
        winLevelButton = toolbarBuilder.buildButton("WinLevel", "Window/Level", "winlevel");
        resetButton = toolbarBuilder.buildButton("resetLUT","Reset LUTs of all images in current modality","histolutReset");
        presetButton = toolbarBuilder.buildButton("presetLUT","Reload preset-LUTs of all images in current modality","histolut");
        lutButton = toolbarBuilder.buildButton("copyLUT", "Copy LUT of current image to all images in current modailty", "histolutCopy");
        //resetButton.setEnabled(false);
        //presetButton.setEnabled(false);
        helpLabel = new JLabel("<html><a href=\" \">help</a></html>");
        helpLabel.addMouseListener(this);
        
        annotationsButtonGroup = new ButtonGroup();
        annotationsButtonGroup.add(displayAnnotationsButton);
        annotationsButtonGroup.add(hideAnnotationsButton);
        displayAnnotationsButton.setSelected(true);
        
        
        
/*        JButton separator = new JButton(MipavUtil.getIcon("separator.gif"));
        separator.setMargin(new Insets(0, 0, 0, 0));
        separator.setBorderPainted(false);
        separator.setFocusPainted(false);*/
        //lutButton.setEnabled(false);
        gbc.anchor = GridBagConstraints.WEST;
        gbc.gridx = 0;
        gbc.gridy = 0;
        toolbarPanel.add(displayAnnotationsButton, gbc);
        
        gbc.gridx = 1;
        gbc.gridy = 0;
        toolbarPanel.add(hideAnnotationsButton, gbc);
        
        sep1 = new JButton(MipavUtil.getIcon("separator.gif"));
        sep1.setMargin(new Insets(0, 0, 0, 0));
        sep1.setBorderPainted(false);
        sep1.setFocusPainted(false);
        
        gbc.gridx = 2;
        gbc.gridy = 0;
        toolbarPanel.add(sep1,gbc);
        
        gbc.gridx = 3;
        gbc.gridy = 0;
        toolbarPanel.add(magButton, gbc);
        gbc.gridx = 4;
        gbc.gridy = 0;
        toolbarPanel.add(unMagButton, gbc);
        gbc.gridx = 5;
        gbc.gridy = 0;
        toolbarPanel.add(zoomOneButton, gbc);
        gbc.gridx = 6;
        gbc.gridy = 0;
        toolbarPanel.add(zoomToScreenButton, gbc);
        
        sep2 = new JButton(MipavUtil.getIcon("separator.gif"));
        sep2.setMargin(new Insets(0, 0, 0, 0));
        sep2.setBorderPainted(false);
        sep2.setFocusPainted(false);
        
        gbc.gridx = 7;
        gbc.gridy = 0;
        toolbarPanel.add(sep2,gbc);
        gbc.gridx = 8;
        toolbarPanel.add(saveButton,gbc);
        
        sep3 = new JButton(MipavUtil.getIcon("separator.gif"));
        sep3.setMargin(new Insets(0, 0, 0, 0));
        sep3.setBorderPainted(false);
        sep3.setFocusPainted(false);
        
        gbc.gridx = 9;
        toolbarPanel.add(sep3, gbc);
        gbc.gridx = 10;
        gbc.gridy = 0;
        toolbarPanel.add(winLevelButton, gbc);
        gbc.gridx = 11;
        gbc.gridy = 0;
        toolbarPanel.add(resetButton, gbc);
        gbc.gridx = 12;
        gbc.gridy = 0;
        toolbarPanel.add(presetButton, gbc);
        gbc.gridx = 13;
        gbc.gridy = 0;
        toolbarPanel.add(lutButton, gbc);
        //gbc.gridx = 14;
       // toolbarPanel.add(infoLabel,gbc);
        
        gbc.anchor = GridBagConstraints.CENTER;


        //atlas panel
        gbc = new GridBagConstraints();
        atlasPanel = new JPanel(new GridBagLayout());
        sliceSlider = new JSlider(JSlider.VERTICAL, 0, numZSlices-1, currentZSlice);
        sliceSlider.setMajorTickSpacing(10);
        sliceSlider.setPaintTicks(true);
        sliceSlider.addChangeListener(this);
        sliceSlider.addMouseWheelListener(this);
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
        
        opacitySlider = new JSlider(JSlider.HORIZONTAL, 0, 100, 50);
        opacitySlider.setMajorTickSpacing(10);
        opacitySlider.setPaintTicks(true);
        opacitySlider.addChangeListener(this);
        opacitySlider.setComponentOrientation(ComponentOrientation.LEFT_TO_RIGHT);
        currentOpacity = 50;
        opacityLabelTable.put(50, new JLabel(String.valueOf(currentOpacity/100f))); 
        opacitySlider.setLabelTable(opacityLabelTable);
        opacitySlider.setPaintLabels(true);
        
        
        ageLabel = new JLabel("Age (months)");
        opacityLabel = new JLabel("Opacity");
        imagePanel = new JPanel(new GridBagLayout());
        
        imageGBC.anchor = GridBagConstraints.CENTER;
       
        imagePanel.setMinimumSize(new Dimension(initialXImagePanelSize, initialYImagePanelSize));
        imageScrollPanel = new JScrollPane(ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED,ScrollPaneConstants.HORIZONTAL_SCROLLBAR_AS_NEEDED);
        imageScrollPanel.setPreferredSize(new Dimension(initialXImagePanelSize, initialYImagePanelSize));
        imageScrollPanel.setMinimumSize(new Dimension(initialXImagePanelSize+200, initialYImagePanelSize+200));
        imageScrollPanel.addMouseWheelListener(this);
        imageScrollPanel.setViewportView(imagePanel);
        
        //initialize and put the 0th axial t1 time slice in the imagePanel and call show
        currentComponentImage = t1ComponentImages[0];
        this.setImageA(t1AtlasImages[0]);
        
        
		 
        currentComponentImage.setSlice(currentZSlice);
        
        float newZoom = (float)(initialXImagePanelSize-15)/(float)currentComponentImage.getImageA().getExtents()[0];
		currentZoom = newZoom;
		setTitle(title + "zoom:" + df.format(currentZoom));
        
        currentComponentImage.setZoom(currentZoom, currentZoom);
        imagePanel.add(currentComponentImage,imageGBC);
        
        
        //File file = new File(pedsHome + axialMaskPathStrings[0]);
		//loadImage(file, currentComponentImage, false, false, false, 0, 0, 0, 0, true);

		
        setLUTb(lutb);
        setLUTa(luta);
        //setActiveImage(ViewJFrameBase.IMAGE_B);
        //displayMode = ViewJFrameBase.IMAGE_B;
        //setLUTb(lutb);
        
        //currentComponentImage.setActiveImage(ViewJFrameBase.IMAGE_B);
        //displayMode = ViewJFrameBase.IMAGE_B;
        //currentComponentImage.useHighlight(true);
        setImageB(maskImages[0]);
        //currentComponentImage.setAlpha(currentOpacity/100f);
        setAlphaBlend(currentOpacity);
        currentComponentImage.show(0,currentZSlice,null,lutb,true);



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
        //iconWidth = axialIconComponentImage.getWidth();

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
        
        
        
        togglePanel = new JPanel(new GridBagLayout());
        GridBagConstraints gbc2 = new GridBagConstraints();
        

        gbc2.anchor = GridBagConstraints.WEST;
        //gbc2.insets = new Insets(0,10,0,10);
        
        
        gbc2.gridx = 1;
        gbc2.gridy = 0;
        gbc2.fill = GridBagConstraints.BOTH;
        gbc2.weightx = 1;
        gbc2.insets = new Insets(10,0,10,0);
        togglePanel.add(t1Button,gbc2);
        gbc2.gridy = 1;
        togglePanel.add(t2Button,gbc2);
        gbc2.gridy = 2;
        gbc2.insets = new Insets(10,0,80,0);
        togglePanel.add(pdButton,gbc2);
        
        gbc2.insets = new Insets(0,0,0,0);
        gbc2.gridx = 0;
        gbc2.gridy = 3;
        togglePanel.add(axialRadio,gbc2);
        gbc2.gridy = 4;
        togglePanel.add(coronalRadio,gbc2);
        gbc2.gridy = 5;
        togglePanel.add(sagittalRadio,gbc2);
        gbc2.gridx = 1;
        gbc2.gridy = 3;
        togglePanel.add(axialIconPanel,gbc2);
        gbc2.gridy = 4;
        togglePanel.add(coronalIconPanel,gbc2);
        gbc2.gridy = 5;
        togglePanel.add(sagittalIconPanel,gbc2);
        togglePanel.setMinimumSize(new Dimension(125,300));
        
        
        
        
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.gridx = 2;
        gbc.gridy = 0;
        atlasPanel.add(ageLabel,gbc);
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.gridy = 4;
        gbc.insets = new Insets(10,0,10,0);
        atlasPanel.add(opacityLabel, gbc);
        
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
        atlasPanel.add(togglePanel,gbc);
        gbc.gridx = 2;
        gbc.gridy = 3;
        gbc.weightx = 1;
        gbc.weighty = 0;
        gbc.insets = new Insets(10,0,10,0);
        atlasPanel.add(opacitySlider,gbc);
         
        

        
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
        gbc.gridx = 2;
        gbc.insets = new Insets(10,10,10,5);
        mainPanel.add(helpLabel,gbc);
        gbc.weightx = 1;
        gbc.anchor = GridBagConstraints.CENTER;
        gbc.gridx = 0;
        gbc.gridy = 1;
        gbc.gridwidth = 2;
        gbc.weighty = 1;
        gbc.fill = GridBagConstraints.BOTH;
        mainPanel.add(atlasPanel,gbc);
        
        
        setToolBarDisabled();
        
        
        getContentPane().add(mainPanel);
        pack();
        this.setMinimumSize(this.getSize());
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
			 //setImageB(maskImages[currentAge]);
			 //currentComponentImage.setAlpha(currentOpacity/100f);
			 setAlphaBlend(currentOpacity);
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
		        
		 }else if(source == opacitySlider) {
			 if(currentOpacity != 0 && currentOpacity != 100) {
				 opacityLabelTable.remove(currentOpacity);
			 }
			 
			 
			 currentOpacity = opacitySlider.getValue();
			 opacityLabelTable.put(currentOpacity, new JLabel(String.valueOf(currentOpacity/100f)));
			 opacitySlider.setLabelTable(opacityLabelTable);
			 opacitySlider.repaint();

			 //currentComponentImage.setAlpha(currentOpacity/100f);
			 setAlphaBlend(currentOpacity);
		     currentComponentImage.show(0,currentZSlice,null,null,true);
			 
			 
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
						    
						     //imageGBC.anchor = GridBagConstraints.CENTER;
						     
						     if(currentModality.equals(T1)) {
						    	 this.setImageA(t1AtlasImages[currentAge]);
							 }else if(currentModality.equals(T2)) {
								 this.setImageA(t2AtlasImages[currentAge]);
							 }else {
								 this.setImageA(pdAtlasImages[currentAge]);
							 }

						     setImageB(maskImages[currentAge]);

						     //currentComponentImage.setAlpha(currentOpacity/100f);
						     setAlphaBlend(currentOpacity);
						     currentComponentImage.show(0,currentZSlice,null,lutb,true);
						     
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
					        
					        setImageB(maskImages[currentAge]);
					        //currentComponentImage.setAlpha(currentOpacity/100f);
					        setAlphaBlend(currentOpacity);
					        currentComponentImage.show(0,currentZSlice,null,lutb,true);
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
					        
					        setImageB(maskImages[currentAge]);
					        //currentComponentImage.setAlpha(currentOpacity/100f);
					        setAlphaBlend(currentOpacity);
					        currentComponentImage.show(0,currentZSlice,null,lutb,true);
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
					        
					        setImageB(maskImages[currentAge]);
					        //currentComponentImage.setAlpha(currentOpacity/100f);
					        setAlphaBlend(currentOpacity);
					        currentComponentImage.show(0,currentZSlice,null,lutb,true);
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
		}else if(command.equals("ZoomToScreen")) {
			zoomToScreen();
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
			if(currentComponentImage.getActiveLUT() != null) {
				currentComponentImage.resetLUT(currentComponentImage.getActiveLUT(), img);
				img.notifyImageDisplayListeners( currentComponentImage.getActiveLUT(), true );
			}
			
			
			
			
			if(currentModality.equals(T1)) {
				for(int i=0;i<t1ComponentImages.length;i++) {
					if(t1ComponentImages[i].getActiveLUT() != null) {
						t1ComponentImages[i].resetLUT(t1ComponentImages[i].getActiveLUT(), t1AtlasImages[i]);
						t1AtlasImages[i].notifyImageDisplayListeners( t1ComponentImages[i].getActiveLUT(), true );
					}
				}
			}else if(currentModality.equals(T2)) {
				for(int i=0;i<t2ComponentImages.length;i++) {
					if(t2ComponentImages[i].getActiveLUT() != null) {
						t2ComponentImages[i].resetLUT(t2ComponentImages[i].getActiveLUT(), t2AtlasImages[i]);
						t2AtlasImages[i].notifyImageDisplayListeners( t2ComponentImages[i].getActiveLUT(), true );
					}
				}
			}else if(currentModality.equals(PD)) {
				for(int i=0;i<pdComponentImages.length;i++) {
					if(pdComponentImages[i].getActiveLUT() != null) {
						pdComponentImages[i].resetLUT(pdComponentImages[i].getActiveLUT(), pdAtlasImages[i]);
						pdAtlasImages[i].notifyImageDisplayListeners( pdComponentImages[i].getActiveLUT(), true );
					}
				}
			}
			
			
			
			
			
			
			//currentComponentImage.repaint();
			//repaint();
		}else if(command.equals("presetLUT")) {
			loadPresetLUTS() ;
		}else if(command.equals("axial")) {
			if(!currentOrientation.equals(AXIAL)) {
				//resetButton.setEnabled(false);
				//presetButton.setEnabled(false);
		        //lutButton.setEnabled(false);
				setToolBarDisabled();
				nullifyStructures();
				if(t2.isAlive()) {
					((PopulateModelImages)t2).setIsInterrupted(true);
					
				}
				currentOrientation = AXIAL;
				setCursor(new Cursor(Cursor.WAIT_CURSOR));
				//t1 = new Thread(new PopulateModelImages(this,AXIAL), "thread1");
				t2 = new PopulateModelImages(this,AXIAL);
				try {
					t2.start();
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
				//resetButton.setEnabled(false);
				//presetButton.setEnabled(false);
		        //lutButton.setEnabled(false);
		        setToolBarDisabled();
				nullifyStructures();
				if(t2.isAlive()) {
					((PopulateModelImages)t2).setIsInterrupted(true);
					
				}
				currentOrientation = CORONAL;
				setCursor(new Cursor(Cursor.WAIT_CURSOR));
				//t1 = new Thread(new PopulateModelImages(this,CORONAL), "thread1");
				t2 = new PopulateModelImages(this,CORONAL);
				try {
					t2.start();
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
				//resetButton.setEnabled(false);
				//presetButton.setEnabled(false);
		        //lutButton.setEnabled(false);
		        setToolBarDisabled();
				nullifyStructures();
				if(t2.isAlive()) {
					((PopulateModelImages)t2).setIsInterrupted(true);
					
				}
				currentOrientation = SAGITTAL;
				setCursor(new Cursor(Cursor.WAIT_CURSOR));
				//t1 = new Thread(new PopulateModelImages(this,SAGITTAL), "thread1");
				t2 = new PopulateModelImages(this,SAGITTAL);
				try {
					t2.start();
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
			/*if(currentOrientation.equals(CORONAL)) {
				System.out.println("here");
				try {
					//if(testCounter != 1) {
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
						System.out.println(vois.length);
						for (int k = 0; k < vois.length; k++) {
							
							if(vois[k].getColor() == null) {
								vois[k].setColor(Color.white);
			                }
							vois[k].getGeometricCenter();	
							
							img.registerVOI(vois[k]);
							
				        }

						
	
						img.notifyImageDisplayListeners();
						
						//testCounter++;
					//}
					
					 try {
						 System.out.println(showAnnotationsToggle);
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
			
			*/
		}else if(command.equals("displayAnnotations")) {
			displayAnnotations = true;
			ViewJComponentPedsAtlasImage comp;
			for(int i=0;i<t1ComponentImages.length;i++) {
				comp = t1ComponentImages[i];
				comp.setDrawVOIs(true);
				imagePanel.validate();
				this.repaint();
			}
			for(int i=0;i<t2ComponentImages.length;i++) {
				comp = t2ComponentImages[i];
				comp.setDrawVOIs(true);
				imagePanel.validate();
				this.repaint();
				
			}
			for(int i=0;i<pdComponentImages.length;i++) {
				comp = pdComponentImages[i];
				comp.setDrawVOIs(true);
				imagePanel.validate();
				this.repaint();
			}
			
		}else if(command.equals("hideAnnotations")) {
			displayAnnotations = false;
			ViewJComponentPedsAtlasImage comp;
			for(int i=0;i<t1ComponentImages.length;i++) {
				comp = t1ComponentImages[i];
				comp.setDrawVOIs(false);
				imagePanel.validate();
				this.repaint();
			}
			for(int i=0;i<t2ComponentImages.length;i++) {
				comp = t2ComponentImages[i];
				comp.setDrawVOIs(false);
				imagePanel.validate();
				this.repaint();
				
			}
			for(int i=0;i<pdComponentImages.length;i++) {
				comp = pdComponentImages[i];
				comp.setDrawVOIs(false);
				imagePanel.validate();
				this.repaint();
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
			JFileChooser chooser = new JFileChooser();
            chooser.setDialogTitle("Save screen capture image as");
            String defaultName = currentComponentImage.getFrame().getImageNameA() + "_"+ currentModality + "_slice" + currentZSlice + ".jpg";
            File f = new File(defaultName);
            chooser.setSelectedFile(f);
            
            final int returnVal = chooser.showSaveDialog(null);
			
            if (returnVal == JFileChooser.APPROVE_OPTION) {
                String fileName = chooser.getSelectedFile().getName();
                if(!fileName.endsWith(".jpg")) {
                	MipavUtil.displayError("Screen capture images need to end in jpg");
                	return;
                }
                String fileDir = String.valueOf(chooser.getCurrentDirectory()) + File.separatorChar;

                writeImage(fileDir,fileName);
            } else {
                return;
            }
            
			//writeImage();
		}

	}
	
	
	
	
	
	
	public void writeImage(String fileDir, String filename) {
		int[] extents = new int[2];
		int[] pixels;
        int bufferSize;
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
            
            FileIO fileIO = new FileIO();
            fileIO.setQuiet(true);
            
            
            
            FileWriteOptions opts = new FileWriteOptions(true);
            opts.setIsScript(true);
            opts.setFileType(FileUtility.JPEG);
            opts.setFileDirectory(fileDir);
            opts.setFileName(filename);
            opts.setBeginSlice(0);
            opts.setOptionsSet(true);
            
            fileIO.writeImage(screenCaptureImage, opts);
            

            //new ViewJFrameImage(screenCaptureImage);

            
            
            
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
					        this.setImageA(t1AtlasImages[currentAge]);
					        setImageB(maskImages[currentAge]);
					        //currentComponentImage.setAlpha(currentOpacity/100f);
					        setAlphaBlend(currentOpacity);
					        currentComponentImage.show(0,currentZSlice,null,lutb,true);
					        imagePanel.add(currentComponentImage,imageGBC);
					        
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
					        this.setImageA(t2AtlasImages[currentAge]);
					        setImageB(maskImages[currentAge]);
					        //currentComponentImage.setAlpha(currentOpacity/100f);
					        currentComponentImage.setZoom(currentZoom, currentZoom);
					        setAlphaBlend(currentOpacity);
					        currentComponentImage.show(0,currentZSlice,null,lutb,true);
					        imagePanel.add(currentComponentImage,imageGBC);

					        
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
					        this.setImageA(pdAtlasImages[currentAge]);
					        setImageB(maskImages[currentAge]);
					        //currentComponentImage.setAlpha(currentOpacity/100f);
					        setAlphaBlend(currentOpacity);
					        currentComponentImage.show(0,currentZSlice,null,lutb,true);
					        imagePanel.add(currentComponentImage,imageGBC);

					        
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
					        this.setImageA(t1AtlasImages[currentAge]);
					        setImageB(maskImages[currentAge]);
					        //currentComponentImage.setAlpha(currentOpacity/100f);
					        setAlphaBlend(currentOpacity);
					        currentComponentImage.show(0,currentZSlice,null,lutb,true);
					        imagePanel.add(currentComponentImage,imageGBC);

					        
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
					        this.setImageA(t2AtlasImages[currentAge]);
					        setImageB(maskImages[currentAge]);
					        //currentComponentImage.setAlpha(currentOpacity/100f);
					        setAlphaBlend(currentOpacity);
					        currentComponentImage.show(0,currentZSlice,null,lutb,true);
					        imagePanel.add(currentComponentImage,imageGBC);

					        
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
					        this.setImageA(pdAtlasImages[currentAge]);
					        setImageB(maskImages[currentAge]);
					        //currentComponentImage.setAlpha(currentOpacity/100f);
					        setAlphaBlend(currentOpacity);
					        currentComponentImage.show(0,currentZSlice,null,lutb,true);
					        imagePanel.add(currentComponentImage,imageGBC);

					        
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
					        this.setImageA(t1AtlasImages[currentAge]);
					        setImageB(maskImages[currentAge]);
					        //currentComponentImage.setAlpha(currentOpacity/100f);
					        setAlphaBlend(currentOpacity);
					        currentComponentImage.show(0,currentZSlice,null,lutb,true);
					        imagePanel.add(currentComponentImage,imageGBC);

					        
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
					        this.setImageA(t2AtlasImages[currentAge]);
					        setImageB(maskImages[currentAge]);
					        //currentComponentImage.setAlpha(currentOpacity/100f);
					        setAlphaBlend(currentOpacity);
					        currentComponentImage.show(0,currentZSlice,null,lutb,true);
					        imagePanel.add(currentComponentImage,imageGBC);

					        
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
					        this.setImageA(pdAtlasImages[currentAge]);
					        setImageB(maskImages[currentAge]);
					        //currentComponentImage.setAlpha(currentOpacity/100f);
					        setAlphaBlend(currentOpacity);
					        currentComponentImage.show(0,currentZSlice,null,lutb,true);
					        imagePanel.add(currentComponentImage,imageGBC);

					        
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
	
	
	public void loadPresetLUTS() {
		if(currentModality.equals(T1)) {
			 for(int i=0;i<t1AtlasImages.length;i++) {
			    	float fLevel = t1LevelPreset;
			    	float fWindow = t1WindowPreset;
			    	float fMinImageWin = (float)t1AtlasImages[i].getMin();
			    	
			        float fMaxImageWin = (float)t1AtlasImages[i].getMax();
			        
			        m_afXWin[2] = fLevel + (fWindow / 2.0f);
			         m_afXWin[1] = fLevel - (fWindow / 2.0f);
			         m_afYWin[2] = m_afYWin[3];
			         m_afYWin[1] = m_afYWin[0];

			         if (m_afXWin[2] > fMaxImageWin) {
			             m_afYWin[2] = 255.0f * (m_afXWin[2] - fMaxImageWin) / fWindow;

			             if (m_afYWin[2] > 255.0f) {
			                 m_afYWin[2] = 255.0f;
			             }
			             m_afXWin[2] = fMaxImageWin;
			         }

			         if (m_afXWin[1] < fMinImageWin) {
			             m_afYWin[1] = 255.0f - (255.0f * (fMinImageWin - m_afXWin[1]) / fWindow);

			             if (m_afYWin[1] < 0.0f) {
			                 m_afYWin[1] = 0.0f;
			             }
			             m_afXWin[1] = fMinImageWin;
			         }

			         t1ComponentImages[i].getActiveLUT().getTransferFunction().importArrays( m_afXWin, m_afYWin, 4 );
			         t1AtlasImages[i].notifyImageDisplayListeners( t1ComponentImages[i].getActiveLUT(), true );
			    }
		}else if(currentModality.equals(T2)) {
			for(int i=0;i<t2AtlasImages.length;i++) {
		    	float fLevel = t2LevelPreset;
		    	float fWindow = t2WindowPreset;
		    	float fMinImageWin = (float)t2AtlasImages[i].getMin();
		    	
		        float fMaxImageWin = (float)t2AtlasImages[i].getMax();
		        
		        m_afXWin[2] = fLevel + (fWindow / 2.0f);
		         m_afXWin[1] = fLevel - (fWindow / 2.0f);
		         m_afYWin[2] = m_afYWin[3];
		         m_afYWin[1] = m_afYWin[0];

		         if (m_afXWin[2] > fMaxImageWin) {
		             m_afYWin[2] = 255.0f * (m_afXWin[2] - fMaxImageWin) / fWindow;

		             if (m_afYWin[2] > 255.0f) {
		                 m_afYWin[2] = 255.0f;
		             }
		             m_afXWin[2] = fMaxImageWin;
		         }

		         if (m_afXWin[1] < fMinImageWin) {
		             m_afYWin[1] = 255.0f - (255.0f * (fMinImageWin - m_afXWin[1]) / fWindow);

		             if (m_afYWin[1] < 0.0f) {
		                 m_afYWin[1] = 0.0f;
		             }
		             m_afXWin[1] = fMinImageWin;
		         }
		    	
		         
		         t2ComponentImages[i].getActiveLUT().getTransferFunction().importArrays( m_afXWin, m_afYWin, 4 );
		         t2AtlasImages[i].notifyImageDisplayListeners( t2ComponentImages[i].getActiveLUT(), true );
		    }
		}else if(currentModality.equals(PD)) {
		    for(int i=0;i<pdAtlasImages.length;i++) {
		    	float fLevel = pdLevelPreset;
		    	float fWindow = pdWindowPreset;
		    	float fMinImageWin = (float)pdAtlasImages[i].getMin();
		    	
		        float fMaxImageWin = (float)pdAtlasImages[i].getMax();
		        
		        m_afXWin[2] = fLevel + (fWindow / 2.0f);
		         m_afXWin[1] = fLevel - (fWindow / 2.0f);
		         m_afYWin[2] = m_afYWin[3];
		         m_afYWin[1] = m_afYWin[0];

		         if (m_afXWin[2] > fMaxImageWin) {
		             m_afYWin[2] = 255.0f * (m_afXWin[2] - fMaxImageWin) / fWindow;

		             if (m_afYWin[2] > 255.0f) {
		                 m_afYWin[2] = 255.0f;
		             }
		             m_afXWin[2] = fMaxImageWin;
		         }

		         if (m_afXWin[1] < fMinImageWin) {
		             m_afYWin[1] = 255.0f - (255.0f * (fMinImageWin - m_afXWin[1]) / fWindow);

		             if (m_afYWin[1] < 0.0f) {
		                 m_afYWin[1] = 0.0f;
		             }
		             m_afXWin[1] = fMinImageWin;
		         }
		    	
		         
		         pdComponentImages[i].getActiveLUT().getTransferFunction().importArrays( m_afXWin, m_afYWin, 4 );
		         pdAtlasImages[i].notifyImageDisplayListeners( pdComponentImages[i].getActiveLUT(), true );
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
        //currentComponentImage.setAlpha(currentOpacity/100f);
        setAlphaBlend(currentOpacity);
        currentComponentImage.show(0,currentZSlice,null,null,true);
        
        
        setTitle(title + "zoom:" + df.format(currentZoom));
        
        imageScrollPanel.validate();
		
		 this.repaint();
    }
    
    
    
    public void zoomToScreen() {
    	if ( !unMagButton.isEnabled()) {
            unMagButton.setEnabled(true);
        }
    	if(imageScrollPanel.getVerticalScrollBar().isVisible() || imageScrollPanel.getHorizontalScrollBar().isVisible()) {
    		//need to do this in order to get proper fit to screen
    		zoomOne();
    	}
    	
    	int width = imageScrollPanel.getViewport().getWidth();
    	int height = imageScrollPanel.getViewport().getHeight();
    	int imageHeight = currentComponentImage.getImageA().getExtents()[1];
    	int imageWidth = currentComponentImage.getImageA().getExtents()[0];
    	float imageAspectRatio = (float)imageWidth/(float)imageHeight;
    	float viewportAspectRatio = (float)width/(float)height;
    	
    	
    	if(viewportAspectRatio <= imageAspectRatio) {
    		float newZoom = (float)(width-15)/(float)imageWidth;
    		currentZoom = newZoom;

            currentComponentImage.setZoom(newZoom, newZoom);
            validate();
            //currentComponentImage.setAlpha(currentOpacity/100f);
            setAlphaBlend(currentOpacity);
            currentComponentImage.show(0,currentZSlice,null,null,true);

            setTitle(title + "zoom:" + df.format(currentZoom));
           
            if (currentZoom >= 32) {
                magButton.setEnabled(false);
            }
            
            imageScrollPanel.validate();
    		
    		 this.repaint();
    		
    		
    	}else {
    		float newZoom = (float)(height-15)/(float)imageHeight;
    		currentZoom = newZoom;

            currentComponentImage.setZoom(newZoom, newZoom);
            validate();
            //currentComponentImage.setAlpha(currentOpacity/100f);
            setAlphaBlend(currentOpacity);
            currentComponentImage.show(0,currentZSlice,null,null,true);
            
            
            setTitle(title + "zoom:" + df.format(currentZoom));
           
            if (currentZoom >= 32) {
                magButton.setEnabled(false);
            }
            
            imageScrollPanel.validate();
    		
    		 this.repaint();
    	}
    	
    }
	
	
	
	
	/**
     * magnifies image
     */
    public void magImage() {
        if ( !unMagButton.isEnabled()) {
            unMagButton.setEnabled(true);
        }
        float newZoom;
        currentZoom = (float)Math.floor(currentZoom);
        newZoom = currentZoom + 1.0f;
        
       
        currentZoom = newZoom;

        currentComponentImage.setZoom(newZoom, newZoom);
        validate();
        //currentComponentImage.setAlpha(currentOpacity/100f);
        setAlphaBlend(currentOpacity);
        currentComponentImage.show(0,currentZSlice,null,null,true);
        
        
        setTitle(title + "zoom:" + df.format(currentZoom));
       
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
        if(currentZoom%1 != 0) {
        	currentZoom = (float)Math.floor(currentZoom) + 1.0f;
        }
        
        newZoom = currentZoom - 1.0f;
        
        currentZoom = newZoom;

        currentComponentImage.setZoom(newZoom, newZoom);
        validate();
        //currentComponentImage.setAlpha(currentOpacity/100f);
        setAlphaBlend(currentOpacity);
        currentComponentImage.show(0,currentZSlice,null,null,true);
        
        
        setTitle(title + "zoom:" + df.format(currentZoom));
        
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
			if(maskImages[i] != null) {
				maskImages[i].disposeLocal(false);
				maskImages[i] = null;
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
    	if(t2.isAlive()) {
			((PopulateModelImages)t2).setIsInterrupted(true);
			
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
    /**
     * Sets the reference to imageB.
     * 
     * @param _imageB image to set the frame to
     */
    public void setImageB(final ModelImage _imageB) {

        final Vector<ViewImageUpdateInterface> frameList = imageA.getImageFrameVector();
        float min, max;

        if (frameList == null) {
            return;
        }

        for (int i = 0; i < frameList.size(); i++) {

            if ( (frameList.elementAt(i) instanceof ViewJFrameBase)
                    && ( ((ViewJFrameBase) frameList.elementAt(i)) != this)) {
                ((ViewJFrameBase) frameList.elementAt(i)).setImageB(_imageB);
            }
        }

        if ( (imageB != null) && ( !imageB.equals(_imageB))) {
            //imageB.disposeLocal(); // Dispose of the memory of the old image
        }

        imageB = _imageB;

        // imageB.setImageOrder(ModelImage.IMAGE_B);
        //setZoomB();

        if (currentComponentImage != null) {
            currentComponentImage.setImageB(imageB);
        } else {
            return;
        }

        imageB.addImageDisplayListener(this);

        if ( !imageB.isColorImage() ) {
            final int[] dimExtentsLUT = new int[2];

            dimExtentsLUT[0] = 4;
            dimExtentsLUT[1] = 256;

            final ModelLUT LUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);

            float imgMin = (float) imageB.getMin();
            float imgMax = (float) imageB.getMax();

            if (imageB.getType() == ModelStorageBase.UBYTE) {
                min = 0;
                max = 255;
                imgMin = 0;
                imgMax = 255;
            } else if (imageB.getType() == ModelStorageBase.BYTE) {
                min = -128;
                max = 127;
                imgMin = -128;
                imgMax = 127;
            } else {
                min = (float) imageB.getMin();
                max = (float) imageB.getMax();
            }

            LUT.resetTransferLine(min, imgMin, max, imgMax);
            imageB.notifyImageDisplayListeners(LUT, true);

            if (imageA.getHistogramFrame() != null) {
                imageB.addImageDisplayListener(imageA.getHistogramFrame());
            }
        } 
        else if ( imageB.isColorImage() ) {
            imageB.notifyImageDisplayListeners(null, true);

            if (imageA.getHistogramFrame() != null) {
                imageB.addImageDisplayListener(imageA.getHistogramFrame());
            }
        }

        //if (getLUTb() != null) {
            //getLUTb().zeroToOneLUTAdjust();
        //}
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
		 if (currentComponentImage != null) {
	            return currentComponentImage.getImageB();
	        } else {
	            return null;
	        }
	}

	@Override
	public void removeControls() {
		// TODO Auto-generated method stub
		
	}

	
	public void setActiveImage(int active) {
		 if (currentComponentImage != null) {
			 currentComponentImage.setActiveImage(active);
	        }
	        if (voiManager != null) {
	            voiManager.getVOIManager(0).setImageB(imageB);
	            voiManager.getVOIManager(0).setActiveImage(active);
	        }

	        if (active == ViewJFrameBase.IMAGE_A) {
	            displayMode = ViewJFrameBase.IMAGE_A;
	            setTitle();
	        } else {
	            displayMode = ViewJFrameBase.IMAGE_B;
	            setTitle();
	        }

	        if (linkTriFrame != null) {
	            linkTriFrame.setActiveImage(active);
	        }


	        updateImages(false);
		
	}


	public void setAlphaBlend(int value) {
		currentComponentImage.setAlphaBlend(100 - value);
		
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
	public boolean updateImages(boolean forceShow) {
		  if (currentComponentImage == null) {
	            return false;
	        }

	        if (currentComponentImage.show(currentComponentImage.getTimeSlice(), currentComponentImage.getSlice(), null, null, forceShow, -1) == false) {
	            return false;
	        }

	        final ViewControlsImage myControls = getControls();

	        if (myControls != null) {
	            myControls.repaint();
	        }

	        return true;
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
        //currentComponentImage.setAlpha(currentOpacity/100f);
        setAlphaBlend(currentOpacity);
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
	
	
	public synchronized void setExtractedMisc(boolean extractedMisc) {
		this.extractedMisc = extractedMisc;
		notify();
	}







	public synchronized void setExtractedAxialT1(boolean extractedAxialT1) {
		this.extractedAxialT1 = extractedAxialT1;
		notify();
	}







	public synchronized void setExtractedAxialT2(boolean extractedAxialT2) {
		this.extractedAxialT2 = extractedAxialT2;
		notify();
	}







	public synchronized void setExtractedAxialPD(boolean extractedAxialPD) {
		this.extractedAxialPD = extractedAxialPD;
		notify();
	}







	public synchronized void setExtractedCoronalT1(boolean extractedCoronalT1) {
		this.extractedCoronalT1 = extractedCoronalT1;
		notify();
	}







	public synchronized void setExtractedCoronalT2(boolean extractedCoronalT2) {
		this.extractedCoronalT2 = extractedCoronalT2;
		notify();
	}







	public synchronized void setExtractedCoronalPD(boolean extractedCoronalPD) {
		this.extractedCoronalPD = extractedCoronalPD;
		notify();
	}







	public synchronized void setExtractedSagittalT1(boolean extractedSagittalT1) {
		this.extractedSagittalT1 = extractedSagittalT1;
		notify();
	}







	public synchronized void setExtractedSagittalT2(boolean extractedSagittalT2) {
		this.extractedSagittalT2 = extractedSagittalT2;
		notify();
	}







	public synchronized void setExtractedSagittalPD(boolean extractedSagittalPD) {
		this.extractedSagittalPD = extractedSagittalPD;
		notify();
	}
	
	
	






	public synchronized boolean isExtractedMisc() {
		notify();
		return extractedMisc;
	}







	public synchronized boolean isExtractedAxialT1() {
		notify();
		return extractedAxialT1;
	}







	public synchronized boolean isExtractedAxialT2() {
		notify();
		return extractedAxialT2;
	}







	public synchronized boolean isExtractedAxialPD() {
		notify();
		return extractedAxialPD;
	}







	public synchronized boolean isExtractedCoronalT1() {
		notify();
		return extractedCoronalT1;
	}







	public synchronized boolean isExtractedCoronalT2() {
		notify();
		return extractedCoronalT2;
	}







	public synchronized boolean isExtractedCoronalPD() {
		notify();
		return extractedCoronalPD;
	}







	public synchronized boolean isExtractedSagittalT1() {
		notify();
		return extractedSagittalT1;
	}







	public synchronized boolean isExtractedSagittalT2() {
		notify();
		return extractedSagittalT2;
	}







	public synchronized boolean isExtractedSagittalPD() {
		notify();
		return extractedSagittalPD;
	}




	
	
	public void progressStateChanged(ProgressChangeEvent e) {
		// TODO Auto-generated method stub
		
	}
	

	////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	




	/**
	 * This inner class downloads and extracts atlas images to user_home/mipav/pedsAtas folder
	 */
	public class DownloadAndExtractImages extends Thread {
		
		public void run() {
			downloadAndExtractImages();
		}
		
		
		public void downloadAndExtractImages() {
	        progressBar = new ViewJProgressBar("PedsAtlas", "Downloading and extracting image files...", 0, 100, true);
	        progressBar.setSeparateThread(true);
            progressBar.setVisible(true);
       
	        
			String pedsHomeString = pedsHome;
			
			long begTime = System.currentTimeMillis();
			
			//the order below is very important.
			ArrayList<String> urlStrings = new ArrayList<String>();
			
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_misc.tar.gz");
			
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_axial_mask.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_coronal_mask.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_sagittal_mask.tar.gz");
			
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_axial_t1_byte_00-08.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_axial_t1_byte_08-17.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_axial_t1_byte_17-33.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_axial_t1_byte_33-60.tar.gz"); 
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_axial_t2_byte_00-08.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_axial_t2_byte_08-17.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_axial_t2_byte_17-33.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_axial_t2_byte_33-60.tar.gz"); 
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_axial_pd_byte_00-08.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_axial_pd_byte_08-17.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_axial_pd_byte_17-33.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_axial_pd_byte_33-60.tar.gz");
			
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_coronal_t1_byte_00-08.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_coronal_t1_byte_08-17.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_coronal_t1_byte_17-33.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_coronal_t1_byte_33-60.tar.gz"); 
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_coronal_t2_byte_00-08.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_coronal_t2_byte_08-17.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_coronal_t2_byte_17-33.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_coronal_t2_byte_33-60.tar.gz"); 
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_coronal_pd_byte_00-08.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_coronal_pd_byte_08-17.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_coronal_pd_byte_17-33.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_coronal_pd_byte_33-60.tar.gz");
			
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_sagittal_t1_byte_00-08.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_sagittal_t1_byte_08-17.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_sagittal_t1_byte_17-33.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_sagittal_t1_byte_33-60.tar.gz"); 
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_sagittal_t2_byte_00-08.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_sagittal_t2_byte_08-17.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_sagittal_t2_byte_17-33.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_sagittal_t2_byte_33-60.tar.gz"); 
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_sagittal_pd_byte_00-08.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_sagittal_pd_byte_08-17.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_sagittal_pd_byte_17-33.tar.gz");
			urlStrings.add("http://mipav.cit.nih.gov/distribution/pedsAtlas/pedsAtlas_sagittal_pd_byte_33-60.tar.gz");
			
			
			
			try {
				
				for(int i=0;i<urlStrings.size();i++) {
					
					
					
					
					String urlString = urlStrings.get(i);
					
					File pedsHomeFile = new File(pedsHomeString);
					if(!pedsHomeFile.exists()) {
						pedsHomeFile.mkdir();
					}
					
					URL url = new URL(urlString);
					URLConnection urlc = url.openConnection();

					GZIPInputStream gis = new GZIPInputStream(urlc.getInputStream());
					TarInputStream tin = new TarInputStream(gis);

				    TarEntry tarEntry = tin.getNextEntry();

				     while (tarEntry != null) {
			                File destPath = new File(pedsHomeString + File.separatorChar + tarEntry.getName());
			                Preferences.debug("Processing " + destPath.getAbsoluteFile() + "\n", Preferences.DEBUG_MINOR);

			                if (!tarEntry.isDirectory()) {
			                	FileOutputStream fout = new FileOutputStream(destPath);
			                    tin.copyEntryContents(fout);
			                    fout.close();
			                } else {
			                    destPath.mkdir();
			                }

			                tarEntry = tin.getNextEntry();
			         }

			         tin.close();
					
			         
			         
			         
			         
			         
			        if(i == 1) {
							if (progressBar != null) {
								progressBar.setMessage("Downloading and extracting mask images");
							}
							
					}if(i == 4) {
						if (progressBar != null) {
							progressBar.setMessage("Downloading and extracting Axial T1 images");
						}
						setExtractedMisc(true);
					}else if(i == 8) {
						if (progressBar != null) {
							progressBar.setMessage("Downloading and extracting Axial T2 images");
						}
						setExtractedAxialT1(true);
					}else if(i == 12) {
						if (progressBar != null) {
							progressBar.setMessage("Downloading and extracting Axial PD images");
						}
						setExtractedAxialT2(true);
					}else if(i == 16) {
						if (progressBar != null) {
							progressBar.setMessage("Downloading and extracting Coronal T1 images");
						}
						setExtractedAxialPD(true);
					}else if(i == 20) {
						if (progressBar != null) {
							progressBar.setMessage("Downloading and extracting Coronal T2 images");
						}
						setExtractedCoronalT1(true);
					}else if(i == 24) {
						if (progressBar != null) {
							progressBar.setMessage("Downloading and extracting Coronal PD images");
						}
						setExtractedCoronalT2(true);
					}else if(i == 28) {
						if (progressBar != null) {
							progressBar.setMessage("Downloading and extracting Sagittal T1 images");
						}
						setExtractedCoronalPD(true);
					}else if(i == 32) {
						if (progressBar != null) {
							progressBar.setMessage("Downloading and extracting Sagittal T2 images");
						}
						setExtractedSagittalT1(true);
					}else if(i == 36) {
						if (progressBar != null) {
							progressBar.setMessage("Downloading and extracting Sagittal PD images");
						}
						setExtractedSagittalT2(true);
					}else if(i == 39) {
						setExtractedSagittalPD(true);
					}
					
					int value = (int)(((float)(i+1)/urlStrings.size())*100);
					if (progressBar != null && mainPanel != null) {
						progressBar.updateValue(value);
						//progressBar.update(progressBar.getGraphics());
					}else if(progressBar != null) {
						progressBar.updateValueImmed(value);
					}
				}

				
			}catch(MalformedURLException e) {
				e.printStackTrace();
			}catch(IOException e) {
				e.printStackTrace();
			}finally {
				
		        final long endTime = System.currentTimeMillis();
		        final long diffTime = endTime - begTime;
		        final float seconds = ((float) diffTime) / 1000;
		        if (progressBar != null) {
                    progressBar.setVisible(false);
                    progressBar.dispose();
		        }
		        System.out.println("Downloading and extracting took " + seconds + " seconds");

			}
			
			
		}


	}
	
	
	
	
	
	
	
	
	
	
	
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
			try {
				finishedLoading = false;
				currAge = currentAge;
				long begTime = System.currentTimeMillis();
				populateModelImages(orient);
				/*if(orient.equals(CORONAL)) {
					loadAnnotations(CORONAL);  
				}else if(orient.equals(AXIAL)) {
					loadAnnotations(AXIAL);  
				}else {
					loadAnnotations(SAGITTAL);  
				}*/
				if(isInterrupted()) {
					//resetButton.setEnabled(false);
					//presetButton.setEnabled(false);
					//lutButton.setEnabled(false);
					setToolBarDisabled();
				}else {
					//resetButton.setEnabled(true);
					//presetButton.setEnabled(true);
					//lutButton.setEnabled(true);
					setToolBarEnabled();
				}
				finishedLoading = true;
				long endTime = System.currentTimeMillis();
		        long diffTime = endTime - begTime;
		        float seconds = ((float) diffTime) / 1000;
		        //loadPresetLUTS();
		        System.out.println("**Loading images took " + seconds + " seconds \n");
			}catch(NullPointerException e) {
				//do nothing
			}
		}
		
		 private void initVOI(ModelImage img, ViewJComponentPedsAtlasImage comp)
		    {
			 	try {
				 	if(img != null && comp != null) {
					 	owner.setActiveImage(img);
				        voiManager = new VOIManagerInterface( owner, img, null, 1, false, null );
				        voiManager.getVOIManager(0).init( owner, img, null, comp, comp, comp.getOrientation() );
				        comp.setVOIManager(voiManager.getVOIManager(0));
				 	}
			 	}catch(NullPointerException e) {
			 		//do nothing
			 	}
		    }
		 
		 
		 
		 
		 
		 private void loadAnnotations(ModelImage img) {
				String annotationsFileName;
				if(orient.equals(CORONAL)) {
					annotationsFileName = coronalAnnotationsFilename;
				}else if(orient.equals(AXIAL)) {
					annotationsFileName = axialAnnotationsFilename;
				}else {
					annotationsFileName = sagittalAnnotationsFilename;
				}
				try {
					VOI[] vois;
					
					fileVOI = new FileVOI(annotationsFileName, annotationsFileDir,img);
					vois = fileVOI.readVOI(true);
					for (int k = 0; k < vois.length; k++) {
						if(vois[k].getColor() == null) {
							vois[k].setColor(Color.white);
		                }
						vois[k].getGeometricCenter();	
						img.registerVOI(vois[k]);
			        }

					//System.out.println(vois.length);
				}catch(Exception ef) {
					ef.printStackTrace();
				}
			}
		 
		 
		 public void loadPresetLUT(ModelImage img, ViewJComponentPedsAtlasImage comp, String modality ) {
			 
			 float fLevel = 0;
			 float fWindow = 0;
		     
		     if(modality.equals("t1")) {
		    	 fLevel = t1LevelPreset;
			     fWindow = t1WindowPreset;
		     }else if(modality.equals("t2")) {
		    	 fLevel = t2LevelPreset;
			     fWindow = t2WindowPreset;
		     }else if(modality.equals("pd")) {
		    	 fLevel = pdLevelPreset;
			     fWindow = pdWindowPreset;
		     }
		     
		     
		    	float fMinImageWin = (float)img.getMin();
		    	
		        float fMaxImageWin = (float)img.getMax();
		        
		        m_afXWin[2] = fLevel + (fWindow / 2.0f);
		         m_afXWin[1] = fLevel - (fWindow / 2.0f);
		         m_afYWin[2] = m_afYWin[3];
		         m_afYWin[1] = m_afYWin[0];

		         if (m_afXWin[2] > fMaxImageWin) {
		             m_afYWin[2] = 255.0f * (m_afXWin[2] - fMaxImageWin) / fWindow;

		             if (m_afYWin[2] > 255.0f) {
		                 m_afYWin[2] = 255.0f;
		             }
		             m_afXWin[2] = fMaxImageWin;
		         }

		         if (m_afXWin[1] < fMinImageWin) {
		             m_afYWin[1] = 255.0f - (255.0f * (fMinImageWin - m_afXWin[1]) / fWindow);

		             if (m_afYWin[1] < 0.0f) {
		                 m_afYWin[1] = 0.0f;
		             }
		             m_afXWin[1] = fMinImageWin;
		         }

		         comp.getActiveLUT().getTransferFunction().importArrays( m_afXWin, m_afYWin, 4 );
		         img.notifyImageDisplayListeners( comp.getActiveLUT(), true );
		         
		         
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
				while(!isExtractedAxialT1()) {
					//do nothing until axial t1 images are extracted
				}
				loadAxialT1();
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedAxialT2()) {
					//do nothing until axial t2 images are extracted
				}
				loadAxialT2();
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedAxialPD()) {
					//do nothing until axial pd images are extracted
				}
				loadAxialPD();
				
			}else if(currentModality.equals(T2)) {
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedAxialT2()) {
					//do nothing until axial t2 images are extracted
				}
				loadAxialT2();
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedAxialT1()) {
					//do nothing until axial t1 images are extracted
				}
				loadAxialT1();
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedAxialPD()) {
					//do nothing until axial pd images are extracted
				}
				loadAxialPD();
				
			}else if(currentModality.equals(PD)) {
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedAxialPD()) {
					//do nothing until axialpd images are extracted
				}
				loadAxialPD();
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedAxialT1()) {
					//do nothing until axial t1 images are extracted
				}
				loadAxialT1();
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedAxialT2()) {
					//do nothing until axial t2 images are extracted
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
				if(isInterrupted()) {
					return;
				}
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t1AtlasImages[i], null, imageBuffer, pixBuffer, 1, t1AtlasImages[i].getExtents(), false, FileInfoBase.AXIAL, T1);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
	

					//owner.setImageA(t1AtlasImages[i]);
					//load in imageB mask image
					File file = new File(pedsHome + axialMaskPathStrings[i]);
					//loadImage(file, comp, false, false, false, 0, 0, 0, 0, true);
					ModelImage imgB = fileIO.readImage(file.getName(), file.getParent() + File.separator, false, null, true);
					maskImages[i] = imgB;
				
				int[] dimExtentsLUT = new int[2];
	            dimExtentsLUT[0] = 4;
	            dimExtentsLUT[1] = 256;
	            ModelLUT LUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
	            if(isInterrupted()) {
					return;
				}
				comp.setLUTa(LUT);
				loadPresetLUT(t1AtlasImages[i], comp, "t1");
				if(isInterrupted()) {
					return;
				}
				initVOI(t1AtlasImages[i],comp);
				loadAnnotations(t1AtlasImages[i]);
				if(displayAnnotations) {
					comp.setDrawVOIs(true);
				}else {
					comp.setDrawVOIs(false);
				}
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
				if(isInterrupted()) {
					return;
				}
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t1AtlasImages[i], null, imageBuffer, pixBuffer, 1, t1AtlasImages[i].getExtents(), false, FileInfoBase.AXIAL, T1);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				
				//load in imageB mask image
				//owner.setImageA(t1AtlasImages[i]);
				//File file = new File(pedsHome + axialMaskPathStrings[i]);
				//loadImage(file, comp, false, false, false, 0, 0, 0, 0, true);
				
				File file = new File(pedsHome + axialMaskPathStrings[i]);
				//loadImage(file, comp, false, false, false, 0, 0, 0, 0, true);
				ModelImage imgB = fileIO.readImage(file.getName(), file.getParent() + File.separator, false, null, true);
				maskImages[i] = imgB;
				
				
				int[] dimExtentsLUT = new int[2];
	            dimExtentsLUT[0] = 4;
	            dimExtentsLUT[1] = 256;
	            ModelLUT LUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
	            if(isInterrupted()) {
					return;
				}
	            comp.setLUTa(LUT);
				loadPresetLUT(t1AtlasImages[i], comp, "t1");
				if(isInterrupted()) {
					return;
				}
				initVOI(t1AtlasImages[i],comp);
				loadAnnotations(t1AtlasImages[i]);
				if(displayAnnotations) {
					comp.setDrawVOIs(true);
				}else {
					comp.setDrawVOIs(false);
				}
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
				if(isInterrupted()) {
					return;
				}
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t2AtlasImages[i], null, imageBuffer, pixBuffer, 1, t2AtlasImages[i].getExtents(), false, FileInfoBase.AXIAL, T2);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				
				//load in imageB mask image
				//File file = new File(pedsHome + axialMaskPathStrings[i]);
				//loadImage(file, comp, false, false, false, 0, 0, 0, 0, true);

				
				
				int[] dimExtentsLUT = new int[2];
	            dimExtentsLUT[0] = 4;
	            dimExtentsLUT[1] = 256;
	            ModelLUT LUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
	            if(isInterrupted()) {
					return;
				}
	            comp.setLUTa(LUT);
				loadPresetLUT(t2AtlasImages[i], comp, "t2");
				if(isInterrupted()) {
					return;
				}
				initVOI(t2AtlasImages[i],comp);
				loadAnnotations(t2AtlasImages[i]);
				if(displayAnnotations) {
					comp.setDrawVOIs(true);
				}else {
					comp.setDrawVOIs(false);
				}
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
				if(isInterrupted()) {
					return;
				}
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t2AtlasImages[i], null, imageBuffer, pixBuffer, 1, t2AtlasImages[i].getExtents(), false, FileInfoBase.AXIAL, T2);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				
				//load in imageB mask image
				//File file = new File(pedsHome + axialMaskPathStrings[i]);
				//loadImage(file, comp, false, false, false, 0, 0, 0, 0, true);
				
				
				int[] dimExtentsLUT = new int[2];
	            dimExtentsLUT[0] = 4;
	            dimExtentsLUT[1] = 256;
	            ModelLUT LUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
	            if(isInterrupted()) {
					return;
				}
	            comp.setLUTa(LUT);
				loadPresetLUT(t2AtlasImages[i], comp, "t2");
				if(isInterrupted()) {
					return;
				}
				initVOI(t2AtlasImages[i],comp);
				loadAnnotations(t2AtlasImages[i]);
				if(displayAnnotations) {
					comp.setDrawVOIs(true);
				}else {
					comp.setDrawVOIs(false);
				}
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
				if(isInterrupted()) {
					return;
				}
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, pdAtlasImages[i], null, imageBuffer, pixBuffer, 1, pdAtlasImages[i].getExtents(), false, FileInfoBase.AXIAL, PD);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				
				//load in imageB mask image
				//File file = new File(pedsHome + axialMaskPathStrings[i]);
				//loadImage(file, comp, false, false, false, 0, 0, 0, 0, true);
				
				
				int[] dimExtentsLUT = new int[2];
	            dimExtentsLUT[0] = 4;
	            dimExtentsLUT[1] = 256;
	            ModelLUT LUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
	            if(isInterrupted()) {
					return;
				}
	            comp.setLUTa(LUT);
				loadPresetLUT(pdAtlasImages[i], comp, "pd");
				if(isInterrupted()) {
					return;
				}
				initVOI(pdAtlasImages[i],comp);
				loadAnnotations(pdAtlasImages[i]);
				if(displayAnnotations) {
					comp.setDrawVOIs(true);
				}else {
					comp.setDrawVOIs(false);
				}
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
				if(isInterrupted()) {
					return;
				}
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, pdAtlasImages[i], null, imageBuffer, pixBuffer, 1, pdAtlasImages[i].getExtents(), false, FileInfoBase.AXIAL, PD);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				
				//load in imageB mask image
				//File file = new File(pedsHome + axialMaskPathStrings[i]);
				//loadImage(file, comp, false, false, false, 0, 0, 0, 0, true);
				
				
				int[] dimExtentsLUT = new int[2];
	            dimExtentsLUT[0] = 4;
	            dimExtentsLUT[1] = 256;
	            ModelLUT LUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
	            if(isInterrupted()) {
					return;
				}
	            comp.setLUTa(LUT);
				loadPresetLUT(pdAtlasImages[i], comp, "pd");
				if(isInterrupted()) {
					return;
				}
				initVOI(pdAtlasImages[i],comp);
				loadAnnotations(pdAtlasImages[i]);
				if(displayAnnotations) {
					comp.setDrawVOIs(true);
				}else {
					comp.setDrawVOIs(false);
				}
				setPDComponentImage(comp,i);
			}
		}
		
		
		public void loadCoronalImages() {
			if(currentModality.equals(T1)) {
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedCoronalT1()) {
					//do nothing until axial t1 images are extracted
				}
				loadCoronalT1();
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedCoronalT2()) {
					//do nothing until axial t2 images are extracted
				}
				loadCoronalT2();
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedCoronalPD()) {
					//do nothing until axial pd images are extracted
				}
				loadCoronalPD();
				
			}else if(currentModality.equals(T2)) {
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedCoronalT2()) {
					//do nothing until axial t2 images are extracted
				}
				loadCoronalT2();
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedCoronalT1()) {
					//do nothing until axial t1 images are extracted
				}
				loadCoronalT1();
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedCoronalPD()) {
					//do nothing until axial pd images are extracted
				}
				loadCoronalPD();
				
			}else if(currentModality.equals(PD)) {
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedCoronalPD()) {
					//do nothing until axial t1 images are extracted
				}
				loadCoronalPD();
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedCoronalT1()) {
					//do nothing until axial t1 images are extracted
				}
				loadCoronalT1();
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedCoronalT2()) {
					//do nothing until axial t2 images are extracted
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
				if(isInterrupted()) {
					return;
				}
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t1AtlasImages[i], null, imageBuffer, pixBuffer, 1, t1AtlasImages[i].getExtents(), false, FileInfoBase.CORONAL, T1);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				
				//load in imageB mask image
				//File file = new File(pedsHome + coronalMaskPathStrings[i]);
				//loadImage(file, comp, false, false, false, 0, 0, 0, 0, true);
				
				File file = new File(pedsHome + coronalMaskPathStrings[i]);
				ModelImage imgB = fileIO.readImage(file.getName(), file.getParent() + File.separator, false, null, true);
				maskImages[i] = imgB;
				
				int[] dimExtentsLUT = new int[2];
	            dimExtentsLUT[0] = 4;
	            dimExtentsLUT[1] = 256;
	            ModelLUT LUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
	            if(isInterrupted()) {
					return;
				}
	            comp.setLUTa(LUT);
				loadPresetLUT(t1AtlasImages[i], comp, "t1");
				if(isInterrupted()) {
					return;
				}
				initVOI(t1AtlasImages[i],comp);
				loadAnnotations(t1AtlasImages[i]);
				if(displayAnnotations) {
					comp.setDrawVOIs(true);
				}else {
					comp.setDrawVOIs(false);
				}
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
					fileVOI.readVOI(true);
				}catch(IOException e) {
					e.printStackTrace();
				}
				float[] imageBuffer = initImageBuffer(t1AtlasImages[i].getExtents(), true);
				int[] pixBuffer = initPixelBuffer(t1AtlasImages[i].getExtents());
				if(isInterrupted()) {
					return;
				}
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t1AtlasImages[i], null, imageBuffer, pixBuffer, 1, t1AtlasImages[i].getExtents(), false, FileInfoBase.CORONAL, T1);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				
				//load in imageB mask image
				//File file = new File(pedsHome + coronalMaskPathStrings[i]);
				//loadImage(file, comp, false, false, false, 0, 0, 0, 0, true);
				
				
				File file = new File(pedsHome + coronalMaskPathStrings[i]);
				ModelImage imgB = fileIO.readImage(file.getName(), file.getParent() + File.separator, false, null, true);
				maskImages[i] = imgB;
				
				int[] dimExtentsLUT = new int[2];
	            dimExtentsLUT[0] = 4;
	            dimExtentsLUT[1] = 256;
	            ModelLUT LUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
	            if(isInterrupted()) {
					return;
				}
	            comp.setLUTa(LUT);
				loadPresetLUT(t1AtlasImages[i], comp, "t1");
				if(isInterrupted()) {
					return;
				}
				initVOI(t1AtlasImages[i],comp);
				loadAnnotations(t1AtlasImages[i]);
				if(displayAnnotations) {
					comp.setDrawVOIs(true);
				}else {
					comp.setDrawVOIs(false);
				}
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
				if(isInterrupted()) {
					return;
				}
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t2AtlasImages[i], null, imageBuffer, pixBuffer, 1, t2AtlasImages[i].getExtents(), false, FileInfoBase.CORONAL, T2);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				
				//load in imageB mask image
				//File file = new File(pedsHome + coronalMaskPathStrings[i]);
				//loadImage(file, comp, false, false, false, 0, 0, 0, 0, true);
				
				
				int[] dimExtentsLUT = new int[2];
	            dimExtentsLUT[0] = 4;
	            dimExtentsLUT[1] = 256;
	            ModelLUT LUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
	            if(isInterrupted()) {
					return;
				}
	            comp.setLUTa(LUT);
				loadPresetLUT(t2AtlasImages[i], comp, "t2");
				if(isInterrupted()) {
					return;
				}
				initVOI(t2AtlasImages[i],comp);
				loadAnnotations(t2AtlasImages[i]);
				if(displayAnnotations) {
					comp.setDrawVOIs(true);
				}else {
					comp.setDrawVOIs(false);
				}
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
				if(isInterrupted()) {
					return;
				}
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t2AtlasImages[i], null, imageBuffer, pixBuffer, 1, t2AtlasImages[i].getExtents(), false, FileInfoBase.CORONAL, T2);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				
				//load in imageB mask image
				//File file = new File(pedsHome + coronalMaskPathStrings[i]);
				//loadImage(file, comp, false, false, false, 0, 0, 0, 0, true);
				
				
				int[] dimExtentsLUT = new int[2];
	            dimExtentsLUT[0] = 4;
	            dimExtentsLUT[1] = 256;
	            ModelLUT LUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
	            if(isInterrupted()) {
					return;
				}
	            comp.setLUTa(LUT);
				loadPresetLUT(t2AtlasImages[i], comp, "t2");
				if(isInterrupted()) {
					return;
				}
				initVOI(t2AtlasImages[i],comp);
				loadAnnotations(t2AtlasImages[i]);
				if(displayAnnotations) {
					comp.setDrawVOIs(true);
				}else {
					comp.setDrawVOIs(false);
				}
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
				if(isInterrupted()) {
					return;
				}
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, pdAtlasImages[i], null, imageBuffer, pixBuffer, 1, pdAtlasImages[i].getExtents(), false, FileInfoBase.CORONAL, PD);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				
				//load in imageB mask image
				//File file = new File(pedsHome + coronalMaskPathStrings[i]);
				//loadImage(file, comp, false, false, false, 0, 0, 0, 0, true);
				
				
				int[] dimExtentsLUT = new int[2];
	            dimExtentsLUT[0] = 4;
	            dimExtentsLUT[1] = 256;
	            ModelLUT LUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
	            if(isInterrupted()) {
					return;
				}
	            comp.setLUTa(LUT);
				loadPresetLUT(pdAtlasImages[i], comp, "pd");
				if(isInterrupted()) {
					return;
				}
				initVOI(pdAtlasImages[i],comp);
				loadAnnotations(pdAtlasImages[i]);
				if(displayAnnotations) {
					comp.setDrawVOIs(true);
				}else {
					comp.setDrawVOIs(false);
				}
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
				if(isInterrupted()) {
					return;
				}
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, pdAtlasImages[i], null, imageBuffer, pixBuffer, 1, pdAtlasImages[i].getExtents(), false, FileInfoBase.CORONAL, PD);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				
				//load in imageB mask image
				//File file = new File(pedsHome + coronalMaskPathStrings[i]);
				//loadImage(file, comp, false, false, false, 0, 0, 0, 0, true);
				
				
				int[] dimExtentsLUT = new int[2];
	            dimExtentsLUT[0] = 4;
	            dimExtentsLUT[1] = 256;
	            ModelLUT LUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
	            if(isInterrupted()) {
					return;
				}
	            comp.setLUTa(LUT);
				loadPresetLUT(pdAtlasImages[i], comp, "pd");
				if(isInterrupted()) {
					return;
				}
				initVOI(pdAtlasImages[i],comp);
				loadAnnotations(pdAtlasImages[i]);
				if(displayAnnotations) {
					comp.setDrawVOIs(true);
				}else {
					comp.setDrawVOIs(false);
				}
				setPDComponentImage(comp,i);
			}
		
		}
		
		
		
		public void loadSagittalImages() {
			if(currentModality.equals(T1)) {
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedSagittalT1()) {
					//do nothing until sagittal t1 images are extracted
				}
				loadSagittalT1();
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedSagittalT2()) {
					//do nothing until sagittal t2 images are extracted
				}
				loadSagittalT2();
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedSagittalPD()) {
					//do nothing until sagittal pd images are extracted
				}
				loadSagittalPD();
				
			}else if(currentModality.equals(T2)) {
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedSagittalT2()) {
					//do nothing until sagittal t2 images are extracted
				}
				loadSagittalT2();
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedSagittalT1()) {
					//do nothing until sagittal t1 images are extracted
				}
				loadSagittalT1();
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedSagittalPD()) {
					//do nothing until sagittal pd images are extracted
				}
				loadSagittalPD();
				
			}else if(currentModality.equals(PD)) {
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedSagittalPD()) {
					//do nothing until sagittal pd images are extracted
				}
				loadSagittalPD();
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedSagittalT1()) {
					//do nothing until sagittal t1 images are extracted
				}
				loadSagittalT1();
				if(isInterrupted()) {
					return;
				}
				while(!isExtractedSagittalT2()) {
					//do nothing until sagittal t2 images are extracted
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
				if(isInterrupted()) {
					return;
				}
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t1AtlasImages[i], null, imageBuffer, pixBuffer, 1, t1AtlasImages[i].getExtents(), false, FileInfoBase.SAGITTAL, T1);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				
				//load in imageB mask image
				//File file = new File(pedsHome + sagittalMaskPathStrings[i]);
				//loadImage(file, comp, false, false, false, 0, 0, 0, 0, true);
				
				File file = new File(pedsHome + sagittalMaskPathStrings[i]);
				ModelImage imgB = fileIO.readImage(file.getName(), file.getParent() + File.separator, false, null, true);
				maskImages[i] = imgB;
				
				
				int[] dimExtentsLUT = new int[2];
	            dimExtentsLUT[0] = 4;
	            dimExtentsLUT[1] = 256;
	            ModelLUT LUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
	            if(isInterrupted()) {
					return;
				}
	            comp.setLUTa(LUT);
				loadPresetLUT(t1AtlasImages[i], comp, "t1");
				if(isInterrupted()) {
					return;
				}
				initVOI(t1AtlasImages[i],comp);
				loadAnnotations(t1AtlasImages[i]);
				if(displayAnnotations) {
					comp.setDrawVOIs(true);
				}else {
					comp.setDrawVOIs(false);
				}
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
				if(isInterrupted()) {
					return;
				}
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t1AtlasImages[i], null, imageBuffer, pixBuffer, 1, t1AtlasImages[i].getExtents(), false, FileInfoBase.SAGITTAL, T1);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				
				//load in imageB mask image
				//File file = new File(pedsHome + sagittalMaskPathStrings[i]);
				//loadImage(file, comp, false, false, false, 0, 0, 0, 0, true);
				
				File file = new File(pedsHome + sagittalMaskPathStrings[i]);
				ModelImage imgB = fileIO.readImage(file.getName(), file.getParent() + File.separator, false, null, true);
				maskImages[i] = imgB;
				
				int[] dimExtentsLUT = new int[2];
	            dimExtentsLUT[0] = 4;
	            dimExtentsLUT[1] = 256;
	            ModelLUT LUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
	            if(isInterrupted()) {
					return;
				}
	            comp.setLUTa(LUT);
				loadPresetLUT(t1AtlasImages[i], comp, "t1");
				if(isInterrupted()) {
					return;
				}
				initVOI(t1AtlasImages[i],comp);
				loadAnnotations(t1AtlasImages[i]);
				if(displayAnnotations) {
					comp.setDrawVOIs(true);
				}else {
					comp.setDrawVOIs(false);
				}
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
				if(isInterrupted()) {
					return;
				}
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t2AtlasImages[i], null, imageBuffer, pixBuffer, 1, t2AtlasImages[i].getExtents(), false, FileInfoBase.SAGITTAL, T2);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				
				//load in imageB mask image
				//File file = new File(pedsHome + sagittalMaskPathStrings[i]);
				//loadImage(file, comp, false, false, false, 0, 0, 0, 0, true);
				
				
				int[] dimExtentsLUT = new int[2];
	            dimExtentsLUT[0] = 4;
	            dimExtentsLUT[1] = 256;
	            ModelLUT LUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
	            if(isInterrupted()) {
					return;
				}
	            comp.setLUTa(LUT);
				loadPresetLUT(t2AtlasImages[i], comp, "t2");
				if(isInterrupted()) {
					return;
				}
				initVOI(t2AtlasImages[i],comp);
				loadAnnotations(t2AtlasImages[i]);
				if(displayAnnotations) {
					comp.setDrawVOIs(true);
				}else {
					comp.setDrawVOIs(false);
				}
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
				if(isInterrupted()) {
					return;
				}
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, t2AtlasImages[i], null, imageBuffer, pixBuffer, 1, t2AtlasImages[i].getExtents(), false, FileInfoBase.SAGITTAL, T2);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				
				//load in imageB mask image
				//File file = new File(pedsHome + sagittalMaskPathStrings[i]);
				//loadImage(file, comp, false, false, false, 0, 0, 0, 0, true);
				
				
				int[] dimExtentsLUT = new int[2];
	            dimExtentsLUT[0] = 4;
	            dimExtentsLUT[1] = 256;
	            ModelLUT LUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
	            if(isInterrupted()) {
					return;
				}
	            comp.setLUTa(LUT);
				loadPresetLUT(t2AtlasImages[i], comp, "t2");
				if(isInterrupted()) {
					return;
				}
				initVOI(t2AtlasImages[i],comp);
				loadAnnotations(t2AtlasImages[i]);
				if(displayAnnotations) {
					comp.setDrawVOIs(true);
				}else {
					comp.setDrawVOIs(false);
				}
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
				infoLabel.setText("Loading sagittal PD images : " + extraSpace + c + "/" + numAgeTicks + " ");
				atlasFile = new File(pedsHome + sagittalPDPathStrings[i]);
				pdAtlasImages[i] = fileIO.readImage(atlasFile.getName(), atlasFile.getParent() + File.separator, false, null);
				pdAtlasImages[i].addImageDisplayListener(owner);
				float[] imageBuffer = initImageBuffer(pdAtlasImages[i].getExtents(), true);
				int[] pixBuffer = initPixelBuffer(pdAtlasImages[i].getExtents());
				if(isInterrupted()) {
					return;
				}
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, pdAtlasImages[i], null, imageBuffer, pixBuffer, 1, pdAtlasImages[i].getExtents(), false, FileInfoBase.SAGITTAL, PD);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				
				//load in imageB mask image
				//File file = new File(pedsHome + sagittalMaskPathStrings[i]);
				//loadImage(file, comp, false, false, false, 0, 0, 0, 0, true);
				
				int[] dimExtentsLUT = new int[2];
	            dimExtentsLUT[0] = 4;
	            dimExtentsLUT[1] = 256;
	            ModelLUT LUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
	            if(isInterrupted()) {
					return;
				}
	            comp.setLUTa(LUT);
				loadPresetLUT(pdAtlasImages[i], comp, "pd");
				if(isInterrupted()) {
					return;
				}
				initVOI(pdAtlasImages[i],comp);
				loadAnnotations(pdAtlasImages[i]);
				if(displayAnnotations) {
					comp.setDrawVOIs(true);
				}else {
					comp.setDrawVOIs(false);
				}
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
				infoLabel.setText("Loading sagittal PD images : " + extraSpace + c + "/" + numAgeTicks + " ");
				c++;
				if(c==12) {
					c=1;
				}
				atlasFile = new File(pedsHome + sagittalPDPathStrings[i]);
				pdAtlasImages[i] = fileIO.readImage(atlasFile.getName(), atlasFile.getParent() + File.separator, false, null);
				pdAtlasImages[i].addImageDisplayListener(owner);
				float[] imageBuffer = initImageBuffer(pdAtlasImages[i].getExtents(), true);
				int[] pixBuffer = initPixelBuffer(pdAtlasImages[i].getExtents());
				if(isInterrupted()) {
					return;
				}
				ViewJComponentPedsAtlasImage comp = new ViewJComponentPedsAtlasImage(owner, pdAtlasImages[i], null, imageBuffer, pixBuffer, 1, pdAtlasImages[i].getExtents(), false, FileInfoBase.SAGITTAL, PD);
				comp.addMouseWheelListener(owner);
				comp.setBuffers(imageBuffer, null, pixBuffer, null);
				
				//load in imageB mask image
				//File file = new File(pedsHome + sagittalMaskPathStrings[i]);
				//loadImage(file, comp, false, false, false, 0, 0, 0, 0, true);
				
				int[] dimExtentsLUT = new int[2];
	            dimExtentsLUT[0] = 4;
	            dimExtentsLUT[1] = 256;
	            ModelLUT LUT = new ModelLUT(ModelLUT.GRAY, 256, dimExtentsLUT);
	            if(isInterrupted()) {
					return;
				}
	            comp.setLUTa(LUT);
				loadPresetLUT(pdAtlasImages[i], comp, "pd");
				if(isInterrupted()) {
					return;
				}
				initVOI(pdAtlasImages[i],comp);
				loadAnnotations(pdAtlasImages[i]);
				if(displayAnnotations) {
					comp.setDrawVOIs(true);
				}else {
					comp.setDrawVOIs(false);
				}
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







	@Override
	public void mouseClicked(MouseEvent e) {
		Object source = e.getSource();



        if (source == helpLabel) {
            if (e.getButton() == MouseEvent.BUTTON1) {
            	
            	 openURL(pedsHome + File.separator + "config" + File.separator + "PedsAltasHelp.html");
                

            	
            }
        }
		
	}



	 /**
     * Launches browser...code obtained from: Bare Bones Browser Launch by Dem Pilafian Web Page Copyright (c) 2007
     * Center Key Software Source Code and Javadoc are Public Domain http://www.centerkey.com/java/browser
     * 
     * @param url
     */
    public void openURL(String url) {

        String osName = System.getProperty("os.name");
        try {
            if (osName.startsWith("Mac OS")) {
                Class fileMgr = Class.forName("com.apple.eio.FileManager");
                Method openURL = fileMgr.getDeclaredMethod("openURL", new Class[] {String.class});
                openURL.invoke(null, new Object[] {url});
            } else if (osName.startsWith("Windows")) {
                Runtime.getRuntime().exec("rundll32 url.dll,FileProtocolHandler " + url);
            } else { // assume Unix or Linux
                String[] browsers = {"firefox", "opera", "konqueror", "epiphany", "mozilla", "netscape"};
                String browser = null;
                for (int count = 0; count < browsers.length && browser == null; count++) {
                    if (Runtime.getRuntime().exec(new String[] {"which", browsers[count]}).waitFor() == 0) {
                        browser = browsers[count];
                    }
                }
                if (browser == null) {
                    System.out.println("Can not find web browser");
                } else {
                    Runtime.getRuntime().exec(new String[] {browser, url});
                }
            }
        } catch (Exception e) {
            System.out.println("Can not find web browser");
        }
    }



	/**
     * mouse entered
     */
    public void mouseEntered(MouseEvent event) {
        Object source = event.getSource();

        if (source == helpLabel) {
            setCursor(new Cursor(Cursor.HAND_CURSOR));
        }

    }

    /**
     * mouse exited
     */
    public void mouseExited(MouseEvent event) {
        Object source = event.getSource();

        if (source == helpLabel) {
            setCursor(new Cursor(Cursor.DEFAULT_CURSOR));
        }

    }







	@Override
	public void mousePressed(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}







	@Override
	public void mouseReleased(MouseEvent e) {
		// TODO Auto-generated method stub
		
	}

}
